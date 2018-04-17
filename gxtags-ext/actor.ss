(import :std/actor
        :std/sort
        :std/sugar
        :std/srfi/1
        :std/srfi/13
        :std/text/json
        (only-in :gerbil/gambit spawn current-thread display-exception write-string)
        (only-in :std/misc/ports read-file-lines read-all-as-lines)

        :clan/utils/base
        (only-in :clan/utils/files maybe-replace-file)

        (only-in :chream/utils/all ensure-file-exists!)
        :chream/utils/text/json
        :chream/gxtags-ext/tag-impl)

(export #t)

(defproto tag-worker
  event:
  (put! input)
  (del! key)
  (stop!)
  call:
  (table)
  (file)
  (lookup key)
  (search key)
  (search-regexp key))

(def (%tag-file-worker-put! file input)
  (let* ((input (path-normalize input))
         (file (path-normalize file))
         (tags (tag-input input)))
    (maybe-replace-file
     file
     (lambda (json)
       (cond ((json-empty? json) tags)
             (else (json-merge! json tags)
                   json)))
     reader: read-json-equal
     writer: write-json)))

(def (tag-file-worker file)
  (let (file (path-normalize file))
    (ensure-json-file-exists! file)
    (let lp ()
      (try
       (<- ((!tag-worker.lookup key k)
            (let (tag-table (read-json-equal-file file))
              (!!value (tag-lookup key tag-table) k)
              (lp)))

           ((!tag-worker.search key k)
            (let (tag-table (read-json-equal-file file))
              (!!value (tag-search key tag-table) k)
              (lp)))

           ((!tag-worker.search-regexp key k)
            (let (tag-table (read-json-equal-file file))
              (!!value (tag-search-regexp key tag-table) k)
              (lp)))

           ((!tag-worker.file k)
            (!!value file k)
            (lp))

           ((!tag-worker.table k)
            (!!value (read-json-equal-file file) k)
            (lp))

           ((!tag-worker.put! input)
            (%tag-file-worker-put! file input)
            (lp))

           ((!tag-worker.stop!)
            (displayln "tag-file-worker thread stopped: " (current-thread))
            (void)))
       (catch (exception? e) (begin (newline)
                                    (display "Actor error: ")
                                    (display (current-thread))
                                    (newline)
                                    (display-exception e)
                                    (displayln "Restarting worker..")
                                    (newline)
                                    (lp)))))))

(defproto tag-table
  event:
  (insert! input tagfile)
  (delete! input tagfile)
  (stop!)
  call:
  (table)
  (files)
  (workers)
  (lookup-in-file key tagfile)
  (search-in-file key tagfile)
  (search-regexp-in-file key tagfile)
  (lookup key)
  (search key)
  (search-regexp key))

(def (tags-index index-file)
  (def (find-worker file actors)
    (let lp ((actors-1 actors))
      (if (null? actors-1)
        #f
        (let ((cact (car actors-1))
              (ract (cdr actors-1)))
          (if (equal? file (!!tag-worker.file cact))
            cact
            (lp ract))))))

  (def (save-tag-file! index-file new-tagfile)
    (let (act #f)
      (maybe-replace-file
       index-file
       (lambda (files)
         (sort [new-tagfile . files] string<?))
       reader: read-all-as-lines
       writer: (lambda (lines out)
                 (for-each (lambda (path)
                             (write-string path out)
                             (newline out))
                           lines)))))

  (let (index-file (path-normalize index-file))
    (ensure-file-exists! index-file)
    (let* ((tag-files (read-file-lines index-file))
           (workers (map (cut spawn tag-file-worker <>) tag-files)))
      (let lp ()
        (try
         (<- ((!tag-table.lookup key k)
              (!!value  (append-map (cut !!tag-worker.lookup <> key) workers) k)
              (lp))
             ((!tag-table.search key k)
              (!!value  (append-map (cut !!tag-worker.search <> key) workers) k)
              (lp))
             ((!tag-table.search-regexp key k)
              (!!value  (append-map (cut !!tag-worker.search-regexp <> key) workers) k)
              (lp))
             ((!tag-table.files k)
              (!!value tag-files k)
              (lp))
             ((!tag-table.table k)
              (let ((tags-table (make-json)))
                (for-each (lambda (act)
                            (json-merge! tags-table (!!tag-worker.table act)))
                          workers)
                (!!value tags-table k))
              (lp))
             ((!tag-table.workers k)
              (!!value workers k)
              (lp))
             ((!tag-table.insert! inputs tagfile)
              (if-let (act (find-worker tagfile workers))
                      ;; Search in worker actors.
                      (for-each (cut !!tag-worker.put! act <>) inputs)
                      ;; Make new actor and add to registry.
                      (let (act (spawn tag-file-worker tagfile))
                        (for-each (cut !!tag-worker.put! act <>) inputs)
                        (save-tag-file! index-file tagfile)
                        (set! tag-files [tagfile . tag-files])
                        (set! workers [act . workers])))
              (lp))
             ((!tag-table.stop!)
              (displayln "tags-index thread stopped: " (current-thread))))
         (catch (exception? e) (begin (newline)
                                      (display "Actor error-tags-index: ")
                                      (display (current-thread))
                                      (newline)
                                      (display-exception e)
                                      (newline)
                                      (displayln "Restaring..")
                                      (newline)
                                      (lp))))))))

(def default-tags-index
  (spawn tags-index
         (path-normalize "~/.gerbil/tags/default-index")))
