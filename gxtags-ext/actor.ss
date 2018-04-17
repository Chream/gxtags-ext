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

        :chream/utils/all
        :chream/gxtags-ext/tag-impl)

(export #t)

;;
;; Main tag-table actor
;;


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

(def (tags-table-actor index-file)
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
              (!!value  (append-map (cut !!tag-worker.lookup <> key) workers) k))
             ((!tag-table.search key k)
              (!!value  (append-map (cut !!tag-worker.search <> key) workers) k))
             ((!tag-table.search-regexp key k)
              (!!value  (append-map (cut !!tag-worker.search-regexp <> key) workers) k))
             ((!tag-table.files k)
              (!!value tag-files k))
             ((!tag-table.table k)
              (let ((tags-table (make-json)))
                (for-each (lambda (act)
                            (json-merge! tags-table (!!tag-worker.table act)))
                          workers)
                (!!value tags-table k)))
             ((!tag-table.workers k)
              (!!value workers k))
             ((!tag-table.insert! inputs tagfile)
              (let (act (or (find-worker tagfile workers)
                            (spawn tag-file-worker tagfile)))
                (when (!!tag-worker.new? act)
                  (save-tag-file! index-file tagfile)
                  (set! tag-files [tagfile . tag-files])
                  (set! workers [act . workers]))
                (for-each (cut !!tag-worker.put! act <>) inputs)))
             ((!tag-table.stop!)
              (displayln "tags-table-actor thread stopped: " (current-thread))))
         (catch (exception? e) (begin (newline)
                                      (display "Actor error-tags-table-actor: ")
                                      (display (current-thread))
                                      (newline)
                                      (display-exception e)
                                      (newline)
                                      (displayln "Restaring..")
                                      (newline)))
         (finally (lp)))))))

;;
;; Worker actor.
;;

(defproto tag-worker
  event:
  (put! input)
  (del! key)
  (stop!)
  call:
  (table)
  (file)
  (new?)
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
  (let ((file (path-normalize file))
        (new? #t))
    (ensure-json-file-exists! file)
    (let lp ()
      (try
       (<- ((!tag-worker.lookup key k)
            (let (tag-table (read-json-equal-file file))
              (!!value (tag-lookup key tag-table) k)))
           ((!tag-worker.search key k)
            (let (tag-table (read-json-equal-file file))
              (!!value (tag-search key tag-table) k)))
           ((!tag-worker.search-regexp key k)
            (let (tag-table (read-json-equal-file file))
              (!!value (tag-search-regexp key tag-table) k)))
           ((!tag-worker.file k)
            (!!value file k))
           ((!tag-worker.table k)
            (!!value (read-json-equal-file file) k))
           ((!tag-worker.put! input)
            (%tag-file-worker-put! file input))
           ((!tag-worker.stop!)
            (displayln "tag-file-worker thread stopped: " (current-thread))
            (void))
           ((!tag-worker.new? k)
            (!!value new? k)))
       (catch (exception? e) (begin (newline)
                                    (display "Worker Actor error: ")
                                    (display (current-thread))
                                    (newline)
                                    (display-exception e)
                                    (displayln "Restarting worker..")
                                    (newline)))
       (finally (begin (when new?
                         (set! new? #f))
                       (lp)))))))

;;
;; Exported state
;;
(def default-tags-table
  (spawn tags-table-actor
         (path-normalize "~/.gerbil/tags/default-index")))
