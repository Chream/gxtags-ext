(import :std/actor
        :std/sort
        :std/sugar
        :std/srfi/1
        :std/srfi/13
        :std/text/json
        (only-in :gerbil/gambit
                 spawn
                 spawn/group
                 spawn-actor
                 current-thread
                 display-exception
                 write-string
                 make-thread-group
                 thread-group->thread-list
                 thread-yield!
                 thread-name)
        (only-in :std/misc/ports
                 read-file-lines
                 read-all-as-lines)

        :clan/utils/base
        (only-in :clan/utils/files maybe-replace-file)

        :chream/utils/all
        "tag-impl")

(export #t)

;; State

(def worker-thread-group
  (make-parameter (make-thread-group 'tag-workers)))
(def index-thread-group
  (make-parameter (make-thread-group 'tag-indicies)))

;; setters/getters

(def (find-actor name thread-group)
  (let lp ((actors (thread-group->thread-list thread-group)))
    (cond ((null? actors) #f)
          ((equal? name (thread-name (car actors))) (car actors))
          (else (lp (cdr actors))))))

(def (spawn-worker tag-file)
  (let* ((tag-file (path-normalize tag-file))
         (w (spawn-actor tag-worker-actor
                         [tag-file]
                         tag-file
                         (worker-thread-group))))
    w))

(def (spawn-index (index-file "~/.gerbil/tags/default-index"))
  (let* ((index-file (path-normalize index-file))
         (i (spawn-actor tags-table-actor
                         [index-file]
                         index-file
                         (index-thread-group))))
    i))

(def (maybe-spawn-worker tag-file)
  (let* (w (find-actor (path-normalize tag-file) (worker-thread-group)))
    (if w w (spawn-worker tag-file))))

(def (maybe-spawn-index index-file)
  (let* (w (find-actor (path-normalize index-file) (index-thread-group)))
    (if w w (spawn-index index-file))))

(def (stop-worker tag-file)
  (!!tag-worker.stop! (find-actor (path-normalize tag-file)
                                  (worker-thread-group))))

(def (stop-workers)
  (for-each (cut !!tag-worker.stop! <>)
            (thread-group->thread-list (worker-thread-group))))

(def (stop-index index-file)
  (!!tag-table.stop! (find-actor (path-normalize index-file)
                                 (index-thread-group))))

(def (stop-indicies)
  (for-each (cut !!tag-worker.stop! <>)
            (thread-group->thread-list (index-thread-group))))

(def (worker-thread-list)
  (thread-group->thread-list (worker-thread-group)))

(def (index-thread-list)
  (thread-group->thread-list (index-thread-group)))

;;
;; Main tag-table actor
;;

(defproto tag-table
  event:
  (insert! input tagfile)
  (delete! input tagfile)
  (stop!)
  call:
  (files)
  (lookup key)
  (search key)
  (search-regexp key))

(def (tags-table-actor index-file)

  (def (maybe-save-tag-file! index-file new-tagfile)
    (maybe-replace-file
     index-file
     (lambda (files)
       (delete-duplicates (sort [new-tagfile . files] string<?)))
     reader: read-all-as-lines
     writer: (lambda (lines out)
               (for-each (lambda (path)
                           (write-string path out)
                           (newline out))
                         lines))))

  (def (ensure-workers-exists! tag-files)
    (for-each (cut maybe-spawn-worker <>) tag-files))

  (defrules workers-collect-with ()
    ((_ fn files key)
     (apply append
       (filter-map (lambda (w)
                     (and (member (thread-name w) files)
                          (fn w key)))
                   (thread-group->thread-list (worker-thread-group))))))

  (def (make-tags-table-actor index-file)
    (let (tag-files (read-file-lines index-file))
      (ensure-workers-exists! tag-files)
      (let lp ((tag-files tag-files))
        (try
         (<- ((!tag-table.lookup key k)
              (let (result (workers-collect-with !!tag-worker.lookup tag-files key))
                (!!value result k)
                (lp tag-files)))
             ((!tag-table.search key k)
              (let (result (workers-collect-with !!tag-worker.search tag-files key))
                (!!value result k)
                (lp tag-files)))
             ((!tag-table.search-regexp key k)
              (let (result (workers-collect-with !!tag-worker.search-regexp tag-files key))
                (!!value result k)
                (lp tag-files)))
             ((!tag-table.files k)
              (!!value tag-files k)
              (lp tag-files))
             ((!tag-table.insert! inputs tagfile)
              (maybe-save-tag-file! index-file tagfile)
              (let (actor (maybe-spawn-worker tagfile))
                (for-each (cut !!tag-worker.put! actor <>) inputs))
              (lp [tagfile . tag-files]))
             ((!tag-table.stop!)
              (displayln "tags-table-actor thread stopped: " (current-thread))
              (thread-yield!)))
         (catch (exception? e) (begin (simple-actor-exception-handler e)
                                      (lp tag-files)))))))
  (try
   (let* ((index-file (path-normalize index-file)))
     (logg "Creating table actor")
     (ensure-file-exists! index-file)
     ;; If not, create new actor.
     (make-tags-table-actor index-file))
   (catch (e) (simple-actor-exception-handler e))))

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

(def (%tag-worker-actor-put! file input)
  (let* ((input (path-normalize input))
         (file (path-normalize file))
         (tags (tag-input input)))
    (maybe-replace-file
     file
     (lambda (json)
       (when (eof-object? json)
         (set! json (make-json)))
       (cond ((json-empty? json) tags)
             (else (json-merge! tags json)
                   tags)))
     reader: read-json-equal
     writer: write-json)))

(def (tag-worker-actor file)
  (let ((file (path-normalize file)))
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
            (%tag-worker-actor-put! file input)
            (lp))
           ((!tag-worker.stop!)
            (displayln "tag-file-worker thread stopped: " (current-thread))
            (thread-yield!)))
       (catch (exception? e) (begin (simple-actor-exception-handler e)
                                    (lp)))))))

;; Utils

(def (simple-actor-exception-handler e)
  (newline)
  (display "Simple Actor error: ")
  (display (current-thread))
  (newline)
  (display-exception e)  (displayln "Restarting worker..")
  (newline))
