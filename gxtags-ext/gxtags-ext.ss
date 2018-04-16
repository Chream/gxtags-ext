;;; -*- Gerbil -*-
;;; This file is copied and modified from Gerbil Scheme
;;; See https://github.com/vyzo/gerbil
;;; © vyzo

;;; Generate emacs TAGS from gerbil sources
;;; only exported symbols are tagged.
;;; Usage: gxtags [-a] [-o tags-file] file-or-directory ...
;;;
;;; 2018: This file was modified by
;;; Christopher Eames (Chream) <chream-gmx.com>

(import :gerbil/expander
        (only-in :gerbil/compiler/base ast-case)
        (only-in <syntax-case> syntax)
        (only-in :std/srfi/1 append-map)
        :gerbil/gambit
        :std/getopt
        :std/sort
        :std/sugar
        :std/sort
        :std/text/utf8
        :std/text/json
        :std/misc/ports
        (only-in :std/srfi/13 string-contains)
        :std/misc/repr
        (only-in :std/srfi/1 delete-duplicates reverse!)

        :std/pregexp

        (only-in :clan/utils/base !> if-let)
        (only-in :clan/utils/hash hash-ensure-ref)
        (only-in :clan/utils/files maybe-replace-file)

        :std/actor

        :chream/utils/text/json
        (only-in :chream/utils/gerbil/context resolve-module-export-root-module)

        (only-in :chream/utils/misc/asserts check-type)
        (only-in :chream/utils/misc/files file-directory? ensure-file-exists! )
        (only-in :chream/utils/misc/debug logg)
        :chream/utils/misc/location

        :chream/utils/misc/repr
        )

(export #t)

;; (def (main . args)
;;   (def gopt
;;     (getopt (flag 'append "-a"
;;                help: "append to existing tag file")
;;             (option 'output "-o" default: "TAGS"
;;                help: "explicit name of file for tag table")
;;             (flag 'help "-h" "--help"
;;                help: "display help")
;;             (rest-arguments 'input
;;                             help: "source file or directory")
;;             (flag 'reset "--reset"
;;                   help: "reset TAGS database")))

;;   (def (help what)
;;     (getopt-display-help what "gxtags"))

;;   (try
;;    (let (opt (getopt-parse gopt args))
;;      (cond ((hash-get opt 'help)
;;             (help gopt))
;;            ((hash-get opt 'reset)
;;             (reset-tags!))
;;            (else
;;             (let ((inputs (hash-get opt 'input)))
;;               (if (and (null? inputs))
;;                 (begin
;;                   (help gopt)
;;                   (exit 1))
;;                 (run (hash-get opt 'input)
;;                      (hash-get opt 'output)
;;                      (hash-get opt 'append)))))))
;;    (catch (getopt-error? exn)
;;      (help exn)
;;      (exit 1))))

(def (run inputs tagfile append?)
  (def (expand-input-paths base inputs)
    (map (cut path-expand <> (path-directory base))
         inputs))

  (_gx#load-expander!)
  ;; (for-each (cut !!tags.tag-input <> input) inputs)
  ;; (!!tags.)
  )

(def tags-path
  (make-parameter (path-normalize "~/.gerbil/tags/tags.json")))
(def current-tags-table
  (make-parameter (make-hash-table)))

(def (module-tags ctx xtab into: (ht (make-hash-table)))

  (def loc #f)

  (defrules with-loc ()
    ((_ stx body rest ...)
     (let (K (lambda () body rest ...))
       (let (new-loc (stx-source stx))
         (if new-loc
           (let (save-loc loc)
             (set! loc new-loc)
             (K)
             (set! loc save-loc))
           (K))))))

  (def (tag! module eid name)
    (let* ((path (locat-path loc))
           (position (filepos-line (locat-position loc))))
      (let ((tag (make-hash-table size: 4)))
        (json-add! tag "key" (symbol->string name))
        (json-add! tag "path" (json-make-ref! ht "path" path))
        (json-add! tag "mod" (json-make-ref! ht "mod" module))
        (json-add! tag "pos" position)
        (json-append! ht "tags" [tag]))))

  (def (tag-e stx)
    (with-loc stx
              (ast-case stx (%#begin
                             %#begin-syntax
                             %#define-values
                             %#define-syntax
                             %#extern
                             %#module)
                ((%#begin expr ...)
                 (for-each tag-e #'(expr ...)))
                ((%#begin-syntax expr ...)
                 (parameterize ((current-expander-phi (fx1+ (current-expander-phi))))
                   (for-each tag-e #'(expr ...))))
                ((%#define-values (id ...) _)
                 (for-each tag-def (filter values #'(id ...))))
                ((%#define-syntax id _)
                 (tag-def #'id))
                ((%#extern decl ...)
                 (for-each tag-decl #'(decl ...)))
                ((%#module id expr ...)
                 (let ((eid (binding-id (resolve-identifier #'id)))
                       (ctx (syntax-local-e #'id)))
                   ;; this only tags bindings if they are exported by the parent
                   ;; module; this works well for prelude-style module structures
                   ;; but doesn't tag bindings reachable because the module itself
                   ;; is exported
                   ;; TODO if the module is exported, add module's exports to the
                   ;;      tag table
                   (parameterize ((current-expander-context ctx))
                     (tag-name! eid)
                     (for-each tag-e #'(expr ...)))))
                (_ (void)))))

  (def (tag-def id)
    (with-loc id
              (tag-name! (binding-id (resolve-identifier id)))))

  (def (tag-decl decl)
    (ast-case decl ()
      ((id eid)
       (with-loc #'id
                 (tag-name! (stx-e #'eid))))))

  (def (tag-name! eid)
    (alet* ((xtab-res (hash-get xtab eid)))
      (with ([name module-name] xtab-res)
        (tag! module-name eid name))))

  (check-type hash-table? ht)
  (let (stx (module-context-code ctx))
    ;; also tag the module id itself with library prefix
    (let* ((id (expander-context-id ctx))
           (gid (make-symbol ":" id)))
      (parameterize ((current-expander-context ctx)
                     (current-expander-phi 0))
        (tag-e stx))
      ht)))

(def (try-import-module filename)
  (try
   (import-module filename)
   (catch (e) #f)))

(def (tag-srcfile srcfile into: (ht (make-hash-table)))
  "Returns a list of tags.  is:
   '(ID (KEY . ROOT-MODULE) LOCATION).
    where MODULES-EXPORTED is a list of modules where
    the corresponding symbol is exported from. LOCATION
    is a `locat' structure which contains the location infomation."

  (cond
   ((try-import-module srcfile)
    => (lambda (ctx)
         ;; binding-id -> [export-name ...]
         (let ((xtab (make-hash-table-eq))
               (xports (gx#module-context-export ctx)))
           (for-each
             (lambda (xport)
               (let* ((bind (core-resolve-module-export xport))
                      (name (module-export-name xport))
                      (export-root-module (resolve-module-export-root-module xport ctx)))
                 (hash-put! xtab (binding-id bind)
                            [name export-root-module])))
             xports)
           (module-tags ctx xtab into: ht)
           ht)))
   (else #f)))

(def (tag-directory dirname into: (ht (make-hash-table)))
  (let* ((dirname (path-normalize dirname))
         (files (sort (directory-files dirname) string<?))
         (result '()))
    (for-each
      (lambda (file)
        (let ((path (path-expand file dirname)))
          (when (or (file-directory? path)
                    (pregexp-match "[^ssxi].ss$" path))
            (tag-input path into: ht))))
      files))
  ht)

(def (tag-input input into: (ht (make-hash-table)))
  (logg input)
  (let (input (path-normalize input))
    (if (file-exists? input)
      (if (file-directory? input)
        (tag-directory input into: ht)
        (tag-srcfile input into: ht))
      (error "No such file or directory" input)))
  ht)

(def (tag-lookup key (table (current-tags-table)))
  (let (r (filter-map
           (lambda (tag)
             (let ((ckey (json-get tag "key")))
               (if (equal? ckey key)
                 (begin (json-refs-expand! table tag)
                        tag)
                 #f)))
           (json-get table "tags")))
    (if (= 1 (length r))
      (car r)
      (error "tag-lookup: got multiple results! " r))))

(def (tag-search key (table (current-tags-table)))
  (filter-map (lambda (tag)
                (let ((ckey (json-get tag "key")))
                  (if (string-contains ckey key)
                    (begin (json-refs-expand! table tag)
                           tag)
                    #f)))
              (json-get table "tags")))

(def (tag-search-regexp pat (table (current-tags-table)))
  (filter-map (lambda (tag)
                (let ((ckey (json-get tag "key")))
                  (if (pregexp-match pat ckey)
                    (begin (json-refs-expand! table tag)
                           tag)
                    #f)))
              (json-get table "tags")))

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
                                    (display-exception e)))))))

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
          (if (equal? file (!tag-worker.file cact))
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
              (displayln "workers")
              (!!value workers k)
              (lp))
             ((!tag-table.insert! inputs tagfile)
              (let (act (find-worker tagfile workers))
                (if act
                  ;; Search in worker actors.
                  (for-each (cut !!tag-worker.put! act <>) inputs)
                  ;; Make new actor and add to registry.
                  (let (act (spawn tag-file-worker tagfile))
                    (logg act)
                    (for-each (cut !!tag-worker.put! act <>) inputs)
                    (save-tag-file! index-file tagfile)
                    (set! tag-files [tagfile . tag-files])
                    (set! workers [act . workers]))))
              (lp))
             ((!tag-table.stop!)
              (displayln "tags-index thread stopped: " (current-thread))))
         (catch (exception? e) (begin (newline)
                                      (display "Actor error-tags-index: ")
                                      (display (current-thread))
                                      (newline)
                                      (display-exception e))))))))
