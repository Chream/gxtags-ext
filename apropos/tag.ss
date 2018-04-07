(import (only-in :std/sort sort)
        (only-in :gerbil/gambit/ports write-string force-output read-all-as-string)
        (only-in :std/misc/ports read-file-string)
        (only-in :std/misc/string string-trim-prefix)
        :std/text/json
        (only-in :std/pregexp pregexp-match)
        :std/srfi/1
        :std/srfi/13
        :std/format
        :std/misc/repr
        (only-in :gerbil/expander
                 core-resolve-module-export
                 binding-id
                 module-export-name
                 module-context-export)

        (only-in :clan/utils/files clobber-file maybe-replace-file)
        (only-in :clan/utils/base if-let nest !> !!>)
        (only-in :clan/utils/hash hash-ensure-ref)
        (only-in :clan/utils/json pretty-print-json)
        "utils"
        (only-in "gxtags-ext" try-import-module module-tags))

(export #t)

;; (def (main . args)
;;   (def gopt
;;     (getopt (flag 'append "-a"
;;                   help: "append to existing tag file")
;;             (option 'output "-o" default: "TAGS"
;;                     help: "explicit name of file for tag table")
;;             (flag 'help "-h" "--help"
;;                   help: "display help")
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
;;             (tag-index-reset!))
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

;; (def (run inputs tagfile append?)
;;   (def (expand-input-paths base inputs)
;;     (map (cut path-expand <> (path-directory base))
;;          inputs))

;;   (_gx#load-expander!)
;;   (make-tags (expand-input-paths tagfile inputs) tagfile append?))

;; (def current-tags-path
;;   (make-parameter #f))

;; ;; Tags implementation.

;; (def (make-tags inputs tagfile (append? #f))
;;   (let (tagfile (path-normalize tagfile))
;;     (logg tagfile)
;;     (call-with-output-file [path: tagfile append: append?]
;;       (lambda (output)
;;         (parameterize ((current-tags-path (path-normalize tagfile)))
;;           (for-each (cut tags-put! <> output) inputs))))))


(def tags-path
  (make-parameter (path-normalize "~/.gerbil/tags/tags.json")))
(def current-tags-table
  (make-parameter #f))

(def (read-tags-srcfile filename)
  "Returns a list of tags. Format is:
   '(ID (KEY . ROOT-MODULE) LOCATION).
    where MODULES-EXPORTED is a list of modules where
    the corresponding symbol is exported from. LOCATION
    is a `locat' structure which contains the location infomation."

  (cond
   ((try-import-module filename)
    => (lambda (ctx)
         ;; binding-id -> [export-name ...]
         (let ((xtab (make-hash-table-eq))
               (xports (gx#module-context-export ctx)))
           (for-each
             (lambda (xport)
               (let* ((bind (core-resolve-module-export xport))
                      (name (module-export-name xport))
                      (export-root-module (resolve-module-export-root-module xport ctx)))
                 (hash-update! xtab (binding-id bind)
                               (cut cons
                                    (cons name export-root-module)
                                    <>)
                               [])))
             xports)
           (let (res (cdr (module-tags ctx xtab)))
             res))))
   (else #f)))

(def (tag-put-location! module path key locat into: (ht make-hash-table))
  (let* ((file-table (!> ht
                         (cut hash-ensure-ref <> module make-hash-table)
                         (cut hash-ensure-ref <> "locations" make-hash-table)
                         (cut hash-ensure-ref <> path make-hash-table))))
    (hash-put! file-table key (if (locat? locat)
                                (filepos-line (locat-position locat))
                                locat))))

(def (tag-srcfile srcfile into: (ht (make-hash-table)))
  (let* ((srcfile (path-normalize srcfile))
         (tags (read-tags-srcfile srcfile)))
    (when tags
      (for-each
        (lambda (tag)
          (with ([_ [key . root-module] locat] tag)
            (tag-put-location! root-module srcfile key locat into: ht)))
        tags)))
  ht)

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

(def (%tag-filter fn jtable)
  "FN takes FILENAME KEY VALUE PATH.
   NOTE This function is important as it depends on the
   format of the tags. If it is changed this procedure
   needs rewriting."
  (let (ht (make-hash-table))
    (hash-for-each
     (lambda (path tags)
       (hash-for-each
        (lambda (key tag-info)
          (when (fn key tag-info path)
            (let (attr-table (make-hash-table))
              (hash-put! attr-table "position"
                         (tag-position tag-info))
              (hash-put! attr-table "path" path)
              (hash-put! ht key attr-table))))
        tags))
     (hash-get jtable "files"))
    ht))

(def (tag-search-file key file)
  (call-with-input-file file
    (lambda (in)
      (let (json (read-json-equal in))
        (%tag-filter (lambda (ckey . rest)
                       (string-contains ckey key))
                     json)))))

;; (def (tag-search-directory key dir)
;;   (let ((files (sort (directory-files dir) string<?)))
;;     (list->hash-table
;;      (append-map
;;       (lambda (f)
;;         (hash->list
;;          (tag-search key
;;                      (path-normalize
;;                       (string-append dir "/" f)))))
;;       files))))

;; (def (tag-search key (input gtagspath))
;;   (if (file-exists? input)
;;     (if (file-directory? input)
;;       (tag-search-directory key input)
;;       (tag-search-file key input))
;;     (error "No such file or directory" input)))

;; (def (tags-search-regexp-file pat file)
;;   (call-with-input-file file
;;     (lambda (in)
;;       (let (json (read-json-equal in))
;;         (%tag-filter (lambda (key . rest)
;;                        (pregexp-match pat key))
;;                      json)))))

;; (def (tags-search-regexp-directory pat dir)
;;   (let ((files (sort (directory-files dir) string<?)))
;;     (list->hash-table
;;      (append-map
;;       (lambda (f)
;;         (hash->list
;;          (tags-search-regexp pat
;;                             tags-dir: (path-normalize
;;                                        (string-append dir
;;                                                       "/"
;;                                                       f)))))
;;       files))))

;; (def (tags-search-regexp pat tags-dir: (tags-dir gtagspath))
;;   (if (file-exists? tags-dir)
;;     (if (file-directory? tags-dir)
;;       (tags-search-regexp-directory pat tags-dir)
;;       (tags-search-regexp-file pat tags-dir))
;;     (error "No such file or directory" tags-dir)))

;; (def (tag-lookup-file key file)
;;   (call-with-input-file file
;;     (lambda (in)
;;       (let (json (read-json-equal in))
;;         (%tag-filter (lambda (ckey . rest)
;;                        (equal? ckey key))
;;                      json)))))

;; (def (tag-lookup-directory key dir)
;;   (let ((files (sort (directory-files dir) string<?)))
;;     (list->hash-table
;;      (append-map
;;       (lambda (f)
;;         (hash->list
;;          (tag-lookup key
;;                      (path-normalize
;;                       (string-append dir "/" f)))))
;;       files))))

;; (def (tag-lookup key (input gtagspath))
;;   (if (file-exists? input)
;;     (if (file-directory? input)
;;       (tag-lookup-directory key input)
;;       (tag-lookup-file key input))
;;     (error "No such file or directory" input)))

;; ;; accessors

;; (def (tag-position tag-info)
;;   tag-info)

;; Utils

(def (group-by proc list)
  (if list
    (let ((alist '()))
      (for-each (lambda (e)
                  (let* ((key (proc e))
                         (res (assoc key alist)))
                    (if res
                      (set-cdr! res (cons e (cdr res)))
                      (set! alist (cons [key e] alist)))))
                list)
      alist)
    '()))


;; ;; (def (invert-json-branch from to: (to (make-hash-table)))
;; ;;   (let lp ((ht (make-hash-table)))
;; ;;     (hash-for-each
;; ;;      (lambda (cat v)
;; ;;        (if (hash-table? v)
;; ;;          (begin
;; ;;            (invert-json-branch )
;; ;;            )
;; ;;          (begin
;; ;;            (hash-put! ht "value" v)
;; ;;            (hash-put! to cat ht))))))
;; ;;   from
;; ;;   to)
