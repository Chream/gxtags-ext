(import (only-in :std/sort sort)
        (only-in :gerbil/gambit/ports write-string force-output read-all-as-string)
        (only-in :std/misc/ports read-file-string)
        (only-in :std/misc/string string-trim-prefix)
        :std/text/json
        :std/pregexp
        :std/srfi/1
        :std/srfi/13
        :std/format
        :std/misc/repr
        (only-in :gerbil/expander
                 core-resolve-module-export
                 binding-id
                 module-export-name
                 module-context-export)
        (only-in :gerbil/tools/gxtags
                 try-import-module
                 module-tags)

        (only-in :clan/utils/files clobber-file maybe-replace-file)
        (only-in :clan/utils/base if-let nest)
        (only-in :clan/utils/hash hash-ensure-ref)
        "utils")

(export #t)

(def gtagspath (path-normalize "~/.gerbil/tags/"))

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

(def (make-tag-table module)
  (let (ht (make-hash-table))
    (hash-put! ht "module" module)
    (hash-put! ht "files" (make-hash-table))
    ht))

(def (put-tags-srcfile! filename)

  (def (put-tag! ht filename key locat)
    (let* ((position (if (locat? locat)
                       (filepos-line (locat-position locat))
                       locat))
           (files-table (hash-get ht "files"))
           (tags-table (hash-ensure-ref files-table
                                        filename
                                        make-hash-table)))
      (hash-ensure-ref tags-table key (lambda () position))))

  (def (update-json json path module-tags)
    (nest
     (let (json (string->json-object (json-object->string json))))
     (let lp ((tags module-tags))
       (if (or (null? tags) (not tags))
         json
         (let ((tag (car tags))
               (rest (cdr tags)))
           (match tag
             ([_ [key . root-module] locat]
              (put-tag! json path (ensure-string key) locat))
             (else
              (error "whyy.")))
           (lp rest))))))

  (def (put-tags filename tags)
  "Takes a `list' of tags, each entry of the form:
   '(ID (KEY . ROOT-MODULE) LOCAT)."

  (let (tags-by-module (group-by (lambda (tag) (cdadr tag)) tags))
    (for-each
      (lambda (module-spec)
        (let* ((module-name (car module-spec))
               (module-tags (cdr module-spec))
               (path (path-normalize (make-path module-name))))
          (parameterize ((json-symbolic-keys #f))
            (if (file-exists? path)
              (maybe-replace-file path
                                  (lambda (json)
                                    (let (new-json (update-json json filename module-tags))
                                      new-json))
                                  reader: read-json-equal
                                  writer: write-json)
              (let ((json-table (make-tag-table module-name)))
                (call-with-output-file [path: path create: #t]
                  (lambda (out)
                    (write-json (update-json json-table filename module-tags) out)
                    (force-output out))))))))
              tags-by-module)))

  (put-tags filename (read-tags-srcfile (path-normalize filename))))

(def (put-tags-directory! dirname)
  (let* ((dirname (path-normalize dirname))
         (files (sort (directory-files dirname) string<?))
        (result '()))
    (for-each
      (lambda (file)
        (let ((path (path-expand file dirname)))
          (when (or (file-directory? path)
                    (pregexp-match "[^ssxi].ss$" path))
            (put-tags! path))))
      files)))

(def (put-tags! input)
  (let (input (path-normalize input))
    (if (file-exists? input)
      (if (file-directory? input)
        (put-tags-directory! input)
        (put-tags-srcfile! input))
      (error "No such file or directory" input))))

(def (%tag-filter fn jtable)
  "FN takes FILENAME KEY VALUE PATH"
  (let (ht (make-hash-table))
    (hash-for-each
     (lambda (path tags)
       (hash-for-each
        (lambda (key tag-info)
          (when (fn key tag-info path)
            (let (attr-table (make-hash-table))
              (hash-put! attr-table "position"
                         (get-tag-position tag-info))
              (hash-put! attr-table "path" path)
              (hash-put! ht key attr-table))))
        tags))
     (hash-get jtable "files"))
    ht))

(def (lookup-tag-regexp-file pat file)
  (call-with-input-file file
    (lambda (in)
      (let (json (read-json-equal in))
        (%tag-filter (lambda (key . rest)
                      (pregexp-match pat key))
                    json)))))

(def (lookup-tag-regexp-directory pat dir)
  (let ((files (sort (directory-files dir) string<?)))
    (list->hash-table
     (append-map
      (lambda (f)
        (hash->list
         (lookup-tag-regexp pat (path-normalize
                                 (string-append dir
                                                "/"
                                                f)))))
      files))))

(def (lookup-tag-regexp pat (input gtagspath))
  (if (file-exists? input)
    (if (file-directory? input)
      (lookup-tag-regexp-directory pat input)
      (lookup-tag-regexp-file pat input))
    (error "No such file or directory" input)))

(def (lookup-tag-file key file)
  (call-with-input-file file
    (lambda (in)
      (let (json (read-json-equal in))
        (%tag-filter (lambda (ckey . rest)
                       (equal? key ckey))
                     json)))))

(def (lookup-tag-directory key dir)
  (let ((files (sort (directory-files dir) string<?)))
    (list->hash-table
     (append-map
      (lambda (f)
        (hash->list
         (lookup-tag key
                     (path-normalize
                      (string-append dir "/" f)))))
      files))))

(def (lookup-tag key (input gtagspath))
  (if (file-exists? input)
    (if (file-directory? input)
      (lookup-tag-directory key input)
      (lookup-tag-file key input))
    (error "No such file or directory" input)))

;; accessors

(def (get-tag-position tag-info)
  tag-info)

;; Utils

;; (def (invert-json-branch from to: (to (make-hash-table)))
;;   (let lp ((ht (make-hash-table)))
;;     (hash-for-each
;;      (lambda (cat v)
;;        (if (hash-table? v)
;;          (begin
;;            (invert-json-branch )
;;            )
;;          (begin
;;            (hash-put! ht "value" v)
;;            (hash-put! to cat ht))))))
;;   from
;;   to)

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

(def (make-path module)
  (string-append gtagspath
                 (pregexp-replace* "/" module
                                   "__")
                 ".json"))
