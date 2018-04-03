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
        (only-in :clan/utils/base if-let)
        (only-in :clan/utils/hash hash-ensure-ref)
        "utils")

(export #t)

(def (resolve-module-export-root-module xport (ctx (gx#current-expander-context)))
  "Takes XPORT, a `module-export' and returns
   the module name, a `string', in which it is defined."
  (let ((xport-key (gx#module-export-key xport))
        (ctx-table (expander-context-table-all ctx)))
    (let lp ((binding (hash-get ctx-table xport-key)))
      (let (context-id (cond ((gx#import-binding? binding)
                              (if (not (gx#import-binding?
                                        (gx#import-binding-e binding)))
                                (gx#expander-context-id
                                 (gx#import-binding-context binding))
                                (lp (gx#import-binding-e binding))))
                             ((gx#module-binding? binding)
                              (gx#expander-context-id
                               (gx#module-binding-context binding)))
                             (else (gx#expander-context-id ctx))))
        (cond ((symbol? context-id) (symbol->string context-id))
              ((string? context-id) context-id)
              (else (error "cant resolve.")))))))

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
               (let* ((bind (core-resolve-module-export xport)))
                 (hash-update! xtab (binding-id bind)
                            (cut cons
                                 (cons (module-export-name xport)
                                       (resolve-module-export-root-module xport ctx))
                                 <>)
                            [])))
             xports)
           (cdr (module-tags ctx xtab)))))
   (else #f)))

(def (--tags-check tags)
  (for-each (lambda (x) (if (eq? (car x) (caadr x))
                          (error "not equal. ~S != ~S." (car x) (caadr x))))
            (cdr tags)))

(def (write-symbol-tag key position (out (current-output-port)))
  (write key out)
  (write-char #\, out)
  (write position out)
  (newline out))

(def (make-tag-table module)
  (let (ht (make-hash-table))
    (hash-put! ht "module" module)
    (hash-put! ht "files" (make-hash-table))
    ht))

(def (register-tag! ht filename key locat)
  (let* ((position (if (number? locat)
                     locat
                     (filepos-line (locat-position locat))))
         (files-table (hash-get ht "files"))
         (tags-table (hash-ensure-ref files-table filename make-hash-table)))
    (hash-ensure-ref tags-table key (lambda () position))))

(def (register-tags filename tags)
  "Takes a `list' of tags, each entry of the form '(ID (KEY . ROOT-MODULE) LOCAT)."

  (def (make-path module)
    (string-append gtagspath
                   (pregexp-replace* "/" module
                                     "__")
                   ".json"))

  (def (update-json json path module-tags)
    (let (json (string->json-object (json-object->string json)))
      (let lp ((tags module-tags))
        (if (or (null? tags) (not tags))
          json
          (let ((tag (car tags))
                (rest (cdr tags)))
            (match tag
              ([_ [key . root-module] locat]
               (register-tag! json path (ensure-string key) locat))
              (else
               (error "whyy.")))
            (lp rest))))))

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
                                  reader: read-json
                                  writer: write-json)
              (let ((json-table (make-tag-table module-name)))
                (call-with-output-file [path: path create: #t]
                  (lambda (out)
                    (write-json (update-json json-table filename module-tags) out)
                    (force-output out))))))))
              tags-by-module)))

(def (tag-srcfile filename)
  (register-tags filename (read-tags-srcfile (path-normalize filename))))

(def (tag-directory dirname)
  (let* ((dirname (path-normalize dirname))
         (files (sort (directory-files dirname) string<?))
        (result '()))
    (for-each
      (lambda (file)
        (let ((path (path-expand file dirname)))
          (when (or (file-directory? path)
                    (pregexp-match "[^ssxi].ss$" path))
            (tag-input path))))
      files)))

(def (tag-input input)
  (let (input (path-normalize input))
    (if (file-exists? input)
      (if (file-directory? input)
        (tag-directory input)
        (tag-srcfile input))
      (error "No such file or directory" input))))

(def (lookup-location-regexp pat (ns #f))
  (let* ((tagfile-path (string-append gtagspath ns))
         (result (if (and (file-exists? tagfile-path)
                          ns)
                   (lookup-location-regexp-file pat tagfile-path)
                   #f)))
    (if result
      (lookup-location-regexp-input pat gtagspath)
      result)))

(def (lookup-location-symbol sym (ns #f))
  (let (matches (lookup-location-regexp
                 (string-append "^"
                                (symbol->string sym)
                                "$")
                 ns))
    (cond ((null? matches)        #f)
          ((= (length matches) 1) (cdar matches))
          (else (error "found more than one match." matches)))))

(def (lookup-location-id id)
  (let ((ns (id->ns id))
        (key (id->key id)))
    (lookup-location-symbol (if (string? key) (string->symbol key) key) ns)))

(def (lookup-location-regexp-input pat input (ns #f))
  (if (file-exists? input)
    (if (file-directory? input)
      (lookup-location-regexp-directory pat input ns)
      (lookup-location-regexp-file pat input ns))
    (error "No such file or directory" input)))

(def (lookup-location-regexp-file pat file (ns #f))
  (def (entry-name e)
    (car e))
  (def (entry-value e)
    (cadr e))
  (call-with-input-file file
    (lambda (in)
      (let lp ((current-src-path #f)
               (current-line (read-line in))
               (result (list)))
        (if (eof-object? current-line)
          (reverse result)
          (let* ((current-entry (string-split current-line #\,))
                 (name  (entry-name current-entry))
                 (value (entry-value current-entry)))
            (cond ((equal? name "***")
                   (lp value
                       (read-line in)
                       result))
                  ((pregexp-match pat name)
                   (lp current-src-path
                       (read-line in)
                       (cons (cons name
                                   (make-locat (path-normalize current-src-path)
                                               (string->number value)))
                             result)))
                  (else
                   (lp current-src-path
                       (read-line in)
                       result)))))))))

(def (lookup-location-regexp-directory pat dir (ns #f))
  (let ((files (sort (directory-files dir) string<?)))
    (append-map (lambda (f)
                  (lookup-location-regexp-input pat
                                                (path-normalize (string-append dir "/" f))))
                files)))


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

(def gtagspath (path-normalize "~/.gerbil/tags/"))

(def (id->ns id)
  (let (r (string-split (symbol->string id) #\#))
    (if (< 1 (length r))
      (car (string-split (car r) #\[))
      "gerbil/core")))

(def (id->key id)
  (let (r (string-split (symbol->string id) #\#))
    (if (< 1 (length r))
      (cadr r)
      (cadr r))))
