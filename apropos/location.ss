(import (only-in :std/sort sort)
        (only-in :clan/utils/base file-directory?)
        (only-in :gerbil/gambit/ports write-string force-output)
        :std/pregexp
        :std/srfi/1
        :std/srfi/13
        (only-in :gerbil/expander
                 core-resolve-module-export
                 binding-id
                 module-export-name
                 module-context-export)
        (only-in :gerbil/tools/gxtags
                 try-import-module
                 module-tags)
        "utils")

(export #t)

(def (get-tags-input input)
  (if (file-exists? input)
    (if (file-directory? input)
      (get-tags-directory input)
      (get-tags-source-file input))
    (error "No such file or directory" input)))

(def (get-tags-directory dirname)
  (let (files (sort (directory-files dirname) string<?))
    (for-each
      (lambda (file)
        (let ((path (path-expand file dirname)))
          (when (or (file-directory? path)
                    (member (path-extension path) '(".ss" ".ssi")))
            (get-tags-input path))))
      files)))

(def (get-tags-source-file filename)
  (cond
   ((try-import-module filename)
    => (lambda (ctx)
         (let (xtab (make-hash-table-eq)) ; binding-id -> [export-name ...]
           (for-each
             (lambda (xport)
               (let (bind (core-resolve-module-export xport))
                 (hash-update! xtab (binding-id bind)
                               (cut cons (module-export-name xport) <>)
                               [])))
             (module-context-export ctx))
           (cdr (module-tags ctx xtab)))))
   (else #f)))

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

(def (write-tags-input input)
  (if (file-exists? input)
    (if (file-directory? input)
      (write-tags-directory input)
      (write-tags-source-file input))
    (error "No such file or directory" input)))

(def (write-tags-directory dirname)
  (let (files (sort (directory-files dirname) string<?))
    (for-each
      (lambda (file)
        (let ((path (path-expand file dirname)))
          (when (or (file-directory? path)
                    (member (path-extension path) '(".ss")))
            (write-tags-input path))))
      files)))

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

(def (write-tags-source-file filename)
  (def (write-locat locat (port (current-output-port)))
    (let* ((path (locat-path locat))
           (position (locat-position locat)))
      (write position port)))

  (def (write-tag tag (out (current-output-port)))
    (with ([key id locat] tag)
      (write id out)
      (write-char #\, out)
      (write-locat locat out)
      (newline out)))

  (def (make-nstag-path ns)
    (string-append gtagspath (string-trim ns #\:)))

  (let* ((grouped-tags (group-by (lambda (tag) (id->ns (car tag)))
                         (get-tags-source-file filename))))
    (for-each (match <>
                ([ns . tags]
                 (let (path (make-nstag-path ns))
                   (create-directory* (path-directory path))
                   (call-with-output-file [path: (path-normalize path) append: #t]
                     (lambda (out)
                       (write-string (string-append "***," filename) out)
                       (newline out)
                       (for-each (cut write-tag <> out) tags)
                       (force-output out)))))
                (else #f))
              grouped-tags)))

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
