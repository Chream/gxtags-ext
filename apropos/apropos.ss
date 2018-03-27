;;; -*- Gerbil -*-
;;; Christopher Eames (chream) <chream-gmx.com>
;;; gerbil/apropos/apropos/apropos.ss

prelude: :<core>
(import :std/pregexp
        :std/srfi/1
        :std/srfi/13
        :std/misc/ports
        :std/sort
        (only-in :std/misc/string string-trim-suffix)
        (only-in :std/misc/list push!)

        :std/misc/rtd

        (only-in :clan/utils/base compose if-let)
        (only-in :clan/utils/filesystem find-files)
        (only-in :clan/utils/hash hash-filter)
        (only-in :clan/utils/date sleep)


        :gerbil/expander
        :gerbil/core
        <expander-runtime>

        :gerbil/gambit/hash
        :gerbil/gambit/ports
        :gerbil/gambit/threads

        (phi: -1 <MOP>)

        :std/parser

        :std/actor

        :gerbil/tools/gxtags)

(export #t )

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

(def (get-tags-source-file filename (output #f))
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
   (else
    (add-failure filename)
    #f)))

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

(def (write-tags-source-file filename)
  (def (key->ns sym)
    (let (r (string-split (symbol->string sym) #\#))
      (if (< 1 (length r))
        (car (string-split (car r) #\[))
        "null")))

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

  (let* ((grouped-tags (group-by (lambda (tag) (key->ns (car tag)))
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
  (let (result (when ns (lookup-location-regexp-file pat
                         (path-normalize (string-append gtagspath ns)))))
    (if (void? result)
      (lookup-location-regexp-input pat gtagspath)
      result)))

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


;;;;

(def (get-tags mod-ctx)
  (let ((path (gx#module-context-path mod-ctx)))
    (get-tags-source-file path)))

(def (my-stream foo)
  (call-with-input-file (path-normalize "~/repos/gerbil/apropos/apropos/apropos.ss")
    (lambda (in)
      (let ((cs (make-char-stream in)))
        (char-stream-getc cs)))))

(defrules foo ()
  ((_ bar)
   (def (foobar)
     4)))

(foo 1)

(defstruct bar (x y z))

;; utils

(def (slot-nr obj)
  (type-descriptor-slots (object-type obj)))

(def (slot-names obj)
  (type-descriptor-plist (object-type obj)))

(def (object->list obj)
  (cond ((class-instance? obj)
         (class->list obj))
        ((struct-instance? obj #f)
         (struct->list obj))))

(def (object-slot-info obj)
  (cons (object-type obj)
        (map (cut cons <> <>)
             (append-map cdr (slot-names obj))
             ;; Works for class as well.
             ;; will return names though.
             (cdr (struct->list obj)))))

;; binding info

(def (binding-loc self)
  (def (module-context-loc mod-ctx)
    (let* ((tags (get-tags mod-ctx)))
      (let lp ((tags tags))
        (if (or (not tags) (null? tags))
          #f
          (let* ((tag (car tags))
                 (rest (cdr tags))
                 (tag-id (car tag))
                 (loc (caddr tag))
                 (b-id (gx#binding-id self)))
            (if (eq? tag-id b-id)
              loc
              (lp rest)))))))
  (match self
    ((import-binding id key phi e ctx weak?)
     (module-context-loc ctx))
    ((alias-binding id key phi e)
     (object-slot-info e))
    ((syntax-binding id key phi e)
     (cond ((gx#user-expander? e)
            (let ((u-ctx (gx#user-expander-context e)))
              (cond ((module-context? u-ctx)
                     (module-context-loc u-ctx))
                    (else #f))))
           (else #f)))
    ((module-binding id key phi ctx)
     (module-context-loc ctx))
    ((extern-binding id key phi)
     "external")
    ((top-binding id key phi)
     "top")
    (else (if (not self)
            #f
            (error "unknown binding: " self)))))

(def (binding-loc-test-filter)
  (def (gather (conj #f))
    (hash-filter (expander-context-table-all)
                 (lambda (k v)
                   (if conj
                     (not (binding-loc v))
                     (binding-loc v)))))
  (let ((true (gather #f))
        (false (gather #t)))
    (list true false len-true: (hash-length true) len-false: (hash-length false))))

(def (binding-loc-test-map)
  (def (gather (conj #f))
    (hash-filter (expander-context-table-all)
                 (lambda (k v)
                   (let ((loc (binding-loc v)))
                     (if (and loc (not conj))
                       (binding-loc v)
                       v)))))
  (let ((true (gather #f))
        (false (gather #t)))
    (list true false len-true: (length true) len-false: (length false))))

(def (binding-location b)
  (def (get-context b)
    (cond ((gx#syntax-binding? b)
           (let ((e (gx#syntax-binding-e b)))
             (cond ((gx#user-expander? e)
                    (gx#user-expander-context e))
                   (else (error "can find context.")))))
          ((gx#module-binding? b)
           (gx#module-binding-context b))
          (else (error "Could not find module context. "
                  b))))

  (cond ((gx#module-binding? b)
         (let* ((ctx (gx#module-binding-context b))
                (tags (get-tags ctx)))
           (let lp ((tags tags))
             (if (or (not tags) (null? tags))
               #f
               (let* ((tag (car tags))
                      (rest (cdr tags))
                      (tag-id (car tag))
                      (loc (caddr tag))
                      (b-id (gx#binding-id b)))
                 (if (eq? tag-id b-id)
                   loc
                   (lp rest)))))))
        ((gx#syntax-binding? b)
         (let ((e (gx#syntax-binding-e b)))
           (cond ((gx#user-expander? e)
                  (gx#user-expander-context e))
                 ((gx#macro-expander? e))
                 (error "unknown e: " e))))
        (else (error "binding-location. unknown binding type: "
                b))))

(defstruct baf (x y))

(def (syntax-binding-info b)
    (let* ((e (gx#syntax-binding-e b))
           (info-object (gx#expander-e e))
           (type-sym
            (cond ((procedure? info-object)
                   'macro)
                  ((runtime-struct-info? info-object)
                   'struct)
                  ((runtime-class-info? info-object)
                   'class)
                  (else #f)))
           (slots
            (cond ((runtime-struct-info? info-object)
                   (runtime-struct-fields (runtime-type-exhibitor info-object)))
                  ((runtime-class-info? info-object)
                   (runtime-class-slots (runtime-type-exhibitor info-object)))
                  (else #f)))
           (ns
            (cond ((user-expander? e)
                   (gx#module-context-ns (gx#user-expander-context e)))
                  ((macro-expander? e) "macroexp")
                  (else (error "unknown expander." e))))
           (loc
            (cond ((runtime-type-info? info-object)
                   ;; FIXME Should find propoer accessor.
                   (##vector-ref (runtime-type-identifier info-object) 2))
                  ((procedure? info-object)
                   ;; FIXME This does not work.
                   (##procedure-locat info-object))
                  (else "Not available for." info-object)))
           (phi (gx#binding-phi b)))
      (list key: (binding-id b)
            binding: b
            type: type-sym
            phi: phi
            slots: slots
            info-object: info-object
            ns: ns
            location: loc)))

(def (module-binding-info b)
  (let* ((module-context (module-binding-context b))
         (ns (gx#module-context-ns module-context))
         (proc/val  (unless (uninterned-symbol? (gx#binding-id b))
                      (eval (gx#binding-id b))))
         (type (if (procedure? proc/val)
                 'procedure
                 'variable))
         (arg-list (if (eq? type 'procedure)
                     (let (dproc (##decompile proc/val))
                       (if (list? dproc)
                         (cadr dproc)
                         #f))
                     #f))
         ;; (location (binding-location b))
         (phi (gx#binding-phi b)))
    (list (binding-id b)
          binding: b
          type: type
          phi: phi
          ns: ns
          arg-list: arg-list
          location: #f)))

(def (alias-binding-info b)
  (list (binding-id b)
        binding: b))

(def (binding-info binding)
  (cond ((gx#syntax-binding? binding)
         ;; Code for macro bindings
         (syntax-binding-info binding))
        ((gx#extern-binding? binding)
         ;; code for external bindings
         (list binding 'extern-binding))
        ((gx#module-binding? binding)
         ;; code for module bindings
         (module-binding-info binding))
        ((gx#alias-binding? binding)
         (alias-binding-info binding))
        (else (error "unknown binding type: "
                binding))))

(def (expander-context-table-regexp pat (ctx (gx#current-expander-context)))
  "This matches PAT for each bound identifier in the
   current context. Return a table of '(key . #<import-binding>.)"

  (def (ensure-string elt)
    (if (symbol? elt)
      (symbol->string elt)
      elt))

  (let ((bindings (gx#expander-context-table ctx)))
    (hash-filter bindings
                 (lambda (key binding)
                   (pregexp-match (ensure-string pat)
                                  (symbol->string key))))))

(def (hash-empty? table)
  (if (zero? (hash-length table))
    #t
    #f))

(def (expander-context-table-all)
  (let lp ((ctx (current-expander-context))
           (table (make-hash-table))
           (going-up? #t))
    (let ((t (gx#expander-context-table ctx)))
      (let ((table-empty? (hash-empty? t)))
        (cond ((and table-empty? (not going-up?))
               table)
              ((and table-empty? going-up?)
               (lp (gx#core-context-shift (gx#current-expander-context)  1) table #f))
              (going-up?
               (lp (gx#core-context-shift ctx 1) (hash-merge! t table) going-up?))
              ((not going-up?)
               (lp (gx#core-context-shift ctx -1) (hash-merge! t table) going-up?))
              (else (error "wtf?")))))))

(def (expander-context-table-phi phi)
  (gx#expander-context-table
   (gx#core-context-shift (gx#current-expander-context) phi)))

(def (expander-context-table-regex pat (phi all:))
  (let ((ctx-table (if (eq? phi all:)
                     (expander-context-table-all)
                     (expander-context-table-phi phi))))
    (hash-filter ctx-table
                 (lambda (k v)
                   (pregexp-match pat (symbol->string k))))))

(def (import-binding-e-rec binding)
  (cond ((gx#import-binding? binding)
         (import-binding-e-rec (gx#import-binding-e binding)))
        (else binding)))

(def (apr-single sym)
  (let ((binding ((gx#resolve-identifier sym))))
    (binding-info binding)))

(def (apr pat)
  "The function will match the pattern to any bound symbols
   in the current context and return info on the (if so) bound
   value."
  (let ((table (expander-context-table-regex pat all:)))
    (hash-for-each (lambda (k v)
                     (let ((import-e (import-binding-e-rec v)))
                       (hash-put! table k (binding-info import-e))))
                   table)
    table))


(def failures '())

(def (add-failure obj)
  (set! failures (cons obj failures)))

(def (clear)
  (set! failures '()))


;; location

(def (make-locat path position)
  (##make-locat (path->container path) position))

(def (locat? x)
  (##locat? x))

(def (locat-path locat)
  (##locat-container locat))

(def (locat-position locat)
  (##locat-position locat))

(def (path->container-hook-set! x)
  (##path->container-hook-set! x))

(def (path->container path)
  (##path->container path))

(def (container->path-hook-set! x)
  (##container->path-hook-set! x))

(def (container->path container)
  (##container->path container))

(def (make-filepos line col off)
  (##make-filepos line col off))

(def (filepos-line filepos)
  (##filepos-line filepos))

(def (filepos-col filepos)
  (##filepos-col filepos))

(def (pappend-map f . ls)
  (append-map thread-join! (apply map (lambda es (apply spawn f es)) ls)))
