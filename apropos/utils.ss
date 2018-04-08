(import :gerbil/expander
        :std/srfi/1
        (only-in :std/srfi/13 string-contains)
        :std/format
        :std/misc/rtd
        :std/misc/repr
        (only-in :std/pregexp pregexp-match)
        (only-in :std/text/json read-json json-symbolic-keys string->json-object json-object->string)
        (only-in :gerbil/gambit/threads thread-join! spawn)
        (only-in :clan/utils/hash hash-filter)
        (only-in :clan/utils/base nest)
        (only-in :clan/utils/json pretty-print-json))

(export #t)

;; files

(def (file-directory? path)
  (eq? (file-type path) 'directory))

;; location

(def (make-locat path position)
  (##make-locat (path->container path) position))

(def (locat? x)
  (##locat? x))

(def (locat-path locat)
  (if (locat? locat)
    (##locat-container locat)
    (error "Cant get path. Not of type locat" locat)))

(def (locat-position locat)
  (if (locat? locat)
    (##locat-position locat)
    (error "Not locat object: " locat)))

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

;; paralell map

(def (pappend-map f . ls)
  (append-map thread-join! (apply map (lambda es (apply spawn f es)) ls)))

;; Type checking.

(def (check-type specifier? obj)
  (unless (specifier? obj)
    (error (format "Object ~S does not fulfill ~S"
                   obj specifier?))))

;; Object inspection

(def (slot-nr obj)
  (type-descriptor-slots (object-type obj)))

(def (slot-names obj)
  (type-descriptor-plist (object-type obj)))

(def (object->list obj)
  (cond ((class-instance? obj)
         (class->list obj))
        ((struct-instance? obj #f)
         (struct->list obj))))

(def (object-info obj)
  (cons (object-type obj)
        (map (cut cons <> <>)
             (append-map cdr (slot-names obj))
             ;; Works for class as well.
             ;; will not return names though.
             (cdr (struct->list obj)))))

;; Procedure inspection

(def (procedure-name proc)
  (##procedure-name proc))

;; Hash tables

(def (hash-add! ht key value)
  (let ((present? (hash-get ht key)))
    (if present?
      (error (format "In hash-add! Hash key allready present key: ~S\n Old-val: ~S\nNew-val: ~S" key (pp present?) (pp value)))
      (hash-put! ht key value))))

(def (hash-table-length-> ht max-length)
  (call/cc
    (lambda (ret)
      (let (length 0)
        (hash-for-each (lambda (k v)
                         (when (> length max-length)
                           (ret #f)))
                       ht)
        (ret #t)))))

(def (hash-table-length-< ht max-length)
  (call/cc
    (lambda (ret)
      (let (length 0)
        (hash-for-each (lambda (k v)
                         (set! length (fx1+ length))
                         (when (> length max-length)
                           (ret #f)))
                       ht)
        (ret #t)))))

(def (hash-empty? ht)
  (hash-table-length-< ht 0))

(def (hash-single? ht)
  (hash-table-length-< ht 1))

(def (hash-first-value ht)
  (call/cc
    (lambda (ret)
      (hash-for-each (lambda (k v)
                       (ret v))
                     ht)
      (ret #f))))

;; Expander context

(def (expander-context-table-all (ctx (gx#current-expander-context)))
  "Finds all bindings for all phi contexts.
   Returns `table' of (key . <binding>)."
  (let lp ((ctx-1 ctx)
           (table (make-hash-table))
           (going-up? #t))
    (let* ((t (gx#expander-context-table ctx-1))
           (table-empty? (hash-empty? t)))
      (cond ((and table-empty? (not going-up?))
             table)
            ((and table-empty? going-up?)
             ;; Resets to phi=0 and goes down.
             (lp (gx#core-context-shift ctx  1)
                 table
                 #f))
            (going-up?
             (lp (gx#core-context-shift ctx-1 1)
                 (hash-merge! t table)
                 going-up?))
            ((not going-up?)
             (lp (gx#core-context-shift ctx-1 -1)
                 (hash-merge! t table)
                 going-up?))
            (else (error "wtf?"))))))

(def (expander-context-table-phi phi)
  "Finds all bindings for given PHI context.
   Returns `table' of (key . <binding>)."
  (gx#expander-context-table
   (gx#core-context-shift (gx#current-expander-context) phi)))

(def (expander-context-table-search-generator eq-fn?)
  "This matches KEY for each bound identifier in the
   current context. Return a `table' of (key . #<import-binding>.)"
  (lambda (key ctx phi)
    (let ((ctx-table (if (eq? phi all:)
                       (expander-context-table-all ctx)
                       (expander-context-table-phi phi))))
      (hash-filter ctx-table
                   (lambda (k v)
                     ;; (logg key)
                     ;; (logg (symbol->string k))
                     (eq-fn? key (symbol->string k)))))))

(def (expander-context-table-search key
                                    (ctx (current-expander-context))
                                    (phi (current-expander-phi)))
  ((expander-context-table-search-generator (lambda (k v) (string-contains v k))) key ctx phi))

(def (expander-context-table-search-regexp key
                                    (ctx (current-expander-context))
                                    (phi (current-expander-phi)))
  ((expander-context-table-search-generator pregexp-match) key ctx phi))

(def (expander-context-table-lookup key
                                    (ctx (current-expander-context))
                                    (phi (current-expander-phi)))
  (let ((r ((expander-context-table-search-generator equal?)  key ctx phi)))
    (if (hash-single? r)
      (hash-first-value r)
      (error "expander context lookup returned multiple values:" r))))

(def (all-modules-exported xport (ctx-table (expander-context-table-all)))
  (let (xport-key (gx#module-export-key xport))
    (let lp ((binding (hash-get ctx-table xport-key))
             (modules (list (string->symbol
                             (gx#module-context-ns
                              (gx#module-export-context xport))))))
      (if (gx#import-binding? binding)
        (let* ((ctx (gx#import-binding-context binding))
               (e (gx#import-binding-e binding))
               (mod-id (gx#expander-context-id ctx)))
          (lp e (cons mod-id modules)))
        modules))))

(def (resolve-module-export-root-module xport (ctx (gx#current-expander-context)))
  "Takes XPORT, a `module-export' and returns
   the module name, a `string', in which it is defined."
  (nest
   (let ((xport-key (gx#module-export-key xport))
         (ctx-table (expander-context-table-all ctx))))
   (let lp ((binding (hash-get ctx-table xport-key))))
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
           (else (error "cant resolve."))))))

;; text

(def (ensure-string key)
  (cond ((symbol? key)
         (symbol->string key))
        ((list? key)
         (format "~s" key))
        ((pair? key)
         (car key))
        ((string? key) key)
        (else (error "wtf?" key))))

(def (ensure-symbol key)
  (cond ((symbol? key) => values)
        (else (error "wtf? " key))))

;; looping

(define-syntax dolist
  (syntax-rules ()
    ((_ (e lis) exprs ...)
     (for-each (lambda (e) exprs ...) lis))))

;; printing

(define-syntax logg
  (syntax-rules ()
    ((_ var)
     (begin
       (display (format "~S == " 'var))
       (prn var)))))

;; json

(def (read-json-equal in)
  (parameterize ((json-symbolic-keys #f))
    (read-json in)))

(def (copy-json obj)
  (string->json-object (json-object->string obj)))

(def (pp json)
  (pretty-print-json json))

(def (json-input-fn-generator get-fn set-fn! force-set-fn! constructor-fn)
  (lambda (table . entry-spec)
    (logg entry-spec)
    (let lp! ((table-1 table)
              (entry-spec-1 entry-spec))
      (cond ((null? entry-spec-1)
             (error "Invalid number of keys. key-length: : " (length entry-spec))
             ((= 1 (length entry-spec-1))
              (set-fn! table-1 (car entry-spec-1) (constructor-fn))))
            ((= 2 (length entry-spec-1))
             (set-fn! table-1 (car entry-spec-1) (cadr entry-spec-1)))
            (else
             (let* ((key (car entry-spec-1))
                    (entry (get-fn table-1 key)))
               (cond ((hash-table? entry)
                      (lp! entry (cdr entry-spec-1)))
                     (entry
                      (let (ht-1 (make-hash-table))
                        (set-fn! ht-1 "%%old-value" entry)
                        (force-set-fn! table-1 key ht-1)
                        (lp! ht-1 (cdr entry-spec-1))))
                     (else
                      (let (ht-2 (make-hash-table))
                        (set-fn! table-1 key ht-2)
                        (lp! ht-2 (cdr entry-spec-1)))))))))))

(def json-add! (json-input-fn-generator hash-get hash-add! hash-put! make-hash-table))
(def json-put! (json-input-fn-generator hash-get hash-put! hash-put! make-hash-table))
;; add list version?

(def (json-delete! ht . entry-spec)
  (let lp! ((ht-1 ht)
            (entry-spec-1 entry-spec))
    (cond ((null? entry-spec-1)
           (error "Illegal number of arguments Must be odd. length: "
             (length entry-spec-1))
           ((= 1 (length entry-spec-1))
            (let* ((key (car entry-spec-1))
                   (present? (hash-get ht-1 (car entry-spec-1))))
              (if present?
                (begin
                  (hash-remove! ht-1 (car entry-spec-1))
                  (values present? #t))
                (values #f #f)))))
          (else
           (let* ((key (car entry-spec-1))
                  (entry (hash-get ht-1 key)))
             (if (hash-table? entry)
               (lp! entry (cdr entry-spec-1))
               (error "json-delete: key not present!" (car entry-spec-1))))))))

(def (json-get ht . entry-spec)
  (let lp ((ht-1 ht)
           (entry-spec-1 entry-spec))
    (cond ((null? entry-spec)
           (error "illegal arguments."))
          ((= 1 (length entry-spec-1))
           (hash-get ht-1 (car entry-spec-1)))
          (else
           (let* ((key (car entry-spec-1))
                  (entry (hash-get ht-1 (car entry-spec-1))))
             (if (hash-table? entry)
               (lp entry (cdr entry-spec-1))
               #f))))))

(def (json-append! ht . entry-spec)
  (let lp! ((ht-1 ht)
           (entry-spec-1 entry-spec))
    (let (len (length entry-spec-1 ))
      (cond ((null? entry-spec)
             (error "JSON-APPEND: illegal arguments." len))
            ((= 1 len)
             (error "JSON-APPEND: illegal arguments" len))
            ((= 2 len)
             (let* ((key (car entry-spec-1))
                    (new-list (cadr entry-spec-1))
                    (old-list (or (hash-get ht-1 key) []))
                    (app-list (append! new-list old-list)))
               (check-type list? old-list)
               (check-type list? new-list)
               (json-put! ht-1 key app-list)))
            (else
             (let* ((key (car entry-spec-1))
                    (entry (json-get ht-1 key)))
               (if (hash-table? entry)
                 (lp! entry (cdr entry-spec-1))
                 (error "json-append!: key not present!" (car entry-spec-1)))))))))

(defalias make-ht make-hash-table)


;; (def (collect-xns import-binding)
;;   (let lp ((binding import-binding)
;;            (result '()))
;;     (if (not (gx#import-binding? binding))
;;       (reverse result)
;;       (lp (gx#import-binding-e binding)
;;           (cons (gx#module-context-ns
;;                  (gx#import-binding-context binding))
;;                 result)))))

;; (match self
;;   ((import-binding id _ _ _ ctx _) ; id key phi e ctx weak?
;;    (list id (object-slot-info ctx)))
;;   ((alias-binding _ _ _ e) ; id key phi e
;;    (object-slot-info e))
;;   ((syntax-binding _ _ _ e) ; id key phi e
;;    (cond ((gx#user-expander? e)
;;           (let ((u-ctx (gx#user-expander-context e)))
;;             (cond ((module-context? u-ctx)
;;                    (list  u-ctx))
;;                   (else #f))))
;;          (else #f)))
;;   ((module-binding _ _ _ ctx) ; id key phi ctx
;;    (list ctx))
;;   ((extern-binding _ _ _) ; id key phi
;;    "external")
;;   ((top-binding _ _ _)
;;    "top")
;;   (else (if (not self)
;;           #f
;;           (error "unknown binding: " self))))
