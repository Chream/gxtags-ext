(import :std/srfi/1
        :std/format
        :std/misc/rtd
        :std/pregexp
        (only-in :gerbil/gambit/threads thread-join! spawn)
        (only-in :clan/utils/hash hash-filter))

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
  (##locat-container locat))

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

(def (hash-empty? table)
  (if (zero? (hash-length table))
    #t
    #f))

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

(def (expander-context-table-regex pat (phi 0))
  "This matches PAT for each bound identifier in the
   current context. Return a `table' of (key . #<import-binding>.)"
  (def (ensure-string elt)
    (if (symbol? elt)
      (symbol->string elt)
      elt))

  (let ((ctx-table (if (eq? phi all:)
                     (expander-context-table-all)
                     (expander-context-table-phi phi))))
    (hash-filter ctx-table
                 (lambda (k v)
                   (pregexp-match (ensure-string pat) (symbol->string k))))))

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

(def (pp-json json)
  (def (name tree) (car tree))
  (def (children tree) (let (rest (cdr tree))
                         (if (hash-table? rest)
                           (hash->list rest)
                           rest)))
  (let (tree (cons "json-table" (hash->list json)))
    (dolist (child (children tree))
            (displayln (format "~% ~S : ~S ~%"
                               (name tree) (name child))))))





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
