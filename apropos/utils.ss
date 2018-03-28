(import :std/srfi/1
        :std/format
        :std/misc/rtd
        (only-in :gerbil/gambit/threads thread-join! spawn))

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

;; paralell map

(def (pappend-map f . ls)
  (append-map thread-join! (apply map (lambda es (apply spawn f es)) ls)))


;; Type checking.

(def (check-type obj specifier?)
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
