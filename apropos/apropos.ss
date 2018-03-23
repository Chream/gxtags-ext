;;; -*- Gerbil -*-
;;; Christopher Eames (chream) <chream-gmx.com>
;;; gerbil/apropos/apropos/apropos.ss

prelude: :<core>
(import :std/pregexp
        :std/srfi/1
        :std/srfi/13
        :std/misc/ports
        (only-in :std/misc/string string-trim-suffix)
        (only-in :std/misc/list push!)

        :std/misc/rtd

        (only-in :clan/utils/base compose if-let)
        (only-in :clan/utils/filesystem find-files)
        (only-in :clan/utils/hash hash-filter)
        :gerbil/expander
        :gerbil/core
        <expander-runtime>

        (phi: -1 <MOP>))

(export #t )

;; utils

(def (slot-nr obj)
  (type-descriptor-slots (object-type obj)))

(def (slot-names obj)
  (type-descriptor-plist (object-type obj)))

(def (object->list obj)
  (cond ((class-instance? obj)
         (class->list obj))
        ((struct-instance? obj)
         (struct->list obj))))

(def (object-slot-info obj)
  (cons (object-type obj)
        (map (cut cons <> <>)
             (append-map cdr (slot-names obj))
             ;; Works for class as well.
             ;; will return names though.
             (cdr (struct->list obj)))))

;; binding info

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
                  (else "Not available for." info-object))))
      (list key: (binding-id b)
            binding: b
            type: type-sym
            slots: slots
            info-object: info-object
            ns: ns
            location: loc)))

(def (module-binding-info b)
  (let* ((ns (module-context-ns (module-binding-context b)))
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
                     #f)))
    (list (binding-id b)
          binding: b
          type: type
          ns: ns
          arg-list: arg-list)))

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
        (else (error "unknown binding type: "
                binding))))

(def (current-expander-context-table-regexp pat)
  "This matches PAT for each bound identifier in the
   current context. Return a table of '(key . #<import-binding>.)"

  (def (ensure-string elt)
    (if (symbol? elt)
      (symbol->string elt)
      elt))

  (let ((bindings (gx#expander-context-table
                   (gx#current-expander-context))))
    (hash-filter bindings
                 (lambda (key binding)
                   (pregexp-match (ensure-string pat)
                                  (symbol->string key))))))

(def (apr-single sym)
  (let ((binding ((gx#resolve-identifier sym))))
    (binding-info binding)))

(def (apr pat)
  "The function will match the pattern to any bound symbols
   in the current context and return info on the (if so) bound
   value."
  (let ((apr-info '()))
    (hash-for-each (lambda (k v)
                     (let ((binding (gx#resolve-identifier k)))
                       (push! (binding-info binding)
                              apr-info)))
                   (current-expander-context-table-regexp pat))
    apr-info))
