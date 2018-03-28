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
        (only-in :clan/utils/date sleep)


        :gerbil/expander
        :gerbil/core
        <expander-runtime>

        :gerbil/gambit/hash
        :gerbil/gambit/ports
        :gerbil/gambit/threads

        (phi: -1 <MOP>)

        :std/parser

        :gerbil/tools/gxtags

        "location")

(export #t )


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

(def (binding-type binding)
  "Returns a general type for the BINDING. Type, a symbol, can be 'macro,
   'procedure, 'class, 'struct."
  (def (type->symbol type-object)
    (cond ((procedure? type-object)
           'macro)
          ((runtime-struct-info? type-object)
           'struct)
          ((runtime-class-info? type-object)
           'class)
          (else #f)))

  (match binding
    ((import-binding id _ _ _ _ e)
     (binding-type e))
    ((syntax-binding _ _ _ e)
     (type->symbol (gx#expander-e e)))
    ((module-binding id key phi ctx)
     (let ((proc/val (unless (uninterned-symbol? id)
                       (eval id))))
       (if (procedure? proc/val)
         'procedure
         'variable)))
    ((extern-binding _ _ _) 'procedure)
    (else #f)))

(def (binding-ns binding)
  "Returns a `list' of namespaces that the BINDING is in
   the context of."
  (let lp ((binding binding)
           (xns '()))
    (match binding
      ((import-binding _ _ _ e ctx _)
       (let (ns (gx#module-context-ns ctx))
         (match e
           ((import-binding)
            (lp e [ns . xns]))
           (else [ns . xns]))))
      ((syntax-binding _ _ _ e)
       (match e
         ((user-expander _ ctx _)
          (gx#module-context-ns ctx))
         ((macro-expander _)
          "core-macro")
         (else (error "unknown expander." e))))
      ((module-binding _ _ _ ctx)
       (module-context-ns ctx))
      ((extern-binding _ _ _)
       "extern")
      (else #f))))

(def (structure-slots binding)
  (check-type binding gx#syntax-binding?)
  (let (type (!> binding
                 syntax-binding-e
                 gx#expander-e))
    (match type
      ((? runtime-struct-info?)
       (runtime-struct-fields
        (runtime-type-exhibitor type)))
      ((? runtime-class-info?)
       (runtime-class-slots
        (runtime-type-exhibitor type)))
      (else "Unknown structure type." type))))

(def (binding-location binding (xns #f))
  "Returns `locat', the source location of the given BINDING."
  (if (gx#binding? binding)
    (lookup-location-id (gx#binding-id binding))
    (error "Not a binding." binding)))

(def (procedure-lambda-list binding loc)
  (let (dproc (##decompile proc/val))
    (if (list? dproc)
      (cadr dproc)
      #f)))

(def (binding-phi binding)
  (gx#binding-phi binding))


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
        ((gx#import-binding? binding)
         (binding-info (gx#import-binding-e binding)))
        (else (error "unknown binding type: "
                binding))))

(def (hash-empty? table)
  (if (zero? (hash-length table))
    #t
    #f))

(def (expander-context-table-all)
  "This returns all bindings for all phi in the
   running process. Returns `table' of (key . <binding>)."
  (let lp ((ctx (current-expander-context))
           (table (make-hash-table))
           (going-up? #t))
    (let* ((t (gx#expander-context-table ctx))
           (table-empty? (hash-empty? t)))
      (cond ((and table-empty? (not going-up?))
             table)
            ((and table-empty? going-up?)
             (lp (gx#core-context-shift (gx#current-expander-context)  1) table #f))
            (going-up?
             (lp (gx#core-context-shift ctx 1) (hash-merge! t table) going-up?))
            ((not going-up?)
             (lp (gx#core-context-shift ctx -1) (hash-merge! t table) going-up?))
            (else (error "wtf?"))))))

(def (expander-context-table-phi phi)
  (gx#expander-context-table
   (gx#core-context-shift (gx#current-expander-context) phi)))

(def (expander-context-table-regex pat (phi 0))
  "This matches PAT for each bound identifier in the
   current context. Return a table of (key . #<import-binding>.)"
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
