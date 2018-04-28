;;; -*- Gerbil -*-
;;; This file is copied and modified from Gerbil Scheme
;;; See https://github.com/vyzo/gerbil
;;; © vyzo

;;; Generate emacs TAGS from gerbil sources
;;; only exported symbols are tagged.
;;; Usage: gxtags [-a] [-o tags-file] file-or-directory ...
;;;
;;; 2018: This file was modified by
;;; Christopher Eames (Chream) <chream-gmx.com>

(import :gerbil/expander
        (only-in :gerbil/compiler/base ast-case)
        (only-in <syntax-case> syntax)
        (only-in :std/srfi/1 append-map)
        :std/sugar
        :std/sort
        :std/misc/ports
        (only-in :std/srfi/13 string-contains)
        :std/misc/repr
        (only-in :std/srfi/1 delete-duplicates reverse!)

        :std/pregexp

        (only-in :clan/utils/base !> if-let)
        (only-in :clan/utils/hash hash-ensure-ref)

        :chream/utils/all
        )

(export #t)

(def current-tags-table
  (make-parameter (make-hash-table)))

(def (module-tags ctx xtab into: (ht (make-hash-table)))

  (def loc #f)

  (defrules with-loc ()
    ((_ stx body rest ...)
     (let (K (lambda () body rest ...))
       (let (new-loc (stx-source stx))
         (if new-loc
           (let (save-loc loc)
             (set! loc new-loc)
             (K)
             (set! loc save-loc))
           (K))))))

  (def (tag! module eid name)
    (let* ((path (locat-path loc))
           (position (filepos-line (locat-position loc))))
      (let ((tag (make-hash-table size: 4)))
        (json-add! tag "key" (symbol->string name))
        (json-add! tag "path" (json-make-ref! ht "path" path))
        (json-add! tag "mod" (json-make-ref! ht "mod" module))
        (json-add! tag "pos" position)
        (json-append! ht "tags" [tag]))))

  (def (tag-e stx)
    (with-loc stx
              (ast-case stx (%#begin
                             %#begin-syntax
                             %#define-values
                             %#define-syntax
                             %#extern
                             %#module)
                ((%#begin expr ...)
                 (for-each tag-e #'(expr ...)))
                ((%#begin-syntax expr ...)
                 (parameterize ((current-expander-phi (fx1+ (current-expander-phi))))
                   (for-each tag-e #'(expr ...))))
                ((%#define-values (id ...) _)
                 (for-each tag-def (filter values #'(id ...))))
                ((%#define-syntax id _)
                 (tag-def #'id))
                ((%#extern decl ...)
                 (for-each tag-decl #'(decl ...)))
                ((%#module id expr ...)
                 (let ((eid (binding-id (resolve-identifier #'id)))
                       (ctx (syntax-local-e #'id)))
                   ;; this only tags bindings if they are exported by the parent
                   ;; module; this works well for prelude-style module structures
                   ;; but doesn't tag bindings reachable because the module itself
                   ;; is exported
                   ;; TODO if the module is exported, add module's exports to the
                   ;;      tag table
                   (parameterize ((current-expander-context ctx))
                     (tag-name! eid)
                     (for-each tag-e #'(expr ...)))))
                (_ (void)))))

  (def (tag-def id)
    (with-loc id
              (tag-name! (binding-id (resolve-identifier id)))))

  (def (tag-decl decl)
    (ast-case decl ()
      ((id eid)
       (with-loc #'id
                 (tag-name! (stx-e #'eid))))))

  (def (tag-name! eid)
    (alet* ((xtab-res (hash-get xtab eid)))
      (with ([name module-name] xtab-res)
        (tag! module-name eid name))))

  (check-type hash-table? ht)
  (let (stx (module-context-code ctx))
    ;; also tag the module id itself with library prefix
    (let* ((id (expander-context-id ctx))
           (gid (make-symbol ":" id)))
      (parameterize ((current-expander-context ctx)
                     (current-expander-phi 0))
        (tag-e stx))
      ht)))

(def (try-import-module filename)
  (try
   (import-module filename)
   (catch (e) #f)))

(def (tag-srcfile srcfile into: (ht (make-hash-table)))
  "Returns a list of tags.  is:
   '(ID (KEY . ROOT-MODULE) LOCATION).
    where MODULES-EXPORTED is a list of modules where
    the corresponding symbol is exported from. LOCATION
    is a `locat' structure which contains the location infomation."

  (cond
   ((try-import-module srcfile)
    => (lambda (ctx)
         ;; binding-id -> [export-name ...]
         (let ((xtab (make-hash-table-eq))
               (xports (gx#module-context-export ctx)))
           (for-each
             (lambda (xport)
               (let* ((bind (core-resolve-module-export xport))
                      (name (module-export-name xport))
                      (export-root-module (resolve-module-export-root-module xport ctx)))
                 (hash-put! xtab (binding-id bind)
                            [name export-root-module])))
             xports)
           (module-tags ctx xtab into: ht)
           ht)))
   (else #f)))

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
  (let (input (path-normalize input))
    (if (file-exists? input)
      (if (file-directory? input)
        (tag-directory input into: ht)
        (tag-srcfile input into: ht))
      (error "No such file or directory" input)))
  ht)

(def (tag-lookup key (table (current-tags-table)))
  (let (r (filter-map
           (lambda (tag)
             (let ((ckey (json-get tag "key")))
               (if (equal? ckey key)
                 (begin (json-refs-expand! table tag)
                        tag)
                 #f)))
           (json-get table "tags")))
    (if (= 1 (length r))
      (car r)
      (error "tag-lookup: got multiple results! " r))))

(def (tag-search key (table (current-tags-table)))
  (filter-map (lambda (tag)
                (let ((ckey (json-get tag "key")))
                  (if (string-contains ckey key)
                    (begin (json-refs-expand! table tag)
                           tag)
                    #f)))
              (json-get table "tags")))

(def (tag-search-regexp pat (table (current-tags-table)))
  (filter-map (lambda (tag)
                (let ((ckey (json-get tag "key")))
                  (if (pregexp-match pat ckey)
                    (begin (json-refs-expand! table tag)
                           tag)
                    #f)))
              (json-get table "tags")))
