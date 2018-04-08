;;; -*- Gerbil -*-
;;; © vyzo
;;; Generate emacs TAGS from gerbil sources
;;; only exported symbols are tagged.
;;; Usage: gxtags [-a] [-o tags-file] file-or-directory ...

(import :gerbil/expander
        (only-in :gerbil/compiler/base ast-case)
        (only-in <syntax-case> syntax)
        :gerbil/gambit
        :std/getopt
        :std/sugar
        :std/sort
        :std/format
        :std/text/utf8
        :std/text/json
        :std/misc/ports
        (only-in :std/srfi/13 string-contains)
        :std/misc/repr
        (only-in :std/srfi/1 delete-duplicates reverse!)

        :std/pregexp

        (only-in :clan/utils/base !> if-let)
        (only-in :clan/utils/hash hash-ensure-ref)
        (only-in :clan/utils/files maybe-replace-file)

        "utils"
        "binding-utils")

(export #t)

;; (def (main . args)
;;   (def gopt
;;     (getopt (flag 'append "-a"
;;                help: "append to existing tag file")
;;             (option 'output "-o" default: "TAGS"
;;                help: "explicit name of file for tag table")
;;             (flag 'help "-h" "--help"
;;                help: "display help")
;;             (rest-arguments 'input
;;                             help: "source file or directory")
;;             (flag 'reset "--reset"
;;                   help: "reset TAGS database")))

;;   (def (help what)
;;     (getopt-display-help what "gxtags"))

;;   (try
;;    (let (opt (getopt-parse gopt args))
;;      (cond ((hash-get opt 'help)
;;             (help gopt))
;;            ((hash-get opt 'reset)
;;             (reset-tags!))
;;            (else
;;             (let ((inputs (hash-get opt 'input)))
;;               (if (and (null? inputs))
;;                 (begin
;;                   (help gopt)
;;                   (exit 1))
;;                 (run (hash-get opt 'input)
;;                      (hash-get opt 'output)
;;                      (hash-get opt 'append)))))))
;;    (catch (getopt-error? exn)
;;      (help exn)
;;      (exit 1))))

(def (run inputs tagfile append?)
  (def (expand-input-paths base inputs)
    (map (cut path-expand <> (path-directory base))
         inputs))

  (_gx#load-expander!)
  (make-tags (expand-input-paths tagfile inputs) tagfile append?))

(def tags-path
  (make-parameter (path-normalize "~/.gerbil/tags/tags.json")))
(def current-tags-table
  (make-parameter (make-hash-table)))

(def (make-tags inputs)
  (let ((tags (make-hash-table)))
    (for-each (cut tag-input <> into: tags) inputs)
    (maybe-replace-file tags-path
                        (lambda (json)
                          (table-merge! (copy-json json)
                                        tags))
                        reader: read-json-equal
                        writer: write-json)))

(def (make-counter init)
  (lambda ()
    (set! init (fx1+ init))
    init))

(def paths-counter (make-counter 0))
(def module-counter (make-counter 0))

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
      (let ((tag (make-hash-table)))
        (json-add! tag "id" name)
        (json-add! tag "path" path)
        (json-add! tag "module" module)
        (json-add! tag "position" position)
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

(def (tag-srcfile srcfile into: (ht (make-hash-table)))
  "Returns a list of tags. Format is:
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
  (logg input)
  (let (input (path-normalize input))
    (if (file-exists? input)
      (if (file-directory? input)
        (tag-directory input into: ht)
        (tag-srcfile input into: ht))
      (error "No such file or directory" input)))
  ht)

(def (%tag-filter fn jtable into: (ht (make-hash-table)))
  "NOTE: This function is important as it depends on the
   format of the tags. If it is changed this procedure
   might need rewriting."
  (hash-for-each
   (lambda (module mod-attr-ht)
     (hash-for-each
      (lambda (path tags)
        (hash-for-each
         (lambda (key locat)
           (when (fn key module path locat)
             (let (attr-table (make-hash-table))
               (hash-put! attr-table "position"
                          (tag-position locat))
               (hash-put! attr-table "path" path)
               (hash-put! ht key attr-table))))
         tags))
      (hash-get mod-attr-ht "positions")))
   jtable)
  ht)

(def (tag-lookup key (ht (current-tags-table)))
  (let (r (%tag-filter (lambda (ckey . rest)
                         (equal? ckey key))
                       ht))
    (if (hash-single? r)
      (hash-first-value r)
      (error "tag lookup returned multiple values:" r))))

(def (tag-search key (ht (current-tags-table)))
  (%tag-filter (lambda (ckey . rest)
                 (string-contains ckey key))
               ht))

(def (tag-search-regexp pat (ht (current-tags-table)))
  (%tag-filter (lambda (key . rest)
                 (pregexp-match pat key))
               ht))

(def (tag-table-write file (ht (current-tags-table)) append?: (append? #f))
  (maybe-replace-file file
                      (lambda (json)
                        (if append?
                          (begin (hash-merge! ht json)
                                 ht)
                          ht))
                      reader: read-json-equal
                      writer: write-json))

;; accessors

(def (tag-position tag-info)
  tag-info)

(def (try-import-module filename)
  (try
   (import-module filename)
   (catch (e) #f)))
