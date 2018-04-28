(import :std/sugar
        :std/getopt
        (only-in :gerbil/gambit thread-group->thread-list display-exception)

        :chream/utils/all
        "actor")

(export #t)

(def test-input  (path-normalize "~/local/gerbil/src"))
(def test-output (path-normalize "~/.gerbil/tags/std-tags.json"))

(def test-input2  (path-normalize "~/repos/gerbil/gxtags-ext/gxtags-ext/tag-impl.ss"))
(def test-output2 (path-normalize "~/.gerbil/tags/tag-impl-tags.json"))

(def (main . args)
  (def gopt
    (getopt (option 'index "-i" default: "~/.gerbil/tags/default-index"
                    help: "explicit name of file for tag index")
            (option 'output "-o" default: "TAGS.json"
                    help: "explicit name of file for tag table")
            (option 'delete "-d" default: #f
                    help: "delete TAGS file from index")
            (flag 'list-files "-l"
                  help: "list all TAGS files")
            (flag 'help "-h" "--help"
                  help: "display help")
            (rest-arguments 'inputs
                            help: "source file or directory")))

  (def (help what)
    (getopt-display-help what "gxtags"))

  (try
   (let (opt (getopt-parse gopt args))
     (logg "Running gxtags-ext.. ")
     (cond ((hash-get opt 'help)
            (help gopt))
           ((hash-get opt 'delete)
            (displayln "In delete.. Not implemented."))
           ((hash-get opt 'list-files)
            (map (cut !!tag-table.files <>)
                 (thread-group->thread-list
                  (index-thread-group))))
           (else
            (let ((inputs (hash-get opt 'inputs))
                  (output (hash-get opt 'output))
                  (index  (hash-get opt 'index)))
              (ensure-file-exists! output)
              (when (null? inputs)
                (help gopt)
                (exit 1))
              (_gx#load-expander!)
              (let (tag-table (spawn-index index))
                (!!tag-table.insert! tag-table inputs output)
                (for-each (cut stop-worker <>) (!!tag-table.files tag-table))
                (stop-index index))))))
   (catch (getopt-error? exn)
     (help exn)
     (display-exception exn)
     (exit 1))
   (catch (exception? e) (begin (logg "here")
                                (display-exception e)))))
