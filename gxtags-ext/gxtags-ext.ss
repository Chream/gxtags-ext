(import :std/sugar
        :std/getopt

        :chream/utils/all
        :chream/gxtags-ext/actor)

(export #t)

(def test-input  (path-normalize "~/local/gerbil/src"))
(def test-output (path-normalize "~/.gerbil/tags/std-tags.json"))

(def test-input2  (path-normalize "~/repos/gerbil/gxtags-ext/gxtags-ext/tag-impl.ss"))
(def test-output2 (path-normalize "~/.gerbil/tags/tag-impl-tags.json"))

(def (main . args)
  (def gopt
    (getopt (option 'index "-i" default: (path-normalize "~/.gerbil/tags/index")
                    help: "explicit name of file for tag index")
            (option 'output "-o" default: "TAGS"
                    help: "explicit name of file for tag table")
            (option 'delete "-d" default: #f
                    help: "delete TAGS file from index")
            (flag 'list-files "-l"
                    help: "list current TAGS files")
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
            (!!tag-table.files default-tags-table))
           (else
            (let ((inputs (hash-get opt 'inputs))
                  (output (path-normalize (hash-get opt 'output))))
              (ensure-file-exists! output)
              (cond ((null? inputs)
                     (help gopt)
                     (exit 1))
                    (else
                     (_gx#load-expander!)
                     (logg inputs)
                     (logg output)
                     (!!tag-table.insert! default-tags-table inputs output)))))))
   (catch (getopt-error? exn)
     (help exn)
     (exit 1))))
