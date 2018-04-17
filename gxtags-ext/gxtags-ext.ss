(import :std/sugar
        :std/getopt

        :chream/utils/all
        :chream/gxtags-ext/actor)

(export main test-input test-output)

(def test-input  (path-normalize "~/repos/gerbil/gxtags-ext"))
(def test-output (path-normalize "~/.gerbil/tags/TAGS"))

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
            (!!tag-table.files (default-tags-index)))
           (else
            (let ((inputs (hash-get opt 'inputs))
                  (output (path-normalize (hash-get opt 'output))))
              (cond ((null? inputs)
                     (help gopt)
                     (exit 1))
                    (else
                     (_gx#load-expander!)
                     (!!tag-table.insert! default-tags-index inputs output)))))))
   (catch (getopt-error? exn)
     (help exn)
     (exit 1))))
