(import :chream/gxtags-ext/tags
        :std/sugar
        :std/getopt

        :chream/utils/all)

(export main)

(def (main . args)
  (def gopt
    (getopt (flag 'append "-a"
                  help: "append to existing tag file")
            (option 'output "-o" default: "TAGS"
                    help: "explicit name of file for tag table")
            (flag 'help "-h" "--help"
                  help: "display help")
            (rest-arguments 'input
                            help: "source file or directory")))

  (def (help what)
    (getopt-display-help what "gxtags"))

  (try
   (let (opt (getopt-parse gopt args))
     (displayln "Running gxtags-ext.. ")
     (logg opt)
     (cond ((hash-get opt 'help)
            (help gopt))
           (else
            (let ((inputs (hash-get opt 'input)))
              (if (and (null? inputs))
                (begin
                  (help gopt)
                  (exit 1))
                (run (hash-get opt 'input)
                     (hash-get opt 'output)
                     (hash-get opt 'append)))))))
   (catch (getopt-error? exn)
     (help exn)
     (exit 1))))
