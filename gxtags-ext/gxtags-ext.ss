(import :chream/gxtags-ext/tags
        (only-in :gerbil/gambit spawn)
        :std/sugar
        :std/getopt
        :chream/utils/all)

(export main test-input)

(def test-input  (path-normalize "~/repos/gerbil/gxtags-ext"))

(def (main . args)
  (def gopt
    (getopt (option 'index "-i" default: (path-normalize "~/.gerbil/tags/index")
                    help: "explicit name of file for tag index")
            (option 'output "-o" default: "TAGS"
                    help: "explicit name of file for tag table")
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
           (else
            (let ((inputs (hash-get opt 'inputs)))
              (if (and (null? inputs))
                (begin
                  (help gopt)
                  (exit 1))
                (run inputs
                     (hash-get opt 'output)
                     (hash-get opt 'index)))))))
   (catch (getopt-error? exn)
     (help exn)
     (exit 1))))

(def (run inputs tagfile indexfile)
  (def (expand-input-paths base inputs)
    (map (cut path-expand <> (path-directory base))
         inputs))

  (_gx#load-expander!)
  (let ((tagfile (path-normalize tagfile)))
    (logg (current-tags-index))
    (logg tagfile)
    (logg inputs)
    (!!tag-table.insert! (current-tags-index) inputs tagfile)))
