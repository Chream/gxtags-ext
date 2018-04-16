#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import
  :std/build-script :std/srfi/1)

(defbuild-script
  (let ((spec
         (append (append-map
                  (lambda (dir)
                    (filter-map
                     (lambda (filename)
                       (and (equal? (path-extension filename) ".ss")
                            (path-expand (path-strip-extension filename) dir)))
                     (directory-files dir)))
                  ["gxtags-ext"])
                 '((exe: "gxtags-ext/gxtags-ext")))))
    (displayln spec)
    spec))
