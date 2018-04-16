#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import
  :std/build-script :std/srfi/1)

(defbuild-script
  '("gxtags-ext/tags.ss"
    (exe: "gxtags-ext/gxtags-ext")))
