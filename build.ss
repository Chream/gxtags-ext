#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import
  :std/build-script :std/srfi/1)

(defbuild-script
  '("gxtags-ext/tag-impl.ss"
    "gxtags-ext/actor.ss"
    (exe: "gxtags-ext/gxtags-ext")))
