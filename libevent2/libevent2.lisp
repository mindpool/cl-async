(defpackage #:libevent2
  (:use :cl :cffi)
  (:nicknames :le))

(defpackage #:libevent2.accessors
  (:use :cl :cffi :libevent2)
  (:nicknames :le-a))

(in-package :libevent2)

(eval-when (:load-toplevel)
  (define-foreign-library libevent2
    (:darwin (:or "libevent.dylib"))
    (:unix (:or "libevent.so"
                "libevent-2.0.so.5"
                "/usr/lib/libevent.so"
                "/usr/local/lib/libevent.so"
                ; brew's install of libevent on Mac OX X
                "/usr/local/lib/libevent.dylib"))
    (:windows (:or "libevent.dll" "libevent64.dll"))
    (t (:default "libevent")))
  (unless (foreign-library-loaded-p 'libevent2)
    (use-foreign-library libevent2)))

