(in-package :cl-async)

(define-condition dns-error (connection-error) ()
  (:report (lambda (c s) (format s "Connection DNS error: ~a, ~a" (conn-errcode c) (conn-errmsg c))))
  (:documentation "Passed to a failure callback when a DNS error occurs on a connection."))

(defun get-dns-base ()
  "Grabs the current DNS base (or instantiates if it doesn't exist) and also
   tracks how many open DNS base queries there are."
  (prog1 (if *dns-base*
             *dns-base*
             (let ((dns-base (le:evdns-base-new *event-base* 1))
                   (dns-windows (cffi:foreign-symbol-pointer "evdns_base_config_windows_nameservers")))
               (if dns-windows
                   ;; we're on windows, so load entries from registry
                   (cffi:foreign-funcall-pointer dns-windows () :pointer dns-base :int)
                   ;; on *nix, so load from resolv.conf (if available)
                   (when (probe-file #P"/etc/resolv.conf")
                     (le:evdns-base-resolv-conf-parse
                       dns-base
                       le:+dns-options-all+
                       "/etc/resolv.conf")))
               (setf *dns-ref-count* 0
                     *dns-base* dns-base)))
    (incf *dns-ref-count*)))

(defun free-dns-base (dns-base)
  "Free a dns base."
  (when dns-base
    (unless (cffi:null-pointer-p dns-base)
      (free-pointer-data dns-base :preserve-pointer t)
      (le:evdns-base-free dns-base 0))))

(defun release-dns-base ()
  "Decrements the DNS base reference counter. If there are no more references,
   frees the DNS base."
  (decf *dns-ref-count*)
  (when (<= *dns-ref-count* 0)
    (setf *dns-ref-count* 0)
    (free-dns-base *dns-base*)))

(cffi:defcallback dns-cb :void ((errcode :int) (addrinfo :pointer) (data-pointer :pointer))
  "Callback for DNS lookups."
  (let* ((callbacks (get-callbacks data-pointer))
         (resolve-cb (getf callbacks :resolve-cb))
         (event-cb (getf callbacks :event-cb)))
    (unwind-protect
      (catch-app-errors event-cb
        (if (not (zerop errcode))
            ;; DNS call failed, get error
            (funcall event-cb (make-instance 'dns-error :code errcode :msg (le:evutil-gai-strerror errcode)))

            ;; success, pull out address
            (let ((family (le-a:addrinfo-ai-family addrinfo))
                  (addr nil))

              ;(dotimes (i (cffi:foreign-type-size *addrinfo*))
              ;  (format t "byte ~a: ~a~%" i (cffi:mem-aref addrinfo :unsigned-char i)))
              ;(let ((addr-pt (cffi:pointer-address addrinfo))
              ;      (addr-pt-off (cffi:pointer-address (cffi:foreign-slot-pointer addrinfo *addrinfo* 'le::ai-addr))))
              ;  (format t "addr: ~a~%addr: ~a~%diff: ~a~%" addr-pt addr-pt-off (- addr-pt-off addr-pt)))

              (cffi:with-foreign-object (buf :unsigned-char 128)
                ;; note here, we use the OS-dependent addrinfo-ai-addr macro
                ;; defined un util.lisp
                (let* ((ai-addr (addrinfo-ai-addr addrinfo)))
                  (unless (cffi:null-pointer-p ai-addr)
                    (cond
                      ((eq family +af-inet+)
                       (let ((sin-addr (cffi:foreign-slot-pointer ai-addr (le::cffi-type le::sockaddr-in) 'le::sin-addr)))
                         ;(dotimes (i +sockaddr-size+)
                           ;(format t "byte ~a: ~a~%" i (cffi:mem-aref ai-addr :unsigned-char i)))
                         (setf addr (le:evutil-inet-ntop family sin-addr buf 128))))
                      ((eq family +af-inet6+)
                       (let ((sin6-addr (cffi:foreign-slot-pointer ai-addr (le::cffi-type le::sockaddr-in-6) 'le::sin-6-addr-0)))
                         (setf addr (le:evutil-inet-ntop family sin6-addr buf 128))))))))
              (if addr
                  ;; got an address, call resolve-cb
                  (funcall resolve-cb addr family)
                  ;; hmm, didn't get an address. either cam back as ipv6 or 
                  ;; there was some horrible, horrible error.
                  (funcall event-cb (make-instance 'dns-error :code -1 :msg (format nil "Error pulling out address from family: ~a" family))))

              ;; clean up
              (unless (cffi:null-pointer-p addrinfo)
                (le:evutil-freeaddrinfo addrinfo)))))
      (free-pointer-data data-pointer)
      (release-dns-base))))

(defun dns-lookup (host resolve-cb event-cb &key (family +af-unspec+))
  "Asynchronously lookup a DNS address. Note that if an IP address is passed,
   the lookup happens synchronously. If a lookup is synchronous (and instant)
   this returns T, otherwise nil (lookup happening in background). Either way
   the resolve-cb is called with the lookup info (so always assume this is
   async)."
  (check-event-loop-running)
  (assert (member family (list +af-inet+ +af-inet6+ +af-unspec+)))
  (let ((data-pointer (create-data-pointer))
        (dns-base (get-dns-base)))
    (make-foreign-type (hints *addrinfo* :initial #x0 :type-size +addrinfo-size+)
                       (('le::ai-family family)  ;; only want ipv4 for now
                        ('le::ai-flags le:+evutil-ai-canonname+)
                        ('le::ai-socktype le:+sock-stream+)
                        ('le::ai-protocol le:+ipproto-tcp+))
      (save-callbacks data-pointer (list :resolve-cb resolve-cb :event-cb event-cb))
      (attach-data-to-pointer data-pointer dns-base)
      (let ((dns-req (le:evdns-getaddrinfo dns-base host (cffi:null-pointer) hints (cffi:callback dns-cb) data-pointer)))
        (cffi:null-pointer-p dns-req)))))

