Functions and Classes
=====================

You can use cl-async with the prefixes `cl-async:` or `as:`. Throughout the
functions documented below, you will see a lot of `event-cb` callback arguments.
Since any callback labelled `event-cb` has the same specification, they are not
documented here. Please refer to the
[section on error handling](#event-callbacks-and-error-handling-in-general)
for more information on these callbacks (and error handling in general).

- [start-event-loop](#start-event-loop) _function_
- [exit-event-loop](#exit-event-loop) _function_
- [delay](#delay) _function_
- [timer](#timer) _function (deprecated)_
- [signal-handler](#signal-handler) _function_
- [free-signal-handler](#free-signal-handler)
- [clear-signal-handlers](#clear-signal-handlers)
- [dns-lookup](#dns-lookup) _function_
- [tcp-send](#tcp-send) _function_
- [tcp-server](#tcp-server) _function_
- [close-tcp-server](#close-tcp-server)
- [write-socket-data](#write-socket-data) _function_
- [set-socket-timeouts](#set-socket-timeouts) _function_
- [enable-socket](#enable-socket) _function_
- [disable-socket](#disable-socket) _function_
- [socket-closed-p](#socket-closed-p) _function_
- [close-socket](#close-socket) _function_
- [http-client](#http-client) _function_
- [http-server](#http-server) _function_
- [close-http-server](#close-http-server) _function_
- [http-response](#http-response) _function_
- [http-request](#http-request) _class_
  - [http-request-c](#http-request-c) _accessor_
  - [http-request-method](#http-request-method) _accessor_
  - [http-request-uri](#http-request-uri) _accessor_
  - [http-request-resource](#http-request-resource) _accessor_
  - [http-request-querystring](#http-request-querystring) _accessor_
  - [http-request-headers](#http-request-headers) _accessor_
  - [http-request-body](#http-request-body) _accessor_
- [stats](#stats) _function_

### start-event-loop
Start the event loop, giving a function that will be run inside the event loop
once started. `start-event-loop` blocks the main thread until the event loop
returns, which doesn't happen until the loop is empty *or*
[exit-event-loop](#exit-event-loop) is called inside the loop.

This function must be called before any other operations in the library are
allowed. If you try to do an async operation without an event loop running, it
will throw an error.

It allows specifying callbacks for the fatal errors in libevent (called when
libevent would normally exit, taking your app with it), logging, and default
application error handling.

```common-lisp
;; definition:
(start-event-loop start-fn &key fatal-cb logger-cb default-event-cb catch-app-errors)

;; example:
(start-event-loop (lambda () (format t "Event loop started.~%")))
```

##### fatal-cb definition

```common-lisp
(lambda (errcode) ...)
```

##### logger-cb definition

```common-lisp
(lambda (loglevel msg) ...)
```

`loglevel` corresponds to syslog levels.

##### default-event-cb and catch-app-errors
Please see the [application error handling](#application-error-handling) section
for complete information on these. They correspond 1 to 1 with
[\*default-event-handler\*](#default-event-handler) and
[\*catch-application-errors\*](#catch-application-errors). Setting them when
calling `start-event-loop` not only cuts down on `setf`s you have to do when
starting your evented app, but also uses thread-local versions of the vars,
meaning you can start multiple event loops in multiple threads wiithout using
the same values for each thread.

### exit-event-loop
Exit the event loop. This will free up all resources internally and close down
the event loop.

Note that this doesn't let queued events process, and is the equivelent of
doing a force close. Unless you really need to do this and return control to
lisp, try to let your event loop exit of "natural causes" (ie, no events left to
process). You can do this by freeing your signal handlers, servers, etc. This
has the added benefit of letting any connected clients finish their requests
(without accepting new ones) without completely cutting them off.

```common-lisp
;; definition
(exit-event-loop)
```

### delay
Run a function asynchronously. Takes two optional parameters: `time`, the number
of seconds to wait before running the given function (run with no delay if
`nil`), and `event-cb` which can be used to catch application errors should they
occur while running `callback`.

```common-lisp
;; definition:
(delay callback &key time event-cb)

;; example:
(delay (lambda () (format t "Run me immediately after control is given to the event loop.~%")))
(delay (lambda () (format t "I will run 3.2 seconds after calling (delay).~%")) :time 3.2)
```

### timer
_Deprecated_

Yes, even in its infancy, this library has a deprecated function. Use
[delay](#delay) for running functions asynchronously!

### signal-handler
Create a signal handler. This listens for the given `signo` not only in the 
event loop, but also in the lisp app as well. It replaces the current lisp
signal handler by calling C's `signal` function. When a signal handler is freed
via [free-signal-handler](#free-signal-handler), the original lisp signal
handler is restored as it was before binding the signal handler.

Note that signals that aren't freed via [free-signal-handler](#free-signal-handler)
or [clear-signal-handlers](#clear-signal-handlers) will linger on even after all
other events are out of the event loop, which prevents it from exiting. If you
want your event loop to exit naturally, you must free your signals when you're
done with them.

```common-lisp
;; definition
(signal-handler signo signal-cb &key event-cb)

;; example
(signal-handler 2 (lambda (sig) (format t "got SIGINT: ~a~%" sig))
                  (lambda (err) (foramt t "error processing signal callback: ~a~%" err)))
```

The `signo` arg is the POSIX integer signal you want to handle.

In the case of `signal-handler`, `event-cb` will *only* be called when an error
occurs in the signal callback. There are no cl-async events that occur during
signal processing.

##### signal-cb definition
```common-lisp
(lambda (signo) ...)
```

### free-signal-handler
Unbinds a signal handler. This deletes the libevent signal listener event and
also restores the lisp signal handler that existed before calling
[signal-handler](#signal-handler).

```common-lisp
;; definition
(free-signal-handler signo)

;; example
(signal-handler 2 (lambda (sig)
                    (close-server *my-app-server*)
                    (free-signal-handler 2)))
```

### clear-signal-handlers
Clear all cl-async bound signal handlers. This deletes the libevent event
listeners and restores the original lisp signal handlers for each bound signal.

This is useful if you don't want to track all the signals you've bound and
[free](#free-signal-handler) them manually, but don't want to [exit the event
loop forcibly](#exit-event-loop).

```common-lisp
;; definition
(clear-signal-handlers)
```

### dns-lookup
__Note: this is [broken in 64-bit](https://github.com/orthecreedence/cl-async/issues/15).
Help fixing would be appreciated, as I can't seem to figure it out.__

Asynchronously lookup an IP address given a hostname. If the hostname is an IP
address already, the mechanics are the same although the callback is called
synchronously.

Please note that at this time, IPV6 is not supported. Libevent has support for
it, but I don't feel like wrapping up the necessary classes just yet. I'd rather
get IPV4 going and then focus on IPV6 when everything's working. While the
`resolve-cb` supports a family parameter, it will always be `AF_INET` until this
is implemented.

```common-lisp
;; definition
(dns-lookup host resolve-cb event-cb)

;; example
(dns-lookup "www.google.com"
            (lambda (host family)
              (format t "Address: ~a~%" host))
            (lambda (err) (format t "err: ~a~%" err)))
```

##### resolve-cb definition

```common-lisp
(lambda (ip-address-string ip-address-family) ...)
```

As mentioned, until IPV6 is implemented, `ip-address-family` will *always* be
`AF_INET`. To test this, you can use the included libevent2 package's definition
of `libevent2:+af-inet+` or `libevent2:+af-inet-6+` (`le:` for short).

### tcp-send
Open an asynchronous TCP connection to a host (IP or hostname) and port, once
connected send the given data (byte array or string) and process any response
with the given read callback. Also supports timing out after no data is read /
written in (in seconds). 

Note that `tcp-send` always opens a new connection. If you want to send data on
and existing connection (and also be able to set new read/write/event callbacks
on it), check out [write-socket-data](#write-socket-data).

Note that the `host` can be an IP address *or* a hostname, the hostname will
be looked up asynchronously via libevent's DNS implementation. Also note that
the DNS lookup does __not__ use [dns-lookup](#dns-lookup), but directly calls
into the libevent DNS functions, so [IPV6](https://github.com/orthecreedence/cl-async/issues/7) 
and [x86-64](https://github.com/orthecreedence/cl-async/issues/15) are both
supported when using DNS.

```common-lisp
;; definition:
(tcp-send host port data read-cb event-cb &key read-timeout write-timeout)

;; example:
(tcp-send "www.google.com" 80
          (format nil "GET /~c~c" #\return #\newline)
          (lambda (socket data)
            (when (pretend-http-package:process-http-stream data) 
              (close-socket socket)))  ; close the socket if done processing
          #'my-app-error-handler)
```

##### read-cb definition

```common-lisp
(lambda (socket byte-array) ...)
```

`socket` should never be dealt with directly as it may change in the future,
however it *can* be passed to other cl-async functions that take a `socket` arg.

##### write-cb definition

```common-lisp
(lambda (socket) ...)
```

The `write-cb` will be called after data written to the socket's buffer is
flushed out to the socket.

### tcp-server
Bind an asynchronous listener to the given bind address/port and start accepting
connections on it. It takes read and event callbacks (like [tcp-send](#tcp-send)).
If `nil` is passed into the bind address, it effectively binds the listener to
"0.0.0.0" (listens from any address). A connection backlog can be specified when
creating the server via `:backlog`, which defaults to -1.

This function returns a `tcp-server` class, which allows you to close the
server via [close-tcp-server](#close-tcp-server).

```common-lisp
;; definition
(tcp-server bind-address port read-cb event-cb &key (backlog -1))  =>  tcp-server

;; example
(tcp-server "127.0.0.1" 8080
            (lambda (socket data)
              (format t "data: ~a~%" data)
              (write-socket-data socket "i noticed you have brathes. i have brathes too. uhhhhuhuhuh."
                                 :write-cb (lambda (socket)
                                             (close-socket socket))))
            nil)  ;; use *default-event-handler* as the event handler for this operation
```

##### read-cb definition

```common-lisp
(lambda (socket byte-array) ...)
```

`socket` should never be dealt with directly as it may change in the future,
however it *can* be passed to other cl-async functions that take a `socket` arg.

### close-tcp-server
Takes a `tcp-server` class, created by [tcp-server](#tcp-server) and closes the
server it wraps. This can be useful if you want to shut down a TCP server
without forcibly closing all its connections.

If the given server is already closed, this function returns without doing
anything.

```common-lisp
;; definition
(close-tcp-server tcp-server)
```

### write-socket-data
Write data to an existing socket (such as one passed into a `tcp-send` read-cb).
Data can be a byte array or string (converted to a byte array via babel).
Supports resetting the callbacks on the given socket. The `write-cb` is useful
if you want to close the connection after sending data on the socket but want to
make sure the data sent before closing.

Note that if you call this using a socket that has been closed already, it will
throw a [socket-closed](#socket-closed) condition.

```common-lisp
;; definition
(write-socket-data socket data &key read-cb write-cb event-cb)

;; examples
(write-socket-data socket "thanks for connecting. how are you? (good|bad)"
                   :read-cb (lambda (socket data)
                              (my-app:continue-conversation socket data))
                   :event-cb (lambda (err)
                               (format t "condition while having convo: ~a~%" err)))

(write-socket-data socket "invalid command, closing connection"
                   :write-cb (lambda (socket) (close-socket socket)))
```

If you were to close the socket right after sending the data to the buffer,
there's no guarantee it would be sent out. Setting a `write-cb` guarantees that
the data is sent when called.

##### write-cb definition

```common-lisp
(lambda (socket) ...)
```

### set-socket-timeouts
Set the read/write timeouts (in seconds) on a socket. If nil, the timeout is
cleared, otherwise if a number, the timeout is set into the socket such that
when the socket is active and hasn't been read from/written to in the specified
amount of time, it is closed.

`nil` for a timeout value unsets the timeout.

Note that if you call this using a socket that has been closed already, it will
throw a [socket-closed](#socket-closed) condition.

```common-lisp
;; definition
(set-socket-timeouts socket read-sec write-sec)

;; example
(set-socket-timeouts socket 10.5 nil)
```

### enable-socket
Enable read/write monitoring on a socket. This is done automatically by
[tcp-send](#tcp-send) and [write-socket-data](#write-socket-data) so you
probably don't need to worry too much about when to use it. On the other hand,
[disable-socket](#disable-socket) will probably be a bit more useful.

```common-lisp
;; definition
(enable-socket socket &key read write)

;;example
(enable-socket socket :read t :write t)  ; enable read and write monitoring on this socket
```

### disable-socket
Disable read/write monitoring on a socket. This is useful if you get the data
you need from a socket, but while you're processing the data, you don't want the
socket's read timeout to fire. This will both disable the timeouts and callbacks
associated with the socket until enabled again.

```common-lisp
;; definition
(disable-socket socket &key read write)
```

### socket-closed-p
Determines if a socket has been closed already.

```common-lisp
;; definition
(socket-closed-p socket)
```

### close-socket
Close a socket and free its callbacks.

Note that if you call this using a socket that has been closed already, it will
throw a [socket-closed](#socket-closed) condition.

```common-lisp
;; definition
(close-socket socket)
```

### http-client
Asynchronously communicates with an HTTP server. Allows setting the method,
headers, and body in the request which should be enough to make just about any
HTTP request. This functionality wraps the libevent HTTP client.

If a "Host" header isn't passed in, it is automatically set with whatever host
is pulled out of the `uri`. Also, any "Connection" header passed in will be
ignored...for right now, every request is sent out with `Connection: close`,
but this [will probably change to be customizable soon](https://github.com/orthecreedence/cl-async/issues/19).

The `timeout` arg is in seconds.

```common-lisp
;; definition
(http-client uri request-cb event-cb &key (method 'GET) headers body timeout)

;; example
(http-client "http://musio.com/"
             (lambda (status headers body)
               (format t "Result: ~s~%" (list status headers (babel:octets-to-string body :encoding :utf-8))))
             (lambda (err)
               (format t "http event: ~a~%" err))
             :method 'GET
             :headers '(("Accept" . "text/html"))
             :timeout 5)
```

##### request-cb definition

```common-lisp
(lambda (http-status http-headers body-byte-array) ...)
```

- `http-status` is an integer corresponding to the HTTP status code returned.
- `http-headers` is an alist, as such: '(("Content-Type" . "text/html") ...)
- `body-byte-array` is pretty self-explanatory. Convert to string w/ babel if
needed.

### http-server
Start a server that asynchronously processes HTTP requests. It takes data out of
the request and populates the [http-request](#http-request) with it, which is
passed into the request callback.

This function returns an `http-server` class, which allows you to close the
server via [close-http-server](#close-http-server).

Once the application is done processing the request, it must respond by calling
the [http-response](#http-response) function.

If `nil` is passed in into the `bind` arg, the server is bound to "0.0.0.0"

```common-lisp
;; definition
(http-server bind port request-cb event-cb)  =>  http-server

;; example
(http-server "192.168.0.1" 8090
             (lambda (req)
               (format t "Request: ~a~%" req)
               (http-response req :body "hai")))
```

##### request-cb definition

```common-lisp
(lambda (http-request) ... )
```

`http-request` is a on object of type [http-request](#http-request).

### close-http-server
Takes an `http-server` class, created by [http-server](#http-server) and closes
the server it wraps. This can be useful if you want to shut down a HTTP server
without forcibly closing all its connections.

If the given server is already closed, this function returns without doing
anything.

Note: This function closes the listener for new HTTP client requests. Once the
current requests are finished processing, it frees all resources associated with
the server. In other words, a graceful exit.

```common-lisp
;; definition
(close-http-server http-server)

;; example
(let ((server (http-server "127.0.0.1" 80 ...)))
  (signal-handler 2 (lambda (sig)
                      (declare (ignore sig))
                      ;; close the server when we get SIGINT
                      (close-http-server server))))
```

### http-response
This is the function called by the application using an [http-server](#http-server)
after it is done processing a request. It takes the [http-request](#http-request)
object passed into the request callback, along with some information about the
response we're sending.

```common-lisp
;; definition
(http-response http-request &key (status 200) headers (body ""))

;; example
(http-server nil 80
             (lambda (req)
               (http-response req
                              :status 200
                              :headers '(("Content-Type" . "application/json"))
                              :body "{\"name\":\"larry\"}")))
```

### http-request
This is the class passed to an HTTP request callback after a request comes in
from [http-server](#http-server). It must also be passed into
[http-response](#http-response) when the request is finished, since it holds the
pointer to the socket the request came in on.

`http-request` has a pretty-print method associated with it, so if you do
something like `(format t "~a~%" http-request)`, you'll get a nice, detailed
overview of the request (method, uri, headers, content body length (in bytes),
etc).

### http-request accessors
This details the accessors in `http-request`.

##### http-request-c
Pulls out the pointer to the libevent request object. This is included just in
case extra processing is needed on the request that the library doesn't handle
for you. In other words, ignore this accessor unless you know the libevent evhttp
internals and are comfortable using the libevent CFFI wrapper included with
cl-async.

##### http-request-method
Pull out the request method. This is a symbol, and will be one of

```common-lisp
'(GET POST HEAD PUT DELETE OPTIONS TRACE CONNECT PATCH)
```

##### http-request-uri
This is the full request URI in the request. For instance, if the request was

    GET /documents/45?format=json

Then this will be the string "GET /documents/45?format=json"

##### http-request-resource
This is a string of the request resource (path). A request of

    GET /mysite/index?page=4

The resource will be "/mysite/index"

##### http-request-querystring
The querystring from the request (string). Everything after (and not including)
the "?"

##### http-request-headers
All headers given in the request as an alist:

```common-lisp
'(("Host" . "musio.com")
  ("Accept" . "text/html"))
```

##### http-request-body
Get the body out of the request. Since we don't make any assumptions about the
data that's being passed around, it is a byte array. Convert it to a string in
your app via `babel:octets-to-string` if needed.

It's important to note that at this time, multipart form data, posted files, etc
are *not* decoded by `http-server`. As such, it is currently up to your app to
do this. *This may change in the future* and if so, I will do my best to make the
change backwards compatible.

### stats
This function returns data on the current state of the cl-async internals. How
many incoming/outgoing connections, how many registered callbacks, how many
registered data objects, how many open DNS requests, etc.

Data is a plist. Stats might change in the near future.

```common-lisp
;; definition
(stats)
```
