cl-async - Asynchronous operations for Common Lisp
==================================================
So after trying out various non-blocking libraries and frameworks for CL, I was
a bit unsatisfied. [IOLib](http://common-lisp.net/project/iolib/) is probably the
best thing out there for non-blocking TCP, but I had a hard time with the 
interface and using it without beta versions of CFFI. I decided to write a
library that has very simple to understand concepts, an easy to use interface,
and is portable across Linux and Windows. It uses [Libevent2](http://libevent.org/)
as the async backend, which is a fast, stable, portable library for asynchronous
IO (see my [notes on choosing Libevent](#libevent)).

The main goal is to provide an experience that's close to javascript in how it
handles asynchronous operations with ease, but with the speed and power of
lisp. Portability and ease of use are favored over raw speed.

*Please note that at the moment I consider this library BETA. I'm doing my best
to solidify the API and eliminate any bugs. cl-async will most likely get a lot
more fixes and changes once it's put into production, which should hopefully not
be too far off. Stay tuned.*

The documentation is split into a few main sections.

- [__Function and class documentation__](blob/master/docs/functions-and-classes.md)
- [__Conditions and events__](#conditions-and-events)
- [__Event callbacks and error handling__](#event-callbacks-and-error-handling-in-general)
- [__Examples__](#examples)
- [__Benchmarks__](#benchmarks)
- [__Implementation notes__](#implementation-notes)
- [__Drivers__](#drivers)
- [__License__](#license)

See the [TODO list](https://github.com/orthecreedence/cl-async/issues).

Conditions and events
---------------------
When something unexpected happens, cl-async will _instantiate_ (not throw) a
condition that explains what happened and pass it into the given 
[event callback](#event-callbacks-and-error-handling-in-general).
This can happen when an HTTP connection is refused, a TCP socket gets an EOF,
etc. Sometimes these conditions won't necessarily be errors, but rather pieces
of information your application might find useful.

The one condition that is _thrown_ by cl-async is [socket-closed](#socket-closed),
which happens when a closed socket is being operated on by the app.

This goes over the conditions you can expect to see when using cl-async. For
information about how these conditions are handled once created, see the
[section explaining event callbacks and error handling](#event-callbacks-and-error-handling-in-general).

- [connection-info](#connection-info) _condition_
- [connection-error](#connection-error) _condition_
  - [conn-errcode](#conn-errcode) _accessor_
  - [conn-errmsg](#conn-errmsg) _accessor_
- [dns-error](#dns-error) _condition_
- [tcp-info](#tcp-info) _condition_
  - [tcp-socket](#tcp-socket) _accessor_
- [tcp-error](#tcp-error) _condition_
- [tcp-eof](#tcp-eof) _condition_
- [tcp-timeout](#tcp-timeout) _condition_
- [tcp-refused](#tcp-refused) _condition_
- [tcp-accept-error](#tcp-accept-error) _condition_
  - [tcp-accept-error-listener](#tcp-accept-error-listener) _accessor_
  - [tcp-accept-error-tcp-server](#tcp-accept-error-tcp-server) _accessor_
- [socket-closed](#socket-closed) _condition_
- [http-info](#http-info) _condition_
- [http-error](#http-error) _condition_
- [http-timeout](#http-timeout) _condition_
- [http-refused](#http-refused) _condition_

### connection-info
Base connection condition. Signals that "something" happened on a connection.
Meant to be extended.

### connection-error
_extends [connection-info](#connection-info)_

Base connection error. Signals that "something bad" happened on a connection.

##### conn-errcode
The error code associated with the connection error. This is generally retrieved
from the underlying OS, but sometimes cl-async will generate its own error
conditions, in which case errcode will be -1.

##### conn-errmsg
Much like `conn-errcode`, this is generally a system message explaining a
connection error. If it is a cl-async generated error, it will have a string
value explaining what happened.

### dns-error
_extends [connection-error](#connection-error)_

This explains a DNS error (for instance if a DNS lookup fails).

### tcp-info
_extends [connection-info](#connection-info)_

Base TCP condition, says "something" happened on a TCP connection.

##### tcp-socket
Holds the TCP socket class. Can be used to write to the socket or close it.

### tcp-error
_extends [connection-error](#connection-error) and [tcp-info](#tcp-info)_

Describes a general error on a TCP connection. If this is triggered, the socket
will generally be closed by cl-async, and the app doesn't need to worry about
doing this. If the app *does* want to close the socket, it can do so by getting
it from the [tcp-socket](#tcp-socket) accessor on the condition and using
[close-socket](#close-socket).

### tcp-eof
_extends [tcp-info](#tcp-info)_

Triggered when the peer on a TCP connection closes the socket.

### tcp-timeout
_extends [tcp-error](#tcp-error)_

Triggered when a TCP connection times out.

### tcp-refused
_extends [tcp-error](#tcp-error)_

Triggered when a TCP connection is refused by the peer.

### tcp-accept-error
_extends [tcp-error](#tcp-error)_

Passed to a [tcp-server](#tcp-server)'s `event-cb` when there is an error
accepting a client connection.

##### tcp-accept-error-listener
The libevent listener c object. Provided in case your app needs to process it in
some way.

##### tcp-accept-error-tcp-server
The `tcp-server` object that the accept error happened on.

### socket-closed
_extends [tcp-error](#tcp-error)_

This exception is thrown by cl-async when the app tries to perform an operation
on a socket that has already been closed via [close-socket](#close-socket).

### http-info
_extends [connection-info](#connection-info)_

Base HTTP condition.

### http-error
_extends [connection-error](#connection-error) and [http-info](#http-info)_

Base HTTP error condition.

### http-timeout
_extends [http-error](#http-error)_

Triggered when an HTTP connection times out.

### http-refused
_extends [http-error](#http-error)_

Triggered when an HTTP connection is refused by the peer.


Event callbacks (and error handling in general)
-----------------------------------------------
Any parameter labelled `event-cb` is what's known as an "event callback." Event
callbacks have one argument: a condition describing the event that caused them
to be invoked. Originally event callbacks were failure callbacks, but since
non-failure conditions are sometimes useful to an app, it made sense to make it
more generic.

The event conditions generally match conditions in libevent, although they try
to be as informative as possible. Note that conditions are not actually thrown,
but rather instantiated via `make-instance` and passed directly to the event
callback.

### Application error handling
cl-async can be set up to catch errors in your application and pass them to
your `event-cb`. This makes for seamless error handling, and keeps a rogue
condition from exiting the event loop (assuming you have an `event-cb` set for
the operation that generated the condition, or a default event handler that
deals with the condition).

Note that the following variables are also controllable on a per-event-loop
basis via the [start-event-loop](#start-event-loop) keyword arguments
`:catch-app-errors` and `:default-event-cb`. It might actually be favorable to
use [start-event-loop](#start-event-loop) since it creates thread-local versions
of these variables when instantiating, which can be useful if running event
loops in multiple threads.

##### \*catch-application-errors\*
_default: `nil`_

By setting this to true, you allow cl-async to catch conditions in your app and
pass them to the event callback associated with the procedure that triggered the
condition.

If this is left as `nil`, triggered conditions will make their way to the top
level and cause the event loop to exit, cancelling any pending events (unless
you have restarts implemented in your app).

##### \*default-event-handler\*
When [\*catch-application-errors\*](#catch-application-errors) is set to `t`
and an `event-cb` is not specified for an operation, the function assigned to
this variable will be used as the `event-cb`. The default:

```common-lisp
(lambda (err)
  ;; throw the error so we can wrap it in a handler-case
  (handler-case (error err)
    ;; got a connection error, throw it (must do this explicitely since
    ;; connection-error extends connection-info)
    (connection-error () (error err))

    ;; this is just info, let it slide
    (connection-info () nil)

    ;; this an actual error. throw it back to toplevel
    (t () (error err))))
```

This can be changed by your application if different behavior is desired.

Examples
--------
Some limited examples are outlined above, but I learn by example, not reading
function definitions and specifications. So here's some more to get you going.

### An echo server

```common-lisp
(defun my-echo-server ()
  (format t "Starting server.~%")
  (as:tcp-server nil 9003  ; nil is "0.0.0.0"
                 (lambda (socket data)
                   ;; echo the data back into the socket
                   (as:write-socket-data socket data))
                 (lambda (err) (format t "listener event: ~a~%" err)))
  ;; catch sigint
  (as:signal-handler 2 (lambda (sig)
                         (declare (ignore sig))
                         (as:exit-event-loop))))

(as:start-event-loop #'my-echo-server)
```

This echos anything back to the client that was sent, until SIGINT is recieved,
which forcibly closes the event loop and returns to the main thread.

Benchmarks
----------
So far, benchmarks are favorable. From my intial profiling, it seems most of the
time is spent in CFFI when on Windows, but in linux (of course) CFFI is a minor
speed bump, and the actual cl-async:* functions are the main slowdown (which is
good). Because of this, I really recommend running any production server on
linux. This isn't so much because Windows sucks, but because I feel like most
lisp implementations focus on linux performance a lot more than Windows.

On my (already crowded) Linode 512, cl-async (for both [tcp-server](#tcp-server)
and [http-server](#http-server)) was able to process about 40K concurrent
requests with this example before running out of memory:

```common-lisp
(defparameter *http-response*
  (babel:string-to-octets
    (with-output-to-string (s)
      (format s "HTTP/1.1 200 OK~c~c" #\return #\newline)
      (format s "Date: Wed, 03 Oct 2012 23:43:10 GMT~c~c" #\return #\newline)
      (format s "Content-Type: text/plain~c~c" #\return #\newline)
      (format s "Content-Length: 9~c~c" #\return #\newline)
      (format s "~c~c" #\return #\newline)
      (format s "omglolwtf"))))

(defun tcp-server-test (&key stats)
  (as:start-event-loop
    (lambda ()
      (format t "Starting TCP server.~%")
      (let ((listener nil)
            (quit nil)
            (finished-requests 0)
            (last-finished 0)
            (last-time 0))
        (setf listener
              (as:tcp-server nil 9009
                             (lambda (socket data)
                               (declare (ignore data))
                               (as:delay (lambda ()
                                           (unless (as:socket-closed-p socket)
                                             (as:write-socket-data
                                               socket *http-response*
                                               :write-cb (lambda (socket)
                                                           (as:close-socket socket)
                                                           (incf finished-requests)))))
                                         :time 5))
                             (lambda (err)
                               (format t "tcp server event: ~a~%" err))))
        (as:signal-handler 2 (lambda (sig)
                               (declare (ignore sig))
                               (setf quit t)
                               (as:free-signal-handler 2)
                               (as:close-tcp-server listener)))
        (labels ((show-stats ()
                   (let* ((stats (as:stats))
                          (incoming (getf stats :incoming-tcp-connections))
                          (outgoing (getf stats :outgoing-tcp-connections))
                          (now (get-internal-real-time))
                          (sec (/ (- now last-time) internal-time-units-per-second))
                          (rate (/ (- finished-requests last-finished) sec)))
                     (setf last-finished finished-requests
                           last-time now)
                     (format t "incoming: ~a~%outgoing: ~a~%finished: ~a~%rate: ~f req/s~%~%" incoming outgoing finished-requests rate))
                   (unless quit
                     (as:delay #'show-stats :time 1))))
          (when stats (show-stats)))))
    :catch-app-errors t)
  (format t "TCP server exited.~%"))

;; run it
(tcp-server-test :stats t)
```

What's happening here is that the server gets a request, delays 5 seconds, then
responds on the same socket. This allows connections to build up for 5 seconds
before they start getting released, which is a good way to test how many
connections it can handle.

On another neighboring Linode, I ran
```shell
httperf --server=1.2.3.4 --port=9009 --num-conns=40000 --num-calls=10 --hog --rate=6000
```

In the `stats` output, I was getting:

    incoming: 12645
    outgoing: 0
    finished: 7330
    rate: 6026.183 req/s
    
So I was getting ~6000k req/s, and in some tests (longer delay value) I was able
to get the "incoming" connections to 40K. 6000/s seems to be the limit of the
machine `httperf` was running on, not the server, but I can't confirm this yet.
From the tests I ran, memory seems to be the number one constraining factor in
scalability of number of connections. The more memory, the more connections can
be handled.

Implementation notes
--------------------
### Libevent
Libevent was chosen for a few reasons:
 - It provides a socket API. The IOLib library was too undocumented for me
 to figure out. Plus things like delayed functions/timers were not clear to me.
 - It wraps the socket implementation and buffering in a [simple and wonderful
 API](http://www.wangafu.net/~nickm/libevent-book/Ref6_bufferevent.html).
 - It was very easy to generate bindings for and wrap in CFFI.
 - It works with Windows. This is big for me, since I do a ton of development
 on Windows. Libraries that assume "Y WOULD U PROGRAM ON WINDOWS?!?!LOL" have
 their place, but most people who say this probably use Ubuntu anyway (sorry,
 it just slipped out). Libevent can be compiled on Windows just fine, and
 with a bit of work, I'm assuming this wrapper could be programmed to use the
 IOCP parts of libevent (I think for now it uses select())
 - It comes with asynchronous HTTP client/server implementations. These are not
 trivial, and if libevent makes it easier to have an asynchronous CL webserver
 or client, then hell let's use it.

The bindings for libevent are auto-generated. I'm not proud of the bindings
themselves, but because I planned to completely wrap them all along, didn't put
too much work into making them pretty and useful. They will most likely stay
as-is (and undocumented).

### HTTP server
The [http-server](#http-server) is a simple way to get a quick HTTP interface
for your app. However, as someone who does a lot of ops as well as dev, I must
warn you that **I would not trust this to be public-facing**. This is not
because I am a terrible programmer, but because I don't think libevent's HTTP
implementation takes into account a lot of things that other HTTP servers have
been battle tested with.

In other words, put [HAProxy](http://haproxy.1wt.eu/) or [NginX](http://nginx.org/)
(or similar) in front of it. Let someone else bear the brunt of dealing with the
security flaws of the open web so you can focus on building a solid application.

### Internals
cl-async tracks anonymous callbacks and libevent objects using what are called
data pointers. A data pointer is just a CFFI pointer that can be passed around
to libevent callbacks, and can also be used to pull data out of a hash table.
So while CFFI callbacks cannot be anonymous, we can fake it by creating a data
pointer, assigning the app-supplied anonymous callbacks to the data pointer in
a hash table lookup (pointer => callbacks), and sending the pointer (in what
would be a void\* argument in C) to the libevent callback. Once the generic
CFFI callback is fired, it can pull out the anonymous callbacks (as well as any
assigned libevent objects) using the data pointer and do what it needs to with
them. Data pointers (and the data attached to them in the function/data hash
tables) are freed once no longer needed. This is managed completely by cl-async.

### TODO
Please see the [Issues list](https://github.com/orthecreedence/cl-async/issues)
for the complete list of what needs to be done.

Drivers
-------
I plan on building and releasing a number of drivers on top of cl-async:

- [beanstalkd](https://github.com/orthecreedence/beanstalk-async)
- MongoDB
- Drakma (async port, would essentially replace [http-client](#http-client))
- Amazon S3/Cloudfront
- SMTP
- Redis

Note that these are libraries I use every day, so am in a good position to test
them in a production environment. Also, even though cl-async includes a simple
HTTP client, [Drakma](http://weitz.de/drakma/) is a lot more badass and has a
ton more features. Porting it to be asynchronous would be very valuable, and
also would make porting other drivers that work over HTTP to cl-async easier.

The biggest problem with asynchronous IO in lisp is that there are lots of
libraries that provide it, but no drivers built on top of the libraries. Nobody
wants to sit around all day programming database drivers. I think if I get
enough traction behind cl-async by providing drivers for enough services, it
could stand to be the first viable asynchronous programming library for Common
Lisp users.

So all I need is critical mass. WHO'S WITH ME?!?!

License
-------
As always, my code is MIT licenced. Do whatever the hell you want with it.
Enjoy!

