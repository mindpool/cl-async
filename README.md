cl-async - Asynchronous Operations for Common Lisp
==================================================

cl-async provides an event loop for Common Lisp using bindings for the
[Libevent](http://libevent.org/)
C library. This brings modern asynchronous programming to Lisp.


Quick Start
-----------

Callback on a signal:

```common-lisp
```

Callback on a file write:

```common-lisp
```

Callback on a socket write:

```common-lisp
```


About the Project
-----------------

After trying out various non-blocking libraries and frameworks for CL, I was a
bit unsatisfied. [IOLib](http://common-lisp.net/project/iolib/) is probably the
best thing out there for non-blocking TCP, but I had a hard time with the
interface and using it without beta versions of CFFI. I decided to write a
library that has very simple to understand concepts, an easy to use interface,
and is portable across Linux and Windows. It uses
[Libevent2](http://libevent.org/) as the async backend, which is a fast,
stable, portable library for asynchronous IO (see my [notes on choosing
Libevent](#libevent)).

The main goal is to provide an experience that's close to JavaScript in how it
handles asynchronous operations with ease, but with the speed and power of
lisp. Portability and ease of use are favored over raw speed.

Please note: *at the moment I consider this library BETA. I'm doing my best to
solidify the API and eliminate any bugs. cl-async will most likely get a lot
more fixes and changes once it's put into production, which should hopefully
not be too far off. Stay tuned.*


Documentation
-------------

The documentation is split into a few main sections.

- [__Function and class documentation__](../../wiki/Functions-and-Classes)
- [__Conditions and events__](../../wiki/Conditions-and-Events)
- [__Event callbacks and error handling__](../../wiki/Callbacks)
- [__Examples__](../../wiki/Examples)
- [__Benchmarks__](../../wiki/Benchmarks)
- [__Implementation notes__](../../wiki/Implementation-Notes)
- [__Drivers__](../../wiki/Drivers)
- [__License__](../../wiki/License)


Known Issues
------------

See the [TODO list](https://github.com/orthecreedence/cl-async/issues).

