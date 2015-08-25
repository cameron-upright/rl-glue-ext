**Notes:** To use this or any other codec, you must have RL-Glue Core installed first.

### Available Downloads ###

  * [Lisp Codec Full Distribution 1.0](http://rl-glue-ext.googlecode.com/files/lisp-codec-full-1.0.tar.gz) [(File Details)](http://code.google.com/p/rl-glue-ext/downloads/detail?name=lisp-codec-full-1.0.tar.gz)

> The **full** distribution contains a pre-configured ASDF environment with all the ASDF libraries required to use the codec. There is no need of any ASDF installation and configuration knowledge to use this distribution.

> _Recommended for users without [ASDF](http://common-lisp.net/project/asdf/) experience._

> Quick install and usage
```
$ # Choose the install destination.
$ cd /path/to

$ # Unpack the files
$ tar -zxf lisp-codec-full-1.0.tar.gz

$ # Start your Lisp
$ lisp
*

* ; Initalize the ASDF system
* (load #p"/path/to/lisp-codec-full/setup")
```

  * [Lisp Codec ASDF Distribution 1.0](http://rl-glue-ext.googlecode.com/files/lisp-codec-asdf-1.0.tar.gz) [(File Details)](http://code.google.com/p/rl-glue-ext/downloads/detail?name=lisp-codec-asdf-1.0.tar.gz)

> The **asdf** distribution contains only the rl-glue-codec, rl-glue-utils and rl-glue-examples ASDF packages. If you choose this, you have to install the required additional libraries on your own.

> _Recommended for users with [ASDF](http://common-lisp.net/project/asdf/) experience._

  * [Lisp Codec Developer Distribution 1.0](http://rl-glue-ext.googlecode.com/files/lisp-codec-dev-1.0.tar.gz) [(File Details)](http://code.google.com/p/rl-glue-ext/downloads/detail?name=lisp-codec-dev-1.0.tar.gz)

> The **dev** distribution contains a snapshot of the development directory. It includes source code, tests, documentation and development tools.

> _Recommended for Lisp Codec developers._

### Official Documentation ###
  * Lisp Codec Manual [(PDF)](http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Lisp/doc/manual/lisp-codec.pdf)
  * Lisp Codec API Reference [(HTML)](http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Lisp/doc/reference/index.html)

### Lisp Codec Homepage ###
For more information about the Lisp codec, and others, please visit the RL-Glue homepage:
http://glue.rl-community.org

And the Lisp Codec Homepage:
http://glue.rl-community.org/Home/Extensions/lisp-codec

### Past Versions ###
To list all past versions of the Lisp codec, go here:
http://code.google.com/p/rl-glue-ext/downloads/list?q=language:Lisp