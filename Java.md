**Notes:** To use this or any other codec, you must have RL-Glue Core installed first.

### Available Downloads (only one is required) ###

  * [Java User Distribution 2.07](http://rl-glue-ext.googlecode.com/files/java-codec-2.07.tar.gz) [(File Details)](http://code.google.com/p/rl-glue-ext/downloads/detail?name=java-codec-2.07.tar.gz)
> The **user** distribution is no-nonsense distribution that most people will want to download.
> It contains:
    * Java codec compiled into a JAR file (with installer)
    * The documentation (PDF)
    * The examples
    * README file to get you started if you don't like manuals

  * [Java Developer Distribution 2.07](http://rl-glue-ext.googlecode.com/files/java-codec-dev-2.07.tar.gz) [(File Details)](http://code.google.com/p/rl-glue-ext/downloads/detail?name=java-codec-dev-2.07.tar.gz)
> The **dev** distribution is almost a snapshot of the development directory of this project.  It includes all of the source code, the ant compile scripts, tests, javadocs, tests, etc.


These are both platform-independent RL-Glue codecs written in Java.  Java 1.5 or higher is required to use them.  Both distributions include a pre-compiled JAR file that has all of the Java classes already built and packaged.  There is also an automated installer program to add the RL-Glue classes as a system extension so they can be used easily by your programs.  It should install on most Linux and Unix platforms, including Mac OS X and on Microsoft Windows.

### User Quick Install ###
```bash

#Unpack the files
>$ tar -zxf java-codec-2.07.tar.gz

#Change to unpacked directory
>$ cd /path/to/java-codec-2.07

#Install the Codec
>$ java -jar JavaRLGlueCodec.jar --install
```

### Official Documentation ###
  * Java Codec Manual [(HTML)](http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Java/docs/html/index.html) [(PDF)](http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Java/docs/JavaCodec.pdf)

### Java Codec Homepage ###
For more information about the Java codec, and others, please visit the RL-Glue homepage:
http://glue.rl-community.org

And the Java Codec Homepage:
http://glue.rl-community.org/Home/Extensions/java-codec

### Past Versions ###
To list all past versions of the Java codec, go here:
http://code.google.com/p/rl-glue-ext/downloads/list?q=language:Java