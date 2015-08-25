**Notes:** To use most of the codecs, you must have the RL-Glue Core installed first.



# Available Downloads (only one is required) #
## All Platforms : Source Distribution ##
> [RL-Glue Core Project Source Distribution 3.04 (.tar.gz)](http://rl-glue-ext.googlecode.com/files/rlglue-3.04.tar.gz) [(File Details)](http://code.google.com/p/rl-glue-ext/downloads/detail?name=rlglue-3.04.tar.gz)
This is a platform-independent project that was built using the GNU Autotools.  It should install on most Linux and Unix platforms, including Mac OS X (Intel and PowerPC) and on Microsoft Windows under Cygwin.

## Mac OS X : Disk Image (Intel Only) ##
**NOTE:** This disk image is not snow leopard compatible. In fact, RL-Glue and the C/C++ needs to be reinstalled for Snow Leopard from source.  Will fix soon.
> [Mac OS X RL-Glue Core Project and C/C++ Codec Disk Image 3.04](http://rl-glue-ext.googlecode.com/files/RL-Glue-3.04-and-C-Codec.dmg) [(File Details)](http://code.google.com/p/rl-glue-ext/downloads/detail?name=RL-Glue-3.04-and-C-Codec.dmg)
This is an OS X disk image with familiar installer that will install both RL-Glue Core and the C/C++ Codec onto your Mac.  Intel Mac only. You do not need to install the C/C++ Codec separately if you use this option.
It contains:
  * Installer program (.pkg) for pre-compiled libraries, headers, and rl\_glue executable socket server.
  * Uninstall script for removing all of the above
  * The documentation for RL-Glue and the C/C++ codec(PDF)
  * The examples for RL-Glue and the C/C++ codec

## Windows (32-bit) : rl\_glue.exe ##
> [Windows Binary .exe RL-Glue Core Project 3.04](http://rl-glue-ext.googlecode.com/files/RL-Glue-Windows-Binary-3.04.zip) [(File Details)](http://code.google.com/p/rl-glue-ext/downloads/detail?name=RL-Glue-Windows-Binary-3.04.zip)
This is a 32-bit Windows binary package that does NOT require Cygwin to run.  No installation involved.  This does not help you if you want to write code using the C/C++ codec, but it will work for all other codecs.

It contains:
  * rl\_glue.exe executable socket server
  * The documentation for RL-Glue (PDF)
  * A README.txt file with some hints on how to put rl\_glue.exe into your path

---

## Linux .deb and .rpm packages ##


### Debian/Ubuntu : DEB packages ###
> [RL-Glue Core .deb package (32-bit) 3.04](http://rl-glue-ext.googlecode.com/files/rl-glue_3.04-1_i386.deb) [(File Details)](http://code.google.com/p/rl-glue-ext/downloads/detail?name=rl-glue_3.04-1_i386.deb)


> [RL-Glue Core .deb package (64-bit) 3.04](http://rl-glue-ext.googlecode.com/files/rl-glue_3.04-1_amd64.deb) [(File Details)](http://code.google.com/p/rl-glue-ext/downloads/detail?name=rl-glue_3.04-1_amd64.deb)

This is an Debian/Ubuntu package with familiar installer procedure that will install RL-Glue Core

It contains:
  * Installer package (.deb) for pre-compiled libraries, headers, and rl\_glue executable socket server.
  * Can be uninstalled with apt
  * The documentation for RL-Glue (HTML)
  * Man page (man rl\_glue)
  * The examples for RL-Glue
### Redhat and friends : RPM packages ###
> [RL-Glue Core .rpm package (32-bit) 3.04](http://rl-glue-ext.googlecode.com/files/rl-glue-3.04-1.i386.rpm) [(File Details)](http://code.google.com/p/rl-glue-ext/downloads/detail?name=rl-glue-3.04-1.i386.rpm)


> [RL-Glue Core .rpm package (64-bit) 3.04](http://rl-glue-ext.googlecode.com/files/rl-glue-3.04-1.x86_64.rpm) [(File Details)](http://code.google.com/p/rl-glue-ext/downloads/detail?name=rl-glue-3.04-1.x86_64.rpm)

This is an RPM package with familiar installer procedure that will install RL-Glue Core

It contains:
  * Installer package (.rpm) for pre-compiled libraries, headers, and rl\_glue executable socket server.
  * Can be uninstalled with rpm tools
  * The documentation for RL-Glue (HTML)
  * Man page (man rl\_glue)
  * The examples for RL-Glue
# Source Distribution Quick Install Guide #
```bsh

#Unpack the files
> tar -zxf GLUE-DEV-FILE-BASE.tar.gz

#Change to unpacked directory
> cd /path/to/GLUE-DEV-FILE-BASE

#Configure the installation (for advanced options see the manual)
> ./configure

#Build the software
> make

#Install the software (requires admin privileges)
> sudo make install
```

# Official Documentation #
  * RL-Glue Overview [(HTML)](http://rl-glue.googlecode.com/svn/trunk/docs/html/index.html) [(PDF)](http://rl-glue.googlecode.com/svn/trunk/docs/Glue-Overview.pdf)
  * RL-Glue Technical Manual [(HTML)](http://rl-glue.googlecode.com/svn/trunk/docs/tech_html/index.html) [(PDF)](http://rl-glue.googlecode.com/svn/trunk/docs/TechnicalManual.pdf)

# RL-Glue Core Homepage #
For more information about RL-Glue and codecs, please visit the homepage:
http://glue.rl-community.org

And the RL-Glue Core Homepage:
http://glue.rl-community.org/Home/rl-glue

# Change Log #
## Version 3.04 ##
- Fixed a bug in the RL\_network.c code where it would dereference a null pointer if an empty abstract type was sent into rlCopyADTToBuffer.  This bug would sometimes (some codecs) manifest if an episode was ended on the first call to env\_step.  Thanks to Jason Williams for identifying this problem.

# Past Versions #
To list all past versions of the RLGlue Core project, go here:
http://code.google.com/p/rl-glue-ext/downloads/list?q=language:C