**Notes:** To use this or any other codec, you must have RL-Glue Core installed first.



# Available Downloads (only one is required) #

## All Platforms : Source Distribution ##
> [C/C++ Codec Source Distribution 2.0 (.tar.gz)](http://rl-glue-ext.googlecode.com/files/c-codec-2.0.tar.gz) [(File Details)](http://code.google.com/p/rl-glue-ext/downloads/detail?name=c-codec-2.0.tar.gz)
> This is a platform-independent project that was built using the GNU Autotools.  It should install on most Linux and Unix platforms, including Mac OS X (Intel and PowerPC) and on Microsoft Windows under Cygwin.

## Mac OS X : Disk Image (Intel Only) ##
> [Mac OS X RL-Glue Core Project and C/C++ Codec Disk Image 3.04](http://rl-glue-ext.googlecode.com/files/RL-Glue-3.04-and-C-Codec.dmg) [(File Details)](http://code.google.com/p/rl-glue-ext/downloads/detail?name=RL-Glue-3.04-and-C-Codec.dmg)
This is an OS X disk image with familiar installer that will install both RL-Glue Core and the C/C++ Codec onto your Mac.  Intel Mac only. You do not need to install the C/C++ Codec separately if you use this option.
It contains:
  * Installer program (.pkg) for pre-compiled libraries, headers, and rl\_glue executable socket server.
  * Uninstall script for removing all of the above
  * The documentation for RL-Glue and the C/C++ codec(PDF)
  * The examples for RL-Glue and the C/C++ codec

## Linux .deb and .rpm packages ##


### Debian/Ubuntu : DEB packages ###
> [C/C++ Codec .deb package (32-bit) 2.0](http://rl-glue-ext.googlecode.com/files/rl-glue-c-codec_2.0-1_i386.deb) [(File Details)](http://code.google.com/p/rl-glue-ext/downloads/detail?name=rl-glue-c-codec_2.0-1_i386.deb)


> [C/C++ Codec .deb package (64-bit) 2.0](http://rl-glue-ext.googlecode.com/files/rl-glue-c-codec_2.0-1_amd64.deb) [(File Details)](http://code.google.com/p/rl-glue-ext/downloads/detail?name=rl-glue-c-codec_2.0-1_amd64.deb)

This is an Debian/Ubuntu package with familiar installer procedure that will install RL-Glue C/C++ Codec.

It contains:
  * Installer package (.deb) for pre-compiled libraries and headers
  * Can be uninstalled with apt
  * The documentation for the C/C++ codec (HTML)
  * The examples for C/C++ codec
### Redhat and friends : RPM packages ###
> [C/C++ Codec .rpm package (32-bit) 2.0](http://rl-glue-ext.googlecode.com/files/rl-glue-c-codec-2.0-1.i386.rpm) [(File Details)](http://code.google.com/p/rl-glue-ext/downloads/detail?name=rl-glue-c-codec-2.0-1.i386.rpm)


> [C/C++ Codec .rpm package (64-bit) 2.0](http://rl-glue-ext.googlecode.com/files/rl-glue-c-codec-2.0-1.x86_64.rpm) [(File Details)](http://code.google.com/p/rl-glue-ext/downloads/detail?name=rl-glue-c-codec-2.0-1.x86_64.rpm)

This is an RPM package with familiar installer procedure that will install RL-Glue C/C++ codec.

It contains:
  * Installer package (.rpm) for pre-compiled libraries and headers
  * Can be uninstalled with rpm tools
  * The documentation for the C/C++ codec (HTML)
  * The examples for the C/C++ codec
# Source Distribution Quick Install Guide #
```bsh

#Unpack the files
> tar -zxf c-codec-2.0-RC-FINAL-1.tar.gz

#Change to unpacked directory
> cd /path/to/c-codec-2.0-RC-FINAL-1

#Configure the installation (for advanced options see the manual)
> ./configure

#Build the software
> make

#Install the software (requires admin privileges)
> sudo make install
```

# Official Documentation #
  * C/C++ Codec Manual [(HTML)](http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/C/docs/html/index.html) [(PDF)](http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/C/docs/C-Codec.pdf)

# C/C++ Codec Homepage #
For more information about the C/C++ codec, and others, please visit the RL-Glue homepage:
http://glue.rl-community.org

And the C/C++ Codec Homepage:
http://glue.rl-community.org/Home/Extensions/c-c-codec