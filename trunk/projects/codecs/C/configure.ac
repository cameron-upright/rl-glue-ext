AC_INIT(src/RL_client_agent.c)
AC_CONFIG_AUX_DIR(config)
AM_INIT_AUTOMAKE(rlglue_codec_c,1.0-alpha-2)

#Check to make sure we can find the rlglue libraries and headers.  If no, then tell them how to tell us where they are.
AC_CHECK_LIB([rlgluenetdev], [rlConnect], , echo "You must have RL-Glue installed to use this codec.  We could not find the RL-Glue library.  If you have not downloaded it please see http://glue.rl-community.org/. If RL-Glue is not in a standard location please do ./configure LDFLAGS=-L/path/to/rlglue/glue/lib"; exit 1)
AC_CHECK_HEADERS([rlglue/RL_common.h rlglue/Agent_common.h rlglue/Environment_common.h rlglue/RL_glue.h rlglue/network/RL_network.h], ,echo "You must have RL-Glue installed to use this codec.  If you have not downloaded it please see http://glue.rl-community.org/.  We could not find RL-Glue headers: rlglue/RL_common.h rlglue/Agent_common.h rlglue/Environment_common.h rlglue/RL_glue.h rlglue/network/RL_network.h. If they are not in a standard location please do ./configure CFLAGS=-I/path/to/rlglue/glue/headers"; exit 1)

AC_CONFIG_HEADERS(config/config.h)

AC_PATH_PROG(svnversioncommand, svnversion)
if test "X$svnversioncommand" = "X" || test `$svnversioncommand -n '.'` = "exported"; then
AC_DEFINE_UNQUOTED(SVN_VERSION, ["`cat config/saved_svn_version.txt`"], [repository svn version])
else
AC_DEFINE_UNQUOTED(SVN_VERSION, ["`svnversion -n`"], [repository svn version])
`svnversion -n > config/saved_svn_version.txt`
fi


AC_PROG_CC
AC_PROG_INSTALL
AM_PROG_LIBTOOL

AC_OUTPUT(Makefile src/Makefile tests/Makefile)
