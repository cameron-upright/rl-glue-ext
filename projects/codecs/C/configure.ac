AC_INIT(rlglue_codec_c,1.0-beta-1)
AC_CONFIG_SRCDIR(src/RL_client_agent.c)
AC_CONFIG_AUX_DIR(config)
AM_INIT_AUTOMAKE([1.9 tar-ustar])

AC_MSG_CHECKING(if custom rl-glue path was specified)
AC_ARG_WITH(rl-glue,
    [  --with-rl-glue=path      Look for rl-glue in path ],
    [LDFLAGS="$LDFLAGS -L$withval/lib"
	CFLAGS="$CFLAGS -I$withval/include"
     CPPFLAGS="$CPPFLAGS -I$withval/include"
	AC_MSG_RESULT(yes)
	],[AC_MSG_RESULT(no)])

#Check to make sure we can find the rlglue libraries and headers.  If no, then tell them how to tell us where they are.
AC_CHECK_LIB([rlgluenetdev], [rlConnect],[], AC_MSG_ERROR(RL-Glue library not found. You must have RL-Glue installed to use this codec. If you have not downloaded it please see http://glue.rl-community.org/ .  If you do have it installed in a non-standard location you may need to use the --with-rl-glue=/path/to/rlglue command line switch to specify where the rl-glue root is located.))


AC_CHECK_HEADERS([rlglue/RL_common.h rlglue/Agent_common.h rlglue/Environment_common.h rlglue/RL_glue.h rlglue/network/RL_network.h],[] ,AC_MSG_ERROR(RL-Glue library not found. You must have RL-Glue installed to use this codec. If you have not downloaded it please see http://glue.rl-community.org/ .  If you do have it installed in a non-standard location, you may need to use the --with-rl-glue=/path/to/rlglue command line switch to specify where the rl-glue root is located.),[[#ifdef HAVE_RLGLUE_RL_COMMON_H
# include <rlglue/RL_common.h>
#endif
]])

AC_CHECK_HEADERS([ieeefp.h, sys/time.h])
AC_CHECK_FUNCS([isnan isinf gettimeofday])



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
