#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(Stump Window Manager, esyscmd(grep :version paulownia.asd | cut -d\" -f2 | tr -d \\n),  dbjergaard@gmail.com, sabetts@gmail.com)

AC_SUBST(MODULE_DIR)
AC_SUBST(LISP)
AC_SUBST(PAULOWNIA_ASDF_DIR)

# Checks for programs.
AC_ARG_WITH(lisp,    [  --with-lisp=IMPL        use the specified lisp (sbcl or ccl)], LISP=$withval, LISP="any")
AC_ARG_WITH(ros,     [  --with-ros=PATH         specify location of roswell, ROS_PATH=$withval, ROS_PATH=""])

AC_ARG_WITH(module-dir,
                     [  --with-module-dir=PATH specify location of contrib modules],
                     MODULE_DIR=$withval, MODULE_DIR="${HOME}/.paulownia.d/modules")

PAULOWNIA_ASDF_DIR="`pwd`"

if test -x "$ROS_PATH"; then
   ROS=$ROS_PATH
   AC_MSG_CHECKING([for ros])
   AC_MSG_RESULT($ROS)
else
   AC_PATH_PROG([ROS], ros,"")
fi
HAS_SBCL="$(ros list installed | awk '{print $1}' | grep sbcl)"
HAS_CCL="$(ros list installed | awk '{print $1}' | grep ccl)"

if test "x$LISP" = "xany"; then
   if test "x$HAS_SBCL"="xsbcl-bin"; then
      LISP=sbcl-bin
   elif test "x$HAS_CCL"="xccl-bin"; then
      LISP=ccl
   fi
fi

if test "x$LISP" = "xany"; then
   AC_MSG_ERROR([*** No lisp is available. Install one with "$ROS install foo"])
fi

AC_MSG_NOTICE([Using $LISP with ros: ros use $LISP])

# check for makeinfo

AC_CHECK_PROG(MAKEINFO,makeinfo,yes,no)

if test "$MAKEINFO" = "no"; then
   AC_MSG_WARN([Please install makeinfo for the manual.])
fi
# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.
AC_OUTPUT(Makefile)
AC_OUTPUT(paulownia.ros)
AC_OUTPUT(paulownia-tests.asd)
