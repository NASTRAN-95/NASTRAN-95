#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

#AC_PREREQ([2.69])
AC_INIT([nastran], [0.1.95], [luca.dallolio@gmail.com])
AM_INIT_AUTOMAKE([foreign parallel-tests color-tests])
AC_OUTPUT(Makefile bd/Makefile mis/Makefile mds/Makefile man/Makefile inp/Makefile test/Makefile)

# Checks for programs.
AC_PROG_FC()

# Checks for libraries.
AC_PROG_RANLIB()

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.
AC_LANG_PUSH([Fortran])
AX_CHECK_COMPILE_FLAG([-fallow-invalid-boz], [AM_FCFLAGS="$AM_FCFLAGS -fallow-invalid-boz"])
AC_SUBST([AM_FCFLAGS])
AC_LANG_POP([Fortran])

# Checks for library functions.

AC_OUTPUT
