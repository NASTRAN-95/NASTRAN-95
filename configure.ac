#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

#AC_PREREQ([2.69])
AC_INIT([nastran], [0.1.95], [luca.dallolio@gmail.com])
AM_INIT_AUTOMAKE([foreign parallel-tests color-tests])
AC_OUTPUT(Makefile bd/Makefile mis/Makefile mds/Makefile man/Makefile inp/Makefile test/Makefile)

# Checks for programs.
AC_PROG_F77()

# Checks for libraries.
AC_PROG_RANLIB()

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.
AC_LANG_PUSH([Fortran 77])
AX_CHECK_COMPILE_FLAG([-fallow-invalid-boz], [AM_FFLAGS="$AM_FFLAGS -fallow-invalid-boz"])
AC_SUBST([AM_FFLAGS])
AC_LANG_POP([Fortran 77])

# Checks for library functions.

AC_OUTPUT
