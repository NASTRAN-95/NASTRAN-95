# what flags you want to pass to the Fortran compiler & linker#
OPT_OPTS ?= unknown
AM_FCFLAGS = $(OPT_OPTS)
FCFLAGS = -g -ffree-line-length-none -w -fno-range-check -fno-automatic -fcray-pointer -std=legacy

# this lists the binaries to produce, the (non-PHONY, binary) targets in
# the previous manual Makefile
noinst_LIBRARIES = libnastbd.a
libnastbd_a_SOURCES = dpdcbd.f90  gptabd.f90  ifx4bd.f90  of1pbd.f90	of5pbd.f90  of9pbd.f90  pla4bd.f90  sma1bd.f90 \
exiobd.f90  ifp3bd.f90  ifx5bd.f90  of2pbd.f90	of6pbd.f90  ofp1bd.f90  plotbd.f90  sma2bd.f90 \
ferfbd.f90  ifx1bd.f90  ifx6bd.f90  of3pbd.f90	of7pbd.f90  ofp5bd.f90  readbd.f90  ta1abd.f90 \
flbbd.f90   ifx2bd.f90  ifx7bd.f90  of3sbd.f90	of7sbd.f90  ofsnbd.f90  sdr2bd.f90  tabfbd.f90 \
gp3bd.f90   ifx3bd.f90  itembd.f90  of4pbd.f90	of8pbd.f90  ofssbd.f90  semdbd.f90  vdrbd.f90
