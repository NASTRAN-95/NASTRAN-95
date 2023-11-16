# what flags you want to pass to the Fortran compiler & linker
#OPT_OPTS ?= unknown
#AM_FFLAGS = $(OPT_OPTS)
#FFLAGS = -w -fno-range-check -fno-automatic -fcray-pointer -std=legacy
FCFLAGS = -g -ffree-line-length-none -w -fno-range-check -fno-automatic -fcray-pointer -std=legacy -J../include

# this lists the binaries to produce, the (non-PHONY, binary) targets in
# the previous manual Makefile
bin_PROGRAMS = nastran
nastran_SOURCES = \
bckrec.f90  close.f90   dbmio.f90   defcor.f90  dsgefl.f90  dsiodd.f90  dsrdmb.f90  dsskfb.f90  dsxfsz.f90  exfort.f90  intpk.f90   nasopn.f90  read.f90    tdate.f90   zntpki.f90 \
bldpk.f90   corwds.f90  dbmlbk.f90  dsblpk.f90  dsgncl.f90  dsipk1.f90  dsrdnb.f90  dsskff.f90  dummy.f90   filpos.f90  intpki.f90  nastim.f90  rectyp.f90  umffd.f90 \
bldpki.f90  cputim.f90  dbmmgr.f90  dsbpnk.f90  dsgnop.f90  dsmsg1.f90  dsrdpb.f90  dsskrc.f90  emgsoc.f90  forwrt.f90  k2b.f90     numtyp.f90  rewind.f90  unpack.f90 \
bldpkn.f90  dbmalb.f90  dbmmov.f90  dsbrc1.f90  dsgnrd.f90  dsmsg.f90   dsread.f90  dsspos.f90  endget.f90  fwdrec.f90  khrbcd.f90  open.f90    rfopen.f90  waltim.f90 \
bpack.f90   dbmdia.f90  dbmnam.f90  dsclos.f90  dsgnwr.f90  dsocff.f90  dsrlse.f90  dsupkc.f90  endgtb.f90  getstb.f90  khrfn1.f90  pack.f90    savpos.f90  write.f90 \
btstrp.f90  dbmdmp.f90  dbmrel.f90  dscpos.f90  dshxdd.f90  dsopen.f90  dssdcb.f90  dswrit.f90  endput.f90  getstr.f90  khrfn4.f90  putstr.f90  sgino.f90   wrtblk.f90 \
bunpak.f90  dbmfdp.f90  dbmrlb.f90  dsefwr.f90  dshxdp.f90  dsopff.f90  dssend.f90  dswrnb.f90  eof.f90     geturn.f90  klock.f90   qopen.f90   skpfil.f90  wrtfmt.f90 \
bunpk.f90   dbmint.f90  dbmstf.f90  dsfwr1.f90  dsinqr.f90  dsprcl.f90  dssize.f90  dswrt1.f90  errtrc.f90  gnfiat.f90  mapfns.f90  rdblk.f90   sofio.f90   zblpki.f90 \
nastrn.f90

nastran_LDADD = ../mis/libnastran.a ../bd/libnastbd.a
