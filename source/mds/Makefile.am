# what flags you want to pass to the Fortran compiler & linker
#OPT_OPTS ?= unknown
#AM_FFLAGS = $(OPT_OPTS)
#FFLAGS = -w -fno-range-check -fno-automatic -fcray-pointer -std=legacy
FFLAGS = -g -w -fno-range-check -fno-automatic -fcray-pointer -std=legacy -J../include

# this lists the binaries to produce, the (non-PHONY, binary) targets in
# the previous manual Makefile
bin_PROGRAMS = nastran
nastran_SOURCES = \
bckrec.f  close.f   dbmio.f   defcor.f  dsgefl.f  dsiodd.f  dsrdmb.f  dsskfb.f  dsxfsz.f  exfort.f  intpk.f   nasopn.f  read.f    tdate.f   zntpki.f \
bldpk.f   corwds.f  dbmlbk.f  dsblpk.f  dsgncl.f  dsipk1.f  dsrdnb.f  dsskff.f  dummy.f   filpos.f  intpki.f  nastim.f  rectyp.f  umffd.f \
bldpki.f  cputim.f  dbmmgr.f  dsbpnk.f  dsgnop.f  dsmsg1.f  dsrdpb.f  dsskrc.f  emgsoc.f  forwrt.f  k2b.f     numtyp.f  rewind.f  unpack.f \
bldpkn.f  dbmalb.f  dbmmov.f  dsbrc1.f  dsgnrd.f  dsmsg.f   dsread.f  dsspos.f  endget.f  fwdrec.f  khrbcd.f  open.f    rfopen.f  waltim.f \
bpack.f   dbmdia.f  dbmnam.f  dsclos.f  dsgnwr.f  dsocff.f  dsrlse.f  dsupkc.f  endgtb.f  getstb.f  khrfn1.f  pack.f    savpos.f  write.f \
btstrp.f  dbmdmp.f  dbmrel.f  dscpos.f  dshxdd.f  dsopen.f  dssdcb.f  dswrit.f  endput.f  getstr.f  khrfn4.f  putstr.f  sgino.f   wrtblk.f \
bunpak.f  dbmfdp.f  dbmrlb.f  dsefwr.f  dshxdp.f  dsopff.f  dssend.f  dswrnb.f  eof.f     geturn.f  klock.f   qopen.f   skpfil.f  wrtfmt.f \
bunpk.f   dbmint.f  dbmstf.f  dsfwr1.f  dsinqr.f  dsprcl.f  dssize.f  dswrt1.f  errtrc.f  gnfiat.f  mapfns.f  rdblk.f   sofio.f   zblpki.f \
nastrn.f

nastran_LDADD = ../mis/libnastran.a ../bd/libnastbd.a
