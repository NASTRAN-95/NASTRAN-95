# what flags you want to pass to the Fortran compiler & linker
OPT_OPTS ?= unknown
AM_FCFLAGS = $(OPT_OPTS)
FCFLAGS = -ffree-line-length-none -w -fno-range-check -fno-automatic -fcray-pointer -std=legacy -J../include

# this lists the binaries to produce, the (non-PHONY, binary) targets in
# the previous manual Makefile
noinst_LIBRARIES = libnastran.a
libnastran_a_SOURCES = \
a42a8.f90     area.f90    cnorm.f90   dkls.f90    exo2.f90    frlgb.f90   hdvsr.f90   kpanel.f90     mma401.f90  ofppun.f90  psqad1.f90   rotat.f90    smccrd.f90   strp12.o  trlgb.f90 \
a42a8.o     arrm.f90    cnstdd.f90  dlamby.f90  extern.f90  frlg.f90    head.f90    kpltst.f90     mma402.f90  ofprf1.f90  psqad2.f90   rowdyz.f90   smccrs.f90   strpl1.f90  trlgc.f90 \
a82int.f90    ascm01.f90  cnstrc.f90  dlamg.f90   f6211.f90   frmax.f90   hess1.f90   kqdmem.f90     mma403.f90  ofprf2.f90  psqdm1.f90   rsetup.f90   smchlp.f90   strpl1.o  trlgd.f90 \
a82int.o    ascm02.f90  com12.f90   dlbpt2.f90  f89.f90     frmlta.f90  hess2.f90   kqdplt.f90     mma404.f90  ofprs1.f90  psqdm.f90    rsort.f90    smcomp.f90   strpts.f90  trlg.f90 \
adr.f90       ascm03.f90  comb1.f90   dlkapm.f90  fa1.f90     frmltd.f90  hmat.f90    krod.f90       mma411.f90  ofprs2.f90  psqpl1.f90   ruler.f90    smcout.f90   strqd1.f90  trmemd.f90 \
adri.f90      ascm04.f90  comb2.f90   dloop.f90   fa1ke.f90   frmlt.f90   hring.f90   krshft.f90     mma412.f90  ofpsn1.f90  psrod.f90    rzintd.f90   smcph1.f90   strqd2.f90  trmems.f90 \
adri.o      ascm05.f90  combin.f90  dlpt2.f90   fa1k.f90    frmltx.f90  hsbg.f90    kslot.f90      mma413.f90  ofpss1.f90  psta.f90     rzints.f90   smcph2.f90   strscn.f90  trnsp.f90 \
adr.o       ascm06.f90  combo.f90   dmatrs.f90  fa1pka.f90  frr1a1.f90  iapd.f90    ksolid.f90     mma414.f90  ofrf2s.f90  pstamg.f90   sadd.f90     smcrtr.f90   strsl1.f90  trnsps.f90 \
adrprt.f90    ascm07.f90  comect.f90  dmatrx.f90  fa1pke.f90  frrd1c.f90  idf1.f90    ktetra.f90     mma4.f90    ofrs2s.f90  pstpl1.f90   sadotb.f90   smcspl.f90   strsl2.f90  trplmd.f90 \
adrprt.o    ascm08.f90  comugv.f90  dmfgr.f90   fa1pki.f90  frrd1d.f90  idf2.f90    ktrapr.f90     mma.f90     ofsplt.f90  pstq1.f90    sanorm.f90   smleig1.f90  strslv.f90  trplms.f90 \
af.f90        ascm09.f90  cone.f90    dmpalt.f90  fa1pkv.f90  frrd1f.f90  idist.f90   ktrbsc.f90     mmarc1.f90  oldel1.f90  pstq2.f90    saxb.f90     smleig.f90   stube1.f90  trplt.f90 \
af.o        ascm10.f90  conm1d.f90  dmpfil.f90  fa2.f90     frrd2.f90   idplot.f90  ktriqd.f90     mmarc2.f90  oldel2.f90  pstrb1.f90   saxif1.f90   smmats.f90   sub1.f90    trttem.f90 \
ai.f90        ascm11.f90  conm1s.f90  dmpyad.f90  factor.f90  frrd.f90    ifb2ar.f90  ktrirg.f90     mmarc3.f90  oldel3.f90  pstri1.f90   saxif2.f90   smp1.f90     suba.f90    tshear.f90 \
ai.o        ascm12.f90  conm2d.f90  dmpy.f90    factru.f90  frsw2.f90   ifp1b.f90   ktrm6d.f90     mmarc4.f90  olplot.f90  pstri2.f90   sbar1.f90    smp2.f90     subbb.f90   tspl1d.f90 \
ais.f90       ascm13.f90  conm2s.f90  dnorm.f90   failrs.f90  frsw.f90    ifp1c.f90   ktrm6s.f90     mmarm1.f90  onetwo.f90  pstrm1.f90   sbar2.f90    smpyad.f90   subb.f90    tspl1s.f90 \
ais.o       asdmap.f90  conmsg.f90  dpd1.f90    failur.f90  ftube.f90   ifp1d.f90   ktrmem.f90     mmarm2.f90  onlins.f90  pstrm.f90    sbspl2.f90   smsg.f90     subcc.f90   tspl2d.f90 \
akapm.f90     aspro.f90   contor.f90  dpd2.f90    fbs1.f90    fvrs1a.f90  ifp1e.f90   ktrpld.f90     mmarm3.f90  opt2a.f90   pstrq2.f90   scalar.f90   snpdf.f90    subc.f90    tspl2s.f90 \
akapm.o     asycon.f90  copy.f90    dpd3.f90    fbs21.f90   fvrs1b.f90  ifp1.f90    ktrpls.f90     mmarm4.f90  opt2b.f90   pthbdy.f90   scaled.f90   sofcls.f90   sub.f90     tspl3d.f90 \
akappa.f90    ateig.f90   cpyfil.f90  dpd4.f90    fbs2.f90    fvrs1c.f90  ifp1f.f90   ktrplt.f90     modac1.f90  opt2c.f90   ptintr.f90   scalex.f90   sofi.f90     subi.f90    tspl3s.f90 \
akappa.o    autock.f90  cpystr.f90  dpd5.f90    fbs3.f90    fvrs1d.f90  ifp1g.f90   ktshld.f90     modac2.f90  opt2d.f90   pull.f90     scan.f90     sofint.f90   subpb.f90   ttlpge.f90 \
akp2.f90      autosv.f90  crdrd2.f90  dpdaa.f90   fbs4.f90    fvrs1e.f90  ifp1h.f90   ktshls.f90     modacc.f90  optp1a.f90  push.f90     scat.f90     sofo.f90     subp.f90    ttordr.f90 \
akp2.o      axis10.f90  crdrd.f90   dpd.f90     fbs.f90     fvrs2a.f90  ifp1pc.f90  ktube.f90      moda.f90    optp1b.f90  q2bcd.f90    sce1.f90     sofopn.f90   subph1.f90  ttrapr.f90 \
alamda.f90    axis.f90    create.f90  dplot.f90   fbsf.f90    fvrst1.f90  ifp1s.f90   lamx.f90       modb.f90    optp1c.f90  q2bcs.f90    scheme.f90   sofsiz.f90   summ.f90    ttrirg.f90 \
alamda.o    axloop.f90  criggp.f90  dpltst.f90  fbsi1.f90   fvrst2.f90  ifp1xy.f90  line10.f90     modc.f90    optp1d.f90  q2trmd.f90   scone1.f90   softoc.f90   sumphi.f90  tubed.f90 \
alg01.f90     bandit.f90  crspld.f90  dppsb.f90   fbsi2.f90   fwmw.f90    ifp3b.f90   line.f90       mplprt.f90  optpr1.f90  q2trms.f90   scone2.f90   softrl.f90   suplt.f90   tubes.f90 \
alg01.o     bard.f90    crspld.o  dpps.f90    fbsi3.f90   fzy2.f90    ifp3.f90    linein.f90     mpy3a.f90   optpr2.f90  q4bmgd.f90   scone3.f90   sofut.f90    suread.f90  tvor.f90 \
alg02.f90     bar.f90     crspls.f90  dpse2.f90   fbsi4.f90   gauss2.f90  ifp4b.f90   linel.f90      mpy3b.f90   optpx1.f90  q4bmgs.f90   scrlm.f90    solid.f90    suwrt.f90   twistd.f90 \
alg02.o     bars.f90    crspls.o  dpse3.f90   fbsi.f90    geloop.f90  ifp4c.f90   linkup.f90     mpy3c.f90   optpx.f90   q4gmgs.f90   sd2rhd.f90   solve1.f90   switch.f90  twists.f90 \
alg03.f90     basglb.f90  crsub.f90   dpse4.f90   fbsint.f90  gencos.f90  ifp4e.f90   loadsu.f90     mpy3dr.f90  order.f90   q4nrms.f90   sdcin.f90    solve.f90    symbol.f90  type10.f90 \
alg04.f90     bdat01.f90  crsub.o   dpzy.f90    fbsinv.f90  gend.f90    ifp4.f90    locpt.f90      mpy3.f90    ortck.f90   q4shps.f90   sdcins.f90   solver.f90   t3bgbs.f90  typflt.f90 \
alg05.f90     bdat02.f90  csqrtx.f90  dqdmem.f90  fbsrdm.f90  gendsb.f90  ifp4f.f90   lodapp.f90     mpy3ic.f90  ortho.f90   qdmem.f90    sdcmm.f90    sortdg.f90   t3bmgd.f90  typint.f90 \
alg06.f90     bdat03.f90  csqrtx.o  dquad.f90   fcurl.f90   genpar.f90  ifp4g.f90   logfil.f90     mpy3nu.f90  oscxrf.f90  qdmm1d.f90   sdcmps.f90   sort.f90     t3bmgs.f90  unpscr.f90 \
alg07.f90     bdat04.f90  csub.f90    draw.f90    fdit.f90    genvec.f90  ifp5a.f90   loglog.f90     mpy3oc.f90  outmsc.f90  qdmm1.f90    sdcmq.f90    sorti.f90    t3gemd.f90  upart.f90 \
alg08.f90     bdat05.f90  csub.o    drkapm.f90  fdsub.f90   getblk.f90  ifp5.f90    lprops.f90     mpy3p.f90   outpak.f90  qdmm1s.f90   sdcom1.f90   spanl1.f90   t3gems.f90  upcase.f90 \
alg09.f90     bdat06.f90  csumm.f90   drod.f90    fdvect.f90  getdef.f90  ifpdco.f90  lsplin.f90     mpy4t.f90   outpt1.f90  qdmm2d.f90   sdcom2.f90   spanl2.f90   t3pl4d.f90  usrmsg.f90 \
alg10.f90     betrns.f90  csumm.o   drwchr.f90  feer1.f90   gfbs.f90    ifp.f90     magbdy.f90     mpya3d.f90  outpt2.f90  qdmm2.f90    sdcom3.f90   splt10.f90   t3pl4s.f90  valvec.f90 \
alg11.f90     bfsmat.f90  cthmck.f90  ds1a.f90    feer2.f90   gfscom.f90  ifpmdc.f90  magpha.f90     mpyad.f90   outpt3.f90  qdmm2s.f90   sdcom4.f90   sptchk.f90   t3setd.f90  varian.f90 \
alg12.f90     bgrid.f90   cthmck.o  ds1b.f90    feer3.f90   gfsdir.f90  ifppar.f90  Makefile     mpyado.f90  outpt4.f90  qdplt.f90    sdcomp.f90   sqdm11.f90   t3sets.f90  vdra.f90 \
alg13.f90     bint.f90    ctrnsp.f90  ds1etd.f90  feer3x.f90  gfshc.f90   ifppvc.f90  Makefile.am  mpydri.f90  outpt5.f90  qhbdy.f90    sdcompx.f90  sqdm12.f90   ta1a.f90    vdrb.f90 \
alg14.f90     biotsv.f90  ctrnsp.o  ds1.f90     feer4.f90   gfsh.f90    ifs1p.f90   Makefile.in  mpyl.f90    outpt.f90   qloadl.f90   sdcout.f90   sqdm21.f90   ta1b.f90    vdr.f90 \
alg15.f90     bishel.f90  curcas.f90  dschk.f90   feerdd.f90  gfsma.f90   ifs2p.f90   makmcb.f90     mpyq.f90    page2.f90   qparam.f90   sdhtf1.f90   sqdm22.f90   ta1ca.f90   vec.f90 \
alg16.f90     bislc2.f90  curcas.o  dshear.f90  feer.f90    gfsmo2.f90  ifs3p.f90   mapset.f90     mqdplt.f90  page.f90    qparmd.f90   sdhtf2.f90   sqdme1.f90   ta1c.f90    vecprt.f90 \
alg17.f90     bisloc.f90  curv1.f90   dsmg1.f90   ferfbs.f90  gfsmod.f90  ifs4p.f90   maskn.f90      mred1a.f90  pakcol.f90  qparmr.f90   sdhtff.f90   sqdpl1.f90   ta1cpd.f90  viscd.f90 \
alg18.f90     bitpat.f90  curv1.o   dsmg2.f90   ferltd.f90  gfsmrg.f90  ifs5p.f90   masstq.f90     mred1b.f90  param.f90   qriter1.f90  sdr1a.f90    sqrtm.f90    ta1cps.f90  viscs.f90 \
alg19.f90     bmg.f90     curv2.f90   dstroy.f90  ferlts.f90  gfsmt.f90   ifte2.f90   matck.f90      mred1c.f90  paraml.f90  qriter.f90   sdr1b.f90    squd41.f90   ta1etd.f90  wavey.f90 \
alg1.f90      bmgtns.f90  curv2.o   dtranp.f90  ferrdm.f90  gfsptn.f90  ifte4.f90   matdum.f90     mred1d.f90  partn1.f90  quad4d.f90   sdr1.f90     squd42.f90   ta1.f90     wilvec1.f90 \
alg25.f90     border.f90  curv3.f90   dtrbsc.f90  ferrdm.o  gfsspc.f90  ift.f90     matgen.f90     mred1e.f90  partn2.f90  quad4s.f90   sdr2aa.f90   srod1.f90    ta1h.f90    wilvec.f90 \
alg26.f90     bound.f90   curv3.o   dtria.f90   ferswd.f90  gfstrn.f90  iftg.f90    matgpr.f90     mred1.f90   partn3.f90  qvol.f90     sdr2a.f90    srod2.f90    tabfmt.f90  wplt10.f90 \
alg29.f90     bread.f90   curv.f90    dtrmem.f90  ferswd.o  gfswch.f90  ihexd.f90   matprn.f90     mred2a.f90  partn.f90   rand1.f90    sdr2b.f90    ss2d81.f90   table5.f90  wrtmsg.f90 \
alg2.f90      bseqgp.f90  curvit.f90  dtshld.f90  fersws.f90  gibstk.f90  ihex.f90    matprt.f90     mred2b.f90  pcoord.f90  rand2.f90    sdr2c.f90    ss2d82.f90   tablev.f90  wrtprt.f90 \
alg30.f90     bug.f90     curvit.o  dtshls.f90  fersws.o  gi.f90      ihexsd.f90  matvc2.f90     mred2c.f90  permut.f90  rand3.f90    sdr2d.f90    ssg1a.f90    tabpch.f90  wrttrl.f90 \
alg3.f90      calcv.f90   curv.o    dumerg.f90  ferxtd.f90  giggks.f90  ihexs.f90   matvec.f90     mred2d.f90  perpec.f90  rand5.f90    sdr2e.f90    ssg1.f90     tabprt.f90  xcei.f90 \
alg4.f90      case.f90    curvps.f90  dumod1.f90  ferxtd.o  gigtka.f90  ihexss.f90  matwrt.f90     mred2e.f90  pexit.f90   rand6.f90    sdr2.f90     ssg2a.f90    tabpt.f90   xchk.f90 \
alg5.f90      casege.f90  cxloop.f90  dumod2.f90  ferxts.f90  gigtkg.f90  incore.f90  maxdgr.f90     mred2.f90   phdmia.f90  rand7.f90    sdr3a.f90    ssg2b.f90    tapbit.f90  xclean.f90 \
alg6.f90      cdcomp.f90  cxtrny.f90  dumod3.f90  ferxts.o  ginofl.f90  incro.f90   mbamg.f90      mred2f.f90  pidck.f90   rand8.f90    sdr3.f90     ssg2c.f90    termsd.f90  xcsa.f90 \
alg7.f90      cdetm2.f90  cyct1.f90   dumod4.f90  ff100.f90   gipsst.f90  initl2.f90  mbbslj.f90     mred2g.f90  piklvl.f90  random.f90   sdrchk.f90   ssg2.f90     termss.f90  xdcode.f90 \
alg8.f90      cdetm.f90   cyct2a.f90  dumod5.f90  ff100.o   givens.f90  initl2.o  mbcap.f90      mred2h.f90  pkbar.f90   rbmg1.f90    sdretd.f90   ssg3a.f90    tetra.f90   xdph.f90 \
alg9.f90      cdifbs.f90  cyct2b.f90  dumper.f90  ffhelp.f90  gkad1a.f90  initl.f90   mbctr.f90      mred2i.f90  pkqad1.f90  rbmg2.f90    sdrht.f90    ssg3.f90     tiger.f90   xfadj1.f90 \
algan.f90     cdivid.f90  cyct2.f90   dumx.f90    ffhelp.o  gkad1c.f90  initl.o   mbdpdh.f90     mred2j.f90  pkqad2.f90  rbmg3.f90    sdumx1.f90   ssg4.f90     timeeq.f90  xfldef.f90 \
algap.f90     cdtfbs.f90  dadd5.f90   dupart.f90  ffread.f90  gkad.f90    inptt1.f90  mbgae.f90      mred2l.f90  pkqdm1.f90  rbmg4.f90    sdumx2.f90   ssgetd.f90   timts1.f90  xflord.f90 \
algar.f90     cead1a.f90  dadd.f90    dvectr.f90  ffread.o  gkam1a.f90  inptt1.o  mbgate.f90     mred2m.f90  pkqdm.f90   rcard2.f90   seemat.f90   ssght1.f90   timts2.f90  xflszd.f90 \
alg.f90       cead.f90    dadotb.f90  dvmag.f90   filcor.f90  gkam1b.f90  inptt2.f90  mbgaw.f90      mred2n.f90  pkqdms.f90  rcard.f90    selas1.f90   ssght2.f90   timtst.f90  xgpibs.f90 \
algpb.f90     centre.f90  dapoly.f90  dypz.f90    filcor.o  gkam.f90    inptt2.o  mbgeod.f90     mred2o.f90  pkqdpl.f90  rcova.f90    selas2.f90   ssght.f90    tipe.f90    xgpidd.f90 \
algpo.f90     cf1fbs.f90  daxb.f90    dzpy.f90    filswi.f90  gmmatc.f90  inptt3.f90  mbmode.f90     mred2p.f90  pkrod.f90   rcovb.f90    selbo1.f90   ssghtp.f90   tis2d8.f90  xgpidg.f90 \
algpr.f90     cf1ort.f90  dbar.f90    dzy.f90     filswi.o  gmmatd.f90  inptt3.o  mbplot.f90     mrge.f90    pktq1.f90   rcovc.f90    selbo2.f90   ssgkhi.f90   tker.f90    xgpi.f90 \
allmat.f90    cf2fbs.f90  dbase.f90   dzymat.f90  findc.f90   gmmats.f90  inptt4.f90  mbprit.f90     mring.f90   pktq2.f90   rcovds.f90   selcam.f90   ssgslt.f90   tktztk.f90  xgpimw.f90 \
amatrx.f90    cf2ort.f90  dcone.f90   eadd.f90    findc.o   gmmerg.f90  inptt4.o  mbreg.f90      msgwrt.f90  pktrbs.f90  rcove.f90    semint.f90   sslot1.f90   tldrs.f90   xipfl.f90 \
amgb1a.f90    cfactr.f90  dcross.f90  eandm.f90   finder.f90  gmprtn.f90  inptt5.f90  mce1a.f90      mslot.f90   pktri1.f90  rcovem.f90   seteq.f90    sslot2.f90   tlodm6.f90  xlnkdd.f90 \
amgb1b.f90    cfbsor.f90  ddamat.f90  ectloc.f90  finder.o  gnfist.f90  inptt5.o  mce1b.f90      msolid.f90  pktri2.f90  rcovim.f90   setfnd.f90   ssold1.f90   tlodsl.f90  xlnkhd.f90 \
amgb1c.f90    cfe1ao.f90  ddampg.f90  edit.f90    find.f90    go.f90      input4.f90  mce1c.f90      mtimsu.f90  pktrm1.f90  rcovls.f90   setig.f90    ssold2.f90   tlodt1.f90  xmpldd.f90 \
amgb1d.f90    cfe1my.f90  ddcmps.f90  edtl.f90    find.o    gopen.f90   input4.o  mce1d.f90      mtmsu1.f90  pktrm.f90   rcovms.f90   setinp.f90   ssplin.f90   tlodt2.f90  xosgen.f90 \
amgb1.f90     cfe2ao.f90  ddcomp.f90  egnvct.f90  flbelm.f90  gp1.f90     input.f90   mce1.f90       mtrapr.f90  pktrms.f90  rcovo.f90    setlvl.f90   sswtch.f90   tlodt3.f90  xparam.f90 \
amgb1s.f90    cfe2my.f90  ddr1a.f90   eject.f90   flbema.f90  gp2.f90     input.o   mce2.f90       mtrbsc.f90  pktrpl.f90  rcovqv.f90   setval.f90   stack.f90    tlqd4d.f90  xpolck.f90 \
amgb2a.f90    cfeer1.f90  ddr1b.f90   ektrmd.f90  flbemg.f90  gp3a.f90    insert.f90  mcone.f90      mtriqd.f90  pktrq2.f90  rcovr3.f90   sfarea.f90   step2.f90    tlqd4s.f90  xpunp.f90 \
amgb2.f90     cfeer2.f90  ddr1.f90    ektrms.f90  flbmg.f90   gp3b.f90    insert.o  mdumx.f90      mtrirg.f90  pktrqd.f90  rcovr.f90    sfetch.f90   step.f90     tltr3d.f90  xpurge.f90 \
amgbfs.f90    cfeer3.f90  ddr2.f90    elelbl.f90  flbprt.f90  gp3c.f90    int2a8.f90  melbow.f90     mtrplt.f90  pla1.f90    rcovsl.f90   sgena.f90    stord1.f90   tltr3s.f90  xrcard.f90 \
amg.f90       cfeer4.f90  ddr.f90     elim.f90    flbset.f90  gp3d.f90    int2a8.o  merge1.f90     mtrxi.f90   pla2.f90    rcovss.f90   sgenb.f90    stord2.f90   tmtogo.f90  xread.f90 \
amgrod.f90    cfeer.f90   ddrmm1.f90  em1d.f90    flface.f90  gp3.f90     int2al.f90  merged.f90     mtrxin.f90  pla31.f90   rcovui.f90   sgen.f90     stpaic.f90   tmtsio.f90  xrecps.f90 \
amgsba.f90    cfer3d.f90  ddrmm2.f90  em2d.f90    flfree.f90  gp4.f90     int2al.o  merge.f90      mtrxo.f90   pla32.f90   rcovuo.f90   sgenm.f90    stpax1.f90   tmtslp.f90  xrgdcf.f90 \
amgt1a.f90    cfer3s.f90  ddrmma.f90  em3d.f90    flld.f90    gp4prt.f90  intert.f90  mesage.f90     mxcid.f90   pla3.f90    rcovva.f90   shape.f90    stpax2.f90   tmtsot.f90  xrgdev.f90 \
amgt1b.f90    cfnor1.f90  ddrmm.f90   ema1d.f90   fmdi.f90    gp4sp.f90   intert.o  mflud2.f90     mxcids.f90  pla41.f90   rdmodx.f90   shcsgd.f90   stpax3.f90   tordrd.f90  xrgdfm.f90 \
amgt1c.f90    cfnor2.f90  ddrmmp.f90  ema1.f90    fname.f90   gpcyc.f90   intfbs.f90  mflud3.f90     mxcids.o  pla42.f90   re2al.f90    shctsd.f90   stpbg.f90    tordrs.f90  xrgdtb.f90 \
amgt1d.f90    chkopn.f90  ddrmms.f90  ema1s.f90   fndgrd.f90  gpfdr.f90   intlst.f90  mflud4.f90     na12a8.f90  pla4b.f90   read1.f90    shctss.f90   stpbs0.f90   totape.f90  xrgdtp.f90 \
amgt1.f90     cidck.f90   ddumx.f90   emadtq.f90  fndiam.f90  gpstg.f90   intprt.f90  mfree.f90      na12a8.o  pla4.f90    read1.o    sheard.f90   stpbs1.f90   tpztem.f90  xrgnum.f90 \
amgt1s.f90    cifsdd.f90  decode.f90  ema.f90     fndlvl.f90  gpstgn.f90  intvec.f90  mindeg.f90     na12if.f90  plamat.f90  read2.f90    shears.f90   stpda.f90    traile.f90  xrgsst.f90 \
amgt1t.f90    cinfbs.f90  decomp.f90  emastq.f90  fndnxl.f90  gpstgs.f90  inverd.f90  mintrp.f90     na12if.o  plbar1.f90  read2.o    shfors.f90   stpk.f90     trail.f90   xrgsub.f90 \
amgt2a.f90    cinvp1.f90  degree.f90  emfld.f90   fndpar.f90  gptlbl.f90  invers.f90  mma101.f90     nascar.f90  pload1.f90  read3.f90    shgmgd.f90   stplot.f90   tranem.f90  xsave.f90 \
amgt2.f90     cinvp2.f90  delete.f90  emg1b.f90   fndplt.f90  gptsym.f90  invert.f90  mma102.f90     nascar.o  pload3.f90  read3.o    shgmgs.f90   stpphi.f90   tranp1.f90  xscndm.f90 \
ampa.f90      cinvp3.f90  delkls.f90  emgcng.f90  fndpnt.f90  gpwg1a.f90  invfbs.f90  mma103.f90     norm11.f90  pload4.f90  read4.f90    shlsts.f90   stppt2.f90   transp.f90  xsem00.f90 \
ampb1.f90     cinvpr.f90  delset.f90  emgcor.f90  fndset.f90  gpwg1b.f90  invp1.f90   mma104.f90     norm11.o  pload.f90   read4.o    shpsts.f90   stqme2.f90   trapad.f90  xsfadd.f90 \
ampb2.f90     clstab.f90  deltkl.f90  emg.f90     fnxt.f90    gpwg1c.f90  invp2.f90   mma111.f90     norm1.f90   ploadx.f90  read6.f90    shstns.f90   strap1.f90   trapax.f90  xsfa.f90 \
ampb.f90      clvec.f90   desvel.f90  emgfin.f90  fnxtvc.f90  gpwg.f90    invp3.f90   mma112.f90     norm1.o   ploapf.f90  read6.o    shstss.f90   strap2.f90   trbscd.f90  xsort2.f90 \
ampc1.f90     cmauto.f90  detck.f90   emgold.f90  fnxtvd.f90  grav.f90    invpwr.f90  mma113.f90     normal.f90  plod4d.f90  read7.f90    shstts.f90   strax1.f90   trbsc.f90   xsort.f90 \
ampc2.f90     cmcase.f90  detdet.f90  emgout.f90  fnxtv.f90   gravl1.f90  invtr.f90   mma114.f90     normal.o  plod4s.f90  read7.o    shxtrs.f90   strax2.f90   trbscs.f90  xsosgn.f90 \
ampc.f90      cmckcd.f90  detfbs.f90  emgpro.f90  form12.f90  gravl2.f90  is2d8d.f90  mma1.f90       nrlsum.f90  plot.f90    reduce.f90   sihex1.f90   strax3.f90   trd1a2.f90  xtrnsy.f90 \
ampd.f90      cmckdf.f90  detgbs.f90  emgtab.f90  form1.f90   gravl3.f90  is2d8s.f90  mma201.f90     nrlsum.o  pltmrg.f90  reduce.o   sihex2.f90   strax3.o   trd1a.f90   xtrny1.f90 \
ampe.f90      cmcomb.f90  detm1.f90   empcor.f90  form22.f90  gridip.f90  isft.f90    mma202.f90     nsinfo.f90  pltopr.f90  redu.f90     sinc0s1.f90  strbs1.f90   trd1c2.f90  xychar.f90 \
amp.f90       cmcont.f90  detm3.f90   emsg.f90    form2.f90   gtmat1.f90  itcode.f90  mma203.f90     nsinfo.o  pltset.f90  redu.o     sinc0s.f90   strbs1.o   trd1c.f90   xydump.f90 \
ampf.f90      cmdisc.f90  detm4.f90   encode.f90  format.f90  gust1.f90   itmprt.f90  mma204.f90     number.f90  plttra.f90  reig.f90     sjump.f90    stri31.f90   trd1d2.f90  xyfind.f90 \
angtrs.f90    cmhgen.f90  detm5.f90   eqmcka.f90  formg2.f90  gust2.f90   ittype.f90  mma211.f90     number.o  pnm.f90     reig.o     skpfrm.f90   stri31.o   trd1d.f90   xygraf.f90 \
anisop.f90    cmiwrt.f90  detm.f90    eqmck.f90   formgg.f90  gust3.f90   iunion.f90  mma212.f90     odum.f90    polypt.f90  relabl.f90   skprec.f90   stri32.f90   trd1e.f90   xylog.f90 \
anullsub.f90  cmmcon.f90  dfbs.f90    eqmckm.f90  fornam.f90  gust.f90    jacob2.f90  mma213.f90     odum.o    prefix.f90  relabl.o   sma1a.f90    stri32.o   trd.f90     xyout.f90 \
apd12.f90     cmrd2a.f90  dgree.f90   eqmcks.f90  fornum.f90  hbdyd.f90   jacobs.f90  mma214.f90     ofcomp.f90  preloc.f90  remflx.f90   sma1b.f90    strir1.f90   tree.f90    xyplot.f90 \
apd1.f90      cmrd2b.f90  diag36.f90  eqout1.f90  fpont.f90   hbdy.f90    kbar.f90    mma2.f90       ofp1a.f90   premat.f90  remflx.o   sma1.f90     strir1.o   trht1a.f90  xyprpl.f90 \
apd2.f90      cmrd2c.f90  diagon.f90  eqscod.f90  fqrw.f90    hbdys.f90   kcone2.f90  mma301.f90     ofp1b.f90   presax.f90  rename.f90   sma2a.f90    strir2.f90   trht1b.f90  xyprpt.f90 \
apd3.f90      cmrd2d.f90  diam.f90    eqsout.f90  fqrwv.f90   hccom.f90   kconed.f90  mma302.f90     ofp1c.f90   pretab.f90  rename.o   sma2b.f90    strir2.o   trht1c.f90  xytics.f90 \
apd4.f90      cmrd2e.f90  dihex.f90   errmkn.f90  frbk2.f90   hdchk.f90   kcones.f90  mma303.f90     ofp1.f90    pretrd.f90  retblk.f90   sma2.f90     strm61.f90   trht.f90    xytran.f90 \
apd5.f90      cmrd2.f90   dipole.f90  estmag.f90  frbk.f90    hdcoef.f90  kdumx.f90   mma304.f90     ofpcc1.f90  pretrs.f90  rforce.f90   sma3a.f90    strm61.o   tria3d.f90  yrcard.f90 \
apdb1.f90     cmrd2f.f90  dis2d8.f90  etrbkd.f90  frd2a.f90   hdlin.f90   kelas.f90   mma311.f90     ofpcc2.f90  print.f90   rmg.f90      sma3b.f90    strm62.f90   tria3s.f90  zeroc.f90 \
apdb2a.f90    cmrd2g.f90  displa.f90  etrbks.f90  frd2b.f90   hdplot.f90  kelbow.f90  mma312.f90     ofpcf1.f90  proces.f90  rodd.f90     sma3c.f90    strm62.o   triaad.f90  zj.f90 \
apdb2.f90     cmrels.f90  dist.f90    etrbmd.f90  frd2c.f90   hdplt.f90   kflud2.f90  mma313.f90     ofpcf2.f90  procom.f90  rod.f90      sma3.f90     strme1.f90   triaax.f90 \
apdb.f90      cmsfil.f90  dk100.f90   etrbms.f90  frd2d.f90   hdsket.f90  kflud3.f90  mma314.f90     ofpcs1.f90  prolat.f90  rods.f90     smc2cd.f90   strme1.o   tridi1.f90 \
apdcs.f90     cmsofo.f90  dk211.f90   exi2.f90    frd2e.f90   hdsolv.f90  kflud4.f90  mma321.f90     ofpcs2.f90  prompt.f90  rombdk.f90   smc2cs.f90   strnam.f90   tridi.f90 \
apd.f90       cmtimu.f90  dk89.f90    exio1.f90   frd2f.f90   hdstat.f90  khrfn2.f90  mma322.f90     ofp.f90     prtint.f90  romber.f90   smc2rd.f90   strnam.o   trif.f90 \
apdf.f90      cmtoc.f90   dki.f90     exio2.f90   frd2i.f90   hdstus.f90  khrfn3.f90  mma323.f90     ofpgpw.f90  prtmsg.f90  rombsk.f90   smc2rs.f90   strp11.f90   trimem.f90 \
apdoe.f90     cmtrce.f90  dkint.f90   exio.f90    fread.f90   hdsurf.f90  kompnt.f90  mma324.f90     ofpmis.f90  prtprm.f90  rotate1.f90  smcccd.f90   strp11.o   triqd.f90 \
apdr.f90      cnorm1.f90  dkl.f90     exlvl.f90   frlga.f90   hdvs1.f90   korsz.f90   mma3.f90       ofppnt.f90  psbar.f90   rotate.f90   smcccs.f90   strp12.f90   trlga.f90

libnastran_a_LIBADD = ../bd/libnastbd.a
