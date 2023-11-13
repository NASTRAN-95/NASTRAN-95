# what flags you want to pass to the Fortran compiler & linker
OPT_OPTS ?= unknown
AM_FFLAGS = $(OPT_OPTS)
FFLAGS = -w -fno-range-check -fno-automatic -fcray-pointer -std=legacy -J../include

# this lists the binaries to produce, the (non-PHONY, binary) targets in
# the previous manual Makefile
noinst_LIBRARIES = libnastran.a
libnastran_a_SOURCES = \
a42a8.f     area.f    cnorm.f   dkls.f    exo2.f    frlgb.f   hdvsr.f   kpanel.f     mma401.f  ofppun.f  psqad1.f   rotat.f    smccrd.f   strp12.o  trlgb.f \
a42a8.o     arrm.f    cnstdd.f  dlamby.f  extern.f  frlg.f    head.f    kpltst.f     mma402.f  ofprf1.f  psqad2.f   rowdyz.f   smccrs.f   strpl1.f  trlgc.f \
a82int.f    ascm01.f  cnstrc.f  dlamg.f   f6211.f   frmax.f   hess1.f   kqdmem.f     mma403.f  ofprf2.f  psqdm1.f   rsetup.f   smchlp.f   strpl1.o  trlgd.f \
a82int.o    ascm02.f  com12.f   dlbpt2.f  f89.f     frmlta.f  hess2.f   kqdplt.f     mma404.f  ofprs1.f  psqdm.f    rsort.f    smcomp.f   strpts.f  trlg.f \
adr.f       ascm03.f  comb1.f   dlkapm.f  fa1.f     frmltd.f  hmat.f    krod.f       mma411.f  ofprs2.f  psqpl1.f   ruler.f    smcout.f   strqd1.f  trmemd.f \
adri.f      ascm04.f  comb2.f   dloop.f   fa1ke.f   frmlt.f   hring.f   krshft.f     mma412.f  ofpsn1.f  psrod.f    rzintd.f   smcph1.f   strqd2.f  trmems.f \
adri.o      ascm05.f  combin.f  dlpt2.f   fa1k.f    frmltx.f  hsbg.f    kslot.f      mma413.f  ofpss1.f  psta.f     rzints.f   smcph2.f   strscn.f  trnsp.f \
adr.o       ascm06.f  combo.f   dmatrs.f  fa1pka.f  frr1a1.f  iapd.f    ksolid.f     mma414.f  ofrf2s.f  pstamg.f   sadd.f     smcrtr.f   strsl1.f  trnsps.f \
adrprt.f    ascm07.f  comect.f  dmatrx.f  fa1pke.f  frrd1c.f  idf1.f    ktetra.f     mma4.f    ofrs2s.f  pstpl1.f   sadotb.f   smcspl.f   strsl2.f  trplmd.f \
adrprt.o    ascm08.f  comugv.f  dmfgr.f   fa1pki.f  frrd1d.f  idf2.f    ktrapr.f     mma.f     ofsplt.f  pstq1.f    sanorm.f   smleig1.f  strslv.f  trplms.f \
af.f        ascm09.f  cone.f    dmpalt.f  fa1pkv.f  frrd1f.f  idist.f   ktrbsc.f     mmarc1.f  oldel1.f  pstq2.f    saxb.f     smleig.f   stube1.f  trplt.f \
af.o        ascm10.f  conm1d.f  dmpfil.f  fa2.f     frrd2.f   idplot.f  ktriqd.f     mmarc2.f  oldel2.f  pstrb1.f   saxif1.f   smmats.f   sub1.f    trttem.f \
ai.f        ascm11.f  conm1s.f  dmpyad.f  factor.f  frrd.f    ifb2ar.f  ktrirg.f     mmarc3.f  oldel3.f  pstri1.f   saxif2.f   smp1.f     suba.f    tshear.f \
ai.o        ascm12.f  conm2d.f  dmpy.f    factru.f  frsw2.f   ifp1b.f   ktrm6d.f     mmarc4.f  olplot.f  pstri2.f   sbar1.f    smp2.f     subbb.f   tspl1d.f \
ais.f       ascm13.f  conm2s.f  dnorm.f   failrs.f  frsw.f    ifp1c.f   ktrm6s.f     mmarm1.f  onetwo.f  pstrm1.f   sbar2.f    smpyad.f   subb.f    tspl1s.f \
ais.o       asdmap.f  conmsg.f  dpd1.f    failur.f  ftube.f   ifp1d.f   ktrmem.f     mmarm2.f  onlins.f  pstrm.f    sbspl2.f   smsg.f     subcc.f   tspl2d.f \
akapm.f     aspro.f   contor.f  dpd2.f    fbs1.f    fvrs1a.f  ifp1e.f   ktrpld.f     mmarm3.f  opt2a.f   pstrq2.f   scalar.f   snpdf.f    subc.f    tspl2s.f \
akapm.o     asycon.f  copy.f    dpd3.f    fbs21.f   fvrs1b.f  ifp1.f    ktrpls.f     mmarm4.f  opt2b.f   pthbdy.f   scaled.f   sofcls.f   sub.f     tspl3d.f \
akappa.f    ateig.f   cpyfil.f  dpd4.f    fbs2.f    fvrs1c.f  ifp1f.f   ktrplt.f     modac1.f  opt2c.f   ptintr.f   scalex.f   sofi.f     subi.f    tspl3s.f \
akappa.o    autock.f  cpystr.f  dpd5.f    fbs3.f    fvrs1d.f  ifp1g.f   ktshld.f     modac2.f  opt2d.f   pull.f     scan.f     sofint.f   subpb.f   ttlpge.f \
akp2.f      autosv.f  crdrd2.f  dpdaa.f   fbs4.f    fvrs1e.f  ifp1h.f   ktshls.f     modacc.f  optp1a.f  push.f     scat.f     sofo.f     subp.f    ttordr.f \
akp2.o      axis10.f  crdrd.f   dpd.f     fbs.f     fvrs2a.f  ifp1pc.f  ktube.f      moda.f    optp1b.f  q2bcd.f    sce1.f     sofopn.f   subph1.f  ttrapr.f \
alamda.f    axis.f    create.f  dplot.f   fbsf.f    fvrst1.f  ifp1s.f   lamx.f       modb.f    optp1c.f  q2bcs.f    scheme.f   sofsiz.f   summ.f    ttrirg.f \
alamda.o    axloop.f  criggp.f  dpltst.f  fbsi1.f   fvrst2.f  ifp1xy.f  line10.f     modc.f    optp1d.f  q2trmd.f   scone1.f   softoc.f   sumphi.f  tubed.f \
alg01.f     bandit.f  crspld.f  dppsb.f   fbsi2.f   fwmw.f    ifp3b.f   line.f       mplprt.f  optpr1.f  q2trms.f   scone2.f   softrl.f   suplt.f   tubes.f \
alg01.o     bard.f    crspld.o  dpps.f    fbsi3.f   fzy2.f    ifp3.f    linein.f     mpy3a.f   optpr2.f  q4bmgd.f   scone3.f   sofut.f    suread.f  tvor.f \
alg02.f     bar.f     crspls.f  dpse2.f   fbsi4.f   gauss2.f  ifp4b.f   linel.f      mpy3b.f   optpx1.f  q4bmgs.f   scrlm.f    solid.f    suwrt.f   twistd.f \
alg02.o     bars.f    crspls.o  dpse3.f   fbsi.f    geloop.f  ifp4c.f   linkup.f     mpy3c.f   optpx.f   q4gmgs.f   sd2rhd.f   solve1.f   switch.f  twists.f \
alg03.f     basglb.f  crsub.f   dpse4.f   fbsint.f  gencos.f  ifp4e.f   loadsu.f     mpy3dr.f  order.f   q4nrms.f   sdcin.f    solve.f    symbol.f  type10.f \
alg04.f     bdat01.f  crsub.o   dpzy.f    fbsinv.f  gend.f    ifp4.f    locpt.f      mpy3.f    ortck.f   q4shps.f   sdcins.f   solver.f   t3bgbs.f  typflt.f \
alg05.f     bdat02.f  csqrtx.f  dqdmem.f  fbsrdm.f  gendsb.f  ifp4f.f   lodapp.f     mpy3ic.f  ortho.f   qdmem.f    sdcmm.f    sortdg.f   t3bmgd.f  typint.f \
alg06.f     bdat03.f  csqrtx.o  dquad.f   fcurl.f   genpar.f  ifp4g.f   logfil.f     mpy3nu.f  oscxrf.f  qdmm1d.f   sdcmps.f   sort.f     t3bmgs.f  unpscr.f \
alg07.f     bdat04.f  csub.f    draw.f    fdit.f    genvec.f  ifp5a.f   loglog.f     mpy3oc.f  outmsc.f  qdmm1.f    sdcmq.f    sorti.f    t3gemd.f  upart.f \
alg08.f     bdat05.f  csub.o    drkapm.f  fdsub.f   getblk.f  ifp5.f    lprops.f     mpy3p.f   outpak.f  qdmm1s.f   sdcom1.f   spanl1.f   t3gems.f  upcase.f \
alg09.f     bdat06.f  csumm.f   drod.f    fdvect.f  getdef.f  ifpdco.f  lsplin.f     mpy4t.f   outpt1.f  qdmm2d.f   sdcom2.f   spanl2.f   t3pl4d.f  usrmsg.f \
alg10.f     betrns.f  csumm.o   drwchr.f  feer1.f   gfbs.f    ifp.f     magbdy.f     mpya3d.f  outpt2.f  qdmm2.f    sdcom3.f   splt10.f   t3pl4s.f  valvec.f \
alg11.f     bfsmat.f  cthmck.f  ds1a.f    feer2.f   gfscom.f  ifpmdc.f  magpha.f     mpyad.f   outpt3.f  qdmm2s.f   sdcom4.f   sptchk.f   t3setd.f  varian.f \
alg12.f     bgrid.f   cthmck.o  ds1b.f    feer3.f   gfsdir.f  ifppar.f  Makefile     mpyado.f  outpt4.f  qdplt.f    sdcomp.f   sqdm11.f   t3sets.f  vdra.f \
alg13.f     bint.f    ctrnsp.f  ds1etd.f  feer3x.f  gfshc.f   ifppvc.f  Makefile.am  mpydri.f  outpt5.f  qhbdy.f    sdcompx.f  sqdm12.f   ta1a.f    vdrb.f \
alg14.f     biotsv.f  ctrnsp.o  ds1.f     feer4.f   gfsh.f    ifs1p.f   Makefile.in  mpyl.f    outpt.f   qloadl.f   sdcout.f   sqdm21.f   ta1b.f    vdr.f \
alg15.f     bishel.f  curcas.f  dschk.f   feerdd.f  gfsma.f   ifs2p.f   makmcb.f     mpyq.f    page2.f   qparam.f   sdhtf1.f   sqdm22.f   ta1ca.f   vec.f \
alg16.f     bislc2.f  curcas.o  dshear.f  feer.f    gfsmo2.f  ifs3p.f   mapset.f     mqdplt.f  page.f    qparmd.f   sdhtf2.f   sqdme1.f   ta1c.f    vecprt.f \
alg17.f     bisloc.f  curv1.f   dsmg1.f   ferfbs.f  gfsmod.f  ifs4p.f   maskn.f      mred1a.f  pakcol.f  qparmr.f   sdhtff.f   sqdpl1.f   ta1cpd.f  viscd.f \
alg18.f     bitpat.f  curv1.o   dsmg2.f   ferltd.f  gfsmrg.f  ifs5p.f   masstq.f     mred1b.f  param.f   qriter1.f  sdr1a.f    sqrtm.f    ta1cps.f  viscs.f \
alg19.f     bmg.f     curv2.f   dstroy.f  ferlts.f  gfsmt.f   ifte2.f   matck.f      mred1c.f  paraml.f  qriter.f   sdr1b.f    squd41.f   ta1etd.f  wavey.f \
alg1.f      bmgtns.f  curv2.o   dtranp.f  ferrdm.f  gfsptn.f  ifte4.f   matdum.f     mred1d.f  partn1.f  quad4d.f   sdr1.f     squd42.f   ta1.f     wilvec1.f \
alg25.f     border.f  curv3.f   dtrbsc.f  ferrdm.o  gfsspc.f  ift.f     matgen.f     mred1e.f  partn2.f  quad4s.f   sdr2aa.f   srod1.f    ta1h.f    wilvec.f \
alg26.f     bound.f   curv3.o   dtria.f   ferswd.f  gfstrn.f  iftg.f    matgpr.f     mred1.f   partn3.f  qvol.f     sdr2a.f    srod2.f    tabfmt.f  wplt10.f \
alg29.f     bread.f   curv.f    dtrmem.f  ferswd.o  gfswch.f  ihexd.f   matprn.f     mred2a.f  partn.f   rand1.f    sdr2b.f    ss2d81.f   table5.f  wrtmsg.f \
alg2.f      bseqgp.f  curvit.f  dtshld.f  fersws.f  gibstk.f  ihex.f    matprt.f     mred2b.f  pcoord.f  rand2.f    sdr2c.f    ss2d82.f   tablev.f  wrtprt.f \
alg30.f     bug.f     curvit.o  dtshls.f  fersws.o  gi.f      ihexsd.f  matvc2.f     mred2c.f  permut.f  rand3.f    sdr2d.f    ssg1a.f    tabpch.f  wrttrl.f \
alg3.f      calcv.f   curv.o    dumerg.f  ferxtd.f  giggks.f  ihexs.f   matvec.f     mred2d.f  perpec.f  rand5.f    sdr2e.f    ssg1.f     tabprt.f  xcei.f \
alg4.f      case.f    curvps.f  dumod1.f  ferxtd.o  gigtka.f  ihexss.f  matwrt.f     mred2e.f  pexit.f   rand6.f    sdr2.f     ssg2a.f    tabpt.f   xchk.f \
alg5.f      casege.f  cxloop.f  dumod2.f  ferxts.f  gigtkg.f  incore.f  maxdgr.f     mred2.f   phdmia.f  rand7.f    sdr3a.f    ssg2b.f    tapbit.f  xclean.f \
alg6.f      cdcomp.f  cxtrny.f  dumod3.f  ferxts.o  ginofl.f  incro.f   mbamg.f      mred2f.f  pidck.f   rand8.f    sdr3.f     ssg2c.f    termsd.f  xcsa.f \
alg7.f      cdetm2.f  cyct1.f   dumod4.f  ff100.f   gipsst.f  initl2.f  mbbslj.f     mred2g.f  piklvl.f  random.f   sdrchk.f   ssg2.f     termss.f  xdcode.f \
alg8.f      cdetm.f   cyct2a.f  dumod5.f  ff100.o   givens.f  initl2.o  mbcap.f      mred2h.f  pkbar.f   rbmg1.f    sdretd.f   ssg3a.f    tetra.f   xdph.f \
alg9.f      cdifbs.f  cyct2b.f  dumper.f  ffhelp.f  gkad1a.f  initl.f   mbctr.f      mred2i.f  pkqad1.f  rbmg2.f    sdrht.f    ssg3.f     tiger.f   xfadj1.f \
algan.f     cdivid.f  cyct2.f   dumx.f    ffhelp.o  gkad1c.f  initl.o   mbdpdh.f     mred2j.f  pkqad2.f  rbmg3.f    sdumx1.f   ssg4.f     timeeq.f  xfldef.f \
algap.f     cdtfbs.f  dadd5.f   dupart.f  ffread.f  gkad.f    inptt1.f  mbgae.f      mred2l.f  pkqdm1.f  rbmg4.f    sdumx2.f   ssgetd.f   timts1.f  xflord.f \
algar.f     cead1a.f  dadd.f    dvectr.f  ffread.o  gkam1a.f  inptt1.o  mbgate.f     mred2m.f  pkqdm.f   rcard2.f   seemat.f   ssght1.f   timts2.f  xflszd.f \
alg.f       cead.f    dadotb.f  dvmag.f   filcor.f  gkam1b.f  inptt2.f  mbgaw.f      mred2n.f  pkqdms.f  rcard.f    selas1.f   ssght2.f   timtst.f  xgpibs.f \
algpb.f     centre.f  dapoly.f  dypz.f    filcor.o  gkam.f    inptt2.o  mbgeod.f     mred2o.f  pkqdpl.f  rcova.f    selas2.f   ssght.f    tipe.f    xgpidd.f \
algpo.f     cf1fbs.f  daxb.f    dzpy.f    filswi.f  gmmatc.f  inptt3.f  mbmode.f     mred2p.f  pkrod.f   rcovb.f    selbo1.f   ssghtp.f   tis2d8.f  xgpidg.f \
algpr.f     cf1ort.f  dbar.f    dzy.f     filswi.o  gmmatd.f  inptt3.o  mbplot.f     mrge.f    pktq1.f   rcovc.f    selbo2.f   ssgkhi.f   tker.f    xgpi.f \
allmat.f    cf2fbs.f  dbase.f   dzymat.f  findc.f   gmmats.f  inptt4.f  mbprit.f     mring.f   pktq2.f   rcovds.f   selcam.f   ssgslt.f   tktztk.f  xgpimw.f \
amatrx.f    cf2ort.f  dcone.f   eadd.f    findc.o   gmmerg.f  inptt4.o  mbreg.f      msgwrt.f  pktrbs.f  rcove.f    semint.f   sslot1.f   tldrs.f   xipfl.f \
amgb1a.f    cfactr.f  dcross.f  eandm.f   finder.f  gmprtn.f  inptt5.f  mce1a.f      mslot.f   pktri1.f  rcovem.f   seteq.f    sslot2.f   tlodm6.f  xlnkdd.f \
amgb1b.f    cfbsor.f  ddamat.f  ectloc.f  finder.o  gnfist.f  inptt5.o  mce1b.f      msolid.f  pktri2.f  rcovim.f   setfnd.f   ssold1.f   tlodsl.f  xlnkhd.f \
amgb1c.f    cfe1ao.f  ddampg.f  edit.f    find.f    go.f      input4.f  mce1c.f      mtimsu.f  pktrm1.f  rcovls.f   setig.f    ssold2.f   tlodt1.f  xmpldd.f \
amgb1d.f    cfe1my.f  ddcmps.f  edtl.f    find.o    gopen.f   input4.o  mce1d.f      mtmsu1.f  pktrm.f   rcovms.f   setinp.f   ssplin.f   tlodt2.f  xosgen.f \
amgb1.f     cfe2ao.f  ddcomp.f  egnvct.f  flbelm.f  gp1.f     input.f   mce1.f       mtrapr.f  pktrms.f  rcovo.f    setlvl.f   sswtch.f   tlodt3.f  xparam.f \
amgb1s.f    cfe2my.f  ddr1a.f   eject.f   flbema.f  gp2.f     input.o   mce2.f       mtrbsc.f  pktrpl.f  rcovqv.f   setval.f   stack.f    tlqd4d.f  xpolck.f \
amgb2a.f    cfeer1.f  ddr1b.f   ektrmd.f  flbemg.f  gp3a.f    insert.f  mcone.f      mtriqd.f  pktrq2.f  rcovr3.f   sfarea.f   step2.f    tlqd4s.f  xpunp.f \
amgb2.f     cfeer2.f  ddr1.f    ektrms.f  flbmg.f   gp3b.f    insert.o  mdumx.f      mtrirg.f  pktrqd.f  rcovr.f    sfetch.f   step.f     tltr3d.f  xpurge.f \
amgbfs.f    cfeer3.f  ddr2.f    elelbl.f  flbprt.f  gp3c.f    int2a8.f  melbow.f     mtrplt.f  pla1.f    rcovsl.f   sgena.f    stord1.f   tltr3s.f  xrcard.f \
amg.f       cfeer4.f  ddr.f     elim.f    flbset.f  gp3d.f    int2a8.o  merge1.f     mtrxi.f   pla2.f    rcovss.f   sgenb.f    stord2.f   tmtogo.f  xread.f \
amgrod.f    cfeer.f   ddrmm1.f  em1d.f    flface.f  gp3.f     int2al.f  merged.f     mtrxin.f  pla31.f   rcovui.f   sgen.f     stpaic.f   tmtsio.f  xrecps.f \
amgsba.f    cfer3d.f  ddrmm2.f  em2d.f    flfree.f  gp4.f     int2al.o  merge.f      mtrxo.f   pla32.f   rcovuo.f   sgenm.f    stpax1.f   tmtslp.f  xrgdcf.f \
amgt1a.f    cfer3s.f  ddrmma.f  em3d.f    flld.f    gp4prt.f  intert.f  mesage.f     mxcid.f   pla3.f    rcovva.f   shape.f    stpax2.f   tmtsot.f  xrgdev.f \
amgt1b.f    cfnor1.f  ddrmm.f   ema1d.f   fmdi.f    gp4sp.f   intert.o  mflud2.f     mxcids.f  pla41.f   rdmodx.f   shcsgd.f   stpax3.f   tordrd.f  xrgdfm.f \
amgt1c.f    cfnor2.f  ddrmmp.f  ema1.f    fname.f   gpcyc.f   intfbs.f  mflud3.f     mxcids.o  pla42.f   re2al.f    shctsd.f   stpbg.f    tordrs.f  xrgdtb.f \
amgt1d.f    chkopn.f  ddrmms.f  ema1s.f   fndgrd.f  gpfdr.f   intlst.f  mflud4.f     na12a8.f  pla4b.f   read1.f    shctss.f   stpbs0.f   totape.f  xrgdtp.f \
amgt1.f     cidck.f   ddumx.f   emadtq.f  fndiam.f  gpstg.f   intprt.f  mfree.f      na12a8.o  pla4.f    read1.o    sheard.f   stpbs1.f   tpztem.f  xrgnum.f \
amgt1s.f    cifsdd.f  decode.f  ema.f     fndlvl.f  gpstgn.f  intvec.f  mindeg.f     na12if.f  plamat.f  read2.f    shears.f   stpda.f    traile.f  xrgsst.f \
amgt1t.f    cinfbs.f  decomp.f  emastq.f  fndnxl.f  gpstgs.f  inverd.f  mintrp.f     na12if.o  plbar1.f  read2.o    shfors.f   stpk.f     trail.f   xrgsub.f \
amgt2a.f    cinvp1.f  degree.f  emfld.f   fndpar.f  gptlbl.f  invers.f  mma101.f     nascar.f  pload1.f  read3.f    shgmgd.f   stplot.f   tranem.f  xsave.f \
amgt2.f     cinvp2.f  delete.f  emg1b.f   fndplt.f  gptsym.f  invert.f  mma102.f     nascar.o  pload3.f  read3.o    shgmgs.f   stpphi.f   tranp1.f  xscndm.f \
ampa.f      cinvp3.f  delkls.f  emgcng.f  fndpnt.f  gpwg1a.f  invfbs.f  mma103.f     norm11.f  pload4.f  read4.f    shlsts.f   stppt2.f   transp.f  xsem00.f \
ampb1.f     cinvpr.f  delset.f  emgcor.f  fndset.f  gpwg1b.f  invp1.f   mma104.f     norm11.o  pload.f   read4.o    shpsts.f   stqme2.f   trapad.f  xsfadd.f \
ampb2.f     clstab.f  deltkl.f  emg.f     fnxt.f    gpwg1c.f  invp2.f   mma111.f     norm1.f   ploadx.f  read6.f    shstns.f   strap1.f   trapax.f  xsfa.f \
ampb.f      clvec.f   desvel.f  emgfin.f  fnxtvc.f  gpwg.f    invp3.f   mma112.f     norm1.o   ploapf.f  read6.o    shstss.f   strap2.f   trbscd.f  xsort2.f \
ampc1.f     cmauto.f  detck.f   emgold.f  fnxtvd.f  grav.f    invpwr.f  mma113.f     normal.f  plod4d.f  read7.f    shstts.f   strax1.f   trbsc.f   xsort.f \
ampc2.f     cmcase.f  detdet.f  emgout.f  fnxtv.f   gravl1.f  invtr.f   mma114.f     normal.o  plod4s.f  read7.o    shxtrs.f   strax2.f   trbscs.f  xsosgn.f \
ampc.f      cmckcd.f  detfbs.f  emgpro.f  form12.f  gravl2.f  is2d8d.f  mma1.f       nrlsum.f  plot.f    reduce.f   sihex1.f   strax3.f   trd1a2.f  xtrnsy.f \
ampd.f      cmckdf.f  detgbs.f  emgtab.f  form1.f   gravl3.f  is2d8s.f  mma201.f     nrlsum.o  pltmrg.f  reduce.o   sihex2.f   strax3.o   trd1a.f   xtrny1.f \
ampe.f      cmcomb.f  detm1.f   empcor.f  form22.f  gridip.f  isft.f    mma202.f     nsinfo.f  pltopr.f  redu.f     sinc0s1.f  strbs1.f   trd1c2.f  xychar.f \
amp.f       cmcont.f  detm3.f   emsg.f    form2.f   gtmat1.f  itcode.f  mma203.f     nsinfo.o  pltset.f  redu.o     sinc0s.f   strbs1.o   trd1c.f   xydump.f \
ampf.f      cmdisc.f  detm4.f   encode.f  format.f  gust1.f   itmprt.f  mma204.f     number.f  plttra.f  reig.f     sjump.f    stri31.f   trd1d2.f  xyfind.f \
angtrs.f    cmhgen.f  detm5.f   eqmcka.f  formg2.f  gust2.f   ittype.f  mma211.f     number.o  pnm.f     reig.o     skpfrm.f   stri31.o   trd1d.f   xygraf.f \
anisop.f    cmiwrt.f  detm.f    eqmck.f   formgg.f  gust3.f   iunion.f  mma212.f     odum.f    polypt.f  relabl.f   skprec.f   stri32.f   trd1e.f   xylog.f \
anullsub.f  cmmcon.f  dfbs.f    eqmckm.f  fornam.f  gust.f    jacob2.f  mma213.f     odum.o    prefix.f  relabl.o   sma1a.f    stri32.o   trd.f     xyout.f \
apd12.f     cmrd2a.f  dgree.f   eqmcks.f  fornum.f  hbdyd.f   jacobs.f  mma214.f     ofcomp.f  preloc.f  remflx.f   sma1b.f    strir1.f   tree.f    xyplot.f \
apd1.f      cmrd2b.f  diag36.f  eqout1.f  fpont.f   hbdy.f    kbar.f    mma2.f       ofp1a.f   premat.f  remflx.o   sma1.f     strir1.o   trht1a.f  xyprpl.f \
apd2.f      cmrd2c.f  diagon.f  eqscod.f  fqrw.f    hbdys.f   kcone2.f  mma301.f     ofp1b.f   presax.f  rename.f   sma2a.f    strir2.f   trht1b.f  xyprpt.f \
apd3.f      cmrd2d.f  diam.f    eqsout.f  fqrwv.f   hccom.f   kconed.f  mma302.f     ofp1c.f   pretab.f  rename.o   sma2b.f    strir2.o   trht1c.f  xytics.f \
apd4.f      cmrd2e.f  dihex.f   errmkn.f  frbk2.f   hdchk.f   kcones.f  mma303.f     ofp1.f    pretrd.f  retblk.f   sma2.f     strm61.f   trht.f    xytran.f \
apd5.f      cmrd2.f   dipole.f  estmag.f  frbk.f    hdcoef.f  kdumx.f   mma304.f     ofpcc1.f  pretrs.f  rforce.f   sma3a.f    strm61.o   tria3d.f  yrcard.f \
apdb1.f     cmrd2f.f  dis2d8.f  etrbkd.f  frd2a.f   hdlin.f   kelas.f   mma311.f     ofpcc2.f  print.f   rmg.f      sma3b.f    strm62.f   tria3s.f  zeroc.f \
apdb2a.f    cmrd2g.f  displa.f  etrbks.f  frd2b.f   hdplot.f  kelbow.f  mma312.f     ofpcf1.f  proces.f  rodd.f     sma3c.f    strm62.o   triaad.f  zj.f \
apdb2.f     cmrels.f  dist.f    etrbmd.f  frd2c.f   hdplt.f   kflud2.f  mma313.f     ofpcf2.f  procom.f  rod.f      sma3.f     strme1.f   triaax.f \
apdb.f      cmsfil.f  dk100.f   etrbms.f  frd2d.f   hdsket.f  kflud3.f  mma314.f     ofpcs1.f  prolat.f  rods.f     smc2cd.f   strme1.o   tridi1.f \
apdcs.f     cmsofo.f  dk211.f   exi2.f    frd2e.f   hdsolv.f  kflud4.f  mma321.f     ofpcs2.f  prompt.f  rombdk.f   smc2cs.f   strnam.f   tridi.f \
apd.f       cmtimu.f  dk89.f    exio1.f   frd2f.f   hdstat.f  khrfn2.f  mma322.f     ofp.f     prtint.f  romber.f   smc2rd.f   strnam.o   trif.f \
apdf.f      cmtoc.f   dki.f     exio2.f   frd2i.f   hdstus.f  khrfn3.f  mma323.f     ofpgpw.f  prtmsg.f  rombsk.f   smc2rs.f   strp11.f   trimem.f \
apdoe.f     cmtrce.f  dkint.f   exio.f    fread.f   hdsurf.f  kompnt.f  mma324.f     ofpmis.f  prtprm.f  rotate1.f  smcccd.f   strp11.o   triqd.f \
apdr.f      cnorm1.f  dkl.f     exlvl.f   frlga.f   hdvs1.f   korsz.f   mma3.f       ofppnt.f  psbar.f   rotate.f   smcccs.f   strp12.f   trlga.f

libnastran_a_LIBADD = ../bd/libnastbd.a
