!*==xgpi.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE xgpi
!
!     THE PURPOSE OF XGPI IS TO INITIALIZE AND CALL THE FOLLOWING
!     SUBROUTINES - XOSGEN AND XFLORD.
!
   IMPLICIT NONE
   USE c_ifpx0
   USE c_ifpx1
   USE c_l15l8
   USE c_moddmp
   USE c_stapid
   USE c_system
   USE c_two
   USE c_xceitb
   USE c_xdpl
   USE c_xfiat
   USE c_xfist
   USE c_xgpi2
   USE c_xgpi3
   USE c_xgpi4
   USE c_xgpi5
   USE c_xgpi6
   USE c_xgpi8
   USE c_xgpic
   USE c_xgpid
   USE c_xgpie
   USE c_xmdmsk
   USE c_xmssg
   USE c_xoldpt
   USE c_xvps
   USE c_zzzzzz
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: bgnmsk , endmsk , i , i1 , i2 , ib1s , ibegn , ibulk , icase , idelet , idpbuf , idpfct , iend , iendbt , ii , iii ,  &
            & index , iopbuf , iparbt , iparpt , iparw1 , iparw2 , istrbt , j , j1 , j2 , jj , jjj , jtype , k , k1 , kk , kz , l , &
            & l1 , l2 , lbd1 , lbdlcc , lim1 , lim2 , ll , lloscr , llx , loscar , lstdpl , lx , m , maxdpl , nam1 , nam2 , nampt , &
            & ndpfil , nfile , nmask , nptbuf , osbot , ospnt , osprc , ptfct , wrngrl
   INTEGER , DIMENSION(1) :: cnm , dmpcrd , fnm , ibufr , iccnam , icf , icpdpl , med , oscar , ptdic
   INTEGER , SAVE :: filcon , idp , idpwrd , iop , iopwrd , ixtim , jcard , jfile , ndpl , nparam , npt , nptwrd , npvt , nxaltr ,  &
                   & nxcsa , nxvps
   INTEGER , DIMENSION(8) :: ibf
   INTEGER , DIMENSION(2) , SAVE :: ioshdr , nxgpi , nxptdc
   INTEGER , DIMENSION(7) :: itrl
   INTEGER , DIMENSION(6) , SAVE :: itype
   LOGICAL :: lnogo
   INTEGER , DIMENSION(5) :: os
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!                  ** CONTROL CARD NAMES **
!                  ** DMAP    CARD NAMES **
!WKBR COMMON /XGPI3 / PVT(6)
!WKBR COMMON /XCEITB/ CEITBL(2)
   !>>>>EQUIVALENCE (Loscar,Os(1),Core(1)) , (Osprc,Os(2)) , (Osbot,Os(3)) , (Ospnt,Os(4)) , (Oscar(1),Os(5)) ,                          &
!>>>>    & (Oscar(1),Med(1),Fnm(1),Cnm(1),Icpdpl(1)) , (Oscar(1),Ibufr(1),Dmpcrd(1),Ptdic(1))
   !>>>>EQUIVALENCE (Dmap(1),Icf(1)) , (Nmed,Iccnam(1)) , (Mpl(1),Ibf(1)) , (Dpl(1),Ndpfil) , (Dpl(2),Maxdpl) , (Dpl(3),Lstdpl) ,        &
!>>>>    & (Nogo,Lnogo)
!
!           ** DEFINITION OF PROGRAM VARIABLES **
!     LICF   = NUMBER OF WORDS IN ICF ARRAY
!     WRNGRL = COUNTER FOR NUMBER OF TIMES WRONG REEL WAS MOUNTED.
!     FILCON = FLAG INDICATING FILE IS CONTINUED ON NEXT REEL.
!     IDPFCT = DATA POOL FILE NUMBER OF OSCAR FILE
!     IOSHDR = ARRAY CONTAINING HEADER RECORD FOR XOSCAR FILE IN IDP.
!     PTFCT  = PROBLEM TAPE FILE POSITION
!     EORFLG = END OF RECORD FLAG
!     ICST   = COLD START FLAG
!     IUNST  = UNMODIFIED RESTART
!     IMST   = MODIFIED   RESTART
!
!           ** VARIABLES USED IN GINO CALLS **
!     NPTBUF = NEW PROBLEM TAPE BUFFER AREA
!     IOPBUF = OLD PROBLEM TAPE BUFFER AREA
!     IDPBUF = DATA POOL FILE BUFFER AREA
!     NPTWRD = NUMBER OF WORDS READ FROM NEW PROBLEM TAPE
!     IOPWRD = NUMBER OF WORDS READ FROM OLD PROBLEM TAPE
!     IDPWRD = NUMBER OF WORDS READ FROM DATA POOL FILE
!
!           ** SYMBOLS EQUATED TO CONSTANTS **
!     NPT    = NEW PROBLEM TAPE GINO I.D. NAME (NPTP)
!     IOP    = OLD PROBLEM TAPE GINO I.D. NAME (OPTP)
!     IDP    = DATA POOL FILE GINO I.D. NAME   (POOL)
!     NSCR   = SCRATCH FILE USED FOR RIGID FORMAT. DATA IN NSCR WAS
!              PASSED OVER BY XCSA. IT MUST BE THE LAST SCRATCH FILE
!              IN LINK1, AND NOT TO BE OVER WRITTEN BY XSORT2
!              (CURRENTLY, NSCR = 315)
!
   DATA jcard , jfile/4HCARD , 4HFILE/ , idp/4HPOOL/
   DATA npvt/4HPVT / , ixtim/4HXTIM/ , npt/4HNPTP/ , iop/4HOPTP/ , idpwrd/0/ , iopwrd/0/ , ndpl/4HDPL / , ioshdr/4HXOSC , 4HAR  / , &
      & nptwrd/0/ , filcon/0/ , nxvps/4HXVPS/
   DATA itype/1 , 1 , 2 , 2 , 2 , 4/
   DATA nxcsa/4HXCSA/ , nxaltr/4HXALT/ , nparam/216/
   DATA nxptdc/4HXPTD , 4HIC  / , nxgpi/4HXGPI , 4H    /
!
!
!     LOAD COMMON AREAS AND PERFORM INITIAL CALCULATIONS
!
   CALL xgpidd
   CALL xmpldd
   CALL xlnkdd
!
!     INITIALIZE
!
   nscr = 315
   CALL sswtch(4,diag4)
   CALL sswtch(14,diag14)
   CALL sswtch(17,diag17)
   CALL sswtch(25,diag25)
   IF ( diag14==1 ) iflg(3) = 1
   IF ( diag17==1 ) iflg(4) = 1
   IF ( diag4==1 ) iflg(6) = 1
   IF ( diag4/=0 ) iflg(5) = orf(iflg(5),lshift(1,16))
   IF ( diag25==1 ) iflg(5) = 1
!
!     SET DMAP COMPILER DEFAULT OPTION TO LIST FOR
!     APPROACH DMAP RUNS, RESTART RUNS AND SUBSTRUCTURE RUNS
!     RESET TO NO LIST IF ECHO=NONO (IECHO=-2)
!
   IF ( iecho/=-2 .AND. (apprch<2 .OR. sscell/=0) ) iflg(3) = 1
   IF ( diag14==0 .AND. iflg(3)==1 ) diag14 = 2
   IF ( iecho==-2 ) diag14 = 0
   CALL xgpimw(1,0,0,0)
!
   CALL xgpibs
   IF ( nogo>1 ) GOTO 5200
!
!     SET UP GINO BUFFER AREAS FOR OLD PROBLEM TAPE,NEW PROBLEM TAPE
!     AND DATA POOL TAPE.
!
   loscar = korsz(ibufr)
   nptbuf = loscar - ibufsz
!
!     OLD PROBLEM TAPE AND NEW PROBLEM TAPE SHARE BUFFER
!
   iopbuf = nptbuf
   idpbuf = nptbuf - ibufsz
   loscar = idpbuf - 1
!
!     ALLOW MINIMAL SIZE FOR MED ARRAY RESIDING IN OPEN CORE.
!     WE WILL EXPAND MED IF NECESSARY.
!
   medtp = loscar
   lmed = 1
   IF ( loscar<1 ) THEN
!
!     NOT ENOUGH CORE FOR GPI TABLES
!
      CALL xgpidg(38,-loscar,0,0)
!
!     TERMINATE JOB IF NOGO = 1
!
      nogo = 2
      GOTO 5200
   ELSE
!
!     OPEN NEW PROBLEM TAPE AS INPUT FILE
!
      CALL open(*3800,npt,ibufr(nptbuf),0)
!
!     NUMBER OF FILE ON NPT + 1
!
      nrlfl = lshift(tapid(6),16) + 5
!
!     FILE POSITION OF IOP AT ENTRY TO XGPI
!
      ptfct = lshift(otapid(6),16) + 4
!
!     FIND XCSA FILE ON NEW PROBLEM TAPE
!
      nam1 = nxcsa
      nam2 = nblank
   ENDIF
   DO
!
!     SKIP HEADER FILE
!
      CALL skpfil(npt,1)
      CALL read(*4300,*4300,npt,icf,1,1,nptwrd)
!
!     CHECK FOR ALTER FILE
!     SET DIAG14 TO 10 IF ALTER CARDS ARE PRESENT. DIAG14 WOULD BE
!     CHANGED TO 11 IF DMAP CONTAINS POTENTIAL FATAL ERROR. IN SUCH
!     CASE, DMAP LISTING WILL BE PRINTED.
!
      IF ( icf(1)==nxaltr ) THEN
         nrlfl = nrlfl + 1
         IF ( diag14==0 ) diag14 = 10
      ENDIF
!
!     CHECK FOR CHECKPOINT DICTIONARY FILE
!
      IF ( icf(1)==nxptdc(1) ) nrlfl = nrlfl + 1
!
!     CHECK FOR CONTROL FILE
!
      IF ( icf(1)==nxcsa ) THEN
!
!     PROBLEM TAPE IS POSITIONED AT EXECUTIVE CONTROL FILE.
!
         icfpnt = icftop
!
!     READ THE SIX-WORD DATA RECORD
!
         CALL read(*4300,*100,npt,isol,7,1,nptwrd)
         GOTO 4300
      ENDIF
   ENDDO
 100  CALL close(npt,1)
   IF ( iabs(apprch)==1 ) GOTO 700
!
!     FILL MED ARRAY
!
   medtp = 1
!
!     SET VALUE FOR NUMBER OF WORDS PER MED ENTRY
!
   med(medtp+1) = 1
   IF ( start/=icst ) med(medtp+1) = nmskcd + nmskfl + nmskrf
!
   CALL gopen(nscr,ibufr(nptbuf),0)
   lloscr = loscar - 2
!
!     READ THE MED TABLE
!
   CALL read(*4400,*200,nscr,med(medtp+2),lloscr,1,lmed)
!
!     MED TABLE OVERFLOW
!
   CALL xgpidg(14,nmed,nblank,0)
   nogo = 2
   GOTO 5200
!
!     SET VALUE FOR NUMBER OF DMAP INSTRUCTIONS
!
 200  med(medtp) = lmed/med(medtp+1)
!
!     CHECK FOR ILLEGAL NUMBER OF WORDS IN MED TABLE RECORD
!
   IF ( start/=icst .AND. lmed/=med(medtp)*med(medtp+1) ) THEN
!
!     ILLEGAL NUMBER OF WORDS IN MED TABLE RECORD
!
      CALL xgpidg(71,lmed,0,0)
      nogo = 2
      GOTO 5200
   ELSE
!
!     SET THE POINTERS TO THE FILE NAME AND CARD NAME TABLES
!
      fnmtp = medtp + lmed + 2
      cnmtp = fnmtp
      IF ( start==icst ) GOTO 600
      lloscr = lloscr - lmed
!
!     READ THE FILE NAME TABLE
!
      CALL skprec(nscr,1)
      jtype = jfile
      CALL read(*4500,*300,nscr,med(fnmtp+1),lloscr,1,lmed)
      CALL xgpidg(14,nmed,nblank,0)
      nogo = 2
      GOTO 5200
   ENDIF
!
!     SET THE VALUE FOR THE NUMBER OF ENTRIES IN THE FILE NAME TABLE
!
 300  med(fnmtp) = lmed/3
!
!     CHECK FOR ILLEGAL NUMBER OF WORDS IN FILE NAME TABLE RECORD
!
   IF ( lmed/=3*med(fnmtp) ) THEN
!
!     ILLEGAL NUMBER OF WORDS IN CARD OR FILE NAME TABLE RECORD
!
      CALL xgpidg(72,lmed,jtype,0)
      nogo = 2
   ELSE
!
!     CHECK FOR ILLEGAL BIT NUMBERS IN FILE NAME TABLE
!
      istrbt = 31*nmskcd + 1
      iendbt = 31*(nmskcd+nmskfl)
      DO j = 3 , lmed , 3
         IF ( med(fnmtp+j)<istrbt .OR. med(fnmtp+j)>iendbt ) GOTO 4600
      ENDDO
!
!     RESET THE POINTER FOR THE CARD NAME TABLE
!
      cnmtp = fnmtp + 3*fnm(fnmtp) + 1
      lloscr = lloscr - lmed
!
!     READ THE CARD NAME TABLE
!
      CALL skprec(nscr,-2)
      jtype = jcard
      CALL read(*4500,*400,nscr,med(cnmtp+1),lloscr,1,lmed)
      CALL xgpidg(14,nmed,nblank,0)
      nogo = 2
   ENDIF
   GOTO 5200
!
!     SET THE VALUE FOR THE NUMBER OF ENTRIES IN THE CARD NAME TABLE
!
 400  med(cnmtp) = lmed/3
!
!     CHECK FOR ILLEGAL NUMBER OF WORDS IN CARD NAME TABLE RECORD
!
   IF ( lmed/=3*med(cnmtp) ) THEN
      CALL xgpidg(72,lmed,jtype,0)
      nogo = 2
      GOTO 5200
   ELSE
!
!     CHECK FOR ILLEGAL BIT NUMBERS IN CARD NAME TABLE
!
      istrbt = 1
      iendbt = 31*nmskcd
      DO j = 3 , lmed , 3
         IF ( med(cnmtp+j)<istrbt .OR. med(cnmtp+j)>iendbt ) GOTO 4600
      ENDDO
!
!     RESTART - CHECK MEDMSK TABLE
!     IF MEDMSK WORD(S), CORRESPONDING TO RIGID FORMAT SWITCH, IS(ARE)
!     NON-ZERO, SOLUTION HAS BEEN CHANGED.
!     RESET ENTRY SEQUENCE NO. TO INFINITE IF SOLUTION IS CHANGED.
!
      nmask = med(medtp+1)
      ibegn = nmskcd + nmskfl + 1
      DO i = ibegn , nmask
         IF ( medmsk(i)/=0 ) THEN
            seqno = masklo
            start = imst
         ENDIF
      ENDDO
!
!     SEE IF ANY BULK DATA OR CASE CONTROL CARDS HAVE BEEN MODIFIED.
!
      bgnmsk = 1
      endmsk = lbd + lcc
!
!     TURN OFF BIT IN MJMSK ARRAY IF THE CORRESPONDING CARD NAME
!     IS NOT IN THE CARD NAME RESTART TABLE
!
      i1 = cnmtp + 1
      i2 = i1 + 3*cnm(cnmtp) - 3
      DO lx = bgnmsk , endmsk
         IF ( mjmsk(lx)/=0 ) THEN
            l = lx - bgnmsk + 1
            DO l1 = 2 , 32
               IF ( andf(mjmsk(lx),two(l1))/=0 ) THEN
!
!     IGNORE BIT IF IT CORRESPONDS TO QOUT$ OR BOUT$
!
                  IF ( .NOT.(lx==lbd+2 .AND. (l1==3 .OR. l1==4)) ) THEN
                     i = 62*(l-1) + 2*(l1-2) + 1
                     DO ii = i1 , i2 , 3
                        IF ( mjcd(i)==cnm(ii) .AND. mjcd(i+1)==cnm(ii+1) ) GOTO 410
                     ENDDO
                     ii = complf(two(l1))
                     mjmsk(lx) = andf(mjmsk(lx),ii)
                  ENDIF
               ENDIF
 410        ENDDO
         ENDIF
      ENDDO
      IF ( start/=imst ) THEN
!
!     DETERMINE TYPE OF RESTART
!
         index = 0
         iend = lbd
         DO l = bgnmsk , iend
            IF ( mjmsk(l)/=0 ) THEN
               index = 1
               EXIT
            ENDIF
         ENDDO
      ENDIF
      l = lbd + 1
      IF ( start/=imst ) THEN
         IF ( index/=1 ) THEN
            IF ( mjmsk(l)/=0 ) THEN
!
!     CHECK FOR NOLOOP$ AND LOOP$
!                                          2**21
               IF ( mjmsk(l)/=1 .AND. mjmsk(l)/=two(11) ) GOTO 420
            ENDIF
!
!     CHECK FOR GUST$
!                         2**30
            IF ( mjmsk(l+1)<two(2) ) GOTO 450
         ENDIF
 420     start = imst
      ENDIF
!
!     TURN ON POUT$ IF QOUT$ IS ON
!                         2**29                                  2**14
      IF ( andf(mjmsk(l+1),two(3))/=0 ) mjmsk(l) = orf(mjmsk(l),two(18))
!
!     TURN ON AOUT$ IF BOUT$ IS ON
!                         2**28                                  2**22
      IF ( andf(mjmsk(l+1),two(4))/=0 ) mjmsk(l) = orf(mjmsk(l),two(10))
!
!     TURN OFF BOUT$ AND QOUT$
!                 2**28    2**29
      ii = complf(two(4)+two(3))
      mjmsk(l+1) = andf(mjmsk(l+1),ii)
!
!     TURN OFF NOLOOP$ FOR UNMODIFIED RESTARTS
!
 450  IF ( start==iunst .AND. mjmsk(lbd+1)==1 ) mjmsk(lbd+1) = 0
   ENDIF
 500  CALL page1
   IF ( start/=iunst ) THEN
      CALL page2(-2)
      IF ( seqno/=masklo ) WRITE (optape,99001) uim
99001 FORMAT (A29,' 4144, THIS IS A MODIFIED RESTART.')
      IF ( seqno==masklo ) WRITE (optape,99002) uim
99002 FORMAT (A29,' 4145, THIS IS A MODIFIED RESTART INVOLVING RIGID ','FORMAT SWITCH.')
      ibulk = 0
      icase = 0
      DO l = 1 , lbd
         IF ( mjmsk(l)/=0 ) THEN
            ibulk = 1
            EXIT
         ENDIF
      ENDDO
      lbd1 = lbd + 1
      lbdlcc = lbd + lcc
      DO l = lbd1 , lbdlcc
         IF ( mjmsk(l)/=0 ) THEN
            icase = 1
            EXIT
         ENDIF
      ENDDO
      IF ( ibulk/=0 .OR. icase/=0 ) THEN
         CALL page2(-4)
         WRITE (optape,99003) uim
99003    FORMAT (A29,'. CASE CONTROL AND BULK DATA DECK CHANGES AFFECTING',' THIS RESTART ARE INDICATED BELOW.',/)
         DO llx = 1 , 2
            IF ( llx==1 ) THEN
               CALL page2(-3)
               WRITE (optape,99004) uim
99004          FORMAT (A29,'. EFFECTIVE CASE CONTROL DECK CHANGES',/1X,35(1H-))
               IF ( icase/=0 ) THEN
                  CALL page2(-3)
                  IF ( apprch/=-1 ) WRITE (optape,99005)
99005             FORMAT (//,' MASK WORD - BIT POSITION ---- FLAG NAME ---- PACKED',' BIT POSITION',/)
                  IF ( apprch==-1 ) WRITE (optape,99006)
99006             FORMAT (//,' MASK WORD - BIT POSITION ---- FLAG NAME',/)
                  lim1 = lbd1
                  lim2 = lbdlcc
               ELSE
                  CALL page2(-3)
                  WRITE (optape,99017)
                  CYCLE
               ENDIF
            ELSE
               CALL page2(-3)
               WRITE (optape,99007) uim
99007          FORMAT (A29,'. EFFECTIVE BULK DATA DECK CHANGES',/1X,32(1H-))
               IF ( ibulk/=0 ) THEN
                  CALL page2(-3)
                  IF ( apprch/=-1 ) WRITE (optape,99008)
99008             FORMAT (//,' MASK WORD - BIT POSITION - CARD/PARAM NAME - PACKED',' BIT POSITION',/)
                  IF ( apprch==-1 ) WRITE (optape,99009)
99009             FORMAT (//,' MASK WORD - BIT POSITION - CARD/PARAM NAME',/)
                  lim1 = 1
                  lim2 = lbd
               ELSE
                  CALL page2(-3)
                  WRITE (optape,99017)
                  CYCLE
               ENDIF
            ENDIF
            DO l = lim1 , lim2
               IF ( mjmsk(l)/=0 ) THEN
                  CALL page2(-1)
                  WRITE (optape,99010) l
99010             FORMAT (1X,I5)
                  DO k = 2 , 32
                     IF ( andf(mjmsk(l),two(k))/=0 ) THEN
!
!     GET CORRESPONDING CARD NAME FROM MAIN CARD TABLE
!
                        i = 62*(l-1) + 2*(k-2) + 1
                        kz = k - 1
                        CALL page2(-1)
                        IF ( apprch/=-1 ) THEN
!
!     SEARCH RIGID FORMAT CARD NAME RESTART TABLE FOR A MATCH
!
                           DO ii = i1 , i2 , 3
                              IF ( mjcd(i)==cnm(ii) .AND. mjcd(i+1)==cnm(ii+1) ) THEN
!
!     CARD NAME FOUND - SET BIT IN MEDMSK
!
                                 WRITE (optape,99018) kz , mjcd(i) , mjcd(i+1) , cnm(ii+2)
                                 l1 = (cnm(ii+2)-1)/31
                                 ll = l1 + 1
                                 kk = cnm(ii+2) - 31*l1 + 1
                                 medmsk(ll) = orf(medmsk(ll),two(kk))
                                 GOTO 502
                              ENDIF
                           ENDDO
                           WRITE (optape,99019) sfm
                           WRITE (optape,99011) mjcd(i) , mjcd(i+1) , (cnm(ll),cnm(ll+1),ll=i1,i2,3)
99011                      FORMAT (/10X,2A4,//,10(4X,2A4))
                           CALL mesage(-61,0,0)
                        ELSE
                           WRITE (optape,99018) kz , mjcd(i) , mjcd(i+1)
                        ENDIF
                     ENDIF
 502              ENDDO
               ENDIF
            ENDDO
         ENDDO
         IF ( apprch==-1 ) GOTO 800
      ELSE
         IF ( seqno/=masklo ) THEN
            WRITE (optape,99019)
            CALL mesage(-61,0,0)
         ENDIF
         WRITE (optape,99012) uim
99012    FORMAT (A29,'. THERE ARE NO CASE CONTROL OR BULK DATA DECK ','CHANGES AFFECTING THIS RESTART.')
      ENDIF
   ELSE
      WRITE (optape,99013) uim
99013 FORMAT (A29,' 4143, THIS IS AN UNMODIFIED RESTART.')
      bandit = -1
      IF ( apprch==-1 ) GOTO 800
   ENDIF
!
!     MOVE MED AND FILE NAME TABLES TO BOTTOM OF OPEN CORE.
!
 600  CALL close(nscr,1)
   lmed = cnmtp - medtp
   DO i = 1 , lmed
      ll = medtp + lmed - i
      m = loscar - i + 1
      med(m) = med(ll)
   ENDDO
   medtp = loscar - lmed + 1
   fnmtp = medtp + med(medtp)*med(medtp+1) + 2
   loscar = medtp - 1
!
!     DETERMINE TYPE OF RESTART IF IT IS A RESTART OF A DMAP RUN
!
 700  IF ( apprch==-1 ) THEN
      IF ( mjmsk(lbd+1)/=0 ) THEN
!
!     CHECK FOR NOLOOP$ AND LOOP$
!                                                  2**21
         IF ( mjmsk(lbd+1)/=1 .AND. mjmsk(lbd+1)/=two(11) ) GOTO 750
         mjmsk(lbd+1) = 0
      ENDIF
!
!     CHECK FOR GUST$
!                           2**30
      IF ( mjmsk(lbd+2)<two(2) ) THEN
         DO l = 1 , lbd
            IF ( mjmsk(l)/=0 ) GOTO 750
         ENDDO
         GOTO 500
      ENDIF
 750  start = imst
      seqno = lshift(1,16)
      GOTO 500
   ENDIF
!
!     CONTROL FILE LOADED, LOAD PVT TABLE
!     BUMP NUMBER OF FILES IF OLD PROBLEM TAPE HAD ALTERS
!
 800  ptfct = ptfct + alter(2)
   itrl(1) = nparam
   CALL rdtrl(itrl(1))
   IF ( itrl(2)<=0 ) GOTO 1300
   CALL open(*3800,nparam,ibufr(nptbuf),0)
   CALL read(*1300,*900,nparam,pvt(6),2,1,nptwrd)
 900  IF ( pvt(6)/=npvt ) GOTO 4300
   i = 3
!
!      LOAD PVT VALUES INTO PVT TABLES
!
 1000 CALL read(*1200,*1100,nparam,pvt(i),pvt(1)-i+1,0,nptwrd)
!
!     PVT TABLE OVERFLOW
!
   CALL xgpidg(14,npvt,nblank,0)
   GOTO 5200
 1100 i = i + nptwrd
   GOTO 1000
 1200 pvt(2) = i - 1
   CALL close(nparam,1)
!
!     ELIMINATE TRAILER SO FILE WILL BE DELETED
!
   DO i = 2 , 7
      itrl(i) = 0
   ENDDO
   CALL wrttrl(itrl(1))
 1300 IF ( start==icst ) GOTO 2000
   IF ( apprch==-1 .AND. start==imst ) GOTO 2000
!
!     INITIALIZE VPS TABLE FOR RESTART
!     GET FIRST ENTRY IN CHECKPOINT DICTIONARY
!
   ptdtop = 1
   ASSIGN 1400 TO irturn
   GOTO 2400
 1400 i = ptdtop
   IF ( ptdic(ptdtop)/=nxvps ) GOTO 2000
!
!     FIRST ENTRY IN CHECKPOINT DICTIONARY IS XVPS - GET FILE OFF OF OLD
!     PROBLEM TAPE, OPTP
!
   CALL open(*3900,iop,ibufr(iopbuf),2)
!
!     CHECK TO SEE IF OLD RESTART TAPE HAS PVT  J = 0 WITHOUT PVT
!
   j = andf(maskhi,ptdic(ptdtop+2)) - (andf(maskhi,ptfct)+1)
   ptfct = ptfct + j
   CALL skpfil(iop,j)
   CALL read(*5000,*1500,iop,vps(3),2,1,iopwrd)
 1500 IF ( vps(3)/=nxvps .OR. vps(4)/=nblank ) GOTO 5000
   j = vps(1)
   CALL read(*5000,*1600,iop,vps,j,1,iopwrd)
 1600 CALL skpfil(iop,1)
   CALL close(iop,2)
   ptfct = ptfct + 1
   vps(1) = j
!
!     FOR RESTART COMPARE PVT VALUES WITH VPS VALUES. IF NOT EQUAL SET
!     MODFLG INVPS ENTRY.
!
   IF ( pvt(2)<=2 ) GOTO 1800
   i = 3
   j = 3
 1700 DO WHILE ( pvt(2)>=j )
      IF ( pvt(j)==vps(i) .AND. pvt(j+1)==vps(i+1) ) THEN
!
!     FOUND VARIABLE IN PVT TABLE
!
         l = andf(vps(i+2),maskhi)
         pvt(j+2) = orf(pvt(j+2),isgnon)
         DO ll = 1 , l
            ii = i + ll + 2
            jj = j + ll + 2
            vps(i+2) = orf(vps(i+2),modflg)
            vps(ii) = pvt(jj)
         ENDDO
         EXIT
      ELSE
         jj = andf(pvt(j+2),nosgn)
         j = j + itype(jj) + 3
      ENDIF
   ENDDO
   i = i + andf(vps(i+2),maskhi) + 3
   IF ( i<vps(2) ) THEN
      j = 3
      GOTO 1700
   ENDIF
 1800 i = lbd + lcc + 1
   iparpt = mjmsk(i)
   iparw1 = (iparpt-1)/31 + 1
   iparw2 = lbd
   iparbt = mod(iparpt-1,31) + 2
   idelet = 0
   DO j1 = iparw1 , iparw2
      IF ( mjmsk(j1)/=0 ) GOTO 1900
   ENDDO
   idelet = 1
   GOTO 2000
 1900 DO j1 = iparw1 , iparw2
      IF ( mjmsk(j1)/=0 ) THEN
         DO i1 = iparbt , 32
            IF ( andf(mjmsk(j1),two(i1))/=0 ) THEN
               nampt = 2*(31*(j1-1)+i1-1) - 1
               i2 = 3
               DO WHILE ( mjcd(nampt)/=vps(i2) .OR. mjcd(nampt+1)/=vps(i2+1) )
                  i2 = i2 + andf(vps(i2+2),maskhi) + 3
                  IF ( i2>=vps(2) ) GOTO 1920
               ENDDO
               IF ( andf(vps(i2+2),two(2))==0 ) THEN
                  vps(i2) = nblank
                  vps(i2+1) = nblank
               ENDIF
            ENDIF
 1920    ENDDO
      ENDIF
      iparbt = 2
   ENDDO
!
!     DMAP SEQUENCE COMPILATION - PHASE 1
!     ***********************************
!
!     GENERATE OSCAR
!     POSITION NEW PROBLEM TAPE AT ALTER FILE IF IT EXISTS
!
 2000 IF ( alter(1)==0 ) GOTO 2300
   nam1 = nxaltr
   nam2 = nblank
   CALL open(*3800,npt,ibufr(nptbuf),0)
 2100 CALL skpfil(npt,1)
   CALL read(*4300,*2200,npt,icf,2,1,nptwrd)
 2200 IF ( icf(1)/=nxaltr ) GOTO 2100
!
!     ALTER FILE FOUND - INITIALIZE ALTER CELLS
!
   CALL read(*4300,*4300,npt,alter,2,1,nptwrd)
 2300 CALL open(*4700,nscr,ibufr(idpbuf),0)
   CALL xgpimw(1,1,0,0)
   CALL xosgen
   IF ( start/=icst ) THEN
      DO i = 1 , nmask
         medmsk(i) = 0
      ENDDO
   ENDIF
   IF ( alter(1)/=0 ) CALL close(npt,2)
   IF ( pvt(2)>2 ) THEN
      j = 5
      DO WHILE ( pvt(2)>=j )
         IF ( pvt(j)>=0 ) CALL xgpidg(-54,0,pvt(j-2),pvt(j-1))
         jj = andf(pvt(j),nosgn)
         j = j + itype(jj) + 3
      ENDDO
   ENDIF
   IF ( nogo==2 ) GOTO 5200
   CALL close(nscr,1)
   IF ( start/=icst ) CALL xgpimw(2,0,0,0)
   CALL xgpimw(1,0,0,0)
!
!     ALLOW MINIMAL SIZE FOR PTDIC ARRAY IN OPEN CORE.
!     WE WILL EXPAND IF THIS IS RESTART.
!
   ptdtop = oscar(osbot) + osbot
   ptdbot = ptdtop
   lptdic = 3
   ASSIGN 2800 TO irturn
!
 2400 IF ( start==icst ) GOTO 2800
!
!     RESTART - LOAD OLD PROBLEM TAPE DICTIONARY INTO OPEN CORE.
!
   CALL open(*3800,npt,ibufr(nptbuf),0)
!
!     FIND XPTDIC ON NEW PROBLEM TAPE
!
   nam1 = nxptdc(1)
   nam2 = nxptdc(2)
 2500 CALL skpfil(npt,1)
   CALL read(*4300,*2600,npt,ptdic(ptdtop),2,1,nptwrd)
 2600 IF ( ptdic(ptdtop)==nxcsa ) GOTO 4300
   IF ( ptdic(ptdtop)/=nxptdc(1) ) GOTO 2500
!
!     FOUND XPTDIC
!
   lptdic = loscar - ptdtop
   CALL read(*4300,*2700,npt,ptdic(ptdtop),lptdic,1,nptwrd)
!
!     XPTDIC OVERFLOWED
!
   CALL xgpidg(14,nxptdc(1),nxptdc(2),0)
   nogo = 2
   GOTO 5200
 2700 ptdbot = ptdtop + nptwrd - 3
   CALL close(npt,1)
   GOTO irturn
!
!     IF BOTH DIAGS 14 AND 20 ARE ON, TERMINATE JOB
!
 2800 IF ( diag14==1 ) THEN
      CALL sswtch(20,i)
      IF ( i/=0 ) THEN
         WRITE (optape,99014)
99014    FORMAT (//' *** JOB TERMINATED BY DIAG 20',//)
         CALL pexit
      ENDIF
   ENDIF
!
!     DMAP SEQUENCE COMPILATION - PHASE 2
!     ***********************************
!
!     COMPUTE NTU AND LTU FOR DATA SETS IN OSCAR
!
   IF ( nogo/=0 .AND. start/=icst .AND. ptdtop==ptdbot ) GOTO 5200
   CALL xflord
   IF ( diag14==11 ) THEN
!
!     USER DMAP ALTER CONTAINS ERROR, DIAG 14 FLAG IS NOT REQUESTED, AND
!     ECHO IS NOT 'NONO', PRINT RIGID FORMAT BEFORE QUITTING
!
      IF ( iecho/=-2 ) CALL xgpimw(13,0,0,core)
      GOTO 5200
   ELSE
      IF ( nogo/=0 .OR. lnogo ) GOTO 5200
      IF ( diag4/=0 ) CALL dumper
!
!     PURGE ALL FILES IN FIAT TABLE THAT HAVE NOT BEEN GENERATED BY
!     IFP SUBROUTINE
!
      i = ifiat(1)*icfiat - 2
      DO k = 4 , i , icfiat
         IF ( ifiat(k+1)/=0 ) THEN
            IF ( ifiat(k+3)/=0 .OR. ifiat(k+4)/=0 .OR. ifiat(k+5)/=0 ) CYCLE
            IF ( icfiat==11 .AND. (ifiat(k+8)/=0 .OR. ifiat(k+9)/=0 .OR. ifiat(k+10)/=0) ) CYCLE
!
!     FILE NOT GENERATED - PURGE IT.
!
            k1 = ifiat(3)*icfiat + 4
            ifiat(3) = ifiat(3) + 1
            ifiat(k1) = orf(andf(ifiat(k),masklo),maskhi)
            ifiat(k) = andf(ifiat(k),orf(maskhi,losgn))
            ifiat(k1+1) = ifiat(k+1)
            ifiat(k1+2) = ifiat(k+2)
         ENDIF
!
!     MAKE SURE NO RESIDUE LEFT IN FIAT TABLE
!
         j1 = k + 1
         j2 = k + icfiat - 1
         DO j = j1 , j2
            ifiat(j) = 0
         ENDDO
!
      ENDDO
!
!     WRITE OSCAR ON DATA POOL FILE.
!
!     PUT OSCAR NAME IN DPL AND ASSIGN FILE NO.
!
      lstdpl = lstdpl + 1
      i = lstdpl*3 + 1
      dpl(i) = ioshdr(1)
      dpl(i+1) = ioshdr(2)
      dpl(i+2) = ndpfil
      ndpfil = 1 + ndpfil
!
!     WRITE OSCAR HEADER RECORD
!     POSITION FILE
!
      IF ( ndpfil/=2 ) THEN
         CALL open(*4200,idp,ibufr(idpbuf),0)
         CALL skpfil(idp,ndpfil-2)
         CALL close(idp,2)
      ENDIF
      idpfct = ndpfil - 1
      CALL open(*4200,idp,ibufr(idpbuf),3)
      CALL write(idp,ioshdr,2,1)
!
!     IF CHECKPOINT AND RESTART FLAGS ARE ON INSERT CHECKPOINT ENTRY IN
!     OSCAR TO SAVE FILES LISTED IN ICPDPL TABLE
!
      IF ( start/=icst ) THEN
         IF ( icpbot>=icptop .AND. icpflg/=0 ) THEN
!
!     CHECKPOINT ALL FILES LISTED IN ICPDPL
!
            cpntry(7) = (icpbot-icptop+3)/3
            cpntry(1) = 7 + cpntry(7)*2
!
!     FOR UNMODIFIED RESTART - DMAP SEQUENCE NO. OF THIS INITIAL
!     CHECKPOINT MUST = REENTRY POINT - 1
!
            IF ( start==iunst ) cpntry(6) = orf(isgnon,rshift(andf(seqno,masklo),16)-1)
            CALL write(idp,cpntry,7,0)
            DO i = icptop , icpbot , 3
               CALL write(idp,icpdpl(i),2,0)
            ENDDO
            CALL write(idp,0,0,1)
         ELSE
            cpntry(6) = 1
            CALL write(idp,cpntry,6,1)
         ENDIF
!
!     FOR RESTART - INSERT JUMP IN OSCAR TO POSITION OSCAR AT CORRECT
!     REENTRY POINT
!     FOR MODIFIED RESTART - START AT FIRST EXECUTABLE MODULE
!
         IF ( start==imst ) jmp(6) = 1
!
!     SEE IF RE-ENTRY POINT IS WITHIN BOUNDS UNLESS SOLUTION CHANGED.
!
         IF ( andf(seqno,masklo)/=masklo ) THEN
            i = andf(seqno,maskhi)
            IF ( i>oscar(osbot+1) .OR. i==0 ) THEN
!
!     REENTRY POINT NOT WITHIN BOUNDS
!
               CALL xgpidg(46,0,0,0)
               GOTO 5200
            ELSE
               jmp(7) = lshift(i,16)
            ENDIF
         ENDIF
         CALL write(idp,jmp,7,1)
      ENDIF
      ospnt = 1
   ENDIF
   DO
!
!     WRITE NEXT OSCAR ENTRY ON DATA POOL TAPE
!
      CALL write(idp,oscar(ospnt),oscar(ospnt),1)
      IF ( oscar(ospnt+3)/=ixtim ) THEN
         i = andf(oscar(ospnt+2),maskhi)
         IF ( i<=2 .AND. oscar(ospnt+5)<0 ) THEN
!
!     MAKE SURE SYSTEM HAS ENOUGH FILES AVAILABLE TO HANDLE MODULE
!     REQUIREMENTS.
!     COUNT NUMBER OF I/P AND O/P FILES NEEDED
!
            j1 = 2
            IF ( i==2 ) j1 = 1
            k = 0
            l = ospnt + 6
            DO j = 1 , j1
               l2 = oscar(l)*3 - 2 + l
               l1 = l + 1
               IF ( oscar(l1-1)/=0 ) THEN
                  DO l = l1 , l2 , 3
                     IF ( oscar(l)/=0 ) k = k + 1
                  ENDDO
               ENDIF
               l = l2 + 3
            ENDDO
!
!     ADD ON NUMBER OF SCRATCH FILES NEEDED
!
            k = k + oscar(l)
!
!     NOT ENOUGH FILES AVAILABLE FOR MODULE REQUIREMENTS.
!
!
!     OSCAR ENTRY IS XTIME, COMPUTE ROUGH TIME ESTIMATES FOR MODULES IN
!     TIME SEGMENT, AND
!     WRITE XTIME HEADER AND TIME ESTIMATES ONTO DATA POOL
!     (THIS SECTION TEMPORARILY OMITTED)
!
            IF ( ifiat(1)<k ) CALL xgpidg(-37,ospnt,k,ifiat(1))
         ENDIF
      ENDIF
!
!     INCREMENT OSPNT AND CHECK FOR END OF OSCAR
!
      ospnt = ospnt + oscar(ospnt)
      IF ( ospnt>osbot ) THEN
         CALL eof(idp)
         IF ( start==icst ) GOTO 3700
!
!
!     *** RESTART ***
!
         IF ( icpbot<icptop ) GOTO 3700
!
!     LIST ICPDPL CONTENTS
!
         CALL xgpimw(8,icptop,icpbot,icpdpl)
!
!     ELIMINATE PURGED FILES FROM ICPDPL
!
         i1 = icptop
         DO i = i1 , icpbot , 3
            IF ( andf(icpdpl(i+2),maskhi)/=0 ) EXIT
            icptop = icptop + 3
         ENDDO
         IF ( icpbot<icptop ) GOTO 3700
         CALL close(idp,2)
         ib1s = idpbuf
         idpbuf = icpbot + 3
         iopbuf = idpbuf + ibufsz
         CALL gopen(idp,ibufr(idpbuf),3)
!
!     TRANSFER CHECKPOINT INFO FROM OLD PROBLEM TAPE TO DATA POOL TAPE
!
         k = lstdpl*3 + 4
         CALL open(*3900,iop,ibufr(iopbuf),2)
         DO i = icptop , icpbot , 3
            dpl(k+2) = 0
            IF ( andf(icpdpl(i+2),noflgs)>ptfct ) THEN
!
!     MAKE SURE CORRECT REEL IS MOUNTED FOR OLD PROBLEM TAPE
!
               IF ( andf(andf(noflgs,masklo),icpdpl(i+2))==andf(masklo,ptfct) ) GOTO 2840
!
!     ** NEW REEL NEEDED **
!     MOUNT REEL SPECIFIED BY ICPDPL ENTRY
!
               otapid(6) = rshift(andf(noflgs,icpdpl(i+2)),16)
               wrngrl = 0
            ELSE
!
!     FILE IS EQUIVALENCED TO PREVIOUS ENTRY IN DPL
!
               ndpfil = ndpfil - 1
               dpl(k+2) = dpl(k-1)
               GOTO 2880
            ENDIF
!
!     SEND OPERATOR MESSAGE
!
 2810       CALL xeot(iop,rshift(ptfct,16),otapid(6),ibufr(iopbuf))
            CALL open(*3900,iop,ibufr(iopbuf),0)
            CALL read(*4900,*2820,iop,ibf,libf,0,iopwrd)
!
!     SEE THAT CORRECT REEL HAS BEEN MOUNTED.
!
 2820       DO ii = 1 , 6
               IF ( otapid(ii)/=ibf(ii) ) GOTO 2830
            ENDDO
!
!     CORRECT REEL MOUNTED - CARRY ON
!
            CALL skpfil(iop,1)
            ptfct = lshift(otapid(6),16) + 1
            IF ( filcon==0 ) GOTO 2840
            GOTO 2870
 2830       wrngrl = wrngrl + 1
            IF ( wrngrl>=2 ) GOTO 5100
            GOTO 2810
!
!     WRITE FILE ON DATA POOL
!
 2840       CALL skpfil(iop,andf(maskhi,icpdpl(i+2))-(andf(maskhi,ptfct)+1))
!
!     CHECK FOR CORRECT FILE
!
!     5 OR 8 WORDS (DEPEND ON ICFIAT VALUE OF 8 OR 11) WRITTEN TO IOP
!     BY XCHK OF PREVIOUS CHECKPOINT RUN.
!     IF ICFIAT=11, READ 5 WORDS HERE FIRST, AND CHECK IF THERE ARE 3
!     MORE WORDS BEHIND.  I.E. OPTP MAY BE WRITTEN WITH A 5-WORD RECORD
!     IF ICFIAT= 8, READ 5 WORDS
!
            IF ( icfiat==11 ) THEN
               ibf(8) = -999
               CALL read(*4900,*4900,iop,ibf(1),5,0,iopwrd)
               CALL read(*4900,*2850,iop,ibf(6),3,1,iopwrd)
            ELSE
               CALL read(*4900,*4900,iop,ibf,5,1,iopwrd)
               ibf(8) = 0
            ENDIF
!
 2850       DO ii = i , icpbot , 3
               IF ( ibf(1)==icpdpl(ii) .AND. ibf(2)==icpdpl(ii+1) ) GOTO 2860
            ENDDO
            GOTO 4900
!
!     A 5-WORD RECORD READ, EXPANDED (THE TRAILERS) TO 8 WORDS
!
 2860       IF ( ibf(8)==-999 ) THEN
               ibf(8) = andf(ibf(5),65535)
               ibf(7) = rshift(ibf(5),16)
               ibf(6) = andf(ibf(4),65535)
               ibf(5) = rshift(ibf(4),16)
               ibf(4) = andf(ibf(3),65535)
               ibf(3) = rshift(ibf(3),16)
            ENDIF
!
!     COPY FILE TO POOL
!
            CALL write(idp,ibf,icfiat-3,1)
 2870       CALL cpyfil(iop,idp,ibf,libf,iopwrd)
            dpl(k+2) = dpl(k+2) + iopwrd/1000 + 1
!
!     FILE ALL ON DATA POOL TAPE
!
            CALL eof(idp)
            filcon = 0
!
!     MAKE DPL ENTRY FOR ICPDPL ENTRY
!
            dpl(k+2) = orf(orf(lshift(dpl(k+2),16),ndpfil),andf(icpdpl(i+2),ieqflg))
 2880       dpl(k) = icpdpl(i)
            dpl(k+1) = icpdpl(i+1)
            IF ( l8/=0 ) CALL conmsg(dpl(k),2,0)
            k = k + 3
            ndpfil = ndpfil + 1
            lstdpl = 1 + lstdpl
            IF ( lstdpl>maxdpl ) GOTO 4800
            ptfct = andf(noflgs,icpdpl(i+2))
         ENDDO
!
!     FILES ALL COPIED OVER FROM OLD PROBLEM TAPE TO DATA POOL TAPE.
!
         CALL close(iop,1)
!
!     SEE IF XVPS IS ON POOL TAPE
!
         k = lstdpl*3 + 1
         l = ndpfil
         IF ( dpl(k)/=nxvps ) THEN
!
!     VPS FILE IS NOT LAST ENTRY IN DPL - SEARCH DPL FOR IT
!
            DO j = 4 , k , 3
               IF ( dpl(j)==nxvps ) GOTO 2900
            ENDDO
!
!     NO RESTART VPS TABLE
!
            GOTO 3700
         ELSE
!
!     VPS FILE IS LAST ENTRY IN DPL - DELETE ENTRY
!
            lstdpl = lstdpl - 1
            ndpfil = ndpfil - 1
            j = k
            GOTO 2920
         ENDIF
!
!     XVPS FOUND - ZERO NAME WHEN NOT LAST ENTRY IN DPL
!
 2900    dpl(j) = 0
         dpl(j+1) = 0
!
!     XVPS FILE FOUND IN DPL - POSITION POOL TAPE AND INITIALIZE
!     VPS TABLE WITH CHECKPOINT VALUES
!
 2920    CALL close(idp,3)
         CALL open(*4200,idp,ibufr(idpbuf),2)
         CALL skpfil(idp,andf(dpl(j+2),maskhi)-l-1)
         nam1 = nxvps
         nam2 = nblank
         CALL skpfil(idp,1)
         CALL read(*4100,*3000,idp,ibf,libf,1,idpwrd)
         EXIT
      ENDIF
   ENDDO
 3000 IF ( ibf(1)/=nxvps ) GOTO 4100
   CALL read(*4100,*3100,idp,ibf,libf,1,idpwrd)
!
!     COMPARE RESTART PARAMETER NAMES WITH VPS NAMES
!
 3100 k = 3
 3200 j = 3
   IF ( andf(vps(k+2),modflg)/=modflg ) THEN
      DO WHILE ( ibf(2)>=j )
         IF ( ibf(j)==vps(k) .AND. ibf(j+1)==vps(k+1) ) THEN
!
!     PARAMETER NAMES MATCH AND MODFLG NOT ON - INITIALIZE VPS WITH
!     RESTART VALUE.
!
            l = ibf(j+2)
            IF ( idelet/=1 ) THEN
               iparbt = mod(iparpt-1,31) + 2
               DO jjj = iparw1 , iparw2
                  IF ( mjmsk(jjj)/=0 ) THEN
                     DO iii = iparbt , 32
                        IF ( andf(mjmsk(jjj),two(iii))/=0 ) THEN
                           nampt = 2*(31*(jjj-1)+iii-1) - 1
                           IF ( mjcd(nampt)==vps(k) .AND. mjcd(nampt+1)==vps(k+1) ) GOTO 3300
                        ENDIF
                     ENDDO
                  ENDIF
                  iparbt = 2
               ENDDO
            ENDIF
            DO m = 1 , l
               j1 = m + 2 + j
               k1 = m + 2 + k
               vps(k1) = ibf(j1)
            ENDDO
            EXIT
         ELSE
            j = j + ibf(j+2) + 3
         ENDIF
      ENDDO
   ENDIF
!
!     CLEAR FLAGS AND TYPE CODE IN VPS ENTRY AND GET NEXT ENTRY.
!
 3300 vps(k+2) = andf(vps(k+2),maskhi)
   k = k + vps(k+2) + 3
   IF ( k<vps(2) ) GOTO 3200
!
!     FOR UNMODIFIED RESTART LOAD CEITBL FROM LAST CHECKPOINT
!
   CALL read(*4100,*3400,idp,ibf,libf,1,idpwrd)
 3400 IF ( start/=imst ) THEN
      k1 = ceitbl(2)
      j1 = ibf(2)
!
!     FOR RESTART INITIALIZE REPT LOOP COUNTS WITH CHECKPOINT INFO
!
      DO j = 3 , j1 , 4
         DO k = 3 , k1 , 4
            IF ( ceitbl(k+2)==ibf(j+2) .AND. ceitbl(k+3)==ibf(j+3) .AND. ibf(j+2)/=0 ) ceitbl(k+1) = ibf(j+1)
         ENDDO
      ENDDO
   ENDIF
!
!     FOR BOTH MOD AND UNMOD RESTART - LOAD VARIOUS CELLS IN /SYSTEM/
!     WITH LAST CHECKPOINT INFO
!
   CALL read(*3600,*3500,idp,ibf,libf,1,idpwrd)
 3500 mpc = ibf(5)
   spc = ibf(6)
   load = ibf(8)
 3600 CALL close(idp,1)
   idpbuf = ib1s
!
!
!     POSITION DATA POOL TAPE AT FIRST OSCAR ENTRY
!
 3700 CALL close(idp,1)
!
!     *** FIRST, PRODUCE DMAP XREF IF REQUESTED
!
   CALL open(*4200,idp,ibufr(idpbuf),2)
   CALL skpfil(idp,idpfct-1)
   CALL fwdrec(*4000,idp)
   IF ( andf(iflg(5),1)/=0 ) CALL oscxrf(idpfct-1,idpbuf-1)
   CALL close(idp,2)
!
!     WRITE VPS TABLE ON NEW PROBLEM TAPE IF CHECKPOINT FLAG ES SET
!     CLEAR FLAGS IN VPS
!
   k = 3
   DO
      vps(k+2) = andf(vps(k+2),maskhi)
      k = k + vps(k+2) + 3
      IF ( k>=vps(2) ) THEN
         IF ( icpflg/=0 ) THEN
!
!     POSITION TAPE FOR WRITING XVPS
!
            CALL open(*3800,npt,ibufr(nptbuf),0)
            CALL skpfil(npt,andf(nrlfl,maskhi)-1)
            CALL close(npt,2)
            CALL open(*3800,npt,ibufr(nptbuf),3)
            ibf(1) = nxvps
            ibf(2) = nblank
            CALL write(npt,ibf,2,1)
            CALL write(npt,vps,vps(2),1)
!
!     WRITE CEITBL TABLE ON NEW PROBLEM TAPE
!
            CALL write(npt,ceitbl,ceitbl(2),1)
            CALL eof(npt)
            CALL close(npt,2)
!
!     INITIALIZE CHECKPOINT PARAMETERS FOR XCHK AND XCEI ROUTINES
!
            ptdic(ptdtop) = nxvps
            ptdic(ptdtop+1) = nblank
            ptdic(ptdtop+2) = nrlfl
            nrlfl = nrlfl + 1
            seqno = 1
!
!     WRITE NEW DICTIONARY ON XPTD
!
            CALL open(*3800,nxptdc,ibufr(nptbuf),1)
            CALL write(nxptdc,nxptdc,2,1)
            CALL write(nxptdc,nrlfl,2,1)
            CALL write(nxptdc,ptdic(ptdtop),3,1)
            CALL close(nxptdc,1)
!
!     PUNCH DICTIONARY ENTRY FOR XVPS TABLE
!
            nfile = andf(maskhi,ptdic(ptdtop+2))
         ENDIF
         IF ( nogo/=0 .OR. lnogo ) GOTO 5200
         CALL xgpimw(9,nfile,icpflg,ifiat)
         cppgct = pagect
         IF ( iflg(1)==0 ) CALL pexit
!
!     TERMINATE RUN IF ANY OF THE DIAG (17, 25, 28, OR 30) AND DIAG 20
!     ARE REQUESTED SIMULTANEOUSLY
!
         CALL sswtch(20,j)
         IF ( j==0 ) RETURN
         CALL sswtch(28,i)
         CALL sswtch(30,j)
         IF ( diag17+diag25+i+j==0 ) RETURN
         WRITE (optape,99015)
99015    FORMAT (10X,'JOB TERMINATED BY DIAG 20')
         CALL pexit
         EXIT
      ENDIF
   ENDDO
!
!     E R R O R    M E S S A G E S
!
!     UNEXPECTED END OF TAPE ON NEW PROBLEM TAPE
!
 3800 CALL xgpidg(28,0,0,0)
   nogo = 2
   GOTO 5200
!
!     UNEXPECTED END OF TAPE ON OLD PROBLEM TAPE
!
 3900 CALL xgpidg(29,0,0,0)
   nogo = 2
   GOTO 5200
!
!     CANNOT FIND FILE ON DATA POOL TAPE
!
 4000 nam1 = ioshdr(1)
   nam2 = ioshdr(2)
 4100 CALL xgpidg(24,nam1,nam2,0)
   nogo = 2
   GOTO 5200
!
!     UNEXPECTED END OF TAPE ON DATA POOL TAPE
!
 4200 CALL xgpidg(30,0,0,0)
   nogo = 2
   GOTO 5200
!
!     CONTROL FILE INCOMPLETE OR MISSING ON NEW PROBLEM TAPE.
!
 4300 CALL xgpidg(31,nam1,nam2,0)
   nogo = 2
   GOTO 5200
!
!     MED TABLE RECORD MISSING ON SCRATCH FILE
!
 4400 CALL xgpidg(69,nxgpi(1),nxgpi(2),0)
   nogo = 2
   GOTO 5200
!
!     CARD OR FILE NAME TABLE RECORD MISSING ON SCRATCH FILE
!
 4500 CALL xgpidg(70,nxgpi(1),nxgpi(2),jtype)
   nogo = 2
   GOTO 5200
!
!     ILLEGAL BIT NUMBERS IN CARD OR FILE NAME TABLE
!
 4600 CALL xgpidg(73,jtype,0,0)
   nogo = 2
   GOTO 5200
!
!     SCRATCH FILE CONTAINING DMAP DATA COULD NOT BE OPENED
!
 4700 CALL xgpidg(33,nxgpi(1),nxgpi(2),0)
   nogo = 2
   GOTO 5200
!
!     DPL TABLE OVERFLOW
!
 4800 CALL xgpidg(14,ndpl,nblank,0)
   GOTO 5200
!
!     CANNOT FIND FILE ON OLD PROBLEM TAPE
!
 4900 CALL xgpidg(36,icpdpl(i),icpdpl(i+1),0)
   GOTO 5200
 5000 CALL xgpidg(36,ptdic(i),ptdic(i+1),0)
   GOTO 5200
!
!     INCORRECT OLD PROBLEM TAPE MOUNTED
!
 5100 CALL xgpidg(35,0,0,0)
 5200 WRITE (optape,99016)
99016 FORMAT (//5X,'*** JOB TERMINATED DUE TO ABOVE ERRORS')
   CALL mesage(-37,0,nxgpi)
99017 FORMAT (//,' NONE',/)
99018 FORMAT (17X,I3,11X,2A4,14X,I3)
99019 FORMAT (A25,' 4146, LOGIC ERROR IN SUBROUTINE XGPI WHILE ','PROCESSING DATA CHANGES FOR MODIFIED RESTART.')
END SUBROUTINE xgpi
