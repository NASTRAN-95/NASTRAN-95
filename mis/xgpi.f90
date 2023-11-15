
SUBROUTINE xgpi
!
!     THE PURPOSE OF XGPI IS TO INITIALIZE AND CALL THE FOLLOWING
!     SUBROUTINES - XOSGEN AND XFLORD.
!
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Alter(2) , Apprch , Bandit , Bcdcnt , Ceitbl(42) , Cnm(1) , Cnmtp , Core(1) , Cpntry(7) , Cppgct , Diag14 , Diag17 ,     &
         & Diag25 , Diag4 , Dmap(1) , Dmpcnt , Dmpcrd(1) , Dmppnt , Dpl(3) , Eotflg , Fnm(1) , Fnmtp , Iallon , Ibf(8) , Ibuff(20) ,&
         & Ibufr(1) , Ibufsz , Iccnam(1) , Icf(1) , Icfiat , Icfpnt , Icftop , Ichar , Icold , Icpbot , Icpdpl(1) , Icpflg ,        &
         & Icptop , Icrdtp , Icst , Ictlfl(1) , Idmapp , Idmpnt , Idsapp , Iecho , Ieqflg , Iequl , Iestim , Ifiat(3) , Ifirst ,    &
         & Ifist(1) , Iflag , Iflg(6) , Ihapp , Imst , Insert , Intgr , Iplus , Irturn , Isavdw , Isave , Iseqn , Isgnon , Islsh ,  &
         & Isol(1) , Itape , Iunst , Jmp(7) , L15 , L8 , Lbd , Lcc , Lcpdpl , Lctlfl , Length , Libf , Licf , Lmed , Load , Loscar
   LOGICAL Lnogo
   INTEGER Losgn , Lptdic , Lstdpl , Maskhi , Masklo , Masks(1) , Maxdpl , Med(1) , Medmsk(7) , Medpnt , Medtp , Mjcd(1) , Mjmsk(1) &
         & , Modflg , Modidx , Mpc , Mpl(1) , Mplpnt , Nbegin , Nblank , Nbpc , Ncds , Nchkpt , Ncond , Ncpw , Ndmap , Ndpfil ,     &
         & Nend , Nequiv , Nestm1 , Nestm2 , Newcrd , Nexit , Njump , Nmed , Nmskcd , Nmskfl , Nmskrf , Noflgs , Nogo , Nosgn ,     &
         & Noutpt , Npurge , Nrept , Nrlfl , Nsave , Nscr , Nsol , Ntime , Nwpc , Nxequi , Optape , Os(5) , Osbot , Oscar(1) ,      &
         & Ospnt , Osprc , Otapid(6) , Pagect , Ptdbot , Ptdic(1) , Ptdtop , Pvt(200) , Seqno , Seteor , Sol , Spc , Sscell ,       &
         & Start , Subset , Sys12(7) , Sys20 , Sys22(2) , Sys25 , Sys27(42) , Sys4 , Sys7 , Sys70(7) , Sys78(4) , Sys9(2)
   CHARACTER*25 Sfm , Uwm
   INTEGER Tapid(6) , Two(32) , Vps(4)
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /ifpx0 / Lbd , Lcc , Mjmsk
   COMMON /ifpx1 / Ncds , Mjcd
   COMMON /l15l8 / L15 , L8
   COMMON /moddmp/ Iflg
   COMMON /stapid/ Tapid , Otapid
   COMMON /system/ Ibufsz , Optape , Nogo , Sys4 , Mpc , Spc , Sys7 , Load , Sys9 , Pagect , Sys12 , Iecho , Sys20 , Apprch ,       &
                 & Sys22 , Icfiat , Sys25 , Cppgct , Sys27 , Sscell , Sys70 , Bandit , Sys78 , Icpflg
   COMMON /two   / Two
   COMMON /xceitb/ Ceitbl
   COMMON /xdpl  / Dpl
   COMMON /xfiat / Ifiat
   COMMON /xfist / Ifist
   COMMON /xgpi2 / Libf , Mplpnt , Mpl
   COMMON /xgpi3 / Pvt
   COMMON /xgpi4 / Irturn , Insert , Iseqn , Dmpcnt , Idmpnt , Dmppnt , Bcdcnt , Length , Icrdtp , Ichar , Newcrd , Modidx , Licf , &
                 & Isavdw , Dmap
   COMMON /xgpi5 / Isol , Start , Alter , Sol , Subset , Iflag , Iestim , Icftop , Icfpnt , Lctlfl , Ictlfl
   COMMON /xgpi6 / Medtp , Fnmtp , Cnmtp , Medpnt , Lmed , Iplus , Diag14 , Diag17 , Diag4 , Diag25 , Ifirst , Ibuff
   COMMON /xgpi8 / Icptop , Icpbot , Lcpdpl
   COMMON /xgpic / Icold , Islsh , Iequl , Nblank , Nxequi , Nmed , Nsol , Ndmap , Nestm1 , Nestm2 , Nexit , Nbegin , Nend , Njump ,&
                 & Ncond , Nrept , Ntime , Nsave , Noutpt , Nchkpt , Npurge , Nequiv , Ncpw , Nbpc , Nwpc , Maskhi , Masklo ,       &
                 & Isgnon , Nosgn , Iallon , Masks
   COMMON /xgpid / Icst , Iunst , Imst , Ihapp , Idsapp , Idmapp , Isave , Itape , Modflg , Intgr , Losgn , Noflgs , Seteor ,       &
                 & Eotflg , Ieqflg , Cpntry , Jmp
   COMMON /xgpie / Nscr
   COMMON /xmdmsk/ Nmskcd , Nmskfl , Nmskrf , Medmsk
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /xoldpt/ Ptdtop , Ptdbot , Lptdic , Nrlfl , Seqno
   COMMON /xvps  / Vps
   COMMON /zzzzzz/ Core
!
! Local variable declarations
!
   INTEGER andf , complf , korsz , lshift , orf , rshift
   INTEGER bgnmsk , endmsk , filcon , i , i1 , i2 , ib1s , ibegn , ibulk , icase , idelet , idp , idpbuf , idpfct , idpwrd , iend , &
         & iendbt , ii , iii , index , iop , iopbuf , iopwrd , ioshdr(2) , iparbt , iparpt , iparw1 , iparw2 , istrbt , itrl(7) ,   &
         & itype(6) , ixtim , j , j1 , j2 , jcard , jfile , jj , jjj , jtype , k , k1 , kk , kz , l , l1 , l2 , lbd1 , lbdlcc ,     &
         & lim1 , lim2 , ll , lloscr , llx , lx , m , nam1 , nam2 , nampt , ndpl , nfile , nmask , nparam , npt , nptbuf , nptwrd , &
         & npvt , nxaltr , nxcsa , nxgpi(2) , nxptdc(2) , nxvps , ptfct , wrngrl
   EXTERNAL andf , complf , lshift , orf , rshift
!
! End of declarations
!
!
!                  ** CONTROL CARD NAMES **
!                  ** DMAP    CARD NAMES **
!WKBR COMMON /XGPI3 / PVT(6)
!WKBR COMMON /XCEITB/ CEITBL(2)
   EQUIVALENCE (Loscar,Os(1),Core(1)) , (Osprc,Os(2)) , (Osbot,Os(3)) , (Ospnt,Os(4)) , (Oscar(1),Os(5)) ,                          &
    & (Oscar(1),Med(1),Fnm(1),Cnm(1),Icpdpl(1)) , (Oscar(1),Ibufr(1),Dmpcrd(1),Ptdic(1))
   EQUIVALENCE (Dmap(1),Icf(1)) , (Nmed,Iccnam(1)) , (Mpl(1),Ibf(1)) , (Dpl(1),Ndpfil) , (Dpl(2),Maxdpl) , (Dpl(3),Lstdpl) ,        &
    & (Nogo,Lnogo)
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
   Nscr = 315
   CALL sswtch(4,Diag4)
   CALL sswtch(14,Diag14)
   CALL sswtch(17,Diag17)
   CALL sswtch(25,Diag25)
   IF ( Diag14==1 ) Iflg(3) = 1
   IF ( Diag17==1 ) Iflg(4) = 1
   IF ( Diag4==1 ) Iflg(6) = 1
   IF ( Diag4/=0 ) Iflg(5) = orf(Iflg(5),lshift(1,16))
   IF ( Diag25==1 ) Iflg(5) = 1
!
!     SET DMAP COMPILER DEFAULT OPTION TO LIST FOR
!     APPROACH DMAP RUNS, RESTART RUNS AND SUBSTRUCTURE RUNS
!     RESET TO NO LIST IF ECHO=NONO (IECHO=-2)
!
   IF ( Iecho/=-2 .AND. (Apprch<2 .OR. Sscell/=0) ) Iflg(3) = 1
   IF ( Diag14==0 .AND. Iflg(3)==1 ) Diag14 = 2
   IF ( Iecho==-2 ) Diag14 = 0
   CALL xgpimw(1,0,0,0)
!
   CALL xgpibs
   IF ( Nogo>1 ) GOTO 5300
!
!     SET UP GINO BUFFER AREAS FOR OLD PROBLEM TAPE,NEW PROBLEM TAPE
!     AND DATA POOL TAPE.
!
   Loscar = korsz(Ibufr)
   nptbuf = Loscar - Ibufsz
!
!     OLD PROBLEM TAPE AND NEW PROBLEM TAPE SHARE BUFFER
!
   iopbuf = nptbuf
   idpbuf = nptbuf - Ibufsz
   Loscar = idpbuf - 1
!
!     ALLOW MINIMAL SIZE FOR MED ARRAY RESIDING IN OPEN CORE.
!     WE WILL EXPAND MED IF NECESSARY.
!
   Medtp = Loscar
   Lmed = 1
   IF ( Loscar<1 ) THEN
!
!     NOT ENOUGH CORE FOR GPI TABLES
!
      CALL xgpidg(38,-Loscar,0,0)
!
!     TERMINATE JOB IF NOGO = 1
!
      Nogo = 2
      GOTO 5300
   ELSE
!
!     OPEN NEW PROBLEM TAPE AS INPUT FILE
!
      CALL open(*3900,npt,Ibufr(nptbuf),0)
!
!     NUMBER OF FILE ON NPT + 1
!
      Nrlfl = lshift(Tapid(6),16) + 5
!
!     FILE POSITION OF IOP AT ENTRY TO XGPI
!
      ptfct = lshift(Otapid(6),16) + 4
!
!     FIND XCSA FILE ON NEW PROBLEM TAPE
!
      nam1 = nxcsa
      nam2 = Nblank
   ENDIF
   DO
!
!     SKIP HEADER FILE
!
      CALL skpfil(npt,1)
      CALL read(*4400,*4400,npt,Icf,1,1,nptwrd)
!
!     CHECK FOR ALTER FILE
!     SET DIAG14 TO 10 IF ALTER CARDS ARE PRESENT. DIAG14 WOULD BE
!     CHANGED TO 11 IF DMAP CONTAINS POTENTIAL FATAL ERROR. IN SUCH
!     CASE, DMAP LISTING WILL BE PRINTED.
!
      IF ( Icf(1)==nxaltr ) THEN
         Nrlfl = Nrlfl + 1
         IF ( Diag14==0 ) Diag14 = 10
      ENDIF
!
!     CHECK FOR CHECKPOINT DICTIONARY FILE
!
      IF ( Icf(1)==nxptdc(1) ) Nrlfl = Nrlfl + 1
!
!     CHECK FOR CONTROL FILE
!
      IF ( Icf(1)==nxcsa ) THEN
!
!     PROBLEM TAPE IS POSITIONED AT EXECUTIVE CONTROL FILE.
!
         Icfpnt = Icftop
!
!     READ THE SIX-WORD DATA RECORD
!
         CALL read(*4400,*100,npt,Isol,7,1,nptwrd)
         GOTO 4400
      ENDIF
   ENDDO
 100  CALL close(npt,1)
   IF ( iabs(Apprch)==1 ) GOTO 700
!
!     FILL MED ARRAY
!
   Medtp = 1
!
!     SET VALUE FOR NUMBER OF WORDS PER MED ENTRY
!
   Med(Medtp+1) = 1
   IF ( Start/=Icst ) Med(Medtp+1) = Nmskcd + Nmskfl + Nmskrf
!
   CALL gopen(Nscr,Ibufr(nptbuf),0)
   lloscr = Loscar - 2
!
!     READ THE MED TABLE
!
   CALL read(*4500,*200,Nscr,Med(Medtp+2),lloscr,1,Lmed)
!
!     MED TABLE OVERFLOW
!
   CALL xgpidg(14,Nmed,Nblank,0)
   Nogo = 2
   GOTO 5300
!
!     SET VALUE FOR NUMBER OF DMAP INSTRUCTIONS
!
 200  Med(Medtp) = Lmed/Med(Medtp+1)
!
!     CHECK FOR ILLEGAL NUMBER OF WORDS IN MED TABLE RECORD
!
   IF ( Start/=Icst .AND. Lmed/=Med(Medtp)*Med(Medtp+1) ) THEN
!
!     ILLEGAL NUMBER OF WORDS IN MED TABLE RECORD
!
      CALL xgpidg(71,Lmed,0,0)
      Nogo = 2
      GOTO 5300
   ELSE
!
!     SET THE POINTERS TO THE FILE NAME AND CARD NAME TABLES
!
      Fnmtp = Medtp + Lmed + 2
      Cnmtp = Fnmtp
      IF ( Start==Icst ) GOTO 600
      lloscr = lloscr - Lmed
!
!     READ THE FILE NAME TABLE
!
      CALL skprec(Nscr,1)
      jtype = jfile
      CALL read(*4600,*300,Nscr,Med(Fnmtp+1),lloscr,1,Lmed)
      CALL xgpidg(14,Nmed,Nblank,0)
      Nogo = 2
      GOTO 5300
   ENDIF
!
!     SET THE VALUE FOR THE NUMBER OF ENTRIES IN THE FILE NAME TABLE
!
 300  Med(Fnmtp) = Lmed/3
!
!     CHECK FOR ILLEGAL NUMBER OF WORDS IN FILE NAME TABLE RECORD
!
   IF ( Lmed/=3*Med(Fnmtp) ) THEN
!
!     ILLEGAL NUMBER OF WORDS IN CARD OR FILE NAME TABLE RECORD
!
      CALL xgpidg(72,Lmed,jtype,0)
      Nogo = 2
   ELSE
!
!     CHECK FOR ILLEGAL BIT NUMBERS IN FILE NAME TABLE
!
      istrbt = 31*Nmskcd + 1
      iendbt = 31*(Nmskcd+Nmskfl)
      DO j = 3 , Lmed , 3
         IF ( Med(Fnmtp+j)<istrbt .OR. Med(Fnmtp+j)>iendbt ) GOTO 4700
      ENDDO
!
!     RESET THE POINTER FOR THE CARD NAME TABLE
!
      Cnmtp = Fnmtp + 3*Fnm(Fnmtp) + 1
      lloscr = lloscr - Lmed
!
!     READ THE CARD NAME TABLE
!
      CALL skprec(Nscr,-2)
      jtype = jcard
      CALL read(*4600,*400,Nscr,Med(Cnmtp+1),lloscr,1,Lmed)
      CALL xgpidg(14,Nmed,Nblank,0)
      Nogo = 2
   ENDIF
   GOTO 5300
!
!     SET THE VALUE FOR THE NUMBER OF ENTRIES IN THE CARD NAME TABLE
!
 400  Med(Cnmtp) = Lmed/3
!
!     CHECK FOR ILLEGAL NUMBER OF WORDS IN CARD NAME TABLE RECORD
!
   IF ( Lmed/=3*Med(Cnmtp) ) THEN
      CALL xgpidg(72,Lmed,jtype,0)
      Nogo = 2
      GOTO 5300
   ELSE
!
!     CHECK FOR ILLEGAL BIT NUMBERS IN CARD NAME TABLE
!
      istrbt = 1
      iendbt = 31*Nmskcd
      DO j = 3 , Lmed , 3
         IF ( Med(Cnmtp+j)<istrbt .OR. Med(Cnmtp+j)>iendbt ) GOTO 4700
      ENDDO
!
!     RESTART - CHECK MEDMSK TABLE
!     IF MEDMSK WORD(S), CORRESPONDING TO RIGID FORMAT SWITCH, IS(ARE)
!     NON-ZERO, SOLUTION HAS BEEN CHANGED.
!     RESET ENTRY SEQUENCE NO. TO INFINITE IF SOLUTION IS CHANGED.
!
      nmask = Med(Medtp+1)
      ibegn = Nmskcd + Nmskfl + 1
      DO i = ibegn , nmask
         IF ( Medmsk(i)/=0 ) THEN
            Seqno = Masklo
            Start = Imst
         ENDIF
      ENDDO
!
!     SEE IF ANY BULK DATA OR CASE CONTROL CARDS HAVE BEEN MODIFIED.
!
      bgnmsk = 1
      endmsk = Lbd + Lcc
!
!     TURN OFF BIT IN MJMSK ARRAY IF THE CORRESPONDING CARD NAME
!     IS NOT IN THE CARD NAME RESTART TABLE
!
      i1 = Cnmtp + 1
      i2 = i1 + 3*Cnm(Cnmtp) - 3
      DO lx = bgnmsk , endmsk
         IF ( Mjmsk(lx)/=0 ) THEN
            l = lx - bgnmsk + 1
            DO l1 = 2 , 32
               IF ( andf(Mjmsk(lx),Two(l1))/=0 ) THEN
!
!     IGNORE BIT IF IT CORRESPONDS TO QOUT$ OR BOUT$
!
                  IF ( .NOT.(lx==Lbd+2 .AND. (l1==3 .OR. l1==4)) ) THEN
                     i = 62*(l-1) + 2*(l1-2) + 1
                     DO ii = i1 , i2 , 3
                        IF ( Mjcd(i)==Cnm(ii) .AND. Mjcd(i+1)==Cnm(ii+1) ) GOTO 410
                     ENDDO
                     ii = complf(Two(l1))
                     Mjmsk(lx) = andf(Mjmsk(lx),ii)
                  ENDIF
               ENDIF
 410        ENDDO
         ENDIF
      ENDDO
      IF ( Start/=Imst ) THEN
!
!     DETERMINE TYPE OF RESTART
!
         index = 0
         iend = Lbd
         DO l = bgnmsk , iend
            IF ( Mjmsk(l)/=0 ) THEN
               index = 1
               EXIT
            ENDIF
         ENDDO
      ENDIF
      l = Lbd + 1
      IF ( Start/=Imst ) THEN
         IF ( index/=1 ) THEN
            IF ( Mjmsk(l)/=0 ) THEN
!
!     CHECK FOR NOLOOP$ AND LOOP$
!                                          2**21
               IF ( Mjmsk(l)/=1 .AND. Mjmsk(l)/=Two(11) ) GOTO 420
            ENDIF
!
!     CHECK FOR GUST$
!                         2**30
            IF ( Mjmsk(l+1)<Two(2) ) GOTO 450
         ENDIF
 420     Start = Imst
      ENDIF
!
!     TURN ON POUT$ IF QOUT$ IS ON
!                         2**29                                  2**14
      IF ( andf(Mjmsk(l+1),Two(3))/=0 ) Mjmsk(l) = orf(Mjmsk(l),Two(18))
!
!     TURN ON AOUT$ IF BOUT$ IS ON
!                         2**28                                  2**22
      IF ( andf(Mjmsk(l+1),Two(4))/=0 ) Mjmsk(l) = orf(Mjmsk(l),Two(10))
!
!     TURN OFF BOUT$ AND QOUT$
!                 2**28    2**29
      ii = complf(Two(4)+Two(3))
      Mjmsk(l+1) = andf(Mjmsk(l+1),ii)
!
!     TURN OFF NOLOOP$ FOR UNMODIFIED RESTARTS
!
 450  IF ( Start==Iunst .AND. Mjmsk(Lbd+1)==1 ) Mjmsk(Lbd+1) = 0
   ENDIF
 500  CALL page1
   IF ( Start/=Iunst ) THEN
      CALL page2(-2)
      IF ( Seqno/=Masklo ) WRITE (Optape,99001) Uim
99001 FORMAT (A29,' 4144, THIS IS A MODIFIED RESTART.')
      IF ( Seqno==Masklo ) WRITE (Optape,99002) Uim
99002 FORMAT (A29,' 4145, THIS IS A MODIFIED RESTART INVOLVING RIGID ','FORMAT SWITCH.')
      ibulk = 0
      icase = 0
      DO l = 1 , Lbd
         IF ( Mjmsk(l)/=0 ) THEN
            ibulk = 1
            EXIT
         ENDIF
      ENDDO
      lbd1 = Lbd + 1
      lbdlcc = Lbd + Lcc
      DO l = lbd1 , lbdlcc
         IF ( Mjmsk(l)/=0 ) THEN
            icase = 1
            EXIT
         ENDIF
      ENDDO
      IF ( ibulk/=0 .OR. icase/=0 ) THEN
         CALL page2(-4)
         WRITE (Optape,99003) Uim
99003    FORMAT (A29,'. CASE CONTROL AND BULK DATA DECK CHANGES AFFECTING',' THIS RESTART ARE INDICATED BELOW.',/)
         DO llx = 1 , 2
            IF ( llx==1 ) THEN
               CALL page2(-3)
               WRITE (Optape,99004) Uim
99004          FORMAT (A29,'. EFFECTIVE CASE CONTROL DECK CHANGES',/1X,35(1H-))
               IF ( icase/=0 ) THEN
                  CALL page2(-3)
                  IF ( Apprch/=-1 ) WRITE (Optape,99005)
99005             FORMAT (//,' MASK WORD - BIT POSITION ---- FLAG NAME ---- PACKED',' BIT POSITION',/)
                  IF ( Apprch==-1 ) WRITE (Optape,99006)
99006             FORMAT (//,' MASK WORD - BIT POSITION ---- FLAG NAME',/)
                  lim1 = lbd1
                  lim2 = lbdlcc
               ELSE
                  CALL page2(-3)
                  WRITE (Optape,99017)
                  CYCLE
               ENDIF
            ELSE
               CALL page2(-3)
               WRITE (Optape,99007) Uim
99007          FORMAT (A29,'. EFFECTIVE BULK DATA DECK CHANGES',/1X,32(1H-))
               IF ( ibulk/=0 ) THEN
                  CALL page2(-3)
                  IF ( Apprch/=-1 ) WRITE (Optape,99008)
99008             FORMAT (//,' MASK WORD - BIT POSITION - CARD/PARAM NAME - PACKED',' BIT POSITION',/)
                  IF ( Apprch==-1 ) WRITE (Optape,99009)
99009             FORMAT (//,' MASK WORD - BIT POSITION - CARD/PARAM NAME',/)
                  lim1 = 1
                  lim2 = Lbd
               ELSE
                  CALL page2(-3)
                  WRITE (Optape,99017)
                  CYCLE
               ENDIF
            ENDIF
            DO l = lim1 , lim2
               IF ( Mjmsk(l)/=0 ) THEN
                  CALL page2(-1)
                  WRITE (Optape,99010) l
99010             FORMAT (1X,I5)
                  DO k = 2 , 32
                     IF ( andf(Mjmsk(l),Two(k))/=0 ) THEN
!
!     GET CORRESPONDING CARD NAME FROM MAIN CARD TABLE
!
                        i = 62*(l-1) + 2*(k-2) + 1
                        kz = k - 1
                        CALL page2(-1)
                        IF ( Apprch/=-1 ) THEN
!
!     SEARCH RIGID FORMAT CARD NAME RESTART TABLE FOR A MATCH
!
                           DO ii = i1 , i2 , 3
                              IF ( Mjcd(i)==Cnm(ii) .AND. Mjcd(i+1)==Cnm(ii+1) ) THEN
!
!     CARD NAME FOUND - SET BIT IN MEDMSK
!
                                 WRITE (Optape,99018) kz , Mjcd(i) , Mjcd(i+1) , Cnm(ii+2)
                                 l1 = (Cnm(ii+2)-1)/31
                                 ll = l1 + 1
                                 kk = Cnm(ii+2) - 31*l1 + 1
                                 Medmsk(ll) = orf(Medmsk(ll),Two(kk))
                                 GOTO 502
                              ENDIF
                           ENDDO
                           WRITE (Optape,99019) Sfm
                           WRITE (Optape,99011) Mjcd(i) , Mjcd(i+1) , (Cnm(ll),Cnm(ll+1),ll=i1,i2,3)
99011                      FORMAT (/10X,2A4,//,10(4X,2A4))
                           CALL mesage(-61,0,0)
                        ELSE
                           WRITE (Optape,99018) kz , Mjcd(i) , Mjcd(i+1)
                        ENDIF
                     ENDIF
 502              ENDDO
               ENDIF
            ENDDO
         ENDDO
         IF ( Apprch==-1 ) GOTO 900
      ELSE
         IF ( Seqno/=Masklo ) THEN
            WRITE (Optape,99019)
            CALL mesage(-61,0,0)
         ENDIF
         WRITE (Optape,99012) Uim
99012    FORMAT (A29,'. THERE ARE NO CASE CONTROL OR BULK DATA DECK ','CHANGES AFFECTING THIS RESTART.')
      ENDIF
   ELSE
      WRITE (Optape,99013) Uim
99013 FORMAT (A29,' 4143, THIS IS AN UNMODIFIED RESTART.')
      Bandit = -1
      IF ( Apprch==-1 ) GOTO 900
   ENDIF
!
!     MOVE MED AND FILE NAME TABLES TO BOTTOM OF OPEN CORE.
!
 600  CALL close(Nscr,1)
   Lmed = Cnmtp - Medtp
   DO i = 1 , Lmed
      ll = Medtp + Lmed - i
      m = Loscar - i + 1
      Med(m) = Med(ll)
   ENDDO
   Medtp = Loscar - Lmed + 1
   Fnmtp = Medtp + Med(Medtp)*Med(Medtp+1) + 2
   Loscar = Medtp - 1
!
!     DETERMINE TYPE OF RESTART IF IT IS A RESTART OF A DMAP RUN
!
 700  IF ( Apprch/=-1 ) GOTO 900
   IF ( Mjmsk(Lbd+1)/=0 ) THEN
!
!     CHECK FOR NOLOOP$ AND LOOP$
!                                                  2**21
      IF ( Mjmsk(Lbd+1)/=1 .AND. Mjmsk(Lbd+1)/=Two(11) ) GOTO 800
      Mjmsk(Lbd+1) = 0
   ENDIF
!
!     CHECK FOR GUST$
!                           2**30
   IF ( Mjmsk(Lbd+2)<Two(2) ) THEN
      DO l = 1 , Lbd
         IF ( Mjmsk(l)/=0 ) GOTO 800
      ENDDO
      GOTO 500
   ENDIF
 800  Start = Imst
   Seqno = lshift(1,16)
   GOTO 500
!
!     CONTROL FILE LOADED, LOAD PVT TABLE
!     BUMP NUMBER OF FILES IF OLD PROBLEM TAPE HAD ALTERS
!
 900  ptfct = ptfct + Alter(2)
   itrl(1) = nparam
   CALL rdtrl(itrl(1))
   IF ( itrl(2)<=0 ) GOTO 1400
   CALL open(*3900,nparam,Ibufr(nptbuf),0)
   CALL read(*1400,*1000,nparam,Pvt(6),2,1,nptwrd)
 1000 IF ( Pvt(6)/=npvt ) GOTO 4400
   i = 3
!
!      LOAD PVT VALUES INTO PVT TABLES
!
 1100 CALL read(*1300,*1200,nparam,Pvt(i),Pvt(1)-i+1,0,nptwrd)
!
!     PVT TABLE OVERFLOW
!
   CALL xgpidg(14,npvt,Nblank,0)
   GOTO 5300
 1200 i = i + nptwrd
   GOTO 1100
 1300 Pvt(2) = i - 1
   CALL close(nparam,1)
!
!     ELIMINATE TRAILER SO FILE WILL BE DELETED
!
   DO i = 2 , 7
      itrl(i) = 0
   ENDDO
   CALL wrttrl(itrl(1))
 1400 IF ( Start==Icst ) GOTO 2100
   IF ( Apprch==-1 .AND. Start==Imst ) GOTO 2100
!
!     INITIALIZE VPS TABLE FOR RESTART
!     GET FIRST ENTRY IN CHECKPOINT DICTIONARY
!
   Ptdtop = 1
   ASSIGN 1500 TO Irturn
   GOTO 2500
 1500 i = Ptdtop
   IF ( Ptdic(Ptdtop)/=nxvps ) GOTO 2100
!
!     FIRST ENTRY IN CHECKPOINT DICTIONARY IS XVPS - GET FILE OFF OF OLD
!     PROBLEM TAPE, OPTP
!
   CALL open(*4000,iop,Ibufr(iopbuf),2)
!
!     CHECK TO SEE IF OLD RESTART TAPE HAS PVT  J = 0 WITHOUT PVT
!
   j = andf(Maskhi,Ptdic(Ptdtop+2)) - (andf(Maskhi,ptfct)+1)
   ptfct = ptfct + j
   CALL skpfil(iop,j)
   CALL read(*5100,*1600,iop,Vps(3),2,1,iopwrd)
 1600 IF ( Vps(3)/=nxvps .OR. Vps(4)/=Nblank ) GOTO 5100
   j = Vps(1)
   CALL read(*5100,*1700,iop,Vps,j,1,iopwrd)
 1700 CALL skpfil(iop,1)
   CALL close(iop,2)
   ptfct = ptfct + 1
   Vps(1) = j
!
!     FOR RESTART COMPARE PVT VALUES WITH VPS VALUES. IF NOT EQUAL SET
!     MODFLG INVPS ENTRY.
!
   IF ( Pvt(2)<=2 ) GOTO 1900
   i = 3
   j = 3
 1800 DO WHILE ( Pvt(2)>=j )
      IF ( Pvt(j)==Vps(i) .AND. Pvt(j+1)==Vps(i+1) ) THEN
!
!     FOUND VARIABLE IN PVT TABLE
!
         l = andf(Vps(i+2),Maskhi)
         Pvt(j+2) = orf(Pvt(j+2),Isgnon)
         DO ll = 1 , l
            ii = i + ll + 2
            jj = j + ll + 2
            Vps(i+2) = orf(Vps(i+2),Modflg)
            Vps(ii) = Pvt(jj)
         ENDDO
         EXIT
      ELSE
         jj = andf(Pvt(j+2),Nosgn)
         j = j + itype(jj) + 3
      ENDIF
   ENDDO
   i = i + andf(Vps(i+2),Maskhi) + 3
   IF ( i<Vps(2) ) THEN
      j = 3
      GOTO 1800
   ENDIF
 1900 i = Lbd + Lcc + 1
   iparpt = Mjmsk(i)
   iparw1 = (iparpt-1)/31 + 1
   iparw2 = Lbd
   iparbt = mod(iparpt-1,31) + 2
   idelet = 0
   DO j1 = iparw1 , iparw2
      IF ( Mjmsk(j1)/=0 ) GOTO 2000
   ENDDO
   idelet = 1
   GOTO 2100
 2000 DO j1 = iparw1 , iparw2
      IF ( Mjmsk(j1)/=0 ) THEN
         DO i1 = iparbt , 32
            IF ( andf(Mjmsk(j1),Two(i1))/=0 ) THEN
               nampt = 2*(31*(j1-1)+i1-1) - 1
               i2 = 3
               DO WHILE ( Mjcd(nampt)/=Vps(i2) .OR. Mjcd(nampt+1)/=Vps(i2+1) )
                  i2 = i2 + andf(Vps(i2+2),Maskhi) + 3
                  IF ( i2>=Vps(2) ) GOTO 2020
               ENDDO
               IF ( andf(Vps(i2+2),Two(2))==0 ) THEN
                  Vps(i2) = Nblank
                  Vps(i2+1) = Nblank
               ENDIF
            ENDIF
 2020    ENDDO
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
 2100 IF ( Alter(1)==0 ) GOTO 2400
   nam1 = nxaltr
   nam2 = Nblank
   CALL open(*3900,npt,Ibufr(nptbuf),0)
 2200 CALL skpfil(npt,1)
   CALL read(*4400,*2300,npt,Icf,2,1,nptwrd)
 2300 IF ( Icf(1)/=nxaltr ) GOTO 2200
!
!     ALTER FILE FOUND - INITIALIZE ALTER CELLS
!
   CALL read(*4400,*4400,npt,Alter,2,1,nptwrd)
 2400 CALL open(*4800,Nscr,Ibufr(idpbuf),0)
   CALL xgpimw(1,1,0,0)
   CALL xosgen
   IF ( Start/=Icst ) THEN
      DO i = 1 , nmask
         Medmsk(i) = 0
      ENDDO
   ENDIF
   IF ( Alter(1)/=0 ) CALL close(npt,2)
   IF ( Pvt(2)>2 ) THEN
      j = 5
      DO WHILE ( Pvt(2)>=j )
         IF ( Pvt(j)>=0 ) CALL xgpidg(-54,0,Pvt(j-2),Pvt(j-1))
         jj = andf(Pvt(j),Nosgn)
         j = j + itype(jj) + 3
      ENDDO
   ENDIF
   IF ( Nogo==2 ) GOTO 5300
   CALL close(Nscr,1)
   IF ( Start/=Icst ) CALL xgpimw(2,0,0,0)
   CALL xgpimw(1,0,0,0)
!
!     ALLOW MINIMAL SIZE FOR PTDIC ARRAY IN OPEN CORE.
!     WE WILL EXPAND IF THIS IS RESTART.
!
   Ptdtop = Oscar(Osbot) + Osbot
   Ptdbot = Ptdtop
   Lptdic = 3
   ASSIGN 2900 TO Irturn
!
 2500 IF ( Start==Icst ) GOTO 2900
!
!     RESTART - LOAD OLD PROBLEM TAPE DICTIONARY INTO OPEN CORE.
!
   CALL open(*3900,npt,Ibufr(nptbuf),0)
!
!     FIND XPTDIC ON NEW PROBLEM TAPE
!
   nam1 = nxptdc(1)
   nam2 = nxptdc(2)
 2600 CALL skpfil(npt,1)
   CALL read(*4400,*2700,npt,Ptdic(Ptdtop),2,1,nptwrd)
 2700 IF ( Ptdic(Ptdtop)==nxcsa ) GOTO 4400
   IF ( Ptdic(Ptdtop)/=nxptdc(1) ) GOTO 2600
!
!     FOUND XPTDIC
!
   Lptdic = Loscar - Ptdtop
   CALL read(*4400,*2800,npt,Ptdic(Ptdtop),Lptdic,1,nptwrd)
!
!     XPTDIC OVERFLOWED
!
   CALL xgpidg(14,nxptdc(1),nxptdc(2),0)
   Nogo = 2
   GOTO 5300
 2800 Ptdbot = Ptdtop + nptwrd - 3
   CALL close(npt,1)
   GOTO Irturn
!
!     IF BOTH DIAGS 14 AND 20 ARE ON, TERMINATE JOB
!
 2900 IF ( Diag14==1 ) THEN
      CALL sswtch(20,i)
      IF ( i/=0 ) THEN
         WRITE (Optape,99014)
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
   IF ( Nogo/=0 .AND. Start/=Icst .AND. Ptdtop==Ptdbot ) GOTO 5300
   CALL xflord
   IF ( Diag14==11 ) THEN
!
!     USER DMAP ALTER CONTAINS ERROR, DIAG 14 FLAG IS NOT REQUESTED, AND
!     ECHO IS NOT 'NONO', PRINT RIGID FORMAT BEFORE QUITTING
!
      IF ( Iecho/=-2 ) CALL xgpimw(13,0,0,Core)
      GOTO 5300
   ELSE
      IF ( Nogo/=0 .OR. Lnogo ) GOTO 5300
      IF ( Diag4/=0 ) CALL dumper
!
!     PURGE ALL FILES IN FIAT TABLE THAT HAVE NOT BEEN GENERATED BY
!     IFP SUBROUTINE
!
      i = Ifiat(1)*Icfiat - 2
      DO k = 4 , i , Icfiat
         IF ( Ifiat(k+1)/=0 ) THEN
            IF ( Ifiat(k+3)/=0 .OR. Ifiat(k+4)/=0 .OR. Ifiat(k+5)/=0 ) CYCLE
            IF ( Icfiat==11 .AND. (Ifiat(k+8)/=0 .OR. Ifiat(k+9)/=0 .OR. Ifiat(k+10)/=0) ) CYCLE
!
!     FILE NOT GENERATED - PURGE IT.
!
            k1 = Ifiat(3)*Icfiat + 4
            Ifiat(3) = Ifiat(3) + 1
            Ifiat(k1) = orf(andf(Ifiat(k),Masklo),Maskhi)
            Ifiat(k) = andf(Ifiat(k),orf(Maskhi,Losgn))
            Ifiat(k1+1) = Ifiat(k+1)
            Ifiat(k1+2) = Ifiat(k+2)
         ENDIF
!
!     MAKE SURE NO RESIDUE LEFT IN FIAT TABLE
!
         j1 = k + 1
         j2 = k + Icfiat - 1
         DO j = j1 , j2
            Ifiat(j) = 0
         ENDDO
!
      ENDDO
!
!     WRITE OSCAR ON DATA POOL FILE.
!
!     PUT OSCAR NAME IN DPL AND ASSIGN FILE NO.
!
      Lstdpl = Lstdpl + 1
      i = Lstdpl*3 + 1
      Dpl(i) = ioshdr(1)
      Dpl(i+1) = ioshdr(2)
      Dpl(i+2) = Ndpfil
      Ndpfil = 1 + Ndpfil
!
!     WRITE OSCAR HEADER RECORD
!     POSITION FILE
!
      IF ( Ndpfil/=2 ) THEN
         CALL open(*4300,idp,Ibufr(idpbuf),0)
         CALL skpfil(idp,Ndpfil-2)
         CALL close(idp,2)
      ENDIF
      idpfct = Ndpfil - 1
      CALL open(*4300,idp,Ibufr(idpbuf),3)
      CALL write(idp,ioshdr,2,1)
!
!     IF CHECKPOINT AND RESTART FLAGS ARE ON INSERT CHECKPOINT ENTRY IN
!     OSCAR TO SAVE FILES LISTED IN ICPDPL TABLE
!
      IF ( Start/=Icst ) THEN
         IF ( Icpbot>=Icptop .AND. Icpflg/=0 ) THEN
!
!     CHECKPOINT ALL FILES LISTED IN ICPDPL
!
            Cpntry(7) = (Icpbot-Icptop+3)/3
            Cpntry(1) = 7 + Cpntry(7)*2
!
!     FOR UNMODIFIED RESTART - DMAP SEQUENCE NO. OF THIS INITIAL
!     CHECKPOINT MUST = REENTRY POINT - 1
!
            IF ( Start==Iunst ) Cpntry(6) = orf(Isgnon,rshift(andf(Seqno,Masklo),16)-1)
            CALL write(idp,Cpntry,7,0)
            DO i = Icptop , Icpbot , 3
               CALL write(idp,Icpdpl(i),2,0)
            ENDDO
            CALL write(idp,0,0,1)
         ELSE
            Cpntry(6) = 1
            CALL write(idp,Cpntry,6,1)
         ENDIF
!
!     FOR RESTART - INSERT JUMP IN OSCAR TO POSITION OSCAR AT CORRECT
!     REENTRY POINT
!     FOR MODIFIED RESTART - START AT FIRST EXECUTABLE MODULE
!
         IF ( Start==Imst ) Jmp(6) = 1
!
!     SEE IF RE-ENTRY POINT IS WITHIN BOUNDS UNLESS SOLUTION CHANGED.
!
         IF ( andf(Seqno,Masklo)/=Masklo ) THEN
            i = andf(Seqno,Maskhi)
            IF ( i>Oscar(Osbot+1) .OR. i==0 ) THEN
!
!     REENTRY POINT NOT WITHIN BOUNDS
!
               CALL xgpidg(46,0,0,0)
               GOTO 5300
            ELSE
               Jmp(7) = lshift(i,16)
            ENDIF
         ENDIF
         CALL write(idp,Jmp,7,1)
      ENDIF
      Ospnt = 1
   ENDIF
   DO
!
!     WRITE NEXT OSCAR ENTRY ON DATA POOL TAPE
!
      CALL write(idp,Oscar(Ospnt),Oscar(Ospnt),1)
      IF ( Oscar(Ospnt+3)/=ixtim ) THEN
         i = andf(Oscar(Ospnt+2),Maskhi)
         IF ( i<=2 .AND. Oscar(Ospnt+5)<0 ) THEN
!
!     MAKE SURE SYSTEM HAS ENOUGH FILES AVAILABLE TO HANDLE MODULE
!     REQUIREMENTS.
!     COUNT NUMBER OF I/P AND O/P FILES NEEDED
!
            j1 = 2
            IF ( i==2 ) j1 = 1
            k = 0
            l = Ospnt + 6
            DO j = 1 , j1
               l2 = Oscar(l)*3 - 2 + l
               l1 = l + 1
               IF ( Oscar(l1-1)/=0 ) THEN
                  DO l = l1 , l2 , 3
                     IF ( Oscar(l)/=0 ) k = k + 1
                  ENDDO
               ENDIF
               l = l2 + 3
            ENDDO
!
!     ADD ON NUMBER OF SCRATCH FILES NEEDED
!
            k = k + Oscar(l)
!
!     NOT ENOUGH FILES AVAILABLE FOR MODULE REQUIREMENTS.
!
!
!     OSCAR ENTRY IS XTIME, COMPUTE ROUGH TIME ESTIMATES FOR MODULES IN
!     TIME SEGMENT, AND
!     WRITE XTIME HEADER AND TIME ESTIMATES ONTO DATA POOL
!     (THIS SECTION TEMPORARILY OMITTED)
!
            IF ( Ifiat(1)<k ) CALL xgpidg(-37,Ospnt,k,Ifiat(1))
         ENDIF
      ENDIF
!
!     INCREMENT OSPNT AND CHECK FOR END OF OSCAR
!
      Ospnt = Ospnt + Oscar(Ospnt)
      IF ( Ospnt>Osbot ) THEN
         CALL eof(idp)
         IF ( Start==Icst ) GOTO 3800
!
!
!     *** RESTART ***
!
         IF ( Icpbot<Icptop ) GOTO 3800
!
!     LIST ICPDPL CONTENTS
!
         CALL xgpimw(8,Icptop,Icpbot,Icpdpl)
!
!     ELIMINATE PURGED FILES FROM ICPDPL
!
         i1 = Icptop
         DO i = i1 , Icpbot , 3
            IF ( andf(Icpdpl(i+2),Maskhi)/=0 ) EXIT
            Icptop = Icptop + 3
         ENDDO
         IF ( Icpbot<Icptop ) GOTO 3800
         CALL close(idp,2)
         ib1s = idpbuf
         idpbuf = Icpbot + 3
         iopbuf = idpbuf + Ibufsz
         CALL gopen(idp,Ibufr(idpbuf),3)
!
!     TRANSFER CHECKPOINT INFO FROM OLD PROBLEM TAPE TO DATA POOL TAPE
!
         k = Lstdpl*3 + 4
         CALL open(*4000,iop,Ibufr(iopbuf),2)
         DO i = Icptop , Icpbot , 3
            Dpl(k+2) = 0
            IF ( andf(Icpdpl(i+2),Noflgs)>ptfct ) THEN
!
!     MAKE SURE CORRECT REEL IS MOUNTED FOR OLD PROBLEM TAPE
!
               IF ( andf(andf(Noflgs,Masklo),Icpdpl(i+2))==andf(Masklo,ptfct) ) GOTO 2940
!
!     ** NEW REEL NEEDED **
!     MOUNT REEL SPECIFIED BY ICPDPL ENTRY
!
               Otapid(6) = rshift(andf(Noflgs,Icpdpl(i+2)),16)
               wrngrl = 0
            ELSE
!
!     FILE IS EQUIVALENCED TO PREVIOUS ENTRY IN DPL
!
               Ndpfil = Ndpfil - 1
               Dpl(k+2) = Dpl(k-1)
               GOTO 2980
            ENDIF
!
!     SEND OPERATOR MESSAGE
!
 2910       CALL xeot(iop,rshift(ptfct,16),Otapid(6),Ibufr(iopbuf))
            CALL open(*4000,iop,Ibufr(iopbuf),0)
            CALL read(*5000,*2920,iop,Ibf,Libf,0,iopwrd)
!
!     SEE THAT CORRECT REEL HAS BEEN MOUNTED.
!
 2920       DO ii = 1 , 6
               IF ( Otapid(ii)/=Ibf(ii) ) GOTO 2930
            ENDDO
!
!     CORRECT REEL MOUNTED - CARRY ON
!
            CALL skpfil(iop,1)
            ptfct = lshift(Otapid(6),16) + 1
            IF ( filcon==0 ) GOTO 2940
            GOTO 2970
 2930       wrngrl = wrngrl + 1
            IF ( wrngrl>=2 ) GOTO 5200
            GOTO 2910
!
!     WRITE FILE ON DATA POOL
!
 2940       CALL skpfil(iop,andf(Maskhi,Icpdpl(i+2))-(andf(Maskhi,ptfct)+1))
!
!     CHECK FOR CORRECT FILE
!
!     5 OR 8 WORDS (DEPEND ON ICFIAT VALUE OF 8 OR 11) WRITTEN TO IOP
!     BY XCHK OF PREVIOUS CHECKPOINT RUN.
!     IF ICFIAT=11, READ 5 WORDS HERE FIRST, AND CHECK IF THERE ARE 3
!     MORE WORDS BEHIND.  I.E. OPTP MAY BE WRITTEN WITH A 5-WORD RECORD
!     IF ICFIAT= 8, READ 5 WORDS
!
            IF ( Icfiat==11 ) THEN
               Ibf(8) = -999
               CALL read(*5000,*5000,iop,Ibf(1),5,0,iopwrd)
               CALL read(*5000,*2950,iop,Ibf(6),3,1,iopwrd)
            ELSE
               CALL read(*5000,*5000,iop,Ibf,5,1,iopwrd)
               Ibf(8) = 0
            ENDIF
!
 2950       DO ii = i , Icpbot , 3
               IF ( Ibf(1)==Icpdpl(ii) .AND. Ibf(2)==Icpdpl(ii+1) ) GOTO 2960
            ENDDO
            GOTO 5000
!
!     A 5-WORD RECORD READ, EXPANDED (THE TRAILERS) TO 8 WORDS
!
 2960       IF ( Ibf(8)==-999 ) THEN
               Ibf(8) = andf(Ibf(5),65535)
               Ibf(7) = rshift(Ibf(5),16)
               Ibf(6) = andf(Ibf(4),65535)
               Ibf(5) = rshift(Ibf(4),16)
               Ibf(4) = andf(Ibf(3),65535)
               Ibf(3) = rshift(Ibf(3),16)
            ENDIF
!
!     COPY FILE TO POOL
!
            CALL write(idp,Ibf,Icfiat-3,1)
 2970       CALL cpyfil(iop,idp,Ibf,Libf,iopwrd)
            Dpl(k+2) = Dpl(k+2) + iopwrd/1000 + 1
!
!     FILE ALL ON DATA POOL TAPE
!
            CALL eof(idp)
            filcon = 0
!
!     MAKE DPL ENTRY FOR ICPDPL ENTRY
!
            Dpl(k+2) = orf(orf(lshift(Dpl(k+2),16),Ndpfil),andf(Icpdpl(i+2),Ieqflg))
 2980       Dpl(k) = Icpdpl(i)
            Dpl(k+1) = Icpdpl(i+1)
            IF ( L8/=0 ) CALL conmsg(Dpl(k),2,0)
            k = k + 3
            Ndpfil = Ndpfil + 1
            Lstdpl = 1 + Lstdpl
            IF ( Lstdpl>Maxdpl ) GOTO 4900
            ptfct = andf(Noflgs,Icpdpl(i+2))
         ENDDO
!
!     FILES ALL COPIED OVER FROM OLD PROBLEM TAPE TO DATA POOL TAPE.
!
         CALL close(iop,1)
!
!     SEE IF XVPS IS ON POOL TAPE
!
         k = Lstdpl*3 + 1
         l = Ndpfil
         IF ( Dpl(k)/=nxvps ) THEN
!
!     VPS FILE IS NOT LAST ENTRY IN DPL - SEARCH DPL FOR IT
!
            DO j = 4 , k , 3
               IF ( Dpl(j)==nxvps ) GOTO 3000
            ENDDO
!
!     NO RESTART VPS TABLE
!
            GOTO 3800
         ELSE
!
!     VPS FILE IS LAST ENTRY IN DPL - DELETE ENTRY
!
            Lstdpl = Lstdpl - 1
            Ndpfil = Ndpfil - 1
            j = k
            GOTO 3020
         ENDIF
!
!     XVPS FOUND - ZERO NAME WHEN NOT LAST ENTRY IN DPL
!
 3000    Dpl(j) = 0
         Dpl(j+1) = 0
!
!     XVPS FILE FOUND IN DPL - POSITION POOL TAPE AND INITIALIZE
!     VPS TABLE WITH CHECKPOINT VALUES
!
 3020    CALL close(idp,3)
         CALL open(*4300,idp,Ibufr(idpbuf),2)
         CALL skpfil(idp,andf(Dpl(j+2),Maskhi)-l-1)
         nam1 = nxvps
         nam2 = Nblank
         CALL skpfil(idp,1)
         CALL read(*4200,*3100,idp,Ibf,Libf,1,idpwrd)
         EXIT
      ENDIF
   ENDDO
 3100 IF ( Ibf(1)/=nxvps ) GOTO 4200
   CALL read(*4200,*3200,idp,Ibf,Libf,1,idpwrd)
!
!     COMPARE RESTART PARAMETER NAMES WITH VPS NAMES
!
 3200 k = 3
 3300 j = 3
   IF ( andf(Vps(k+2),Modflg)/=Modflg ) THEN
      DO WHILE ( Ibf(2)>=j )
         IF ( Ibf(j)==Vps(k) .AND. Ibf(j+1)==Vps(k+1) ) THEN
!
!     PARAMETER NAMES MATCH AND MODFLG NOT ON - INITIALIZE VPS WITH
!     RESTART VALUE.
!
            l = Ibf(j+2)
            IF ( idelet/=1 ) THEN
               iparbt = mod(iparpt-1,31) + 2
               DO jjj = iparw1 , iparw2
                  IF ( Mjmsk(jjj)/=0 ) THEN
                     DO iii = iparbt , 32
                        IF ( andf(Mjmsk(jjj),Two(iii))/=0 ) THEN
                           nampt = 2*(31*(jjj-1)+iii-1) - 1
                           IF ( Mjcd(nampt)==Vps(k) .AND. Mjcd(nampt+1)==Vps(k+1) ) GOTO 3400
                        ENDIF
                     ENDDO
                  ENDIF
                  iparbt = 2
               ENDDO
            ENDIF
            DO m = 1 , l
               j1 = m + 2 + j
               k1 = m + 2 + k
               Vps(k1) = Ibf(j1)
            ENDDO
            EXIT
         ELSE
            j = j + Ibf(j+2) + 3
         ENDIF
      ENDDO
   ENDIF
!
!     CLEAR FLAGS AND TYPE CODE IN VPS ENTRY AND GET NEXT ENTRY.
!
 3400 Vps(k+2) = andf(Vps(k+2),Maskhi)
   k = k + Vps(k+2) + 3
   IF ( k<Vps(2) ) GOTO 3300
!
!     FOR UNMODIFIED RESTART LOAD CEITBL FROM LAST CHECKPOINT
!
   CALL read(*4200,*3500,idp,Ibf,Libf,1,idpwrd)
 3500 IF ( Start/=Imst ) THEN
      k1 = Ceitbl(2)
      j1 = Ibf(2)
!
!     FOR RESTART INITIALIZE REPT LOOP COUNTS WITH CHECKPOINT INFO
!
      DO j = 3 , j1 , 4
         DO k = 3 , k1 , 4
            IF ( Ceitbl(k+2)==Ibf(j+2) .AND. Ceitbl(k+3)==Ibf(j+3) .AND. Ibf(j+2)/=0 ) Ceitbl(k+1) = Ibf(j+1)
         ENDDO
      ENDDO
   ENDIF
!
!     FOR BOTH MOD AND UNMOD RESTART - LOAD VARIOUS CELLS IN /SYSTEM/
!     WITH LAST CHECKPOINT INFO
!
   CALL read(*3700,*3600,idp,Ibf,Libf,1,idpwrd)
 3600 Mpc = Ibf(5)
   Spc = Ibf(6)
   Load = Ibf(8)
 3700 CALL close(idp,1)
   idpbuf = ib1s
!
!
!     POSITION DATA POOL TAPE AT FIRST OSCAR ENTRY
!
 3800 CALL close(idp,1)
!
!     *** FIRST, PRODUCE DMAP XREF IF REQUESTED
!
   CALL open(*4300,idp,Ibufr(idpbuf),2)
   CALL skpfil(idp,idpfct-1)
   CALL fwdrec(*4100,idp)
   IF ( andf(Iflg(5),1)/=0 ) CALL oscxrf(idpfct-1,idpbuf-1)
   CALL close(idp,2)
!
!     WRITE VPS TABLE ON NEW PROBLEM TAPE IF CHECKPOINT FLAG ES SET
!     CLEAR FLAGS IN VPS
!
   k = 3
   DO
      Vps(k+2) = andf(Vps(k+2),Maskhi)
      k = k + Vps(k+2) + 3
      IF ( k>=Vps(2) ) THEN
         IF ( Icpflg/=0 ) THEN
!
!     POSITION TAPE FOR WRITING XVPS
!
            CALL open(*3900,npt,Ibufr(nptbuf),0)
            CALL skpfil(npt,andf(Nrlfl,Maskhi)-1)
            CALL close(npt,2)
            CALL open(*3900,npt,Ibufr(nptbuf),3)
            Ibf(1) = nxvps
            Ibf(2) = Nblank
            CALL write(npt,Ibf,2,1)
            CALL write(npt,Vps,Vps(2),1)
!
!     WRITE CEITBL TABLE ON NEW PROBLEM TAPE
!
            CALL write(npt,Ceitbl,Ceitbl(2),1)
            CALL eof(npt)
            CALL close(npt,2)
!
!     INITIALIZE CHECKPOINT PARAMETERS FOR XCHK AND XCEI ROUTINES
!
            Ptdic(Ptdtop) = nxvps
            Ptdic(Ptdtop+1) = Nblank
            Ptdic(Ptdtop+2) = Nrlfl
            Nrlfl = Nrlfl + 1
            Seqno = 1
!
!     WRITE NEW DICTIONARY ON XPTD
!
            CALL open(*3900,nxptdc,Ibufr(nptbuf),1)
            CALL write(nxptdc,nxptdc,2,1)
            CALL write(nxptdc,Nrlfl,2,1)
            CALL write(nxptdc,Ptdic(Ptdtop),3,1)
            CALL close(nxptdc,1)
!
!     PUNCH DICTIONARY ENTRY FOR XVPS TABLE
!
            nfile = andf(Maskhi,Ptdic(Ptdtop+2))
         ENDIF
         IF ( Nogo/=0 .OR. Lnogo ) GOTO 5300
         CALL xgpimw(9,nfile,Icpflg,Ifiat)
         Cppgct = Pagect
         IF ( Iflg(1)==0 ) CALL pexit
!
!     TERMINATE RUN IF ANY OF THE DIAG (17, 25, 28, OR 30) AND DIAG 20
!     ARE REQUESTED SIMULTANEOUSLY
!
         CALL sswtch(20,j)
         IF ( j==0 ) RETURN
         CALL sswtch(28,i)
         CALL sswtch(30,j)
         IF ( Diag17+Diag25+i+j==0 ) RETURN
         WRITE (Optape,99015)
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
 3900 CALL xgpidg(28,0,0,0)
   Nogo = 2
   GOTO 5300
!
!     UNEXPECTED END OF TAPE ON OLD PROBLEM TAPE
!
 4000 CALL xgpidg(29,0,0,0)
   Nogo = 2
   GOTO 5300
!
!     CANNOT FIND FILE ON DATA POOL TAPE
!
 4100 nam1 = ioshdr(1)
   nam2 = ioshdr(2)
 4200 CALL xgpidg(24,nam1,nam2,0)
   Nogo = 2
   GOTO 5300
!
!     UNEXPECTED END OF TAPE ON DATA POOL TAPE
!
 4300 CALL xgpidg(30,0,0,0)
   Nogo = 2
   GOTO 5300
!
!     CONTROL FILE INCOMPLETE OR MISSING ON NEW PROBLEM TAPE.
!
 4400 CALL xgpidg(31,nam1,nam2,0)
   Nogo = 2
   GOTO 5300
!
!     MED TABLE RECORD MISSING ON SCRATCH FILE
!
 4500 CALL xgpidg(69,nxgpi(1),nxgpi(2),0)
   Nogo = 2
   GOTO 5300
!
!     CARD OR FILE NAME TABLE RECORD MISSING ON SCRATCH FILE
!
 4600 CALL xgpidg(70,nxgpi(1),nxgpi(2),jtype)
   Nogo = 2
   GOTO 5300
!
!     ILLEGAL BIT NUMBERS IN CARD OR FILE NAME TABLE
!
 4700 CALL xgpidg(73,jtype,0,0)
   Nogo = 2
   GOTO 5300
!
!     SCRATCH FILE CONTAINING DMAP DATA COULD NOT BE OPENED
!
 4800 CALL xgpidg(33,nxgpi(1),nxgpi(2),0)
   Nogo = 2
   GOTO 5300
!
!     DPL TABLE OVERFLOW
!
 4900 CALL xgpidg(14,ndpl,Nblank,0)
   GOTO 5300
!
!     CANNOT FIND FILE ON OLD PROBLEM TAPE
!
 5000 CALL xgpidg(36,Icpdpl(i),Icpdpl(i+1),0)
   GOTO 5300
 5100 CALL xgpidg(36,Ptdic(i),Ptdic(i+1),0)
   GOTO 5300
!
!     INCORRECT OLD PROBLEM TAPE MOUNTED
!
 5200 CALL xgpidg(35,0,0,0)
 5300 WRITE (Optape,99016)
99016 FORMAT (//5X,'*** JOB TERMINATED DUE TO ABOVE ERRORS')
   CALL mesage(-37,0,nxgpi)
99017 FORMAT (//,' NONE',/)
99018 FORMAT (17X,I3,11X,2A4,14X,I3)
99019 FORMAT (A25,' 4146, LOGIC ERROR IN SUBROUTINE XGPI WHILE ','PROCESSING DATA CHANGES FOR MODIFIED RESTART.')
END SUBROUTINE xgpi
