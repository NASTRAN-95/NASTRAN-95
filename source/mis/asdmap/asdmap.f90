!*==asdmap.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE asdmap
!
!     THIS ROUTINE PROCESSES THE SUBSTRUCTURE COMMAND DATA DECK
!
!     IT CREATES A SET OF SUBSTRUCTURE DATA ON THE FRONT OF THE CASE
!     FILE AND GENERATES DMAP ALTERS FOR THE XALTER FILE. THE ALTERS ARE
!     PLACED FIRST ON THE SCRATCH FILE AND THEN COPIED TO THE PROBLEM
!     TAPE
!
   USE c_asdbd
   USE c_blank
   USE c_ginox
   USE c_machin
   USE c_output
   USE c_sofcom
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: alt1 , alt2 , blank , bwd , case , disk , dolsn , dry , drygo , ends , eqsn , gorun , idg , idry , inpt ,      &
                   & iopen , istp , item , jnew , k4wd , kbeg , kwd , lpar , mach , mskp , mwd , name , nano , nbrec , ncom ,       &
                   & ndbvar , new , nh1 , nh1a , nphase , nprec , nr3var , nrec , nsave , nsol , nstp , nxalt , nxcsa , nxl2 ,      &
                   & oper , opti , outp , papp , pass , pawd , pitm , poap , poit , posi , pove , ptape , pvec , pwd , rang , run , &
                   & scrt , sof , step , titl
   LOGICAL :: alter , altfl , ifin , recov , reject , skip , solve
   INTEGER , DIMENSION(18) :: alts
   INTEGER , DIMENSION(2) , SAVE :: asd1 , asd2 , ncasec , ncases , subnam
   INTEGER :: bandit , buf1 , buf2 , buf3 , cname , dryflg , file , i , i2 , i6777 , iac , iap2 , iapp , iblksz , ibuf , icard ,    &
            & icnext , icom , ier , ifile , inam , inex , intp , iprec , iread , irec , irun , isav , iskp , isof , isol , isopt ,  &
            & istep , j , jcom , jopt , jx , k , kalt , kfile , kpch , kprt , kwds , l , m , nc , ndbs , newbt , nlines , nlpp ,    &
            & nn , nogo , nopen , nout , ns , nsolv1 , nsolv2 , nvar , nw , nwds , nwords , nx , nz , obits , ofile , outt , phase ,&
            & sol
   INTEGER , DIMENSION(20) :: card
   INTEGER , DIMENSION(30) :: cdata
   INTEGER , DIMENSION(2,25) , SAVE :: comnd
   INTEGER , DIMENSION(2) :: corey
   INTEGER , DIMENSION(2,6,5) , SAVE :: dbval
   INTEGER , DIMENSION(6) , SAVE :: dbvar
   INTEGER , DIMENSION(18,60) :: dmap
   INTEGER , DIMENSION(3) , SAVE :: dvec , phs
   INTEGER , DIMENSION(2,14) , SAVE :: exdef
   INTEGER , DIMENSION(3,200) :: extra , ivar , var
   REAL :: fact
   INTEGER , DIMENSION(7) :: fname
   INTEGER , DIMENSION(9) :: ii
   INTEGER , DIMENSION(200) :: itemp , ocard
   INTEGER , DIMENSION(5) , SAVE :: itmn , r3var
   INTEGER , DIMENSION(2,100) :: nasub
   INTEGER , DIMENSION(16) , SAVE :: nhead
   LOGICAL , SAVE :: pass2
   INTEGER , DIMENSION(5,5) , SAVE :: r3val
   INTEGER , DIMENSION(1) :: z
   EXTERNAL andf , ascm01 , ascm02 , ascm03 , ascm04 , ascm05 , ascm06 , ascm07 , ascm08 , ascm09 , ascm10 , ascm11 , ascm12 ,      &
          & ascm13 , aspro , close , combo , complf , conmsg , eof , fwdrec , korsz , lshift , mesage , open , orf , page , page2 , &
          & read , redu , rewind , rshift , skpfil , sofioi , sswtch , write , xrcard , xread
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   !>>>>EQUIVALENCE (Ibuf,Sys(1)) , (Outt,Sys(2)) , (Nogo,Sys(3)) , (Intp,Sys(4)) , (Nlpp,Sys(9)) , (Nlines,Sys(12)) , (Iprec,Sys(55)) , &
   !>>>> & (Bandit,Sys(77))
   !>>>>EQUIVALENCE (var(1,1),ivar(1,1)) , (itemp(1),ocard(1)) , (Corex(1),Corey(1),Ndbs) , (Corey(2),Z(1))
   DATA alt1/4HALTE/ , alt2/4HR   / , asd1/4HASDM , 4HBEGN/ , asd2/4HASDM , 4HEND / , blank/4H    / , case/4HCASE/ , disk/4HDISK/ , &
      & dolsn/4H$   / , dry/4HDRY / , drygo/4HDRYG/ , ends/4HENDS/ , eqsn/4H=   /
   DATA exdef/ - 1 , 0 , 4HTAPE , 4H     , -1 , 0 , 4HINTE , 4H     , 0 , 0 , 4HNORE , 4H     , 4HALL  , 4H     , 4HWHOL , 4HESOF , &
      & 8*4HXXXX , -1 , 0 , -1 , 0/
   DATA idg/2H0 / , idry/2H-1/ , inpt/4HINPT/ , iopen/1/ , istp/2H1 / , item/4HITEM/
   DATA itmn/4HITM1 , 4HITM2 , 4HITM3 , 4HITM4 , 4HITM5/
   DATA jnew/4HNEW / , kbeg/4HBEGI/ , kwd/4HK   / , gorun/4HGO  / , lpar/4H(   / , mach/4HMACH/ , mwd/4HM   / , name/4HNAME/ ,      &
       &nano/4HNANO/ , nbrec/4HBREC/ , ncasec/4HCASE , 4HCC  / , ncases/4HCASE , 4HSS  / , new/4HNEW /
   DATA nhead/2*4H     , 4HN A  , 4HS T  , 4HR A  , 4HN  S , 4H U B , 4H S T , 4H R U , 4H C T , 4H U R , 4H E   , 4HD E  , 4HC K  ,&
       &4H E C , 4H H O/
   DATA nh1/4H1   / , nh1a/4H1A  / , nphase/3/ , nprec/4HPREC/ , nrec/4HRECO/ , nsave/4HSAVE/ , nsol/4HSOL / , nstp/4HNSTP/ ,       &
       &poit/4HPOIT/ , nxalt/4HXALT/ , nxcsa/4HXCSA/ , nxl2/4HER  / , oper/4HOPER/ , opti/4HOPTI/ , pass/4HPASS/ , pass2/.FALSE./
   DATA phs/4HE1   , 4HE2   , 4HE3  /
   DATA posi/4HPOSI/ , ptape/4HNPTP/ , pwd/4HP   / , run/4HRUN / , scrt/301/ , sof/4HSOF / , step/4HSTEP/ , titl/4HTITL/
   DATA mskp/4HMSKP/
   DATA papp/4HPAPP/ , pawd/4HPA  / , pitm/4HPITM/ , poap/4HPOAP/ , pove/4HPOVE/ , pvec/4HPVEC/
   DATA bwd/4HB   / , k4wd/4HK4  /
   DATA outp/4HOUTP/ , rang/4HRANG/
   DATA dvec/4HDVEC , 4HUDVF , 4HUDVT/
   DATA ndbvar/6/
   DATA dbvar/4HGORL , 4HPVEC , 4HUVEC , 4HPFTL , 4HOVEC , 4HOVC2/
   DATA dbval/4HGEOM , 4H4    , 4HPGG  , 4H     , 4HUGV  , 4H     , 4H     , 4H     , 4HOUGV , 4H1    , 4HOUGV , 4H     , 4HGEOM ,  &
       &4H4    , 4HPGG  , 4H     , 4HUGV  , 4H     , 4H     , 4H     , 4HOUGV , 4H1    , 4HOUGV , 4H     , 4HLAMA , 4H     ,        &
      & 4H     , 4H     , 4HPHIG , 4H     , 4H     , 4H     , 4HOPHI , 4HG1   , 4HOPHI , 4HG    , 4HGEOM , 4H4    , 4HPPF  ,        &
      & 4H     , 4HUGV  , 4H     , 4HPPF  , 4H     , 4HOUGV , 4H1    , 4HOUGV , 4H     , 4HGEOM , 4H4    , 4HPPT  , 4H     ,        &
      & 4HUGV  , 4H     , 4HTOL  , 4H     , 4HOUGV , 4H1    , 4HOUGV , 4H    /
   DATA nr3var/5/
   DATA r3var/4HUAPH , 4HPGVC , 4HPSVC , 4HDYNT , 4HQVEC/
   DATA r3val/4HULV  , 4HPGS  , 4HPSS  , 4H     , 4HQG   , 4HULV  , 4HPGS  , 4HPSS  , 4H     , 4HQG   , 4HPHIA , 4H     , 4H     ,  &
       &4HLAMA , 4HQG   , 4HUDVF , 4H     , 4H     , 4HPPF  , 4HQPC  , 4HUDVT , 4HPPT  , 4HPST  , 4HTOL  , 4HQP  /
   DATA ncom/25/
   DATA comnd/4HSUBS , 1 , 4HRUN  , 2 , 4HENDD , 2 , 4HCOMB , 3 , 4HREDU , 4 , 4HSOLV , 5 , 4HRECO , 6 , 4HMREC , 6 , 4HBREC , 7 ,  &
       &4HMRED , 9 , 4HCRED , 13 , 4HDEST , 10 , 4HEDIT , 10 , 4HEQUI , 10 , 4HSOFP , 10 , 4HDELE , 10 , 4HRENA , 10 , 4HSOFI , 11 ,&
       &4HSOFO , 11 , 4HREST , 11 , 4HDUMP , 11 , 4HCHEC , 11 , 4HCOMP , 11 , 4HAPPE , 11 , 4HPLOT , 12/
   DATA subnam/4HASDM , 4HAP  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
         CALL conmsg(asd1,2,0)
         DO i = 64 , 68
            iotit(i) = blank
         ENDDO
         DO i = 1 , 16
            ihead(i) = nhead(i)
         ENDDO
         CALL page
         nz = korsz(z(1))
         buf1 = nz - ibuf + 1
         buf2 = buf1 - ibuf
         buf3 = buf2 - ibuf
!
!     INITIALIZE THE CASE CONTROL FILE
!
         CALL open(*320,case,z(buf2),1)
         CALL close(case,1)
         iopen = 1
         nopen = buf3 - 1
         IF ( nopen<=100 ) CALL mesage(-8,100-nopen,subnam)
         first = .TRUE.
         skip = .FALSE.
         isopt = 0
!
!     SET NUMBER OF POSSIBLE COMMANDS HERE
!
!     SET LAST WORD INDICATER
!
         i6777 = rshift(complf(0),1)
!
!     READ FIRST CARD AFTER CEND
!
         ASSIGN 20 TO iread
         spag_nextblock_1 = 3
      CASE (2)
         IF ( skip ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( nlines>=nlpp ) CALL page
         nlines = nlines + 1
         WRITE (outt,99020) card
         spag_nextblock_1 = 3
      CASE (3)
         CALL xread(*300,card)
         CALL xrcard(ocard,200,card)
         IF ( ocard(1)>0 .AND. ocard(2)==blank ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ocard(1)==0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ocard(2)==titl .OR. ocard(2)==kbeg ) GOTO 300
         spag_nextblock_1 = 4
      CASE (4)
         skip = .FALSE.
         GOTO iread
!                      90?  NOT ASSIGNED BY ANYBODY    G.CHAN  4/93
!
 20      IF ( ocard(1)>0 .AND. ocard(2)==comnd(1,1) ) THEN
!
!     PROCESS SUBSTRUCTURE CARD
!
            cname = comnd(1,1)
            j = ocard(1)*2
            DO i = 1 , nphase
               IF ( ocard(j+1)==phs(i) ) THEN
                  phase = i
                  alter = .TRUE.
                  icom = 1
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
!
!     NO PHASE IS DEFINED
!
            WRITE (outt,99001) uwm
99001       FORMAT (A25,' 6002, INCORRECT PHASE DATA')
            nlines = nlines + 2
            alter = .FALSE.
            nogo = 1
            icom = 1
            phase = 2
         ELSE
!
!     NO SUBSTRUCTURE CARD
!
            WRITE (outt,99002) ufm
!
99002       FORMAT (A23,' 6001. SUBSTRUCTURE DATA IS REQUIRED WITH THIS ','APPROACH')
            nlines = nlines + 2
            nogo = 1
            phase = 2
            alter = .FALSE.
            skip = .TRUE.
            icom = 1
            IF ( ocard(2)==ends ) THEN
               spag_nextblock_1 = 19
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
!
!     FOUND PHASE. TURN BANDIT OFF IF PHASE IS 2
!
         IF ( phase==2 ) bandit = -1
         j = 2
         iapp = iabs(sys(21))
         IF ( iapp/=2 ) alter = .FALSE.
         iap2 = sys(69)/10
         IF ( iap2==1 ) alter = .FALSE.
         sol = 1
         IF ( .NOT.alter ) GOTO 80
         file = ptape
         kalt = 0
         kfile = 0
         CALL open(*320,ptape,z(buf1),0)
         spag_nextblock_1 = 6
      CASE (6)
         CALL skpfil(ptape,1)
         kfile = kfile + 1
         CALL read(*320,*40,ptape,fname,7,1,nwords)
 40      IF ( fname(1)/=nxalt ) THEN
            IF ( fname(1)/=nxcsa ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            altfl = .FALSE.
            sol = 1
            IF ( iapp/=3 ) THEN
               CALL read(*320,*60,ptape,ii,6,0,nwds)
               sol = ii(5)
            ENDIF
         ELSE
!
            kalt = kfile
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 60      CALL rewind(ptape)
         IF ( kalt/=0 ) altfl = .TRUE.
         IF ( altfl ) THEN
            CALL skpfil(ptape,kalt)
            CALL fwdrec(*320,ptape)
            CALL read(*320,*80,ptape,alts,2,1,nwds)
         ELSE
            CALL skpfil(ptape,kfile)
         ENDIF
!
!     NO XALTER FILE
!
!     OPEN CASE FILE FOR SUBSTRUCTURE DATA OR TITLE
!
 80      IF ( phase/=3 ) THEN
            file = case
            CALL open(*320,case,z(buf2),1)
            CALL write(case,ncases,2,1)
            file = scrt
         ENDIF
!
!     SET UP INITAL VALUES
!
         iac = 0
         istep = 0
         ndbs = 0
         dryflg = 1
         obits = 55
         IF ( sol==1 ) obits = 5
         IF ( sol==2 ) obits = 7
         IF ( sol==3 ) obits = 3
         IF ( sol==8 ) obits = 55
         IF ( sol==9 ) obits = 55
         newbt = obits
         recov = .FALSE.
         solve = .FALSE.
         iapp = sys(21)
         IF ( iapp==3 ) alter = .FALSE.
         IF ( alter ) THEN
            CALL open(*320,scrt,z(buf3),1)
            ii(1) = nxalt
            ii(2) = nxl2
            CALL write(scrt,ii,2,1)
         ENDIF
         nsof = 0
         isof = 1
         nname(1) = inpt
         stat = 1
         length(1) = 100
         paswd(1) = blank
         paswd(2) = blank
!
!     READ PASSWORD AND SOF DECLARATIONS
!
         inex = 0
         ASSIGN 100 TO iread
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 100     IF ( ocard(2)==pass ) THEN
            k = 4
            IF ( ocard(5)==eqsn ) k = 6
            paswd(1) = ocard(k)
            paswd(2) = ocard(k+1)
            ASSIGN 100 TO iread
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( ocard(2)/=sof ) THEN
            IF ( inex==1 ) THEN
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            inex = 1
            skip = .TRUE.
            icnext = 1
         ELSE
            k = 4
            IF ( ocard(5)==lpar ) THEN
               k = 9
               isof = ocard(7)
            ENDIF
            IF ( isof<0 .OR. isof>10 ) THEN
               WRITE (outt,99021) ufm
               nlines = nlines + 1
               nogo = 1
               ASSIGN 100 TO iread
               spag_nextblock_1 = 2
            ELSE
               nsof = nsof + 1
               IF ( ocard(k+1)==eqsn ) k = k + 2
               IF ( ocard(k+4)==jnew .OR. ocard(k+5)==jnew ) stat = 0
               nname(isof) = ocard(k)
               length(isof) = ocard(k+3)
               IF ( ocard(k+2)==-1 ) THEN
                  ASSIGN 100 TO iread
                  spag_nextblock_1 = 2
               ELSE
                  length(isof) = 100
                  IF ( nlines+3>nlpp ) CALL page
                  nlines = nlines + 3
                  IF ( .NOT.skip ) WRITE (outt,99020) card
                  WRITE (outt,99003) uwm , isof
99003             FORMAT (A25,', SOF(',I2,') FILESIZE NOT SPECIFIED. DEFAULT OF ','100K WORDS WILL BE ALLOCATED',/)
                  ASSIGN 100 TO iread
                  IF ( skip ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  spag_nextblock_1 = 3
               ENDIF
            ENDIF
            CYCLE
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
!
!     START PROCESSING SUBSTRUCTURE COMMAND CARDS HERE
!     TOP OF COMMAND LOOP
!
         icom = icnext
         IF ( ocard(2)==ends ) THEN
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO l = 1 , 30
            cdata(l) = ocard(l)
         ENDDO
         spag_nextblock_1 = 8
      CASE (8)
         cname = comnd(1,icom)
         jcom = comnd(2,icom)
         IF ( icom==6 .AND. sol>3 ) jcom = 8
         reject = .FALSE.
         IF ( jcom==2 ) THEN
            CALL ascm02(cname,phase,sol,nogo)
         ELSEIF ( jcom==3 ) THEN
            CALL ascm03(cname,phase,sol,nogo)
         ELSEIF ( jcom==4 ) THEN
            CALL ascm04(cname,phase,sol,nogo)
         ELSEIF ( jcom==5 ) THEN
            CALL ascm05(cname,phase,sol,nogo)
         ELSEIF ( jcom==6 ) THEN
            CALL ascm06(cname,phase,sol,nogo)
         ELSEIF ( jcom==7 ) THEN
            CALL ascm07(cname,phase,sol,nogo)
         ELSEIF ( jcom==8 ) THEN
            CALL ascm08(cname,phase,sol,nogo)
         ELSEIF ( jcom==9 ) THEN
            CALL ascm09(cname,phase,sol,nogo)
         ELSEIF ( jcom==10 ) THEN
            CALL ascm10(cname,phase,sol,nogo)
         ELSEIF ( jcom==11 ) THEN
            CALL ascm11(cname,phase,sol,nogo)
         ELSEIF ( jcom==12 ) THEN
            CALL ascm12(cname,phase,sol,nogo)
         ELSEIF ( jcom==13 ) THEN
            CALL ascm13(cname,phase,sol,nogo)
         ELSE
            CALL ascm01(cname,phase,sol,nogo)
         ENDIF
         jx = 0
         istep = istep + 1
!
!     TRANSFER RAW DMAP TO WORKING AREA
!
         m = irdm - 1
         DO j = 1 , nrdm
            DO i = 1 , 18
               m = m + 1
               dmap(i,j) = idat(m)
            ENDDO
         ENDDO
!
!     READ IN EXTRAS, FIND IN OPTION LIST, STOP AT NEXT COMMAND
!
         ASSIGN 120 TO iread
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 120     IF ( ocard(2)==pass .OR. ocard(2)==sof ) GOTO 100
         spag_nextblock_1 = 9
      CASE (9)
         IF ( reject ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( itemp(2)/=dolsn ) THEN
            IF ( itemp(2)==ends ) THEN
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( itemp(2)/=opti ) THEN
               IF ( nxtra/=0 ) THEN
                  m = ixtra - 1
                  DO i = 1 , nxtra
                     m = m + 1
                     IF ( itemp(2)==idat(m) ) GOTO 130
!
!     CARD IS NOT AN EXTRA
!
                  ENDDO
               ENDIF
!
!      CHECK AND SET IF COMMAND CARD
!
               DO i = 2 , ncom
                  icnext = i
                  IF ( ocard(2)==comnd(1,i) ) THEN
                     spag_nextblock_1 = 10
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
               GOTO 140
            ELSE
               newbt = 0
               i2 = 4
               IF ( itemp(5)==eqsn ) i2 = 6
               DO i = 1 , 6
                  j = 2*i + i2 - 2
                  IF ( itemp(j)==kwd ) newbt = orf(newbt,1)
                  IF ( itemp(j)==mwd ) newbt = orf(newbt,2)
                  IF ( itemp(j)==pwd ) newbt = orf(newbt,4)
                  IF ( itemp(j)==pawd ) newbt = orf(newbt,8)
                  IF ( itemp(j)==bwd ) newbt = orf(newbt,16)
                  IF ( itemp(j)==k4wd ) newbt = orf(newbt,32)
               ENDDO
               IF ( andf(newbt,12)==12 ) GOTO 140
               IF ( istep<=1 ) obits = newbt
               ASSIGN 120 TO iread
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     FOUND AN EXTRA, STORE SEQUENTIALLY AS PAIRS OF TWO WORD ITEMS
!
 130        jx = jx + 1
            extra(1,jx) = itemp(2)
            i2 = 4
            IF ( itemp(5)==eqsn ) i2 = 6
            extra(2,jx) = itemp(i2)
            extra(3,jx) = itemp(i2+1)
!
!     SPECIAL OUTPUT EXTRA
!
            IF ( itemp(2)/=outp ) THEN
               IF ( itemp(2)==rang ) THEN
                  jx = jx + 1
                  extra(1,jx) = rang
                  i2 = i2 + 2
                  IF ( itemp(i2)==-1 .OR. itemp(i2)==-2 ) THEN
                     extra(2,jx) = itemp(i2)
                     extra(3,jx) = itemp(i2+1)
                  ELSE
                     extra(2,jx) = extra(2,jx-1)
                     extra(3,jx) = extra(3,jx-1)
                     extra(3,jx-1) = 0
                  ENDIF
               ELSEIF ( extra(1,jx)==run ) THEN
                  extra(3,jx) = blank
                  extra(2,jx) = idry
                  IF ( itemp(i2)/=dry ) THEN
                     IF ( itemp(i2)==gorun ) THEN
                        extra(2,jx) = idg
                     ELSEIF ( itemp(i2)==step ) THEN
                        extra(2,jx) = istp
                     ELSEIF ( itemp(i2)/=drygo ) THEN
                        jx = jx - 1
                     ELSE
                        dryflg = 0
                     ENDIF
                  ENDIF
               ENDIF
               ASSIGN 120 TO iread
            ELSE
               extra(2,jx) = -1
               extra(3,jx) = 0
               DO WHILE ( itemp(i2)==-1 .AND. itemp(i2+1)>0 .AND. itemp(i2+1)<=31 )
                  j = lshift(1,itemp(i2+1)-1)
                  extra(3,jx) = orf(extra(3,jx),j)
                  i2 = i2 + 2
               ENDDO
               ASSIGN 120 TO iread
            ENDIF
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
 140        WRITE (outt,99021) ufm
            nlines = nlines + 2
            nogo = 1
         ENDIF
         ASSIGN 120 TO iread
         spag_nextblock_1 = 2
      CASE (10)
!
!     FOR PHASE 3 RECOVERY, CHANGE RECO TO BREC
!
         IF ( phase==3 .AND. comnd(1,icnext)==nrec ) THEN
            ocard(2) = nbrec
            icnext = 9
         ENDIF
         spag_nextblock_1 = 11
      CASE (11)
!
         IF ( icom==2 ) THEN
!
!     RUN COMMAND (SOMETIMES AN EXTRA)
!
            i2 = 4
            IF ( cdata(5)==eqsn ) i2 = 6
            var(1,1) = cdata(2)
            var(2,1) = istp
            var(3,1) = blank
            IF ( cdata(i2)/=step ) THEN
               var(2,1) = idry
               IF ( cdata(i2)==drygo ) dryflg = 0
            ENDIF
            IF ( dryflg==0 ) THEN
               spag_nextblock_1 = 15
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            nvar = 3
            nout = 0
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( icom==3 ) THEN
            spag_nextblock_1 = 19
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( icom==4 ) THEN
!
!     COMBINE OPERATION, USES SUBROUTINE COMBO
!
            CALL combo(cdata,jx,extra,iac,nasub,ns,var(1,3),ier)
            nvar = 3*(5+jx+3*ns)
            var(1,1) = ns
            var(2,1) = 0
            var(3,1) = 0
            var(1,2) = nstp
            var(2,2) = -1
            var(3,2) = istep
            nvar = nvar + 3
            var(nvar+1,1) = pitm
            var(nvar+2,1) = pvec
            var(nvar+3,1) = blank
            IF ( andf(obits,8)/=0 ) var(nvar+2,1) = papp
            nvar = nvar + 3
            nout = nvar
            IF ( ier/=0 ) THEN
               spag_nextblock_1 = 25
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( icom==5 .OR. icom==10 .OR. icom==11 ) THEN
!
!     REDUCE, MREDUCE, CREDUCE OPERATIONS - VARIABLES TO BE SET ARE
!
!                STEP - STEP NO.
!                NONA - NO. OF SUBSTRUCTURE A
!                NONB - NO. OF SUBSTRUCTURE B
!                NAMA - NAME OF SUBSTRUCTURE A
!                NAMB - NAME OF SUBSTRUCTURE B
!                PREC - PRECISION FLAG
!                PITM - LOAD ITEM
!                POIT - LOAD TRANSFORMATION ITEM
!
            CALL redu(cdata,jx,extra,iac,nasub,nvar,var(1,2),iprec,ier)
            var(1,1) = step
            var(2,1) = -1
            var(3,1) = istep
            nvar = nvar + 3
            var(nvar+1,1) = pitm
            var(nvar+2,1) = pvec
            var(nvar+3,1) = blank
            var(nvar+4,1) = poit
            var(nvar+5,1) = pove
            var(nvar+6,1) = blank
            IF ( andf(obits,8)/=0 ) THEN
               var(nvar+2,1) = papp
               var(nvar+5,1) = poap
            ENDIF
            nvar = nvar + 6
            nout = nvar
            IF ( ier/=0 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( icom==6 ) THEN
!
!     SOLVE OPERATION - VARIABLES ARE SUBSTRUCTURE NAME AND ALTER NO S
!
            nvar = 33
            nout = nvar
            i2 = 4
            IF ( cdata(5)==eqsn ) i2 = 6
            IF ( cdata(1)*2<i2 ) THEN
               spag_nextblock_1 = 26
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            var(1,8) = name
            var(2,8) = cdata(i2)
            var(3,8) = cdata(i2+1)
            nsolv1 = cdata(i2)
            nsolv2 = cdata(i2+1)
!
!     FIND STRUCTURE NUMBER
!
            ns = iac
            IF ( ns/=0 ) THEN
               DO i = 1 , ns
                  IF ( cdata(i2)==nasub(1,i) .AND. cdata(i2+1)==nasub(2,i) ) GOTO 150
               ENDDO
            ENDIF
            ns = ns + 1
            nasub(1,ns) = cdata(i2)
            nasub(2,ns) = cdata(i2+1)
            i = ns
 150        var(1,9) = nano
            var(2,9) = -1
            var(3,9) = i
            var(1,10) = step
            var(2,10) = -1
            var(3,10) = istep
            IF ( jcom==8 ) THEN
               var(1,11) = dvec(1)
               var(2,11) = dvec(2)
               var(3,11) = blank
               IF ( sol==9 ) var(2,11) = dvec(3)
            ELSE
               var(1,11) = nsol
               var(2,11) = blank
               var(3,11) = blank
            ENDIF
            IF ( sol/=1 ) THEN
               IF ( sol==2 ) var(2,11) = nh1a
               IF ( sol==3 ) var(2,11) = nh1
               nvar = 36
               nout = 36
               var(1,12) = mskp
               var(2,12) = blank
               var(3,12) = blank
            ENDIF
            iac = ns
            m = iph - 1
            DO i = 1 , 7
               m = m + 2
               var(1,i) = alt1
               var(2,i) = idat(m-1)
               var(3,i) = idat(m)
            ENDDO
            solve = .TRUE.
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( icom==7 .OR. icom==8 ) THEN
!
!     RECOVERY PHASE2 - VARIABLES ARE SOLUTION STRUCTURE NAME,
!                       PRINT, NAME AND/OR SAVE, NAME+ALTER
!
            i2 = 4
            IF ( cdata(5)==eqsn ) i2 = 6
            IF ( cdata(1)*2<i2 ) THEN
               spag_nextblock_1 = 26
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            var(1,1) = ncases(1)
            var(2,1) = ncases(2)
            var(3,1) = blank
            var(1,2) = name
            var(2,2) = cdata(i2)
            var(3,2) = cdata(i2+1)
            isol = sol
            IF ( sol>3 ) isol = isol - 4
            IF ( icom==8 ) isol = 3
            DO i = 1 , ndbvar
               var(1,i+2) = dbvar(i)
               var(2,i+2) = dbval(1,i,isol)
               IF ( icom==8 .AND. i<4 ) var(2,i+2) = blank
               var(3,i+2) = dbval(2,i,isol)
               IF ( icom==8 .AND. i<4 ) var(3,i+2) = blank
            ENDDO
            var(1,9) = nsol
            var(2,9) = -1
            var(3,9) = sol
            IF ( icom==8 ) var(3,9) = 3
            var(1,10) = step
            var(2,10) = -1
            var(3,10) = istep
            IF ( jx>0 ) THEN
               DO i = 1 , jx
                  DO k = 1 , 3
                     var(k,i+10) = extra(k,i)
                  ENDDO
               ENDDO
            ENDIF
            IF ( .NOT.(solve) ) THEN
!
!     SAVE OPTION BITS AND SET TO ZERO
!
               obits = 0
               var(1,4) = 0
               var(2,1) = ncasec(2)
            ENDIF
            recov = .TRUE.
            nvar = 3*jx + 30
            nout = nvar
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( icom==9 ) THEN
!
!     RECOVERY, PHASE 3.  VARIABLES ARE NAME, SOL, STEP, PREC, UAPH,
!     PGVC, PSVC, DYNT, QVEC
!
            i2 = 4
            IF ( cdata(i2)==eqsn ) i2 = i2 + 2
            m = iph - 1
            DO i = 1 , 3
               m = m + 2
               var(1,i) = alt1
               var(2,i) = idat(m-1)
               var(3,i) = idat(m)
            ENDDO
            isol = sol
            IF ( sol>3 ) isol = isol - 4
            DO i = 1 , nr3var
               var(1,i+3) = r3var(i)
               var(2,i+3) = r3val(i,isol)
               var(3,i+3) = blank
            ENDDO
            var(1,9) = name
            var(2,9) = cdata(i2)
            var(3,9) = cdata(i2+1)
            var(1,10) = nsol
            var(2,10) = -1
            var(3,10) = sol
            var(1,11) = step
            var(2,11) = -1
            var(3,11) = istep
            var(1,12) = nprec
            var(2,12) = -1
            var(3,12) = iprec
            nvar = 36
            nout = 0
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( icom==12 .OR. icom==13 .OR. icom==14 .OR. icom==15 .OR. icom==16 .OR. icom==17 ) THEN
!
!     UTILITY COMMANDS - USE SOFOUT MODULE TO MANIPULATE SOF FILE(S).
!     DESTROY, EDITOUT, EQUIV, PRINT, DELETE, AND RENAME
!
            nvar = 0
            i2 = 4
            kwds = 1
            SPAG_Loop_1_1: DO
!
!     DECODE AND STORE COMMAND DATA FROM HEADER CARD
!
               kwds = kwds + 1
               IF ( cdata(i2+1)==lpar .OR. cdata(i2+1)==eqsn ) i2 = i2 + 2
               var(2,kwds) = cdata(i2)
               var(3,kwds) = cdata(i2+1)
               i2 = i2 + 2
               IF ( cdata(i2)==i6777 .OR. cdata(i2+1)==i6777 ) EXIT SPAG_Loop_1_1
               IF ( var(2,kwds)==-1 ) i2 = i2 + 1
               IF ( kwds>=8 ) EXIT SPAG_Loop_1_1
            ENDDO SPAG_Loop_1_1
!
!     INSERT VARIABLE NAMES
!
            j = icom - 11
            var(1,1) = oper
            var(2,1) = cname
            var(3,1) = blank
            jopt = 0
            IF ( j==2 ) THEN
            ELSEIF ( j==3 .OR. j==6 ) THEN
!
!     EQUIV A,B   +PREFIX = B CARD
!
               var(1,2) = name
               var(1,3) = new
               nvar = 3
               IF ( j==6 ) THEN
                  spag_nextblock_1 = 13
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( kwds<2 ) THEN
                  spag_nextblock_1 = 25
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( jx<1 ) THEN
                  WRITE (outt,99004) uwm
99004             FORMAT (A25,' 6004, NO PREFIX DEFINED AFTER EQUIVALENCE.')
                  nlines = nlines + 2
               ELSE
                  var(1,4) = extra(1,1)
                  var(2,4) = extra(2,1)
                  var(3,4) = extra(3,1)
                  nvar = 4
               ENDIF
               spag_nextblock_1 = 13
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( j==4 ) THEN
            ELSEIF ( j==5 ) THEN
               i2 = 2
               spag_nextblock_1 = 12
               CYCLE SPAG_DispatchLoop_1
            ELSE
!
!      DESTROY NAME
!
               var(1,2) = name
!
!     EDITOUT(CODE) = NAME
!
               nvar = 2
               spag_nextblock_1 = 13
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     PRINT(CODE) = NAME,ITM1,ITM2,ITM3,ITM4,ITM5
!
            IF ( var(2,2)/=-1 ) THEN
               i2 = 2
            ELSE
               var(1,2) = opti
               jopt = 1
               i2 = 3
            ENDIF
         ELSEIF ( icom==18 .OR. icom==19 .OR. icom==20 .OR. icom==21 .OR. icom==22 .OR. icom==23 .OR. icom==24 ) THEN
!
!     EXIO OPERATIONS -
!     SOFIN, SOFOUT, RESTORE, DUMP, CHECK, COMPRESS AND APPEND
!
            nvar = 42
            nout = 0
!
            DO i = 1 , 14
               var(1,i) = 100 + i
               var(2,i) = exdef(1,i)
               var(3,i) = exdef(2,i)
            ENDDO
!
!     DECODE COMMAND CARD
!
            var(2,5) = cdata(2)
            var(3,5) = cdata(3)
            i2 = 4
            IF ( cdata(5)==lpar ) THEN
               var(2,4) = cdata(6)
               var(3,4) = cdata(7)
               i2 = 8
            ENDIF
            IF ( cdata(i2+1)==eqsn ) i2 = i2 + 2
            IF ( cdata(i2)==i6777 .OR. cdata(i2+1)==lpar ) THEN
               WRITE (outt,99005) ufm
99005          FORMAT (A23,' 6008, ILLEGAL INPUT ON THE PREVIOUS COMMAND.',/5X,'MISSING FILE NAME FOR IO OPERATION')
               nlines = nlines + 3
               spag_nextblock_1 = 16
            ELSE
               var(2,3) = cdata(i2)
               var(3,3) = cdata(i2+1)
               IF ( cdata(i2+2)/=i6777 ) THEN
                  var(2,2) = cdata(i2+2)
                  var(3,2) = cdata(i2+3)
               ENDIF
!
!     SET EXTRAS
!
               nn = 0
               IF ( jx/=0 ) THEN
                  DO i = 1 , jx
                     IF ( extra(1,i)==mach ) THEN
                        k = 1
                     ELSEIF ( extra(1,i)==posi ) THEN
                        k = 6
                     ELSEIF ( extra(1,i)/=item ) THEN
                        IF ( extra(1,i)/=name ) CYCLE
                        nn = nn + 1
                        k = nn + 7
                     ELSE
                        k = 7
                     ENDIF
                     var(2,k) = extra(2,i)
                     var(3,k) = extra(3,i)
                  ENDDO
               ENDIF
!
!     SET DISK FIELD FOR COMPRESS ETC
!
               IF ( icom>=24 ) var(2,2) = disk
               spag_nextblock_1 = 14
            ENDIF
            CYCLE
         ELSEIF ( icom==25 ) THEN
!
!     PLOT COMMAND
!          FORMAT
!     PLOT NAME
!
            nvar = 6
            nout = 0
            i2 = 4
            IF ( cdata(i2)==eqsn ) i2 = 6
            IF ( cdata(1)*2<i2 ) THEN
               spag_nextblock_1 = 25
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            var(1,1) = name
            var(2,1) = cdata(i2)
            var(3,1) = cdata(i2+1)
            var(1,2) = step
            var(2,2) = -1
            var(3,2) = istep
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
!
!     SUBSTRUCTURE   PHASES
!         PHASE 1
!      VARIABLES,     NO.     TYPE      POSITION      DEFINITION
!                     1,2,3    I           1    ALTE,R.F. REMOVE NUMBERS
!                     4,5,6    I           4    ALTE,R.F. REMOVE NUMBERS
!                     7,8,9                     ALTE,R.F. REMOVE NUMBERS
!                     10,11,12                  SAVE,-1, PLOT SET ID
!                 1   13,14,15                  RUN ,-1, RUN  FLAG
!                 1   16,17,18                  NAME, SUBS NAME
!
         ELSEIF ( phase==2 ) THEN
!
!     PHASE 2 PROCESS
!
            IF ( jx<=0 ) THEN
               jx = 1
               extra(1,1) = run
               extra(2,1) = istp
               extra(3,1) = blank
            ENDIF
            var(1,1) = alt1
            var(2,1) = 4
            IF ( sol==1 ) var(2,1) = 5
            var(3,1) = 0
!
            DO j = 1 , jx
               DO i = 1 , 3
                  var(i,j+1) = extra(i,j)
               ENDDO
            ENDDO
            nvar = 3*(1+jx)
            nout = 0
            DO i = 1 , 5
               dmap(1,i) = -1
            ENDDO
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( phase==3 ) THEN
!
!     PHASE 3 PROCESSING
!     NORMALLY THIS IS A RESTART, IF NOT THE DATA WILL BE REGENERATED
!
            nvar = 6
            var(1,1) = alt1
            var(2,1) = idat(iph)
            var(3,1) = idat(iph+1)
            var(1,2) = run
            var(2,2) = istp
            var(3,2) = blank
            IF ( jx>=1 ) THEN
               IF ( extra(1,1)==run ) var(2,2) = extra(2,1)
            ENDIF
            nout = 0
            DO i = 1 , 5
               dmap(1,i) = -1
            ENDDO
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ELSE
            nvar = 24
            nout = 0
            DO i = 1 , nvar
               var(i,1) = 0
            ENDDO
            var(1,8) = pitm
            var(2,8) = pvec
            var(3,8) = blank
            IF ( andf(obits,8)/=0 ) var(2,8) = papp
            DO i = 1 , jx
               DO j = 1 , 3
                  var(j,i+4) = extra(j,i)
               ENDDO
            ENDDO
            nx = jx + 4
            inam = 0
            irun = 0
            isav = 0
!
!     CHECK FOR REQUIRED NAME
!
            DO i = 5 , nx
               IF ( var(1,i)==name ) inam = i
               IF ( var(1,i)==run ) irun = i
               IF ( var(1,i)==nsave ) isav = i
            ENDDO
!
!     NO NAME DEFINED IS A LEVEL 3 ERROR
!
            IF ( inam<=0 ) THEN
               spag_nextblock_1 = 25
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( irun==0 ) THEN
               irun = nx + 1
               var(1,irun) = run
               var(2,irun) = istp
               var(3,irun) = blank
               nx = nx + 1
            ENDIF
            IF ( isav==0 ) THEN
               var(1,nx+1) = nsave
               var(2,nx+1) = -1
               var(3,nx+1) = 0
            ENDIF
            m = iph - 1
            DO i = 1 , 4
               m = m + 2
               var(1,i) = alt1
               var(2,i) = idat(m-1)
               var(3,i) = idat(m)
            ENDDO
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 12
      CASE (12)
         var(1,i2) = name
         ns = kwds - i2
         DO i = 1 , ns
            j = i2 + i
            var(1,j) = itmn(i)
         ENDDO
         nvar = kwds
         spag_nextblock_1 = 13
      CASE (13)
         nout = 0
         IF ( jopt/=1 ) THEN
            nvar = nvar + 1
            var(1,nvar) = opti
            var(2,nvar) = -1
            var(3,nvar) = 32
            IF ( icom==15 ) var(3,nvar) = 0
         ENDIF
         nvar = 3*nvar
         spag_nextblock_1 = 14
      CASE (14)
!
!     PROCESS VARIABLE CHARACTERS IF DMAP IS TO BE GENERATED
!
         IF ( alter ) THEN
            CALL aspro(dmap,ivar,nvar,obits,sol)
!
!     RESET OPTION BITS IF DUMMY VALUE WAS USED
!
            obits = newbt
            IF ( nogo<1 ) THEN
!
!     WRITE  DMAP ON  SCRATCH FILE
!
               DO i = 1 , nrdm
                  spag_nextblock_2 = 1
                  SPAG_DispatchLoop_2: DO
                     SELECT CASE (spag_nextblock_2)
                     CASE (1)
!
!     GO TO SPECIAL CODE IF AN ALTER CARD
!
                        IF ( dmap(1,i)/=alt1 ) THEN
!
!     WRITE ORDINARY DMAP DATA HERE
!
                           CALL write(scrt,dmap(1,i),18,1)
                           CYCLE
                        ELSE
!
                           ii(1) = dmap(2,i)
                           ii(2) = dmap(3,i)
                           IF ( .NOT.altfl ) THEN
                              spag_nextblock_2 = 3
                              CYCLE SPAG_DispatchLoop_2
                           ENDIF
                           IF ( ii(2)==0 ) ii(2) = -ii(1)
                        ENDIF
                        spag_nextblock_2 = 2
                     CASE (2)
                        IF ( alts(2)==0 ) alts(2) = -alts(1)
!
                        IF ( alts(1)>iabs(ii(2)) ) THEN
                           spag_nextblock_2 = 3
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
!
!     OVERLAPPING DMAP
!
                        IF ( iabs(alts(2))>=ii(1) ) THEN
                           spag_nextblock_1 = 26
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
!
!     ALTERS ENCOUNTERED BEFORE NEW ALTERS
!
                        IF ( alts(2)<0 ) alts(2) = 0
                        CALL write(scrt,alts,2,1)
                        file = ptape
                        DO
                           CALL read(*154,*152,ptape,alts,18,1,nwds)
!
!     DMAP DATA ENCOUNTERED
!
                           CALL write(scrt,alts,18,1)
                        ENDDO
!
!     MORE ALTERS ENCOUNTERED
!
 152                    IF ( nwds==2 ) THEN
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
!
!     END OF USER ALTERS
!
 154                    altfl = .FALSE.
                        spag_nextblock_2 = 3
                     CASE (3)
!
!     INSERT NEW DMAP ALTERS
!
                        IF ( ii(2)<0 ) ii(2) = 0
!
                        CALL write(scrt,ii,2,1)
                        EXIT SPAG_DispatchLoop_2
                     END SELECT
                  ENDDO SPAG_DispatchLoop_2
               ENDDO
            ENDIF
         ENDIF
         spag_nextblock_1 = 15
      CASE (15)
!
!     WRITE COMMAND AND VARIABLE DATA ON CASE CONTROL FILE
!
         IF ( phase/=3 ) THEN
            ii(1) = cname
            ii(2) = nout
            CALL write(case,ii,2,0)
            CALL write(case,ivar,nout,1)
         ENDIF
         spag_nextblock_1 = 16
      CASE (16)
!
!
         IF ( itemp(2)/=ends ) THEN
!
!
            reject = .FALSE.
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 17
      CASE (17)
!
!     ENDSUBS ENCOUNTERED,  STOP PROCESS
!     ENSURE THAT A RECOVER ALWAYS EXISTS FOLLOWING A SOLVE
!
         IF ( phase/=2 .OR. .NOT.solve .OR. recov ) THEN
!
!     CHECK SOF AND PASSWORD DECLARATIONS
!
            IF ( paswd(1)/=blank ) THEN
               IF ( nsof>0 ) THEN
                  spag_nextblock_1 = 18
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
            CALL page2(2)
            WRITE (outt,99006) ufm
99006       FORMAT (A23,' 6011, SOF DATA PASSWORD MISSING')
            nogo = 1
         ELSE
!
!     CONSTRUCT A DUMMY INPUT CARD
!
            cdata(1) = 4
            cdata(2) = nrec
            cdata(3) = blank
            cdata(4) = nsolv1
            cdata(5) = nsolv2
            skip = .TRUE.
            icom = 7
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 18
      CASE (18)
         iblksz = ibuf - 4
         IF ( mchn==3 .OR. mchn==4 ) iblksz = iginob
         fact = 1000.0/iblksz
         DO i = 1 , 10
            length(i) = length(i)*fact
            jx = length(i)/2
            length(i) = 2*jx
         ENDDO
!
!     INITIALIZE DIRECT ACCESS FILES FOR IBM 360/370 MACHINES
!
         IF ( mchn==2 ) CALL sofioi
         spag_nextblock_1 = 19
      CASE (19)
         CALL page2(1)
         WRITE (outt,99007)
99007    FORMAT (7X,7HENDSUBS)
         IF ( .NOT.alter ) THEN
            spag_nextblock_1 = 24
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     WRAP UP DMAP
!     PUT LABEL ON END OF ALTER DECK
!
         cname = comnd(1,3)
         CALL ascm02(cname,phase,sol,nogo)
         m = irdm + 18
         CALL write(scrt,idat(m),18,1)
!
!     REPEAT ALTER IF DRYGO IS ON
!
         IF ( dryflg==0 ) THEN
            DO i = 1 , 3
               m = m + 18
               CALL write(scrt,idat(m),18,1)
            ENDDO
         ENDIF
!
!     JUMP TO FINISH OF RIGID FORMAT
!
         IF ( phase/=3 ) CALL write(scrt,idat(91),18,1)
         ifile = ptape
         ofile = scrt
         ifin = .FALSE.
         pass2 = .FALSE.
         IF ( .NOT.altfl ) THEN
            CALL eof(scrt)
            CALL read(*320,*160,ptape,ii,9,1,nw)
         ELSE
            IF ( alts(2)<0 ) alts(2) = 0
            CALL write(scrt,alts,2,1)
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     COPY REMAINDER OF PROBLEM TAPE TO SCRATCH FILE
!
 160     ifin = .FALSE.
 180     IF ( ii(1)==nxcsa ) ifin = .TRUE.
         CALL write(ofile,ii,nw,1)
!
!
         irec = 1
         spag_nextblock_1 = 20
      CASE (20)
         DO
            CALL read(*220,*200,ifile,z(iopen),nopen,0,nwds)
!
            CALL write(ofile,z(iopen),nopen,0)
         ENDDO
!
!     SET ALTER FLAG ON SOL RECORD OF XCSA FILE
!
 200     IF ( ifin .AND. pass2 .AND. irec==1 ) z(iopen+2) = 1
         CALL write(ofile,z(iopen),nwds,1)
         irec = irec + 1
         spag_nextblock_1 = 20
         CYCLE SPAG_DispatchLoop_1
 220     CALL eof(ofile)
         IF ( ifin ) THEN
            CALL close(ifile,1)
            CALL close(ofile,3)
            IF ( pass2 ) THEN
!
               CALL close(scrt,1)
               spag_nextblock_1 = 24
               CYCLE SPAG_DispatchLoop_1
            ELSE
!
!     PRINT OR PUNCH ALTER DECK HERE
!
!     DIAG 23 REQUESTS PRINT
!     DIAG 24 REQUESTS PUNCH
!
               CALL sswtch(23,kprt)
               CALL sswtch(24,kpch)
               IF ( kprt==0 .AND. kpch==0 ) THEN
                  spag_nextblock_1 = 23
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               icard = 0
               CALL open(*320,scrt,z(buf3),0)
            ENDIF
         ELSE
            CALL read(*320,*180,ifile,ii,9,1,nw)
            GOTO 180
         ENDIF
         spag_nextblock_1 = 21
      CASE (21)
!
         CALL page
         WRITE (outt,99008)
99008    FORMAT (5X,'ALTER DECK ECHO')
         nlines = nlines + 1
         spag_nextblock_1 = 22
      CASE (22)
         DO WHILE ( nlines<nlpp .OR. kprt==0 )
            CALL read(*260,*240,scrt,card,18,1,nw)
!
!     DMAP CARD
!
            nc = 18
            IF ( kprt/=0 ) WRITE (outt,99009) icard , (card(i),i=1,nc)
99009       FORMAT (4X,I5,4X,18A4)
            IF ( kprt/=0 ) nlines = nlines + 1
            IF ( kpch/=0 ) WRITE (lpch,99010) (card(i),i=1,nc)
99010       FORMAT (18A4)
            icard = icard + 1
         ENDDO
         spag_nextblock_1 = 21
         CYCLE SPAG_DispatchLoop_1
 240     IF ( icard<=0 ) THEN
            icard = 1
!
!      ALTER CARD
!
         ELSEIF ( card(2)<=0 ) THEN
            IF ( kprt/=0 ) WRITE (outt,99011) icard , alt1 , alt2 , card(1)
99011       FORMAT (5X,I4,4X,2A4,I8)
            IF ( kprt/=0 ) nlines = nlines + 1
            IF ( kpch/=0 ) WRITE (lpch,99012) alt1 , alt2 , card(1)
99012       FORMAT (2A4,I8)
            icard = icard + 1
         ELSE
            IF ( kprt/=0 ) WRITE (outt,99013) icard , alt1 , alt2 , (card(i),i=1,2)
99013       FORMAT (5X,I4,4X,2A4,I8,1H,,I3)
            IF ( kprt/=0 ) nlines = nlines + 1
            IF ( kpch/=0 ) WRITE (lpch,99014) alt1 , alt2 , card(1) , card(2)
99014       FORMAT (2A4,I8,1H,,I3)
            icard = icard + 1
         ENDIF
         spag_nextblock_1 = 22
         CYCLE SPAG_DispatchLoop_1
!
!     END OF FILE
!
 260     CALL close(scrt,0)
         spag_nextblock_1 = 23
      CASE (23)
         CALL open(*320,scrt,z(buf3),0)
         CALL open(*320,ptape,z(buf1),0)
!
!     COPY SCRATCH TO PROB.TAPE, FIRST POSITION PTAPE TO XALTER OR
!     XCSA FILE
!
         iskp = kfile
         IF ( kalt/=0 ) iskp = kalt
         CALL skpfil(ptape,iskp)
         CALL close(ptape,2)
         CALL open(*320,ptape,z(buf1),3)
         CALL read(*320,*280,scrt,ii,9,1,nw)
 280     pass2 = .TRUE.
         ifin = .FALSE.
         ifile = scrt
         ofile = ptape
         GOTO 180
      CASE (24)
!
!     CLOSE CASE CONTROL
!
         IF ( phase/=3 ) CALL close(case,2)
         CALL conmsg(asd2,2,0)
         RETURN
!
!     USER FATAL MESSAGES
!
 300     WRITE (outt,99015) ufm
99015    FORMAT (A23,' 6017, MISSING ENDSUBS CARD.')
         CALL mesage(-37,0,subnam)
!
!     SYSTEM ERROR MESSAGES
!
 320     WRITE (outt,99016) sfm , file
99016    FORMAT (A25,' 6007, IMPROPER FILE SETUP FOR ',A4)
         nlines = nlines + 2
         WRITE (outt,99017) ufm
99017    FORMAT (A23,' 6009, UNRECOVERABLE ERROR CONDITIONS IN SUBROUTINE',' ASDMAP')
         nlines = nlines + 2
         nogo = 3
         CALL close(scrt,1)
         CALL close(case,1)
         CALL close(ptape,1)
         RETURN
      CASE (25)
         WRITE (outt,99018) ufm , cname
99018    FORMAT (A23,' 6005, ILLEGAL OR MISSING DATA FOR THE PREVIOUS ','COMMAND - ',A4)
         nlines = nlines + 2
         nogo = 1
         spag_nextblock_1 = 16
      CASE (26)
         WRITE (outt,99019) ufm , alts(1) , alts(2) , ii
99019    FORMAT (A23,' 6006, DMAP ALTERS  ',2I8,/5X,'INTERFERE WITH SUBSTRUCTURE ALTERS  ',2I4)
         nlines = nlines + 3
         nogo = 1
         spag_nextblock_1 = 16
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99020 FORMAT (1H ,4X,20A4)
99021 FORMAT (A23,' 6003. ILLEGAL COMMANDS OR OPTIONS DEFINED ON NEXT ','CARD')
END SUBROUTINE asdmap
