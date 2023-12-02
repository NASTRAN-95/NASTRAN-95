!*==ifp1pc.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifp1pc(I81,Icont,Pocard,Org,Porg)
!
!     SUBROUTINE TO PERFORM FIRST-LEVEL CHECKING OF STRUCTURE PLOTTER
!     CONTROL CARD FORMAT.
!
   USE c_system
   USE c_xifp1
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: I81
   INTEGER :: Icont
   INTEGER , DIMENSION(1) :: Pocard
   INTEGER :: Org
   INTEGER :: Porg
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: allon , anti , defo , eor , forg , hidd , i , iax , icrd , ierr , integ , ipr1 , ipr2 , iprm , iro , irtn , isplot ,  &
            & istb , istt , itype , ivc , iword , iwrd , j , magn , mode , msgno , ncrd , nint , nopt , nreal , nro , nthru , poin ,&
            & porg1 , proj , symm , thru
   INTEGER , DIMENSION(3) , SAVE :: axes , idvpr , maxes
   INTEGER , DIMENSION(5) , SAVE :: camera , lblpr
   INTEGER , DIMENSION(400) :: case
   INTEGER , DIMENSION(20) , SAVE :: cntur
   INTEGER , DIMENSION(25) , SAVE :: coord
   INTEGER , DIMENSION(1) :: core
   INTEGER , DIMENSION(401) :: corey
   INTEGER , DIMENSION(21) , SAVE :: ctype
   LOGICAL , DIMENSION(3) :: flag
   INTEGER , SAVE :: ilnk , lag , plan , sepa , ter
   INTEGER , DIMENSION(2) , SAVE :: nast
   INTEGER , DIMENSION(11) :: origin
   INTEGER , DIMENSION(28) , SAVE :: pltpr
   INTEGER , DIMENSION(12) , SAVE :: setp2
   INTEGER , DIMENSION(33) , SAVE :: setpr
   EXTERNAL anullsub , complf , mvbits , page2 , rshift
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
   INTEGER :: spag_nextblock_4
   !>>>>EQUIVALENCE (proj,ctype(11)) , (defo,idvpr(1)) , (symm,pltpr(13)) , (anti,pltpr(14)) , (magn,cntur(13)) , (thru,pltpr(22)) ,     &
!>>>>    & (poin,lblpr(2)) , (core(1),Corey(401)) , (Corex(1),Corey(1),Case(1)) , (hidd,pltpr(24))
   DATA ctype/4HPLOT , 4HORTH , 4HPERS , 4HSTER , 4HAXES , 4HVIEW , 4HMAXI , 4HCSCA , 4HFIND , 4HCONT , 4HPROJ , 4HOCUL , 4HCAME ,  &
       &4HPAPE , 4HPEN  , 4HPTIT , 4HSCAL , 4HORIG , 4HVANT , 4HSET  , 4HREGI/
   DATA camera/4HFILM , 4HPAPE , 4HBOTH , 4HBLAN , 4HFRAM/
   DATA axes/4HX    , 4HY    , 4HZ   /
   DATA maxes/4HMX   , 4HMY   , 4HMZ  /
   DATA cntur/4HMAJP , 4HMINP , 4HMAXS , 4HXNOR , 4HYNOR , 4HZNOR , 4HXYSH , 4HXZSH , 4HYZSH , 4HXDIS , 4HYDIS , 4HZDIS , 4HMAGN ,  &
       &4HNRM1 , 4HNRM2 , 4HSH12 , 4HSH1Z , 4HSH2Z , 4HBDSH , 4HSTRA/
   DATA setpr/4HINCL , 4HEXCL , 4HEXCE , 4HELEM , 4HGRID , 4HALL  , 4HAERO , 4HAXIF , 4HBAR  , 4HCONE , 4HCONR , 4HHEXA , 4HFLUI ,  &
       &4HIHEX , 4HPLOT , 4HQDME , 4HQDPL , 4HQUAD , 4HROD  , 4HSHEA , 4HSLOT , 4HTETR , 4HTORD , 4HTRAP , 4HTRBS , 4HTRIA ,        &
      & 4HTRME , 4HTRPL , 4HTUBE , 4HTWIS , 4HVISC , 4HWEDG , 4HHBDY/
   DATA setp2/4HAX   , 4HRG   , 4H1    , 4H2    , 4H3    , 4H4    , 4HD2   , 4HD3   , 4HD4   , 4HM    , 4HM1   , 4HM2  /
   DATA pltpr/4HSET  , 4HSTAT , 4HMODA , 4HCMOD , 4HFREQ , 4HTRAN , 4HCONT , 4HRANG , 4HTIME , 4HPHAS , 4HMAGN , 4HORIG , 4HSYMM ,  &
       &4HANTI , 4HPEN  , 4HDENS , 4HSYMB , 4HLABE , 4HSHAP , 4HVECT , 4HOUTL , 4HTHRU , 4HMAXI , 4HHIDD , 4HSHRI , 4HNOFI ,        &
      & 4HFILL , 4HOFFS/
   DATA idvpr/4HDEFO , 4HVELO , 4HACCE/
   DATA coord/4HYX   , 4HZX   , 4HZY   , 4HXY   , 4HXZ   , 4HYZ   , 4HX    , 4HY    , 4HZ    , 4HXYZ  , 4HRXY  , 4HRXZ  , 4HRYZ  ,  &
       &4HR    , 4HRN   , 4HXN   , 4HYN   , 4HZN   , 4HXYN  , 4HXZN  , 4HYZN  , 4HXYZN , 4HRXYN , 4HRXZN , 4HRYZN/
   DATA lblpr/4HGRID , 4HPOIN , 4HELEM , 4HBOTH , 4HEPID/
   DATA ter/4HTER / , plan/4HPLAN/ , sepa/4HSEPA/
   DATA lag/4HLAG / , nast/4HSC   , 4HCALC/ , ilnk/4HNS01/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
!     INITIALIZE
!
         IF ( intra>1 .OR. ilink/=ilnk ) THEN
            DO i = 1 , 200
               core(i) = Pocard(i)
            ENDDO
         ENDIF
         allon = complf(0)
         eor = rshift(allon,1)
         isplot = 0
         iwrd = I81
!
!     BRANCH FOR CONTINUATION CARD
!                                  SET   PLOT  FIND
         IF ( Icont/=0 ) THEN
            IF ( Icont==1 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Icont==2 ) THEN
               spag_nextblock_1 = 31
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Icont==3 ) GOTO 300
            IF ( Icont==4 ) THEN
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
         IF ( core(iwrd)<0 ) THEN
!
!     SET UP ERROR MESSAGE
!
            ASSIGN 380 TO ierr
            msgno = 348
            spag_nextblock_1 = 66
         ELSEIF ( core(iwrd)==0 ) THEN
            spag_nextblock_1 = 6
         ELSE
            spag_nextblock_1 = 3
         ENDIF
      CASE (2)
         IF ( core(iwrd)<=0 ) GOTO 20
         spag_nextblock_1 = 3
      CASE (3)
         IF ( core(iwrd)==eor ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         mode = core(iwrd)
         iwrd = iwrd + 1
         spag_nextblock_1 = 4
      CASE (4)
!
!     BRANCH FOR CARD TYPE
!
         iword = core(iwrd)
         IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
         DO i = 1 , 20
            IF ( iword==ctype(i) ) THEN
               IF ( i==1 ) THEN
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( i==2 .OR. i==3 .OR. i==4 ) THEN
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( i==5 ) THEN
                  spag_nextblock_1 = 9
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( i==6 ) THEN
                  spag_nextblock_1 = 10
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( i==7 ) THEN
                  spag_nextblock_1 = 11
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( i==8 ) THEN
                  spag_nextblock_1 = 12
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( i==9 ) THEN
                  spag_nextblock_1 = 13
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( i==10 ) THEN
                  spag_nextblock_1 = 20
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( i==11 ) THEN
                  spag_nextblock_1 = 21
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( i==12 ) THEN
                  spag_nextblock_1 = 22
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( i==13 ) THEN
                  spag_nextblock_1 = 24
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( i==14 .OR. i==15 .OR. i==16 ) GOTO 20
               IF ( i==17 ) THEN
                  spag_nextblock_1 = 27
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( i==18 ) THEN
                  spag_nextblock_1 = 28
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( i==19 ) THEN
                  spag_nextblock_1 = 29
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( i==20 ) THEN
                  spag_nextblock_1 = 30
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
!
!    1         PLOT  ORTH  PERS  STER  AXES  VIEW  MAXI  CSCA  FIND
!    2         CONT  PROJ  OCUL  CAME  PAPE   PEN  PTIT  SCAL  ORIG
!    3         VANT   SET
!
         ENDDO
         ASSIGN 400 TO ierr
         msgno = 349
         spag_nextblock_1 = 66
         CYCLE SPAG_DispatchLoop_1
 20      DO WHILE ( mode>0 )
            iwrd = iwrd + 2
            mode = mode - 1
         ENDDO
         spag_nextblock_1 = 5
      CASE (5)
         DO WHILE ( core(iwrd)<0 )
            IF ( core(iwrd)==-4 ) iwrd = iwrd + 1
            iwrd = iwrd + 2
         ENDDO
         IF ( core(iwrd)/=0 .AND. core(iwrd)/=eor ) THEN
            mode = core(iwrd)
            iwrd = iwrd + 1
            GOTO 20
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
         Icont = 0
         IF ( core(iwrd)==0 ) Icont = 1
         spag_nextblock_1 = 69
      CASE (7)
!
!     BRANCH TO PLOT OR PLOTTER
!
         iword = core(iwrd+1)
         IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
         IF ( iword==ter ) THEN
!
!     PLOTTER CARD
!
            iword = core(iwrd+2)
            IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
            IF ( iword/=nast(1) .AND. iword/=nast(2) ) GOTO 20
            ASSIGN 420 TO ierr
            msgno = 350
            spag_nextblock_1 = 67
         ELSE
            isplot = 1
!
!     PLOT COMMAND CARD
!
            iwrd = iwrd + 2
            mode = mode - 1
            GOTO 260
         ENDIF
      CASE (8)
!
!     PROJECTION CARD
!
         iwrd = iwrd + 2
         mode = mode - 1
         iword = core(iwrd)
         IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
         IF ( iword/=proj ) THEN
            ASSIGN 40 TO irtn
            iprm = proj
            spag_nextblock_1 = 56
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 40      iwrd = iwrd + 2
         mode = mode - 1
         IF ( mode<=0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
      CASE (9)
         SPAG_Loop_1_1: DO
!
!     AXES CARD
!
            iwrd = iwrd + 2
            mode = mode - 1
            iword = core(iwrd)
            IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
            IF ( core(iwrd)/=allon .AND. iword/=blank ) THEN
               DO j = 1 , 3
                  flag(j) = .FALSE.
               ENDDO
               i = 0
               EXIT SPAG_Loop_1_1
            ENDIF
         ENDDO SPAG_Loop_1_1
         DO
            IF ( core(iwrd)/=allon .AND. iword/=blank ) THEN
               DO j = 1 , 3
                  IF ( iword==axes(j) .OR. iword==maxes(j) ) flag(j) = .TRUE.
               ENDDO
               i = i + 1
            ENDIF
            iwrd = iwrd + 2
            CALL anullsub(iwrd)
            mode = mode - 1
            IF ( i<3 ) THEN
               iword = core(iwrd)
               IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
            ELSE
!
               ASSIGN 20 TO irtn
               IF ( .NOT.flag(1) .OR. .NOT.flag(2) .OR. .NOT.flag(3) ) THEN
                  spag_nextblock_1 = 58
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               DO
                  iword = core(iwrd)
                  IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
                  IF ( iword==symm .OR. iword==anti ) THEN
                     iwrd = iwrd + 2
                     mode = mode - 1
                     IF ( mode<=0 ) THEN
                        spag_nextblock_1 = 5
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     IF ( core(iwrd)==0 .OR. core(iwrd)==eor ) THEN
                        spag_nextblock_1 = 6
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( core(iwrd)/=allon .AND. iword/=blank ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     iwrd = iwrd + 2
                     mode = mode - 1
                     IF ( mode<=0 ) THEN
                        spag_nextblock_1 = 59
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
         spag_nextblock_1 = 10
      CASE (10)
!
!     VIEW COMMAND
!
         nreal = 3
         nopt = 0
         spag_nextblock_1 = 23
      CASE (11)
!
!     MAXIMUM DEFORMATION CARD
!
         nreal = 1
         nopt = 0
         iword = core(iwrd+2)
         IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
         IF ( iword==defo ) THEN
            spag_nextblock_1 = 23
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ASSIGN 20 TO irtn
         iprm = core(iwrd+2)
         spag_nextblock_1 = 57
      CASE (12)
!
!     CSCALE CARD
!
         ASSIGN 20 TO irtn
         DO
            iwrd = iwrd + 2
            mode = mode - 1
            IF ( mode<=0 ) THEN
               IF ( core(iwrd)+1<0 ) THEN
!
                  nreal = 1
                  nopt = 0
                  spag_nextblock_1 = 25
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( core(iwrd)+1==0 ) THEN
                  WRITE (nout,99001)
99001             FORMAT (/5X,'REAL VALUE, NOT INTEGER, IS NOW USED FOR CSCALE')
               ENDIF
               spag_nextblock_1 = 61
               CYCLE SPAG_DispatchLoop_1
            ELSE
               iword = core(iwrd)
               IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
               IF ( core(iwrd)/=allon .AND. iword/=blank ) THEN
                  spag_nextblock_1 = 59
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDDO
         spag_nextblock_1 = 13
      CASE (13)
!
!     FIND COMMAND
!
         iwrd = iwrd + 2
         mode = mode - 1
 60      SPAG_Loop_1_2: DO WHILE ( core(iwrd)/=0 .AND. core(iwrd)/=eor )
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  ASSIGN 80 TO irtn
                  IF ( mode<=0 ) THEN
                     spag_nextblock_1 = 59
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  iword = core(iwrd)
                  IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
                  IF ( core(iwrd)==allon .OR. iword==blank ) THEN
                     spag_nextblock_1 = 13
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  DO i = 17 , 21
                     itype = i - 16
                     IF ( iword==ctype(i) ) THEN
                        IF ( itype==1 ) THEN
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                        IF ( itype==2 .OR. itype==4 ) THEN
                           spag_nextblock_2 = 3
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                        IF ( itype==3 ) THEN
                           spag_nextblock_1 = 14
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        IF ( itype==5 ) THEN
                           spag_nextblock_1 = 15
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                     ENDIF
!                SCAL  ORIG  VANT   SET  REGI
!
                  ENDDO
                  iprm = core(iwrd)
                  spag_nextblock_1 = 57
                  CYCLE SPAG_DispatchLoop_1
               CASE (2)
!
                  nreal = 1
                  DO
                     iwrd = iwrd + 2
                     mode = mode - 1
                     IF ( mode<=0 ) THEN
                        IF ( core(iwrd)==0 .OR. core(iwrd)==eor ) EXIT SPAG_Loop_1_2
                        spag_nextblock_1 = 16
                        CYCLE SPAG_DispatchLoop_1
                     ELSE
                        iword = core(iwrd)
                        IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
                        IF ( core(iwrd)/=allon .AND. iword/=blank ) EXIT SPAG_DispatchLoop_2
                     ENDIF
                  ENDDO
                  spag_nextblock_2 = 3
               CASE (3)
!
                  iprm = core(iwrd)
                  ASSIGN 60 TO irtn
                  DO
                     iwrd = iwrd + 2
                     mode = mode - 1
                     IF ( mode<=0 ) THEN
                        integ = 1
                        IF ( core(iwrd)==eor ) THEN
                           spag_nextblock_1 = 60
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        IF ( core(iwrd)==-1 ) integ = 0
                        IF ( core(iwrd)==-4 ) iwrd = iwrd + 1
                        IF ( itype==2 ) THEN
                           forg = core(iwrd+1)
                           Org = Org + 1
                           origin(Org) = forg
                        ENDIF
                        iwrd = iwrd + 2
                        IF ( Porg<0 ) THEN
                           Porg = 0
                           porg1 = forg
                        ENDIF
                        spag_nextblock_1 = 17
                        CYCLE SPAG_DispatchLoop_1
                     ELSE
                        iword = core(iwrd)
                        IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
                        IF ( core(iwrd)/=allon .AND. iword/=blank ) THEN
                           spag_nextblock_1 = 60
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                     ENDIF
                  ENDDO
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO SPAG_Loop_1_2
         spag_nextblock_1 = 19
      CASE (14)
!
         iwrd = iwrd + 2
         mode = mode - 1
         ASSIGN 80 TO irtn
         IF ( mode<=0 ) THEN
            iprm = poin
            spag_nextblock_1 = 56
         ELSE
            iword = core(iwrd)
            IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
            IF ( iword==poin ) THEN
               spag_nextblock_1 = 13
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            iprm = core(iwrd)
            spag_nextblock_1 = 57
         ENDIF
      CASE (15)
!
         nreal = 4
         SPAG_Loop_1_3: DO
            iwrd = iwrd + 2
            mode = mode - 1
            IF ( mode<=0 ) EXIT SPAG_Loop_1_3
            iword = core(iwrd)
            IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
            IF ( core(iwrd)/=allon .AND. iword/=blank ) THEN
               spag_nextblock_1 = 62
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO SPAG_Loop_1_3
         spag_nextblock_1 = 16
      CASE (16)
         integ = 0
         ASSIGN 60 TO irtn
         DO i = 1 , nreal
            IF ( core(iwrd)==-1 ) integ = 1
            IF ( core(iwrd)==-4 ) iwrd = iwrd + 1
            iwrd = iwrd + 2
         ENDDO
         spag_nextblock_1 = 17
      CASE (17)
         IF ( integ>0 ) THEN
            spag_nextblock_1 = 61
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 18
      CASE (18)
         IF ( core(iwrd)==0 .OR. core(iwrd)==eor ) THEN
            spag_nextblock_1 = 19
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         mode = core(iwrd)
         iwrd = iwrd + 1
         GOTO 60
!
 80      DO WHILE ( core(iwrd)/=0 .AND. core(iwrd)/=eor )
            iwrd = iwrd + 1
         ENDDO
         spag_nextblock_1 = 19
      CASE (19)
         Icont = 0
         IF ( core(iwrd)==0 ) Icont = 4
         spag_nextblock_1 = 69
      CASE (20)
!
!     CONTOUR
!
         iwrd = iwrd + 2
         mode = mode - 1
         ASSIGN 20 TO irtn
         DO WHILE ( core(iwrd)/=0 .AND. core(iwrd)/=eor )
            iword = core(iwrd)
            IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
            IF ( core(iwrd)==allon .OR. iword==blank ) THEN
               iwrd = iwrd + 2
               mode = mode - 1
               IF ( mode<=0 ) THEN
                  spag_nextblock_1 = 59
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSE
               DO i = 1 , 20
                  IF ( iword==cntur(i) ) GOTO 20
               ENDDO
               iprm = core(iwrd)
               spag_nextblock_1 = 57
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 6
      CASE (21)
!
!     PROJECTION PLANE SEPARATION
!
         iwrd = iwrd + 2
         mode = mode - 1
         ASSIGN 20 TO irtn
         IF ( mode<=0 ) THEN
            iprm = plan
            spag_nextblock_1 = 56
         ELSE
            iword = core(iwrd)
            IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
            IF ( iword/=plan ) THEN
               iprm = core(iwrd)
               spag_nextblock_1 = 57
            ELSE
               iword = core(iwrd+2)
               IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
               IF ( iword==sepa ) THEN
                  nreal = 1
                  nopt = 0
                  spag_nextblock_1 = 23
               ELSE
                  iprm = core(iwrd)
                  spag_nextblock_1 = 57
               ENDIF
            ENDIF
         ENDIF
      CASE (22)
!
!     OCULAR SEPARATION
!
         nreal = 1
         nopt = 0
         iword = core(iwrd+2)
         IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
         IF ( iword/=sepa ) THEN
            ASSIGN 20 TO irtn
            iprm = core(iwrd+2)
            spag_nextblock_1 = 57
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 23
      CASE (23)
         DO
!
            iwrd = iwrd + 2
            mode = mode - 1
            IF ( mode<=0 ) THEN
               spag_nextblock_1 = 25
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 24
      CASE (24)
         SPAG_Loop_1_4: DO
!
!     CAMERA
!
            iwrd = iwrd + 2
            mode = mode - 1
            IF ( mode<=0 ) EXIT SPAG_Loop_1_4
            iword = core(iwrd)
            IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
            IF ( core(iwrd)/=allon .AND. iword/=blank ) THEN
               IF ( core(iwrd)==eor .OR. core(iwrd)==0 ) THEN
                  spag_nextblock_1 = 63
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               DO i = 1 , 4
                  IF ( iword==camera(i) ) GOTO 85
               ENDDO
               iprm = core(iwrd)
               ASSIGN 20 TO irtn
               spag_nextblock_1 = 57
               CYCLE SPAG_DispatchLoop_1
 85            DO
                  iwrd = iwrd + 2
                  mode = mode - 1
                  IF ( mode<=0 ) EXIT SPAG_Loop_1_4
                  iword = core(iwrd)
                  IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
                  IF ( core(iwrd)/=allon .AND. iword/=blank ) THEN
                     i = i + 1
                     IF ( iword/=camera(4) .AND. iword/=camera(5) ) THEN
                        ASSIGN 20 TO irtn
                        IF ( i/=4 ) THEN
                           spag_nextblock_1 = 4
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        spag_nextblock_1 = 59
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
         ENDDO SPAG_Loop_1_4
         IF ( core(iwrd)==eor .OR. core(iwrd)==0 ) THEN
            IF ( i<=3 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 63
         ELSE
            ASSIGN 20 TO irtn
            IF ( core(iwrd)+1/=0 ) THEN
               spag_nextblock_1 = 61
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            iwrd = iwrd + 2
            spag_nextblock_1 = 2
         ENDIF
      CASE (25)
!
!     TEST FOR REAL VALUES
!
         iro = 0
         nro = nreal
         spag_nextblock_1 = 26
      CASE (26)
         integ = 0
         ASSIGN 20 TO irtn
         DO i = 1 , nro
            IF ( core(iwrd)>=0 .OR. core(iwrd)<-4 ) THEN
               IF ( iro<=0 ) THEN
                  spag_nextblock_1 = 62
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
            IF ( core(iwrd)==-1 ) integ = 1
            IF ( core(iwrd)==-4 ) iwrd = iwrd + 1
            iwrd = iwrd + 2
         ENDDO
         IF ( integ/=0 ) THEN
            ASSIGN 100 TO irtn
            spag_nextblock_1 = 61
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 100     IF ( core(iwrd)<0 ) THEN
            IF ( iro==1 .OR. nopt==0 ) THEN
               spag_nextblock_1 = 59
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            iro = 1
            nro = nopt
            spag_nextblock_1 = 26
         ELSEIF ( core(iwrd)==0 ) THEN
            spag_nextblock_1 = 6
         ELSE
            spag_nextblock_1 = 3
         ENDIF
      CASE (27)
!
!     SCALE
!
         nreal = 1
         nopt = 1
         spag_nextblock_1 = 23
      CASE (28)
!
!     ORIGIN
!
         nreal = 3
         nopt = 0
         SPAG_Loop_1_5: DO
            iwrd = iwrd + 2
            mode = mode - 1
            IF ( mode<=0 ) EXIT SPAG_Loop_1_5
            iword = core(iwrd)
            IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
            IF ( core(iwrd)/=allon .AND. iword/=blank ) EXIT SPAG_Loop_1_5
         ENDDO SPAG_Loop_1_5
         IF ( core(iwrd)==-1 ) THEN
            IF ( core(iwrd)==-4 ) iwrd = iwrd + 1
            iwrd = iwrd + 2
            ASSIGN 20 TO irtn
            IF ( core(iwrd)==eor ) THEN
               spag_nextblock_1 = 62
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( core(iwrd)<0 ) THEN
               spag_nextblock_1 = 25
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            mode = core(iwrd)
            iwrd = iwrd + 1
            iword = core(iwrd)
            IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
            IF ( core(iwrd)==allon .OR. iword==blank ) THEN
               spag_nextblock_1 = 23
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 59
         ELSE
            iprm = ctype(18)
            ASSIGN 20 TO irtn
            spag_nextblock_1 = 60
         ENDIF
      CASE (29)
!
!     VANTAGE POINT
!
         nreal = 3
         nopt = 1
         iword = core(iwrd+2)
         IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
         IF ( iword==poin ) THEN
            spag_nextblock_1 = 23
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ASSIGN 20 TO irtn
         iprm = core(iwrd+2)
         spag_nextblock_1 = 57
      CASE (30)
!
!     SET DEFINITION CARD
!
         nint = 0
         nthru = 0
         DO
            iwrd = iwrd + 2
            mode = mode - 1
            IF ( mode<=0 ) THEN
               IF ( core(iwrd)==-1 ) GOTO 140
               ASSIGN 120 TO irtn
               spag_nextblock_1 = 61
               CYCLE SPAG_DispatchLoop_1
            ELSE
               iword = core(iwrd)
               IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
               IF ( core(iwrd)/=allon .AND. iword/=blank ) THEN
                  iprm = ctype(20)
                  ASSIGN 160 TO irtn
                  spag_nextblock_1 = 60
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDDO
 120     IF ( core(iwrd)==-4 ) iwrd = iwrd + 1
!
 140     iwrd = iwrd + 2
         nreal = 0
         spag_nextblock_1 = 31
      CASE (31)
         IF ( core(iwrd)<0 ) THEN
            nint = nint + 1
            IF ( core(iwrd)==-1 .OR. nreal/=0 ) GOTO 140
            ASSIGN 140 TO irtn
            spag_nextblock_1 = 61
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( core(iwrd)/=0 ) THEN
            spag_nextblock_1 = 33
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 32
      CASE (32)
         Icont = 2
         nthru = 0
         spag_nextblock_1 = 69
      CASE (33)
         IF ( core(iwrd)/=eor ) THEN
            mode = core(iwrd)
            iwrd = iwrd + 1
         ELSE
            Icont = 0
            spag_nextblock_1 = 69
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 160     SPAG_Loop_1_6: DO
            iword = core(iwrd)
            IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
            IF ( core(iwrd)/=allon .AND. iword/=blank ) THEN
               iword = core(iwrd)
               IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
               IF ( iword/=thru ) GOTO 200
               nthru = nthru + 1
               IF ( core(iwrd-3)==-1 .AND. core(iwrd+2)==-1 ) THEN
                  IF ( nthru==1 ) EXIT SPAG_Loop_1_6
                  IF ( nint>=2 .AND. core(iwrd-2)>core(iwrd-4) ) EXIT SPAG_Loop_1_6
                  ASSIGN 180 TO irtn
                  ASSIGN 620 TO ierr
                  msgno = 359
                  spag_nextblock_1 = 66
               ELSE
                  ASSIGN 180 TO irtn
                  nreal = 1
                  spag_nextblock_1 = 64
               ENDIF
               CYCLE SPAG_DispatchLoop_1
            ELSE
               iwrd = iwrd + 2
               mode = mode - 1
               IF ( mode<=0 ) THEN
                  spag_nextblock_1 = 31
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_1_6
 180     nint = 0
         iwrd = iwrd + 2
         mode = mode - 1
         IF ( mode<=0 ) THEN
            spag_nextblock_1 = 31
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 200     IF ( core(iwrd)==0 ) THEN
            spag_nextblock_1 = 32
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( core(iwrd)==eor ) THEN
            spag_nextblock_1 = 33
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         iword = core(iwrd)
         IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
         IF ( core(iwrd)/=allon .AND. iword/=blank ) THEN
            DO i = 1 , 33
               IF ( iword==setpr(i) ) THEN
                  IF ( i==1 .OR. i==2 .OR. i==3 .OR. i==4 .OR. i==6 .OR. i==7 .OR. i==9 .OR. i==10 .OR. i==11 .OR. i==15 .OR.       &
                     & i==17 .OR. i==19 .OR. i==20 .OR. i==22 .OR. i==23 .OR. i==25 .OR. i==27 .OR. i==28 .OR. i==29 .OR. i==30 .OR.&
                     & i==31 .OR. i==32 .OR. i==33 ) GOTO 220
                  IF ( i==5 ) THEN
                     spag_nextblock_1 = 34
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( i==8 ) THEN
                     spag_nextblock_1 = 35
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( i==12 .OR. i==18 ) THEN
                     spag_nextblock_1 = 36
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( i==13 ) THEN
                     spag_nextblock_1 = 37
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( i==14 ) THEN
                     spag_nextblock_1 = 38
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( i==16 ) THEN
                     spag_nextblock_1 = 39
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( i==21 ) THEN
                     spag_nextblock_1 = 40
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( i==24 ) THEN
                     spag_nextblock_1 = 41
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( i==26 ) THEN
                     spag_nextblock_1 = 42
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
!
!    1          INCL  EXCL  EXCE  ELEM  GRID   ALL  AERO  AXIF   BAR
!    2          CONE  CONR  HEXA  FLUI  IHEX  PLOT  QDME  QDPL  QUAD
!    3           ROD  SHEA  SLOT  TETR  TORD  TRAP  TRBS  TRIA  TRME
!    4          TRPL  TUBE  TWIS VISCX  WEDG  HBDY
!
            ENDDO
            ASSIGN 220 TO irtn
            iprm = core(iwrd)
            spag_nextblock_1 = 57
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 220     SPAG_Loop_1_7: DO
            iwrd = iwrd + 2
            mode = mode - 1
            IF ( mode<=0 ) EXIT SPAG_Loop_1_7
            iword = core(iwrd)
            IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
            IF ( core(iwrd)/=allon .AND. iword/=blank ) GOTO 200
         ENDDO SPAG_Loop_1_7
 240     nthru = 0
         spag_nextblock_1 = 31
      CASE (34)
!
         iwrd = iwrd + 2
         mode = mode - 1
         IF ( mode<=0 ) THEN
            ASSIGN 240 TO irtn
            iprm = poin
            spag_nextblock_1 = 56
         ELSE
            iword = core(iwrd)
            IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
            IF ( iword==poin ) GOTO 220
            ASSIGN 200 TO irtn
            iprm = core(iwrd)
            spag_nextblock_1 = 57
         ENDIF
      CASE (35)
!
         istt = 4
         istb = 6
         spag_nextblock_1 = 43
      CASE (36)
!
         istt = 3
         istb = 6
         spag_nextblock_1 = 43
      CASE (37)
!
         istt = 7
         istb = 9
         spag_nextblock_1 = 43
      CASE (38)
!
         istt = 3
         istb = 5
         spag_nextblock_1 = 43
      CASE (39)
!
         istt = 10
         istb = 12
         spag_nextblock_1 = 43
      CASE (40)
!
         istt = 5
         istb = 6
         spag_nextblock_1 = 43
      CASE (41)
!
         istt = 1
         istb = 2
         spag_nextblock_1 = 43
      CASE (42)
!
         istt = 1
         istb = 5
         spag_nextblock_1 = 43
      CASE (43)
!
         iword = core(iwrd+1)
         IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
         DO i = istt , istb
            IF ( iword==setp2(i) ) GOTO 220
         ENDDO
         ASSIGN 220 TO irtn
         iprm = core(iwrd)
         spag_nextblock_1 = 57
         CYCLE SPAG_DispatchLoop_1
 260     IF ( core(iwrd)==0 .OR. core(iwrd)==eor ) GOTO 320
         iword = core(iwrd)
         IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
         IF ( core(iwrd)/=allon .AND. iword/=blank ) THEN
            DO i = 1 , 28
               IF ( iword==pltpr(i) ) THEN
                  IF ( i==1 .OR. i==12 .OR. i==15 .OR. i==16 .OR. i==17 .OR. i==28 ) THEN
                     spag_nextblock_1 = 44
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( i==2 .OR. i==3 .OR. i==4 ) THEN
                     spag_nextblock_1 = 45
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( i==5 .OR. i==6 ) THEN
                     spag_nextblock_1 = 46
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( i==7 .OR. i==11 .OR. i==19 .OR. i==21 .OR. i==24 .OR. i==26 .OR. i==27 ) GOTO 280
                  IF ( i==8 .OR. i==9 ) THEN
                     spag_nextblock_1 = 47
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( i==10 ) THEN
                     spag_nextblock_1 = 48
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( i==13 .OR. i==14 ) THEN
                     spag_nextblock_1 = 49
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( i==18 ) THEN
                     spag_nextblock_1 = 52
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( i==20 ) THEN
                     spag_nextblock_1 = 50
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( i==22 ) THEN
                     spag_nextblock_1 = 55
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( i==23 ) THEN
                     spag_nextblock_1 = 53
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( i==25 ) THEN
                     spag_nextblock_1 = 54
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
!
!    1            SET  STAT  MODA  CMOD  FREQ  TRAN  CONT  RANG  TIME
!    2           PHAS  MAGN  ORIG  SYMM  ANTI   PEN  DENS  SYMB  LABE
!    3           SHAP  VECT  OUTL  THRU  MAXI  HIDD  SHRI  NOFI  FILL
!    4           OFFS
!
            ENDDO
            ASSIGN 280 TO irtn
            iprm = core(iwrd)
            spag_nextblock_1 = 57
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
 280     iwrd = iwrd + 2
         mode = mode - 1
         IF ( mode>0 ) GOTO 260
         GOTO 300
      CASE (44)
!
         iprm = core(iwrd)
         ASSIGN 260 TO irtn
         SPAG_Loop_1_8: DO
            iwrd = iwrd + 2
            mode = mode - 1
            IF ( mode<=0 ) EXIT SPAG_Loop_1_8
            iword = core(iwrd)
            IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
            IF ( core(iwrd)/=allon .AND. iword/=blank ) THEN
               spag_nextblock_1 = 60
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO SPAG_Loop_1_8
!
 300     DO WHILE ( core(iwrd)<0 )
            spag_nextblock_3 = 1
            SPAG_DispatchLoop_3: DO
               SELECT CASE (spag_nextblock_3)
               CASE (1)
                  IF ( i==12 ) THEN
                     Porg = core(iwrd+1)
                     IF ( Org>0 ) THEN
                        DO i = 1 , Org
                           IF ( Porg==origin(i) ) THEN
                              spag_nextblock_3 = 2
                              CYCLE SPAG_DispatchLoop_3
                           ENDIF
                        ENDDO
                     ENDIF
                     ASSIGN 680 TO ierr
                     msgno = 362
                     spag_nextblock_1 = 67
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  spag_nextblock_3 = 2
               CASE (2)
                  iwrd = iwrd + 2
                  EXIT SPAG_DispatchLoop_3
               END SELECT
            ENDDO SPAG_DispatchLoop_3
         ENDDO
!
 320     IF ( core(iwrd)==0 ) THEN
            Icont = 3
         ELSEIF ( core(iwrd)/=eor ) THEN
            mode = core(iwrd)
            iwrd = iwrd + 1
            GOTO 260
         ELSE
            Icont = 0
         ENDIF
         spag_nextblock_1 = 69
      CASE (45)
!
         ipr1 = core(iwrd)
         ipr2 = core(iwrd+1)
         iwrd = iwrd + 2
         mode = mode - 1
         IF ( mode<=0 ) THEN
            iprm = defo
            ASSIGN 300 TO irtn
            spag_nextblock_1 = 56
         ELSE
            iword = core(iwrd)
            IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
            DO i = 1 , 3
!                                     DEFO  VELO  ACCE
               IF ( iword==idvpr(i) ) THEN
                  IF ( i==1 ) GOTO 280
                  IF ( i==2 .OR. i==3 ) THEN
                     spag_nextblock_1 = 65
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
            ENDDO
            ASSIGN 280 TO irtn
            iprm = core(iwrd)
            spag_nextblock_1 = 57
         ENDIF
      CASE (46)
!
         iwrd = iwrd + 2
         mode = mode - 1
         IF ( mode<=0 ) THEN
            ASSIGN 300 TO irtn
            iprm = defo
            spag_nextblock_1 = 56
         ELSE
!
            iword = core(iwrd)
            IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
            DO i = 1 , 3
               IF ( iword==idvpr(i) ) GOTO 280
            ENDDO
            ASSIGN 280 TO irtn
            iprm = core(iwrd)
            spag_nextblock_1 = 57
         ENDIF
      CASE (47)
!
         nreal = 2
         ASSIGN 260 TO irtn
 340     DO
            iwrd = iwrd + 2
            mode = mode - 1
            IF ( mode<=0 ) THEN
               integ = 0
               DO i = 1 , nreal
                  IF ( core(iwrd)>=0 ) GOTO 345
                  IF ( core(iwrd)==-1 ) integ = 1
                  IF ( core(iwrd)==-4 ) iwrd = iwrd + 1
                  iwrd = iwrd + 2
               ENDDO
               IF ( integ<=0 ) GOTO 300
               ASSIGN 300 TO irtn
               spag_nextblock_1 = 61
               CYCLE SPAG_DispatchLoop_1
 345           ASSIGN 320 TO irtn
               spag_nextblock_1 = 62
               CYCLE SPAG_DispatchLoop_1
            ELSE
               iword = core(iwrd)
               IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
               IF ( core(iwrd)/=allon .AND. iword/=blank ) THEN
                  spag_nextblock_1 = 62
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDDO
         spag_nextblock_1 = 48
      CASE (48)
!
         iwrd = iwrd + 2
         mode = mode - 1
         nreal = 1
         IF ( mode<=0 ) THEN
            ASSIGN 300 TO irtn
            iprm = lag
            spag_nextblock_1 = 56
         ELSE
            iword = core(iwrd)
            IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
            IF ( iword==lag ) GOTO 340
            ASSIGN 340 TO irtn
            iprm = core(iwrd)
            spag_nextblock_1 = 57
         ENDIF
      CASE (49)
!
         ncrd = 9
         icrd = 1
         ivc = 0
         spag_nextblock_1 = 51
      CASE (50)
         ncrd = 25
         icrd = 4
         ivc = 1
         spag_nextblock_1 = 51
      CASE (51)
         ASSIGN 300 TO irtn
         iax = 0
         DO
            iwrd = iwrd + 2
            mode = mode - 1
            IF ( mode<=0 ) THEN
               spag_nextblock_1 = 58
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            SPAG_Loop_2_9: DO
               spag_nextblock_4 = 1
               SPAG_DispatchLoop_4: DO
                  SELECT CASE (spag_nextblock_4)
                  CASE (1)
                     iword = core(iwrd)
                     IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
                     IF ( core(iwrd)==allon .OR. iword==blank ) EXIT SPAG_Loop_2_9
                     DO i = icrd , ncrd
                        IF ( iword==coord(i) ) THEN
                           spag_nextblock_4 = 2
                           CYCLE SPAG_DispatchLoop_4
                        ENDIF
                     ENDDO
                     IF ( iax<=0 ) THEN
                        spag_nextblock_1 = 58
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     GOTO 260
                  CASE (2)
                     iwrd = iwrd + 2
                     mode = mode - 1
                     IF ( mode<=0 ) GOTO 320
                     IF ( ivc>0 ) GOTO 260
                     IF ( iax>0 ) GOTO 260
                     iax = 1
                     EXIT SPAG_DispatchLoop_4
                  END SELECT
               ENDDO SPAG_DispatchLoop_4
            ENDDO SPAG_Loop_2_9
         ENDDO
         spag_nextblock_1 = 52
      CASE (52)
         SPAG_Loop_1_10: DO
!
            iwrd = iwrd + 2
            mode = mode - 1
            IF ( mode<=0 ) GOTO 300
            iword = core(iwrd)
            IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
            IF ( core(iwrd)/=allon .AND. iword/=blank ) THEN
               DO i = 1 , 5
                  IF ( iword==lblpr(i) ) THEN
                     IF ( i==1 ) CYCLE SPAG_Loop_1_10
                     IF ( i==2 .OR. i==3 .OR. i==4 .OR. i==5 ) GOTO 280
                  ENDIF
!                                     GRID  POIN  ELEM  BOTH  EPID
               ENDDO
               GOTO 260
            ENDIF
         ENDDO SPAG_Loop_1_10
         spag_nextblock_1 = 53
      CASE (53)
!
         iwrd = iwrd + 2
         mode = mode - 1
         IF ( mode<=0 ) THEN
            ASSIGN 300 TO irtn
            spag_nextblock_1 = 59
            CYCLE SPAG_DispatchLoop_1
         ELSE
            iword = core(iwrd)
            IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
            IF ( iword/=defo ) THEN
               ASSIGN 360 TO irtn
               iprm = core(iwrd)
               spag_nextblock_1 = 57
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
 360     nreal = 1
         GOTO 340
      CASE (54)
!
         iwrd = iwrd + 2
         iword = core(iwrd)
         IF ( bit64 ) CALL mvbits(blank,0,32,iword,0)
         IF ( iword==hidd ) GOTO 280
         mode = mode - 1
         IF ( mode>=0 ) GOTO 300
         ASSIGN 300 TO irtn
         spag_nextblock_1 = 59
      CASE (55)
!
         IF ( core(iwrd-3)==-1 .AND. core(iwrd+2)==-1 ) GOTO 280
         ASSIGN 280 TO irtn
         spag_nextblock_1 = 64
      CASE (56)
         ASSIGN 440 TO ierr
         msgno = 351
         spag_nextblock_1 = 66
      CASE (57)
         ASSIGN 460 TO ierr
         msgno = 351
         spag_nextblock_1 = 66
      CASE (58)
         ASSIGN 480 TO ierr
         msgno = 352
         spag_nextblock_1 = 66
      CASE (59)
         ASSIGN 500 TO ierr
         msgno = 353
         spag_nextblock_1 = 66
      CASE (60)
         ASSIGN 520 TO ierr
         msgno = 354
         spag_nextblock_1 = 67
      CASE (61)
         ASSIGN 540 TO ierr
         msgno = 355
         spag_nextblock_1 = 66
      CASE (62)
         ASSIGN 560 TO ierr
         msgno = 356
         spag_nextblock_1 = 66
      CASE (63)
         ASSIGN 580 TO ierr
         msgno = 357
         spag_nextblock_1 = 67
      CASE (64)
         ASSIGN 600 TO ierr
         msgno = 358
         spag_nextblock_1 = 66
      CASE (65)
         ASSIGN 640 TO ierr
         msgno = 360
         spag_nextblock_1 = 66
      CASE (66)
!
         CALL page2(2)
         WRITE (nout,99002) ufm , msgno
99002    FORMAT (A23,I4)
         IF ( pltopt<=2 ) nogo = 1
         spag_nextblock_1 = 68
      CASE (67)
         CALL page2(2)
         WRITE (nout,99003) uwm , msgno
99003    FORMAT (A25,I4)
         spag_nextblock_1 = 68
      CASE (68)
!
         GOTO ierr
!
 380     WRITE (nout,99004)
99004    FORMAT (5X,'FIRST CHARACTER ON CARD IS NUMERIC. INCORRECT FORMAT',' OR INCORRECT CONTINUATION ON PREVIOUS CARD')
         GOTO 20
!
 400     WRITE (nout,99005) core(iwrd)
99005    FORMAT (5X,'PLOT COMMAND ',A4,' NOT RECOGNIZED.  CHECK SPELLING ',                                                         &
                &'AND FORMAT ON THIS CARD AND CONTINUATION ON PREVIOUS ONE')
         GOTO 20
 420     WRITE (nout,99006)
99006    FORMAT (1H+,30X,' - ONLY NASTRAN GENERAL PURPOSE PLOTTER IS ','SUPPORTED')
         GOTO 20
!
 440     WRITE (nout,99007) iprm
99007    FORMAT (1H+,30X,' - KEYWORD ',A4,' NOT FOUND')
         GOTO irtn
!
 460     WRITE (nout,99008) iprm
99008    FORMAT (1H+,30X,' - KEYWORD ',A4,' NOT RECOGNIZED')
         GOTO irtn
!
 480     WRITE (nout,99009)
99009    FORMAT (1H+,30X,' - COORDINATE AXES INCORRECTLY DEFINED')
         GOTO irtn
!
 500     WRITE (nout,99010)
99010    FORMAT (1H+,30X,' - INCORRECT FORMAT')
         GOTO irtn
!
 520     WRITE (nout,99011) iprm
99011    FORMAT (1H+,30X,3H - ,A4,' IDENTIFICATION NUMBER NOT DEFINED')
         GOTO irtn
!
 540     WRITE (nout,99012)
99012    FORMAT (1H+,30X,' - DATA TYPE IS INCORRECT')
         GOTO irtn
!
 560     WRITE (nout,99013)
99013    FORMAT (1H+,30X,' - ONE OR MORE REQUIRED REAL VALUES MISSING')
         GOTO irtn
!
 580     WRITE (nout,99014)
99014    FORMAT (1H+,30X,' - CAMERA OPTION NOT SPECIFIED')
         GOTO 20
!
 600     WRITE (nout,99015)
99015    FORMAT (1H+,30X,' - THRU MUST BE PRECEDED AND FOLLOWED BY INTEGER',' VALUES')
         GOTO irtn
!
 620     WRITE (nout,99016)
99016    FORMAT (1H+,30X,' - THRU RANGE OVERLAPS RANGE OF PREVIOUS THRU')
         GOTO 180
!
 640     WRITE (nout,99017) ipr1 , ipr2
99017    FORMAT (1H+,30X,' - ONLY DEFORMATION VALID WITH ',2A4)
         GOTO 280
!
 660     WRITE (nout,99018) forg , Porg
99018    FORMAT (1H+,30X,' - A NEW ORIGIN',I8,' WAS DEFINED IN A FIND ','CARD, BUT IT IS NOT USED BY THE IMMEDIATE PLOT CARD',/5X,  &
                &'(ORIGIN',I8,' WILL BE USED FOR THIS PLOT)',/)
         spag_nextblock_1 = 70
         CYCLE SPAG_DispatchLoop_1
!
 680     WRITE (nout,99019) Porg
99019    FORMAT (1H+,30X,' - ORIGIN',I8,' IS UNDEFINED')
         GOTO 280
      CASE (69)
!
         IF ( isplot==0 .OR. Porg==-1 ) RETURN
         IF ( Porg==0 ) Porg = porg1
         IF ( forg/=0 .AND. forg/=Porg ) THEN
            ASSIGN 660 TO ierr
            msgno = 361
            spag_nextblock_1 = 67
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 70
      CASE (70)
         forg = 0
         Porg = 0
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ifp1pc
