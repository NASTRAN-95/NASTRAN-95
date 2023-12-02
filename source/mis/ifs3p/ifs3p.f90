!*==ifs3p.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifs3p() !HIDESTARS (*,*,*)
   IMPLICIT NONE
   USE C_CIFS3P
   USE C_IFPDTA
   USE C_IFPX2
   USE C_IFPX3
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   REAL :: anum , den , factor , term1 , term2 , xin
   INTEGER , SAVE :: arigid , blnk , brigid , crba , crbe , crigid , crtr , drigid , endt , erigid , frigid , ind , irigid , iscr1 ,&
                   & ium , kk , ls , lud , lz , mset , nt1 , thru
   LOGICAL , SAVE :: first , perm , prol , prt
   CHARACTER(19) , SAVE :: gcc , scc
   INTEGER , DIMENSION(6) :: ia , ib , ic , ja , jb , jc
   INTEGER :: ifile , ih , il , ill , im , imid , ip , irg , items , itot1 , itot2 , ixin , iz , j , jrigid , knt1 , kz , kzflag ,  &
            & l , l1 , l2 , l3 , l5 , l6 , l8 , l9 , lb , line , lk , lll , m3 , msegs , nbpw , ncomp , nharms , nnt , nsegs , nt , &
            & r , r1
   INTEGER , DIMENSION(4) , SAVE :: iones
   INTEGER , DIMENSION(2) , SAVE :: nam
   LOGICAL :: nos , noud , rbe
   INTEGER , DIMENSION(92) :: q
   REAL , DIMENSION(50) :: rm
   EXTERNAL close , gopen , ifpdco , mesage , page1 , page2 , read , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!HURNB 11/93
!HURNE
!HURNB 11/93
!HURNE
!HURNB 11/93
!HURNE
   !>>>>EQUIVALENCE (M(1),Rm(1)) , (Line,Idummy(9)) , (Nbpw,Idummy(37))
!HURNB 11/93
   !>>>>EQUIVALENCE (xin,ixin)
!HURNE
   DATA prol , endt/.FALSE. , 4HENDT/ , perm/.FALSE./
   DATA first , prt/2*.TRUE./
   DATA lud , lz , kk , ls/4HUD   , 4HZ    , 4HK    , 4HS   / , nt1/250/
   DATA arigid/4HCRIG/ , brigid/4HD1  / , crigid/4HD2  / , irigid/1/
   DATA drigid/4HD3  / , mset/4HMSET/ , blnk/4H    / , thru/4HTHRU/
   DATA erigid/4H1   / , frigid/4H2   / , ind/4HIN  /
   DATA crtr/4HCRTR/ , crba/4HCRBA/ , crbe/4HCRBE/ , ium/4HUM  /
   DATA scc/'SORTED CARD COUNT ='/ , gcc/'GENERATED CARD    -'/
!HURNB 11/93
   DATA iscr1/301/ , iones/4* - 1/ , nam/4HIFS3 , 4HP   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!HURNE
!
         IF ( K<=100 ) THEN
            IF ( K==1 ) THEN
!
!*******              1-GRID            ********************************
!
               IF ( Mf(2)==0 ) M(2) = Igdst2
               IF ( Mf(6)==0 ) M(6) = Igdst6
               IF ( Mf(7)==0 ) M(7) = Igdst7
               IF ( Mf(8)==0 ) M(8) = Igdst8
               IF ( M(1)<=0 .OR. M(2)<0 .OR. M(6)<-1 ) GOTO 20
               IF ( .NOT.(M(6)>=0 .OR. Grdmsg) ) THEN
                  CALL page2(2)
                  WRITE (Nout,99001) Uwm
99001             FORMAT (A23,' 302, ONE OR MORE GRID CARDS HAVE DISPLACEMENT ','COORDINATE SYSTEM ID OF -1')
                  Grdmsg = .TRUE.
               ENDIF
               IF ( ifpdco(M(7)) ) GOTO 20
               IF ( ifpdco(M(8)) ) GOTO 20
               IF ( Mf(8)/=0 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               N = 8
               GOTO 40
            ELSEIF ( K==2 ) THEN
!
!*******        2-GRDSET       ****************************************
!
               IF ( G1==0 ) GOTO 20
               G1 = 0
               IF ( M(2)==0 .AND. M(6)==0 .AND. M(7)==0 .AND. M(8)==0 ) GOTO 20
               IF ( M(2)<0 .OR. M(6)<-1 .OR. M(7)<0 .OR. M(8)<0 ) GOTO 20
               IF ( ifpdco(M(7)) .OR. ifpdco(M(8)) ) GOTO 20
               IF ( Mf(8)/=0 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               Igdst2 = M(2)
               Igdst6 = M(6)
               Igdst7 = M(7)
               Igdst8 = M(8)
               RETURN 2
            ELSEIF ( K==3 .OR. K==4 .OR. K==5 .OR. K==6 .OR. K==7 .OR. K==8 .OR. K==9 .OR. K==10 .OR. K==11 .OR. K==14 .OR.         &
                   & K==15 .OR. K==16 .OR. K==18 .OR. K==19 .OR. K==20 .OR. K==21 .OR. K==22 .OR. K==23 .OR. K==24 .OR. K==25 .OR.  &
                   & K==26 .OR. K==27 .OR. K==29 .OR. K==30 .OR. K==31 .OR. K==32 .OR. K==33 .OR. K==34 .OR. K==35 .OR. K==36 .OR.  &
                   & K==37 .OR. K==38 .OR. K==39 .OR. K==40 .OR. K==41 .OR. K==42 .OR. K==43 .OR. K==44 .OR. K==45 .OR. K==46 .OR.  &
                   & K==47 .OR. K==48 .OR. K==49 .OR. K==50 .OR. K==51 .OR. K==52 .OR. K==53 .OR. K==54 .OR. K==55 .OR. K==56 .OR.  &
                   & K==57 .OR. K==58 .OR. K==59 .OR. K==60 .OR. K==61 .OR. K==62 .OR. K==63 .OR. K==64 .OR. K==65 .OR. K==66 .OR.  &
                   & K==67 .OR. K==68 .OR. K==69 .OR. K==70 .OR. K==71 .OR. K==72 .OR. K==73 .OR. K==74 .OR. K==75 .OR. K==76 .OR.  &
                   & K==77 .OR. K==78 .OR. K==79 .OR. K==80 .OR. K==81 .OR. K==85 .OR. K==86 .OR. K==87 .OR. K==88 .OR. K==89 .OR.  &
                   & K==90 .OR. K==91 .OR. K==93 .OR. K==94 .OR. K==95 .OR. K==96 .OR. K==97 .OR. K==98 .OR. K==99 .OR. K==100 )    &
                   & THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( K==12 ) THEN
               spag_nextblock_1 = 49
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( K==13 .OR. K==83 ) THEN
!
!******      13-SPCADD, 83-MPCADD    **********************************
!
               IF ( Km==1 ) THEN
!
!***** TEMPORARY UNFIX FOR SPCADD AND MPCADD ***************************
!
                  N = 0
               ELSE
                  Km = 1
                  IF ( M(1)<=0 ) Baddat = .TRUE.
                  Id = M(1)
                  I(1) = Id
                  IF ( M(2)<=0 .OR. M(3)<0 ) Baddat = .TRUE.
                  IF ( M(3)==0 ) CALL page2(2)
                  IF ( M(3)==0 ) WRITE (Nout,99002) Uwm
99002             FORMAT (A25,' 4124, THE SPCADD OR MPCADD UNION CONSISTS OF A ','SINGLE SET.')
                  N = 1
               ENDIF
               DO l = 1 , 8
                  IF ( Mf(l)/=0 .AND. Mf(l)/=1 ) Badfor = .TRUE.
               ENDDO
               DO
                  N = N + 1
                  IF ( M(N)<0 ) THEN
                     Baddat = .TRUE.
                     spag_nextblock_1 = 59
                     CYCLE SPAG_DispatchLoop_1
                  ELSEIF ( M(N)==0 ) THEN
                     spag_nextblock_1 = 59
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     I(N) = M(N)
                     IF ( N>=8 ) THEN
                        spag_nextblock_1 = 60
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDIF
               ENDDO
            ELSEIF ( K==17 ) THEN
!
!******        17-MPC       ******************************************
!
               IF ( M(3)>6 .OR. M(6)>6 ) Baddat = .TRUE.
               IF ( Ithrml==1 ) THEN
                  IF ( M(3)>1 .OR. M(6)>1 ) Baddat = .TRUE.
               ENDIF
               IF ( Km/=0 ) THEN
                  IF ( Mf(1)/=0 .OR. Mf(8)/=0 ) Badfor = .TRUE.
                  ASSIGN 280 TO r
               ELSE
                  Km = 1
                  nt = 0
                  IF ( Mf(1)/=1 .OR. Mf(8)/=0 ) Badfor = .TRUE.
                  ASSIGN 260 TO r
               ENDIF
               DO l = 2 , 7
                  IF ( Mf(l)/=0 ) THEN
                     IF ( l==4 .OR. l==7 ) THEN
                        IF ( Mf(l)/=2 ) Badfor = .TRUE.
                     ELSE
                        IF ( Mf(l)/=1 ) Badfor = .TRUE.
                     ENDIF
                  ENDIF
               ENDDO
               GOTO r
            ELSEIF ( K==28 ) THEN
!
!******         28-GENEL         **************************************
!
               IF ( L0==2 .OR. L0==4 ) THEN
                  l3 = 1
                  N = 0
                  spag_nextblock_1 = 39
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( L0==3 ) THEN
                  L0 = L0 + 1
                  lb = 0
                  IF ( Mf(1)/=3 .OR. (M(1)/=lz .AND. M(1)/=kk) ) THEN
                     l8 = 0
                     IF ( Mf(1)/=3 .OR. Mf(2)/=0 ) Badfor = .TRUE.
                     IF ( M(1)/=lud ) Baddat = .TRUE.
                     l3 = 3
                     noud = .FALSE.
                     DO l = 2 , 8
                        M(l) = M(l+1)
                     ENDDO
                     N = 0
                     spag_nextblock_1 = 39
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     L0 = L0 + 1
                     lb = 2
                     I(1) = -1
                     I(2) = 0
                     spag_nextblock_1 = 42
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ELSEIF ( L0==5 ) THEN
                  IF ( M(1)/=lz .AND. M(1)/=kk ) Baddat = .TRUE.
                  spag_nextblock_1 = 42
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( L0==6 .OR. L0==8 ) THEN
                  l3 = 1
                  lb = 0
                  spag_nextblock_1 = 44
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( L0==7 ) THEN
                  IF ( M(1)/=ls ) Baddat = .TRUE.
                  l9 = l6*L7
                  lb = 1
                  I(1) = L7
                  nos = .FALSE.
                  spag_nextblock_1 = 43
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  L0 = L0 + 1
                  kzflag = 0
                  l8 = 0
                  noud = .TRUE.
                  nos = .TRUE.
                  IF ( Mf(1)/=1 .OR. Mf(2)/=0 ) Badfor = .TRUE.
                  IF ( M(1)<=0 ) Baddat = .TRUE.
                  Id = M(1)
                  I(1) = Id
                  N = 1
                  l3 = 3
                  spag_nextblock_1 = 39
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( K==82 ) THEN
!
!******         82-PARAM         ***********************************
!
               IF ( Mf(1)/=3 .OR. Mf(2)<=0 .OR. Mf(3)/=0 .AND. Mf(3)/=Mf(2) ) THEN
                  spag_nextblock_1 = 48
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( Mf(3)/=0 .AND. Mf(3)/=2 .AND. Mf(3)/=4 ) THEN
                  spag_nextblock_1 = 48
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               DO l = 4 , 8
                  IF ( Mf(l)/=0 ) THEN
                     spag_nextblock_1 = 48
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
               IF ( Nparam+7<=Nopen ) THEN
                  ip = 2*Nbuf + Nparam
                  Ibuff(ip+1) = M(1)
                  Ibuff(ip+2) = M(2)
                  Ibuff(ip+3) = Mf(2)
                  Ibuff(ip+4) = M(3)
                  Nparam = Nparam + 4
                  IF ( Mf(2)>2 .OR. Mf(3)/=0 ) THEN
                     Ibuff(ip+5) = M(4)
                     Nparam = Nparam + 1
                     IF ( Mf(2)>4 .OR. Mf(3)/=0 ) THEN
                        IF ( Mf(3)==4 ) THEN
                           Ibuff(ip+3) = 6
                           Ibuff(ip+6) = M(5)
                           Ibuff(ip+7) = M(6)
                           Nparam = Nparam + 2
                        ELSE
                           Ibuff(ip+3) = 5
                        ENDIF
                     ENDIF
                  ENDIF
               ELSE
                  CALL page2(2)
                  WRITE (Nout,99003) Sfm
99003             FORMAT (A25,' 330, NO ROOM IN CORE FOR PARAM CARDS.')
                  Abort = .TRUE.
               ENDIF
               RETURN
            ELSEIF ( K==84 ) THEN
               spag_nextblock_1 = 56
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( K==92 ) THEN
               spag_nextblock_1 = 50
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         IF ( Kx<=100 ) THEN
            IF ( Kx==1 .OR. Kx==2 .OR. Kx==3 .OR. Kx==4 .OR. Kx==5 .OR. Kx==6 .OR. Kx==7 .OR. Kx==8 .OR. Kx==9 .OR. Kx==10 .OR.     &
               & Kx==11 .OR. Kx==12 .OR. Kx==13 .OR. Kx==14 .OR. Kx==15 .OR. Kx==16 .OR. Kx==17 .OR. Kx==18 .OR. Kx==19 .OR.        &
               & Kx==20 .OR. Kx==21 .OR. Kx==22 .OR. Kx==24 .OR. Kx==25 .OR. Kx==27 .OR. Kx==28 .OR. Kx==29 .OR. Kx==30 .OR.        &
               & Kx==33 .OR. Kx==34 .OR. Kx==35 .OR. Kx==36 .OR. Kx==37 .OR. Kx==40 .OR. Kx==41 .OR. Kx==42 .OR. Kx==58 .OR.        &
               & Kx==59 .OR. Kx==60 .OR. Kx==61 .OR. Kx==62 .OR. Kx==63 .OR. Kx==64 .OR. Kx==65 .OR. Kx==66 .OR. Kx==67 .OR.        &
               & Kx==68 .OR. Kx==69 .OR. Kx==70 .OR. Kx==71 .OR. Kx==72 .OR. Kx==73 .OR. Kx==74 .OR. Kx==75 .OR. Kx==76 .OR.        &
               & Kx==77 .OR. Kx==78 .OR. Kx==79 .OR. Kx==80 .OR. Kx==81 .OR. Kx==86 .OR. Kx==87 .OR. Kx==88 .OR. Kx==89 .OR.        &
               & Kx==90 .OR. Kx==91 .OR. Kx==92 .OR. Kx==93 .OR. Kx==94 .OR. Kx==95 .OR. Kx==96 .OR. Kx==97 .OR. Kx==98 .OR.        &
               & Kx==99 .OR. Kx==100 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Kx==23 ) THEN
               spag_nextblock_1 = 56
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Kx==26 ) THEN
!
!*****         126-FREQ       ******************************************
!
               IF ( Idfreq ) Iddsf = 0
               Idfreq = .FALSE.
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Kx==31 .OR. Kx==32 ) THEN
!
!******     131-RLOAD1, 132-RLOAD2    **********************************
!
               IF ( M(5)==0 .AND. M(6)==0 ) GOTO 20
               IF ( M(1)<=0 .OR. M(2)<=0 .OR. M(3)<0 .OR. M(4)<0 ) GOTO 20
               IF ( M(5)<0 .OR. M(6)<0 ) GOTO 20
               N = 6
               GOTO 40
            ELSEIF ( Kx==38 ) THEN
!
!*******       138-TLOAD1      *****************************************
!
               IF ( M(1)<=0 .OR. M(2)<=0 .OR. M(3)<0 .OR. M(5)<=0 ) GOTO 20
               IF ( M(4)<0 .OR. M(4)>4 ) GOTO 20
               N = 5
               GOTO 40
            ELSEIF ( Kx==39 ) THEN
!
!*******       139-TLOAD2      *****************************************
!
               IF ( M(1)<=0 .OR. M(2)<=0 .OR. M(3)<0 ) GOTO 20
               IF ( rm(5)<0. .OR. rm(6)<=rm(5) .OR. rm(7)<0. ) GOTO 20
               IF ( M(4)<0 .OR. M(4)>4 ) GOTO 20
               N = 10
               GOTO 40
            ELSEIF ( Kx==43 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Kx==44 ) THEN
!
!******        144-AXIC           **************************************
!
               IF ( Iax ) THEN
                  CALL page2(2)
                  WRITE (Nout,99004) Ufm
99004             FORMAT (A23,' 329, ONLY ONE(1) AXIC CARD ALLOWED.')
                  Abort = .TRUE.
                  RETURN
               ELSE
                  Iax = .TRUE.
                  Nn = 998
                  DO l = 1 , nt1
                     IF ( T4(1,l)>0 ) T3(1,l) = T3(1,K)
                  ENDDO
!HURD2 11/93
!     IF (M(1).LT.0 .OR. M(1).GT.998 .OR. M(2).NE.0) GO TO 8
!     NN = M(1)
!HURNB 11/93
!
! M.LT.0 CHECK IS REMOVED TO ALLOW FOR SINGLE HARMONIC
!
!     IF(M(1).LT.0.OR.M(1).GT.998.OR.M(2).NE.0)GO TO 8
                  IF ( M(1)>998 .OR. M(2)/=0 ) GOTO 20
                  Nns = M(1)
                  Nn = iabs(M(1))
                  Oneh = .FALSE.
                  IF ( Nns<0 ) Oneh = .TRUE.
!HURNE
                  N = 2
                  IF ( Nn>15 .AND. nbpw<=32 ) THEN
                     WRITE (Nout,99005) Uwm
99005                FORMAT (A25,', POTENTIAL SYSTEM FATAL ERROR DUE TO LARGE HARMONIC',' (LARGER THAN 15) ON 32-BIT WORD MACHINE')
                  ENDIF
                  GOTO 40
               ENDIF
            ELSEIF ( Kx==45 ) THEN
!  OR GO TO 1447
!
!******        145-RINGAX         **************************************
!
               IF ( M(1)<=0 .OR. rm(3)<=0. ) GOTO 20
               ih = Nn
               ASSIGN 60 TO r
               ASSIGN 20 TO r1
               spag_nextblock_1 = 58
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Kx==46 ) THEN
!
!******        146-CCONEAX        **************************************
!
               IF ( M(1)<=0 .OR. M(3)<=0 .OR. M(4)<=0 ) GOTO 20
               IF ( Mf(2)==0 ) M(2) = M(1)
               IF ( M(2)<=0 .OR. M(4)==M(3) ) GOTO 20
               ih = Nn
               ASSIGN 80 TO r
               ASSIGN 20 TO r1
               spag_nextblock_1 = 58
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Kx==47 ) THEN
!
!******        147-PCONEAX        **************************************
!
               IF ( M(1)<=0 ) GOTO 20
               IF ( M(2)==0 .AND. M(3)/=0 .OR. M(2)<0 ) GOTO 20
               IF ( M(4)==0 .AND. M(5)/=0 .OR. M(4)<0 ) GOTO 20
               IF ( M(6)==0 .AND. M(7)/=0 .OR. M(6)<0 ) GOTO 20
               IF ( M(2)/=0 .AND. M(3)==0 ) GOTO 20
               IF ( M(6)/=0 .AND. M(7)==0 ) GOTO 20
               ih = Nn
               ASSIGN 100 TO r
               ASSIGN 20 TO r1
               spag_nextblock_1 = 58
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Kx==48 ) THEN
!
!******        148-SPCAX       *****************************************
!
               IF ( M(1)<=0 .OR. M(2)<=0 .OR. M(3)<0 ) GOTO 20
               IF ( ifpdco(M(4)) ) GOTO 20
!HURNB 11/93
               IF ( Mf(3)==0 ) THEN
!HURNB 11/93
!
! HID IS BLANK - GENERATE HID FOR THIS SPCAX FOR ALL HARMONICS
!
                  nharms = Nns + 1
                  IF ( Oneh ) nharms = 1
                  DO il = 1 , nharms
                     N = N + 5
                     I(N-4) = M(1)
                     I(N-3) = M(2)
                     I(N-1) = M(4)
                     I(N) = M(5)
                     I(N-2) = il - 1
                     IF ( Oneh ) I(N-2) = Nn
                  ENDDO
                  RETURN
               ELSE
!HURNE
                  ASSIGN 20 TO r1
                  ASSIGN 120 TO r
                  ih = M(3)
                  spag_nextblock_1 = 58
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( Kx==49 ) THEN
!HURNE
!
!******        149-MPCAX       *****************************************
!
               IF ( M(7)>6 ) Baddat = .TRUE.
               IF ( Ithrml==1 .AND. M(7)>1 ) Baddat = .TRUE.
               IF ( Km/=0 ) THEN
                  l1 = 1
                  IF ( M(3)>6 ) Baddat = .TRUE.
                  IF ( Ithrml==1 .AND. M(3)>1 ) Baddat = .TRUE.
                  ASSIGN 160 TO r
               ELSE
                  Km = 1
                  nt = 0
!HURNB 11/93
                  Blankh = .FALSE.
!HURNE
                  IF ( Mf(1)/=1 .OR. Mf(2)/=0 .OR. Mf(3)/=0 .OR. Mf(4)/=0 ) Badfor = .TRUE.
                  l1 = 5
!HURNB 11/93
                  IF ( Mf(6)==0 ) Blankh = .TRUE.
                  IF ( Blankh ) CALL gopen(iscr1,Ibuff(2*Nbuf+1),1)
!HURNE
                  ASSIGN 140 TO r
               ENDIF
               DO l = l1 , 8
                  IF ( Mf(l)/=0 ) THEN
                     IF ( l==4 .OR. l==8 ) THEN
                        IF ( Mf(l)/=2 ) Badfor = .TRUE.
                     ELSE
                        IF ( Mf(l)/=1 ) Badfor = .TRUE.
                     ENDIF
                  ENDIF
               ENDDO
               GOTO r
            ELSEIF ( Kx==50 .OR. Kx==51 ) THEN
!
!******        151-SUPAX, 150-OMITAX     *******************************
!
               l = 1
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Kx==52 ) THEN
!
!******        152-POINTAX        **************************************
!
               N = 3
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Kx==53 ) THEN
!
!******        153-SECTAX         **************************************
!
               N = 5
               IF ( rm(3)<=0 ) GOTO 20
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Kx==54 ) THEN
!
!******        154-PRESAX         **************************************
!
               N = 6
               IF ( M(1)<=0 .OR. M(4)<=0 .OR. M(4)==M(3) ) GOTO 20
               IF ( Ipiez==1 ) THEN
                  ASSIGN 40 TO r
                  spag_nextblock_1 = 12
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  IF ( M(3)<=0 ) GOTO 20
                  IF ( abs(rm(5))>=abs(rm(6)) .AND. sign(1.,rm(5))==sign(1.,rm(6)) ) GOTO 20
                  ASSIGN 40 TO r
                  spag_nextblock_1 = 12
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( Kx==55 ) THEN
!
!******        155-TEMPAX         **************************************
!
               DO l = 1 , 5 , 4
                  IF ( M(l)/=0 .OR. M(l+1)/=0 .OR. M(l+2)/=0 ) THEN
                     IF ( M(l)<=0 .OR. M(l+1)<=0 ) GOTO 20
                     N = N + 4
                     I(N-3) = M(l)
                     I(N-2) = M(l+1)
                     I(N-1) = M(l+2)
                     I(N) = M(l+3)
                  ENDIF
               ENDDO
               IF ( N<=0 ) GOTO 20
               ASSIGN 99999 TO r
               spag_nextblock_1 = 12
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Kx==56 .OR. Kx==57 ) THEN
!
!******     156-FORCEAX, 157-MOMAX    *******************************
!
               IF ( M(1)<=0 .OR. M(2)<=0 ) GOTO 20
               IF ( Mf(3)==2 .OR. Mf(3)==4 ) GOTO 20
               IF ( Mf(3)/=3 .AND. M(3)<0 ) GOTO 20
               N = 8
               l = 4
               I(1) = M(1)
               I(2) = M(2)
               I(3) = M(3)
               I(4) = 0
               IF ( Mf(3)==3 ) I(4) = M(4)
               IF ( Mf(3)==3 ) l = 5
               I(5) = M(l)
               I(6) = M(l+1)
               I(7) = M(l+2)
               I(8) = M(l+3)
               RETURN
            ELSEIF ( Kx==82 .OR. Kx==83 .OR. Kx==84 ) THEN
!
!******     182-DAREA, 183-DELAY, 184-DPHASE      *******************
!
               IF ( M(1)<=0 ) GOTO 20
               DO l = 2 , 5 , 3
!HURNB 11/93
!     WRITE(6,10003)L,M(L),M(L+1),M(L+2),N,NNS,(I(IL),IL=1,N)
!0003 FORMAT(7H DAREA0,6I10/(1X,24I5))
!HURNE
                  IF ( M(l)/=0 .OR. M(l+1)/=0 .OR. M(l+2)/=0 ) THEN
                     IF ( M(l)<=0 .OR. M(l+1)<0 .OR. M(l+1)>6 ) GOTO 20
                     N = N + 4
                     I(N-3) = M(1)
                     I(N-2) = M(l)
                     I(N-1) = M(l+1)
                     I(N) = M(l+2)
!HURNB 11/93
                     IF ( Iax ) THEN
                        IF ( M(l)<1000000 ) THEN
!
! FOR AXIC PROBLEMS AND GRID ID ON DAREA .LT. 10**6, GENERATE DAREAS FOR
! HARMONICS, COMPUTING THE GRID ID.  ASSUME  PRESSURE VALUE IS GIVEN FOR
! ZERO HARMONIC; FOR HIGHER HARMONICS, HALVE IT.
!
                           nharms = Nns + 1
                           IF ( Oneh ) nharms = 1
                           DO il = 1 , nharms
                              ill = il
                              IF ( Nns<0 .OR. il/=1 ) THEN
                                 IF ( il>1 ) THEN
                                    N = N + 4
                                    I(N-3) = M(1)
                                    I(N-1) = M(l+1)
                                 ELSE
!
! NNS.LT.0 .AND. IL.EQ.1
!
                                    ill = Nn + 1
                                 ENDIF
                                 xin = 0.5*rm(l+2)
                                 I(N) = ixin
                              ENDIF
                              I(N-2) = M(l) + 1000000*ill
                           ENDDO
                        ENDIF
                     ENDIF
                  ENDIF
!HURNE
               ENDDO
!HURNB 11/93
!      WRITE(6,10001)NHARMS,NNS,N,(I(IL),IL=1,N)
!10001 FORMAT(6H DAREA,3I10/(1X,24I5))
!HURNE
               IF ( N<=0 ) GOTO 20
               RETURN
            ELSEIF ( Kx==85 ) THEN
!
!*****      143-DSFACT(1430), 185-PLFACT(1420)     ********************
!
               IF ( Lplf<0 ) GOTO 20
               IF ( Lplf==0 ) THEN
                  Lplf = 1
                  Iddsf = 0
               ENDIF
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         IF ( Ky<=100 ) THEN
            IF ( Ky==1 .OR. Ky==2 .OR. Ky==3 .OR. Ky==4 .OR. Ky==5 .OR. Ky==6 .OR. Ky==7 .OR. Ky==8 .OR. Ky==9 .OR. Ky==10 .OR.     &
               & Ky==11 .OR. Ky==12 .OR. Ky==13 .OR. Ky==14 .OR. Ky==15 .OR. Ky==17 .OR. Ky==18 .OR. Ky==19 .OR. Ky==20 .OR.        &
               & Ky==21 .OR. Ky==22 .OR. Ky==23 .OR. Ky==24 .OR. Ky==25 .OR. Ky==26 .OR. Ky==27 .OR. Ky==28 .OR. Ky==29 .OR.        &
               & Ky==30 .OR. Ky==31 .OR. Ky==32 .OR. Ky==33 .OR. Ky==34 .OR. Ky==35 .OR. Ky==36 .OR. Ky==37 .OR. Ky==38 .OR.        &
               & Ky==39 .OR. Ky==40 .OR. Ky==41 .OR. Ky==42 .OR. Ky==43 .OR. Ky==45 .OR. Ky==46 .OR. Ky==47 .OR. Ky==48 .OR.        &
               & Ky==49 .OR. Ky==50 .OR. Ky==51 .OR. Ky==52 .OR. Ky==53 .OR. Ky==54 .OR. Ky==55 .OR. Ky==56 .OR. Ky==57 .OR.        &
               & Ky==58 .OR. Ky==59 .OR. Ky==60 .OR. Ky==61 .OR. Ky==62 .OR. Ky==63 .OR. Ky==64 .OR. Ky==65 .OR. Ky==66 .OR.        &
               & Ky==67 .OR. Ky==68 .OR. Ky==69 .OR. Ky==70 .OR. Ky==71 .OR. Ky==72 .OR. Ky==75 .OR. Ky==76 .OR. Ky==77 .OR.        &
               & Ky==78 .OR. Ky==80 .OR. Ky==81 .OR. Ky==82 .OR. Ky==83 .OR. Ky==89 .OR. Ky==91 .OR. Ky==92 .OR. Ky==93 .OR.        &
               & Ky==94 .OR. Ky==95 .OR. Ky==96 .OR. Ky==99 .OR. Ky==100 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Ky==16 ) THEN
               spag_nextblock_1 = 50
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Ky==44 ) THEN
!
!******        244-RADMTX     *****************************************
!
               IF ( Km==1 ) THEN
                  l1 = 1
               ELSE
                  Km = 1
                  IF ( Mf(1)/=1 ) Badfor = .TRUE.
                  Id = M(1)
                  IF ( Id<=Idrad ) Baddat = .TRUE.
                  Idrad = Id
                  I(1) = Id
                  N = 1
                  l1 = 2
               ENDIF
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Ky==73 .OR. Ky==74 ) THEN
!
!*****      273-AEFACT , 274-FLFACT    ********************************
!
               IF ( Km==1 ) THEN
                  l1 = 1
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  Km = 1
                  IF ( Mf(1)/=1 ) Badfor = .TRUE.
                  IF ( M(1)<=0 ) Baddat = .TRUE.
                  I(1) = M(1)
                  N = 1
                  l1 = 2
                  IF ( Mf(3)/=3 ) THEN
                     spag_nextblock_1 = 6
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( M(3)/=thru .OR. M(4)/=blnk ) Baddat = .TRUE.
                  IF ( Mf(2)/=2 .OR. Mf(4)/=2 .OR. Mf(5)/=1 .OR. Mf(6)/=2 ) Badfor = .TRUE.
                  IF ( M(6)<=1 ) Baddat = .TRUE.
                  IF ( M(5)==M(2) ) Baddat = .TRUE.
                  imid = 0
                  IF ( rm(5)-rm(7)>=0. .AND. rm(7)-rm(2)<0. ) imid = 1
                  IF ( rm(5)-rm(7)<=0. .AND. rm(7)-rm(2)>0. ) imid = 1
                  IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
                     IF ( Badfor .OR. Baddat ) THEN
                        spag_nextblock_1 = 7
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( imid/=0 ) THEN
                        rm(7) = 0.5*(rm(2)+rm(5))
                        CALL page2(3)
                        WRITE (Nout,99006) Uwm , I(1)
99006                   FORMAT (A25,' 528, FACTOR FMID IN FLFACT SET',I9,' DOES NOT LIE ','BETWEEN F1 AND FNF.',/5X,                &
                               &'IT IS BEING RESET TO (F1 + ','FNF)/2.0')
                     ENDIF
                     T4(2,K) = T4(2,K) + 1
                     CALL write(204,I,1,0)
                     l = 1
                     DO
                        term1 = (M(6)-l)*(rm(5)-rm(7))
                        term2 = (l-1)*(rm(7)-rm(2))
                        anum = rm(2)*term1 + rm(5)*term2
                        den = term1 + term2
                        factor = anum/den
                        T4(2,K) = T4(2,K) + 1
                        CALL write(204,factor,1,0)
                        l = l + 1
                        IF ( l>M(6) ) THEN
                           I(1) = -1
                           T4(2,K) = T4(2,K) + 1
                           CALL write(204,I,1,0)
                           N = 0
                           Km = 0
                           Kn = 0
                           spag_nextblock_1 = 4
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                     ENDDO
                  ELSE
                     Badfor = .TRUE.
                     Kn = 1
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
            ELSEIF ( Ky==79 ) THEN
!
!******        279-CRIGD1         **********************************
!
               Kn = 1
               IF ( irigid==2 ) THEN
                  N = 0
                  irg = 1
                  spag_nextblock_1 = 14
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  irigid = irigid + 1
                  IF ( Mf(1)/=1 ) Badfor = .TRUE.
                  IF ( M(1)<=0 ) Baddat = .TRUE.
                  I(1) = M(1)
                  N = 2
                  IF ( Mf(2)/=1 ) Badfor = .TRUE.
                  IF ( M(2)<1 ) Baddat = .TRUE.
                  I(2) = M(2)
                  IF ( Mf(4)/=3 ) THEN
                     irg = 3
                     spag_nextblock_1 = 14
                     CYCLE SPAG_DispatchLoop_1
                  ELSEIF ( M(4)/=thru .OR. M(5)/=blnk ) THEN
                     Baddat = .TRUE.
                     IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
                        spag_nextblock_1 = 15
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     Badfor = .TRUE.
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ELSEIF ( M1(1)==0 .AND. M1(2)==0 ) THEN
                     Badfor = .TRUE.
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     IF ( Mf(3)/=1 .OR. Mf(5)/=1 ) Badfor = .TRUE.
                     IF ( M(3)<=0 .OR. M(6)<=0 ) Baddat = .TRUE.
                     IF ( M(6)<=M(3) ) Baddat = .TRUE.
                     DO l = 6 , 8
                        IF ( Mf(l)/=0 ) Badfor = .TRUE.
                     ENDDO
                     IF ( Badfor .OR. Baddat ) THEN
                        spag_nextblock_1 = 15
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     T4(2,K) = T4(2,K) + 2
                     CALL write(210,M,2,0)
                     l = M(3)
                     DO
                        I(1) = l
                        DO j = 1 , 6
                           I(j+1) = j
                        ENDDO
                        T4(2,K) = T4(2,K) + 7
                        CALL write(210,I,7,0)
                        l = l + 1
                        IF ( l>M(6) ) THEN
                           irigid = 1
                           DO j = 1 , 7
                              I(j) = -1
                           ENDDO
                           IF ( M1(1)==arigid .AND. M1(2)==brigid ) I(2) = 0
                           N = 0
                           Kn = 0
                           T4(2,K) = T4(2,K) + 7
                           CALL write(210,I,7,0)
                           spag_nextblock_1 = 4
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                     ENDDO
                  ENDIF
               ENDIF
            ELSEIF ( Ky==84 ) THEN
!
!******        284-CRIGD2        **********************************
!
               Kn = 1
               IF ( irigid==2 ) THEN
                  N = 0
                  irg = 1
               ELSE
                  irigid = irigid + 1
                  IF ( Mf(1)/=1 ) Badfor = .TRUE.
                  IF ( M(1)<=0 ) Baddat = .TRUE.
                  I(1) = M(1)
                  N = 2
                  IF ( Mf(2)/=1 ) Badfor = .TRUE.
                  IF ( M(2)<1 ) Baddat = .TRUE.
                  I(2) = M(2)
                  irg = 3
               ENDIF
               DO l = irg , 8 , 2
                  l1 = l
                  IF ( M(l)<=0 ) THEN
                     spag_nextblock_1 = 18
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( M(l+1)<=0 ) Baddat = .TRUE.
                  IF ( Mf(l)/=1 .OR. Mf(l+1)/=1 ) Badfor = .TRUE.
                  I(N+1) = M(l)
                  IF ( ifpdco(M(l+1)) ) Baddat = .TRUE.
                  DO j = 1 , 6
                     I(N+1+j) = Ll(j)
                  ENDDO
                  N = N + 7
               ENDDO
               IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               spag_nextblock_1 = 17
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Ky==85 ) THEN
!
!******        285-CTRIAAX       ***************************************
!
               IF ( M(1)<=0 .OR. M(2)<=0 ) GOTO 20
               IF ( M(3)<=0 .OR. M(4)<=0 ) GOTO 20
               IF ( M(3)==M(4) ) GOTO 20
               IF ( M(3)==M(5) ) GOTO 20
               ih = Nn
               ASSIGN 20 TO r1
               ASSIGN 300 TO r
               spag_nextblock_1 = 58
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Ky==86 .OR. Ky==88 ) THEN
!
!******       286-PTRIAX, 288-PTRAPAX   *******************************
!
               IF ( M(1)<=0 ) GOTO 20
               ih = Nn
               ASSIGN 20 TO r1
               ASSIGN 320 TO r
               spag_nextblock_1 = 58
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Ky==87 ) THEN
!
!*******       287-CTRAPAX             ********************************
!
               IF ( M(1)<=0 .OR. M(2)<=0 ) GOTO 20
               IF ( M(3)==M(4) ) GOTO 20
               IF ( M(3)==M(5) ) GOTO 20
               ih = Nn
               ASSIGN 20 TO r1
               ASSIGN 340 TO r
               spag_nextblock_1 = 58
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Ky==90 ) THEN
!
!******        290-VARIAN        **************************************
!
               IF ( Km==1 ) THEN
                  l1 = 1
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  Km = 1
                  IF ( Nvar/=0 ) GOTO 20
                  Nvar = 1
                  l1 = 1
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( Ky==97 ) THEN
!
!******         297-CRIGDR      *************************************
!
               DO l = 1 , 5 , 4
                  IF ( M(l)/=0 .OR. M(l+1)/=0 .OR. M(l+2)/=0 .OR. M(l+3)/=0 ) THEN
                     IF ( M(l)<=0 .OR. M(l+1)<=0 .OR. M(l+2)<=0 .OR. M(l+3)<=0 ) GOTO 20
                     IF ( M(l+1)==M(l+2) ) GOTO 20
                     IF ( M(l+3)>3 ) THEN
                        WRITE (Nout,99022) Ufm , blnk , M(l) , Knt
                        Baddat = .TRUE.
                     ELSE
                        N = N + 4
                        IF ( N>4 .AND. M(l)==M(l-4) ) GOTO 20
                        I(N-3) = M(l)
                        I(N-2) = M(l+1)
                        I(N-1) = M(l+2)
                        I(N) = M(l+3)
                     ENDIF
                  ENDIF
               ENDDO
               IF ( N<=0 ) GOTO 20
               RETURN
            ELSEIF ( Ky==98 ) THEN
               spag_nextblock_1 = 19
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         kz = Ky - 100
         IF ( kz<=53 ) THEN
            IF ( .NOT.(kz<47 .OR. kz>51 .OR. .NOT.first) ) THEN
               first = .FALSE.
               IF ( prt ) THEN
                  CALL page1
                  WRITE (Nout,99007) Uim
99007             FORMAT (A29,', CONVERSIONS OF RIGID ELEMENTS, CRROD, CRBAR, ',                                                    &
                         &'CRTRPLT, CRBE1, AND CRBE2, TO CRIGDR, CRIGD2, OR CRIGD3',/5X,                                            &
                         &'ARE AS FOLLOWS (BLANK FIELDS MAY BE PRINTED AS ZEROS','. CONTINUATION FIELDS ARE NOT PRINTED) -',/)
                  line = 8
               ENDIF
            ENDIF
            IF ( kz==29 ) THEN
!
!******     329-PROLATE     ********************************************
!
               IF ( Km/=0 ) THEN
                  l1 = 1
               ELSE
                  IF ( prol ) Baddat = .TRUE.
                  prol = .TRUE.
                  Km = 1
                  IF ( Mf(1)/=2 .OR. Mf(2)/=2 ) Badfor = .TRUE.
                  IF ( rm(1)<=rm(2) ) Baddat = .TRUE.
                  DO l = 3 , 6
                     IF ( Mf(l)/=1 ) Badfor = .TRUE.
                     IF ( M(l)<0 ) Baddat = .TRUE.
                  ENDDO
                  IF ( M(3)<2 ) Baddat = .TRUE.
                  IF ( M(4)<2 ) Baddat = .TRUE.
                  IF ( M(5)>30 ) Baddat = .TRUE.
                  IF ( M(6)>M(5) ) M(6) = M(5)
                  Id = M(1)
                  nsegs = M(3)
                  msegs = M(4)
                  itot1 = (nsegs-1)*(msegs+1) + 2
                  itot2 = (nsegs-1)*msegs + 2
                  DO l = 1 , 6
                     I(l) = M(l)
                  ENDDO
                  N = 6
                  l1 = 7
                  items = 0
               ENDIF
               DO l = l1 , 8
                  IF ( Mf(l)/=1 .AND. Mf(l)/=3 ) Badfor = .TRUE.
                  IF ( Mf(l)==3 ) THEN
                     spag_nextblock_1 = 62
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  items = items + 1
                  IF ( M(l)<=0 ) Baddat = .TRUE.
                  N = N + 1
                  I(N) = M(l)
               ENDDO
               Kn = 1
               spag_nextblock_1 = 61
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kz==30 ) THEN
!
!******      330-PERMBDY       *****************************************
!
               IF ( Km==0 ) THEN
                  IF ( perm ) Baddat = .TRUE.
                  perm = .TRUE.
                  Km = 1
               ENDIF
               DO l = 1 , 8
                  IF ( Mf(l)/=1 .AND. Mf(l)/=3 ) Badfor = .TRUE.
                  IF ( Mf(l)==3 ) THEN
                     spag_nextblock_1 = 62
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( M(l)<=0 ) Baddat = .TRUE.
                  N = N + 1
                  I(N) = M(l)
               ENDDO
               Kn = 1
               spag_nextblock_1 = 61
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kz==32 ) THEN
               spag_nextblock_1 = 49
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kz==45 ) THEN
!
!******        345-STREAML1      **************************************
!
               IF ( Km==1 ) THEN
                  l1 = 1
                  spag_nextblock_1 = 46
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  Km = 1
                  IF ( Mf(1)/=1 ) Badfor = .TRUE.
                  IF ( M(1)<=0 ) Baddat = .TRUE.
                  IF ( M(1)<=0 ) Baddat = .TRUE.
                  I(1) = M(1)
                  N = 1
                  IF ( Mf(3)/=3 .OR. M(3)/=thru ) THEN
                     l1 = 2
                     spag_nextblock_1 = 46
                     CYCLE SPAG_DispatchLoop_1
                  ELSEIF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
                     IF ( Mf(2)/=1 .OR. Mf(4)/=1 ) Badfor = .TRUE.
                     IF ( M(2)<=0 .OR. M(5)<=0 .OR. (M(2)>M(5)) ) Baddat = .TRUE.
                     IF ( .NOT.(Badfor .OR. Baddat) ) THEN
                        CALL write(204,I,N,0)
                        l1 = M(2)
                        l2 = M(5)
                        DO l = l1 , l2
                           CALL write(204,l,1,0)
                        ENDDO
                        N = 0
                     ENDIF
                     spag_nextblock_1 = 47
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     Kn = 1
                     Badfor = .TRUE.
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
            ELSEIF ( kz==46 ) THEN
!
!******        346-STREAML2      **************************************
!
!     THEORY DEPENDENT RESTRICTION -  (3.GE. NSTNS .LE.10)
!
               IF ( M(1)<=0 ) GOTO 20
               IF ( M(2)<3 .OR. M(2)>10 ) GOTO 20
               IF ( rm(4)<=0.0 ) GOTO 20
               DO l = 6 , 9
                  IF ( rm(l)<=0.0 ) GOTO 20
               ENDDO
               IF ( rm(3)<=-90.0 .OR. rm(3)>=90.0 ) GOTO 20
               IF ( rm(10)<=-90.0 .OR. rm(10)>=90.0 ) GOTO 20
               N = 10
               GOTO 40
            ELSEIF ( kz==47 ) THEN
!
!******       347-CRROD        *****************************************
!
!     MAP THIS RIGID ELEMENT INTO CRIGID3 FORM
!
               IF ( Mf(1)+Mf(2)+Mf(3)/=3 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( M(1)<=0 .OR. M(2)<=0 .OR. M(3)<=0 ) GOTO 20
               IF ( M(2)==M(3) ) GOTO 20
               IF ( M(4)<0 .OR. M(5)<0 ) THEN
                  spag_nextblock_1 = 32
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               l = M(4) + M(5)
               IF ( l<1 .OR. l>3 ) THEN
                  spag_nextblock_1 = 32
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( M(4)/=0 .AND. M(5)/=0 ) THEN
                  spag_nextblock_1 = 32
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( prt ) THEN
                  CALL page2(3)
                  IF ( M(4)/=0 ) WRITE (Nout,99008) scc , Knt , (M(j),j=1,4) , gcc , M(1) , M(3) , M(2) , M(4)
99008             FORMAT (/25X,A19,I7,1H-,5X,'CRROD ',4I8,/25X,A19,13X,'CRIGDR',4I8)
                  IF ( M(4)==0 ) WRITE (Nout,99009) scc , Knt , (M(j),j=1,3) , M(5) , gcc , (M(j),j=1,3) , M(5)
99009             FORMAT (/25X,A19,I7,1H-,5X,'CRROD ',3I8,8X,I8,/25X,A19,13X,'CRIGDR',4I8)
               ENDIF
               l = M(3)
               IF ( M(4)/=0 ) THEN
                  l = M(2)
                  M(2) = M(3)
                  M(3) = l
                  M(5) = M(4)
               ENDIF
               M(4) = M(5)
               N = 4
               GOTO 40
            ELSEIF ( kz==48 ) THEN
!
!******       348-CRBAR        *****************************************
!
!     MAP THIS RIGID ELEMENT INTO CRIGD3 FORM
!
               IF ( Mf(1)+Mf(2)+Mf(3)/=3 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( M(1)<=0 .OR. M(2)<=0 .OR. M(3)<=0 ) GOTO 20
               IF ( M(2)==M(3) ) GOTO 20
               rbe = .FALSE.
               IF ( M(6)==0 .AND. M(7)==0 ) rbe = .TRUE.
               IF ( M(4)==0 .AND. M(5)==0 ) THEN
!
                  WRITE (Nout,99022) Ufm , ind , M(1) , knt1
                  GOTO 20
               ELSEIF ( ifpdco(M(4)) ) THEN
                  WRITE (Nout,99022) Ufm , ind , M(1) , knt1
                  GOTO 20
               ELSE
                  lk = 1
                  DO l = 1 , 6
                     lll = Ll(l)
                     IF ( rbe .AND. lll==0 ) M(6) = M(6) + l*lk
                     IF ( lll==0 ) lk = lk*10
                     ia(l) = lll
                  ENDDO
                  IF ( ifpdco(M(5)) ) THEN
                     WRITE (Nout,99022) Ufm , ind , M(1) , knt1
                     GOTO 20
                  ELSE
                     lk = 1
                     DO l = 1 , 6
                        lll = Ll(l)
                        IF ( rbe .AND. lll==0 ) M(7) = M(7) + l*lk
                        IF ( lll==0 ) lk = lk*10
                        ib(l) = lll
                     ENDDO
                     IF ( rbe ) THEN
                        spag_nextblock_1 = 26
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( ifpdco(M(6)) ) THEN
                        spag_nextblock_1 = 32
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     DO l = 1 , 6
                        IF ( ia(l)/=0 ) THEN
                           IF ( ia(l)==Ll(l) ) THEN
                              spag_nextblock_1 = 32
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                        ENDIF
                        ja(l) = Ll(l)
                     ENDDO
                     IF ( ifpdco(M(7)) ) THEN
                        spag_nextblock_1 = 32
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     DO l = 1 , 6
                        IF ( ib(l)/=0 ) THEN
                           IF ( ib(l)==Ll(l) ) THEN
                              spag_nextblock_1 = 32
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                        ENDIF
                        jb(l) = Ll(l)
                     ENDDO
                     spag_nextblock_1 = 26
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
            ELSEIF ( kz==49 ) THEN
!
!******      349-CRTRPLT      ******************************************
!
!     MAP THIS RIGID ELEMENT INTO CRIGD3 FORM
!
               IF ( Mf(1)+Mf(2)+Mf(3)+Mf(4)/=4 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( M(1)<=0 .OR. M(2)<=0 .OR. M(3)<=0 .OR. M(4)<=0 ) GOTO 20
               IF ( M(2)==M(3) .OR. M(2)==M(4) .OR. M(3)==M(4) ) GOTO 20
               IF ( M(5)==0 .AND. M(6)==0 .AND. M(7)==0 ) THEN
                  WRITE (Nout,99022) Ufm , ind , M(1) , knt1
                  GOTO 20
               ELSE
                  rbe = .FALSE.
                  IF ( M(9)==0 .AND. M(10)==0 .AND. M(11)==0 ) rbe = .TRUE.
                  IF ( ifpdco(M(5)) ) THEN
                     WRITE (Nout,99022) Ufm , ind , M(1) , knt1
                     GOTO 20
                  ELSE
                     lk = 1
                     DO l = 1 , 6
                        lll = Ll(l)
                        IF ( rbe .AND. lll==0 ) M(9) = M(9) + l*lk
                        IF ( lll==0 ) lk = lk*10
                        ia(l) = lll
                     ENDDO
                     IF ( ifpdco(M(6)) ) THEN
                        WRITE (Nout,99022) Ufm , ind , M(1) , knt1
                        GOTO 20
                     ELSE
                        lk = 1
                        DO l = 1 , 6
                           lll = Ll(l)
                           IF ( rbe .AND. lll==0 ) M(10) = M(10) + l*lk
                           IF ( lll==0 ) lk = lk*10
                           ib(l) = lll
                        ENDDO
                        IF ( ifpdco(M(7)) ) THEN
                           WRITE (Nout,99022) Ufm , ind , M(1) , knt1
                           GOTO 20
                        ELSE
                           lk = 1
                           DO l = 1 , 6
                              lll = Ll(l)
                              IF ( rbe .AND. lll==0 ) M(11) = M(11) + l*lk
                              IF ( lll==0 ) lk = lk*10
                              ic(l) = lll
                           ENDDO
                           IF ( rbe ) THEN
                              spag_nextblock_1 = 28
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( ifpdco(M(9)) ) THEN
                              spag_nextblock_1 = 32
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           DO l = 1 , 6
                              IF ( ia(l)/=0 ) THEN
                                 IF ( ia(l)==Ll(l) ) THEN
                                    spag_nextblock_1 = 32
                                    CYCLE SPAG_DispatchLoop_1
                                 ENDIF
                              ENDIF
                              ja(l) = Ll(l)
                           ENDDO
                           IF ( ifpdco(M(10)) ) THEN
                              spag_nextblock_1 = 32
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           DO l = 1 , 6
                              IF ( ib(l)/=0 ) THEN
                                 IF ( ib(l)==Ll(l) ) THEN
                                    spag_nextblock_1 = 32
                                    CYCLE SPAG_DispatchLoop_1
                                 ENDIF
                              ENDIF
                              jb(l) = Ll(l)
                           ENDDO
                           IF ( ifpdco(M(11)) ) THEN
                              spag_nextblock_1 = 32
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           DO l = 1 , 6
                              IF ( ic(l)/=0 ) THEN
                                 IF ( ic(l)==Ll(l) ) THEN
                                    spag_nextblock_1 = 32
                                    CYCLE SPAG_DispatchLoop_1
                                 ENDIF
                              ENDIF
                              jc(l) = Ll(l)
                           ENDDO
                           spag_nextblock_1 = 28
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ELSEIF ( kz==50 ) THEN
               spag_nextblock_1 = 19
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kz==51 ) THEN
!
!******    351-CRBE2         *******************************************
!
!     MAP THIS RIGID ELEMENT INTO CRIGD2 FORM
!
               Kn = 1
               IF ( irigid==2 ) THEN
                  N = 0
                  irg = 1
                  spag_nextblock_1 = 29
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  irigid = irigid + 1
                  knt1 = Knt
                  l6 = 60
                  L7 = l6
                  l8 = 0
                  IF ( Mf(1)+Mf(2)+Mf(3)/=3 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( M(1)<=0 .OR. M(2)<=0 ) GOTO 20
                  I(1) = M(1)
                  I(2) = M(2)
                  q(1) = M(1)
                  q(2) = M(2)
                  m3 = M(3)
                  l8 = l8 + 2
                  q(L7+1) = M(1)
                  q(L7+2) = M(2)
                  q(L7+3) = m3
                  L7 = L7 + 3
                  N = 2
                  irg = 4
                  IF ( ifpdco(m3) ) Baddat = .TRUE.
                  IF ( m3==0 ) Baddat = .TRUE.
                  spag_nextblock_1 = 29
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( kz==52 ) THEN
!
!******    352-CRBE3         *******************************************
!
!     CARD 3, OR CARDS 2 AND 3, CAN BE OMITTED IF THE CARD(S) CONTAINS
!     ALL BLANKS.
!     CARD 5, OR CARDS 4 AND 5, CAN BE OMITTED IF THE CARD(S) CONTIANS
!     ALL BLANKS, OR DEFAULT FOR THE 'UM' OPTION IS USED
!
!     ACTUALLY THIS CRBE3 INPUT CARD IS NOT WHAT SHOWN IN THE USER'S
!     MANUAL. THE LIST OF G(I,J) CAN BE AS LONG AS NEEDED. THEREFORE
!     CARDS 2 AND 3 CAN BE EXPANDED BEYOND THE 3 GRID POINTS AS SHOWN.
!     THE 4TH AND 5TH CARDS CAN BE EXPANDED TOO. THE WI AND CI FIELDS
!     NEED NOT BE IN THE FIELDS AS SHOWN IN THE EXAMPLE OF THE MANUAL
!
!     CHANGES DONE IN 92 VERSION WERE REMOVED AND REPLACED BY 91 CODE
!     SEE 93 CODE FOR THESE CHANGES
!
!     IM HERE IS CARD NUMBER COUNT
!
               IF ( Km==0 ) THEN
                  Km = 1
                  im = 1
                  IF ( Mf(1)+Mf(3)+Mf(4)/=3 ) Badfor = .TRUE.
                  IF ( M(1)<=0 .OR. M(3)<=0 .OR. M(4)<=0 ) Baddat = .TRUE.
                  IF ( ifpdco(M(4)) ) Baddat = .TRUE.
                  IF ( Mf(5)/=2 ) Baddat = .TRUE.
                  I(1) = M(1)
                  I(2) = M(3)
                  I(3) = M(4)
!
! ... NOTE - COMPONENTS IN LL NOT SENT OUT IN CRBE3
!
                  N = 3
                  l1 = 5
!
!
               ELSEIF ( Mf(1)==3 ) THEN
                  IF ( M(1)/=ium ) Baddat = .TRUE.
                  I(N+1) = -1
                  I(N+2) = -2
                  N = N + 2
                  im = 0
                  l1 = 3
                  spag_nextblock_1 = 34
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( im==0 ) THEN
                  l1 = 2
                  spag_nextblock_1 = 34
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  l1 = 1
               ENDIF
               DO l = l1 , 8
                  IF ( Mf(l)==2 ) THEN
                     IF ( l1/=5 ) THEN
                        N = N + 1
                        I(N) = -1
                     ENDIF
                     im = 1
!WKBI 11/93 SPR93018
                     l1 = 1
                     N = N + 1
                     I(N) = M(l)
                  ELSEIF ( Mf(l)/=0 ) THEN
                     IF ( Mf(l)/=1 .OR. M(l)<=0 ) Baddat = .TRUE.
                     IF ( im/=-1 ) THEN
                        IF ( ifpdco(M(l)) ) Baddat = .TRUE.
                     ENDIF
                     im = -1
                     N = N + 1
                     I(N) = M(l)
                  ENDIF
               ENDDO
               IF ( M1(1)/=0 ) THEN
                  N = N + 1
                  I(N) = -1
               ELSE
                  Kn = 1
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               spag_nextblock_1 = 33
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kz==53 ) THEN
!
!******    353-CRSPLINE      *******************************************
!
               IF ( Km/=0 ) THEN
                  l1 = 1
                  IF ( im==-9 ) THEN
                     spag_nextblock_1 = 38
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  spag_nextblock_1 = 35
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  Km = 1
                  im = -1
                  IF ( Mf(1)/=1 .OR. M(1)<=0 ) THEN
                     spag_nextblock_1 = 38
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( Mf(2)==0 ) rm(2) = .1
                  IF ( rm(2)<=0. ) THEN
                     spag_nextblock_1 = 38
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( Mf(3)/=1 .OR. M(3)<=0 ) THEN
                     spag_nextblock_1 = 38
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  I(1) = M(1)
                  I(2) = M(2)
                  I(3) = M(3)
                  N = 3
                  l1 = 4
                  spag_nextblock_1 = 35
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         CALL page2(2)
         WRITE (Nout,99010) Sfm
99010    FORMAT (A25,' 322, ILLEGAL ENTRY TO IFS3P.')
         Abort = .TRUE.
         RETURN 1
      CASE (3)
         Badfor = .TRUE.
         RETURN 1
 20      Baddat = .TRUE.
         RETURN 1
 40      DO l = 1 , N
            I(l) = M(l)
         ENDDO
         spag_nextblock_1 = 4
      CASE (4)
         RETURN 3
      CASE (5)
         IF ( Km==1 ) THEN
            l1 = 1
         ELSE
            Km = 1
            IF ( Mf(1)/=1 ) Badfor = .TRUE.
            Id = M(1)
            IF ( Id<=Iddsf ) Baddat = .TRUE.
            Iddsf = Id
            I(1) = Id
            IF ( Mf(2)/=2 ) Badfor = .TRUE.
            N = 2
            l1 = 3
            I(N) = M(2)
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
         DO l = l1 , 8
            IF ( Mf(l)==0 ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Mf(l)/=2 ) Badfor = .TRUE.
            N = N + 1
            I(N) = M(l)
         ENDDO
         IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
            Kn = 1
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
         Km = 0
         N = N + 1
         I(N) = -1
         Kn = 0
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (8)
         IF ( l==1 ) Badfor = .TRUE.
         DO l2 = l , 8
            IF ( Mf(l2)/=0 ) Badfor = .TRUE.
         ENDDO
         IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Badfor = .TRUE.
         Kn = 1
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 60      IF ( ifpdco(M(7)) ) GOTO 20
         N = 4
         I(1) = M(1)
         I(2) = M(3)
         I(3) = M(4)
         I(4) = M(7)
         RETURN
 80      N = 4
         GOTO 40
 100     N = 24
         GOTO 40
 120     N = 5
         GOTO 40
 140     IF ( M(1)<=0 ) Baddat = .TRUE.
         Id = M(1)
         N = 1
         I(N) = Id
         ih = Nn
         ASSIGN 180 TO r
         ASSIGN 20 TO r1
         spag_nextblock_1 = 58
         CYCLE SPAG_DispatchLoop_1
 160     N = 0
 180     DO l = l1 , 5 , 4
            IF ( M(l)/=0 .OR. M(l+1)/=0 .OR. M(l+2)/=0 .OR. M(l+3)/=0 ) THEN
               IF ( M(l)<=0 .OR. M(l+1)<0 .OR. M(l+2)<0 .OR. M(l+3)==0 .AND. l1==5 ) Baddat = .TRUE.
!HURNB 11/93
               IF ( Blankh .AND. l1==1 .AND. Mf(l+1)/=0 ) Badfor = .TRUE.
               IF ( .NOT.Blankh .AND. l1==1 .AND. Mf(l+1)==0 ) Badfor = .TRUE.
!HURNE
               N = N + 4
               I(N-3) = M(l)
               I(N-2) = M(l+1)
               I(N-1) = M(l+2)
               I(N) = M(l+3)
            ENDIF
         ENDDO
         nt = nt + N
         IF ( N<4 ) Baddat = .TRUE.
         Kn = 1
!HURNB 11/93
         IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
            IF ( .NOT.Blankh ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL write(iscr1,I,N,0)
!      WRITE(6,10005)N,(I(IL),IL=1,N)
!10005 FORMAT(6H MPCAX,6I5)
            N = 0
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
!HURNE
            IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!HURNB 11/93
!HURNE
         N = N + 4
         I(N-3) = -1
         I(N-2) = -1
         I(N-1) = -1
         I(N) = -1
         Kn = 0
         Km = 0
         IF ( nt<9 ) Baddat = .TRUE.
!HURNB 11/93
         IF ( .NOT.Blankh ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
! MPCAX CARD DONE - GENERATE CARDS FOR ALL HARMONICS ASSUMING THE ONE JU
! STORED (WITH BLANK HARMONIC) IS FOR THE ZERO HARMONIC
!
         IF ( nt>Nopen ) CALL mesage(-8,0,nam)
         CALL write(iscr1,I,N-4,1)
!     WRITE(6,10006)N,(I(IL),IL=1,N)
!0006 FORMAT(7H MPCAX1,10I5)
         CALL close(iscr1,1)
         CALL gopen(iscr1,Ibuff(2*Nbuf+1),0)
         CALL read(*200,*220,iscr1,Ibuff(3*Nbuf+1),Nopen,0,nnt)
 200     CALL mesage(-8,0,nam)
 220     CALL close(iscr1,1)
!     WRITE(6,10007)NT,NNT,(IBUFF(3*NBUF+IL),IL=1,NNT)
!0007 FORMAT(7H MPCAX2,10I5)
         IF ( nt/=nnt ) CALL mesage(-61,0,0)
!
! ALL MPCAX CARD INFO FOR THIS CARD IS READ IN. GENERATE FOR ALL HARMONI
!
         nharms = Nns + 1
         IF ( Oneh ) nharms = 1
         DO l = 1 , nharms
            ill = l - 1
            IF ( Oneh ) ill = iabs(Nns)
            DO il = 3 , nt , 4
               Ibuff(3*Nbuf+il) = ill
            ENDDO
            T4(2,K) = T4(2,K) + nt
            CALL write(215,Ibuff(3*Nbuf+1),nt,0)
            T4(2,K) = T4(2,K) + 4
            CALL write(215,iones,4,0)
         ENDDO
!HURNE
         N = 0
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (9)
         IF ( M(l)==0 .AND. M(l+1)==0 .AND. M(l+2)==0 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( M(l)<=0 .OR. M(l+1)<0 ) GOTO 20
         IF ( ifpdco(M(l+2)) ) GOTO 20
         ASSIGN 240 TO r
         ASSIGN 20 TO r1
         ih = M(l+1)
         spag_nextblock_1 = 58
         CYCLE SPAG_DispatchLoop_1
 240     N = N + 3
         IF ( N>3 .AND. M(l)==M(l-3) .AND. M(l-1)==M(l-4) .AND. M(l-2)==M(l-5) ) GOTO 20
         I(N-2) = M(l)
         I(N-1) = M(l+1)
         I(N) = M(l+2)
         spag_nextblock_1 = 10
      CASE (10)
         l = l + 3
         IF ( l==4 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( N<=0 ) GOTO 20
         RETURN
      CASE (11)
         IF ( M(1)<=0 .OR. M(2)<=0 ) GOTO 20
         ASSIGN 40 TO r
         spag_nextblock_1 = 12
      CASE (12)
         ih = Nn
         ASSIGN 20 TO r1
         spag_nextblock_1 = 58
         CYCLE SPAG_DispatchLoop_1
 260     IF ( M(1)<=0 ) Baddat = .TRUE.
         Id = M(1)
         IF ( M(2)<=0 .OR. M(3)<0 .OR. M(4)==0 ) Baddat = .TRUE.
         IF ( Ids==Id .AND. Jms==M(2) .AND. Kms==M(3) ) Baddat = .TRUE.
         Ids = Id
         Jms = M(2)
         Kms = M(3)
         N = 4
         DO l = 1 , 4
            I(l) = M(l)
         ENDDO
         spag_nextblock_1 = 13
         CYCLE SPAG_DispatchLoop_1
 280     N = 0
         IF ( M(2)/=0 .OR. M(3)/=0 .OR. M(4)/=0 ) THEN
            IF ( M(2)<=0 .OR. M(3)<0 ) Baddat = .TRUE.
            N = 3
            DO l = 2 , 4
               I(l-1) = M(l)
            ENDDO
         ENDIF
         spag_nextblock_1 = 13
      CASE (13)
         IF ( M(5)/=0 .OR. M(6)/=0 .OR. M(7)/=0 ) THEN
            IF ( M(5)<=0 .OR. M(6)<0 ) Baddat = .TRUE.
            N = N + 3
            I(N-2) = M(5)
            I(N-1) = M(6)
            I(N) = M(7)
         ENDIF
         IF ( N<=0 ) Baddat = .TRUE.
         nt = nt + N
         DO l = 1 , 8
            M(l) = 0
         ENDDO
         Kn = 1
         IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
            N = N + 3
            I(N-2) = -1
            I(N-1) = -1
            I(N) = -1
            Kn = 0
            Km = 0
            IF ( nt<7 ) Baddat = .TRUE.
         ENDIF
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (14)
         DO l = irg , 8
            l1 = l
            IF ( M(l)<=0 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Mf(l)/=1 ) Badfor = .TRUE.
            I(N+1) = M(l)
            DO j = 1 , 6
               I(N+1+j) = j
            ENDDO
            N = N + 7
         ENDDO
         IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 15
      CASE (15)
         irigid = 1
         DO j = 1 , 7
            I(N+j) = -1
         ENDDO
         IF ( M1(1)==arigid .AND. M1(2)==brigid ) I(N+2) = 0
         N = N + 7
         Kn = 0
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (16)
         DO lk = l1 , 8
            IF ( M(lk)/=0 ) Baddat = .TRUE.
            IF ( Mf(lk)/=0 ) Badfor = .TRUE.
         ENDDO
         IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Badfor = .TRUE.
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (17)
         irigid = 1
         DO j = 1 , 7
            I(N+j) = -1
         ENDDO
         IF ( M1(1)==arigid .AND. M1(2)==crigid ) I(N+2) = 0
         N = N + 7
         Kn = 0
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (18)
         IF ( M1(1)==0 .AND. M1(2)==0 ) Baddat = .TRUE.
         DO lk = l1 , 8
            IF ( M(lk)/=0 ) Baddat = .TRUE.
            IF ( Mf(lk)/=0 ) Badfor = .TRUE.
         ENDDO
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
      CASE (19)
!
!******      298-CRIGD3, 350-CRBE1       ******************************
!
         Kn = 1
         IF ( irigid==2 ) THEN
            spag_nextblock_1 = 23
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( irigid==3 ) THEN
            spag_nextblock_1 = 24
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( irigid==4 ) THEN
!
            l1 = 2
            l2 = 6
            l6 = 0
            IF ( Mf(1)/=0 .OR. Mf(2)/=1 .OR. Mf(3)/=1 ) Badfor = .TRUE.
            IF ( M(1)/=0 .OR. M(2)<1 .OR. M(3)<1 ) Baddat = .TRUE.
            N = 0
         ELSE
            irigid = 2
            jrigid = 1
            knt1 = Knt
            l1 = 2
            l2 = 6
            l6 = 0
            IF ( Mf(1)/=1 .OR. Mf(2)/=1 .OR. Mf(3)/=1 ) Badfor = .TRUE.
            IF ( M(1)<1 .OR. M(2)<1 .OR. M(3)<1 ) Baddat = .TRUE.
            N = 1
            I(1) = M(1)
            q(1) = M(1)
            l8 = 1
            ncomp = 0
         ENDIF
         spag_nextblock_1 = 20
      CASE (20)
         l5 = l2 + 2
         DO l = l1 , l2 , 2
            l3 = l + 1
            IF ( Mf(l-l6)==0 ) THEN
               spag_nextblock_1 = 22
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Mf(l-l6)/=1 .OR. Mf(l-l6+1)/=1 ) Badfor = .TRUE.
            IF ( M(l)<1 .OR. M(l+1)<1 ) Baddat = .TRUE.
            IF ( prt ) THEN
               q(l8+1) = M(l)
               q(l8+2) = M(l3)
               l8 = l8 + 2
            ENDIF
            I(N+1) = M(l)
            IF ( ifpdco(M(l+1)) ) Baddat = .TRUE.
            DO j = 1 , 6
               I(N+j+1) = Ll(j)
               IF ( irigid/=4 ) THEN
                  IF ( Ll(j)/=0 ) ncomp = ncomp + 1
               ENDIF
            ENDDO
            N = N + 7
            IF ( irigid/=4 ) THEN
               IF ( ncomp>6 ) Baddat = .TRUE.
            ENDIF
         ENDDO
         IF ( Mf(l5-l6)/=0 ) Badfor = .TRUE.
         IF ( M(l5)/=0 ) Baddat = .TRUE.
         IF ( jrigid==3 ) THEN
            spag_nextblock_1 = 25
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 21
      CASE (21)
         IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
            Badfor = .TRUE.
            irigid = 1
         ELSE
            IF ( M1f(2)/=0 .AND. ncomp<6 ) Baddat = .TRUE.
         ENDIF
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (22)
         DO lk = l3 , l5
            IF ( Mf(lk-l6)/=0 ) Badfor = .TRUE.
            IF ( M(lk)/=0 ) Baddat = .TRUE.
         ENDDO
         IF ( jrigid==1 .OR. jrigid==2 ) THEN
            spag_nextblock_1 = 21
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( jrigid==3 ) THEN
            spag_nextblock_1 = 25
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 23
      CASE (23)
!
         IF ( Mf(1)==0 ) THEN
            irigid = 3
            jrigid = 2
            l1 = 2
            l2 = 6
            l6 = 0
            IF ( Mf(2)/=1 .OR. Mf(3)/=1 ) Badfor = .TRUE.
            IF ( M(1)/=0 .OR. M(2)<1 .OR. M(3)<1 ) Baddat = .TRUE.
            N = 0
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 24
      CASE (24)
!
         irigid = 4
         jrigid = 3
         l1 = 3
         l2 = 7
         l6 = 1
         L7 = l8
         IF ( Mf(1)/=3 .OR. Mf(2)/=1 .OR. Mf(3)/=1 ) Badfor = .TRUE.
         IF ( (M(1)/=mset .AND. M(1)/=ium) .OR. M(2)/=blnk .OR. M(3)<1 .OR. M(4)<1 ) THEN
            WRITE (Nout,99022) Ufm , blnk , q(1) , knt1
            GOTO 20
         ELSE
            N = 1
            I(1) = mset
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ENDIF
      CASE (25)
         IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
            irigid = 1
            DO j = 1 , 7
               I(N+j) = -1
            ENDDO
            IF ( M1(1)==arigid .AND. M1(2)==drigid ) I(N+2) = 0
            IF ( M1(1)==crbe .AND. M1(2)==erigid ) I(N+2) = 0
            N = N + 7
            Kn = 0
            IF ( .NOT.(kz/=50 .OR. .NOT.prt) ) THEN
               lk = (l8+4)/3 + 2
               CALL page2(lk)
               WRITE (Nout,99011) scc , knt1 , (q(j),j=1,L7)
99011          FORMAT (/25X,A19,I7,1H-,5X,'CRBE1 ',7I8,/,(71X,6I8))
               lk = L7 + 1
               WRITE (Nout,99012) (q(j),j=lk,l8)
99012          FORMAT (69X,'UM',6I8,/,(71X,6I8))
               WRITE (Nout,99013) gcc , (q(j),j=1,L7)
99013          FORMAT (25X,A19,13X,'CRIGD3',7I8,/,(71X,6I8))
               WRITE (Nout,99014) (q(j),j=lk,l8)
99014          FORMAT (67X,'MSET',6I8,/,(71X,6I8))
            ENDIF
         ENDIF
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (26)
!
         IF ( prt ) THEN
            CALL page2(4)
            WRITE (Nout,99015) scc , Knt , (M(l),l=1,7) , gcc , M(1) , M(2) , M(4) , M(3) , M(5) , M(2) , M(6) , M(3) , M(7)
99015       FORMAT (/25X,A19,I7,1H-,5X,'CRBAR ',7I8,/25X,A19,13X,'CRIGD3',5I8,/67X,'MSET',4I8)
         ENDIF
         spag_nextblock_1 = 27
      CASE (27)
!
!     KZ=48 (CRBAR),   KZ=49 (CRTRPLT)
!
         ncomp = 0
         DO l = 1 , 6
            IF ( ia(l)/=0 ) ncomp = ncomp + 1
            IF ( ib(l)/=0 ) ncomp = ncomp + 1
            IF ( kz==49 ) THEN
               IF ( ic(l)/=0 ) ncomp = ncomp + 1
            ENDIF
         ENDDO
         IF ( ncomp/=6 ) THEN
            WRITE (Nout,99022) Ufm , ind , M(1) , knt1
            GOTO 20
         ELSE
            lk = 0
            IF ( kz==49 ) lk = 1
            I(1) = M(1)
            N = 2
            IF ( M(4+lk)/=0 ) THEN
               I(N) = M(2)
               DO j = 1 , 6
                  I(N+j) = ia(j)
               ENDDO
               N = N + 7
            ENDIF
            IF ( M(5+lk)/=0 ) THEN
               I(N) = M(3)
               DO j = 1 , 6
                  I(N+j) = ib(j)
               ENDDO
               N = N + 7
            ENDIF
            IF ( kz==49 .AND. M(6+lk)/=0 ) THEN
               I(N) = M(4)
               DO j = 1 , 6
                  I(j+N) = ic(j)
               ENDDO
               N = N + 7
            ENDIF
!
            I(N) = mset
            N = N + 1
            IF ( rbe ) THEN
               DO j = 1 , 6
                  IF ( ia(j)==0 ) ia(j) = -j
                  IF ( ia(j)>0 ) ia(j) = 0
                  IF ( ib(j)==0 ) ib(j) = -j
                  IF ( ib(j)>0 ) ib(j) = 0
                  IF ( kz==49 ) THEN
                     IF ( ic(j)==0 ) ic(j) = -j
                     IF ( ic(j)>0 ) ic(j) = 0
                  ENDIF
               ENDDO
            ENDIF
            IF ( kz==49 ) lk = 3
            IF ( M(6+lk)/=0 ) THEN
               I(N) = M(2)
               DO j = 1 , 6
                  IF ( rbe ) I(N+j) = -ia(j)
                  IF ( .NOT.rbe ) I(N+j) = ja(j)
               ENDDO
               N = N + 7
            ENDIF
            IF ( M(7+lk)/=0 ) THEN
               I(N) = M(3)
               DO j = 1 , 6
                  IF ( rbe ) I(N+j) = -ib(j)
                  IF ( .NOT.rbe ) I(N+j) = jb(j)
               ENDDO
               N = N + 7
            ENDIF
            IF ( kz==49 .AND. M(8+lk)/=0 ) THEN
               I(N) = M(4)
               DO j = 1 , 6
                  IF ( rbe ) I(N+j) = -ic(j)
                  IF ( .NOT.rbe ) I(N+j) = jc(j)
               ENDDO
               N = N + 7
            ENDIF
            N = N - 1
            DO j = 1 , 7
               I(N+j) = -1
            ENDDO
            IF ( M1(1)==crtr .OR. M1(1)==crba ) I(N+2) = 0
            N = N + 7
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
      CASE (28)
         IF ( prt ) THEN
            knt1 = Knt
            IF ( .NOT.rbe ) knt1 = Knt - 1
            CALL page2(5)
            WRITE (Nout,99016) scc , knt1 , (M(l),l=1,7) , (M(l),l=9,11) , gcc , M(1) , M(2) , M(5) , M(3) , M(6) , M(4) , M(7) ,   &
                             & M(2) , M(9) , M(3) , M(10) , M(4) , M(11)
99016       FORMAT (/25X,A19,I7,1H-,5X,'CRTRPLT',I7,6I8,/63X,3I8,/25X,A19,13X,'CRIGD3',7I8,/67X,'MSET',6I8)
         ENDIF
         spag_nextblock_1 = 27
         CYCLE SPAG_DispatchLoop_1
      CASE (29)
         DO l = irg , 8
            IF ( Mf(l)==0 ) THEN
               spag_nextblock_1 = 31
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Mf(l)/=1 ) Badfor = .TRUE.
            IF ( M(l)<=0 ) Baddat = .TRUE.
            IF ( l8<l6 ) THEN
               q(l8+1) = M(l)
               q(l8+2) = m3
            ENDIF
            l8 = l8 + 2
            IF ( L7<92 ) q(L7+1) = M(l)
            L7 = L7 + 1
            I(N+1) = M(l)
            DO j = 1 , 6
               I(N+1+j) = Ll(j)
            ENDDO
            N = N + 7
         ENDDO
         IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 30
      CASE (30)
         irigid = 1
         DO j = 1 , 7
            I(N+j) = -1
         ENDDO
         IF ( M1(1)==crbe .AND. M1(2)==frigid ) I(N+2) = 0
         N = N + 7
         Kn = 0
         IF ( prt ) THEN
            l3 = L7
            l5 = l8
            IF ( l3>92 ) l3 = 92
            IF ( l5>l6 ) l5 = l6
            j = (l5+2)/8 + (l3-l6+2)/8 + 2
            CALL page2(j)
            l6 = l6 + 1
            WRITE (Nout,99017) scc , knt1 , (q(j),j=l6,l3)
99017       FORMAT (/25X,A19,I7,1H-,5X,'CRBE2 ',8I8,/,(63X,8I8))
            WRITE (Nout,99018) gcc , (q(j),j=1,l5)
99018       FORMAT (25X,A19,13X,'CRIGD2',8I8,/,(63X,8I8))
            IF ( l8>l6 .OR. L7>102 ) WRITE (Nout,99019)
99019       FORMAT (57X,'*** ABOVE PRINTOUT MAY BE IMCOMPLETE.  DATA IS OK')
         ENDIF
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (31)
         l1 = l
         IF ( l1<=8 ) THEN
            DO l = l1 , 8
               IF ( M(l)/=0 ) Baddat = .TRUE.
               IF ( Mf(l)/=0 ) Badfor = .TRUE.
            ENDDO
         ENDIF
         IF ( M1(1)==0 .AND. M1(2)==0 ) Baddat = .TRUE.
         spag_nextblock_1 = 30
         CYCLE SPAG_DispatchLoop_1
      CASE (32)
         WRITE (Nout,99022) Ufm , blnk , M(1) , knt1
         GOTO 20
      CASE (33)
         Kn = 0
         Km = 0
         N = N + 1
         I(N) = -3
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (34)
         DO l = 2 , 6 , 2
            IF ( Mf(l)/=0 ) THEN
               IF ( Mf(l)/=1 .OR. M(l1)<=0 ) Baddat = .TRUE.
               IF ( Mf(l+1)/=1 .OR. M(l1+1)<=0 ) Baddat = .TRUE.
               IF ( ifpdco(M(l1+1)) ) Baddat = .TRUE.
               I(N+1) = M(l1)
               I(N+2) = M(l1+1)
               N = N + 2
            ENDIF
            l1 = l1 + 2
         ENDDO
         IF ( M1(1)/=0 ) THEN
            spag_nextblock_1 = 33
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (35)
         DO l = l1 , 8
            IF ( Mf(l)/=0 .AND. Mf(l)/=1 ) THEN
               spag_nextblock_1 = 38
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( im==-1 .AND. M(l)<0 ) THEN
               spag_nextblock_1 = 38
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( im==-1 .AND. M(l)==0 ) THEN
               spag_nextblock_1 = 36
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( im/=-1 ) THEN
               IF ( ifpdco(M(l)) ) THEN
                  spag_nextblock_1 = 38
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
!
! ... NOTE - COMPONENTS IN LL NOT SENT OUT IN CRSPLINE
!
            im = -im
            N = N + 1
            I(N) = M(l)
         ENDDO
         IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
            spag_nextblock_1 = 37
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         N = N + 1
         I(N) = 0
         spag_nextblock_1 = 36
      CASE (36)
         im = -9
         N = N + 1
         I(N) = -1
         IF ( l/=8 ) THEN
            l1 = l
            DO l = l1 , 8
               IF ( Mf(l)/=0 ) THEN
                  spag_nextblock_1 = 38
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
         spag_nextblock_1 = 37
      CASE (37)
!
         Kn = 1
         IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
            Kn = 0
            Km = 0
            N = N + 1
            I(N) = -1
         ENDIF
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (38)
         Baddat = .TRUE.
         spag_nextblock_1 = 37
         CYCLE SPAG_DispatchLoop_1
 300     N = 6
         GOTO 40
 320     N = 17
         GOTO 40
 340     N = 7
         GOTO 40
      CASE (39)
         DO l = l3 , 8
            IF ( Mf(l)/=0 .AND. Mf(l)/=1 ) Badfor = .TRUE.
         ENDDO
         l5 = 1
         DO l = l3 , 7 , 2
            IF ( M(l)==0 ) THEN
               spag_nextblock_1 = 41
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            l5 = l + 2
            l8 = l8 + 1
            N = N + 2
            I(N-1) = M(l)
            I(N) = M(l+1)
            IF ( M(l)>0 ) THEN
               IF ( M(l+1)>=0 .AND. M(l+1)<=6 ) CYCLE
            ENDIF
            Baddat = .TRUE.
         ENDDO
         IF ( M1f(2)/=3 ) THEN
            spag_nextblock_1 = 45
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 40
      CASE (40)
         N = N + 2
         I(N-1) = -1
         I(N) = l8
         L0 = L0 + 1
         IF ( L0==5 ) THEN
            L7 = l8
         ELSE
            l6 = l8
         ENDIF
         spag_nextblock_1 = 45
         CYCLE SPAG_DispatchLoop_1
      CASE (41)
         DO l = l5 , 7 , 2
            IF ( M(l)/=0 .OR. M(l+1)/=0 ) Baddat = .TRUE.
         ENDDO
         IF ( l5<=1 ) Baddat = .TRUE.
         IF ( M1f(2)==3 ) THEN
            spag_nextblock_1 = 40
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Baddat = .TRUE.
         spag_nextblock_1 = 45
         CYCLE SPAG_DispatchLoop_1
      CASE (42)
         l9 = (l6*(l6+1))/2
         lb = lb + 1
         IF ( M(1)==lz ) kzflag = 1
         IF ( M(1)==kk ) kzflag = 2
         I(lb) = kzflag
         spag_nextblock_1 = 43
      CASE (43)
         L0 = L0 + 1
         l8 = 0
         IF ( Mf(1)/=3 ) Badfor = .TRUE.
         l3 = 2
         DO l = 2 , 8
            M(l) = M(l+1)
         ENDDO
         spag_nextblock_1 = 44
      CASE (44)
         DO l = l3 , 8
            IF ( Mf(l)/=2 .AND. Mf(l)/=0 ) Badfor = .TRUE.
         ENDDO
         N = lb
         l5 = l9 - l8 + l3 - 1
         IF ( l5>8 ) l5 = 8
         DO l = l3 , l5
            N = N + 1
            I(N) = M(l)
         ENDDO
         l5 = l9 - l8 + l3
         l8 = l8 + N - lb
         IF ( l9<=l8 ) THEN
            IF ( l9/=l8 ) THEN
               DO l = l5 , 8
                  IF ( M(l)/=0 ) Baddat = .TRUE.
               ENDDO
            ENDIF
            IF ( L0==8 ) THEN
               L0 = 1
            ELSE
               L0 = L0 + 1
            ENDIF
         ENDIF
         spag_nextblock_1 = 45
      CASE (45)
         DO l = 1 , 8
            M(l) = 0
         ENDDO
         Kn = 1
         IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
            Kn = 0
            IF ( Id>La1 ) THEN
               La1 = Id
               IF ( .NOT.(.NOT.noud .AND. L7/=6 .AND. nos .AND. kzflag==1) ) THEN
                  IF ( .NOT.(L7==0 .AND. .NOT.nos) ) THEN
                     L7 = 0
                     IF ( .NOT.(L0==1 .AND. .NOT.noud) ) THEN
                        N = N + 1
                        I(N) = 0
                        L0 = 1
                     ENDIF
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
            ENDIF
            Baddat = .TRUE.
            L0 = 1
            L7 = 0
            La1 = Id
         ENDIF
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (46)
         DO l = l1 , 8
            IF ( Mf(l)/=0 .AND. Mf(l)/=1 ) Badfor = .TRUE.
         ENDDO
         DO l = l1 , 8
            IF ( M(l)<0 ) THEN
               Baddat = .TRUE.
            ELSEIF ( M(l)/=0 ) THEN
               N = N + 1
               I(N) = M(l)
            ENDIF
         ENDDO
         IF ( N<l1 ) Baddat = .TRUE.
         Kn = 1
         IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 47
      CASE (47)
         Km = 0
         N = N + 1
         I(N) = -1
         Kn = 0
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (48)
         WRITE (Nout,99020) Ufm , M(1) , M(2) , Knt
99020    FORMAT (A23,' 331, IMPROPER PARAM CARD ',2A4,10X,'SORTED CARD COUNT =',I7)
         CALL page2(2)
         Abort = .TRUE.
         RETURN
      CASE (49)
!
!*******    12-SPC1(3980), 92-OMIT1(3981), 216-ASET1(3981)   ***********
!          332-CFLSTR(3980)
!
         iz = 2
         ifile = 210
         IF ( K==332 ) ifile = 208
         spag_nextblock_1 = 51
         CYCLE SPAG_DispatchLoop_1
      CASE (50)
         iz = 1
         ifile = 210
         spag_nextblock_1 = 51
      CASE (51)
         IF ( Km/=0 ) THEN
            l1 = 1
            spag_nextblock_1 = 53
            CYCLE SPAG_DispatchLoop_1
         ELSE
            Km = 1
            IF ( Mf(iz)/=0 .AND. Mf(iz)/=1 ) Badfor = .TRUE.
            IF ( K/=332 ) THEN
               IF ( ifpdco(M(iz)) ) Baddat = .TRUE.
               IF ( iz/=2 ) THEN
                  spag_nextblock_1 = 52
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
            IF ( Mf(1)/=1 ) Badfor = .TRUE.
            IF ( M(1)<=0 ) Baddat = .TRUE.
         ENDIF
         spag_nextblock_1 = 52
      CASE (52)
         Id = M(1)
         I(1) = M(1)
         IF ( iz==2 ) I(2) = M(2)
         N = iz
         l1 = iz + 1
         IF ( Mf(iz+2)==3 .AND. M(iz+2)==thru ) THEN
            IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
               IF ( Mf(iz+1)/=1 .OR. Mf(iz+3)/=1 ) Badfor = .TRUE.
               IF ( M(iz+1)<=0 .OR. M(iz+4)<=M(iz+1) ) Baddat = .TRUE.
               DO l = iz , 4
                  IF ( Mf(l+4)/=0 ) Badfor = .TRUE.
               ENDDO
               IF ( Badfor .OR. Baddat ) THEN
                  spag_nextblock_1 = 55
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               CALL write(ifile,M,iz,0)
               l1 = M(iz+1)
               l2 = M(iz+4)
               l = l1
               DO
                  CALL write(ifile,l,1,0)
                  l = l + 1
                  IF ( l>l2 ) THEN
                     N = 0
                     spag_nextblock_1 = 55
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
            ELSE
               Kn = 1
               Badfor = .TRUE.
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 53
      CASE (53)
         DO l = l1 , 8
            IF ( Mf(l)/=0 .AND. Mf(l)/=1 ) Badfor = .TRUE.
         ENDDO
         DO l = l1 , 8
            IF ( Mf(l)==1 ) THEN
               spag_nextblock_1 = 54
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         Baddat = .TRUE.
         spag_nextblock_1 = 54
      CASE (54)
         DO l = l1 , 8
            IF ( M(l)<0 ) THEN
               Baddat = .TRUE.
            ELSEIF ( M(l)/=0 ) THEN
               N = N + 1
               I(N) = M(l)
            ENDIF
         ENDDO
         Kn = 1
         IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 55
      CASE (55)
         Km = 0
         N = N + 1
         I(N) = -1
         Kn = 0
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (56)
!
!******        84-LOAD, 123-DLOAD      *******************************
!
         IF ( Km==1 ) THEN
            N = 0
         ELSE
            Km = 1
            IF ( Mf(1)/=0 .AND. Mf(1)/=1 .OR. Mf(2)/=0 .AND. Mf(2)/=2 ) Badfor = .TRUE.
            IF ( M(1)<=0 ) Baddat = .TRUE.
            Id = M(1)
            I(1) = Id
            I(2) = M(2)
            IF ( M(4)<=0 ) Baddat = .TRUE.
            N = 2
         ENDIF
         l8 = N + 1
         DO l = l8 , 7 , 2
            IF ( Mf(l)/=0 .AND. Mf(l)/=2 .OR. Mf(l+1)/=0 .AND. Mf(l+1)/=1 ) Badfor = .TRUE.
         ENDDO
         SPAG_Loop_1_1: DO
            N = N + 2
            IF ( M(N)<0 ) THEN
               Baddat = .TRUE.
               EXIT SPAG_Loop_1_1
            ELSEIF ( M(N)==0 ) THEN
               EXIT SPAG_Loop_1_1
            ELSE
               I(N-1) = M(N-1)
               I(N) = M(N)
               IF ( N>=8 ) THEN
                  spag_nextblock_1 = 57
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_1_1
         N = N - 2
         L7 = 1
         l8 = N + 1
         DO l = l8 , 8
            IF ( Mf(l)/=0 ) Baddat = .TRUE.
         ENDDO
         IF ( N<=0 ) Baddat = .TRUE.
         spag_nextblock_1 = 57
      CASE (57)
         Kn = 1
         IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
            Km = 0
            N = N + 2
            I(N-1) = -1
            I(N) = -1
            Kn = 0
            L7 = 0
         ELSEIF ( L7==1 ) THEN
            Baddat = .TRUE.
            L7 = 0
         ENDIF
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (58)
!
!     ******************************************************************
!
         IF ( .NOT.Iax ) THEN
            IF ( Lh ) WRITE (Nout,99021) Ufm
99021       FORMAT (A23,' 332, AXIC CARD REQUIRED.')
            IF ( Lh ) CALL page2(2)
            Lh = .FALSE.
            Abort = .TRUE.
         ELSEIF ( ih>Nn .OR. ih<0 ) THEN
            GOTO r1
         ENDIF
         GOTO r
      CASE (59)
         N = N - 1
         L7 = 1
         l8 = N + 1
         DO l = l8 , 8
            IF ( Mf(l)/=0 ) Baddat = .TRUE.
         ENDDO
         IF ( N<=0 ) Baddat = .TRUE.
         spag_nextblock_1 = 60
      CASE (60)
         Kn = 1
         IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
            Km = 0
            N = N + 1
            I(N) = -1
            Kn = 0
            L7 = 0
         ELSEIF ( L7==1 ) THEN
            Baddat = .TRUE.
            L7 = 0
         ENDIF
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (61)
         IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Baddat = .TRUE.
         spag_nextblock_1 = 63
         CYCLE SPAG_DispatchLoop_1
      CASE (62)
         IF ( M(l)/=endt ) THEN
            Baddat = .TRUE.
            spag_nextblock_1 = 61
            CYCLE SPAG_DispatchLoop_1
         ELSE
            IF ( M1(1)==0 .AND. M1(2)==0 ) Baddat = .TRUE.
         ENDIF
         spag_nextblock_1 = 63
      CASE (63)
         Km = 0
         Kn = 0
         IF ( K/=330 ) THEN
            IF ( items/=itot1 .AND. items/=itot2 ) Baddat = .TRUE.
         ENDIF
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99022 FORMAT (A23,', ILLEGAL ',A2,'DEPENDENT D.O.F.',' FOR RIGID ELEMENT',I9,' SORTED COUNT',I8)
!
99999 END SUBROUTINE ifs3p
