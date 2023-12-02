!*==ifs3p.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifs3p() !HIDESTARS (*,*,*)
   USE c_cifs3p
   USE c_ifpdta
   USE c_ifpx2
   USE c_ifpx3
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
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
         IF ( k<=100 ) THEN
            IF ( k==1 ) THEN
!
!*******              1-GRID            ********************************
!
               IF ( mf(2)==0 ) m(2) = igdst2
               IF ( mf(6)==0 ) m(6) = igdst6
               IF ( mf(7)==0 ) m(7) = igdst7
               IF ( mf(8)==0 ) m(8) = igdst8
               IF ( m(1)<=0 .OR. m(2)<0 .OR. m(6)<-1 ) GOTO 20
               IF ( .NOT.(m(6)>=0 .OR. grdmsg) ) THEN
                  CALL page2(2)
                  WRITE (nout,99001) uwm
99001             FORMAT (A23,' 302, ONE OR MORE GRID CARDS HAVE DISPLACEMENT ','COORDINATE SYSTEM ID OF -1')
                  grdmsg = .TRUE.
               ENDIF
               IF ( ifpdco(m(7)) ) GOTO 20
               IF ( ifpdco(m(8)) ) GOTO 20
               IF ( mf(8)/=0 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               n = 8
               GOTO 40
            ELSEIF ( k==2 ) THEN
!
!*******        2-GRDSET       ****************************************
!
               IF ( g1==0 ) GOTO 20
               g1 = 0
               IF ( m(2)==0 .AND. m(6)==0 .AND. m(7)==0 .AND. m(8)==0 ) GOTO 20
               IF ( m(2)<0 .OR. m(6)<-1 .OR. m(7)<0 .OR. m(8)<0 ) GOTO 20
               IF ( ifpdco(m(7)) .OR. ifpdco(m(8)) ) GOTO 20
               IF ( mf(8)/=0 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               igdst2 = m(2)
               igdst6 = m(6)
               igdst7 = m(7)
               igdst8 = m(8)
               RETURN 2
            ELSEIF ( k==3 .OR. k==4 .OR. k==5 .OR. k==6 .OR. k==7 .OR. k==8 .OR. k==9 .OR. k==10 .OR. k==11 .OR. k==14 .OR.         &
                   & k==15 .OR. k==16 .OR. k==18 .OR. k==19 .OR. k==20 .OR. k==21 .OR. k==22 .OR. k==23 .OR. k==24 .OR. k==25 .OR.  &
                   & k==26 .OR. k==27 .OR. k==29 .OR. k==30 .OR. k==31 .OR. k==32 .OR. k==33 .OR. k==34 .OR. k==35 .OR. k==36 .OR.  &
                   & k==37 .OR. k==38 .OR. k==39 .OR. k==40 .OR. k==41 .OR. k==42 .OR. k==43 .OR. k==44 .OR. k==45 .OR. k==46 .OR.  &
                   & k==47 .OR. k==48 .OR. k==49 .OR. k==50 .OR. k==51 .OR. k==52 .OR. k==53 .OR. k==54 .OR. k==55 .OR. k==56 .OR.  &
                   & k==57 .OR. k==58 .OR. k==59 .OR. k==60 .OR. k==61 .OR. k==62 .OR. k==63 .OR. k==64 .OR. k==65 .OR. k==66 .OR.  &
                   & k==67 .OR. k==68 .OR. k==69 .OR. k==70 .OR. k==71 .OR. k==72 .OR. k==73 .OR. k==74 .OR. k==75 .OR. k==76 .OR.  &
                   & k==77 .OR. k==78 .OR. k==79 .OR. k==80 .OR. k==81 .OR. k==85 .OR. k==86 .OR. k==87 .OR. k==88 .OR. k==89 .OR.  &
                   & k==90 .OR. k==91 .OR. k==93 .OR. k==94 .OR. k==95 .OR. k==96 .OR. k==97 .OR. k==98 .OR. k==99 .OR. k==100 )    &
                   & THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( k==12 ) THEN
               spag_nextblock_1 = 49
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( k==13 .OR. k==83 ) THEN
!
!******      13-SPCADD, 83-MPCADD    **********************************
!
               IF ( km==1 ) THEN
!
!***** TEMPORARY UNFIX FOR SPCADD AND MPCADD ***************************
!
                  n = 0
               ELSE
                  km = 1
                  IF ( m(1)<=0 ) baddat = .TRUE.
                  id = m(1)
                  i(1) = id
                  IF ( m(2)<=0 .OR. m(3)<0 ) baddat = .TRUE.
                  IF ( m(3)==0 ) CALL page2(2)
                  IF ( m(3)==0 ) WRITE (nout,99002) uwm
99002             FORMAT (A25,' 4124, THE SPCADD OR MPCADD UNION CONSISTS OF A ','SINGLE SET.')
                  n = 1
               ENDIF
               DO l = 1 , 8
                  IF ( mf(l)/=0 .AND. mf(l)/=1 ) badfor = .TRUE.
               ENDDO
               DO
                  n = n + 1
                  IF ( m(n)<0 ) THEN
                     baddat = .TRUE.
                     spag_nextblock_1 = 59
                     CYCLE SPAG_DispatchLoop_1
                  ELSEIF ( m(n)==0 ) THEN
                     spag_nextblock_1 = 59
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     i(n) = m(n)
                     IF ( n>=8 ) THEN
                        spag_nextblock_1 = 60
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDIF
               ENDDO
            ELSEIF ( k==17 ) THEN
!
!******        17-MPC       ******************************************
!
               IF ( m(3)>6 .OR. m(6)>6 ) baddat = .TRUE.
               IF ( ithrml==1 ) THEN
                  IF ( m(3)>1 .OR. m(6)>1 ) baddat = .TRUE.
               ENDIF
               IF ( km/=0 ) THEN
                  IF ( mf(1)/=0 .OR. mf(8)/=0 ) badfor = .TRUE.
                  ASSIGN 280 TO r
               ELSE
                  km = 1
                  nt = 0
                  IF ( mf(1)/=1 .OR. mf(8)/=0 ) badfor = .TRUE.
                  ASSIGN 260 TO r
               ENDIF
               DO l = 2 , 7
                  IF ( mf(l)/=0 ) THEN
                     IF ( l==4 .OR. l==7 ) THEN
                        IF ( mf(l)/=2 ) badfor = .TRUE.
                     ELSE
                        IF ( mf(l)/=1 ) badfor = .TRUE.
                     ENDIF
                  ENDIF
               ENDDO
               GOTO r
            ELSEIF ( k==28 ) THEN
!
!******         28-GENEL         **************************************
!
               IF ( l0==2 .OR. l0==4 ) THEN
                  l3 = 1
                  n = 0
                  spag_nextblock_1 = 39
               ELSEIF ( l0==3 ) THEN
                  l0 = l0 + 1
                  lb = 0
                  IF ( mf(1)/=3 .OR. (m(1)/=lz .AND. m(1)/=kk) ) THEN
                     l8 = 0
                     IF ( mf(1)/=3 .OR. mf(2)/=0 ) badfor = .TRUE.
                     IF ( m(1)/=lud ) baddat = .TRUE.
                     l3 = 3
                     noud = .FALSE.
                     DO l = 2 , 8
                        m(l) = m(l+1)
                     ENDDO
                     n = 0
                     spag_nextblock_1 = 39
                  ELSE
                     l0 = l0 + 1
                     lb = 2
                     i(1) = -1
                     i(2) = 0
                     spag_nextblock_1 = 42
                  ENDIF
               ELSEIF ( l0==5 ) THEN
                  IF ( m(1)/=lz .AND. m(1)/=kk ) baddat = .TRUE.
                  spag_nextblock_1 = 42
               ELSEIF ( l0==6 .OR. l0==8 ) THEN
                  l3 = 1
                  lb = 0
                  spag_nextblock_1 = 44
               ELSEIF ( l0==7 ) THEN
                  IF ( m(1)/=ls ) baddat = .TRUE.
                  l9 = l6*l7
                  lb = 1
                  i(1) = l7
                  nos = .FALSE.
                  spag_nextblock_1 = 43
               ELSE
                  l0 = l0 + 1
                  kzflag = 0
                  l8 = 0
                  noud = .TRUE.
                  nos = .TRUE.
                  IF ( mf(1)/=1 .OR. mf(2)/=0 ) badfor = .TRUE.
                  IF ( m(1)<=0 ) baddat = .TRUE.
                  id = m(1)
                  i(1) = id
                  n = 1
                  l3 = 3
                  spag_nextblock_1 = 39
               ENDIF
               CYCLE
            ELSEIF ( k==82 ) THEN
!
!******         82-PARAM         ***********************************
!
               IF ( mf(1)/=3 .OR. mf(2)<=0 .OR. mf(3)/=0 .AND. mf(3)/=mf(2) ) THEN
                  spag_nextblock_1 = 48
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( mf(3)/=0 .AND. mf(3)/=2 .AND. mf(3)/=4 ) THEN
                  spag_nextblock_1 = 48
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               DO l = 4 , 8
                  IF ( mf(l)/=0 ) THEN
                     spag_nextblock_1 = 48
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
               IF ( nparam+7<=nopen ) THEN
                  ip = 2*nbuf + nparam
                  ibuff(ip+1) = m(1)
                  ibuff(ip+2) = m(2)
                  ibuff(ip+3) = mf(2)
                  ibuff(ip+4) = m(3)
                  nparam = nparam + 4
                  IF ( mf(2)>2 .OR. mf(3)/=0 ) THEN
                     ibuff(ip+5) = m(4)
                     nparam = nparam + 1
                     IF ( mf(2)>4 .OR. mf(3)/=0 ) THEN
                        IF ( mf(3)==4 ) THEN
                           ibuff(ip+3) = 6
                           ibuff(ip+6) = m(5)
                           ibuff(ip+7) = m(6)
                           nparam = nparam + 2
                        ELSE
                           ibuff(ip+3) = 5
                        ENDIF
                     ENDIF
                  ENDIF
               ELSE
                  CALL page2(2)
                  WRITE (nout,99003) sfm
99003             FORMAT (A25,' 330, NO ROOM IN CORE FOR PARAM CARDS.')
                  abort = .TRUE.
               ENDIF
               RETURN
            ELSEIF ( k==84 ) THEN
               spag_nextblock_1 = 56
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( k==92 ) THEN
               spag_nextblock_1 = 50
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         IF ( kx<=100 ) THEN
            IF ( kx==1 .OR. kx==2 .OR. kx==3 .OR. kx==4 .OR. kx==5 .OR. kx==6 .OR. kx==7 .OR. kx==8 .OR. kx==9 .OR. kx==10 .OR.     &
               & kx==11 .OR. kx==12 .OR. kx==13 .OR. kx==14 .OR. kx==15 .OR. kx==16 .OR. kx==17 .OR. kx==18 .OR. kx==19 .OR.        &
               & kx==20 .OR. kx==21 .OR. kx==22 .OR. kx==24 .OR. kx==25 .OR. kx==27 .OR. kx==28 .OR. kx==29 .OR. kx==30 .OR.        &
               & kx==33 .OR. kx==34 .OR. kx==35 .OR. kx==36 .OR. kx==37 .OR. kx==40 .OR. kx==41 .OR. kx==42 .OR. kx==58 .OR.        &
               & kx==59 .OR. kx==60 .OR. kx==61 .OR. kx==62 .OR. kx==63 .OR. kx==64 .OR. kx==65 .OR. kx==66 .OR. kx==67 .OR.        &
               & kx==68 .OR. kx==69 .OR. kx==70 .OR. kx==71 .OR. kx==72 .OR. kx==73 .OR. kx==74 .OR. kx==75 .OR. kx==76 .OR.        &
               & kx==77 .OR. kx==78 .OR. kx==79 .OR. kx==80 .OR. kx==81 .OR. kx==86 .OR. kx==87 .OR. kx==88 .OR. kx==89 .OR.        &
               & kx==90 .OR. kx==91 .OR. kx==92 .OR. kx==93 .OR. kx==94 .OR. kx==95 .OR. kx==96 .OR. kx==97 .OR. kx==98 .OR.        &
               & kx==99 .OR. kx==100 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( kx==23 ) THEN
               spag_nextblock_1 = 56
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( kx==26 ) THEN
!
!*****         126-FREQ       ******************************************
!
               IF ( idfreq ) iddsf = 0
               idfreq = .FALSE.
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kx==31 .OR. kx==32 ) THEN
!
!******     131-RLOAD1, 132-RLOAD2    **********************************
!
               IF ( m(5)==0 .AND. m(6)==0 ) GOTO 20
               IF ( m(1)<=0 .OR. m(2)<=0 .OR. m(3)<0 .OR. m(4)<0 ) GOTO 20
               IF ( m(5)<0 .OR. m(6)<0 ) GOTO 20
               n = 6
               GOTO 40
            ELSEIF ( kx==38 ) THEN
!
!*******       138-TLOAD1      *****************************************
!
               IF ( m(1)<=0 .OR. m(2)<=0 .OR. m(3)<0 .OR. m(5)<=0 ) GOTO 20
               IF ( m(4)<0 .OR. m(4)>4 ) GOTO 20
               n = 5
               GOTO 40
            ELSEIF ( kx==39 ) THEN
!
!*******       139-TLOAD2      *****************************************
!
               IF ( m(1)<=0 .OR. m(2)<=0 .OR. m(3)<0 ) GOTO 20
               IF ( rm(5)<0. .OR. rm(6)<=rm(5) .OR. rm(7)<0. ) GOTO 20
               IF ( m(4)<0 .OR. m(4)>4 ) GOTO 20
               n = 10
               GOTO 40
            ELSEIF ( kx==43 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kx==44 ) THEN
!
!******        144-AXIC           **************************************
!
               IF ( iax ) THEN
                  CALL page2(2)
                  WRITE (nout,99004) ufm
99004             FORMAT (A23,' 329, ONLY ONE(1) AXIC CARD ALLOWED.')
                  abort = .TRUE.
                  RETURN
               ELSE
                  iax = .TRUE.
                  nn = 998
                  DO l = 1 , nt1
                     IF ( t4(1,l)>0 ) t3(1,l) = t3(1,k)
                  ENDDO
!HURD2 11/93
!     IF (M(1).LT.0 .OR. M(1).GT.998 .OR. M(2).NE.0) GO TO 8
!     NN = M(1)
!HURNB 11/93
!
! M.LT.0 CHECK IS REMOVED TO ALLOW FOR SINGLE HARMONIC
!
!     IF(M(1).LT.0.OR.M(1).GT.998.OR.M(2).NE.0)GO TO 8
                  IF ( m(1)>998 .OR. m(2)/=0 ) GOTO 20
                  nns = m(1)
                  nn = iabs(m(1))
                  oneh = .FALSE.
                  IF ( nns<0 ) oneh = .TRUE.
!HURNE
                  n = 2
                  IF ( nn>15 .AND. nbpw<=32 ) THEN
                     WRITE (nout,99005) uwm
99005                FORMAT (A25,', POTENTIAL SYSTEM FATAL ERROR DUE TO LARGE HARMONIC',' (LARGER THAN 15) ON 32-BIT WORD MACHINE')
                  ENDIF
                  GOTO 40
               ENDIF
            ELSEIF ( kx==45 ) THEN
!  OR GO TO 1447
!
!******        145-RINGAX         **************************************
!
               IF ( m(1)<=0 .OR. rm(3)<=0. ) GOTO 20
               ih = nn
               ASSIGN 60 TO r
               ASSIGN 20 TO r1
               spag_nextblock_1 = 58
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kx==46 ) THEN
!
!******        146-CCONEAX        **************************************
!
               IF ( m(1)<=0 .OR. m(3)<=0 .OR. m(4)<=0 ) GOTO 20
               IF ( mf(2)==0 ) m(2) = m(1)
               IF ( m(2)<=0 .OR. m(4)==m(3) ) GOTO 20
               ih = nn
               ASSIGN 80 TO r
               ASSIGN 20 TO r1
               spag_nextblock_1 = 58
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kx==47 ) THEN
!
!******        147-PCONEAX        **************************************
!
               IF ( m(1)<=0 ) GOTO 20
               IF ( m(2)==0 .AND. m(3)/=0 .OR. m(2)<0 ) GOTO 20
               IF ( m(4)==0 .AND. m(5)/=0 .OR. m(4)<0 ) GOTO 20
               IF ( m(6)==0 .AND. m(7)/=0 .OR. m(6)<0 ) GOTO 20
               IF ( m(2)/=0 .AND. m(3)==0 ) GOTO 20
               IF ( m(6)/=0 .AND. m(7)==0 ) GOTO 20
               ih = nn
               ASSIGN 100 TO r
               ASSIGN 20 TO r1
               spag_nextblock_1 = 58
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kx==48 ) THEN
!
!******        148-SPCAX       *****************************************
!
               IF ( m(1)<=0 .OR. m(2)<=0 .OR. m(3)<0 ) GOTO 20
               IF ( ifpdco(m(4)) ) GOTO 20
!HURNB 11/93
               IF ( mf(3)==0 ) THEN
!HURNB 11/93
!
! HID IS BLANK - GENERATE HID FOR THIS SPCAX FOR ALL HARMONICS
!
                  nharms = nns + 1
                  IF ( oneh ) nharms = 1
                  DO il = 1 , nharms
                     n = n + 5
                     i(n-4) = m(1)
                     i(n-3) = m(2)
                     i(n-1) = m(4)
                     i(n) = m(5)
                     i(n-2) = il - 1
                     IF ( oneh ) i(n-2) = nn
                  ENDDO
                  RETURN
               ELSE
!HURNE
                  ASSIGN 20 TO r1
                  ASSIGN 120 TO r
                  ih = m(3)
                  spag_nextblock_1 = 58
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( kx==49 ) THEN
!HURNE
!
!******        149-MPCAX       *****************************************
!
               IF ( m(7)>6 ) baddat = .TRUE.
               IF ( ithrml==1 .AND. m(7)>1 ) baddat = .TRUE.
               IF ( km/=0 ) THEN
                  l1 = 1
                  IF ( m(3)>6 ) baddat = .TRUE.
                  IF ( ithrml==1 .AND. m(3)>1 ) baddat = .TRUE.
                  ASSIGN 160 TO r
               ELSE
                  km = 1
                  nt = 0
!HURNB 11/93
                  blankh = .FALSE.
!HURNE
                  IF ( mf(1)/=1 .OR. mf(2)/=0 .OR. mf(3)/=0 .OR. mf(4)/=0 ) badfor = .TRUE.
                  l1 = 5
!HURNB 11/93
                  IF ( mf(6)==0 ) blankh = .TRUE.
                  IF ( blankh ) CALL gopen(iscr1,ibuff(2*nbuf+1),1)
!HURNE
                  ASSIGN 140 TO r
               ENDIF
               DO l = l1 , 8
                  IF ( mf(l)/=0 ) THEN
                     IF ( l==4 .OR. l==8 ) THEN
                        IF ( mf(l)/=2 ) badfor = .TRUE.
                     ELSE
                        IF ( mf(l)/=1 ) badfor = .TRUE.
                     ENDIF
                  ENDIF
               ENDDO
               GOTO r
            ELSEIF ( kx==50 .OR. kx==51 ) THEN
!
!******        151-SUPAX, 150-OMITAX     *******************************
!
               l = 1
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kx==52 ) THEN
!
!******        152-POINTAX        **************************************
!
               n = 3
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kx==53 ) THEN
!
!******        153-SECTAX         **************************************
!
               n = 5
               IF ( rm(3)<=0 ) GOTO 20
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kx==54 ) THEN
!
!******        154-PRESAX         **************************************
!
               n = 6
               IF ( m(1)<=0 .OR. m(4)<=0 .OR. m(4)==m(3) ) GOTO 20
               IF ( ipiez==1 ) THEN
                  ASSIGN 40 TO r
                  spag_nextblock_1 = 12
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  IF ( m(3)<=0 ) GOTO 20
                  IF ( abs(rm(5))>=abs(rm(6)) .AND. sign(1.,rm(5))==sign(1.,rm(6)) ) GOTO 20
                  ASSIGN 40 TO r
                  spag_nextblock_1 = 12
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( kx==55 ) THEN
!
!******        155-TEMPAX         **************************************
!
               DO l = 1 , 5 , 4
                  IF ( m(l)/=0 .OR. m(l+1)/=0 .OR. m(l+2)/=0 ) THEN
                     IF ( m(l)<=0 .OR. m(l+1)<=0 ) GOTO 20
                     n = n + 4
                     i(n-3) = m(l)
                     i(n-2) = m(l+1)
                     i(n-1) = m(l+2)
                     i(n) = m(l+3)
                  ENDIF
               ENDDO
               IF ( n<=0 ) GOTO 20
               ASSIGN 99999 TO r
               spag_nextblock_1 = 12
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kx==56 .OR. kx==57 ) THEN
!
!******     156-FORCEAX, 157-MOMAX    *******************************
!
               IF ( m(1)<=0 .OR. m(2)<=0 ) GOTO 20
               IF ( mf(3)==2 .OR. mf(3)==4 ) GOTO 20
               IF ( mf(3)/=3 .AND. m(3)<0 ) GOTO 20
               n = 8
               l = 4
               i(1) = m(1)
               i(2) = m(2)
               i(3) = m(3)
               i(4) = 0
               IF ( mf(3)==3 ) i(4) = m(4)
               IF ( mf(3)==3 ) l = 5
               i(5) = m(l)
               i(6) = m(l+1)
               i(7) = m(l+2)
               i(8) = m(l+3)
               RETURN
            ELSEIF ( kx==82 .OR. kx==83 .OR. kx==84 ) THEN
!
!******     182-DAREA, 183-DELAY, 184-DPHASE      *******************
!
               IF ( m(1)<=0 ) GOTO 20
               DO l = 2 , 5 , 3
!HURNB 11/93
!     WRITE(6,10003)L,M(L),M(L+1),M(L+2),N,NNS,(I(IL),IL=1,N)
!0003 FORMAT(7H DAREA0,6I10/(1X,24I5))
!HURNE
                  IF ( m(l)/=0 .OR. m(l+1)/=0 .OR. m(l+2)/=0 ) THEN
                     IF ( m(l)<=0 .OR. m(l+1)<0 .OR. m(l+1)>6 ) GOTO 20
                     n = n + 4
                     i(n-3) = m(1)
                     i(n-2) = m(l)
                     i(n-1) = m(l+1)
                     i(n) = m(l+2)
!HURNB 11/93
                     IF ( iax ) THEN
                        IF ( m(l)<1000000 ) THEN
!
! FOR AXIC PROBLEMS AND GRID ID ON DAREA .LT. 10**6, GENERATE DAREAS FOR
! HARMONICS, COMPUTING THE GRID ID.  ASSUME  PRESSURE VALUE IS GIVEN FOR
! ZERO HARMONIC; FOR HIGHER HARMONICS, HALVE IT.
!
                           nharms = nns + 1
                           IF ( oneh ) nharms = 1
                           DO il = 1 , nharms
                              ill = il
                              IF ( nns<0 .OR. il/=1 ) THEN
                                 IF ( il>1 ) THEN
                                    n = n + 4
                                    i(n-3) = m(1)
                                    i(n-1) = m(l+1)
                                 ELSE
!
! NNS.LT.0 .AND. IL.EQ.1
!
                                    ill = nn + 1
                                 ENDIF
                                 xin = 0.5*rm(l+2)
                                 i(n) = ixin
                              ENDIF
                              i(n-2) = m(l) + 1000000*ill
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
               IF ( n<=0 ) GOTO 20
               RETURN
            ELSEIF ( kx==85 ) THEN
!
!*****      143-DSFACT(1430), 185-PLFACT(1420)     ********************
!
               IF ( lplf<0 ) GOTO 20
               IF ( lplf==0 ) THEN
                  lplf = 1
                  iddsf = 0
               ENDIF
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         IF ( ky<=100 ) THEN
            IF ( ky==1 .OR. ky==2 .OR. ky==3 .OR. ky==4 .OR. ky==5 .OR. ky==6 .OR. ky==7 .OR. ky==8 .OR. ky==9 .OR. ky==10 .OR.     &
               & ky==11 .OR. ky==12 .OR. ky==13 .OR. ky==14 .OR. ky==15 .OR. ky==17 .OR. ky==18 .OR. ky==19 .OR. ky==20 .OR.        &
               & ky==21 .OR. ky==22 .OR. ky==23 .OR. ky==24 .OR. ky==25 .OR. ky==26 .OR. ky==27 .OR. ky==28 .OR. ky==29 .OR.        &
               & ky==30 .OR. ky==31 .OR. ky==32 .OR. ky==33 .OR. ky==34 .OR. ky==35 .OR. ky==36 .OR. ky==37 .OR. ky==38 .OR.        &
               & ky==39 .OR. ky==40 .OR. ky==41 .OR. ky==42 .OR. ky==43 .OR. ky==45 .OR. ky==46 .OR. ky==47 .OR. ky==48 .OR.        &
               & ky==49 .OR. ky==50 .OR. ky==51 .OR. ky==52 .OR. ky==53 .OR. ky==54 .OR. ky==55 .OR. ky==56 .OR. ky==57 .OR.        &
               & ky==58 .OR. ky==59 .OR. ky==60 .OR. ky==61 .OR. ky==62 .OR. ky==63 .OR. ky==64 .OR. ky==65 .OR. ky==66 .OR.        &
               & ky==67 .OR. ky==68 .OR. ky==69 .OR. ky==70 .OR. ky==71 .OR. ky==72 .OR. ky==75 .OR. ky==76 .OR. ky==77 .OR.        &
               & ky==78 .OR. ky==80 .OR. ky==81 .OR. ky==82 .OR. ky==83 .OR. ky==89 .OR. ky==91 .OR. ky==92 .OR. ky==93 .OR.        &
               & ky==94 .OR. ky==95 .OR. ky==96 .OR. ky==99 .OR. ky==100 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( ky==16 ) THEN
               spag_nextblock_1 = 50
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( ky==44 ) THEN
!
!******        244-RADMTX     *****************************************
!
               IF ( km==1 ) THEN
                  l1 = 1
               ELSE
                  km = 1
                  IF ( mf(1)/=1 ) badfor = .TRUE.
                  id = m(1)
                  IF ( id<=idrad ) baddat = .TRUE.
                  idrad = id
                  i(1) = id
                  n = 1
                  l1 = 2
               ENDIF
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( ky==73 .OR. ky==74 ) THEN
!
!*****      273-AEFACT , 274-FLFACT    ********************************
!
               IF ( km==1 ) THEN
                  l1 = 1
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  km = 1
                  IF ( mf(1)/=1 ) badfor = .TRUE.
                  IF ( m(1)<=0 ) baddat = .TRUE.
                  i(1) = m(1)
                  n = 1
                  l1 = 2
                  IF ( mf(3)/=3 ) THEN
                     spag_nextblock_1 = 6
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( m(3)/=thru .OR. m(4)/=blnk ) baddat = .TRUE.
                  IF ( mf(2)/=2 .OR. mf(4)/=2 .OR. mf(5)/=1 .OR. mf(6)/=2 ) badfor = .TRUE.
                  IF ( m(6)<=1 ) baddat = .TRUE.
                  IF ( m(5)==m(2) ) baddat = .TRUE.
                  imid = 0
                  IF ( rm(5)-rm(7)>=0. .AND. rm(7)-rm(2)<0. ) imid = 1
                  IF ( rm(5)-rm(7)<=0. .AND. rm(7)-rm(2)>0. ) imid = 1
                  IF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
                     IF ( badfor .OR. baddat ) THEN
                        spag_nextblock_1 = 7
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( imid/=0 ) THEN
                        rm(7) = 0.5*(rm(2)+rm(5))
                        CALL page2(3)
                        WRITE (nout,99006) uwm , i(1)
99006                   FORMAT (A25,' 528, FACTOR FMID IN FLFACT SET',I9,' DOES NOT LIE ','BETWEEN F1 AND FNF.',/5X,                &
                               &'IT IS BEING RESET TO (F1 + ','FNF)/2.0')
                     ENDIF
                     t4(2,k) = t4(2,k) + 1
                     CALL write(204,i,1,0)
                     l = 1
                     DO
                        term1 = (m(6)-l)*(rm(5)-rm(7))
                        term2 = (l-1)*(rm(7)-rm(2))
                        anum = rm(2)*term1 + rm(5)*term2
                        den = term1 + term2
                        factor = anum/den
                        t4(2,k) = t4(2,k) + 1
                        CALL write(204,factor,1,0)
                        l = l + 1
                        IF ( l>m(6) ) THEN
                           i(1) = -1
                           t4(2,k) = t4(2,k) + 1
                           CALL write(204,i,1,0)
                           n = 0
                           km = 0
                           kn = 0
                           spag_nextblock_1 = 4
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                     ENDDO
                  ELSE
                     badfor = .TRUE.
                     kn = 1
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
            ELSEIF ( ky==79 ) THEN
!
!******        279-CRIGD1         **********************************
!
               kn = 1
               IF ( irigid==2 ) THEN
                  n = 0
                  irg = 1
                  spag_nextblock_1 = 14
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  irigid = irigid + 1
                  IF ( mf(1)/=1 ) badfor = .TRUE.
                  IF ( m(1)<=0 ) baddat = .TRUE.
                  i(1) = m(1)
                  n = 2
                  IF ( mf(2)/=1 ) badfor = .TRUE.
                  IF ( m(2)<1 ) baddat = .TRUE.
                  i(2) = m(2)
                  IF ( mf(4)/=3 ) THEN
                     irg = 3
                     spag_nextblock_1 = 14
                     CYCLE SPAG_DispatchLoop_1
                  ELSEIF ( m(4)/=thru .OR. m(5)/=blnk ) THEN
                     baddat = .TRUE.
                     IF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
                        spag_nextblock_1 = 15
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     badfor = .TRUE.
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ELSEIF ( m1(1)==0 .AND. m1(2)==0 ) THEN
                     badfor = .TRUE.
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     IF ( mf(3)/=1 .OR. mf(5)/=1 ) badfor = .TRUE.
                     IF ( m(3)<=0 .OR. m(6)<=0 ) baddat = .TRUE.
                     IF ( m(6)<=m(3) ) baddat = .TRUE.
                     DO l = 6 , 8
                        IF ( mf(l)/=0 ) badfor = .TRUE.
                     ENDDO
                     IF ( badfor .OR. baddat ) THEN
                        spag_nextblock_1 = 15
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     t4(2,k) = t4(2,k) + 2
                     CALL write(210,m,2,0)
                     l = m(3)
                     DO
                        i(1) = l
                        DO j = 1 , 6
                           i(j+1) = j
                        ENDDO
                        t4(2,k) = t4(2,k) + 7
                        CALL write(210,i,7,0)
                        l = l + 1
                        IF ( l>m(6) ) THEN
                           irigid = 1
                           DO j = 1 , 7
                              i(j) = -1
                           ENDDO
                           IF ( m1(1)==arigid .AND. m1(2)==brigid ) i(2) = 0
                           n = 0
                           kn = 0
                           t4(2,k) = t4(2,k) + 7
                           CALL write(210,i,7,0)
                           spag_nextblock_1 = 4
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                     ENDDO
                  ENDIF
               ENDIF
            ELSEIF ( ky==84 ) THEN
!
!******        284-CRIGD2        **********************************
!
               kn = 1
               IF ( irigid==2 ) THEN
                  n = 0
                  irg = 1
               ELSE
                  irigid = irigid + 1
                  IF ( mf(1)/=1 ) badfor = .TRUE.
                  IF ( m(1)<=0 ) baddat = .TRUE.
                  i(1) = m(1)
                  n = 2
                  IF ( mf(2)/=1 ) badfor = .TRUE.
                  IF ( m(2)<1 ) baddat = .TRUE.
                  i(2) = m(2)
                  irg = 3
               ENDIF
               DO l = irg , 8 , 2
                  l1 = l
                  IF ( m(l)<=0 ) THEN
                     spag_nextblock_1 = 18
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( m(l+1)<=0 ) baddat = .TRUE.
                  IF ( mf(l)/=1 .OR. mf(l+1)/=1 ) badfor = .TRUE.
                  i(n+1) = m(l)
                  IF ( ifpdco(m(l+1)) ) baddat = .TRUE.
                  DO j = 1 , 6
                     i(n+1+j) = ll(j)
                  ENDDO
                  n = n + 7
               ENDDO
               IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               spag_nextblock_1 = 17
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( ky==85 ) THEN
!
!******        285-CTRIAAX       ***************************************
!
               IF ( m(1)<=0 .OR. m(2)<=0 ) GOTO 20
               IF ( m(3)<=0 .OR. m(4)<=0 ) GOTO 20
               IF ( m(3)==m(4) ) GOTO 20
               IF ( m(3)==m(5) ) GOTO 20
               ih = nn
               ASSIGN 20 TO r1
               ASSIGN 300 TO r
               spag_nextblock_1 = 58
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( ky==86 .OR. ky==88 ) THEN
!
!******       286-PTRIAX, 288-PTRAPAX   *******************************
!
               IF ( m(1)<=0 ) GOTO 20
               ih = nn
               ASSIGN 20 TO r1
               ASSIGN 320 TO r
               spag_nextblock_1 = 58
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( ky==87 ) THEN
!
!*******       287-CTRAPAX             ********************************
!
               IF ( m(1)<=0 .OR. m(2)<=0 ) GOTO 20
               IF ( m(3)==m(4) ) GOTO 20
               IF ( m(3)==m(5) ) GOTO 20
               ih = nn
               ASSIGN 20 TO r1
               ASSIGN 340 TO r
               spag_nextblock_1 = 58
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( ky==90 ) THEN
!
!******        290-VARIAN        **************************************
!
               IF ( km==1 ) THEN
                  l1 = 1
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  km = 1
                  IF ( nvar/=0 ) GOTO 20
                  nvar = 1
                  l1 = 1
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( ky==97 ) THEN
!
!******         297-CRIGDR      *************************************
!
               DO l = 1 , 5 , 4
                  IF ( m(l)/=0 .OR. m(l+1)/=0 .OR. m(l+2)/=0 .OR. m(l+3)/=0 ) THEN
                     IF ( m(l)<=0 .OR. m(l+1)<=0 .OR. m(l+2)<=0 .OR. m(l+3)<=0 ) GOTO 20
                     IF ( m(l+1)==m(l+2) ) GOTO 20
                     IF ( m(l+3)>3 ) THEN
                        WRITE (nout,99022) ufm , blnk , m(l) , knt
                        baddat = .TRUE.
                     ELSE
                        n = n + 4
                        IF ( n>4 .AND. m(l)==m(l-4) ) GOTO 20
                        i(n-3) = m(l)
                        i(n-2) = m(l+1)
                        i(n-1) = m(l+2)
                        i(n) = m(l+3)
                     ENDIF
                  ENDIF
               ENDDO
               IF ( n<=0 ) GOTO 20
               RETURN
            ELSEIF ( ky==98 ) THEN
               spag_nextblock_1 = 19
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         kz = ky - 100
         IF ( kz<=53 ) THEN
            IF ( .NOT.(kz<47 .OR. kz>51 .OR. .NOT.first) ) THEN
               first = .FALSE.
               IF ( prt ) THEN
                  CALL page1
                  WRITE (nout,99007) uim
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
               IF ( km/=0 ) THEN
                  l1 = 1
               ELSE
                  IF ( prol ) baddat = .TRUE.
                  prol = .TRUE.
                  km = 1
                  IF ( mf(1)/=2 .OR. mf(2)/=2 ) badfor = .TRUE.
                  IF ( rm(1)<=rm(2) ) baddat = .TRUE.
                  DO l = 3 , 6
                     IF ( mf(l)/=1 ) badfor = .TRUE.
                     IF ( m(l)<0 ) baddat = .TRUE.
                  ENDDO
                  IF ( m(3)<2 ) baddat = .TRUE.
                  IF ( m(4)<2 ) baddat = .TRUE.
                  IF ( m(5)>30 ) baddat = .TRUE.
                  IF ( m(6)>m(5) ) m(6) = m(5)
                  id = m(1)
                  nsegs = m(3)
                  msegs = m(4)
                  itot1 = (nsegs-1)*(msegs+1) + 2
                  itot2 = (nsegs-1)*msegs + 2
                  DO l = 1 , 6
                     i(l) = m(l)
                  ENDDO
                  n = 6
                  l1 = 7
                  items = 0
               ENDIF
               DO l = l1 , 8
                  IF ( mf(l)/=1 .AND. mf(l)/=3 ) badfor = .TRUE.
                  IF ( mf(l)==3 ) THEN
                     spag_nextblock_1 = 62
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  items = items + 1
                  IF ( m(l)<=0 ) baddat = .TRUE.
                  n = n + 1
                  i(n) = m(l)
               ENDDO
               kn = 1
               spag_nextblock_1 = 61
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kz==30 ) THEN
!
!******      330-PERMBDY       *****************************************
!
               IF ( km==0 ) THEN
                  IF ( perm ) baddat = .TRUE.
                  perm = .TRUE.
                  km = 1
               ENDIF
               DO l = 1 , 8
                  IF ( mf(l)/=1 .AND. mf(l)/=3 ) badfor = .TRUE.
                  IF ( mf(l)==3 ) THEN
                     spag_nextblock_1 = 62
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( m(l)<=0 ) baddat = .TRUE.
                  n = n + 1
                  i(n) = m(l)
               ENDDO
               kn = 1
               spag_nextblock_1 = 61
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kz==32 ) THEN
               spag_nextblock_1 = 49
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kz==45 ) THEN
!
!******        345-STREAML1      **************************************
!
               IF ( km==1 ) THEN
                  l1 = 1
                  spag_nextblock_1 = 46
               ELSE
                  km = 1
                  IF ( mf(1)/=1 ) badfor = .TRUE.
                  IF ( m(1)<=0 ) baddat = .TRUE.
                  IF ( m(1)<=0 ) baddat = .TRUE.
                  i(1) = m(1)
                  n = 1
                  IF ( mf(3)/=3 .OR. m(3)/=thru ) THEN
                     l1 = 2
                     spag_nextblock_1 = 46
                  ELSEIF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
                     IF ( mf(2)/=1 .OR. mf(4)/=1 ) badfor = .TRUE.
                     IF ( m(2)<=0 .OR. m(5)<=0 .OR. (m(2)>m(5)) ) baddat = .TRUE.
                     IF ( .NOT.(badfor .OR. baddat) ) THEN
                        CALL write(204,i,n,0)
                        l1 = m(2)
                        l2 = m(5)
                        DO l = l1 , l2
                           CALL write(204,l,1,0)
                        ENDDO
                        n = 0
                     ENDIF
                     spag_nextblock_1 = 47
                  ELSE
                     kn = 1
                     badfor = .TRUE.
                     spag_nextblock_1 = 4
                  ENDIF
               ENDIF
               CYCLE
            ELSEIF ( kz==46 ) THEN
!
!******        346-STREAML2      **************************************
!
!     THEORY DEPENDENT RESTRICTION -  (3.GE. NSTNS .LE.10)
!
               IF ( m(1)<=0 ) GOTO 20
               IF ( m(2)<3 .OR. m(2)>10 ) GOTO 20
               IF ( rm(4)<=0.0 ) GOTO 20
               DO l = 6 , 9
                  IF ( rm(l)<=0.0 ) GOTO 20
               ENDDO
               IF ( rm(3)<=-90.0 .OR. rm(3)>=90.0 ) GOTO 20
               IF ( rm(10)<=-90.0 .OR. rm(10)>=90.0 ) GOTO 20
               n = 10
               GOTO 40
            ELSEIF ( kz==47 ) THEN
!
!******       347-CRROD        *****************************************
!
!     MAP THIS RIGID ELEMENT INTO CRIGID3 FORM
!
               IF ( mf(1)+mf(2)+mf(3)/=3 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( m(1)<=0 .OR. m(2)<=0 .OR. m(3)<=0 ) GOTO 20
               IF ( m(2)==m(3) ) GOTO 20
               IF ( m(4)<0 .OR. m(5)<0 ) THEN
                  spag_nextblock_1 = 32
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               l = m(4) + m(5)
               IF ( l<1 .OR. l>3 ) THEN
                  spag_nextblock_1 = 32
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( m(4)/=0 .AND. m(5)/=0 ) THEN
                  spag_nextblock_1 = 32
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( prt ) THEN
                  CALL page2(3)
                  IF ( m(4)/=0 ) WRITE (nout,99008) scc , knt , (m(j),j=1,4) , gcc , m(1) , m(3) , m(2) , m(4)
99008             FORMAT (/25X,A19,I7,1H-,5X,'CRROD ',4I8,/25X,A19,13X,'CRIGDR',4I8)
                  IF ( m(4)==0 ) WRITE (nout,99009) scc , knt , (m(j),j=1,3) , m(5) , gcc , (m(j),j=1,3) , m(5)
99009             FORMAT (/25X,A19,I7,1H-,5X,'CRROD ',3I8,8X,I8,/25X,A19,13X,'CRIGDR',4I8)
               ENDIF
               l = m(3)
               IF ( m(4)/=0 ) THEN
                  l = m(2)
                  m(2) = m(3)
                  m(3) = l
                  m(5) = m(4)
               ENDIF
               m(4) = m(5)
               n = 4
               GOTO 40
            ELSEIF ( kz==48 ) THEN
!
!******       348-CRBAR        *****************************************
!
!     MAP THIS RIGID ELEMENT INTO CRIGD3 FORM
!
               IF ( mf(1)+mf(2)+mf(3)/=3 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( m(1)<=0 .OR. m(2)<=0 .OR. m(3)<=0 ) GOTO 20
               IF ( m(2)==m(3) ) GOTO 20
               rbe = .FALSE.
               IF ( m(6)==0 .AND. m(7)==0 ) rbe = .TRUE.
               IF ( m(4)==0 .AND. m(5)==0 ) THEN
!
                  WRITE (nout,99022) ufm , ind , m(1) , knt1
                  GOTO 20
               ELSEIF ( ifpdco(m(4)) ) THEN
                  WRITE (nout,99022) ufm , ind , m(1) , knt1
                  GOTO 20
               ELSE
                  lk = 1
                  DO l = 1 , 6
                     lll = ll(l)
                     IF ( rbe .AND. lll==0 ) m(6) = m(6) + l*lk
                     IF ( lll==0 ) lk = lk*10
                     ia(l) = lll
                  ENDDO
                  IF ( ifpdco(m(5)) ) THEN
                     WRITE (nout,99022) ufm , ind , m(1) , knt1
                     GOTO 20
                  ELSE
                     lk = 1
                     DO l = 1 , 6
                        lll = ll(l)
                        IF ( rbe .AND. lll==0 ) m(7) = m(7) + l*lk
                        IF ( lll==0 ) lk = lk*10
                        ib(l) = lll
                     ENDDO
                     IF ( rbe ) THEN
                        spag_nextblock_1 = 26
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( ifpdco(m(6)) ) THEN
                        spag_nextblock_1 = 32
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     DO l = 1 , 6
                        IF ( ia(l)/=0 ) THEN
                           IF ( ia(l)==ll(l) ) THEN
                              spag_nextblock_1 = 32
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                        ENDIF
                        ja(l) = ll(l)
                     ENDDO
                     IF ( ifpdco(m(7)) ) THEN
                        spag_nextblock_1 = 32
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     DO l = 1 , 6
                        IF ( ib(l)/=0 ) THEN
                           IF ( ib(l)==ll(l) ) THEN
                              spag_nextblock_1 = 32
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                        ENDIF
                        jb(l) = ll(l)
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
               IF ( mf(1)+mf(2)+mf(3)+mf(4)/=4 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( m(1)<=0 .OR. m(2)<=0 .OR. m(3)<=0 .OR. m(4)<=0 ) GOTO 20
               IF ( m(2)==m(3) .OR. m(2)==m(4) .OR. m(3)==m(4) ) GOTO 20
               IF ( m(5)==0 .AND. m(6)==0 .AND. m(7)==0 ) THEN
                  WRITE (nout,99022) ufm , ind , m(1) , knt1
                  GOTO 20
               ELSE
                  rbe = .FALSE.
                  IF ( m(9)==0 .AND. m(10)==0 .AND. m(11)==0 ) rbe = .TRUE.
                  IF ( ifpdco(m(5)) ) THEN
                     WRITE (nout,99022) ufm , ind , m(1) , knt1
                     GOTO 20
                  ELSE
                     lk = 1
                     DO l = 1 , 6
                        lll = ll(l)
                        IF ( rbe .AND. lll==0 ) m(9) = m(9) + l*lk
                        IF ( lll==0 ) lk = lk*10
                        ia(l) = lll
                     ENDDO
                     IF ( ifpdco(m(6)) ) THEN
                        WRITE (nout,99022) ufm , ind , m(1) , knt1
                        GOTO 20
                     ELSE
                        lk = 1
                        DO l = 1 , 6
                           lll = ll(l)
                           IF ( rbe .AND. lll==0 ) m(10) = m(10) + l*lk
                           IF ( lll==0 ) lk = lk*10
                           ib(l) = lll
                        ENDDO
                        IF ( ifpdco(m(7)) ) THEN
                           WRITE (nout,99022) ufm , ind , m(1) , knt1
                           GOTO 20
                        ELSE
                           lk = 1
                           DO l = 1 , 6
                              lll = ll(l)
                              IF ( rbe .AND. lll==0 ) m(11) = m(11) + l*lk
                              IF ( lll==0 ) lk = lk*10
                              ic(l) = lll
                           ENDDO
                           IF ( rbe ) THEN
                              spag_nextblock_1 = 28
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( ifpdco(m(9)) ) THEN
                              spag_nextblock_1 = 32
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           DO l = 1 , 6
                              IF ( ia(l)/=0 ) THEN
                                 IF ( ia(l)==ll(l) ) THEN
                                    spag_nextblock_1 = 32
                                    CYCLE SPAG_DispatchLoop_1
                                 ENDIF
                              ENDIF
                              ja(l) = ll(l)
                           ENDDO
                           IF ( ifpdco(m(10)) ) THEN
                              spag_nextblock_1 = 32
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           DO l = 1 , 6
                              IF ( ib(l)/=0 ) THEN
                                 IF ( ib(l)==ll(l) ) THEN
                                    spag_nextblock_1 = 32
                                    CYCLE SPAG_DispatchLoop_1
                                 ENDIF
                              ENDIF
                              jb(l) = ll(l)
                           ENDDO
                           IF ( ifpdco(m(11)) ) THEN
                              spag_nextblock_1 = 32
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           DO l = 1 , 6
                              IF ( ic(l)/=0 ) THEN
                                 IF ( ic(l)==ll(l) ) THEN
                                    spag_nextblock_1 = 32
                                    CYCLE SPAG_DispatchLoop_1
                                 ENDIF
                              ENDIF
                              jc(l) = ll(l)
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
               kn = 1
               IF ( irigid==2 ) THEN
                  n = 0
                  irg = 1
                  spag_nextblock_1 = 29
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  irigid = irigid + 1
                  knt1 = knt
                  l6 = 60
                  l7 = l6
                  l8 = 0
                  IF ( mf(1)+mf(2)+mf(3)/=3 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( m(1)<=0 .OR. m(2)<=0 ) GOTO 20
                  i(1) = m(1)
                  i(2) = m(2)
                  q(1) = m(1)
                  q(2) = m(2)
                  m3 = m(3)
                  l8 = l8 + 2
                  q(l7+1) = m(1)
                  q(l7+2) = m(2)
                  q(l7+3) = m3
                  l7 = l7 + 3
                  n = 2
                  irg = 4
                  IF ( ifpdco(m3) ) baddat = .TRUE.
                  IF ( m3==0 ) baddat = .TRUE.
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
               IF ( km==0 ) THEN
                  km = 1
                  im = 1
                  IF ( mf(1)+mf(3)+mf(4)/=3 ) badfor = .TRUE.
                  IF ( m(1)<=0 .OR. m(3)<=0 .OR. m(4)<=0 ) baddat = .TRUE.
                  IF ( ifpdco(m(4)) ) baddat = .TRUE.
                  IF ( mf(5)/=2 ) baddat = .TRUE.
                  i(1) = m(1)
                  i(2) = m(3)
                  i(3) = m(4)
!
! ... NOTE - COMPONENTS IN LL NOT SENT OUT IN CRBE3
!
                  n = 3
                  l1 = 5
!
!
               ELSEIF ( mf(1)==3 ) THEN
                  IF ( m(1)/=ium ) baddat = .TRUE.
                  i(n+1) = -1
                  i(n+2) = -2
                  n = n + 2
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
                  IF ( mf(l)==2 ) THEN
                     IF ( l1/=5 ) THEN
                        n = n + 1
                        i(n) = -1
                     ENDIF
                     im = 1
!WKBI 11/93 SPR93018
                     l1 = 1
                     n = n + 1
                     i(n) = m(l)
                  ELSEIF ( mf(l)/=0 ) THEN
                     IF ( mf(l)/=1 .OR. m(l)<=0 ) baddat = .TRUE.
                     IF ( im/=-1 ) THEN
                        IF ( ifpdco(m(l)) ) baddat = .TRUE.
                     ENDIF
                     im = -1
                     n = n + 1
                     i(n) = m(l)
                  ENDIF
               ENDDO
               IF ( m1(1)/=0 ) THEN
                  n = n + 1
                  i(n) = -1
               ELSE
                  kn = 1
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               spag_nextblock_1 = 33
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kz==53 ) THEN
!
!******    353-CRSPLINE      *******************************************
!
               IF ( km/=0 ) THEN
                  l1 = 1
                  IF ( im==-9 ) THEN
                     spag_nextblock_1 = 38
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  spag_nextblock_1 = 35
               ELSE
                  km = 1
                  im = -1
                  IF ( mf(1)/=1 .OR. m(1)<=0 ) THEN
                     spag_nextblock_1 = 38
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( mf(2)==0 ) rm(2) = .1
                  IF ( rm(2)<=0. ) THEN
                     spag_nextblock_1 = 38
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( mf(3)/=1 .OR. m(3)<=0 ) THEN
                     spag_nextblock_1 = 38
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  i(1) = m(1)
                  i(2) = m(2)
                  i(3) = m(3)
                  n = 3
                  l1 = 4
                  spag_nextblock_1 = 35
               ENDIF
               CYCLE
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         CALL page2(2)
         WRITE (nout,99010) sfm
99010    FORMAT (A25,' 322, ILLEGAL ENTRY TO IFS3P.')
         abort = .TRUE.
         RETURN 1
      CASE (3)
         badfor = .TRUE.
         RETURN 1
 20      baddat = .TRUE.
         RETURN 1
 40      DO l = 1 , n
            i(l) = m(l)
         ENDDO
         spag_nextblock_1 = 4
      CASE (4)
         RETURN 3
      CASE (5)
         IF ( km==1 ) THEN
            l1 = 1
         ELSE
            km = 1
            IF ( mf(1)/=1 ) badfor = .TRUE.
            id = m(1)
            IF ( id<=iddsf ) baddat = .TRUE.
            iddsf = id
            i(1) = id
            IF ( mf(2)/=2 ) badfor = .TRUE.
            n = 2
            l1 = 3
            i(n) = m(2)
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
         DO l = l1 , 8
            IF ( mf(l)==0 ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( mf(l)/=2 ) badfor = .TRUE.
            n = n + 1
            i(n) = m(l)
         ENDDO
         IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
            kn = 1
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
         km = 0
         n = n + 1
         i(n) = -1
         kn = 0
         spag_nextblock_1 = 4
      CASE (8)
         IF ( l==1 ) badfor = .TRUE.
         DO l2 = l , 8
            IF ( mf(l2)/=0 ) badfor = .TRUE.
         ENDDO
         IF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         badfor = .TRUE.
         kn = 1
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 60      IF ( ifpdco(m(7)) ) GOTO 20
         n = 4
         i(1) = m(1)
         i(2) = m(3)
         i(3) = m(4)
         i(4) = m(7)
         RETURN
 80      n = 4
         GOTO 40
 100     n = 24
         GOTO 40
 120     n = 5
         GOTO 40
 140     IF ( m(1)<=0 ) baddat = .TRUE.
         id = m(1)
         n = 1
         i(n) = id
         ih = nn
         ASSIGN 180 TO r
         ASSIGN 20 TO r1
         spag_nextblock_1 = 58
         CYCLE SPAG_DispatchLoop_1
 160     n = 0
 180     DO l = l1 , 5 , 4
            IF ( m(l)/=0 .OR. m(l+1)/=0 .OR. m(l+2)/=0 .OR. m(l+3)/=0 ) THEN
               IF ( m(l)<=0 .OR. m(l+1)<0 .OR. m(l+2)<0 .OR. m(l+3)==0 .AND. l1==5 ) baddat = .TRUE.
!HURNB 11/93
               IF ( blankh .AND. l1==1 .AND. mf(l+1)/=0 ) badfor = .TRUE.
               IF ( .NOT.blankh .AND. l1==1 .AND. mf(l+1)==0 ) badfor = .TRUE.
!HURNE
               n = n + 4
               i(n-3) = m(l)
               i(n-2) = m(l+1)
               i(n-1) = m(l+2)
               i(n) = m(l+3)
            ENDIF
         ENDDO
         nt = nt + n
         IF ( n<4 ) baddat = .TRUE.
         kn = 1
!HURNB 11/93
         IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
            IF ( .NOT.blankh ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL write(iscr1,i,n,0)
!      WRITE(6,10005)N,(I(IL),IL=1,N)
!10005 FORMAT(6H MPCAX,6I5)
            n = 0
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
!HURNE
            IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!HURNB 11/93
!HURNE
         n = n + 4
         i(n-3) = -1
         i(n-2) = -1
         i(n-1) = -1
         i(n) = -1
         kn = 0
         km = 0
         IF ( nt<9 ) baddat = .TRUE.
!HURNB 11/93
         IF ( .NOT.blankh ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
! MPCAX CARD DONE - GENERATE CARDS FOR ALL HARMONICS ASSUMING THE ONE JU
! STORED (WITH BLANK HARMONIC) IS FOR THE ZERO HARMONIC
!
         IF ( nt>nopen ) CALL mesage(-8,0,nam)
         CALL write(iscr1,i,n-4,1)
!     WRITE(6,10006)N,(I(IL),IL=1,N)
!0006 FORMAT(7H MPCAX1,10I5)
         CALL close(iscr1,1)
         CALL gopen(iscr1,ibuff(2*nbuf+1),0)
         CALL read(*200,*220,iscr1,ibuff(3*nbuf+1),nopen,0,nnt)
 200     CALL mesage(-8,0,nam)
 220     CALL close(iscr1,1)
!     WRITE(6,10007)NT,NNT,(IBUFF(3*NBUF+IL),IL=1,NNT)
!0007 FORMAT(7H MPCAX2,10I5)
         IF ( nt/=nnt ) CALL mesage(-61,0,0)
!
! ALL MPCAX CARD INFO FOR THIS CARD IS READ IN. GENERATE FOR ALL HARMONI
!
         nharms = nns + 1
         IF ( oneh ) nharms = 1
         DO l = 1 , nharms
            ill = l - 1
            IF ( oneh ) ill = iabs(nns)
            DO il = 3 , nt , 4
               ibuff(3*nbuf+il) = ill
            ENDDO
            t4(2,k) = t4(2,k) + nt
            CALL write(215,ibuff(3*nbuf+1),nt,0)
            t4(2,k) = t4(2,k) + 4
            CALL write(215,iones,4,0)
         ENDDO
!HURNE
         n = 0
         spag_nextblock_1 = 4
      CASE (9)
         IF ( m(l)==0 .AND. m(l+1)==0 .AND. m(l+2)==0 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( m(l)<=0 .OR. m(l+1)<0 ) GOTO 20
         IF ( ifpdco(m(l+2)) ) GOTO 20
         ASSIGN 240 TO r
         ASSIGN 20 TO r1
         ih = m(l+1)
         spag_nextblock_1 = 58
         CYCLE SPAG_DispatchLoop_1
 240     n = n + 3
         IF ( n>3 .AND. m(l)==m(l-3) .AND. m(l-1)==m(l-4) .AND. m(l-2)==m(l-5) ) GOTO 20
         i(n-2) = m(l)
         i(n-1) = m(l+1)
         i(n) = m(l+2)
         spag_nextblock_1 = 10
      CASE (10)
         l = l + 3
         IF ( l==4 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( n<=0 ) GOTO 20
         RETURN
      CASE (11)
         IF ( m(1)<=0 .OR. m(2)<=0 ) GOTO 20
         ASSIGN 40 TO r
         spag_nextblock_1 = 12
      CASE (12)
         ih = nn
         ASSIGN 20 TO r1
         spag_nextblock_1 = 58
         CYCLE SPAG_DispatchLoop_1
 260     IF ( m(1)<=0 ) baddat = .TRUE.
         id = m(1)
         IF ( m(2)<=0 .OR. m(3)<0 .OR. m(4)==0 ) baddat = .TRUE.
         IF ( ids==id .AND. jms==m(2) .AND. kms==m(3) ) baddat = .TRUE.
         ids = id
         jms = m(2)
         kms = m(3)
         n = 4
         DO l = 1 , 4
            i(l) = m(l)
         ENDDO
         spag_nextblock_1 = 13
         CYCLE SPAG_DispatchLoop_1
 280     n = 0
         IF ( m(2)/=0 .OR. m(3)/=0 .OR. m(4)/=0 ) THEN
            IF ( m(2)<=0 .OR. m(3)<0 ) baddat = .TRUE.
            n = 3
            DO l = 2 , 4
               i(l-1) = m(l)
            ENDDO
         ENDIF
         spag_nextblock_1 = 13
      CASE (13)
         IF ( m(5)/=0 .OR. m(6)/=0 .OR. m(7)/=0 ) THEN
            IF ( m(5)<=0 .OR. m(6)<0 ) baddat = .TRUE.
            n = n + 3
            i(n-2) = m(5)
            i(n-1) = m(6)
            i(n) = m(7)
         ENDIF
         IF ( n<=0 ) baddat = .TRUE.
         nt = nt + n
         DO l = 1 , 8
            m(l) = 0
         ENDDO
         kn = 1
         IF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
            n = n + 3
            i(n-2) = -1
            i(n-1) = -1
            i(n) = -1
            kn = 0
            km = 0
            IF ( nt<7 ) baddat = .TRUE.
         ENDIF
         spag_nextblock_1 = 4
      CASE (14)
         DO l = irg , 8
            l1 = l
            IF ( m(l)<=0 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( mf(l)/=1 ) badfor = .TRUE.
            i(n+1) = m(l)
            DO j = 1 , 6
               i(n+1+j) = j
            ENDDO
            n = n + 7
         ENDDO
         IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 15
      CASE (15)
         irigid = 1
         DO j = 1 , 7
            i(n+j) = -1
         ENDDO
         IF ( m1(1)==arigid .AND. m1(2)==brigid ) i(n+2) = 0
         n = n + 7
         kn = 0
         spag_nextblock_1 = 4
      CASE (16)
         DO lk = l1 , 8
            IF ( m(lk)/=0 ) baddat = .TRUE.
            IF ( mf(lk)/=0 ) badfor = .TRUE.
         ENDDO
         IF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         badfor = .TRUE.
         spag_nextblock_1 = 4
      CASE (17)
         irigid = 1
         DO j = 1 , 7
            i(n+j) = -1
         ENDDO
         IF ( m1(1)==arigid .AND. m1(2)==crigid ) i(n+2) = 0
         n = n + 7
         kn = 0
         spag_nextblock_1 = 4
      CASE (18)
         IF ( m1(1)==0 .AND. m1(2)==0 ) baddat = .TRUE.
         DO lk = l1 , 8
            IF ( m(lk)/=0 ) baddat = .TRUE.
            IF ( mf(lk)/=0 ) badfor = .TRUE.
         ENDDO
         spag_nextblock_1 = 17
      CASE (19)
!
!******      298-CRIGD3, 350-CRBE1       ******************************
!
         kn = 1
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
            IF ( mf(1)/=0 .OR. mf(2)/=1 .OR. mf(3)/=1 ) badfor = .TRUE.
            IF ( m(1)/=0 .OR. m(2)<1 .OR. m(3)<1 ) baddat = .TRUE.
            n = 0
         ELSE
            irigid = 2
            jrigid = 1
            knt1 = knt
            l1 = 2
            l2 = 6
            l6 = 0
            IF ( mf(1)/=1 .OR. mf(2)/=1 .OR. mf(3)/=1 ) badfor = .TRUE.
            IF ( m(1)<1 .OR. m(2)<1 .OR. m(3)<1 ) baddat = .TRUE.
            n = 1
            i(1) = m(1)
            q(1) = m(1)
            l8 = 1
            ncomp = 0
         ENDIF
         spag_nextblock_1 = 20
      CASE (20)
         l5 = l2 + 2
         DO l = l1 , l2 , 2
            l3 = l + 1
            IF ( mf(l-l6)==0 ) THEN
               spag_nextblock_1 = 22
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( mf(l-l6)/=1 .OR. mf(l-l6+1)/=1 ) badfor = .TRUE.
            IF ( m(l)<1 .OR. m(l+1)<1 ) baddat = .TRUE.
            IF ( prt ) THEN
               q(l8+1) = m(l)
               q(l8+2) = m(l3)
               l8 = l8 + 2
            ENDIF
            i(n+1) = m(l)
            IF ( ifpdco(m(l+1)) ) baddat = .TRUE.
            DO j = 1 , 6
               i(n+j+1) = ll(j)
               IF ( irigid/=4 ) THEN
                  IF ( ll(j)/=0 ) ncomp = ncomp + 1
               ENDIF
            ENDDO
            n = n + 7
            IF ( irigid/=4 ) THEN
               IF ( ncomp>6 ) baddat = .TRUE.
            ENDIF
         ENDDO
         IF ( mf(l5-l6)/=0 ) badfor = .TRUE.
         IF ( m(l5)/=0 ) baddat = .TRUE.
         IF ( jrigid==3 ) THEN
            spag_nextblock_1 = 25
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 21
      CASE (21)
         IF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
            badfor = .TRUE.
            irigid = 1
         ELSE
            IF ( m1f(2)/=0 .AND. ncomp<6 ) baddat = .TRUE.
         ENDIF
         spag_nextblock_1 = 4
      CASE (22)
         DO lk = l3 , l5
            IF ( mf(lk-l6)/=0 ) badfor = .TRUE.
            IF ( m(lk)/=0 ) baddat = .TRUE.
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
         IF ( mf(1)==0 ) THEN
            irigid = 3
            jrigid = 2
            l1 = 2
            l2 = 6
            l6 = 0
            IF ( mf(2)/=1 .OR. mf(3)/=1 ) badfor = .TRUE.
            IF ( m(1)/=0 .OR. m(2)<1 .OR. m(3)<1 ) baddat = .TRUE.
            n = 0
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
         l7 = l8
         IF ( mf(1)/=3 .OR. mf(2)/=1 .OR. mf(3)/=1 ) badfor = .TRUE.
         IF ( (m(1)/=mset .AND. m(1)/=ium) .OR. m(2)/=blnk .OR. m(3)<1 .OR. m(4)<1 ) THEN
            WRITE (nout,99022) ufm , blnk , q(1) , knt1
            GOTO 20
         ELSE
            n = 1
            i(1) = mset
            spag_nextblock_1 = 20
         ENDIF
      CASE (25)
         IF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
            irigid = 1
            DO j = 1 , 7
               i(n+j) = -1
            ENDDO
            IF ( m1(1)==arigid .AND. m1(2)==drigid ) i(n+2) = 0
            IF ( m1(1)==crbe .AND. m1(2)==erigid ) i(n+2) = 0
            n = n + 7
            kn = 0
            IF ( .NOT.(kz/=50 .OR. .NOT.prt) ) THEN
               lk = (l8+4)/3 + 2
               CALL page2(lk)
               WRITE (nout,99011) scc , knt1 , (q(j),j=1,l7)
99011          FORMAT (/25X,A19,I7,1H-,5X,'CRBE1 ',7I8,/,(71X,6I8))
               lk = l7 + 1
               WRITE (nout,99012) (q(j),j=lk,l8)
99012          FORMAT (69X,'UM',6I8,/,(71X,6I8))
               WRITE (nout,99013) gcc , (q(j),j=1,l7)
99013          FORMAT (25X,A19,13X,'CRIGD3',7I8,/,(71X,6I8))
               WRITE (nout,99014) (q(j),j=lk,l8)
99014          FORMAT (67X,'MSET',6I8,/,(71X,6I8))
            ENDIF
         ENDIF
         spag_nextblock_1 = 4
      CASE (26)
!
         IF ( prt ) THEN
            CALL page2(4)
            WRITE (nout,99015) scc , knt , (m(l),l=1,7) , gcc , m(1) , m(2) , m(4) , m(3) , m(5) , m(2) , m(6) , m(3) , m(7)
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
            WRITE (nout,99022) ufm , ind , m(1) , knt1
            GOTO 20
         ELSE
            lk = 0
            IF ( kz==49 ) lk = 1
            i(1) = m(1)
            n = 2
            IF ( m(4+lk)/=0 ) THEN
               i(n) = m(2)
               DO j = 1 , 6
                  i(n+j) = ia(j)
               ENDDO
               n = n + 7
            ENDIF
            IF ( m(5+lk)/=0 ) THEN
               i(n) = m(3)
               DO j = 1 , 6
                  i(n+j) = ib(j)
               ENDDO
               n = n + 7
            ENDIF
            IF ( kz==49 .AND. m(6+lk)/=0 ) THEN
               i(n) = m(4)
               DO j = 1 , 6
                  i(j+n) = ic(j)
               ENDDO
               n = n + 7
            ENDIF
!
            i(n) = mset
            n = n + 1
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
            IF ( m(6+lk)/=0 ) THEN
               i(n) = m(2)
               DO j = 1 , 6
                  IF ( rbe ) i(n+j) = -ia(j)
                  IF ( .NOT.rbe ) i(n+j) = ja(j)
               ENDDO
               n = n + 7
            ENDIF
            IF ( m(7+lk)/=0 ) THEN
               i(n) = m(3)
               DO j = 1 , 6
                  IF ( rbe ) i(n+j) = -ib(j)
                  IF ( .NOT.rbe ) i(n+j) = jb(j)
               ENDDO
               n = n + 7
            ENDIF
            IF ( kz==49 .AND. m(8+lk)/=0 ) THEN
               i(n) = m(4)
               DO j = 1 , 6
                  IF ( rbe ) i(n+j) = -ic(j)
                  IF ( .NOT.rbe ) i(n+j) = jc(j)
               ENDDO
               n = n + 7
            ENDIF
            n = n - 1
            DO j = 1 , 7
               i(n+j) = -1
            ENDDO
            IF ( m1(1)==crtr .OR. m1(1)==crba ) i(n+2) = 0
            n = n + 7
            spag_nextblock_1 = 4
         ENDIF
      CASE (28)
         IF ( prt ) THEN
            knt1 = knt
            IF ( .NOT.rbe ) knt1 = knt - 1
            CALL page2(5)
            WRITE (nout,99016) scc , knt1 , (m(l),l=1,7) , (m(l),l=9,11) , gcc , m(1) , m(2) , m(5) , m(3) , m(6) , m(4) , m(7) ,   &
                             & m(2) , m(9) , m(3) , m(10) , m(4) , m(11)
99016       FORMAT (/25X,A19,I7,1H-,5X,'CRTRPLT',I7,6I8,/63X,3I8,/25X,A19,13X,'CRIGD3',7I8,/67X,'MSET',6I8)
         ENDIF
         spag_nextblock_1 = 27
      CASE (29)
         DO l = irg , 8
            IF ( mf(l)==0 ) THEN
               spag_nextblock_1 = 31
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( mf(l)/=1 ) badfor = .TRUE.
            IF ( m(l)<=0 ) baddat = .TRUE.
            IF ( l8<l6 ) THEN
               q(l8+1) = m(l)
               q(l8+2) = m3
            ENDIF
            l8 = l8 + 2
            IF ( l7<92 ) q(l7+1) = m(l)
            l7 = l7 + 1
            i(n+1) = m(l)
            DO j = 1 , 6
               i(n+1+j) = ll(j)
            ENDDO
            n = n + 7
         ENDDO
         IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 30
      CASE (30)
         irigid = 1
         DO j = 1 , 7
            i(n+j) = -1
         ENDDO
         IF ( m1(1)==crbe .AND. m1(2)==frigid ) i(n+2) = 0
         n = n + 7
         kn = 0
         IF ( prt ) THEN
            l3 = l7
            l5 = l8
            IF ( l3>92 ) l3 = 92
            IF ( l5>l6 ) l5 = l6
            j = (l5+2)/8 + (l3-l6+2)/8 + 2
            CALL page2(j)
            l6 = l6 + 1
            WRITE (nout,99017) scc , knt1 , (q(j),j=l6,l3)
99017       FORMAT (/25X,A19,I7,1H-,5X,'CRBE2 ',8I8,/,(63X,8I8))
            WRITE (nout,99018) gcc , (q(j),j=1,l5)
99018       FORMAT (25X,A19,13X,'CRIGD2',8I8,/,(63X,8I8))
            IF ( l8>l6 .OR. l7>102 ) WRITE (nout,99019)
99019       FORMAT (57X,'*** ABOVE PRINTOUT MAY BE IMCOMPLETE.  DATA IS OK')
         ENDIF
         spag_nextblock_1 = 4
      CASE (31)
         l1 = l
         IF ( l1<=8 ) THEN
            DO l = l1 , 8
               IF ( m(l)/=0 ) baddat = .TRUE.
               IF ( mf(l)/=0 ) badfor = .TRUE.
            ENDDO
         ENDIF
         IF ( m1(1)==0 .AND. m1(2)==0 ) baddat = .TRUE.
         spag_nextblock_1 = 30
      CASE (32)
         WRITE (nout,99022) ufm , blnk , m(1) , knt1
         GOTO 20
      CASE (33)
         kn = 0
         km = 0
         n = n + 1
         i(n) = -3
         spag_nextblock_1 = 4
      CASE (34)
         DO l = 2 , 6 , 2
            IF ( mf(l)/=0 ) THEN
               IF ( mf(l)/=1 .OR. m(l1)<=0 ) baddat = .TRUE.
               IF ( mf(l+1)/=1 .OR. m(l1+1)<=0 ) baddat = .TRUE.
               IF ( ifpdco(m(l1+1)) ) baddat = .TRUE.
               i(n+1) = m(l1)
               i(n+2) = m(l1+1)
               n = n + 2
            ENDIF
            l1 = l1 + 2
         ENDDO
         IF ( m1(1)/=0 ) THEN
            spag_nextblock_1 = 33
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
      CASE (35)
         DO l = l1 , 8
            IF ( mf(l)/=0 .AND. mf(l)/=1 ) THEN
               spag_nextblock_1 = 38
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( im==-1 .AND. m(l)<0 ) THEN
               spag_nextblock_1 = 38
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( im==-1 .AND. m(l)==0 ) THEN
               spag_nextblock_1 = 36
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( im/=-1 ) THEN
               IF ( ifpdco(m(l)) ) THEN
                  spag_nextblock_1 = 38
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
!
! ... NOTE - COMPONENTS IN LL NOT SENT OUT IN CRSPLINE
!
            im = -im
            n = n + 1
            i(n) = m(l)
         ENDDO
         IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
            spag_nextblock_1 = 37
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         n = n + 1
         i(n) = 0
         spag_nextblock_1 = 36
      CASE (36)
         im = -9
         n = n + 1
         i(n) = -1
         IF ( l/=8 ) THEN
            l1 = l
            DO l = l1 , 8
               IF ( mf(l)/=0 ) THEN
                  spag_nextblock_1 = 38
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
         spag_nextblock_1 = 37
      CASE (37)
!
         kn = 1
         IF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
            kn = 0
            km = 0
            n = n + 1
            i(n) = -1
         ENDIF
         spag_nextblock_1 = 4
      CASE (38)
         baddat = .TRUE.
         spag_nextblock_1 = 37
         CYCLE SPAG_DispatchLoop_1
 300     n = 6
         GOTO 40
 320     n = 17
         GOTO 40
 340     n = 7
         GOTO 40
      CASE (39)
         DO l = l3 , 8
            IF ( mf(l)/=0 .AND. mf(l)/=1 ) badfor = .TRUE.
         ENDDO
         l5 = 1
         DO l = l3 , 7 , 2
            IF ( m(l)==0 ) THEN
               spag_nextblock_1 = 41
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            l5 = l + 2
            l8 = l8 + 1
            n = n + 2
            i(n-1) = m(l)
            i(n) = m(l+1)
            IF ( m(l)>0 ) THEN
               IF ( m(l+1)>=0 .AND. m(l+1)<=6 ) CYCLE
            ENDIF
            baddat = .TRUE.
         ENDDO
         IF ( m1f(2)/=3 ) THEN
            spag_nextblock_1 = 45
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 40
      CASE (40)
         n = n + 2
         i(n-1) = -1
         i(n) = l8
         l0 = l0 + 1
         IF ( l0==5 ) THEN
            l7 = l8
         ELSE
            l6 = l8
         ENDIF
         spag_nextblock_1 = 45
      CASE (41)
         DO l = l5 , 7 , 2
            IF ( m(l)/=0 .OR. m(l+1)/=0 ) baddat = .TRUE.
         ENDDO
         IF ( l5<=1 ) baddat = .TRUE.
         IF ( m1f(2)==3 ) THEN
            spag_nextblock_1 = 40
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         baddat = .TRUE.
         spag_nextblock_1 = 45
      CASE (42)
         l9 = (l6*(l6+1))/2
         lb = lb + 1
         IF ( m(1)==lz ) kzflag = 1
         IF ( m(1)==kk ) kzflag = 2
         i(lb) = kzflag
         spag_nextblock_1 = 43
      CASE (43)
         l0 = l0 + 1
         l8 = 0
         IF ( mf(1)/=3 ) badfor = .TRUE.
         l3 = 2
         DO l = 2 , 8
            m(l) = m(l+1)
         ENDDO
         spag_nextblock_1 = 44
      CASE (44)
         DO l = l3 , 8
            IF ( mf(l)/=2 .AND. mf(l)/=0 ) badfor = .TRUE.
         ENDDO
         n = lb
         l5 = l9 - l8 + l3 - 1
         IF ( l5>8 ) l5 = 8
         DO l = l3 , l5
            n = n + 1
            i(n) = m(l)
         ENDDO
         l5 = l9 - l8 + l3
         l8 = l8 + n - lb
         IF ( l9<=l8 ) THEN
            IF ( l9/=l8 ) THEN
               DO l = l5 , 8
                  IF ( m(l)/=0 ) baddat = .TRUE.
               ENDDO
            ENDIF
            IF ( l0==8 ) THEN
               l0 = 1
            ELSE
               l0 = l0 + 1
            ENDIF
         ENDIF
         spag_nextblock_1 = 45
      CASE (45)
         DO l = 1 , 8
            m(l) = 0
         ENDDO
         kn = 1
         IF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
            kn = 0
            IF ( id>la1 ) THEN
               la1 = id
               IF ( .NOT.(.NOT.noud .AND. l7/=6 .AND. nos .AND. kzflag==1) ) THEN
                  IF ( .NOT.(l7==0 .AND. .NOT.nos) ) THEN
                     l7 = 0
                     IF ( .NOT.(l0==1 .AND. .NOT.noud) ) THEN
                        n = n + 1
                        i(n) = 0
                        l0 = 1
                     ENDIF
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
            ENDIF
            baddat = .TRUE.
            l0 = 1
            l7 = 0
            la1 = id
         ENDIF
         spag_nextblock_1 = 4
      CASE (46)
         DO l = l1 , 8
            IF ( mf(l)/=0 .AND. mf(l)/=1 ) badfor = .TRUE.
         ENDDO
         DO l = l1 , 8
            IF ( m(l)<0 ) THEN
               baddat = .TRUE.
            ELSEIF ( m(l)/=0 ) THEN
               n = n + 1
               i(n) = m(l)
            ENDIF
         ENDDO
         IF ( n<l1 ) baddat = .TRUE.
         kn = 1
         IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 47
      CASE (47)
         km = 0
         n = n + 1
         i(n) = -1
         kn = 0
         spag_nextblock_1 = 4
      CASE (48)
         WRITE (nout,99020) ufm , m(1) , m(2) , knt
99020    FORMAT (A23,' 331, IMPROPER PARAM CARD ',2A4,10X,'SORTED CARD COUNT =',I7)
         CALL page2(2)
         abort = .TRUE.
         RETURN
      CASE (49)
!
!*******    12-SPC1(3980), 92-OMIT1(3981), 216-ASET1(3981)   ***********
!          332-CFLSTR(3980)
!
         iz = 2
         ifile = 210
         IF ( k==332 ) ifile = 208
         spag_nextblock_1 = 51
      CASE (50)
         iz = 1
         ifile = 210
         spag_nextblock_1 = 51
      CASE (51)
         IF ( km/=0 ) THEN
            l1 = 1
            spag_nextblock_1 = 53
            CYCLE SPAG_DispatchLoop_1
         ELSE
            km = 1
            IF ( mf(iz)/=0 .AND. mf(iz)/=1 ) badfor = .TRUE.
            IF ( k/=332 ) THEN
               IF ( ifpdco(m(iz)) ) baddat = .TRUE.
               IF ( iz/=2 ) THEN
                  spag_nextblock_1 = 52
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
            IF ( mf(1)/=1 ) badfor = .TRUE.
            IF ( m(1)<=0 ) baddat = .TRUE.
         ENDIF
         spag_nextblock_1 = 52
      CASE (52)
         id = m(1)
         i(1) = m(1)
         IF ( iz==2 ) i(2) = m(2)
         n = iz
         l1 = iz + 1
         IF ( mf(iz+2)==3 .AND. m(iz+2)==thru ) THEN
            IF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
               IF ( mf(iz+1)/=1 .OR. mf(iz+3)/=1 ) badfor = .TRUE.
               IF ( m(iz+1)<=0 .OR. m(iz+4)<=m(iz+1) ) baddat = .TRUE.
               DO l = iz , 4
                  IF ( mf(l+4)/=0 ) badfor = .TRUE.
               ENDDO
               IF ( badfor .OR. baddat ) THEN
                  spag_nextblock_1 = 55
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               CALL write(ifile,m,iz,0)
               l1 = m(iz+1)
               l2 = m(iz+4)
               l = l1
               DO
                  CALL write(ifile,l,1,0)
                  l = l + 1
                  IF ( l>l2 ) THEN
                     n = 0
                     spag_nextblock_1 = 55
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
            ELSE
               kn = 1
               badfor = .TRUE.
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 53
      CASE (53)
         DO l = l1 , 8
            IF ( mf(l)/=0 .AND. mf(l)/=1 ) badfor = .TRUE.
         ENDDO
         DO l = l1 , 8
            IF ( mf(l)==1 ) THEN
               spag_nextblock_1 = 54
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         baddat = .TRUE.
         spag_nextblock_1 = 54
      CASE (54)
         DO l = l1 , 8
            IF ( m(l)<0 ) THEN
               baddat = .TRUE.
            ELSEIF ( m(l)/=0 ) THEN
               n = n + 1
               i(n) = m(l)
            ENDIF
         ENDDO
         kn = 1
         IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 55
      CASE (55)
         km = 0
         n = n + 1
         i(n) = -1
         kn = 0
         spag_nextblock_1 = 4
      CASE (56)
!
!******        84-LOAD, 123-DLOAD      *******************************
!
         IF ( km==1 ) THEN
            n = 0
         ELSE
            km = 1
            IF ( mf(1)/=0 .AND. mf(1)/=1 .OR. mf(2)/=0 .AND. mf(2)/=2 ) badfor = .TRUE.
            IF ( m(1)<=0 ) baddat = .TRUE.
            id = m(1)
            i(1) = id
            i(2) = m(2)
            IF ( m(4)<=0 ) baddat = .TRUE.
            n = 2
         ENDIF
         l8 = n + 1
         DO l = l8 , 7 , 2
            IF ( mf(l)/=0 .AND. mf(l)/=2 .OR. mf(l+1)/=0 .AND. mf(l+1)/=1 ) badfor = .TRUE.
         ENDDO
         SPAG_Loop_1_1: DO
            n = n + 2
            IF ( m(n)<0 ) THEN
               baddat = .TRUE.
               EXIT SPAG_Loop_1_1
            ELSEIF ( m(n)==0 ) THEN
               EXIT SPAG_Loop_1_1
            ELSE
               i(n-1) = m(n-1)
               i(n) = m(n)
               IF ( n>=8 ) THEN
                  spag_nextblock_1 = 57
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_1_1
         n = n - 2
         l7 = 1
         l8 = n + 1
         DO l = l8 , 8
            IF ( mf(l)/=0 ) baddat = .TRUE.
         ENDDO
         IF ( n<=0 ) baddat = .TRUE.
         spag_nextblock_1 = 57
      CASE (57)
         kn = 1
         IF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
            km = 0
            n = n + 2
            i(n-1) = -1
            i(n) = -1
            kn = 0
            l7 = 0
         ELSEIF ( l7==1 ) THEN
            baddat = .TRUE.
            l7 = 0
         ENDIF
         spag_nextblock_1 = 4
      CASE (58)
!
!     ******************************************************************
!
         IF ( .NOT.iax ) THEN
            IF ( lh ) WRITE (nout,99021) ufm
99021       FORMAT (A23,' 332, AXIC CARD REQUIRED.')
            IF ( lh ) CALL page2(2)
            lh = .FALSE.
            abort = .TRUE.
         ELSEIF ( ih>nn .OR. ih<0 ) THEN
            GOTO r1
         ENDIF
         GOTO r
      CASE (59)
         n = n - 1
         l7 = 1
         l8 = n + 1
         DO l = l8 , 8
            IF ( mf(l)/=0 ) baddat = .TRUE.
         ENDDO
         IF ( n<=0 ) baddat = .TRUE.
         spag_nextblock_1 = 60
      CASE (60)
         kn = 1
         IF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
            km = 0
            n = n + 1
            i(n) = -1
            kn = 0
            l7 = 0
         ELSEIF ( l7==1 ) THEN
            baddat = .TRUE.
            l7 = 0
         ENDIF
         spag_nextblock_1 = 4
      CASE (61)
         IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         baddat = .TRUE.
         spag_nextblock_1 = 63
      CASE (62)
         IF ( m(l)/=endt ) THEN
            baddat = .TRUE.
            spag_nextblock_1 = 61
            CYCLE SPAG_DispatchLoop_1
         ELSE
            IF ( m1(1)==0 .AND. m1(2)==0 ) baddat = .TRUE.
         ENDIF
         spag_nextblock_1 = 63
      CASE (63)
         km = 0
         kn = 0
         IF ( k/=330 ) THEN
            IF ( items/=itot1 .AND. items/=itot2 ) baddat = .TRUE.
         ENDIF
         spag_nextblock_1 = 4
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99022 FORMAT (A23,', ILLEGAL ',A2,'DEPENDENT D.O.F.',' FOR RIGID ELEMENT',I9,' SORTED COUNT',I8)
!
99999 END SUBROUTINE ifs3p
