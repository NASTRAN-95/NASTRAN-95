!*==ifs4p.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifs4p() !HIDESTARS (*,*,*)
   IMPLICIT NONE
   USE C_CIFS4P
   USE C_IFPDTA
   USE C_IFPX1
   USE C_IFPX3
   USE C_MACHIN
   USE C_SYSTEM
   USE C_XMSSG
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: bcda , bcdaxi , bcdno , bcdnon , bcds , bcdyes , thru
   REAL :: dc1 , dc2 , dc3 , dl1 , dl2 , dlc , dx1 , dx2 , dy1 , dy2 , dz1 , dz2 , zz
   INTEGER :: i1 , iem , ifo , ii , in , iqvl , isid , ity1 , k2078 , kfl , kout , kz , l , l1 , l2 , l3 , l4 , l5 , l6 , lll ,     &
            & lp1 , n1 , nn , npts , ntot , ret , ty1 , ty2
   INTEGER , DIMENSION(2) :: nm
   INTEGER , DIMENSION(24) :: save
   REAL , DIMENSION(100) :: z
   EXTERNAL ifpdco , page2 , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
   !>>>>EQUIVALENCE (Z(1),M(1)) , (Kout,J(2))
   DATA thru , bcdyes , bcdno/4HTHRU , 4HYES  , 4HNO  /
   DATA bcds , bcda , bcdnon/4HS    , 4HA    , 4HNONE/
   DATA bcdaxi/4HAXIS/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         IF ( K<=100 ) THEN
            IF ( K==1 .OR. K==2 .OR. K==3 .OR. K==4 .OR. K==5 .OR. K==6 .OR. K==7 .OR. K==8 .OR. K==9 .OR. K==10 .OR. K==11 .OR.    &
               & K==12 .OR. K==13 .OR. K==14 .OR. K==15 .OR. K==16 .OR. K==17 .OR. K==18 .OR. K==19 .OR. K==20 .OR. K==21 .OR.      &
               & K==22 .OR. K==23 .OR. K==24 .OR. K==25 .OR. K==26 .OR. K==27 .OR. K==28 .OR. K==29 .OR. K==30 .OR. K==31 .OR.      &
               & K==32 .OR. K==33 .OR. K==34 .OR. K==35 .OR. K==36 .OR. K==37 .OR. K==38 .OR. K==39 .OR. K==40 .OR. K==41 .OR.      &
               & K==42 .OR. K==43 .OR. K==44 .OR. K==45 .OR. K==46 .OR. K==47 .OR. K==48 .OR. K==49 .OR. K==50 .OR. K==51 .OR.      &
               & K==52 .OR. K==53 .OR. K==54 .OR. K==55 .OR. K==56 .OR. K==57 .OR. K==58 .OR. K==59 .OR. K==60 .OR. K==61 .OR.      &
               & K==62 .OR. K==63 .OR. K==64 .OR. K==65 .OR. K==66 .OR. K==67 .OR. K==68 .OR. K==69 .OR. K==70 .OR. K==71 .OR.      &
               & K==72 .OR. K==73 .OR. K==74 .OR. K==75 .OR. K==76 .OR. K==77 .OR. K==78 .OR. K==81 .OR. K==82 .OR. K==83 .OR.      &
               & K==84 .OR. K==85 .OR. K==86 .OR. K==87 .OR. K==88 .OR. K==89 .OR. K==92 .OR. K==93 .OR. K==94 .OR. K==95 .OR.      &
               & K==96 .OR. K==97 .OR. K==99 .OR. K==100 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( K==79 ) THEN
!
!******              79-CTRIARG,80-CTRAPRG             ****************
!
               i1 = 4
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( K==80 ) THEN
               i1 = 5
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( K==90 .OR. K==91 ) THEN
!
!*******       MATS1,MATT1        **************************************
!
               DO l = 1 , 11
                  IF ( M(l)<0 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  I(l) = M(l)
               ENDDO
               N = 11
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( K==98 ) THEN
!
!*******       TEMPD              **************************************
!
               DO l = 1 , 7 , 2
                  IF ( M(l)/=0 .OR. M(l+1)/=0 ) THEN
                     IF ( M(l)<=0 ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     N = N + 2
                     I(N-1) = M(l)
                     I(N) = M(l+1)
                     IF ( N>2 ) THEN
                        DO l1 = 4 , N , 2
                           IF ( I(N-1)==I(l1-3) ) THEN
                              spag_nextblock_1 = 4
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                        ENDDO
                     ENDIF
                  ENDIF
               ENDDO
               IF ( N<=0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         IF ( Kx<=100 ) THEN
            IF ( Kx==1 .OR. Kx==3 .OR. Kx==6 .OR. Kx==7 .OR. Kx==8 .OR. Kx==9 .OR. Kx==10 .OR. Kx==11 .OR. Kx==12 .OR. Kx==13 .OR.  &
               & Kx==14 .OR. Kx==15 .OR. Kx==16 .OR. Kx==17 .OR. Kx==18 .OR. Kx==19 .OR. Kx==20 .OR. Kx==21 .OR. Kx==23 .OR.        &
               & Kx==25 .OR. Kx==26 .OR. Kx==27 .OR. Kx==28 .OR. Kx==29 .OR. Kx==30 .OR. Kx==31 .OR. Kx==32 .OR. Kx==33 .OR.        &
               & Kx==34 .OR. Kx==35 .OR. Kx==36 .OR. Kx==37 .OR. Kx==38 .OR. Kx==39 .OR. Kx==40 .OR. Kx==41 .OR. Kx==42 .OR.        &
               & Kx==43 .OR. Kx==44 .OR. Kx==45 .OR. Kx==46 .OR. Kx==47 .OR. Kx==48 .OR. Kx==49 .OR. Kx==50 .OR. Kx==51 .OR.        &
               & Kx==52 .OR. Kx==53 .OR. Kx==54 .OR. Kx==55 .OR. Kx==56 .OR. Kx==57 .OR. Kx==58 .OR. Kx==59 .OR. Kx==60 .OR.        &
               & Kx==61 .OR. Kx==62 .OR. Kx==63 .OR. Kx==64 .OR. Kx==65 .OR. Kx==66 .OR. Kx==67 .OR. Kx==68 .OR. Kx==69 .OR.        &
               & Kx==70 .OR. Kx==71 .OR. Kx==72 .OR. Kx==73 .OR. Kx==74 .OR. Kx==75 .OR. Kx==76 .OR. Kx==77 .OR. Kx==78 .OR.        &
               & Kx==79 .OR. Kx==80 .OR. Kx==81 .OR. Kx==82 .OR. Kx==83 .OR. Kx==84 .OR. Kx==85 .OR. Kx==86 .OR. Kx==87 .OR.        &
               & Kx==88 .OR. Kx==90 .OR. Kx==91 .OR. Kx==92 .OR. Kx==93 .OR. Kx==94 .OR. Kx==97 .OR. Kx==98 .OR. Kx==100 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Kx==2 .OR. Kx==89 ) THEN
!
!**************    MATT2,189-MATT3     *********************************
!
               DO l = 1 , 16
                  IF ( M(l)<0 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  I(l) = M(l)
               ENDDO
               IF ( M(1)==0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               N = 16
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Kx==4 ) THEN
!
!******           104-CTORDRG           ************************
!
               IF ( M(1)<=0 .OR. M(3)<=0 .OR. M(4)<=0 .OR. M(3)==M(4) .OR. z(5)<0.0 .OR. z(5)>180.0 .OR. z(6)<0.0 .OR. z(6)>180.0 ) &
                  & THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( Mf(2)==0 ) M(2) = M(1)
               IF ( M(2)<=0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               N = 7
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Kx==5 .OR. Kx==24 ) THEN
!
!*******       SPOINT,124-EPOINT    ************************************
!
               IF ( Mf(2)==3 ) THEN
                  IF ( M(2)/=thru ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( Mf(1)/=1 .OR. Mf(3)/=1 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  k2078 = 208
                  IF ( K==124 ) k2078 = 207
                  l1 = 1
                  l2 = 4
                  DO l = l2 , 8
                     IF ( Mf(l)/=0 ) THEN
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
                  IF ( M(l2)>9999999 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  ii = M(l1) - 1
                  l2 = M(l2) - M(l1)
                  IF ( ii<0 .OR. l2<=0 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  l1 = 1
                  DO l = 1 , l2
                     ii = ii + 1
                     CALL write(k2078,ii,1,0)
                  ENDDO
                  I(1) = ii + 1
                  N = 1
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  DO l = 1 , 8
                     IF ( Mf(l)/=1 .AND. Mf(l)/=0 ) THEN
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( M(l)<0 ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( M(l)/=0 ) THEN
                        IF ( M(l)>999999 ) THEN
                           spag_nextblock_1 = 4
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        N = N + 1
                        I(N) = M(l)
                        IF ( N>1 ) THEN
                           DO l1 = 2 , N
                              IF ( I(N)==I(l1-1) ) THEN
                                 spag_nextblock_1 = 4
                                 CYCLE SPAG_DispatchLoop_1
                              ENDIF
                           ENDDO
                        ENDIF
                     ENDIF
                  ENDDO
                  IF ( N<=0 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( Kx==22 ) THEN
!
!*******         122-MAT3        *****************************
!
               IF ( M(1)<=0 .OR. z(2)<0. .OR. z(3)<0. .OR. z(4)<0. .OR. z(9)<0. .OR. z(10)<0. .OR. z(11)<0. ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( abs(z(5))>1. .OR. abs(z(6))>1. .OR. abs(z(7))>1. ) THEN
                  CALL page2(2)
                  WRITE (Nout,99001) Uwm , T1(1,K) , T1(2,K) , Knt
99001             FORMAT (A25,' 301, BULK DATA CARD ',2A4,' CONTAINS INCONSISTENT',' DATA.',10X,'SORTED CARD COUNT =',I7)
               ENDIF
               N = 16
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Kx==95 ) THEN
!
!
!*******       195-RANDPS       ****************************************
!
               IF ( M(1)<=0 .OR. M(2)<=0 .OR. M(3)<M(2) .OR. M(6)<0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( M(2)==M(3) .AND. z(5)/=0.0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               N = 6
               IF ( kout<=2 ) THEN
                  spag_nextblock_1 = 9
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( M(1)==J(kout) ) THEN
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( kout==J(1) ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Kx==96 ) THEN
!
!*******       196-RANDT1       ****************************************
!
               IF ( kout>2 ) THEN
                  DO in = 3 , kout
                     IF ( M(1)==J(in) ) THEN
                        spag_nextblock_1 = 10
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
               ENDIF
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Kx==99 ) THEN
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         IF ( Ky<=100 ) THEN
            IF ( Ky==1 ) THEN
!
!**********          201-TEMPP1          *******************************
!
               IF ( Km/=0 ) THEN
                  spag_nextblock_1 = 16
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               nn = 6
               N = 6
               Id = M(1)
               IF ( Mf(5)==-32767 ) THEN
                  spag_nextblock_1 = 14
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( Mf(7)/=0 .OR. Mf(8)/=0 ) Badfor = .TRUE.
               spag_nextblock_1 = 12
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Ky==2 ) THEN
!
!*******       202-TEMPP2         **************************************
!
               IF ( Km/=0 ) THEN
                  spag_nextblock_1 = 16
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               nn = 8
               N = 8
               Id = M(1)
               IF ( Mf(5)==-32767 ) THEN
                  spag_nextblock_1 = 14
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( Mf(7)/=0 .AND. Mf(7)/=2 .OR. Mf(8)/=0 .AND. Mf(8)/=2 ) Badfor = .TRUE.
               spag_nextblock_1 = 12
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Ky==3 ) THEN
               spag_nextblock_1 = 17
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Ky==4 ) THEN
!
!*******       204-TEMPRB         **************************************
!
               IF ( Km/=0 ) THEN
                  IF ( Km>1 ) THEN
                     spag_nextblock_1 = 16
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  N = 0
                  DO l = 1 , 8
                     IF ( Mf(l)==-32767 ) THEN
                        spag_nextblock_1 = 19
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( Mf(l)/=0 .AND. Mf(l)/=2 ) Badfor = .TRUE.
                     I(l+8) = M(l)
                     save(l+8) = M(l)
                  ENDDO
                  spag_nextblock_1 = 18
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  nn = 16
                  N = 0
                  Id = M(1)
                  l1 = 1
                  IF ( Mf(1)/=1 .OR. Mf(2)/=1 ) Badfor = .TRUE.
                  DO l = 3 , 8
                     IF ( Mf(l)/=0 .AND. Mf(l)/=2 ) THEN
                        IF ( Mf(l)==-32767 ) THEN
                           DO l5 = l , 8
                              M(l5) = 0
                           ENDDO
                        ELSE
                           Badfor = .TRUE.
                        ENDIF
                     ENDIF
                  ENDDO
                  IF ( M(1)<=0 .OR. M(2)<=0 ) Baddat = .TRUE.
                  DO l = 1 , 8
                     I(l) = M(l)
                     save(l) = M(l)
                  ENDDO
                  spag_nextblock_1 = 18
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( Ky==5 ) THEN
!
!*******       205-GRIDB          **************************************
!
               ASSIGN 20 TO ret
               spag_nextblock_1 = 32
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Ky==6 ) THEN
               spag_nextblock_1 = 21
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Ky==7 ) THEN
!
!*******       207-RINGFL         **************************************
!
               ASSIGN 60 TO ret
               spag_nextblock_1 = 32
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Ky==8 ) THEN
!
!*******       208-PRESPT         **************************************
!
               ASSIGN 80 TO ret
               spag_nextblock_1 = 32
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Ky==9 ) THEN
!
!*******       209-CFLUID2        **************************************
!
               kfl = 2
               ASSIGN 100 TO ret
               spag_nextblock_1 = 32
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Ky==10 ) THEN
!
!*******       210-CFLUID3        **************************************
!
               kfl = 3
               ASSIGN 100 TO ret
               spag_nextblock_1 = 32
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Ky==11 ) THEN
!
!*******       211-CFLUID4        **************************************
!
               kfl = 4
               ASSIGN 100 TO ret
               spag_nextblock_1 = 32
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Ky==12 ) THEN
!
!*******       212-AXIF           **************************************
!
               N = 0
               IF ( Km==0 ) THEN
                  IF ( Iaxf>0 ) THEN
                     CALL page2(2)
                     WRITE (Nout,99002) Ufm
99002                FORMAT (A23,' 4121, ONLY ONE (1) AXIF CARD ALLOWED IN BULK DATA.')
                     Abort = .TRUE.
                  ELSE
                     Iaxf = Iaxf + 1
                     IF ( Mf(1)/=1 .OR. Mf(2)/=0 .AND. Mf(2)/=2 .OR. Mf(3)/=0 .AND. Mf(3)/=2 .OR. Mf(4)/=0 .AND. Mf(4)/=2 .OR.      &
                        & Mf(5)/=3 ) Badfor = .TRUE.
                     IF ( Mf(7)/=0 .OR. Mf(8)/=0 .OR. Mf(6)/=0 .AND. Mf(6)/=3 ) Badfor = .TRUE.
                     IF ( Mf(3)==0 ) M(3) = 1
                     IF ( M(5)/=bcdyes .AND. M(5)/=bcdno ) Baddat = .TRUE.
                     IF ( M(5)==bcdyes ) M(5) = 1
                     IF ( M(5)==bcdno ) M(5) = 0
                     CALL write(215,M,5,0)
                     IF ( Mf(6)==3 ) THEN
                        IF ( M(7)/=bcdnon ) Baddat = .TRUE.
                        IF ( M1(1)==0 .AND. M1(2)==0 ) Baddat = .TRUE.
                     ELSE
                        IF ( M1(1)/=0 .OR. M1(2)/=0 ) Baddat = .TRUE.
                     ENDIF
                  ENDIF
                  spag_nextblock_1 = 31
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( Mf(2)/=3 ) THEN
                  DO l = 1 , 8
                     IF ( Mf(l)/=0 ) THEN
                        IF ( Mf(l)==1 ) THEN
                           IF ( M(l)<=Naxf ) Baddat = .TRUE.
                           N = N + 1
                           Naxf = M(l)
                           I(N) = M(l)
                        ELSE
                           Badfor = .TRUE.
                           N = 0
                           spag_nextblock_1 = 31
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                     ENDIF
                  ENDDO
                  IF ( N<=0 ) Baddat = .TRUE.
                  spag_nextblock_1 = 31
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( Mf(4)==3 ) THEN
                  l1 = M(7)
                  l2 = l1
                  IF ( Mf(1)/=1 .OR. Mf(3)/=1 .OR. Mf(5)/=1 .OR. Mf(6)/=0 .OR. Mf(7)/=0 .OR. Mf(8)/=0 ) THEN
                     spag_nextblock_1 = 29
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( M(1)<M(4) .AND. M(7)>0 .AND. M(7)<=M(4) .AND. mod(M(4)-M(1),M(7))==0 ) THEN
                     spag_nextblock_1 = 30
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  Baddat = .TRUE.
                  spag_nextblock_1 = 31
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  l1 = 1
                  l2 = 1
                  IF ( Mf(1)/=1 .OR. Mf(3)/=1 ) THEN
                     spag_nextblock_1 = 29
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  DO l = 4 , 8
                     IF ( Mf(l)/=0 ) THEN
                        spag_nextblock_1 = 29
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
                  IF ( M(1)<M(4) .AND. M(1)>=0 ) THEN
                     spag_nextblock_1 = 30
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  Baddat = .TRUE.
                  spag_nextblock_1 = 31
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( Ky==13 ) THEN
               spag_nextblock_1 = 21
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Ky==14 ) THEN
               ASSIGN 80 TO ret
               spag_nextblock_1 = 32
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Ky==15 .OR. Ky==16 .OR. Ky==23 .OR. Ky==24 .OR. Ky==25 .OR. Ky==26 .OR. Ky==27 .OR. Ky==28 .OR. Ky==29 .OR.    &
                   & Ky==30 .OR. Ky==31 .OR. Ky==32 .OR. Ky==33 .OR. Ky==34 .OR. Ky==35 .OR. Ky==36 .OR. Ky==37 .OR. Ky==38 .OR.    &
                   & Ky==40 .OR. Ky==41 .OR. Ky==43 .OR. Ky==44 .OR. Ky==45 .OR. Ky==46 .OR. Ky==47 .OR. Ky==48 .OR. Ky==49 .OR.    &
                   & Ky==50 .OR. Ky==51 .OR. Ky==52 .OR. Ky==53 .OR. Ky==54 .OR. Ky==55 .OR. Ky==56 .OR. Ky==57 .OR. Ky==58 .OR.    &
                   & Ky==59 .OR. Ky==60 .OR. Ky==61 .OR. Ky==62 .OR. Ky==63 .OR. Ky==64 .OR. Ky==65 .OR. Ky==66 .OR. Ky==67 .OR.    &
                   & Ky==68 .OR. Ky==69 .OR. Ky==70 .OR. Ky==71 .OR. Ky==72 .OR. Ky==73 .OR. Ky==74 .OR. Ky==75 .OR. Ky==76 .OR.    &
                   & Ky==77 .OR. Ky==78 .OR. Ky==79 .OR. Ky==80 .OR. Ky==81 .OR. Ky==82 .OR. Ky==83 .OR. Ky==84 .OR. Ky==85 .OR.    &
                   & Ky==86 .OR. Ky==87 .OR. Ky==88 .OR. Ky==89 .OR. Ky==90 .OR. Ky==91 .OR. Ky==92 .OR. Ky==93 .OR. Ky==94 .OR.    &
                   & Ky==97 .OR. Ky==98 .OR. Ky==99 .OR. Ky==100 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Ky==17 ) THEN
!
!*******       217-CTETRA,  335-CFTETRA  *******************************
!
               N = 6
               spag_nextblock_1 = 33
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Ky==18 ) THEN
!
!*******       218-CWEDGE,  336-CFWEDGE  *******************************
!
               N = 8
               spag_nextblock_1 = 33
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Ky==19 ) THEN
               spag_nextblock_1 = 34
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Ky==20 ) THEN
               spag_nextblock_1 = 35
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Ky==21 ) THEN
!
!*******       221-DMIAX          **************************************
!
               IF ( Fphys1 ) THEN
                  Fphys1 = .FALSE.
                  nm(1) = 0
                  nm(2) = 0
               ENDIF
               IF ( Km/=0 ) THEN
                  IF ( M(1)<=0 .OR. M(2)<0 .OR. M(2)>6 ) THEN
                     spag_nextblock_1 = 37
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( Mf(1)/=1 .OR. Mf(2)/=1 .AND. Mf(2)/=0 ) THEN
                     spag_nextblock_1 = 37
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( Mf(4)/=0 .AND. Mf(4)+ity1/=4 ) THEN
                     spag_nextblock_1 = 37
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( Mf(5)/=0 .AND. ty1/=3 .AND. ty1/=4 ) THEN
                     spag_nextblock_1 = 37
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( iabs(M(3))>Naxf ) THEN
                     spag_nextblock_1 = 37
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( Mf(3)/=1 .AND. Mf(3)/=0 ) THEN
                     spag_nextblock_1 = 37
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  N = 3
                  I(2) = M(2)
                  l1 = 3
                  l2 = 3
                  ASSIGN 140 TO ret
                  spag_nextblock_1 = 38
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( M(3)==0 ) THEN
                  ASSIGN 120 TO ret
                  spag_nextblock_1 = 32
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  IF ( M(1)/=nm(1) .OR. M(2)/=nm(2) ) THEN
                     spag_nextblock_1 = 36
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( Mf(2)/=1 .OR. Mf(3)/=1 .AND. Mf(3)/=0 ) THEN
                     spag_nextblock_1 = 37
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( Mf(4)/=1 .AND. Mf(4)/=0 ) THEN
                     spag_nextblock_1 = 37
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( M(3)<=0 .OR. M(4)<0 .OR. M(4)>6 ) THEN
                     spag_nextblock_1 = 37
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( iabs(M(5))>Naxf ) THEN
                     spag_nextblock_1 = 37
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( Mf(5)/=0 .OR. Mf(6)/=0 .OR. Mf(7)/=0 .OR. Mf(8)/=0 ) THEN
                     spag_nextblock_1 = 37
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
                     spag_nextblock_1 = 37
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  N = 2
                  I(2) = M(4)
                  l1 = 4
                  l2 = 5
                  ASSIGN 160 TO ret
                  spag_nextblock_1 = 38
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( Ky==22 ) THEN
!
!*******       222-FLSYM          **************************************
!
               IF ( Lflsym ) THEN
                  CALL page2(2)
                  WRITE (Nout,99003) Ufm
99003             FORMAT (A23,' 4123, ONLY ONE (1) FLSYM CARD ALLOWED IN BULK DATA')
                  Abort = .TRUE.
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  Lflsym = .TRUE.
                  ASSIGN 180 TO ret
                  spag_nextblock_1 = 32
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( Ky==39 .OR. Ky==42 ) THEN
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Ky==95 ) THEN
               spag_nextblock_1 = 17
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Ky==96 ) THEN
!
!*******      296-TEMPP4    ********************************************
!
               IF ( Km/=0 ) THEN
                  IF ( Km>1 ) THEN
                     spag_nextblock_1 = 16
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  N = 0
                  l3 = 8*Km
                  IF ( Mf(7)/=0 .AND. Mf(8)/=0 ) Badfor = .TRUE.
                  DO l = 1 , 6
                     IF ( Mf(l)/=0 .AND. Mf(l)/=2 ) THEN
                        IF ( Mf(l)==-32767 ) THEN
                           DO l6 = l , 6
                              M(l6) = 0
                           ENDDO
                        ELSE
                           Badfor = .TRUE.
                        ENDIF
                     ENDIF
                  ENDDO
                  DO l = 1 , 6
                     l5 = l3 + l
                     I(l5) = M(l)
                     save(l5) = M(l)
                  ENDDO
                  spag_nextblock_1 = 20
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  nn = 14
                  N = 0
                  Id = M(1)
                  l1 = 1
                  IF ( Mf(1)/=1 .OR. Mf(2)/=1 ) Badfor = .TRUE.
                  DO l = 3 , 8
                     IF ( Mf(l)/=0 .AND. Mf(l)/=2 ) THEN
                        IF ( Mf(l)==-32767 ) THEN
                           DO l5 = l , 8
                              M(l5) = 0
                           ENDDO
                        ELSE
                           Badfor = .TRUE.
                        ENDIF
                     ENDIF
                  ENDDO
                  IF ( M(1)<=0 .OR. M(2)<=0 ) Baddat = .TRUE.
                  DO l = 1 , 8
                     I(l) = M(l)
                     save(l) = M(l)
                  ENDDO
                  spag_nextblock_1 = 20
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDIF
         kz = K - 300
         IF ( kz<=39 ) THEN
            IF ( kz==21 ) THEN
!
!*******     321-CEMLOOP     *******************************************
!
               IF ( M(1)<=0 .OR. M(13)<0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( M(3)==0 ) THEN
                  dx1 = z(4) - z(10)
                  dy1 = z(5) - z(11)
                  dz1 = z(6) - z(12)
                  dx2 = z(7) - z(10)
                  dy2 = z(8) - z(11)
                  dz2 = z(9) - z(12)
                  dl1 = dx1**2 + dy1**2 + dz1**2
                  dl2 = dx2**2 + dy2**2 + dz2**2
                  IF ( abs(dl1-dl2)>1.E-4 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  dc1 = dy1*dz2 - dy2*dz1
                  dc2 = dx2*dz1 - dx1*dz2
                  dc3 = dx1*dy2 - dy1*dx2
                  dlc = sqrt(dc1**2+dc2**2+dc3**2)
                  IF ( dlc/sqrt(dl2)<.0001 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  N = 13
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  IF ( M(5)/=0 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  DO iem = 7 , 13
                     IF ( M(iem)/=0 ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
                  N = 13
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( kz==22 .OR. kz==26 ) THEN
!
!*******    322-SPCFLD,   326-REMFLUX      *****************************
!
               IF ( M(1)<=0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( M(2)<0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( M(6)==-1 ) THEN
                  DO l = 7 , 8
                     IF ( Mf(l)/=0 ) THEN
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
                  N = 6
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( Mf(7)==3 ) THEN
                  IF ( M(7)/=thru ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( Mf(6)/=1 .OR. Mf(8)/=1 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  l1 = 6
                  l2 = 9
                  ii = M(l1) - 1
                  l2 = M(l2) - M(l1)
                  IF ( ii<0 .OR. l2<=0 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  l1 = 1
                  DO l = 1 , 5
                     I(l) = M(l)
                  ENDDO
                  N = 6
                  DO l = l1 , l2
                     I(6) = l + ii
                     CALL write(209,I,N,0)
                  ENDDO
                  I(6) = ii + l2 + 1
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  DO l = 6 , 8
                     IF ( Mf(l)/=1 .AND. Mf(l)/=0 ) THEN
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( M(l)<0 ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( M(l)/=0 ) THEN
                        N = N + 6
                        I(N-5) = M(1)
                        I(N-4) = M(2)
                        I(N-3) = M(3)
                        I(N-2) = M(4)
                        I(N-1) = M(5)
                        I(N) = M(l)
                     ENDIF
                  ENDDO
                  IF ( N<=0 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( kz==23 ) THEN
!
!*****   323-CIS2D8   **************************************************
!
               IF ( M(1)<=0 .OR. M(2)<=0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( M(11)<0 .OR. z(12)<0. ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( M(11)==0 ) M(11) = 2
               IF ( M(11)/=2 .AND. M(11)/=3 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               DO l = 3 , 10
                  IF ( M(l)<=0 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
               DO l = 3 , 9
                  lp1 = l + 1
                  DO lll = lp1 , 10
                     IF ( M(l)==M(lll) ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
               ENDDO
               N = 12
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kz==24 ) THEN
!
!*****   324-PIS2D8   **************************************************
!
               IF ( z(3)<=0. ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( M(1)<=0 .OR. M(2)<=0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               N = 3
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kz==25 ) THEN
!
!*****   325-GEMLOOP   *************************************************
!
               IF ( Mf(1)/=1 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( Mf(2)/=2 .AND. Mf(2)/=0 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( Mf(3)/=1 .AND. Mf(3)/=0 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( M(1)<=0 .OR. M(3)<0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!
!     FOR NOW, CID MUST BE 0
!
               IF ( M(3)/=0 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               npts = 0
               DO l = 4 , 49 , 3
                  IF ( Mf(l)==3 ) THEN
                     spag_nextblock_1 = 40
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  npts = npts + 1
                  IF ( Mf(l)/=2 .AND. Mf(l)/=0 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( Mf(l+1)/=2 .AND. Mf(l+1)/=0 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( Mf(l+2)/=2 .AND. Mf(l+2)/=0 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kz==27 ) THEN
!
!*****   327-BFIELD   **************************************************
!
               IF ( M(1)<0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( M(2)==-1 ) THEN
                  DO l = 3 , 8
                     IF ( Mf(l)/=0 ) THEN
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
                  N = 2
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( Mf(3)==3 ) THEN
                  IF ( M(3)/=thru ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( Mf(2)/=1 .OR. Mf(4)/=1 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  l1 = 2
                  l2 = 5
                  ii = M(l1) - 1
                  l2 = M(l2) - M(l1)
                  IF ( ii<0 .OR. l2<=0 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  l1 = 1
                  I(1) = M(1)
                  N = 2
                  DO l = l1 , l2
                     I(2) = l + ii
                     CALL write(201,I,N,0)
                  ENDDO
                  I(2) = ii + l2 + 1
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  DO l = 2 , 8
                     IF ( Mf(l)/=1 .AND. Mf(l)/=0 ) THEN
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( M(l)<0 ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( M(l)/=0 ) THEN
                        N = N + 2
                        I(N-1) = M(1)
                        I(N) = M(l)
                     ENDIF
                  ENDDO
                  IF ( N<=0 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( kz==28 ) THEN
!
!*****   328-MDIPOLE     ***********************************************
!
               IF ( M(1)<=0 .OR. M(2)<0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( z(9)<0. .OR. z(10)<0. ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               N = 10
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kz==33 ) THEN
               spag_nextblock_1 = 34
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kz==34 ) THEN
               spag_nextblock_1 = 35
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kz==35 ) THEN
               N = 6
               spag_nextblock_1 = 33
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kz==36 ) THEN
               N = 8
               spag_nextblock_1 = 33
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         CALL page2(2)
         WRITE (Nout,99004) Sfm
99004    FORMAT (A25,' 322, ILLEGAL ENTRY TO IFS4P.')
         Abort = .TRUE.
         RETURN 1
      CASE (3)
         Badfor = .TRUE.
         RETURN 1
      CASE (4)
         Baddat = .TRUE.
         RETURN 1
      CASE (5)
         DO l = 1 , N
            I(l) = M(l)
         ENDDO
         spag_nextblock_1 = 6
      CASE (6)
         RETURN
      CASE (7)
         RETURN 3
      CASE (8)
         IF ( M(1)<=0 .OR. M(i1+2)<=0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO l = 2 , i1
            IF ( M(l)<=0 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( l/=2 ) THEN
               DO l1 = l , i1
                  IF ( M(l-1)==M(l1) ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
         N = i1 + 2
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      CASE (9)
         kout = kout + 1
         J(kout) = M(1)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      CASE (10)
         IF ( M(1)<=0 .OR. M(2)<=0 .OR. z(3)<0.0 .OR. z(4)<=z(3) ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         N = 4
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      CASE (11)
!
!*****         199-PLOAD2,239-QBDY1,242-QVOL   *************************
!
         IF ( Km/=0 ) THEN
            l = 1
         ELSE
            IF ( Mf(1)/=1 .OR. Mf(2)/=2 .AND. Mf(2)/=0 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( M(1)<=0 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            l = 3
            isid = M(1)
            iqvl = M(2)
         ENDIF
         IF ( Mf(8)==3 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ntot = 0
         k2078 = 209
         SPAG_Loop_1_1: DO WHILE ( M(l)/=0 )
            IF ( M(l)<0 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Mf(l)==3 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Mf(l+1)==3 ) THEN
               IF ( M(l+1)/=thru ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( Mf(l+3)/=1 .AND. Mf(l+3)/=0 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               l1 = M(l) - 1
               l2 = M(l+3) - l1
               IF ( l2<=1 .OR. l1<0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               DO ii = 1 , l2
                  N = N + 3
                  I(N-2) = isid
                  I(N-1) = iqvl
                  I(N) = ii + l1
                  IF ( N>=48 ) THEN
                     CALL write(k2078,I,N,0)
                     ntot = ntot + N
                     N = 0
                  ENDIF
               ENDDO
               l = l + 4
            ELSE
               IF ( Mf(l)/=1 .AND. Mf(l)/=0 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               N = N + 3
               I(N-2) = isid
               I(N-1) = iqvl
               I(N) = M(l)
               l = l + 1
               IF ( N>=48 ) THEN
                  CALL write(k2078,I,N,0)
                  ntot = ntot + N
                  N = 0
               ENDIF
            ENDIF
            IF ( l>8 ) EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
         T4(2,K) = T4(2,K) + ntot
         IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
            Km = 1
         ELSE
            Km = 0
         ENDIF
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
      CASE (12)
         DO l = 3 , 6
            IF ( Mf(l)/=0 .AND. Mf(l)/=2 ) Badfor = .TRUE.
         ENDDO
         spag_nextblock_1 = 13
      CASE (13)
         IF ( Mf(1)/=1 .OR. Mf(2)/=1 ) Badfor = .TRUE.
         IF ( M(1)<=0 .OR. M(2)<=0 ) Baddat = .TRUE.
         DO l = 1 , N
            I(l) = M(l)
            save(l) = M(l)
         ENDDO
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
      CASE (14)
         DO l = 3 , 4
            IF ( Mf(l)/=0 .AND. Mf(l)/=2 ) Badfor = .TRUE.
         ENDDO
         spag_nextblock_1 = 13
         CYCLE SPAG_DispatchLoop_1
      CASE (15)
         IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
            Kn = 1
            Km = Km + 1
         ELSE
            Km = 0
            Kn = 0
         ENDIF
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
      CASE (16)
         IF ( Mf(2)==3 .OR. Mf(5)==3 ) THEN
            N = 0
            IF ( Mf(7)/=0 .OR. Mf(8)/=0 ) THEN
               IF ( Mf(4)/=0 .OR. Mf(5)/=-32767 ) THEN
                  Badfor = .TRUE.
                  spag_nextblock_1 = 15
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
            l1 = -1
            SPAG_Loop_1_2: DO l = 1 , 4 , 3
               IF ( Mf(l)/=0 .OR. Mf(l+1)/=0 .OR. Mf(l+2)/=0 ) THEN
                  IF ( Mf(l)==1 .AND. Mf(l+1)==3 .AND. Mf(l+2)==1 ) THEN
                     l1 = l1 + 1
                     l2 = l1 + l
                     IF ( M(l2)>0 .AND. M(l2+1)==thru .AND. M(l2+3)>M(l2) ) THEN
                        l3 = M(l2)
                        l4 = M(l2+3)
                        DO l5 = l3 , l4
                           save(2) = l5
                           CALL write(209,save,nn,0)
                        ENDDO
                     ELSE
                        Baddat = .TRUE.
                     ENDIF
                  ELSE
                     IF ( Mf(l+1)==-32767 ) EXIT SPAG_Loop_1_2
                     Badfor = .TRUE.
                  ENDIF
               ENDIF
            ENDDO SPAG_Loop_1_2
         ELSE
            N = 0
            SPAG_Loop_1_3: DO l = 1 , 8
               IF ( Mf(l)/=0 ) THEN
                  IF ( Mf(l)/=1 ) THEN
                     IF ( Mf(l)==-32767 ) EXIT SPAG_Loop_1_3
                     Badfor = .TRUE.
                  ELSEIF ( M(l)>0 ) THEN
                     save(2) = M(l)
                     CALL write(209,save,nn,0)
                  ELSE
                     Baddat = .TRUE.
                  ENDIF
               ENDIF
            ENDDO SPAG_Loop_1_3
         ENDIF
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
      CASE (17)
!
!*******       203-TEMPP3         **************************************
!
         IF ( Km/=0 ) THEN
            IF ( Km>2 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            N = 0
            l3 = 8*Km
            DO l = 1 , 7 , 2
               IF ( Mf(l)/=0 .OR. Mf(l+1)/=0 ) THEN
                  IF ( Mf(l)/=-32767 ) THEN
                     IF ( Mf(l)/=0 .AND. Mf(l)/=2 .OR. Mf(l+1)/=0 .AND. Mf(l+1)/=2 ) Badfor = .TRUE.
                     IF ( zz>=z(l) ) Baddat = .TRUE.
                  ELSE
                     Mf(7) = 0
                     Mf(8) = 0
                  ENDIF
               ENDIF
               zz = z(l)
               l5 = l3 + l
               I(l5) = M(l)
               save(l5) = M(l)
               I(l5+1) = M(l+1)
               save(l5+1) = M(l+1)
            ENDDO
         ELSE
            nn = 24
            N = 0
            Id = M(1)
            l1 = 1
            IF ( Mf(1)/=1 .OR. Mf(2)/=1 ) Badfor = .TRUE.
            DO l = 3 , 8
               IF ( Mf(l)/=0 .AND. Mf(l)/=2 ) THEN
                  IF ( Mf(l)==-32767 ) THEN
                     DO l5 = l , 8
                        M(l5) = 0
                     ENDDO
                     Mf(7) = 0
                     Mf(8) = 0
                  ELSE
                     Badfor = .TRUE.
                  ENDIF
               ENDIF
            ENDDO
            IF ( M(1)<=0 .OR. M(2)<=0 ) Baddat = .TRUE.
            IF ( z(3)>=z(5) ) Baddat = .TRUE.
            zz = z(5)
            IF ( Mf(7)/=0 .OR. Mf(8)/=0 ) THEN
               IF ( zz>=z(7) ) Baddat = .TRUE.
            ENDIF
            zz = z(7)
            DO l = 1 , 8
               I(l) = M(l)
               save(l) = M(l)
            ENDDO
         ENDIF
         l1 = l1 + 8
         IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
            Km = Km + 1
            Kn = 1
            IF ( Km<3 ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
            Km = 0
            Kn = 0
         ENDIF
         IF ( l1<=nn ) THEN
            DO l = l1 , nn
               I(l) = 0
               save(l) = 0
            ENDDO
         ENDIF
         N = nn
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
      CASE (18)
         l1 = l1 + 8
         IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
            Km = Km + 1
            Kn = 1
            IF ( Km<2 ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
            Km = 0
            Kn = 0
         ENDIF
         IF ( l1<=nn ) THEN
            DO l = l1 , nn
               I(l) = 0
               save(l) = 0
            ENDDO
         ENDIF
         N = nn
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
      CASE (19)
         DO l = 5 , 8
            I(l+8) = 0
            save(l+8) = 0
!
!    TEMPG IS MODELLED AFTER TEMPP3
!    TEMPP4 IS MODELLED AFTER TEMPP1,EXCEPT THAT TEMPP1 HAS ONE LESS C
!
!
!*******      295-TEMPG     ********************************************
!
         ENDDO
         spag_nextblock_1 = 18
         CYCLE SPAG_DispatchLoop_1
      CASE (20)
         l1 = l1 + 8
         IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
            Km = Km + 1
            Kn = 1
            IF ( Km<2 ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
            Km = 0
            Kn = 0
         ENDIF
         IF ( l1<=nn ) THEN
            DO l = l1 , nn
               I(l) = 0
               save(l) = 0
            ENDDO
         ENDIF
         N = nn
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 20      IF ( M(1)<=0 .OR. M(6)<0 .OR. M(8)<=0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ifpdco(M(7)) ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         N = 5
         I(1) = M(1)
         I(2) = M(4)
         I(3) = M(6)
         I(4) = M(7)
         I(5) = M(8)
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
      CASE (21)
!
!*******       206-FSLIST         **************************************
!
         IF ( Km/=0 ) THEN
            l1 = 1
            l2 = 0
            spag_nextblock_1 = 24
            CYCLE SPAG_DispatchLoop_1
         ELSE
            ASSIGN 40 TO ret
            spag_nextblock_1 = 32
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 40      IF ( Mf(1)==0 .OR. Mf(1)==2 ) THEN
            IF ( .NOT.(Mf(1)==0 .OR. (Mf(1)==2 .AND. z(1)>0.0)) ) THEN
               spag_nextblock_1 = 23
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Mf(1)==0 ) M(1) = 1
            I(1) = M(1)
            N = 1
            l1 = 2
            l2 = 0
            IF ( Mf(2)/=3 ) THEN
               spag_nextblock_1 = 24
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( M(2)/=bcdaxi ) THEN
               spag_nextblock_1 = 23
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            N = N + 1
            I(N) = 0
            l1 = l1 + 1
            l2 = 1
            spag_nextblock_1 = 24
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 22
      CASE (22)
         Badfor = .TRUE.
         spag_nextblock_1 = 28
         CYCLE SPAG_DispatchLoop_1
      CASE (23)
         Baddat = .TRUE.
         spag_nextblock_1 = 28
         CYCLE SPAG_DispatchLoop_1
      CASE (24)
         DO l = l1 , 8
            l3 = l + l2
            IF ( Mf(l)==3 ) THEN
               spag_nextblock_1 = 26
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Mf(l)==0 ) THEN
               spag_nextblock_1 = 27
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Mf(l)/=1 ) THEN
               spag_nextblock_1 = 22
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( M(l3)<=0 ) THEN
               spag_nextblock_1 = 23
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            N = N + 1
            I(N) = M(l3)
         ENDDO
         spag_nextblock_1 = 25
      CASE (25)
         IF ( N<=0 ) THEN
            spag_nextblock_1 = 23
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 28
         CYCLE SPAG_DispatchLoop_1
      CASE (26)
         IF ( M(l3)/=bcdaxi ) THEN
            spag_nextblock_1 = 23
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         N = N + 1
         I(N) = 0
         spag_nextblock_1 = 27
      CASE (27)
         IF ( l/=8 ) THEN
            l = l + 1
            DO l2 = l , 8
               IF ( Mf(l2)/=0 ) THEN
                  spag_nextblock_1 = 22
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
         IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
            spag_nextblock_1 = 23
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 25
         CYCLE SPAG_DispatchLoop_1
      CASE (28)
         IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
            Kn = 1
            Km = Km + 1
         ELSE
            Km = 0
            Kn = 0
            N = N + 1
            I(N) = -1
         ENDIF
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 60      DO l = 1 , 5 , 4
            IF ( M(l)/=0 .OR. M(l+1)/=0 .OR. M(l+2)/=0 .OR. M(l+3)/=0 ) THEN
               IF ( M(l)<=0 .OR. z(l+1)<=0.0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               N = N + 4
               IF ( N>4 .AND. M(l)==M(l-4) ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( M(l)<=99999 ) THEN
                  I(N-3) = M(l)
                  I(N-2) = M(l+1)
                  I(N-1) = M(l+2)
                  I(N) = M(l+3)
               ELSE
                  CALL page2(2)
                  WRITE (Nout,99008) Ufm
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDDO
         IF ( N<=0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 80      IF ( M(1)<=0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO l = 3 , 7 , 2
            IF ( M(l)/=0 .OR. M(l+1)/=0 ) THEN
               IF ( M(l)<=0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               N = N + 3
               I(N-2) = M(1)
               I(N-1) = M(l)
               I(N) = M(l+1)
            ENDIF
         ENDDO
         IF ( N<=0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 100     IF ( M(1)<=0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( M(1)<=99999 ) THEN
            DO l = 2 , kfl
               IF ( M(l)<=0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( l/=kfl ) THEN
                  l2 = l + 1
                  DO l1 = l2 , kfl
                     IF ( M(l)==M(l1) ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
            I(1) = M(1)
            N = kfl + 3
            IF ( Mf(6)==0 ) M(6) = 1
            IF ( Mf(7)==0 ) M(7) = 1
            I(kfl+2) = M(6)
            I(kfl+3) = M(7)
            DO l = 1 , kfl
               I(l+1) = M(l+1)
            ENDDO
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ELSE
            CALL page2(2)
            WRITE (Nout,99008) Ufm
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
      CASE (29)
         Badfor = .TRUE.
         spag_nextblock_1 = 31
         CYCLE SPAG_DispatchLoop_1
      CASE (30)
         IF ( M(1)<=Naxf ) Baddat = .TRUE.
         IF ( M(1)>0 ) THEN
            l2 = M(1)
         ELSE
            CALL write(215,0,1,0)
         ENDIF
         l3 = M(4)
         DO l = l2 , l3 , l1
            CALL write(215,l,1,0)
         ENDDO
         Naxf = l3
         spag_nextblock_1 = 31
      CASE (31)
         IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
            Kn = 1
            Km = Km + 1
         ELSE
            Km = 0
            Kn = 0
            IF ( Naxf>=100 ) THEN
               CALL page2(2)
               WRITE (Nout,99005) Ufm , Naxf
99005          FORMAT (A23,' 4125, MAXIMUM ALLOWABLE HARMONIC ID IS 99.  DATA ','CONTAINS MAXIMUM =',I20)
               Abort = .TRUE.
            ENDIF
            N = N + 1
            I(N) = -1
         ENDIF
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
      CASE (32)
         IF ( Iaxf<=0 ) THEN
            IF ( Lharm ) CALL page2(2)
            IF ( Lharm ) WRITE (Nout,99006) Ufm
99006       FORMAT (A23,' 4122, AXIF CARD REQUIRED.')
            Lharm = .FALSE.
            Abort = .TRUE.
         ENDIF
!
!*******       213-BDYLIST        **************************************
!
!
!*******       214-FREEPT         **************************************
!
         GOTO ret
      CASE (33)
         DO l = 1 , N
            IF ( M(l)<=0 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         n1 = N - 1
         DO l = 3 , n1
            l2 = l + 1
            DO l1 = l2 , N
               IF ( M(l)==M(l1) ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDDO
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      CASE (34)
!
!*******       219-CHEXA1,  333-CFHEX1   *******************************
!
         IF ( Mf(15)/=0 .OR. Mf(16)/=0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         N = 10
         spag_nextblock_1 = 33
         CYCLE SPAG_DispatchLoop_1
      CASE (35)
!
!*******       220-CHEXA2,  334-CFHEX2   *******************************
!
         IF ( Mf(15)/=0 .OR. Mf(16)/=0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         N = 10
         spag_nextblock_1 = 33
         CYCLE SPAG_DispatchLoop_1
 120     IF ( Mf(1)/=3 .OR. M(1)==nm(1) .AND. M(2)==nm(2) ) THEN
            spag_nextblock_1 = 36
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ifo = M(4)
         ty1 = M(5)
         ity1 = 2*mod(ty1,2)
         ty2 = M(6)
         IF ( Mach==12 ) THEN
            IF ( ty2==2 .OR. ty2==4 ) ty2 = ty2 - 1
         ENDIF
         IF ( ifo/=1 .AND. ifo/=2 .AND. ifo/=6 ) THEN
            spag_nextblock_1 = 36
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ty1<=0 .OR. ty1>4 .OR. ty2<=0 .OR. ty2>4 ) THEN
            spag_nextblock_1 = 36
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ty2==1 .AND. ty1==3 ) THEN
            spag_nextblock_1 = 36
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         nm(1) = M(1)
         nm(2) = M(2)
         IF ( Mf(6)/=0 .OR. Mf(7)/=0 .OR. Mf(8)/=0 ) THEN
            spag_nextblock_1 = 37
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( M1f(2)/=3 .OR. M1(3)/=nm(1) .OR. M1(4)/=nm(2) ) THEN
            spag_nextblock_1 = 37
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         N = 9
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 140     I(3) = M(4)
         IF ( ty1/=1 ) THEN
            N = 4
            I(4) = M(5)
            IF ( ty1/=2 .AND. ty1/=3 ) THEN
               N = 6
               I(5) = M(6)
               I(6) = M(7)
            ENDIF
         ENDIF
         IF ( M1(1)==0 .AND. M1(2)==0 ) GOTO 160
         N = N + 2
         I(N-1) = -1
         I(N) = -1
         IF ( M1(1)/=T1(1,K) .OR. M1(2)/=T1(2,K) .OR. M1(3)/=nm(1) .OR. M1(4)/=nm(2) ) THEN
            N = N + 2
            I(N-1) = -1
            I(N) = -1
         ENDIF
         spag_nextblock_1 = 39
         CYCLE SPAG_DispatchLoop_1
      CASE (36)
         nm(1) = M(1)
         nm(2) = M(2)
         spag_nextblock_1 = 37
      CASE (37)
         Abort = .TRUE.
         CALL page2(2)
         WRITE (Nout,99007) Ufm , nm(1) , nm(2)
99007    FORMAT (A23,' 4126, BAD DATA OR FORMAT OR NON-UNIQUE NAME, DMIAX',1X,2A4)
         IF ( M1(1)==0 .AND. M1(2)==0 ) GOTO 160
         spag_nextblock_1 = 39
         CYCLE SPAG_DispatchLoop_1
      CASE (38)
         IF ( Mf(l1)/=1 ) THEN
            I(1) = M(l2-2)
         ELSEIF ( M(l2)<0 ) THEN
            I(1) = 500000*(1-M(l2)*2) + M(l2-2)
         ELSE
            I(1) = 1000000*(1+M(l2)) + M(l2-2)
         ENDIF
         GOTO ret
      CASE (39)
         Km = 0
         Kn = 0
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 160     Kn = 1
         Km = Km + 1
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 180     IF ( Mf(1)/=1 .OR. Mf(2)/=3 .OR. Mf(3)/=3 ) Badfor = .TRUE.
         DO l = 4 , 8
            IF ( Mf(l)/=0 ) Badfor = .TRUE.
         ENDDO
         IF ( M(1)<2 .OR. M(2)/=bcds .AND. M(2)/=bcda .OR. M(4)/=bcds .AND. M(4)/=bcda ) Baddat = .TRUE.
         IF ( mod(M(1),2)/=0 ) Baddat = .TRUE.
         IF ( M(2)==bcds ) M(2) = +1
         IF ( M(2)==bcda ) M(2) = -1
         IF ( M(4)==bcds ) M(3) = +1
         IF ( M(4)==bcda ) M(3) = -1
         N = 3
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      CASE (40)
         IF ( npts<2 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO lll = l , 49
            M(lll) = 0
         ENDDO
         DO l = 1 , 3
            I(l) = M(l)
         ENDDO
         I(4) = npts
         DO l = 4 , 48
            I(l+1) = M(l)
         ENDDO
         N = 49
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99008 FORMAT (A23,' 5004, FLUID POINT ID ON CFLUID OR RINGFL CARD ','EXCEEDS 999999 LIMIT')
!
END SUBROUTINE ifs4p
