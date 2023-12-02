!*==ifs4p.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifs4p(*,*,*)
   USE c_cifs4p
   USE c_ifpdta
   USE c_ifpx1
   USE c_ifpx3
   USE c_machin
   USE c_system
   USE c_xmssg
   IMPLICIT NONE
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
         IF ( k<=100 ) THEN
            IF ( k==1 .OR. k==2 .OR. k==3 .OR. k==4 .OR. k==5 .OR. k==6 .OR. k==7 .OR. k==8 .OR. k==9 .OR. k==10 .OR. k==11 .OR.    &
               & k==12 .OR. k==13 .OR. k==14 .OR. k==15 .OR. k==16 .OR. k==17 .OR. k==18 .OR. k==19 .OR. k==20 .OR. k==21 .OR.      &
               & k==22 .OR. k==23 .OR. k==24 .OR. k==25 .OR. k==26 .OR. k==27 .OR. k==28 .OR. k==29 .OR. k==30 .OR. k==31 .OR.      &
               & k==32 .OR. k==33 .OR. k==34 .OR. k==35 .OR. k==36 .OR. k==37 .OR. k==38 .OR. k==39 .OR. k==40 .OR. k==41 .OR.      &
               & k==42 .OR. k==43 .OR. k==44 .OR. k==45 .OR. k==46 .OR. k==47 .OR. k==48 .OR. k==49 .OR. k==50 .OR. k==51 .OR.      &
               & k==52 .OR. k==53 .OR. k==54 .OR. k==55 .OR. k==56 .OR. k==57 .OR. k==58 .OR. k==59 .OR. k==60 .OR. k==61 .OR.      &
               & k==62 .OR. k==63 .OR. k==64 .OR. k==65 .OR. k==66 .OR. k==67 .OR. k==68 .OR. k==69 .OR. k==70 .OR. k==71 .OR.      &
               & k==72 .OR. k==73 .OR. k==74 .OR. k==75 .OR. k==76 .OR. k==77 .OR. k==78 .OR. k==81 .OR. k==82 .OR. k==83 .OR.      &
               & k==84 .OR. k==85 .OR. k==86 .OR. k==87 .OR. k==88 .OR. k==89 .OR. k==92 .OR. k==93 .OR. k==94 .OR. k==95 .OR.      &
               & k==96 .OR. k==97 .OR. k==99 .OR. k==100 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( k==79 ) THEN
!
!******              79-CTRIARG,80-CTRAPRG             ****************
!
               i1 = 4
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( k==80 ) THEN
               i1 = 5
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( k==90 .OR. k==91 ) THEN
!
!*******       MATS1,MATT1        **************************************
!
               DO l = 1 , 11
                  IF ( m(l)<0 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  i(l) = m(l)
               ENDDO
               n = 11
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( k==98 ) THEN
!
!*******       TEMPD              **************************************
!
               DO l = 1 , 7 , 2
                  IF ( m(l)/=0 .OR. m(l+1)/=0 ) THEN
                     IF ( m(l)<=0 ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     n = n + 2
                     i(n-1) = m(l)
                     i(n) = m(l+1)
                     IF ( n>2 ) THEN
                        DO l1 = 4 , n , 2
                           IF ( i(n-1)==i(l1-3) ) THEN
                              spag_nextblock_1 = 4
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                        ENDDO
                     ENDIF
                  ENDIF
               ENDDO
               IF ( n<=0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         IF ( kx<=100 ) THEN
            IF ( kx==1 .OR. kx==3 .OR. kx==6 .OR. kx==7 .OR. kx==8 .OR. kx==9 .OR. kx==10 .OR. kx==11 .OR. kx==12 .OR. kx==13 .OR.  &
               & kx==14 .OR. kx==15 .OR. kx==16 .OR. kx==17 .OR. kx==18 .OR. kx==19 .OR. kx==20 .OR. kx==21 .OR. kx==23 .OR.        &
               & kx==25 .OR. kx==26 .OR. kx==27 .OR. kx==28 .OR. kx==29 .OR. kx==30 .OR. kx==31 .OR. kx==32 .OR. kx==33 .OR.        &
               & kx==34 .OR. kx==35 .OR. kx==36 .OR. kx==37 .OR. kx==38 .OR. kx==39 .OR. kx==40 .OR. kx==41 .OR. kx==42 .OR.        &
               & kx==43 .OR. kx==44 .OR. kx==45 .OR. kx==46 .OR. kx==47 .OR. kx==48 .OR. kx==49 .OR. kx==50 .OR. kx==51 .OR.        &
               & kx==52 .OR. kx==53 .OR. kx==54 .OR. kx==55 .OR. kx==56 .OR. kx==57 .OR. kx==58 .OR. kx==59 .OR. kx==60 .OR.        &
               & kx==61 .OR. kx==62 .OR. kx==63 .OR. kx==64 .OR. kx==65 .OR. kx==66 .OR. kx==67 .OR. kx==68 .OR. kx==69 .OR.        &
               & kx==70 .OR. kx==71 .OR. kx==72 .OR. kx==73 .OR. kx==74 .OR. kx==75 .OR. kx==76 .OR. kx==77 .OR. kx==78 .OR.        &
               & kx==79 .OR. kx==80 .OR. kx==81 .OR. kx==82 .OR. kx==83 .OR. kx==84 .OR. kx==85 .OR. kx==86 .OR. kx==87 .OR.        &
               & kx==88 .OR. kx==90 .OR. kx==91 .OR. kx==92 .OR. kx==93 .OR. kx==94 .OR. kx==97 .OR. kx==98 .OR. kx==100 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( kx==2 .OR. kx==89 ) THEN
!
!**************    MATT2,189-MATT3     *********************************
!
               DO l = 1 , 16
                  IF ( m(l)<0 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  i(l) = m(l)
               ENDDO
               IF ( m(1)==0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               n = 16
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kx==4 ) THEN
!
!******           104-CTORDRG           ************************
!
               IF ( m(1)<=0 .OR. m(3)<=0 .OR. m(4)<=0 .OR. m(3)==m(4) .OR. z(5)<0.0 .OR. z(5)>180.0 .OR. z(6)<0.0 .OR. z(6)>180.0 ) &
                  & THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( mf(2)==0 ) m(2) = m(1)
               IF ( m(2)<=0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               n = 7
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kx==5 .OR. kx==24 ) THEN
!
!*******       SPOINT,124-EPOINT    ************************************
!
               IF ( mf(2)==3 ) THEN
                  IF ( m(2)/=thru ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( mf(1)/=1 .OR. mf(3)/=1 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  k2078 = 208
                  IF ( k==124 ) k2078 = 207
                  l1 = 1
                  l2 = 4
                  DO l = l2 , 8
                     IF ( mf(l)/=0 ) THEN
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
                  IF ( m(l2)>9999999 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  ii = m(l1) - 1
                  l2 = m(l2) - m(l1)
                  IF ( ii<0 .OR. l2<=0 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  l1 = 1
                  DO l = 1 , l2
                     ii = ii + 1
                     CALL write(k2078,ii,1,0)
                  ENDDO
                  i(1) = ii + 1
                  n = 1
                  spag_nextblock_1 = 6
               ELSE
                  DO l = 1 , 8
                     IF ( mf(l)/=1 .AND. mf(l)/=0 ) THEN
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( m(l)<0 ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( m(l)/=0 ) THEN
                        IF ( m(l)>999999 ) THEN
                           spag_nextblock_1 = 4
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        n = n + 1
                        i(n) = m(l)
                        IF ( n>1 ) THEN
                           DO l1 = 2 , n
                              IF ( i(n)==i(l1-1) ) THEN
                                 spag_nextblock_1 = 4
                                 CYCLE SPAG_DispatchLoop_1
                              ENDIF
                           ENDDO
                        ENDIF
                     ENDIF
                  ENDDO
                  IF ( n<=0 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  spag_nextblock_1 = 6
               ENDIF
               CYCLE
            ELSEIF ( kx==22 ) THEN
!
!*******         122-MAT3        *****************************
!
               IF ( m(1)<=0 .OR. z(2)<0. .OR. z(3)<0. .OR. z(4)<0. .OR. z(9)<0. .OR. z(10)<0. .OR. z(11)<0. ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( abs(z(5))>1. .OR. abs(z(6))>1. .OR. abs(z(7))>1. ) THEN
                  CALL page2(2)
                  WRITE (nout,99001) uwm , t1(1,k) , t1(2,k) , knt
99001             FORMAT (A25,' 301, BULK DATA CARD ',2A4,' CONTAINS INCONSISTENT',' DATA.',10X,'SORTED CARD COUNT =',I7)
               ENDIF
               n = 16
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kx==95 ) THEN
!
!
!*******       195-RANDPS       ****************************************
!
               IF ( m(1)<=0 .OR. m(2)<=0 .OR. m(3)<m(2) .OR. m(6)<0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( m(2)==m(3) .AND. z(5)/=0.0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               n = 6
               IF ( kout<=2 ) THEN
                  spag_nextblock_1 = 9
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( m(1)==j(kout) ) THEN
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( kout==j(1) ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kx==96 ) THEN
!
!*******       196-RANDT1       ****************************************
!
               IF ( kout>2 ) THEN
                  DO in = 3 , kout
                     IF ( m(1)==j(in) ) THEN
                        spag_nextblock_1 = 10
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
               ENDIF
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kx==99 ) THEN
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         IF ( ky<=100 ) THEN
            IF ( ky==1 ) THEN
!
!**********          201-TEMPP1          *******************************
!
               IF ( km/=0 ) THEN
                  spag_nextblock_1 = 16
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               nn = 6
               n = 6
               id = m(1)
               IF ( mf(5)==-32767 ) THEN
                  spag_nextblock_1 = 14
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( mf(7)/=0 .OR. mf(8)/=0 ) badfor = .TRUE.
               spag_nextblock_1 = 12
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( ky==2 ) THEN
!
!*******       202-TEMPP2         **************************************
!
               IF ( km/=0 ) THEN
                  spag_nextblock_1 = 16
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               nn = 8
               n = 8
               id = m(1)
               IF ( mf(5)==-32767 ) THEN
                  spag_nextblock_1 = 14
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( mf(7)/=0 .AND. mf(7)/=2 .OR. mf(8)/=0 .AND. mf(8)/=2 ) badfor = .TRUE.
               spag_nextblock_1 = 12
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( ky==3 ) THEN
               spag_nextblock_1 = 17
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( ky==4 ) THEN
!
!*******       204-TEMPRB         **************************************
!
               IF ( km/=0 ) THEN
                  IF ( km>1 ) THEN
                     spag_nextblock_1 = 16
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  n = 0
                  DO l = 1 , 8
                     IF ( mf(l)==-32767 ) THEN
                        spag_nextblock_1 = 19
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( mf(l)/=0 .AND. mf(l)/=2 ) badfor = .TRUE.
                     i(l+8) = m(l)
                     save(l+8) = m(l)
                  ENDDO
                  spag_nextblock_1 = 18
               ELSE
                  nn = 16
                  n = 0
                  id = m(1)
                  l1 = 1
                  IF ( mf(1)/=1 .OR. mf(2)/=1 ) badfor = .TRUE.
                  DO l = 3 , 8
                     IF ( mf(l)/=0 .AND. mf(l)/=2 ) THEN
                        IF ( mf(l)==-32767 ) THEN
                           DO l5 = l , 8
                              m(l5) = 0
                           ENDDO
                        ELSE
                           badfor = .TRUE.
                        ENDIF
                     ENDIF
                  ENDDO
                  IF ( m(1)<=0 .OR. m(2)<=0 ) baddat = .TRUE.
                  DO l = 1 , 8
                     i(l) = m(l)
                     save(l) = m(l)
                  ENDDO
                  spag_nextblock_1 = 18
               ENDIF
               CYCLE
            ELSEIF ( ky==5 ) THEN
!
!*******       205-GRIDB          **************************************
!
               ASSIGN 20 TO ret
               spag_nextblock_1 = 32
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( ky==6 ) THEN
               spag_nextblock_1 = 21
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( ky==7 ) THEN
!
!*******       207-RINGFL         **************************************
!
               ASSIGN 60 TO ret
               spag_nextblock_1 = 32
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( ky==8 ) THEN
!
!*******       208-PRESPT         **************************************
!
               ASSIGN 80 TO ret
               spag_nextblock_1 = 32
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( ky==9 ) THEN
!
!*******       209-CFLUID2        **************************************
!
               kfl = 2
               ASSIGN 100 TO ret
               spag_nextblock_1 = 32
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( ky==10 ) THEN
!
!*******       210-CFLUID3        **************************************
!
               kfl = 3
               ASSIGN 100 TO ret
               spag_nextblock_1 = 32
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( ky==11 ) THEN
!
!*******       211-CFLUID4        **************************************
!
               kfl = 4
               ASSIGN 100 TO ret
               spag_nextblock_1 = 32
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( ky==12 ) THEN
!
!*******       212-AXIF           **************************************
!
               n = 0
               IF ( km==0 ) THEN
                  IF ( iaxf>0 ) THEN
                     CALL page2(2)
                     WRITE (nout,99002) ufm
99002                FORMAT (A23,' 4121, ONLY ONE (1) AXIF CARD ALLOWED IN BULK DATA.')
                     abort = .TRUE.
                  ELSE
                     iaxf = iaxf + 1
                     IF ( mf(1)/=1 .OR. mf(2)/=0 .AND. mf(2)/=2 .OR. mf(3)/=0 .AND. mf(3)/=2 .OR. mf(4)/=0 .AND. mf(4)/=2 .OR.      &
                        & mf(5)/=3 ) badfor = .TRUE.
                     IF ( mf(7)/=0 .OR. mf(8)/=0 .OR. mf(6)/=0 .AND. mf(6)/=3 ) badfor = .TRUE.
                     IF ( mf(3)==0 ) m(3) = 1
                     IF ( m(5)/=bcdyes .AND. m(5)/=bcdno ) baddat = .TRUE.
                     IF ( m(5)==bcdyes ) m(5) = 1
                     IF ( m(5)==bcdno ) m(5) = 0
                     CALL write(215,m,5,0)
                     IF ( mf(6)==3 ) THEN
                        IF ( m(7)/=bcdnon ) baddat = .TRUE.
                        IF ( m1(1)==0 .AND. m1(2)==0 ) baddat = .TRUE.
                     ELSE
                        IF ( m1(1)/=0 .OR. m1(2)/=0 ) baddat = .TRUE.
                     ENDIF
                  ENDIF
                  spag_nextblock_1 = 31
               ELSEIF ( mf(2)/=3 ) THEN
                  DO l = 1 , 8
                     IF ( mf(l)/=0 ) THEN
                        IF ( mf(l)==1 ) THEN
                           IF ( m(l)<=naxf ) baddat = .TRUE.
                           n = n + 1
                           naxf = m(l)
                           i(n) = m(l)
                        ELSE
                           badfor = .TRUE.
                           n = 0
                           spag_nextblock_1 = 31
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                     ENDIF
                  ENDDO
                  IF ( n<=0 ) baddat = .TRUE.
                  spag_nextblock_1 = 31
               ELSEIF ( mf(4)==3 ) THEN
                  l1 = m(7)
                  l2 = l1
                  IF ( mf(1)/=1 .OR. mf(3)/=1 .OR. mf(5)/=1 .OR. mf(6)/=0 .OR. mf(7)/=0 .OR. mf(8)/=0 ) THEN
                     spag_nextblock_1 = 29
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( m(1)<m(4) .AND. m(7)>0 .AND. m(7)<=m(4) .AND. mod(m(4)-m(1),m(7))==0 ) THEN
                     spag_nextblock_1 = 30
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  baddat = .TRUE.
                  spag_nextblock_1 = 31
               ELSE
                  l1 = 1
                  l2 = 1
                  IF ( mf(1)/=1 .OR. mf(3)/=1 ) THEN
                     spag_nextblock_1 = 29
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  DO l = 4 , 8
                     IF ( mf(l)/=0 ) THEN
                        spag_nextblock_1 = 29
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
                  IF ( m(1)<m(4) .AND. m(1)>=0 ) THEN
                     spag_nextblock_1 = 30
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  baddat = .TRUE.
                  spag_nextblock_1 = 31
               ENDIF
               CYCLE
            ELSEIF ( ky==13 ) THEN
               spag_nextblock_1 = 21
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( ky==14 ) THEN
               ASSIGN 80 TO ret
               spag_nextblock_1 = 32
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( ky==15 .OR. ky==16 .OR. ky==23 .OR. ky==24 .OR. ky==25 .OR. ky==26 .OR. ky==27 .OR. ky==28 .OR. ky==29 .OR.    &
                   & ky==30 .OR. ky==31 .OR. ky==32 .OR. ky==33 .OR. ky==34 .OR. ky==35 .OR. ky==36 .OR. ky==37 .OR. ky==38 .OR.    &
                   & ky==40 .OR. ky==41 .OR. ky==43 .OR. ky==44 .OR. ky==45 .OR. ky==46 .OR. ky==47 .OR. ky==48 .OR. ky==49 .OR.    &
                   & ky==50 .OR. ky==51 .OR. ky==52 .OR. ky==53 .OR. ky==54 .OR. ky==55 .OR. ky==56 .OR. ky==57 .OR. ky==58 .OR.    &
                   & ky==59 .OR. ky==60 .OR. ky==61 .OR. ky==62 .OR. ky==63 .OR. ky==64 .OR. ky==65 .OR. ky==66 .OR. ky==67 .OR.    &
                   & ky==68 .OR. ky==69 .OR. ky==70 .OR. ky==71 .OR. ky==72 .OR. ky==73 .OR. ky==74 .OR. ky==75 .OR. ky==76 .OR.    &
                   & ky==77 .OR. ky==78 .OR. ky==79 .OR. ky==80 .OR. ky==81 .OR. ky==82 .OR. ky==83 .OR. ky==84 .OR. ky==85 .OR.    &
                   & ky==86 .OR. ky==87 .OR. ky==88 .OR. ky==89 .OR. ky==90 .OR. ky==91 .OR. ky==92 .OR. ky==93 .OR. ky==94 .OR.    &
                   & ky==97 .OR. ky==98 .OR. ky==99 .OR. ky==100 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( ky==17 ) THEN
!
!*******       217-CTETRA,  335-CFTETRA  *******************************
!
               n = 6
               spag_nextblock_1 = 33
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( ky==18 ) THEN
!
!*******       218-CWEDGE,  336-CFWEDGE  *******************************
!
               n = 8
               spag_nextblock_1 = 33
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( ky==19 ) THEN
               spag_nextblock_1 = 34
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( ky==20 ) THEN
               spag_nextblock_1 = 35
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( ky==21 ) THEN
!
!*******       221-DMIAX          **************************************
!
               IF ( fphys1 ) THEN
                  fphys1 = .FALSE.
                  nm(1) = 0
                  nm(2) = 0
               ENDIF
               IF ( km/=0 ) THEN
                  IF ( m(1)<=0 .OR. m(2)<0 .OR. m(2)>6 ) THEN
                     spag_nextblock_1 = 37
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( mf(1)/=1 .OR. mf(2)/=1 .AND. mf(2)/=0 ) THEN
                     spag_nextblock_1 = 37
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( mf(4)/=0 .AND. mf(4)+ity1/=4 ) THEN
                     spag_nextblock_1 = 37
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( mf(5)/=0 .AND. ty1/=3 .AND. ty1/=4 ) THEN
                     spag_nextblock_1 = 37
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( iabs(m(3))>naxf ) THEN
                     spag_nextblock_1 = 37
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( mf(3)/=1 .AND. mf(3)/=0 ) THEN
                     spag_nextblock_1 = 37
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  n = 3
                  i(2) = m(2)
                  l1 = 3
                  l2 = 3
                  ASSIGN 140 TO ret
                  spag_nextblock_1 = 38
               ELSEIF ( m(3)==0 ) THEN
                  ASSIGN 120 TO ret
                  spag_nextblock_1 = 32
               ELSE
                  IF ( m(1)/=nm(1) .OR. m(2)/=nm(2) ) THEN
                     spag_nextblock_1 = 36
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( mf(2)/=1 .OR. mf(3)/=1 .AND. mf(3)/=0 ) THEN
                     spag_nextblock_1 = 37
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( mf(4)/=1 .AND. mf(4)/=0 ) THEN
                     spag_nextblock_1 = 37
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( m(3)<=0 .OR. m(4)<0 .OR. m(4)>6 ) THEN
                     spag_nextblock_1 = 37
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( iabs(m(5))>naxf ) THEN
                     spag_nextblock_1 = 37
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( mf(5)/=0 .OR. mf(6)/=0 .OR. mf(7)/=0 .OR. mf(8)/=0 ) THEN
                     spag_nextblock_1 = 37
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
                     spag_nextblock_1 = 37
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  n = 2
                  i(2) = m(4)
                  l1 = 4
                  l2 = 5
                  ASSIGN 160 TO ret
                  spag_nextblock_1 = 38
               ENDIF
               CYCLE
            ELSEIF ( ky==22 ) THEN
!
!*******       222-FLSYM          **************************************
!
               IF ( lflsym ) THEN
                  CALL page2(2)
                  WRITE (nout,99003) ufm
99003             FORMAT (A23,' 4123, ONLY ONE (1) FLSYM CARD ALLOWED IN BULK DATA')
                  abort = .TRUE.
                  spag_nextblock_1 = 6
               ELSE
                  lflsym = .TRUE.
                  ASSIGN 180 TO ret
                  spag_nextblock_1 = 32
               ENDIF
               CYCLE
            ELSEIF ( ky==39 .OR. ky==42 ) THEN
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( ky==95 ) THEN
               spag_nextblock_1 = 17
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( ky==96 ) THEN
!
!*******      296-TEMPP4    ********************************************
!
               IF ( km/=0 ) THEN
                  IF ( km>1 ) THEN
                     spag_nextblock_1 = 16
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  n = 0
                  l3 = 8*km
                  IF ( mf(7)/=0 .AND. mf(8)/=0 ) badfor = .TRUE.
                  DO l = 1 , 6
                     IF ( mf(l)/=0 .AND. mf(l)/=2 ) THEN
                        IF ( mf(l)==-32767 ) THEN
                           DO l6 = l , 6
                              m(l6) = 0
                           ENDDO
                        ELSE
                           badfor = .TRUE.
                        ENDIF
                     ENDIF
                  ENDDO
                  DO l = 1 , 6
                     l5 = l3 + l
                     i(l5) = m(l)
                     save(l5) = m(l)
                  ENDDO
                  spag_nextblock_1 = 20
               ELSE
                  nn = 14
                  n = 0
                  id = m(1)
                  l1 = 1
                  IF ( mf(1)/=1 .OR. mf(2)/=1 ) badfor = .TRUE.
                  DO l = 3 , 8
                     IF ( mf(l)/=0 .AND. mf(l)/=2 ) THEN
                        IF ( mf(l)==-32767 ) THEN
                           DO l5 = l , 8
                              m(l5) = 0
                           ENDDO
                        ELSE
                           badfor = .TRUE.
                        ENDIF
                     ENDIF
                  ENDDO
                  IF ( m(1)<=0 .OR. m(2)<=0 ) baddat = .TRUE.
                  DO l = 1 , 8
                     i(l) = m(l)
                     save(l) = m(l)
                  ENDDO
                  spag_nextblock_1 = 20
               ENDIF
               CYCLE
            ENDIF
         ENDIF
         kz = k - 300
         IF ( kz<=39 ) THEN
            IF ( kz==21 ) THEN
!
!*******     321-CEMLOOP     *******************************************
!
               IF ( m(1)<=0 .OR. m(13)<0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( m(3)==0 ) THEN
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
                  n = 13
                  spag_nextblock_1 = 5
               ELSE
                  IF ( m(5)/=0 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  DO iem = 7 , 13
                     IF ( m(iem)/=0 ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
                  n = 13
                  spag_nextblock_1 = 5
               ENDIF
               CYCLE
            ELSEIF ( kz==22 .OR. kz==26 ) THEN
!
!*******    322-SPCFLD,   326-REMFLUX      *****************************
!
               IF ( m(1)<=0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( m(2)<0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( m(6)==-1 ) THEN
                  DO l = 7 , 8
                     IF ( mf(l)/=0 ) THEN
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
                  n = 6
                  spag_nextblock_1 = 5
               ELSEIF ( mf(7)==3 ) THEN
                  IF ( m(7)/=thru ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( mf(6)/=1 .OR. mf(8)/=1 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  l1 = 6
                  l2 = 9
                  ii = m(l1) - 1
                  l2 = m(l2) - m(l1)
                  IF ( ii<0 .OR. l2<=0 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  l1 = 1
                  DO l = 1 , 5
                     i(l) = m(l)
                  ENDDO
                  n = 6
                  DO l = l1 , l2
                     i(6) = l + ii
                     CALL write(209,i,n,0)
                  ENDDO
                  i(6) = ii + l2 + 1
                  spag_nextblock_1 = 6
               ELSE
                  DO l = 6 , 8
                     IF ( mf(l)/=1 .AND. mf(l)/=0 ) THEN
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( m(l)<0 ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( m(l)/=0 ) THEN
                        n = n + 6
                        i(n-5) = m(1)
                        i(n-4) = m(2)
                        i(n-3) = m(3)
                        i(n-2) = m(4)
                        i(n-1) = m(5)
                        i(n) = m(l)
                     ENDIF
                  ENDDO
                  IF ( n<=0 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  spag_nextblock_1 = 6
               ENDIF
               CYCLE
            ELSEIF ( kz==23 ) THEN
!
!*****   323-CIS2D8   **************************************************
!
               IF ( m(1)<=0 .OR. m(2)<=0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( m(11)<0 .OR. z(12)<0. ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( m(11)==0 ) m(11) = 2
               IF ( m(11)/=2 .AND. m(11)/=3 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               DO l = 3 , 10
                  IF ( m(l)<=0 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
               DO l = 3 , 9
                  lp1 = l + 1
                  DO lll = lp1 , 10
                     IF ( m(l)==m(lll) ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
               ENDDO
               n = 12
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
               IF ( m(1)<=0 .OR. m(2)<=0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               n = 3
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kz==25 ) THEN
!
!*****   325-GEMLOOP   *************************************************
!
               IF ( mf(1)/=1 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( mf(2)/=2 .AND. mf(2)/=0 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( mf(3)/=1 .AND. mf(3)/=0 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( m(1)<=0 .OR. m(3)<0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!
!     FOR NOW, CID MUST BE 0
!
               IF ( m(3)/=0 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               npts = 0
               DO l = 4 , 49 , 3
                  IF ( mf(l)==3 ) THEN
                     spag_nextblock_1 = 40
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  npts = npts + 1
                  IF ( mf(l)/=2 .AND. mf(l)/=0 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( mf(l+1)/=2 .AND. mf(l+1)/=0 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( mf(l+2)/=2 .AND. mf(l+2)/=0 ) THEN
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
               IF ( m(1)<0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( m(2)==-1 ) THEN
                  DO l = 3 , 8
                     IF ( mf(l)/=0 ) THEN
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
                  n = 2
                  spag_nextblock_1 = 5
               ELSEIF ( mf(3)==3 ) THEN
                  IF ( m(3)/=thru ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( mf(2)/=1 .OR. mf(4)/=1 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  l1 = 2
                  l2 = 5
                  ii = m(l1) - 1
                  l2 = m(l2) - m(l1)
                  IF ( ii<0 .OR. l2<=0 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  l1 = 1
                  i(1) = m(1)
                  n = 2
                  DO l = l1 , l2
                     i(2) = l + ii
                     CALL write(201,i,n,0)
                  ENDDO
                  i(2) = ii + l2 + 1
                  spag_nextblock_1 = 6
               ELSE
                  DO l = 2 , 8
                     IF ( mf(l)/=1 .AND. mf(l)/=0 ) THEN
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( m(l)<0 ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( m(l)/=0 ) THEN
                        n = n + 2
                        i(n-1) = m(1)
                        i(n) = m(l)
                     ENDIF
                  ENDDO
                  IF ( n<=0 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  spag_nextblock_1 = 6
               ENDIF
               CYCLE
            ELSEIF ( kz==28 ) THEN
!
!*****   328-MDIPOLE     ***********************************************
!
               IF ( m(1)<=0 .OR. m(2)<0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( z(9)<0. .OR. z(10)<0. ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               n = 10
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kz==33 ) THEN
               spag_nextblock_1 = 34
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kz==34 ) THEN
               spag_nextblock_1 = 35
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kz==35 ) THEN
               n = 6
               spag_nextblock_1 = 33
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kz==36 ) THEN
               n = 8
               spag_nextblock_1 = 33
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         CALL page2(2)
         WRITE (nout,99004) sfm
99004    FORMAT (A25,' 322, ILLEGAL ENTRY TO IFS4P.')
         abort = .TRUE.
         RETURN 1
      CASE (3)
         badfor = .TRUE.
         RETURN 1
      CASE (4)
         baddat = .TRUE.
         RETURN 1
      CASE (5)
         DO l = 1 , n
            i(l) = m(l)
         ENDDO
         spag_nextblock_1 = 6
      CASE (6)
         RETURN
      CASE (7)
         RETURN 3
      CASE (8)
         IF ( m(1)<=0 .OR. m(i1+2)<=0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO l = 2 , i1
            IF ( m(l)<=0 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( l/=2 ) THEN
               DO l1 = l , i1
                  IF ( m(l-1)==m(l1) ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
         n = i1 + 2
         spag_nextblock_1 = 5
      CASE (9)
         kout = kout + 1
         j(kout) = m(1)
         spag_nextblock_1 = 5
      CASE (10)
         IF ( m(1)<=0 .OR. m(2)<=0 .OR. z(3)<0.0 .OR. z(4)<=z(3) ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         n = 4
         spag_nextblock_1 = 5
      CASE (11)
!
!*****         199-PLOAD2,239-QBDY1,242-QVOL   *************************
!
         IF ( km/=0 ) THEN
            l = 1
         ELSE
            IF ( mf(1)/=1 .OR. mf(2)/=2 .AND. mf(2)/=0 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( m(1)<=0 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            l = 3
            isid = m(1)
            iqvl = m(2)
         ENDIF
         IF ( mf(8)==3 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ntot = 0
         k2078 = 209
         SPAG_Loop_1_1: DO WHILE ( m(l)/=0 )
            IF ( m(l)<0 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( mf(l)==3 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( mf(l+1)==3 ) THEN
               IF ( m(l+1)/=thru ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( mf(l+3)/=1 .AND. mf(l+3)/=0 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               l1 = m(l) - 1
               l2 = m(l+3) - l1
               IF ( l2<=1 .OR. l1<0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               DO ii = 1 , l2
                  n = n + 3
                  i(n-2) = isid
                  i(n-1) = iqvl
                  i(n) = ii + l1
                  IF ( n>=48 ) THEN
                     CALL write(k2078,i,n,0)
                     ntot = ntot + n
                     n = 0
                  ENDIF
               ENDDO
               l = l + 4
            ELSE
               IF ( mf(l)/=1 .AND. mf(l)/=0 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               n = n + 3
               i(n-2) = isid
               i(n-1) = iqvl
               i(n) = m(l)
               l = l + 1
               IF ( n>=48 ) THEN
                  CALL write(k2078,i,n,0)
                  ntot = ntot + n
                  n = 0
               ENDIF
            ENDIF
            IF ( l>8 ) EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
         t4(2,k) = t4(2,k) + ntot
         IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
            km = 1
         ELSE
            km = 0
         ENDIF
         spag_nextblock_1 = 6
      CASE (12)
         DO l = 3 , 6
            IF ( mf(l)/=0 .AND. mf(l)/=2 ) badfor = .TRUE.
         ENDDO
         spag_nextblock_1 = 13
      CASE (13)
         IF ( mf(1)/=1 .OR. mf(2)/=1 ) badfor = .TRUE.
         IF ( m(1)<=0 .OR. m(2)<=0 ) baddat = .TRUE.
         DO l = 1 , n
            i(l) = m(l)
            save(l) = m(l)
         ENDDO
         spag_nextblock_1 = 15
      CASE (14)
         DO l = 3 , 4
            IF ( mf(l)/=0 .AND. mf(l)/=2 ) badfor = .TRUE.
         ENDDO
         spag_nextblock_1 = 13
      CASE (15)
         IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
            kn = 1
            km = km + 1
         ELSE
            km = 0
            kn = 0
         ENDIF
         spag_nextblock_1 = 7
      CASE (16)
         IF ( mf(2)==3 .OR. mf(5)==3 ) THEN
            n = 0
            IF ( mf(7)/=0 .OR. mf(8)/=0 ) THEN
               IF ( mf(4)/=0 .OR. mf(5)/=-32767 ) THEN
                  badfor = .TRUE.
                  spag_nextblock_1 = 15
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
            l1 = -1
            SPAG_Loop_1_2: DO l = 1 , 4 , 3
               IF ( mf(l)/=0 .OR. mf(l+1)/=0 .OR. mf(l+2)/=0 ) THEN
                  IF ( mf(l)==1 .AND. mf(l+1)==3 .AND. mf(l+2)==1 ) THEN
                     l1 = l1 + 1
                     l2 = l1 + l
                     IF ( m(l2)>0 .AND. m(l2+1)==thru .AND. m(l2+3)>m(l2) ) THEN
                        l3 = m(l2)
                        l4 = m(l2+3)
                        DO l5 = l3 , l4
                           save(2) = l5
                           CALL write(209,save,nn,0)
                        ENDDO
                     ELSE
                        baddat = .TRUE.
                     ENDIF
                  ELSE
                     IF ( mf(l+1)==-32767 ) EXIT SPAG_Loop_1_2
                     badfor = .TRUE.
                  ENDIF
               ENDIF
            ENDDO SPAG_Loop_1_2
         ELSE
            n = 0
            SPAG_Loop_1_3: DO l = 1 , 8
               IF ( mf(l)/=0 ) THEN
                  IF ( mf(l)/=1 ) THEN
                     IF ( mf(l)==-32767 ) EXIT SPAG_Loop_1_3
                     badfor = .TRUE.
                  ELSEIF ( m(l)>0 ) THEN
                     save(2) = m(l)
                     CALL write(209,save,nn,0)
                  ELSE
                     baddat = .TRUE.
                  ENDIF
               ENDIF
            ENDDO SPAG_Loop_1_3
         ENDIF
         spag_nextblock_1 = 15
      CASE (17)
!
!*******       203-TEMPP3         **************************************
!
         IF ( km/=0 ) THEN
            IF ( km>2 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            n = 0
            l3 = 8*km
            DO l = 1 , 7 , 2
               IF ( mf(l)/=0 .OR. mf(l+1)/=0 ) THEN
                  IF ( mf(l)/=-32767 ) THEN
                     IF ( mf(l)/=0 .AND. mf(l)/=2 .OR. mf(l+1)/=0 .AND. mf(l+1)/=2 ) badfor = .TRUE.
                     IF ( zz>=z(l) ) baddat = .TRUE.
                  ELSE
                     mf(7) = 0
                     mf(8) = 0
                  ENDIF
               ENDIF
               zz = z(l)
               l5 = l3 + l
               i(l5) = m(l)
               save(l5) = m(l)
               i(l5+1) = m(l+1)
               save(l5+1) = m(l+1)
            ENDDO
         ELSE
            nn = 24
            n = 0
            id = m(1)
            l1 = 1
            IF ( mf(1)/=1 .OR. mf(2)/=1 ) badfor = .TRUE.
            DO l = 3 , 8
               IF ( mf(l)/=0 .AND. mf(l)/=2 ) THEN
                  IF ( mf(l)==-32767 ) THEN
                     DO l5 = l , 8
                        m(l5) = 0
                     ENDDO
                     mf(7) = 0
                     mf(8) = 0
                  ELSE
                     badfor = .TRUE.
                  ENDIF
               ENDIF
            ENDDO
            IF ( m(1)<=0 .OR. m(2)<=0 ) baddat = .TRUE.
            IF ( z(3)>=z(5) ) baddat = .TRUE.
            zz = z(5)
            IF ( mf(7)/=0 .OR. mf(8)/=0 ) THEN
               IF ( zz>=z(7) ) baddat = .TRUE.
            ENDIF
            zz = z(7)
            DO l = 1 , 8
               i(l) = m(l)
               save(l) = m(l)
            ENDDO
         ENDIF
         l1 = l1 + 8
         IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
            km = km + 1
            kn = 1
            IF ( km<3 ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
            km = 0
            kn = 0
         ENDIF
         IF ( l1<=nn ) THEN
            DO l = l1 , nn
               i(l) = 0
               save(l) = 0
            ENDDO
         ENDIF
         n = nn
         spag_nextblock_1 = 7
      CASE (18)
         l1 = l1 + 8
         IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
            km = km + 1
            kn = 1
            IF ( km<2 ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
            km = 0
            kn = 0
         ENDIF
         IF ( l1<=nn ) THEN
            DO l = l1 , nn
               i(l) = 0
               save(l) = 0
            ENDDO
         ENDIF
         n = nn
         spag_nextblock_1 = 7
      CASE (19)
         DO l = 5 , 8
            i(l+8) = 0
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
      CASE (20)
         l1 = l1 + 8
         IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
            km = km + 1
            kn = 1
            IF ( km<2 ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
            km = 0
            kn = 0
         ENDIF
         IF ( l1<=nn ) THEN
            DO l = l1 , nn
               i(l) = 0
               save(l) = 0
            ENDDO
         ENDIF
         n = nn
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 20      IF ( m(1)<=0 .OR. m(6)<0 .OR. m(8)<=0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ifpdco(m(7)) ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         n = 5
         i(1) = m(1)
         i(2) = m(4)
         i(3) = m(6)
         i(4) = m(7)
         i(5) = m(8)
         spag_nextblock_1 = 6
      CASE (21)
!
!*******       206-FSLIST         **************************************
!
         IF ( km/=0 ) THEN
            l1 = 1
            l2 = 0
            spag_nextblock_1 = 24
         ELSE
            ASSIGN 40 TO ret
            spag_nextblock_1 = 32
         ENDIF
         CYCLE
 40      IF ( mf(1)==0 .OR. mf(1)==2 ) THEN
            IF ( .NOT.(mf(1)==0 .OR. (mf(1)==2 .AND. z(1)>0.0)) ) THEN
               spag_nextblock_1 = 23
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( mf(1)==0 ) m(1) = 1
            i(1) = m(1)
            n = 1
            l1 = 2
            l2 = 0
            IF ( mf(2)/=3 ) THEN
               spag_nextblock_1 = 24
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( m(2)/=bcdaxi ) THEN
               spag_nextblock_1 = 23
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            n = n + 1
            i(n) = 0
            l1 = l1 + 1
            l2 = 1
            spag_nextblock_1 = 24
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 22
      CASE (22)
         badfor = .TRUE.
         spag_nextblock_1 = 28
      CASE (23)
         baddat = .TRUE.
         spag_nextblock_1 = 28
      CASE (24)
         DO l = l1 , 8
            l3 = l + l2
            IF ( mf(l)==3 ) THEN
               spag_nextblock_1 = 26
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( mf(l)==0 ) THEN
               spag_nextblock_1 = 27
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( mf(l)/=1 ) THEN
               spag_nextblock_1 = 22
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( m(l3)<=0 ) THEN
               spag_nextblock_1 = 23
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            n = n + 1
            i(n) = m(l3)
         ENDDO
         spag_nextblock_1 = 25
      CASE (25)
         IF ( n<=0 ) THEN
            spag_nextblock_1 = 23
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 28
      CASE (26)
         IF ( m(l3)/=bcdaxi ) THEN
            spag_nextblock_1 = 23
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         n = n + 1
         i(n) = 0
         spag_nextblock_1 = 27
      CASE (27)
         IF ( l/=8 ) THEN
            l = l + 1
            DO l2 = l , 8
               IF ( mf(l2)/=0 ) THEN
                  spag_nextblock_1 = 22
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
         IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
            spag_nextblock_1 = 23
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 25
      CASE (28)
         IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
            kn = 1
            km = km + 1
         ELSE
            km = 0
            kn = 0
            n = n + 1
            i(n) = -1
         ENDIF
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 60      DO l = 1 , 5 , 4
            IF ( m(l)/=0 .OR. m(l+1)/=0 .OR. m(l+2)/=0 .OR. m(l+3)/=0 ) THEN
               IF ( m(l)<=0 .OR. z(l+1)<=0.0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               n = n + 4
               IF ( n>4 .AND. m(l)==m(l-4) ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( m(l)<=99999 ) THEN
                  i(n-3) = m(l)
                  i(n-2) = m(l+1)
                  i(n-1) = m(l+2)
                  i(n) = m(l+3)
               ELSE
                  CALL page2(2)
                  WRITE (nout,99008) ufm
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDDO
         IF ( n<=0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 80      IF ( m(1)<=0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO l = 3 , 7 , 2
            IF ( m(l)/=0 .OR. m(l+1)/=0 ) THEN
               IF ( m(l)<=0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               n = n + 3
               i(n-2) = m(1)
               i(n-1) = m(l)
               i(n) = m(l+1)
            ENDIF
         ENDDO
         IF ( n<=0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 100     IF ( m(1)<=0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( m(1)<=99999 ) THEN
            DO l = 2 , kfl
               IF ( m(l)<=0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( l/=kfl ) THEN
                  l2 = l + 1
                  DO l1 = l2 , kfl
                     IF ( m(l)==m(l1) ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
            i(1) = m(1)
            n = kfl + 3
            IF ( mf(6)==0 ) m(6) = 1
            IF ( mf(7)==0 ) m(7) = 1
            i(kfl+2) = m(6)
            i(kfl+3) = m(7)
            DO l = 1 , kfl
               i(l+1) = m(l+1)
            ENDDO
            spag_nextblock_1 = 6
         ELSE
            CALL page2(2)
            WRITE (nout,99008) ufm
            spag_nextblock_1 = 4
         ENDIF
      CASE (29)
         badfor = .TRUE.
         spag_nextblock_1 = 31
      CASE (30)
         IF ( m(1)<=naxf ) baddat = .TRUE.
         IF ( m(1)>0 ) THEN
            l2 = m(1)
         ELSE
            CALL write(215,0,1,0)
         ENDIF
         l3 = m(4)
         DO l = l2 , l3 , l1
            CALL write(215,l,1,0)
         ENDDO
         naxf = l3
         spag_nextblock_1 = 31
      CASE (31)
         IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
            kn = 1
            km = km + 1
         ELSE
            km = 0
            kn = 0
            IF ( naxf>=100 ) THEN
               CALL page2(2)
               WRITE (nout,99005) ufm , naxf
99005          FORMAT (A23,' 4125, MAXIMUM ALLOWABLE HARMONIC ID IS 99.  DATA ','CONTAINS MAXIMUM =',I20)
               abort = .TRUE.
            ENDIF
            n = n + 1
            i(n) = -1
         ENDIF
         spag_nextblock_1 = 7
      CASE (32)
         IF ( iaxf<=0 ) THEN
            IF ( lharm ) CALL page2(2)
            IF ( lharm ) WRITE (nout,99006) ufm
99006       FORMAT (A23,' 4122, AXIF CARD REQUIRED.')
            lharm = .FALSE.
            abort = .TRUE.
         ENDIF
!
!*******       213-BDYLIST        **************************************
!
!
!*******       214-FREEPT         **************************************
!
         GOTO ret
      CASE (33)
         DO l = 1 , n
            IF ( m(l)<=0 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         n1 = n - 1
         DO l = 3 , n1
            l2 = l + 1
            DO l1 = l2 , n
               IF ( m(l)==m(l1) ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDDO
         spag_nextblock_1 = 5
      CASE (34)
!
!*******       219-CHEXA1,  333-CFHEX1   *******************************
!
         IF ( mf(15)/=0 .OR. mf(16)/=0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         n = 10
         spag_nextblock_1 = 33
      CASE (35)
!
!*******       220-CHEXA2,  334-CFHEX2   *******************************
!
         IF ( mf(15)/=0 .OR. mf(16)/=0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         n = 10
         spag_nextblock_1 = 33
         CYCLE SPAG_DispatchLoop_1
 120     IF ( mf(1)/=3 .OR. m(1)==nm(1) .AND. m(2)==nm(2) ) THEN
            spag_nextblock_1 = 36
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ifo = m(4)
         ty1 = m(5)
         ity1 = 2*mod(ty1,2)
         ty2 = m(6)
         IF ( mach==12 ) THEN
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
         nm(1) = m(1)
         nm(2) = m(2)
         IF ( mf(6)/=0 .OR. mf(7)/=0 .OR. mf(8)/=0 ) THEN
            spag_nextblock_1 = 37
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( m1f(2)/=3 .OR. m1(3)/=nm(1) .OR. m1(4)/=nm(2) ) THEN
            spag_nextblock_1 = 37
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         n = 9
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 140     i(3) = m(4)
         IF ( ty1/=1 ) THEN
            n = 4
            i(4) = m(5)
            IF ( ty1/=2 .AND. ty1/=3 ) THEN
               n = 6
               i(5) = m(6)
               i(6) = m(7)
            ENDIF
         ENDIF
         IF ( m1(1)==0 .AND. m1(2)==0 ) GOTO 160
         n = n + 2
         i(n-1) = -1
         i(n) = -1
         IF ( m1(1)/=t1(1,k) .OR. m1(2)/=t1(2,k) .OR. m1(3)/=nm(1) .OR. m1(4)/=nm(2) ) THEN
            n = n + 2
            i(n-1) = -1
            i(n) = -1
         ENDIF
         spag_nextblock_1 = 39
      CASE (36)
         nm(1) = m(1)
         nm(2) = m(2)
         spag_nextblock_1 = 37
      CASE (37)
         abort = .TRUE.
         CALL page2(2)
         WRITE (nout,99007) ufm , nm(1) , nm(2)
99007    FORMAT (A23,' 4126, BAD DATA OR FORMAT OR NON-UNIQUE NAME, DMIAX',1X,2A4)
         IF ( m1(1)==0 .AND. m1(2)==0 ) GOTO 160
         spag_nextblock_1 = 39
      CASE (38)
         IF ( mf(l1)/=1 ) THEN
            i(1) = m(l2-2)
         ELSEIF ( m(l2)<0 ) THEN
            i(1) = 500000*(1-m(l2)*2) + m(l2-2)
         ELSE
            i(1) = 1000000*(1+m(l2)) + m(l2-2)
         ENDIF
         GOTO ret
      CASE (39)
         km = 0
         kn = 0
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 160     kn = 1
         km = km + 1
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 180     IF ( mf(1)/=1 .OR. mf(2)/=3 .OR. mf(3)/=3 ) badfor = .TRUE.
         DO l = 4 , 8
            IF ( mf(l)/=0 ) badfor = .TRUE.
         ENDDO
         IF ( m(1)<2 .OR. m(2)/=bcds .AND. m(2)/=bcda .OR. m(4)/=bcds .AND. m(4)/=bcda ) baddat = .TRUE.
         IF ( mod(m(1),2)/=0 ) baddat = .TRUE.
         IF ( m(2)==bcds ) m(2) = +1
         IF ( m(2)==bcda ) m(2) = -1
         IF ( m(4)==bcds ) m(3) = +1
         IF ( m(4)==bcda ) m(3) = -1
         n = 3
         spag_nextblock_1 = 5
      CASE (40)
         IF ( npts<2 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO lll = l , 49
            m(lll) = 0
         ENDDO
         DO l = 1 , 3
            i(l) = m(l)
         ENDDO
         i(4) = npts
         DO l = 4 , 48
            i(l+1) = m(l)
         ENDDO
         n = 49
         spag_nextblock_1 = 6
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99008 FORMAT (A23,' 5004, FLUID POINT ID ON CFLUID OR RINGFL CARD ','EXCEEDS 999999 LIMIT')
!
END SUBROUTINE ifs4p
