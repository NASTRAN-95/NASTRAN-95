!*==nrlsum.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE nrlsum
   IMPLICIT NONE
   USE C_BLANK
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a , b , cosphi , p , phi , q , r , rm , rmax , rx , rxy , ry , ryz , rz , rzx , s , sa , sb , sc , sd , sn , so , sq ,   &
         & ss , st , sum , sx , sxy , sy , syz , sz , szx , t , thresh , v , x
   INTEGER :: buf1 , buf2 , buf3 , buf4 , elid , eltype , file , i , i1 , i2 , i3 , i5 , iend , ifil , ij , ipts , iret , iscr ,    &
            & iskip , isub , iwords , j , j2 , j3 , j4 , j5 , j6 , j7 , j8 , j9 , lcore , n , ndir , ns , nwds , ofil , oldtyp
   REAL , DIMENSION(3,3) :: dcos , smat
   REAL , SAVE :: dtor
   INTEGER , SAVE :: i0 , nrlfor , nrlstr , oef2 , oes2 , scr1 , scr2 , scr3
   INTEGER , DIMENSION(2) , SAVE :: idir , nam
   INTEGER , DIMENSION(3) , SAVE :: inum , scr
   INTEGER , DIMENSION(146) :: istres
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(3) :: nsub
   REAL , DIMENSION(6) :: sig
   REAL , DIMENSION(3) :: sigp
   REAL , DIMENSION(146) :: stress
   REAL , DIMENSION(20) :: z
   EXTERNAL close , fread , fwdrec , gopen , korsz , mesage , open , read , sadotb , saxb , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
   INTEGER :: spag_nextblock_4
!
!     NRLSUM   OES2,OEF2/NRLSTR,NRLFOR/V,N,NMODES/V,N,NSHOCK(NDIR)/
!              C,Y,DIRECT=123/C,Y,SQRSS=0 $
!
!     NRLSUM COMPUTES NRL SUM STRESSES AND FORCES FOR DDAM. IT IS
!     ASSUMED THAT THE USER HAS REQUESTED STRESSES AND FORCES IN SORT2
!     FORMAT (BUT RESULTS WILL BE SORT1). NRLSUM READS ITEMS FOR AN
!     ELEMENT (FOR ALL SUBCASES) AND COMPUTES THE NRL SUM.  UP TO 3
!     SCRATCH FILES ARE USED TO STORE THE SUMS FOR EACH SHOCK DIRECTION.
!     PRINCIPAL STRESSES WILL BE COMPUTED BASED ON THE SUMS. THE NUMBER
!     OF SUBCASES IS NMODES*NSHOCK WITH THE ORDER 1-NMODES,
!     NMODES+1 - 2*NMODES, ... NSHOCK*NMODES.
!
!     (IF (SQRSS.EQ.1), SQUARE ROOT OF THE SUM OF THE SQUARES IS USED
!     INSTEAD OF NRL SUM
!
   !>>>>EQUIVALENCE (Z(1),Iz(1)) , (stress(1),istres(1))
   !>>>>EQUIVALENCE (sigp(1),sa) , (sigp(2),sb) , (sigp(3),sc)
   !>>>>EQUIVALENCE (sig(1),sx) , (sig(2),sy) , (sig(3),sz) , (sig(4),sxy) , (sig(5),syz) , (sig(6),szx)
   DATA oes2 , nrlstr , scr1 , scr2 , scr3/101 , 201 , 301 , 302 , 303/
   DATA oef2 , nrlfor/102 , 202/
   DATA scr/301 , 302 , 303/
   DATA idir/4HDIRE , 4HCTIO/
   DATA inum/4HN 1  , 4HN 2  , 4HN 3 /
   DATA dtor/0.0174532925E0/
   DATA nam/4HNRLS , 4HUM  / , i0/0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         lcore = korsz(z)
         buf1 = lcore - Sysbuf + 1
         buf2 = buf1 - Sysbuf
         buf3 = buf2 - Sysbuf
         buf4 = buf3 - Sysbuf
         IF ( Nshock/=3 ) THEN
            IF ( Nshock==2 ) THEN
!
               buf4 = buf3
            ELSE
               buf4 = buf2
               buf3 = buf2
            ENDIF
         ENDIF
!
         lcore = buf4 - 1
         IF ( lcore<=0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ndir = Nshock
         IF ( ndir<=1 ) THEN
            nsub(1) = Direct
         ELSEIF ( ndir>2 ) THEN
            nsub(1) = 1
            nsub(2) = 2
            nsub(3) = 3
         ELSEIF ( Direct==23 ) THEN
            nsub(1) = 2
            nsub(2) = 3
         ELSE
            nsub(1) = 1
            nsub(2) = 2
            IF ( Direct==13 ) nsub(2) = 3
         ENDIF
         ifil = oes2
         ofil = nrlstr
         spag_nextblock_1 = 2
      CASE (2)
!
         file = ifil
         oldtyp = 0
         CALL open(*60,ifil,z(buf1),0)
         CALL fwdrec(*80,ifil)
         CALL gopen(scr1,z(buf2),1)
         IF ( Nshock>1 ) CALL gopen(scr2,z(buf3),1)
         IF ( Nshock==3 ) CALL gopen(scr3,z(buf4),1)
         spag_nextblock_1 = 3
      CASE (3)
!
         CALL read(*40,*100,ifil,stress,146,1,iwords)
         eltype = istres(3)
         nwds = istres(10)
         i5 = istres(5)
         elid = istres(5)/10
!
!     REFORMULATE TO SORT 1 FORMAT
!
         istres(2) = 5
         IF ( ifil==oef2 ) istres(2) = 4
         istres(141) = idir(1)
         istres(142) = idir(2)
!
!     WRITE ONTO SCRATCH ONLY FOR NEW ELEMENT TYPE
!
         IF ( eltype/=oldtyp ) THEN
            DO i = 1 , Nshock
               istres(4) = i
               istres(5) = i
               istres(8) = i
               isub = nsub(i)
               istres(143) = inum(isub)
               IF ( oldtyp/=0 ) CALL write(scr(i),0,0,1)
               CALL write(scr(i),stress,146,1)
            ENDDO
!
            oldtyp = eltype
!
!     READ STRESS INFO FOR NUMBER OF MODES AND SHOCK DIRECTIONS
!
            IF ( Nmodes*nwds>lcore ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         DO ns = 1 , Nshock
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  iscr = 300 + ns
!
                  CALL fread(ifil,z(1),nwds*Nmodes,0)
!
!     GO TO PROPER SECTION FOR EACH ELEMENT TYPE
!
!     FOR FORCES, COMPUTATIONS ARE EASIER. SO LETS NOT HAVE A COMPUTED
!     GO TO
!
                  IF ( ifil/=oes2 ) THEN
!
                     IF ( eltype>=20 .AND. eltype<=33 ) CYCLE
                     IF ( eltype>=39 .AND. eltype<=52 ) CYCLE
                     IF ( eltype==62 .OR. eltype==68 .OR. eltype==69 .OR. eltype==72 ) CYCLE
                     IF ( eltype>=65 .AND. eltype<=67 ) CYCLE
                     IF ( eltype==9 .OR. eltype==16 .OR. eltype==73 .OR. eltype==76 ) CYCLE
                     i3 = 1
                     i2 = nwds
                     i1 = 2
                     IF ( eltype==35 .OR. eltype==70 .OR. eltype==71 ) i1 = 3
                     ASSIGN 28 TO iret
!
!
                  ELSEIF ( eltype==2 ) THEN
!
!     BEAM
!
                     i1 = 2
                     i2 = 5
                     i3 = 1
                     ASSIGN 4 TO iret
                  ELSEIF ( eltype==4 .OR. eltype==5 ) THEN
!
!     SHEAR
!
                     i1 = 2
                     i2 = 3
                     i3 = 1
                     ASSIGN 8 TO iret
                  ELSEIF ( eltype==6 .OR. eltype==7 .OR. eltype==8 .OR. eltype==15 .OR. eltype==17 .OR. eltype==18 .OR.             &
                         & eltype==19 .OR. eltype==64 .OR. eltype==83 ) THEN
!
!     TRBSC, TRPLT, QDPLT, TRIA1, TRIA2, TRIA3, QUAD1, QUAD2, QUAD4
!
                     i1 = 3
                     i2 = 5
                     i3 = 1
                     j3 = 3
                     j4 = 4
                     j5 = 5
                     j6 = 6
                     j7 = 7
                     j8 = 8
                     j9 = 9
                     ASSIGN 10 TO iret
                  ELSEIF ( eltype==9 .OR. eltype==16 .OR. eltype==62 .OR. eltype==63 ) THEN
!
!     TRMEM, QDMEM,  QDMEM1, QDMEM2
!
                     i1 = 2
                     i2 = 4
                     i3 = 1
                     j3 = 2
                     j4 = 3
                     j5 = 4
                     j6 = 5
                     j7 = 6
                     j8 = 7
                     j9 = 8
                     ASSIGN 10 TO iret
                  ELSEIF ( eltype==11 .OR. eltype==12 .OR. eltype==13 ) THEN
!
!     CELAS1,2,3
!
                     i1 = 2
                     i2 = 2
                     i3 = 1
                     ASSIGN 28 TO iret
                  ELSEIF ( eltype==14 .OR. eltype==20 .OR. eltype==21 .OR. eltype==22 .OR. eltype==23 .OR. eltype==24 .OR.          &
                         & eltype==25 .OR. eltype==26 .OR. eltype==27 .OR. eltype==28 .OR. eltype==29 .OR. eltype==30 .OR.          &
                         & eltype==31 .OR. eltype==32 .OR. eltype==33 .OR. eltype==43 .OR. eltype==44 .OR. eltype==45 .OR.          &
                         & eltype==46 .OR. eltype==47 .OR. eltype==48 .OR. eltype==49 .OR. eltype==50 .OR. eltype==51 .OR.          &
                         & eltype==52 .OR. eltype==68 .OR. eltype==69 .OR. eltype==72 .OR. eltype==77 .OR. eltype==78 .OR.          &
                         & eltype==79 .OR. eltype==80 .OR. eltype==81 .OR. eltype==82 ) THEN
                     CYCLE
                  ELSEIF ( eltype==34 ) THEN
!
!     BAR - ADD AXIAL STRESS TO EXTENSIONAL STRESSES DUE TO BENDING
!           BEFORE COMPUTING NRL SUMS. THEN ZERO OUT AXIAL STRESS
!           AND MAX AND MIN STRESSES
!
                     i1 = 2
                     i2 = 5
                     i3 = 1
                     DO j = 1 , Nmodes
                        isub = nwds*(j-1)
                        DO i = 2 , 5
                           z(isub+i) = z(isub+i) + z(isub+6)
                        ENDDO
                        DO i = 10 , 13
                           z(isub+i) = z(isub+i) + z(isub+6)
                        ENDDO
                     ENDDO
                     ASSIGN 12 TO iret
                  ELSEIF ( eltype==35 ) THEN
!
!     CONEAX
!
                     i1 = 4
                     i2 = 6
                     i3 = 1
                     j3 = 4
                     j4 = 5
                     j5 = 6
                     j6 = 7
                     j7 = 8
                     j8 = 9
                     j9 = 10
                     ASSIGN 10 TO iret
                  ELSEIF ( eltype==36 ) THEN
!
!     TRIARG
!
                     i1 = 2
                     i2 = 5
                     i3 = 1
                     ASSIGN 28 TO iret
                  ELSEIF ( eltype==37 ) THEN
!
!     TRAPRG
!
                     i1 = 2
                     i2 = 21
                     i3 = 1
                     ASSIGN 28 TO iret
                  ELSEIF ( eltype==38 ) THEN
!
!     TORDRG
!
                     i1 = 2
                     i2 = 16
                     i3 = 1
                     ASSIGN 28 TO iret
                  ELSEIF ( eltype==39 .OR. eltype==40 .OR. eltype==41 .OR. eltype==42 ) THEN
!
!     TETRA, WEDGE, HEXA1, HEXA2
!
                     i1 = 2
                     i2 = 7
                     i3 = 1
                     ASSIGN 16 TO iret
                  ELSEIF ( eltype==53 .OR. eltype==54 .OR. eltype==55 .OR. eltype==56 .OR. eltype==57 .OR. eltype==58 .OR.          &
                         & eltype==59 .OR. eltype==60 .OR. eltype==61 ) THEN
!
!     DUM1 - DUM9
!
                     i1 = 2
                     i2 = 10
                     i3 = 1
                     ASSIGN 28 TO iret
                  ELSEIF ( eltype==65 .OR. eltype==66 .OR. eltype==67 ) THEN
!
!     IHEX1,2,3
!
                     i1 = 3
                     i2 = 4
                     i3 = 1
                     ASSIGN 22 TO iret
                  ELSEIF ( eltype==70 ) THEN
!
!     TRIAAX
!
                     i1 = 3
                     i2 = 11
                     i3 = 1
                     ASSIGN 28 TO iret
                  ELSEIF ( eltype==71 ) THEN
!
!     TRAPAX
!
                     i1 = 3
                     i2 = 47
                     i3 = 1
                     ASSIGN 28 TO iret
                  ELSEIF ( eltype==73 .OR. eltype==74 .OR. eltype==75 ) THEN
!
!     TRIM6, TRPLT1, TRSHL
!
                     iend = 8
                     iskip = 8
                     IF ( eltype==73 ) THEN
                        iend = 4
                        iskip = 7
                     ENDIF
                     j2 = -5
                     ij = 0
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
                  ELSEIF ( eltype==76 ) THEN
!
!     IS2D8
!
                     ij = 0
                     j2 = 1
                     spag_nextblock_2 = 3
                     CYCLE SPAG_DispatchLoop_2
                  ELSE
!
!     ROD, TUBE, CONROD
!
                     i1 = 2
                     i2 = 4
                     i3 = 2
                     ASSIGN 2 TO iret
                  ENDIF
                  spag_nextblock_2 = 6
                  CYCLE SPAG_DispatchLoop_2
!
!     IGNORE MARGINS OF SAFETY
!
 2                Iz(i0+3) = 1
                  Iz(i0+5) = 1
                  GOTO 28
 4                z(6) = z(5) + amax1(z(2),z(3),z(4))
                  z(7) = z(5) + amin1(z(2),z(3),z(4))
                  Iz(i0+8) = 1
                  i1 = 9
                  i2 = 11
                  i3 = 1
                  ASSIGN 6 TO iret
                  spag_nextblock_2 = 6
                  CYCLE SPAG_DispatchLoop_2
 6                z(12) = z(5) + amax1(z(9),z(10),z(11))
                  z(13) = z(5) + amin1(z(9),z(10),z(11))
                  Iz(i0+14) = 1
                  GOTO 28
 8                Iz(i0+4) = 1
                  GOTO 28
 10               ss = .5*(z(j3)+z(j4))
                  st = z(j3) - z(j4)
                  sq = sqrt(.25*st**2+z(j5)**2)
                  z(j7) = ss + sq
                  z(j8) = ss - sq
                  z(j9) = sq
                  sd = 2.*z(j5)
                  IF ( abs(sd)<1.E-15 .AND. abs(st)<1.E-15 ) THEN
                     z(j6) = 0.
                  ELSE
                     z(j6) = atan2(sd,st)*28.6478898
                  ENDIF
                  IF ( j3==11 ) GOTO 28
                  IF ( eltype==9 .OR. eltype==16 ) GOTO 28
!                 TRMEM             QDMEM
                  IF ( eltype==62 .OR. eltype==63 ) GOTO 28
!                QDMEM1            QDMEM2
                  IF ( eltype==35 ) THEN
                     IF ( j3==12 ) GOTO 28
                     i1 = 12
                     i2 = 14
                     i3 = 1
                     j3 = 12
                     j4 = 13
                     j5 = 14
                     j6 = 15
                     j7 = 16
                     j8 = 17
                     j9 = 18
                     spag_nextblock_2 = 6
                     CYCLE SPAG_DispatchLoop_2
                  ELSE
!                  CONEAX
                     i1 = 11
                     i2 = 13
                     i3 = 1
                     j3 = 11
                     j4 = 12
                     j5 = 13
                     j6 = 14
                     j7 = 15
                     j8 = 16
                     j9 = 17
                     spag_nextblock_2 = 6
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
 12               z(6) = 0.
                  z(7) = 0.
                  z(8) = 0.
                  Iz(i0+9) = 1
                  i1 = 10
                  i2 = 13
                  i3 = 1
                  ASSIGN 14 TO iret
                  spag_nextblock_2 = 6
                  CYCLE SPAG_DispatchLoop_2
 14               z(14) = 0.
                  z(15) = 0.
                  Iz(i0+16) = 1
                  GOTO 28
 16               z(8) = sqrt((z(2)-z(3))**2+(z(3)-z(4))**2+(z(4)-z(2))**2+6.*(z(5)**2+z(6)**2+z(7)**2))/3.
                  z(9) = -(z(2)+z(3)+z(4))/3.
                  GOTO 28
               CASE (2)
                  ij = ij + 1
                  j2 = j2 + iskip
                  j4 = j2 + 2
                  i1 = j2
                  i2 = j4
                  i3 = 1
                  ASSIGN 18 TO iret
                  spag_nextblock_2 = 6
                  CYCLE SPAG_DispatchLoop_2
 18               ss = .5*(z(j2)+z(j2+1))
                  st = z(j2) - z(j2+1)
                  sq = sqrt(.25*st**2+z(j4)**2)
                  z(j4+2) = ss + sq
                  z(j4+3) = ss - sq
                  z(j4+4) = sq
                  sd = 2.*z(j4)
                  IF ( abs(sd)<1.E-15 .AND. abs(st)<1.E-15 ) THEN
                     z(j4+1) = 0.
                  ELSE
                     z(j4+1) = atan2(sd,st)*28.6478898
                  ENDIF
                  IF ( ij<iend ) THEN
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  GOTO 28
               CASE (3)
                  ij = ij + 1
                  j2 = j2 + 5
                  j4 = j2 + 2
                  i1 = j2
                  i2 = j4
                  i3 = 1
                  ASSIGN 20 TO iret
                  spag_nextblock_2 = 6
                  CYCLE SPAG_DispatchLoop_2
 20               IF ( ij<8 ) THEN
                     spag_nextblock_2 = 3
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  GOTO 28
 22               i1 = 11
                  IF ( eltype==67 ) i1 = 12
!                   IHEX3
                  i2 = i1 + 1
                  ASSIGN 24 TO iret
                  spag_nextblock_2 = 6
                  CYCLE SPAG_DispatchLoop_2
 24               i1 = i1 + 6
                  i2 = i1 + 1
                  ASSIGN 26 TO iret
                  spag_nextblock_2 = 6
                  CYCLE SPAG_DispatchLoop_2
!
!     COMPUTE PRINCIPAL STRESSES
!
 26               sig(1) = z(3)
                  sig(2) = z(11)
                  sig(3) = z(17)
                  sig(4) = z(4)
                  sig(5) = z(12)
                  sig(6) = z(18)
                  IF ( eltype==67 ) THEN
!                   IHEX3
                     sig(2) = z(12)
                     sig(3) = z(18)
                     sig(5) = z(13)
                     sig(6) = z(19)
                  ENDIF
!*****
!     SOLVE CUBIC EQUATION FOR PRINCIPAL STRESSES
!*****
!
!     S**3 + P*S**2 + Q*S + R = 0.0
!
!     REF. -- CRC STANDARD MATH TABLES 14TH ED., PP. 392,3
!
                  rm = 0.0
                  DO i = 1 , 6
                     IF ( abs(sig(i))>rm ) rm = abs(sig(i))
                  ENDDO
                  IF ( rm>0.0 ) THEN
                     thresh = 1.0E-5
                     SPAG_Loop_2_1: DO
                        DO i = 1 , 6
                           IF ( abs(sig(i)/rm)<thresh ) sig(i) = 0.0
                        ENDDO
                        rx = sx/rm
                        ry = sy/rm
                        rz = sz/rm
                        rxy = sxy/rm
                        ryz = syz/rm
                        rzx = szx/rm
                        p = -rx - ry - rz
                        q = rx*ry + ry*rz + rz*rx - rxy**2 - ryz**2 - rzx**2
                        r = -(rx*ry*rz+2.0*rxy*ryz*rzx-rx*ryz**2-ry*rzx**2-rz*rxy**2)
                        a = (3.0*q-p**2)/3.0
                        b = (2.0*p**3-9.0*p*q+27.0*r)/27.0
                        x = -a**3/27.0
                        IF ( x>0.0 ) THEN
                           cosphi = -(b/2.0)/sqrt(x)
                           IF ( abs(cosphi)<=1.0 ) THEN
                              phi = acos(cosphi)
                              x = 2.0*sqrt(-a/3.0)
                              spag_nextblock_2 = 4
                              CYCLE SPAG_DispatchLoop_2
                           ENDIF
!
!     CHECK FOR IMAGINARY ROOTS
!
                        ELSEIF ( abs(x)<=rm*1.0E-6 ) THEN
!
!     CHECK FOR 3 EQUAL ROOTS
!
                           IF ( abs(b)<=1.0E-6 ) THEN
                              x = 0.0
                              phi = 0.0
                              spag_nextblock_2 = 4
                              CYCLE SPAG_DispatchLoop_2
                           ENDIF
                        ENDIF
                        thresh = 10.0*thresh
                        IF ( thresh>=1.1E-3 ) EXIT SPAG_Loop_2_1
                     ENDDO SPAG_Loop_2_1
                  ENDIF
                  sa = 0.0
                  sb = 0.0
                  sc = 0.0
                  spag_nextblock_2 = 5
                  CYCLE SPAG_DispatchLoop_2
               CASE (4)
                  sa = (x*cos(phi/3.0)-p/3.0)*rm
                  sb = (x*cos(phi/3.0+120.0*dtor)-p/3.0)*rm
                  sc = (x*cos(phi/3.0+240.0*dtor)-p/3.0)*rm
                  rm = 0.0
                  DO i = 1 , 3
                     IF ( abs(sigp(i))>rm ) rm = abs(sigp(i))
                  ENDDO
                  DO i = 1 , 3
                     IF ( abs(sigp(i)/rm)<1.0E-5 ) sigp(i) = 0.0
                  ENDDO
                  spag_nextblock_2 = 5
               CASE (5)
!*****
!     COMPUTE MEAN STRESS OR PRESSURE
!*****
                  sn = -(sa+sb+sc)/3.0
!*****
!     COMPUTE OCTAHEDRAL SHEAR STRESS
!*****
                  so = sqrt(((sa+sn)**2+(sb+sn)**2+(sc+sn)**2)/3.0)
!*****
!     COMPUTE DIRECTION COSINES OF THE PRINCIPAL PLANES
!*****
                  rm = 1.0E-6
                  DO i = 1 , 3
                     spag_nextblock_3 = 1
                     SPAG_DispatchLoop_3: DO
                        SELECT CASE (spag_nextblock_3)
                        CASE (1)
                           IF ( sigp(i)/=0.0 ) THEN
                              smat(1,1) = 1.0 - sx/sigp(i)
                              smat(2,1) = -sxy/sigp(i)
                              smat(3,1) = -szx/sigp(i)
                              smat(1,2) = smat(2,1)
                              smat(2,2) = 1.0 - sy/sigp(i)
                              smat(3,2) = -syz/sigp(i)
                              smat(1,3) = smat(3,1)
                              smat(2,3) = smat(3,2)
                              smat(3,3) = 1.0 - sz/sigp(i)
                              CALL saxb(smat(1,1),smat(1,2),dcos(1,i))
                              rx = sadotb(dcos(1,i),dcos(1,i))
                              j = 1
                              CALL saxb(smat(1,2),smat(1,3),dcos(1,i))
                              ry = sadotb(dcos(1,i),dcos(1,i))
                              IF ( ry>rx ) j = 2
                              CALL saxb(smat(1,3),smat(1,1),dcos(1,i))
                              rz = sadotb(dcos(1,i),dcos(1,i))
                              IF ( rz>ry .AND. rz>rx ) j = 3
                              p = smat(1,j)
                              q = smat(2,j)
                              r = smat(3,j)
                              IF ( j<2 ) THEN
                                 j = 2
                              ELSEIF ( j==2 ) THEN
                                 j = 3
                              ELSE
                                 j = 1
                              ENDIF
                              s = smat(1,j)
                              t = smat(2,j)
                              v = smat(3,j)
                              IF ( abs(q)>rm ) THEN
                                 rx = v - t*r/q
                                 IF ( abs(rx)<=rm ) THEN
                                    rx = s - t*p/q
                                    IF ( abs(rx)<=rm ) THEN
                                       spag_nextblock_3 = 2
                                       CYCLE SPAG_DispatchLoop_3
                                    ENDIF
                                    ry = -r/q
                                    x = 1.0 + ry*ry
                                    dcos(1,i) = 0.0
                                    dcos(3,i) = 1.0/sqrt(x)
                                    dcos(2,i) = ry*dcos(3,i)
                                    CYCLE
                                 ELSE
                                    rz = -(s-t*p/q)/rx
                                    ry = -(p+r*rz)/q
                                 ENDIF
                              ELSEIF ( abs(r)<=rm ) THEN
                                 IF ( abs(p)<=rm ) THEN
                                    spag_nextblock_3 = 2
                                    CYCLE SPAG_DispatchLoop_3
                                 ENDIF
                                 IF ( abs(v)<=rm ) THEN
                                    IF ( abs(t)<=rm ) THEN
                                       spag_nextblock_3 = 2
                                       CYCLE SPAG_DispatchLoop_3
                                    ENDIF
                                    dcos(1,i) = 0.0
                                    dcos(2,i) = 0.0
                                    dcos(3,i) = 1.0
                                    CYCLE
                                 ELSE
                                    rz = -t/v
                                    x = 1.0 + rz*rz
                                    dcos(1,i) = 0.0
                                    dcos(2,i) = 1.0/sqrt(x)
                                    dcos(3,i) = rz*dcos(2,i)
                                    CYCLE
                                 ENDIF
                              ELSE
                                 rz = -p/r
                                 IF ( abs(t)<=rm ) THEN
                                    IF ( abs(s-v*p/r)<=rm ) THEN
                                       spag_nextblock_3 = 2
                                       CYCLE SPAG_DispatchLoop_3
                                    ENDIF
                                    dcos(1,i) = 0.0
                                    dcos(2,i) = 1.0
                                    dcos(3,i) = 0.0
                                    CYCLE
                                 ELSE
                                    ry = -(s-v*p/r)/t
                                 ENDIF
                              ENDIF
                              x = 1.0 + rz*rz + ry*ry
                              dcos(1,i) = 1.0/sqrt(x)
                              dcos(2,i) = ry*dcos(1,i)
                              dcos(3,i) = rz*dcos(1,i)
                              CYCLE
                           ENDIF
                           spag_nextblock_3 = 2
                        CASE (2)
                           dcos(1,i) = 0.0
                           dcos(2,i) = 0.0
                           dcos(3,i) = 0.0
                           EXIT SPAG_DispatchLoop_3
                        END SELECT
                     ENDDO SPAG_DispatchLoop_3
                  ENDDO
                  ipts = 0
                  IF ( eltype==67 ) ipts = 1
!                   IHEX3
                  z(5) = sa
                  z(9) = sn
                  z(10) = so
                  z(ipts+13) = sb
                  z(ipts+19) = sc
                  DO i = 1 , 3
                     z(5+i) = dcos(1,i)
                     z(ipts+13+i) = dcos(2,i)
                     z(ipts+19+i) = dcos(3,i)
                  ENDDO
                  GOTO 28
               CASE (6)
!
!     PERFORM NRL SUMS
!
                  DO i = i1 , i2 , i3
                     sum = 0.
                     rmax = 0.
                     DO j = 1 , Nmodes
                        isub = nwds*(j-1) + i
                        sum = sum + z(isub)**2
                        IF ( abs(z(isub))>rmax ) rmax = abs(z(isub))
                     ENDDO
                     IF ( Sqrss==1 ) rmax = 0.
                     sum = sum - rmax**2
                     sum = rmax + sqrt(sum)
                     z(i) = sum
                  ENDDO
!
                  GOTO iret
!
!     WRITE NRL SUMS TO APPROPRIATE SCRATCH FILE
!
 28               Iz(1) = i5
                  CALL write(iscr,z,nwds,0)
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
!
         ENDDO
!
!     DONE WITH THIS ELEMENT.  SINCE WE ARE WRITING IN SORT1, EOR IS
!     NEEDED ON SCRATCH FILE ONLY IF ELEMENT TYPE CHANGES.  THIS WILL BE
!     CHECKED ABOVE.  SKIP EOR ON OES2 AND GO BACK.
!
         file = ifil
         CALL fwdrec(*80,ifil)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     EOF ON OES2.  WRITE EOR ON SCRATCH FILE AND COPY THEM TO OUTPUT
!     DATA BLOCK.
!
 40      CALL close(ifil,1)
!
         DO i = 2 , 7
            mcb(i) = 1
         ENDDO
         DO i = 1 , Nshock
            CALL write(scr(i),0,0,1)
            CALL close(scr(i),1)
            mcb(1) = scr(i)
            CALL wrttrl(mcb)
         ENDDO
!
         lcore = buf2 - 1
         CALL gopen(ofil,z(buf1),1)
         DO i = 1 , Nshock
            spag_nextblock_4 = 1
            SPAG_DispatchLoop_4: DO
               SELECT CASE (spag_nextblock_4)
               CASE (1)
                  CALL gopen(scr(i),z(buf2),0)
                  spag_nextblock_4 = 2
               CASE (2)
                  DO
!
                     CALL read(*44,*42,scr(i),z,lcore,0,iwords)
                     CALL write(ofil,z,lcore,0)
                  ENDDO
!
!     EOR
!
 42               CALL write(ofil,z,iwords,1)
                  spag_nextblock_4 = 2
                  CYCLE SPAG_DispatchLoop_4
!
!     EOF
!
 44               CALL close(scr(i),1)
                  EXIT SPAG_DispatchLoop_4
               END SELECT
            ENDDO SPAG_DispatchLoop_4
!
         ENDDO
!
         CALL close(ofil,1)
         mcb(1) = ofil
         CALL wrttrl(mcb)
!
!     GO BACK FOR FORCES
!
 60      IF ( ifil==oef2 ) RETURN
         ifil = oef2
         ofil = nrlfor
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
 80      n = -2
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 100     n = -3
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
         n = -8
         file = 0
         spag_nextblock_1 = 5
      CASE (5)
         CALL mesage(n,file,nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE nrlsum
