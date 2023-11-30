
SUBROUTINE rforce(Lcore)
   IMPLICIT NONE
   REAL A(4) , Degra , Dumy(25) , Pi , Radeg , S4pisq , Ti(3,3) , To(3,3) , Twophi , Z(1)
   INTEGER Bgpdt , Ieol , Ieor , Ihalf , Ii , Incr , Irow , Isystm(175) , It1 , Ix(5) , Jhalf , Jj , Lc , Mach , Mgg , Mn , Nn(11) ,&
         & Old , Slt , Sysbuf
   COMMON /condas/ Pi , Twophi , Radeg , Degra , S4pisq
   COMMON /loadx / Lc , Slt , Bgpdt , Old , Nn , Mgg
   COMMON /machin/ Mach , Ihalf , Jhalf
   COMMON /system/ Sysbuf , Dumy , Mn
   COMMON /tranx / Ix , To
   COMMON /unpakx/ It1 , Ii , Jj , Incr
   COMMON /xcstm / Ti
   COMMON /zntpkx/ A , Irow , Ieol , Ieor
   COMMON /zzzzzz/ Z
   INTEGER Lcore
   INTEGER andf , rshift
   REAL card(6) , mr(3,3) , mt(3,3) , mtr(3,3) , ra(4) , ri(4) , wb(3) , wg(3) , xm(6,6)
   LOGICAL cupmas , nonshl
   INTEGER file , i , i1 , icard(6) , icrrqd , iflag , ip1 , iptax , ir , ira , iy(7) , j , kountm , name(2) , ncol , nharms ,      &
         & nrings , strtmn
   EXTERNAL andf , rshift
!
!     COMPUTES STATIC LOADS DUE TO ROTATING COORDINATE SYSTEMS
!
   EQUIVALENCE (icard(1),card(1)) , (ir,ri(1)) , (ira,ra(1))
   EQUIVALENCE (Sysbuf,Isystm(1))
   DATA name/4HRFOR , 4HCE  /
!
!     DEFINITION OF VARIABLES
!
!     SLT      STATIC LOAD TABLE
!     BGPDT    BASIC GRID POINT DEFINITION TABLE
!     MGG      MASS  MATRIX
!     FILE     FILE NAME FOR ERROR MESAGES
!     CARD     CARD IMAGE OF RFORCE CARD
!     RA       BGPDT ENTRY FOR AXIAL GRID POINT
!     WB       OMEGA-S IN BASIC COORDINATES
!     II       SIL OF CURRENT  POINT
!     IT1      UNPACK TYPE(REAL)
!     INCR     INCREMENT( TO ROW STORE COLUMNS)
!     RI       BGPDT ENTRY FOR CURRENT GRID POINT
!     WG       OMEGA-S IN GLOBAL COORDINANTS AT CURRENT GRID POINT
!     XM       6X6 DIAGONAL PARTION OF MGG
!     MT       3X3 PARTITION OF  MGG
!     MR       3X3 PARTITION OF  MGG
!     MTR      3X3 PARTITION OF  MGG
!     OLD      CURRENT POSITION OF BGPDT  0 IMPLIES BEGINNING
!
!
!     BRING IN CARD IMAGE
!
   CALL fread(Slt,card,6,0)
!
!     FIND LOCATION OF AXIAL GRID POINT
!
   DO i = 1 , 3
      ra(i+1) = 0.0
   ENDDO
   IF ( icard(1)/=0 ) THEN
      CALL fndpnt(ra(1),icard(1))
!
!     CHECK FOR GRID POINT
!
      IF ( ira==-1 ) THEN
         DO i = 1 , 3
            ra(i+1) = 0.0
         ENDDO
      ENDIF
   ENDIF
   CALL rewind(Bgpdt)
   CALL skprec(Bgpdt,1)
!
!     CONVERT WI'S TO BASIC COORDINANTS
!
   DO i = 4 , 6
      wb(i-3) = card(i)*Twophi*card(3)
   ENDDO
   IF ( icard(2)/=0 ) THEN
      CALL fdcstm(icard(2))
      CALL mpyl(To,wb,3,3,1,wg)
      DO i = 1 , 3
         wb(i) = wg(i)
      ENDDO
   ENDIF
!
!     OPEN MASS MATRIX
!
   j = Lcore - Sysbuf
   IF ( j<=0 ) THEN
      icrrqd = iabs(j) + 1
      CALL mesage(-8,icrrqd,name)
   ENDIF
   CALL gopen(Mgg,Z(j),0)
   It1 = 1
!
!     TEST FOR COUPLED MASS
!
   iy(1) = Mgg
   CALL rdtrl(iy)
   cupmas = .FALSE.
   IF ( iy(6)/=1 ) THEN
      IF ( iy(6)>6 ) cupmas = .TRUE.
      IF ( .NOT.(cupmas) ) THEN
         Incr = 0
         ncol = iy(2)
         DO i = 1 , ncol
            Ii = 0
            CALL unpack(*20,Mgg,A)
            IF ( Jj-Ii>6 ) cupmas = .TRUE.
            IF ( cupmas ) EXIT
 20      ENDDO
         CALL rewind(Mgg)
         CALL skprec(Mgg,1)
      ENDIF
   ENDIF
   Ii = 1
   Incr = 6
!
!     TEST FOR CONICAL SHELL PROBLEM
!
   nonshl = .TRUE.
   IF ( Mn/=0 ) THEN
      nonshl = .FALSE.
      nharms = Mn
      nrings = Isystm(161)
      iy(1) = Bgpdt
      CALL rdtrl(iy)
      strtmn = iy(2) - nharms*nrings
      iptax = 0
      kountm = 0
   ENDIF
!
!     BRING IN BGPDT
!
 100  file = Bgpdt
   CALL read(*500,*400,Bgpdt,ri(1),4,0,iflag)
!
!     TEST FOR CONICAL SHELL PROCESSING
!
   IF ( .NOT.(nonshl) ) THEN
      iptax = iptax + 1
      IF ( iptax<strtmn ) THEN
!
         IF ( ir/=-1 ) CALL skprec(Mgg,6)
      ELSE
         kountm = kountm + 1
         IF ( kountm>nrings ) GOTO 400
!
!     CONICAL SHELL PROCESSING
!     COMPUTE A = R*WB**2
!
         xm(2,3) = 0.0
         xm(3,3) = 0.0
         xm(1,3) = ri(2)*wb(2)*wb(2)
         GOTO 200
      ENDIF
   ENDIF
!
!     CHECK FOR SCALAR POINT
!
   IF ( ir==-1 ) THEN
      CALL skprec(Mgg,1)
      Ii = Ii + 1
      GOTO 100
!
!     TEST FOR COUPLED MASS PROCESSING
!
   ELSEIF ( cupmas ) THEN
!
!     COUPLED MASS PROCESSING
!     COMPUTE -WB*(WB*(RI - RA))
!
      DO i = 1 , 3
         xm(i,1) = ri(i+1) - ra(i+1)
      ENDDO
      CALL cross(wb(1),xm(1,1),xm(1,3))
      CALL cross(xm(1,3),wb(1),xm(1,1))
      IF ( ir==0 ) THEN
         DO i = 1 , 3
            xm(i,3) = xm(i,1)
         ENDDO
      ELSE
         CALL basglb(xm(1,1),xm(1,3),ri(2),ir)
      ENDIF
   ELSE
!
!     CONVERT WB'S TO GLOBAL COORDINATES AT RI
!
      DO i = 1 , 3
         wg(i) = wb(i)
      ENDDO
      IF ( ir/=0 ) CALL basglb(wb(1),wg(1),ri(2),ir)
!
!     BRING IN  6X6  ON DIAGONAL OF MASS MATRIX
!
      Jj = Ii + 5
      DO j = 1 , 6
         DO i = 1 , 6
            xm(i,j) = 0.0
         ENDDO
      ENDDO
      DO i = 1 , 6
         CALL unpack(*150,Mgg,xm(i,1))
 150  ENDDO
!
!     MOVE  6X6 TO PARTITIONS
!
      DO i = 1 , 3
         DO j = 1 , 3
            mt(j,i) = xm(j,i)
            mr(j,i) = xm(j+3,i+3)
            mtr(j,i) = xm(j+3,i)
         ENDDO
      ENDDO
!
!     COMPUTE WBX(RI-RA)
!
      DO i = 1 , 3
         xm(i,1) = ri(i+1) - ra(i+1)
      ENDDO
      CALL cross(wb(1),xm(1,1),xm(1,3))
      DO i = 1 , 3
         xm(i,1) = xm(i,3)
      ENDDO
      IF ( ir/=0 ) CALL mpyl(Ti(1,1),xm(1,1),3,3,1,xm(1,3))
!
!     COMPUTE MOMENTS
!
      CALL mpyl(mr(1,1),wg(1),3,3,1,xm(1,1))
      CALL cross(xm(1,1),wg(1),xm(1,2))
      CALL mpylt(mtr(1,1),xm(1,3),3,3,1,xm(1,1))
      CALL cross(xm(1,1),wg,xm(1,4))
      j = Ii + 2
      DO i = 1 , 3
         j = j + 1
         Z(j) = Z(j) + xm(i,2) + xm(i,4)
      ENDDO
!
!     COMPUTE FORCES
!
      CALL mpyl(mtr(1,1),wg(1),3,3,1,xm(1,1))
      CALL cross(xm(1,1),wg(1),xm(1,2))
      CALL mpyl(mt(1,1),xm(1,3),3,3,1,xm(1,1))
      CALL cross(xm(1,1),wg,xm(1,4))
      j = Ii - 1
      DO i = 1 , 3
         j = j + 1
         Z(j) = Z(j) + xm(i,4) + xm(i,2)
      ENDDO
!
!     BUMP  II
!
      Ii = Ii + 6
      GOTO 100
   ENDIF
!
!     COMPUTE F = M*A
!
 200  i1 = 1
   DO i = 1 , 3
      CALL intpk(*300,Mgg,0,i1,0)
      IF ( xm(i,3)==0.0 ) THEN
         CALL skprec(Mgg,1)
      ELSE
         DO
            CALL zntpki
            Z(Irow) = Z(Irow) + A(1)*xm(i,3)
            IF ( Ieol==1 ) EXIT
         ENDDO
      ENDIF
 300  ENDDO
   CALL skprec(Mgg,3)
   GOTO 100
!
!     EOR IN BGPDT
!
 400  CALL close(Mgg,1)
   CALL rewind(Bgpdt)
   Old = 0
   CALL skprec(Bgpdt,1)
   RETURN
 500  DO
      ip1 = -2
!
!     FILE ERRORS
!
      CALL mesage(ip1,file,name(1))
   ENDDO
END SUBROUTINE rforce
