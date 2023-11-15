
SUBROUTINE amgt1s(Input,Fmat,Xyzb,Index)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Amach , Amachr , Blspc , Bspace , Chord , Dcbdzb , Degra , Den , Dum(2) , Mach , Maxmac , Minmac , Pi , Radeg , Redf ,      &
      & Refc , Refcrd , Refden , Refmac , Refstg , Refswp , Refvel , Rfreq , S4piso , Sigma , Stager , Sweep , Sysbuf , Twopi ,     &
      & Vel , Xsign
   LOGICAL Debug , Tsonic
   INTEGER Iout , Iref , Mcb(7) , Nlines , Nrow , Nstns , Nstnsx , Sln
   CHARACTER*23 Ufm
   COMMON /amgbug/ Debug
   COMMON /amgmn / Mcb , Nrow , Dum , Refc , Sigma , Rfreq
   COMMON /condas/ Pi , Twopi , Radeg , Degra , S4piso
   COMMON /system/ Sysbuf , Iout
   COMMON /tamg1l/ Iref , Minmac , Maxmac , Nlines , Nstns , Refstg , Refcrd , Refmac , Refden , Refvel , Refswp , Sln , Nstnsx ,   &
                 & Stager , Chord , Dcbdzb , Bspace , Mach , Den , Vel , Sweep , Amach , Redf , Blspc , Amachr , Tsonic , Xsign
   COMMON /xmssg / Ufm
!
! Dummy argument declarations
!
   INTEGER Input
   REAL Fmat(Nstns,Nstns) , Xyzb(3,Nstns)
   INTEGER Index(1)
!
! Local variable declarations
!
   REAL al1 , al1sq , al2sq , an , arg , ch2 , determ , dum1 , pic , tbl(3,3) , x , xa , xb , xba , ya , yb , yba , za , zb , zba
   INTEGER i , ising , j , nstns3
!
! End of declarations
!
!
!     COMPUTE F(INVERSE) FOR THIS STREAMLINE
!
!
!     READ STREAMLINE DATA
!
   nstns3 = 3*Nstns
   CALL fread(Input,Sln,10,0)
   CALL fread(Input,Xyzb,nstns3,0)
   IF ( Debug ) CALL bug1('ACPT-SLN  ',10,Sln,10)
   IF ( Debug ) CALL bug1('XYZB      ',20,Xyzb,nstns3)
!
!     (1) COMPUTE BASIC TO LOCAL TRANSFORMATION
!         XYZB ARRAY CONTAINS X,Y,Z COORDINATES IN BASIC SYSTEM
!         FOR ALL NODES ON THE STREAMLINE LEADING EDGE TO TRAILING EDGE
!     (2) TRANSFORM BASIC X,Y,Z ON STREAMLINE TO LOCAL X,Y,Z-S
!     (3) COMPUTE FMAT(NSTNS X NSTNS)
!     (4) COMPUTE FMAT(INVERS) - USE -
!         CALL INVERS(NSTNS,FMAT,NSTNS,DUM1,0,DETERM,ISING,INDEX)
!
   xa = Xyzb(1,1)
   ya = Xyzb(2,1)
   za = Xyzb(3,1)
   xb = Xyzb(1,Nstns)
   yb = Xyzb(2,Nstns)
   zb = Xyzb(3,Nstns)
   xba = xb - xa
   yba = yb - ya
   zba = zb - za
   al2sq = xba**2 + yba**2
   al1sq = al2sq + zba**2
   al1 = sqrt(al1sq)
   tbl(1,1) = xba/al1
   tbl(1,2) = yba/al1
   tbl(1,3) = zba/al1
   Fmat(1,1) = 1.0
   pic = Pi/Chord
   ch2 = 2.0/Chord
   DO i = 2 , Nstns
      x = tbl(1,1)*(Xyzb(1,i)-Xyzb(1,1)) + tbl(1,2)*(Xyzb(2,i)-Xyzb(2,1)) + tbl(1,3)*(Xyzb(3,i)-Xyzb(3,1))
      Fmat(1,i) = 0.0
      Fmat(i,1) = 1.0
      Fmat(i,2) = ch2*x
      DO j = 3 , Nstns
         an = j - 2
         arg = pic*an*x
         Fmat(i,j) = sin(arg)
      ENDDO
   ENDDO
   IF ( Debug ) CALL bug1('FMAT      ',50,Fmat,Nstns*Nstns)
   ising = -1
   CALL invers(Nstns,Fmat,Nstns,dum1,0,determ,ising,Index)
   IF ( Debug ) CALL bug1('FMAT-INV  ',60,Fmat,Nstns*Nstns)
   IF ( ising==2 ) THEN
!
!     ERROR MESSAGE,  SINGULAR MATRIX
!
      WRITE (Iout,99001) Ufm , Sln
99001 FORMAT (A23,' -AMG MODULE- SINGULAR MATRIX IN ROUTINE AMGT1S FOR',' STREAML2, SLN =',I3,/39X,'CHECK STREAML2 BULK DATA CARD.')
      CALL mesage(-61,0,0)
      GOTO 99999
   ENDIF
   RETURN
99999 END SUBROUTINE amgt1s
