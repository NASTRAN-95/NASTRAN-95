
SUBROUTINE amgb1s(Input,Fmat,Xyzb,Index,Radii,Wfact,Nline)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Amach , Amachr , Blspc , Bspace , Chord , Degra , Den , Dum(2) , Flowa , Mach , Maxmac , Minmac , Pi , Radeg , Radius ,     &
      & Redf , Refc , Refcrd , Refden , Refflo , Refmac , Refstg , Refvel , Rfreq , S4piso , Sigma , Stager , Sysbuf , Twopi , Vel ,&
      & Xsign
   LOGICAL Debug , Tsonic
   INTEGER Iout , Iref , Mcb(7) , Nlines , Nrow , Nstns , Nstnsx , Sln
   CHARACTER*23 Ufm
   COMMON /amgbug/ Debug
   COMMON /amgmn / Mcb , Nrow , Dum , Refc , Sigma , Rfreq
   COMMON /bamg1l/ Iref , Minmac , Maxmac , Nlines , Nstns , Refstg , Refcrd , Refmac , Refden , Refvel , Refflo , Sln , Nstnsx ,   &
                 & Stager , Chord , Radius , Bspace , Mach , Den , Vel , Flowa , Amach , Redf , Blspc , Amachr , Tsonic , Xsign
   COMMON /condas/ Pi , Twopi , Radeg , Degra , S4piso
   COMMON /system/ Sysbuf , Iout
   COMMON /xmssg / Ufm
!
! Dummy argument declarations
!
   INTEGER Input , Nline
   REAL Wfact
   REAL Fmat(Nstns,Nstns) , Radii(1) , Xyzb(3,Nstns)
   INTEGER Index(1)
!
! Local variable declarations
!
   REAL al1 , al1sq , al2 , al2sq , an , arg , ch2 , determ , dum1 , pic , tbl(3,3) , x , xa , xb , xba , ya , yb , yba , za , zb , &
      & zba
   INTEGER i , ising , j , k , l , nstns3
!
! End of declarations
!
!
!     COMPUTE F(INVERSE) AND WFACT FOR THIS STREAMLINE
!
!
!     READ STREAMLINE DATA
!
   nstns3 = 3*Nstns
   CALL fread(Input,Sln,10,0)
   CALL fread(Input,Xyzb,nstns3,0)
   IF ( Debug ) CALL bug1('ACPT-SLN  ',11,Sln,10)
   IF ( Debug ) CALL bug1('XYZB      ',12,Xyzb,nstns3)
!
!     COMPUTE PARAMETERS
!
   Amach = Mach*cos(Degra*(Flowa-Stager))
   Amachr = Refmac*cos(Degra*(Refflo-Refstg))
   IF ( Debug ) CALL bug1('AMACH     ',13,Amach,1)
   IF ( Debug ) CALL bug1('AMACHR    ',14,Amachr,1)
!
!     (1) COMPUTE BASIC TO LOCAL TRANSFORMATION
!         XYZB ARRAY CONTAINS X,Y,Z COORDINATES IN BASIC SYSTEM
!         FOR ALL NODES ON THE STREAMLINE LEADING EDGE TO TRAILING EDGE
!     (2) TRANSFORM BASIC X,Y,Z ON STREAMLINE TO LOCAL X,Y,Z-S
!     (3) COMPUTE FMAT(NSTNS X NSTNS)
!     (4) COMPUTE FMAT(INVERS) - USE -
!         CALL INVERS(NSTNS,FMAT,NSTNS,DUM1,0,DETERM,ISING,INDEX)
!     (5) COMPUTE WFACT FOR THIS STREAMLINE - NOTE - ALL RADIUS HAVE
!         ALREADY BEEN STORED IN ARRAY RADII FOR ALL STREAMLINES
!
   xa = Xyzb(1,1)
   ya = Xyzb(2,1)
   za = Xyzb(3,1)
   xb = Xyzb(1,Nstns)
   yb = Xyzb(2,Nstns)
   zb = Xyzb(3,Nstns)
!
!     EVALUATE  TBL  ROW 2
!
   xba = xb - xa
   yba = yb - ya
   zba = zb - za
   al2sq = xba**2 + yba**2
   al2 = sqrt(al2sq)
   al1sq = al2sq + zba**2
   al1 = sqrt(al1sq)
   tbl(2,1) = -Xsign*(yba/al2)
   tbl(2,2) = Xsign*(xba/al2)
   tbl(2,3) = 0.0
!
!     EVAL  TBL  ROW 1
!
   tbl(1,1) = xba/al1
   tbl(1,2) = yba/al1
   tbl(1,3) = zba/al1
!
!     EVALUATE TBL  ROW 3
!
   tbl(3,1) = -tbl(1,3)*(xba/al2)
   tbl(3,2) = -tbl(1,3)*(yba/al2)
   tbl(3,3) = al2/al1
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
!     ERROR MESSAGE, SINGULAR MATRIX
!
      WRITE (Iout,99001) Ufm , Sln
99001 FORMAT (A23,' -AMG MODULE- SINGULAR MATRIX IN ROUTINE AMGB1S FOR',' STREAML2, SLN =',I3,/39X,'CHECK STREAML2 BULK DATA CARD.')
      CALL mesage(-61,0,0)
      GOTO 99999
   ENDIF
   k = Nline + 1
   l = k - 2
   IF ( Nline==1 ) l = 1
   IF ( Nline==Nlines ) k = Nlines
!
!     COMPUT WFACT FOR THIS STREAMLINE
!
   Wfact = (Den/Refden)*(Vel/Refvel)**2*((Amach*Refmac)/(Mach*Amachr))**2*(Radii(k)-Radii(l))*0.5
   IF ( Debug ) CALL bug1('WFACT     ',70,Wfact,1)
   RETURN
99999 RETURN
END SUBROUTINE amgb1s