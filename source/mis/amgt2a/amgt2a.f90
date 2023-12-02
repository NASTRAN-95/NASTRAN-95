!*==amgt2a.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE amgt2a(Input,Fmat,Xyzb,Index)
   USE c_amgbug
   USE c_amgmn
   USE c_condas
   USE c_system
   USE c_tamg2l
   USE c_xmssg
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Input
   REAL , DIMENSION(nstns,nstns) :: Fmat
   REAL , DIMENSION(3,nstns) :: Xyzb
   INTEGER , DIMENSION(1) :: Index
!
! Local variable declarations rewritten by SPAG
!
   REAL :: al1 , al1sq , al2sq , an , arg , ch2 , determ , dum1 , pic , x , xa , xb , xba , ya , yb , yba , za , zb , zba
   INTEGER :: i , ising , j , nstns3
   REAL , DIMENSION(3,3) :: tbl
   EXTERNAL bug1 , fread , invers , mesage
!
! End of declarations rewritten by SPAG
!
!
!     COMPUTE F(INVERSE) FOR THIS STREAMLINE
!
!
!     READ STREAMLINE DATA
!
   nstns3 = 3*nstns
   CALL fread(Input,sln,10,0)
   CALL fread(Input,Xyzb,nstns3,0)
   IF ( debug ) CALL bug1('ACPT-SLN  ',10,sln,10)
   IF ( debug ) CALL bug1('XYZB      ',20,Xyzb,nstns3)
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
   xb = Xyzb(1,nstns)
   yb = Xyzb(2,nstns)
   zb = Xyzb(3,nstns)
   xba = xb - xa
   yba = yb - ya
   zba = zb - za
   al2sq = xba**2 + yba**2
   al1sq = al2sq + zba**2
   al1 = sqrt(al1sq)
!
!     EVAL  TBL  ROW 1
!
   tbl(1,1) = xba/al1
   tbl(1,2) = yba/al1
   tbl(1,3) = zba/al1
   Fmat(1,1) = 1.0
   pic = pi/chord
   ch2 = 2.0/chord
   DO i = 2 , nstns
      x = tbl(1,1)*(Xyzb(1,i)-Xyzb(1,1)) + tbl(1,2)*(Xyzb(2,i)-Xyzb(2,1)) + tbl(1,3)*(Xyzb(3,i)-Xyzb(3,1))
      Fmat(1,i) = 0.0
      Fmat(i,1) = 1.0
      Fmat(i,2) = ch2*x
      DO j = 3 , nstns
         an = j - 2
         arg = pic*an*x
         Fmat(i,j) = sin(arg)
      ENDDO
   ENDDO
   IF ( debug ) CALL bug1('FMAT      ',50,Fmat,nstns*nstns)
   ising = -1
   CALL invers(nstns,Fmat,nstns,dum1,0,determ,ising,Index)
   IF ( debug ) CALL bug1('FMAT-INV  ',60,Fmat,nstns*nstns)
   IF ( ising==2 ) THEN
!
!     ERROR MESSAGE.  SINGULAR MATRIX
!
      WRITE (iout,99001) ufm , sln
99001 FORMAT (A23,' -AMG MODULE- SINGULAR MATRIX IN ROUTINE AMGT2A FOR',' STREAML2, SLN =',I3,/39X,'CHECK STREAML2 BULK DATA CARD.')
      CALL mesage(-61,0,0)
      RETURN
   ENDIF
END SUBROUTINE amgt2a
