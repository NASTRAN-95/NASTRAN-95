!*==crdrd.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE crdrd(Mu,Indcom,N23) !HIDESTARS (*,*,Mu,Indcom,N23)
   IMPLICIT NONE
   USE C_GP4FIL
   USE C_GP4PRM
   USE C_MACHIN
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Mu
   INTEGER :: Indcom
   INTEGER :: N23
!
! Local variable declarations rewritten by SPAG
!
   REAL :: cdep , coeff , rlngth , xd , yd , zd
   REAL , DIMENSION(3) :: ddrcos , idrcos , rodcos
   REAL , DIMENSION(9) :: deptfm , indtfm
   REAL , DIMENSION(1) :: dz , rz
   INTEGER :: i , idep , mask15
   INTEGER , DIMENSION(2) :: mcode
   EXTERNAL gmmats , transs , write
!
! End of declarations rewritten by SPAG
!
!
!     WRITE THE RIGID ROD ELEMENT ON THE RG FILE
!
!     EXTERNAL        ORF    ,LSHIFT
!     INTEGER         ORF    ,LSHIFT
   !>>>>EQUIVALENCE (Z(1),Dz(1),Rz(1))
!
!     INDTFM = INDEPENDENT GRID POINT TRANSFORMATION MATRIX
!     DEPTFM = DEPENDENT GRID POINT TRANSFORMATION MATRIX
!     RODCOS = BASIC COSINES OF ROD ELEMENT
!     IDRCOS = DIRECTION COSINES OF INDEPENDENT GRID POINT
!     DDRCOS = DIRECTION COSINES OF DEPENDENT GRID POINT
!
   mask15 = Jhalf/2
!
!     OBTAIN TRANSFORMATION MATRIX
!
   IF ( Z(Knkl1+3)/=0 ) THEN
      DO i = 1 , 4
         Buf(i) = Z(Knkl1+2+i)
      ENDDO
      CALL transs(Buf,indtfm)
   ENDIF
   IF ( Z(Knkl1+10)/=0 ) THEN
      DO i = 1 , 4
         Buf(i) = Z(Knkl1+9+i)
      ENDDO
      CALL transs(Buf,deptfm)
   ENDIF
!
!     COMPUTE THE LENGTH OF THE RIGID ROD ELEMENT
!
   xd = rz(Knkl1+11) - rz(Knkl1+4)
   yd = rz(Knkl1+12) - rz(Knkl1+5)
   zd = rz(Knkl1+13) - rz(Knkl1+6)
!
!     CHECK TO SEE IF LENGTH OF ROD IS ZERO
!
   IF ( xd==0.0 .AND. yd==0.0 .AND. zd==0.0 ) RETURN 1
   rlngth = sqrt(xd*xd+yd*yd+zd*zd)
!
!     COMPUTE THE BASIC DIRECTION COSINES OF THE RIGID ROD ELEMENT
!
   rodcos(1) = xd/rlngth
   rodcos(2) = yd/rlngth
   rodcos(3) = zd/rlngth
!
!     OBTAIN THE DIRECTION COSINES ASSOCIATED WITH
!     THE INDEPENDENT GRID POINT
!
   IF ( Z(Knkl1+3)/=0 ) THEN
      CALL gmmats(rodcos,1,3,0,indtfm,3,3,0,idrcos)
   ELSE
      DO i = 1 , 3
         idrcos(i) = rodcos(i)
      ENDDO
   ENDIF
!
!     OBTAIN THE DIRECTION COSINES ASSOCIATED WITH
!     THE DEPENDENT GRID POINT
!
   IF ( Z(Knkl1+10)/=0 ) THEN
      CALL gmmats(rodcos,1,3,0,deptfm,3,3,0,ddrcos)
   ELSE
      DO i = 1 , 3
         ddrcos(i) = rodcos(i)
      ENDDO
   ENDIF
!
!     DETERMINE THE DEPENDENT SIL AND THE CORRESPONDING COEFFICIENT
!
   SPAG_Loop_1_1: DO i = 1 , 3
      IF ( Indcom==i ) THEN
         idep = Z(Knkl1+6+i)
         cdep = rodcos(i)
         EXIT SPAG_Loop_1_1
      ENDIF
   ENDDO SPAG_Loop_1_1
!
!     CHECK TO SEE IF RIGID ROD IS PROPERLY DEFINED
!
   IF ( abs(cdep)<0.0 ) RETURN 2
   mcode(2) = idep
   IF ( idep>mask15 ) N23 = 3
   DO i = 1 , 3
      mcode(1) = Z(Knkl1+i-1)
      IF ( mcode(1)>mask15 ) N23 = 3
      coeff = -idrcos(i)/cdep
      CALL write(Rgt,mcode,2,0)
      CALL write(Rgt,coeff,1,0)
      mcode(1) = Z(Knkl1+6+i)
      IF ( mcode(1)>mask15 ) N23 = 3
      coeff = ddrcos(i)/cdep
      CALL write(Rgt,mcode,2,0)
      CALL write(Rgt,coeff,1,0)
   ENDDO
   Z(Mu) = idep
   Mu = Mu - 1
END SUBROUTINE crdrd
