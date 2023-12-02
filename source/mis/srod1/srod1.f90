!*==srod1.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE srod1
   USE c_matin
   USE c_matout
   USE c_sdr2x5
   USE c_sdr2x6
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: gcovrl
   INTEGER , DIMENSION(13) :: iecpt
   EXTERNAL gmmats , mat , transs
!
! End of declarations rewritten by SPAG
!
!*****
! THIS ROUTINE IS PHASE I OF STRESS DATA RECOVERY FOR THE ROD.
!*****
!
!
!
!
!
!
!
! INPUT AND OUTPUT BLOCK
!
!
! SCRATCH BLOCK
!
!
! INPUT AND OUTPUT BLOCKS FOR SUBROUTINE MAT
!
!
!
!
!
!
!
   !>>>>EQUIVALENCE (Iecpt(1),Ecpt(1))
!
! CALL MAT TO GET MATERIAL PROPERTIES
!
   matidc = iecpt(4)
   matflg = 1
   eltemp = ecpt(17)
   CALL mat(iecpt(1))
!
! SET UP VECTOR ALONG THE ROD, COMPUTE LENGTH AND NORMALIZE
!
   xn(1) = ecpt(10) - ecpt(14)
   xn(2) = ecpt(11) - ecpt(15)
   xn(3) = ecpt(12) - ecpt(16)
   xl = xn(1)**2 + xn(2)**2 + xn(3)**2
   xl = sqrt(xl)
   xn(1) = xn(1)/xl
   xn(2) = xn(2)/xl
   xn(3) = xn(3)/xl
   eoverl = e/xl
   gcovrl = g*ecpt(6)/xl
   ibase = 0
!
! TRANSFORM XN VECTOR IF POINT A IS NOT IN BASIC COORDINATES.
!
   IF ( iecpt(9)/=0 ) THEN
      ibase = 3
      CALL transs(iecpt(9),ti)
      CALL gmmats(xn(1),3,1,1,ti(1),3,3,0,xn(4))
   ENDIF
   sat(1) = xn(ibase+1)*eoverl
   sat(2) = xn(ibase+2)*eoverl
   sat(3) = xn(ibase+3)*eoverl
   sar(1) = xn(ibase+1)*gcovrl
   sar(2) = xn(ibase+2)*gcovrl
   sar(3) = xn(ibase+3)*gcovrl
!
! TRANSFORM XN VECTOR IF POINT B IS NOT IN BASIC COORDINATES.
!
   ibase = 0
   IF ( iecpt(13)/=0 ) THEN
      ibase = 3
      CALL transs(iecpt(13),ti)
      CALL gmmats(xn(1),3,1,1,ti(1),3,3,0,xn(4))
   ENDIF
   sbt(1) = -xn(ibase+1)*eoverl
   sbt(2) = -xn(ibase+2)*eoverl
   sbt(3) = -xn(ibase+3)*eoverl
   sbr(1) = -xn(ibase+1)*gcovrl
   sbr(2) = -xn(ibase+2)*gcovrl
   sbr(3) = -xn(ibase+3)*gcovrl
!
! FILL REMAINDER OF OUTPUT BLOCK
!
   st = -alpha*e
   sdelta = -eoverl
   area = ecpt(5)
   IF ( ecpt(6)/=0 ) THEN
      fjovrc = ecpt(7)/ecpt(6)
   ELSE
      fjovrc = 0.0
   ENDIF
   tsubc0 = tsub0
   sigmat = sigt
   sigmac = sigc
   sigmas = sigs
   ielid = iecpt(1)
   isilno(1) = iecpt(2)
   isilno(2) = iecpt(3)
END SUBROUTINE srod1
