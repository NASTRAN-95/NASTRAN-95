!*==srod1.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE srod1
   IMPLICIT NONE
   USE C_MATIN
   USE C_MATOUT
   USE C_SDR2X5
   USE C_SDR2X6
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
   Matidc = iecpt(4)
   Matflg = 1
   Eltemp = Ecpt(17)
   CALL mat(iecpt(1))
!
! SET UP VECTOR ALONG THE ROD, COMPUTE LENGTH AND NORMALIZE
!
   Xn(1) = Ecpt(10) - Ecpt(14)
   Xn(2) = Ecpt(11) - Ecpt(15)
   Xn(3) = Ecpt(12) - Ecpt(16)
   Xl = Xn(1)**2 + Xn(2)**2 + Xn(3)**2
   Xl = sqrt(Xl)
   Xn(1) = Xn(1)/Xl
   Xn(2) = Xn(2)/Xl
   Xn(3) = Xn(3)/Xl
   Eoverl = E/Xl
   gcovrl = G*Ecpt(6)/Xl
   Ibase = 0
!
! TRANSFORM XN VECTOR IF POINT A IS NOT IN BASIC COORDINATES.
!
   IF ( iecpt(9)/=0 ) THEN
      Ibase = 3
      CALL transs(iecpt(9),Ti)
      CALL gmmats(Xn(1),3,1,1,Ti(1),3,3,0,Xn(4))
   ENDIF
   Sat(1) = Xn(Ibase+1)*Eoverl
   Sat(2) = Xn(Ibase+2)*Eoverl
   Sat(3) = Xn(Ibase+3)*Eoverl
   Sar(1) = Xn(Ibase+1)*gcovrl
   Sar(2) = Xn(Ibase+2)*gcovrl
   Sar(3) = Xn(Ibase+3)*gcovrl
!
! TRANSFORM XN VECTOR IF POINT B IS NOT IN BASIC COORDINATES.
!
   Ibase = 0
   IF ( iecpt(13)/=0 ) THEN
      Ibase = 3
      CALL transs(iecpt(13),Ti)
      CALL gmmats(Xn(1),3,1,1,Ti(1),3,3,0,Xn(4))
   ENDIF
   Sbt(1) = -Xn(Ibase+1)*Eoverl
   Sbt(2) = -Xn(Ibase+2)*Eoverl
   Sbt(3) = -Xn(Ibase+3)*Eoverl
   Sbr(1) = -Xn(Ibase+1)*gcovrl
   Sbr(2) = -Xn(Ibase+2)*gcovrl
   Sbr(3) = -Xn(Ibase+3)*gcovrl
!
! FILL REMAINDER OF OUTPUT BLOCK
!
   St = -Alpha*E
   Sdelta = -Eoverl
   Area = Ecpt(5)
   IF ( Ecpt(6)/=0 ) THEN
      Fjovrc = Ecpt(7)/Ecpt(6)
   ELSE
      Fjovrc = 0.0
   ENDIF
   Tsubc0 = Tsub0
   Sigmat = Sigt
   Sigmac = Sigc
   Sigmas = Sigs
   Ielid = iecpt(1)
   Isilno(1) = iecpt(2)
   Isilno(2) = iecpt(3)
END SUBROUTINE srod1
