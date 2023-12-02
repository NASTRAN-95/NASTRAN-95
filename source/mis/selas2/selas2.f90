!*==selas2.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE selas2
   IMPLICIT NONE
   USE C_SDR2X4
   USE C_SDR2X7
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   REAL :: disp1 , disp2
   INTEGER :: icoeff , idisp , iu
!
! End of declarations rewritten by SPAG
!
!*****
! THIS ROUTINE IS PHASE II OF STRESS DATA RECOVERY FOR THE SCALAR SPRING
! ELEMENTS ELAS1, ELAS2, ELAS3 AND ELAS4.
!*****
!
!
!
!
! SDR2 VARIABLE CORE
!
!
! BLOCK FOR POINTERS, LOADING TEMPERATURE AND ELEMENT DEFORMATION.
!
!
! SDR2 INPUT AND OUTPUT BLOCK
!
   !>>>>EQUIVALENCE (Scoeff,Icoeff)
!
!
!
   idisp = Ivec - 1
   disp1 = 0.0
   disp2 = 0.0
   IF ( Isilno(1)>0 ) THEN
      iu = idisp + Isilno(1)
      disp1 = Zz(iu)
   ENDIF
   IF ( Isilno(2)>0 ) THEN
      iu = idisp + Isilno(2)
      disp2 = Zz(iu)
   ENDIF
   Jfelid = Jelid
   Force = Stiff*(disp1-disp2)
   IF ( icoeff==(-1) ) RETURN
   Stress = Scoeff*Force
   Jselid = Jelid
END SUBROUTINE selas2
