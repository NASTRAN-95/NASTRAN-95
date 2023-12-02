!*==selas2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE selas2
   USE c_sdr2x4
   USE c_sdr2x7
   USE c_zzzzzz
   IMPLICIT NONE
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
   idisp = ivec - 1
   disp1 = 0.0
   disp2 = 0.0
   IF ( isilno(1)>0 ) THEN
      iu = idisp + isilno(1)
      disp1 = zz(iu)
   ENDIF
   IF ( isilno(2)>0 ) THEN
      iu = idisp + isilno(2)
      disp2 = zz(iu)
   ENDIF
   jfelid = jelid
   force = stiff*(disp1-disp2)
   IF ( icoeff==(-1) ) RETURN
   stress = scoeff*force
   jselid = jelid
END SUBROUTINE selas2
