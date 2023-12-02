!*==ddr1.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ddr1
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: phidh , scr1 , udv , uhv
   EXTERNAL ssg2b
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     DYNAMIC DATA RECOVERY PART1
!
!     INPUTS  2 UHV,PHIDH
!
!     OUTPUTS 1 UDV
!
!     SCRATCHES  1
!
   DATA uhv , phidh , udv , scr1/101 , 102 , 201 , 301/
!
!     TRANSFPRM TO MODAL DISPLACEMENTS
!
   CALL ssg2b(phidh,uhv,0,udv,0,1,1,scr1)
END SUBROUTINE ddr1
