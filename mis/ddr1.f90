
SUBROUTINE ddr1
   IMPLICIT NONE
!
! Local variable declarations
!
   INTEGER phidh , scr1 , udv , uhv
!
! End of declarations
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
