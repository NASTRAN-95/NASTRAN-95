!*==ktube.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ktube
   IMPLICIT NONE
   USE C_CONDAS
   USE C_SMA1DP
   USE C_SMA1ET
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , m
   EXTERNAL krod
!
! End of declarations rewritten by SPAG
!
!*****
! THE TUBE BEING SO SIMILAR TO THE ROD, WE ALTER THE ECPT FOR THE TUBE
! SO THAT IT IS IDENTICAL TO THE ONE FOR THE ROD AND THEN CALL KROD
! TO COMPUTE THE ELEMENT STIFFNESS MATRICES.
!*****
!
!
!
!                      E C P T  F O R  T H E  T U B E
!
!
!
! ECPT( 1)  -  ELEMENT ID.
! ECPT( 2)  -  SCALAR INDEX NUMBER FOR GRID POINT A
! ECPT( 3)  -  SCALAR INDEX NUMBER FOR GRID POINT B
! ECPT( 4)  -  MATERIAL ID.
! ECPT( 5)  -  OUTSIDE DIAMETER
! ECPT( 6)  -  THICKNESS
! ECPT( 7)  -  NON-STRUCTURAL MASS
! ECPT( 8)  -  COOR. SYS. ID. FOR GRID POINT A
! ECPT( 9)  -  BASIC COORDINATES OF GRID POINT A
! ECPT(10)  -                ...
! ECPT(11)  -                ...
! ECPT(12)  -  COOR. SYS. ID. FOR GRID POINT B
! ECPT(13)  -  BASIC COORDINATES OF GRID POINT B
! ECPT(14)  -                ...
! ECPT(15)  -                ...
! ECPT(16)  -  ELEMENT TEMPERATURE
!
!
!
!
!
!
!
!
!
!
!
!
   Temp = Ecpt(5) - Ecpt(6)
!
! COMPUTE AREA, TORSIONAL INERTIA AND STRESS COEFFICIENT.
!
   A = Temp*Ecpt(6)*Pi
   Fj = .25*A*(Temp**2+Ecpt(6)**2)
   C = .5*Ecpt(5)
!
! MOVE THE -END- OF THE ARRAY -DOWN ONE SLOT- SO THAT ENTRIES 7 THRU 16
! OF THE ECPT WILL BE STORED AT POSITIONS 8 THRU 17.
!
   m = 18
   DO i = 1 , 10
      m = m - 1
      Ecpt(m) = Ecpt(m-1)
   ENDDO
   Ecpt(5) = A
   Ecpt(6) = Fj
   Ecpt(7) = C
   CALL krod
END SUBROUTINE ktube
