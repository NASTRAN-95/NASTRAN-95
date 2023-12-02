!*==stube1.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE stube1
   USE c_condas
   USE c_sdr2x5
   USE c_sdr2x6
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , m
   EXTERNAL srod1
!
! End of declarations rewritten by SPAG
!
!*****
! THE TUBE BEING SO SIMILAR TO THE ROD, WE ALTER THE ECPT FOR THE TUBE
! SO THAT IT IS IDENTICAL TO THE ONE FOR THE ROD AND THEN CALL SROD1
! TO COMPUTE THE PHASE I PARAMETERS FOR STRESS DATA RECOVERY FOR THE ROD
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
! SDR2 PHASE I INPUT AND OUTPUT BLOCK
!
!
! SDR2 SCRATCH BLOCK
!
!
! PHYSICAL CONSTANTS
!
!
!
!
   temp = ecpt(5) - ecpt(6)
!
! COMPUTE AREA, TORSIONAL INERTIA AND STRESS COEFFICIENT.
!
   a = temp*ecpt(6)*pi
   fj = .25*a*(temp**2+ecpt(6)**2)
   c = .5*ecpt(5)
!
! MOVE THE -END- OF THE ARRAY -DOWN ONE SLOT- SO THAT ENTRIES 7 THRU 16
! OF THE ECPT WILL BE STORED AT POSITIONS 8 THRU 17.
!
   m = 18
   DO i = 1 , 10
      m = m - 1
      ecpt(m) = ecpt(m-1)
   ENDDO
   ecpt(5) = a
   ecpt(6) = fj
   ecpt(7) = c
   CALL srod1
END SUBROUTINE stube1
