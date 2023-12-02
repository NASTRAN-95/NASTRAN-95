!*==tubes.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE tubes
   USE c_condas
   USE c_emgest
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a , c , fj , temp
   INTEGER :: i , m
   EXTERNAL rods
!
! End of declarations rewritten by SPAG
!
!
!***
!  THE TUBE BEING SO SIMILAR TO THE ROD, WE ALTER THE EST FOR THE TUBE
!  SO THAT IT IS IDENTICAL TO THE ONE FOR THE ROD AND THEN CALL RODS
! SINGLE PRECISION VERSION
! SINGLE AND DOUBLE PRECISION VERSIONS OF THIS ROUTINE ARE IDENTICAL
! APART FROM THE NAME AND THE CALL TO RODD (RODS)
!***
!
!
! EST( 1) - ELEMENT ID.
! EST( 2) - SCALAR INDEX NUMBER FOR GRID POINT A
! EST( 3) - SCALAR INDEX NUMBER FOR GRID POINT B
! EST( 4) - MATERIAL ID.
! EST( 5) - OUTSIDE DIAMETER
! EST( 6) - THICKNESS
! EST( 7) - NON-STRUCTURAL MASS
! EST( 8) - COOR. SYS. ID. FOR GRID POINT A
! EST( 9) - BASIC COORDINATES OF GRID POINT A
! EST(10) -                ...
! EST(11) -                ...
! EST(12) - COOR. SYS. ID. FOR GRID POINT B
! EST(13) - BASIC COORDINATES OF GRID POINT B
! EST(14) -               ...
! EST(15) -               ...
! EST(16) - ELEMENT TEMPERATURE
!
!
! ----------------------------------------------------------------------
!
   temp = est(5) - est(6)
   a = temp*est(6)*pi
   fj = .25*a*(temp**2+est(6)**2)
   c = .5*est(5)
   m = 18
   DO i = 1 , 10
      m = m - 1
      est(m) = est(m-1)
   ENDDO
   est(5) = a
   est(6) = fj
   est(7) = c
   CALL rods
END SUBROUTINE tubes
