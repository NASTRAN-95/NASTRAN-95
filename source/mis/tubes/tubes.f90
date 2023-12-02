!*==tubes.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE tubes
   IMPLICIT NONE
   USE C_CONDAS
   USE C_EMGEST
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
   temp = Est(5) - Est(6)
   a = temp*Est(6)*Pi
   fj = .25*a*(temp**2+Est(6)**2)
   c = .5*Est(5)
   m = 18
   DO i = 1 , 10
      m = m - 1
      Est(m) = Est(m-1)
   ENDDO
   Est(5) = a
   Est(6) = fj
   Est(7) = c
   CALL rods
END SUBROUTINE tubes
