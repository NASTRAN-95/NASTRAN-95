!*==saxb.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE saxb(A,B,C)
   IMPLICIT NONE
   REAL A(3) , B(3) , C(3)
   REAL d(3)
!*****
!  SINGLE-PRECISION VERSION
!
!  THIS ROUTINE PERFORMS A X B INTO C.  (C MAY OVERLAP A OR B IN CORE.)
!*****
   d(1) = A(2)*B(3) - A(3)*B(2)
   d(2) = A(3)*B(1) - A(1)*B(3)
   d(3) = A(1)*B(2) - A(2)*B(1)
   C(1) = d(1)
   C(2) = d(2)
   C(3) = d(3)
   RETURN
!
!*****
   ENTRY sapb(A,B,C)
!
!  THIS ROUTINE PERFORMS A + B INTO C.
!*****
   C(1) = A(1) + B(1)
   C(2) = A(2) + B(2)
   C(3) = A(3) + B(3)
   RETURN
!
!*****
   ENTRY samb(A,B,C)
!
!  THIS ROUTINE PERFORMS A - B INTO C.
!*****
   C(1) = A(1) - B(1)
   C(2) = A(2) - B(2)
   C(3) = A(3) - B(3)
END SUBROUTINE saxb
