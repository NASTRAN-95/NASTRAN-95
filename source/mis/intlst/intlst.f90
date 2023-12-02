!*==intlst.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE intlst(List,N,Sign,N1,N2)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: List
   INTEGER :: N
   INTEGER :: Sign
   INTEGER :: N1
   INTEGER :: N2
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i
   INTEGER , SAVE :: thru , to
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
   DATA to , thru/2HTO , 4HTHRU/
!
   Sign = isign(1,List(N))
   N1 = iabs(List(N))
   IF ( List(N+1)==to .OR. List(N+1)==thru ) THEN
!
      N2 = iabs(List(N+2))
      N = N + 3
      IF ( N1>N2 ) THEN
         i = N1
         N1 = N2
         N2 = i
      ENDIF
   ELSE
      N2 = N1
      N = N + 1
   ENDIF
!
END SUBROUTINE intlst
