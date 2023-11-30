
SUBROUTINE intlst(List,N,Sign,N1,N2)
   IMPLICIT NONE
   INTEGER N , N1 , N2 , Sign
   INTEGER List(1)
   INTEGER i , thru , to
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
