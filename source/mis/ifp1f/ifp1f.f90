!*==ifp1f.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifp1f(Iword,Ii) !HIDESTARS (*,Iword,Ii)
   USE c_ifp1a
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iword
   INTEGER :: Ii
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: core
   INTEGER :: i , j , k , l
   EXTERNAL khrfn1
!
! End of declarations rewritten by SPAG
!
!
!     FINDS FIRST 4 NON-BLANK CHARACTERS
!
   !>>>>EQUIVALENCE (Corex(1),Corey(1)) , (Core(1),Corey(401))
!
   Iword = izzzbb
   l = 1
   Ii = 0
   DO i = 1 , 18
      DO j = 1 , ncpw4
         k = khrfn1(izzzbb,1,core(i),j)
         IF ( k/=iben ) THEN
            IF ( Ii==0 ) Ii = i
            Iword = khrfn1(Iword,l,k,1)
            l = l + 1
            IF ( l>ncpw4 ) RETURN
         ENDIF
      ENDDO
   ENDDO
   RETURN 1
END SUBROUTINE ifp1f
