!*==format.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE format(A,N1x,N2x,N3x,L1x,L2x)
   IMPLICIT NONE
   USE C_SYSTEM
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(6) :: A
   INTEGER :: N1x
   INTEGER :: N2x
   INTEGER :: N3x
   INTEGER :: L1x
   INTEGER :: L2x
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(6,6,2) :: f
   REAL , DIMENSION(6) , SAVE :: f11 , f12 , f21 , f22 , f31 , f32 , f41 , f42 , f51 , f52 , f61 , f62
   REAL , DIMENSION(6) :: ff
   INTEGER :: i , l , l1 , l2 , n , n1 , n2 , n3
!
! End of declarations rewritten by SPAG
!
!
!  $MIXED_FORMATS
!
   !>>>>EQUIVALENCE (f11(1),f(1,1,1)) , (f21(1),f(1,2,1)) , (f31(1),f(1,3,1)) , (f41(1),f(1,4,1)) , (f51(1),f(1,5,1)) , (f61(1),f(1,6,1))&
!>>>>    & , (f12(1),f(1,1,2)) , (f22(1),f(1,2,2)) , (f32(1),f(1,3,2)) , (f42(1),f(1,4,2)) , (f52(1),f(1,5,2)) , (f62(1),f(1,6,2))
   DATA f11/4H(I5, , 4H49X, , 4H1P,1 , 4HE19. , 4H6,I0 , 4H58) / , f21/4H(I5, , 4H40X, , 4H1P,2 , 4HE19. , 4H6,I0 , 4H48) / ,       &
       &f31/4H(I5, , 4H30X, , 4H1P,0 , 4HE19. , 4H6,I0 , 4H39) / , f41/4H(I5, , 4H21X, , 4H1P,4 , 4HE19. , 4H6,I0 , 4H29) / ,       &
       &f51/4H(I5, , 4H11X, , 4H1P,5 , 4HE19. , 4H6,I0 , 4H20) / , f61/4H(I5, , 4H02X, , 4H1P,6 , 4HE19. , 4H6,I0 , 4H10) /
   DATA f12/4H(I5, , 4H02X, , 4H1P,1 , 4HE19. , 4H6,I1 , 4H05) / , f22/4H(I5, , 4H02X, , 4H1P,2 , 4HE19. , 4H6,I0 , 4H86) / ,       &
       &f32/4H(I5, , 4H02X, , 4H1P,3 , 4HE19. , 4H6,I0 , 4H67) / , f42/4H(I5, , 4H02X, , 4H1P,4 , 4HE19. , 4H6,I0 , 4H48) / ,       &
       &f52/4H(I5, , 4H02X, , 4H1P,5 , 4HE19. , 4H6,I0 , 4H29) / , f62/4H(I5, , 4H02X, , 4H1P,6 , 4HE19. , 4H6,I0 , 4H10) /
!
   n1 = N1x
   n2 = N2x
   n3 = N3x
   l1 = L1x
   l2 = L2x
   n = (n2-n1+n3)/n3
   IF ( n>0 ) THEN
      IF ( n>6 ) n = 6
      l = 2
      IF ( l1<=0 .OR. l2<=0 ) l = 1
      DO i = 1 , 6
         ff(i) = f(i,n,l)
      ENDDO
      l1 = iabs(l1)
      l2 = iabs(l2)
      WRITE (Mo,ff,ERR=99999) l1 , (A(i),i=n1,n2,n3) , l2
   ENDIF
!
99999 END SUBROUTINE format
