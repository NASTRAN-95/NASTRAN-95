
SUBROUTINE format(A,N1x,N2x,N3x,L1x,L2x)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Mo
   REAL Skip
   COMMON /system/ Skip , Mo
!
! Dummy argument declarations
!
   INTEGER L1x , L2x , N1x , N2x , N3x
   REAL A(6)
!
! Local variable declarations
!
   REAL f(6,6,2) , f11(6) , f12(6) , f21(6) , f22(6) , f31(6) , f32(6) , f41(6) , f42(6) , f51(6) , f52(6) , f61(6) , f62(6) , ff(6)
   INTEGER i , l , l1 , l2 , n , n1 , n2 , n3
!
! End of declarations
!
!
!  $MIXED_FORMATS
!
   EQUIVALENCE (f11(1),f(1,1,1)) , (f21(1),f(1,2,1)) , (f31(1),f(1,3,1)) , (f41(1),f(1,4,1)) , (f51(1),f(1,5,1)) , (f61(1),f(1,6,1))&
    & , (f12(1),f(1,1,2)) , (f22(1),f(1,2,2)) , (f32(1),f(1,3,2)) , (f42(1),f(1,4,2)) , (f52(1),f(1,5,2)) , (f62(1),f(1,6,2))
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
99999 RETURN
END SUBROUTINE format
