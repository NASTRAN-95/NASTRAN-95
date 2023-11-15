
SUBROUTINE ifp1f(*,Iword,Ii)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Core(1) , Corex(1) , Corey(401) , Skip1(4) , Skip2(4) , Skip3(3)
   INTEGER Iben , Izzzbb , Ncpw4
   COMMON /ifp1a / Skip1 , Ncpw4 , Skip2 , Izzzbb , Skip3 , Iben
   COMMON /zzzzzz/ Corex
!
! Dummy argument declarations
!
   INTEGER Ii , Iword
!
! Local variable declarations
!
   INTEGER i , j , k , l
   INTEGER khrfn1
!
! End of declarations
!
!
!     FINDS FIRST 4 NON-BLANK CHARACTERS
!
   EQUIVALENCE (Corex(1),Corey(1)) , (Core(1),Corey(401))
!
   Iword = Izzzbb
   l = 1
   Ii = 0
   DO i = 1 , 18
      DO j = 1 , Ncpw4
         k = khrfn1(Izzzbb,1,Core(i),j)
         IF ( k/=Iben ) THEN
            IF ( Ii==0 ) Ii = i
            Iword = khrfn1(Iword,l,k,1)
            l = l + 1
            IF ( l>Ncpw4 ) GOTO 99999
         ENDIF
      ENDDO
   ENDDO
   RETURN 1
99999 END SUBROUTINE ifp1f
