
SUBROUTINE ssg2a(Pg,Pnbar,Pm,Pvact)
   IMPLICIT NONE
   INTEGER Core(6) , Ia1(7) , Ia11(7) , Ia12(7) , Ia21(7) , Ia22(7) , Icore(1) , Lcore , Lcr , N , No(4) , Rule
   COMMON /parmeg/ Ia1 , Ia11 , Ia12 , Ia21 , Ia22 , Lcr , Rule
   COMMON /patx  / Lcore , N , No
   COMMON /zzzzzz/ Icore
   INTEGER Pg , Pm , Pnbar , Pvact
   INTEGER i , pvect(7)
   INTEGER korsz
!
   EQUIVALENCE (Icore(1),Core(1))
!
!
   pvect(1) = Pvact
   CALL rdtrl(pvect)
   Ia1(1) = Pg
   CALL rdtrl(Ia1)
   Ia11(1) = Pnbar
   Ia12(1) = Pm
   DO i = 2 , 5
      Ia11(i) = Ia1(i)
      Ia12(i) = Ia1(i)
   ENDDO
   Ia11(3) = N
   Ia12(3) = No(1)
   Ia21(1) = 0
   Ia22(1) = 0
   Rule = 0
   Lcr = korsz(Core)
   Core(1) = 0
   Core(2) = 1
   Core(3) = Ia1(2)
   Core(4) = 2
   Core(5) = 1
   Core(6) = 0
   CALL partn(Core,pvect,Core)
   IF ( Ia11(1)/=0 ) CALL wrttrl(Ia11)
   IF ( Ia12(1)/=0 ) CALL wrttrl(Ia12)
END SUBROUTINE ssg2a
