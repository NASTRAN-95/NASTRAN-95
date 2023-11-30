
SUBROUTINE eadd(P,Prec)
   IMPLICIT NONE
   DOUBLE PRECISION Alpha(2) , Beta(2)
   REAL Core(1) , Xx
   INTEGER Ia(7) , Ialp , Ib(7) , Ibeta , Ibuck , Ic(7) , Iev(7) , Ik(7) , Im(7) , Ka(5) , Lc , Mcbs(67) , Nn(13) , Nomat , Nz
   COMMON /blank / Xx
   COMMON /regean/ Im , Ik , Iev , Ka , Lc , Nn , Ibuck
   COMMON /saddx / Nomat , Nz , Mcbs
   COMMON /zzzzzz/ Core
   INTEGER Prec
   DOUBLE PRECISION P(1)
   INTEGER i , kprec
   INTEGER korsz
!
   EQUIVALENCE (Mcbs(1),Ia(1)) , (Mcbs(8),Ialp) , (Mcbs(9),Alpha(1)) , (Mcbs(13),Ib(1)) , (Mcbs(20),Ibeta) , (Mcbs(21),Beta(1)) ,   &
    & (Mcbs(61),Ic(1))
!
   Nz = (korsz(Core)/2)*2 - Lc
   DO i = 1 , 7
      Ia(i) = Im(i)
      Ib(i) = Ik(i)
      Ic(i) = Ik(i)
   ENDDO
   Ic(1) = Ka(1)
   kprec = Ik(5)
   IF ( Prec>=1 .AND. Prec<=4 ) kprec = Prec
   Ialp = kprec
   Alpha(1) = P(1)
   Ibeta = kprec
   Beta(1) = 1.0D0
   Nomat = 2
   CALL sadd(Core,Core)
   CALL wrttrl(Ic)
END SUBROUTINE eadd
