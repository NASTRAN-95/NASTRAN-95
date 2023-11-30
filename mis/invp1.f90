
SUBROUTINE invp1
   IMPLICIT NONE
   DOUBLE PRECISION Dalpha(2) , Dbeta(2) , Lambda
   REAL Dum(36) , Z(1)
   INTEGER Filea(7) , Fileb(7) , Filec(7) , Filek(7) , Filem(7) , Ij(8) , Ik(2) , Iprec , Ksystm(56) , Nomat , Nz , Rdp , Scr1 ,    &
         & Sqr , Typalp , Typbta
   COMMON /invpwx/ Filek , Filem , Scr1
   COMMON /invpxx/ Lambda
   COMMON /names / Ij , Rdp , Ik , Sqr
   COMMON /saddx / Nomat , Nz , Filea , Typalp , Dalpha , Fileb , Typbta , Dbeta , Dum , Filec
   COMMON /system/ Ksystm
   COMMON /zzzzzz/ Z
   INTEGER i
   INTEGER korsz
!
!     INVP1 INITIALIZES AND CALLS SUBROUTINE ADD FOR INVPWR
!
   !>>>>EQUIVALENCE (Ksystm(55),Iprec)
!
!     SET UP CALL TO ADD
!
   DO i = 1 , 7
      Filea(i) = Filem(i)
      Fileb(i) = Filek(i)
   ENDDO
   Dalpha(1) = -Lambda
   Dbeta(1) = 1.0D0
   Typalp = Iprec
   Typbta = Iprec
   Nz = korsz(Z)
   Filec(1) = Scr1
   Filec(2) = Filek(2)
   Filec(3) = Filek(3)
   Filec(4) = Sqr
   Filec(5) = Iprec
   Nomat = 2
   IF ( Fileb(1)==0 ) Nomat = 1
   CALL sadd(Z,Z)
   CALL wrttrl(Filec)
END SUBROUTINE invp1