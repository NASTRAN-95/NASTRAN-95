
SUBROUTINE frd2b(A,Alp,B,Bet,C,Gam,D,Del,E,Eps,Out)
   IMPLICIT NONE
   REAL Alpha(4) , Beta(4) , Delta(4) , Epsln(4) , Gama(4) , Z(1)
   INTEGER Ih , Iprec , Ksystm(54) , Lcore , Mc(7) , Mcba(7) , Mcbb(7) , Mcbc(7) , Mcbd(7) , Mcbe(7) , Nomat , Typa , Typb , Typc , &
         & Typd , Type
   COMMON /frd2bc/ Ih
   COMMON /saddx / Nomat , Lcore , Mcba , Typa , Alpha , Mcbb , Typb , Beta , Mcbc , Typc , Gama , Mcbd , Typd , Delta , Mcbe ,     &
                 & Type , Epsln , Mc
   COMMON /system/ Ksystm , Iprec
   COMMON /zzzzzz/ Z
   INTEGER A , B , C , D , E , Out
   REAL Alp(2) , Bet(2) , Del(2) , Eps(2) , Gam(2)
   INTEGER i , ifo , irow , ity , n , nc
   INTEGER korsz
!
!     ADD UP MATRICIES
!
!
   nc = korsz(Z)
   Nomat = 5
   Lcore = nc
   Typa = 3
   Typb = 3
   Typc = 3
   Typd = 3
   Type = 3
   Alpha(1) = Alp(1)
   Alpha(2) = Alp(2)
   Beta(1) = Bet(1)
   Beta(2) = Bet(2)
   Gama(1) = Gam(1)
   Gama(2) = Gam(2)
   Delta(1) = Del(1)
   Delta(2) = Del(2)
   Epsln(1) = Eps(1)
   Epsln(2) = Eps(2)
   Mcba(1) = A
   Mcbb(1) = B
   Mcbc(1) = C
   Mcbd(1) = D
   Mcbe(1) = E
   CALL rdtrl(Mcba)
   CALL rdtrl(Mcbb)
   CALL rdtrl(Mcbc)
   CALL rdtrl(Mcbd)
   CALL rdtrl(Mcbe)
   ifo = 6
   ity = 3
   IF ( Ih==0 .AND. Iprec==2 ) ity = 4
!
!     IH IN /FRD2BC/ IS INITIALIZED BY ROUTINE FRRD2.
!     (COMPLEX D.P. ARITHMETIC IS USED IF IH = 0)
!
   n = 0
   DO i = 1 , 49 , 12
      IF ( Mcba(i)<0 ) Mcba(i) = 0
      IF ( Mcba(i+1)==0 ) Mcba(i) = 0
      IF ( Mcba(i)/=0 ) THEN
         IF ( n==0 ) n = Mcba(i+1)
         irow = Mcba(i+2)
         IF ( Mcba(i+3)/=6 ) ifo = 1
      ENDIF
   ENDDO
   CALL makmcb(Mc,Out,irow,ifo,ity)
   Mc(2) = n
   CALL sadd(Z,Z)
   CALL wrttrl(Mc)
   CALL dmpfil(-Out,Z,nc)
END SUBROUTINE frd2b