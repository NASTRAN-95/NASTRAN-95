!*==frd2b.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE frd2b(A,Alp,B,Bet,C,Gam,D,Del,E,Eps,Out)
   IMPLICIT NONE
   USE C_FRD2BC
   USE C_SADDX
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: A
   REAL , DIMENSION(2) :: Alp
   INTEGER :: B
   REAL , DIMENSION(2) :: Bet
   INTEGER :: C
   REAL , DIMENSION(2) :: Gam
   INTEGER :: D
   REAL , DIMENSION(2) :: Del
   INTEGER :: E
   REAL , DIMENSION(2) :: Eps
   INTEGER :: Out
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ifo , irow , ity , n , nc
   EXTERNAL dmpfil , korsz , makmcb , rdtrl , sadd , wrttrl
!
! End of declarations rewritten by SPAG
!
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
