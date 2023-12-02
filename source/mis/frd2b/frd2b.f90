!*==frd2b.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE frd2b(A,Alp,B,Bet,C,Gam,D,Del,E,Eps,Out)
   USE c_frd2bc
   USE c_saddx
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
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
   nc = korsz(z)
   nomat = 5
   lcore = nc
   typa = 3
   typb = 3
   typc = 3
   typd = 3
   type = 3
   alpha(1) = Alp(1)
   alpha(2) = Alp(2)
   beta(1) = Bet(1)
   beta(2) = Bet(2)
   gama(1) = Gam(1)
   gama(2) = Gam(2)
   delta(1) = Del(1)
   delta(2) = Del(2)
   epsln(1) = Eps(1)
   epsln(2) = Eps(2)
   mcba(1) = A
   mcbb(1) = B
   mcbc(1) = C
   mcbd(1) = D
   mcbe(1) = E
   CALL rdtrl(mcba)
   CALL rdtrl(mcbb)
   CALL rdtrl(mcbc)
   CALL rdtrl(mcbd)
   CALL rdtrl(mcbe)
   ifo = 6
   ity = 3
   IF ( ih==0 .AND. iprec==2 ) ity = 4
!
!     IH IN /FRD2BC/ IS INITIALIZED BY ROUTINE FRRD2.
!     (COMPLEX D.P. ARITHMETIC IS USED IF IH = 0)
!
   n = 0
   DO i = 1 , 49 , 12
      IF ( mcba(i)<0 ) mcba(i) = 0
      IF ( mcba(i+1)==0 ) mcba(i) = 0
      IF ( mcba(i)/=0 ) THEN
         IF ( n==0 ) n = mcba(i+1)
         irow = mcba(i+2)
         IF ( mcba(i+3)/=6 ) ifo = 1
      ENDIF
   ENDDO
   CALL makmcb(mc,Out,irow,ifo,ity)
   mc(2) = n
   CALL sadd(z,z)
   CALL wrttrl(mc)
   CALL dmpfil(-Out,z,nc)
END SUBROUTINE frd2b
