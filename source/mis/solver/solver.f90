!*==solver.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE solver(Lower,X,B,In,Out,Eps,Ifl,Scr)
USE C_FBSX
USE C_MPYADX
USE C_SYSTEM
USE C_XMSSG
USE C_ZNTPKX
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Lower
   INTEGER :: X
   INTEGER :: B
   INTEGER :: In
   INTEGER :: Out
   REAL :: Eps
   INTEGER :: Ifl
   INTEGER :: Scr
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: denom , num
   INTEGER :: ioutpt , iprec , k , n1 , n2 , ncol , sysbuf
   INTEGER , DIMENSION(2) :: name
   EXTERNAL close , fbs , fname , gopen , intpk , korsz , makmcb , mpyad , rdtrl , wrttrl , zntpki
!
! End of declarations rewritten by SPAG
!
!
!    SOLVER PERFORMS THREE OPERATIONS--
!    1. SOLVES FOR B BY FORWARD-BACKWARD SUBSTITUTION
!    2. COMPUTES OUT = IN + B(T)*X
!    3. IF REQUESTED, COMPUTES EPSILON = NORM(OUT)/NORM(IN)
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(55),Iprec) , (Ksystm(2),Ioutpt)
!
!     INITIALIZE MATRIX CONTROL BLOCKS FOR FORWARD-BACKWARD SOLUTION
!
   Nz = korsz(Z)
   Filel(1) = Lower
   CALL rdtrl(Filel)
   Fileb(1) = B
   CALL rdtrl(Fileb)
   CALL makmcb(Filex,X,Fileb(3),Fileb(4),iprec)
   Prec = iprec
   Sign = -1
!
!     SOLVE A*X = -B FOR X WHERE A HAS BEEN FACTORED
!
   Scr1 = Scr
   CALL fbs(Z,Z)
   CALL wrttrl(Filex)
!
!     INITIALIZE MATRIX CONTROL BLOCKS FOR MPYAD OPERATION
!
   DO k = 1 , 7
      Filee(k) = Fileb(k)
      Filef(k) = Filex(k)
   ENDDO
   Fileg(1) = In
   CALL rdtrl(Fileg)
   CALL makmcb(Fileh,Out,Fileg(3),Fileg(4),iprec)
   Nzz = Nz
   T = 1
   Signab = 1
   Signc = 1
   Precx = iprec
   Scrtch = Scr
!
!     COMPUTE OUT = IN + B(T)*X
!
   CALL mpyad(Z,Z,Z)
   CALL wrttrl(Fileh)
!
!     IF REQUESTED,COMPUTE EPS = NORM(OUT) / NORM(IN)
!
   IF ( Ifl==0 ) RETURN
   n1 = Nz - sysbuf
   n2 = n1 - sysbuf
   CALL gopen(Out,Z(n1+1),0)
   CALL gopen(In,Z(n2+1),0)
   num = 0.0D0
   denom = 0.0D0
   ncol = Fileg(2)
   DO k = 1 , ncol
      CALL intpk(*50,Out,0,2,0)
      SPAG_Loop_2_1: DO
         CALL zntpki
         num = num + dabs(Ad(1))*dabs(Ad(1))
         IF ( Eol/=0 ) EXIT SPAG_Loop_2_1
      ENDDO SPAG_Loop_2_1
 50   CALL intpk(*100,In,0,2,0)
      SPAG_Loop_2_2: DO
         CALL zntpki
         denom = denom + dabs(Ad(1))*dabs(Ad(1))
         IF ( Eol/=0 ) EXIT SPAG_Loop_2_2
      ENDDO SPAG_Loop_2_2
 100  ENDDO
   IF ( denom==0.0D0 ) THEN
      CALL fname(In,name)
      WRITE (ioutpt,99001) Uwm , name
99001 FORMAT (A25,' 2401, ',2A4,' MATRIX IS NULL.  AN ARBITRARY VALUE ','OF 1.0 IS THEREFORE ASSIGNED TO',/5X,                      &
             &'THE RIGID BODY ERROR RATIO (EPSILON SUB E).')
      Eps = 1.0
   ELSE
      Eps = dsqrt(num/denom)
   ENDIF
   CALL close(In,1)
   CALL close(Out,1)
END SUBROUTINE solver
