
SUBROUTINE solver(Lower,X,B,In,Out,Eps,Ifl,Scr)
   IMPLICIT NONE
   DOUBLE PRECISION Ad(2)
   INTEGER Eol , Eor , Fileb(7) , Filee(7) , Filef(7) , Fileg(7) , Fileh(7) , Filel(7) , Fileu(7) , Filex(7) , I , Ioutpt , Iprec , &
         & Ksystm(65) , Nz , Nzz , Prec , Precx , Scr1 , Scrtch , Sign , Signab , Signc , Sysbuf , T
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   REAL Z(1)
   COMMON /fbsx  / Filel , Fileu , Fileb , Filex , Nz , Prec , Sign , Scr1
   COMMON /mpyadx/ Filee , Filef , Fileg , Fileh , Nzz , T , Signab , Signc , Precx , Scrtch
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm
   COMMON /zntpkx/ Ad , I , Eol , Eor
   COMMON /zzzzzz/ Z
   INTEGER B , Ifl , In , Lower , Out , Scr , X
   REAL Eps
   DOUBLE PRECISION denom , num
   INTEGER k , n1 , n2 , name(2) , ncol
   INTEGER korsz
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
   CALL makmcb(Filex,X,Fileb(3),Fileb(4),Iprec)
   Prec = Iprec
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
   CALL makmcb(Fileh,Out,Fileg(3),Fileg(4),Iprec)
   Nzz = Nz
   T = 1
   Signab = 1
   Signc = 1
   Precx = Iprec
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
   n1 = Nz - Sysbuf
   n2 = n1 - Sysbuf
   CALL gopen(Out,Z(n1+1),0)
   CALL gopen(In,Z(n2+1),0)
   num = 0.0D0
   denom = 0.0D0
   ncol = Fileg(2)
   DO k = 1 , ncol
      CALL intpk(*50,Out,0,2,0)
      DO
         CALL zntpki
         num = num + dabs(Ad(1))*dabs(Ad(1))
         IF ( Eol/=0 ) EXIT
      ENDDO
 50   CALL intpk(*100,In,0,2,0)
      DO
         CALL zntpki
         denom = denom + dabs(Ad(1))*dabs(Ad(1))
         IF ( Eol/=0 ) EXIT
      ENDDO
 100  ENDDO
   IF ( denom==0.0D0 ) THEN
      CALL fname(In,name)
      WRITE (Ioutpt,99001) Uwm , name
99001 FORMAT (A25,' 2401, ',2A4,' MATRIX IS NULL.  AN ARBITRARY VALUE ','OF 1.0 IS THEREFORE ASSIGNED TO',/5X,                      &
             &'THE RIGID BODY ERROR RATIO (EPSILON SUB E).')
      Eps = 1.0
   ELSE
      Eps = dsqrt(num/denom)
   ENDIF
   CALL close(In,1)
   CALL close(Out,1)
END SUBROUTINE solver