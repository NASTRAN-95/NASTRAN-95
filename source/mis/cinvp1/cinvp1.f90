!*==cinvp1.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cinvp1
USE C_CINVPX
USE C_CINVXX
USE C_NAMES
USE C_SADDX
USE C_SYSTEM
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(2) :: alpha , beta
   INTEGER :: i , itypal , itypbt
   INTEGER , DIMENSION(7) :: ifila , ifilb , ifilc
   EXTERNAL korsz , sadd
!
! End of declarations rewritten by SPAG
!
!*******
!     CINVP1 INITIALIZES AND CALLS SUBROUTINE ADD FOR CINVPR
!*******
!
!
   !>>>>EQUIVALENCE (Mcbs(1),Ifila(1)) , (Mcbs(8),Itypal) , (Mcbs(61),Ifilc(1)) , (Mcbs(13),Ifilb(1)) , (Mcbs(20),Itypbt) ,              &
!>>>>    & (Mcbs(21),Beta(1)) , (Mcbs(9),Alpha(1))
!*******
!     FORM -(B+LAMBDA*M) ON SCR2
!*******
   Nomat = 2
   DO i = 1 , 7
      ifila(i) = Filem(i)
      ifilb(i) = Fileb(i)
   ENDDO
   alpha(1) = -Lambda(1)
   alpha(2) = -Lambda(2)
   beta(1) = -1.D0
   beta(2) = 0.D0
   itypal = Cdp
   itypbt = Cdp
   Nz = korsz(Z)
   IF ( Switch==-204 ) Nz = Nz - 2*Sysbuf
   ifilc(1) = Scr2
   IF ( Switch/=0 ) ifilc(1) = Scr11
   ifilc(2) = Filek(2)
   ifilc(3) = Filek(3)
   ifilc(4) = 1
   ifilc(5) = Cdp
   CALL sadd(Z,Z)
!*******
!     FORM (LAMBDA**2*M+LAMBDA*B+K) ON SCR1
!*******
   DO i = 1 , 7
      ifila(i) = Filek(i)
   ENDDO
   ifilb(1) = ifilc(1)
   ifilb(2) = Filek(2)
   ifilb(3) = Filek(3)
   ifilb(4) = Sqr
   alpha(2) = 0.D0
   beta(1) = -Lambda(1)
   beta(2) = -Lambda(2)
   ifilb(5) = Cdp
   alpha(1) = 1.D0
   ifilc(1) = Scr1
   CALL sadd(Z,Z)
END SUBROUTINE cinvp1
