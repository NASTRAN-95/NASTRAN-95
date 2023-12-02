!*==cinvp1.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cinvp1
   USE c_cinvpx
   USE c_cinvxx
   USE c_names
   USE c_saddx
   USE c_system
   USE c_zzzzzz
   USE iso_fortran_env
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
   nomat = 2
   DO i = 1 , 7
      ifila(i) = filem(i)
      ifilb(i) = fileb(i)
   ENDDO
   alpha(1) = -lambda(1)
   alpha(2) = -lambda(2)
   beta(1) = -1.D0
   beta(2) = 0.D0
   itypal = cdp
   itypbt = cdp
   nz = korsz(z)
   IF ( switch==-204 ) nz = nz - 2*sysbuf
   ifilc(1) = scr2
   IF ( switch/=0 ) ifilc(1) = scr11
   ifilc(2) = filek(2)
   ifilc(3) = filek(3)
   ifilc(4) = 1
   ifilc(5) = cdp
   CALL sadd(z,z)
!*******
!     FORM (LAMBDA**2*M+LAMBDA*B+K) ON SCR1
!*******
   DO i = 1 , 7
      ifila(i) = filek(i)
   ENDDO
   ifilb(1) = ifilc(1)
   ifilb(2) = filek(2)
   ifilb(3) = filek(3)
   ifilb(4) = sqr
   alpha(2) = 0.D0
   beta(1) = -lambda(1)
   beta(2) = -lambda(2)
   ifilb(5) = cdp
   alpha(1) = 1.D0
   ifilc(1) = scr1
   CALL sadd(z,z)
END SUBROUTINE cinvp1
