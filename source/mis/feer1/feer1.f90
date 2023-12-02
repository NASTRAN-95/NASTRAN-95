!*==feer1.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE feer1
   USE c_feercx
   USE c_feerxx
   USE c_names
   USE c_saddx
   USE c_system
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , iprec
   EXTERNAL korsz , sadd , wrttrl
!
! End of declarations rewritten by SPAG
!
!
!     FEER1 INITIALIZES AND CALLS SUBROUTINE ADD FOR FEER
!
   !>>>>EQUIVALENCE (Ksystm(55),Iprec)
!
!     SET UP CALL TO ADD
!
   DO i = 1 , 7
      filea(i) = filem(i)
      fileb(i) = filek(i)
   ENDDO
   dalpha(1) = lambda
   dbeta(1) = 1.0D+0
   typalp = iprec
   typbta = iprec
   nz = korsz(z)
   filec(1) = scr1
   filec(2) = filek(2)
   filec(3) = filek(3)
   filec(4) = sqr
   filec(5) = iprec
   nomat = 2
   IF ( fileb(1)==0 ) nomat = 1
   CALL sadd(z,z)
   CALL wrttrl(filec)
END SUBROUTINE feer1
