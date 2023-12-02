!*==cfbsor.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cfbsor(Ll,Ul,Bx,Xx,Iopt)
   IMPLICIT NONE
   USE C_FBSX
   USE C_GFBSX
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ll
   INTEGER :: Ul
   INTEGER :: Bx
   INTEGER :: Xx
   INTEGER :: Iopt
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , nz
   INTEGER , DIMENSION(7) :: mcb
   EXTERNAL fbs , gfbs , korsz , rdtrl , wrttrl
!
! End of declarations rewritten by SPAG
!
!
!
   nz = korsz(Iz)
   mcb(1) = iabs(Bx)
   CALL rdtrl(mcb)
   IF ( Iopt==1 ) THEN
!
!     UNSYMETRIC FBS
!
      Jfl(1) = Ll
      CALL rdtrl(Jfl)
      Jfu(1) = Ul
      CALL rdtrl(Jfu)
      DO i = 1 , 7
         Jfb(i) = mcb(i)
         Jfx(i) = mcb(i)
      ENDDO
      Jfx(1) = Xx
      Jx = nz
      Jprec = Iprec
      Jfx(5) = max0(Jfl(5),Jfb(5))
      Jsign = +1
      IF ( Bx<0 ) Jsign = -1
      CALL gfbs(Iz,Iz)
      CALL wrttrl(Jfx)
   ELSE
!
!     SYMETRIC FBS
!
      Mfl(1) = Ll
      CALL rdtrl(Mfl)
      DO i = 1 , 7
         Mfb(i) = mcb(i)
         Mfx(i) = mcb(i)
      ENDDO
      Mfx(1) = Xx
      Mx = nz
      Mfx(5) = max0(Mfl(5),Mfb(5))
      Mprec = Iprec
      Msign = +1
      IF ( Bx<0 ) Msign = -1
      CALL fbs(Iz,Iz)
      CALL wrttrl(Mfx)
   ENDIF
END SUBROUTINE cfbsor
