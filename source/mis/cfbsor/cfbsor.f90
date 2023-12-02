!*==cfbsor.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cfbsor(Ll,Ul,Bx,Xx,Iopt)
   USE c_fbsx
   USE c_gfbsx
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
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
   nz = korsz(iz)
   mcb(1) = iabs(Bx)
   CALL rdtrl(mcb)
   IF ( Iopt==1 ) THEN
!
!     UNSYMETRIC FBS
!
      jfl(1) = Ll
      CALL rdtrl(jfl)
      jfu(1) = Ul
      CALL rdtrl(jfu)
      DO i = 1 , 7
         jfb(i) = mcb(i)
         jfx(i) = mcb(i)
      ENDDO
      jfx(1) = Xx
      jx = nz
      jprec = iprec
      jfx(5) = max0(jfl(5),jfb(5))
      jsign = +1
      IF ( Bx<0 ) jsign = -1
      CALL gfbs(iz,iz)
      CALL wrttrl(jfx)
   ELSE
!
!     SYMETRIC FBS
!
      mfl(1) = Ll
      CALL rdtrl(mfl)
      DO i = 1 , 7
         mfb(i) = mcb(i)
         mfx(i) = mcb(i)
      ENDDO
      mfx(1) = Xx
      mx = nz
      mfx(5) = max0(mfl(5),mfb(5))
      mprec = iprec
      msign = +1
      IF ( Bx<0 ) msign = -1
      CALL fbs(iz,iz)
      CALL wrttrl(mfx)
   ENDIF
END SUBROUTINE cfbsor
