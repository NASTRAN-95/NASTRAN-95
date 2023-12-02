!*==cfactr.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cfactr(A,Ll,Ul,Scr1,Scr2,Scr3,Iopt)
   USE c_cdcmpx
   USE c_sdccsp
   USE c_sfact
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: A
   INTEGER :: Ll
   INTEGER :: Ul
   INTEGER :: Scr1
   INTEGER :: Scr2
   INTEGER :: Scr3
   INTEGER :: Iopt
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , nz
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL cdcomp , korsz , mesage , rdtrl , sdcomp , wrttrl
!
! End of declarations rewritten by SPAG
!
!
   DATA name/4HCFAC , 4HTR  /
!
!
   nz = korsz(iz)
   mcb(1) = A
   CALL rdtrl(mcb)
   IF ( mcb(4)/=6 ) THEN
!
!     UNSYMMETRIC  COMPLEX
!
      DO i = 1 , 7
         fa(i) = mcb(i)
         fl(i) = mcb(i)
         fu(i) = mcb(i)
      ENDDO
      fl(1) = Ll
      fu(1) = Ul
      fl(4) = 4
      fu(4) = 5
      sr1 = Scr1
      sr2 = Scr2
      sr3 = Scr3
      nx = nz
!     IB    = 0
!
!     IF IB IS SET TO ZERO HERE, T08021 PRINTS 27 MORE MESSAGES 3027
!     AND 3028 FROM GENVEC WHICH IS CALLED BY CFACTR, WHCIH IS CALLED BY
!     FRD2C, IN FRRD2 MODULE
!
!IBMI 6/93
      ibbar = 0
      CALL cdcomp(*100,iz,iz,iz)
      CALL wrttrl(fu)
      CALL wrttrl(fl)
      Iopt = 1
   ELSE
!
!     SYMMETRIC  COMPLEX
!
      DO i = 1 , 7
         mfa(i) = mcb(i)
         mfl(i) = mcb(i)
         mfc(i) = mcb(i)
      ENDDO
      mfl(1) = Ll
      mfc(1) = Ul
      mfl(4) = 4
      mfc(4) = 5
      m1fil = Scr1
      m2fil = Scr2
      mxx = nz
      m3fil = Scr3
      ichol = 0
      CALL sdcomp(*100,iz,iz,iz)
      CALL wrttrl(mfl)
      Iopt = 2
   ENDIF
   RETURN
!
!     ERRORS
!
 100  CALL mesage(-5,A,name)
!
END SUBROUTINE cfactr
