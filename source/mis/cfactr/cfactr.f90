!*==cfactr.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cfactr(A,Ll,Ul,Scr1,Scr2,Scr3,Iopt)
USE C_CDCMPX
USE C_SDCCSP
USE C_SFACT
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
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
   nz = korsz(Iz)
   mcb(1) = A
   CALL rdtrl(mcb)
   IF ( mcb(4)/=6 ) THEN
!
!     UNSYMMETRIC  COMPLEX
!
      DO i = 1 , 7
         Fa(i) = mcb(i)
         Fl(i) = mcb(i)
         Fu(i) = mcb(i)
      ENDDO
      Fl(1) = Ll
      Fu(1) = Ul
      Fl(4) = 4
      Fu(4) = 5
      Sr1 = Scr1
      Sr2 = Scr2
      Sr3 = Scr3
      Nx = nz
!     IB    = 0
!
!     IF IB IS SET TO ZERO HERE, T08021 PRINTS 27 MORE MESSAGES 3027
!     AND 3028 FROM GENVEC WHICH IS CALLED BY CFACTR, WHCIH IS CALLED BY
!     FRD2C, IN FRRD2 MODULE
!
!IBMI 6/93
      Ibbar = 0
      CALL cdcomp(*100,Iz,Iz,Iz)
      CALL wrttrl(Fu)
      CALL wrttrl(Fl)
      Iopt = 1
   ELSE
!
!     SYMMETRIC  COMPLEX
!
      DO i = 1 , 7
         Mfa(i) = mcb(i)
         Mfl(i) = mcb(i)
         Mfc(i) = mcb(i)
      ENDDO
      Mfl(1) = Ll
      Mfc(1) = Ul
      Mfl(4) = 4
      Mfc(4) = 5
      M1fil = Scr1
      M2fil = Scr2
      Mxx = nz
      M3fil = Scr3
      Ichol = 0
      CALL sdcomp(*100,Iz,Iz,Iz)
      CALL wrttrl(Mfl)
      Iopt = 2
   ENDIF
   RETURN
!
!     ERRORS
!
 100  CALL mesage(-5,A,name)
!
END SUBROUTINE cfactr
