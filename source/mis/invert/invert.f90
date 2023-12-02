!*==invert.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE invert(Ia,Ib,Scr1)
   IMPLICIT NONE
   USE C_INVTRX
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ia
   INTEGER :: Ib
   INTEGER :: Scr1
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL invtr , korsz , mesage , rdtrl , wrttrl
!
! End of declarations rewritten by SPAG
!
!
!     DRIVER  FOR  INVTR
!
!     INVERTS  LOWER OR UPPER TRIANGLE  IA  ONTO  IB
!
!     SCR1 WILL BE USED  ONLY IF  IA  IS AN  UPPER  TRIANGLE
!
!
!
   DATA name/4HINVE , 4HRT  /
!
!     FILL  MATRIX  CONTROL  BLOCKS  FOR  A  AND  B
!
   Fa(1) = Ia
   CALL rdtrl(Fa)
   Fb(1) = Ia
   CALL rdtrl(Fb)
   Fb(1) = Ib
   Scrfil = Scr1
   Prec = Fa(5)
   Nx = korsz(Z)
   CALL invtr(*100,Z,Z)
   CALL wrttrl(Fb)
   RETURN
 100  DO
!
!     SINGULAR  MATRIX
!
      CALL mesage(-5,Fa,name)
   ENDDO
END SUBROUTINE invert
