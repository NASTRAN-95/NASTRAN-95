!*==invert.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE invert(Ia,Ib,Scr1)
   USE c_invtrx
   USE c_zzzzzz
   IMPLICIT NONE
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
   fa(1) = Ia
   CALL rdtrl(fa)
   fb(1) = Ia
   CALL rdtrl(fb)
   fb(1) = Ib
   scrfil = Scr1
   prec = fa(5)
   nx = korsz(z)
   CALL invtr(*100,z,z)
   CALL wrttrl(fb)
   RETURN
 100  DO
!
!     SINGULAR  MATRIX
!
      CALL mesage(-5,fa,name)
   ENDDO
END SUBROUTINE invert
