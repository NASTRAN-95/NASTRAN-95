!*==stppt2.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE stppt2(Input,W1jk,W2jk)
   IMPLICIT NONE
   USE C_AMGP2
   USE C_PACKX
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Input
   INTEGER :: W1jk
   INTEGER :: W2jk
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , nj
   COMPLEX :: one , zero
   EXTERNAL fread , pack
!
! End of declarations rewritten by SPAG
!
   one = (1.0,0.0)
   zero = (0.0,0.0)
   CALL fread(Input,nj,1,1)
   DO i = 1 , nj
      Nn = Ii
      CALL pack(one,W1jk,Tw1jk)
      CALL pack(zero,W2jk,Tw2jk)
      Ii = Ii + 1
   ENDDO
END SUBROUTINE stppt2
