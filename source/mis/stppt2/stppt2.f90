!*==stppt2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE stppt2(Input,W1jk,W2jk)
   USE c_amgp2
   USE c_packx
   IMPLICIT NONE
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
      nn = ii
      CALL pack(one,W1jk,tw1jk)
      CALL pack(zero,W2jk,tw2jk)
      ii = ii + 1
   ENDDO
END SUBROUTINE stppt2
