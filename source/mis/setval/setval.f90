!*==setval.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE setval
   IMPLICIT NONE
   USE C_BLANK
   USE C_OSCENT
   USE C_SYSTEM
   USE C_XVPS
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , j , k , nbpw
   INTEGER , DIMENSION(2) , SAVE :: subnam
   EXTERNAL andf , mesage , rshift
!
! End of declarations rewritten by SPAG
!
!
   !>>>>EQUIVALENCE (Ksystm(40),Nbpw)
   DATA subnam/4HSETV , 4HAL  /
!
   j = 12
   DO i = 1 , 5
!
!     CHECK ODD PARAMETERS TO FIND VARIABLE ONES
!
      IF ( andf(rshift(Oscar(j+1),nbpw-1),1)==0 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
!
!     PARAMETER IS VARIABLE
!
      k = andf(Oscar(j+1),65535)
      P(1,i) = P(2,i)
      Vps(k) = P(1,i)
      j = j + 2
      IF ( andf(rshift(Oscar(j),nbpw-1),1)==0 ) j = j + 1
   ENDDO
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
!
      IF ( i<=1 ) CALL mesage(-7,0,subnam)
   END SUBROUTINE spag_block_1
!
END SUBROUTINE setval
