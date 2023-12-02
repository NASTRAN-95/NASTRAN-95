!*==setval.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE setval
   USE c_blank
   USE c_oscent
   USE c_system
   USE c_xvps
   IMPLICIT NONE
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
      IF ( andf(rshift(oscar(j+1),nbpw-1),1)==0 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
!
!     PARAMETER IS VARIABLE
!
      k = andf(oscar(j+1),65535)
      p(1,i) = p(2,i)
      vps(k) = p(1,i)
      j = j + 2
      IF ( andf(rshift(oscar(j),nbpw-1),1)==0 ) j = j + 1
   ENDDO
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
!
      IF ( I<=1 ) CALL mesage(-7,0,Subnam)
   END SUBROUTINE spag_block_1
!
END SUBROUTINE setval
