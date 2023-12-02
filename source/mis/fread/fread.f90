!*==fread.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fread(File,Block,N,Eor)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: File
   REAL , DIMENSION(1) :: Block
   INTEGER :: N
   INTEGER :: Eor
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: k , spag_nextblock_1
   REAL , DIMENSION(2) , SAVE :: subnam
   EXTERNAL mesage , read
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
   DATA subnam/4H FRE , 4HAD  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         CALL read(*20,*40,File,Block,N,Eor,k)
         RETURN
 20      k = -2
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 40      k = -3
         spag_nextblock_1 = 2
      CASE (2)
         DO
            CALL mesage(k,File,subnam)
         ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE fread
