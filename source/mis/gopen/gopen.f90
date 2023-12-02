!*==gopen.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gopen(File,Buffer,Option)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: File
   REAL , DIMENSION(1) :: Buffer
   INTEGER :: Option
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: err
   REAL , DIMENSION(2) :: header
   INTEGER , SAVE :: inpnor , outnor , outrew
   REAL , DIMENSION(2) , SAVE :: subnam
   EXTERNAL fname , mesage , open , read , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
   DATA subnam/4H GOP , 4HEN  /
   DATA outrew , inpnor , outnor/1 , 2 , 3/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         CALL open(*20,File,Buffer,Option)
         IF ( Option/=inpnor .AND. Option/=outnor ) THEN
            IF ( Option==outrew ) THEN
               CALL fname(File,header)
               CALL write(File,header,2,1)
            ELSE
               CALL read(*40,*60,File,header,2,1,err)
            ENDIF
         ENDIF
         RETURN
!
 20      err = -1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 40      err = -2
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 60      err = -3
         spag_nextblock_1 = 2
      CASE (2)
         CALL mesage(err,File,subnam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE gopen
