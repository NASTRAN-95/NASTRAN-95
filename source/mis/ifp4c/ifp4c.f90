!*==ifp4c.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifp4c(File,Scrt,Buf1,Buf2,Eof)
   USE c_names
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: File
   INTEGER :: Scrt
   INTEGER , DIMENSION(10) :: Buf1
   INTEGER , DIMENSION(10) :: Buf2
   LOGICAL :: Eof
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: eor , noeor
   INTEGER :: flag , i
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , DIMENSION(2) :: name2
   INTEGER , DIMENSION(7) :: trail
   INTEGER , DIMENSION(10) :: work
   EXTERNAL fname , mesage , open , rdtrl , read , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE, CALLED BY IFP4, OPENS THE 2 FILES AND COPIES THE
!     HEADER RECORD FROM -FILE- TO -SCRT-.
!
   DATA name/4HIFP4 , 4HC   / , eor , noeor/1 , 0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         trail(1) = File
         DO i = 2 , 7
            trail(i) = 0
         ENDDO
         CALL rdtrl(trail)
         DO i = 2 , 7
            IF ( trail(i)/=0 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
!
!     FILE IS NULL
!
         Eof = .TRUE.
         CALL open(*40,Scrt,Buf2,wrtrew)
         CALL fname(File,name2)
         CALL write(Scrt,name2,2,eor)
         RETURN
      CASE (2)
         CALL open(*80,File,Buf1,rdrew)
         Eof = .FALSE.
         CALL open(*40,Scrt,Buf2,wrtrew)
         DO
            CALL read(*60,*20,File,work,10,noeor,flag)
            CALL write(Scrt,work,10,noeor)
         ENDDO
 20      CALL write(Scrt,work,flag,eor)
         RETURN
!
 40      CALL mesage(-1,Scrt,name)
 60      CALL mesage(-2,File,name)
 80      CALL mesage(-1,File,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ifp4c
