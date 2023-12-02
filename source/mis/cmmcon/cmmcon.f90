!*==cmmcon.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmmcon(Nce)
   USE c_cmb001
   USE c_cmb002
   USE c_cmb003
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nce
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: aaa
   INTEGER :: i , ifile , iloc , ist , isub , j , k , loc , ncem1 , nnn , nwd
   EXTERNAL close , mesage , open , read , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS SUBROUTINE DETERMINES WHETHER MORE THAN ONE CONNECTION ENTRY
!     HAS BEEN SPECIFIED FOR A GIVEN IP NUMBER.
!
   DATA aaa/4HCMMC , 4HON  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     READ CONNECTION ENTRIES INTO OPEN CORE
!
         nwd = 2 + npsub
         mcon = .TRUE.
         ifile = scconn
         CALL open(*60,scconn,z(buf1),0)
         j = 0
         Nce = 0
         spag_nextblock_1 = 2
      CASE (2)
         CALL read(*40,*20,scconn,z(score+j),10,1,nnn)
 20      Nce = Nce + 1
         z(score+j) = Nce
         j = j + nwd
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 40      CALL close(scconn,1)
!
!     SWEEP THROUGH CONNECTION ENTRIES AND DETERMINE THOSE THAT
!     REPRESENT MULTIPLE CONNECTIONS.
!
         mcon = .FALSE.
         ncem1 = Nce - 1
!
         DO k = 1 , ncem1
            DO i = 1 , npsub
               ist = score + i + (k-1)*nwd + 1
               IF ( z(ist)/=0 ) THEN
                  DO j = 1 , Nce
                     IF ( k/=j ) THEN
                        isub = score + 1 + i + (j-1)*nwd
                        IF ( z(ist)==z(isub) ) THEN
                           iloc = i + 1
                           z(ist-iloc) = -1*iabs(z(ist-iloc))
                           z(isub-iloc) = -1*iabs(z(isub-iloc))
                           mcon = .TRUE.
                        ENDIF
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ENDDO
!
         IF ( .NOT.mcon ) RETURN
!
!     GENERATE OUTPUT FILE OF CONNECTION ENTRY IDS
!
         ifile = scmcon
         CALL open(*60,scmcon,z(buf1),1)
         DO i = 1 , Nce
            loc = score + (i-1)*nwd
            IF ( z(loc)<0 ) CALL write(scmcon,iabs(z(loc)),1,0)
         ENDDO
         CALL write(scmcon,0,0,1)
         CALL close(scmcon,1)
         RETURN
!
 60      CALL mesage(-1,ifile,aaa)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE cmmcon
