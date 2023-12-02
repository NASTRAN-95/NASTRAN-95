!*==cmmcon.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmmcon(Nce)
   IMPLICIT NONE
   USE C_CMB001
   USE C_CMB002
   USE C_CMB003
   USE C_ZZZZZZ
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
         nwd = 2 + Npsub
         Mcon = .TRUE.
         ifile = Scconn
         CALL open(*60,Scconn,Z(Buf1),0)
         j = 0
         Nce = 0
         spag_nextblock_1 = 2
      CASE (2)
         CALL read(*40,*20,Scconn,Z(Score+j),10,1,nnn)
 20      Nce = Nce + 1
         Z(Score+j) = Nce
         j = j + nwd
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 40      CALL close(Scconn,1)
!
!     SWEEP THROUGH CONNECTION ENTRIES AND DETERMINE THOSE THAT
!     REPRESENT MULTIPLE CONNECTIONS.
!
         Mcon = .FALSE.
         ncem1 = Nce - 1
!
         DO k = 1 , ncem1
            DO i = 1 , Npsub
               ist = Score + i + (k-1)*nwd + 1
               IF ( Z(ist)/=0 ) THEN
                  DO j = 1 , Nce
                     IF ( k/=j ) THEN
                        isub = Score + 1 + i + (j-1)*nwd
                        IF ( Z(ist)==Z(isub) ) THEN
                           iloc = i + 1
                           Z(ist-iloc) = -1*iabs(Z(ist-iloc))
                           Z(isub-iloc) = -1*iabs(Z(isub-iloc))
                           Mcon = .TRUE.
                        ENDIF
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ENDDO
!
         IF ( .NOT.Mcon ) RETURN
!
!     GENERATE OUTPUT FILE OF CONNECTION ENTRY IDS
!
         ifile = Scmcon
         CALL open(*60,Scmcon,Z(Buf1),1)
         DO i = 1 , Nce
            loc = Score + (i-1)*nwd
            IF ( Z(loc)<0 ) CALL write(Scmcon,iabs(Z(loc)),1,0)
         ENDDO
         CALL write(Scmcon,0,0,1)
         CALL close(Scmcon,1)
         RETURN
!
 60      CALL mesage(-1,ifile,aaa)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE cmmcon
