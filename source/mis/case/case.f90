!*==case.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE case
   IMPLICIT NONE
   USE C_BLANK
   USE C_NAMES
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: branch , buf1 , buf2 , buf3 , file , i , ifrqst , ilf , iload , ils , imrqst , iocnt , irset , iry , j , m8 , n ,     &
            & ncc , nogo
   INTEGER , DIMENSION(20) :: buf
   INTEGER , SAVE :: casecc , casexx , ib2pp , ifreq , ik2pp , im2pp , imeth , irand , itfl , nrigds , psdl
   INTEGER , DIMENSION(2) , SAVE :: error , nam
   INTEGER , DIMENSION(7) , SAVE :: mcb
   INTEGER , DIMENSION(40) , SAVE :: rfmts
   EXTERNAL close , fname , fwdrec , korsz , mesage , open , read , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     CASE READS THE CASE CONTROL DATA BLOCK AND WRITES A NEW
!     DATA BLOCK WHICH CONTAINS ONLY THOSE RECORDS WHICH DESCRIBE THE
!     CURRENT CASE IN THE LOOP. ADDITIONALLY, THE LOOP CONTROL PARAMETER
!     IS SET.
!
!
!
!     DATA DESCRIBING DATA BLOCK FILE NAMES AND POSITION
!     OF PARAMETERS IN THE CASE CONTROL RECORD.
!
   DATA casecc/101/ , casexx/201/ , ik2pp/139/ , im2pp/141/ , ib2pp/143/ , itfl/15/ , psdl/102/ , irand/163/
   DATA error/4HPSDL , 4HCASE/
   DATA ifreq/14/ , imeth/5/
!
!     DATA DEFINING RIGID FORMATS.
!
   DATA nrigds/10/ , rfmts/4HSTAT , 4HICS  , 4HREIG , 4HEN   , 4HDS0  , 4H     , 4HDS1  , 4H     , 4HFREQ , 4H     , 4HTRAN ,       &
      & 4HSNT  , 4HBKL0 , 4H     , 4HBKL1 , 4H     , 4HCEIG , 4HEN   , 4HPLA  , 4H     , 20*0/
!
!     MISC DATA
!
   DATA nam/4HCASE , 4H    / , mcb/7*0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     PERFORM BUFFER ALLOCATION.
!
         buf1 = korsz(Z) - Sysbuf + 1
         buf3 = buf1 - Sysbuf
         buf2 = buf3 - Sysbuf
         iry = 0
         m8 = -8
         IF ( Count<=0 ) Count = 1
         Loop = 1
         iocnt = Count
!
!     SET PARAMETER FOR APPROACH.
!
         n = 2*nrigds - 1
         DO i = 1 , n , 2
            IF ( rfmts(i)==App(1) ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         CALL mesage(30,75,App)
         i = 19
         spag_nextblock_1 = 2
      CASE (2)
         branch = (i+1)/2
!
!     OPEN CASECC. SKIP RECORDS ALREADY PROCESSED. OPEN CASEXX.
!     WRITE HEADER RECORD. THEN BRANCH ON APPROACH.
!
         file = casecc
         CALL open(*160,casecc,Z(buf1),Rdrew)
         DO i = 1 , Count
            CALL fwdrec(*180,casecc)
         ENDDO
         file = casexx
         CALL open(*160,casexx,Z(buf2),Wrtrew)
         CALL fname(casexx,buf)
         CALL write(casexx,buf,2,1)
         IF ( branch==1 .OR. branch==3 .OR. branch==4 .OR. branch==7 .OR. branch==8 .OR. branch==10 ) GOTO 140
         IF ( branch==6 ) THEN
!
!     TRANSIENT RESPONSE.
!
            CALL read(*180,*120,casecc,Z,buf2,1,ncc)
            CALL mesage(m8,0,nam)
            GOTO 120
         ELSE
!
!     COMPLEX EIGENVALUES OR FREQUENCY RESPONSE.
!
            CALL read(*180,*20,casecc,Z,buf2,1,ncc)
            CALL mesage(m8,0,nam)
         ENDIF
 20      buf(1) = Z(ik2pp)
         buf(2) = Z(ik2pp+1)
         buf(3) = Z(im2pp)
         buf(4) = Z(im2pp+1)
         buf(5) = Z(ib2pp)
         buf(6) = Z(ib2pp+1)
         buf(7) = Z(itfl)
         irset = Z(irand)
         ifrqst = Z(ifreq)
         imrqst = Z(imeth)
         IF ( branch==5 .AND. irset/=0 ) iry = 1
         IF ( iry==0 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     BUILD LIST OF UNIQUE LOAD ID-S
!
         file = psdl
         CALL open(*60,psdl,Z(buf3),Rdrew)
         CALL fwdrec(*100,psdl)
         ils = buf2
         ilf = buf2 - 1
         spag_nextblock_1 = 3
      CASE (3)
         SPAG_Loop_1_1: DO
            CALL read(*100,*40,psdl,Z(ncc+1),6,0,j)
            IF ( Z(ncc+1)==irset ) THEN
               j = 1
               iload = Z(ncc+2)
               IF ( ils/=ilf+1 ) EXIT SPAG_Loop_1_1
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 4
      CASE (4)
         DO i = ils , ilf
            IF ( Z(i)==iload ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 5
      CASE (5)
!
!     NEW LOAD ID
!
         ils = ils - 1
         Z(ils) = iload
         spag_nextblock_1 = 6
      CASE (6)
         IF ( j==0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         j = 0
         iload = Z(ncc+3)
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
!
!     END OF PSDL RECORD
!
 40      CALL close(psdl,Clsrew)
         IF ( ils==ilf+1 ) CALL mesage(-31,irset,error(1))
         buf2 = ils - 1
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
!
!     NO PSDL IS EQUIVALENT TO NO RANDOM
!
 60      iry = 0
         spag_nextblock_1 = 7
      CASE (7)
         CALL write(casexx,Z,ncc,1)
         Count = Count + 1
         IF ( iry/=0 ) THEN
!
!     CHECK  SUBCASE ID-S
!
            DO i = ils , ilf
               IF ( Z(1)==Z(i) ) THEN
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
      CASE (8)
!
!     MARK USED
!
         Z(i) = -Z(i)
         spag_nextblock_1 = 9
      CASE (9)
         CALL read(*100,*80,casecc,Z,buf2,1,ncc)
         CALL mesage(m8,0,nam)
 80      IF ( Z(ik2pp)/=buf(1) .OR. Z(ik2pp+1)/=buf(2) .OR. Z(im2pp)/=buf(3) .OR. Z(im2pp+1)/=buf(4) .OR. Z(ib2pp)/=buf(5) .OR.     &
            & Z(ib2pp+1)/=buf(6) ) GOTO 140
         IF ( Z(itfl)/=buf(7) ) GOTO 140
         IF ( Z(imeth)/=0 .AND. Z(imeth)/=imrqst ) GOTO 140
!
!     TEST FOR CHANGED FREQUENCY SET
!
         IF ( Z(ifreq)/=ifrqst .AND. branch==5 ) GOTO 140
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 100     Count = -1
         GOTO 140
 120     CALL write(casexx,Z,ncc,1)
         Count = Count + 1
         CALL read(*100,*140,casecc,Z,buf2,1,ncc)
!
!     CLOSE FILES. WRITE TRAILER. RETURN.
!
 140     CALL close(casecc,Clsrew)
         CALL close(casexx,Clsrew)
         mcb(1) = casexx
         mcb(2) = Count
         CALL wrttrl(mcb)
         IF ( Count<=1 .AND. iocnt==1 ) Loop = -1
!
!     CHECK ALL PSDL ACCOUNTED FOR
!
         IF ( iry/=0 ) THEN
            nogo = 0
            DO i = ils , ilf
               IF ( Z(i)>=0 ) THEN
                  nogo = -1
                  CALL mesage(33,Z(i),nam)
               ENDIF
            ENDDO
            IF ( nogo<0 ) CALL mesage(-7,0,nam)
         ENDIF
         RETURN
!
!     FATAL FILE ERRORS.
!
 160     n = -1
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 180     n = -2
         file = casecc
         spag_nextblock_1 = 10
      CASE (10)
         DO
            CALL mesage(n,file,nam)
         ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE case
