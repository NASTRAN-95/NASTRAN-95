!*==cmrels.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmrels
   IMPLICIT NONE
   USE C_CMB001
   USE C_CMB002
   USE C_CMB003
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: aaa
   LOGICAL :: first
   INTEGER :: i , icode , id , ifile , ii , imsg , ist , ist1 , ist2 , iw , j , kid , kj , n , nc , nce , nnn , nw , nw1 , nw2 ,    &
            & nwrd , ps1 , ps2 , stce
   INTEGER , DIMENSION(7,3) :: ix
   INTEGER , DIMENSION(32) :: list
   EXTERNAL andf , bisloc , close , decode , eof , fwdrec , mesage , open , read , skpfil , sort , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     THIS SUBROUTINE ENFORCES THE RELES DATA SPECIFIED FOR THE
!     COMB1 MODULE.
!
   DATA aaa/4HCMRE , 4HLS  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         ifile = Scbdat
         kj = 0
         DO i = 1 , 7
            DO j = 1 , 3
               ix(i,j) = 0
            ENDDO
         ENDDO
         DO i = 1 , Npsub
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  first = .TRUE.
                  CALL open(*80,Scbdat,Z(Buf1),0)
                  CALL skpfil(Scbdat,3)
                  spag_nextblock_2 = 2
               CASE (2)
                  DO
                     CALL read(*4,*120,Scbdat,id,1,0,n)
                     IF ( id==i ) THEN
                        CALL read(*100,*2,Scbdat,Z(Score+kj),Lcore,1,nw)
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ELSE
                        CALL fwdrec(*4,Scbdat)
                     ENDIF
                  ENDDO
 2                IF ( first ) ix(i,2) = Score + kj
                  first = .FALSE.
                  ix(i,3) = ix(i,3) + nw/2
                  kj = kj + nw
                  Lcore = Lcore - nw
                  ix(i,1) = 1
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
 4                CALL close(Scbdat,1)
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         DO i = 1 , Npsub
            IF ( ix(i,1)/=0 ) THEN
               ist = ix(i,2)
               nw = ix(i,3)*2
               CALL sort(0,0,2,1,Z(ist),nw)
            ENDIF
         ENDDO
         ifile = Scconn
         CALL open(*80,Scconn,Z(Buf2),0)
         nwrd = 2 + Npsub
         nce = 0
         stce = Score + kj
         spag_nextblock_1 = 2
      CASE (2)
         CALL read(*40,*20,Scconn,Z(Score+kj),Lcore,1,nnn)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 20      kj = kj + nwrd
         nce = nce + 1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 40      CALL close(Scconn,1)
         nce = nwrd*nce
         DO i = 1 , nce , nwrd
            ii = i - 1
            icode = Z(stce+ii+1)
            CALL decode(icode,list,nc)
            IF ( nc/=2 ) CYCLE
            ps1 = list(1) + 1
            ps2 = list(2) + 1
            ist1 = ix(ps1,2)
            ist2 = ix(ps2,2)
            nw1 = ix(ps1,3)
            nw2 = ix(ps2,3)
            IF ( ix(ps1,1)/=0 ) THEN
               kid = Z(stce+ii+1+ps1)
               CALL bisloc(*50,kid,Z(ist1),2,nw1,iw)
               Z(stce+ii) = Z(stce+ii) - andf(Z(stce+ii),Z(ist1+iw))
            ENDIF
 50         IF ( ix(ps2,1)/=0 ) THEN
               kid = Z(stce+ii+1+ps2)
               CALL bisloc(*60,kid,Z(ist2),2,nw2,iw)
               Z(stce+ii) = Z(stce+ii) - andf(Z(stce+ii),Z(ist2+iw))
            ENDIF
 60      ENDDO
         CALL open(*80,Scconn,Z(Buf1),1)
         DO i = 1 , nce , nwrd
            ii = i - 1
            IF ( Z(stce+ii)/=0 ) CALL write(Scconn,Z(stce+ii),nwrd,1)
         ENDDO
         CALL eof(Scconn)
         CALL close(Scconn,1)
         RETURN
!
 80      imsg = -1
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 100     imsg = -2
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 120     imsg = -3
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (3)
         imsg = -8
         spag_nextblock_1 = 4
      CASE (4)
         CALL mesage(imsg,ifile,aaa)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE cmrels
