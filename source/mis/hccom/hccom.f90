!*==hccom.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE hccom(Itype,Lcore,Icore,Nextz,Kcount)
   USE c_packx
   USE c_system
   USE c_zblpkx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Itype
   INTEGER :: Lcore
   INTEGER :: Icore
   INTEGER :: Nextz
   INTEGER :: Kcount
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL :: bldp , eor , incore
   REAL , DIMENSION(63) :: hc
   INTEGER , SAVE :: hccens , scr6
   INTEGER :: i , icount , in , inext , inword , jcount , k , nj , nskip , nskip1 , nwds , nwords
   INTEGER , DIMENSION(2) :: id
   INTEGER , DIMENSION(7) , SAVE :: mcbh
   INTEGER , DIMENSION(2) , SAVE :: nam
   EXTERNAL bldpk , bldpkn , close , fread , fwdrec , gopen , mesage , pack , read , rewind , wrttrl , zblpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
! COMBINES HC CENTROID INFO ON SCR6 TO HCCENS
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
   DATA mcbh/307 , 0 , 0 , 2 , 1 , 0 , 0/
   DATA scr6 , hccens/306 , 307/
   DATA nam/4HHCCO , 4HM   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         ita = 1
         itb = 1
         ii = 1
         incr = 1
         icount = 0
         nskip = 0
         nskip1 = 0
         in = 0
         eor = .FALSE.
         bldp = .FALSE.
         incore = .TRUE.
!
! IF TYPE IS 24 JUST PACK ZEROS ON HCCENS
!
         IF ( Itype==24 ) THEN
!
!
! PACK A COLUMN OF ZEROS CORRESPONDING TO REMFLUX
!
            mcbh(3) = Kcount
            CALL bldpk(1,1,hccens,0,0)
            CALL bldpkn(hccens,0,mcbh)
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
            CALL gopen(scr6,z(Lcore+1),0)
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
! SCR6 HAS 3 ENTRIES PER ELEMENT-ID,NUMBER OF POINTS AT WHICH HC IS
! COMPUTED=N, AND 3*N HC VALUES--THERE IS ONE RECORD PER CARD TYPE
! ON SCR6 FOR THIS SUBCASE
! IF  .NOT. INCORE, THEN WE ARE BACK HERE DUE TO SPILL LOGIC AND  ARE
! TRYING TO FINISH THE FIRST RECORD. SO WE MUST SKIP THE PART OF THE
! RECORD PREVIOUSLY READ.
!
         IF ( .NOT.incore ) CALL fread(scr6,id,-nskip,0)
         inword = 0
         DO
            CALL read(*80,*20,scr6,id,2,0,nwds)
            nskip = nskip + 2
            inext = Nextz + inword
            nwords = 3*id(2)
            IF ( inext+nwords>Icore ) THEN
!
! INFO WILL NOT FIT IN CORE - SPILL LOGIC
!
               incore = .FALSE.
               CALL fwdrec(*80,scr6)
!
! SKIP APPROPRIATE NUMBER OF WORDS IN THIS RECORD TO ACCOUNT FOR
! THE PORTION OF THIS RECORD PREVIOUSLY READ
!
               CALL read(*60,*100,scr6,id,-nskip1,0,nwds)
               GOTO 40
            ELSE
               CALL fread(scr6,z(inext),nwords,0)
!
! INWORD IS THE NUMBER OF WORDS READ INTO CORE ON THIS READ
! NSKIP IS THE TOTAL NUMBER OF WORDS READ FROM SCR6 FROM THIS RECORD
! ICOUNT IS THE TOTAL NUMBER OF WORDS SAVED IN CORE FROM THIS RECORD
!
               inword = inword + nwords
               nskip = nskip + nwords
               icount = icount + nwords
            ENDIF
         ENDDO
!
 20      eor = .TRUE.
         IF ( .NOT.incore ) THEN
            CALL read(*60,*100,scr6,id,-nskip1,0,nwds)
!
! CHECK ON COUNT CONSISTENCY
!
         ELSEIF ( icount/=Kcount ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 40      SPAG_Loop_1_1: DO
!
! EOR ON SCR6, I.E. END OF HC FOR A GIVEN CARD TYPE IN THIS SUBCASE.
! IF OTHER CARD TYPES EXIST IN THIS SUBCASE, THEY ARE IN SUBSEQUENT
! RECORDS. ADD RESULTS TO PREVIOUS ONES
!
            jcount = 0
            DO
               CALL read(*60,*40,scr6,id,2,0,nwds)
               nwords = 3*id(2)
               CALL fread(scr6,hc,nwords,0)
!
! ADD TO PREVIOUS HC FOR THIS ELEMENT- ALL ELEMENTS SHOULD BE  ON SCR6
! IN SAME ORDER IN EVERY RECORD
!
               nj = Nextz + jcount - 1
               DO i = 1 , nwords
                  z(nj+i) = z(nj+i) + hc(i)
               ENDDO
               jcount = jcount + nwords
               IF ( (.NOT.incore) .AND. jcount==inword ) THEN
                  CALL fwdrec(*80,scr6)
                  CALL read(*60,*100,scr6,id,-nskip1,0,nwds)
                  CYCLE SPAG_Loop_1_1
               ENDIF
            ENDDO
            EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
!
!
! DONE FOR THIS SUBCASE. PACK RESULTS. CLOSE SCR6 AND REOPEN TO WRITE
! NEXT SUBCASE (IF ALL DATA CAN FIT INTO CORE)
!
 60      IF ( incore ) THEN
!
            CALL close(scr6,1)
            jj = icount
            mcbh(3) = jj
            CALL pack(z(Nextz),hccens,mcbh)
         ELSE
!
! SPILL LOGIC-PACK OUT INWORD WORDS. THEN REWIND  SCRL AND SKIP DOWN
! AS NECESSARY
!
            IF ( .NOT.bldp ) CALL bldpk(1,1,scr6,0,0)
            bldp = .TRUE.
            DO k = 1 , inword
               a(1) = z(Nextz+k-1)
               irow = in + k
               CALL zblpki
            ENDDO
            IF ( eor ) THEN
!
! DONE FOR THIS SUBCASE (SPILL LOGIC)
!
               CALL close(scr6,1)
               mcbh(3) = icount
               CALL bldpkn(scr6,0,mcbh)
            ELSE
!
               in = in + inword
               CALL rewind(scr6)
               CALL fwdrec(*80,scr6)
               nskip = nskip - 2
               nskip1 = nskip
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
         CALL wrttrl(mcbh)
         IF ( Itype/=24 ) THEN
!
! CHECK ON COUNT CONSISTENCY
!
            IF ( .NOT.(incore) ) THEN
               IF ( icount/=Kcount ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDIF
!
         CALL gopen(scr6,z(Lcore+1),1)
         RETURN
      CASE (4)
!
         WRITE (iout,99001)
99001    FORMAT (58H0***SYSTEM FATAL ERROR,LOGIC ERROR,COUNTS ARE OFF IN HCCOM)
         CALL mesage(-61,0,0)
!
 80      CALL mesage(-2,scr6,nam)
 100     CALL mesage(-3,scr6,nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE hccom
