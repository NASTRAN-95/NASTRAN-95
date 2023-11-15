
SUBROUTINE hccom(Itype,Lcore,Icore,Nextz,Kcount)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL A(4) , Z(1)
   INTEGER Idum , Ii , Incr , Iout , Irow , Ita , Itb , Iz(1) , Jj
   COMMON /packx / Ita , Itb , Ii , Jj , Incr
   COMMON /system/ Idum , Iout
   COMMON /zblpkx/ A , Irow
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Icore , Itype , Kcount , Lcore , Nextz
!
! Local variable declarations
!
   LOGICAL bldp , eor , incore
   REAL hc(63)
   INTEGER hccens , i , icount , id(2) , in , inext , inword , jcount , k , mcbh(7) , nam(2) , nj , nskip , nskip1 , nwds , nwords ,&
         & scr6
!
! End of declarations
!
!
! COMBINES HC CENTROID INFO ON SCR6 TO HCCENS
!
   EQUIVALENCE (Z(1),Iz(1))
   DATA mcbh/307 , 0 , 0 , 2 , 1 , 0 , 0/
   DATA scr6 , hccens/306 , 307/
   DATA nam/4HHCCO , 4HM   /
!
   Ita = 1
   Itb = 1
   Ii = 1
   Incr = 1
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
      GOTO 500
   ELSE
!
      CALL gopen(scr6,Z(Lcore+1),0)
   ENDIF
!
! SCR6 HAS 3 ENTRIES PER ELEMENT-ID,NUMBER OF POINTS AT WHICH HC IS
! COMPUTED=N, AND 3*N HC VALUES--THERE IS ONE RECORD PER CARD TYPE
! ON SCR6 FOR THIS SUBCASE
! IF  .NOT. INCORE, THEN WE ARE BACK HERE DUE TO SPILL LOGIC AND  ARE
! TRYING TO FINISH THE FIRST RECORD. SO WE MUST SKIP THE PART OF THE
! RECORD PREVIOUSLY READ.
!
 100  IF ( .NOT.incore ) CALL fread(scr6,id,-nskip,0)
   inword = 0
   DO
      CALL read(*700,*200,scr6,id,2,0,nwds)
      nskip = nskip + 2
      inext = Nextz + inword
      nwords = 3*id(2)
      IF ( inext+nwords>Icore ) THEN
!
! INFO WILL NOT FIT IN CORE - SPILL LOGIC
!
         incore = .FALSE.
         CALL fwdrec(*700,scr6)
!
! SKIP APPROPRIATE NUMBER OF WORDS IN THIS RECORD TO ACCOUNT FOR
! THE PORTION OF THIS RECORD PREVIOUSLY READ
!
         CALL read(*400,*800,scr6,id,-nskip1,0,nwds)
         GOTO 300
      ELSE
         CALL fread(scr6,Z(inext),nwords,0)
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
 200  eor = .TRUE.
   IF ( .NOT.incore ) THEN
      CALL read(*400,*800,scr6,id,-nskip1,0,nwds)
!
! CHECK ON COUNT CONSISTENCY
!
   ELSEIF ( icount/=Kcount ) THEN
      GOTO 600
   ENDIF
!
! EOR ON SCR6, I.E. END OF HC FOR A GIVEN CARD TYPE IN THIS SUBCASE.
! IF OTHER CARD TYPES EXIST IN THIS SUBCASE, THEY ARE IN SUBSEQUENT
! RECORDS. ADD RESULTS TO PREVIOUS ONES
!
 300  jcount = 0
   DO
      CALL read(*400,*300,scr6,id,2,0,nwds)
      nwords = 3*id(2)
      CALL fread(scr6,hc,nwords,0)
!
! ADD TO PREVIOUS HC FOR THIS ELEMENT- ALL ELEMENTS SHOULD BE  ON SCR6
! IN SAME ORDER IN EVERY RECORD
!
      nj = Nextz + jcount - 1
      DO i = 1 , nwords
         Z(nj+i) = Z(nj+i) + hc(i)
      ENDDO
      jcount = jcount + nwords
      IF ( (.NOT.incore) .AND. jcount==inword ) THEN
         CALL fwdrec(*700,scr6)
         CALL read(*400,*800,scr6,id,-nskip1,0,nwds)
         GOTO 300
      ENDIF
   ENDDO
!
!
! DONE FOR THIS SUBCASE. PACK RESULTS. CLOSE SCR6 AND REOPEN TO WRITE
! NEXT SUBCASE (IF ALL DATA CAN FIT INTO CORE)
!
 400  IF ( incore ) THEN
!
      CALL close(scr6,1)
      Jj = icount
      mcbh(3) = Jj
      CALL pack(Z(Nextz),hccens,mcbh)
   ELSE
!
! SPILL LOGIC-PACK OUT INWORD WORDS. THEN REWIND  SCRL AND SKIP DOWN
! AS NECESSARY
!
      IF ( .NOT.bldp ) CALL bldpk(1,1,scr6,0,0)
      bldp = .TRUE.
      DO k = 1 , inword
         A(1) = Z(Nextz+k-1)
         Irow = in + k
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
         CALL fwdrec(*700,scr6)
         nskip = nskip - 2
         nskip1 = nskip
         GOTO 100
      ENDIF
   ENDIF
!
 500  CALL wrttrl(mcbh)
   IF ( Itype/=24 ) THEN
!
! CHECK ON COUNT CONSISTENCY
!
      IF ( .NOT.(incore) ) THEN
         IF ( icount/=Kcount ) GOTO 600
      ENDIF
   ENDIF
!
   CALL gopen(scr6,Z(Lcore+1),1)
   RETURN
!
 600  WRITE (Iout,99001)
99001 FORMAT (58H0***SYSTEM FATAL ERROR,LOGIC ERROR,COUNTS ARE OFF IN HCCOM)
   CALL mesage(-61,0,0)
!
 700  CALL mesage(-2,scr6,nam)
 800  CALL mesage(-3,scr6,nam)
END SUBROUTINE hccom
