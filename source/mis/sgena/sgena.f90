!*==sgena.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sgena(Type,Buf,Mcb,Ifile,Icode,Iextra,Ofile,Ocode,Oextra)
   IMPLICIT NONE
   USE C_BLANK
   USE C_SGENCM
   USE C_SYSTEM
   USE C_TWO
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: Type
   INTEGER , DIMENSION(1) :: Buf
   INTEGER , DIMENSION(7) :: Mcb
   INTEGER :: Ifile
   INTEGER , DIMENSION(4) :: Icode
   INTEGER :: Iextra
   INTEGER :: Ofile
   INTEGER , DIMENSION(4) :: Ocode
   INTEGER :: Oextra
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(20) :: card
   INTEGER , DIMENSION(6) :: cexist , cin
   INTEGER :: code , comp , i , icd , ig , igr , igrd , igrid , inam , ipt , isil , j , jg , n , nc , ncin , ngrd , npro , nwds
   INTEGER , DIMENSION(2) , SAVE :: subnam
   EXTERNAL andf , bisloc , complf , decode , fread , locate , mesage , orf , page2 , read , splt10 , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE READS SUBSTRUCTURING CONSTRAINT AND DYNAMIC PROPERTY
!     CARDS AND CONVERTS THEM TO NASTRAN FORMAT
!
!     INPUTS -
!
!     TYPE   - BCD CARD NAME
!     BUF    - GINO BUFFER FOR INPUT FILE
!     MCB    - MATRIX CONTROL BLOCK FOR INPUT FILE
!     IFILE  - INPUT FILE NAME
!     ICODE  - LOCATE CODE FOR INPUT CARD TYPE
!     IEXTRA - NUMBER OF EXTRA WORDS (AFTER GRID) TO BE READ
!     OFILE  - OUTPUT FILE NAME
!     OCODE  - LOCATE CODE FOR OUTPUT CARD TYPE
!     OEXTRA - NUMBER OF EXTRA WORDS (AFTER GRID) TO BE WRITTEN
!
   DATA subnam/4HSGEN , 4HA   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     LOCATE CARDS ON FILE
!
         CALL locate(*60,Buf(1),Icode(1),icd)
         Icode(4) = 1
!
!     WRITE HEADER RECORD ON OUTPUT FILE
!
         CALL write(Ofile,Icode(1),3,0)
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_1: DO
!
!     READ SID AND SUBSTRUCTURING NAME FROM CARD
!
            CALL read(*80,*40,Ifile,card,3,0,nwds)
            card(4) = card(1)
            n = 6 + Oextra
            DO i = 5 , n
               card(i) = 0
            ENDDO
!
!     FIND SUBSTRUCTURE
!
            DO i = 1 , Nss
               inam = 2*i + 3
               IF ( Z(inam)==card(2) .AND. Z(inam+1)==card(3) ) EXIT SPAG_Loop_1_1
            ENDDO
!
!     SUBSTRUCTURE NOT FOUND - SKIP OVER DATA
!
            CALL page2(-4)
            WRITE (Outt,99001) Uwm , (card(j),j=2,3) , Type , Name
99001       FORMAT (A25,' 6329, SUBSTRUCTURE ',2A4,' REFERENCED ON ',2A4,' CARD',/30X,'IS NOT A COMPONENT BASIC SUBSTRUCTURE OF ',  &
                   &'SOLUTION STRUCTURE ',2A4,/30X,'THIS CARD WILL BE IGNORED')
            DO
               CALL fread(Ifile,card,2+Iextra,0)
               IF ( card(1)<0 ) CYCLE SPAG_Loop_1_1
            ENDDO
            EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
!
!     FOUND SUBSTRUCTURE NAME
!
         ipt = Iptr + i - 1
         igrd = Z(ipt)
         ngrd = (Z(ipt+1)-Z(ipt))/3
         spag_nextblock_1 = 3
      CASE (3)
         DO
!
!     PROCESS GRID-COMPONENT PAIRS
!
            CALL fread(Ifile,card(5),2+Iextra,0)
            igrid = card(5)
            IF ( igrid==-1 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( igrid/=0 ) THEN
               comp = card(6)
               IF ( comp==0 ) comp = 1
               card(6) = 0
               CALL bisloc(*20,igrid,Z(igrd),3,ngrd,igr)
               ig = igr + igrd - 1
               npro = 0
               DO WHILE ( Z(ig-3)==Z(ig) )
                  IF ( ig<=igrd ) THEN
                     CALL splt10(comp,cin,ncin)
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     ig = ig - 3
                  ENDIF
               ENDDO
!
!     SPLIT COMPONENTS
!
               CALL splt10(comp,cin,ncin)
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
!
!     BAD GRID
!
 20      Nono = 1
         CALL page2(-3)
         WRITE (Outt,99002) Ufm , (card(j),j=2,3) , igrid , comp , Type , Name
99002    FORMAT (A23,' 6022, SUBSTRUCTURE ',2A4,', GRID POINT',I9,', COMPONENTS',I9,1H,/30X,'REFERENCED ON ',2A4,                   &
                &' CARD, DO NOT EXIST ON SOLUTION STRUCTURE ',2A4)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
         DO
            code = Z(ig+2)
            IF ( code==0 ) code = 1
            isil = Z(ig+1)
            CALL decode(code,cexist,nc)
!
!     FIND ACTUAL REMAINING COMPONENTS AND WRITE CONVERTED DATA TO
!     OUTPUT FILE
!
            DO j = 1 , nc
               DO jg = 1 , ncin
                  IF ( cin(jg)-cexist(j)==1 ) THEN
                     npro = npro + 1
                     card(5) = isil + j - 1
                     CALL write(Ofile,card(4),3+Oextra,0)
                  ENDIF
               ENDDO
            ENDDO
            IF ( npro>=ncin ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Z(ig+3)/=Z(ig) ) GOTO 20
            IF ( (ig+3)>=(igrd+3*ngrd) ) GOTO 20
            ig = ig + 3
         ENDDO
!
!     FINISH PROCESSING CARDS BY CLOSING OUTPUT FILE RECORD
!
 40      CALL write(Ofile,0,0,1)
!
!     TURN OFF TRAILER FOR INPUT CARD TYPE
!
         j = (Icode(2)-1)/16
         i = Icode(2) - 16*j
         Mcb(j+2) = andf(complf(Two(i+16)),Mcb(j+2))
!
!     TURN ON TRAILER FOR OUTPUT CARD TYPE
!
         j = (Ocode(2)-1)/16
         i = Ocode(2) - 16*j
         Mcb(j+2) = orf(Two(i+16),Mcb(j+2))
!
!     RETURN
!
 60      RETURN
!
!     ERRORS
!
 80      CALL mesage(-2,Ifile,subnam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE sgena
