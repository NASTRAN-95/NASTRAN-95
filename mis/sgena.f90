
SUBROUTINE sgena(Type,Buf,Mcb,Ifile,Icode,Iextra,Ofile,Ocode,Oextra)
   IMPLICIT NONE
   INTEGER Idry , Iptr , Name(2) , Nono , Nss , Outt , Sysbuf , Two(32) , Z(1)
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /blank / Idry , Name
   COMMON /sgencm/ Nono , Nss , Iptr
   COMMON /system/ Sysbuf , Outt
   COMMON /two   / Two
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Z
   INTEGER Iextra , Ifile , Oextra , Ofile
   INTEGER Buf(1) , Icode(4) , Mcb(7) , Ocode(4) , Type(2)
   INTEGER andf , complf , orf
   INTEGER card(20) , cexist(6) , cin(6) , code , comp , i , icd , ig , igr , igrd , igrid , inam , ipt , isil , j , jg , n , nc ,  &
         & ncin , ngrd , npro , nwds , subnam(2)
   EXTERNAL andf , complf , orf
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
!
!     LOCATE CARDS ON FILE
!
   CALL locate(*700,Buf(1),Icode(1),icd)
   Icode(4) = 1
!
!     WRITE HEADER RECORD ON OUTPUT FILE
!
   CALL write(Ofile,Icode(1),3,0)
!
!     READ SID AND SUBSTRUCTURING NAME FROM CARD
!
 100  CALL read(*800,*600,Ifile,card,3,0,nwds)
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
      IF ( Z(inam)==card(2) .AND. Z(inam+1)==card(3) ) GOTO 200
   ENDDO
!
!     SUBSTRUCTURE NOT FOUND - SKIP OVER DATA
!
   CALL page2(-4)
   WRITE (Outt,99001) Uwm , (card(j),j=2,3) , Type , Name
99001 FORMAT (A25,' 6329, SUBSTRUCTURE ',2A4,' REFERENCED ON ',2A4,' CARD',/30X,'IS NOT A COMPONENT BASIC SUBSTRUCTURE OF ',        &
             &'SOLUTION STRUCTURE ',2A4,/30X,'THIS CARD WILL BE IGNORED')
   DO
      CALL fread(Ifile,card,2+Iextra,0)
      IF ( card(1)<0 ) GOTO 100
   ENDDO
!
!     FOUND SUBSTRUCTURE NAME
!
 200  ipt = Iptr + i - 1
   igrd = Z(ipt)
   ngrd = (Z(ipt+1)-Z(ipt))/3
 300  DO
!
!     PROCESS GRID-COMPONENT PAIRS
!
      CALL fread(Ifile,card(5),2+Iextra,0)
      igrid = card(5)
      IF ( igrid==-1 ) GOTO 100
      IF ( igrid/=0 ) THEN
         comp = card(6)
         IF ( comp==0 ) comp = 1
         card(6) = 0
         CALL bisloc(*400,igrid,Z(igrd),3,ngrd,igr)
         ig = igr + igrd - 1
         npro = 0
         DO WHILE ( Z(ig-3)==Z(ig) )
            IF ( ig<=igrd ) THEN
               CALL splt10(comp,cin,ncin)
               GOTO 500
            ELSE
               ig = ig - 3
            ENDIF
         ENDDO
!
!     SPLIT COMPONENTS
!
         CALL splt10(comp,cin,ncin)
         GOTO 500
      ENDIF
   ENDDO
!
!     BAD GRID
!
 400  Nono = 1
   CALL page2(-3)
   WRITE (Outt,99002) Ufm , (card(j),j=2,3) , igrid , comp , Type , Name
99002 FORMAT (A23,' 6022, SUBSTRUCTURE ',2A4,', GRID POINT',I9,', COMPONENTS',I9,1H,/30X,'REFERENCED ON ',2A4,                      &
             &' CARD, DO NOT EXIST ON SOLUTION STRUCTURE ',2A4)
   GOTO 300
 500  DO
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
      IF ( npro>=ncin ) GOTO 300
      IF ( Z(ig+3)/=Z(ig) ) GOTO 400
      IF ( (ig+3)>=(igrd+3*ngrd) ) GOTO 400
      ig = ig + 3
   ENDDO
!
!     FINISH PROCESSING CARDS BY CLOSING OUTPUT FILE RECORD
!
 600  CALL write(Ofile,0,0,1)
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
 700  RETURN
!
!     ERRORS
!
 800  CALL mesage(-2,Ifile,subnam)
   RETURN
END SUBROUTINE sgena