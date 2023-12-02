!*==dbmmgr.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
 
 
SUBROUTINE dbmmgr(Opcode)
   IMPLICIT NONE
   USE I_DSIOF
   USE C_SYSTEM
   USE C_XFIAT
   USE C_XFIST
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Opcode
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: case/4HCASE/ , pcdb/4HPCDB/ , pool/4HPOOL/ , xpdt/4HXPDT/ , xycd/4HXYCD/
   INTEGER :: i , iblk1 , iblk2 , iblk3 , icblk , icndex , idiff , ind1 , ind2 , indblk , ioff , iprblk , isave , isaveb , itest ,  &
            & k , lastib , nblk , nbuff3 , nexblk
   INTEGER , SAVE :: lenbuf
!
! End of declarations rewritten by SPAG
!
!*********************************************************************
!        / FCB /
!            FCB(1,I) - OPEN FLAG
!            FCB(2,I) - BUFFER ADDRESS
!            FCB(3,I) - CURRENT CLR
!            FCB(4,I) - CURRENT BLOCK NUMBER
!            FCB(5,I) - FIRST BLOCK NUMBER WRITTEN TO THIS FILE
!            FCB(6,I) - LAST BLOCK NUMBER WRITTEN TO THIS FILE
!            FCB(7,I) - MAXIMUM NUMBER OF BLOCKS TO BE ALLOCATED
!                       TO THIS FILE
!            FCB(8,I) - =0, IF NO MATRIX STRINGS WRITTEN TO FILE
!                       =1, OTHERWISE, USED TO INITIALIZE COLUMN
!                           NUMBER TO 1.
!            FCB(9,I) - INDEX TO FIRST IN-MEMORY BLOCK
!            FCB(10,I)- INDEX TO LAST IN-MEMORY BLOCK
!            FCB(11,I)- INDEX TO CURRENT IN-MEMORY BLOCK
!            FCB(12,I)- ORIGINAL BUFFER ADDRESS
!            FCB(13-14,I) - DMAP FILE NAME (2A4)
!            FCB(15,I)- OPEN FLAG FOR EXTERNAL FILE
!        / DBM/
!            IDBBAS - (INPUT)-INDEX TO IN-MEMORY DATA BASE RELATIVE
!                              TO /DBM/
!            IDBFRE - (INPUT)-INDEX TO FREE CHAIN OF IN-MEMORY DATA
!                              BASE RELATIVE TO /DBM/
!            IDBDIR - (INPUT)-INDEX TO FIRST DIRECTORY BLOCK
!            MAXALC - (OUTPUT)-MAXIMUM NUMBER OF BLOCKS AVAILABLE FOR
!                              JOB
!            MAXBLK - (OUTPUT)-MAXIMUM NUMBER OF BLOCKS ALLOCATED(JOB)
!            MAXDSK - (OUTPUT)-MAXIMUM NUMBER OF BLOCKS WRITTEN TO
!                              TO DISK
!            LENALC - (OUTPUT)-LENGTH OF EACH ALLOCATED BLOCK
!            IOCODE - (INPUT) -IO-CODE FOR OPEN/CLOSE CALL
!            IFILEX - (INPUT) -FILE NUMBER FOR GINO FILE IN /XFIAT/
!            NBLOCK - (INPUT/OUTPUT) -BLOCK NUMBER BEING REFERENCED
!            NAME   - (INPUT) -GINO FILE NAME (E.G., 101,201,303,...)
!            INDBAS - INDEX TO START OF BUFFER RELATIVE TO /ZZZZZZ/
!            INDCLR - INDEX TO CLR WITHIN BUFFER RELATIVE TO /ZZZZZZ/
!            INDCBP - INDEX TO CBP WITHIN BUFFER RELATIVE TO /ZZZZZZ/
!        FREE CHAIN FORMAT (ALSO, ALL BLOCKS ALLOCATED)
!               IDBFRE==> WORD 0    POINTER TO PREVIOUS FREE BLOCK
!                                      IN CHAIN, ALWAYS 0 FOR 1ST BLK)
!                         WORD 1    POINTER TO NEXT BLOCK IN CHAIN
!                                      -INITIALLY SET TO ZERO)
!                         WORD 2    NUMBER OF FREE WORDS IN BLOCK
!                         WORD 3    RELATIVE BLOCK NUMBER
!
!         OPCODE
!           1    OPEN
!                  /GINOX/ IOCODE = 0 ; READ WITH REWIND
!                                 = 1 ; WRITE WITH REWIND
!                                 = 2 ; READ WITHOUT REWIND
!                                 = 3 ; WRITE WITHOUT REWIND
!           2    CLOSE
!                  /GINOX/ IOCODE = 1 ; CLOSE WITH REWIND
!                                    (OTHERWISE NO REWIND)
!           3    REWIND
!           4    WRITE
!           5    READ
!           6    POSITION FILE
!                  NBLOCK = BLOCK NUMBER TO POSITION TO
!           7    DELETE FILE
!           8    PROCESS WRTBLK REQUEST (SUBSTRUCTURING)
!           9    PROCESS RDBLK REQUEST (SUBSTRUCTURING)
!********************************************************************
   DATA lenbuf/0/
   IF ( lenbuf==0 ) THEN
! SET UP BLOCK ALLOCATIONS FOR DOUBLE WORD BOUNDARIES
      ibasbf = locfx(Mem)
      lenbuf = Isysbf - 3 + 8
      lenalc = lenbuf
      nbuff3 = Isysbf - 4
      itest = mod(lenbuf,2)
      IF ( itest/=0 ) lenbuf = lenbuf + 1
   ENDIF
   IF ( idbdir/=0 ) THEN
      IF ( name<=100 .OR. name>=400 ) THEN
!30    IF ( NAME .GT. 300 .AND. NAME .LT. 400 ) GO TO 50
!  CHECK FOR CASECC, XYCD, AND PCDB (SETUP IN FIAT FOR PREFACE)
         IF ( name/=case ) THEN
            IF ( name/=xycd ) THEN
               IF ( name/=pcdb ) THEN
                  IF ( name/=xpdt ) THEN
                     IF ( name/=pool ) THEN
! OPCODES OF 8 AND 9 HAVE NO PURPOSE WHEN THERE IS NO USE OF THE
! IN-MEMORY DATA BASE
                        IF ( Opcode/=8 .AND. Opcode/=9 ) THEN
! CALL DBMIO DIRECTLY BECAUSE THIS IS AN EXECUTIVE FILE
                           IF ( fcb(9,ifilex)/=0 ) CALL dbmrel
!  CALL DBMIO DIRECTLY, NO IN-MEMORY DATA BASE
                           CALL dbmio(Opcode)
!      IF ( IFILEX .NE. 48 ) GO TO 55
!      IF ( NAME .NE. 307 ) GO TO 55
!      WRITE(IWR,40646)OPCODE,IOCODE,NBLOCK,IFILEX,NAME,INDBAS
99001                      FORMAT (/,' OPCODE,IOCODE,NBLOCK,IFILEX,NAME,INDBAS=',6I6)
!      WRITE(IWR,40647)(MEM(INDBAS+KB),KB=-4,20)
99002                      FORMAT (' INPUT BUFFER HAS=',/,10(4(1X,Z8),/))
!      WRITE(6,44770)(FCB(K,IFILEX),K=1,15)
99003                      FORMAT (' ENTERRED FCB=',/,2(5I8,/),2I8,4X,2A4,4X,I8)
                        ENDIF
                        GOTO 99999
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
!      CALL DBMFDP
      IF ( Opcode/=2 ) THEN
         IF ( Opcode==3 ) THEN
!****************
! REWIND OPCODE *****************************************************
!****************
! IF FILE IS ON EXTERNAL FILE CALL DBMIO DIRECTLY
            IF ( fcb(9,ifilex)/=0 ) THEN
               nexblk = fcb(9,ifilex)
               fcb(11,ifilex) = nexblk
               fcb(4,ifilex) = 1
! REPLACE BUFFER ADDRESS IN FCB
               fcb(2,ifilex) = locfx(Mem(nexblk+4)) - ibasbf + 1
               CALL dbmmov(indbas,nexblk+4,3)
               iocode = 0
               IF ( fcb(5,ifilex)/=0 ) CALL dbmio(2)
               GOTO 500
            ELSE
               CALL dbmio(Opcode)
               GOTO 99999
            ENDIF
         ELSEIF ( Opcode==4 ) THEN
!****************
! WRITE CODE ********************************************************
!****************
! CHECK TO SEE IF THIS BLOCK IS ON EXTERNAL FILE
            IF ( fcb(15,ifilex)/=0 ) THEN
               CALL dbmio(Opcode)
               GOTO 99999
            ELSE
! CHECK THAT BLOCK NUMBER MATCHES
               nexblk = fcb(11,ifilex)
               iblk1 = fcb(4,ifilex)
               iblk2 = Mem(nexblk+3)
               iblk3 = Mem(nexblk+7)
               IF ( iblk1/=iblk2 .OR. iblk1/=iblk3 ) THEN
                  WRITE (Iwr,99004) ifilex , iblk1 , iblk2 , iblk3
99004             FORMAT (///' BLOCK NUMBERS INCONSISTANT ON WRITE IN DBMMGR',/,' UNIT = ',I4,/,                                    &
                        & ' BLOCK NUMBER EXPECTED (IN FCB)  =',I8,/,' BLOCK NUMBER IN IN-MEMORY BLOCK =',I8,/,                      &
                         &' BLOCK NUMBER IN BUFFER          =',I8)
!      CALL DBMDMP
                  CALL dbmfdp
                  CALL dsmsg(777)
                  CALL mesage(-61,0,0)
               ENDIF
               fcb(4,ifilex) = fcb(4,ifilex) + 1
               nexblk = Mem(indbas-3)
               IF ( nexblk==0 ) THEN
                  CALL dbmalb(lenbuf,nexblk)
                  IF ( nexblk<=0 ) THEN
! NO MORE SPACE IN IN-MEMORY DATA BASE, WRITE DATA TO FILE
! CALL DBMIO TO OPEN EXTERNAL FILE WITH REWIND
                     isave = iocode
                     isaveb = nblock
                     iocode = 1
                     nblock = fcb(4,ifilex)
                     iprblk = indbas
! RESET BUFFER ADDRESS TO BUFFER IN USER'S OPEN CORE
                     fcb(2,ifilex) = fcb(12,ifilex)
                     indbas = fcb(2,ifilex)
                     CALL dbmio(1)
                     iocode = isave
                     nblock = isaveb
!      WRITE(6,88771)(MEM(IPRBLK+K),K=-4,4)
99005                FORMAT (' MEMPRBLK=',9(1X,Z8))
!      WRITE(6,88772)(MEM(INDBAS+K),K=-4,4)
99006                FORMAT (' MEMINDBAS=',9(1X,Z8))
!      PRINT *,' IFILEX,NBLOCK,IPRBLK,INDBAS=',IFILEX,NBLOCK,
!     & IPRBLK,INDBAS
!      PRINT *,' MEM(IPRBLK=',MEM(IPRBLK)
!      WRITE(6,88771)(MEM(IPRBLK+K),K=-4,4)
!      WRITE(6,88772)(MEM(INDBAS+K),K=-4,4)
                     CALL dbmmov(iprblk,indbas,4)
                  ELSE
! ANOTHER BLOCK SUCCESSFULLY ALLOCATED, CONNECT TO CHAIN
                     indblk = fcb(11,ifilex)
                     Mem(indblk+1) = nexblk
                     Mem(nexblk) = indblk
                     Mem(nexblk+1) = 0
                     Mem(nexblk+2) = lenbuf
                     Mem(nexblk+3) = fcb(4,ifilex)
                     fcb(10,ifilex) = nexblk
                     fcb(11,ifilex) = nexblk
                     fcb(2,ifilex) = locfx(Mem(nexblk+4)) - ibasbf + 1
                     CALL dbmmov(indbas,nexblk+4,4)
                  ENDIF
               ELSE
! USE EXISTING BLOCK ALREADY ALLOCATED FROM PREVIOUS OPEN FOR WRITE
                  fcb(11,ifilex) = nexblk
                  fcb(2,ifilex) = locfx(Mem(nexblk+4)) - ibasbf + 1
                  CALL dbmmov(indbas,nexblk+4,4)
               ENDIF
               GOTO 500
            ENDIF
         ELSEIF ( Opcode==5 ) THEN
!****************
! READ CODE *********************************************************
!****************
            IF ( fcb(5,ifilex)==0 ) GOTO 100
            IF ( fcb(4,ifilex)>=(fcb(5,ifilex)-1) ) GOTO 200
            GOTO 100
         ELSEIF ( Opcode==6 ) THEN
            GOTO 300
         ELSEIF ( Opcode==7 ) THEN
!****************
! DELETE CODE *******************************************************
!****************
            IF ( fcb(9,ifilex)/=0 ) CALL dbmrel
            CALL dbmio(7)
            DO k = 1 , 15
               IF ( k/=7 ) fcb(k,ifilex) = 0
            ENDDO
            GOTO 99999
         ELSEIF ( Opcode==8 ) THEN
!****************
! WRTBLK CODE *******************************************************
!****************
! SPECIAL ENTRY FOR SUBSTRUCTURING, MOVE DATA FROM OPENCORE BUFFER
! CALLED BY WRTBLK OF GINO
! ORIGINAL BUFFER IS BEING USED BY GINO, JUST RETURN
            IF ( fcb(15,ifilex)/=0 ) GOTO 99999
            ind1 = fcb(2,ifilex)
            ind2 = fcb(12,ifilex)
            ind1 = ind1 + 2
            ind2 = ind2 + 2
!      PRINT *,' DBMMGR,WRTBLK,IND1,IND2,NBUFF3=',IND1,IND2,NBUFF3
!      PRINT *,' DBMMGR,WRTBLK,INDBAS=',INDBAS
!      WRITE(6,44771)(FCB(K,IFILEX),K=1,15)
!      WRITE(6,44772)(MEM(IND2+K),K=1,8)
99007       FORMAT (' DBMMGR,BUFFER,IND2=',8(1X,Z8))
            DO i = 1 , nbuff3
               Mem(ind1+i) = Mem(ind2+i)
            ENDDO
            GOTO 500
         ELSEIF ( Opcode==9 ) THEN
!****************
! RDBLK  CODE *******************************************************
!****************
! SPECIAL ENTRY FOR SUBSTRUCTURING, MOVE DATA TO ORIGINAL BUFFER IF
! THE IN-MEMORY DATA BASE IS BEING USED
! CALLED BY RDBLK
! ORIGINAL BUFFER IS BEING USED, JUST RETURN
            IF ( fcb(15,ifilex)/=0 ) GOTO 99999
            ind1 = fcb(2,ifilex)
            ind2 = fcb(12,ifilex)
            ind1 = ind1 + 2
            ind2 = ind2 + 2
!      PRINT *,' DBMMGR,RDBLK,IND1,IND2,NBUFF3=',IND1,IND2,NBUFF3
!      PRINT *,' DBMMGR,RDBLK,INDBAS=',INDBAS
!      WRITE(6,44771)(FCB(K,IFILEX),K=1,15)
!      WRITE(6,44773)(MEM(IND1+K),K=1,8)
99008       FORMAT (' DBMMGR,BUFFER,IND1=',8(1X,Z8))
            DO i = 1 , nbuff3
               Mem(ind2+i) = Mem(ind1+i)
            ENDDO
            GOTO 500
         ELSE
!****************
! OPEN CODE *********************************************************
!****************
            fcb(1,ifilex) = iocode
            fcb(12,ifilex) = fcb(2,ifilex)
            IF ( fcb(9,ifilex)/=0 ) THEN
! FILE EXISTS IN IN-MEMORY DATA BASE
               IF ( iocode==0 ) GOTO 10
               IF ( iocode/=1 ) THEN
                  IF ( iocode==2 ) THEN
! FILE IS OPENED FOR READ WITHOUT REWIND
                     nexblk = fcb(10,ifilex)
                     lastib = Mem(nexblk+3)
                     nblock = fcb(4,ifilex)
                     IF ( fcb(4,ifilex)>lastib ) THEN
! NO MORE SPACE WITHIN IN-MEMORY DATA BASE, USE I/O
                        CALL dbmio(Opcode)
                        GOTO 99999
                     ELSE
                        IF ( fcb(4,ifilex)==1 ) GOTO 10
                        nexblk = fcb(11,ifilex)
                        iblk1 = fcb(4,ifilex)
                        iblk2 = Mem(nexblk+3)
                        iblk3 = Mem(nexblk+7)
                        fcb(2,ifilex) = locfx(Mem(nexblk+4)) - ibasbf + 1
! CHECK THAT CURRENT BLOCK NUMBER MATCHES BLOCK NO. IN IN-MEM BLK
                        IF ( iblk1==iblk2 .AND. iblk1==iblk3 ) GOTO 500
                        GOTO 20
                     ENDIF
                  ELSE
                     IF ( iocode/=3 ) GOTO 10
! FILE IS OPENED FOR WRITE WITHOUT REWIND
                     nexblk = fcb(10,ifilex)
                     lastib = Mem(nexblk+3)
                     IF ( fcb(4,ifilex)>lastib ) THEN
                        CALL dbmio(Opcode)
                        GOTO 99999
                     ELSE
!======      IF ( FCB( 4, IFILEX ) .EQ. 1      ) GO TO 160
                        nexblk = fcb(11,ifilex)
! IGNORE ANY PREVIOUSLY WRITTEN BLOCKS FOR THIS FILE
                        fcb(5,ifilex) = 0
                        fcb(6,ifilex) = 0
                        iblk1 = fcb(4,ifilex)
                        iblk2 = Mem(nexblk+3)
                        iblk3 = Mem(nexblk+7)
                        fcb(2,ifilex) = locfx(Mem(nexblk+4)) - ibasbf + 1
! CHECK THAT CURRENT BLOCK NUMBER MATCHES BLOCK NO. IN IN-MEM BLK
                        IF ( iblk1==iblk2 .AND. iblk1==iblk3 ) GOTO 500
                        GOTO 20
                     ENDIF
                  ENDIF
               ENDIF
! CHECK TO SEE IF FILE IS SELF CONTAINED ON DISK
            ELSEIF ( fcb(5,ifilex)/=0 ) THEN
               CALL dbmio(Opcode)
               GOTO 99999
            ENDIF
            IF ( iocode==0 .OR. iocode==2 ) THEN
               WRITE (Iwr,99009) ifilex , fcb(13,ifilex) , fcb(14,ifilex)
99009          FORMAT (///,' DBMMGR ERROR, ATTEMPT TO OPEN FOR READ OR WRITE APP','END:',/,' UNIT-',I4,'  NAME=',2A4,               &
                      &' WHICH DOES NOT EXIST.')
!      CALL DBMDMP
               CALL dsmsg(777)
               CALL mesage(-61,0,0)
            ENDIF
! NEW FILE NAME FOR IFILEX, RELEASE ANY PREVIOUSLY ALLOCATED BLOCKS
            IF ( fcb(9,ifilex)/=0 ) CALL dbmrel
! CREATE FILE ENTRY IN FCB
            DO i = 3 , 11
               IF ( i/=7 ) fcb(i,ifilex) = 0
            ENDDO
            fcb(4,ifilex) = 1
            nblock = 1
! ALLOCATE FIRST BLOCK
            CALL dbmalb(lenbuf,nexblk)
            IF ( nexblk<=0 ) THEN
               CALL dbmio(Opcode)
               GOTO 99999
            ELSE
               fcb(9,ifilex) = nexblk
               fcb(10,ifilex) = nexblk
               fcb(11,ifilex) = nexblk
! INITIALIZE PREVIOUS, NEXT, LENGTH AND BLOCK NUMBER FOR ALLOCATED BLK
               Mem(nexblk) = 0
               Mem(nexblk+1) = 0
               Mem(nexblk+2) = lenbuf
               Mem(nexblk+3) = 1
               fcb(2,ifilex) = locfx(Mem(nexblk+4)) - ibasbf + 1
               CALL dbmmov(indbas,nexblk+4,4)
               GOTO 500
            ENDIF
! FILE IS OPENED FOR READ WITH REWIND
 10         nexblk = fcb(9,ifilex)
            IF ( nexblk<=0 ) THEN
               WRITE (Iwr,99010) ifilex
99010          FORMAT (///,' DBMMGR ERROR, ATTEMPT TO READ FILE WITH NO BLOCKS'/,' UNIT=',I4)
!      CALL DBMDMP
               CALL dsmsg(777)
               CALL mesage(-61,0,0)
            ENDIF
            fcb(11,ifilex) = nexblk
            fcb(4,ifilex) = 1
            nblock = 1
            fcb(2,ifilex) = locfx(Mem(nexblk+4)) - ibasbf + 1
            CALL dbmmov(indbas,nexblk+4,3)
! FILE IS OPENED FOR WRITE WITH REWIND
            GOTO 500
         ENDIF
 20      WRITE (Iwr,99011) ifilex , iblk1 , iblk2 , iblk3
99011    FORMAT (///' BLOCK NUMBERS INCONSISTANT ON OPEN IN DBMMGR',/,' UNIT =',I4,/,' BLOCK NUMBER EXPECTED (IN FCB)  =',I8,/,     &
                &' BLOCK NUMBER IN IN-MEMORY BLOCK =',I8,/,' BLOCK NUMBER IN BUFFER          =',I8)
!      CALL DBMDMP
         CALL dbmfdp
         CALL dsmsg(777)
         CALL mesage(-61,0,0)
      ENDIF
!****************
! CLOSE CODE ********************************************************
!****************
! CHECK TO SEE IF FILE HAS IN-MEMORY BLOCKS
      IF ( fcb(9,ifilex)/=0 ) THEN
!WKBDB SPR94012 10/94
!      IF ( IOCODE .NE. 1 ) GO TO 225
!C CLOSE FILE WITH REWIND
!      FCB( 11, IFILEX ) = FCB(  9, IFILEX )
!      FCB(  4, IFILEX ) = 1
!      IF ( FCB( 5, IFILEX ) .NE. 0 ) GO TO 210
!WKBDE SPR94012 10/94
! IF FILE IS OPENED FOR READ THAN GO COMPUTE STATISTICS
         IF ( fcb(1,ifilex)/=0 .AND. fcb(1,ifilex)/=2 ) THEN
            IF ( fcb(15,ifilex)==0 ) THEN
! FILE OPENED FOR WRITE AND FILE NOT SPILLED TO DISK, THEN
! RELEASE LAST ALLOCATED BLOCK, BECAUSE IT WAS NOT USED
               nexblk = fcb(11,ifilex)
               DO
! RESET LAST BLOCK POINTER, GET PREVIOUS BLOCK ALLOCATED
!WKBNB SPR94012 10/94
                  iblock = Mem(nexblk+3)
! CHECK IF LAST BLOCK NOT USED, THERE COULD HAVE BEEN A BACKPSPACE BACK
! TO A PREVIOUS USED BLOCK (CAUSED BY CLOSE CALLING DSBRC1 TO BACKSPACE
! OVER AN EOF THAT WAS AT THE END OF A PREVIOUS BLOCK).
                  IF ( iblock>nblock ) THEN
!WKBNE SPR94012 10/94
                     indblk = Mem(nexblk)
                     fcb(10,ifilex) = indblk
                     fcb(11,ifilex) = indblk
                     fcb(4,ifilex) = Mem(indblk+3)
                     fcb(2,ifilex) = locfx(Mem(indblk+4)) - ibasbf + 1
                     CALL dbmrlb(nexblk)
                     EXIT
                  ELSE
                     nexblk = Mem(nexblk+1)
                     IF ( nexblk==0 ) EXIT
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
!WKBNB SPR94012 10/94
         IF ( iocode==1 ) THEN
! CLOSE FILE WITH REWIND
            fcb(11,ifilex) = fcb(9,ifilex)
            fcb(4,ifilex) = 1
         ENDIF
!WKBNE SPR94012 10/94
!WKBR  SPR94012 10/94
!240   IF ( FCB( 5, IFILEX ) .NE. 0 ) CALL DBMIO ( OPCODE )
         IF ( fcb(5,ifilex)/=0 ) CALL dbmio(Opcode)
         IF ( fcb(5,ifilex)>fcb(6,ifilex) ) THEN
! SPECIAL CASE, LAST BLOCK ALLOCATED WAS FOR DISK BUT NEVER USED, RESET
! INDBAS BACK TO LAST IN-MEMORY BLOCK
            nexblk = fcb(10,ifilex)
            fcb(2,ifilex) = locfx(Mem(nexblk+4)) - ibasbf + 1
            fcb(5,ifilex) = 0
            fcb(6,ifilex) = 0
            fcb(11,ifilex) = fcb(10,ifilex)
         ENDIF
      ELSE
         CALL dbmio(Opcode)
      ENDIF
      GOTO 500
   ELSE
! OPCODES OF 8 AND 9 HAVE NO PURPOSE WHEN THERE IS NO USE OF THE
! IN-MEMORY DATA BASE
      IF ( Opcode/=8 .AND. Opcode/=9 ) CALL dbmio(Opcode)
      GOTO 99999
   ENDIF
 100  fcb(4,ifilex) = fcb(4,ifilex) + 1
   nexblk = Mem(indbas-3)
   IF ( nexblk<=0 ) THEN
      WRITE (Iwr,99012) fcb(4,ifilex) , ifilex
99012 FORMAT (///,' ERROR IN DBMMGR DURING READ',/,' EXPECTED ANOTHER ',' IN-MEMORY BLOCK FOR BLOCK=',I8,' UNIT=',I3)
!      CALL DBMDMP
      CALL dbmfdp
      CALL dsmsg(777)
      CALL mesage(-61,0,0)
   ENDIF
   fcb(2,ifilex) = locfx(Mem(nexblk+4)) - ibasbf + 1
   fcb(11,ifilex) = nexblk
   CALL dbmmov(indbas,nexblk+4,3)
   iblk1 = fcb(4,ifilex)
   iblk2 = Mem(nexblk+3)
   iblk3 = Mem(nexblk+7)
   IF ( iblk1==iblk2 .AND. iblk1==iblk3 ) GOTO 500
   WRITE (Iwr,99013) ifilex , iblk1 , iblk2 , iblk3
99013 FORMAT (///' BLOCK NUMBERS INCONSISTANT ON READ IN DBMMGR',/,' UNIT =',I4,/,' BLOCK NUMBER  (IN FCB)          =',I8,/,        &
             &' BLOCK NUMBER IN IN-MEMORY BLOCK =',I8,/,' BLOCK NUMBER IN BUFFER          =',I8)
!      CALL DBMDMP
   CALL dbmfdp
   CALL dsmsg(777)
   CALL mesage(-61,0,0)
! BLOCK IS NOT IN MEMORY, CALL DBMIO
 200  IF ( fcb(15,ifilex)==0 ) THEN
      isave = iocode
      isaveb = nblock
      iocode = 0
      nblock = fcb(4,ifilex) + 1
      iprblk = indbas
      indbas = fcb(12,ifilex)
      fcb(2,ifilex) = indbas
      CALL dbmio(1)
      iocode = isave
      nblock = isaveb
      CALL dbmmov(iprblk,indbas,3)
      GOTO 99999
   ELSEIF ( fcb(4,ifilex)>fcb(6,ifilex) ) THEN
      WRITE (Iwr,99014) ifilex
99014 FORMAT (///,' DBMMGR ERROR, ATTEMPT TO READ BEYOND EOF',/' UNIT=',I5)
!      CALL DBMDMP
      CALL dbmfdp
      CALL dsmsg(777)
      CALL mesage(-61,0,0)
   ELSE
      indbas = fcb(12,ifilex)
      fcb(2,ifilex) = indbas
      CALL dbmio(Opcode)
      GOTO 99999
   ENDIF
!****************
! POSITION CODE *****************************************************
!****************
 300  IF ( fcb(5,ifilex)/=0 ) THEN
      IF ( nblock>=fcb(5,ifilex) ) THEN
         IF ( fcb(15,ifilex)/=0 ) THEN
            fcb(4,ifilex) = nblock
            indbas = fcb(12,ifilex)
            fcb(2,ifilex) = indbas
            CALL dbmio(Opcode)
         ELSE
            isave = iocode
            iocode = 0
            iprblk = indbas
            indbas = fcb(12,ifilex)
            fcb(2,ifilex) = indbas
            fcb(4,ifilex) = nblock
            CALL dbmio(1)
            iocode = isave
            CALL dbmmov(iprblk,indbas,3)
         ENDIF
         GOTO 99999
      ENDIF
   ENDIF
! BLOCK IS IN THE IN-MEMORY DATA BASE, WALK CHAIN TO CORRECT BLOCK
   ioff = 1
   nblk = nblock - 1
   nexblk = fcb(9,ifilex)
   IF ( nblock/=1 ) THEN
      icndex = fcb(11,ifilex)
      IF ( icndex/=0 ) THEN
         nexblk = icndex
         icblk = Mem(icndex+3)
         IF ( icblk==nblock ) GOTO 400
         idiff = nblock - icblk
         nblk = iabs(idiff)
         IF ( idiff<0 ) ioff = 0
      ENDIF
      DO i = 1 , nblk
         nexblk = Mem(nexblk+ioff)
      ENDDO
   ENDIF
! SET DIRECTORY ENTRIES FOR THE POSITIONED BLOCK
 400  fcb(11,ifilex) = nexblk
   fcb(4,ifilex) = nblock
   fcb(2,ifilex) = locfx(Mem(nexblk+4)) - ibasbf + 1
   CALL dbmmov(indbas,nexblk+4,3)
! SET INDBAS TO POINT TO CURRENT BUFFER
 500  indbas = fcb(2,ifilex)
!      IF ( NAME .NE. 307 ) GO TO 7777
!      IF ( IFILEX .NE. 48 ) GO TO 7777
!      PRINT *,' DBMMGR RETURNING,IFILEX,INDBAS=',IFILEX,INDBAS
!      PRINT *,' DBMMGR RETURNING,INDCLR,INDCBP=',INDCLR,INDCBP
!      write(6,40648)(mem(kb),kb=indbas-4,indbas+8)
99015 FORMAT (' returned buffer=',/,10(4(1x,z8),/))
!      WRITE(6,44771)(FCB(K,IFILEX),K=1,15)
!      CALL DBMFDP
99016 FORMAT (' returned FCB=',/,2(5I8,/),2I8,4X,2A4,4X,I8)
99999 END SUBROUTINE dbmmgr
