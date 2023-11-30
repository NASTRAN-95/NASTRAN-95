
SUBROUTINE dbmio(Opcode)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'GINOX.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER Opcode
!
!  OPCODE
!         = 1   OPEN, IOCODE = 0 OPEN FOR READ WITH REWIND
!                            = 1 OPEN FOR WRITE WITH REWIND
!                            = 2 OPEN FOR READ WITHOUT REWIND
!                            = 3 OPEN FOR WRITE WITHOUT REWIND
!         = 2   CLOSE, IOCODE = 1 CLOSE WITH REWIND
!                                OTHERWISE, NO REWIND
!         = 3   REWIND
!         = 4   WRITE ONE BLOCK
!         = 5   READ ONE BLOCK
!         = 6   POSITION
!         = 7   DELETE FILE
!         = 8   WRTBLK CODE
!         = 9   RDBLK CODE
!-----------------------------------------------------------------------
!      PRINT *,' DBMIO CALLED WITH OPCODE,IFILEX=',OPCODE,IFILEX
!      PRINT *,' DBMIO,NBLOCK,IOCODE=',NBLOCK,IOCODE
!      WRITE(6,40646)(FCB(K,IFILEX),K=1,15)
99001 FORMAT (' DBMIO-ENTRY,FCB=',/I3,I7,4I5,I7,I2,4I7,1X,2A4,I4)
   IF ( Opcode==2 ) THEN
!-CLOSE -----------------------------
      CALL dsgncl
      IF ( Iocode==0 ) Fcb(4,Ifilex) = 1
      IF ( Fcb(15,Ifilex)==701 .OR. Fcb(15,Ifilex)==703 ) THEN
         Fcb(4,Ifilex) = Fcb(4,Ifilex) - 1
         Fcb(6,Ifilex) = Fcb(6,Ifilex) - 1
      ENDIF
      Fcb(15,Ifilex) = 0
      GOTO 99999
   ELSEIF ( Opcode==3 ) THEN
!-REWIND ----------------------------
      Fcb(4,Ifilex) = 1
      Nblock = 1
!-POSITION AND READ BLOCK "NBLOCK"
      IF ( Fcb(15,Ifilex)/=701 .AND. Fcb(15,Ifilex)/=703 ) CALL dsgnrd
      GOTO 99999
   ELSEIF ( Opcode==4 ) THEN
!-WRITE -----------------------------
      CALL dsgnwr
      Fcb(4,Ifilex) = Fcb(4,Ifilex) + 1
      IF ( Fcb(4,Ifilex)>Fcb(6,Ifilex) ) Fcb(6,Ifilex) = Fcb(4,Ifilex)
      GOTO 99999
   ELSEIF ( Opcode==5 ) THEN
!-READ
      Fcb(4,Ifilex) = Fcb(4,Ifilex) + 1
      Nblock = Fcb(4,Ifilex)
      CALL dsgnrd
      GOTO 99999
   ELSEIF ( Opcode==6 ) THEN
      CALL dsgnrd
      GOTO 99999
   ELSEIF ( Opcode==7 ) THEN
!-DELETE FILE
      OPEN (Ifilex,FILE=Mdsnam(Ifilex),STATUS='UNKNOWN')
      CLOSE (Ifilex,STATUS='DELETE')
      Fcb(5,Ifilex) = 0
      Fcb(6,Ifilex) = 0
      GOTO 99999
   ELSEIF ( Opcode==8 ) THEN
!-SPECIAL WRTBLK CALL
      PRINT * , ' ERROR, DBMIO CALLED FOR WRTBLK CALL'
      STOP
!      WRITE(6,40647)(FCB(K,IFILEX),K=1,15)
99002 FORMAT (' DBMIO-EXIT,FCB=',/I3,I7,4I5,I7,I2,4I7,1X,2A4,I4)
   ELSEIF ( Opcode==9 ) THEN
      GOTO 300
   ELSE
!-OPEN ------------------------------
!     OPEN FILE ACCORDING TO IOCODE
!       =0, OPEN AND READ FIRST BLOCK
!       =1, OPEN AND RETURN ( OPEN FOR WRITE )
!       =2, OPEN AND READ THE CURRENT BLOCK WHEN FILE WAS CLOSED
!       =3, OPEN AND READ THE CURRENT BLOCK WHEN FILE WAS CLOSED
      CALL dsgnop
      Fcb(15,Ifilex) = 700 + Iocode
      Fcb(1,Ifilex) = Iocode
      IF ( Iocode==0 ) THEN
         Fcb(4,Ifilex) = Nblock
         CALL dsgnrd
         GOTO 99999
      ELSEIF ( Iocode/=1 ) THEN
         IF ( Iocode==2 ) GOTO 200
         IF ( Iocode==3 ) GOTO 200
         Fcb(4,Ifilex) = Nblock
         CALL dsgnrd
         GOTO 99999
      ENDIF
   ENDIF
 100  Fcb(4,Ifilex) = Nblock
   Fcb(5,Ifilex) = Nblock
   Fcb(6,Ifilex) = Nblock
   GOTO 99999
 200  Nblock = Fcb(4,Ifilex)
   IF ( Fcb(5,Ifilex)/=0 ) THEN
      CALL dsgnrd
      GOTO 99999
   ELSE
      Nblock = 1
      GOTO 100
   ENDIF
!-SPECIAL RDBLK CALL
 300  PRINT * , ' ERROR, DBMIO CALLED FOR RDBLK CALL'
   STOP
99999 RETURN
END SUBROUTINE dbmio