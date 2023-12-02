!*==dbmio.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dbmio(Opcode)
   USE I_DSIOF
   USE I_GINOX
   USE I_XNSTRN
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
      IF ( iocode==0 ) fcb(4,ifilex) = 1
      IF ( fcb(15,ifilex)==701 .OR. fcb(15,ifilex)==703 ) THEN
         fcb(4,ifilex) = fcb(4,ifilex) - 1
         fcb(6,ifilex) = fcb(6,ifilex) - 1
      ENDIF
      fcb(15,ifilex) = 0
      RETURN
   ELSEIF ( Opcode==3 ) THEN
!-REWIND ----------------------------
      fcb(4,ifilex) = 1
      nblock = 1
!-POSITION AND READ BLOCK "NBLOCK"
      IF ( fcb(15,ifilex)/=701 .AND. fcb(15,ifilex)/=703 ) CALL dsgnrd
      RETURN
   ELSEIF ( Opcode==4 ) THEN
!-WRITE -----------------------------
      CALL dsgnwr
      fcb(4,ifilex) = fcb(4,ifilex) + 1
      IF ( fcb(4,ifilex)>fcb(6,ifilex) ) fcb(6,ifilex) = fcb(4,ifilex)
      RETURN
   ELSEIF ( Opcode==5 ) THEN
!-READ
      fcb(4,ifilex) = fcb(4,ifilex) + 1
      nblock = fcb(4,ifilex)
      CALL dsgnrd
      RETURN
   ELSEIF ( Opcode==6 ) THEN
      CALL dsgnrd
      RETURN
   ELSEIF ( Opcode==7 ) THEN
!-DELETE FILE
      OPEN (ifilex,FILE=mdsnam(ifilex),STATUS='UNKNOWN')
      CLOSE (ifilex,STATUS='DELETE')
      fcb(5,ifilex) = 0
      fcb(6,ifilex) = 0
      RETURN
   ELSEIF ( Opcode==8 ) THEN
!-SPECIAL WRTBLK CALL
      PRINT * , ' ERROR, DBMIO CALLED FOR WRTBLK CALL'
      STOP
!      WRITE(6,40647)(FCB(K,IFILEX),K=1,15)
99002 FORMAT (' DBMIO-EXIT,FCB=',/I3,I7,4I5,I7,I2,4I7,1X,2A4,I4)
   ELSEIF ( Opcode==9 ) THEN
      CALL spag_block_3
      RETURN
   ELSE
!-OPEN ------------------------------
!     OPEN FILE ACCORDING TO IOCODE
!       =0, OPEN AND READ FIRST BLOCK
!       =1, OPEN AND RETURN ( OPEN FOR WRITE )
!       =2, OPEN AND READ THE CURRENT BLOCK WHEN FILE WAS CLOSED
!       =3, OPEN AND READ THE CURRENT BLOCK WHEN FILE WAS CLOSED
      CALL dsgnop
      fcb(15,ifilex) = 700 + iocode
      fcb(1,ifilex) = iocode
      IF ( iocode==0 ) THEN
         fcb(4,ifilex) = nblock
         CALL dsgnrd
         RETURN
      ELSEIF ( iocode/=1 ) THEN
         IF ( iocode==2 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         IF ( iocode==3 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         fcb(4,ifilex) = nblock
         CALL dsgnrd
         RETURN
      ENDIF
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      fcb(4,ifilex) = nblock
      fcb(5,ifilex) = nblock
      fcb(6,ifilex) = nblock
      RETURN
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
      nblock = fcb(4,ifilex)
      IF ( fcb(5,ifilex)/=0 ) THEN
         CALL dsgnrd
         RETURN
      ELSE
         nblock = 1
         CALL spag_block_1
         RETURN
      ENDIF
   END SUBROUTINE spag_block_2
   SUBROUTINE spag_block_3
!-SPECIAL RDBLK CALL
      PRINT * , ' ERROR, DBMIO CALLED FOR RDBLK CALL'
      STOP
   END SUBROUTINE spag_block_3
END SUBROUTINE dbmio
