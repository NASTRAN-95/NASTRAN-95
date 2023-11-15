
SUBROUTINE cpystr(Inblk,Outblk,Flag,Col)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Prc(2) , Rlcmpx(4) , Words(4)
   DOUBLE PRECISION Xnd(1)
   REAL Xns(1)
   COMMON /type  / Prc , Words , Rlcmpx
   COMMON /zzzzzz/ Xns
!
! Dummy argument declarations
!
   INTEGER Col , Flag
   INTEGER Inblk(15) , Outblk(15)
!
! Local variable declarations
!
   INTEGER in , jout , nprev , nstr , out , prec , rc , type
!
! End of declarations
!
!
!     CPYSTR COPIES A LOGICAL RECORD WRITTEN IN STRING FORMAT
!     FROM ONE FILE TO ANOTHER FILE.
!
!     INBLK  = 15-WORD STRING COMMUNICATION BLOCK FOR INPUT FILE
!     OUTBLK = 15-WORD STRING COMMUNICATION BLOCK FOR OUTPUT FILE
!     FLAG .NE. 0 MEANS 1ST CALL GETSTR HAS BEEN MADE FOR THE RECORD
!          .EQ. 0 MEANS 1ST CALL GETSTR HAS NOT BEEN MADE
!     COL .EQ. 0 MEANS COLUMN NUMBER IS IN INBLK(12)
!         .NE. 0 MEANS COL IS COLUMN NUMBER
!
!
   EQUIVALENCE (Xns(1),Xnd(1))
!
!     ON OPTION, MAKE 1ST CALL TO GETSTR AND THEN INITIALIZE
!
   IF ( Flag==0 ) THEN
      Inblk(8) = -1
      CALL getstr(*300,Inblk)
   ENDIF
   Outblk(2) = Inblk(2)
   Outblk(3) = Inblk(3)
   Outblk(4) = Inblk(4)
   Outblk(8) = -1
   Outblk(12) = Col
   IF ( Col==0 ) Outblk(12) = Inblk(12)
   Outblk(13) = 0
   type = Inblk(2)
   prec = Prc(type)
   rc = Rlcmpx(type)
!
!     COPY A STRING
!
 100  CALL putstr(Outblk)
   nprev = 0
   Outblk(7) = min0(Inblk(6),Outblk(6))
   DO
      in = Inblk(5)
      out = Outblk(5)
      nstr = out + rc*(Outblk(7)-nprev) - 1
      IF ( prec==2 ) THEN
!
         DO jout = out , nstr
            Xnd(jout) = Xnd(in)
            in = in + 1
         ENDDO
      ELSE
!
         DO jout = out , nstr
            Xns(jout) = Xns(in)
            in = in + 1
         ENDDO
      ENDIF
!
!     TEST FOR END OF INPUT STRING(S)
!
      IF ( Outblk(7)==Inblk(6)+nprev ) THEN
!
!     INPUT STRING HAS BEEN COPIED.  GET ANOTHER STRING.
!
         CALL endget(Inblk)
         CALL getstr(*200,Inblk)
!
!     TEST FOR STRING CONTIGUOUS WITH PREVIOUS STRING.
!     IF SO, AND IF TERMS AVAILABLE, CONCATENATE WITH PREVIOUS STRING.
!
         IF ( Inblk(4)==Outblk(4)+Outblk(7) ) THEN
            IF ( Outblk(7)<Outblk(6) ) THEN
               Outblk(5) = nstr + 1
               nprev = Outblk(7)
               Outblk(7) = min0(Outblk(7)+Inblk(6),Outblk(6))
               CYCLE
            ENDIF
         ENDIF
         Outblk(13) = Outblk(13) + Outblk(7)
         CALL endput(Outblk)
         Outblk(4) = Inblk(4)
      ELSE
         Outblk(13) = Outblk(13) + Outblk(7)
         CALL endput(Outblk)
         Outblk(4) = Outblk(4) + Outblk(7)
         Inblk(6) = Inblk(6) - (Outblk(7)-nprev)
         Inblk(5) = in
      ENDIF
      GOTO 100
   ENDDO
!
!     NO MORE STRINGS -  CLOSE RECORD AND RETURN
!
 200  Outblk(8) = 1
   CALL endput(Outblk)
   Outblk(13) = (Outblk(13)+Outblk(7))*Words(type)
   RETURN
!
!     HERE IF NO STRINGS IN RECORD - MAKE A NULL RECORD
!
 300  Outblk(2) = 1
   Outblk(3) = 0
   Outblk(8) = -1
   CALL putstr(Outblk)
   Outblk(8) = 1
   CALL endput(Outblk)
END SUBROUTINE cpystr
