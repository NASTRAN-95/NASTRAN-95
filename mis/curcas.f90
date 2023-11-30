
SUBROUTINE curcas(*,Nskip,Trl,Mcb,Zz,Ibuf)
   IMPLICIT NONE
   INTEGER Ird , Irdrw , Isbz , Iwt , Iwtrw , Knerw , Knrw , Krew
   COMMON /names / Ird , Irdrw , Iwt , Iwtrw , Krew , Knrw , Knerw
   COMMON /system/ Isbz
   INTEGER Ibuf , Nskip
   INTEGER Mcb(7) , Trl(7) , Zz(1)
   INTEGER count , i , ibf2 , icnt , j , krw , parm(4)
   REAL rcnt
!   THIS SUBROUTINE COPIES MATRIX FILE TRL(1) TO FILE MCB(1)
! SKIPPING NSKIP-1 MATRIX COLUMNS.  PRIMARY USE IS TO CREATE A MATRIX
! THAT INCLUDES ONLY SUBCASES IN THE CURRENT DMAP LOOP.
!   ALL FILES ARE OPENED, CLOSED AND TRIALERS WRITTEN.
!   IF NSKIP WOULD RESULT IN NO-COPY, MCB(1) IS SET TO TRL(1).
!     TRL - INPUT TRAILER FOR FILE BEING CONVERTED.
!     MCB - OUTPUT TRAILER - WORD 1 HAS GINO FILE NAME.
!     ZZ  - OPEN CORE.
!     IBUF- LOCATION OF TWO GINO BUFFERS.
!     NSKIP - ONE MORE THAN THE SUBCASES TO SKIP.
!       * - NONSTANDARD RETURN IF UNABLE TO PROCESS.
!-----
!
   EQUIVALENCE (icnt,rcnt)
   DATA parm(3) , parm(4)/4HCURC , 2HAS/
!
   parm(2) = Trl(1)
   IF ( Nskip<=1 ) THEN
      Mcb(1) = Trl(1)
   ELSE
!  . FOR STATICS THE NUMBER OF SUBCASES SKIPPED = NO. COLUMNS SKIPPED.
!  .  OTHER ANALYSIS TYPES NEED TO SUPPLY PROPER VALUE FOR NSKIP...
      i = Nskip - 1
      ibf2 = Ibuf + Isbz
      IF ( Ibuf<=0 ) THEN
         parm(1) = +8
         GOTO 300
      ELSE
!
         CALL rdtrl(Trl)
         IF ( Trl(1)<=0 ) GOTO 100
         IF ( Trl(2)<=i ) THEN
            parm(1) = +7
            GOTO 300
         ELSE
            CALL open(*100,Trl(1),Zz(ibf2),Irdrw)
            parm(2) = Mcb(1)
            CALL open(*100,Mcb(1),Zz(Ibuf),Iwtrw)
            CALL write(Mcb(1),Mcb(1),2,1)
            parm(2) = Trl(1)
            CALL fwdrec(*200,Trl(1))
!
            Mcb(2) = Trl(2) - i
            Mcb(3) = Trl(3)
            Mcb(4) = Trl(4)
            Mcb(5) = Trl(5)
            Mcb(6) = Trl(6)
            DO j = 1 , i
               CALL fwdrec(*200,Trl(1))
            ENDDO
            CALL cpyfil(Trl,Mcb,Zz,Ibuf-1,count)
            rcnt = count
            Mcb(7) = icnt
            CALL eof(Mcb)
!
            CALL close(Trl(1),krw)
            CALL close(Mcb(1),krw)
            CALL wrttrl(Mcb(1))
         ENDIF
      ENDIF
   ENDIF
   RETURN
!
!  . ERROR MESSAGES...
!
 100  parm(1) = +1
   GOTO 300
 200  parm(1) = +2
!
 300  CALL mesage(parm(1),parm(2),parm(3))
   RETURN 1
END SUBROUTINE curcas
