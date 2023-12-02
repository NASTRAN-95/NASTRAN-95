!*==curcas.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE curcas(Nskip,Trl,Mcb,Zz,Ibuf) !HIDESTARS (*,Nskip,Trl,Mcb,Zz,Ibuf)
   IMPLICIT NONE
   USE C_NAMES
   USE C_SYSTEM
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nskip
   INTEGER , DIMENSION(7) :: Trl
   INTEGER , DIMENSION(7) :: Mcb
   INTEGER , DIMENSION(1) :: Zz
   INTEGER :: Ibuf
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: count , i , ibf2 , icnt , j , krw
   INTEGER , DIMENSION(4) , SAVE :: parm
   REAL :: rcnt
   EXTERNAL close , cpyfil , eof , fwdrec , mesage , open , rdtrl , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   !>>>>EQUIVALENCE (icnt,rcnt)
   DATA parm(3) , parm(4)/4HCURC , 2HAS/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ELSE
!
               CALL rdtrl(Trl)
               IF ( Trl(1)<=0 ) GOTO 20
               IF ( Trl(2)<=i ) THEN
                  parm(1) = +7
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  CALL open(*20,Trl(1),Zz(ibf2),Irdrw)
                  parm(2) = Mcb(1)
                  CALL open(*20,Mcb(1),Zz(Ibuf),Iwtrw)
                  CALL write(Mcb(1),Mcb(1),2,1)
                  parm(2) = Trl(1)
                  CALL fwdrec(*40,Trl(1))
!
                  Mcb(2) = Trl(2) - i
                  Mcb(3) = Trl(3)
                  Mcb(4) = Trl(4)
                  Mcb(5) = Trl(5)
                  Mcb(6) = Trl(6)
                  DO j = 1 , i
                     CALL fwdrec(*40,Trl(1))
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
 20      parm(1) = +1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 40      parm(1) = +2
         spag_nextblock_1 = 2
      CASE (2)
!
         CALL mesage(parm(1),parm(2),parm(3))
         RETURN 1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE curcas
