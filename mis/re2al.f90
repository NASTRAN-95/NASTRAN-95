
SUBROUTINE re2al(Re,Alph)
   IMPLICIT NONE
   REAL Dummy(37)
   INTEGER Ibuf , Nbpw , Nout
   COMMON /system/ Ibuf , Nout , Dummy , Nbpw
   REAL Re
   INTEGER Alph(2)
   INTEGER lshift
   EXTERNAL lshift
!
!
   CALL fp2a8(*200,Re,Alph)
   IF ( Nbpw<60 ) THEN
   ELSEIF ( Nbpw==60 ) THEN
!
!     FOR 60- OR 64- BIT MACHINES, SAVE THE SECOND HALF OF REAL NUMBER
!     IN THE SECOND ALPH WORD. THAT IS -
!     THE FULL REAL NUMBER IS IN ALPH(1), ALL 8 BYTES, OR
!     FIRST 4 BYTES IN ALPH(1), AND LAST 4 BYTES IN ALPH(2)
!
      Alph(2) = lshift(Alph(1),24)
   ELSE
      Alph(2) = lshift(Alph(1),32)
   ENDIF
 100  RETURN
!
 200  WRITE (Nout,99001)
99001 FORMAT (99X,'(IN FP2A8, CALLED FROM RE2AL)')
   CALL mesage(-61,0,0)
   GOTO 100
END SUBROUTINE re2al
