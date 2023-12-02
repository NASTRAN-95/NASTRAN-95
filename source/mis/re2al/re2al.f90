!*==re2al.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE re2al(Re,Alph)
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: Re
   INTEGER , DIMENSION(2) :: Alph
   EXTERNAL fp2a8 , lshift , mesage
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
         CALL fp2a8(*20,Re,Alph)
         IF ( nbpw<60 ) THEN
         ELSEIF ( nbpw==60 ) THEN
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
         spag_nextblock_1 = 2
      CASE (2)
         RETURN
!
 20      WRITE (nout,99001)
99001    FORMAT (99X,'(IN FP2A8, CALLED FROM RE2AL)')
         CALL mesage(-61,0,0)
         spag_nextblock_1 = 2
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE re2al
