!*==dsskfb.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dsskfb(Nn)
   USE I_DSIOF
   USE I_XNSTRN
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER Nn
   INTEGER id , n
   n = Nn
   SPAG_Loop_1_2: DO WHILE ( n/=0 )
      SPAG_Loop_2_1: DO
         CALL dsbrc1
         id = iand(ibase(indclr),maskq1)
         IF ( id==idsef ) THEN
            n = n + 1
            EXIT SPAG_Loop_2_1
         ELSEIF ( nblock==1 ) THEN
            IF ( (indclr-indbas)<=5 ) EXIT SPAG_Loop_1_2
         ENDIF
      ENDDO SPAG_Loop_2_1
   ENDDO SPAG_Loop_1_2
END SUBROUTINE dsskfb
