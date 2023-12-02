!*==skpfil.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE skpfil(File,N)
   USE i_dsiof
   USE I_DSIOF
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INTEGER File , N
   IF ( N/=0 ) THEN
      name = File
      CALL dsgefl
      irword = N
      IF ( N>0 ) THEN
         IF ( iprvop/=0 ) CALL dsmsg(4)
         CALL dsskff(N)
      ELSEIF ( (indclr-indbas)/=5 ) THEN
         CALL dsskfb(N)
      ELSEIF ( nblock/=1 ) THEN
         CALL dsskfb(N)
      ENDIF
      CALL dssdcb
   ENDIF
END SUBROUTINE skpfil
