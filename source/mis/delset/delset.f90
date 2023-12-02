!*==delset.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE delset
   IMPLICIT NONE
   USE C_GPTA1
   USE C_SYSTEM
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(9) , SAVE :: dumtyp
   INTEGER :: i , izero , n , nc , nd , ngrids , np
!
! End of declarations rewritten by SPAG
!
!*****
!  THIS ROUTINE SETS VARIABLES FOR DUMMY ELEMENTS IN /GPTA1/
!
!  ALL MODULES USING /GPTA1/ SHOULD BE SURE TO CALL THIS ROUTINE
!  SO AS TO INSURE THAT DATA FOR ANY DUMMY ELEMENTS PRESENT GETS
!  INSERTED INTO /GPTA1/.
!*****
!
!
!
   DATA dumtyp/53 , 54 , 55 , 56 , 57 , 58 , 59 , 60 , 61/
!
   DO i = 1 , 9
      ngrids = Idum(i)/10000000
      nc = mod(Idum(i),10000000)/10000
      np = mod(Idum(i),10000)/10
!
!     ND IS DECODE AND USED IN ROUTINES DS1 AND DS1A
!
      nd = mod(Idum(i),10)
      izero = (dumtyp(i)-1)*Incr
      Ne(izero+6) = nc + ngrids + 2
      Ne(izero+9) = np + 2
      IF ( np==0 ) Ne(izero+9) = 0
      Ne(izero+10) = ngrids
      n = 5*ngrids + 3 + np + nc
      Ne(izero+12) = n
      Ne(izero+15) = ngrids + 2
   ENDDO
END SUBROUTINE delset
