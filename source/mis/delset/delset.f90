!*==delset.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE delset
   USE c_gpta1
   USE c_system
   IMPLICIT NONE
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
      ngrids = idum(i)/10000000
      nc = mod(idum(i),10000000)/10000
      np = mod(idum(i),10000)/10
!
!     ND IS DECODE AND USED IN ROUTINES DS1 AND DS1A
!
      nd = mod(idum(i),10)
      izero = (dumtyp(i)-1)*incr
      ne(izero+6) = nc + ngrids + 2
      ne(izero+9) = np + 2
      IF ( np==0 ) ne(izero+9) = 0
      ne(izero+10) = ngrids
      n = 5*ngrids + 3 + np + nc
      ne(izero+12) = n
      ne(izero+15) = ngrids + 2
   ENDDO
END SUBROUTINE delset
