
SUBROUTINE delset
   IMPLICIT NONE
   INTEGER Idum(9) , Incr , Last , Ne(1) , Nelem , Nskip(45)
   COMMON /gpta1 / Nelem , Last , Incr , Ne
   COMMON /system/ Nskip , Idum
   INTEGER dumtyp(9) , i , izero , n , nc , nd , ngrids , np
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