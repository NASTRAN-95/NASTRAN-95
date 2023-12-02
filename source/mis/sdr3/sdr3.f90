!*==sdr3.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdr3
   USE c_system
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ifile
   INTEGER , DIMENSION(6) :: ofpfil
   EXTERNAL ofpdmp , sdr3a
!
! End of declarations rewritten by SPAG
!
!
!*****
!  MAIN DRIVER FOR THE SDR3 MODULE...
!*****
   CALL sdr3a(ofpfil(1))
!*****
!  IF ANY OF THE SIX DATA-BLOCKS DID NOT COMPLETE SORT2 CALL OFPDMP
!*****
   DO i = 1 , 6
      IF ( ofpfil(i)/=0 ) THEN
         WRITE (l,99001) i , ofpfil(i)
99001    FORMAT (1H1,20(131(1H*)/),                                                                                                 &
                &95H0DUE TO ERRORS MENTIONED PREVIOUSLY, SDR3 IS CALLING THE -OFP- TO OUTPUT SDR3-INPUT-DATA-BLOCK-,I2,             &
                &17H IN SORT-I FORMAT/28H THE SDR3 TRACEBACK NUMBER =,I3//20(131(1H*)/))
         ifile = i + 100
         CALL ofpdmp(ifile)
      ENDIF
   ENDDO
END SUBROUTINE sdr3
