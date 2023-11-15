
SUBROUTINE sdr3
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER L
   REAL Sysbuf
   COMMON /system/ Sysbuf , L
!
! Local variable declarations
!
   INTEGER i , ifile , ofpfil(6)
!
! End of declarations
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
         WRITE (L,99001) i , ofpfil(i)
99001    FORMAT (1H1,20(131(1H*)/),                                                                                                 &
                &95H0DUE TO ERRORS MENTIONED PREVIOUSLY, SDR3 IS CALLING THE -OFP- TO OUTPUT SDR3-INPUT-DATA-BLOCK-,I2,             &
                &17H IN SORT-I FORMAT/28H THE SDR3 TRACEBACK NUMBER =,I3//20(131(1H*)/))
         ifile = i + 100
         CALL ofpdmp(ifile)
      ENDIF
   ENDDO
END SUBROUTINE sdr3
