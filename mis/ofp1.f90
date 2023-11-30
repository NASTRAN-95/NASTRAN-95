
SUBROUTINE ofp1
   IMPLICIT NONE
   REAL Core(1)
   INTEGER Id(50) , Iflag , Ksys(100) , L123(5) , Linet , Nout , Of(6)
   COMMON /system/ Ksys
   COMMON /zzzzzz/ Core
   INTEGER i , line
!
!     THIS ROUTINE OUTPUTS A PAGE HEADING BASED ON PARAMETERS COMING
!     THROUGH COMMON.
!     THIS ROUTINE CALLS OPF1A, OFP1B OR OFP1C FOR ACTUAL PRINTING, SUCH
!     THAT OFP1A, OFP1B AND OFP1C CAN BE OVERLAYED IN PARALLEL.
!
   !>>>>EQUIVALENCE (Core(1),Of(1),L123(1)) , (Id(1),Of(6)) , (Nout,Ksys(2)) , (Linet,Ksys(12)) , (Iflag,Ksys(33))
!
!     IFLAG IS WORD 33 OF /SYSTEM/ AND IS SET TO INCIDATE OFP PRINTED
!     LAST.
!
   CALL page1
   Iflag = 1
   DO i = 1 , 5
      line = L123(i)
      IF ( line<0 ) THEN
      ELSEIF ( line==0 ) THEN
!
         WRITE (Nout,99001)
99001    FORMAT (1H )
      ELSEIF ( line>174 ) THEN
         IF ( line>380 ) THEN
!
! ... 381 UP -
!
            CALL ofp1c(line)
         ELSE
!
! ... 175 THRU 380 -
!
            CALL ofp1b(line)
         ENDIF
      ELSE
!
! ... 1 THRU 174-
!
         CALL ofp1a(line)
      ENDIF
!
   ENDDO
   Linet = Linet + 4
END SUBROUTINE ofp1