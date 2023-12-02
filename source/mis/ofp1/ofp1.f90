!*==ofp1.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE ofp1
   IMPLICIT NONE
   USE c_system
   USE c_zzzzzz
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , iflag , line , linet , nout
   INTEGER , DIMENSION(50) :: id
   INTEGER , DIMENSION(5) :: l123
   INTEGER , DIMENSION(6) :: of
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
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
   iflag = 1
   DO i = 1 , 5
      line = l123(i)
      IF ( line<0 ) THEN
      ELSEIF ( line==0 ) THEN
!
         WRITE (nout,99001)
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
   linet = linet + 4
END SUBROUTINE ofp1
