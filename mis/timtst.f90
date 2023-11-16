
SUBROUTINE timtst
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Isysbf , M , N , Nout , O1 , O2 , T
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /blank / N , M , T , O1 , O2
   COMMON /system/ Isysbf , Nout
   COMMON /xmssg / Ufm , Uwm
!
! End of declarations
!
!
!     TIMETEST   /,/ C,N,N / C,N,M / C,N,T / C,N,O1 / C,N,O2 $
!
!
   IF ( O1<1 .OR. O1>2 ) THEN
!
!     ERROR MESSAGES
!
      WRITE (Nout,99001) Uwm
99001 FORMAT (A25,' 2195, ILLEGAL VALUE FOR P4 =',I7)
!
      WRITE (Nout,99002)
99002 FORMAT ('0*** MODULE TIMETEST TERMINAL ERROR.')
      GOTO 99999
   ELSEIF ( O1==2 ) THEN
!
      CALL timts2
   ELSE
!
      CALL timts1
   ENDIF
!
   RETURN
!
!
99999 RETURN
END SUBROUTINE timtst
