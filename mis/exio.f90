
SUBROUTINE exio
   IMPLICIT NONE
   REAL Datype(2) , Pdate , Ptime , Sysbuf , Xmach
   INTEGER Device(2) , Dry , Format(2) , Head1(96) , Head2(96) , Inbcds(2,5) , Mode(2) , Names(10) , Nout , Pos(2) , Uname(2)
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /blank / Dry , Xmach , Device , Uname , Format , Mode , Pos , Datype , Names , Pdate , Ptime
   COMMON /output/ Head1 , Head2
   COMMON /system/ Sysbuf , Nout
   COMMON /xmssg / Ufm , Uwm
   INTEGER bcds(2,10) , blank , exte , i , inte , j
!
!     THE MAIN PURPOSE OF THIS MODULE IS TO COPY DATA BETWEEN THE
!     RESIDENT SOF AND AN EXTERNAL TAPE OR DISK FILE.  AS AN EXTRA
!     ADDED ATTRACTION, IT WILL ALSO APPEND AN EXTERNAL SOF (CREATED BY
!     SOME OTHER NASTRAN RUNS) TO THE RESIDENT SOF AND COMPRESS THE
!     RESIDENT SOF.
!
!     OPTIONS ARE -
!
!     (1) DUMP (RESTORE) THE ENTIRE SOF TO (FROM) AN EXTERNAL FILE.
!         INTERNAL FORM ONLY.  THIS IS THE MOST EFFICIENT MEANS TO SAVE
!         OR RECOVER A BACKUP COPY OF THE SOF, EXCEPT FOR SYSTEM UTILITY
!         PROGRAMS.
!
!     (2) COPY SELECTED ITEMS BETWEEN THE SOF AND AN EXTERNAL FILE.
!
!     (3) CHECK THE EXTERNAL FILE AND PRINT OUT A LIST OF ALL SUBSTRUC-
!         TURES AND ITEMS ON IT ALONG WITH THE DATE AND TIME EACH WAS
!         CREATED.
!
!     (4) APPEND AN EXTERNAL SOF TO THE RESIDENT SOF.
!
!     (5) COMPRESS THE RESIDENT SOF. (PLACE ALL ITEMS IN CONTIGUOUS
!         BLOCKS ON THE SOF AND ELIMINATE ALL EMBEDDED FREE BLOCKS)
!
!     FEBRUARY 1974
!
   EQUIVALENCE (inte,bcds(1,7)) , (exte,bcds(1,8)) , (Device(1),Inbcds(1,1))
   DATA blank/4H    /
   DATA bcds/4HSOFI , 4HN    , 4HSOFO , 4HUT   , 4HREST , 4HORE  , 4HCHEC , 4HK    , 4HCOMP , 4HRESS , 4HAPPE , 4HND   , 4HINTE ,   &
       &4HRNAL , 4HEXTE , 4HRNAL , 4HREWI , 4HND   , 4HNORE , 4HWIND/
!
   DO i = 1 , 96
      Head2(i) = blank
   ENDDO
   DO i = 1 , 5
      DO j = 1 , 10
         IF ( Inbcds(1,i)==bcds(1,j) ) THEN
            Inbcds(2,i) = bcds(2,j)
            EXIT
         ENDIF
      ENDDO
   ENDDO
!
   DO i = 1 , 2
      Head2(i) = Mode(i)
      Head2(i+3) = Format(i)
      Head2(i+6) = Device(i)
      Head2(i+9) = Uname(i)
      Head2(i+12) = Pos(i)
   ENDDO
!
!     INTERNAL FORMAT - GINO I/O IS USED FOR DATA WHICH WILL BE READ OR
!                       WAS WRITTEN ON THE SAME HARDWARE.
!
   IF ( Format(1)==inte ) CALL exio1
!
!     EXTERNAL FORMAT - FORTRAN I/O IS USED FOR DATA WHICH WILL BE READ
!                       OR WAS WRITTEN ON A DIFFERENT MACHINE.
!
   IF ( Format(1)==exte ) CALL exio2
!
!     CHECK VALIDITY OF FORMAT TO ASCERTAIN WHETHER EITHER EXIO1 OR
!     EXIO2 WAS CALLED.
!
   IF ( Format(1)==inte .OR. Format(1)==exte ) RETURN
   WRITE (Nout,99001) Uwm , Format
99001 FORMAT (A25,' 6333, ',2A4,' IS AN INVALID FORMAT PARAMETER FOR ','MODULE EXIO.')
   Dry = -2
END SUBROUTINE exio
