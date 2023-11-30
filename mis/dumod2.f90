
SUBROUTINE dumod2
   IMPLICIT NONE
   INTEGER Parm1 , Parm2 , Parm3 , Parm4 , Parm7(2)
   DOUBLE PRECISION Parm10(2) , Parm8
   REAL Parm5 , Parm6 , X(1)
   COMPLEX Parm9
   COMMON /blank / Parm1 , Parm2 , Parm3 , Parm4 , Parm5 , Parm6 , Parm7 , Parm8 , Parm9 , Parm10
   COMMON /zzzzzz/ X
!
!*****
!
!     DUMMY DECK FOR MODULE DUMMOD2 - SEE USER'S MANUAL SECTION 5.6.
!                                     FOR MODULE PROPERTIES, CHECK
!                                     SUBROUTINE XMPLDD OR USE DIAG 31.
!
!*****
!
!
!
!     INTEGER INFILE(8),OUTFIL(8),SCRFIL(10)
!
!
!     DATA INFILE /101,102,103,104,105,106,107,108/
!     DATA OUTFIL /201,202,203,204,205,206,207,208/
!     DATA SCRFIL /301,302,303,304,305,306,307,308,309,310/
!
END SUBROUTINE dumod2