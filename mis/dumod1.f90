
SUBROUTINE dumod1
   IMPLICIT NONE
   INTEGER Parm1 , Parm2 , Parm3 , Parm4 , Parm5
   COMMON /blank / Parm1 , Parm2 , Parm3 , Parm4 , Parm5
!
!
!     DUMMY DECK FOR MODULE DUMMOD1 - SEE USER'S MANUAL SECTION 5.6.
!                                     FOR MODULE PROPERTIES, CHECK
!                                     SUBROUTINE XMPLDD OR USE DIAG 31.
!
!     REVISED 5/91  BY G.CHAN/UNISYS
!     THIS DUMMY MODULE IS IN LINK3 ONLY (SEE XLNKDD). IT CAN BE USED TO
!     FORCE NASTRAN TO DO A LINK SWITCHING AND THEN CONTINUE. e.g.
!     MODULE XXX (NOT IN LINK3) GETS INTO TROUBLE WITH SFM 3018
!     (REQUIREMENTS EXCEED AVAILABLE FILES). USE DMAP ALTER TO CALL THIS
!     DUMOD1 MODLUE A STEP AHEAD OF MODULE XXX. THUS NASTRAN IS FORCED
!     TO DO A LINKSWITCH AND HOUSEKEEPING MODULE XSFA IS CALLED TO CLEAN
!     UP THE FILE ALLOCATION TABLE. THIS MAY SOLVE THE SFM 3018 PROBLEM.
!
!
!     INTEGER INFILE(1),OUTFIL(2),SCRFIL(3)
!
!     COMMON /ZZZZZZ/ X(1)
!
!     DATA INFILE /101/
!     DATA OUTFIL /201,202/
!     DATA SCRFIL /301,302,303/
!
END SUBROUTINE dumod1
