!*==/home/marcusmae/nasa/nastran/SPAGged/C_XECHOX.f90  created by SPAG 7.61RG at 01:00 on 21 Mar 2022
MODULE C_XECHOX
!
!     *****  PRINCIPAL BLOCK DATA PROGRAM FOR NASTRAN  *****
!     (NOTE - MACHINE DEPENDENT CONSTANTS ARE INITIALIZED IN BTSTRP)
!
!     REVISED 7/91 BY G.CHAN/UNISYS
!     MAKE SURE THERE IS NO VARIABLES OR ARRAYS NOT INITIALIZED. GAPS
!     OR MISSING INITIALIZED DATA MAY CAUSE PROBLEMS IN SOME MACHINES.
!
   INTEGER, DIMENSION(4) :: Iecho
   INTEGER :: Iwasff, Ixsort, Noecho
   INTEGER, DIMENSION(3) :: Ncard
!
!     --------------     /XECHOX/ AND /XREADX/     ---------------------
!
!     IECHO      = USED IN FREE-FIELD INPUT FOR INPUT CARD ECHO CONTROL
!     IXSORT,IWASFF,NCARD = USED LOCALLY AMONG XSORT, XREAD, AND FFREAD
!     NOECHO     = USED IN FFREAD AND XCSA ROUTINES
!
!     SCREEN,PROM= LOGICAL UNIT FOR TERMINAL SCREEN AND PROMPT SYMBOL
!     CONTRL NOTYET,STAR,PCT = FREE-FIELD INPUT FLAGS USED IN XREAD
!     LOOP,KOUNT = LOOP COUNT USED IN XREAD
!     ICONT      = 36 CONTROL WORDS USED IN FREE-FILED INPUT NOT TO BE
!                  DESTROYED
!
   DATA iecho , ixsort , iwasff , ncard , noecho/4*0 , 0 , 0 , 3*0 , 0/

END MODULE C_XECHOX