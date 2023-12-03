!*==/home/marcusmae/nasa/nastran/SPAGged/C_XMSSG.f90  created by SPAG 7.61RG at 01:00 on 21 Mar 2022
MODULE C_XMSSG
!
!     *****  PRINCIPAL BLOCK DATA PROGRAM FOR NASTRAN  *****
!     (NOTE - MACHINE DEPENDENT CONSTANTS ARE INITIALIZED IN BTSTRP)
!
!     REVISED 7/91 BY G.CHAN/UNISYS
!     MAKE SURE THERE IS NO VARIABLES OR ARRAYS NOT INITIALIZED. GAPS
!     OR MISSING INITIALIZED DATA MAY CAUSE PROBLEMS IN SOME MACHINES.
!
   CHARACTER(25) :: Sfm, Uwm
   CHARACTER(31) :: Sim
   CHARACTER(27) :: Swm
   CHARACTER(23) :: Ufm
   CHARACTER(29) :: Uim
!
!     -------------------     /XMSSG  /     ----------------------------
!
!     USER FATAL/WARNING/INFO AND SYSTEM FATAL/WARNING/INFO MESSAGES
!
!                               1         2         3
!                      1234567890123456789012345678901
   DATA ufm/'0*** USER FATAL MESSAGE'/
   DATA uwm/'0*** USER WARNING MESSAGE'/
   DATA uim/'0*** USER INFORMATION MESSAGE'/
   DATA sfm/'0*** SYSTEM FATAL MESSAGE'/
   DATA swm/'0*** SYSTEM WARNING MESSAGE'/
   DATA sim/'0*** SYSTEM INFORMATION MESSAGE'/
!

END MODULE C_XMSSG