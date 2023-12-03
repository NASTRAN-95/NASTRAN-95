!*==/home/marcusmae/nasa/nastran/SPAGged/C_SOFCOM.f90  created by SPAG 7.61RG at 01:00 on 21 Mar 2022
MODULE C_SOFCOM
!
!     *****  PRINCIPAL BLOCK DATA PROGRAM FOR NASTRAN  *****
!     (NOTE - MACHINE DEPENDENT CONSTANTS ARE INITIALIZED IN BTSTRP)
!
!     REVISED 7/91 BY G.CHAN/UNISYS
!     MAKE SURE THERE IS NO VARIABLES OR ARRAYS NOT INITIALIZED. GAPS
!     OR MISSING INITIALIZED DATA MAY CAUSE PROBLEMS IN SOME MACHINES.
!
   INTEGER :: Asofcb, Nfiles, Status
   INTEGER, DIMENSION(10) :: Filnam, Filsiz
   LOGICAL :: First, Opnsof
   INTEGER, DIMENSION(2) :: Psswrd
!
!     -------------------     / SOFCOM /     ---------------------------
!
!     SOFCOM DEFINES THE NAMES AND SIZES OF THE SOF FILES AND THE STATE
!     OF THE SOF
!     NFILES = NUMBER OF FILES ALLOCATED TO THE SOF (MAX 10)
!     FILNAM = 4 CHAR. BCD NAMES OF THE SOF FILES
!     FILSIZ = SIZES OF THE SOF FILES EXPRESSED IN AN EVEN NUMBER OF
!              BLOCKS
!     STATUS = SOF STATUS.  0 - SOF IS EMPTY.  1 - SOF IS NOT EMPTY.
!     PSSWRD = BCD PASSWORD FOR THE SOF.  EACH RUN USING THE SAME SOF
!              MUST USE THE SAME PASSWORD.
!     FIRST  = .TRUE. IF SOFINT HAS NOT YET BEEN CALLED TO INITIALIZE
!              THE SOF FOR THIS RUN.  OTHERWISE .FALSE.
!     OPNSOF = .TRUE. IF THE SOF IS OPEN.  .FALSE. IF IT IS CLOSED.
!     ASOFCB = ADDRESS OF SOF CONTROL BLOCKS ON IBM 360/370 COMPUTERS
!
   DATA nfiles/1/
   DATA filnam/4HINPT , 9*0/
   DATA filsiz/100 , 9*0/
   DATA status/1/
   DATA psswrd/2*4H    /
   DATA first/.TRUE./
   DATA opnsof/.FALSE./
   DATA asofcb/0/

END MODULE C_SOFCOM