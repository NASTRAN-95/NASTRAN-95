!*==/home/marcusmae/nasa/nastran/SPAGged/C_STAPID.f90  created by SPAG 7.61RG at 01:00 on 21 Mar 2022
MODULE C_STAPID
!
!     *****  PRINCIPAL BLOCK DATA PROGRAM FOR NASTRAN  *****
!     (NOTE - MACHINE DEPENDENT CONSTANTS ARE INITIALIZED IN BTSTRP)
!
!     REVISED 7/91 BY G.CHAN/UNISYS
!     MAKE SURE THERE IS NO VARIABLES OR ARRAYS NOT INITIALIZED. GAPS
!     OR MISSING INITIALIZED DATA MAY CAUSE PROBLEMS IN SOME MACHINES.
!
   INTEGER :: Idumf
   REAL, DIMENSION(6) :: Otapid, Tapid
!
!     -------------------     / STAPID /     ---------------------------
!
!     STAPID CONTAINS THE I.D. FOR THE NEW AND OLD PROBLEM TAPES.
!     TAPID  = SIX-WORD I.D. FOR NEW PROBLEM TAPE.
!     OTAPID = SIX-WORD I.D. FOR OLD PROBLEM TAPE.
!     IDUMF  = (OBSOLETE) ID FOR USER-S MASTER FILE.
!
   DATA tapid/6*0.0/ , otapid/6*0.0/
   DATA idumf/0/

END MODULE C_STAPID
