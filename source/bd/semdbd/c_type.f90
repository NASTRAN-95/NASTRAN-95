!*==/home/marcusmae/nasa/nastran/SPAGged/C_TYPE.f90  created by SPAG 7.61RG at 01:00 on 21 Mar 2022
MODULE C_TYPE
!
!     *****  PRINCIPAL BLOCK DATA PROGRAM FOR NASTRAN  *****
!     (NOTE - MACHINE DEPENDENT CONSTANTS ARE INITIALIZED IN BTSTRP)
!
!     REVISED 7/91 BY G.CHAN/UNISYS
!     MAKE SURE THERE IS NO VARIABLES OR ARRAYS NOT INITIALIZED. GAPS
!     OR MISSING INITIALIZED DATA MAY CAUSE PROBLEMS IN SOME MACHINES.
!
   INTEGER, DIMENSION(4) :: Nwds, Rc
   INTEGER, DIMENSION(2) :: Prc
   REAL, DIMENSION(6) :: X
!
!     -------------------     / TYPE   /     ---------------------------
!
!     TYPE DEFINES PROPERTIES AS A FUNCTION OF ARITHMETIC TYPE.
!     PRC    = PRECISION (1=SP, 2=DP).
!     NWDS   = NO. OF WORDS PER ELEMENT.
!     RC     = ARITHMETIC (1=REAL, 2=COMPLEX).
!     X      = PAD TO DEFINE WORK AREA.
!
   DATA prc/1 , 2/ , nwds/1 , 2 , 2 , 4/ , rc/1 , 1 , 2 , 2/ , x/6*0.0/

END MODULE C_TYPE
