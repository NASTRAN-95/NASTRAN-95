!*==/home/marcusmae/nasa/nastran/SPAGged/C_IFPX0.f90  created by SPAG 7.61RG at 01:00 on 21 Mar 2022
MODULE C_IFPX0
!IFX1BD
!     DEFINITION OF VARIABLES IN /IFPX1/ AND /IFPX0/
!*****
!
!     COMMON /IFPX1/
!     --------------
!
!     N          = TOTAL NUMBER OF PAIRED ENTRIES IN THE IBD AND
!                  IPR ARRAYS
!                = (TOTAL DIMENSION OF IBD + ACTIVE IPR ARRAYS)/2
!
!     IBD ARRAYS = ARRAYS CONTAINING PAIRED ENTRIES OF BULK DATA
!                  CARD NAMES
!
!     IPR ARRAYS = ARRAYS CONTAINING PAIRED ENTRIES OF BULK DATA
!                  PARAMETER NAMES
!
!     CAUTION 1 -- THE TOTAL DIMENSION OF THE IBD AND IPR ARRAYS
!                  MUST BE A MULTIPLE OF 62 (OR, IN OTHER WORDS,
!                  AN EVEN MULTIPLE OF 31)
!
!                  SEE NOTES 1 AND 2 BELOW
!
!     ICC ARRAYS = ARRAYS CONTAINING PAIRED ENTRIES OF CASE CONTROL
!                  FLAG NAMES FOR USE IN RESTART RUNS
!
!     CAUTION 2 -- THE TOTAL DIMENSION OF THE ICC ARRAYS MUST BE A
!                  MULTIPLE OF 62 (OR, IN OTHER WORDS, AN EVEN
!                  MULTIPLE OF 31)
!
!                  SEE NOTE 3 BELOW
!
!     NOTES
!     -----
!
!              1.  IF NEW BULK DATA CARD NAMES ARE TO BE ADDED,
!                  USE THE EXISTING PADDING WORDS (OF THE 4H****
!                  TYPE) IN THE IBD ARRAYS.  IF NECESSARY, EXPAND
!                  THE IBD ARRAYS KEEPING CAUTION 1 IN MIND.
!
!              2.  IF NEW BULK DATA PARAMETER NAMES ARE TO BE ADDED,
!                  USE THE EXISTING PADDING WORDS (OF THE 4H****
!                  TYPE) IN THE IPR ARRAYS.  IF NECESSARY, EXPAND
!                  THE IPR ARRAYS KEEPING CAUTION 1 IN MIND.
!
!              3.  IF NEW CASE CONTROL FLAG NAMES ARE TO BE ADDED,
!                  USE THE EXISTING PADDING WORDS (OF THE 4H****
!                  TYPE) IN THE ICC ARRAYS.  IF NECESSARY, EXPAND
!                  THE ICC ARRAYS KEEPING CAUTION 2 IN MIND.
!
!              4.  THE IBD ARRAYS ARE IN SYSCHRONIZTION WITH THE I ARRAY
!                  IN IFX2BD, IFX3BD, IFX4BD, IFX5BD, AND IFX6BD
!                  (E.G. CONM1 POSITIONS IN IBD2, CONTINUATION 3, THE DA
!                  FOR CONM1 IN IFX2BD IS IN I2, CONTINUATION 3 CARD)
!*****
!
   INTEGER :: Iparpt, Lbdpr, Lcc
   INTEGER, DIMENSION(18) :: Iwrds
!
!*****
!     INITIALIZATION OF VARIABLES IN COMMON /IFPX0/
!*****
!
!     THE VALUES ASSIGNED BELOW TO THE VARIABLES IN COMMON /IFPX0/
!     ARE AS PER THEIR DEFINITIONS GIVEN EARLIER IN THE COMMENTS
!     AND ARE DERIVED FROM THE COMMON /IFPX1/ INFORMATION
!*****
   DATA lbdpr , lcc , iwrds , iparpt/16 , 2 , 18*0 , 401/
!
END MODULE C_IFPX0