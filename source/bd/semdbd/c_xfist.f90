!*==/home/marcusmae/nasa/nastran/SPAGged/C_XFIST.f90  created by SPAG 7.61RG at 01:00 on 21 Mar 2022
MODULE C_XFIST
!
!     *****  PRINCIPAL BLOCK DATA PROGRAM FOR NASTRAN  *****
!     (NOTE - MACHINE DEPENDENT CONSTANTS ARE INITIALIZED IN BTSTRP)
!
!     REVISED 7/91 BY G.CHAN/UNISYS
!     MAKE SURE THERE IS NO VARIABLES OR ARRAYS NOT INITIALIZED. GAPS
!     OR MISSING INITIALIZED DATA MAY CAUSE PROBLEMS IN SOME MACHINES.
!
   INTEGER, DIMENSION(112) :: Fist
   INTEGER :: Lfist, Nfist
!
!     -------------------     / XFIST  /     ---------------------------
!
!     XFIST IS THE FILE STATUS TABLE (FIST).
!     NFIST  = TOTAL NO. OF ENTRIES IN FIST.
!     LFIST  = NO. OF ENTRIES IN THE CURRENT FIST.
!     FIST   = TABLE OF TWO-WORD ENTRIES.
!              FIRST WORD IS GINO FILE NAME.
!              SECOND WORD POINTS TO XFIAT IF .GT. 0 (I.E. NON-PERMANENT
!              ENTRY), OR POINTS TO XXFIAT IF .LE. 0 (I.E. PERMANENT
!              ENTRY). SIGN BIT MUST BE SET FOR ZERO POINTER ON 7094.
!
!     USE VALUES BELOW WHEN ICFIAT (24TH WORD OF /SYSTEM/) IS 8
!    6           201,  3,   202, 11,   203, 19,   204, 27,   205, 35,
!    7        4HCASE, 43,   207, 51,4HPCDB, 59,   208, 67,   209, 75,
!    8           210, 83,   211, 91,   213, 99,   214,107,   215,115,
!    9           216,123,   301,131,   302,  3,   303, 11,   304, 19,
!    O           305, 27,   306, 35,4HXYCD,139,   307, 67,   308, 75,
!    1           309, 83,   310, 91,   311, 99,   312,107,   313,115,
!    2           314,123,   315,147/
!
!     USE VALUES BELOW WHEN ICFIAT (24TH WORD OF /SYSTEM/) IS 11
   DATA nfist/56/ , lfist/56/ , fist/4HPOOL , 0 , 4HOPTP , -1 , 4HNPTP , -2 , 4HUMF  , -3 , 4HNUMF , -4 , 4HPLT1 , -5 , 4HPLT2 ,    &
      & -6 , 4HINPT , -7 , 4HINP1 , -8 , 4HINP2 , -9 , 4HINP3 , -10 , 4HINP4 , -11 , 4HINP5 , -12 , 4HINP6 , -13 , 4HINP7 , -14 ,   &
       &4HINP8 , -15 , 4HINP9 , -16 , 4HXPTD , -17 , 4HSOF  , -18 , 4HUT1  , -19 , 4HUT2  , -20 , 4HUT3  , -21 , 4HUT4  , -22 ,     &
       &4HUT5  , -23 , 201 , 3 , 202 , 14 , 203 , 25 , 204 , 36 , 205 , 47 , 4HCASE , 58 , 207 , 69 , 4HPCDB , 80 , 208 , 91 , 209 ,&
      & 102 , 210 , 113 , 211 , 124 , 213 , 135 , 214 , 146 , 215 , 157 , 216 , 168 , 301 , 179 , 302 , 3 , 303 , 14 , 304 , 25 ,   &
      & 305 , 36 , 306 , 47 , 4HXYCD , 190 , 307 , 91 , 308 , 102 , 309 , 113 , 310 , 124 , 311 , 135 , 312 , 146 , 313 , 157 ,     &
      & 314 , 168 , 315 , 201/
!
END MODULE C_XFIST
