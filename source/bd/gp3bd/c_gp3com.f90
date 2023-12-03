!*==/home/marcusmae/nasa/nastran/SPAGged/C_GP3COM.f90  created by SPAG 8.01RF at 14:46 on  2 Dec 2023
MODULE C_GP3COM
   INTEGER, DIMENSION(50) :: Buf
   REAL :: Buf1, Buf2, Buf3
   INTEGER, DIMENSION(60) :: Carddt, Cardid, Mask, Status
   INTEGER :: Eqexin, Geom2, Geom3, Gptt, Igrav, Ipld3, Ipload, Nopld2, Ntypes, Scr1, Scr2, Slt
   INTEGER, DIMENSION(30) :: Idno
   INTEGER, DIMENSION(2) :: Load, Pload2, Pload3, Temp, Tempd, Tempg, Tempp1, Tempp2, Tempp3, Tempp4, Temprb
!GP3BD
!
!     BLOCK DATA PROGRAM FOR MODULE GP3.
!
!
!
!     GINO NAMES FOR INPUT, OUTPUT AND SCRATCH FILES.
!
   DATA geom3 , eqexin , geom2 , slt , gptt , scr1 , scr2/101 , 102 , 103 , 201 , 202 , 301 , 302/
!
!     DATA DEFINING LOAD CARDS--
!     CARDID - TWO-WORD RECORD ID DEFINING CARD TYPE.
!     CARDDT - TWO WORDS PER CARD TYPE. 1ST WORD IS NO. OF WORDS PER
!              CARD. 2ND WORD IS POINTER IN MASK TABLE TO ENTRY WHICH
!              DESCRIBES THE NUMBER AND LOCATION OF GRID POINTS ON THE
!              CARD.
!     MASK   - TABLE AS DESCRIBED ABOVE.
!     IDNO   - INTERNAL CARD TYPE ID.
!
!              FORCE1   FORCE2   FORCE    GRAV     RFORCE
!              MOMNT1   MOMNT2   MOMENT   PLOAD    SLOAD
!              PRESAX   QHBDY    QVOL     QBDY1    QBDY2
!              QVECT    PLOAD3   PLOAD1   PLOADX   CEMLOOP
!              SPCFLD   GEMLOOP  REMFLUX  MDIPOLE  PLOAD4
!
   DATA cardid/4001 , 40 , 4101 , 41 , 4201 , 42 , 4401 , 44 , 5509 , 55 , 4601 , 46 , 4701 , 47 , 4801 , 48 , 5101 , 51 , 5401 ,   &
      & 54 , 5215 , 52 , 4309 , 43 , 5209 , 52 , 4509 , 45 , 4909 , 49 , 5009 , 50 , 7109 , 71 , 6909 , 69 , 7001 , 70 , 3109 , 31 ,&
      & 3209 , 32 , 3309 , 33 , 3409 , 34 , 3509 , 35 , 6709 , 67 , 0000 , 00 , 0000 , 00 , 0000 , 00 , 0000 , 00 , 0000 , 00/
!
!WKBR 2/95 SPR94015 DATA    CARDDT/ 5, 3,    7, 7,    7, 1,    6, 0,    8, 1,
   DATA carddt/5 , 3 , 7 , 7 , 7 , 1 , 6 , 0 , 7 , 1 , 5 , 3 , 7 , 7 , 7 , 1 , 6 , 13 , 3 , 1 , 7 , 18 , 8 , 21 , 3 , 0 , 3 , 0 ,   &
      & 6 , 0 , 6 , 0 , 39 , 26 , 8 , 0 , 6 , 14 , 13 , 0 , 6 , 28 , 49 , 0 , 6 , 0 , 10 , 0 , 12 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , &
      & 0 , 0 , 0/
!
   DATA status/ - 1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 ,   &
      & 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 ,    &
      & -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0/
!
   DATA idno/3 , 5 , 1 , 8 , 10 , 4 , 6 , 2 , 9 , 7 , 11 , 12 , 13 , 14 , 15 , 16 , 17 , 18 , 19 , 21 , 20 , 22 , 24 , 23 , 25 , 0 ,&
      & 0 , 0 , 0 , 0/
!
   DATA mask/1 , 2 , 3 , 2 , 4 , 5 , 5 , 2 , 4 , 5 , 6 , 7 , 4 , 3 , 4 , 5 , 6 , 2 , 3 , 4 , 4 , 5 , 6 , 7 , 8 , 32 , -8 , 1 , 6 ,  &
      & 31*0/
!
!     MISCELANEOUS DATA.
!
   DATA ntypes/49/igrav/7/ipload/17/pload2/6809 , 68/load/4551 , 61/nopld2/0/temp/5701 , 57/tempd/5641 , 65/tempp1/8109 ,           &
      & 81/tempp2/8209 , 82/tempp3/8309 , 83/temprb/8409 , 84/pload3/7109 , 71/ipld3/33/tempg/8509 , 85/tempp4/8609 , 86/
END MODULE c_gp3com