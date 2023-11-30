
BLOCKDATA gp3bd
   IMPLICIT NONE
   INTEGER Buf(50) , Carddt(60) , Cardid(60) , Eqexin , Geom2 , Geom3 , Gptt , Idno(30) , Igrav , Ipld3 , Ipload , Load(2) ,        &
         & Mask(60) , Nopld2 , Ntypes , Pload2(2) , Pload3(2) , Scr1 , Scr2 , Slt , Status(60) , Temp(2) , Tempd(2) , Tempg(2) ,    &
         & Tempp1(2) , Tempp2(2) , Tempp3(2) , Tempp4(2) , Temprb(2)
   REAL Buf1 , Buf2 , Buf3
   COMMON /gp3com/ Geom3 , Eqexin , Geom2 , Slt , Gptt , Scr1 , Scr2 , Buf1 , Buf2 , Buf , Cardid , Idno , Carddt , Mask , Status , &
                 & Ntypes , Ipload , Igrav , Pload2 , Load , Nopld2 , Temp , Tempd , Tempp1 , Tempp2 , Tempp3 , Temprb , Buf3 ,     &
                 & Pload3 , Ipld3 , Tempg , Tempp4
!GP3BD
!
!     BLOCK DATA PROGRAM FOR MODULE GP3.
!
!
!
!     GINO NAMES FOR INPUT, OUTPUT AND SCRATCH FILES.
!
   DATA Geom3 , Eqexin , Geom2 , Slt , Gptt , Scr1 , Scr2/101 , 102 , 103 , 201 , 202 , 301 , 302/
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
   DATA Cardid/4001 , 40 , 4101 , 41 , 4201 , 42 , 4401 , 44 , 5509 , 55 , 4601 , 46 , 4701 , 47 , 4801 , 48 , 5101 , 51 , 5401 ,   &
      & 54 , 5215 , 52 , 4309 , 43 , 5209 , 52 , 4509 , 45 , 4909 , 49 , 5009 , 50 , 7109 , 71 , 6909 , 69 , 7001 , 70 , 3109 , 31 ,&
      & 3209 , 32 , 3309 , 33 , 3409 , 34 , 3509 , 35 , 6709 , 67 , 0000 , 00 , 0000 , 00 , 0000 , 00 , 0000 , 00 , 0000 , 00/
!
!WKBR 2/95 SPR94015 DATA    CARDDT/ 5, 3,    7, 7,    7, 1,    6, 0,    8, 1,
   DATA Carddt/5 , 3 , 7 , 7 , 7 , 1 , 6 , 0 , 7 , 1 , 5 , 3 , 7 , 7 , 7 , 1 , 6 , 13 , 3 , 1 , 7 , 18 , 8 , 21 , 3 , 0 , 3 , 0 ,   &
      & 6 , 0 , 6 , 0 , 39 , 26 , 8 , 0 , 6 , 14 , 13 , 0 , 6 , 28 , 49 , 0 , 6 , 0 , 10 , 0 , 12 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , &
      & 0 , 0 , 0/
!
   DATA Status/ - 1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 ,   &
      & 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0 ,    &
      & -1 , 0 , -1 , 0 , -1 , 0 , -1 , 0/
!
   DATA Idno/3 , 5 , 1 , 8 , 10 , 4 , 6 , 2 , 9 , 7 , 11 , 12 , 13 , 14 , 15 , 16 , 17 , 18 , 19 , 21 , 20 , 22 , 24 , 23 , 25 , 0 ,&
      & 0 , 0 , 0 , 0/
!
   DATA Mask/1 , 2 , 3 , 2 , 4 , 5 , 5 , 2 , 4 , 5 , 6 , 7 , 4 , 3 , 4 , 5 , 6 , 2 , 3 , 4 , 4 , 5 , 6 , 7 , 8 , 32 , -8 , 1 , 6 ,  &
      & 31*0/
!
!     MISCELANEOUS DATA.
!
   DATA Ntypes/49/Igrav/7/Ipload/17/Pload2/6809 , 68/Load/4551 , 61/Nopld2/0/Temp/5701 , 57/Tempd/5641 , 65/Tempp1/8109 ,           &
      & 81/Tempp2/8209 , 82/Tempp3/8309 , 83/Temprb/8409 , 84/Pload3/7109 , 71/Ipld3/33/Tempg/8509 , 85/Tempp4/8609 , 86/
END BLOCKDATA gp3bd