
BLOCKDATA tabfbd
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Hx(32,40) , Hx01(32) , Hx02(32) , Hx03(32) , Hx04(32) , Hx05(32) , Hx06(32) , Hx07(32) , Hx08(32) , Hx09(32) , Hx10(32) ,&
         & Hx11(32) , Hx12(32) , Hx13(32) , Hx14(32) , Hx15(32) , Hx16(32) , Hx17(32) , Hx18(32) , Hx19(32) , Hx20(32) , Hx21(32) , &
         & Hx22(32) , La , Na(2,21) , Re(21)
   COMMON /tabftx/ La , Na , Hx , Re
!
! End of declarations
!
!TABFBD
! TABFTX - BLOCK DATA PROGRAM FOR MODULE TABPRT
!
!
!
!
   EQUIVALENCE (Hx01(1),Hx(1,1))
   EQUIVALENCE (Hx02(1),Hx(1,2))
   EQUIVALENCE (Hx03(1),Hx(1,3))
   EQUIVALENCE (Hx04(1),Hx(1,4))
   EQUIVALENCE (Hx05(1),Hx(1,5))
   EQUIVALENCE (Hx06(1),Hx(1,6))
   EQUIVALENCE (Hx07(1),Hx(1,7))
   EQUIVALENCE (Hx08(1),Hx(1,8))
   EQUIVALENCE (Hx09(1),Hx(1,9))
   EQUIVALENCE (Hx10(1),Hx(1,10))
   EQUIVALENCE (Hx11(1),Hx(1,11))
   EQUIVALENCE (Hx12(1),Hx(1,12))
   EQUIVALENCE (Hx13(1),Hx(1,13))
   EQUIVALENCE (Hx14(1),Hx(1,14))
   EQUIVALENCE (Hx15(1),Hx(1,15))
   EQUIVALENCE (Hx16(1),Hx(1,16))
   EQUIVALENCE (Hx17(1),Hx(1,17))
   EQUIVALENCE (Hx18(1),Hx(1,18))
   EQUIVALENCE (Hx19(1),Hx(1,19))
   EQUIVALENCE (Hx20(1),Hx(1,20))
   EQUIVALENCE (Hx21(1),Hx(1,21))
   EQUIVALENCE (Hx22(1),Hx(1,22))
!
!-----------------------------------------------------------------------
!
   DATA La/9/
   DATA Re/1 , 1 , 1 , 1 , 1 , 1 , 1 , 0 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1/
!
   DATA Na/4HBGPD , 4HT    , 4HGPL  , 4H     , 4HCSTM , 4H     , 4HGPLD , 4H     , 4HEQEX , 4HIN   , 4HEQDY , 4HN    , 4HGPDT ,     &
       &4H     , 4HGPTT , 4H     , 4HGPCT , 4H     , 4H*10* , 4H**** , 4H*11* , 4H**** , 4H*12* , 4H**** , 4H*13* , 4H**** ,        &
      & 4H*14* , 4H**** , 4H*15* , 4H**** , 4H*16* , 4H**** , 4H*17* , 4H**** , 4H*18* , 4H**** , 4H*19* , 4H**** , 4H*20* ,        &
      & 4H**** , 4H*21* , 4H****/
!
!
   DATA Hx01/4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     ,   &
       &4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     ,        &
      & 4H     , 4H     , 4H     , 4H     , 4H     , 4H    /
!
   DATA Hx02/4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4HFORM , 4HATTE , 4HD LI , 4HST O ,   &
       &4HF TA , 4HBLE  , 4HDATA , 4H BLO , 4HCK   , 4H**** , 4H**** , 4H     , 4H( RE , 4HCORD , 4H**** , 4H  )  , 4H     ,        &
      & 4H     , 4H     , 4H     , 4H     , 4H     , 4H    /
!
   DATA Hx03/4H     , 4H     , 4H     , 4H     , 4HINTE , 4HRNAL , 4H     , 4H COO , 4HRDIN , 4HATE  , 4H     , 4H     , 4H     ,   &
       &4H COO , 4HRDIN , 4HATES , 4H IN  , 4HBASI , 4HC CO , 4HORDI , 4HNATE , 4H SYS , 4HTEM  , 4H     , 4H     , 4H     ,        &
      & 4H     , 4H     , 4H     , 4H     , 4H     , 4H    /
!
   DATA Hx04/4H     , 4H     , 4H     , 4H     , 4H   I , 4HD    , 4H     , 4H SYS , 4HTEM  , 4HID   , 4H     , 4H     , 4H   X ,   &
       &4H     , 4H     , 4H     , 4H     , 4H   Y , 4H     , 4H     , 4H     , 4H     , 4H   Z , 4H     , 4H     , 4H     ,        &
      & 4H     , 4H     , 4H     , 4H     , 4H     , 4H    /
!
   DATA Hx05/4H     , 4H  IN , 4HTERN , 4HAL   , 4H     , 4H EXT , 4HERNA , 4HL GR , 4HID   , 4H     , 4H EXT , 4HERNA , 4HL GR ,   &
       &4HID   , 4H     , 4H EXT , 4HERNA , 4HL GR , 4HID   , 4H     , 4H EXT , 4HERNA , 4HL GR , 4HID   , 4H     , 4H EXT ,        &
      & 4HERNA , 4HL GR , 4HID   , 4H     , 4H     , 4H    /
!
   DATA Hx06/4H     , 4H     , 4H ID  , 4H     , 4H     , 4H OR  , 4HSCAL , 4HAR I , 4HD    , 4H     , 4H OR  , 4HSCAL , 4HAR I ,   &
       &4HD    , 4H     , 4H OR  , 4HSCAL , 4HAR I , 4HD    , 4H     , 4H OR  , 4HSCAL , 4HAR I , 4HD    , 4H     , 4H OR  ,        &
      & 4HSCAL , 4HAR I , 4HD    , 4H     , 4H     , 4H    /
!
   DATA Hx07/4H     , 4H  IN , 4HTERN , 4HAL   , 4H     , 4H   E , 4HXTER , 4HNAL  , 4HGRID , 4H   S , 4HEQUE , 4HNCE  , 4H     ,   &
       &4H     , 4HEXTE , 4HRNAL , 4H GRI , 4HD    , 4HSEQU , 4HENCE , 4H     , 4H     , 4H EXT , 4HERNA , 4HL GR , 4HID   ,        &
      & 4H SEQ , 4HUENC , 4HE    , 4H     , 4H     , 4H    /
!
   DATA Hx08/4H     , 4H     , 4H ID  , 4H     , 4H     , 4H   O , 4HR SC , 4HALAR , 4H ID  , 4H     , 4HNUMB , 4HER   , 4H     ,   &
       &4H     , 4HOR S , 4HCALA , 4HR ID , 4H     , 4H NUM , 4HBER  , 4H     , 4H     , 4H OR  , 4HSCAL , 4HAR I , 4HD    ,        &
      & 4H  NU , 4HMBER , 4H     , 4H     , 4H     , 4H    /
!
   DATA Hx09/4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     ,   &
       &4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     ,        &
      & 4H     , 4H     , 4H     , 4H     , 4H     , 4H    /
!
   DATA Hx10/4H     , 4H     , 4H     , 4H  N  , 4H     , 4H   I , 4HD    , 4H     , 4HTYPE , 4H     , 4H     , 4H R(I , 4H,1)  ,   &
       &4H     , 4H     , 4H     , 4H R(I , 4H,2)  , 4H     , 4H     , 4H     , 4H R(I , 4H,3)  , 4H     , 4H     , 4H     ,        &
      & 4H     , 4H     , 4H     , 4HT(I) , 4H     , 4H    /
!
   DATA Hx11/4H   E , 4HXTER , 4HNAL  , 4H     , 4HEXTE , 4HRNAL , 4H GRI , 4HD    , 4HINTE , 4HRNAL , 4H     , 4H EXT , 4HERNA ,   &
       &4HL GR , 4HID   , 4H INT , 4HERNA , 4HL    , 4H  EX , 4HTERN , 4HAL G , 4HRID  , 4H  IN , 4HTERN , 4HAL   , 4H   E ,        &
      & 4HXTER , 4HNAL  , 4HGRID , 4H   I , 4HNTER , 4HNAL /
!
   DATA Hx12/4H   S , 4HORT  , 4HID   , 4H     , 4HOR S , 4HCALA , 4HR ID , 4H     , 4H NUM , 4HBER  , 4H     , 4H OR  , 4HSCAL ,   &
       &4HAR I , 4HD    , 4H  NU , 4HMBER , 4H     , 4H  OR , 4H SCA , 4HLAR  , 4HID   , 4H   N , 4HUMBE , 4HR    , 4H   O ,        &
      & 4HR SC , 4HALAR , 4H ID  , 4H     , 4HNUMB , 4HER  /
!
   DATA Hx13/4H   I , 4HNTER , 4HNAL  , 4H     , 4H     , 4HCOOR , 4HDINA , 4HTE   , 4H     , 4H     , 4H COO , 4HRDIN , 4HATES ,   &
       &4H IN  , 4HDEFI , 4HNING , 4H COO , 4HRDIN , 4HATE  , 4HSYST , 4HEM   , 4H     , 4H  DI , 4HSPLA , 4HCEME , 4HNT C ,        &
      & 4HOOR- , 4H     , 4HCONS , 4HTRAI , 4HNT   , 4H    /
!
   DATA Hx14/4H     , 4H  ID , 4H     , 4H     , 4H     , 4HSYST , 4HEM   , 4H     , 4H     , 4H     , 4H  X  , 4H     , 4H     ,   &
       &4H     , 4H     , 4HY    , 4H     , 4H     , 4H     , 4H Z   , 4H     , 4H     , 4H  DI , 4HNATE , 4H SYS , 4HTEM  ,        &
      & 4HID   , 4H     , 4H   C , 4HODE  , 4H     , 4H    /
!
   DATA Hx15/4H   I , 4HTERN , 4HAL   , 4H     , 4H   T , 4HEMPE , 4HRATU , 4HRE   , 4H     , 4H     , 4HDEFA , 4HULT  , 4HTEMP ,   &
       &4HERAT , 4HURE  , 4H     , 4H     , 4H   R , 4HECOR , 4HD NU , 4HMBER , 4H FOR , 4H     , 4H     , 4H     , 4H     ,        &
      & 4H     , 4H     , 4H     , 4H     , 4H     , 4H    /
!
   DATA Hx16/4H     , 4HINDE , 4HX    , 4H     , 4H     , 4H SET , 4H ID  , 4H     , 4H     , 4H     , 4H     , 4H  OR , 4H FLA ,   &
       &4HG    , 4H     , 4H     , 4H     , 4H ADD , 4HITIO , 4HNAL  , 4HTEMP , 4H. DA , 4HTA   , 4H     , 4H     , 4H     ,        &
      & 4H     , 4H     , 4H     , 4H     , 4H     , 4H    /
!
   DATA Hx17/4H  SU , 4HBSEQ , 4HUENT , 4H REC , 4HORDS , 4H OF  , 4H G P , 4H T T , 4H  TE , 4HMPER , 4HATUR , 4HE DA , 4HTA A ,   &
       &4HRE L , 4HISTE , 4HD UN , 4HDER  , 4HSET  , 4HID A , 4HND E , 4HLEME , 4HNT T , 4HYPE  , 4HBY E , 4HLEME , 4HNT I ,        &
      & 4HD    , 4H     , 4H     , 4H     , 4H     , 4H    /
!
   DATA Hx18/4H   R , 4HECOR , 4HD NU , 4HMBER , 4H   T , 4HEMPE , 4HRATU , 4HRE S , 4HET I , 4HD    , 4H     , 4H     , 4H     ,   &
       &4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     ,        &
      & 4H     , 4H     , 4H     , 4H     , 4H     , 4H    /
!
   DATA Hx19/4H     , 4H     , 4H     , 4H  F  , 4HO R  , 4HM A  , 4HT T  , 4HE D  , 4H  L  , 4HI S  , 4HT    , 4HO F  , 4H  T  ,   &
       &4HA B  , 4HL E  , 4H  D  , 4HA T  , 4HA    , 4HB L  , 4HO C  , 4HK    , 4HG P  , 4HC T  , 4H     , 4H     , 4H     ,        &
      & 4H     , 4H     , 4H     , 4H     , 4H     , 4H    /
!
   DATA Hx20/4H  RE , 4HCORD , 4H     , 4HPIVO , 4HT  C , 4HONNE , 4HCTIN , 4HG    , 4H     , 4H     , 4H     , 4H     , 4H SOR ,   &
       &4HTED  , 4HLIST , 4H OF  , 4H S I , 4H L   , 4HNUMB , 4HERS  , 4HOF C , 4HONNE , 4HCTED , 4H POI , 4HNTS  , 4H     ,        &
      & 4H     , 4H     , 4H     , 4H     , 4H     , 4H    /
!
   DATA Hx21/4H  NU , 4HMBER , 4H     , 4HS I  , 4HL    , 4H NUM , 4HBER  , 4H     , 4H( 1  , 4H)    , 4H  (  , 4H2 )  , 4H     ,   &
       &4H( 3  , 4H)    , 4H  (  , 4H4 )  , 4H     , 4H( 5  , 4H)    , 4H  (  , 4H6 )  , 4H     , 4H( 7  , 4H)    , 4H  (  ,        &
      & 4H8 )  , 4H     , 4H( 9  , 4H)    , 4H ( 1 , 4H0 ) /
!
   DATA Hx22/4H   S , 4HORT  , 4HID   , 4H     , 4HOR S , 4HCALA , 4HR ID , 4H   C , 4HODED , 4H SIL , 4H     , 4H OR  , 4HSCAL ,   &
       &4HAR I , 4HD    , 4HCODE , 4HD SI , 4HL    , 4H  OR , 4H SCA , 4HLAR  , 4HID   , 4H COD , 4HED S , 4HIL   , 4H   O ,        &
      & 4HR SC , 4HALAR , 4H ID  , 4H  CO , 4HDED  , 4HSIL /
!
END BLOCKDATA tabfbd
