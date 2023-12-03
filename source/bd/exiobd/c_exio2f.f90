!*==/home/marcusmae/nasa/nastran/SPAGged/C_EXIO2F.f90  created by SPAG 8.01RF at 14:46 on  2 Dec 2023
MODULE C_EXIO2F
!
!     BLOCK DATA SUBPROGRAM FOR MODULE EXIO.
!
   INTEGER, DIMENSION(32) :: F1
   INTEGER, DIMENSION(35) :: F2
   INTEGER, DIMENSION(40) :: F3
!
   DATA f1/4H(3A4 , 4H,4I8 , 4H,88X , 4H)    , 4H(33A , 4H4)   , 4H(2A4 , 4H,2I8 , 4H,26A , 4H4,4X , 4H)    , 4H(04) , 4H(05) ,     &
       &4H(2(I , 4H8,1P , 4H,3E1 , 4H3.6) , 4H,38X , 3H)   , 4H(07) , 4H(2I8 , 4H,1P, , 4H2E13 , 4H.6,9 , 4H0X)  , 4H(1P, , 4H10E1 ,&
       &4H3.6, , 4H2X)  , 4H(16I , 4H8,4X , 4H)   /
   DATA f2/4H(11) , 4H(12) , 4H(2A4 , 4H,1P, , 4H2E13 , 4H.6,9 , 4H8X)  , 4H(14) , 4H(15) , 4H(2A4 , 4H,3I8 , 4H,6(2 , 4HA4,I ,     &
       &4H8),4 , 3HX)  , 4H(8(2 , 4HA4,I , 4H8),4 , 4HX)   , 4H(I8, , 4H5(I8 , 4H,1P, , 4HE13. , 4H6),1 , 3H9X) , 4H(6(I , 4H8,1P , &
       &4H,E13 , 4H.6), , 4H6X)  , 4H(2I8 , 4H,1P, , 4H5E13 , 4H.6,5 , 4H1X) /
   DATA f3/4H(6(I , 4H6,1P , 4H,E13 , 4H.6), , 4H18X) , 4H(5(I , 4H6,1P , 4H,D20 , 4H.13) , 4H,2X) , 4H(4(I , 4H6,1P , 4H,2E1 ,     &
       &4H3.6) , 4H,4X) , 4H(2(I , 4H6,1P , 4H,2D2 , 4H0.13 , 4H),40 , 3HX)  , 4H(A4, , 4HI8)  , 4H(4I8 , 4H,1P, , 4H6E13 , 4H.6,2 ,&
       &4H2X)  , 4H(2(5 , 4HI8,1 , 4HP,E1 , 4H3.6) , 4H,26X , 3H)   , 4H(2(6 , 4HI8,1 , 4HP,E1 , 4H3.6) , 4H,10X , 3H)  /

END MODULE C_EXIO2F
