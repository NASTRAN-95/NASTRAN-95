!*==exiobd.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
BLOCKDATA exiobd
   IMPLICIT NONE
   USE C_EXIO2F
   USE C_EXIO2P
!
!     BLOCK DATA SUBPROGRAM FOR MODULE EXIO.
!
   DATA Nf/28/
   DATA P1/1 , 7 , 0 , 0 , 0 , 5 , 33 , 0 , 0 , 0 , 7 , 30 , 0 , 0 , 0 , 12 , 0 , 0 , 0 , 0 , 13 , 0 , 0 , 0 , 0 , 14 , 8 , 0 , 0 , &
      & 0 , 20 , 0 , 0 , 0 , 0 , 21 , 4 , 0 , 0 , 0 , 26 , 10 , 0 , 0 , 0 , 30 , 16 , 0 , 0 , 0/
   DATA P2/33 , 0 , 0 , 0 , 0 , 34 , 0 , 0 , 0 , 0 , 35 , 4 , 0 , 0 , 0 , 40 , 0 , 0 , 0 , 0 , 41 , 0 , 0 , 0 , 0 , 42 , 23 , 0 ,   &
      & 0 , 0 , 48 , 24 , 0 , 0 , 0 , 52 , 11 , 0 , 0 , 0 , 58 , 12 , 0 , 0 , 0 , 63 , 7 , 0 , 0 , 0/
   DATA P3/68 , 12 , 0 , 0 , 0 , 73 , 15 , 0 , 0 , 0 , 78 , 12 , 0 , 0 , 0 , 83 , 10 , 0 , 0 , 0 , 89 , 2 , 0 , 0 , 0 , 91 , 10 ,   &
      & 0 , 0 , 0 , 96 , 12 , 0 , 0 , 0 , 102 , 14 , 0 , 0 , 0/
   DATA F1/4H(3A4 , 4H,4I8 , 4H,88X , 4H)    , 4H(33A , 4H4)   , 4H(2A4 , 4H,2I8 , 4H,26A , 4H4,4X , 4H)    , 4H(04) , 4H(05) ,     &
       &4H(2(I , 4H8,1P , 4H,3E1 , 4H3.6) , 4H,38X , 3H)   , 4H(07) , 4H(2I8 , 4H,1P, , 4H2E13 , 4H.6,9 , 4H0X)  , 4H(1P, , 4H10E1 ,&
       &4H3.6, , 4H2X)  , 4H(16I , 4H8,4X , 4H)   /
   DATA F2/4H(11) , 4H(12) , 4H(2A4 , 4H,1P, , 4H2E13 , 4H.6,9 , 4H8X)  , 4H(14) , 4H(15) , 4H(2A4 , 4H,3I8 , 4H,6(2 , 4HA4,I ,     &
       &4H8),4 , 3HX)  , 4H(8(2 , 4HA4,I , 4H8),4 , 4HX)   , 4H(I8, , 4H5(I8 , 4H,1P, , 4HE13. , 4H6),1 , 3H9X) , 4H(6(I , 4H8,1P , &
       &4H,E13 , 4H.6), , 4H6X)  , 4H(2I8 , 4H,1P, , 4H5E13 , 4H.6,5 , 4H1X) /
   DATA F3/4H(6(I , 4H6,1P , 4H,E13 , 4H.6), , 4H18X) , 4H(5(I , 4H6,1P , 4H,D20 , 4H.13) , 4H,2X) , 4H(4(I , 4H6,1P , 4H,2E1 ,     &
       &4H3.6) , 4H,4X) , 4H(2(I , 4H6,1P , 4H,2D2 , 4H0.13 , 4H),40 , 3HX)  , 4H(A4, , 4HI8)  , 4H(4I8 , 4H,1P, , 4H6E13 , 4H.6,2 ,&
       &4H2X)  , 4H(2(5 , 4HI8,1 , 4HP,E1 , 4H3.6) , 4H,26X , 3H)   , 4H(2(6 , 4HI8,1 , 4HP,E1 , 4H3.6) , 4H,10X , 3H)  /
END BLOCKDATA exiobd
