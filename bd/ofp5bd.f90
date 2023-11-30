
BLOCKDATA ofp5bd
   IMPLICIT NONE
   INTEGER E1(100) , E21(100) , E41(100) , E61(100) , E81(100) , Esingl(64)
   COMMON /ofpbd5/ Esingl , E1 , E21 , E41 , E61 , E81
!OFP5BD
!*****
!     SPACING ARRAY - ESINGL
!*****
   DATA Esingl(1)/4H/   /
   DATA Esingl(2)/4H15X /
   DATA Esingl(3)/4H10X /
   DATA Esingl(4)/4H5X  /
   DATA Esingl(5)/4H1X  /
   DATA Esingl(6)/4H/10X/
   DATA Esingl(7)/4H16X /
   DATA Esingl(8)/4H2H1 /
   DATA Esingl(9)/4H2H2 /
   DATA Esingl(10)/4H2H3 /
   DATA Esingl(11)/4H2H4 /
   DATA Esingl(12)/4H2H5 /
   DATA Esingl(13)/4H7X  /
   DATA Esingl(14)/4H/16X/
   DATA Esingl(15)/4H/13X/
   DATA Esingl(16)/4H4X  /
   DATA Esingl(17)/4H/14X/
   DATA Esingl(18)/4H11X /
   DATA Esingl(19)/4H/24X/
   DATA Esingl(20)/4H1H0 /
   DATA Esingl(21)/4H2H //
   DATA Esingl(22)/4H2HEN/
   DATA Esingl(23)/4H2HDA/
   DATA Esingl(24)/4H2HDB/
   DATA Esingl(25)/4H/1H0/
   DATA Esingl(26)/4H23X /
   DATA Esingl(27)/4H/26X/
   DATA Esingl(28)/4H/9X /
   DATA Esingl(29)/4H/12X/
   DATA Esingl(30)/4H/1H /
   DATA Esingl(31)/4H/20X/
   DATA Esingl(32)/4H/32X/
   DATA Esingl(33)/4H/28X/
   DATA Esingl(34)/4H/15X/
   DATA Esingl(35)/4H/19X/
   DATA Esingl(36)/4H/21X/
   DATA Esingl(37)/4H/11X/
   DATA Esingl(38)/4H/17X/
   DATA Esingl(39)/4H2X  /
   DATA Esingl(40)/4H1HX /
   DATA Esingl(41)/4H2HXY/
   DATA Esingl(42)/4H1HA /
   DATA Esingl(43)/4H2HLX/
   DATA Esingl(44)/4H1HY /
   DATA Esingl(45)/4H2HYZ/
   DATA Esingl(46)/4H1HB /
   DATA Esingl(47)/4H2HLY/
   DATA Esingl(48)/4H1HZ /
   DATA Esingl(49)/4H2HZX/
   DATA Esingl(50)/4H1HC /
   DATA Esingl(51)/4H2HLZ/
   DATA Esingl(52)/4H2HCP/
   DATA Esingl(53)/4H2HMP/
   DATA Esingl(54)/4H2HC /
   DATA Esingl(55)/4H3X  /
   DATA Esingl(56)/4H/30X/
   DATA Esingl(57)/4H9X  /
   DATA Esingl(58)/4H/23X/
   DATA Esingl(59)/4H6X  /
   DATA Esingl(60)/4H39X /
   DATA Esingl(61)/4H24X /
   DATA Esingl(62)/4H    /
   DATA Esingl(63)/4H    /
   DATA Esingl(64)/4H    /
!
!
!     FORMAT BUILDING BLOCK E-ARRAY
!
!
!                    -STANDARD-                 -ALTERNATES-
!                 ****************        ***********************
   DATA E1/4H1P,E , 4H15.6 , 4H0P,F , 4H6.1  , 4H,9X  , 4H1P,E , 4H16.6 , 4H0P,F , 4H7.1  , 4H,9X  , 4H1P,E , 4H17.6 , 4H0P,F ,     &
       &4H8.1  , 4H,9X  , 4H1P,E , 4H18.6 , 4H0P,F , 4H9.1  , 4H,9X  , 4H1P,E , 4H19.6 , 4H0P,F , 4H10.1 , 4H,9X  , 4H1P,E ,        &
      & 4H20.6 , 4H0P,F , 4H11.1 , 4H,9X  , 4H1P,E , 4H21.6 , 4H0P,F , 4H12.1 , 4H,9X  , 4H1P,E , 4H30.6 , 4H0P,F , 4H21.1 ,        &
      & 4H,9X  , 4H1P,E , 4H26.6 , 4H0P,F , 4H17.1 , 4H,9X  , 4H1P,E , 4H24.6 , 4H0P,F , 4H15.1 , 4H,9X  , 4H0P,F , 4H11.4 ,        &
      & 4H0P,F , 4H8.1  , 4H,3X  , 4H0P,F , 4H14.4 , 4H0P,F , 4H11.1 , 4H,3X  , 4H1P,E , 4H28.6 , 4H0P,F , 4H19.1 , 4H,9X  ,        &
      & 4H1P,E , 4H37.6 , 4H0P,F , 4H28.1 , 4H,9X  , 4H1P,E , 4H22.6 , 4H0P,F , 4H17.1 , 4H,5X  , 4H1P,E , 4H14.6 , 4H0P,F ,        &
      & 4H5.1  , 4H,9X  , 4H0P,F , 4H15.4 , 4H0P,F , 4H12.1 , 4H,3X  , 4H0P,F , 4H9.4  , 4H0P,F , 4H6.1  , 4H,3X  , 4H0P,F ,        &
      & 4H15.3 , 4H0P,F , 4H12.1 , 4H 3X  , 4H1P,E , 4H23.6 , 4H0P,F , 4H14.1 , 4H,9X /
   DATA E21/4H1P,E , 4H35.6 , 4H0P,F , 4H26.1 , 4H,9X  , 4H1P,E , 4H25.6 , 4H0P,F , 4H16.1 , 4H,9X  , 4H1P,E , 4H50.6 , 4H0P,F ,    &
       &4H41.1 , 4H,9X  , 4H0P,F , 4H46.4 , 4H0P,F , 4H43.1 , 4H,3X  , 4H     , 4H     , 4H0P,F , 4H12.1 , 4H,3X  , 4H0P,F ,        &
      & 4H20.4 , 4H0P,F , 4H17.1 , 4H,3X  , 4H0P,F , 4H16.4 , 4H0P,F , 4H13.1 , 4H,3X  , 4H0P,F , 4H22.4 , 4H0P,F , 4H19.1 ,        &
      & 4H,3X  , 4H1P,E , 4H27.6 , 4H0P,F , 4H18.1 , 4H,9X  , 4H0P,F , 4H12.5 , 4H0P,F , 4H11.1 , 4H,3X  , 4H1P,E , 4H13.5 ,        &
      & 4H0P,F , 4H5.1  , 4H,8X  , 4H0P,F , 4H13.3 , 4H0P,F , 4H9.1  , 4H,4X  , 4H0P,F , 4H18.4 , 4H0P,F , 4H15.1 , 4H,3X  ,        &
      & 4H0P,F , 4H26.4 , 4H0P,F , 4H23.1 , 4H,3X  , 4H1P,E , 4H14.5 , 4H0P,F , 4H6.1  , 4H,8X  , 4H0P,F , 4H14.3 , 4H0P,F ,        &
      & 4H10.1 , 4H,4X  , 4H0P,F , 4H5.2  , 4H0P,F , 4H4.1  , 4H,1X  , 4H1P,E , 4H13.6 , 4H0P,F , 4H4.1  , 4H,9X  , 4H     ,        &
      & 4H     , 4H     , 4H     , 4H     , 4H1P,E , 4H9.1  , 4HA1   , 4H,8X  , 4H    /
   DATA E41/4H6X,A , 4H1,3X , 4HI7   , 4H,3X  , 4H     , 4HI15  , 4H     , 4H     , 4H     , 4H     , 4HI9,1 , 4HX    , 4H     ,    &
       &4H     , 4H     , 4H1H0, , 4HI8   , 4H     , 4H     , 4H     , 4H1X,I , 4H13   , 4H     , 4H     , 4H     , 4H1X,I ,        &
      & 4H8    , 4H     , 4H     , 4H     , 4H1H0, , 4HI7   , 4H     , 4H     , 4H     , 4H6X,I , 4H8    , 4H     , 4H     ,        &
      & 4H     , 4H1X,I , 4H15   , 4H     , 4H     , 4H     , 4H1X,I , 4H12   , 4H     , 4H     , 4H     , 4HI10  , 4H     ,        &
      & 4H     , 4H     , 4H     , 4HI7,1 , 4HX    , 4H     , 4H     , 4H     , 4H3X,A , 4H4    , 4H     , 4H     , 4H     ,        &
      & 4H1H0, , 4HI13  , 4H     , 4H     , 4H     , 4H1X,I , 4H20   , 4H     , 4H     , 4H     , 4H5X,A , 4H1,3X , 4HI5   ,        &
      & 4H,4X  , 4H     , 4H1X,I , 4H22   , 4H     , 4H     , 4H     , 4HI12  , 4H     , 4H     , 4H     , 4H     , 4H1X,I ,        &
      & 4H19   , 4H     , 4H     , 4H     , 4HI16  , 4H     , 4H     , 4H     , 4H    /
   DATA E61/4HI8   , 4H     , 4HA4   , 4H,4X  , 4H     , 4HI9   , 4H     , 4HA4   , 4H,5X  , 4H     , 4HI11  , 4H     , 4HA4   ,    &
       &4H,7X  , 4H     , 4HI20  , 4H     , 4HA4   , 4H,16X , 4H     , 4HI19  , 4H     , 4HA4   , 4H,15X , 4H     , 4H1X,I ,        &
      & 4H23   , 4H     , 4H     , 4H     , 4HI23  , 4H     , 4H     , 4H     , 4H     , 4HI28  , 4H     , 4H     , 4H     ,        &
      & 4H     , 4H/1H  , 4H,I18 , 4H     , 4H     , 4H     , 4H1H0, , 4HI15  , 4H     , 4H     , 4H     , 4H1H0, , 4HI14  ,        &
      & 4H     , 4H     , 4H     , 4H0P,F , 4H22.4 , 4HI9   , 4H,13X , 4H     , 4H0P,F , 4H16.4 , 4HI5   , 4H,11X , 4H     ,        &
      & 4H0P,F , 4H10.4 , 4H     , 4H     , 4H     , 4H1H0, , 4HI19  , 4H     , 4H     , 4H     , 4H1H0, , 4HI20  , 4H     ,        &
      & 4H     , 4H     , 4HI10, , 4H5X   , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4HI8,  ,        &
      & 4H2X   , 4H3X,3 , 4HHCEN , 4H,A4  , 4HF8.3 , 4H     , 4H     , 4H     , 4H    /
!AIX 7            , 4H  9E ,4H11.3        , 4H  9( ,4HF9.3 ,4H,2X)
!AIX 9            , 4H/, E ,4H11.3        , 4H/0P, ,4HF7.1 ,4H,4X
   DATA E81/4H1H0, , 4HI27  , 4H     , 4H     , 4H     , 4H1H0, , 4HI5   , 4H     , 4H     , 4H     , 4H1H0, , 4HI3   , 4H     ,    &
       &4H     , 4H     , 4HI4   , 4H     , 4H     , 4H     , 4H     , 4H1P,E , 4H11.4 , 4H0P,F , 4H4.1  , 4H,7X  , 4HA4   ,        &
      & 4H     , 4H     , 4H     , 4H     , 4H1P9E , 4H11.3 , 4H0P9( , 4HF9.3 , 4H,2X) , 4H0P,F , 4H22.3 , 4H0P,F , 4H20.1 ,        &
      & 4H,2X  , 4H/1PE , 4H11.3 , 4H/0PF , 4H7.1  , 4H,4X  , 4H0P,F , 4H19.4 , 4HI6   , 4H,13X , 4H     , 4HF8.2 , 4H     ,        &
      & 4H     , 4H     , 4H     , 4H1P,E , 4H12.5 , 4H     , 4H     , 4H     , 4H1H0, , 4HI12  , 4H     , 4H     , 4H     ,        &
      & 4H4X,I , 4H8    , 4H4X,A , 4H4,4X , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     ,        &
      & 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     ,        &
      & 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H    /
END BLOCKDATA ofp5bd
