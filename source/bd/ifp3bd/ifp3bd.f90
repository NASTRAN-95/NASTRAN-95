!*==ifp3bd.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
BLOCKDATA ifp3bd
   USE c_ifp3cm
   IMPLICIT NONE
!IFP3BD
!     B L O C K   D A T A   F O R   I F P 3
!
!
!
!
   DATA one/1/ , zero/0/
!
   DATA file(1) , file(2)/201 , 208/
   DATA file(3) , file(4)/209 , 210/
   DATA file(5) , file(6)/301 , 215/
!
   DATA iname(1) , iname(2)/4HGEOM , 4H1   /
   DATA iname(3) , iname(4)/4HGEOM , 4H2   /
   DATA iname(5) , iname(6)/4HGEOM , 4H3   /
   DATA iname(7) , iname(8)/4HGEOM , 4H4   /
   DATA iname(9) , iname(10)/4HSCRT , 4HCH  /
   DATA iname(11) , iname(12)/4HAXIC , 4H    /
!
   DATA cdtype(1) , cdtype(2)/4HAXIC , 4H    /
   DATA cdtype(3) , cdtype(4)/4HCCON , 4HEAX /
   DATA cdtype(5) , cdtype(6)/4HFORC , 4HEAX /
   DATA cdtype(7) , cdtype(8)/4HFORC , 4HE   /
   DATA cdtype(9) , cdtype(10)/4HGRAV , 4H    /
   DATA cdtype(11) , cdtype(12)/4HLOAD , 4H    /
   DATA cdtype(13) , cdtype(14)/4HMOMA , 4HX   /
   DATA cdtype(15) , cdtype(16)/4HMOME , 4HNT  /
   DATA cdtype(17) , cdtype(18)/4HMPCA , 4HDD  /
   DATA cdtype(19) , cdtype(20)/4HMPCA , 4HX   /
   DATA cdtype(21) , cdtype(22)/4HOMIT , 4HAX  /
   DATA cdtype(23) , cdtype(24)/4HPOIN , 4HTAX /
   DATA cdtype(25) , cdtype(26)/4HPRES , 4HAX  /
   DATA cdtype(27) , cdtype(28)/4HRING , 4HAX  /
   DATA cdtype(29) , cdtype(30)/4HSECT , 4HAX  /
   DATA cdtype(31) , cdtype(32)/4HSEQG , 4HP   /
   DATA cdtype(33) , cdtype(34)/4HSPCA , 4HDD  /
   DATA cdtype(35) , cdtype(36)/4HSPCA , 4HX   /
   DATA cdtype(37) , cdtype(38)/4HSUPA , 4HX   /
   DATA cdtype(39) , cdtype(40)/4HTEMP , 4HAX  /
   DATA cdtype(41) , cdtype(42)/4HTEMP , 4HD   /
   DATA cdtype(43) , cdtype(44)/4HCTRI , 4HAAX /
   DATA cdtype(45) , cdtype(46)/4HCTRA , 4HPAX /
   DATA cdtype(47) , cdtype(48)/4HRFOR , 4HCE  /
!
   DATA axic1/515 , 5 , 0/
   DATA cconex/8515 , 85 , 0/
   DATA forcex/2115 , 21 , 0/
   DATA force/4201 , 42 , 0/
   DATA grav/4401 , 44 , 0/
   DATA load/4551 , 61 , 0/
   DATA momax/3815 , 38 , 0/
   DATA moment/4801 , 48 , 0/
   DATA mpcadd/4891 , 60 , 0/
   DATA mpcax/4015 , 40 , 0/
   DATA omitax/4315 , 43 , 0/
   DATA pointx/4915 , 49 , 0/
   DATA presax/5215 , 52 , 0/
   DATA ringax/5615 , 56 , 0/
   DATA sectax/6315 , 63 , 0/
   DATA seqgp/5301 , 53 , 0/
   DATA spcax/6215 , 62 , 0/
   DATA supax/6415 , 64 , 0/
   DATA tempax/6815 , 68 , 0/
   DATA tempd/5641 , 65 , 0/
   DATA pload/5101 , 51 , 0/
   DATA mpc/4901 , 49 , 0/
   DATA spc/5501 , 55 , 0/
   DATA grid/4501 , 45 , 0/
   DATA suport/5601 , 56 , 0/
   DATA temp/5701 , 57 , 0/
   DATA omit/5001 , 50 , 0/
   DATA spcadd/5491 , 59 , 0/
   DATA ctriaa/7012 , 70 , 0/
   DATA ctrapa/7042 , 74 , 0/
   DATA rforce/5509 , 55 , 0/
   DATA iconso/0/
   DATA neg111/ - 1 , -1 , -1/
   DATA t65535/65535 , 65535 , 65535/
   DATA iheadb/4HI N  , 4HP U  , 4HT    , 4HD A  , 4HT A  , 4H  E  , 4HR R  , 4HO R  , 4HS    , 4HD E  , 4HT E  , 4HC T  , 4HE D  , &
       &4H  B  , 4HY    , 4HI F  , 4HP 3  , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     ,        &
      & 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H (AX , 4HIS-S , 4HYMME , 4HTRIC , 4H CON , 4HICAL ,        &
      & 4H SHE , 4HLL D , 4HATA  , 4HPROC , 4HESSO , 4HR-GE , 4HNERA , 4HTOR) , 4H     , 4H     , 4H     , 4H     , 4H     ,        &
      & 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     ,        &
      & 4H === , 4H==== , 4H==== , 4H==== , 4H==== , 4H==== , 4H==== , 4H==== , 4H==== , 4H==== , 4H==== , 4H==== , 4H==== ,        &
      & 4H==== , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     ,        &
      & 4H     , 4H     , 4H     , 4H     , 4H    /
!
END BLOCKDATA ifp3bd
