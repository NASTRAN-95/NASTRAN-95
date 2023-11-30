
BLOCKDATA ifp3bd
   IMPLICIT NONE
   INTEGER Axic1(3) , Cconex(3) , Cdtype(50) , Ctrapa(3) , Ctriaa(3) , File(6) , Force(3) , Forcex(3) , Grav(3) , Grid(3) , Iconso ,&
         & Iheadb(96) , Iname(12) , Load(3) , Momax(3) , Moment(3) , Mpc(3) , Mpcadd(3) , Mpcax(3) , Neg111(3) , Omit(3) , Omitax(3)&
         & , One , Pload(3) , Pointx(3) , Presax(3) , Rforce(3) , Ringax(3) , Sectax(3) , Seqgp(3) , Spc(3) , Spcadd(3) , Spcax(3) ,&
         & Supax(3) , Suport(3) , T65535(3) , Temp(3) , Tempax(3) , Tempd(3) , Zero
   COMMON /ifp3cm/ File , Iname , Cdtype , Axic1 , Cconex , Forcex , Force , Grav , Load , Momax , Moment , Mpcadd , Mpcax ,        &
                 & Omitax , Pointx , Presax , Ringax , Sectax , Seqgp , Spcax , Supax , Tempax , Tempd , Pload , Mpc , Spc , Grid , &
                 & Suport , Neg111 , T65535 , Temp , Omit , Spcadd , One , Zero , Iheadb , Ctriaa , Ctrapa , Iconso , Rforce
!IFP3BD
!     B L O C K   D A T A   F O R   I F P 3
!
!
!
!
   DATA One/1/ , Zero/0/
!
   DATA File(1) , File(2)/201 , 208/
   DATA File(3) , File(4)/209 , 210/
   DATA File(5) , File(6)/301 , 215/
!
   DATA Iname(1) , Iname(2)/4HGEOM , 4H1   /
   DATA Iname(3) , Iname(4)/4HGEOM , 4H2   /
   DATA Iname(5) , Iname(6)/4HGEOM , 4H3   /
   DATA Iname(7) , Iname(8)/4HGEOM , 4H4   /
   DATA Iname(9) , Iname(10)/4HSCRT , 4HCH  /
   DATA Iname(11) , Iname(12)/4HAXIC , 4H    /
!
   DATA Cdtype(1) , Cdtype(2)/4HAXIC , 4H    /
   DATA Cdtype(3) , Cdtype(4)/4HCCON , 4HEAX /
   DATA Cdtype(5) , Cdtype(6)/4HFORC , 4HEAX /
   DATA Cdtype(7) , Cdtype(8)/4HFORC , 4HE   /
   DATA Cdtype(9) , Cdtype(10)/4HGRAV , 4H    /
   DATA Cdtype(11) , Cdtype(12)/4HLOAD , 4H    /
   DATA Cdtype(13) , Cdtype(14)/4HMOMA , 4HX   /
   DATA Cdtype(15) , Cdtype(16)/4HMOME , 4HNT  /
   DATA Cdtype(17) , Cdtype(18)/4HMPCA , 4HDD  /
   DATA Cdtype(19) , Cdtype(20)/4HMPCA , 4HX   /
   DATA Cdtype(21) , Cdtype(22)/4HOMIT , 4HAX  /
   DATA Cdtype(23) , Cdtype(24)/4HPOIN , 4HTAX /
   DATA Cdtype(25) , Cdtype(26)/4HPRES , 4HAX  /
   DATA Cdtype(27) , Cdtype(28)/4HRING , 4HAX  /
   DATA Cdtype(29) , Cdtype(30)/4HSECT , 4HAX  /
   DATA Cdtype(31) , Cdtype(32)/4HSEQG , 4HP   /
   DATA Cdtype(33) , Cdtype(34)/4HSPCA , 4HDD  /
   DATA Cdtype(35) , Cdtype(36)/4HSPCA , 4HX   /
   DATA Cdtype(37) , Cdtype(38)/4HSUPA , 4HX   /
   DATA Cdtype(39) , Cdtype(40)/4HTEMP , 4HAX  /
   DATA Cdtype(41) , Cdtype(42)/4HTEMP , 4HD   /
   DATA Cdtype(43) , Cdtype(44)/4HCTRI , 4HAAX /
   DATA Cdtype(45) , Cdtype(46)/4HCTRA , 4HPAX /
   DATA Cdtype(47) , Cdtype(48)/4HRFOR , 4HCE  /
!
   DATA Axic1/515 , 5 , 0/
   DATA Cconex/8515 , 85 , 0/
   DATA Forcex/2115 , 21 , 0/
   DATA Force/4201 , 42 , 0/
   DATA Grav/4401 , 44 , 0/
   DATA Load/4551 , 61 , 0/
   DATA Momax/3815 , 38 , 0/
   DATA Moment/4801 , 48 , 0/
   DATA Mpcadd/4891 , 60 , 0/
   DATA Mpcax/4015 , 40 , 0/
   DATA Omitax/4315 , 43 , 0/
   DATA Pointx/4915 , 49 , 0/
   DATA Presax/5215 , 52 , 0/
   DATA Ringax/5615 , 56 , 0/
   DATA Sectax/6315 , 63 , 0/
   DATA Seqgp/5301 , 53 , 0/
   DATA Spcax/6215 , 62 , 0/
   DATA Supax/6415 , 64 , 0/
   DATA Tempax/6815 , 68 , 0/
   DATA Tempd/5641 , 65 , 0/
   DATA Pload/5101 , 51 , 0/
   DATA Mpc/4901 , 49 , 0/
   DATA Spc/5501 , 55 , 0/
   DATA Grid/4501 , 45 , 0/
   DATA Suport/5601 , 56 , 0/
   DATA Temp/5701 , 57 , 0/
   DATA Omit/5001 , 50 , 0/
   DATA Spcadd/5491 , 59 , 0/
   DATA Ctriaa/7012 , 70 , 0/
   DATA Ctrapa/7042 , 74 , 0/
   DATA Rforce/5509 , 55 , 0/
   DATA Iconso/0/
   DATA Neg111/ - 1 , -1 , -1/
   DATA T65535/65535 , 65535 , 65535/
   DATA Iheadb/4HI N  , 4HP U  , 4HT    , 4HD A  , 4HT A  , 4H  E  , 4HR R  , 4HO R  , 4HS    , 4HD E  , 4HT E  , 4HC T  , 4HE D  , &
       &4H  B  , 4HY    , 4HI F  , 4HP 3  , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     ,        &
      & 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H (AX , 4HIS-S , 4HYMME , 4HTRIC , 4H CON , 4HICAL ,        &
      & 4H SHE , 4HLL D , 4HATA  , 4HPROC , 4HESSO , 4HR-GE , 4HNERA , 4HTOR) , 4H     , 4H     , 4H     , 4H     , 4H     ,        &
      & 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     ,        &
      & 4H === , 4H==== , 4H==== , 4H==== , 4H==== , 4H==== , 4H==== , 4H==== , 4H==== , 4H==== , 4H==== , 4H==== , 4H==== ,        &
      & 4H==== , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     ,        &
      & 4H     , 4H     , 4H     , 4H     , 4H    /
!
END BLOCKDATA ifp3bd