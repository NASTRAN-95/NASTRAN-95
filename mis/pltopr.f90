
SUBROUTINE pltopr
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Axis(3) , Bframs , Camera , Daxis(3) , Eof , Icntvl , Idirec , Iwhere , Layer , Model , Ncntr , Nopens , Norg , Npens ,  &
         & Org , Origin(11) , Paptyp(2) , Penclr(8,2) , Pensiz(8) , Ploter , Pltmdl(2) , Pltype , Prject , Prnt , Tapden
   REAL Cntin3 , Cntr(50) , Cntsin , Cscale , D0 , For , Origx3(11,4) , Papsiz(2) , Pbufsz , Penpap , R0 , S0l , S0r , S0s ,        &
      & Scale(2) , Skip23(23) , Skpa(2) , Skpb(3) , Skpc(2) , Skpcom(20) , Skpd(2) , Skpplt(17) , Skpscl(3) , T0 , Vangle(9) ,      &
      & Vantx1 , Vantx2(2) , Xy(11,3)
   COMMON /blank / Skpcom , Prnt
   COMMON /pltdat/ Model , Ploter , Skpplt , Cscale , Skpa , Cntsin , Skpb , Nopens , Skpc , Pltype , Skpd , Eof , Cntin3
   COMMON /xxparm/ Pbufsz , Camera , Bframs , Pltmdl , Tapden , Npens , Papsiz , Paptyp , Pensiz , Penclr , Penpap , Scale ,        &
                 & Skpscl , Axis , Daxis , Vangle , Vantx1 , R0 , S0l , S0r , T0 , D0 , Vantx2 , Prject , S0s , For , Org , Norg ,  &
                 & Origin , Origx3 , Xy , Ncntr , Cntr , Icntvl , Iwhere , Idirec , Skip23 , Layer
!
! Local variable declarations
!
   INTEGER a1(9) , a2(12) , a3(25) , a4(13) , a5(17) , a6(6) , b1(16) , b2(16) , b5(10) , blank , c1(16) , c2(17) , c3(24) , d1(11) &
         & , d2(6) , d3(29) , displa(10) , dist(12) , e1(12) , e2(20) , e3(14) , e4(18) , f1(22) , f2(25) , f3(12) , f4(23) ,       &
         & film(2) , g1(13) , g2(11) , g3(16) , g4(6) , g5(11) , g6(8) , g7(4) , h1(11) , i , ilay(20) , itype(4) , j , k , l ,     &
         & list(20) , m , minus , mm , n , na1 , na2 , na3 , na4 , na5 , na6 , nb1 , nb2 , nb5 , nc1 , nc2 , nc3 , nd1 , nd2 , nd3 ,&
         & ne1 , ne2 , ne3 , ne4 , nf1 , nf2 , nf3 , nf4 , ng1 , ng2 , ng3 , ng4 , ng5 , ng6 , ng7 , nh1 , np6 , nskip , p6(23)
   REAL alst(20)
   INTEGER paper(2) , plttyp(3,3) , plus , prj(3,3) , skip , space , strain(4) , stress(34) , symm(4) , way(4) , xyz(3)
!
! End of declarations
!
!
   EQUIVALENCE (list(1),alst(1))
!
   DATA nskip , skip/1 , 4H(1X)/
   DATA film , paper/4HFILM , 1H  , 4HPAPE , 1HR/
!
!     PLOTTER TYPE FORMATS.
!
   DATA np6/23/
   DATA p6/4H(10X , 4H,38H , 4HTHE  , 4HFOLL , 4HOWIN , 4HG PL , 4HOTS  , 4HARE  , 4HFOR  , 4HA NA , 4HSTPL , 4HT ,2 , 4HA4,A ,     &
       &4H2,8H , 4HPLOT , 4HTER  , 4H,2A4 , 4H,17H , 4HTYPI , 4HNG C , 4HAPAB , 4HILIT , 4HY,/)/
   DATA plttyp/4HMICR , 4HOFIL , 1HM , 4H  TA , 4HBLE  , 1H  , 4H  DR , 4HUM   , 1H /
   DATA itype/4HWITH , 4H     , 4HWITH , 4HOUT /
!
!     GENERAL PLOTTER FORMATS.
!
   DATA na1/9/
   DATA a1/4H(//, , 4H25H  , 4HP L  , 4HO T  , 4HT E  , 4HR    , 4H D A , 4H T A , 4H,/) /
   DATA na2/12/
   DATA a2/4H(10X , 4H,27H , 4HTHE  , 4HPLOT , 4H TAP , 4HE IS , 4H WRI , 4HTTEN , 4H AT, , 4HI4,4 , 4HH BP , 4HI,/)/
   DATA na3/25/
   DATA a3/4H(10X , 4H,89H , 4HTHE  , 4HPLOT , 4HS AR , 4HE SE , 4HPARA , 4HTED  , 4HBY E , 4HND-O , 4HF-FI , 4HLE M , 4HARKS ,     &
       &4H...T , 4HWO E , 4HND-O , 4HF-FI , 4HLE M , 4HARKS , 4H FOL , 4HLOW  , 4HTHE  , 4HLAST , 4H PLO , 4HT,/)/
   DATA na4/13/
   DATA a4/4H(10X , 4H,41H , 4HAN E , 4HND-O , 4HF-FI , 4HLE M , 4HARK  , 4HFOLL , 4HOWS  , 4HTHE  , 4HLAST , 4H PLO , 4HT,/)/
   DATA na5/17/
   DATA a5/4H(10X , 4H,56H , 4HTHE  , 4HFIRS , 4HT CO , 4HMMAN , 4HD FO , 4HR EA , 4HCH P , 4HLOT  , 4HCONT , 4HAINS , 4H THE ,     &
       &4H PLO , 4HT NU , 4HMBER , 4H,/) /
   DATA na6/6/
   DATA a6/4H(10X , 4H,9HC , 4HSCAL , 4HE =  , 4H,F5. , 4H2,/)/
!
!     TABLE PLOTTER FORMATS.
!
   DATA nb1/16/
   DATA b1/4H(10X , 4H,30H , 4HSET  , 4HTHE  , 4HX +  , 4HY SC , 4HALE  , 4HFACT , 4HORS  , 4HAT,F , 4H6.1, , 4H12H  , 4HCOUN ,     &
       &4HTS/I , 4HNCH, , 4H/)  /
   DATA nb2/16/
   DATA b2/4H(10X , 4H,12H , 4HPAPE , 4HR SI , 4HZE = , 4H,F5. , 4H1,2H , 4H X,F , 4H5.1, , 4H16H, , 4H  PA , 4HPER  , 4HTYPE ,     &
       &4H = , , 4H2A4, , 4H/)  /
   DATA nb5/10/
   DATA b5/4H(10X , 4H,3HP , 4HEN,I , 4H2,7H , 4H - S , 4HIZE, , 4HI2,2 , 4HH, , , 4H2A4, , 4H/)  /
!
!     ELECTRONIC PLOTTER FORMATS.
!
   DATA nc1/16/
   DATA c1/4H(10X , 4H,37H , 4HTHE  , 4HFOLL , 4HOWIN , 4HG PL , 4HOTS  , 4HARE  , 4HREQU , 4HESTE , 4HD ON , 4H ,A4 , 4H,A1, ,     &
       &4H5H O , 4HNLY, , 4H/)  /
   DATA nc2/17/
   DATA c2/4H(10X , 4H,54H , 4HTHE  , 4HFOLL , 4HOWIN , 4HG PL , 4HOTS  , 4HARE  , 4HREQU , 4HESTE , 4HD ON , 4H BOT , 4HH FI ,     &
       &4HLM + , 4H PAP , 4HER,/ , 4H)   /
   DATA nc3/24/
   DATA c3/4H(10X , 4H,I1, , 4H79H  , 4HBLAN , 4HK FR , 4HAMES , 4H WIL , 4HL BE , 4H INS , 4HERTE , 4HD ON , 4H FIL , 4HM ON ,     &
       &4HLY B , 4HETWE , 4HEN E , 4HACH  , 4HOF T , 4HHE F , 4HOLLO , 4HWING , 4H PLO , 4HTS,/ , 4H)   /
!
!     ENGINEERING DATA FORMATS.
!
   DATA nd1/11/
   DATA d1/4H(//3 , 4H3H E , 4H N G , 4H I N , 4H E E , 4H R I , 4H N G , 4H     , 4HD A  , 4HT A, , 4H/)  /
   DATA nd2/6/
   DATA d2/4H(10X , 4H,3A4 , 4H,11H , 4H PRO , 4HJECT , 4HION)/
   DATA nd3/29/
   DATA d3/4H(10X , 4H,29H , 4HROTA , 4HTION , 4HS (D , 4HEGRE , 4HES)  , 4H- GA , 4HMMA  , 4H=,F7 , 4H.2,8 , 4HH, B , 4HETA  ,     &
       &4H=,F7 , 4H.2,9 , 4HH, A , 4HLPHA , 4H =,F , 4H7.2, , 4H10H, , 4H  AX , 4HES = , 4H ,2A , 4H1,2( , 4H1H,, , 4H2A1) ,        &
      & 4H,2H, , 4H ,4A , 4H4)  /
   DATA prj , plus , minus , xyz , symm , blank/4HORTH , 4HOGRA , 4HPHIC , 4HPERS , 4HPECT , 4HIVE  , 4HSTER , 4HEOSC , 4HOPIC ,    &
      & 1H+ , 1H- , 1HX , 1HY , 1HZ , 4HANTI , 4HSYMM , 4HETRI , 1HC , 1H /
!
!     ORTHOGRAPHIC + PERSPECTIVE ENGINEERING DATA FORMATS.
!
   DATA ne1/12/
   DATA e1/4H(10X , 4H,29H , 4HSCAL , 4HE (O , 4HBJEC , 4HT-TO , 4H-PLO , 4HT SI , 4HZE)  , 4H=,1P , 4H,E13 , 4H.6) /
   DATA ne2/20/
   DATA e2/4H(10X , 4H,29H , 4HVANT , 4HAGE  , 4HPOIN , 4HT (I , 4HNCHE , 4HS) - , 4H RO  , 4H=,1P , 4H,E13 , 4H.6,6 , 4HH, S ,     &
       &4H0 =, , 4HE13. , 4H6,6H , 4H, T0 , 4H =,E , 4H13.6 , 4H)   /
   DATA ne3/14/
   DATA e3/4H(10X , 4H,38H , 4HPROJ , 4HECTI , 4HON P , 4HLANE , 4H SEP , 4HARAT , 4HION  , 4H(INC , 4HHES) , 4H =,1 , 4HP,E1 ,     &
       &4H3.6)/
   DATA ne4/18/
   DATA e4/4H(10X , 4H,6HO , 4HRIGI , 4HN,I8 , 4H,11H , 4H   - , 4H   X , 4H0 =, , 4H1P,E , 4H14.6 , 4H,6H, , 4H Y0  , 4H=,E1 ,     &
       &4H4.6, , 4H5X,8 , 4HH(IN , 4HCHES , 4H))  /
!
!     STEREO ENGINEERING DATA FORMATS.
!
   DATA nf1/22/
   DATA f1/4H(10X , 4H,30H , 4HSCAL , 4HES - , 4H (MO , 4HDEL- , 4HTO-P , 4HLOT  , 4HSIZE , 4H =,1 , 4HP,E1 , 4H3.6, , 4H25H, ,     &
       &4H  OB , 4HJECT , 4H-TO- , 4HMODE , 4HL SI , 4HZE = , 4H,E13 , 4H.6,1 , 4HH)) /
   DATA nf2/25/
   DATA f2/4H(10X , 4H,29H , 4HVANT , 4HAGE  , 4HPOIN , 4HT (I , 4HNCHE , 4HS) - , 4H R0  , 4H=,1P , 4H,E13 , 4H.6,9 , 4HH, S ,     &
       &4H0(L) , 4H =,E , 4H13.6 , 4H,9H, , 4H S0( , 4HR) = , 4H,E13 , 4H.6,6 , 4HH, T , 4H0 =, , 4HE13. , 4H6)  /
   DATA nf3/12/
   DATA f3/4H(10X , 4H,28H , 4HOCUL , 4HAR S , 4HEPAR , 4HATIO , 4HN (I , 4HNCHE , 4HS) = , 4H,1P, , 4HE13. , 4H6)  /
   DATA nf4/23/
   DATA f4/4H(10X , 4H,6HO , 4HRIGI , 4HN,I8 , 4H,14H , 4H   - , 4H   X , 4H0(L) , 4H =,1 , 4HP,E1 , 4H4.6, , 4H9H,  , 4HX0(R ,     &
       &4H) =, , 4HE14. , 4H6,6H , 4H, Y0 , 4H =,E , 4H14.6 , 4H,5X, , 4H8H(I , 4HNCHE , 4HS)) /
!
!     CONTOUR PLOTTING DATA FORMATS
!
   DATA ng1/13/g1/4H(//4 , 4H2H C , 4H O N , 4H T O , 4H U R , 4H   P , 4H L O , 4H T T , 4H I N , 4H G   , 4H D A , 4H T A ,       &
      & 4H,/) /
   DATA ng2/11/g2/4H(9X, , 4H32HA , 4HBOVE , 4H PLO , 4HT IS , 4H A C , 4HONTO , 4HUR P , 4HLOT  , 4HOF , , 4H4A4)/
   DATA ng3/16/g3/4H(9X, , 4H52HT , 4HHE C , 4HONTO , 4HUR V , 4HALUE , 4HS AR , 4HE CA , 4HLCUL , 4HATED , 4H AT  , 4HFIBR ,       &
      & 4HE DI , 4HSTAN , 4HCE , , 4H3A4)/
   DATA ng4/6/g4/4H(9X, , 4H4HIN , 4H A,2 , 4HA4,6 , 4HHSYS , 4HTEM)/
   DATA ng5/11/g5/4H(//, , 4H51X, , 4H28HT , 4HABLE , 4H  OF , 4H  PL , 4HOTTI , 4HNG   , 4HSYMB , 4HOLS, , 4H/)  /
   DATA ng6/8/g6/4H(5(5 , 4HX,13 , 4HHSYM , 4HBOL  , 4H VAL , 4HUE,6 , 4HX),/ , 4H)   /
   DATA ng7/4/g7/4H(5(I , 4H9,1P , 4H,E15 , 4H.6))/
!
   DATA nh1/11/
   DATA h1/4H(//5 , 4H0X,2 , 4H9HPL , 4HOT M , 4HODUL , 4HE ME , 4HSSAG , 4HES C , 4HONTI , 4HNUE  , 4H,/) /
!
   DATA strain/4HSTRA , 4HIN E , 4HNERG , 4HIES / , dist/4H Z2  , 2*1H  , 4H Z1  , 2*1H  , 4HMAX  , 4H- Z1 , 4H,Z2  , 4HAVER ,      &
       &4H-Z1, , 4HZ2  / , way/4H LOC , 4HAL   , 4H COM , 4HMON /
!
   DATA space/4H    /displa/4HDEFO , 4HRMAT , 4HION  , 1HX , 1HY , 1HZ , 3HMAG , 3*0/
!
!                              1              3
!               5 (1)          7 (2)          9 (3)         11 (4)
!              13 (5)         15 (6)         17     18      19
!              20 (14)        22 (15)        24(16)         26 (17)
!              28 (18)        30 (19)        32             34
   DATA stress/4HSTRE , 4HSS,  , 4HSHEA , 4HR -  , 4HMAJO , 4HR-PR , 4HMINO , 4HR-PR , 4HMAXI , 4HMUM  , 4HNORM , 4HAL X , 4HNORM , &
       &4HAL Y , 4HNORM , 4HAL Z , 4HXY   , 4HXZ   , 4HYZ   , 4HNORM , 4HAL 1 , 4HNORM , 4HAL 2 , 4HSHEA , 4HR 12 , 4HSHEA ,        &
      & 4HR 1Z , 4HSHEA , 4HR 1Z , 4HBOND , 4HSH12 , 4HLAYE , 4HR NU , 4HMBER/
!
   DATA ilay/4H  1  , 4H  2  , 4H  3  , 4H  4  , 4H  5  , 4H  6  , 4H  7  , 4H  8  , 4H  9  , 4H 10  , 4H 11  , 4H 12  , 4H 13  ,   &
       &4H 14  , 4H 15  , 4H 16  , 4H 17  , 4H 18  , 4H 19  , 4H 20 /
!
   IF ( Ncntr>0 ) THEN
!
!     CONTOUR PLOTTING DATA
!
      list(1) = 0
      CALL wrtprt(Prnt,list,g1,ng1)
      list(1) = 4
      IF ( Icntvl>9 .AND. Icntvl<14 ) THEN
!
!     DISPLACEMENT CONTOURS
!
         i = 1
         list(2) = displa(i)
         list(3) = displa(i+1)
         list(4) = displa(i+2)
         list(5) = displa(Icntvl-6)
         CALL wrtprt(Prnt,list,g2,ng2)
         j = 3
      ELSE
!
!     STRESS CONTOURS
!
         i = 1
         IF ( Icntvl>6 .OR. Icntvl==3 ) i = 3
         IF ( Icntvl>=14 .AND. Icntvl<=19 ) i = 1
         IF ( Icntvl/=20 ) THEN
!
            list(2) = stress(i)
            list(3) = stress(i+1)
            i = Icntvl*2 + 3
            IF ( Icntvl>13 .AND. Icntvl<20 ) i = (Icntvl-14)*2 + 20
            IF ( Icntvl>6 .AND. Icntvl<=9 ) i = Icntvl + 10
            list(4) = stress(i)
            list(5) = space
            IF ( Icntvl<7 .OR. Icntvl>=14 ) list(5) = stress(i+1)
            CALL wrtprt(Prnt,list,g2,ng2)
!
!     ADDING LAYER NUMBER TO OUTPUT WHEN REQUESTED
!
            IF ( Icntvl<14 .OR. Icntvl==20 ) THEN
!
               list(1) = 3
               i = Iwhere
               IF ( Iwhere<=0 ) i = 0
               i = i*3 + 1
               list(2) = dist(i)
               list(3) = dist(i+1)
               list(4) = dist(i+2)
               CALL wrtprt(Prnt,list,g3,ng3)
            ELSE
               list(1) = 4
               list(2) = stress(32)
               list(3) = stress(33)
               list(4) = stress(34)
               list(5) = ilay(Layer)
               CALL wrtprt(Prnt,list,g2,ng2)
            ENDIF
         ELSE
!
!     STRAIN CONTOURS
!
            list(1) = 4
            list(2) = strain(1)
            list(3) = strain(2)
            list(4) = strain(3)
            list(5) = strain(4)
            CALL wrtprt(Prnt,list,g2,ng2)
         ENDIF
!
         j = 2*(Idirec-1) + 1
      ENDIF
      IF ( Icntvl>=4 .AND. Icntvl/=13 ) THEN
         list(1) = 2
         list(2) = way(j)
         list(3) = way(j+1)
         CALL wrtprt(Prnt,list,g4,ng4)
      ENDIF
      list(1) = 0
      CALL wrtprt(Prnt,list,g5,ng5)
      CALL wrtprt(Prnt,list,g6,ng6)
      l = (Ncntr-1)/10 + 1
      list(1) = 2*l
      k = min0(Ncntr,10)
      DO j = 1 , k
         n = j + (l-1)*10
         m = 2
         DO i = j , n , 10
            IF ( i>Ncntr ) GOTO 20
            list(m) = i
            alst(m+1) = Cntr(i)
            m = m + 2
         ENDDO
         GOTO 40
 20      list(1) = list(1) - 2
         l = l - 1
 40      CALL wrtprt(Prnt,list,g7,ng7)
      ENDDO
   ELSE
!
!     PRINT THE PLOTTER ID.
!
      list(1) = 0
      CALL write(Prnt,list,1,0)
      CALL wrtprt(Prnt,list,a1,na1)
!
!     NASTRAN GENERAL PURPOSE PLOTTER.
!
      list(1) = 5
      j = iabs(Pltype)
      DO i = 1 , 3
         list(i+1) = plttyp(i,j)
      ENDDO
      mm = 1
      IF ( Pltype<0 ) mm = 3
      list(5) = itype(mm)
      list(6) = itype(mm+1)
      CALL wrtprt(Prnt,list,p6,np6)
!
!     GENERAL PLOTTER INFORMATION.
!
      IF ( Tapden>0 ) THEN
         list(1) = 1
         list(2) = Tapden
         CALL wrtprt(Prnt,list,a2,na2)
      ENDIF
      IF ( Eof/=0 ) THEN
         CALL wrtprt(Prnt,0,a4,na4)
      ELSE
         CALL wrtprt(Prnt,0,a3,na3)
      ENDIF
      CALL wrtprt(Prnt,0,a5,na5)
      list(1) = 1
      alst(2) = Cscale
      CALL wrtprt(Prnt,list,a6,na6)
      IF ( iabs(Pltype)<2 ) THEN
!
!     ELECTRONIC PLOTTER INFORMATION.
!
         IF ( Camera<2 ) THEN
            list(2) = film(1)
            list(3) = film(2)
         ELSEIF ( Camera==2 ) THEN
            list(2) = paper(1)
            list(3) = paper(2)
         ELSE
            CALL wrtprt(Prnt,0,c2,nc2)
            GOTO 60
         ENDIF
         list(1) = 2
         CALL wrtprt(Prnt,list,c1,nc1)
 60      IF ( Camera/=2 .AND. Bframs/=0 ) THEN
            list(1) = 1
            list(2) = Bframs
            CALL wrtprt(Prnt,list,c3,nc3)
         ENDIF
         GOTO 100
      ELSEIF ( iabs(Pltype)==2 ) THEN
!
!     TABLE PLOTTER INFORMATION.
!
         list(1) = 1
         alst(2) = Cntsin
         CALL wrtprt(Prnt,list,b1,nb1)
      ENDIF
      list(1) = 4
      alst(2) = Papsiz(1)
      alst(3) = Papsiz(2)
      list(4) = Paptyp(1)
      list(5) = Paptyp(2)
      CALL wrtprt(Prnt,list,b2,nb2)
!
      list(1) = 4
      n = min0(Npens,Nopens)
      DO i = 1 , n
         list(2) = i
         list(3) = Pensiz(i)
         IF ( list(3)>=0 ) THEN
            list(4) = Penclr(i,1)
            list(5) = Penclr(i,2)
            IF ( list(4)/=blank .OR. list(5)/=blank ) CALL wrtprt(Prnt,list,b5,nb5)
         ENDIF
      ENDDO
      CALL wrtprt(Prnt,0,skip,nskip)
!
!     ENGINEERING DATA.
!
 100  CALL wrtprt(Prnt,0,d1,nd1)
      list(1) = 3
      DO i = 1 , 3
         list(i+1) = prj(i,Prject)
      ENDDO
      CALL wrtprt(Prnt,list,d2,nd2)
!
      list(1) = 13
      alst(2) = Vangle(3)
      IF ( Vangle(2)<=-1.E10 ) THEN
         IF ( Prject/=2 ) Vangle(2) = Vangle(4)
         IF ( Prject==2 ) Vangle(2) = Vangle(5)
      ENDIF
      alst(3) = Vangle(2)
      alst(4) = Vangle(1)
      DO i = 1 , 3
         j = 2*i + 3
         k = iabs(Axis(i))
         list(j) = plus
         IF ( Axis(i)<0 ) list(j) = minus
         list(j+1) = xyz(k)
      ENDDO
      n = 1
      IF ( Axis(1)==Daxis(1) ) n = 2
      list(14) = blank
      j = 1
      DO i = n , 4
         list(j+10) = symm(i)
         j = j + 1
      ENDDO
      CALL wrtprt(Prnt,list,d3,nd3)
      IF ( Prject==3 ) THEN
!
!     STEREO ENGINEERING DATA.
!
         list(1) = 2
         alst(2) = Scale(1)/Cntin3
         alst(3) = Scale(2)
         CALL wrtprt(Prnt,list,f1,nf1)
         list(1) = 4
         alst(2) = R0
         alst(3) = S0l
         alst(4) = S0r
         alst(5) = T0
         CALL wrtprt(Prnt,list,f2,nf2)
         list(1) = 1
         alst(2) = D0
         CALL wrtprt(Prnt,list,e3,ne3)
         alst(2) = S0s
         CALL wrtprt(Prnt,list,f3,nf3)
!
         CALL wrtprt(Prnt,0,skip,nskip)
         list(1) = 4
         DO i = 1 , Org
            list(2) = Origin(i)
            alst(3) = Xy(i,1)/Cntsin
            alst(4) = Xy(i,2)/Cntsin
            alst(5) = Xy(i,3)/Cntsin
            CALL wrtprt(Prnt,list,f4,nf4)
         ENDDO
      ELSE
!
!     ORTHOGRAPHIC + PERSPECTIVE ENGINEERING DATA.
!
         list(1) = 1
         alst(2) = Scale(1)/Cntsin
         CALL wrtprt(Prnt,list,e1,ne1)
         IF ( Prject/=1 ) THEN
            list(1) = 3
            alst(2) = R0
            alst(3) = S0l
            alst(4) = T0
            CALL wrtprt(Prnt,list,e2,ne2)
            list(1) = 1
            alst(2) = D0
            CALL wrtprt(Prnt,list,e3,ne3)
         ENDIF
!
         CALL wrtprt(Prnt,0,skip,nskip)
         list(1) = 3
         DO i = 1 , Org
            list(2) = Origin(i)
            alst(3) = Xy(i,1)/Cntsin
            alst(4) = Xy(i,3)/Cntsin
            CALL wrtprt(Prnt,list,e4,ne4)
         ENDDO
      ENDIF
   ENDIF
!
   list(1) = 0
   CALL wrtprt(Prnt,list,h1,nh1)
END SUBROUTINE pltopr
