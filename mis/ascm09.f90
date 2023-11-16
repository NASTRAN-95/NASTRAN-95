
SUBROUTINE ascm09(Name,Iphase,Isol,Nogo)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Idat(882) , Ioct , Iph , Iptbs , Irdm , Ixtra , Noct , Nph , Nptbs , Nrdm , Nxtra
   COMMON /asdbd / Irdm , Nrdm , Ixtra , Nxtra , Ioct , Noct , Iptbs , Nptbs , Iph , Nph , Idat
!
! Dummy argument declarations
!
   INTEGER Iphase , Isol , Name , Nogo
!
! Local variable declarations
!
   INTEGER comnd(6,1) , i , icomnd , isave(30) , j , k , l , oct(3,16) , oct1(3,16) , ptbs(7,53) , ptbs1(7,18) , ptbs2(7,18) ,      &
         & ptbs3(7,17) , rdmap(18,25) , rdmap1(18,9) , rdmap2(18,9) , rdmap3(18,7) , subnam(2) , xtra(13)
   INTEGER khrfn1
   REAL slash
!
! End of declarations
!
!
!     MREDUCE COMMAND DMAP DATA
!
   EQUIVALENCE (rdmap1(1,1),rdmap(1,1)) , (oct1(1,1),oct(1,1)) , (rdmap2(1,1),rdmap(1,10)) , (ptbs1(1,1),ptbs(1,1)) ,               &
    & (rdmap3(1,1),rdmap(1,19)) , (ptbs2(1,1),ptbs(1,19)) , (ptbs3(1,1),ptbs(1,37))
   DATA comnd/4HMRED , 25 , 13 , 16 , 53 , 0/
   DATA slash/1H//
   DATA isave/1 , 15 , 1 , 2 , 11 , 2 , 4 , 12 , 1 , 4 , 16 , 3 , 5 , 5 , 1 , 19 , 7 , 3 , 19 , 8 , 3 , 19 , 9 , 3 , 22 , 15 , 2 ,  &
      & 24 , 6 , 2/
   DATA rdmap1/4HMRED , 4H1    , 4H  CA , 4HSECC , 4H,GEO , 4HM4,D , 4HYNAM , 4HICS, , 4HCSTM , 4H/USE , 4HTR,E , 4HEDR, , 4HEQST , &
       &4H,DMR , 4H!*NA , 4HMEA  , 4H  */ , 4H     , 4H     , 4H     , 4H  S, , 4HN,DR , 4HY/ST , 4HP/S, , 4HN,NO , 4HFIX/ ,        &
      & 4HS,N, , 4HSKIP , 4HM!*R , 4HEAL* , 4H $   , 5*4H     , 4HCOND , 4H     , 4H  LB , 4HM3ST , 4HP,DR , 4HY $  , 12*4H     ,   &
       &4HSOFI , 4H     , 4H  /K , 4HNOA, , 4HMNOA , 4H,PNO , 4HA,BN , 4HOA,K , 4H4NOA , 4H/S,N , 4H,DRY , 4H!*NA , 4HMEA  ,        &
      & 4H  */ , 4H*KMT , 4HX*!* , 4HMMTX , 4H*/   , 4H     , 4H     , 4H  *P , 4HVEC* , 4H!*BM , 4HTX*/ , 4H*K4M , 4HX* $ ,        &
      & 4H     , 4H     , 8*4H     , 4HCOND , 4H     , 4H  LB , 4HM2ST , 4HP,SK , 4HIPM  , 4H$    , 4H     , 4H     , 4H     ,      &
       &8*4H     , 4HEQUI , 4HV    , 4H  KN , 4HOA,K , 4HFFX/ , 4HNOFI , 4HX $  , 4H     , 4H     , 4H     , 8*4H     , 4HEQUI ,    &
       &4HV    , 4H  MN , 4HOA,M , 4HFFX/ , 4HNOFI , 4HX $  , 4H     , 4H     , 4H     , 8*4H     , 4HEQUI , 4HV    , 4H  BN ,      &
       &4HOA,B , 4HFFX/ , 4HNOFI , 4HX $  , 4H     , 4H     , 4H     , 8*4H    /
   DATA rdmap2/4HEQUI , 4HV    , 4H  K4 , 4HNOA, , 4HK4FF , 4HX/NO , 4HFIX  , 4H$    , 4H     , 4H     , 8*4H     , 4HCOND ,        &
      & 4H     , 4H  LB , 4HM1ST , 4HP,NO , 4HFIX  , 4H$    , 4H     , 4H     , 4H     , 8*4H     , 4HSCE1 , 4H     , 4H  US ,      &
       &4HETR, , 4HKNOA , 4H,MNO , 4HA,BN , 4HOA,K , 4H4NOA , 4H/KFF , 4HX,KF , 4HSX,K , 4HSSX, , 4HMFFX , 4H,BFF , 4HX,K4 ,        &
      & 4HFFX  , 4H$    , 4HLABE , 4HL    , 4H  LB , 4HM1ST , 4HP $  , 13*4H     , 4HREAD , 4H     , 4H  KF , 4HFX,M , 4HFFX, ,     &
       &4HBFFX , 4H,K4F , 4HFX,E , 4HEDR, , 4HUSET , 4HR,/L , 4HAMAR , 4H,PHI , 4HR,MI , 4HR,OE , 4HIGR/ , 4H*MOD , 4HES*/ ,        &
      & 4H     , 4H     , 4H  NE , 4HIGVS , 4H $   , 13*4H     , 4HOFP  , 4H     , 4H  LA , 4HMAR, , 4HOEIG , 4HR,,, , 4H,//  ,     &
       &4H$    , 4H     , 4H     , 8*4H     , 4HEQUI , 4HV    , 4H  PH , 4HIR,P , 4HHIS/ , 4HNOFI , 4HX $  , 4H     , 4H     ,      &
       &4H     , 8*4H     , 4HCOND , 4H     , 4H  LB , 4HM2ST , 4HP,NO , 4HFIX  , 4H$    , 4H     , 4H     , 4H     , 8*4H    /
   DATA rdmap3/4HUMER , 4HGE   , 4H  US , 4HETR, , 4HPHIR , 4H,/PH , 4HIS!* , 4HN*!* , 4HF*!* , 4HS* $ , 8*4H     , 4HLABE ,        &
      & 4HL    , 4H  LB , 4HM2ST , 4HP $  , 13*4H     , 4HMRED , 4H2    , 4H  CA , 4HSECC , 4H,LAM , 4HAR,P , 4HHIS, , 4HEQST ,     &
       &4H,USE , 4HTR,K , 4HNOA, , 4HMNOA , 4H,BNO , 4HA,K4 , 4HNOA, , 4HPNOA , 4H,DMR , 4H,    , 4H     , 4H     , 4H  QS ,        &
      & 4HM/KN , 4HOB,M , 4HNOB, , 4HBNOB , 4H,K4N , 4HOB,P , 4HNOB, , 4HPONO , 4HB/ST , 4HP/S, , 4HN,DR , 4HY!*P , 4HVEC* ,        &
      & 4H $   , 4H     , 4HLABE , 4HL    , 4H  LB , 4HM3ST , 4HP $  , 13*4H     , 4HLODA , 4HPP   , 4H  PN , 4HOB,P , 4HONOB ,     &
       &4H/!*N , 4HAMEB , 4H   * , 4H/S,N , 4H,DRY , 4H $   , 7*4H     , 4HCOND , 4H     , 4H  FI , 4HNIS, , 4HDRY  , 4H$    ,      &
       &12*4H    /
   DATA xtra/4HNAME , 4HBOUN , 4HFIXE , 4HMETH , 4HRANG , 4HNMAX , 4HOLDM , 4HOLDB , 4HUSER , 4HOUTP , 4HRGRI , 4HRNAM , 4HRSAV/
   DATA oct1/6 , 8 , 0 , 7 , 8 , 1 , 8 , 8 , 2 , 9 , 8 , 16 , 10 , 8 , 32 , 11 , 8 , 0 , 12 , 8 , 0 , 13 , 8 , 0 , 14 , 8 , 0 , 15 ,&
      & 8 , 0 , 16 , 8 , 0 , 17 , 8 , 0 , 18 , 8 , 0 , 19 , 8 , 0 , 20 , 8 , 0 , 24 , 0 , 8/
   DATA ptbs1/1 , 59 , 59 , 8 , 4HNAMA , 0 , 0 , 2 , 19 , 19 , 3 , 4HSTEP , 0 , 0 , 3 , 15 , 15 , 3 , 4HSTEP , 0 , 0 , 4 , 12 , 13 ,&
      & 3 , 4HNONA , 1 , -1 , 4 , 17 , 18 , 3 , 4HNONA , 2 , -1 , 4 , 22 , 23 , 3 , 4HNONA , 12 , -1 , 4 , 27 , 28 , 3 , 4HNONA ,   &
      & 16 , -1 , 4 , 32 , 34 , 3 , 4HNONA , 32 , -1 , 4 , 47 , 47 , 8 , 4HNAMA , 0 , 0 , 5 , 12 , 12 , 4 , 4HPITM , 0 , 0 , 6 ,    &
      & 15 , 15 , 3 , 4HSTEP , 0 , 0 , 7 , 11 , 12 , 3 , 4HNONA , 0 , 0 , 8 , 11 , 12 , 3 , 4HNONA , 0 , 0 , 9 , 11 , 12 , 3 ,      &
       &4HNONA , 0 , 0 , 10 , 11 , 13 , 3 , 4HNONA , 0 , 0 , 11 , 15 , 15 , 3 , 4HSTEP , 0 , 0 , 12 , 17 , 18 , 3 , 4HNONA , 1 , 0 ,&
      & 12 , 22 , 23 , 3 , 4HNONA , 2 , 0/
   DATA ptbs2/12 , 27 , 28 , 3 , 4HNONA , 16 , 0 , 12 , 32 , 34 , 3 , 4HNONA , 32 , 0 , 12 , 38 , 42 , 0 , 4HNAMA , 1 , 0 , 12 ,    &
      & 43 , 47 , 0 , 4HNAMA , 1 , 0 , 12 , 48 , 52 , 0 , 4HNAMA , 1 , 0 , 12 , 53 , 57 , 0 , 4HNAMA , 2 , 0 , 12 , 58 , 62 , 0 ,   &
       &4HNAMA , 16 , 0 , 12 , 63 , 68 , 0 , 4HNAMA , 32 , 0 , 13 , 15 , 15 , 3 , 4HSTEP , 0 , 0 , 14 , 11 , 15 , 0 , 4HNAMA , 1 ,  &
      & 0 , 14 , 16 , 20 , 0 , 4HNAMA , 2 , 0 , 14 , 21 , 25 , 0 , 4HNAMA , 16 , 0 , 14 , 26 , 31 , 0 , 4HNAMA , 32 , 0 , 18 , 15 , &
      & 15 , 3 , 4HSTEP , 0 , 0 , 20 , 15 , 15 , 3 , 4HSTEP , 0 , 0 , 21 , 18 , 23 , 0 , 4HNAMA , 55 , 0 , 21 , 24 , 28 , 0 ,       &
      & 4HNAMA , 55 , 0 , 21 , 40 , 41 , 3 , 4HNONA , 1 , 0/
   DATA ptbs3/21 , 45 , 46 , 3 , 4HNONA , 2 , 0 , 21 , 50 , 51 , 3 , 4HNONA , 16 , 0 , 21 , 55 , 57 , 3 , 4HNONA , 32 , 0 , 21 ,    &
      & 61 , 62 , 3 , 4HNONA , 12 , 0 , 22 , 11 , 14 , 0 , 4HNAMA , 131072 , 0 , 22 , 15 , 16 , 3 , 4HNONB , 1 , -1 , 22 , 20 , 21 ,&
      & 3 , 4HNONB , 2 , -1 , 22 , 25 , 26 , 3 , 4HNONB , 16 , -1 , 22 , 30 , 32 , 3 , 4HNONB , 32 , -1 , 22 , 36 , 37 , 3 ,        &
      & 4HNONB , 12 , -1 , 22 , 41 , 43 , 3 , 4HNONB , 12 , -1 , 22 , 47 , 47 , 3 , 4HSTEP , 0 , 0 , 22 , 60 , 60 , 4 , 4HPITM ,    &
      & 12 , 0 , 23 , 15 , 15 , 3 , 4HSTEP , 0 , 0 , 24 , 11 , 12 , 3 , 4HNONB , 0 , 0 , 24 , 16 , 18 , 3 , 4HNONB , 0 , 0 , 24 ,   &
      & 24 , 24 , 8 , 4HNAMB , 0 , 0/
   DATA subnam/4HASCM , 2H09/
!
!     RESTORE TO ORIGINAL DATA BY REPLACEING ! BY / IN RDMAP ARRAY
!     (SEE ASCM01 FOR EXPLANATION))
!
   DO l = 1 , 30 , 3
      i = isave(l+1)
      j = isave(l)
      k = isave(l+2)
      rdmap(i,j) = khrfn1(rdmap(i,j),k,slash,1)
   ENDDO
!
!     VALIDATE COMMAND AND SET POINTERS
!
   IF ( Name/=comnd(1,1) ) THEN
!
!     INPUT ERROR
!
      CALL mesage(7,0,subnam)
      Nogo = 1
      GOTO 99999
   ELSE
      icomnd = 1
      Irdm = 1
      Nrdm = comnd(2,icomnd)
      Ixtra = Irdm + 18*Nrdm
      Nxtra = comnd(3,icomnd)
      Ioct = Ixtra + Nxtra
      Noct = comnd(4,icomnd)
      Iptbs = Ioct + 3*Noct
      Nptbs = comnd(5,icomnd)
      Iph = Iptbs + 7*Nptbs
      Nph = comnd(6,icomnd)
!
!     MOVE RDMAP DATA
!
      k = 0
      IF ( Nrdm/=0 ) THEN
         DO j = 1 , Nrdm
            DO i = 1 , 18
               k = k + 1
               Idat(k) = rdmap(i,j)
            ENDDO
         ENDDO
      ENDIF
!
!     MOVE XTRA DATA
!
      IF ( Nxtra/=0 ) THEN
         DO i = 1 , Nxtra
            k = k + 1
            Idat(k) = xtra(i)
         ENDDO
      ENDIF
!
!     MOVE OCT DATA
!
      IF ( Noct/=0 ) THEN
         DO j = 1 , Noct
            DO i = 1 , 3
               k = k + 1
               Idat(k) = oct(i,j)
            ENDDO
         ENDDO
      ENDIF
!
!     MOVE PTBS DATA
!
      IF ( Nptbs/=0 ) THEN
         DO j = 1 , Nptbs
            DO i = 1 , 7
               k = k + 1
               Idat(k) = ptbs(i,j)
            ENDDO
         ENDDO
      ENDIF
   ENDIF
!
   RETURN
!
99999 RETURN
END SUBROUTINE ascm09
