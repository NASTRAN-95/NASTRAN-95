!*==ascm08.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ascm08(Name,Iphase,Isol,Nogo)
   IMPLICIT NONE
   USE C_ASDBD
   USE C_PHAS28
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Name
   INTEGER :: Iphase
   INTEGER :: Isol
   INTEGER :: Nogo
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(6,1) , SAVE :: comnd
   INTEGER :: i , icomnd , j , k , l
   INTEGER , DIMENSION(21) , SAVE :: isave
   INTEGER , DIMENSION(3,23) :: oct
   INTEGER , DIMENSION(3,18) , SAVE :: oct1
   INTEGER , DIMENSION(3,5) , SAVE :: oct2
   INTEGER , DIMENSION(7,25) :: ptbs
   INTEGER , DIMENSION(7,18) , SAVE :: ptbs1
   INTEGER , DIMENSION(7,7) , SAVE :: ptbs2
   INTEGER , DIMENSION(18,55) :: rdmap
   INTEGER , DIMENSION(18,9) , SAVE :: rdmap1 , rdmap2 , rdmap3 , rdmap4 , rdmap5 , rdmap6
   INTEGER , DIMENSION(18,1) , SAVE :: rdmap7
   REAL , SAVE :: slash
   INTEGER , DIMENSION(2) , SAVE :: subnam
   EXTERNAL khrfn1 , mesage
!
! End of declarations rewritten by SPAG
!
!
!     SOLVE COMMAND DMAP DATA FOR DYNAMIC ANALYSIS
!
   !>>>>EQUIVALENCE (rdmap1(1,1),rdmap(1,1)) , (oct1(1,1),oct(1,1)) , (rdmap2(1,1),rdmap(1,10)) , (oct2(1,1),oct(1,19)) ,                &
!>>>>    & (rdmap3(1,1),rdmap(1,19)) , (ptbs1(1,1),ptbs(1,1)) , (rdmap4(1,1),rdmap(1,28)) , (ptbs2(1,1),ptbs(1,19)) ,                    &
!>>>>    & (rdmap5(1,1),rdmap(1,37)) , (rdmap6(1,1),rdmap(1,46)) , (rdmap7(1,1),rdmap(1,55))
   DATA comnd/4HSOLV , 55 , 0 , 23 , 25 , 14/
   DATA slash/1H//
   DATA isave/4 , 11 , 3 , 13 , 10 , 1 , 13 , 14 , 3 , 13 , 16 , 2 , 54 , 8 , 2 , 54 , 9 , 2 , 54 , 10 , 2/
   DATA rdmap1/4HALTE , 4HR    , 4H  (G , 4HP1)  , 4H$    , 13*4H     , 4HPARA , 4HM    , 4H  // , 4H*NOP , 4H*/AL , 4HWAYS ,       &
      & 4H=-1  , 4H$    , 4H     , 4H     , 8*4H     , 4HSGEN , 4H     , 4H  CA , 4HSECC , 4H,GEO , 4HM3,G , 4HEOM4 , 4H,DYN ,      &
       &4HAMIC , 4HS/CA , 4HSESS , 4H,CAS , 4HEI,G , 4HPL,E , 4HQEXI , 4HN,GP , 4HDT,  , 4H     , 4H     , 4H     , 4H  BG ,        &
      & 4HPDT, , 4HSIL, , 4HGE3S , 4H,GE4 , 4HS,DY , 4HNS/S , 4H,N,D , 4HRY!* , 4HNAME , 4HSOLS , 4H*/S, , 4HN,LU , 4HSET/ ,        &
      & 4H     , 4H     , 4H     , 4H     , 4H  S, , 4HN,NO , 4HGPDT , 4H $   , 12*4H     , 4HPURG , 4HE    , 4H  CS , 4HTM $ ,     &
       &14*4H     , 4HEQUI , 4HV    , 4H  GE , 4H3S,G , 4HEOM3 , 4H/ALW , 4HAYS/ , 4HGE4S , 4H,GEO , 4HM4/A , 4HLWAY , 4HS/CA ,     &
       &4HSEI, , 4HCASE , 4HCC/A , 4HLWAY , 4HS/   , 4H     , 4H     , 4H     , 4H  DY , 4HNS,D , 4HYNAM , 4HICS/ , 4HALWA ,        &
      & 4HYS $ , 4H     , 4H     , 8*4H     , 4HCOND , 4H     , 4H  LB , 4HSTP, , 4HDRY  , 4H$    , 12*4H    /
   DATA rdmap2/4HALTE , 4HR    , 4H  (P , 4HLOT) , 4H $   , 13*4H     , 4HALTE , 4HR    , 4H  (C , 4HOND) , 4H $   , 13*4H     ,    &
       &4HALTE , 4HR    , 4H  (G , 4HPWG) , 4H $   , 13*4H     , 4HSOFI , 4H     , 4H  /K , 4HNOS, , 4HMNOS , 4H,BNO , 4HS,K4 ,     &
       &4HNOS, , 4H/DRY , 4H!*NA , 4HMESO , 4HLS*/ , 4H*KMT , 4HX*!* , 4HMMTX , 4H*!*B , 4HMTX* , 4H/    , 4H     , 4H     ,        &
      & 4H  *K , 4H4MX* , 4H $   , 13*4H     , 4HEQUI , 4HV    , 4H  KN , 4HOS,K , 4HGG/N , 4HOKGG , 4HX $  , 4H     , 4H     ,     &
       &4H     , 8*4H     , 4HCOND , 4H     , 4H  LB , 4H2K,N , 4HOKGG , 4HX $  , 12*4H     , 4HADD  , 4H     , 4H  KG , 4HGX,K ,   &
       &4HNOS/ , 4HKGG/ , 4H(1.0 , 4H,0.0 , 4H)/(1 , 4H.0,0 , 4H.0)  , 4H$    , 6*4H     , 4HLABE , 4HL    , 4H  LB , 4H2K $ ,      &
       &14*4H    /
   DATA rdmap3/4HEQUI , 4HV    , 4H  MN , 4HOS,M , 4HGG/N , 4HOMGG , 4H $   , 4H     , 4H     , 4H     , 8*4H     , 4HCOND ,        &
      & 4H     , 4H  LB , 4H2M,N , 4HOMGG , 4H $   , 12*4H     , 4HADD  , 4H     , 4H  MG , 4HG,MN , 4HOS/M , 4HGGX/ , 4H(1.0 ,     &
       &4H,0.0 , 4H)/(1 , 4H.0,0 , 4H.0)  , 4H$    , 6*4H     , 4HEQUI , 4HV    , 4H  MG , 4HGX,M , 4HGG/A , 4HLWAY , 4HS $  ,      &
       &4H     , 4H     , 4H     , 8*4H     , 4HLABE , 4HL    , 4H  LB , 4H2M $ , 14*4H     , 4HEQUI , 4HV    , 4H  BN , 4HOS,B ,   &
       &4HGG/N , 4HOBGG , 4H $   , 4H     , 4H     , 4H     , 8*4H     , 4HCOND , 4H     , 4H  LB , 4H2B,N , 4HOBGG , 4H $   ,      &
       &12*4H     , 4HADD  , 4H     , 4H  BG , 4HG,BN , 4HOS/B , 4HGGX/ , 4H(1.0 , 4H,0.0 , 4H)/(1 , 4H.0,0 , 4H.0)  , 4H$    ,     &
       &6*4H     , 4HEQUI , 4HV    , 4H  BG , 4HGX,B , 4HGG/A , 4HLWAY , 4HS $  , 4H     , 4H     , 4H     , 8*4H    /
   DATA rdmap4/4HLABE , 4HL    , 4H  LB , 4H2B $ , 14*4H     , 4HEQUI , 4HV    , 4H  K4 , 4HNOS, , 4HK4GG , 4H/NOK , 4H4GG  ,       &
      & 4H$    , 4H     , 4H     , 8*4H     , 4HCOND , 4H     , 4H  LB , 4H2K4, , 4HNOK4 , 4HGG $ , 12*4H     , 4HADD  , 4H     ,   &
       &4H  K4 , 4HGG,K , 4H4NOS , 4H/K4G , 4HGX/  , 4H(1.0 , 4H,0.0 , 4H)/(1 , 4H.0,0 , 4H.0)  , 4H$    , 5*4H     , 4HEQUI ,      &
       &4HV    , 4H  K4 , 4HGGX, , 4HK4GG , 4H/ALW , 4HAYS  , 4H$    , 4H     , 4H     , 8*4H     , 4HLABE , 4HL    , 4H  LB ,      &
       &4H2K4  , 4H$    , 13*4H     , 4HLABE , 4HL    , 4H  LB , 4HSTP  , 4H$    , 13*4H     , 4HCHKP , 4HNT   , 4H  MG , 4HG,BG ,  &
       &4HG,K4 , 4HGG $ , 12*4H     , 4HALTE , 4HR    , 4H  (P , 4HARAM , 4H) $  , 13*4H    /
   DATA rdmap5/4HPARA , 4HM    , 4H  // , 4H*AND , 4H*/MD , 4HEMA/ , 4HNOUE , 4H/NOM , 4H2PP  , 4H$    , 8*4H     , 4HPARA ,        &
      & 4HM    , 4H  // , 4H*ADD , 4H*/KD , 4HEK2/ , 4H1/0  , 4H$    , 4H     , 4H     , 8*4H     , 4HPARA , 4HM    , 4H  // ,      &
       &4H*ADD , 4H*/NO , 4HMGG/ , 4H1/0  , 4H$    , 4H     , 4H     , 8*4H     , 4HPARA , 4HM    , 4H  // , 4H*ADD , 4H*/NO ,      &
       &4HBGG/ , 4H1/0  , 4H$    , 4H     , 4H     , 8*4H     , 4HPARA , 4HM    , 4H  // , 4H*ADD , 4H*/NO , 4HK4GG , 4H/1/0 ,      &
       &4H $   , 4H     , 4H     , 8*4H     , 4HALTE , 4HR    , 4H  (E , 4HQUIV , 4H) $  , 13*4H     , 4HEQUI , 4HV    , 4H  K2 ,   &
       &4HDD,K , 4HDD/K , 4HDEK2 , 4H $   , 4H     , 4H     , 4H     , 8*4H     , 4HEQUI , 4HV    , 4H  M2 , 4HDD,M , 4HDD/N ,      &
       &4HOMGG , 4H $   , 4H     , 4H     , 4H     , 8*4H     , 4HEQUI , 4HV    , 4H  B2 , 4HDD,B , 4HDD/N , 4HOBGG , 4H $   ,      &
       &4H     , 4H     , 4H     , 8*4H    /
   DATA rdmap6/4HALTE , 4HR    , 4H  (S , 4HDR2) , 4H $   , 13*4H     , 4HEQUI , 4HV    , 4H  UP , 4HVF,U , 4HPVC/ , 4HNOA  ,       &
      & 4H$    , 4H     , 4H     , 4H     , 8*4H     , 4HCOND , 4H     , 4H  LB , 4HL19, , 4HNOA  , 4H$    , 12*4H     , 4HSDR1 ,   &
       &4H     , 4H  US , 4HETD, , 4H,UDV , 4HF,,, , 4HGOD, , 4HGMD, , 4H,,,/ , 4HUPVC , 4H,,/1 , 4H/DYN , 4HAMIC , 4HS $  ,        &
       &4*4H     , 4HLABE , 4HL    , 4H  LB , 4HL19  , 4H$    , 13*4H     , 4HCHKP , 4HNT   , 4H  UP , 4HVC $ , 14*4H     , 4HEQUI ,&
       &4HV    , 4H  UP , 4HVC,U , 4HGV/N , 4HOUE  , 4H$    , 4H     , 4H     , 4H     , 8*4H     , 4HCOND , 4H     , 4H  LB ,      &
       &4HUE,N , 4HOUE  , 4H$    , 12*4H     , 4HUPAR , 4HTN   , 4H  US , 4HET,U , 4HPVC/ , 4HUGV, , 4HUEV, , 4H,!*P , 4H*!*G ,     &
       &4H*!*E , 4H* $  , 7*4H    /
   DATA rdmap7/4HLABE , 4HL    , 4H  LB , 4HUE $ , 14*4H    /
   DATA oct1/15 , 0 , 1 , 16 , 0 , 1 , 17 , 0 , 1 , 18 , 0 , 1 , 19 , 0 , 2 , 20 , 0 , 2 , 21 , 0 , 2 , 22 , 0 , 2 , 23 , 0 , 2 ,   &
      & 24 , 0 , 16 , 25 , 0 , 16 , 26 , 0 , 16 , 27 , 0 , 16 , 28 , 0 , 16 , 29 , 0 , 32 , 30 , 0 , 32 , 31 , 0 , 32 , 32 , 0 , 32/
   DATA oct2/33 , 0 , 32 , 38 , 0 , 1 , 39 , 0 , 2 , 40 , 0 , 16 , 41 , 0 , 32/
   DATA ptbs1/1 , 11 , 11 , 5 , 1 , 0 , 0 , 4 , 43 , 45 , 8 , 4HNAME , 0 , 0 , 9 , 13 , 13 , 3 , 4HSTEP , 0 , 0 , 10 , 11 , 11 , 6 ,&
      & 2 , 0 , 0 , 11 , 11 , 11 , 6 , 3 , 0 , 0 , 12 , 11 , 11 , 6 , 4 , 0 , 0 , 13 , 12 , 13 , 3 , 4HNANO , 1 , -1 , 13 , 17 ,    &
      & 18 , 3 , 4HNANO , 2 , -1 , 13 , 22 , 23 , 3 , 4HNANO , 16 , -1 , 13 , 27 , 29 , 3 , 4HNANO , 32 , -1 , 13 , 37 , 39 , 8 ,   &
       &4HNAME , 0 , 0 , 15 , 11 , 12 , 3 , 4HNANO , 0 , 0 , 17 , 16 , 17 , 3 , 4HNANO , 0 , 0 , 19 , 11 , 12 , 3 , 4HNANO , 0 , 0 ,&
      & 21 , 15 , 16 , 3 , 4HNANO , 0 , 0 , 24 , 11 , 12 , 3 , 4HNANO , 0 , 0 , 26 , 15 , 16 , 3 , 4HNANO , 0 , 0 , 29 , 11 , 13 ,  &
      & 3 , 4HNANO , 0 , 0/
   DATA ptbs2/31 , 16 , 18 , 3 , 4HNANO , 0 , 0 , 34 , 13 , 13 , 3 , 4HSTEP , 0 , 0 , 36 , 11 , 11 , 7 , 5 , 0 , 0 , 42 , 11 , 11 , &
      & 7 , 6 , 0 , 0 , 46 , 11 , 11 , 6 , 7 , 0 , 0 , 47 , 11 , 11 , 4 , 4HDVEC , 0 , 0 , 49 , 18 , 18 , 4 , 4HDVEC , 0 , 0/
   DATA subnam/4HASCM , 2H08/
!
!     RESTORE TO ORIGINAL DATA BY REPLACEING ! BY / IN RDMAP ARRAY
!     (SEE ASCM01 FOR EXPLANATION))
!
   DO l = 1 , 21 , 3
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
      RETURN
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
!
!     MOVE PHASE 2 DATA
!
      IF ( Iphase==2 .AND. Nph/=0 ) THEN
         DO i = 1 , Nph
            k = k + 1
            Idat(k) = Ipas28(i)
         ENDDO
      ENDIF
   ENDIF
!
   RETURN
!
END SUBROUTINE ascm08
