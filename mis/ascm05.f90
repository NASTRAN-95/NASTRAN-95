
SUBROUTINE ascm05(Name,Iphase,Isol,Nogo)
   IMPLICIT NONE
   INTEGER Idat(673) , Ioct , Ipas25(14) , Iph , Iptbs , Irdm , Ixtra , Noct , Nph , Nptbs , Nrdm , Nxtra
   COMMON /asdbd / Irdm , Nrdm , Ixtra , Nxtra , Ioct , Noct , Iptbs , Nptbs , Iph , Nph , Idat
   COMMON /phas25/ Ipas25
   INTEGER Iphase , Isol , Name , Nogo
   INTEGER comnd(6,1) , i , icomnd , j , k , oct(3,5) , oct1(3,5) , ptbs(7,20) , ptbs1(7,18) , ptbs2(7,2) , rdmap(18,28) ,          &
         & rdmap1(18,9) , rdmap2(18,9) , rdmap3(18,9) , rdmap4(18,1) , subnam(2)
   INTEGER khrfn1
   REAL slash
!
!     SOLVE COMMAND DMAP DATA
!
   EQUIVALENCE (rdmap1(1,1),rdmap(1,1)) , (oct1(1,1),oct(1,1)) , (rdmap2(1,1),rdmap(1,10)) , (ptbs1(1,1),ptbs(1,1)) ,               &
    & (rdmap3(1,1),rdmap(1,19)) , (ptbs2(1,1),ptbs(1,19)) , (rdmap4(1,1),rdmap(1,28))
   DATA comnd/4HSOLV , 28 , 0 , 5 , 20 , 14/
   DATA slash/1H//
   DATA rdmap1/4HALTE , 4HR    , 4H  (G , 4HP1)  , 4H$    , 13*4H     , 4HPARA , 4HM    , 4H  // , 4H*NOP , 4H*/AL , 4HWAYS ,       &
      & 4H=-1  , 4H$    , 4H     , 4H     , 8*4H     , 4HSGEN , 4H     , 4H  CA , 4HSECC , 4H,GEO , 4HM3,G , 4HEOM4 , 4H,DYN ,      &
       &4HAMIC , 4HS/CA , 4HSESS , 4H,CAS , 4HEI,G , 4HPL,E , 4HQEXI , 4HN,GP , 4HDT,  , 4H     , 4H     , 4H     , 4H  BG ,        &
      & 4HPDT, , 4HSIL, , 4HGE3S , 4H,GE4 , 4HS,DY , 4HNS/S , 4H,N,D , 4HRY!* , 4HNAME , 4HSOLS , 4H*/S, , 4HN,LU , 4HSET/ ,        &
      & 4H     , 4H     , 4H     , 4H     , 4H  S, , 4HN,NO , 4HGPDT , 4H $   , 12*4H     , 4HPURG , 4HE    , 4H  CS , 4HTM $ ,     &
       &14*4H     , 4HEQUI , 4HV    , 4H  GE , 4H3S,G , 4HEOM3 , 4H/ALW , 4HAYS/ , 4HGE4S , 4H,GEO , 4HM4/A , 4HLWAY , 4HS/CA ,     &
       &4HSEI, , 4HCASE , 4HCC/A , 4HLWAY , 4HS/   , 4H     , 4H     , 4H     , 4H  DY , 4HNS,D , 4HYNAM , 4HICS/ , 4HALWA ,        &
      & 4HYS $ , 4H     , 4H     , 8*4H     , 4HCOND , 4H     , 4H  LB , 4HSTP, , 4HDRY  , 4H$    , 12*4H    /
   DATA rdmap2/4HALTE , 4HR    , 4H  (P , 4HLOT) , 4H $   , 13*4H     , 4HALTE , 4HR    , 4H  (C , 4HOND) , 4H $   , 13*4H     ,    &
       &4HCOND , 4H     , 4H  LB , 4HSOL, , 4HNOSI , 4HMP $ , 12*4H     , 4HALTE , 4HR    , 4H  (O , 4HPTP) , 4H $   , 13*4H     ,  &
       &4HCOND , 4H     , 4H  LB , 4HSOL, , 4HNOMG , 4HG $  , 12*4H     , 4HALTE , 4HR    , 4H  (S , 4HMA3) , 4H $   , 13*4H     ,  &
       &4HLABE , 4HL    , 4H  LB , 4HSOL  , 4H$    , 13*4H     , 4HSOFI , 4H     , 4H  /K , 4HNOS, , 4HMNOS , 4H,,,/ , 4HDRY/ ,     &
       &4H*NAM , 4HESOL , 4HS*!* , 4HKMTX , 4H*!*M , 4HMTX* , 4H $   , 4*4H     , 4HEQUI , 4HV    , 4H  KN , 4HOS,K , 4HGG/N ,      &
       &4HOSIM , 4HP $  , 4H     , 4H     , 4H     , 8*4H    /
   DATA rdmap3/4HEQUI , 4HV    , 4H  MN , 4HOS,M , 4HGG/N , 4HOSIM , 4HP $  , 4H     , 4H     , 4H     , 8*4H     , 4HCOND ,        &
      & 4H     , 4H  LB , 4HSTP, , 4HNOSI , 4HMP $ , 12*4H     , 4HADD  , 4H     , 4H  KG , 4HGX,K , 4HNOS/ , 4HKGG/ , 4H(1.0 ,     &
       &4H,0.0 , 4H)/(1 , 4H.0,0 , 4H.0)  , 4H$    , 6*4H     , 4HADD  , 4H     , 4H  MG , 4HG,MN , 4HOS/M , 4HGGX/ , 4H(1.0 ,      &
       &4H,0.0 , 4H)/(1 , 4H.0,0 , 4H.0)  , 4H$    , 6*4H     , 4HEQUI , 4HV    , 4H  MG , 4HGX,M , 4HGG/A , 4HLWAY , 4HS $  ,      &
       &4H     , 4H     , 4H     , 8*4H     , 4HLABE , 4HL    , 4H  LB , 4HSTP  , 4H$    , 13*4H     , 4HCHKP , 4HNT   , 4H  MG ,   &
       &4HG $  , 14*4H     , 4HALTE , 4HR    , 4H  (G , 4HP4)  , 4H$    , 13*4H     , 4HCOND , 4H     , 4H  LB , 4HSEND , 4H,DRY ,  &
       &4H $   , 12*4H    /
   DATA rdmap4/4HALTE , 4HR    , 4H  (S , 4HDR2) , 4H $   , 13*4H    /
   DATA oct1/18 , 0 , 1 , 19 , 0 , 2 , 21 , 0 , 1 , 22 , 0 , 2 , 23 , 0 , 2/
   DATA ptbs1/1 , 11 , 11 , 5 , 1 , 0 , 0 , 4 , 43 , 45 , 8 , 4HNAME , 0 , 0 , 9 , 13 , 13 , 3 , 4HSTEP , 0 , 0 , 10 , 11 , 11 , 6 ,&
      & 2 , 0 , 0 , 11 , 11 , 11 , 6 , 6 , 0 , 0 , 12 , 50 , 50 , 0 , 4HSOL  , 0 , 0 , 13 , 11 , 11 , 6 , 7 , 0 , 0 , 14 , 50 , 50 ,&
      & 0 , 4HMSKP , 0 , 0 , 15 , 11 , 11 , 6 , 3 , 0 , 0 , 17 , 12 , 13 , 3 , 4HNANO , 1 , -1 , 17 , 17 , 18 , 3 , 4HNANO , 2 ,    &
      & -1 , 17 , 28 , 30 , 8 , 4HNAME , 0 , 0 , 18 , 11 , 12 , 3 , 4HNANO , 0 , 0 , 19 , 11 , 12 , 3 , 4HNANO , 0 , 0 , 20 , 13 ,  &
      & 13 , 3 , 4HSTEP , 0 , 0 , 21 , 16 , 17 , 3 , 4HNANO , 0 , 0 , 22 , 15 , 16 , 3 , 4HNANO , 0 , 0 , 24 , 13 , 13 , 3 ,        &
      & 4HSTEP , 0 , 0/
   DATA ptbs2/26 , 11 , 11 , 5 , 4 , 0 , 0 , 28 , 11 , 11 , 6 , 5 , 0 , 0/
   DATA subnam/4HASCM , 2H05/
!
!     RESTORE TO ORIGINAL DATA BY REPLACEING ! BY / IN RDMAP ARRAY
!     (SEE ASCM01 FOR EXPLANATION))
!
   rdmap(11,4) = khrfn1(rdmap(11,4),3,slash,1)
   rdmap(10,17) = khrfn1(rdmap(10,17),3,slash,1)
   rdmap(12,17) = khrfn1(rdmap(12,17),2,slash,1)
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
         DO i = 1 , 4
            k = k + 1
            Idat(k) = Ipas25(i)
         ENDDO
         DO i = 9 , 14
            k = k + 1
            Idat(k) = Ipas25(i)
         ENDDO
         DO i = 5 , 8
            k = k + 1
            Idat(k) = Ipas25(i)
         ENDDO
      ENDIF
   ENDIF
!
   RETURN
!
99999 RETURN
END SUBROUTINE ascm05
