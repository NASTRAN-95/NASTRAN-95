
SUBROUTINE ascm07(Name,Iphase,Isol,Nogo)
   IMPLICIT NONE
   INTEGER Idat(605) , Ioct , Ipas37(6) , Iph , Iptbs , Irdm , Ixtra , Noct , Nph , Nptbs , Nrdm , Nxtra
   COMMON /asdbd / Irdm , Nrdm , Ixtra , Nxtra , Ioct , Noct , Iptbs , Nptbs , Iph , Nph , Idat
   COMMON /phas37/ Ipas37
   INTEGER Iphase , Isol , Name , Nogo
   INTEGER comnd(6,1) , i , icomnd , j , k , oct(3,13) , oct1(3,13) , ptbs(7,26) , ptbs1(7,18) , ptbs2(7,8) , rdmap(18,21) ,        &
         & rdmap1(18,9) , rdmap2(18,9) , rdmap3(18,3) , subnam(2)
   INTEGER khrfn1
   REAL slash
!
!     BRECOVER COMMAND DMAP DATA
!
   !>>>>EQUIVALENCE (rdmap1(1,1),rdmap(1,1)) , (oct1(1,1),oct(1,1)) , (rdmap2(1,1),rdmap(1,10)) , (ptbs1(1,1),ptbs(1,1)) ,               &
!>>>>    & (rdmap3(1,1),rdmap(1,19)) , (ptbs2(1,1),ptbs(1,19))
   DATA comnd/4HBREC , 21 , 0 , 13 , 26 , 6/
   DATA slash/1H//
   DATA rdmap1/4HALTE , 4HR    , 4H  (S , 4HOLVE , 4H) $  , 13*4H     , 4HPARA , 4HM    , 4H  // , 4H*NOP , 4H*/AL , 4HWAYS ,       &
      & 4H=-1  , 4H$    , 4H     , 4H     , 8*4H     , 4HSSG1 , 4H     , 4H  SL , 4HT,BG , 4HPDT, , 4HCSTM , 4H,SIL , 4H,EST ,      &
       &4H,MPT , 4H,GPT , 4HT,ED , 4HT,MG , 4HG,CA , 4HSECC , 4H,DIT , 4H,/PG , 4H,,,, , 4H/    , 4H     , 4H     , 4H  LU ,        &
      & 4HSET/ , 4HNSKI , 4HP $  , 12*4H     , 4HSSG2 , 4H     , 4H  US , 4HET,G , 4HM,YS , 4H,KFS , 4H,GO, , 4H,PG/ , 4HQR,P ,     &
       &4HO,PS , 4H,PL  , 4H$    , 6*4H     , 4HRCOV , 4HR3   , 4H  ,P , 4HG,PS , 4H,PO, , 4HYS/U , 4HAS , , 4HQAS, , 4HPGS, ,      &
       &4HPSS, , 4HPOS, , 4HYSS, , 4HLAMA , 4H/SOL , 4HN!*N , 4HAME  , 4H   * , 4H/    , 4H     , 4H     , 4H  NO , 4HUE $ ,        &
       &14*4H     , 4HEQUI , 4HV    , 4H  PG , 4HS,PG , 4H/ALW , 4HAYS  , 4H$    , 4H     , 4H     , 4H     , 8*4H     , 4HEQUI ,   &
       &4HV    , 4H  PS , 4HS,PS , 4H/ALW , 4HAYS  , 4H$    , 4H     , 4H     , 4H     , 8*4H    /
   DATA rdmap2/4HEQUI , 4HV    , 4H  PO , 4HS,PO , 4H/ALW , 4HAYS  , 4H$    , 4H     , 4H     , 4H     , 8*4H     , 4HEQUI ,        &
      & 4HV    , 4H  YS , 4HS,YS , 4H/ALW , 4HAYS  , 4H$    , 4H     , 4H     , 4H     , 8*4H     , 4HCOND , 4H     , 4H  LB ,      &
       &4HSSTP , 4H,OMI , 4HT $  , 12*4H     , 4HFBS  , 4H     , 4H  LO , 4HO,,P , 4HOS/U , 4HOOV/ , 4H1/1/ , 4HPREC , 4H/0 $ ,     &
       &4H     , 8*4H     , 4HLABE , 4HL    , 4H  LB , 4HSSTP , 4H $   , 13*4H     , 4HOFP  , 4H     , 4H  LA , 4HMA,, , 4H,,,/ ,   &
       &4H/CAR , 4HDNO  , 4H$    , 4H     , 4H     , 8*4H     , 4HALTE , 4HR    , 4H  (S , 4HDR1) , 4H $   , 13*4H     , 4HUMER ,   &
       &4HGE   , 4H  US , 4HET,Q , 4HAS,/ , 4HQGS/ , 4H*G*/ , 4H*A*/ , 4H*O*  , 4H$    , 8*4H     , 4HADD  , 4H     , 4H  QG ,      &
       &4H ,QG , 4HS/QG , 4HT/   , 4H(1.0 , 4H,0.0 , 4H)/(1 , 4H.0,0 , 4H.0)  , 4H$    , 6*4H    /
   DATA rdmap3/4HEQUI , 4HV    , 4H  QG , 4HT,QG , 4H /AL , 4HWAYS , 4H $   , 4H     , 4H     , 4H     , 8*4H     , 4HEQUI ,        &
      & 4HV    , 4H  CA , 4HSECC , 4H,CAS , 4HEXX/ , 4HALWA , 4HYS $ , 4H     , 4H     , 8*4H     , 4HALTE , 4HR    , 4H  (R ,      &
       &4HEPT) , 4H $   , 13*4H    /
   DATA oct1/3 , 983040 , 12 , 4 , 983040 , 12 , 5 , 524288 , 12 , 8 , 1835008 , 12 , 9 , 1835008 , 12 , 10 , 1835008 , 12 , 11 ,   &
      & 1835008 , 12 , 12 , 1835008 , 12 , 13 , 1835008 , 12 , 14 , 1835008 , 12 , 15 , 1769472 , 0 , 20 , 458752 , 0 , 21 ,        &
      & 458752 , 0/
   DATA ptbs1/1 , 11 , 11 , 0 , 1 , 0 , 0 , 5 , 1 , 1 , 0 , 4HNAME , 0 , 0 , 5 , 19 , 21 , 0 , 4HNAME , 1048576 , 0 , 5 , 33 , 35 , &
      & 0 , 4HNAME , 0 , 0 , 5 , 36 , 38 , 0 , 4HNAME , 0 , 0 , 5 , 42 , 44 , 0 , 4HNAME , 0 , 0 , 6 , 12 , 14 , 0 , 4HNAME ,       &
      & 524300 , 0 , 6 , 15 , 17 , 0 , 4HNAME , 524300 , 0 , 6 , 18 , 20 , 0 , 4HNAME , 1572876 , 0 , 6 , 21 , 23 , 0 , 4HNAME ,    &
      & 1572876 , 0 , 6 , 24 , 24 , 4 , 4HUAPH , 0 , 0 , 6 , 33 , 33 , 3 , 4HPGVC , 0 , 0 , 6 , 37 , 37 , 3 , 4HPSVC , 0 , 0 , 6 ,  &
      & 41 , 44 , 0 , 4HNAME , 1572876 , 0 , 6 , 45 , 48 , 0 , 4HNAME , 1572876 , 0 , 6 , 49 , 49 , 4 , 4HDYNT , 196608 , 0 , 6 ,   &
      & 54 , 54 , 4 , 4HSOL  , 0 , 0 , 6 , 60 , 60 , 8 , 4HNAME , 0 , 0/
   DATA ptbs2/7 , 11 , 15 , 0 , 4HNAME , 458752 , 0 , 12 , 14 , 14 , 3 , 4HSTEP , 0 , 0 , 13 , 29 , 29 , 4 , 4HPREC , 0 , 0 , 14 ,  &
      & 14 , 14 , 3 , 4HSTEP , 0 , 0 , 16 , 11 , 11 , 0 , 2 , 0 , 0 , 18 , 11 , 11 , 3 , 4HQVEC , 0 , 0 , 19 , 15 , 15 , 3 ,        &
      & 4HQVEC , 0 , 0 , 21 , 11 , 11 , 0 , 3 , 0 , 0/
   DATA subnam/4HASCM , 2H07/
!
!     RESTORE TO ORIGINAL DATA BY REPLACEING ! BY / IN RDMAP ARRAY
!     (SEE ASCM01 FOR EXPLANATION))
!
   rdmap(15,6) = khrfn1(rdmap(15,6),2,slash,1)
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
!     MOVE PHASE 3 DATA
!
      IF ( Iphase==3 .AND. Nph/=0 ) THEN
         DO i = 1 , Nph
            k = k + 1
            Idat(k) = Ipas37(i)
         ENDDO
      ENDIF
   ENDIF
!
   RETURN
!
99999 RETURN
END SUBROUTINE ascm07