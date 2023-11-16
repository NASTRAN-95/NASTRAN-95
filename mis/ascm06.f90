
SUBROUTINE ascm06(Name,Iphase,Isol,Nogo)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Idat(543) , Ioct , Iph , Iptbs , Irdm , Ixtra , Noct , Nph , Nptbs , Nrdm , Nxtra
   COMMON /asdbd / Irdm , Nrdm , Ixtra , Nxtra , Ioct , Noct , Iptbs , Nptbs , Iph , Nph , Idat
!
! Dummy argument declarations
!
   INTEGER Iphase , Isol , Name , Nogo
!
! Local variable declarations
!
   INTEGER comnd(6,2) , i , icomnd , j , k , oct(3,1) , oct1(3,1) , ptbs(7,31) , ptbs1(7,18) , ptbs2(7,13) , rdmap(18,17) ,         &
         & rdmap1(18,9) , rdmap2(18,8) , subnam(2) , xtra(15)
   INTEGER khrfn1
   REAL slash
!
! End of declarations
!
!
!     RECOVER, MRECOVER COMMAND DMAP DATA
!
   EQUIVALENCE (rdmap1(1,1),rdmap(1,1)) , (ptbs1(1,1),ptbs(1,1)) , (rdmap2(1,1),rdmap(1,10)) , (ptbs2(1,1),ptbs(1,19)) ,            &
    & (oct1(1,1),oct(1,1))
   DATA comnd/4HRECO , 17 , 15 , 1 , 31 , 0 , 4HMREC , 17 , 15 , 1 , 31 , 0/
   DATA slash/1H//
   DATA rdmap1/4HFILE , 4H     , 4H  U1 , 4H=APP , 4HEND/ , 4HU2=A , 4HPPEN , 4HD/U3 , 4H=APP , 4HEND/ , 4HU4=A , 4HPPEN , 4HD/U5 , &
       &4H=APP , 4HEND  , 4H$    , 4H     , 4H     , 4HPARA , 4HM    , 4H  // , 4H*ADD , 4H*/IL , 4HOOP/ , 4H0/0  , 4H$    ,        &
      & 4H     , 4H     , 8*4H     , 4HLABE , 4HL    , 4H  LB , 4HSTP  , 4H$    , 13*4H     , 4HRCOV , 4HR    , 4H  CA , 4HSESS ,   &
       &4H,GEO , 4HM4,K , 4HGG,M , 4HGG,P , 4HGG,U , 4HGV , , 4HDIT, , 4HDLT, , 4HBGG, , 4HK4GG , 4H,PPF , 4H/OUG , 4HV1 , ,        &
      & 4H     , 4H     , 4H     , 4H  OP , 4HG1,O , 4HQG1, , 4HU1,U , 4H2,U3 , 4H,U4, , 4HU5/S , 4H,N,D , 4HRY/S , 4H,N,I ,        &
      & 4HLOOP , 4H/STP , 4H!*NA , 4HMEFS , 4HS */ , 4H     , 4H     , 4H     , 4H  NS , 4HOL/N , 4HEIGV , 4H/S,N , 4H,LUI ,        &
      & 4H/S,N , 4H,U1N , 4H/S,N , 4H,U2N , 4H/S,N , 4H,U3N , 4H/S,N , 4H,U4N , 4H/S,N , 4H,U5N , 4H/    , 4H     , 4H     ,        &
      & 4H  S, , 4HN,NO , 4HSORT , 4H2/V, , 4HY,UT , 4HHRES , 4HH/V, , 4HY,PT , 4HHRES , 4HH/V, , 4HY,QT , 4HHRES , 4HH $  ,        &
       &3*4H     , 4HEQUI , 4HV    , 4H  OU , 4HGV1  , 4H,OUG , 4HV /N , 4HOSOR , 4HT2/O , 4HQG1, , 4HOQG/ , 4HNOSO , 4HRT2  ,      &
       &4H$    , 5*4H     , 4HEQUI , 4HV    , 4H  OP , 4HG1,O , 4HPG/N , 4HOSOR , 4HT2 $ , 4H     , 4H     , 4H     , 8*4H    /
   DATA rdmap2/4HCOND , 4H     , 4H  NS , 4HT2ST , 4HP,NO , 4HSORT , 4H2 $  , 4H     , 4H     , 4H     , 8*4H     , 4HSDR3 ,        &
      & 4H     , 4H  OU , 4HGV1  , 4H,OPG , 4H1,OQ , 4HG1,, , 4H,/OU , 4HGV , , 4HOPG, , 4HOQG, , 4H,, $ , 6*4H     , 4HLABE ,      &
       &4HL    , 4H  NS , 4HT2ST , 4HP $  , 13*4H     , 4HOFP  , 4H     , 4H  OU , 4HGV , , 4HOPG, , 4HOQG, , 4H,,// , 4HS,N, ,     &
       &4HCARD , 4HNO $ , 8*4H     , 4HCOND , 4H     , 4H  LB , 4HBSTP , 4H,ILO , 4HOP $ , 12*4H     , 4HREPT , 4H     , 4H  LB ,   &
       &4HSTP, , 4H100  , 4H$    , 12*4H     , 4HLABE , 4HL    , 4H  LB , 4HBSTP , 4H $   , 13*4H     , 4HSOFO , 4H     , 4H  ,U ,  &
       &4H1,U2 , 4H,U3, , 4HU4,U , 4H5//- , 4H1!*X , 4HXXXX , 4HXXX* , 4H $   , 7*4H    /
   DATA xtra/4HPRIN , 4HSAVE , 4HDISP , 4HOLOA , 4HSPCF , 4HMODE , 4HRANG , 4HSUBC , 4HSORT , 4HBASI , 4HVELO , 4HACCE , 4HENER ,   &
       &4HUIMP , 4HSTEP/
   DATA oct1/9 , 262144 , 0/
   DATA ptbs1/3 , 13 , 13 , 3 , 4HSTEP , 0 , 0 , 4 , 11 , 15 , 2 , 4HCASE , 0 , 0 , 4 , 18 , 18 , 5 , 4HGORL , 0 , 0 , 4 , 24 , 27 ,&
      & 0 , 4HNAME , 1 , 0 , 4 , 28 , 31 , 0 , 4HNAME , 2 , 0 , 4 , 32 , 32 , 3 , 4HPVEC , 0 , 0 , 4 , 36 , 36 , 4 , 4HUVEC , 0 ,   &
      & 0 , 4 , 41 , 44 , 0 , 4HNAME , 458752 , 0 , 4 , 45 , 48 , 0 , 4HNAME , 458752 , 0 , 4 , 49 , 52 , 0 , 4HNAME , 458768 , 0 , &
      & 4 , 53 , 57 , 0 , 4HNAME , 458784 , 0 , 4 , 58 , 58 , 3 , 4HPFTL , 0 , 0 , 4 , 62 , 62 , 6 , 4HOVEC , 0 , 0 , 5 , 11 , 15 , &
      & 0 , 4HNAME , 262144 , 0 , 5 , 54 , 54 , 3 , 4HSTEP , 0 , 0 , 5 , 57 , 59 , 8 , 4HNAME , 0 , 0 , 6 , 11 , 11 , 4 , 4HSOL  ,  &
      & 0 , 0 , 6 , 16 , 21 , 0 , 4HNAME , 1769472 , 0/
   DATA ptbs2/8 , 11 , 11 , 6 , 4HOVEC , 0 , 0 , 8 , 18 , 18 , 5 , 4HOVC2 , 0 , 0 , 10 , 15 , 15 , 3 , 4HSTEP , 0 , 0 , 11 , 11 ,   &
      & 11 , 6 , 4HOVEC , 0 , 0 , 11 , 18 , 22 , 0 , 4HNAME , 262144 , 0 , 11 , 31 , 31 , 5 , 4HOVC2 , 0 , 0 , 11 , 37 , 40 , 0 ,   &
       &4HNAME , 262144 , 0 , 12 , 15 , 15 , 3 , 4HSTEP , 0 , 0 , 13 , 11 , 11 , 5 , 4HOVC2 , 0 , 0 , 13 , 17 , 20 , 0 , 4HNAME ,   &
      & 262144 , 0 , 14 , 14 , 14 , 3 , 4HSTEP , 0 , 0 , 15 , 13 , 13 , 3 , 4HSTEP , 0 , 0 , 16 , 14 , 14 , 3 , 4HSTEP , 0 , 0/
   DATA subnam/4HASCM , 2H06/
!
!     RESTORE TO ORIGINAL DATA BY REPLACEING ! BY / IN RDMAP ARRAY
!     (SEE ASCM01 FOR EXPLANATION))
!
   rdmap(15,5) = khrfn1(rdmap(15,5),1,slash,1)
   rdmap(8,17) = khrfn1(rdmap(8,17),2,slash,1)
!
!     VALIDATE COMMAND AND SET POINTERS
!
   DO i = 1 , 2
      IF ( Name==comnd(1,i) ) GOTO 100
   ENDDO
!
!     INPUT ERROR
!
   CALL mesage(7,0,subnam)
   Nogo = 1
   GOTO 99999
 100  icomnd = i
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
!
   RETURN
!
99999 RETURN
END SUBROUTINE ascm06
