!*==ascm06.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ascm06(Name,Iphase,Isol,Nogo)
   USE c_asdbd
   IMPLICIT NONE
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
   INTEGER , DIMENSION(6,2) , SAVE :: comnd
   INTEGER :: i , icomnd , j , k
   INTEGER , DIMENSION(3,1) :: oct
   INTEGER , DIMENSION(3,1) , SAVE :: oct1
   INTEGER , DIMENSION(7,31) :: ptbs
   INTEGER , DIMENSION(7,18) , SAVE :: ptbs1
   INTEGER , DIMENSION(7,13) , SAVE :: ptbs2
   INTEGER , DIMENSION(18,17) :: rdmap
   INTEGER , DIMENSION(18,9) , SAVE :: rdmap1
   INTEGER , DIMENSION(18,8) , SAVE :: rdmap2
   REAL , SAVE :: slash
   INTEGER , DIMENSION(2) , SAVE :: subnam
   INTEGER , DIMENSION(15) , SAVE :: xtra
   EXTERNAL khrfn1 , mesage
!
! End of declarations rewritten by SPAG
!
!
!     RECOVER, MRECOVER COMMAND DMAP DATA
!
   !>>>>EQUIVALENCE (rdmap1(1,1),rdmap(1,1)) , (ptbs1(1,1),ptbs(1,1)) , (rdmap2(1,1),rdmap(1,10)) , (ptbs2(1,1),ptbs(1,19)) ,            &
!>>>>    & (oct1(1,1),oct(1,1))
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
      IF ( Name==comnd(1,i) ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
   ENDDO
!
!     INPUT ERROR
!
   CALL mesage(7,0,subnam)
   Nogo = 1
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
      Icomnd = I
      irdm = 1
      nrdm = Comnd(2,Icomnd)
      ixtra = irdm + 18*nrdm
      nxtra = Comnd(3,Icomnd)
      ioct = ixtra + nxtra
      noct = Comnd(4,Icomnd)
      iptbs = ioct + 3*noct
      nptbs = Comnd(5,Icomnd)
      iph = iptbs + 7*nptbs
      nph = Comnd(6,Icomnd)
!
!     MOVE RDMAP DATA
!
      K = 0
      IF ( nrdm/=0 ) THEN
         DO J = 1 , nrdm
            DO I = 1 , 18
               K = K + 1
               idat(K) = Rdmap(I,J)
            ENDDO
         ENDDO
      ENDIF
!
!     MOVE XTRA DATA
!
      IF ( nxtra/=0 ) THEN
         DO I = 1 , nxtra
            K = K + 1
            idat(K) = Xtra(I)
         ENDDO
      ENDIF
!
!     MOVE OCT DATA
!
      IF ( noct/=0 ) THEN
         DO J = 1 , noct
            DO I = 1 , 3
               K = K + 1
               idat(K) = Oct(I,J)
            ENDDO
         ENDDO
      ENDIF
!
!     MOVE PTBS DATA
!
      IF ( nptbs/=0 ) THEN
         DO J = 1 , nptbs
            DO I = 1 , 7
               K = K + 1
               idat(K) = Ptbs(I,J)
            ENDDO
         ENDDO
      ENDIF
!
   END SUBROUTINE spag_block_1
!
END SUBROUTINE ascm06
