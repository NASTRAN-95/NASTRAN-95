
SUBROUTINE ascm03(Name,Iphase,Isol,Nogo)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Idat(1156) , Ioct , Iph , Iptbs , Irdm , Ixtra , Noct , Nph , Nptbs , Nrdm , Nxtra
   COMMON /asdbd / Irdm , Nrdm , Ixtra , Nxtra , Ioct , Noct , Iptbs , Nptbs , Iph , Nph , Idat
!
! Dummy argument declarations
!
   INTEGER Iphase , Isol , Name , Nogo
!
! Local variable declarations
!
   INTEGER comnd(6,1) , i , icomnd , isave(111) , j , k , l , oct(3,21) , oct1(3,18) , oct2(3,3) , ptbs(7,93) , ptbs1(7,18) ,       &
         & ptbs2(7,18) , ptbs3(7,18) , ptbs4(7,18) , ptbs5(7,18) , ptbs6(7,3) , rdmap(18,24) , rdmap1(18,9) , rdmap2(18,9) ,        &
         & rdmap3(18,6) , subnam(2) , xtra(10)
   INTEGER khrfn1
   REAL slash
!
! End of declarations
!
!
!     COMBINE COMMAND DMAP DATA
!
   EQUIVALENCE (rdmap1(1,1),rdmap(1,1)) , (oct1(1,1),oct(1,1)) , (rdmap2(1,1),rdmap(1,10)) , (oct2(1,1),oct(1,19)) ,                &
    & (rdmap3(1,1),rdmap(1,19)) , (ptbs1(1,1),ptbs(1,1)) , (ptbs2(1,1),ptbs(1,19)) , (ptbs3(1,1),ptbs(1,37)) ,                      &
    & (ptbs4(1,1),ptbs(1,55)) , (ptbs5(1,1),ptbs(1,73)) , (ptbs6(1,1),ptbs(1,91))
   DATA comnd/4HCOMB , 24 , 10 , 21 , 93 , 0/
   DATA slash/1H//
   DATA isave/3 , 15 , 3 , 3 , 16 , 3 , 4 , 6 , 1 , 4 , 11 , 3 , 4 , 14 , 2 , 5 , 6 , 1 , 6 , 8 , 1 , 7 , 15 , 3 , 7 , 16 , 3 , 8 , &
      & 6 , 1 , 8 , 11 , 3 , 8 , 14 , 2 , 9 , 6 , 1 , 10 , 8 , 1 , 11 , 15 , 3 , 11 , 16 , 3 , 12 , 6 , 1 , 12 , 11 , 3 , 12 , 14 , &
      & 2 , 13 , 6 , 1 , 14 , 8 , 1 , 15 , 15 , 3 , 15 , 16 , 3 , 16 , 6 , 1 , 16 , 11 , 3 , 16 , 14 , 2 , 17 , 6 , 1 , 18 , 8 , 1 ,&
      & 19 , 17 , 3 , 20 , 5 , 1 , 20 , 10 , 3 , 20 , 13 , 2 , 20 , 16 , 1 , 21 , 6 , 1 , 22 , 8 , 2 , 22 , 11 , 1 , 24 , 5 , 1/
   DATA rdmap1/4HCOMB , 4H1    , 4H  CA , 4HSECC , 4H,GEO , 4HM4// , 4HSTP/ , 4HS,N, , 4HDRY/ , 4H*PVE , 4HC* $ , 7*4H     ,        &
      & 4HCOND , 4H     , 4H  LB , 4HSTP, , 4HDRY  , 4H$    , 12*4H     , 4HCOMB , 4H2    , 4H  ,K , 4HN01, , 4HKN02 , 4H,KN0 ,     &
       &4H3,KN , 4H04,K , 4HN05, , 4HKN06 , 4H,KN0 , 4H7/KN , 4HSC/S , 4H,N,D , 4HRY!* , 4HK*!* , 4H     , 4H*/   , 4H     ,        &
      & 4H     , 4H  *N , 4HAME0 , 4H001* , 4H!*NA , 4HME00 , 4H02*/ , 4H*NAM , 4HE000 , 4H3*!* , 4HNAME , 4H0004 , 4H*!*N ,        &
      & 4HAME0 , 4H005* , 4H/    , 4H     , 4H     , 4H     , 4H  *N , 4HAME0 , 4H006* , 4H!*NA , 4HME00 , 4H07*  , 4H$    ,        &
      & 4H     , 8*4H     , 4HSOFO , 4H     , 4H  ,K , 4HNSC, , 4H,,,/ , 4H/S,N , 4H,DRY , 4H!*NA , 4HMEC  , 4H  */ , 4H*KMT ,      &
       &4HX* $ , 6*4H     , 4HCOMB , 4H2    , 4H  ,M , 4HN01, , 4HMN02 , 4H,MN0 , 4H3,MN , 4H04,M , 4HN05, , 4HMN06 , 4H,MN0 ,      &
       &4H7/MN , 4HSC/S , 4H,N,D , 4HRY!* , 4HM*!* , 4H     , 4H*/   , 4H     , 4H     , 4H  *N , 4HAME0 , 4H001* , 4H!*NA ,        &
      & 4HME00 , 4H02*/ , 4H*NAM , 4HE000 , 4H3*!* , 4HNAME , 4H0004 , 4H*!*N , 4HAME0 , 4H005* , 4H/    , 4H     , 4H     ,        &
      & 4H     , 4H  *N , 4HAME0 , 4H006* , 4H!*NA , 4HME00 , 4H07*  , 4H$    , 4H     , 8*4H    /
   DATA rdmap2/4HSOFO , 4H     , 4H  ,M , 4HNSC, , 4H,,,/ , 4H/S,N , 4H,DRY , 4H!*NA , 4HMEC  , 4H  */ , 4H*MMT , 4HX* $ ,          &
      & 6*4H     , 4HCOMB , 4H2    , 4H  ,P , 4HN01, , 4HPN02 , 4H,PN0 , 4H3,PN , 4H04,P , 4HN05, , 4HPN06 , 4H,PN0 , 4H7/PN ,      &
       &4HSC/S , 4H,N,D , 4HRY!* , 4HP*!* , 4HPVEC , 4H*/   , 4H     , 4H     , 4H  *N , 4HAME0 , 4H001* , 4H!*NA , 4HME00 ,        &
      & 4H02*/ , 4H*NAM , 4HE000 , 4H3*!* , 4HNAME , 4H0004 , 4H*!*N , 4HAME0 , 4H005* , 4H/    , 4H     , 4H     , 4H     ,        &
      & 4H  *N , 4HAME0 , 4H006* , 4H!*NA , 4HME00 , 4H07*  , 4H$    , 4H     , 8*4H     , 4HSOFO , 4H     , 4H  ,P , 4HNSC, ,      &
       &4H,,,/ , 4H/S,N , 4H,DRY , 4H!*NA , 4HMEC  , 4H  */ , 4H*PVE , 4HC* $ , 6*4H     , 4HCOMB , 4H2    , 4H  ,B , 4HN01, ,      &
       &4HBN02 , 4H,BN0 , 4H3,BN , 4H04,B , 4HN05, , 4HBN06 , 4H,BN0 , 4H7/BN , 4HSC/S , 4H,N,D , 4HRY!* , 4HB*!* , 4H     ,        &
      & 4H*/   , 4H     , 4H     , 4H  *N , 4HAME0 , 4H001* , 4H!*NA , 4HME00 , 4H02*/ , 4H*NAM , 4HE000 , 4H3*!* , 4HNAME ,        &
      & 4H0004 , 4H*!*N , 4HAME0 , 4H005* , 4H/    , 4H     , 4H     , 4H     , 4H  *N , 4HAME0 , 4H006* , 4H!*NA , 4HME00 ,        &
      & 4H07*  , 4H$    , 4H     , 8*4H     , 4HSOFO , 4H     , 4H  ,B , 4HNSC, , 4H,,,/ , 4H/S,N , 4H,DRY , 4H!*NA , 4HMEC  ,      &
       &4H  */ , 4H*BMT , 4HX* $ , 6*4H    /
   DATA rdmap3/4HCOMB , 4H2    , 4H  ,K , 4H4N01 , 4H,K4N , 4H02,K , 4H4N03 , 4H,K4N , 4H04,K , 4H4N05 , 4H,K4N , 4H06,K , 4H4N07 , &
       &4H/K4N , 4HSC/S , 4H,N,D , 4HRY!* , 4HK4*/ , 4H     , 4H     , 4H  *  , 4H   * , 4H!*NA , 4HME00 , 4H01*/ , 4H*NAM ,        &
      & 4HE000 , 4H2*!* , 4HNAME , 4H0003 , 4H*!*N , 4HAME0 , 4H004* , 4H!*NA , 4HME00 , 4H05*/ , 4H     , 4H     , 4H  *N ,        &
      & 4HAME0 , 4H006* , 4H!*NA , 4HME00 , 4H07*  , 4H$    , 4H     , 8*4H     , 4HSOFO , 4H     , 4H  ,K , 4H4NSC , 4H,,,, ,      &
       &4H//S, , 4HN,DR , 4HY!*N , 4HAMEC , 4H   * , 4H!*K4 , 4HMX*  , 4H$    , 5*4H     , 4HLABE , 4HL    , 4H  LB , 4HSTP  ,      &
       &4H$    , 13*4H     , 4HLODA , 4HPP   , 4H  PN , 4HSC,/ , 4H!*NA , 4HMEC  , 4H  */ , 4HS,N, , 4HDRY  , 4H$    , 8*4H    /
   DATA xtra/4HSORT , 4HNAME , 4HNAMS , 4HTOLE , 4HCONN , 4HCOMP , 4HTRAN , 4HSYMT , 4HSEAR , 4HOUTP/
   DATA oct1/3 , 0 , 1 , 4 , 0 , 1 , 5 , 0 , 1 , 6 , 0 , 1 , 7 , 0 , 2 , 8 , 0 , 2 , 9 , 0 , 2 , 10 , 0 , 2 , 11 , 0 , 12 , 12 , 0 ,&
      & 12 , 13 , 0 , 12 , 14 , 0 , 12 , 15 , 0 , 16 , 16 , 0 , 16 , 17 , 0 , 16 , 18 , 0 , 16 , 19 , 0 , 32 , 20 , 0 , 32/
   DATA oct2/21 , 0 , 32 , 22 , 0 , 32 , 24 , 0 , 8/
   DATA ptbs1/1 , 24 , 25 , 3 , 4HNSTP , 0 , 0 , 1 , 36 , 38 , 4 , 4HPITM , 12 , 0 , 2 , 13 , 13 , 3 , 4HNSTP , 0 , 0 , 3 , 12 ,    &
      & 13 , 3 , 4HN1   , 0 , 1 , 3 , 17 , 18 , 3 , 4HN2   , 0 , 1 , 3 , 22 , 23 , 3 , 4HN3   , 0 , 1 , 3 , 27 , 28 , 3 , 4HN4   ,  &
      & 0 , 1 , 3 , 32 , 33 , 3 , 4HN5   , 0 , 1 , 3 , 37 , 38 , 3 , 4HN6   , 0 , 1 , 3 , 42 , 43 , 3 , 4HN7   , 0 , 1 , 3 , 47 ,   &
      & 48 , 3 , 4HNCNO , 1 , -1 , 4 , 11 , 12 , 8 , 4HNA1  , 0 , 0 , 4 , 21 , 23 , 8 , 4HNA2  , 0 , 0 , 4 , 32 , 34 , 8 , 4HNA3  , &
      & 0 , 0 , 4 , 43 , 45 , 8 , 4HNA4  , 0 , 0 , 4 , 54 , 56 , 8 , 4HNA5  , 0 , 0 , 5 , 11 , 12 , 8 , 4HNA6  , 0 , 0 , 5 , 21 ,   &
      & 23 , 8 , 4HNA7  , 0 , 0/
   DATA ptbs2/6 , 12 , 13 , 3 , 4HNCNO , 1 , 1 , 6 , 29 , 31 , 8 , 4HNAMC , 0 , 0 , 7 , 12 , 13 , 3 , 4HN1   , 0 , 1 , 7 , 17 , 18 ,&
      & 3 , 4HN2   , 0 , 1 , 7 , 22 , 23 , 3 , 4HN3   , 0 , 1 , 7 , 27 , 28 , 3 , 4HN4   , 0 , 1 , 7 , 32 , 33 , 3 , 4HN5   , 0 ,   &
      & 1 , 7 , 37 , 38 , 3 , 4HN6   , 0 , 1 , 7 , 42 , 43 , 3 , 4HN7   , 0 , 1 , 7 , 47 , 48 , 3 , 4HNCNO , 2 , -1 , 8 , 11 , 12 , &
      & 8 , 4HNA1  , 0 , 0 , 8 , 21 , 23 , 8 , 4HNA2  , 0 , 0 , 8 , 32 , 34 , 8 , 4HNA3  , 0 , 0 , 8 , 43 , 45 , 8 , 4HNA4  , 0 ,   &
      & 0 , 8 , 54 , 56 , 8 , 4HNA5  , 0 , 0 , 9 , 11 , 12 , 8 , 4HNA6  , 0 , 0 , 9 , 21 , 23 , 8 , 4HNA7  , 0 , 0 , 10 , 12 , 13 , &
      & 3 , 4HNCNO , 2 , 1/
   DATA ptbs3/10 , 29 , 31 , 8 , 4HNAMC , 0 , 0 , 11 , 12 , 13 , 3 , 4HN1   , 0 , 1 , 11 , 17 , 18 , 3 , 4HN2   , 0 , 1 , 11 , 22 , &
      & 23 , 3 , 4HN3   , 0 , 1 , 11 , 27 , 28 , 3 , 4HN4   , 0 , 1 , 11 , 32 , 33 , 3 , 4HN5   , 0 , 1 , 11 , 37 , 38 , 3 ,        &
      & 4HN6   , 0 , 1 , 11 , 42 , 43 , 3 , 4HN7   , 0 , 1 , 11 , 47 , 48 , 3 , 4HNCNO , 12 , -1 , 11 , 63 , 65 , 4 , 4HPITM , 0 ,  &
      & 0 , 12 , 11 , 12 , 8 , 4HNA1  , 0 , 0 , 12 , 21 , 23 , 8 , 4HNA2  , 0 , 0 , 12 , 32 , 34 , 8 , 4HNA3  , 0 , 0 , 12 , 43 ,   &
      & 45 , 8 , 4HNA4  , 0 , 0 , 12 , 54 , 56 , 8 , 4HNA5  , 0 , 0 , 13 , 11 , 12 , 8 , 4HNA6  , 0 , 0 , 13 , 21 , 23 , 8 ,        &
      & 4HNA7  , 0 , 0 , 14 , 12 , 13 , 3 , 4HNCNO , 12 , 1/
   DATA ptbs4/14 , 29 , 31 , 8 , 4HNAMC , 0 , 0 , 14 , 40 , 42 , 4 , 4HPITM , 0 , 0 , 15 , 12 , 13 , 3 , 4HN1   , 0 , 1 , 15 , 17 , &
      & 18 , 3 , 4HN2   , 0 , 1 , 15 , 22 , 23 , 3 , 4HN3   , 0 , 1 , 15 , 27 , 28 , 3 , 4HN4   , 0 , 1 , 15 , 32 , 33 , 3 ,        &
      & 4HN5   , 0 , 1 , 15 , 37 , 38 , 3 , 4HN6   , 0 , 1 , 15 , 42 , 43 , 3 , 4HN7   , 0 , 1 , 15 , 47 , 48 , 3 , 4HNCNO , 16 ,   &
      & -1 , 16 , 11 , 12 , 8 , 4HNA1  , 0 , 0 , 16 , 21 , 23 , 8 , 4HNA2  , 0 , 0 , 16 , 32 , 34 , 8 , 4HNA3  , 0 , 0 , 16 , 43 ,  &
      & 45 , 8 , 4HNA4  , 0 , 0 , 16 , 54 , 56 , 8 , 4HNA5  , 0 , 0 , 17 , 11 , 12 , 8 , 4HNA6  , 0 , 0 , 17 , 21 , 23 , 8 ,        &
      & 4HNA7  , 0 , 0 , 18 , 12 , 13 , 3 , 4HNCNO , 16 , 1/
   DATA ptbs5/18 , 29 , 31 , 8 , 4HNAMC , 0 , 0 , 19 , 12 , 14 , 3 , 4HN1   , 0 , 1 , 19 , 18 , 20 , 3 , 4HN2   , 0 , 1 , 19 , 24 , &
      & 26 , 3 , 4HN3   , 0 , 1 , 19 , 30 , 32 , 3 , 4HN4   , 0 , 1 , 19 , 36 , 38 , 3 , 4HN5   , 0 , 1 , 19 , 42 , 44 , 3 ,        &
      & 4HN6   , 0 , 1 , 19 , 48 , 50 , 3 , 4HN7   , 0 , 1 , 19 , 54 , 56 , 3 , 4HNCNO , 32 , -1 , 20 , 17 , 19 , 8 , 4HNA1  , 0 ,  &
      & 0 , 20 , 28 , 30 , 8 , 4HNA2  , 0 , 0 , 20 , 39 , 41 , 8 , 4HNA3  , 0 , 0 , 20 , 50 , 52 , 8 , 4HNA4  , 0 , 0 , 20 , 61 ,   &
      & 63 , 8 , 4HNA5  , 0 , 0 , 21 , 11 , 12 , 8 , 4HNA6  , 0 , 0 , 21 , 21 , 23 , 8 , 4HNA7  , 0 , 0 , 22 , 12 , 14 , 3 ,        &
      & 4HNCNO , 32 , 1 , 22 , 30 , 32 , 8 , 4HNAMC , 0 , 0/
   DATA ptbs6/23 , 11 , 13 , 3 , 4HNSTP , 0 , 0 , 24 , 11 , 12 , 3 , 4HNCNO , 0 , 1 , 24 , 17 , 19 , 8 , 4HNAMC , 0 , 0/
   DATA subnam/4HASCM , 2H03/
!
!     RESTORE TO ORIGINAL DATA BY REPLACEING ! BY / IN RDMAP ARRAY
!     (SEE ASCM01 FOR EXPLANATION))
!
   DO l = 1 , 111 , 3
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
END SUBROUTINE ascm03
