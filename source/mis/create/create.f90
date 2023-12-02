!*==create.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE create(Gplst,X,U,Deform,Conmin,Conmax,Elmtid,Store,Lcor,B1,B2)
   IMPLICIT NONE
   USE C_BLANK
   USE C_XXPARM
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Gplst
   REAL , DIMENSION(3,1) :: X
   REAL , DIMENSION(2,1) :: U
   INTEGER :: Deform
   REAL :: Conmin
   REAL :: Conmax
   INTEGER , DIMENSION(100) :: Elmtid
   REAL , DIMENSION(202) :: Store
   INTEGER :: Lcor
   INTEGER :: B1
   INTEGER :: B2
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(2,4) :: c
   REAL , DIMENSION(2) :: centrd , third
   REAL :: contur , detail , eigen , twopi
   INTEGER :: elid , esym , i , ieltyp , ieor , ig , index , irdest , is , ist , isub , iwds , j , jtj , k , kest , kq4 , kt3 ,     &
            & layskp , laytot , m , mem , newoes , ngppe , nlayer , nlfin , nwds , offset , stress
   INTEGER , DIMENSION(2) :: err , idummy
   INTEGER , DIMENSION(7) , SAVE :: estsym
   INTEGER , DIMENSION(12) :: gpts
   INTEGER , DIMENSION(14) , SAVE :: isym , itype
   INTEGER , SAVE :: kbar , nmsg1 , ntypes
   INTEGER , DIMENSION(20) , SAVE :: msg1 , skipwd
   REAL , DIMENSION(2,5) :: pt
   EXTERNAL bckrec , centre , close , fread , fwdrec , gopen , open , read , write , wrtprt
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
   !>>>>EQUIVALENCE (New,Newoes) , (kq4,isym(13)) , (kt3,isym(14))
   DATA ntypes/14/
   DATA isym/2HSH , 2HT1 , 2HTB , 2HTP , 2HTM , 2HQP , 2HQM , 2HT2 , 2HQ2 , 2HQ1 , 2HM1 , 2HM2 , 2HQ4 , 2HT3/ , kbar/2HBR/
   DATA itype/4 , 6 , 7 , 8 , 9 , 15 , 16 , 17 , 18 , 19 , 62 , 63 , 64 , 83/
   DATA estsym/7*0/
   DATA skipwd/ - 5 , -6 , -7 , -1 , -2 , -3 , -4 , -5 , -6 , 4*0 , 0 , -1 , -2 , -5 , -6 , -7 , 0/
   DATA nmsg1 , msg1/20 , 4H(48H , 4H NO  , 4HSTRE , 4HSS C , 4HALCU , 4HLATI , 4HON F , 4HOUND , 4H FOR , 4H ELE , 4HMENT ,        &
      & 4H NUM , 4HBER  , 4H,I8, , 4H19H  , 4H- EL , 4HEMEN , 4HT IG , 4HNORE , 4HD.) /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         twopi = 8.0*atan(1.0)
         irdest = 0
         CALL gopen(Scr1,Gplst(B1),1)
         stress = Oes1
         IF ( (Icntvl>=4 .AND. Icntvl<=9) .AND. Direct==2 ) stress = newoes
         IF ( stress==Oes1 .AND. (Icntvl==6 .OR. Icntvl==8 .OR. Icntvl==9) ) GOTO 140
         IF ( stress==Oes1 ) skipwd(7) = -3
         CALL open(*140,stress,Gplst(B2),0)
         Conmin = 0.0
         Conmax = 0.0
         ieor = 0
!
!     CREATE A LIST OF ELEMENTS TYPES TO BE PLOTTED IN THIS SET
!
         jtj = 1
         k = 1
         kest = 0
         CALL read(*160,*160,Est,esym,1,0,m)
         IF ( Icntvl==20 ) THEN
            CALL fread(Est,ngppe,1,0)
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     ELIMINATE ALL BUT MAXSHEAR FOR CSHEAR ELEMENT
!
         IF ( esym/=isym(1) .OR. Icntvl==3 ) THEN
!
!     ELIMINATE MID STRESS FOR TRIA1, QUAD1, TRPLT, OR QDPLT
!
            IF ( .NOT.((esym==isym(2) .OR. esym==isym(10) .OR. esym==isym(4) .OR. esym==isym(6)) .AND. Where==3) ) THEN
!
!     ELIMINATE Z2 AND AVER STRESS FOR CTRMEM, CQDMEM, MEM1, MEM2
!
               IF ( .NOT.((esym==isym(5) .OR. esym==isym(7) .OR. esym==isym(11) .OR. esym==isym(12)) .AND. (Where==-1 .OR. Where==3)&
                  & ) ) THEN
!
!     ELIMINATE Z1, Z2 AND MAX FOR TRIA2 OR TRBSC ELEMENTS
!
                  IF ( .NOT.((esym==isym(8) .OR. esym==isym(3)) .AND. (iabs(Where)==1 .OR. Where==2)) ) THEN
                     DO i = 1 , ntypes
                        IF ( esym==isym(i) ) GOTO 2
                     ENDDO
                  ENDIF
                  CALL fread(Est,ngppe,1,0)
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
 2                estsym(k) = i
                  k = k + 1
               ENDIF
            ENDIF
         ENDIF
         CALL fread(Est,ngppe,1,0)
         spag_nextblock_1 = 3
      CASE (3)
!
         offset = 0
         IF ( esym==kbar ) offset = 6
         IF ( esym==kt3 .OR. esym==kq4 ) offset = 1
         DO
!
!     FLUSH TO NEXT SYMBOL
!
            CALL fread(Est,elid,1,0)
            IF ( elid==0 ) THEN
               IF ( jtj==1 ) THEN
!
!     READ NEXT SYMBOL
!
                  CALL read(*20,*20,Est,esym,1,0,m)
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( jtj==2 ) THEN
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
            j = 1 + ngppe + offset
            CALL fread(Est,0,-j,0)
         ENDDO
!
!     LOOP BACK UNTIL ALL EST SYMBOLS ARE IN CORE
!
 20      k = k - 1
         CALL bckrec(Est)
         jtj = 2
         spag_nextblock_1 = 4
      CASE (4)
!
!     NOTE THAT THE ASSUMPTION THAT STRESS AND EST FILES ARE ORDERED IN
!     THE SAME WAY IS NO LONGER NECESSARY
!
         IF ( ieor==0 ) CALL fwdrec(*120,stress)
         IF ( Icntvl==20 ) THEN
            CALL fwdrec(*120,stress)
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         SPAG_Loop_1_1: DO
            CALL read(*120,*100,stress,idummy,2,0,m)
            CALL fread(stress,ieltyp,1,0)
            CALL fread(stress,isub,1,0)
            CALL fread(stress,detail,1,0)
            CALL fread(stress,eigen,1,0)
            eigen = sqrt(abs(eigen))/twopi
            CALL fread(stress,0,-3,0)
            CALL fread(stress,nwds,1,1)
            IF ( Sub<=0 .OR. isub==Sub ) THEN
               IF ( Flag/=1.0 .OR. detail==Value ) THEN
                  IF ( Flag/=2.0 .OR. abs(eigen-Value)<=1.0E-5 ) THEN
                     ieor = 0
                     DO i = 1 , k
                        j = estsym(i)
                        IF ( j/=0 ) THEN
                           IF ( ieltyp==itype(j) ) EXIT SPAG_Loop_1_1
                        ENDIF
                     ENDDO
                  ENDIF
               ENDIF
            ENDIF
!
!     SKIP THIS TYPE
!
            CALL fwdrec(*120,stress)
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 6
      CASE (6)
!
!     YES, WE DO WANT THIS ELEMENT TYPES STRESS DATA.  FIND THIS TYPES
!     ELEMENTS IN THE EST
!
         CALL read(*80,*80,Est,esym,1,0,m)
         irdest = 1
         CALL fread(Est,ngppe,1,0)
!
!     FLUSH THE FILE UNTIL FOUND
!
         IF ( Icntvl/=20 .AND. esym/=isym(j) ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
         CALL write(Scr1,esym,1,0)
         kest = kest + 1
         mem = 0
         IF ( ieltyp==9 .OR. ieltyp==16 .OR. ieltyp==15 .OR. ieltyp==62 .OR. ieltyp==63 ) mem = 1
!         TRMEM(9), QDMEM(16), QDPLT(15), QDMEM1(62), QDMEM2(63)
!
         iwds = skipwd(Icntvl)
         IF ( Icntvl>13 ) THEN
!         SHEAR(4)
!
            is = 0
         ELSE
            IF ( mem==1 ) iwds = iwds + 1
            IF ( Where==-1 .AND. mem/=1 ) iwds = iwds - 8
            nwds = -nwds - iwds + 2
            IF ( iabs(Where)/=1 .AND. mem/=1 ) nwds = nwds + 8
            IF ( Where==-1 .AND. mem==1 ) THEN
               CALL fwdrec(*120,stress)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( ieltyp==4 .AND. Icntvl/=3 ) THEN
               CALL fwdrec(*120,stress)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSE
               is = 0
            ENDIF
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
         DO
!
!     READ STRESS FILE
!
            is = is + 1
            CALL read(*40,*40,stress,Elmtid(is),1,0,m)
            IF ( Icntvl>9 .AND. Icntvl/=20 ) THEN
               CALL fread(stress,nlayer,1,0)
               laytot = nlayer*11
               layskp = -((Layer-1)*10+2)
               CALL fread(stress,0,layskp,0)
            ENDIF
            IF ( ieltyp/=4 ) THEN
               CALL fread(stress,0,iwds,0)
               CALL fread(stress,Store(is),1,0)
               IF ( Icntvl>9 .AND. Icntvl/=20 ) THEN
                  nlfin = -(laytot-1+layskp+iwds)
                  CALL fread(stress,0,nlfin,0)
               ELSEIF ( Icntvl<20 ) THEN
                  IF ( iabs(Where)/=1 .AND. mem/=1 ) THEN
                     CALL fread(stress,0,-7,0)
                     CALL fread(stress,contur,1,0)
                  ENDIF
                  CALL fread(stress,0,nwds,0)
                  IF ( mem==1 .AND. is>=Lcor ) THEN
                     spag_nextblock_1 = 8
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( mem/=1 ) THEN
                     IF ( Where==2 ) Store(is) = amax1(Store(is),contur)
                     IF ( Where==3 ) Store(is) = (Store(is)+contur)/2.0
                     IF ( is>=Lcor ) THEN
                        spag_nextblock_1 = 8
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDIF
               ELSE
                  CALL fread(stress,0,-1,0)
               ENDIF
            ELSE
!
!     MAXIMUM SHEAR FOR CSHEAR ELEMENT
!
               CALL fread(stress,Store(is),1,0)
               CALL fread(stress,0,-2,0)
               IF ( is==Lcor ) THEN
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDDO
!
!     END OF RECORD ON STRESS FILE
!
 40      ieor = 1
         is = is - 1
         spag_nextblock_1 = 8
      CASE (8)
         SPAG_Loop_1_2: DO
!
!     STORE STRESS VALUES WITH ELEMENT ID.S
!
            CALL fread(Est,elid,1,0)
            IF ( elid/=0 ) THEN
               CALL fread(Est,0,-1,0)
               CALL fread(Est,gpts,ngppe+offset,0)
!
!     THE VERY NEXT LINE WAS ACCIDENTALLY DROPPED IN 88 VERSION
!
               IF ( elid>Elmtid(is)/10 ) THEN
!
!     REFILL STRESS STORAGE AREA
!
                  is = 0
                  IF ( ieor==0 ) THEN
                     spag_nextblock_1 = 7
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  err(1) = 1
                  err(2) = elid
                  CALL wrtprt(Prnt,err,msg1,nmsg1)
                  CYCLE
               ELSE
                  DO ist = 1 , is
                     IF ( elid==Elmtid(ist)/10 ) GOTO 45
                  ENDDO
                  err(1) = 1
                  err(2) = elid
                  CALL wrtprt(Prnt,err,msg1,nmsg1)
                  CYCLE
               ENDIF
!
!     FIND ELEMENTS CENTROID
!
 45            DO i = 1 , ngppe
                  ig = gpts(i)
                  ig = iabs(Gplst(ig))
                  IF ( Deform/=0 ) THEN
                     pt(1,i) = U(1,ig)
                     pt(2,i) = U(2,ig)
                  ELSE
                     pt(1,i) = X(2,ig)
                     pt(2,i) = X(3,ig)
                  ENDIF
               ENDDO
               third(1) = pt(1,3)
               third(2) = pt(2,3)
               index = 1
               pt(1,ngppe+1) = pt(1,1)
               pt(2,ngppe+1) = pt(2,1)
               IF ( ngppe>=4 ) THEN
                  index = 4
                  CALL centre(*60,pt(1,1),pt(2,1),pt(1,2),pt(2,2),pt(1,3),pt(2,3),pt(1,4),pt(2,4),centrd)
                  third(1) = centrd(1)
                  third(2) = centrd(2)
               ENDIF
               DO i = 1 , index
                  CALL centre(*60,pt(1,i),pt(2,i),pt(1,i+1),pt(2,i+1),(third(1)+pt(1,i+1))*.5,(third(2)+pt(2,i+1))*.5,              &
                            & (third(1)+pt(1,i))*.5,(third(2)+pt(2,i))*.5,centrd)
                  c(1,i) = centrd(1)
                  c(2,i) = centrd(2)
               ENDDO
               IF ( ngppe>=4 ) CALL centre(*60,c(1,1),c(2,1),c(1,2),c(2,2),c(1,3),c(2,3),c(1,4),c(2,4),centrd)
            ENDIF
            EXIT SPAG_Loop_1_2
         ENDDO SPAG_Loop_1_2
 60      CALL write(Scr1,elid,1,0)
         IF ( elid/=0 ) THEN
            CALL write(Scr1,Store(ist),1,0)
            CALL write(Scr1,centrd,2,0)
            IF ( Conmin/=0.0 .OR. Conmax/=0.0 ) THEN
               Conmin = amin1(Conmin,Store(ist))
               Conmax = amax1(Conmax,Store(ist))
            ELSE
               Conmin = Store(ist)
               Conmax = Conmin
            ENDIF
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 80      IF ( kest/=k ) THEN
            CALL bckrec(Est)
            irdest = 0
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 100     DO WHILE ( stress/=newoes )
            CALL read(*120,*120,Oes1,0,-3,0,m)
            CALL fread(Oes1,isub,1,0)
            CALL fread(Oes1,detail,1,0)
            CALL fread(Oes1,eigen,1,1)
            eigen = sqrt(abs(eigen))/twopi
            IF ( isub/=Sub ) GOTO 120
            IF ( Flag==1.0 .AND. detail/=Value ) GOTO 120
            IF ( Flag==2.0 .AND. abs(eigen-Value)>1.0E-5 ) GOTO 120
            CALL fwdrec(*120,Oes1)
         ENDDO
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 120     CALL bckrec(stress)
         spag_nextblock_1 = 9
      CASE (9)
         CALL close(stress,2)
 140     CALL write(Scr1,0,0,1)
         CALL close(Scr1,1)
         IF ( irdest<=0 ) RETURN
 160     CALL bckrec(Est)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE create
