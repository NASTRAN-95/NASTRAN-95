
SUBROUTINE create(Gplst,X,U,Deform,Conmin,Conmax,Elmtid,Store,Lcor,B1,B2)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Cntr(50) , Flag , Scr2 , Ski(2) , Skip(12) , Skip1(7) , Skip4(157) , Skp20(20) , Value
   INTEGER Direct , Est , Icntvl , Layer , Ncntr , New , Newoes , Oes1 , Prnt , Scr1 , Sub , Where
   COMMON /blank / Skip , Est , Skip1 , Prnt , Ski , Oes1 , Scr1 , Scr2 , New
   COMMON /xxparm/ Skip4 , Ncntr , Cntr , Icntvl , Where , Direct , Sub , Flag , Value , Skp20 , Layer
!
! Dummy argument declarations
!
   INTEGER B1 , B2 , Deform , Lcor
   REAL Conmax , Conmin
   INTEGER Elmtid(100) , Gplst(1)
   REAL Store(202) , U(2,1) , X(3,1)
!
! Local variable declarations
!
   REAL c(2,4) , centrd(2) , contur , detail , eigen , pt(2,5) , third(2) , twopi
   INTEGER elid , err(2) , estsym(7) , esym , gpts(12) , i , idummy(2) , ieltyp , ieor , ig , index , irdest , is , ist , isub ,    &
         & isym(14) , itype(14) , iwds , j , jtj , k , kbar , kest , kq4 , kt3 , layskp , laytot , m , mem , msg1(20) , ngppe ,     &
         & nlayer , nlfin , nmsg1 , ntypes , nwds , offset , skipwd(20) , stress
!
! End of declarations
!
!
   EQUIVALENCE (New,Newoes) , (kq4,isym(13)) , (kt3,isym(14))
   DATA ntypes/14/
   DATA isym/2HSH , 2HT1 , 2HTB , 2HTP , 2HTM , 2HQP , 2HQM , 2HT2 , 2HQ2 , 2HQ1 , 2HM1 , 2HM2 , 2HQ4 , 2HT3/ , kbar/2HBR/
   DATA itype/4 , 6 , 7 , 8 , 9 , 15 , 16 , 17 , 18 , 19 , 62 , 63 , 64 , 83/
   DATA estsym/7*0/
   DATA skipwd/ - 5 , -6 , -7 , -1 , -2 , -3 , -4 , -5 , -6 , 4*0 , 0 , -1 , -2 , -5 , -6 , -7 , 0/
   DATA nmsg1 , msg1/20 , 4H(48H , 4H NO  , 4HSTRE , 4HSS C , 4HALCU , 4HLATI , 4HON F , 4HOUND , 4H FOR , 4H ELE , 4HMENT ,        &
      & 4H NUM , 4HBER  , 4H,I8, , 4H19H  , 4H- EL , 4HEMEN , 4HT IG , 4HNORE , 4HD.) /
!
   twopi = 8.0*atan(1.0)
   irdest = 0
   CALL gopen(Scr1,Gplst(B1),1)
   stress = Oes1
   IF ( (Icntvl>=4 .AND. Icntvl<=9) .AND. Direct==2 ) stress = Newoes
   IF ( stress==Oes1 .AND. (Icntvl==6 .OR. Icntvl==8 .OR. Icntvl==9) ) GOTO 1600
   IF ( stress==Oes1 ) skipwd(7) = -3
   CALL open(*1600,stress,Gplst(B2),0)
   Conmin = 0.0
   Conmax = 0.0
   ieor = 0
!
!     CREATE A LIST OF ELEMENTS TYPES TO BE PLOTTED IN THIS SET
!
   jtj = 1
   k = 1
   kest = 0
   CALL read(*1700,*1700,Est,esym,1,0,m)
   IF ( Icntvl==20 ) THEN
      CALL fread(Est,ngppe,1,0)
      GOTO 200
   ENDIF
!
!     ELIMINATE ALL BUT MAXSHEAR FOR CSHEAR ELEMENT
!
 100  IF ( esym/=isym(1) .OR. Icntvl==3 ) THEN
!
!     ELIMINATE MID STRESS FOR TRIA1, QUAD1, TRPLT, OR QDPLT
!
      IF ( .NOT.((esym==isym(2) .OR. esym==isym(10) .OR. esym==isym(4) .OR. esym==isym(6)) .AND. Where==3) ) THEN
!
!     ELIMINATE Z2 AND AVER STRESS FOR CTRMEM, CQDMEM, MEM1, MEM2
!
         IF ( .NOT.((esym==isym(5) .OR. esym==isym(7) .OR. esym==isym(11) .OR. esym==isym(12)) .AND. (Where==-1 .OR. Where==3)) )   &
            & THEN
!
!     ELIMINATE Z1, Z2 AND MAX FOR TRIA2 OR TRBSC ELEMENTS
!
            IF ( .NOT.((esym==isym(8) .OR. esym==isym(3)) .AND. (iabs(Where)==1 .OR. Where==2)) ) THEN
               DO i = 1 , ntypes
                  IF ( esym==isym(i) ) GOTO 110
               ENDDO
            ENDIF
            CALL fread(Est,ngppe,1,0)
            GOTO 200
 110        estsym(k) = i
            k = k + 1
         ENDIF
      ENDIF
   ENDIF
   CALL fread(Est,ngppe,1,0)
!
 200  offset = 0
   IF ( esym==kbar ) offset = 6
   IF ( esym==kt3 .OR. esym==kq4 ) offset = 1
!
!     FLUSH TO NEXT SYMBOL
!
 300  CALL fread(Est,elid,1,0)
   IF ( elid==0 ) THEN
      IF ( jtj==1 ) THEN
!
!     READ NEXT SYMBOL
!
         CALL read(*400,*400,Est,esym,1,0,m)
         GOTO 100
      ELSEIF ( jtj==2 ) THEN
         GOTO 700
      ENDIF
   ENDIF
   j = 1 + ngppe + offset
   CALL fread(Est,0,-j,0)
   GOTO 300
!
!     LOOP BACK UNTIL ALL EST SYMBOLS ARE IN CORE
!
 400  k = k - 1
   CALL bckrec(Est)
   jtj = 2
!
!     NOTE THAT THE ASSUMPTION THAT STRESS AND EST FILES ARE ORDERED IN
!     THE SAME WAY IS NO LONGER NECESSARY
!
 500  IF ( ieor==0 ) CALL fwdrec(*1400,stress)
   IF ( Icntvl==20 ) THEN
      CALL fwdrec(*1400,stress)
      GOTO 700
   ENDIF
 600  DO
      CALL read(*1400,*1300,stress,idummy,2,0,m)
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
                     IF ( ieltyp==itype(j) ) GOTO 700
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
      ENDIF
!
!     SKIP THIS TYPE
!
      CALL fwdrec(*1400,stress)
   ENDDO
!
!     YES, WE DO WANT THIS ELEMENT TYPES STRESS DATA.  FIND THIS TYPES
!     ELEMENTS IN THE EST
!
 700  CALL read(*1200,*1200,Est,esym,1,0,m)
   irdest = 1
   CALL fread(Est,ngppe,1,0)
!
!     FLUSH THE FILE UNTIL FOUND
!
   IF ( Icntvl/=20 .AND. esym/=isym(j) ) GOTO 200
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
         CALL fwdrec(*1400,stress)
         GOTO 600
      ELSEIF ( ieltyp==4 .AND. Icntvl/=3 ) THEN
         CALL fwdrec(*1400,stress)
         GOTO 600
      ELSE
         is = 0
      ENDIF
   ENDIF
 800  DO
!
!     READ STRESS FILE
!
      is = is + 1
      CALL read(*900,*900,stress,Elmtid(is),1,0,m)
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
            IF ( mem==1 .AND. is>=Lcor ) GOTO 1000
            IF ( mem/=1 ) THEN
               IF ( Where==2 ) Store(is) = amax1(Store(is),contur)
               IF ( Where==3 ) Store(is) = (Store(is)+contur)/2.0
               IF ( is>=Lcor ) GOTO 1000
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
         IF ( is==Lcor ) GOTO 1000
      ENDIF
   ENDDO
!
!     END OF RECORD ON STRESS FILE
!
 900  ieor = 1
   is = is - 1
 1000 DO
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
            IF ( ieor==0 ) GOTO 800
            err(1) = 1
            err(2) = elid
            CALL wrtprt(Prnt,err,msg1,nmsg1)
            CYCLE
         ELSE
            DO ist = 1 , is
               IF ( elid==Elmtid(ist)/10 ) GOTO 1020
            ENDDO
            err(1) = 1
            err(2) = elid
            CALL wrtprt(Prnt,err,msg1,nmsg1)
            CYCLE
         ENDIF
!
!     FIND ELEMENTS CENTROID
!
 1020    DO i = 1 , ngppe
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
            CALL centre(*1100,pt(1,1),pt(2,1),pt(1,2),pt(2,2),pt(1,3),pt(2,3),pt(1,4),pt(2,4),centrd)
            third(1) = centrd(1)
            third(2) = centrd(2)
         ENDIF
         DO i = 1 , index
            CALL centre(*1100,pt(1,i),pt(2,i),pt(1,i+1),pt(2,i+1),(third(1)+pt(1,i+1))*.5,(third(2)+pt(2,i+1))*.5,(third(1)+pt(1,i))&
                      & *.5,(third(2)+pt(2,i))*.5,centrd)
            c(1,i) = centrd(1)
            c(2,i) = centrd(2)
         ENDDO
         IF ( ngppe>=4 ) CALL centre(*1100,c(1,1),c(2,1),c(1,2),c(2,2),c(1,3),c(2,3),c(1,4),c(2,4),centrd)
      ENDIF
      EXIT
   ENDDO
 1100 CALL write(Scr1,elid,1,0)
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
      GOTO 1000
   ENDIF
 1200 IF ( kest/=k ) THEN
      CALL bckrec(Est)
      irdest = 0
      GOTO 500
   ENDIF
 1300 DO WHILE ( stress/=Newoes )
      CALL read(*1400,*1400,Oes1,0,-3,0,m)
      CALL fread(Oes1,isub,1,0)
      CALL fread(Oes1,detail,1,0)
      CALL fread(Oes1,eigen,1,1)
      eigen = sqrt(abs(eigen))/twopi
      IF ( isub/=Sub ) GOTO 1400
      IF ( Flag==1.0 .AND. detail/=Value ) GOTO 1400
      IF ( Flag==2.0 .AND. abs(eigen-Value)>1.0E-5 ) GOTO 1400
      CALL fwdrec(*1400,Oes1)
   ENDDO
   GOTO 1500
 1400 CALL bckrec(stress)
 1500 CALL close(stress,2)
 1600 CALL write(Scr1,0,0,1)
   CALL close(Scr1,1)
   IF ( irdest<=0 ) GOTO 99999
 1700 CALL bckrec(Est)
99999 RETURN
END SUBROUTINE create
