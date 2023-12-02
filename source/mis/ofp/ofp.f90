!*==ofp.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE ofp
!
!     THE OUTPUT FILE PROCESSOR
!
!     THIS SUBROUTINE IS THE MAIN AND ONLY DRIVER.
!     OFP1 OUTPUTS HEADINGS ONLY.
!
   IMPLICIT NONE
   USE C_BLANK
   USE C_OFP1ID
   USE C_OFPBD1
   USE C_OFPBD5
   USE C_OFPCOM
   USE C_OUTPUT
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   *0() :: 
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: a4 , bk1 , ceigen , center , comma , cparen , e156 , e236 , e9pt1 , eend , f156 , f174 , f236 , freq , hex1 ,  &
                   & hex2 , hex3 , i12 , i15x , i1h0 , i1x , i1xx , i2x , i2xx , i6x , i8 , i9x , iblank , ih0 , iheat , istar ,    &
                   & oparen , pe , pf , phase , reigen , static , trans
   LOGICAL :: axic , dummy , elemen , eor , ese , fluid , gpfb , gpst , headng , heat , onefil , pack , pnched , solset , strain
   INTEGER :: axif , device , file , flag , from , grdpt , i , i1 , i2 , iapp , icore , icurv , idd , ieltyp , ifile , ifmt , igse ,&
            & iharm , ihd , incr , iopt , itemp , itherm , itype , ix , j , jj , k , k1 , k2 , kk , l , l1 , l2 , l3 , l4 , l5 ,    &
            & lastid , line , maxlns , maxn , nadd , nharm , nlines , nmult , nwds , nwdsav , oldhrm , point , sort , sysbuf , type
   INTEGER , DIMENSION(23,4) , SAVE :: b
   INTEGER , DIMENSION(1) :: buff
   CHARACTER(1) , DIMENSION(300) :: cfmt
   REAL*8 , DIMENSION(50) :: dout
   INTEGER , DIMENSION(6) , SAVE :: filex
   REAL , DIMENSION(2) :: fimag
   INTEGER , DIMENSION(300) :: fmt
   REAL , DIMENSION(10) :: freal
   INTEGER , DIMENSION(4) , SAVE :: gse
   INTEGER , DIMENSION(2) , SAVE :: i15blk , scan
   INTEGER , DIMENSION(50) :: id
   INTEGER :: Ifile1
   INTEGER , DIMENSION(5) :: imag
   INTEGER , DIMENSION(100) :: iout
   INTEGER , DIMENSION(20) :: isave
   INTEGER , DIMENSION(56) :: of
   REAL , DIMENSION(100) :: out
   INTEGER , DIMENSION(10) :: real
   INTEGER , DIMENSION(96) :: tsave
!
! End of declarations rewritten by SPAG
!
!WKBI
!ZZ   COMMON /ZZOFPX/  CORE(1)
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),L) , (Ksystm(9),Maxlns) , (Ksystm(12),Line) , (Ksystm(38),Axif) , (Ksystm(56),Itherm)&
   !>>>> & , (freal(1),real(1)) , (fimag(1),imag(1)) , (Id(3),ieltyp) , (iout(1),out(1),dout(1)) , (L1,Of(1),Core(1)) , (L2,Of(2)) ,     &
   !>>>> & (L3,Of(3)) , (L4,Of(4)) , (L5,Of(5)) , (Id(1),Of(6)) , (Buff(1),Of(56))
!WKBI
   !>>>>EQUIVALENCE (cfmt,fmt)
   DATA pe/4H1P,E/ , pf/4H0P,F/
   DATA e236/4H23.6/ , f236/4H14.1/
   DATA e156/4H15.6/ , f156/4H6.1 /
   DATA i8/4H,I8,/ , i12/4H,I11/
   DATA i2x/4H2X  / , i2xx/4H,2X /
   DATA i1x/4H(1X / , i1xx/4H,1X /
   DATA istar/4H,1H*/ , i15x/4H/14X/
   DATA ih0/4H/1H0/ , i1h0/4H(1H0/
   DATA i9x/4H,9X / , i6x/4H,6X /
   DATA f174/4H17.4/
   DATA static , reigen , freq , trans , bk1 , ceigen/1 , 2 , 5 , 6 , 8 , 9/
   DATA a4 , comma , cparen , oparen/4HA4   , 4H,    , 4H)    , 4H(   /
   DATA eend/195/ , i15blk/4HA4,  , 4H11X / , gse/4HG    , 4HS    , 4HE    , 4HM   /
   DATA filex/101 , 102 , 103 , 104 , 105 , 106/
   DATA iblank , e9pt1/4H     , 195/
   DATA iheat/4HHEAT/ , center/4HTER /
   DATA phase/4H1P9E/ , scan/4HSCAN , 4HNED /
   DATA hex1 , hex2 , hex3/4HHEX1 , 4HHEX2 , 4HHEX3/
!
!
!     THE FOLLOWING ARE ZERO POINTERS TO THE DATA-BLOCK AND LINE-SET
!     SELECTION LISTS.  TO THIS THE SUBSET SELECTION POINTER IS ADDED.
!     THE SUBSET SELECTION POINTER IS BASED ON INFORMATION IN THE ID
!     RECORD.
!
!     A MINUS ONE IN THE FOLLOWING ARRAY INDICATES AN UNDEFINED OUTPUT.
!
!            S O R T - I    S O R T - I I
!          *************** ****************        ZERO-BASE
!          REAL    COMPLEX REAL    COMPLEX    POINTERS INTO C-ARY
!     *************************************   *******************
!     DISPLACEMENT VECTOR
   DATA b(1,1) , b(1,2) , b(1,3) , b(1,4)/0 , 4 , 2 , 6/
!
!     LOAD VECTOR
   DATA b(2,1) , b(2,2) , b(2,3) , b(2,4)/8 , 12 , 10 , 14/
!
!     SPCF VECTOR
   DATA b(3,1) , b(3,2) , b(3,3) , b(3,4)/16 , 20 , 18 , 22/
!
!     ELEMENT FORCE   (ZERO POINTERS INTO OVERLAY BLOCK DATA)
   DATA b(4,1) , b(4,2) , b(4,3) , b(4,4)/0 , 0 , 0 , 0/
!
!     ELEMENT STRESS  (ZERO POINTERS FOR OVERLAY BLOCK DATA)
   DATA b(5,1) , b(5,2) , b(5,3) , b(5,4)/0 , 0 , 0 , 0/
!
!     EIGENVALUE SUMMARY
   DATA b(6,1) , b(6,2) , b(6,3) , b(6,4)/38 , 39 , -1 , -1/
!
!     EIGENVECTOR
   DATA b(7,1) , b(7,2) , b(7,3) , b(7,4)/72 , 40 , -1 , -1/
!
!     GPST
   DATA b(8,1) , b(8,2) , b(8,3) , b(8,4)/73 , -1 , -1 , -1/
!
!     EIGENVALUE ANALYSIS SUMMARY
   DATA b(9,1) , b(9,2) , b(9,3) , b(9,4)/64 , 68 , -1 , -1/
!
!     VELOCITY VECTOR
   DATA b(10,1) , b(10,2) , b(10,3) , b(10,4)/24 , 30 , 26 , 32/
!
!     ACCELERATION VECTOR
   DATA b(11,1) , b(11,2) , b(11,3) , b(11,4)/25 , 34 , 27 , 36/
!
!     NON-LINEAR-FORCE VECTOR
   DATA b(12,1) , b(12,2) , b(12,3) , b(12,4)/28 , -1 , 29 , -1/
!
!     GRID-POINT-WEIGHT-OUTPUT
   DATA b(13,1) , b(13,2) , b(13,3) , b(13,4)/ - 1 , -1 , -1 , -1/
!
!     EIGENVECTOR (SOLUTION SET FROM VDR)
   DATA b(14,1) , b(14,2) , b(14,3) , b(14,4)/ - 1 , 60 , -1 , 62/
!
!     DISP-VECTOR (SOLUTION SET FROM VDR)
   DATA b(15,1) , b(15,2) , b(15,3) , b(15,4)/42 , 44 , 43 , 46/
!
!     VELO-VECTOR (SOLUTION SET FROM VDR)
   DATA b(16,1) , b(16,2) , b(16,3) , b(16,4)/48 , 50 , 49 , 52/
!
!     ACCE-VECTOR (SOLUTION SET FROM VDR)
   DATA b(17,1) , b(17,2) , b(17,3) , b(17,4)/54 , 56 , 55 , 58/
!
!     ELEMENT STRAIN ENERGY (FROM GPFDR)
   DATA b(18,1) , b(18,2) , b(18,3) , b(18,4)/74 , -1 , -1 , -1/
!
!     GRID POINT FORCE BALANCE (FROM GPFDR)
   DATA b(19,1) , b(19,2) , b(19,3) , b(19,4)/76 , -1 , -1 , -1/
!
!     MPCFORCE VECTOR
   DATA b(20,1) , b(20,2) , b(20,3) , b(20,4)/78 , -1 , -1 , -1/
!
!     ELEMENT STRAIN/CURVATURE (ZERO POINTER FOR OVERLAY BLOCK DATA)
   DATA b(21,1) , b(21,2) , b(21,3) , b(21,4)/0 , -1 , -1 , -1/
!
!     STRESSES IN LAYERED COMPOSITE ELEMENTS (ZERO POINTER)
   DATA b(22,1) , b(22,2) , b(22,3) , b(22,4)/0 , 0 , 0 , 0/
!
!     FORCES IN LAYERED COMPOSITE ELEMENTS   (ZERO POINTER)
   DATA b(23,1) , b(23,2) , b(23,3) , b(23,4)/0 , 0 , 0 , 0/
!     ************************************   *******************
!
!     SAVE OLD TITLES WHATEVER THEY BE AND RESTORE BEFORE RETURNING
!
   CALL totape(3,buff(1))
   heat = .FALSE.
   IF ( itherm/=0 ) heat = .TRUE.
   Option(1) = 0
   IF ( heat ) Option(1) = iheat
   onefil = .FALSE.
   GOTO 100
!
!
   ENTRY ofpdmp(Ifile1)
!     =====================
!
   onefil = .TRUE.
 100  DO i = 1 , 96
      tsave(i) = Head(i)
   ENDDO
!
   icore = korsz(buff)
   IF ( icore>=sysbuf ) THEN
!
      line = 0
      ifile = 0
   ELSE
      WRITE (6,99001) Uwm , icore , sysbuf
99001 FORMAT (A25,' 2043, OFP HAS INSUFFICIENT CORE FOR ONE GINO ','BUFFER ****    OFP NOT EXECUTED.')
      RETURN
   ENDIF
!
!     LOOP FOR 6 FILES
!
 200  ifile = ifile + 1
   IF ( onefil .AND. ifile>1 ) GOTO 2500
   file = filex(ifile)
   IF ( onefil ) file = Ifile1
   CALL open(*2400,file,buff(1),0)
   from = 55
   CALL fwdrec(*2200,file)
 300  DO
      CALL read(*2300,*2300,file,id(1),50,0,flag)
      CALL read(*2300,*2300,file,Head(1),96,1,flag)
      axic = .FALSE.
      Temper = .FALSE.
      dummy = .FALSE.
      gpst = .FALSE.
      sort = 1
      pnched = .FALSE.
      headng = .FALSE.
      gpfb = .FALSE.
      ese = .FALSE.
      strain = .FALSE.
!
!     COMPUTE I AND J, THE B ARRAY SUBSCRIPTS
!
      j = id(2)/1000
      i = id(2) - j*1000
      j = j + 1
      IF ( i==4 .OR. i==5 .OR. i==21 ) THEN
         icurv = id(3)/1000
         id(3) = id(3) - 1000*icurv
      ENDIF
!
      pack = .FALSE.
      solset = .FALSE.
      fluid = .FALSE.
      IF ( axif/=0 ) fluid = .TRUE.
      elemen = .FALSE.
      iapp = id(1)/10
      nadd = 1
      from = 75
      IF ( j>4 ) GOTO 2200
      from = 77
      IF ( j<0 ) GOTO 2200
      from = 80
      IF ( i<1 .OR. i>23 ) GOTO 2200
      IF ( j>2 ) sort = 2
      IF ( j==3 .AND. iapp==static ) THEN
         IF ( Head(74)==scan(1) .AND. Head(75)==scan(2) ) THEN
            DO ihd = 65 , 72
               IF ( ihd>=68 ) Head(ihd+22) = iblank
               Head(ihd) = iblank
            ENDDO
         ELSE
            DO ihd = 65 , 96
               Head(ihd) = iblank
            ENDDO
         ENDIF
      ENDIF
      IF ( i/=4 ) THEN
         IF ( i/=5 ) THEN
            IF ( i/=6 ) THEN
               IF ( i==8 ) THEN
!
!     GPST
!
                  gpst = .TRUE.
                  GOTO 350
               ELSEIF ( i==9 ) THEN
!
!     EIGENVALUE ANALYSIS SUMMARY
!       ID(3) = 1  DETERMINANT METHOD TABLE
!       ID(3) = 2  INVERSE POWER TABLE
!       ID(3) = 3  DETERMINANT METHOD SWEPT FUNCTION DATA VECTORS
!       ID(3) = 4  UPPER HESSENBERG METHOD TABLE
!
                  nadd = 6*(id(3)-1) + 1
                  from = 300
!
!     VELOCITY VECTOR
!
!
!     ACCELERATION VECTOR
!
                  IF ( id(3)>4 ) GOTO 2200
                  GOTO 350
               ELSEIF ( i==13 ) THEN
!
!     GRID-POINT-WEIGHT-OUTPUT
!     (FROM = 345 AND 355 ARE SETUP IN OFPGPW)
!
                  from = 340
                  IF ( j>1 ) GOTO 2200
                  CALL ofpgpw(*2200,file,dout,from)
                  CYCLE
               ELSEIF ( i==14 .OR. i==15 .OR. i==16 .OR. i==17 ) THEN
!
!     EIGENVECTOR, DISPLACEMENT, VELOCITY, ACCELERATION
!     (VDR OUTPUT ONLY)
!
                  pack = .TRUE.
                  solset = .TRUE.
                  GOTO 350
               ELSEIF ( i==18 ) THEN
!
!     ELEMENT STRAIN ENERGY.
!
                  ese = .TRUE.
                  iopt = 3
                  GOTO 350
               ELSEIF ( i==19 ) THEN
!
!     GRID POINT FORCE BALANCE.
!
                  gpfb = .TRUE.
                  iopt = 4
                  lastid = 0
                  GOTO 350
               ELSEIF ( i==20 ) THEN
               ELSEIF ( i==21 ) THEN
!
!     ELEMENT STRAIN/CURVATURE
!
                  from = 410
!WKBR NCL93012 3/94     1    ID(3).NE.19) GO TO 2020
                  IF ( id(3)/=6 .AND. id(3)/=17 .AND. id(3)/=18 .AND. id(3)/=19 .AND. id(3)/=64 .AND. id(3)/=83 ) GOTO 2200
                  from = 415
                  IF ( icurv>2 ) GOTO 2200
                  strain = .TRUE.
                  elemen = .TRUE.
                  iopt = 2
                  nadd = 0
                  IF ( id(3)==6 ) nadd = 1
                  IF ( id(3)==17 ) nadd = 7
                  IF ( id(3)==18 ) nadd = 13
                  IF ( id(3)==19 ) nadd = 19
!WKBNB NCL93012 3/94
                  IF ( id(3)==64 ) nadd = 55
                  IF ( id(3)==83 ) nadd = 61
!WKBNE NCL93012 3/94
                  IF ( icurv==1 ) nadd = nadd + 24
                  IF ( icurv==2 ) nadd = 49
                  GOTO 350
               ELSEIF ( i==22 .OR. i==23 ) THEN
!
!     STRESSES AND FORCES IN LAYERED COMPOSITE ELEMENTS
!
                  CALL ofcomp(*300,file,j,ieltyp,iapp,headng,pnched,i)
                  CYCLE
               ELSE
                  pack = .TRUE.
                  IF ( id(3)==1000 ) axic = .TRUE.
                  IF ( i/=1 ) THEN
                     IF ( i==2 ) THEN
!
!     LOAD VECTOR
!
                        IF ( j==3 .AND. iapp==trans ) nadd = 7
                     ELSEIF ( i==3 ) THEN
                        GOTO 305
                     ELSEIF ( i==7 ) THEN
                     ELSEIF ( i==10 ) THEN
                     ELSEIF ( i==11 ) THEN
                     ELSEIF ( i==12 ) THEN
!
!     NON-LINERAR FORCE VECTOR
!
                        solset = .TRUE.
                     ELSE
                        CALL mesage(-61,0,0)
                        GOTO 302
                     ENDIF
                     GOTO 350
                  ENDIF
!
!     DISPLACEMENT VECTOR
!
 302              IF ( j==3 .AND. iapp==trans ) nadd = 7
                  IF ( Option(1)==iheat ) THEN
                     IF ( i==1 .AND. (j==1 .OR. j==3) ) Temper = .TRUE.
                  ENDIF
                  GOTO 350
               ENDIF
!
!     SPCF VECTOR, MPCF VECTOR
!
 305           IF ( j==3 .AND. iapp==trans ) nadd = 7
!WKBI 11/93 SPR93007
               pack = .TRUE.
            ENDIF
            GOTO 350
         ENDIF
      ENDIF
!
!     ELEMENT FORCE, ELEMENT STRESS
!
      from = 240
      IF ( id(3)<1 .OR. id(3)>100 ) GOTO 2200
      IF ( id(3)>52 .AND. id(3)<62 ) dummy = .TRUE.
      elemen = .TRUE.
      iopt = 2
      IF ( icurv>0 .AND. j==1 ) THEN
!
!     ELEMENT STRESS IN MATERIAL COORDINATE SYSTEM
!
         from = 260
         IF ( icurv>2 ) GOTO 2200
         nadd = 0
         IF ( id(3)==6 ) nadd = 1
         IF ( id(3)==17 ) nadd = 7
         IF ( id(3)==18 ) nadd = 13
         IF ( id(3)==19 ) nadd = 19
!
!     EIGENVALUE SUMMARY
!
!
!     EIGENVECTOR
!
         IF ( icurv==2 ) nadd = 25
      ELSEIF ( icurv>0 .AND. (j==2 .OR. j==4) ) THEN
         from = 250
         IF ( icurv>1 ) GOTO 2200
         nadd = 0
         IF ( id(3)==6 ) nadd = 1
         IF ( id(3)==17 ) nadd = 13
         IF ( id(3)==18 ) nadd = 25
         IF ( id(3)==19 ) nadd = 37
      ELSE
         nadd = 6*(id(3)-1) + 1
         IF ( j==2 .OR. j==4 ) nadd = nadd*2 - 1
      ENDIF
!
 350  from = 500
      IF ( b(i,j)==-1 ) GOTO 2200
      IF ( pack ) iopt = 1
      point = nadd + b(i,j)*6
!
!     IS THIS MAGNITUDE / PHASE OUTPUT
!
      IF ( id(9)==3 .AND. (iapp==freq .OR. iapp==ceigen) ) point = point + 6
!
      IF ( strain ) THEN
         CALL ofpsn1(ix,l1,l2,l3,l4,l5,point)
         from = 665
      ELSEIF ( elemen ) THEN
!
!     CALL PARTICULAR STRESS OR FORCE OVERLAY CONSIDERING
!     REAL, COMPLEX, SORT1, SORT2.
!
         IF ( dummy ) THEN
!
            CALL odum(1,ix,itype,nmult,nlines,id)
            dummy = .FALSE.
            from = 580
         ELSE
            IF ( icurv>0 ) THEN
               IF ( j==1 ) THEN
!
                  CALL ofpss1(ix,l1,l2,l3,l4,l5,point)
                  from = 655
                  GOTO 400
               ELSEIF ( j==2 ) THEN
!
                  CALL ofpcc1(ix,l1,l2,l3,l4,l5,point)
                  from = 675
                  GOTO 400
               ELSEIF ( j==4 ) THEN
!
                  CALL ofpcc2(ix,l1,l2,l3,l4,l5,point)
                  from = 685
                  GOTO 400
               ENDIF
            ENDIF
            itype = j + 4*(5-i)
            IF ( itype==2 ) THEN
!
               CALL ofpcs1(ix,l1,l2,l3,l4,l5,point)
               from = 535
            ELSEIF ( itype==3 ) THEN
!
               IF ( iapp/=static ) THEN
                  CALL ofprs2(ix,l1,l2,l3,l4,l5,point)
                  from = 555
               ELSE
                  CALL ofrs2s(ix,l1,l2,l3,l4,l5,point)
                  from = 545
               ENDIF
            ELSEIF ( itype==4 ) THEN
!
               CALL ofpcs2(ix,l1,l2,l3,l4,l5,point)
               from = 565
            ELSEIF ( itype==5 ) THEN
!
               CALL ofprf1(ix,l1,l2,l3,l4,l5,point)
               from = 575
               IF ( .NOT.(.NOT.heat .OR. id(3)==82) ) THEN
                  IF ( id(10)/=-9 ) THEN
!
!     REAL FORCE SORT 1 (HEAT)
!
                     l2 = 297
                     IF ( id(10)==5 ) l2 = 302
                     l4 = 0
                     l5 = 298
                     IF ( id(10)==5 ) l5 = 300
                  ELSE
                     l2 = 405
                     l4 = 0
                     l5 = 406
                     id(10) = 9
                  ENDIF
                  GOTO 450
               ENDIF
            ELSEIF ( itype==6 ) THEN
!
               CALL ofpcf1(ix,l1,l2,l3,l4,l5,point)
               from = 615
               IF ( heat ) GOTO 2200
            ELSEIF ( itype==7 ) THEN
!
               IF ( iapp/=static ) THEN
                  CALL ofprf2(ix,l1,l2,l3,l4,l5,point)
                  from = 635
                  IF ( .NOT.(.NOT.heat .OR. id(3)==82) ) THEN
!
!     REAL FORCE SORT 2 (HEAT)
!
                     l1 = 108
                     l2 = 297
                     IF ( id(10)==5 ) l2 = 302
                     l5 = 299
                     IF ( id(10)==5 ) l5 = 301
                     GOTO 450
                  ENDIF
               ELSE
                  CALL ofrf2s(ix,l1,l2,l3,l4,l5,point)
                  from = 625
               ENDIF
            ELSEIF ( itype==8 ) THEN
!
               CALL ofpcf2(ix,l1,l2,l3,l4,l5,point)
               from = 645
               IF ( heat ) GOTO 2200
            ELSE
!
               CALL ofprs1(ix,l1,l2,l3,l4,l5,point)
               from = 525
            ENDIF
         ENDIF
      ELSE
!
!     CALL NON-STRESS AND NON-FORCE OVERLAY.
!
         CALL ofpmis(ix,l1,l2,l3,l4,l5,point)
         from = 505
      ENDIF
!
 400  IF ( ix==0 ) THEN
!
!     ERROR CONDITION THIS FILE
!
         WRITE (l,99002) Swm , ix , point , from
99002    FORMAT (A27,', OFP BLOCK DATA ROUTINES UNAVAILABLE FOR THIS ','ELEMENT.',11X,'IX,POINT,FROM =,',3I5)
         GOTO 2200
      ENDIF
!
!     IF THERMAL DISPLACEMENTS IN -HEAT- PROBLEMS, CHANGE HEADING
!     FROM  DISPLACEMENT TO TEMPERATURE
!
 450  IF ( Temper .AND. l2==1 ) l2 = 253
      IF ( Temper .AND. l3==1 ) l3 = 253
!
!     HEAT PROBLEMS REAL-SORT1-VECTORS ONLY
!
      IF ( heat .AND. pack .AND. j==1 ) l5 = 296
      IF ( heat .AND. sort==2 .AND. .NOT.elemen ) l5 = 303
      IF ( axic ) l4 = -1
      IF ( axic ) l5 = 203
      IF ( axic .AND. iapp==trans .AND. j==3 ) l5 = 402
      IF ( axic .AND. iapp==static .AND. j==3 ) l5 = 403
      IF ( axic .AND. iapp==freq .AND. j==4 ) l5 = 404
      IF ( j==1 ) THEN
         IF ( iapp==trans .AND. i/=8 ) l1 = 106
         IF ( (iapp==reigen .OR. iapp==bk1) .AND. i/=6 .AND. i/=8 .AND. i/=9 ) l1 = 102
      ELSEIF ( j==2 ) THEN
         IF ( iapp==ceigen .AND. i/=6 .AND. i/=9 ) l1 = 110
      ENDIF
      idd = 0
      IF ( sort/=1 ) THEN
         idd = id(5)
         itemp = idd/10
         device = idd - 10*itemp
         device = mod(device,8)
         idd = itemp
         id(5) = idd
         IF ( heat .AND. .NOT.elemen .AND. sort==2 ) l5 = 303
         IF ( iapp==static ) idd = -1
      ENDIF
!
!     SORT2 HARMONIC VECTOR OUTPUT
!
      IF ( .NOT.(.NOT.pack .OR. .NOT.fluid .OR. sort==1) ) THEN
         IF ( id(5)>=500000 ) THEN
            IF ( l1==107 ) l1 = 229
         ENDIF
         fluid = .FALSE.
         IF ( ese .OR. gpfb ) EXIT
      ENDIF
      IF ( .NOT.(pack .OR. elemen) ) THEN
         CALL ofp1
         headng = .TRUE.
      ENDIF
      EXIT
   ENDDO
!
!     OUTPUT THE DATA BLOCK
!
   eor = .FALSE.
   IF ( elemen .AND. id(3)==35 ) axic = .TRUE.
   IF ( elemen .AND. id(3)==70 ) axic = .TRUE.
   IF ( elemen .AND. id(3)==71 ) axic = .TRUE.
   IF ( axic ) solset = .TRUE.
!
!     D(IX) CONTINS TWO VALUES IN A PACKED 4 DIGIT NUMBER.
!     THE RIGHT 2 DIGITS GIVES THE NUMBER OF LINES FORMAT PRODUCES.
!     THE LEFT 2 DIGITS GIVES THE NUMBER OF DATA VECTORS PER LINE.
!     IF  THE LEFT 2 DIGITS ARE 0 OR NULL, 1 VECTOR IS ASSUMED.
!
   IF ( heat ) THEN
      nmult = 1
      nlines = 1
   ELSEIF ( .NOT.(dummy) ) THEN
      nmult = D(ix)/100
      nlines = D(ix) - nmult*100
   ENDIF
   IF ( nmult==0 ) nmult = 1
   nwds = id(10)*nmult
   nwdsav = nwds
   maxn = maxlns - nlines
!
   IF ( nwds==0 ) GOTO 300
 500  IF ( eor ) GOTO 300
   from = 900
   CALL read(*2200,*600,file,iout(1),nwds,0,flag)
   GOTO 700
 600  IF ( flag==0 ) GOTO 300
   IF ( flag==0 ) GOTO 300
   nwds = flag
   nwdsav = nwds
   eor = .TRUE.
 700  i1 = 0
   IF ( .NOT.(axic) ) THEN
      igse = iout(2)
      IF ( pack .OR. gpst ) iout(2) = gse(igse)
      IF ( .NOT.(ese .OR. gpfb) ) THEN
         IF ( .NOT.pack .AND. .NOT.elemen ) GOTO 800
      ENDIF
   ENDIF
   IF ( sort/=2 ) THEN
      incr = id(10)
      i = 1
      k1 = 1
      DO
         itemp = iout(i)/10
         device = iout(i) - 10*itemp
         device = mod(device,8)
         iout(i) = itemp
         IF ( device>=4 ) THEN
            CALL ofppun(iout(i),iout(i),incr,iopt,idd,pnched)
            device = device - 4
         ENDIF
         IF ( device>0 ) THEN
            i = i + incr
            IF ( i>nwds ) EXIT
         ELSE
!
!     ELIMINATE VECTOR FROM MULTIPLE VECTOR PER LINE
!
            nwds = nwds - incr
            IF ( nwds>i ) THEN
               k1 = k1 + incr
               k2 = k1 + incr - 1
               jj = i - 1
               DO j = k1 , k2
                  jj = jj + 1
                  iout(jj) = iout(j)
               ENDDO
            ELSE
               device = 1
               IF ( nwds>0 ) EXIT
               IF ( eor ) GOTO 300
               nwds = nwdsav
               GOTO 500
            ENDIF
         ENDIF
      ENDDO
   ENDIF
   IF ( device>=4 ) THEN
      IF ( elemen ) THEN
!
!     SORT 2 ELEMENT PUNCH
!
         incr = id(10)
         DO jj = 1 , nwds , incr
            CALL ofppun(iout(jj),iout(jj),incr,iopt,idd,pnched)
         ENDDO
      ELSE
         CALL ofppun(iout(1),iout(1),nwds,iopt,idd,pnched)
      ENDIF
   ENDIF
   IF ( device/=1 .AND. device/=5 ) GOTO 500
 800  IF ( .NOT.pack .OR. axic ) GOTO 1000
   IF ( fluid .AND. sort==1 .AND. iout(1)>=500000 ) GOTO 1700
 900  IF ( iout(2)/=gse(1) ) THEN
!
!     SPECIAL ROUTINE TO PACK SCALAR OR EXTRA POINTS OUTPUT..
!     PACKING IS PERFORMED ONLY WHEN IDS ARE SEQUENTIAL,
!     AND THE TYPE REMAINS THE SAME.
!
      i = 1
      grdpt = iout(1)
      type = iout(2)
      DO
         IF ( i/=6 ) THEN
            DO
               from = 1740
               CALL read(*2200,*1600,file,isave(1),nwds,0,flag)
               igse = isave(2)
               IF ( pack ) isave(2) = gse(igse)
               IF ( sort/=2 ) THEN
                  itemp = isave(1)/10
                  device = isave(1) - 10*itemp
                  device = mod(device,8)
                  isave(1) = itemp
               ENDIF
               IF ( device>=4 ) CALL ofppun(isave(1),isave(1),nwds,iopt,idd,pnched)
               IF ( device==1 .OR. device==3 .OR. device==5 .OR. device==7 ) THEN
                  j = grdpt + i
                  IF ( fluid .AND. isave(1)>=500000 ) EXIT
                  IF ( isave(2)/=type .OR. isave(1)/=(grdpt+i) ) EXIT
!
!     PACK THIS VECTOR INTO LINE OF DATA
!     IF COMPLEX TWO LINES OF DATA
!     IMAGINARY PART WILL BE PACKED EVEN IF IT DOES NOT EXIST.
!
                  i = i + 1
                  iout(i+2) = isave(3)
                  iout(i+8) = isave(9)
                  GOTO 950
               ENDIF
            ENDDO
         ENDIF
         GOTO 1500
 950  ENDDO
   ENDIF
!
!     BUILD FORMAT CHECKING DATA FOR SPECIAL CASES.
!
 1000 i = 1
   IF ( heat .AND. elemen .AND. id(3)/=82 ) THEN
!
!     ELEMENT FORCES IN HEAT PROBLEMS
!
      IF ( line>maxn .OR. .NOT.headng ) CALL ofp1
      headng = .TRUE.
      IF ( sort==2 ) THEN
!
!     BRANCH ON SPECIAL HBDY FORCES
!
         IF ( nwds==5 ) THEN
            WRITE (l,99003) (out(kk),kk=1,5)
99003       FORMAT (18X,5(1P,E18.6))
         ELSEIF ( iout(5)==1 ) THEN
!RPKR THE FOLLOWING LINE HAS BEEN CHANGED
!1560 WRITE  (L,1570) (OUT(KK),KK= 1,4),OUT(7)
            WRITE (l,99004) out(1) , iout(2) , iout(3) , out(4) , out(7)
99004       FORMAT (1P,E14.6,4X,2A4,1P,E17.6,34X,1P,E17.6)
         ELSEIF ( iout(6)==1 ) THEN
!RPKR THE FOLLOWING LINE HAS BEEN CHANGED
!1540 WRITE  (L,1550) (OUT(KK),KK=1,5),OUT(7),OUT(8)
            WRITE (l,99005) out(1) , iout(2) , iout(3) , out(4) , out(5) , out(7) , out(8)
99005       FORMAT (1P,E14.6,4X,2A4,2(1P,E17.6),17X,2(1P,E17.6))
         ELSEIF ( iout(2)==hex1 .OR. iout(2)==hex2 ) THEN
!RPKR THE FOLLOWING LINE HAS BEEN CHANGED
!1600 WRITE  (L,1610) OUT(1),OUT(2),IOUT(3),(OUT(I),I=4,9)
            WRITE (l,99006) out(1) , iout(2) , iout(3) , (out(i),i=4,9)
99006       FORMAT (1P,E14.6,1X,A4,I7,6(1P,E17.6))
         ELSEIF ( iout(2)==hex3 ) THEN
!RPKR THE FOLLOWING LINE HAS BEEN CHANGED
!1620 WRITE  (L,1630) (OUT(I),I=1,9)
            WRITE (l,99007) out(1) , iout(2) , iout(3) , (out(i),i=4,9)
99007       FORMAT (1P,E14.6,2X,A4,2X,A4,6(1P,E17.6))
         ELSE
!RPKR THE FOLLOWING LINE HAS BEEN CHANGED
!RPKR WRITE  (L,1530) (OUT(I),I=1,9)
            WRITE (l,99008) out(1) , iout(2) , iout(3) , (out(i),i=4,9)
99008       FORMAT (1P,E14.6,4X,2A4,6(1P,E17.6))
         ENDIF
!
!     BRANCH ON SPECIAL HBDY OUTPUT
!
      ELSEIF ( nwds==5 ) THEN
!
         WRITE (l,99009) iout(1) , out(2) , out(3) , out(4) , out(5)
99009    FORMAT (18X,I18,4(1P,E18.6))
      ELSEIF ( iout(5)==1 ) THEN
!RPKR THE FOLLOWING LINE HAS BEEN CHANGED
!1440 WRITE  (L,1450) IOUT(1),OUT(2),OUT(3),OUT(4),OUT(7)
         WRITE (l,99010) iout(1) , iout(2) , iout(3) , out(4) , out(7)
99010    FORMAT (I14,4X,2A4,1P,E17.6,34X,1P,E17.6)
      ELSEIF ( iout(6)==1 ) THEN
!RPKR THE FOLLOWING LINE HAS BEEN CHANGED
!1420 WRITE  (L,1430) IOUT(1),OUT(2),OUT(3),OUT(4),OUT(5),OUT(7),OUT(8)
         WRITE (l,99011) iout(1) , iout(2) , iout(3) , out(4) , out(5) , out(7) , out(8)
99011    FORMAT (I14,4X,2A4,2(1P,E17.6),17X,2(1P,E17.6))
      ELSEIF ( iout(2)==hex1 .OR. iout(2)==hex2 ) THEN
!
         WRITE (l,99012) (iout(i),i=1,3) , (out(i),i=4,9)
99012    FORMAT (I14,1X,A4,I7,6(1P,E17.6))
      ELSEIF ( iout(2)==hex3 ) THEN
         WRITE (l,99013) (iout(i),i=1,3) , (out(i),i=4,9)
99013    FORMAT (I14,2X,A4,2X,A4,6(1P,E17.6))
      ELSE
!RPKR THE FOLLOWING LINE HAS BEEN CHANGED
!RPKR WRITE  (L,1410) IOUT(1),(OUT(I),I=2,9)
         WRITE (l,99014) (iout(i),i=1,3) , (out(i),i=4,9)
99014    FORMAT (I14,4X,2A4,6(1P,E17.6))
      ENDIF
   ELSEIF ( dummy ) THEN
!
!     DUMMY ELEMENT
!
      IF ( line>maxn .OR. .NOT.headng ) CALL odum(2,l,itype,iapp,0,id)
      headng = .TRUE.
      nwds = nwdsav
      CALL odum(3,l,itype,iapp,nwds,iout)
   ELSE
      fmt(1) = oparen
      ifmt = 1
      j = ix + 1
      GOTO 1200
   ENDIF
   GOTO 1400
 1100 j = j + 1
   ifmt = ifmt + 1
   fmt(ifmt) = comma
!
!     IF K IS NEGATIVE THEN BUILDING BLOCK IS NOT FOR A VARIABLE.
!     IN THIS CASE THEN K IS ACTUAL POINTER TO BE USED IN THE ESINGL ARR
!
 1200 k = D(j)
   IF ( k<0 ) THEN
      k = -k
      ifmt = ifmt + 1
      fmt(ifmt) = Esingl(k)
   ELSEIF ( k==0 ) THEN
!
!     OUTPUT THE LINE OR LINES OF DATA WITH THE NEW FORMAT
!
      fmt(ifmt) = cparen
      IF ( line>maxn .OR. .NOT.headng ) CALL ofp1
      headng = .TRUE.
      nwds = nwdsav
!
!     IF GRID-POINT-FORCE-BALANCE ENTRY, BLANK OUT NONEXISTENT (ZERO)
!     ELEMENT ID-S.
!
      IF ( gpfb ) THEN
         IF ( iout(2)==0 ) THEN
            iout(2) = iblank
            fmt(9) = a4
            fmt(10) = i9x
         ENDIF
!
!     ALSO, FOR GPFB, SET FORMAT TO SPACE TWO LINES ON NEW POINT-ID.
!
         IF ( iout(1)/=lastid ) THEN
            lastid = iout(1)
            fmt(2) = ih0
            line = line + 2
         ENDIF
      ENDIF
!hgs revixe call to use new ofppnt: gfortran doesn't like variable format
!     ofppnt now calls forwrt to perform write
      CALL ofppnt(iout,nwds,fmt)
      GOTO 1400
   ELSE
!
!     CHECK FOR  SPECIAL PACKING FORMATS
!
      k = 5*k - 5
      IF ( .NOT.axic ) THEN
         IF ( .NOT.(.NOT.pack .OR. iout(2)==gse(1)) ) THEN
            IF ( (i>=i1 .AND. i<=8) .OR. (i>=i2 .AND. i<=14) ) THEN
!
!     SPECIAL BLOCKS FOR PACKED OUTPUT
!
               ifmt = ifmt + 1
               fmt(ifmt) = a4
!
               i = i + 1
               GOTO 1100
            ENDIF
         ENDIF
      ELSEIF ( k==200 .OR. k==275 ) THEN
         IF ( iout(2)==iblank ) GOTO 1250
         GOTO 1300
      ENDIF
!
!     IF SOLSET AND K=0 OR K=80 OR K=365 OR K=75 USE I15BLK IF INTEGER 1
!
      IF ( solset ) THEN
         IF ( k==0 .OR. k==80 .OR. k==365 .OR. k==75 ) THEN
            IF ( iout(i)==1 ) THEN
               iout(i) = iblank
               ifmt = ifmt + 2
               fmt(ifmt-1) = i15blk(1)
               fmt(ifmt) = i15blk(2)
               IF ( axic ) fmt(ifmt) = i2x
               i = i + 1
               GOTO 1100
            ENDIF
         ENDIF
      ENDIF
!
!     CHECK FOR  0.0 ON AN E-FORMAT
!
      IF ( k<eend ) THEN
         IF ( out(i)==0 ) GOTO 1300
      ELSEIF ( k==440 ) THEN
         IF ( out(i)==0 ) GOTO 1300
!
!     CHECK FOR MID-EDGE OR CENTER STRESS POINTS ON ISOPARAMETRIC
!     SOLID ELEMENTS
!
      ELSEIF ( (k/=390 .AND. k/=395) .OR. iout(i)/=0 ) THEN
!
!     CHECK FOR SPECIAL INTEGER ON E9.1 FORMAT ONLY
!
         IF ( k==e9pt1 .AND. iout(i)==1 ) THEN
            iout(i) = iblank
            GOTO 1300
!
!     CHECK FOR SPECIAL GPST FORMATS
!
         ELSEIF ( iout(i)/=0 .OR. k<301 .OR. k>325 ) THEN
!
!     CHECK FOR HARMONIC NUMBER OR POINT ANGLE
!
            IF ( k==355 .OR. k==360 .OR. k==445 ) THEN
               IF ( iout(i)>0 .AND. iout(i)<1000 ) THEN
                  iout(i) = iout(i) - 1
                  GOTO 1300
               ENDIF
            ENDIF
!
!     CHECK FOR PHASE ANGLE ON STRESSES WITH TRAPAX AND TRIAAX ELEMENTS
!
!     COMMENTS FROM G.CHAN/UNISYS   1/93
!     FMT AND PHASE ARE LOCAL. I SEE NOBODY SETTING UP FMT() TO PHASE.
!     PHASE IS '1PE9'. IN ANSI FORTRAN STANDARD, A COMMA IS NEEDED
!     BETWEEN P AND E IF PHASE IS REALLY USED IN SETTING UP FMT().
!
            IF ( k==430 .AND. id(9)==3 .AND. iapp==freq .AND. fmt(ifmt-4)==phase ) GOTO 1300
         ELSE
            iout(i) = iblank
            GOTO 1300
         ENDIF
      ELSE
         IF ( k==395 ) THEN
            iout(i) = iblank
         ELSE
            iout(i) = center
         ENDIF
         GOTO 1300
      ENDIF
!
!     NO OTHER SPECIAL CHECKS AT THIS TIME
!
!     *** ADD FORMAT BLOCKS ***
!
!     STANDARD BLOCKS
!
 1250 ifmt = ifmt + 2
      fmt(ifmt-1) = E(k+1)
      fmt(ifmt) = E(k+2)
      i = i + 1
   ENDIF
   GOTO 1100
!
!     ALTERNATE BLOCKS
!
 1300 ifmt = ifmt + 3
   fmt(ifmt-2) = E(k+3)
   fmt(ifmt-1) = E(k+4)
   fmt(ifmt) = E(k+5)
   i = i + 1
   GOTO 1100
!
 1400 line = line + nlines
   IF ( eor ) GOTO 300
   IF ( axic ) GOTO 500
   IF ( .NOT.pack .OR. iout(2)==gse(1) .OR. i1==9 ) GOTO 500
!
!     TRANSFER THE SAVED BLOCK
!
   DO i = 1 , nwds
      iout(i) = isave(i)
   ENDDO
   IF ( .NOT.fluid ) GOTO 900
   IF ( iout(1)>=500000 ) GOTO 1700
   GOTO 900
!
!     PUT BLANKS IN ANY OPEN SLOTS
!
 1500 j = i + 3
   IF ( j<=8 ) THEN
      DO k = j , 8
         iout(k) = iblank
         iout(k+6) = iblank
      ENDDO
   ENDIF
!
   i1 = j
   i2 = j + 6
   GOTO 1000
!
 1600 eor = .TRUE.
   GOTO 1500
!
!     SPECIAL LOGIC FOR SORT-1 VECTOR OUTPUT IN A FLUID PROBLEM FOR
!     HARMONIC POINTS ONLY
!
 1700 oldhrm = -1
   l5 = 230
   line = maxn + 1
   k = 0
   eor = .FALSE.
!
!     DECODE THE HARMONIC
!
 1800 itemp = mod(iout(1),500000)
   nharm = (iout(1)-itemp)/500000
   iout(1) = itemp
   IF ( oldhrm==-1 ) oldhrm = nharm
   IF ( nharm/=oldhrm .OR. k>=5 ) GOTO 2100
   k = k + 1
 1900 real(2*k-1) = iout(1)
   real(2*k) = iout(3)
   imag(k) = iout(9)
   DO
      from = 1810
      CALL read(*2200,*2000,file,iout(1),nwds,0,flag)
!
!     PUNCH PROCESSING
!
      itemp = iout(1)/10
      device = iout(1) - 10*itemp
      iout(1) = itemp
      IF ( device<4 ) GOTO 1800
      device = mod(device,8)
      CALL ofppun(iout(1),iout(1),incr,iopt,idd,pnched)
      device = device - 4
      IF ( device>0 ) GOTO 1800
   ENDDO
!
!     OUTPUT THE LINE OF DATA
!
 2000 eor = .TRUE.
   IF ( k<=0 ) GOTO 300
!
!     BUILD THE FORMAT
!
 2100 fmt(1) = i1x
   IF ( nlines>1 ) fmt(1) = i1h0
   fmt(2) = i12
   fmt(3) = i2xx
   ifmt = 3
!
!     ADD STAR IF THIS IS AN UN-SYMETRIC HARMONIC
!
   IF ( mod(oldhrm,2)/=0 ) THEN
      fmt(3) = istar
      fmt(4) = i1xx
      ifmt = 4
   ENDIF
!
!     VARIABLES IN MAIN LINE
!
   DO i = 1 , k
      fmt(ifmt+1) = i8
      IF ( freal(2*i)/=0 ) THEN
         fmt(ifmt+2) = pe
         fmt(ifmt+3) = e156
         ifmt = ifmt + 3
      ELSE
         fmt(ifmt+2) = pf
         fmt(ifmt+3) = f156
         fmt(ifmt+4) = i9x
         ifmt = ifmt + 4
      ENDIF
   ENDDO
!
!     VARIABLES IN SECOND LINE IF COMPLEX
!
   IF ( nlines>1 ) THEN
      ifmt = ifmt + 1
      fmt(ifmt) = i15x
      DO i = 1 , k
         ifmt = ifmt + 1
         fmt(ifmt) = comma
         IF ( fimag(i)==0 ) THEN
            fmt(ifmt+1) = pf
            fmt(ifmt+2) = f236
            fmt(ifmt+3) = i9x
            ifmt = ifmt + 3
         ELSEIF ( l3==126 ) THEN
            fmt(ifmt+1) = pf
            fmt(ifmt+2) = f174
            fmt(ifmt+3) = i6x
            ifmt = ifmt + 3
         ELSE
            fmt(ifmt+1) = pe
            fmt(ifmt+2) = e236
            ifmt = ifmt + 2
         ENDIF
      ENDDO
   ENDIF
!
!     COMPLETE FORMAT
!
   fmt(ifmt+1) = cparen
   IF ( line>maxn ) CALL ofp1
   line = line + nlines
   k2 = 2*k
   iharm = (oldhrm-1)/2
   IF ( nlines<=1 ) THEN
!RPKR THE FOLLOWING LINE HAS BEEN REPLACED
!1950 WRITE (L,FMT) IHARM,(FREAL(I),I=1,K2)
      WRITE (l,fmt) iharm , (real(i),freal(i+1),i=1,k2,2)
   ELSE
!RPKR THE FOLLOWING LINE HAS BEEN REPLACED BY TWO LINES
!RPKR WRITE (L,FMT) IHARM,(FREAL(I),I=1,K2),(FIMAG(I),I=1,K)
      WRITE (l,fmt) iharm , (real(i),freal(i+1),i=1,k2,2) , (fimag(i),i=1,k)
   ENDIF
   k = 1
   oldhrm = nharm
   IF ( .NOT.(eor) ) GOTO 1900
   GOTO 300
 2200 WRITE (l,99015) Uwm , from
99015 FORMAT (A25,' 3030, OFP UNABLE TO PROCESS DATA BLOCK.  A TABLE ','PRINT OF THE DATA BLOCK FOLLOWS.   FROM =',I5,'/OFP')
   CALL close(file,1)
   CALL tabprt(file)
   GOTO 2400
!
!     CLOSE FILE UP
!
 2300 CALL close(file,1)
 2400 IF ( ifile/=6 ) GOTO 200
!
!     RESTORE TITLES TO WHATEVER THEY WERE AT ENTRY TO OFP
!
 2500 DO i = 1 , 96
      Head(i) = tsave(i)
   ENDDO
END SUBROUTINE ofp
