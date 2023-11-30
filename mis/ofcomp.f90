
SUBROUTINE ofcomp(File,Type,Eltyp,Iapp,Headng,Pnched,Form) !HIDESTARS (*,File,Type,Eltyp,Iapp,Headng,Pnched,Form)
   IMPLICIT NONE
   REAL Core(1)
   INTEGER Head(96) , Icard , Id(50) , Iflg , Isubs , Isubtl(32) , Itherm , Ititle(32) , Ksystm(100) , L1 , L2 , L3 , L4 , L5 ,     &
         & Label(32) , Line , Maxlns , Nout , Of(58) , Punch
   COMMON /blank / Icard
   COMMON /output/ Head
   COMMON /system/ Ksystm
   COMMON /zzzzzz/ Core
   INTEGER Eltyp , File , Form , Iapp , Type
   LOGICAL Headng , Pnched
   INTEGER andf
   INTEGER astr , blnk , buf(86) , ceig , device , elemid , failth , flag , freq , hill(2) , hoffmn(2) , ibuf(3) , icheck , idd ,   &
         & idevce , ieltyp , ifail(2) , ii , iply , ist(86) , itemp , j , jj , jout , k , left , length , maxflg , nlayer , nlines ,&
         & numwds , nwds , nword , static , strain(2) , stresf(2) , subst(3) , tsaiwu(2)
   REAL bufr(86) , failmx , rbuf(3) , rid(50) , rst(86) , time
   LOGICAL cmpxdt , force , heat , magpha , quad4 , sort1 , sort2 , stress , strn , tria3
   CHARACTER*5 q4 , t3 , t3q4
   EXTERNAL andf
!
!     OFP ROUTINE TO HANDLE PRINT AND PUNCH OF LAYERED COMPOSITE
!     ELEMENT STRESSES AND FORCES.  CURRENTLY, THIS INVOLVES ONLY
!     THE CQUAD4 AND CTRIA3 ELEMENTS.
!
!     FILE     = OUTPUT FILE UNDER PROCESSING
!     TYPE     = TYPE OF DATA-  REAL   , SORT 1       = 1
!                               COMPLEX, SORT 1       = 2
!                               REAL   , SORT 2       = 3
!                               COMPLEX, SORT 2       = 4
!     ELTYP    = ELEMENT TYPE-  QUAD4                 = 64
!                               TRIA3                 = 83
!     IAPP     = SOLUTION TYPE
!     HEADNG   = INDICATES PRINT HEADINGS ARE DONE FOR A PAGE
!     PNCHED   = INDICATES PUNCH HEADINGS ARE DONE
!     FORM     = DATA TYPE-     STRESSES              = 22
!                               FORCES                = 23
!                               STRAIN                = 21
!
!     INTEGER         REIG,TRANS,BK1,ELEC
!     REAL            HARMON,PANGLE,BUFF(1)
!     COMMON /ZZOFPX/ L1,L2,L3,L4,L5,ID(50),HARMON,PANGLE,BUFF(1)
   !>>>>EQUIVALENCE (ist(1),rst(1)) , (Id(1),rid(1)) , (buf(1),bufr(1)) , (ibuf(1),rbuf(1)) , (ifail(1),failmx) , (ifail(2),maxflg) ,    &
!>>>>    & (Ksystm(2),Nout) , (Ksystm(9),Maxlns) , (Ksystm(12),Line) , (Ksystm(33),Iflg) , (Ksystm(56),Itherm) , (Ksystm(69),Isubs) ,    &
!>>>>    & (Ksystm(91),Punch) , (Head(1),Ititle(1)) , (Head(65),Label(1)) , (Head(33),Isubtl(1)) , (L1,Of(1),Core(1)) , (L2,Of(2)) ,     &
!>>>>    & (L3,Of(3)) , (Id(1),Of(6)) , (L4,Of(4)) , (L5,Of(5))
!     EQUIVALENCE     (HARMON    ,OF (56)), (PANGLE   ,OF   (57)),
!    1                (BUFF(1)   ,OF (58))
!
   DATA static , freq , ceig/1 , 5 , 9/
!     DATA  REIG,TRANS,BK1,ELEC / 2 , 6 , 8 , 11 /
   DATA hill , hoffmn , tsaiwu , stresf/4H   H , 4HILL  , 4HHOFF , 4HMAN  , 4HTSAI , 4H-WU  , 4H STR , 4HESS /
   DATA strain/4H STR , 4HAIN /
   DATA blnk , astr/4H     , 4H  * /
   DATA subst/4HSUBS , 4HTRUC , 4HTURE/
   DATA t3q4 , t3 , q4/' ' , 'TRIA3' , 'QUAD4'/
!
!     INITIALIZE
!
   cmpxdt = Type==2 .OR. Type==4
   sort1 = Type<=2
   sort2 = Type>2
   heat = Itherm==1
   magpha = Id(9)==3 .AND. (Iapp==freq .OR. Iapp==ceig)
   quad4 = Eltyp==64
   tria3 = Eltyp==83
   stress = Form==22
   force = Form==23
   strn = Form==21
   IF ( heat .OR. sort2 .OR. cmpxdt ) GOTO 400
!
!     GET THE DEVICE CODE IF SORT=2,  1=PRINT  2=POST  4=PUNCH
!
   IF ( .NOT.(sort1) ) THEN
      idd = Id(5)/10
      device = Id(5) - 10*idd
      idevce = device
      Id(5) = idd
      elemid = idd
   ENDIF
!
!     GET THE NUMBER OF OUTPUT WORDS PER ELEMENT.
!
   nwds = Id(10)
   IF ( nwds==0 ) GOTO 400
   IF ( force ) GOTO 300
!
!     ********************
!     ******* READ *******
!     ********************
!
 100  CALL read(*600,*400,File,ist(1),3,0,flag)
   IF ( sort1 ) elemid = ist(1)
   IF ( sort2 ) time = rst(1)
   nlayer = ist(2)
   failth = ist(3)
   iply = 0
 200  iply = iply + 1
   IF ( iply>nlayer ) GOTO 100
!
 300  CALL read(*600,*500,File,ist(1),nwds,0,flag)
   IF ( stress .AND. iply==nlayer ) CALL read(*600,*600,File,ifail,2,0,flag)
   IF ( force ) elemid = ist(1)
!
!     GET THE DEVICE CODE IF SORT=1,   1=PRINT  2=POST  4=PUNCH
!
   IF ( .NOT.(sort2) ) THEN
      IF ( .NOT.(stress .AND. iply>1) ) THEN
         itemp = elemid/10
         device = elemid - 10*itemp
         idevce = device
         elemid = itemp
      ENDIF
   ENDIF
!
!     *********************
!     ******* PUNCH *******
!     *********************
!
   IF ( device>=4 ) THEN
!
!     TAKE OUT INDEX FAILURE FLAGS FOR STRESSES
!
      numwds = nwds
      IF ( stress ) numwds = numwds - 2
      DO ii = 1 , nwds
         buf(ii) = ist(ii)
      ENDDO
      IF ( .NOT.(force) ) THEN
         buf(6) = buf(7)
         buf(7) = buf(8)
         buf(8) = buf(9)
      ENDIF
!
      IF ( .NOT.(Pnched) ) THEN
!
!     PUNCH HEADINGS - TITLE, SUBTITLE, AND LABEL
!
         Icard = Icard + 1
         WRITE (Punch,99001) (Ititle(j),j=1,15) , Icard
99001    FORMAT (10H$TITLE   =,15A4,2X,I8)
         Icard = Icard + 1
         WRITE (Punch,99002) (Isubtl(j),j=1,15) , Icard
99002    FORMAT (10H$SUBTITLE=,15A4,2X,I8)
         Icard = Icard + 1
         WRITE (Punch,99003) (Label(j),j=1,15) , Icard
99003    FORMAT (10H$LABEL   =,15A4,2X,I8)
!
!     IF SUBSTRUCTURE (PHASE2) EXTRACTED ALSO SUBS-NAME AND COMPONENT
!
         IF ( Isubs/=0 ) THEN
            IF ( Isubtl(20)==subst(1) .AND. Isubtl(21)==subst(2) .AND. Isubtl(22)==subst(3) ) THEN
               Icard = Icard + 1
               WRITE (Punch,99037) (Isubtl(j),j=20,26) , Icard
               Icard = Icard + 1
               WRITE (Punch,99037) (Label(j),j=20,26) , Icard
            ENDIF
         ENDIF
!
         Icard = Icard + 1
         IF ( stress ) WRITE (Punch,99004) Icard
99004    FORMAT (17H$ELEMENT STRESSES,55X,I8)
         IF ( force ) WRITE (Punch,99005) Icard
99005    FORMAT (15H$ELEMENT FORCES,57X,I8)
!
!     REAL, REAL/IMAGINARY, MAGNITUDE/PHASE
!
         Icard = Icard + 1
         IF ( .NOT.(cmpxdt) ) THEN
            WRITE (Punch,99006) Icard
99006       FORMAT (12H$REAL OUTPUT,60X,I8)
         ELSEIF ( magpha ) THEN
            WRITE (Punch,99007) Icard
99007       FORMAT (23H$MAGNITUDE-PHASE OUTPUT,49X,I8)
         ELSE
            WRITE (Punch,99008) Icard
99008       FORMAT (22H$REAL-IMAGINARY OUTPUT,50X,I8)
         ENDIF
!
!     SUBCASE OR ELEMENT ID
!
         Icard = Icard + 1
         IF ( sort2 ) THEN
            WRITE (Punch,99009) elemid , Icard
99009       FORMAT (13H$ELEMENT ID =,I10,49X,I8)
         ELSE
            WRITE (Punch,99010) Id(4) , Icard
99010       FORMAT (13H$SUBCASE ID =,I12,47X,I8)
         ENDIF
!
!     PUNCH ELEMENT TYPE NUMBER,
!     IT IS SWITCHED TO MATCH THOSE OF POST PROCESSOR.
!
         Icard = Icard + 1
         ieltyp = Id(3)
         t3q4 = t3
         IF ( ieltyp==64 ) t3q4 = q4
         WRITE (Punch,99011) ieltyp , t3q4 , Icard
99011    FORMAT (15H$ELEMENT TYPE =,I12,4H   (,A5,1H),37X,I8)
!
!     EIGENVALUE, FREQUENCY, OR TIME
!
         IF ( Iapp==1 .OR. Iapp==3 .OR. Iapp==4 .OR. Iapp==7 .OR. Iapp==10 .OR. Iapp==11 ) THEN
         ELSEIF ( Iapp==5 ) THEN
!
!     FREQUENCY OR TIME
!
            IF ( .NOT.(sort2) ) THEN
               Icard = Icard + 1
               WRITE (Punch,99012) rid(5) , Icard
99012          FORMAT (12H$FREQUENCY =,E16.7,44X,I8)
            ENDIF
         ELSEIF ( Iapp==6 ) THEN
            IF ( .NOT.(sort2) ) THEN
               Icard = Icard + 1
               WRITE (Punch,99013) rid(5) , Icard
99013          FORMAT (7H$TIME =,E16.7,49X,I8)
            ENDIF
         ELSE
!
!     PUNCH EIGENVALUE
!
            Icard = Icard + 1
            IF ( sort1 .AND. cmpxdt ) THEN
               WRITE (Punch,99014) rid(6) , rid(7) , Id(5) , Icard
99014          FORMAT (15H$EIGENVALUE = (,E15.7,1H,,E15.7,8H) MODE =,I6,12X,I8)
            ELSE
               WRITE (Punch,99015) rid(6) , Id(5) , Icard
99015          FORMAT (13H$EIGENVALUE =,E15.7,2X,6HMODE =,I6,30X,I8)
            ENDIF
         ENDIF
!
         Pnched = .TRUE.
      ENDIF
!
!     PUNCH HEADINGS COMPLETE
!
      Icard = Icard + 1
!
!     ELEMENT STRESSES,  FIRST SUB-RECORD
!
      IF ( force ) THEN
!
!     ELEMENT FORCES,  FIRST SUB-RECORD
!
         IF ( sort2 .AND. Iapp/=static ) THEN
!
!     FIRST CARD BEGINS WITH A REAL
!
            WRITE (Punch,99016) bufr(1) , bufr(2) , bufr(3) , bufr(4) , Icard
99016       FORMAT (4(1P,E18.6),I8)
         ELSE
!
!     FIRST CARD BEGINS WITH AN INTEGER
!
            WRITE (Punch,99017) buf(1) , bufr(2) , bufr(3) , bufr(4) , Icard
99017       FORMAT (I10,8X,3(1P,E18.6),I8)
         ENDIF
         nword = 4
      ELSE
         IF ( iply>1 ) THEN
            WRITE (Punch,99018) buf(1) , bufr(2) , bufr(3) , Icard
99018       FORMAT (6H-CONT-,12X,I10,8X,2(1P,E18.6),I8)
!
         ELSEIF ( sort2 .AND. Iapp/=static ) THEN
!
!     FIRST CARD BEGINS WITH A REAL
!
            WRITE (Punch,99019) time , buf(1) , bufr(2) , bufr(3) , Icard
99019       FORMAT (1P,E18.6,I10,8X,2(1P,E18.6),I8)
         ELSE
!
!     FIRST CARD BEGINS WITH AN INTEGER
!
            WRITE (Punch,99020) elemid , buf(1) , bufr(2) , bufr(3) , Icard
99020       FORMAT (I10,8X,I10,8X,2(1P,E18.6),I8)
         ENDIF
         nword = 3
      ENDIF
!
      length = 8
      DO
!
!     SUBSEQUENT SUB-RECORDS
!
         left = numwds - nword
         IF ( left>0 ) THEN
!
!     PUNCH THE SUB-RECORDS
!
            DO WHILE ( nword<length )
               Icard = Icard + 1
               nword = nword + 3
               jout = 3
               IF ( nword>length ) THEN
                  nword = nword - 1
                  jout = 2
                  IF ( nword/=length ) THEN
                     nword = nword - 1
                     jout = 1
                  ENDIF
               ENDIF
!
               jj = nword - jout + 1
               DO ii = 1 , jout
                  ibuf(ii) = buf(jj)
                  jj = jj + 1
               ENDDO
               IF ( jout==2 ) THEN
!
!     2 WORDS OUT
!
                  IF ( iply<nlayer ) WRITE (Punch,99021) rbuf(1) , rbuf(2) , Icard
99021             FORMAT (6H-CONT-,12X,1P,E18.6,0P,F18.4,18X,I8)
                  IF ( iply==nlayer ) WRITE (Punch,99022) rbuf(1) , rbuf(2) , rbuf(3) , Icard
99022             FORMAT (6H-CONT-,12X,1P,E18.6,2(0P,F18.4),I8)
               ELSEIF ( jout==3 ) THEN
!
!     3 WORDS OUT
!
                  WRITE (Punch,99023) rbuf(1) , rbuf(2) , rbuf(3) , Icard
99023             FORMAT (6H-CONT-,12X,1P,E18.6,0P,F18.4,1P,E18.6,I8)
               ELSE
!
!     1 WORD OUT
!
                  WRITE (Punch,99024) rbuf(1) , Icard
99024             FORMAT (6H-CONT-,12X,1P,E18.6,36X,I8)
               ENDIF
               IF ( jout<3 ) EXIT
            ENDDO
         ELSE
!
!     END OF PUNCH, SEE IF PRINT IS REQUESTED
!
            IF ( sort1 ) idevce = device - 4
            EXIT
         ENDIF
      ENDDO
   ENDIF
   IF ( andf(idevce,1)/=0 ) THEN
!
!     *********************
!     ******* PRINT *******
!     *********************
!
!     WRITE TITLES IF HAVE NOT DONE SO YET
!
      icheck = 0
      IF ( .NOT.(Line<=Maxlns-2 .AND. Headng) ) THEN
         Iflg = 1
         CALL page1
         Headng = .TRUE.
         icheck = 1
      ENDIF
!
!     *** PRINT OF ELEMENT STRESSES ***
!
      IF ( force ) THEN
!
!     *** PRINT OF ELEMENT FORCES ***
!
!
!     BRANCH ON TYPE OF OUTPUT
!
         IF ( Type==2 ) THEN
         ELSEIF ( Type==3 ) THEN
         ELSEIF ( Type/=4 ) THEN
!
!     *** REAL, SORT 1 ***
!
            IF ( icheck/=0 ) THEN
               IF ( Iapp==1 .OR. Iapp==3 .OR. Iapp==4 .OR. Iapp==5 .OR. Iapp==7 .OR. Iapp==9 .OR. Iapp==10 .OR. Iapp==11 ) THEN
                  WRITE (Nout,99041)
               ELSEIF ( Iapp==6 ) THEN
                  WRITE (Nout,99039) rid(5)
               ELSEIF ( Iapp==8 ) THEN
                  WRITE (Nout,99040) rid(6)
               ELSE
!
                  WRITE (Nout,99038) Id(5) , rid(8) , rid(6)
               ENDIF
!
               IF ( quad4 ) THEN
                  WRITE (Nout,99025)
99025             FORMAT (22X,'F O R C E S   I N   L A Y E R E D   C O M P O S ','I T E   E L E M E N T S   ( Q U A D 4 )'/)
               ELSEIF ( tria3 ) THEN
                  WRITE (Nout,99026)
99026             FORMAT (22X,'F O R C E S   I N   L A Y E R E D   C O M P O S ','I T E   E L E M E N T S   ( T R I A 3 )'/)
               ENDIF
               WRITE (Nout,99027)
99027          FORMAT (6X,'ELEMENT',18X,'- MEMBRANE  FORCES -',22X,'- BENDING','   MOMENTS -',11X,'- TRANSVERSE SHEAR FORCES -')
               WRITE (Nout,99028)
99028          FORMAT (8X,'ID',16X,2HFX,12X,2HFY,12X,3HFXY,11X,2HMX,12X,2HMY,12X,3HMXY,11X,2HVX,12X,2HVY)
            ENDIF
!
!     WRITE THE DATA
!
            WRITE (Nout,99029) elemid , (rst(k),k=2,9)
99029       FORMAT (1H0,4X,I8,6X,8(1X,1P,E13.5))
            nlines = 2
!
!     *** COMPLEX, SORT 1 ***
!
!
!     *** REAL, SORT 2 ***
!
!
!     *** COMPLEX, SORT 2 ***
!
            GOTO 350
         ENDIF
!
!     BRANCH ON TYPE OF OUTPUT
!
      ELSEIF ( Type==2 ) THEN
      ELSEIF ( Type==3 ) THEN
      ELSEIF ( Type/=4 ) THEN
!
!     *** REAL, SORT 1 ***
!
         IF ( icheck/=0 ) THEN
            IF ( Iapp==1 .OR. Iapp==3 .OR. Iapp==4 .OR. Iapp==5 .OR. Iapp==7 .OR. Iapp==9 .OR. Iapp==10 .OR. Iapp==11 ) THEN
               WRITE (Nout,99041)
            ELSEIF ( Iapp==6 ) THEN
               WRITE (Nout,99039) rid(5)
            ELSEIF ( Iapp==8 ) THEN
               WRITE (Nout,99040) rid(6)
            ELSE
!
               WRITE (Nout,99038) Id(5) , rid(8) , rid(6)
            ENDIF
!
            IF ( quad4 ) THEN
               WRITE (Nout,99030)
99030          FORMAT (20X,'S T R E S S E S   I N   L A Y E R E D   ','C O M P O S I T E   E L E M E N T S   ( Q U A D 4 )')
            ELSEIF ( tria3 ) THEN
               WRITE (Nout,99031)
99031          FORMAT (20X,'S T R E S S E S   I N   L A Y E R E D   ','C O M P O S I T E   E L E M E N T S   ( T R I A 3 )')
            ENDIF
            WRITE (Nout,99032)
99032       FORMAT ('0 ELEMENT',3X,'PLY *STRESSES IN FIBER AND MATRIX',' DIRECTIONS*  *DIRECT FIBER *  *INTER-LAMINAR STRESS',      &
                   &'ES*  * SHEAR BOND  *   *MAXIMUM*')
            WRITE (Nout,99033)
99033       FORMAT (4X,'ID',6X,'ID  *  NORMAL-1',6X,'NORMAL-2',6X,'SHEAR-12 *  *FAILURE INDEX*  *SHEAR-1Z',6X,'SHEAR-2Z*',          &
                   &'  *FAILURE INDEX*   * INDEX *',/)
         ENDIF
!
!     WRITE THE DATA
!     BUT FIRST, MODIFY THE FAILURE INDEX FLAGS FROM INTEGER TO BCD
!
         IF ( ist(6)==0 ) ist(6) = blnk
         IF ( ist(6)==1 ) ist(6) = astr
         IF ( ist(10)==0 ) ist(10) = blnk
         IF ( ist(10)==1 ) ist(10) = astr
!
         IF ( iply>1 ) THEN
!
            WRITE (Nout,99034) ist(1) , (rst(k),k=2,5) , ist(6) , (rst(k),k=7,9) , ist(10)
99034       FORMAT (11X,I4,3(1P,E14.5),2X,0P,F10.3,A4,2(1P,E14.5),0P,F10.3,A4)
            nlines = 1
            IF ( iply>=nlayer ) THEN
!
!     IF THE LAST LAYER, CHECK THE MAXIMUM FAILURE INDEX
!
               nlines = 2
               IF ( maxflg==0 ) maxflg = blnk
               IF ( maxflg==1 ) maxflg = astr
               IF ( failth/=0 ) THEN
                  IF ( failth==1 ) THEN
                     WRITE (Nout,99042) hill(1) , hill(2) , failmx , maxflg
                  ELSEIF ( failth==2 ) THEN
                     WRITE (Nout,99042) hoffmn(1) , hoffmn(2) , failmx , maxflg
                  ELSEIF ( failth==3 ) THEN
                     WRITE (Nout,99042) tsaiwu(1) , tsaiwu(2) , failmx , maxflg
                  ELSEIF ( failth==4 ) THEN
                     WRITE (Nout,99042) stresf(1) , stresf(2) , failmx , maxflg
                  ELSEIF ( failth==5 ) THEN
                     WRITE (Nout,99042) strain(1) , strain(2) , failmx , maxflg
                  ELSE
                     GOTO 305
                  ENDIF
                  GOTO 350
               ENDIF
 305           failmx = 0.0
               WRITE (Nout,99035) failmx
99035          FORMAT (1H ,116X,0P,F10.3)
            ENDIF
         ELSE
            WRITE (Nout,99036) elemid , ist(1) , (rst(k),k=2,5) , ist(6) , (rst(k),k=7,9) , ist(10)
99036       FORMAT (1H0,I8,2X,I4,3(1P,E14.5),2X,0P,F10.3,A4,2(1P,E14.5),0P,F10.3,A4)
            nlines = 3
         ENDIF
         GOTO 350
      ENDIF
      GOTO 400
!
!     DONE WITH ONE ENTRY, GO BACK AND READ ANOTHER ONE.
!
 350  Line = Line + nlines
      IF ( .NOT.(stress) ) GOTO 300
      GOTO 200
   ELSE
      IF ( .NOT.(stress) ) GOTO 300
      GOTO 200
   ENDIF
!
 400  RETURN
!
 500  IF ( force ) RETURN
 600  RETURN 1
99037 FORMAT (1H$,7A4,43X,I8)
99038 FORMAT (6X,'MODE NUMBER = ',I4,26X,'FREQUENCY = ',1P,E13.6,26X,'EIGENVALUE = ',1P,E13.6)
99039 FORMAT (6X,6HTIME =,1P,E14.6)
99040 FORMAT (6X,12HEIGENVALUE =,1P,E14.6)
99041 FORMAT (1H )
!
!     *** COMPLEX, SORT 1 ***
!
!
!     *** REAL, SORT 2 ***
!
!
!     *** COMPLEX, SORT 2 ***
!
99042 FORMAT (1H ,41X,2A4,'FAILURE THEORY WAS USED FOR THIS ELEMENT.',26X,0P,F10.3,A4)
!
!
END SUBROUTINE ofcomp