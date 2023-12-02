!*==emgpro.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE emgpro(Ibuf)
   USE c_blank
   USE c_emgdic
   USE c_emgest
   USE c_emgfil
   USE c_emgprm
   USE c_gpta1
   USE c_iemg1b
   USE c_iemgod
   USE c_iemgot
   USE c_matout
   USE c_sma1cl
   USE c_sma2cl
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(7) :: Ibuf
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: dosi , subr
   INTEGER , SAVE :: eor , noeor , scr3 , scr4
   INTEGER :: estwds , i , i1 , i2 , iadd , iaddd , ibfind , ibuf1 , ibuf2 , ibuf3 , icg , icrq , idprim , ifile , igoto , imatch , &
            & indcng , index , index1 , ipr , iprec , iprime , iqdmm1 , iqdmm2 , isave1 , isave2 , isil , iwords , izero , j ,      &
            & jfile , jjcore , jltype , kht , l , l1 , l2 , lnum , local , ltypes , m , n , nnwrds , nscal1 , nscal2 , nsils ,      &
            & nwords , outpt , ret , savjcr , savncr , sysbuf
   REAL , DIMENSION(12) :: estx
   INTEGER , DIMENSION(32) :: ipos , sil
   INTEGER , DIMENSION(1) :: iz
   REAL :: thk
   REAL , DIMENSION(2) , SAVE :: trim6 , trpl1 , trshl
   EXTERNAL axif2d , axif2s , axif3d , axif3s , axif4d , axif4s , bard , bars , bisloc , close , coned , cones , conm1d , conm1s ,  &
          & conm2d , conm2s , delset , elbowd , elbows , filpos , flmasd , flmass , flud2d , flud2s , flud3d , flud3s , flud4d ,    &
          & flud4s , ftube , fwdrec , gopen , hbdyd , hbdys , hexa1d , hexa1s , hexa2d , hexa2s , ihexd , ihexs , is2d8d , is2d8s , &
          & kdum1 , kdum2 , kdum3 , kdum4 , kdum5 , kdum6 , kdum7 , kdum8 , kdum9 , ktrm6d , ktrm6s , ktrpld , ktrpls , ktshld ,    &
          & ktshls , mesage , open , page2 , plotld , plotls , qdmemd , qdmems , qdmm1d , qdmm1s , qdmm2d , qdmm2s , qdpltd ,       &
          & qdplts , quad1d , quad1s , quad2d , quad2s , quad4d , quad4s , read , rodd , rods , savpos , scaled , sheard , shears , &
          & skprec , slot3d , slot3s , slot4d , slot4s , tetrad , tetras , tordrd , tordrs , trapad , trapax , traprd , traprs ,    &
          & trbscd , trbscs , tria1d , tria1s , tria2d , tria2s , tria3d , tria3s , triaad , triaax , triard , triars , trmemd ,    &
          & trmems , trpltd , trplts , tubed , tubes , twistd , twists , viscd , viscs , wedged , wedges , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE OF THE -EMG- MODULE IS THE MAIN PROCESSOR.  IT WILL
!     PASS THE -EST- DATA BLOCK ONCE, ELEMENT TYPE BY ELEMENT TYPE.
!
!     ELEMENT TYPES CONTRIBUTING TO STIFFNESS, MASS, OR DAMPING MATRICES
!     WILL BE PROCESSED.
!
   !>>>>EQUIVALENCE (Ksystm(2),Outpt) , (Ksystm(1),Sysbuf) , (Ksystm(55),Iprec) , (Estbuf(1),Estx(1)) , (Iz(1),Z(1))
   DATA trim6 , trpl1 , trshl/4HCTRI , 4HM6   , 4HCTRP , 4HLT1  , 4HCTRS , 4HHL  /
   DATA scr3 , scr4/303 , 304/
   DATA eor , noeor/1 , 0/ , subr/4HEMGP , 4HRO  /
   DATA dosi/4HDOUB , 4HSING/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         iqdmm1 = 0
         iqdmm2 = 0
         nval(1) = 0
         nval(2) = 0
         nval(3) = 0
         ltypes = 0
         ktypes = 0
         dummy = 0.0D0
         icall = 0
         ilast = 0
!
!     INITIALIZE /SMA1CL/ AND /SMA2CL/
!
         knogo = 0
         mnogo = 0
         kdummy(10) = 10
         mdummy(10) = 10
!
!     FOLLOWING CALL PREPS /GPTA1/ FOR DUMMY ELEMENTS
!
         CALL delset
!
!     DEFINE WORKING CORE BLOCK FOR RESET PURPOSES.
!
         ipr = precis
         IF ( ipr/=1 ) ipr = 0
         savjcr = jcore
         savncr = ncore
         estid = 0
         lnum = lcong/2
         spag_nextblock_1 = 2
      CASE (2)
!
!     READ THE ELEMENT TYPE FROM THE EST.
!
         CALL read(*100,*160,est,eltype,1,noeor,iwords)
         izero = incr*(eltype-1)
!
!     CHECK FOR ALLOWABLE ELEMENT TYPES
!
         IF ( eltype/=2 .AND. eltype/=32 .AND. eltype/=33 .AND. eltype/=68 .AND. eltype/=69 .AND. eltype/=72 ) THEN
            IF ( eltype>=1 .AND. eltype<=nelem ) THEN
!
!     RESTORE CORE POINTERS
!
               jcore = savjcr
               ncore = savncr
!
!     CLEAR ESTBUF
!
               DO i = 1 , 200
                  estbuf(i) = 0
               ENDDO
!
!     SET VARIOUS PARAMETERS = FUNCTION OF THIS ELEMENT TYPE
!
!     TURN ON COUPLED MASS FLAG IF EITHER OF ALL-COUPLED-MASS-FLAG
!     OR SPECIFIC-TYPE-COUPLED-MASS-FLAG IS ON.
!
               IF ( flags(2)==0 ) THEN
                  icmbar = -1
               ELSEIF ( nocmas<0 ) THEN
                  icmbar = -1
               ELSEIF ( nocmas==0 ) THEN
                  IF ( eltype==34 ) THEN
                     IF ( ncpbar<=0 ) THEN
                        icmbar = -1
                     ELSE
!
                        icmbar = 1
                     ENDIF
                  ELSEIF ( eltype==1 ) THEN
                     IF ( ncprod<=0 ) THEN
                        icmbar = -1
                     ELSE
                        icmbar = 1
                     ENDIF
                  ELSEIF ( eltype==19 ) THEN
                     IF ( ncpqd1<=0 ) THEN
                        icmbar = -1
                     ELSE
                        icmbar = 1
                     ENDIF
                  ELSEIF ( eltype==18 ) THEN
                     IF ( ncpqd2<=0 ) THEN
                        icmbar = -1
                     ELSE
                        icmbar = 1
                     ENDIF
                  ELSEIF ( eltype==6 ) THEN
                     IF ( ncptr1<=0 ) THEN
                        icmbar = -1
                     ELSE
                        icmbar = 1
                     ENDIF
                  ELSEIF ( eltype==17 ) THEN
                     IF ( ncptr2<=0 ) THEN
                        icmbar = -1
                     ELSE
                        icmbar = 1
                     ENDIF
                  ELSEIF ( eltype==3 ) THEN
                     IF ( ncptub<=0 ) THEN
                        icmbar = -1
                     ELSE
                        icmbar = 1
                     ENDIF
                  ELSEIF ( eltype==15 ) THEN
                     IF ( ncpqdp<=0 ) THEN
                        icmbar = -1
                     ELSE
                        icmbar = 1
                     ENDIF
                  ELSEIF ( eltype==8 ) THEN
                     IF ( ncptrp<=0 ) THEN
                        icmbar = -1
                     ELSE
                        icmbar = 1
                     ENDIF
                  ELSEIF ( eltype==7 ) THEN
                     IF ( ncptrb<=0 ) THEN
                        icmbar = -1
                     ELSE
                        icmbar = 1
                     ENDIF
                  ELSE
                     icmbar = -1
                  ENDIF
               ELSE
                  icmbar = 1
               ENDIF
!
               jltype = 2*eltype - ipr
               estwds = elem(izero+12)
               nsils = elem(izero+10)
               isil = elem(izero+13)
               IF ( elem(izero+9)/=0 ) isil = isil - 1
               i1 = isil
               i2 = isil + nsils - 1
               isave2 = 0
               IF ( estwds<=200 ) THEN
!
!     CHECK TO SEE IF ILLEGAL ELEMENTS ARE USED IN -HEAT- FORMULATION
!
                  IF ( heat ) THEN
                     IF ( eltype/=1 .AND. eltype/=3 .AND. eltype/=6 ) THEN
                        IF ( eltype<9 .OR. eltype>14 ) THEN
                           IF ( eltype<16 .OR. eltype>24 ) THEN
                              IF ( eltype/=34 .AND. eltype/=36 .AND. eltype/=37 ) THEN
                                 IF ( eltype<39 .OR. eltype>42 ) THEN
                                    IF ( eltype/=52 .AND. eltype/=62 .AND. eltype/=63 ) THEN
                                       IF ( eltype<64 .OR. eltype>67 ) THEN
                                         IF ( eltype/=80 .AND. eltype/=81 .AND. eltype/=83 ) THEN
!
                                         WRITE (outpt,99001) ufm , elem(izero+1) , elem(izero+2) , eltype
99001                                    FORMAT (A23,' 3115, EMGPRO FINDS ',2A4,' ELEMENTS (ELEMENT TYPE ',I3,                      &
                                           &') PRESENT IN A HEAT FORMULATION.')
                                         spag_nextblock_1 = 4
                                         CYCLE SPAG_DispatchLoop_1
                                         ENDIF
                                       ENDIF
                                    ENDIF
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
!
!     SET UP VARIABLES TO BE WRITTEN AS DICTIONARY 3-WORD HEADER
!
                  nlocs = nsils
                  ldict = nlocs + 5
                  spag_nextblock_1 = 5
               ELSE
                  WRITE (outpt,99002) sfm , eltype
99002             FORMAT (A25,' 3106, EMGPRO FINDS THAT ELEMENT TYPE ',I3,' HAS EST ENTRIES TOO LARGE TO HANDLE CURRENTLY.')
                  spag_nextblock_1 = 4
               ENDIF
               CYCLE
            ENDIF
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         WRITE (outpt,99003) sfm , elem(izero+1) , elem(izero+2) , eltype
99003    FORMAT (A25,' 3105, EMGPRO FINDS ',2A4,' ELEMENTS (ELEM. TYPE ',I3,') UNDEFINED IN EST DATA BLOCK AND/OR ELEMENT ROUTINE.')
         spag_nextblock_1 = 4
      CASE (4)
         CALL fwdrec(*120,est)
         error = .TRUE.
         spag_nextblock_1 = 2
      CASE (5)
!
!     READ AN ELEMENT EST ENTRY
!
         CALL read(*120,*80,est,estbuf,estwds,noeor,iwords)
         elid = estbuf(1)
         estid = estid + 1
!
!     CHECK TO SEE IF THIS ELEMENT IS CONGRUENT TO ANOTHER ALREADY
!     POSSESSING A DICTIONARY IN CORE.
!
         IF ( .NOT.anycon ) GOTO 20
         CALL bisloc(*20,elid,z(icong),2,lnum,j)
!
!     MATCH FOUND.  CHECK FOR DICTIONARY-TABLE ON PRIMARY.
!
         iprime = z(icong+j)
         idprim = z(icong+j-1)
         SPAG_Loop_1_1: DO
            IF ( iprime<0 ) THEN
!
!     IPRIME IS NEGATIVE TABLE ADDRESS IMPLYING DICTIONARY EXISTS.
!
               IF ( error ) GOTO 20
               iprime = -iprime
               imatch = 0
               ibfind = 1
               j = 0
               EXIT SPAG_Loop_1_1
            ELSEIF ( iprime==0 ) THEN
!
!     SET UP ELEMENT MATRIX MAPPING ARRAY FOR LATER USE BY OTHER
!     ELEMENTS IN THIS CONGRUENT SET
!
               icg = jcore
               jjcore = jcore + 2*nsils + 5
               icrq = jjcore - ncore
               IF ( jjcore>=ncore ) THEN
                  WRITE (outpt,99004) uim , idprim
!
99004             FORMAT (A29,' 2382, ELEMENT MATRICES FOR ELEMENTS CONGRUENT TO ','ELEMENT ID =',I10,/5X,                          &
                         &'WILL BE RE-COMPUTED AS THERE IS',' INSUFFICIENT CORE AT THIS TIME TO HOLD CONGRUENCY ','MAPPING DATA.')
                  WRITE (outpt,99012) icrq
                  CALL page2(4)
                  GOTO 20
               ELSE
                  jcore = jjcore
                  iz(icg) = idprim
                  iz(icg+1) = nsils
                  iz(icg+2) = 0
                  iz(icg+3) = 0
                  iz(icg+4) = 0
                  igoto = 0
                  spag_nextblock_1 = 17
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSE
!
!     IPRIME POINTS TO PRIMARY ID
!
               idprim = z(iprime)
               iprime = z(iprime+1)
            ENDIF
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 6
      CASE (6)
         j = j + 1
         iadd = z(iprime+j)
         IF ( iadd<=0 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     COPY DICTIONARY FROM CORE TO DICTIONARY FILE.
!
         z(iadd) = estid
         flags(j) = flags(j) + 1
         CALL write(dictn(j),z(iadd),5,noeor)
         iaddd = iadd + 5
         IF ( imatch==1 ) THEN
            CALL write(dictn(j),z(iaddd),nsils,noeor)
            spag_nextblock_1 = 8
         ELSE
            IF ( imatch==2 ) THEN
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            indcng = savjcr
            igoto = 1
            DO WHILE ( iz(indcng)/=idprim )
               jjcore = indcng + 2*iz(indcng+1) + 5
               IF ( jjcore>=ncore ) THEN
                  WRITE (outpt,99005) swm , estid
99005             FORMAT (A27,' 2383, UNABLE TO LOCATE CONGRUENCY MAPPING DATA FOR',' ELEMENT ID =',I10,1H.,/5X,                    &
                         &'ELEMENT MATRICES FOR THIS ','ELEMENT WILL, THEREFORE, BE RE-COMPUTED.')
                  CALL page2(4)
                  GOTO 20
               ELSE
                  indcng = jjcore
               ENDIF
            ENDDO
            spag_nextblock_1 = 17
         ENDIF
      CASE (7)
         imatch = 2
         spag_nextblock_1 = 18
      CASE (8)
         ibfind = ibfind + 2
         IF ( j<3 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
!
!     BRANCH ON ELEMENT TYPE.  INDIVIDUAL ROUTINES WILL COMPUTE AND
!     OUTPUT ALL MATRIX TYPES DESIRED BASED ON FLAGS AVAILABLE TO THEM.
!
 20      IF ( eltype/=ltypes ) THEN
            ltypes = eltype
            IF ( ltypes>nelem ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL page2(3)
            WRITE (outpt,99006) uim , dosi(ipr+1) , elem(izero+1) , elem(izero+2) , eltype , elid
99006       FORMAT (A29,' 3113,',/5X,'EMG MODULE PROCESSING ',A4,'LE PRECISION ',2A4,' ELEMENTS (ELEMENT TYPE ',I3,                 &
                   &') STARTING WITH ID ',I8)
            IF ( eltype>=84 .AND. eltype<=86 ) WRITE (outpt,99007)
99007       FORMAT (5X,'(STEPPING THRU ONLY. NO REAL COMPUTATION HERE FOR ','THIS DIFFERENTIAL STIFFNESS ELEMENT)')
         ENDIF
         IF ( l38==1 ) THEN
            CALL page2(1)
            WRITE (outpt,99008) elid
99008       FORMAT (5X,'ELEMENT ',I8,' IS BEING PROCESSED')
         ENDIF
         local = jltype - 100
         IF ( local<=0 ) THEN
!
!     PAIRED -GO TO- ENTRIES PER ELEMENT SINGLE/DOUBLE PRECISION
!
!             1 CROD      2 C.....    3 CTUBE     4 CSHEAR    5 CTWIST
!
!             6 CTRIA1    7 CTRBSC    8 CTRPLT    9 CTRMEM   10 CONROD
!
!            11 ELAS1    12 ELAS2    13 ELAS3    14 ELAS4    15 CQDPLT
!
!            16 CQDMEM   17 CTRIA2   18 CQUAD2   19 CQUAD1   20 CDAMP1
!
!            21 CDAMP2   22 CDAMP3   23 CDAMP4   24 CVISC    25 CMASS1
!
!            26 CMASS2   27 CMASS3   28 CMASS4   29 CONM1    30 CONM2
!
!            31 PLOTEL   32 C.....   33 C.....   34 CBAR     35 CCONEAX
!
!            36 CTRIARG  37 CTRAPRG  38 CTORDRG  39 CTETRA   40 CWEDGE
!
!            41 CHEXA1   42 CHEXA2   43 CFLUID2  44 CFLUID3  45 CFLUID4
!
!            46 CFLMASS  47 CAXIF2   48 CAXIF3   49 CAXIF4   50 CSLOT3
!
            IF ( jltype==1 .OR. jltype==19 ) THEN
!
!     ==================================================================
!     A WALKING TOUR OF EMG TO COMPUTE STIFFNESS (K-) AMD MASS (M-)
!     MATRICES FOR AN 'OLD' ELEMENT SUCH AS CTRIA2.
!     SEE HOW EASY IT IS.      G.CHAN/UNISYS, 7/87
!
!       EMG SUPPORTING ROUTINES -
!       EMGTAB,EMGCNG,EMGCOR,EMGFIN,
!       EMGSOC (WHICH COMPUTES OFFSET BETWWEN /ZZEMGX/ AND /ZZEMII/ AND
!           /   SETS ICORE,JCORE,NCORE IN /EMGPRM/ FOR OPEN CORE USAGE)
!          /
!         /                                --->EMG1B---->EMGOUT
!        /                                /   OUTPUT PIVOT ROW PARTITION
!     EMG---->EMGPRO---->CTRIA2          /    AFTER KTRIQD IS DONE, AND
!               /      AN ENTRY POINT   /     ALSO AFTER MTRIQD
!            /ZZEMGX/     IN           /                             (*)
!                        OLDEL3--->EMGOLD--->KTRIQD--->KTRMEM--->KTRPLT
!                                    /                              /
!                                    ------->MTRIQD             /ZZEM14/
!            (*)                                 (&)          UNIT 14 IS
!             KTRPLT---------------->KTRBSC                   ALLOCATED
!           TO COMPUTE BENDING     TO COMPUTE MEMBRANE        TO CTRIA2
!           FOR CTRIA2             FOR CTRIA2                 BY TA1ABD
!                  /                      /
!                 ------->SMA1B<----------
!                           \
!                            ---->EMG1B---->EMGOUT
!                                          OUTPUT A K-MATRIX
!           (&)                            PARTITION
!             MTRIQD BRANCH, FOR M-MATRIX
!             FOR CTRIA2 ELEMENT, IS SIMILARLY
!             STRUCTURED AS THAT OF THE KTRIQD BRANCH
!
!             REPEAT DAMPING B-MATRIX IF NECESSARY
!             IF ELEMENT HAS HEAT CAPBABILITY - WHAT DO I DO NOW?
!
!     THIS SYMBOL '>' IS RIGHT ARROW HEAD, AND '<' IS LEFT ARROW HEAD
!     ==================================================================
!
               CALL rods
            ELSEIF ( jltype==2 .OR. jltype==20 ) THEN
               CALL rodd
            ELSEIF ( jltype==3 .OR. jltype==4 .OR. jltype==63 .OR. jltype==64 .OR. jltype==65 .OR. jltype==66 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( jltype==5 ) THEN
               CALL tubes
            ELSEIF ( jltype==6 ) THEN
               CALL tubed
            ELSEIF ( jltype==7 ) THEN
               CALL shears
            ELSEIF ( jltype==8 ) THEN
               CALL sheard
            ELSEIF ( jltype==9 ) THEN
               CALL twists
            ELSEIF ( jltype==10 ) THEN
               CALL twistd
            ELSEIF ( jltype==11 ) THEN
               CALL tria1s
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( jltype==12 ) THEN
               CALL tria1d
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( jltype==13 ) THEN
               CALL trbscs
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( jltype==14 ) THEN
               CALL trbscd
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( jltype==15 ) THEN
               CALL trplts
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( jltype==16 ) THEN
               CALL trpltd
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( jltype==17 ) THEN
               CALL trmems
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( jltype==18 ) THEN
               CALL trmemd
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( jltype==21 .OR. jltype==22 ) THEN
               nscal1 = 1
               nscal2 = 1
               CALL scaled(nscal1,nscal2)
            ELSEIF ( jltype==23 .OR. jltype==24 ) THEN
               nscal1 = 2
               nscal2 = 1
               CALL scaled(nscal1,nscal2)
            ELSEIF ( jltype==25 .OR. jltype==26 ) THEN
               nscal1 = 3
               nscal2 = 1
               CALL scaled(nscal1,nscal2)
            ELSEIF ( jltype==27 .OR. jltype==28 ) THEN
               nscal1 = 4
               nscal2 = 1
               CALL scaled(nscal1,nscal2)
            ELSEIF ( jltype==29 ) THEN
               CALL qdplts
               spag_nextblock_1 = 13
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( jltype==30 ) THEN
               CALL qdpltd
               spag_nextblock_1 = 13
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( jltype==31 ) THEN
               GOTO 40
            ELSEIF ( jltype==32 ) THEN
               GOTO 60
            ELSEIF ( jltype==33 ) THEN
               CALL tria2s
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( jltype==34 ) THEN
               CALL tria2d
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( jltype==35 ) THEN
               CALL quad2s
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( jltype==36 ) THEN
               CALL quad2d
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( jltype==37 ) THEN
               CALL quad1s
               spag_nextblock_1 = 14
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( jltype==38 ) THEN
               CALL quad1d
               spag_nextblock_1 = 14
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( jltype==39 .OR. jltype==40 ) THEN
               nscal1 = 1
               nscal2 = 3
               CALL scaled(nscal1,nscal2)
            ELSEIF ( jltype==41 .OR. jltype==42 ) THEN
               nscal1 = 2
               nscal2 = 3
               CALL scaled(nscal1,nscal2)
            ELSEIF ( jltype==43 .OR. jltype==44 ) THEN
               nscal1 = 3
               nscal2 = 3
               CALL scaled(nscal1,nscal2)
            ELSEIF ( jltype==45 .OR. jltype==46 ) THEN
               nscal1 = 4
               nscal2 = 3
               CALL scaled(nscal1,nscal2)
            ELSEIF ( jltype==47 ) THEN
               CALL viscs
               IF ( flags(3)==0 ) WRITE (outpt,99011) uwm
            ELSEIF ( jltype==48 ) THEN
               CALL viscd
               IF ( flags(3)==0 ) WRITE (outpt,99011) uwm
            ELSEIF ( jltype==49 .OR. jltype==50 ) THEN
               nscal1 = 1
               nscal2 = 2
               CALL scaled(nscal1,nscal2)
            ELSEIF ( jltype==51 .OR. jltype==52 ) THEN
               nscal1 = 2
               nscal2 = 2
               CALL scaled(nscal1,nscal2)
            ELSEIF ( jltype==53 .OR. jltype==54 ) THEN
               nscal1 = 3
               nscal2 = 2
               CALL scaled(nscal1,nscal2)
            ELSEIF ( jltype==55 .OR. jltype==56 ) THEN
               nscal1 = 4
               nscal2 = 2
               CALL scaled(nscal1,nscal2)
            ELSEIF ( jltype==57 ) THEN
               CALL conm1s
            ELSEIF ( jltype==58 ) THEN
               CALL conm1d
            ELSEIF ( jltype==59 ) THEN
               CALL conm2s
            ELSEIF ( jltype==60 ) THEN
               CALL conm2d
            ELSEIF ( jltype==61 ) THEN
               CALL plotls
            ELSEIF ( jltype==62 ) THEN
               CALL plotld
            ELSEIF ( jltype==67 ) THEN
               CALL bars
            ELSEIF ( jltype==68 ) THEN
               CALL bard
            ELSEIF ( jltype==69 ) THEN
               CALL cones
            ELSEIF ( jltype==70 ) THEN
               CALL coned
            ELSEIF ( jltype==71 ) THEN
               CALL triars
            ELSEIF ( jltype==72 ) THEN
               CALL triard
            ELSEIF ( jltype==73 ) THEN
               CALL traprs
            ELSEIF ( jltype==74 ) THEN
               CALL traprd
            ELSEIF ( jltype==75 ) THEN
               CALL tordrs
            ELSEIF ( jltype==76 ) THEN
               CALL tordrd
            ELSEIF ( jltype==77 ) THEN
               CALL tetras
            ELSEIF ( jltype==78 ) THEN
               CALL tetrad
            ELSEIF ( jltype==79 ) THEN
               CALL wedges
            ELSEIF ( jltype==80 ) THEN
               CALL wedged
            ELSEIF ( jltype==81 ) THEN
               CALL hexa1s
            ELSEIF ( jltype==82 ) THEN
               CALL hexa1d
            ELSEIF ( jltype==83 ) THEN
               CALL hexa2s
            ELSEIF ( jltype==84 ) THEN
               CALL hexa2d
            ELSEIF ( jltype==85 ) THEN
               CALL flud2s
            ELSEIF ( jltype==86 ) THEN
               CALL flud2d
            ELSEIF ( jltype==87 ) THEN
               CALL flud3s
            ELSEIF ( jltype==88 ) THEN
               CALL flud3d
            ELSEIF ( jltype==89 ) THEN
               CALL flud4s
            ELSEIF ( jltype==90 ) THEN
               CALL flud4d
            ELSEIF ( jltype==91 ) THEN
               CALL flmass
            ELSEIF ( jltype==92 ) THEN
               CALL flmasd
            ELSEIF ( jltype==93 ) THEN
               CALL axif2s
            ELSEIF ( jltype==94 ) THEN
               CALL axif2d
            ELSEIF ( jltype==95 ) THEN
               CALL axif3s
            ELSEIF ( jltype==96 ) THEN
               CALL axif3d
            ELSEIF ( jltype==97 ) THEN
               CALL axif4s
            ELSEIF ( jltype==98 ) THEN
               CALL axif4d
            ELSEIF ( jltype==99 ) THEN
               CALL slot3s
            ELSEIF ( jltype==100 ) THEN
               CALL slot3d
            ELSE
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 9
      CASE (9)
!
!
!            51 CSLOT4   52 CHBDY    53 CDUM1    54 CDUM2    55 CDUM3
!
!            56 CDUM4    57 CDUM5    58 CDUM6    59 CDUM7    60 CDUM8
!
!            61 CDUM9    62 CQDMEM1  63 CQDMEM2  64 CQUAD4   65 CIHEX1
!
!            66 CIHEX2   67 CIHEX3   68 CQUADTS  69 CTRIATS  70 CTRIAAX
!
!            71 CTRAPAX  72 CAERO1   73 CTRIM6   74 CTRPLT1  75 CTRSHL
!
!            76 CFHEX1   77 CFHEX2   78 CFTETRA  79 CFWEDGE  80 CIS2D8
!
!            81 CELBOW   82 FTUBE    83 CTRIA3   84 CPSE2    85 CPSE3
!
!            86 CPSE4
!
         IF ( local==1 ) THEN
            CALL slot4s
            spag_nextblock_1 = 5
         ELSEIF ( local==2 ) THEN
            CALL slot4d
            spag_nextblock_1 = 5
         ELSEIF ( local==3 ) THEN
            CALL hbdys
            spag_nextblock_1 = 5
         ELSEIF ( local==4 ) THEN
            CALL hbdyd
            spag_nextblock_1 = 5
         ELSEIF ( local==5 .OR. local==6 ) THEN
            CALL kdum1
            spag_nextblock_1 = 5
         ELSEIF ( local==7 .OR. local==8 ) THEN
            CALL kdum2
            spag_nextblock_1 = 5
         ELSEIF ( local==9 .OR. local==10 ) THEN
            CALL kdum3
            spag_nextblock_1 = 5
         ELSEIF ( local==11 .OR. local==12 ) THEN
            CALL kdum4
            spag_nextblock_1 = 5
         ELSEIF ( local==13 .OR. local==14 ) THEN
            CALL kdum5
            spag_nextblock_1 = 5
         ELSEIF ( local==15 .OR. local==16 ) THEN
            CALL kdum6
            spag_nextblock_1 = 5
         ELSEIF ( local==17 .OR. local==18 ) THEN
            CALL kdum7
            spag_nextblock_1 = 5
         ELSEIF ( local==19 .OR. local==20 ) THEN
            CALL kdum8
            spag_nextblock_1 = 5
         ELSEIF ( local==21 .OR. local==22 ) THEN
            CALL kdum9
            spag_nextblock_1 = 5
         ELSEIF ( local==23 ) THEN
            IF ( .NOT.heat ) THEN
               CALL qdmm1s
               spag_nextblock_1 = 11
            ELSE
               IF ( iqdmm1/=0 ) GOTO 40
               ASSIGN 40 TO ret
               iqdmm1 = 1
               spag_nextblock_1 = 16
            ENDIF
         ELSEIF ( local==24 ) THEN
            IF ( .NOT.heat ) THEN
               CALL qdmm1d
               spag_nextblock_1 = 11
            ELSE
               IF ( iqdmm1/=0 ) GOTO 60
               ASSIGN 60 TO ret
               iqdmm1 = 1
               spag_nextblock_1 = 16
            ENDIF
         ELSEIF ( local==25 ) THEN
            IF ( .NOT.heat ) THEN
               CALL qdmm2s
               spag_nextblock_1 = 11
            ELSE
               IF ( iqdmm2/=0 ) GOTO 40
               ASSIGN 40 TO ret
               iqdmm2 = 1
               spag_nextblock_1 = 16
            ENDIF
         ELSEIF ( local==26 ) THEN
            IF ( .NOT.heat ) THEN
               CALL qdmm2d
               spag_nextblock_1 = 11
            ELSE
               IF ( iqdmm2/=0 ) GOTO 60
               ASSIGN 60 TO ret
               iqdmm2 = 1
               spag_nextblock_1 = 16
            ENDIF
         ELSEIF ( local==27 ) THEN
            CALL quad4s
            spag_nextblock_1 = 5
         ELSEIF ( local==28 ) THEN
            CALL quad4d
            spag_nextblock_1 = 5
         ELSEIF ( local==29 .OR. local==31 .OR. local==33 ) THEN
            CALL ihexs(eltype-64)
            spag_nextblock_1 = 5
         ELSEIF ( local==30 .OR. local==32 .OR. local==34 ) THEN
            CALL ihexd(eltype-64)
            spag_nextblock_1 = 5
         ELSEIF ( local==35 .OR. local==36 .OR. local==37 .OR. local==38 .OR. local==43 .OR. local==44 ) THEN
            spag_nextblock_1 = 3
         ELSEIF ( local==39 ) THEN
            CALL triaax
            spag_nextblock_1 = 5
         ELSEIF ( local==40 ) THEN
            CALL triaad
            spag_nextblock_1 = 5
         ELSEIF ( local==41 ) THEN
            CALL trapax
            spag_nextblock_1 = 5
         ELSEIF ( local==42 ) THEN
            CALL trapad
            spag_nextblock_1 = 5
         ELSEIF ( local==45 ) THEN
            CALL ktrm6s
            l = 14
            spag_nextblock_1 = 15
         ELSEIF ( local==46 ) THEN
            CALL ktrm6d
            l = 14
            spag_nextblock_1 = 15
         ELSEIF ( local==47 ) THEN
            CALL ktrpls
            l = 24
            spag_nextblock_1 = 15
         ELSEIF ( local==48 ) THEN
            CALL ktrpld
            l = 24
            spag_nextblock_1 = 15
         ELSEIF ( local==49 ) THEN
            CALL ktshls
            l = 28
            spag_nextblock_1 = 15
         ELSEIF ( local==50 ) THEN
            CALL ktshld
            l = 28
            spag_nextblock_1 = 15
         ELSEIF ( local==51 ) THEN
            CALL hexa1s
            spag_nextblock_1 = 5
         ELSEIF ( local==52 ) THEN
            CALL hexa1d
            spag_nextblock_1 = 5
         ELSEIF ( local==53 ) THEN
            CALL hexa2s
            spag_nextblock_1 = 5
         ELSEIF ( local==54 ) THEN
            CALL hexa2d
            spag_nextblock_1 = 5
         ELSEIF ( local==55 ) THEN
            CALL tetras
            spag_nextblock_1 = 5
         ELSEIF ( local==56 ) THEN
            CALL tetrad
            spag_nextblock_1 = 5
         ELSEIF ( local==57 ) THEN
            CALL wedges
            spag_nextblock_1 = 5
         ELSEIF ( local==58 ) THEN
            CALL wedged
            spag_nextblock_1 = 5
         ELSEIF ( local==59 ) THEN
            CALL is2d8s
            spag_nextblock_1 = 5
         ELSEIF ( local==60 ) THEN
            CALL is2d8d
            spag_nextblock_1 = 5
         ELSEIF ( local==61 ) THEN
            CALL elbows
            spag_nextblock_1 = 5
         ELSEIF ( local==62 ) THEN
            CALL elbowd
            spag_nextblock_1 = 5
         ELSEIF ( local==63 .OR. local==64 ) THEN
            CALL ftube
            spag_nextblock_1 = 5
         ELSEIF ( local==65 ) THEN
            CALL tria3s
            spag_nextblock_1 = 5
         ELSEIF ( local==66 ) THEN
            CALL tria3d
            spag_nextblock_1 = 5
         ELSEIF ( local==67 .OR. local==68 .OR. local==69 .OR. local==70 .OR. local==71 .OR. local==72 ) THEN
            spag_nextblock_1 = 5
         ELSE
            CALL rods
            spag_nextblock_1 = 5
         ENDIF
      CASE (10)
         kht = 7
         l = 9
         IF ( volume/=0 .OR. surfac/=0 ) THEN
            CALL write(scr4,elem(izero+1),2,0)
            CALL write(scr4,estbuf(1),1,0)
            estx(5) = estx(kht)
            estx(6) = rho
            estbuf(7) = 3
            CALL write(scr4,estbuf(5),3,0)
            CALL write(scr4,estbuf(2),3,0)
            CALL write(scr4,estbuf(l),12,1)
         ENDIF
         spag_nextblock_1 = 5
      CASE (11)
         kht = 8
         l = 10
         spag_nextblock_1 = 12
      CASE (12)
         IF ( volume/=0 .OR. surfac/=0 ) THEN
            CALL write(scr4,elem(izero+1),2,0)
            CALL write(scr4,estbuf(1),1,0)
            estx(5) = estx(kht)
            estx(6) = rho
            estbuf(7) = 4
            CALL write(scr4,estbuf(5),3,0)
            CALL write(scr4,estbuf(2),4,0)
            CALL write(scr4,estbuf(l),16,1)
         ENDIF
         spag_nextblock_1 = 5
      CASE (13)
         kht = 10
         l = 14
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
 40      CALL qdmems
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
 60      CALL qdmemd
         spag_nextblock_1 = 11
      CASE (14)
         IF ( estx(12)<=0.0 ) estx(12) = estx(8)
         kht = 12
         l = 14
         spag_nextblock_1 = 12
      CASE (15)
         IF ( volume/=0.0 .OR. surfac/=0.0 ) THEN
            estx(8) = elem(izero+1)
            estx(9) = elem(izero+2)
            IF ( estx(11)<=0.0 ) estx(11) = estx(10)
            IF ( estx(12)<=0.0 ) estx(12) = estx(10)
            thk = (estx(10)+estx(11)+estx(12))/3.
            estbuf(10) = estbuf(1)
            estx(11) = thk
            estx(12) = rho
            estbuf(13) = 6
            CALL write(scr4,estbuf(8),6,0)
            CALL write(scr4,estbuf(2),6,0)
            CALL write(scr4,estbuf(l),24,1)
         ENDIF
         spag_nextblock_1 = 5
      CASE (16)
!
!     PRINT WARNING MESSAGE TO INDICATE THAT QDMEM1 ELEMENTS
!     (ELEMENT TYPE 62) AND QDMEM2 ELEMENTS (ELEMENT TYPE 63)
!     ARE REPLACED BY QDMEM ELEMENTS (ELEMENT TYPE 16) IN
!     -HEAT- FORMULATION
!
         index = 15*incr
         index1 = 16
         CALL page2(3)
         WRITE (outpt,99009) uwm , elem(izero+1) , elem(izero+2) , eltype , elem(index+1) , elem(index+2) , index1
99009    FORMAT (A25,' 3144, EMGPRO FINDS ',2A4,' ELEMENTS (ELEMENT TYPE ',I3,') PRESENT IN A HEAT FORMULATION AND IS',/5X,         &
                &'REPLACING',' THE SAME BY ',2A4,' ELEMENTS (ELEMENT TYPE ',I3,2H).)
         GOTO ret
!
!     ALL ELEMENTS OF THIS ELEMENT TYPE PROCESSED.
!     COMPLETE DICTIONARY RECORD FOR ELEMENT TYPE.
!
 80      IF ( .NOT.(error) ) THEN
            DO i = 1 , 3
               IF ( flags(i)>0 ) THEN
                  flags(i) = -flags(i)
                  CALL write(dictn(i),0,0,eor)
               ENDIF
            ENDDO
         ENDIF
!
!     FOR SAFETY AND IF CONGRUENCY EXISTS CLEAR OFF ANY TABLE POINTERS
!     ON PRIMARY-IDS IN THE CONGRUENCY LIST
!
         IF ( anycon ) THEN
            DO i = icong , ncong , 2
               IF ( z(i+1)<0 ) z(i+1) = 0
            ENDDO
         ENDIF
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     ALL ELEMENT TYPES HAVE BEEN PROCESSED.
!
 100     IF ( knogo>0 .OR. mnogo>0 ) CALL mesage(-61,0,0)
         RETURN
!
!     IMPROPER ENCOUNTER OF AN -EOF-
!
 120     jfile = est
 140     CALL mesage(-2,jfile,subr)
!
!     IMPROPER ENCOUNTER OF AN -EOR-
!
 160     jfile = est
         CALL mesage(-3,jfile,subr)
!
!     FILE NOT IN FIST
!
 180     CALL mesage(-1,jfile,subr)
         spag_nextblock_1 = 17
      CASE (17)
!
!     COMPUTE MAPPING DATA FOR CONGRUENT ELEMENTS
!
         l1 = nsils
         DO l = i1 , i2
            IF ( estbuf(l)==0 ) THEN
               m = l1
               l1 = l1 - 1
            ELSE
               m = 1
               DO n = i1 , i2
                  IF ( estbuf(n)<estbuf(l) ) THEN
                  ELSEIF ( estbuf(n)==estbuf(l) ) THEN
                     IF ( n>=l ) CYCLE
                  ELSE
                     CYCLE
                  ENDIF
                  IF ( estbuf(n)/=0 ) m = m + 1
               ENDDO
            ENDIF
            ipos(m) = l - i1 + 1
            sil(m) = estbuf(l)
         ENDDO
         IF ( igoto==1 ) THEN
            DO l = 1 , nsils
               IF ( ipos(l)/=iz(indcng+nsils+l+4) ) THEN
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            imatch = 1
            CALL write(dictn(j),z(iaddd),nsils,noeor)
            spag_nextblock_1 = 8
         ELSE
            DO l = i1 , i2
               l1 = l - i1 + 1
               SPAG_Loop_2_2: DO n = 1 , nsils
                  IF ( estbuf(l)==sil(n) ) THEN
                     iz(icg+l1+4) = n
                     EXIT SPAG_Loop_2_2
                  ENDIF
               ENDDO SPAG_Loop_2_2
               iz(icg+nsils+l1+4) = ipos(l1)
            ENDDO
            GOTO 20
         ENDIF
      CASE (18)
!
!     CHECK IF THE ELEMENT MATRIX IS DIAGONAL
!
         IF ( iz(iadd+1)/=2 ) THEN
!
!     ELEMENT MATRIX IS SQUARE.
!     PICK UP ELEMENT MATRIX DATA FOR A CONGRUENT ELEMENT THAT HAS
!     ALREADY BEEN PROCESSED AND STORE IT ON SCR3.
!
            ibuf1 = ncore - sysbuf - 2
            icrq = jcore - ibuf1
            IF ( icrq<=0 ) THEN
               ibuf3 = ibuf1 - 1
               icrq = jcore - ibuf3 + 36*nsils*iprec
               IF ( icrq<=0 ) THEN
                  ifile = mats(j)
                  IF ( iz(indcng+j+1)==0 ) THEN
                     CALL savpos(ifile,isave1)
                     CALL close(ifile,1)
                     ibuf2 = Ibuf(ibfind+1)
                     CALL gopen(ifile,z(ibuf2),0)
                     CALL filpos(ifile,z(iaddd))
                     IF ( isave2/=0 ) THEN
                        jfile = scr3
                        CALL open(*180,scr3,z(ibuf1),3)
                     ELSE
                        CALL gopen(scr3,z(ibuf1),1)
                     ENDIF
                     jfile = ifile
                     DO l1 = 1 , nsils
                        CALL read(*140,*182,ifile,z(jcore),ibuf3,eor,n)
 182                    CALL write(scr3,z(jcore),n,eor)
                        IF ( l1==1 ) CALL savpos(scr3,iz(indcng+j+1))
                     ENDDO
                     CALL filpos(ifile,isave1)
                     CALL skprec(ifile,1)
                     CALL close(ifile,2)
                     CALL open(*180,ifile,z(ibuf2),3)
                     CALL savpos(scr3,isave2)
                     CALL close(scr3,1)
                  ENDIF
!
!     ELEMENT MATRIX DATA IS AVAILABLE ON SCR3.  REARRANGE IT IN
!     THE REQUIRED ORDER AND WRITE IT ON THE OUTPUT DATA BLOCK.
!
                  CALL gopen(scr3,z(ibuf1),0)
                  jfile = scr3
                  DO l = 1 , nsils
                     CALL filpos(scr3,iz(indcng+j+1))
                     m = ipos(l)
                     n = iz(indcng+m+4) - 1
                     CALL skprec(scr3,n)
                     CALL read(*140,*184,scr3,z(jcore),ibuf3,eor,n)
 184                 nnwrds = n/(nsils*iprec)
                     nnwrds = sqrt(nnwrds+0.5)
                     nwords = nnwrds*iprec
                     jjcore = jcore
                     DO l2 = 1 , nnwrds
                        DO l1 = 1 , nsils
                           m = ipos(l1)
                           n = iz(indcng+m+4) - 1
                           CALL write(ifile,z(jjcore+n*nwords),nwords,noeor)
                        ENDDO
                        jjcore = jjcore + nwords*nsils
                     ENDDO
                     CALL write(ifile,0,0,1)
                     CALL savpos(ifile,isave1)
                     CALL write(dictn(j),isave1,1,noeor)
                  ENDDO
                  CALL filpos(scr3,isave2)
                  CALL skprec(scr3,1)
                  CALL close(scr3,2)
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
            WRITE (outpt,99010) uim , estid
99010       FORMAT (A29,' 2384, CONGRUENCY OF ELEMENT ID =',I10,' WILL BE IGNORED AND ITS ELEMENT MATRICES',/5X,                    &
                   &'WILL BE RE-COMPUTED AS THERE IS INSUFFICIENT CORE AT ','THIS TIME TO PERFORM CONGRUENCY MAPPING COMPUTATIONS.')
            WRITE (outpt,99012) icrq
            CALL page2(4)
            GOTO 20
         ELSE
!
!     ELEMENT MATRIX IS DIAGONAL.
!     RE-WRITE ONLY THE ELEMENT DICTIONARY FOR A CONGRUENT ELEMENT.
!
            DO l = 1 , nsils
               m = ipos(l)
               n = iz(indcng+m+4) - 1
               CALL write(dictn(j),z(iaddd+n),1,noeor)
            ENDDO
            spag_nextblock_1 = 8
         ENDIF
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99011 FORMAT (A25,' 2422, VISC DATA NOT PROCESSED BY EMGPRO.')
99012 FORMAT (5X,'ADDITIONAL CORE NEEDED =',I9,' WORDS.')
!
END SUBROUTINE emgpro
