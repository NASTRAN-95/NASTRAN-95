
SUBROUTINE emgpro(Ibuf)
   IMPLICIT NONE
   LOGICAL Anycon , Error , Heat
   INTEGER Cstm , Dictn(3) , Dit , Elem(1) , Elid , Eltype , Est , Estbuf(200) , Estid , Flags(3) , Geom2 , Icall , Icmbar , Icong ,&
         & Icore , Icstm , Idit , Ihmat , Ilast , Imat , Incr , Iprec , Iz(1) , Jcore , Kdummy(22) , Kflags(3) , Knogo , Ksystm(65) &
         & , Ktypes , L38 , Last , Lcong , Lcstm , Ldict , Lhmat , Lmat , Mats(3) , Mdummy(20) , Mnogo , Mpt , Ncong , Ncore ,      &
         & Ncpbar , Ncpqd1 , Ncpqd2 , Ncpqdp , Ncprod , Ncptr1 , Ncptr2 , Ncptrb , Ncptrp , Ncptub , Ncstm , Ndit , Nelem , Nhmat , &
         & Nlocs , Nmat , Nob , Nocmas , Nok , Nok4gg , Nokdgg , Nom , Nval(3) , Outpt , Precis , Sysbuf , Z(1)
   DOUBLE PRECISION Dummy
   REAL Egnu(6) , Estx(12) , Rho , Surfac , Volume
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Nok , Nom , Nob , Nok4gg , Nokdgg , Nocmas , Ncpbar , Ncprod , Ncpqd1 , Ncpqd2 , Ncptr1 , Ncptr2 , Ncptub ,      &
                 & Ncpqdp , Ncptrp , Ncptrb , Volume , Surfac
   COMMON /emgdic/ Eltype , Ldict , Nlocs , Elid , Estid
   COMMON /emgest/ Estbuf
   COMMON /emgfil/ Est , Cstm , Mpt , Dit , Geom2 , Mats , Dictn
   COMMON /emgprm/ Icore , Jcore , Ncore , Icstm , Ncstm , Imat , Nmat , Ihmat , Nhmat , Idit , Ndit , Icong , Ncong , Lcong ,      &
                 & Anycon , Flags , Precis , Error , Heat , Icmbar , Lcstm , Lmat , Lhmat , Kflags , L38
   COMMON /gpta1 / Nelem , Last , Incr , Elem
   COMMON /iemg1b/ Icall , Ilast
   COMMON /iemgod/ Dummy , Ktypes
   COMMON /iemgot/ Nval
   COMMON /matout/ Egnu , Rho
   COMMON /sma1cl/ Kdummy , Knogo
   COMMON /sma2cl/ Mdummy , Mnogo
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   COMMON /zzzzzz/ Z
   INTEGER Ibuf(7)
   INTEGER dosi(2) , eor , estwds , i , i1 , i2 , iadd , iaddd , ibfind , ibuf1 , ibuf2 , ibuf3 , icg , icrq , idprim , ifile ,     &
         & igoto , imatch , indcng , index , index1 , ipos(32) , ipr , iprime , iqdmm1 , iqdmm2 , isave1 , isave2 , isil , iwords , &
         & izero , j , jfile , jjcore , jltype , kht , l , l1 , l2 , lnum , local , ltypes , m , n , nnwrds , noeor , nscal1 ,      &
         & nscal2 , nsils , nwords , ret , savjcr , savncr , scr3 , scr4 , sil(32) , subr(2)
   REAL thk , trim6(2) , trpl1(2) , trshl(2)
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
!
   iqdmm1 = 0
   iqdmm2 = 0
   Nval(1) = 0
   Nval(2) = 0
   Nval(3) = 0
   ltypes = 0
   Ktypes = 0
   Dummy = 0.0D0
   Icall = 0
   Ilast = 0
!
!     INITIALIZE /SMA1CL/ AND /SMA2CL/
!
   Knogo = 0
   Mnogo = 0
   Kdummy(10) = 10
   Mdummy(10) = 10
!
!     FOLLOWING CALL PREPS /GPTA1/ FOR DUMMY ELEMENTS
!
   CALL delset
!
!     DEFINE WORKING CORE BLOCK FOR RESET PURPOSES.
!
   ipr = Precis
   IF ( ipr/=1 ) ipr = 0
   savjcr = Jcore
   savncr = Ncore
   Estid = 0
   lnum = Lcong/2
!
!     READ THE ELEMENT TYPE FROM THE EST.
!
 100  CALL read(*2000,*2300,Est,Eltype,1,noeor,iwords)
   izero = Incr*(Eltype-1)
!
!     CHECK FOR ALLOWABLE ELEMENT TYPES
!
   IF ( Eltype/=2 .AND. Eltype/=32 .AND. Eltype/=33 .AND. Eltype/=68 .AND. Eltype/=69 .AND. Eltype/=72 ) THEN
      IF ( Eltype>=1 .AND. Eltype<=Nelem ) THEN
!
!     RESTORE CORE POINTERS
!
         Jcore = savjcr
         Ncore = savncr
!
!     CLEAR ESTBUF
!
         DO i = 1 , 200
            Estbuf(i) = 0
         ENDDO
!
!     SET VARIOUS PARAMETERS = FUNCTION OF THIS ELEMENT TYPE
!
!     TURN ON COUPLED MASS FLAG IF EITHER OF ALL-COUPLED-MASS-FLAG
!     OR SPECIFIC-TYPE-COUPLED-MASS-FLAG IS ON.
!
         IF ( Flags(2)==0 ) THEN
            Icmbar = -1
         ELSEIF ( Nocmas<0 ) THEN
            Icmbar = -1
         ELSEIF ( Nocmas==0 ) THEN
            IF ( Eltype==34 ) THEN
               IF ( Ncpbar<=0 ) THEN
                  Icmbar = -1
               ELSE
!
                  Icmbar = 1
               ENDIF
            ELSEIF ( Eltype==1 ) THEN
               IF ( Ncprod<=0 ) THEN
                  Icmbar = -1
               ELSE
                  Icmbar = 1
               ENDIF
            ELSEIF ( Eltype==19 ) THEN
               IF ( Ncpqd1<=0 ) THEN
                  Icmbar = -1
               ELSE
                  Icmbar = 1
               ENDIF
            ELSEIF ( Eltype==18 ) THEN
               IF ( Ncpqd2<=0 ) THEN
                  Icmbar = -1
               ELSE
                  Icmbar = 1
               ENDIF
            ELSEIF ( Eltype==6 ) THEN
               IF ( Ncptr1<=0 ) THEN
                  Icmbar = -1
               ELSE
                  Icmbar = 1
               ENDIF
            ELSEIF ( Eltype==17 ) THEN
               IF ( Ncptr2<=0 ) THEN
                  Icmbar = -1
               ELSE
                  Icmbar = 1
               ENDIF
            ELSEIF ( Eltype==3 ) THEN
               IF ( Ncptub<=0 ) THEN
                  Icmbar = -1
               ELSE
                  Icmbar = 1
               ENDIF
            ELSEIF ( Eltype==15 ) THEN
               IF ( Ncpqdp<=0 ) THEN
                  Icmbar = -1
               ELSE
                  Icmbar = 1
               ENDIF
            ELSEIF ( Eltype==8 ) THEN
               IF ( Ncptrp<=0 ) THEN
                  Icmbar = -1
               ELSE
                  Icmbar = 1
               ENDIF
            ELSEIF ( Eltype==7 ) THEN
               IF ( Ncptrb<=0 ) THEN
                  Icmbar = -1
               ELSE
                  Icmbar = 1
               ENDIF
            ELSE
               Icmbar = -1
            ENDIF
         ELSE
            Icmbar = 1
         ENDIF
!
         jltype = 2*Eltype - ipr
         estwds = Elem(izero+12)
         nsils = Elem(izero+10)
         isil = Elem(izero+13)
         IF ( Elem(izero+9)/=0 ) isil = isil - 1
         i1 = isil
         i2 = isil + nsils - 1
         isave2 = 0
         IF ( estwds<=200 ) THEN
!
!     CHECK TO SEE IF ILLEGAL ELEMENTS ARE USED IN -HEAT- FORMULATION
!
            IF ( Heat ) THEN
               IF ( Eltype/=1 .AND. Eltype/=3 .AND. Eltype/=6 ) THEN
                  IF ( Eltype<9 .OR. Eltype>14 ) THEN
                     IF ( Eltype<16 .OR. Eltype>24 ) THEN
                        IF ( Eltype/=34 .AND. Eltype/=36 .AND. Eltype/=37 ) THEN
                           IF ( Eltype<39 .OR. Eltype>42 ) THEN
                              IF ( Eltype/=52 .AND. Eltype/=62 .AND. Eltype/=63 ) THEN
                                 IF ( Eltype<64 .OR. Eltype>67 ) THEN
                                    IF ( Eltype/=80 .AND. Eltype/=81 .AND. Eltype/=83 ) THEN
!
                                       WRITE (Outpt,99001) Ufm , Elem(izero+1) , Elem(izero+2) , Eltype
99001                                  FORMAT (A23,' 3115, EMGPRO FINDS ',2A4,' ELEMENTS (ELEMENT TYPE ',I3,                        &
                                         &') PRESENT IN A HEAT FORMULATION.')
                                       GOTO 300
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
            Nlocs = nsils
            Ldict = Nlocs + 5
            GOTO 400
         ELSE
            WRITE (Outpt,99002) Sfm , Eltype
99002       FORMAT (A25,' 3106, EMGPRO FINDS THAT ELEMENT TYPE ',I3,' HAS EST ENTRIES TOO LARGE TO HANDLE CURRENTLY.')
            GOTO 300
         ENDIF
      ENDIF
   ENDIF
 200  WRITE (Outpt,99003) Sfm , Elem(izero+1) , Elem(izero+2) , Eltype
99003 FORMAT (A25,' 3105, EMGPRO FINDS ',2A4,' ELEMENTS (ELEM. TYPE ',I3,') UNDEFINED IN EST DATA BLOCK AND/OR ELEMENT ROUTINE.')
 300  CALL fwdrec(*2100,Est)
   Error = .TRUE.
   GOTO 100
!
!     READ AN ELEMENT EST ENTRY
!
 400  CALL read(*2100,*1900,Est,Estbuf,estwds,noeor,iwords)
   Elid = Estbuf(1)
   Estid = Estid + 1
!
!     CHECK TO SEE IF THIS ELEMENT IS CONGRUENT TO ANOTHER ALREADY
!     POSSESSING A DICTIONARY IN CORE.
!
   IF ( .NOT.Anycon ) GOTO 800
   CALL bisloc(*800,Elid,Z(Icong),2,lnum,j)
!
!     MATCH FOUND.  CHECK FOR DICTIONARY-TABLE ON PRIMARY.
!
   iprime = Z(Icong+j)
   idprim = Z(Icong+j-1)
   DO
      IF ( iprime<0 ) THEN
!
!     IPRIME IS NEGATIVE TABLE ADDRESS IMPLYING DICTIONARY EXISTS.
!
         IF ( Error ) GOTO 800
         iprime = -iprime
         imatch = 0
         ibfind = 1
         j = 0
         EXIT
      ELSEIF ( iprime==0 ) THEN
!
!     SET UP ELEMENT MATRIX MAPPING ARRAY FOR LATER USE BY OTHER
!     ELEMENTS IN THIS CONGRUENT SET
!
         icg = Jcore
         jjcore = Jcore + 2*nsils + 5
         icrq = jjcore - Ncore
         IF ( jjcore>=Ncore ) THEN
            WRITE (Outpt,99004) Uim , idprim
!
99004       FORMAT (A29,' 2382, ELEMENT MATRICES FOR ELEMENTS CONGRUENT TO ','ELEMENT ID =',I10,/5X,                                &
                   &'WILL BE RE-COMPUTED AS THERE IS',' INSUFFICIENT CORE AT THIS TIME TO HOLD CONGRUENCY ','MAPPING DATA.')
            WRITE (Outpt,99012) icrq
            CALL page2(4)
            GOTO 800
         ELSE
            Jcore = jjcore
            Iz(icg) = idprim
            Iz(icg+1) = nsils
            Iz(icg+2) = 0
            Iz(icg+3) = 0
            Iz(icg+4) = 0
            igoto = 0
            GOTO 2500
         ENDIF
      ELSE
!
!     IPRIME POINTS TO PRIMARY ID
!
         idprim = Z(iprime)
         iprime = Z(iprime+1)
      ENDIF
   ENDDO
 500  j = j + 1
   iadd = Z(iprime+j)
   IF ( iadd<=0 ) GOTO 700
!
!     COPY DICTIONARY FROM CORE TO DICTIONARY FILE.
!
   Z(iadd) = Estid
   Flags(j) = Flags(j) + 1
   CALL write(Dictn(j),Z(iadd),5,noeor)
   iaddd = iadd + 5
   IF ( imatch==1 ) THEN
      CALL write(Dictn(j),Z(iaddd),nsils,noeor)
      GOTO 700
   ELSE
      IF ( imatch==2 ) GOTO 2600
      indcng = savjcr
      igoto = 1
      DO WHILE ( Iz(indcng)/=idprim )
         jjcore = indcng + 2*Iz(indcng+1) + 5
         IF ( jjcore>=Ncore ) THEN
            WRITE (Outpt,99005) Swm , Estid
99005       FORMAT (A27,' 2383, UNABLE TO LOCATE CONGRUENCY MAPPING DATA FOR',' ELEMENT ID =',I10,1H.,/5X,                          &
                   &'ELEMENT MATRICES FOR THIS ','ELEMENT WILL, THEREFORE, BE RE-COMPUTED.')
            CALL page2(4)
            GOTO 800
         ELSE
            indcng = jjcore
         ENDIF
      ENDDO
      GOTO 2500
   ENDIF
 600  imatch = 2
   GOTO 2600
 700  ibfind = ibfind + 2
   IF ( j>=3 ) GOTO 400
   GOTO 500
!
!     BRANCH ON ELEMENT TYPE.  INDIVIDUAL ROUTINES WILL COMPUTE AND
!     OUTPUT ALL MATRIX TYPES DESIRED BASED ON FLAGS AVAILABLE TO THEM.
!
 800  IF ( Eltype/=ltypes ) THEN
      ltypes = Eltype
      IF ( ltypes>Nelem ) GOTO 200
      CALL page2(3)
      WRITE (Outpt,99006) Uim , dosi(ipr+1) , Elem(izero+1) , Elem(izero+2) , Eltype , Elid
99006 FORMAT (A29,' 3113,',/5X,'EMG MODULE PROCESSING ',A4,'LE PRECISION ',2A4,' ELEMENTS (ELEMENT TYPE ',I3,') STARTING WITH ID ', &
            & I8)
      IF ( Eltype>=84 .AND. Eltype<=86 ) WRITE (Outpt,99007)
99007 FORMAT (5X,'(STEPPING THRU ONLY. NO REAL COMPUTATION HERE FOR ','THIS DIFFERENTIAL STIFFNESS ELEMENT)')
   ENDIF
   IF ( L38==1 ) THEN
      CALL page2(1)
      WRITE (Outpt,99008) Elid
99008 FORMAT (5X,'ELEMENT ',I8,' IS BEING PROCESSED')
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
         GOTO 200
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
         GOTO 1000
      ELSEIF ( jltype==12 ) THEN
         CALL tria1d
         GOTO 1000
      ELSEIF ( jltype==13 ) THEN
         CALL trbscs
         GOTO 1000
      ELSEIF ( jltype==14 ) THEN
         CALL trbscd
         GOTO 1000
      ELSEIF ( jltype==15 ) THEN
         CALL trplts
         GOTO 1000
      ELSEIF ( jltype==16 ) THEN
         CALL trpltd
         GOTO 1000
      ELSEIF ( jltype==17 ) THEN
         CALL trmems
         GOTO 1000
      ELSEIF ( jltype==18 ) THEN
         CALL trmemd
         GOTO 1000
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
         GOTO 1300
      ELSEIF ( jltype==30 ) THEN
         CALL qdpltd
         GOTO 1300
      ELSEIF ( jltype==31 ) THEN
         GOTO 1400
      ELSEIF ( jltype==32 ) THEN
         GOTO 1500
      ELSEIF ( jltype==33 ) THEN
         CALL tria2s
         GOTO 1000
      ELSEIF ( jltype==34 ) THEN
         CALL tria2d
         GOTO 1000
      ELSEIF ( jltype==35 ) THEN
         CALL quad2s
         GOTO 1100
      ELSEIF ( jltype==36 ) THEN
         CALL quad2d
         GOTO 1100
      ELSEIF ( jltype==37 ) THEN
         CALL quad1s
         GOTO 1600
      ELSEIF ( jltype==38 ) THEN
         CALL quad1d
         GOTO 1600
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
         IF ( Flags(3)==0 ) WRITE (Outpt,99011) Uwm
      ELSEIF ( jltype==48 ) THEN
         CALL viscd
         IF ( Flags(3)==0 ) WRITE (Outpt,99011) Uwm
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
         GOTO 900
      ENDIF
      GOTO 400
   ENDIF
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
 900  IF ( local==1 ) THEN
      CALL slot4s
      GOTO 400
   ELSEIF ( local==2 ) THEN
      CALL slot4d
      GOTO 400
   ELSEIF ( local==3 ) THEN
      CALL hbdys
      GOTO 400
   ELSEIF ( local==4 ) THEN
      CALL hbdyd
      GOTO 400
   ELSEIF ( local==5 .OR. local==6 ) THEN
      CALL kdum1
      GOTO 400
   ELSEIF ( local==7 .OR. local==8 ) THEN
      CALL kdum2
      GOTO 400
   ELSEIF ( local==9 .OR. local==10 ) THEN
      CALL kdum3
      GOTO 400
   ELSEIF ( local==11 .OR. local==12 ) THEN
      CALL kdum4
      GOTO 400
   ELSEIF ( local==13 .OR. local==14 ) THEN
      CALL kdum5
      GOTO 400
   ELSEIF ( local==15 .OR. local==16 ) THEN
      CALL kdum6
      GOTO 400
   ELSEIF ( local==17 .OR. local==18 ) THEN
      CALL kdum7
      GOTO 400
   ELSEIF ( local==19 .OR. local==20 ) THEN
      CALL kdum8
      GOTO 400
   ELSEIF ( local==21 .OR. local==22 ) THEN
      CALL kdum9
      GOTO 400
   ELSEIF ( local==23 ) THEN
      IF ( .NOT.Heat ) THEN
         CALL qdmm1s
         GOTO 1100
      ELSE
         IF ( iqdmm1/=0 ) GOTO 1400
         ASSIGN 1400 TO ret
         iqdmm1 = 1
         GOTO 1800
      ENDIF
   ELSEIF ( local==24 ) THEN
      IF ( .NOT.Heat ) THEN
         CALL qdmm1d
         GOTO 1100
      ELSE
         IF ( iqdmm1/=0 ) GOTO 1500
         ASSIGN 1500 TO ret
         iqdmm1 = 1
         GOTO 1800
      ENDIF
   ELSEIF ( local==25 ) THEN
      IF ( .NOT.Heat ) THEN
         CALL qdmm2s
         GOTO 1100
      ELSE
         IF ( iqdmm2/=0 ) GOTO 1400
         ASSIGN 1400 TO ret
         iqdmm2 = 1
         GOTO 1800
      ENDIF
   ELSEIF ( local==26 ) THEN
      IF ( .NOT.Heat ) THEN
         CALL qdmm2d
         GOTO 1100
      ELSE
         IF ( iqdmm2/=0 ) GOTO 1500
         ASSIGN 1500 TO ret
         iqdmm2 = 1
         GOTO 1800
      ENDIF
   ELSEIF ( local==27 ) THEN
      CALL quad4s
      GOTO 400
   ELSEIF ( local==28 ) THEN
      CALL quad4d
      GOTO 400
   ELSEIF ( local==29 .OR. local==31 .OR. local==33 ) THEN
      CALL ihexs(Eltype-64)
      GOTO 400
   ELSEIF ( local==30 .OR. local==32 .OR. local==34 ) THEN
      CALL ihexd(Eltype-64)
      GOTO 400
   ELSEIF ( local==35 .OR. local==36 .OR. local==37 .OR. local==38 .OR. local==43 .OR. local==44 ) THEN
      GOTO 200
   ELSEIF ( local==39 ) THEN
      CALL triaax
      GOTO 400
   ELSEIF ( local==40 ) THEN
      CALL triaad
      GOTO 400
   ELSEIF ( local==41 ) THEN
      CALL trapax
      GOTO 400
   ELSEIF ( local==42 ) THEN
      CALL trapad
      GOTO 400
   ELSEIF ( local==45 ) THEN
      CALL ktrm6s
      l = 14
      GOTO 1700
   ELSEIF ( local==46 ) THEN
      CALL ktrm6d
      l = 14
      GOTO 1700
   ELSEIF ( local==47 ) THEN
      CALL ktrpls
      l = 24
      GOTO 1700
   ELSEIF ( local==48 ) THEN
      CALL ktrpld
      l = 24
      GOTO 1700
   ELSEIF ( local==49 ) THEN
      CALL ktshls
      l = 28
      GOTO 1700
   ELSEIF ( local==50 ) THEN
      CALL ktshld
      l = 28
      GOTO 1700
   ELSEIF ( local==51 ) THEN
      CALL hexa1s
      GOTO 400
   ELSEIF ( local==52 ) THEN
      CALL hexa1d
      GOTO 400
   ELSEIF ( local==53 ) THEN
      CALL hexa2s
      GOTO 400
   ELSEIF ( local==54 ) THEN
      CALL hexa2d
      GOTO 400
   ELSEIF ( local==55 ) THEN
      CALL tetras
      GOTO 400
   ELSEIF ( local==56 ) THEN
      CALL tetrad
      GOTO 400
   ELSEIF ( local==57 ) THEN
      CALL wedges
      GOTO 400
   ELSEIF ( local==58 ) THEN
      CALL wedged
      GOTO 400
   ELSEIF ( local==59 ) THEN
      CALL is2d8s
      GOTO 400
   ELSEIF ( local==60 ) THEN
      CALL is2d8d
      GOTO 400
   ELSEIF ( local==61 ) THEN
      CALL elbows
      GOTO 400
   ELSEIF ( local==62 ) THEN
      CALL elbowd
      GOTO 400
   ELSEIF ( local==63 .OR. local==64 ) THEN
      CALL ftube
      GOTO 400
   ELSEIF ( local==65 ) THEN
      CALL tria3s
      GOTO 400
   ELSEIF ( local==66 ) THEN
      CALL tria3d
      GOTO 400
   ELSEIF ( local==67 .OR. local==68 .OR. local==69 .OR. local==70 .OR. local==71 .OR. local==72 ) THEN
      GOTO 400
   ELSE
      CALL rods
      GOTO 400
   ENDIF
 1000 kht = 7
   l = 9
   IF ( Volume/=0 .OR. Surfac/=0 ) THEN
      CALL write(scr4,Elem(izero+1),2,0)
      CALL write(scr4,Estbuf(1),1,0)
      Estx(5) = Estx(kht)
      Estx(6) = Rho
      Estbuf(7) = 3
      CALL write(scr4,Estbuf(5),3,0)
      CALL write(scr4,Estbuf(2),3,0)
      CALL write(scr4,Estbuf(l),12,1)
   ENDIF
   GOTO 400
 1100 kht = 8
   l = 10
 1200 IF ( Volume/=0 .OR. Surfac/=0 ) THEN
      CALL write(scr4,Elem(izero+1),2,0)
      CALL write(scr4,Estbuf(1),1,0)
      Estx(5) = Estx(kht)
      Estx(6) = Rho
      Estbuf(7) = 4
      CALL write(scr4,Estbuf(5),3,0)
      CALL write(scr4,Estbuf(2),4,0)
      CALL write(scr4,Estbuf(l),16,1)
   ENDIF
   GOTO 400
 1300 kht = 10
   l = 14
   GOTO 1200
 1400 CALL qdmems
   GOTO 1100
 1500 CALL qdmemd
   GOTO 1100
 1600 IF ( Estx(12)<=0.0 ) Estx(12) = Estx(8)
   kht = 12
   l = 14
   GOTO 1200
 1700 IF ( Volume/=0.0 .OR. Surfac/=0.0 ) THEN
      Estx(8) = Elem(izero+1)
      Estx(9) = Elem(izero+2)
      IF ( Estx(11)<=0.0 ) Estx(11) = Estx(10)
      IF ( Estx(12)<=0.0 ) Estx(12) = Estx(10)
      thk = (Estx(10)+Estx(11)+Estx(12))/3.
      Estbuf(10) = Estbuf(1)
      Estx(11) = thk
      Estx(12) = Rho
      Estbuf(13) = 6
      CALL write(scr4,Estbuf(8),6,0)
      CALL write(scr4,Estbuf(2),6,0)
      CALL write(scr4,Estbuf(l),24,1)
   ENDIF
   GOTO 400
!
!     PRINT WARNING MESSAGE TO INDICATE THAT QDMEM1 ELEMENTS
!     (ELEMENT TYPE 62) AND QDMEM2 ELEMENTS (ELEMENT TYPE 63)
!     ARE REPLACED BY QDMEM ELEMENTS (ELEMENT TYPE 16) IN
!     -HEAT- FORMULATION
!
 1800 index = 15*Incr
   index1 = 16
   CALL page2(3)
   WRITE (Outpt,99009) Uwm , Elem(izero+1) , Elem(izero+2) , Eltype , Elem(index+1) , Elem(index+2) , index1
99009 FORMAT (A25,' 3144, EMGPRO FINDS ',2A4,' ELEMENTS (ELEMENT TYPE ',I3,') PRESENT IN A HEAT FORMULATION AND IS',/5X,'REPLACING',&
             &' THE SAME BY ',2A4,' ELEMENTS (ELEMENT TYPE ',I3,2H).)
   GOTO ret
!
!     ALL ELEMENTS OF THIS ELEMENT TYPE PROCESSED.
!     COMPLETE DICTIONARY RECORD FOR ELEMENT TYPE.
!
 1900 IF ( .NOT.(Error) ) THEN
      DO i = 1 , 3
         IF ( Flags(i)>0 ) THEN
            Flags(i) = -Flags(i)
            CALL write(Dictn(i),0,0,eor)
         ENDIF
      ENDDO
   ENDIF
!
!     FOR SAFETY AND IF CONGRUENCY EXISTS CLEAR OFF ANY TABLE POINTERS
!     ON PRIMARY-IDS IN THE CONGRUENCY LIST
!
   IF ( Anycon ) THEN
      DO i = Icong , Ncong , 2
         IF ( Z(i+1)<0 ) Z(i+1) = 0
      ENDDO
   ENDIF
   GOTO 100
!
!     ALL ELEMENT TYPES HAVE BEEN PROCESSED.
!
 2000 IF ( Knogo>0 .OR. Mnogo>0 ) CALL mesage(-61,0,0)
   RETURN
!
!     IMPROPER ENCOUNTER OF AN -EOF-
!
 2100 jfile = Est
 2200 CALL mesage(-2,jfile,subr)
!
!     IMPROPER ENCOUNTER OF AN -EOR-
!
 2300 jfile = Est
   CALL mesage(-3,jfile,subr)
!
!     FILE NOT IN FIST
!
 2400 CALL mesage(-1,jfile,subr)
!
!     COMPUTE MAPPING DATA FOR CONGRUENT ELEMENTS
!
 2500 l1 = nsils
   DO l = i1 , i2
      IF ( Estbuf(l)==0 ) THEN
         m = l1
         l1 = l1 - 1
      ELSE
         m = 1
         DO n = i1 , i2
            IF ( Estbuf(n)<Estbuf(l) ) THEN
            ELSEIF ( Estbuf(n)==Estbuf(l) ) THEN
               IF ( n>=l ) CYCLE
            ELSE
               CYCLE
            ENDIF
            IF ( Estbuf(n)/=0 ) m = m + 1
         ENDDO
      ENDIF
      ipos(m) = l - i1 + 1
      sil(m) = Estbuf(l)
   ENDDO
   IF ( igoto==1 ) THEN
      DO l = 1 , nsils
         IF ( ipos(l)/=Iz(indcng+nsils+l+4) ) GOTO 600
      ENDDO
      imatch = 1
      CALL write(Dictn(j),Z(iaddd),nsils,noeor)
      GOTO 700
   ELSE
      DO l = i1 , i2
         l1 = l - i1 + 1
         DO n = 1 , nsils
            IF ( Estbuf(l)==sil(n) ) THEN
               Iz(icg+l1+4) = n
               EXIT
            ENDIF
         ENDDO
         Iz(icg+nsils+l1+4) = ipos(l1)
      ENDDO
      GOTO 800
   ENDIF
!
!     CHECK IF THE ELEMENT MATRIX IS DIAGONAL
!
 2600 IF ( Iz(iadd+1)/=2 ) THEN
!
!     ELEMENT MATRIX IS SQUARE.
!     PICK UP ELEMENT MATRIX DATA FOR A CONGRUENT ELEMENT THAT HAS
!     ALREADY BEEN PROCESSED AND STORE IT ON SCR3.
!
      ibuf1 = Ncore - Sysbuf - 2
      icrq = Jcore - ibuf1
      IF ( icrq<=0 ) THEN
         ibuf3 = ibuf1 - 1
         icrq = Jcore - ibuf3 + 36*nsils*Iprec
         IF ( icrq<=0 ) THEN
            ifile = Mats(j)
            IF ( Iz(indcng+j+1)==0 ) THEN
               CALL savpos(ifile,isave1)
               CALL close(ifile,1)
               ibuf2 = Ibuf(ibfind+1)
               CALL gopen(ifile,Z(ibuf2),0)
               CALL filpos(ifile,Z(iaddd))
               IF ( isave2/=0 ) THEN
                  jfile = scr3
                  CALL open(*2400,scr3,Z(ibuf1),3)
               ELSE
                  CALL gopen(scr3,Z(ibuf1),1)
               ENDIF
               jfile = ifile
               DO l1 = 1 , nsils
                  CALL read(*2200,*2602,ifile,Z(Jcore),ibuf3,eor,n)
 2602             CALL write(scr3,Z(Jcore),n,eor)
                  IF ( l1==1 ) CALL savpos(scr3,Iz(indcng+j+1))
               ENDDO
               CALL filpos(ifile,isave1)
               CALL skprec(ifile,1)
               CALL close(ifile,2)
               CALL open(*2400,ifile,Z(ibuf2),3)
               CALL savpos(scr3,isave2)
               CALL close(scr3,1)
            ENDIF
!
!     ELEMENT MATRIX DATA IS AVAILABLE ON SCR3.  REARRANGE IT IN
!     THE REQUIRED ORDER AND WRITE IT ON THE OUTPUT DATA BLOCK.
!
            CALL gopen(scr3,Z(ibuf1),0)
            jfile = scr3
            DO l = 1 , nsils
               CALL filpos(scr3,Iz(indcng+j+1))
               m = ipos(l)
               n = Iz(indcng+m+4) - 1
               CALL skprec(scr3,n)
               CALL read(*2200,*2605,scr3,Z(Jcore),ibuf3,eor,n)
 2605          nnwrds = n/(nsils*Iprec)
               nnwrds = sqrt(nnwrds+0.5)
               nwords = nnwrds*Iprec
               jjcore = Jcore
               DO l2 = 1 , nnwrds
                  DO l1 = 1 , nsils
                     m = ipos(l1)
                     n = Iz(indcng+m+4) - 1
                     CALL write(ifile,Z(jjcore+n*nwords),nwords,noeor)
                  ENDDO
                  jjcore = jjcore + nwords*nsils
               ENDDO
               CALL write(ifile,0,0,1)
               CALL savpos(ifile,isave1)
               CALL write(Dictn(j),isave1,1,noeor)
            ENDDO
            CALL filpos(scr3,isave2)
            CALL skprec(scr3,1)
            CALL close(scr3,2)
            GOTO 700
         ENDIF
      ENDIF
      WRITE (Outpt,99010) Uim , Estid
99010 FORMAT (A29,' 2384, CONGRUENCY OF ELEMENT ID =',I10,' WILL BE IGNORED AND ITS ELEMENT MATRICES',/5X,                          &
             &'WILL BE RE-COMPUTED AS THERE IS INSUFFICIENT CORE AT ','THIS TIME TO PERFORM CONGRUENCY MAPPING COMPUTATIONS.')
      WRITE (Outpt,99012) icrq
      CALL page2(4)
      GOTO 800
   ELSE
!
!     ELEMENT MATRIX IS DIAGONAL.
!     RE-WRITE ONLY THE ELEMENT DICTIONARY FOR A CONGRUENT ELEMENT.
!
      DO l = 1 , nsils
         m = ipos(l)
         n = Iz(indcng+m+4) - 1
         CALL write(Dictn(j),Z(iaddd+n),1,noeor)
      ENDDO
      GOTO 700
   ENDIF
99011 FORMAT (A25,' 2422, VISC DATA NOT PROCESSED BY EMGPRO.')
99012 FORMAT (5X,'ADDITIONAL CORE NEEDED =',I9,' WORDS.')
!
END SUBROUTINE emgpro