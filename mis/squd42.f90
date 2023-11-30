
SUBROUTINE squd42
   IMPLICIT NONE
   REAL Alfab(3) , Alfam(3) , App(2) , Avgthk , Degrad , Dumm(30) , Dummy(35) , Epsln(8) , Epslnt(6) , Forsul(46) , Fxcntr ,        &
      & Fxycnt , Fycntr , G(36) , G2alfb(30) , Gpth(4) , Gt(36) , Khit(3) , Mominr , Phiout(2395) , Pi , Raddeg , Reali(5) ,        &
      & Shpfnc(4) , Sigma(3) , Signx(4) , Signy(4) , Skp(4) , Sort2 , Stemp(8) , Strs(2) , Strx(2) , Stry(2) , Tes(9) , Tesu(9) ,   &
      & Tesv(4) , Thikns(5) , Tsigma(8) , Tst(20) , Tstr(50) , Tsub0 , Twopi , Vxcntr , Vycntr , Xpoint(2) , Z(1)
   INTEGER Comps , Elid , Extrnl(8) , Fdest , Flag , Icount , Idum(2) , Intz(1) , Iorder(8) , Ipcmp , Ipcmp1 , Ipcmp2 , Ipid ,      &
         & Istres , Ivec , Ivecn , Kforce , Kpoint , Ksdrde(141) , Ksil(8) , Kstrs , Ksystm(60) , Ldtemp , Nfors(46) , Nout ,       &
         & Npcmp , Npcmp1 , Npcmp2 , Nphi(2395) , Nstot , Nstrop , Oef1l , Oes1l , Sdest
   LOGICAL Ostrai
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /blank / App , Sort2 , Idum , Comps , Skp , Ostrai
   COMMON /condas/ Pi , Twopi , Raddeg , Degrad
   COMMON /sdr2c1/ Ipcmp , Npcmp , Ipcmp1 , Npcmp1 , Ipcmp2 , Npcmp2 , Nstrop
   COMMON /sdr2de/ Ksdrde
   COMMON /sdr2x2/ Dumm , Oes1l , Oef1l
   COMMON /sdr2x4/ Dummy , Ivec , Ivecn , Ldtemp
   COMMON /sdr2x7/ Phiout
   COMMON /sdr2x8/ Sigma , Icount , Nstot , Thikns , Istres , Kpoint , Extrnl , Tstr , Xpoint , Shpfnc , Epsln , Khit , G2alfb ,    &
                 & Tst , Tes , Tesu , Tesv , Reali , Gt , Epslnt , Tsigma , Signx , Signy , Vxcntr , Vycntr , Fxcntr , Fycntr ,     &
                 & Fxycnt , Strx , Stry , Strs , Forsul
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Z
   REAL abbd(6,6) , alpha(3) , c , c2 , const , delt , delta(48) , deltat(8) , detg2 , detrm , dumc(6) , ei(2) , epsa , epsavg(6) , &
      & epsb(3) , epse(3) , epslne(3) , epss , epst(3) , epstot(3) , eta , ezerot(6) , fb(2) , fbmax , fbond , fibre , fimax ,      &
      & findex , fpmax , g2(9) , g3(4) , gdum , mintr , mther(6) , offset , proj , s , s2 , sb , sigavg , sigma1 , sigma2 , sigyp , &
      & stiff(36) , stres(86) , strese(3) , stresl(3) , strnb(3) , strnbc(3) , strnt(3) , strntc(3) , taumax , tbar , tdelta(6) ,   &
      & theta , thetae , thick , ti , tlam , tmean , tmi(9) , tprime , trans(9) , trnar(2) , trnshr(2) , tstb(5,5) , tstn(50) ,     &
      & tstt(5,5) , tsubi , tsubo , txy2 , u(36) , ultstn(6) , v(2) , x1 , x2 , xi , xn11 , xn22 , xx
   INTEGER andf
   LOGICAL bendng , compos , debug , extrm , four , grids , intgs , layer , maxsh , pcmp , pcmp1 , pcmp2 , snrvrx , snrvry ,        &
         & tempp1 , tempp2 , trnflx , vonms
   INTEGER center , elemid , fthr , i , i1 , i199 , i2 , i3 , i99 , iav , ichk , icontr , idelt , idpont , ie , ieps , iet , iflag ,&
         & ifor , iforce , ig2 , ig21 , ig22 , ig2a , ig2ab , igrid(5) , ii , ijj , ik , ikk , ikkn , indx(6,3) , indxg2(3,3) ,     &
         & inplan , inpln1 , ip , ipc11 , ipc21 , ipn(5) , ipoint , ir , is , ish , isig , ising , isngg2 , ist , it , itb , its ,  &
         & itype , ix , ixtr , iz , iz1 , izta , j , j1 , j2 , j3 , j4 , jdelt , jg2 , jg22 , js , jxtr , k , kdelt , kk , ksh ,    &
         & kshp , ksig , lamopt , ldelt , li , lk , ll , logz12 , lpc11 , lpcomp
   INTEGER mem , mm , ndof , nforce , nlay , nlayer , nn , nnode , nstres(86) , nstrqt , pcomp , pcomp1 , pcomp2 , pidloc , plyid , &
         & souti , strain , sym , symmem
   REAL z1(5) , z2(5) , zbar(2) , zeta , zk , zk1 , zref , zsubi
   EXTERNAL andf
!
!     PHASE 2 STRESS RECOVERY FOR 4-NODE ISOPARAMETRIC QUADRILATERAL
!     SHELL ELEMENT (QUAD4)
!
!     NOTE - FOR LAMINATED COMPOSITE ELEMENTS THE FOLLOWING ARE
!            NOT SUPPORTED
!
!         1. VARIABLE GRID POINT THICKNESS
!         3. TEMPERATURE AT 'FIBRE' DISTANCE
!
!         ALSO STRESSES ARE ONLY EVALUATED AT THE ELEMENT CENTRE
!         AND SIMILARILY FOR STRESS RESULTANTS
!
!
!     ALGORITHM -
!
!     1- STRAIN RECOVERY DATA IS SENT BY PHASE 1 THRU 'PHIOUT',
!        WHICH INCLUDES ALL THE NECESSARY TRANSFORMATIONS AND
!        STRAIN RECOVERY MATRICES. THE DATA IS REPEATED FOR EACH
!        STRESS EVALUATION POINT.
!     2- GLOBAL DISPLACEMENT VECTOR ENTERS THE ROUTINE IN CORE.
!     3- BASED ON THE DATA IN /SDR2X4/, LOCATION OF THE GLOBAL
!        DISPLACEMENT VECTOR FOR THE CURRENT SUBCASE IS DETERMINED.
!     4- WORD 132 OF /SDR2DE/ CONTAINS THE STRESS OUTPUT REQUEST
!        OPTION FOR THE CURRENT SUBCASE.
!     5- ELEMENT/GRID POINT TEMPERATURE DATA ENTERS THE ROUTINE
!        THRU /SDR2DE/ (POSITIONS 97-103, 104-129 NOT USED.)
!     6- ELEMENT STRAINS ARE CALCULATED, CORRECTED FOR THERMAL
!        STRAINS, AND PREMULTIPLIED BY G-MATRIX.
!
!WKBNB NCL93012 3/94
!WKBNE NCL93012 3/94
!WKBR 3/95 SPR94017 2  INDXG2(3,3),INDX(6,3),OPRQST,FLAG,IPN(5),COMPS,
!    5,               GPSTRS,INDEXU(3,3),INDEXV(2,3)
!WKBR NCL93012 3/94      COMMON /BLANK / APP(2),SORT2,IDUM(2),COMPS
   !>>>>EQUIVALENCE (Z(1),Intz(1)) , (Nfors(1),Forsul(1)) , (Nphi(1),Phiout(1)) , (nstres(1),stres(1)) , (Elid,Nphi(1)) ,                &
!>>>>    & (Ksil(1),Nphi(2)) , (Tsub0,Phiout(18)) , (Iorder(1),Nphi(10)) , (Avgthk,Phiout(21)) , (Mominr,Phiout(22)) , (G(1),Phiout(23)) &
!>>>>    & , (Alfam(1),Phiout(59)) , (Gpth(1),Phiout(65)) , (Alfab(1),Phiout(62)) , (Ipid,Nphi(79)) , (Kstrs,Ksdrde(42)) ,               &
!>>>>    & (Kforce,Ksdrde(41)) , (Stemp(1),Ksdrde(97)) , (Sdest,Ksdrde(26)) , (Fdest,Ksdrde(33)) , (Nout,Ksystm(2)) , (Stemp(7),Flag)
!    1,               (INDEXU(1,1),INDEXV(1,1))
   DATA debug/.FALSE./
   DATA center/4HCNTR/
   DATA const/0.57735026918962/
   DATA epss/1.0E-11/
   DATA epsa/1.0E-7/
   DATA ipn/1 , 4 , 2 , 3 , 5/
   DATA pcomp/0/
   DATA pcomp1/1/
   DATA pcomp2/2/
   DATA sym/1/
   DATA mem/2/
   DATA symmem/3/
   DATA strain/5/
!
!     DEFINE PHIOUT(2395), THE TRANSMITTED DATA BLOCK
!
!     ADDRESS     DESCRIPTIONS
!
!        1        ELID
!      2 - 9      SIL NUMBERS
!     10 - 17     IORDER
!       18        TREF
!     19 - 20     FIBRE DISTANCES Z1, Z2 AS SPECIFIED ON PSHELL CARD
!       21        AVGTHK- AVERAGE THICKNESS OF THE ELEMENT
!       22        MOMINR- MOMENT OF INERTIA FACTOR
!     23 - 58     GBAR-MATRIX, 6X6 MATRIX OF MATERIAL PROPERTY (W/O G3)
!     59 - 61     THERMAL EXPANSION COEFFICIENTS FOR MEMBRANE
!     62 - 64     THERMAL EXPANSION COEFFICIENTS FOR BENDING
!     65 - 68     CORNER NODE THICKNESSES
!     69 - 77     TUM-MATRIX, 3X3 TRANSFORMATION FROM MATERIAL TO USER
!                 DEFINED COORDINATE SYSTEM
!       78        OFFSET OF ELEMENT FROM GP PLANE
!       79        ORIGINAL PROPERTY ID FOR COMPOSITES
!     80 - 79+9*NNODE
!                 TEG-MATRIX, A 3X3 MATRIX FOR THE TRANSFORMATION
!                 MATRIX FROM GLOBAL COORD TO ELMT COORD FOR
!                 EACH NODE.
!                 TEG-MATRIX, 3X3 DATA ARE REPEATED FOR NNODES
!     --------
!     START FROM PHIOUT(79+9*NNODE+1) AS A REFERENCE ADDRESS
!                       79+9*4    +1= 116
!
!     ADDRESS     DESCRIPTIONS
!
!        1        T, MEMBRANE THICKNESS AT THIS EVALUATION POINT
!      2 - 10     TES-MATRIX, A 3X3 TRANSFORMATION MATRIX FROM ELEM.
!                        C.S. TO USER DEFINED STRESS C.S. AT THIS
!                        EVALUATION POINT
!     11 - 19     CORRECTION TO GBAR-MATRIX FOR MEMBRANE-BENDING
!                        COUPLING AT THIS EVALUATION POINT
!     20 - 28     TMI-MATRIX, 3X3 TRANSFORMATION FROM TANGENT TO MATERIA
!     29 - 32     G3-MATRIX
!     33 - 32+NNODE
!                 ELEMENT SHAPE FUNCTION VALUES AT THIS EVAL. POINT
!     32+NNODE+1 -
!     32+NNODE+8*NDOF
!                 B-MATRIX, 8 X NDOF
!
!     --------    ABOVE DATA BATCH REPEATED 10 TIMES
!
!     TOTAL PHIOUT WORDS = (116-1) + (32+4+8*(6*4))*10
!                        =    115  + (32+4+192)*10 = 115 + 2280 = 2395
!
!
!     DEFINE STRES (TOTAL OF 86 WORDS), THE STRESS OUTPUT DATA BLOCK
!
!     ADDRESS     DESCRIPTIONS
!
!        1        ELID
!     -------------------------------------------------------
!        2        INTEGRATION POINT NUMBER
!     3  - 10     STRESSES FOR LOWER POINTS
!     11 - 18     STRESSES FOR UPPER POINTS
!     ---------   ABOVE DATA REPEATED 4 TIMES
!     70 - 86     STRESSES FOR CENTER POINT
!
!     DEFINE FORSUL (TOTAL OF 46 WORDS), THE FORCE RESULTANT OUTPUT
!     DATA BLOCK.
!
!     ADDRESS    DESCRIPTIONS
!
!        1       ELID
!     ------------------------------------------------
!        2       GRID POINT NUMBER
!      3 - 10    FORCES
!     --------   ABOVE DATA REPEATED 4 TIMES
!     38 - 46    FORCES FOR CENTER POINT
!
!     NSTOT  = NUMBER OF DATA OUTPUT THRU 'STRES'
!     NFORCE = NUMBER OF DATA OUTPUT THRU 'FORSUL'
!     NNODE  = TOTAL NUMBER OF NODES
!     NDOF   = TOTAL NUMBER OF DEGREES OF FREEDOM
!     LDTEMP = SWITCH TO DETERMINE IF THERMAL EFFECTS ARE PRESENT
!     ICOUNT = POINTER FOR PHIOUT DATA
!
!     STAGE 1 -  INITIALIZATION
!     =========================
!
!WKBNB 3/95 SPR94017
   DO i = 1 , 6
      epsavg(i) = 0.
   ENDDO
!WKBNE 3/95 SPR94017
   Nstot = 1 + 5 + 5*2*8
   nforce = 1 + 5*9
   nnode = 0
   DO ichk = 1 , 8
      IF ( Ksil(ichk)>0 ) nnode = nnode + 1
      Extrnl(ichk) = 0
   ENDDO
   ndof = 6*nnode
   four = nnode==4
!
!     COMMENTS FROM G.C. 2/1990
!     EXTRNL ARE SET TO ZEROS ABOVE AND NEVER SET TO ANY VALUE LATER.
!     IT IS THEN USED TO SET IGRID. WHAT'S EXTRNL FOR?
!     THE ANSWER IS THAT EXTRNL AND IGRID ARE USED ONLY WHEN GRIDS FLAG
!     IS TRUE. GRIDS IS FALSE IN COSMIC VERSION.
!
!     ALSO, A MISSING ROUTINE, FNDGID, SUPPOSELY RETURNS EXTERNAL GRID
!     NUMBER FROM SIL INDEX. FNDGID IS LOCATED A FEW LINES BELOW 80
!
!     CHECK THE OUTPUT AND STRESS REQUEST
!
   grids = .FALSE.
   intgs = .TRUE.
   maxsh = andf(Nstrop,1)==0
   vonms = andf(Nstrop,1)/=0
   extrm = andf(Nstrop,2)==0
   layer = andf(Nstrop,2)/=0
   bendng = Mominr>0.0
!
!     NOTE - MAXSH AND EXTRM ARE NO LONGER USED
!
!     IF LAYERED STRESS/STARIN OUTPUT IS REQUESTED, AND THERE ARE NO
!     LAYERED COMPOSITE DATA, SET LAYER FLAG TO FALSE
!
   IF ( layer .AND. Npcmp+Npcmp1+Npcmp2<=0 ) layer = .FALSE.
!
!     IF LAYERED OUTPUT IS REQUESTED BUT THE CURRENT ELEMENT IS NOT A
!     LAYERED COMPOSITE, SET LAYER FLAG TO FALSE
!
   IF ( layer .AND. Ipid<0 ) layer = .FALSE.
!
!WKBDB 3/95 SPR94017
!      OPRQST = -2
!      IF (KSTRS  .EQ. 1) OPRQST = OPRQST + 1
!      IF (KFORCE .EQ. 1) OPRQST = OPRQST + 2
!WKBI NCL93012 3/94
!      IF ( OSTRAI ) OPRQST = OPRQST + 1
!      IF (OPRQST .EQ.-2) RETURN
!WKBDE 3/95 SPR94017
!WKBI  3/95 SPR94017
   IF ( (Kstrs/=1) .AND. (Kforce/=1) .AND. (.NOT.Ostrai) ) RETURN
!
!     CHECK FOR FIBRE DISTANCES Z1 AND Z2 BEING BLANK
!
   logz12 = -4
   IF ( Nphi(19)==0 ) logz12 = logz12 + 2
   IF ( Nphi(20)==0 ) logz12 = logz12 + 4
!
!     CHECK FOR THE TYPE OF TEMPERATURE DATA
!     NOTES  1- TYPE TEMPP1 ALSO INCLUDES TYPE TEMPP3
!            2- IF NIETHER TYPE IS TRUE, GRID POINT TEMPERATURES
!               ARE PRESENT.
!
   tempp1 = Flag==13
   tempp2 = Flag==2
!
!     CHECK FOR OFFSET AND COMPOSITES
!
   offset = Phiout(78)
   compos = Comps== - 1 .AND. Ipid>0
!
!     ZERO OUT STRESS AND FORCE RESULTANT ARRAYS
!
   DO k = 1 , Nstot
      stres(k) = 0.0
   ENDDO
   DO i = 1 , nforce
      Forsul(i) = 0.0
   ENDDO
   nstres(1) = Elid
   Nfors(1) = Elid
!
!     ZERO OUT THE COPY OF GBAR-MATRIX TO BE USED BY THIS ROUTINE
!
   DO k = 1 , 36
      Gt(k) = 0.0
   ENDDO
!
!     STAGE 2 - ARRANGEMENT OF INCOMING DATA
!     ======================================
!
!     SORT THE GRID TEMPERATURE CHANGES INTO SIL ORDER (IF PRESENT)
!
   IF ( Ldtemp/=-1 ) THEN
      IF ( .NOT.(tempp1 .OR. tempp2) ) THEN
!
!     DO 50 K = 1,NNODE
!     KPOINT = IORDER(K)
!  50 DELTAT(K) = STEMP(KPOINT)
!
!     COMMENTS FORM G.CHAN/UNISYS  2/93
!     THE ABOVE DO 50 LOOP DOES NOT WORK SINCE STEMP(2 THRU NNODE) = 0.0
!
         DO k = 1 , nnode
            deltat(k) = Stemp(1)
         ENDDO
      ENDIF
   ENDIF
!
!     PICK UP THE GLOBAL DISPLACEMENT VECTOR AND TRANSFORM IT
!     INTO THE ELEMENT C.S.
!
   DO idelt = 1 , nnode
      jdelt = Ivec + Ksil(idelt) - 2
      kdelt = 6*(idelt-1)
      DO ldelt = 1 , 6
         tdelta(ldelt) = Z(jdelt+ldelt)
      ENDDO
!
!     FETCH TEG-MATRIX 3X3 FOR EACH NODE AND LOAD IT IN A 6X6 MATRIX
!     INCLUDE THE EFFECTS OF OFFSET
!
      CALL tldrs(offset,idelt,Phiout(80),u)
      CALL gmmats(u,6,6,0,tdelta,6,1,0,delta(kdelt+1))
   ENDDO
!
!     GET THE EXTERNAL GRID POINT ID NUMBERS FOR CORRESPONDING SIL
!     NUMBERS.
!
!     CALL FNDGID (ELID,8,KSIL,EXTRNL)
!
!     STAGE 3 - CALCULATION OF STRAINS
!     ================================
!
!     INTEGRATION DATA IN PHIOUT IS ARRANGED IN ETA, XI INCREASING
!     SEQUENCE.
!
   isig = 1
   Icount = -(8*ndof+nnode+32) + 79 + 9*nnode
!
   DO inplan = 1 , 5
      inpln1 = ipn(inplan)
!
!     MATCH GRID ID NUMBER WHICH IS IN SIL ORDER
!
      IF ( inplan==5 ) THEN
!
         igrid(inplan) = center
      ELSE
         DO i = 1 , nnode
            IF ( Iorder(i)==inpln1 ) THEN
               igrid(inplan) = Extrnl(i)
               EXIT
            ENDIF
         ENDDO
      ENDIF
!
      DO izta = 1 , 2
         zeta = (izta*2-3)*const
!
         Icount = Icount + 8*ndof + nnode + 32
         IF ( izta/=2 ) THEN
!
!     THICKNESS AND MOMENT OF INERTIA AT THIS POINT
!
            Thikns(inplan) = Phiout(Icount+1)
            IF ( grids .AND. inplan/=5 ) Thikns(inplan) = Gpth(inpln1)
            Reali(inplan) = Mominr*Thikns(inplan)**3/12.0
!
!     DETERMINE FIBER DISTANCE VALUES
!
            IF ( logz12==-4 ) THEN
!
               z1(inplan) = Phiout(19)
               z2(inplan) = Phiout(20)
            ELSEIF ( logz12<0 ) THEN
!
               z1(inplan) = -0.5*Thikns(inplan)
               z2(inplan) = Phiout(20)
            ELSEIF ( logz12==0 ) THEN
!
               z1(inplan) = Phiout(19)
               z2(inplan) = 0.5*Thikns(inplan)
            ELSE
!
               z1(inplan) = -0.5*Thikns(inplan)
               z2(inplan) = -z1(inplan)
            ENDIF
         ENDIF
!
!     FIRST COMPUTE LOCAL STRAINS UNCORRECTED FOR THERMAL STRAINS
!     AT THIS EVALUATION POINT.
!
!        EPSLN  = PHIOUT(KSIG) * DELTA
!          EPS  =       B      *   U
!          8X1        8XNDOF    NDOFX1
!
         ksig = Icount + nnode + 33
         CALL gmmats(Phiout(ksig),8,ndof,0,delta(1),ndof,1,0,Epsln)
!
!     CALCULATE THERMAL STRAINS IF TEMPERATURES ARE PRESENT
!
         IF ( Ldtemp/=-1 ) THEN
            DO iet = 1 , 6
               Epslnt(iet) = 0.0
            ENDDO
!
!     A) MEMBRANE STRAINS
!
            IF ( tempp1 .OR. tempp2 ) THEN
!
!     ELEMENT TEMPERATURES
!
               tbar = Stemp(1)
            ELSE
!
!     GRID TEMPERATURES
!
               kshp = Icount + 32
               tbar = 0.0
               DO ish = 1 , nnode
                  ksh = kshp + ish
                  tbar = tbar + Phiout(ksh)*deltat(ish)
               ENDDO
               tmean = tbar
            ENDIF
            tbar = tbar - Tsub0
            DO ieps = 1 , 3
               Epslnt(ieps) = -tbar*Alfam(ieps)
            ENDDO
!
!     B) BENDING STRAINS (ELEMENT TEMPERATURES ONLY)
!
            IF ( bendng ) THEN
               IF ( tempp1 .OR. tempp2 ) THEN
!
!     EXTRACT G2-MATRIX FROM GBAR-MATRIX AND CORRECT IT FOR COUPLING
!
                  ig21 = 0
                  DO ig2 = 1 , 3
                     ig22 = (ig2-1)*6 + 21
                     DO jg2 = 1 , 3
                        ig21 = ig21 + 1
                        jg22 = jg2 + ig22
                        g2(ig21) = G(jg22) + Phiout(Icount+10+ig21)
                     ENDDO
                  ENDDO
!
                  ig2ab = (isig*3)/5 + 1
                  CALL gmmats(g2,3,3,0,Alfab,3,1,0,G2alfb(ig2ab))
!
                  IF ( tempp1 ) THEN
!
                     tprime = Stemp(2)
                     DO ieps = 4 , 6
                        Epslnt(ieps) = -tprime*Alfab(ieps-3)*zeta*Thikns(inplan)/2.
                     ENDDO
                  ELSE
                     CALL invers(3,g2,3,gdum,0,detg2,isngg2,indxg2)
                     CALL gmmats(g2,3,3,0,Stemp(2),3,1,0,Khit)
                     DO ieps = 4 , 6
                        Epslnt(ieps) = Khit(ieps-3)*zeta*Thikns(inplan)/(2.*Reali(inplan))
                     ENDDO
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
!
!     MODIFY GBAR-MATRIX
!
         i1 = -6
         i2 = 12
         i3 = 11 + Icount
         DO i = 1 , 3
            i1 = i1 + 6
            i2 = i2 + 6
            DO j = 1 , 3
               j1 = j + i1
               j3 = j1 + 3
               j4 = j + i2
               j2 = j4 + 3
               Gt(j1) = G(j1)
               Gt(j2) = G(j2)
               Gt(j3) = G(j3) + Phiout(i3)
               Gt(j4) = G(j4) + Phiout(i3)
               i3 = i3 + 1
            ENDDO
         ENDDO
!
!     DETERMINE G MATRIX FOR THIS EVALUATION POINT
!
         DO i = 1 , 4
            g3(i) = Phiout(Icount+28+i)
         ENDDO
!
         IF ( Ldtemp/=-1 ) THEN
!
!     CORRECT STRAINS FOR THERMAL EFFECTS
!
            DO i = 1 , 6
               Epsln(i) = Epsln(i) + Epslnt(i)
            ENDDO
         ENDIF
!
!     CALCULATE STRESS VECTOR
!
         CALL gmmats(Gt(1),6,6,0,Epsln(1),6,1,0,Tsigma(1))
         CALL gmmats(g3(1),2,2,0,Epsln(7),2,1,0,Tsigma(7))
!WKBNB NCL93012 3/94
         IF ( izta==1 ) THEN
            DO iav = 1 , 3
               epsavg(iav) = epsavg(iav) + Epsln(iav)
            ENDDO
            DO iav = 4 , 6
               epsavg(iav) = epsavg(iav) + Epsln(iav)/const
            ENDDO
         ENDIF
!WKBNE NCL93012 3/94
         IF ( bendng ) THEN
!
!     COMBINE STRESSES ONLY IF 'BENDING'
!
            DO i = 1 , 3
               Tsigma(i) = Tsigma(i+3)
            ENDDO
         ENDIF
!
!
!     TRANSFORM STRESSES FROM ELEMENT TO STRESS C.S.
!
         DO i = 1 , 9
            Tes(i) = Phiout(Icount+1+i)
         ENDDO
!
         Tesu(1) = Tes(1)*Tes(1)
         Tesu(2) = Tes(4)*Tes(4)
         Tesu(3) = Tes(1)*Tes(4)
         Tesu(4) = Tes(2)*Tes(2)
         Tesu(5) = Tes(5)*Tes(5)
         Tesu(6) = Tes(2)*Tes(5)
         Tesu(7) = Tes(1)*Tes(2)*2.0
         Tesu(8) = Tes(4)*Tes(5)*2.0
         Tesu(9) = Tes(1)*Tes(5) + Tes(2)*Tes(4)
!
         CALL gmmats(Tesu(1),3,3,1,Tsigma(1),3,1,0,Tstr(isig))
!
         Tesv(1) = Tes(5)*Tes(9) + Tes(6)*Tes(8)
         Tesv(2) = Tes(2)*Tes(9) + Tes(8)*Tes(3)
         Tesv(3) = Tes(4)*Tes(9) + Tes(7)*Tes(6)
         Tesv(4) = Tes(1)*Tes(9) + Tes(3)*Tes(7)
!
         isig = isig + 3
         CALL gmmats(Tesv(1),2,2,1,Tsigma(7),2,1,0,Tstr(isig))
!
         isig = isig + 2
      ENDDO
   ENDDO
!
!     IF REQUIRED, EXTRAPOLATE STRESSES FROM INTEGRATION POINTS
!     TO CORNER POINTS.
!
!     FIRST EXTRAPOLATE ACROSS ZETA, REGARDLESS OF INPLANE REQUEST
!
   DO ikk = 1 , 5
      itb = (ikk-1)*10
      DO ijj = 1 , 5
         tstb(ikk,ijj) = Tstr(itb+ijj)
         tstt(ikk,ijj) = Tstr(itb+5+ijj)
      ENDDO
   ENDDO
!
   x1 = -const
   x2 = -x1
!
   DO k = 1 , 2
      ik = 0
      xx = -1.0
      IF ( k==2 ) xx = -xx
      IF ( k==2 ) ik = 5
!
      xn22 = (xx-x1)/(x2-x1)
      xn11 = 1.0 - xn22
!
      DO i = 1 , 5
         ikkn = (i-1)*10 + ik
         DO j = 1 , 5
            tstn(ikkn+j) = tstb(i,j)*xn11 + tstt(i,j)*xn22
         ENDDO
      ENDDO
   ENDDO
!
   DO ii = 1 , 50
      Tstr(ii) = tstn(ii)
   ENDDO
!
   IF ( .NOT.(intgs .OR. compos) ) THEN
!
      ixtr = 5
      jxtr = ixtr*4
!
      iz1 = 0
      DO iz = 1 , 2
!
         DO i = 1 , jxtr
            Tst(i) = 0.0
         ENDDO
!
!     FOR THE SAKE OF COMPATIBILITY BETWEEN THE CONVENTION FOR
!     SHEAR FORCES, AND THE CONVENTION FOR EXTRAPOLATION, WE MAY
!     HAVE TO CHANGE THE SIGNS AROUND FOR SPECIFIC POINTS. THEY
!     WILL BE RETURNED TO THE ORIGINAL SIGNS AFTER EXTRAPOLATION IS
!     COMPLETE.
!
!WKBR 3/95 SPR94017      IF (OPRQST .LT. 0) GO TO 460
         IF ( Kforce==1 ) THEN
            DO i = 1 , 4
               j = (i-1)*2*ixtr + iz1 + 4
               IF ( Tstr(j)==0.0 ) THEN
                  Signy(i) = 0.0
               ELSE
                  Signy(i) = Tstr(j)/abs(Tstr(j))
               ENDIF
               IF ( Tstr(j+1)==0.0 ) THEN
                  Signx(i) = 0.0
               ELSE
                  Signx(i) = Tstr(j+1)/abs(Tstr(j+1))
               ENDIF
            ENDDO
!
            snrvry = .FALSE.
            IF ( Signy(1)*Signy(2)<=0.0 .OR. Signy(3)*Signy(4)<=0.0 .OR. Signy(3)*Signy(1)<=0.0 ) snrvry = .TRUE.
            snrvrx = .FALSE.
            IF ( Signx(1)*Signx(2)<=0.0 .OR. Signx(3)*Signx(4)<=0.0 .OR. Signx(3)*Signx(1)<=0.0 ) snrvrx = .TRUE.
!
            IF ( snrvry ) THEN
               Tstr(iz1+4) = -Tstr(iz1+4)
               Tstr(iz1+4+4*ixtr) = -Tstr(iz1+4+4*ixtr)
            ENDIF
            IF ( snrvrx ) THEN
               Tstr(iz1+5) = -Tstr(iz1+5)
               Tstr(iz1+5+2*ixtr) = -Tstr(iz1+5+2*ixtr)
            ENDIF
         ENDIF
!
         Xpoint(1) = -1.0
         Xpoint(2) = +1.0
         ir = 0
!
         DO ix = 1 , 2
            xi = Xpoint(ix)
!
            DO ie = 1 , 2
               eta = Xpoint(ie)
!
               Shpfnc(1) = 0.75*(const-xi)*(const-eta)
               Shpfnc(2) = 0.75*(const-xi)*(const+eta)
               Shpfnc(3) = 0.75*(const+xi)*(const-eta)
               Shpfnc(4) = 0.75*(const+xi)*(const+eta)
!
               li = ir*ixtr
               ir = ir + 1
!
               DO is = 1 , 4
                  lk = (is-1)*2*ixtr + iz1
!
                  DO it = 1 , ixtr
                     Tst(li+it) = Tst(li+it) + Shpfnc(is)*Tstr(lk+it)
                  ENDDO
               ENDDO
            ENDDO
         ENDDO
!
         j1 = 0
         DO is = 1 , 4
            j2 = (is-1)*2*ixtr + iz1
            DO js = 1 , ixtr
               j1 = j1 + 1
               j2 = j2 + 1
               Tstr(j2) = Tst(j1)
            ENDDO
         ENDDO
!
!     CHANGE THE SIGNS BACK, IF NECESSARY
!
!WKBR 3/95 SPR94017     IF (OPRQST .LT. 0) GO TO 520
         IF ( Kforce==1 ) THEN
            IF ( snrvry ) THEN
               Tstr(iz1+4) = -Tstr(iz1+4)
               Tstr(iz1+4+4*ixtr) = -Tstr(iz1+4+4*ixtr)
            ENDIF
            IF ( snrvrx ) THEN
               Tstr(iz1+5) = -Tstr(iz1+5)
               Tstr(iz1+5+2*ixtr) = -Tstr(iz1+5+2*ixtr)
            ENDIF
         ENDIF
         iz1 = iz1 + ixtr
      ENDDO
   ENDIF
!
!     STAGE 4 - CALCULATION OF OUTPUT STRESSES
!     ========================================
!
!WKBR 3/95 SPR94017     IF (OPRQST .EQ. 0) GO TO 740
   IF ( .NOT.((Kstrs/=1) .AND. (.NOT.Ostrai)) ) THEN
!
!WKBNB NCL93012 3/94
      DO iav = 1 , 3
         epsavg(iav) = epsavg(iav)/5.
      ENDDO
      DO iav = 4 , 6
         epsavg(iav) = epsavg(iav)/(5.*Phiout(21)/2.)
      ENDDO
!WKBNE NCL93012 3/94
      isig = 0
      ig2a = 0
      Strx(1) = 0.0
      Strx(2) = 0.0
      Stry(1) = 0.0
      Stry(2) = 0.0
      Strs(1) = 0.0
      Strs(2) = 0.0
      DO inplan = 1 , 5
         inpln1 = inplan
         IF ( inplan==2 ) inpln1 = 4
         IF ( inplan==3 ) inpln1 = 2
         IF ( inplan==4 ) inpln1 = 3
!
         Istres = (inpln1-1)*17 + 2
!
         idpont = igrid(inplan)
         IF ( intgs ) idpont = inpln1
         IF ( intgs .AND. inplan==5 ) idpont = center
         nstres(Istres) = idpont
         thick = Thikns(inplan)
!
         DO iz = 1 , 2
            IF ( iz==2 ) Istres = Istres + 8
            fibre = z1(inplan)
            IF ( iz==2 ) fibre = z2(inplan)
!WKBNB NCL93012 3/94
            IF ( .NOT.Ostrai ) THEN
!WKBNE NCL93012 3/94
               stres(Istres+1) = fibre
!
!     EVALUATE STRESSES AT THIS FIBRE DISTANCE
!
               DO i = 1 , 3
                  Sigma(i) = (0.5-fibre/thick)*Tstr(isig+i) + (0.5+fibre/thick)*Tstr(isig+i+5)
               ENDDO
!
!     IF TEMPERATURES ARE PRESENT, CORRECT STRESSES FOR THERMAL
!     STRESSES ASSOCIATED WITH THE DATA RELATED TO FIBRE DISTANCES.
!
               IF ( Ldtemp/=-1 ) THEN
!
!     IF NO BENDING, TREAT IT LIKE GRID POINT TEMPERATURES
!
                  IF ( bendng ) THEN
                     IF ( tempp1 ) THEN
!
                        tsubi = Stemp(2+iz)
                        IF ( abs(tsubi)<epss ) GOTO 5
                        tsubi = tsubi - tprime*fibre
                     ELSE
                        IF ( .NOT.(tempp2) ) GOTO 5
!
                        tsubi = Stemp(4+iz)
                        IF ( abs(tsubi)<epss ) GOTO 5
                        DO ist = 1 , 3
                           Sigma(ist) = Sigma(ist) - Stemp(ist+1)*fibre/Reali(inplan)
                        ENDDO
                     ENDIF
                     tsubi = tsubi - tbar
                     DO its = 1 , 3
                        Sigma(its) = Sigma(its) - tsubi*G2alfb(ig2a+its)
                     ENDDO
                  ENDIF
               ENDIF
!
!     AVERAGE THE VALUES FROM OTHER 4 POINTS FOR THE CENTER POINT
!
 5             IF ( inplan==5 ) THEN
                  Sigma(1) = Strx(iz)
                  Sigma(2) = Stry(iz)
                  Sigma(3) = Strs(iz)
               ELSE
                  Strx(iz) = Strx(iz) + 0.25*Sigma(1)
                  Stry(iz) = Stry(iz) + 0.25*Sigma(2)
                  Strs(iz) = Strs(iz) + 0.25*Sigma(3)
               ENDIF
            ELSEIF ( iz/=1 ) THEN
               nstres(Istres+1) = -1
               Sigma(1) = epsavg(4)
               Sigma(2) = epsavg(5)
               Sigma(3) = epsavg(6)
            ELSE
               nstres(Istres+1) = 0
               Sigma(1) = epsavg(1)
               Sigma(2) = epsavg(2)
               Sigma(3) = epsavg(3)
            ENDIF
            DO is = 1 , 3
               stres(Istres+1+is) = Sigma(is)
            ENDDO
!
!     CALCULATE PRINCIPAL STRESSES
!
            sigavg = 0.5*(Sigma(1)+Sigma(2))
            proj = 0.5*(Sigma(1)-Sigma(2))
            taumax = proj*proj + Sigma(3)*Sigma(3)
!WKBNB 7/94 SPR94004
            IF ( Ostrai ) THEN
               taumax = proj*proj + Sigma(3)*Sigma(3)/4.
               taumax = sqrt(taumax)
!WKBNE 7/94 SPR94004
            ELSEIF ( abs(taumax)<=epss ) THEN
               taumax = 0.0
            ELSE
!WKBI  7/94 SPR94004
               taumax = sqrt(taumax)
            ENDIF
!
!     PRINCIPAL ANGLE
!
            txy2 = Sigma(3)*2.0
            proj = proj*2.0
            IF ( abs(txy2)<=epsa .AND. abs(proj)<=epsa ) THEN
               stres(Istres+5) = 0.0
            ELSE
               stres(Istres+5) = 28.647890*atan2(txy2,proj)
            ENDIF
            sigma1 = sigavg + taumax
            sigma2 = sigavg - taumax
            stres(Istres+6) = sigma1
            stres(Istres+7) = sigma2
!
!     OUTPUT VON MISES YIELD STRESS IF ASKED FOR BY THE USER
!
            IF ( vonms ) THEN
!
               sigyp = sigma1*sigma1 + sigma2*sigma2 - sigma1*sigma2
               IF ( abs(sigyp)<=epss ) THEN
                  sigyp = 0.0
               ELSE
                  sigyp = sqrt(sigyp)
               ENDIF
               stres(Istres+8) = sigyp
            ELSE
               stres(Istres+8) = taumax
!WKBI NCL93012 3/94
               IF ( Ostrai ) stres(Istres+8) = 2.*taumax
            ENDIF
!
            ig2a = ig2a + 3
         ENDDO
         isig = isig + 10
      ENDDO
!WKBNB NCL93012 3/94
      DO iav = 1 , 6
         epsavg(iav) = 0.
      ENDDO
   ENDIF
!WKBNE NCL93012 3/94
!
!     STAGE 5 - ELEMENT FORCE OUTPUT
!     ==============================
!
   IF ( .NOT.(layer) ) THEN
!WKBR 3/95 SPR94017     IF (OPRQST .LT. 0) GO TO 790
      IF ( Kforce/=1 ) GOTO 100
   ENDIF
!
   isig = 0
   Vxcntr = 0.0
   Vycntr = 0.0
   Fxcntr = 0.0
   Fycntr = 0.0
   Fxycnt = 0.0
   DO inplan = 1 , 5
      inpln1 = inplan
      IF ( inplan==2 ) inpln1 = 4
      IF ( inplan==3 ) inpln1 = 2
      IF ( inplan==4 ) inpln1 = 3
      thick = Thikns(inplan)
!
      iforce = (inpln1-1)*9 + 2
!
      idpont = igrid(inplan)
      IF ( intgs ) idpont = inpln1
      IF ( intgs .AND. inplan==5 ) idpont = center
      Nfors(iforce) = idpont
!
!     CALCULATE FORCES AT MID-SURFACE LEVEL
!
      DO ifor = 1 , 3
         Forsul(iforce+ifor) = (Tstr(isig+ifor)+Tstr(isig+ifor+5))*thick/2.
         Forsul(iforce+ifor+3) = (Tstr(isig+ifor)-Tstr(isig+ifor+5))*Reali(inplan)/thick
      ENDDO
!
!     INTERCHANGE 7 AND 8 POSITIONS TO BE COMPATIBLE WITH THE
!     OUTPUT FORMAT OF VX AND VY (WE HAVE CALCULATED VY AND VX)
!
      IF ( inplan==5 ) THEN
         Forsul(iforce+1) = Fxcntr
         Forsul(iforce+2) = Fycntr
         Forsul(iforce+3) = Fxycnt
         Forsul(iforce+7) = Vxcntr
         Forsul(iforce+8) = Vycntr
      ELSE
         Forsul(iforce+7) = (Tstr(isig+5)+Tstr(isig+10))*thick*0.5
         Forsul(iforce+8) = (Tstr(isig+4)+Tstr(isig+9))*thick*0.5
!
!     SUBSTITUTE THE AVERAGE OF CORNER (OR INTEGRATION) POINT
!     MEMBRANE AND SHEAR FORCES FOR THE CENTER POINT
!
         Fxcntr = Fxcntr + Forsul(iforce+1)*0.25
         Fycntr = Fycntr + Forsul(iforce+2)*0.25
         Fxycnt = Fxycnt + Forsul(iforce+3)*0.25
         Vxcntr = Vxcntr + Forsul(iforce+7)*0.25
         Vycntr = Vycntr + Forsul(iforce+8)*0.25
      ENDIF
!
      isig = isig + 10
   ENDDO
!
!     DO NOT WRITE TO PHIOUT IF LAYER STRESSES ARE REQUESTED
!     BECAUSE PHIOUT NEEDS TO BE INTACT
   IF ( layer ) THEN
!
!     ELEMENT LAYER STRESS CALCULATION
!
!     CHECK STRESS AND FORCE OUTPUT REQUEST
!
      IF ( (Kforce/=0 .OR. Kstrs/=0) .AND. .NOT.compos ) THEN
         WRITE (Nout,99001) Ufm
99001    FORMAT (A23,', LAYER STRESS OR FORCE RECOVERY WAS REQUESTED WHILE',' PROBLEM WAS NOT SET UP FOR',/5X,'LAYER COMPUTATION')
         CALL mesage(-61,0,0)
         GOTO 99999
      ELSE
!
!     WRITE FORCE RESULTANTS TO OEF1L IF REQUESTED
!         1.    10*ELEMENT ID + DEVICE CODE (FDEST)
!        2-9.   FORCE RESULTANTS
!               FX, FY, FXY, MX, MY, MXY, VX, VY
!
         IF ( Kforce/=0 ) THEN
            elemid = 10*Elid + Fdest
            IF ( Ldtemp==-1 ) THEN
               CALL write(Oef1l,elemid,1,0)
               CALL write(Oef1l,Forsul(39),8,0)
            ENDIF
         ENDIF
!
         IF ( Kstrs==0 .AND. Ldtemp==-1 ) RETURN
         elemid = 10*Elid + Sdest
!
!     LOCATE PID BY CARRYING OUT A SEQUENTIAL SEARCH
!     OF THE PCOMPS DATA BLOCK, AND ALSO DETERMINE
!     THE TYPE OF 'PCOMP' BULK DATA ENTRY.
!
!     SET POINTER LPCOMP
!
         lpcomp = Ipcmp + Npcmp + Npcmp1 + Npcmp2
!
!
!     POINTER DESCRIPITION
!     --------------------
!     IPCMP  - LOCATION OF START OF PCOMP DATA IN CORE
!     NPCMP  - NUMBER OF WORDS OF PCOMP DATA
!     IPCMP1 - LOCATION OF START OF PCOMP1 DATA IN CORE
!     NPCMP1 - NUMBER OF WORDS OF PCOMP1 DATA
!     IPCMP2 - LOCATION OF START OF PCOMP2 DATA IN CORE
!     NPCMP2 - NUMBER OF WORDS OF PCOMP2 DATA
!
!     ITYPE  - TYPE OF PCOMP BULK DATA ENTRY
!
!     LAMOPT - LAMINATION GENERATION OPTION
!            = SYM  (SYMMETRIC)
!            = MEM  (MEMBRANE )
!            = SYMMEM  (SYMMETRIC-MEMBRANE)
!
!     FTHR   - FAILURE THEORY
!            = 1    HILL
!            = 2    HOFFMAN
!            = 3    TSAI-WU
!            = 4    MAX-STRESS
!            = 5    MAX-STRAIN
!
!     ULTSTN - ULTIMATE STRENGTH VALUES
!
!     SET POINTERS
!
         itype = -1
!
         pcmp = .FALSE.
         pcmp1 = .FALSE.
         pcmp2 = .FALSE.
!
         pcmp = Npcmp>0
         pcmp1 = Npcmp1>0
         pcmp2 = Npcmp2>0
!
!     CHECK IF NO 'PCOMP' DATA HAS BEEN READ INTO CORE
!
         IF ( .NOT.pcmp .AND. .NOT.pcmp1 .AND. .NOT.pcmp2 ) GOTO 800
!
!     SEARCH FOR PID IN PCOMP DATA
!
         IF ( .NOT.pcmp ) GOTO 300
!
         ip = Ipcmp
         IF ( Intz(ip)==Ipid ) THEN
            itype = pcomp
            GOTO 600
         ELSE
            ipc11 = Ipcmp1 - 1
            DO ip = Ipcmp , ipc11
               IF ( Intz(ip)==-1 .AND. ip<(Ipcmp1-1) ) THEN
                  IF ( Intz(ip+1)==Ipid ) GOTO 20
               ENDIF
            ENDDO
            GOTO 300
         ENDIF
!
 20      ip = ip + 1
         itype = pcomp
         GOTO 600
      ENDIF
!
!     STAGE 7 - SHIPPING OF NORMAL STRESSES
!     =====================================
!
!     STORE THE STRESSES WHERE THE HIGHER LEVEL ROUTINES EXPECT
!     TO FIND THEM.
!     BUT FIRST, MOVE THE CENTER POINT STRESSES TO THE TOP.
!
!WKBR 3/95 SPR94017     IF (OPRQST .EQ. 0) GO TO 840
   ELSEIF ( (Kstrs/=1) .AND. (.NOT.Ostrai) ) THEN
      GOTO 200
   ENDIF
 100  Nphi(101) = nstres(1)
   DO i = 3 , 18
      i99 = i + 99
      Nphi(i99) = nstres(i+68)
   ENDDO
!
!     DEBUG PRINTOUT
!
   IF ( debug ) WRITE (Nout,99002) (stres(i),i=71,86)
99002 FORMAT (' SQUD42 - STRESSES',(/1X,8E13.5))
!
   DO i = 19 , 86
      i99 = i + 99
      Nphi(i99) = nstres(i-17)
   ENDDO
!
!     STORE FORCES IN THEIR APPROPRIATE LOCATION
!
!WKBR 3/95 SPR94017     IF (OPRQST .LT. 0) RETURN
   IF ( Kforce/=1 ) RETURN
 200  Nphi(201) = Nfors(1)
   DO i = 3 , 10
      i199 = i + 199
      Nphi(i199) = Nfors(i+36)
   ENDDO
!
!     DEBUG PRINTOUT
!
   IF ( debug ) WRITE (Nout,99003) (Forsul(i),i=39,46)
99003 FORMAT (' SQUD42 - FORCES',(/1X,8E13.5))
!
   DO i = 11 , 46
      i199 = i + 199
      Nphi(i199) = Nfors(i-9)
   ENDDO
!
!     PROCESSING FOR NORMAL STRESS REQUEST COMPLETED
!
   GOTO 700
!
!     SEARCH FOR PID IN PCOMP1 DATA
!
 300  IF ( pcmp1 ) THEN
      ip = Ipcmp1
      IF ( Intz(ip)==Ipid ) THEN
         itype = pcomp1
         GOTO 600
      ELSE
         ipc21 = Ipcmp2 - 1
         DO ip = Ipcmp1 , ipc21
            IF ( Intz(ip)==-1 .AND. ip<(Ipcmp2-1) ) THEN
               IF ( Intz(ip+1)==Ipid ) GOTO 350
            ENDIF
         ENDDO
         GOTO 400
      ENDIF
!
 350  ip = ip + 1
      itype = pcomp1
      GOTO 600
   ENDIF
!
!     SEARCH FOR PID IN PCOMP2 DATA
!
 400  IF ( pcmp2 ) THEN
!
      ip = Ipcmp2
      IF ( Intz(ip)==Ipid ) THEN
         itype = pcomp2
         GOTO 600
      ELSE
         lpc11 = lpcomp - 1
         DO ip = Ipcmp2 , lpc11
            IF ( Intz(ip)==-1 .AND. ip<(lpcomp-1) ) THEN
               IF ( Intz(ip+1)==Ipid ) GOTO 450
            ENDIF
         ENDDO
         GOTO 500
      ENDIF
!
 450  ip = ip + 1
      itype = pcomp2
      GOTO 600
   ENDIF
!
!     CHECK IF PID HAS NOT BEEN LOCATED
!
 500  IF ( itype==-1 ) GOTO 800
!
!     LOCATION OF PID
!
 600  pidloc = ip
   lamopt = Intz(pidloc+8)
!
!     INTILIZE
!
   DO ir = 1 , 3
      strnt(ir) = 0.0
      strnb(ir) = 0.0
   ENDDO
!
!     CALCULATION OF STRAINS
!
!     INTEGRATION DATA IN PHIOUT IS ARRANGED IN ETA,XI INCREASING
!     SEQUENCE.
!
   isig = 1
   Icount = -(8*ndof+nnode+32) + 79 + 9*nnode
!
   DO inplan = 1 , 5
      inpln1 = ipn(inplan)
!
!     MATCH GRID ID NUMBER WHICH IS IN SIL ORDER
!
      IF ( inplan==5 ) THEN
!
         igrid(inplan) = center
      ELSE
         DO i = 1 , nnode
            IF ( Iorder(i)==inpln1 ) THEN
               igrid(inplan) = Extrnl(i)
               EXIT
            ENDIF
         ENDDO
      ENDIF
!
      DO izta = 1 , 2
         zeta = (izta*2-3)*const
!
         Icount = Icount + 8*ndof + nnode + 32
!
!     FIRST COMPUTE LOCAL STRAINS AT THIS EVALUATION POINT
!
!        EPSLN = PHIOUT(KSIG) * DELTA
!          EPS =        B     *   U
!          8X1        8XNDOF    NDOFX1
!
         ksig = Icount + nnode + 33
         CALL gmmats(Phiout(ksig),8,ndof,0,delta(1),ndof,1,0,Epsln)
!
!     TRANSFORM THE STRAINS AT THIS EVALUATION POINT TO THE
!     MATERIAL COORDINATE SYSTEM
!
         DO ir = 1 , 9
            tmi(ir) = Phiout(Icount+19+ir)
         ENDDO
!
!     TOTAL STRAIN AT EVALUATION POINT = MEMBRANE + BENDING
!
         DO ir = 1 , 3
            epstot(ir) = Epsln(ir) + Epsln(ir+3)
         ENDDO
!
!     GENERATE TRANS-MATRIX TO TRANSFORM STRAINS FROM I TO M SYSTEM
!
         trans(1) = tmi(1)*tmi(1)
         trans(2) = tmi(2)*tmi(2)
         trans(3) = tmi(1)*tmi(2)
         trans(4) = tmi(4)*tmi(4)
         trans(5) = tmi(5)*tmi(5)
         trans(6) = tmi(4)*tmi(5)
         trans(7) = 2.0*tmi(1)*tmi(4)
         trans(8) = 2.0*tmi(2)*tmi(5)
         trans(9) = tmi(1)*tmi(5) + tmi(2)*tmi(4)
!
!     TRANSFORM TOTAL STRAINS
!
         CALL gmmats(trans(1),3,3,0,epstot(1),3,1,0,epse(1))
!
         IF ( inplan==5 ) THEN
!
!     TOTAL STRAIN VECTORS AT ELEMENT CENTRE
!
            DO ir = 1 , 3
               IF ( izta==2 ) THEN
                  strntc(ir) = epse(ir)
               ELSE
                  strnbc(ir) = epse(ir)
               ENDIF
            ENDDO
         ELSE
!
!     AVERAGE THE STRAIN VECTORS OF THE FOUR INTGS POINTS AT EACH
!     LEVEL TO CALCULATE THE ELEMENT CENTRE STRAIN VECTOR FOR THE
!     UPPER AND BOTTOM LEVELS.
!
            DO ir = 1 , 3
               IF ( izta==2 ) THEN
                  strnt(ir) = strnt(ir) + 0.25*epse(ir)
               ELSE
                  strnb(ir) = strnb(ir) + 0.25*epse(ir)
               ENDIF
            ENDDO
         ENDIF
!
      ENDDO
   ENDDO
!
!     EXTRAPOLATE STRAINS ACROSS ZETA
!
   DO ir = 1 , 3
      epst(ir) = (strnt(ir)-strnb(ir))*(+1.0+const)/(2.0*const) + strnb(ir)
      epsb(ir) = (strnt(ir)-strnb(ir))*(-1.0+const)/(2.0*const) + strnb(ir)
   ENDDO
!
!     CALCULATE LAYER STRESSES AND FAILURE INDICES (IF REQUESTED)
!     AND WRITE TO THE OUTPUT FILE OES1L
!         1.    10*ELEMENT ID + DEVICE CODE (SDEST)
!         2.    NLAYER - NUMBER OF LAYERS FOR LAMINATE
!         3.    TYPE OF FAILURE THEORY SELECTED
!
!         4.    PLY ID
!       5,6,7.  LAYER STRESSES
!         8.    PLY FAILURE INDEX (FP)
!         9.    IFLAG (= 1 IF FP.GE.0.999, DEFAULT = 0)
!       10,11.  INTERLAMINAR SHEAR STRESSES
!        12.    SHEAR BONDING INDEX (SB)
!        13.    IFLAG (= 1 IF SB.GE.0.999, DEFAULT = 0)
!         :     4 - 13 REPEATED FOR THE NUMBER OF LAYERS WITH
!         :           LAYER STRESS REQUEST
!      LAST-1.  MAXIMUM FAILURE INDEX OF LAMINATE  (FIMAX)
!       LAST.   IFLAG (= 1 IF FIMAX.GE.0.999, DEFAULT = 0)
!
!      1-LAST.  REPEAT FOR NUMBER OF ELEMENTS
!
!       (NOTE - ONLY THE ELEMENT CENTRE VALUES ARE CALCULATED)
!
!     == 1.
!
   IF ( Kstrs==1 ) CALL write(Oes1l,elemid,1,0)
!
!     DETERMINE INTRINSIC LAMINATE PROPERTIES
!
!     LAMINATE THICKNESS
!
   tlam = Phiout(21)
!
!     REFERENCE SURFACE
!
   zref = -tlam/2.0
!
!     NUMBER OF LAYERS
!
   nlay = Intz(pidloc+1)
!
!     FOR PCOMP BULK DATA DETERMINE HOW MANY LAYERS HAVE THE STRESS
!     OUTPUT REQUEST (SOUTI)
!     NOTE - FOR PCOMP1 OR PCOMP2 BULK DATA ENTRIES LAYER
!            STRESSES ARE OUTPUT FOR ALL LAYERS.
!
   nlayer = nlay
!
   IF ( itype==pcomp ) THEN
!
      nstrqt = 0
      DO k = 1 , nlay
         IF ( Intz(pidloc+8+4*k)==1 ) nstrqt = nstrqt + 1
      ENDDO
      nlayer = nstrqt
   ENDIF
!
!     WRITE TOTAL NUMBER OF LAYERS WITH STRESS REQ TO OES1L
!
   IF ( lamopt==sym .OR. lamopt==symmem ) nlayer = 2*nlayer
!
!     == 2.
!
   IF ( Kstrs==1 ) CALL write(Oes1l,nlayer,1,0)
!
!     SET POINTER
!
   IF ( itype==pcomp ) ipoint = pidloc + 8 + 4*nlay
   IF ( itype==pcomp1 ) ipoint = pidloc + 8 + nlay
   IF ( itype==pcomp2 ) ipoint = pidloc + 8 + 2*nlay
!
!     FAILURE THEORY TO BE USED IN COMPUTING FAILURE INDICES
!
   fthr = Intz(pidloc+5)
!
!     WRITE TO OUTPUT FILE TYPE OF FAILURE THEORY SELECTED
!
!     == 3.
!
   IF ( Kstrs==1 ) CALL write(Oes1l,fthr,1,0)
!
!     SHEAR BONDING STRENGTH
!
   sb = Z(pidloc+4)
   findex = 0.0
   fbond = 0.0
   fpmax = 0.0
   fbmax = 0.0
   fimax = 0.0
!
!     SET TRNFLX IF INTERLAMINAR SHEAR STRESS CALCULATIONS
!     IS REQUIRED
!
   trnflx = .FALSE.
!
!     TRANSVERSE SHEAR STRESS RESULTANTS QX AND QY
!
   v(1) = Forsul(45)
   v(2) = Forsul(46)
   trnflx = v(1)/=0.0 .AND. v(2)/=0.0
   IF ( trnflx ) THEN
      IF ( itype==pcomp ) icontr = ipoint + 27*nlay
      IF ( itype==pcomp1 .OR. itype==pcomp2 ) icontr = ipoint + 25 + 2*nlay
!
!     LAMINATE BENDING INERTIA
!
      ei(1) = Z(icontr+1)
      ei(2) = Z(icontr+2)
!
!     LOCATION OF NEUTRAL SURFACE
!
      zbar(1) = Z(icontr+3)
      zbar(2) = Z(icontr+4)
   ENDIF
!
!     INTILIZISE
!
   DO ll = 1 , 2
      trnar(ll) = 0.0
      trnshr(ll) = 0.0
   ENDDO
!
!     ALLOW FOR THE ORIENTATION OF THE MATERIAL AXIS FROM
!     THE USER DEFINED COORDINATE SYSTEM
!
   thetae = acos(Phiout(69))
   thetae = thetae*Degrad
!
!     SWITCH FOR THEMAL EFFECTS
!
   IF ( Ldtemp/=-1 ) THEN
!
!     LAMINATE REFERENCE (OR LAMINATION) TEMPERATURE
!
      tsubo = Z(ipoint+24)
!
!     MEAN ELEMENT TEMPERATURE
!
      tbar = tmean
      IF ( tempp1 .OR. tempp2 ) tbar = Stemp(1)
      IF ( lamopt/=mem .AND. lamopt/=symmem ) THEN
         IF ( tempp1 .OR. tempp2 ) THEN
!
!     TEMPERATURE GRADIENT TPRIME
!
            IF ( tempp1 ) tprime = Stemp(2)
!
            IF ( tempp2 ) THEN
!
!     COMPUTE REFERENCE SURFACE STRAINS AND CURVATURES
!     DUE TO THERMAL MOMENTS
!
!     MOMENT OF INERTIA OF LAMINATE
!
               mintr = (tlam**3)/12.0
!
!     DETERMINE ABBD-MATRIX FROM PHIOUT(23-58)
!
               Icount = 89 + 9*nnode
               DO ll = 1 , 3
                  DO mm = 1 , 3
                     nn = mm + 6*(ll-1)
                     ii = mm + 3*(ll-1)
                     abbd(ll,mm) = Phiout(nn+22)*tlam
                     abbd(ll,mm+3) = Phiout(Icount+ii)*(tlam*tlam)/(-6.0*const)
                     abbd(ll+3,mm) = Phiout(Icount+ii)*(tlam*tlam)/(-6.0*const)
                     abbd(ll+3,mm+3) = Phiout(nn+43)*mintr
                  ENDDO
               ENDDO
!
!     COMPUTE THERMAL REF STRAINS AND CURVATURES
!                                   -1
!        EZEROT-VECTOR =  ABBD-MATRIX   X  MTHR-VECTOR
!
               mther(1) = 0.0
               mther(2) = 0.0
               mther(3) = 0.0
               mther(4) = Stemp(2)
               mther(5) = Stemp(3)
               mther(6) = Stemp(4)
!
               CALL invers(6,abbd,6,dumc,0,detrm,ising,indx)
!
               DO ll = 1 , 6
                  DO mm = 1 , 6
                     nn = mm + 6*(ll-1)
                     stiff(nn) = abbd(ll,mm)
                  ENDDO
               ENDDO
!
               CALL gmmats(stiff(1),6,6,0,mther(1),6,1,0,ezerot(1))
            ENDIF
         ENDIF
      ENDIF
   ENDIF
!
!
   DO ll = 1 , 6
      Forsul(ll) = 0.0
   ENDDO
!
!     LOOP OVER NLAY
!
   DO k = 1 , nlay
!
!     ZSUBI -DISTANCE FROM REFERENCE SURFACE TO MID OF LAYER K
!
      zk1 = zk
      IF ( k==1 ) zk1 = zref
      IF ( itype==pcomp ) zk = zk1 + Z(pidloc+6+4*k)
      IF ( itype==pcomp1 ) zk = zk1 + Z(pidloc+7)
      IF ( itype==pcomp2 ) zk = zk1 + Z(pidloc+7+2*k)
!
      zsubi = (zk+zk1)/2.0
!
!     LAYER THICKNESS
!
      ti = zk - zk1
!
!     CALCULATE STRAIN VECTOR AT STN ZSUBI
!
      DO ir = 1 , 3
         epslne(ir) = (.5-zsubi/tlam)*epsb(ir) + (.5+zsubi/tlam)*epst(ir)
      ENDDO
!
!     LAYER ORIENTATION
!
      IF ( itype==pcomp ) theta = Z(pidloc+7+4*k)
      IF ( itype==pcomp1 ) theta = Z(pidloc+8+k)
      IF ( itype==pcomp2 ) theta = Z(pidloc+8+2*k)
!
!     BUILD TRANS-MATRIX TO TRANSFORM LAYER STRAINS FROM MATERIAL
!     TO FIBRE DIRECTION.
!
      theta = theta*Degrad
!
      c = cos(theta)
      c2 = c*c
      s = sin(theta)
      s2 = s*s
!
      trans(1) = c2
      trans(2) = s2
      trans(3) = c*s
      trans(4) = s2
      trans(5) = c2
      trans(6) = -c*s
      trans(7) = -2.0*c*s
      trans(8) = 2.0*c*s
      trans(9) = c2 - s2
!
!     TRANSFORM STRAINS FROM ELEMENT TO FIBRE COORD SYSTEM
!
      CALL gmmats(trans(1),3,3,0,epslne(1),3,1,0,Epsln(1))
!
!     SWITCH FOR TEMPERATURE EFFECTS
!
      IF ( Ldtemp/=-1 ) THEN
!
!     CORRECT LAYER STRAIN VECTOR FOR THERMAL EFFECTS
!
!     LAYER THERMAL COEFFICIENTS OF EXPANSION ALPHA-VECTOR
!
         DO ll = 1 , 3
            alpha(ll) = Z(ipoint+13+ll)
         ENDDO
!
!     ELEMENT TEMPERATURE
!
         delt = tbar - tsubo
!
         IF ( lamopt/=mem .AND. lamopt/=symmem ) THEN
!
!     TEMPERATURE GRADIENT TPRIME
!
            IF ( tempp1 ) delt = delt + zsubi*tprime
         ENDIF
!
         DO ll = 1 , 3
            Epslnt(ll) = -alpha(ll)*delt
         ENDDO
!
         IF ( lamopt/=mem .AND. lamopt/=symmem ) THEN
            IF ( tempp2 ) THEN
!
!     COMPUTE STRAIN DUE TO THERMAL MOMENTS
!
               DO ll = 1 , 3
                  Epslnt(ll) = Epslnt(ll) + (ezerot(ll)+zsubi*ezerot(ll+3))
               ENDDO
            ENDIF
         ENDIF
!
!     COMBINE MECHANICAL AND THERMAL STRAINS
!
         DO ll = 1 , 3
            Epsln(ll) = Epsln(ll) + Epslnt(ll)
         ENDDO
      ENDIF
!
!
!     CALCULATE STRESS VECTOR STRESL IN FIBRE COORD SYS
!
!     STRESL-VECTOR  =  G-MATRIX  X  EPSLN-VECTOR
!
      CALL gmmats(Z(ipoint+1),3,3,0,Epsln,3,1,0,stresl(1))
!
!     USE FORCE RESTULANTS CALCULATED PREVIOUSLY
!     I.E. AT EXTREME FIBER STATIONS EXCEPT FOR THERMAL LOADING CASES
!
      IF ( Ldtemp/=-1 ) THEN
         IF ( Kforce/=0 ) THEN
!
!     TRANSFORM LAYER STRESSES TO ELEMENT AXIS
!
            IF ( thetae>0.0 ) theta = theta + thetae
!
!     BUILD STRESS TRANSFORMATION MATRIX
!
            c = cos(theta)
            c2 = c*c
            s = sin(theta)
            s2 = s*s
!
            trans(1) = c2
            trans(2) = s2
            trans(3) = -2.0*c*s
            trans(4) = s2
            trans(5) = c2
            trans(6) = 2.0*c*s
            trans(7) = c*s
            trans(8) = -c*s
            trans(9) = c2 - s2
!
            CALL gmmats(trans(1),3,3,0,stresl(1),3,1,0,strese(1))
!
            DO ir = 1 , 3
               Forsul(ir) = Forsul(ir) + strese(ir)*ti
               IF ( lamopt/=mem .AND. lamopt/=symmem ) Forsul(ir+3) = Forsul(ir+3) - strese(ir)*ti*zsubi
            ENDDO
         ENDIF
      ENDIF
!
      IF ( fthr>0 ) THEN
!
!     WRITE ULTIMATE STRENGTH VALUES TO ULTSTN
!
         DO ir = 1 , 6
            ultstn(ir) = Z(ipoint+16+ir)
         ENDDO
!
!     CALL FTHR TO COMPUTE FAILURE INDEX FOR PLY
!
         IF ( fthr==strain ) THEN
!
            CALL failur(fthr,ultstn,Epsln,findex)
         ELSE
            CALL failur(fthr,ultstn,stresl,findex)
         ENDIF
!
!     DETERMINE THE MAX FAILURE INDEX
!
         IF ( abs(findex)>=abs(fpmax) ) fpmax = findex
      ENDIF
!
!
!     SET POINTERS
!
      IF ( itype==pcomp ) icontr = ipoint + 25
      IF ( itype==pcomp1 .OR. itype==pcomp2 ) icontr = ipoint + 23 + 2*k
!
      IF ( lamopt/=mem .AND. lamopt/=symmem ) THEN
         IF ( trnflx ) THEN
!
!     CALCULATE INTERLAMINAR SHEAR STRESSES
!
            DO ir = 1 , 2
               trnar(ir) = trnar(ir) + (Z(icontr+ir))*ti*(zbar(ir)-zsubi)
            ENDDO
!
!     THE INTERLAMINAR SHEAR STRESSES AT STN ZSUBI
!
            DO ir = 1 , 2
               trnshr(ir) = v(ir)*trnar(ir)/ei(ir)
            ENDDO
!
!     CALCULATE SHEAR BONDING FAILURE INDEX FB
!     NOTE- SB IS ALWAYS POSITIVE
!
            IF ( sb/=0.0 ) THEN
!
               DO ir = 1 , 2
                  fb(ir) = abs(trnshr(ir))/sb
               ENDDO
!
               fbond = fb(1)
               IF ( fb(2)>fb(1) ) fbond = fb(2)
!
!     CALCULATE MAX SHEAR BONDING INDEX
!
               IF ( fbond>=fbmax ) fbmax = fbond
            ENDIF
         ENDIF
      ENDIF
!
!
      IF ( Kstrs/=0 ) THEN
!
!     WRITE TO OUTPUT FILE THE FOLLOWING
!       4.    PLY (OR LAYER) ID
!     5,6,7.  LAYER STRESSES
!       8.    LAYER FAILURE INDEX
!       9.    IFLAG (= 1 IF FP.GE.0.999, DEFAULT = 0)
!     10,11.  INTERLAMINAR SHEAR STRESSES
!      12.    SHEAR BONDING FAILURE INDEX
!      13.    IFLAG (= 1 IF SB.GE.0.999, DEFAULT = 0)
!
!     CHECK LAYER STRESS OUTPUT REQUEST (SOUTI) FOR PCOMP BULK DATA
!     (NOT SUPPORTED FOR PCOMP1 OR PCOMP2 BULK DATA)
!
         IF ( itype==pcomp ) THEN
            souti = Intz(pidloc+8+4*k)
            IF ( souti==0 ) GOTO 650
         ENDIF
         plyid = k
!
!     == 4.
!
         CALL write(Oes1l,plyid,1,0)
!
!     == 5,6,7.
!
         CALL write(Oes1l,stresl(1),3,0)
!
!     == 8.
!
         CALL write(Oes1l,findex,1,0)
!
!     SET IFLAG
!
         iflag = 0
         IF ( abs(findex)>=0.999 ) iflag = 1
!
!     == 9.
!
         CALL write(Oes1l,iflag,1,0)
!
!     == 10,11.
!
         CALL write(Oes1l,trnshr(1),2,0)
!
!     == 12.
!
         CALL write(Oes1l,fbond,1,0)
!
!     SET IFLAG
!
         iflag = 0
         IF ( abs(fbond)>=0.999 ) iflag = 1
!
!     == 13.
!
         CALL write(Oes1l,iflag,1,0)
      ENDIF
!
!
!     UPDATE IPOINT FOR PCOMP BULK DATA ENTRY
!
 650  IF ( itype==pcomp .AND. k/=nlay ) ipoint = ipoint + 27
!
   ENDDO
!
!     FALL HERE IF SYMMETRIC OPTION HAS BEEN EXERCISED
!
   IF ( lamopt==sym .OR. lamopt==symmem ) THEN
!
!     LOOP OVER SYMMETRIC LAYERS
!
      DO kk = 1 , nlay
         k = nlay + 1 - kk
!
!     ZSUBI -DISTANCE FROM REFERENCE SURFACE TO MID OF LAYER K
!
         zk1 = zk
         IF ( itype==pcomp ) zk = zk1 + Z(pidloc+6+4*k)
         IF ( itype==pcomp1 ) zk = zk1 + Z(pidloc+7)
         IF ( itype==pcomp2 ) zk = zk1 + Z(pidloc+7+2*k)
!
         zsubi = (zk+zk1)/2.0
!
!     LAYER THICKNESS
!
         ti = zk - zk1
!
!     CALCULATE STRAIN VECTOR AT STN ZSUBI
!
         DO ir = 1 , 3
            epslne(ir) = (.5-zsubi/tlam)*epsb(ir) + (.5+zsubi/tlam)*epst(ir)
         ENDDO
!
!     LAYER ORIENTATION
!
         IF ( itype==pcomp ) theta = Z(pidloc+7+4*k)
         IF ( itype==pcomp1 ) theta = Z(pidloc+8+k)
         IF ( itype==pcomp2 ) theta = Z(pidloc+8+2*k)
!
!     BUILD TRANS-MATRIX TO TRANSFORM LAYER STRAINS FROM MATERIAL
!     TO FIBRE DIRECTION.
!
         theta = theta*Degrad
         c = cos(theta)
         c2 = c*c
         s = sin(theta)
         s2 = s*s
!
         trans(1) = c2
         trans(2) = s2
         trans(3) = c*s
         trans(4) = s2
         trans(5) = c2
         trans(6) = -c*s
         trans(7) = -2.0*c*s
         trans(8) = 2.0*c*s
         trans(9) = c2 - s2
!
!     TRANSFORM STRAINS FROM MATERIAL TO FIBRE COORD SYSTEM
!
         CALL gmmats(trans(1),3,3,0,epslne(1),3,1,0,Epsln(1))
!
!     SWITCH FOR TEMPERATURE EFFECTS
!
         IF ( Ldtemp/=-1 ) THEN
!
!     CORRECT LAYER STRAIN VECTOR FOR THERMAL EFFECTS
!
!     LAYER THERMAL COEFFICIENTS OF EXPANSION ALPHA-VECTOR
!
            DO ll = 1 , 3
               alpha(ll) = Z(ipoint+13+ll)
            ENDDO
!
!     ELEMENT TEMPERATURE
!
            delt = tbar - tsubo
            IF ( lamopt/=symmem ) THEN
!
!     TEMPERATURE GRADIENT TPRIME
!
               IF ( tempp1 ) delt = delt + zsubi*tprime
            ENDIF
!
            DO ll = 1 , 3
               Epslnt(ll) = -alpha(ll)*delt
            ENDDO
!
            IF ( lamopt/=symmem ) THEN
               IF ( tempp2 ) THEN
!
!     COMPUTE STRAIN DUE TO THERMAL MOMENTS
!
                  DO ll = 1 , 3
                     Epslnt(ll) = Epslnt(ll) + (ezerot(ll)+zsubi*ezerot(ll+3))
                  ENDDO
               ENDIF
            ENDIF
!
!     COMBINE MECHANICAL AND THERMAL STRAINS
!
            DO ll = 1 , 3
               Epsln(ll) = Epsln(ll) + Epslnt(ll)
            ENDDO
         ENDIF
!
!
!     CALCULATE STRESS VECTOR STRESL IN FIBRE COORD SYS
!
!     STRESL-VECTOR =  G-MATRIX  X  EPSLN-VECTOR
!
         CALL gmmats(Z(ipoint+1),3,3,0,Epsln,3,1,0,stresl(1))
!
!     COMPUTE FORCE RESULTANTS IF REQUESTED
!
         IF ( Ldtemp/=-1 ) THEN
            IF ( Kforce/=0 ) THEN
!
!     TRANSFORM LAYER STRESSES TO ELEMENT AXIS
!
               IF ( thetae>0.0 ) theta = theta + thetae
!
!     BUILD STRESS TRANSFORMATION MATRIX
!
               c = cos(theta)
               c2 = c*c
               s = sin(theta)
               s2 = s*s
!
               trans(1) = c2
               trans(2) = s2
               trans(3) = -2.0*c*s
               trans(4) = s2
               trans(5) = c2
               trans(6) = 2.0*c*s
               trans(7) = c*s
               trans(8) = -c*s
               trans(9) = c2 - s2
!
               CALL gmmats(trans(1),3,3,0,stresl(1),3,1,0,strese(1))
!
               DO ir = 1 , 3
                  Forsul(ir) = Forsul(ir) + strese(ir)*ti
                  IF ( lamopt/=symmem ) Forsul(ir+3) = Forsul(ir+3) - strese(ir)*ti*zsubi
               ENDDO
            ENDIF
         ENDIF
!
         IF ( fthr>0 ) THEN
!
!     WRITE ULTIMATE STRENGTH VALUES TO ULTSTN
!
            DO ir = 1 , 6
               ultstn(ir) = Z(ipoint+16+ir)
            ENDDO
!
!     CALL FTHR TO COMPUTE FAILURE INDEX FOR PLY
!
            IF ( fthr==strain ) THEN
!
               CALL failur(fthr,ultstn,Epsln,findex)
            ELSE
               CALL failur(fthr,ultstn,stresl,findex)
            ENDIF
!
!     DETERMINE THE MAX FAILURE INDEX
!
            IF ( abs(findex)>=abs(fpmax) ) fpmax = findex
         ENDIF
!
!
!     SET POINTERS
!
         IF ( itype==pcomp ) icontr = ipoint + 25
         IF ( itype==pcomp1 .OR. itype==pcomp2 ) icontr = ipoint + 23 + 2*k
!
         IF ( lamopt/=symmem ) THEN
            IF ( trnflx ) THEN
!
!     CALCULATE INTERLAMINAR SHEAR STRESSES
!
               DO ir = 1 , 2
                  trnar(ir) = trnar(ir) + (Z(icontr+ir))*ti*(zbar(ir)-zsubi)
               ENDDO
!
!     THE INTERLAMINAR SHEAR STRESSES AT STN ZSUBI
!
               DO ir = 1 , 2
                  trnshr(ir) = v(ir)*trnar(ir)/ei(ir)
               ENDDO
!
!     CALCULATE SHEAR BONDING FAILURE INDEX FB
!     NOTE- SB IS ALWAYS POSITIVE
!
               IF ( sb/=0.0 ) THEN
!
                  DO ir = 1 , 2
                     fb(ir) = abs(trnshr(ir))/sb
                  ENDDO
!
                  fbond = fb(1)
                  IF ( fb(2)>fb(1) ) fbond = fb(2)
!
!     CALCULATE MAX SHEAR BONDING INDEX
!
                  IF ( fbond>=fbmax ) fbmax = fbond
               ENDIF
            ENDIF
         ENDIF
!
!
         IF ( Kstrs/=0 ) THEN
!
!     WRITE TO OUTPUT FILE THE FOLLOWING
!       4.     PLY (OR LAYER) ID
!     5,6,7.   LAYER STRESSES
!       8.     LAYER FAILURE INDEX
!       9.     IFLAG (= 1 IF FP.GE.0.999, DEFAULT = 0)
!     10,11.   INTERLAMINAR SHEAR STRESSES
!      12.     SHEAR BONDING FAILURE INDEX
!      13.     IFLAG (= 1 IF SB.GE.0.999, DEFAULT = 0)
!
!     CHECK LAYER STRESS OUTPUT REQUEST (SOUTI) FOR PCOMP BULK DATA
!     (NOT SUPPORTED FOR PCOMP1 OR PCOMP2 BULK DATA)
!
            IF ( itype==pcomp ) THEN
               souti = Intz(pidloc+8+4*k)
               IF ( souti==0 ) GOTO 660
            ENDIF
            plyid = nlay + kk
!
!     == 4.
!
            CALL write(Oes1l,plyid,1,0)
!
!     == 5,6,7
!
            CALL write(Oes1l,stresl(1),3,0)
!
!     == 8.
!
            CALL write(Oes1l,findex,1,0)
!
!     SET IFLAG
!
            iflag = 0
            IF ( abs(findex)>=0.999 ) iflag = 1
!
!     == 9.
!
            CALL write(Oes1l,iflag,1,0)
!
!     == 10,11.
!
            CALL write(Oes1l,trnshr(1),2,0)
!
!     == 12.
!
            CALL write(Oes1l,fbond,1,0)
!
!     SET IFLAG
!
            iflag = 0
            IF ( abs(fbond)>=0.999 ) iflag = 1
!
!     == 13.
!
            CALL write(Oes1l,iflag,1,0)
         ENDIF
!
!     UPDATE IPOINT FOR PCOMP BULK DATA ENTRY
!
 660     IF ( itype==pcomp ) ipoint = ipoint - 27
      ENDDO
   ENDIF
!
   IF ( fthr>0 ) THEN
!
!     DETERMINE 'FIMAX' THE MAX FAILURE INDEX FOR THE LAMINATE
!
      fimax = fpmax
      IF ( fbmax>abs(fpmax) ) fimax = fbmax
   ENDIF
!
!     == LAST-1.
!
   IF ( Kstrs==1 ) CALL write(Oes1l,fimax,1,0)
!
   iflag = 0
   IF ( abs(fimax)>=0.999 ) iflag = 1
!
!     == LAST.
!
   IF ( Kstrs==1 ) CALL write(Oes1l,iflag,1,0)
!
   IF ( Kforce/=0 ) THEN
      IF ( Ldtemp/=-1 ) THEN
         CALL write(Oef1l,elemid,1,0)
         CALL write(Oef1l,Forsul(1),6,0)
         CALL write(Oef1l,Forsul(45),2,0)
      ENDIF
   ENDIF
!
 700  RETURN
!
!     ERROR MESSAGES
!
 800  WRITE (Nout,99004) Uwm
99004 FORMAT (A25,' - NO PCOMP, PCOMP1 OR PCOMP2 DATA AVAILABLE FOR ','LAYER STRESS RECOVERY BY SUBROUTINE SQUD42.')
   GOTO 700
99999 RETURN
END SUBROUTINE squd42