!*==stri32.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE stri32
   IMPLICIT NONE
   USE C_BLANK
   USE C_OUTREQ
   USE C_SDR2C1
   USE C_SDR2DE
   USE C_SDR2X2
   USE C_SDR2X4
   USE C_SDR2X7
   USE C_SDR2X8
   USE C_TMPDAT
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(3) :: alfab , alfam , gpth
   REAL :: avgthk , tprime
   LOGICAL :: bendng , compos
   INTEGER , SAVE :: center , istart , nblnk
   INTEGER :: device , flag , i , icount , idelt , ieps , iet , ig2 , inpl , inplan , ish , isngg2 , itmp , iz1o , iz2o , jdelt ,   &
            & jg2 , k , kdelt , kforc , kpoint , kstrn , kstrs , ldelt , mominr , ndof , ndof8 , nepsop , nnode , oes1al
   REAL , DIMENSION(6,6) :: g
   REAL , DIMENSION(8) :: stemp
!
! End of declarations rewritten by SPAG
!
!
!     ROUTINE TO RECOVER CTRIA3 ELEMENT FORCES, STRAINS, AND STRESSES.
!     PHASE 2.
!
!     WAS NAMED T3ST2D/S (DISP) IN UAI CODE
!
!     ALGORITHM:
!
!     1- STRAIN RECOVERY DATA IS SENT BY PHASE 1 THRU PH1OUT IN /SDR2X7/
!        WHICH INCLUDES ALL THE NECESSARY TRANSFORMATIONS AND STRAIN
!        RECOVERY MATRICES. A MAJOR PORTION OF THE DATA IS REPEATED FOR
!        EACH STRESS EVALUATION POINT.
!     2- GLOBAL DISPLACEMENT VECTOR, WHICH RESIDES IN CORE, IS PASSED TO
!        THE ROUTINE THRU THE CALLING SEQUENCE.
!     3- NSTROP IN /SDR2C1/ CONTAINS THE STRESS OUTPUT REQUEST OPTION
!        FOR THE CURRENT SUBCASE.
!     4- WORD 151 OF /SDR2DE/ CONTAINS THE STRAIN OUTPUT REQUEST OPTION
!        FOR THE CURRENT SUBCASE (NEPSOP).
!     5- ELEMENT/GRID POINT TEMPERATURE DATA ENTERS THE ROUTINE THRU
!        /SDR2DE/ (POSITIONS 97-129.)
!     6- ELEMENT STRAINS ARE CALCULATED, CORRECTED FOR THERMAL STRAINS,
!        AND PREMULTIPLIED BY [G].
!
!
!     *****************       RESIDES IN COMMON BLOCK /SDR2X7/
!     PH1OUT DATA BLOCK       TOTAL NO. OF WORDS =  713
!     *****************
!
!     PH1OUT( 1)    = ELID, ELEMENT ID
!     PH1OUT( 2- 4) = SIL NUMBERS
!     PH1OUT( 5- 7) = ARRAY IORDER
!     PH1OUT( 8)    = TSUB0, REFERENCE TEMP.
!     PH1OUT( 9-10) = Z1 AND Z2, FIBER DISTANCES
!     PH1OUT(11)    = ID OF THE ORIGINAL PCOMPI PROPERTY ENTRY
!     PH1OUT(12)    = DUMMY WORD (FOR ALLIGNMENT)
!
!     PH1RST( 1)    = AVGTHK, AVERAGE THICKNESS
!     PH1RST( 2)    = MOMINR, MOMENT OF INER. FACTOR
!     PH1RST( 3-38) = 6X6 MATERIAL PROPERTY (NO SHEAR)
!     PH1RST(39-41) = THERMAL EXPANSION COEFFICIENTS FOR MEMBRANE
!     PH1RST(42-44) = THERMAL EXPANSION COEFFICIENTS FOR BENDING
!     PH1RST(45-47) = NODAL   THICKNESSES
!     PH1RST(48)    = OFFSET OF ELEMENT FROM GP PLANE
!     PH1RST(49-57) = 3X3 USER-TO-MATERIAL COORD. TRANSF. MATRIX, UEM
!     PH1RST(58-66) = 3X3 ELEM-TO-STRSS/STRAIN  TRANSF. TENSOR, TES
!     PH1RST(67-93) = 3X3 GLOBAL-TO-ELEM COORD. TRANSF. MATRICES, TEG,
!                     ONE FOR EACH NODE
!
!     THE FOLLOWING IS REPEATED FOR EACH EVALUATION POINT (4 TIMES).
!     THE EVALUATION POINTS ARE AT THE CENTER OF THE ELEMENT AND
!     STANDARD TRIANGULAR POINTS. THE CHOICE OF THE FINAL STRESS/
!     FORCE OUTPUT POINTS IS MADE AT THE SUBCASE LEVEL (PHASE 2).
!
!              1             ELEMENT THICKNESS AT THIS POINT
!            2 - 5           OUT-OF-PLANE-SHEAR-FORCE/STRAIN MATRIX
!            6 - 8           ELEMENT SHAPE FUNCTION VALUES
!          8+1 - 8+8*NDOF    STRAIN RECOVERY MATRIX
!
!
!WKBI NCL93012 3/94
!WKBR NCL93012 3/94 COMMON /BLANK / APP(2),SORT2,IDUM(2),COMPS
!    1,               DUM(13),KTYPE
   !>>>>EQUIVALENCE (Iz1o,Z1o) , (Iz2o,Z2o) , (Avgthk,Ph1rst(1)) , (Mominr,Ph1rst(2)) , (G(1,1),Ph1rst(3)) , (Alfam(1),Ph1rst(39)) ,     &
!>>>>    & (Alfab(1),Ph1rst(42)) , (Gpth(1),Ph1rst(45)) , (Device,Ksdrde(2)) , (Nepsop,Ksdrde(151)) , (Kstrs,Ksdrde(42)) ,               &
!>>>>    & (Kstrn,Ksdrde(142)) , (Kforc,Ksdrde(41)) , (Stemp(1),Ksdrde(97)) , (Stemp(7),Flag) , (Oes1al,Oes1l)
   DATA istart/93/
   DATA center/4HCNTR/
   DATA nblnk/4HBLNK/
!
!     INITIALIZE
!
!     NNODE  = TOTAL NUMBER OF NODES
!     NDOF   = TOTAL NUMBER OF DEGREES OF FREEDOM
!     LDTEMP = FLAG INDICATING THE PRESENCE OF TEMPERATURE LOADS
!     ICOUNT = POINTER FOR PH1RST DATA
!
!     STRCUR = STRAIN/CURVATURE OUTPUT REQUEST FLAG
!
   nnode = 3
   ndof = 6*nnode
   ndof8 = 8*ndof
   Temper = Ldtemp/= - 1
   bendng = mominr>0.0
!
!     CHECK FOR OFFSET AND COMPOSITES
!
   Offset = Ph1rst(48)
   compos = Comps== - 1 .AND. Ipid>0
!
!     CHECK THE OUTPUT STRESS FORCE AND STRAIN REQUESTS
!
   Stsreq = kstrs==1
   Forreq = kforc==1
   Stnreq = kstrn==1
!
!     STRESS OUTPUT REQUEST FLAGS
!
!     GRIDS = ANDF(NSTROP, 1).NE.0
!     VONMS = ANDF(NSTROP, 8).NE.0
!     LAYER = ANDF(NSTROP,32).NE.0 .AND. COMPOS .AND. KTYPE.EQ.1
!
   Grids = .FALSE.
   Vonms = andf(Nstrop,1)/=0
   Layer = andf(Nstrop,2)/=0
!
!     STRAIN OUTPUT REQUEST FLAGS
!
!     GRIDSS = ANDF(NEPSOP,  1).NE.0 .AND. STNREQ
!     VONMSS = ANDF(NEPSOP,  8).NE.0 .AND. STNREQ
!     LAYERS = ANDF(NEPSOP, 32).NE.0 .AND. COMPOS .AND. KTYPE.EQ.1
!     STRCUR = ANDF(NEPSOP,128).NE.0 .AND. STNREQ
!
   Gridss = .FALSE.
   Vonmss = .FALSE.
   Layers = .FALSE.
   Strcur = .FALSE.
!WKBNB NCL93012 3/94
   Stnreq = Ostrai
   Strcur = Ostrai
!WKBNE NCL93012 3/94
!
!     IF USER ERRONEOUSLY REQESTS LAYERED OUTPUT AND THERE ARE NO LAYER-
!     COMPOSITE DATA, SET LAYER FLAGS TO FALSE
!
   IF ( Npcmp+Npcmp1+Npcmp2<=0 ) THEN
      Layer = .FALSE.
      Layers = .FALSE.
!
!     USER CORRECTLY REQUESTS LAYERED OUTPUT, BUT CURRENT ELEMENT IS NOT
!     A LAYER-COMPOSITE; SET LAYER FLAGS TO FALSE
!
   ELSEIF ( Ipid<=0 ) THEN
      Layer = .FALSE.
      Layers = .FALSE.
   ENDIF
!
!     SET DEFAULTS FOR FORCE IF STRESS ABSENT
!
   IF ( .NOT.(.NOT.Forreq .OR. Nstrop/=0) ) Layer = .FALSE.
!
!     CHECK FOR THE TYPE OF TEMPERATURE DATA (SET BY SDRETD)
!     - TYPE TEMPP1 ALSO INCLUDES TYPE TEMPP3.
!     - IF TEMPPI ARE NOT SUPPLIED, GRID POINT TEMPERATURES ARE PRESENT.
!
   Tempp1 = flag==13
   Tempp2 = flag==2
!
!     GET THE EXTERNAL GRID POINT ID NUMBERS FOR CORRESPONDING SIL NOS.
!
!     CALL FNDGID (ELID,3,KSIL,EXTRNL)
!
   DO i = 1 , nnode
      Extrnl(i) = 0
   ENDDO
!
!     COMMENTS FROM G.C.  2/1990
!     EXTRNL ARE SET TO ZEROS HERE. IT IS USED LATER FOR SETTING IDR
!     ARRAY. BOTH EXTRNL AND IDR ARE USED ONLY WHEN GRIDS IS TRUE.
!     IN COSMIC VERSION, GRIDS IS FALSE.
!
!
!     PREPARE TO REARRANGE STRESSES, STRAINS, AND FORCES ACCORDING TO
!     EXTERNAL ORDER
!
   IF ( .NOT.Grids .AND. .NOT.Gridss ) THEN
      Idr(1) = 1
      Idr(2) = 2
      Idr(3) = 3
   ELSE
      DO inpl = 1 , 3
         DO i = 1 , nnode
            IF ( Iorder(i)==inpl ) THEN
               Idr(inpl) = Extrnl(i)
               EXIT
            ENDIF
         ENDDO
      ENDDO
   ENDIF
!
!     ARRANGE THE INCOMING DATA
!
!     SORT THE GRID TEMPERATURE CHANGES INTO SIL ORDER
!
   IF ( .NOT.(.NOT.Temper .OR. (Tempp1 .AND. Tempp2)) ) THEN
      DO k = 1 , nnode
         kpoint = Iorder(k)
         Deltat(k) = stemp(kpoint)
      ENDDO
   ENDIF
!
!     PICK UP THE GLOBAL DISPLACEMENT VECTOR AND TRANSFORM IT INTO THE
!     ELEMENT COORD. SYSTEM
!
   DO idelt = 1 , nnode
      jdelt = Ivec + Ksil(idelt) - 2
      kdelt = 6*(idelt-1) + 1
      DO ldelt = 1 , 6
         Tdelta(ldelt) = Disp(jdelt+ldelt)
      ENDDO
!
!     FETCH [TEG] 3X3 FOR EACH NODE, LOAD IT INTO A 6X6 MATRIX AND
!     INCLUDE THE EFFECTS OF OFFSET
!
      CALL tldrs(Offset,idelt,Ph1rst(67),U)
      CALL gmmats(U,6,6,0,Tdelta,6,1,0,Delta(kdelt))
   ENDDO
!
!     RECOVER THE STRESS-TO-ELEMENT ORTHOGONAL TRANSFORMATION AND BUILD
!     THE ELEMENT-TO-STRESS 'STRAIN' TENSOR TRANSFORMATION.
!     IF LAYER OUTPUT IS REQUESTED, STRAINS MUST BE TRANSFORMED TO THE
!     MATERIAL COORDINATE SYSTEM.
!
   DO i = 1 , 9
      Uem(i) = Ph1rst(48+i)
      Tes(i) = Ph1rst(57+i)
   ENDDO
   CALL shstts(Tes,Ues,Ves)
!
!     RECOVER STRAINS AT EVALUATION POINTS
!
!     THE ARRANGEMENT OF EVALUATION POINTS ON THE MID-SURFACE FOLLOWS
!     THE SEQUENCE OF GRID POINTS AS INPUT BY THE USER. THEREFORE,
!     SHUFFLING OF DATA IS ONLY REQUIRED TO MATCH THE USER-DEFINED ORDER
!     OF INPUT.
!
!     PRESET THE PH1RST COUNTER TO THE START OF THE REPEATED SECTION
!     WHICH WILL NOW BE FILLED.
!
   icount = istart
!
   DO inplan = 1 , 4
!
!     MATCH GRID ID NUMBER WHICH IS IN SIL ORDER
!
      Igrid(inplan) = center
      IF ( inplan>1 ) THEN
         DO i = 1 , nnode
            IF ( Iorder(i)==inplan-1 ) Igrid(inplan) = Extrnl(i)
         ENDDO
      ENDIF
!
!     THICKNESS AND MOMENT OF INERTIA AT THIS POINT
!
      Thikns(inplan) = Ph1rst(icount+1)
      IF ( (Grids .OR. Gridss) .AND. inplan/=1 ) Thikns(inplan) = gpth(inplan-1)
      T3ov12 = Thikns(inplan)**3/12.0
!
!     DETERMINE FIBER DISTANCE VALUES
!
      Z12(1,inplan) = Z1o
      IF ( iz1o==nblnk ) Z12(1,inplan) = -0.5*Thikns(inplan)
!
      Z12(2,inplan) = Z2o
      IF ( iz2o==nblnk ) Z12(2,inplan) = 0.5*Thikns(inplan)
!
!
!     FIRST COMPUTE LOCAL STRAINS UNCORRECTED FOR THERMAL STRAINS AT
!     THIS EVALUATION POINT.
!
!     EPSLN = PH1RST(KSIG) * DELTA
!       EPS =        B     *   U
!       8X1        8XNDOF    NDOFX1
!
      CALL gmmats(Ph1rst(icount+9),8,ndof,0,Delta(1),ndof,1,0,Epsln)
!
      IF ( .NOT.(.NOT.Layer .AND. .NOT.Layers) ) THEN
!
!     TRANSFORM UNCORRECTED STRAINS FROM ELEMENT TO MATERIAL COORD.
!     SYSTEM TO BE USED FOR ELEMENT LAYER STRAINS
!
         CALL gmmats(Uem(1),3,3,0,Epsln(1),3,1,0,Epsumi(1,inplan))
         CALL gmmats(Uem(1),3,3,0,Epsln(4),3,1,0,Epsumi(4,inplan))
!
         DO i = 1 , 6
            Epscmi(i,inplan) = Epsumi(i,inplan)
         ENDDO
      ENDIF
!
      IF ( .NOT.(.NOT.Forreq .AND. Layer .AND. Layers) ) THEN
!
!     TRANSFORM UNCORRECTED STRAINS FROM ELEMENT TO STRESS COORD. SYSTEM
!     TO BE USED FOR ELEMENT STRAINS
!
         CALL gmmats(Ues(1),3,3,0,Epsln(1),3,1,0,Epsusi(1,inplan))
         CALL gmmats(Ues(1),3,3,0,Epsln(4),3,1,0,Epsusi(4,inplan))
!
         DO i = 1 , 6
            Epscsi(i,inplan) = Epsusi(i,inplan)
         ENDDO
      ENDIF
!
!     IF REQUIRED, COMPUTE SHEAR FORCES AT THIS EVALUATION POINT IN THE
!     ELEMENT COORD. SYSTEM, THEN TRANSFORM AND STORE THEM. CONSULT
!     SHSTTS DOCUMENTATION ON WHY [VES] MAY BE USED TO TRANSFORM FORCES
!     DESPITE THE FACT THAT IT IS MEANT FOR STRAINS.
!     SHEAR STRAINS MAY NOT BE TRANSFORMED BEFORE MULTIPLICATION BECAUSE
!     [G3] IS DIRECTION-DEPENDENT.
!
      IF ( Forreq .OR. Layer .OR. Layers ) THEN
         CALL gmmats(Ph1rst(icount+2),2,2,0,Epsln(7),2,1,0,Vxvy)
         CALL gmmats(Ves(1),2,2,0,Vxvy,2,1,0,Qveci(1,inplan))
      ENDIF
!
!     CALCULATE THERMAL STRAINS IF TEMPERATURES ARE PRESENT
!
      IF ( Temper ) THEN
         DO iet = 1 , 6
            Epslnt(iet) = 0.0
         ENDDO
!
!     MEMBRANE STRAINS
!
         IF ( .NOT.Tempp1 .AND. .NOT.Tempp2 ) THEN
            Tbar = 0.0
            DO ish = 1 , nnode
               Tbar = Tbar + Ph1rst(icount+5+ish)*Deltat(ish)
            ENDDO
         ELSE
            Tbar = stemp(1)
         ENDIF
!
         DO ieps = 1 , 3
            Epslnt(ieps) = (Tbar-Tsub0)*alfam(ieps)
         ENDDO
!
!     BENDING STRAINS (ELEMENT TEMPERATURES ONLY)
!
         IF ( .NOT.(.NOT.bendng .OR. .NOT.(Tempp1 .AND. Tempp2)) ) THEN
!
!     EXTRACT [G2] FROM [G]
!
            DO ig2 = 1 , 3
               DO jg2 = 1 , 3
                  G2(ig2,jg2) = g(ig2+3,jg2+3)
               ENDDO
            ENDDO
            CALL gmmats(G2,3,3,0,alfab,3,1,0,G2alfb(1,inplan))
!
            IF ( Tempp2 ) THEN
               DO ig2 = 1 , 3
                  DO jg2 = 1 , 3
                     G2(ig2,jg2) = G2(ig2,jg2)*T3ov12
                  ENDDO
               ENDDO
!
               DO itmp = 1 , 3
                  Stempd(itmp) = stemp(itmp+1)
               ENDDO
!
               CALL invers(3,G2,3,Gdum,0,Detg2,isngg2,Indxg2)
               CALL gmmats(G2,3,3,0,Stempd,3,1,0,Epslnt(4))
!
            ELSEIF ( Tempp1 ) THEN
               tprime = stemp(2)
               DO ieps = 4 , 6
                  Epslnt(ieps) = -tprime*alfab(ieps-3)
               ENDDO
            ENDIF
         ENDIF
!
!     CORRECT STRAINS FOR THERMAL EFFECTS
!
         DO i = 1 , 6
            Epslnm(i) = Epsln(i) - Epslnt(i)
         ENDDO
!
         IF ( Layer ) THEN
!
!     TRANSFORM CORRECTED STRAINS FROM ELEMENT TO MATERIAL COOR. SYSTEM
!     TO BE USED FOR ELEMENT LAYER STRESSES
!
            CALL gmmats(Uem(1),3,3,0,Epslnm(1),3,1,0,Epscmi(1,inplan))
            CALL gmmats(Uem(1),3,3,0,Epslnm(4),3,1,0,Epscmi(4,inplan))
         ENDIF
!
         IF ( .NOT.(Layer .AND. .NOT.Forreq) ) THEN
!
!     TRANSFORM CORRECTED STRAINS FROM ELEMENT TO STRESS COORD. SYSTEM
!     TO BE USED FOR ELEMENT STRESSES AND ELEMENT (LAYER) FORCES
!
            CALL gmmats(Ues(1),3,3,0,Epslnm(1),3,1,0,Epscsi(1,inplan))
            CALL gmmats(Ues(1),3,3,0,Epslnm(4),3,1,0,Epscsi(4,inplan))
         ENDIF
      ENDIF
!
!     CORRECT THE CURVATURE SIGNS WHEN THE Z-AXIS OF THE TARGET STRESS
!     COORD. SYSTEM IS FLIPPED WITH RESPECT TO THE USER COORD. SYSTEM.
!     THIS DOES NOT AFFECT THE MEMBRANE STRAINS, AND TRANSVERSE SHEAR
!     STRAIN TRANSFORMATION TAKES CARE OF THOSE COMPONENTS.
!
      IF ( Ph1rst(66)<0.0 ) THEN
         DO i = 4 , 6
            Epscmi(i,inplan) = -Epscmi(i,inplan)
            Epscsi(i,inplan) = -Epscsi(i,inplan)
            Epsumi(i,inplan) = -Epsumi(i,inplan)
            Epsusi(i,inplan) = -Epsusi(i,inplan)
         ENDDO
      ENDIF
!
!     END OF THE STRAIN RECOVERY LOOP
!
!     INCREMENT THE PH1RST POINTER
!
      icount = icount + 8 + ndof8
   ENDDO
!
!
!     IF REQUIRED, EXTRAPOLATE NON-CENTER VALUES FROM EVALUATION POINTS
!     TO GRID POINTS.
!
   IF ( Gridss ) CALL shxtrs(6,nnode,Epsusi(1,2))
   IF ( Grids ) CALL shxtrs(6,nnode,Epscsi(1,2))
   IF ( Grids .AND. Forreq ) CALL shxtrs(2,nnode,Qveci(1,2))
!
!     CALCULATE AND OUTPUT STRESSES
!
   IF ( Stsreq .AND. .NOT.Layer ) CALL shstss(4,Elid,Igrid,Thikns,Z12,g,Epscsi,stemp,Tbar,G2alfb,bendng,Idr)
!
!     CALCULATE AND OUTPUT STRAINS
!
   IF ( Stnreq .AND. .NOT.Layers ) CALL shstns(4,Elid,Igrid,Z12,Epsusi,bendng,Idr)
!
!     CALCULATE AND OUTPUT FORCES
!
   IF ( Forreq .OR. Layer .OR. Layers ) CALL shfors(4,Elid,Igrid,Thikns,g,Epscsi,Qveci,Idr)
!
!     CALCULATE AND OUTPUT LAYER-RELATED INFORMATION
!
   IF ( Layer .OR. Layers ) CALL shlsts(Elid,Ipid,avgthk,Epsumi,Epscmi)
!
END SUBROUTINE stri32
