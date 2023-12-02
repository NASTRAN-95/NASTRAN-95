!*==stri32.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE stri32
   IMPLICIT NONE
   USE c_blank
   USE c_outreq
   USE c_sdr2c1
   USE c_sdr2de
   USE c_sdr2x2
   USE c_sdr2x4
   USE c_sdr2x7
   USE c_sdr2x8
   USE c_tmpdat
   USE c_zzzzzz
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
! Local variable declarations rewritten by SPAG
!
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
   temper = ldtemp/= - 1
   bendng = mominr>0.0
!
!     CHECK FOR OFFSET AND COMPOSITES
!
   offset = ph1rst(48)
   compos = comps== - 1 .AND. ipid>0
!
!     CHECK THE OUTPUT STRESS FORCE AND STRAIN REQUESTS
!
   stsreq = kstrs==1
   forreq = kforc==1
   stnreq = kstrn==1
!
!     STRESS OUTPUT REQUEST FLAGS
!
!     GRIDS = ANDF(NSTROP, 1).NE.0
!     VONMS = ANDF(NSTROP, 8).NE.0
!     LAYER = ANDF(NSTROP,32).NE.0 .AND. COMPOS .AND. KTYPE.EQ.1
!
   grids = .FALSE.
   vonms = andf(nstrop,1)/=0
   layer = andf(nstrop,2)/=0
!
!     STRAIN OUTPUT REQUEST FLAGS
!
!     GRIDSS = ANDF(NEPSOP,  1).NE.0 .AND. STNREQ
!     VONMSS = ANDF(NEPSOP,  8).NE.0 .AND. STNREQ
!     LAYERS = ANDF(NEPSOP, 32).NE.0 .AND. COMPOS .AND. KTYPE.EQ.1
!     STRCUR = ANDF(NEPSOP,128).NE.0 .AND. STNREQ
!
   gridss = .FALSE.
   vonmss = .FALSE.
   layers = .FALSE.
   strcur = .FALSE.
!WKBNB NCL93012 3/94
   stnreq = ostrai
   strcur = ostrai
!WKBNE NCL93012 3/94
!
!     IF USER ERRONEOUSLY REQESTS LAYERED OUTPUT AND THERE ARE NO LAYER-
!     COMPOSITE DATA, SET LAYER FLAGS TO FALSE
!
   IF ( npcmp+npcmp1+npcmp2<=0 ) THEN
      layer = .FALSE.
      layers = .FALSE.
!
!     USER CORRECTLY REQUESTS LAYERED OUTPUT, BUT CURRENT ELEMENT IS NOT
!     A LAYER-COMPOSITE; SET LAYER FLAGS TO FALSE
!
   ELSEIF ( ipid<=0 ) THEN
      layer = .FALSE.
      layers = .FALSE.
   ENDIF
!
!     SET DEFAULTS FOR FORCE IF STRESS ABSENT
!
   IF ( .NOT.(.NOT.forreq .OR. nstrop/=0) ) layer = .FALSE.
!
!     CHECK FOR THE TYPE OF TEMPERATURE DATA (SET BY SDRETD)
!     - TYPE TEMPP1 ALSO INCLUDES TYPE TEMPP3.
!     - IF TEMPPI ARE NOT SUPPLIED, GRID POINT TEMPERATURES ARE PRESENT.
!
   tempp1 = flag==13
   tempp2 = flag==2
!
!     GET THE EXTERNAL GRID POINT ID NUMBERS FOR CORRESPONDING SIL NOS.
!
!     CALL FNDGID (ELID,3,KSIL,EXTRNL)
!
   DO i = 1 , nnode
      extrnl(i) = 0
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
   IF ( .NOT.grids .AND. .NOT.gridss ) THEN
      idr(1) = 1
      idr(2) = 2
      idr(3) = 3
   ELSE
      DO inpl = 1 , 3
         DO i = 1 , nnode
            IF ( iorder(i)==inpl ) THEN
               idr(inpl) = extrnl(i)
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
   IF ( .NOT.(.NOT.temper .OR. (tempp1 .AND. tempp2)) ) THEN
      DO k = 1 , nnode
         kpoint = iorder(k)
         deltat(k) = stemp(kpoint)
      ENDDO
   ENDIF
!
!     PICK UP THE GLOBAL DISPLACEMENT VECTOR AND TRANSFORM IT INTO THE
!     ELEMENT COORD. SYSTEM
!
   DO idelt = 1 , nnode
      jdelt = ivec + ksil(idelt) - 2
      kdelt = 6*(idelt-1) + 1
      DO ldelt = 1 , 6
         tdelta(ldelt) = disp(jdelt+ldelt)
      ENDDO
!
!     FETCH [TEG] 3X3 FOR EACH NODE, LOAD IT INTO A 6X6 MATRIX AND
!     INCLUDE THE EFFECTS OF OFFSET
!
      CALL tldrs(offset,idelt,ph1rst(67),u)
      CALL gmmats(u,6,6,0,tdelta,6,1,0,delta(kdelt))
   ENDDO
!
!     RECOVER THE STRESS-TO-ELEMENT ORTHOGONAL TRANSFORMATION AND BUILD
!     THE ELEMENT-TO-STRESS 'STRAIN' TENSOR TRANSFORMATION.
!     IF LAYER OUTPUT IS REQUESTED, STRAINS MUST BE TRANSFORMED TO THE
!     MATERIAL COORDINATE SYSTEM.
!
   DO i = 1 , 9
      uem(i) = ph1rst(48+i)
      tes(i) = ph1rst(57+i)
   ENDDO
   CALL shstts(tes,ues,ves)
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
      igrid(inplan) = center
      IF ( inplan>1 ) THEN
         DO i = 1 , nnode
            IF ( iorder(i)==inplan-1 ) igrid(inplan) = extrnl(i)
         ENDDO
      ENDIF
!
!     THICKNESS AND MOMENT OF INERTIA AT THIS POINT
!
      thikns(inplan) = ph1rst(icount+1)
      IF ( (grids .OR. gridss) .AND. inplan/=1 ) thikns(inplan) = gpth(inplan-1)
      t3ov12 = thikns(inplan)**3/12.0
!
!     DETERMINE FIBER DISTANCE VALUES
!
      z12(1,inplan) = z1o
      IF ( iz1o==nblnk ) z12(1,inplan) = -0.5*thikns(inplan)
!
      z12(2,inplan) = z2o
      IF ( iz2o==nblnk ) z12(2,inplan) = 0.5*thikns(inplan)
!
!
!     FIRST COMPUTE LOCAL STRAINS UNCORRECTED FOR THERMAL STRAINS AT
!     THIS EVALUATION POINT.
!
!     EPSLN = PH1RST(KSIG) * DELTA
!       EPS =        B     *   U
!       8X1        8XNDOF    NDOFX1
!
      CALL gmmats(ph1rst(icount+9),8,ndof,0,delta(1),ndof,1,0,epsln)
!
      IF ( .NOT.(.NOT.layer .AND. .NOT.layers) ) THEN
!
!     TRANSFORM UNCORRECTED STRAINS FROM ELEMENT TO MATERIAL COORD.
!     SYSTEM TO BE USED FOR ELEMENT LAYER STRAINS
!
         CALL gmmats(uem(1),3,3,0,epsln(1),3,1,0,epsumi(1,inplan))
         CALL gmmats(uem(1),3,3,0,epsln(4),3,1,0,epsumi(4,inplan))
!
         DO i = 1 , 6
            epscmi(i,inplan) = epsumi(i,inplan)
         ENDDO
      ENDIF
!
      IF ( .NOT.(.NOT.forreq .AND. layer .AND. layers) ) THEN
!
!     TRANSFORM UNCORRECTED STRAINS FROM ELEMENT TO STRESS COORD. SYSTEM
!     TO BE USED FOR ELEMENT STRAINS
!
         CALL gmmats(ues(1),3,3,0,epsln(1),3,1,0,epsusi(1,inplan))
         CALL gmmats(ues(1),3,3,0,epsln(4),3,1,0,epsusi(4,inplan))
!
         DO i = 1 , 6
            epscsi(i,inplan) = epsusi(i,inplan)
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
      IF ( forreq .OR. layer .OR. layers ) THEN
         CALL gmmats(ph1rst(icount+2),2,2,0,epsln(7),2,1,0,vxvy)
         CALL gmmats(ves(1),2,2,0,vxvy,2,1,0,qveci(1,inplan))
      ENDIF
!
!     CALCULATE THERMAL STRAINS IF TEMPERATURES ARE PRESENT
!
      IF ( temper ) THEN
         DO iet = 1 , 6
            epslnt(iet) = 0.0
         ENDDO
!
!     MEMBRANE STRAINS
!
         IF ( .NOT.tempp1 .AND. .NOT.tempp2 ) THEN
            tbar = 0.0
            DO ish = 1 , nnode
               tbar = tbar + ph1rst(icount+5+ish)*deltat(ish)
            ENDDO
         ELSE
            tbar = stemp(1)
         ENDIF
!
         DO ieps = 1 , 3
            epslnt(ieps) = (tbar-tsub0)*alfam(ieps)
         ENDDO
!
!     BENDING STRAINS (ELEMENT TEMPERATURES ONLY)
!
         IF ( .NOT.(.NOT.bendng .OR. .NOT.(tempp1 .AND. tempp2)) ) THEN
!
!     EXTRACT [G2] FROM [G]
!
            DO ig2 = 1 , 3
               DO jg2 = 1 , 3
                  g2(ig2,jg2) = g(ig2+3,jg2+3)
               ENDDO
            ENDDO
            CALL gmmats(g2,3,3,0,alfab,3,1,0,g2alfb(1,inplan))
!
            IF ( tempp2 ) THEN
               DO ig2 = 1 , 3
                  DO jg2 = 1 , 3
                     g2(ig2,jg2) = g2(ig2,jg2)*t3ov12
                  ENDDO
               ENDDO
!
               DO itmp = 1 , 3
                  stempd(itmp) = stemp(itmp+1)
               ENDDO
!
               CALL invers(3,g2,3,gdum,0,detg2,isngg2,indxg2)
               CALL gmmats(g2,3,3,0,stempd,3,1,0,epslnt(4))
!
            ELSEIF ( tempp1 ) THEN
               tprime = stemp(2)
               DO ieps = 4 , 6
                  epslnt(ieps) = -tprime*alfab(ieps-3)
               ENDDO
            ENDIF
         ENDIF
!
!     CORRECT STRAINS FOR THERMAL EFFECTS
!
         DO i = 1 , 6
            epslnm(i) = epsln(i) - epslnt(i)
         ENDDO
!
         IF ( layer ) THEN
!
!     TRANSFORM CORRECTED STRAINS FROM ELEMENT TO MATERIAL COOR. SYSTEM
!     TO BE USED FOR ELEMENT LAYER STRESSES
!
            CALL gmmats(uem(1),3,3,0,epslnm(1),3,1,0,epscmi(1,inplan))
            CALL gmmats(uem(1),3,3,0,epslnm(4),3,1,0,epscmi(4,inplan))
         ENDIF
!
         IF ( .NOT.(layer .AND. .NOT.forreq) ) THEN
!
!     TRANSFORM CORRECTED STRAINS FROM ELEMENT TO STRESS COORD. SYSTEM
!     TO BE USED FOR ELEMENT STRESSES AND ELEMENT (LAYER) FORCES
!
            CALL gmmats(ues(1),3,3,0,epslnm(1),3,1,0,epscsi(1,inplan))
            CALL gmmats(ues(1),3,3,0,epslnm(4),3,1,0,epscsi(4,inplan))
         ENDIF
      ENDIF
!
!     CORRECT THE CURVATURE SIGNS WHEN THE Z-AXIS OF THE TARGET STRESS
!     COORD. SYSTEM IS FLIPPED WITH RESPECT TO THE USER COORD. SYSTEM.
!     THIS DOES NOT AFFECT THE MEMBRANE STRAINS, AND TRANSVERSE SHEAR
!     STRAIN TRANSFORMATION TAKES CARE OF THOSE COMPONENTS.
!
      IF ( ph1rst(66)<0.0 ) THEN
         DO i = 4 , 6
            epscmi(i,inplan) = -epscmi(i,inplan)
            epscsi(i,inplan) = -epscsi(i,inplan)
            epsumi(i,inplan) = -epsumi(i,inplan)
            epsusi(i,inplan) = -epsusi(i,inplan)
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
   IF ( gridss ) CALL shxtrs(6,nnode,epsusi(1,2))
   IF ( grids ) CALL shxtrs(6,nnode,epscsi(1,2))
   IF ( grids .AND. forreq ) CALL shxtrs(2,nnode,qveci(1,2))
!
!     CALCULATE AND OUTPUT STRESSES
!
   IF ( stsreq .AND. .NOT.layer ) CALL shstss(4,elid,igrid,thikns,z12,g,epscsi,stemp,tbar,g2alfb,bendng,idr)
!
!     CALCULATE AND OUTPUT STRAINS
!
   IF ( stnreq .AND. .NOT.layers ) CALL shstns(4,elid,igrid,z12,epsusi,bendng,idr)
!
!     CALCULATE AND OUTPUT FORCES
!
   IF ( forreq .OR. layer .OR. layers ) CALL shfors(4,elid,igrid,thikns,g,epscsi,qveci,idr)
!
!     CALCULATE AND OUTPUT LAYER-RELATED INFORMATION
!
   IF ( layer .OR. layers ) CALL shlsts(elid,ipid,avgthk,epsumi,epscmi)
!
END SUBROUTINE stri32
