
SUBROUTINE shstns(Numpx,Elid,Igrid,Z12,Epslni,Bendng,Idr)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL App(2) , Comps , Dum71(100) , Forsul(200) , Sk2(39) , Skp(4) , Sort2 , Stres(100) , Strin(100)
   LOGICAL Forreq , Grids , Gridss , Layer , Layers , Ostrai , Stnreq , Strcur , Stsreq , Vonms , Vonmss
   INTEGER Idum(2) , Midve , Nstres(1) , Nstrin(1)
   COMMON /blank / App , Sort2 , Idum , Comps , Skp , Ostrai , Sk2 , Midve
   COMMON /outreq/ Stsreq , Stnreq , Forreq , Strcur , Grids , Vonms , Layer , Gridss , Vonmss , Layers
   COMMON /sdr2x7/ Dum71 , Stres , Forsul , Strin
!
! Dummy argument declarations
!
   LOGICAL Bendng
   INTEGER Elid , Numpx
   REAL Epslni(6,1) , Z12(2,1)
   INTEGER Idr(1) , Igrid(1)
!
! Local variable declarations
!
   LOGICAL cosmic
   REAL epsil(3) , epsilp(4) , epss , fiber
   INTEGER i , inplan , inptmp , istrin , its , iz , nump , nump1
!
! End of declarations
!
!
!     TO CALCULATE SHELL ELEMENT STRAINS FOR A 2-D FORMULATION BASE.
!     COMPOSITE LAYER STRAINS ARE NOT CALCULATED IN THIS ROUTINE.
!
!
!     INPUT :
!           NUMPX  - NUMBER OF EVALUATION POINTS
!           ELID   - ELEMENT ID
!           IGRID  - ARRAY IF EXTERNAL GRID IDS
!           Z12    - EVALUATION POINT FIBER DISTANCES
!           EPSLNI - CORRECTED STRAINS AT EVALUATION POINTS
!           BENDNG - INDICATES THE PRESENCE OF BENDING BEHAVIOR
!           IDR    - REORDERING ARRAY BASED ON EXTERNAL GRID POINT ID'S
!          /OUTREQ/- OUTPUT REQUEST LOGICAL FLAGS
!
!     OUTPUT:
!           STRAINS ARE PLACED AT THE PROPER LOCATION IN /SDR2X7/.
!
!
!     THE STRAIN OUTPUT DATA BLOCK, UAI CODE
!
!     ADDRESS    DESCRIPTIONS
!
!        1       ELID
!     --------------------------------------------------------------
!        2       GRID POINT NUMBER OR 'CNTR'
!      3 - 10    STRAINS FOR LOWER POINTS OR MEMBRANE STRAINS
!     11 - 18    STRAINS FOR UPPER POINTS OR BENDING CURVATURES
!     ---------- ABOVE DATA REPEATED 3 TIMES
!                FOR GRID POINTS
!
!
!     THE STRAIN OUTPUT DATA BLOCK, AT ELEMENT CENTER ONLY, COSMIC
!
!     ADDRESS    DESCRIPTIONS
!
!        1       ELID
!     --------------------------------------------------------------
!        2       LOWER FIBER DISTANCE
!      3 -  9    STRAINS FOR LOWER POINTS OR MEMBRANE STRAINS
!       10       UPPER FIBER DISTANCE
!     11 - 17    STRAINS FOR UPPER POINTS OR BENDING CURVATURES
!     ---------- ABOVE DATA REPEATED 3 TIMES
!                FOR GRID POINTS
!
!
!WKBI NCL93012 3/94
   EQUIVALENCE (Nstrin(1),Strin(1))
!WKBI NCL93012 3/94
   EQUIVALENCE (Nstres(1),Stres(1))
!WKBNB 7/94 SPR94004
!WKBNE 7/94 SPR94004
   DATA cosmic , epss/.TRUE. , 1.0E-17/
!
!
!     ELEMENT ENTER COMPUATION ONLY FOR COSMIC
!     I.E. CALLER SHOULD PASS 1 IN NUMPX FOR COSMIC, 4 FOR UAI
!
   nump = Numpx
   IF ( cosmic ) nump = 1
!
   Nstrin(1) = Elid
!
!     START THE LOOP ON EVALUATION POINTS
!
   nump1 = nump - 1
   DO inplan = 1 , nump
!
      istrin = 1
      IF ( .NOT.(cosmic) ) THEN
!
         istrin = (inplan-1)*17 + 2
         Nstrin(istrin) = inplan - 1
         IF ( .NOT.(.NOT.Gridss .OR. inplan<=1) ) THEN
            DO inptmp = 1 , nump1
               IF ( Idr(inptmp)==Igrid(inplan) ) GOTO 10
            ENDDO
            CALL errtrc('SHSTNS  ',100)
 10         istrin = inptmp*17 + 2
            Nstrin(istrin) = Igrid(inplan)
         ENDIF
         IF ( inplan==1 ) Nstrin(istrin) = Igrid(inplan)
      ENDIF
!
!     START THE LOOP ON FIBERS
!
      DO iz = 1 , 2
         IF ( Strcur ) THEN
!
!     IF STRAIN/CURVATURE IS REQUESTED, SIMPLY OUTPUT THE AVAILABLE
!     STRAINS.
!
            Strin(istrin+1) = 0.0
            DO i = 1 , 3
               epsil(i) = 0.0
            ENDDO
!WKBI 7/94 SPR94004
            IF ( .NOT.(Ostrai .AND. iz==2) ) THEN
               IF ( iz==1 ) THEN
                  DO i = 1 , 3
                     epsil(i) = Epslni(i,inplan)
                  ENDDO
!WKBI 7/94 SPR94004
                  IF ( Ostrai .AND. iz==1 ) GOTO 40
               ENDIF
               IF ( .NOT.Bendng .OR. iz/=2 ) GOTO 20
            ENDIF
!WKBI 7/94 SPR94004
            DO i = 1 , 3
               epsil(i) = Epslni(i+3,inplan)
            ENDDO
            GOTO 40
         ENDIF
!
!     IF FIBER STRAINS ARE REQUESTED, EVALUATE STRAINS AT THIS FIBER
!     DISTANCE
!
 20      fiber = Z12(iz,inplan)
         Strin(istrin+1) = fiber
         DO i = 1 , 3
            epsil(i) = Epslni(i,inplan) - Epslni(i+3,inplan)*fiber
         ENDDO
!
!     CLEANUP AND SHIP CALCULATED STRAINS
!
 40      DO its = 1 , 3
            IF ( abs(epsil(its))<=epss ) epsil(its) = 0.0
!WKBR NCL93012 3/94      STRIN(ISTRIN+1+ITS) = EPSIL(ITS)
            Stres(istrin+1+its) = epsil(its)
         ENDDO
!
!     CALCULATE PRINCIPAL STRAINS
!
         CALL shpsts(epsil(1),Vonmss,epsilp)
!WKBDB NCL93012 3/94
!      STRIN(ISTRIN+5) = EPSILP(1)
!      STRIN(ISTRIN+6) = EPSILP(2)
!      STRIN(ISTRIN+7) = EPSILP(3)
!      STRIN(ISTRIN+8) = EPSILP(4)
!WKBDE NCL93012 3/94
!WKBNB NCL93012 3/94
         Nstres(istrin+1) = 0
         IF ( iz==2 ) Nstres(istrin+1) = -1
         Stres(istrin+5) = epsilp(1)
         Stres(istrin+6) = epsilp(2)
         Stres(istrin+7) = epsilp(3)
         Stres(istrin+8) = epsilp(4)*2.
!WKBNE NCL93012 3/94
!
         istrin = istrin + 8
      ENDDO
   ENDDO
!
END SUBROUTINE shstns
