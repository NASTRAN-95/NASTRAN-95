!*==shstns.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE shstns(Numpx,Elid,Igrid,Z12,Epslni,Bendng,Idr)
   IMPLICIT NONE
   USE C_BLANK
   USE C_OUTREQ
   USE C_SDR2X7
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Numpx
   INTEGER :: Elid
   INTEGER , DIMENSION(1) :: Igrid
   REAL , DIMENSION(2,1) :: Z12
   REAL , DIMENSION(6,1) :: Epslni
   LOGICAL :: Bendng
   INTEGER , DIMENSION(1) :: Idr
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL , SAVE :: cosmic
   REAL , DIMENSION(3) :: epsil
   REAL , DIMENSION(4) :: epsilp
   REAL , SAVE :: epss
   REAL :: fiber
   INTEGER :: i , inplan , inptmp , istrin , its , iz , nump , nump1
   INTEGER , DIMENSION(1) :: nstres , nstrin
   EXTERNAL errtrc , shpsts
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   !>>>>EQUIVALENCE (Nstrin(1),Strin(1))
!WKBI NCL93012 3/94
   !>>>>EQUIVALENCE (Nstres(1),Stres(1))
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
   nstrin(1) = Elid
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
         nstrin(istrin) = inplan - 1
         IF ( .NOT.(.NOT.Gridss .OR. inplan<=1) ) THEN
            DO inptmp = 1 , nump1
               IF ( Idr(inptmp)==Igrid(inplan) ) GOTO 10
            ENDDO
            CALL errtrc('SHSTNS  ',100)
 10         istrin = inptmp*17 + 2
            nstrin(istrin) = Igrid(inplan)
         ENDIF
         IF ( inplan==1 ) nstrin(istrin) = Igrid(inplan)
      ENDIF
!
!     START THE LOOP ON FIBERS
!
      DO iz = 1 , 2
         spag_nextblock_1 = 1
         SPAG_DispatchLoop_1: DO
            SELECT CASE (spag_nextblock_1)
            CASE (1)
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
                        IF ( Ostrai .AND. iz==1 ) THEN
                           spag_nextblock_1 = 3
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                     ENDIF
                     IF ( .NOT.Bendng .OR. iz/=2 ) THEN
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDIF
!WKBI 7/94 SPR94004
                  DO i = 1 , 3
                     epsil(i) = Epslni(i+3,inplan)
                  ENDDO
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               spag_nextblock_1 = 2
            CASE (2)
!
!     IF FIBER STRAINS ARE REQUESTED, EVALUATE STRAINS AT THIS FIBER
!     DISTANCE
!
               fiber = Z12(iz,inplan)
               Strin(istrin+1) = fiber
               DO i = 1 , 3
                  epsil(i) = Epslni(i,inplan) - Epslni(i+3,inplan)*fiber
               ENDDO
               spag_nextblock_1 = 3
            CASE (3)
!
!     CLEANUP AND SHIP CALCULATED STRAINS
!
               DO its = 1 , 3
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
               nstres(istrin+1) = 0
               IF ( iz==2 ) nstres(istrin+1) = -1
               Stres(istrin+5) = epsilp(1)
               Stres(istrin+6) = epsilp(2)
               Stres(istrin+7) = epsilp(3)
               Stres(istrin+8) = epsilp(4)*2.
!WKBNE NCL93012 3/94
!
               istrin = istrin + 8
               EXIT SPAG_DispatchLoop_1
            END SELECT
         ENDDO SPAG_DispatchLoop_1
      ENDDO
   ENDDO
!
END SUBROUTINE shstns
