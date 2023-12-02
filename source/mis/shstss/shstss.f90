!*==shstss.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE shstss(Numpx,Elid,Igrid,Thikns,Z12,G,Epscsi,Stemp,Tbar,G2alfb,Bendng,Idr)
   USE c_outreq
   USE c_sdr2x7
   USE c_tmpdat
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Numpx
   INTEGER :: Elid
   INTEGER , DIMENSION(1) :: Igrid
   REAL , DIMENSION(1) :: Thikns
   REAL , DIMENSION(2,1) :: Z12
   REAL , DIMENSION(6,6) :: G
   REAL , DIMENSION(6,1) :: Epscsi
   REAL , DIMENSION(2) :: Stemp
   REAL :: Tbar
   REAL , DIMENSION(3,1) :: G2alfb
   LOGICAL :: Bendng
   INTEGER , DIMENSION(1) :: Idr
!
! Local variable declarations rewritten by SPAG
!
   REAL :: const , fiber , t3ov12 , thick , tprime , tsubi
   LOGICAL , SAVE :: cosmic
   REAL , SAVE :: epss
   INTEGER :: i , inplan , inptmp , ist , istres , its , iz , j , nump , nump1
   INTEGER , DIMENSION(1) :: nstres
   REAL , DIMENSION(3,3) :: s1mat , s2mat
   REAL , DIMENSION(3) :: sigma
   REAL , DIMENSION(4) :: sigmap
   EXTERNAL errtrc , shpsts
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     TO CALCULATE SHELL ELEMENT STRESSES FOR A 2-D FORMULATION BASE.
!     COMPOSITE LAYER STRESSES ARE NOT CALCULATED IN THIS ROUTINE.
!
!
!     INPUT :
!           NUMPX  - NUMBER OF EVALUATION POINTS
!           ELID   - ELEMENT ID
!           IGRID  - ARRAY IF EXTERNAL GRID IDS
!           THIKNS - EVALUATION POINT THICKNESSES
!           Z12    - EVALUATION POINT FIBER DISTANCES
!           G      - 6X6 STRESS-STRAIN MATRIX
!           EPSCSI - CORRECTED STRAINS AT EVALUATION POINTS
!           STEMP  - TEMPERATURE DATA FOR STRESS RECOVERY
!           TBAR   - AVERAGE ELEMENT TEMPERATURE
!           G2ALFB - MATRIX USED IN RECORRECTING OF STRESSES
!           BENDNG - INDICATES THE PRESENCE OF BENDING BEHAVIOR
!           IDR    - REORDERING ARRAY BASED ON EXTERNAL GRID POINT ID'S
!          /TMPDAT/- TEMPERATURE-RELATED LOGICAL FLAGS
!          /OUTREQ/- OUTPUT REQUEST LOGICAL FLAGS
!
!     OUTPUT:
!           STRESSES ARE PLACED AT THE PROPER LOCATION IN /SDR2X7/.
!
!
!     THE STRESS OUTPUT DATA BLOCK (UAI CODE)
!
!     ADDRESS    DESCRIPTIONS
!
!        1       ELID
!     -------------------------------------------------------
!        2       'CNTR'
!        3       LOWER FIBER DISTANCE
!      4 - 10    STRESSES FOR LOWER POINTS AT ELEMENT CENTER POINT
!       11       UPER  FIBER DISTANCE
!     12 - 18    STRESSES FOR UPPER POINTS AT ELEMENT CENTER POINT
!       19       FIRST GRID POINT NUMBER
!     20 - 35    REPEAT  3 TO 18 ABOVE FOR FIRST  GRID POINT
!     36 - 52    REPAET 19 TO 36 ABOVE FOR SECOND GRID POINT
!     53 - 69    REPAET 19 TO 36 ABOVE FOR THIRD  GRID POINT
!
!
!     THE STRESS OUTPUT DATA BLOCK AT ELEMENT CENTER ONLY, COSMIC
!
!     ADDRESS    DESCRIPTIONS
!
!        1       ELID
!     -------------------------------------------------------
!        2       LOWER FIBER DISTANCE
!      3 -  9    STRESSES FOR LOWER POINTS AT ELEMENT CENTER POINT
!       10       UPER  FIBER DISTANCE
!     11 - 17    STRESSES FOR UPPER POINTS AT ELEMENT CENTER POINT
!
!
   !>>>>EQUIVALENCE (Nstres(1),Stres(1))
   DATA cosmic , epss/.TRUE. , 1.0E-11/
!
!
!     ELEMENT CENTER POINT COMPUTAION ONLY FOR COSMIC,
!     I.E. THE CALLER SHOULD PASS 1 IN NUMPX FOR COSMIC, 4 FOR UAI
!
   nump = Numpx
   IF ( cosmic ) nump = 1
!
   nstres(1) = Elid
!
!     START THE LOOP ON EVALUATION POINTS
!
   nump1 = nump - 1
   DO inplan = 1 , nump
      thick = Thikns(inplan)
      t3ov12 = thick*thick*thick/12.0
!
      istres = 1
      IF ( .NOT.(cosmic) ) THEN
!
         istres = (inplan-1)*17 + 2
         nstres(istres) = inplan - 1
         IF ( .NOT.(.NOT.grids .OR. inplan<=1) ) THEN
            DO inptmp = 1 , nump1
               IF ( Idr(inptmp)==Igrid(inplan) ) GOTO 10
            ENDDO
            CALL errtrc('SHSTSS  ',100)
 10         istres = inptmp*17 + 2
            nstres(istres) = Igrid(inplan)
         ENDIF
         IF ( inplan==1 ) nstres(istres) = Igrid(inplan)
      ENDIF
!
!
!     START THE LOOP ON FIBERS
!
      DO iz = 1 , 2
         spag_nextblock_1 = 1
         SPAG_DispatchLoop_1: DO
            SELECT CASE (spag_nextblock_1)
            CASE (1)
               fiber = Z12(iz,inplan)
               stres(istres+1) = fiber
               const = 12.0*fiber/thick
!
!     CREATE [S1] AND [S2]
!
               DO i = 1 , 3
                  DO j = 1 , 3
                     s1mat(i,j) = G(i,j) - const*G(i,j+3)
                     s2mat(i,j) = G(i+3,j+3) - const*G(i,j+3)
                  ENDDO
               ENDDO
!
!     EVALUATE STRESSES AT THIS FIBER DISTANCE
!
               DO i = 1 , 3
                  sigma(i) = 0.0
                  DO j = 1 , 3
                     sigma(i) = sigma(i) + s1mat(i,j)*Epscsi(j,inplan) - fiber*s2mat(i,j)*Epscsi(j+3,inplan)
                  ENDDO
               ENDDO
!
!     IF TEMPERATURES ARE PRESENT, RECORRECT STRESSES FOR THERMAL
!     STRESSES RESULTING FROM TEMPERATURE VALUES AT FIBER DISTANCES.
!
               IF ( .NOT.(.NOT.temper .OR. .NOT.Bendng) ) THEN
                  IF ( .NOT.tempp1 ) THEN
!
                     IF ( .NOT.tempp2 ) THEN
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     tsubi = Stemp(4+iz)
                     IF ( abs(tsubi)<epss ) THEN
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     DO ist = 1 , 3
                        sigma(ist) = sigma(ist) - Stemp(ist+1)*fiber/t3ov12
                     ENDDO
                  ELSE
                     tprime = Stemp(2)
                     tsubi = Stemp(2+iz)
                     IF ( abs(tsubi)<epss ) THEN
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     tsubi = tsubi - tprime*fiber
                  ENDIF
!
                  tsubi = tsubi - Tbar
                  DO its = 1 , 3
                     sigma(its) = sigma(its) - tsubi*G2alfb(its,inplan)
                  ENDDO
               ENDIF
               spag_nextblock_1 = 2
            CASE (2)
!
!     CLEANUP AND SHIP CORRECTED STRESSES
!
               DO its = 1 , 3
                  IF ( abs(sigma(its))<=epss ) sigma(its) = 0.0
                  stres(istres+1+its) = sigma(its)
               ENDDO
!
!     CALCULATE PRINCIPAL STRESSES
!
               CALL shpsts(sigma,vonms,sigmap)
               stres(istres+5) = sigmap(1)
               stres(istres+6) = sigmap(2)
               stres(istres+7) = sigmap(3)
               stres(istres+8) = sigmap(4)
!
               istres = istres + 8
               EXIT SPAG_DispatchLoop_1
            END SELECT
         ENDDO SPAG_DispatchLoop_1
      ENDDO
   ENDDO
!
END SUBROUTINE shstss
