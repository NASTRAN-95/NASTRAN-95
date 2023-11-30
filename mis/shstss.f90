
SUBROUTINE shstss(Numpx,Elid,Igrid,Thikns,Z12,G,Epscsi,Stemp,Tbar,G2alfb,Bendng,Idr)
   IMPLICIT NONE
   REAL Dum71(100) , Forsul(200) , Stres(100) , Strin(100)
   LOGICAL Forreq , Grids , Gridss , Layer , Layers , Stnreq , Strcur , Stsreq , Temper , Tempp1 , Tempp2 , Vonms , Vonmss
   INTEGER Nstres(1)
   COMMON /outreq/ Stsreq , Stnreq , Forreq , Strcur , Grids , Vonms , Layer , Gridss , Vonmss , Layers
   COMMON /sdr2x7/ Dum71 , Stres , Forsul , Strin
   COMMON /tmpdat/ Temper , Tempp1 , Tempp2
   LOGICAL Bendng
   INTEGER Elid , Numpx
   REAL Tbar
   REAL Epscsi(6,1) , G(6,6) , G2alfb(3,1) , Stemp(2) , Thikns(1) , Z12(2,1)
   INTEGER Idr(1) , Igrid(1)
   REAL const , epss , fiber , s1mat(3,3) , s2mat(3,3) , sigma(3) , sigmap(4) , t3ov12 , thick , tprime , tsubi
   LOGICAL cosmic
   INTEGER i , inplan , inptmp , ist , istres , its , iz , j , nump , nump1
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
   Nstres(1) = Elid
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
         Nstres(istres) = inplan - 1
         IF ( .NOT.(.NOT.Grids .OR. inplan<=1) ) THEN
            DO inptmp = 1 , nump1
               IF ( Idr(inptmp)==Igrid(inplan) ) GOTO 10
            ENDDO
            CALL errtrc('SHSTSS  ',100)
 10         istres = inptmp*17 + 2
            Nstres(istres) = Igrid(inplan)
         ENDIF
         IF ( inplan==1 ) Nstres(istres) = Igrid(inplan)
      ENDIF
!
!
!     START THE LOOP ON FIBERS
!
      DO iz = 1 , 2
         fiber = Z12(iz,inplan)
         Stres(istres+1) = fiber
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
         IF ( .NOT.(.NOT.Temper .OR. .NOT.Bendng) ) THEN
            IF ( .NOT.Tempp1 ) THEN
!
               IF ( .NOT.Tempp2 ) GOTO 20
               tsubi = Stemp(4+iz)
               IF ( abs(tsubi)<epss ) GOTO 20
               DO ist = 1 , 3
                  sigma(ist) = sigma(ist) - Stemp(ist+1)*fiber/t3ov12
               ENDDO
            ELSE
               tprime = Stemp(2)
               tsubi = Stemp(2+iz)
               IF ( abs(tsubi)<epss ) GOTO 20
               tsubi = tsubi - tprime*fiber
            ENDIF
!
            tsubi = tsubi - Tbar
            DO its = 1 , 3
               sigma(its) = sigma(its) - tsubi*G2alfb(its,inplan)
            ENDDO
         ENDIF
!
!     CLEANUP AND SHIP CORRECTED STRESSES
!
 20      DO its = 1 , 3
            IF ( abs(sigma(its))<=epss ) sigma(its) = 0.0
            Stres(istres+1+its) = sigma(its)
         ENDDO
!
!     CALCULATE PRINCIPAL STRESSES
!
         CALL shpsts(sigma,Vonms,sigmap)
         Stres(istres+5) = sigmap(1)
         Stres(istres+6) = sigmap(2)
         Stres(istres+7) = sigmap(3)
         Stres(istres+8) = sigmap(4)
!
         istres = istres + 8
      ENDDO
   ENDDO
!
END SUBROUTINE shstss