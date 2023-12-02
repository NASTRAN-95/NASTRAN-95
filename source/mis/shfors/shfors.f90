!*==shfors.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE shfors(Numpx,Elid,Igrid,Thikns,G,Epscsi,Qveci,Idr)
   USE c_outreq
   USE c_sdr2x7
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Numpx
   INTEGER :: Elid
   INTEGER , DIMENSION(1) :: Igrid
   REAL , DIMENSION(1) :: Thikns
   REAL , DIMENSION(6,6) :: G
   REAL , DIMENSION(6,1) :: Epscsi
   REAL , DIMENSION(2,1) :: Qveci
   INTEGER , DIMENSION(1) :: Idr
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL , SAVE :: cosmic
   REAL , DIMENSION(8) :: dforce
   REAL , DIMENSION(6,6) :: gt
   INTEGER :: ifor , iforce , ig , inplan , inptmp , jg , nump , nump1
   INTEGER , DIMENSION(1) :: nfors
   REAL :: t3ov12 , thick , thick2
   EXTERNAL gmmats
!
! End of declarations rewritten by SPAG
!
!
!     TO CALCULATE SHELL ELEMENT FORCES FOR A 2-DL FORMULATION BASE.
!
!
!     INPUT :
!           NUMPX  - NUMBER OF EVALUATION POINTS
!           ELID   - ELEMENT ID
!           IGRID  - ARRAY IF EXTERNAL GRID IDS
!           THIKNS - EVALUATION POINT THICKNESSES
!           G      - 6X6 STRESS-STRAIN MATRIX
!           EPSCSI - CORRECTED STRAINS AT EVALUATION POINTS
!           QVECI  - CALCULATED SHEAR FORCES READY FOR OUTPUT
!           IDR    - REORDERING ARRAY BASED ON EXTERNAL GRID POINT ID'S
!          /OUTREQ/- OUTPUT REQUEST LOGICAL FLAGS
!
!     OUTPUT:
!            FORCES ARE PLACED AT THE PROPER LOCATION IN /SDR2X7/.
!
!
!     THE FORCE RESULTANT OUTPUT DATA BLOCK, UAI CODE
!
!     ADDRESS    DESCRIPTIONS
!
!        1       ELID
!     ------------------------------------------------
!        2       GRID POINT NUMBER OR 'CNTR'
!      3 - 10    FORCES AT ELEMENT CENTER POINT
!     ---------- ABOVE DATA REPEATED 3 TIMES
!                FOR GRID POINTS
!
!
!     THE FORCE RESULTANT OUTPUT DATA BLOCK AT ELEMETN CENTER, COSMIC
!
!     ADDRESS    DESCRIPTIONS
!
!        1       ELID
!     ------------------------------------------------
!      2 - 9     FORCES AT ELEMENT CENTER POINT
!
!
   !>>>>EQUIVALENCE (Nfors(1),Forsul(1))
   DATA cosmic/.TRUE./
!
!
!     ELEMENT CENTER POINT COMPUTAION ONLY FOR COSMIC
!     IE. CALLER SHOULD PASS 1 IN NUMPX FOR COSMIC, 4 FOR UAI
!
   nump = Numpx
   IF ( cosmic ) nump = 1
!
   nfors(1) = Elid
!
!     START THE LOOP ON EVALUATION POINTS
!
   nump1 = nump - 1
   DO inplan = 1 , nump
      thick = Thikns(inplan)
      thick2 = thick*thick
      t3ov12 = thick2*thick/12.0
!
      iforce = 1
      IF ( .NOT.(cosmic) ) THEN
!
         iforce = (inplan-1)*9 + 2
         IF ( .NOT.(grids .AND. gridss) .OR. inplan<=1 ) THEN
            nfors(iforce) = inplan - 1
         ELSE
            SPAG_Loop_2_1: DO inptmp = 1 , nump1
               IF ( Idr(inptmp)==Igrid(inplan) ) EXIT SPAG_Loop_2_1
            ENDDO SPAG_Loop_2_1
            iforce = inptmp*9 + 2
            nfors(iforce) = Igrid(inplan)
         ENDIF
         IF ( inplan==1 ) nfors(iforce) = Igrid(inplan)
      ENDIF
!
!     MODIFY [G], THEN CALCULATE FORCES AND MOMENTS
!
      DO ig = 1 , 3
         DO jg = 1 , 3
            gt(ig,jg) = thick*G(ig,jg)
            gt(ig+3,jg) = thick2*G(ig+3,jg)
            gt(ig,jg+3) = thick2*G(ig,jg+3)
            gt(ig+3,jg+3) = t3ov12*G(ig+3,jg+3)
         ENDDO
      ENDDO
      CALL gmmats(gt,6,6,0,Epscsi(1,inplan),6,1,0,dforce(1))
!
!     OUTPUT QX AND QY (WE HAVE CALCULATED QY AND QX)
!
      dforce(7) = Qveci(2,inplan)
      dforce(8) = Qveci(1,inplan)
!
!     SHIP OUT
!
      DO ifor = 1 , 8
         forsul(iforce+ifor) = dforce(ifor)
      ENDDO
   ENDDO
!
END SUBROUTINE shfors
