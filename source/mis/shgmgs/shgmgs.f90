!*==shgmgs.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE shgmgs(Elid,Tem,Mid,Ts,Noalfa,G,Rho,Gsube,Tsub0,Egnor,Alpha) !HIDESTARS (*,Elid,Tem,Mid,Ts,Noalfa,G,Rho,Gsube,Tsub0,Egnor,Alpha)
   USE c_matin
   USE c_matout
   USE c_terms
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Elid
   REAL , DIMENSION(9) :: Tem
   INTEGER , DIMENSION(4) :: Mid
   REAL :: Ts
   LOGICAL :: Noalfa
   REAL , DIMENSION(36) :: G
   REAL :: Rho
   REAL :: Gsube
   REAL :: Tsub0
   REAL , DIMENSION(4) :: Egnor
   REAL , DIMENSION(6) :: Alpha
!
! Local variable declarations rewritten by SPAG
!
   REAL :: bdum , const , detu , dn12 , dn21 , e1 , e2 , g12x , g1z , g2z , nu12 , nu21 , ps1 , ps2 , tmtset
   REAL , DIMENSION(9) :: gt , u
   INTEGER :: ig , igobk , isngu , it0 , l , lpoint , m , morb , mtype
   INTEGER , DIMENSION(3,3) :: index
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL , DIMENSION(6) :: talpha
   EXTERNAL gmmats , invers , mat
!
! End of declarations rewritten by SPAG
!
!
!     MATERIAL PROPERTY G-MATRICES GENERATOR FOR SHELL ELEMENTS
!
!     SINGLE PRECISION VERSION
!
!     INPUT :
!           ELID   - ELEMENT ID
!           TEM    - 3X3 TRANSFORMATION BETWEEN ELEMENT AND MATERIAL
!                    COORDINATE SYSTEMS
!           MID    - ARRAY OF LENGTH 4, CONTAINS MATERIAL ID'S
!           TS     - EQUIVALENT SHEAR THICKNESS
!           NOALFA - LOGICAL TO REQUEST OR NOT REQUEST THERMAL EXPANSION
!                    COEFFICIENTS
!
!     OUTPUT:
!           G      - ARRAY OF LENGTH 36 (FOUR 3X3), CONATINS MATERIAL
!                    PROPERTIES IN ELEMENT COORD. SYSTEM
!           RHO    - MASS DENSITY FROM MEMBRANE MATERIAL
!           GSUBE  - DAMPING COEFFICIENT FROM MEMBRANE OR BENDING
!                    MATERIALS
!           TSUB0  - REFERENCE TEMPERATURE
!           EGNOR  - ARRAY OF PSEUDO E'S AND G'S FOR SHEAR FACTOR
!                    CALCULATIONS IN BENDING
!           ALPHA  - ARRAY OF THERMAL EXPANSION COEFFICIENTS
!
!     NOTES:
!           1- THIS ROUTINE BUILDS THE MATERIAL PROPERTY MATRIX USING
!              THE OUTPUT OF SUBROUTINE 'MAT' (/MATOUT/).
!              /MATOUT/ IS IN MAT2 FORMAT IF MAT1 AND/OR MAT2 ARE USED
!              /MATOUT/ IS IN MAT8 FORMAT IF MAT8 CARD IS REQUESTED.
!           2- ISOTROPIC, ORTHOTROPIC, AND ANISOTROPIC PROPERTY TYPES
!              ARE SUPPORTED.
!           3- PROPERTIES FOR MEMBRANE, BENDING, SHEAR FLEXIBILITY, AND
!              MEMBRANE/BENDING COUPLING ARE PROCESSED.
!           4- NON-STANDARD RETURN IS TAKEN WHEN THE MATERIAL FOR SHEAR
!              FLEXIBILITY IS NOT PROPERLY DEFINED.
!           5- SOME OF THE CONTENTS OF /MATIN/ MUST BE DEFINED PRIOR TO
!              A CALL TO THIS ROUTINE.
!           6- CONTENTS OF /TERMS/, MID, AND TS MAY BE CHANGED IN THIS
!              ROUTINE.
!
!
!
!                      MAT8 FORMAT...
   !>>>>EQUIVALENCE (E1,G11) , (Nu12,G12) , (E2,G13) , (G2z,G23) , (G1z,G33) , (G12x,G22)
!    2,                (GE  ,E  ),(T0,ALPH12)
!             EQUIV    (MATOUT(1),G11))
   DATA name/4HSHGM , 4HGS  /
!
!
!     INITIALIZE
!
!     SET INFLAG = 12 SO THAT SUBROUTINE MAT WILL SEARCH FOR:
!     ISOTROPIC   MATERIAL PROPERTIES AMONG THE MAT1 CARDS,
!     ORTHOTROPIC MATERIAL PROPERTIES AMONG THE MAT8 CARDS, AND
!     ANISOTROPIC MATERIAL PROPERTIES AMONG THE MAT2 CARDS.
!
   DO ig = 1 , 36
      G(ig) = 0.0
   ENDDO
   DO ig = 1 , 4
      Egnor(ig) = 0.0
   ENDDO
   IF ( .NOT.(Noalfa) ) THEN
      DO ig = 1 , 6
         Alpha(ig) = 0.0
      ENDDO
   ENDIF
!
   Rho = 0.0
   Gsube = 0.0
   Tsub0 = 0.0
   inflag = 12
   igobk = 0
   it0 = 0
!
!     BEGIN THE LOOP TO FETCH PROPERTIES FOR EACH MATERIAL ID. FOR SHEAR
!     FLEXIBILITY MATERIAL, DEFAULT TO THE BENDING MATERIAL IF BENDING
!     IS PRESENT.
!     IF SHEAR MATERIAL IS PRESENT, BUT YIELDS ZEROES, GO BACK AND RESET
!     IT TO BENDING MATERIAL.
!
   m = 0
   SPAG_Loop_1_1: DO
      lpoint = m*9
      m = m + 1
      IF ( m>4 ) THEN
!
!
!     LOOP IS DONE, CHECK FOR ALL ZEROES FOR SHEAR MATERIAL
!
         IF ( G(19)/=0.0 .OR. G(20)/=0.0 .OR. G(21)/=0.0 .OR. G(22)/=0.0 ) EXIT SPAG_Loop_1_1
         igobk = 1
         m = 2
         Mid(3) = 0
         shrflx = .FALSE.
!              0.833333333 = 5.0/6.0
         Ts = 0.833333333
      ELSE
         IF ( m==4 .AND. igobk==1 ) EXIT SPAG_Loop_1_1
         matid = Mid(m)
         IF ( matid==0 .AND. m/=3 ) CYCLE
         IF ( matid==0 .AND. m==3 .AND. .NOT.bendng ) CYCLE
         IF ( matid==0 .AND. m==3 .AND. bendng ) matid = Mid(2)
!
         IF ( m>=1 ) THEN
            IF ( m/=1 ) THEN
               IF ( matid==Mid(m-1) .AND. igobk==0 ) GOTO 20
            ENDIF
            CALL mat(Elid)
            tmtset = matset
            IF ( matset==8.0 ) tmtset = 3.0
            mtype = ifix(tmtset+0.05) - 2
         ENDIF
!
!     SET THE MISC ITEMS
!
 20      IF ( membrn .AND. m==1 ) Rho = rhox
         IF ( .NOT.(membrn .AND. m/=1 .OR. .NOT.membrn .AND. m/=2) ) THEN
            Gsube = ge
            IF ( mtype>0 ) Gsube = e
         ENDIF
         IF ( it0<=0 ) THEN
            it0 = 1
            Tsub0 = tref
            IF ( mtype>0 ) Tsub0 = alph12
         ENDIF
!
!     BRANCH ON MATERIAL TYPE
!
         IF ( mtype<0 ) THEN
!               MAT1,MAT2,MAT8
!
!
!     ISOTROPIC  MATERIALS (MAT1)
!     ---------------------------
!
! 200 IF (M .NE. 3) GO TO 205
            IF ( m==3 ) THEN
!
!     G(LPOINT+1) = MATOUT(3)   <== G13, SHOULD BE MATOUT(6) <== G33
!     G(LPOINT+4) = G(LPOINT+1)
!     IF (G(LPOINT+1).EQ.0.0 .AND. SHRFLX) GO TO 300
!
               G(lpoint+1) = g33
               G(lpoint+4) = g33
               IF ( .NOT.(g33==0.0 .AND. shrflx) ) GOTO 60
               GOTO 40
            ENDIF
         ELSEIF ( mtype==0 ) THEN
!
!     ACCORDING TO Q4GMGS, SHOULD TO TO 220 NEXT
!
! 205 G(LPOINT+1) = G22
!     G(LPOINT+2) = G12*G22
!     G(LPOINT+4) = G12*G22
!     G(LPOINT+5) = G22
!     G(LPOINT+9) = G13         <== G13,  SHOULD IT BE G33 ??
!     GO TO 400
!
!     ANISOTROPIC  MATERIALS (MAT2)
!     -----------------------------
!
            IF ( m==3 ) THEN
!
               IF ( shrflx ) THEN
!
                  G(lpoint+1) = g11
                  G(lpoint+2) = g12
                  G(lpoint+3) = g12
                  G(lpoint+4) = g22
                  IF ( g33==0.0 ) GOTO 60
                  GOTO 40
               ELSE
                  IF ( g11/=0.0 .AND. g22/=0.0 ) THEN
                     dn21 = g12/g11
                     dn12 = g12/g22
                     const = dn21*dn12
                     IF ( const>=0.0 ) THEN
                        ps1 = g11*(1.0-const)
                        ps2 = g22*(1.0-const)
                        IF ( const>0.0 ) const = sqrt(const)
                        const = 2.0*(1.0+const)
                        G(lpoint+1) = ps1/const
                        G(lpoint+4) = ps2/const
                     ENDIF
                  ENDIF
                  GOTO 60
               ENDIF
            ENDIF
!
!     ORTHOTROPIC MATERIALS (MAT8)
!     ----------------------------
!
         ELSEIF ( m==3 ) THEN
!
            IF ( shrflx ) THEN
!
! 270 G(LPOINT+1) = MATOUT(5)         <== COSMIC (5) & (6) INTERCHANGED
!     G(LPOINT+4) = MATOUT(6)
               G(lpoint+1) = g1z
               G(lpoint+4) = g2z
               IF ( g1z/=0.0 .OR. g2z/=0.0 ) GOTO 60
               GOTO 40
            ELSE
               IF ( e1/=0.0 ) THEN
                  nu21 = nu12*e2/e1
                  const = nu21*nu12
                  IF ( const>0.0 ) THEN
                     const = sqrt(const)
                     const = 2.0*(1.0+const)
                     G(lpoint+1) = e1/const
                     G(lpoint+4) = e2/const
                  ENDIF
               ENDIF
               GOTO 60
            ENDIF
         ELSE
            IF ( e1/=0.0 ) THEN
               nu21 = nu12*e2/e1
               const = 1.0 - nu21*nu12
               IF ( const>0.0 ) THEN
                  G(lpoint+1) = e1/const
                  G(lpoint+2) = nu12*e2/const
                  G(lpoint+4) = G(lpoint+2)
                  G(lpoint+5) = e2/const
                  G(lpoint+9) = g12x
               ENDIF
            ENDIF
            GOTO 60
         ENDIF
         G(lpoint+1) = g11
         G(lpoint+2) = g12
         G(lpoint+3) = g13
         G(lpoint+4) = g12
         G(lpoint+5) = g22
         G(lpoint+6) = g23
         G(lpoint+7) = g13
         G(lpoint+8) = g23
         G(lpoint+9) = g33
         GOTO 60
!
!     BAD SHEAR MATERIAL
!
 40      IF ( .NOT.(.NOT.shrflx .AND. bendng) ) RETURN 1
!
!     TRANSFORM NON-ISOTROPIC MATERIALS
!
 60      IF ( mtype>=0 ) THEN
            IF ( m==3 ) THEN
!
               u(1) = Tem(5)*Tem(9) + Tem(6)*Tem(8)
               u(2) = Tem(2)*Tem(9) + Tem(8)*Tem(3)
               u(3) = Tem(4)*Tem(9) + Tem(7)*Tem(6)
               u(4) = Tem(1)*Tem(9) + Tem(3)*Tem(7)
               l = 2
            ELSE
               u(1) = Tem(1)*Tem(1)
               u(2) = Tem(4)*Tem(4)
               u(3) = Tem(1)*Tem(4)
               u(4) = Tem(2)*Tem(2)
               u(5) = Tem(5)*Tem(5)
               u(6) = Tem(2)*Tem(5)
               u(7) = Tem(1)*Tem(2)*2.0
               u(8) = Tem(4)*Tem(5)*2.0
               u(9) = Tem(1)*Tem(5) + Tem(2)*Tem(4)
               l = 3
            ENDIF
!
            CALL gmmats(u(1),l,l,1,G(lpoint+1),l,l,0,gt(1))
            CALL gmmats(gt(1),l,l,0,u(1),l,l,0,G(lpoint+1))
         ENDIF
!
!     GET THE THERMAL EXPANSION COEFFICIENTS, IF NEEDED
!
         IF ( .NOT.(Noalfa .OR. m>2) ) THEN
            morb = (m-1)*3
            IF ( mtype<0 ) THEN
!                MAT1,MAT2,MAT8
!
!     MAT1
!
               Alpha(morb+1) = alph1
               Alpha(morb+2) = alph1
               Alpha(morb+3) = 0.0
               CYCLE
            ELSEIF ( mtype==0 ) THEN
!
!     MAT2
!
               Alpha(morb+1) = alph1
               Alpha(morb+2) = alph2
               Alpha(morb+3) = alph12
            ELSE
!
!     MAT8
!
               Alpha(morb+1) = alph1
               Alpha(morb+2) = alph2
               Alpha(morb+3) = 0.0
            ENDIF
!
!     TRANSFORM THERMAL EXPANSION COEFFICIENTS AND STORE THEM IN ALPHA.
!     THE ALPHAS NEED TO BE PREMULTIPLIED BY [U] INVERSE.
!
            DO ig = 1 , 3
               talpha(ig+morb) = Alpha(ig+morb)
            ENDDO
            morb = morb + 1
            CALL invers(3,u,3,bdum,0,detu,isngu,index)
            CALL gmmats(u,3,3,0,talpha(morb),3,1,0,Alpha(morb))
         ENDIF
      ENDIF
   ENDDO SPAG_Loop_1_1
!
!     SAVE PSEUDO E'S AND G'S FOR SHEAR FACTOR CALCULATIONS
!
   IF ( bendng ) THEN
      Egnor(1) = G(10)
      Egnor(2) = G(14)
      Egnor(3) = G(19)
      Egnor(4) = G(22)
   ENDIF
!
END SUBROUTINE shgmgs
