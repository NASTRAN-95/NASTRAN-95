
SUBROUTINE shgmgd(*,Elid,Tem,Mid,Ts,Noalfa,G,Rho,Gsube,Tsub0,Egnor,Alpha)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Alph1 , Alph12 , Alph2 , Cosmat , Dum(8) , Dummy , E , E1 , E2 , Eltemp , G11 , G12 , G12x , G13 , G1z , G22 , G23 , G2z ,  &
      & G33 , Ge , Matset , Nu12 , Rhox , Sc , Sinmat , Ss , St , Tref
   LOGICAL Bendng , Mbcoup , Membrn , Norpth , Shrflx
   INTEGER Inflag , Matid
   COMMON /matin / Matid , Inflag , Eltemp , Dummy , Sinmat , Cosmat
   COMMON /matout/ G11 , G12 , G13 , G22 , G23 , G33 , Rhox , Alph1 , Alph2 , Alph12 , Tref , Ge , St , Sc , Ss , E , Dum , Matset
   COMMON /terms / Membrn , Bendng , Shrflx , Mbcoup , Norpth
!
! Dummy argument declarations
!
   INTEGER Elid
   REAL Gsube , Tsub0
   LOGICAL Noalfa
   DOUBLE PRECISION Rho , Ts
   DOUBLE PRECISION Alpha(6) , Egnor(4) , G(36) , Tem(9)
   INTEGER Mid(4)
!
! Local variable declarations
!
   DOUBLE PRECISION bdum , const , detu , dn12 , dn21 , gt(9) , ps1 , ps2 , talpha(6) , u(9)
   INTEGER ig , igobk , index(3,3) , isngu , it0 , l , lpoint , m , morb , mtype , name(2)
   REAL nu21 , tmtset
!
! End of declarations
!
!
!     MATERIAL PROPERTY G-MATRICES GENERATOR FOR SHELL ELEMENTS
!
!     DOUBLE PRECISION VERSION
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
   EQUIVALENCE (E1,G11) , (Nu12,G12) , (E2,G13) , (G2z,G23) , (G1z,G33) , (G12x,G22)
!    2,                (GE ,E  ),(T0,ALPH12)
!             EQUIV    (MATOUT(1),G11))
   DATA name/4HSHGM , 4HGD  /
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
      G(ig) = 0.0D0
   ENDDO
   DO ig = 1 , 4
      Egnor(ig) = 0.0D0
   ENDDO
   IF ( .NOT.(Noalfa) ) THEN
      DO ig = 1 , 6
         Alpha(ig) = 0.0D0
      ENDDO
   ENDIF
!
   Rho = 0.0D0
   Gsube = 0.0
   Tsub0 = 0.0
   Inflag = 12
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
   DO
      lpoint = m*9
      m = m + 1
      IF ( m>4 ) THEN
!
!
!     LOOP IS DONE, CHECK FOR ALL ZEROES FOR SHEAR MATERIAL
!
         IF ( G(19)/=0.0D0 .OR. G(20)/=0.0D0 .OR. G(21)/=0.0D0 .OR. G(22)/=0.0D0 ) EXIT
         igobk = 1
         m = 2
         Mid(3) = 0
         Shrflx = .FALSE.
!              0.833333333D0 = 5.0D0/6.0D0
         Ts = 0.833333333D0
      ELSE
         IF ( m==4 .AND. igobk==1 ) EXIT
         Matid = Mid(m)
         IF ( Matid==0 .AND. m/=3 ) CYCLE
         IF ( Matid==0 .AND. m==3 .AND. .NOT.Bendng ) CYCLE
         IF ( Matid==0 .AND. m==3 .AND. Bendng ) Matid = Mid(2)
!
         IF ( m<1 ) GOTO 20
         IF ( m/=1 ) THEN
            IF ( Matid==Mid(m-1) .AND. igobk==0 ) GOTO 20
         ENDIF
         CALL mat(Elid)
         tmtset = Matset
         IF ( Matset==8.0 ) tmtset = 3.0
         mtype = ifix(tmtset+0.05) - 2
!
!     SET THE MISC ITEMS
!
 20      IF ( Membrn .AND. m==1 ) Rho = dble(Rhox)
         IF ( .NOT.(Membrn .AND. m/=1 .OR. .NOT.Membrn .AND. m/=2) ) THEN
            Gsube = Ge
            IF ( mtype>0 ) Gsube = E
         ENDIF
         IF ( it0<=0 ) THEN
            it0 = 1
            Tsub0 = Tref
            IF ( mtype>0 ) Tsub0 = Alph12
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
!     G(LPOINT+1) = MATOUT(3)    <== G13, SHOULD BE MATOUT(6) <== G33
!     G(LPOINT+4) = G(LPOINT+1)
!     IF (G(LPOINT+1).EQ.0.0 .AND. SHRFLX) GO TO 300
!
               G(lpoint+1) = G33
               G(lpoint+4) = G33
               IF ( .NOT.(G33==0.0 .AND. Shrflx) ) GOTO 60
               GOTO 40
            ENDIF
         ELSEIF ( mtype==0 ) THEN
!
!     ACCORDING TO Q4GMGD, SHOULD GO TO 220 NEXT
!
! 205 G(LPOINT+1) = G22
!     G(LPOINT+2) = G12*G22
!     G(LPOINT+4) = G12*G22
!     G(LPOINT+5) = G22
!     G(LPOINT+0) = G13         <== G13,  SHOULD IT BE G33 ??
!     GO TO 400
!
!     ANISOTROPIC  MATERIALS (MAT2)
!     -----------------------------
!
            IF ( m==3 ) THEN
!
               IF ( Shrflx ) THEN
!
                  G(lpoint+1) = G11
                  G(lpoint+2) = G12
                  G(lpoint+3) = G12
                  G(lpoint+4) = G22
                  IF ( G33==0.0 ) GOTO 60
                  GOTO 40
               ELSE
                  IF ( G11/=0.0 .AND. G22/=0.0 ) THEN
                     dn21 = G12/G11
                     dn12 = G12/G22
                     const = dn21*dn12
                     IF ( const>=0.0D0 ) THEN
                        ps1 = dble(G11)*(1.0D0-const)
                        ps2 = dble(G22)*(1.0D0-const)
                        IF ( const>0.0D0 ) const = dsqrt(const)
                        const = 2.0D0*(1.0D0+const)
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
            IF ( Shrflx ) THEN
!
! 270 G(LPOINT+1) = MATOUT(5)        <== COSMIC (5) & (6) INTERCHANGED
!     G(LPOINT+4) = MATOUT(6)
               G(lpoint+1) = G1z
               G(lpoint+4) = G2z
               IF ( G1z/=0.0 .OR. G2z/=0.0 ) GOTO 60
               GOTO 40
            ELSE
               IF ( E1/=0.0 ) THEN
                  nu21 = Nu12*E2/E1
                  const = nu21*Nu12
                  IF ( const>0.0D0 ) THEN
                     const = dsqrt(const)
                     const = 2.0D0*(1.0D0+const)
                     G(lpoint+1) = dble(E1)/const
                     G(lpoint+4) = dble(E2)/const
                  ENDIF
               ENDIF
               GOTO 60
            ENDIF
         ELSE
            IF ( E1/=0.0 ) THEN
               nu21 = Nu12*E2/E1
               const = 1.0D0 - dble(nu21*Nu12)
               IF ( const>0.0D0 ) THEN
                  G(lpoint+1) = E1/const
                  G(lpoint+2) = Nu12*E2/const
                  G(lpoint+4) = G(lpoint+2)
                  G(lpoint+5) = E2/const
                  G(lpoint+9) = G12x
               ENDIF
            ENDIF
            GOTO 60
         ENDIF
         G(lpoint+1) = G11
         G(lpoint+2) = G12
         G(lpoint+3) = G13
         G(lpoint+4) = G12
         G(lpoint+5) = G22
         G(lpoint+6) = G23
         G(lpoint+7) = G13
         G(lpoint+8) = G23
         G(lpoint+9) = G33
         GOTO 60
!
!     BAD SHEAR MATERIAL
!
 40      IF ( .NOT.(.NOT.Shrflx .AND. Bendng) ) RETURN 1
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
               u(7) = Tem(1)*Tem(2)*2.0D0
               u(8) = Tem(4)*Tem(5)*2.0D0
               u(9) = Tem(1)*Tem(5) + Tem(2)*Tem(4)
               l = 3
            ENDIF
!
            CALL gmmatd(u(1),l,l,1,G(lpoint+1),l,l,0,gt(1))
            CALL gmmatd(gt(1),l,l,0,u(1),l,l,0,G(lpoint+1))
         ENDIF
!
!     GET THE THERMAL EXPANSION COEFFICIENTS, IF NEEDED
!
         IF ( .NOT.(Noalfa .OR. m>2) ) THEN
            morb = (m-1)*3
            IF ( mtype<0 ) THEN
!               MAT1,MAT2,MAT8
!
!     MAT1
!
               Alpha(morb+1) = Alph1
               Alpha(morb+2) = Alph1
               Alpha(morb+3) = 0.0D0
               CYCLE
            ELSEIF ( mtype==0 ) THEN
!
!     MAT2
!
               Alpha(morb+1) = Alph1
               Alpha(morb+2) = Alph2
               Alpha(morb+3) = Alph12
            ELSE
!
!     MAT8
!
               Alpha(morb+1) = Alph1
               Alpha(morb+2) = Alph2
               Alpha(morb+3) = 0.0D0
            ENDIF
!
!     TRANSFORM THERMAL EXPANSION COEFFICIENTS AND STORE THEM IN ALPHA.
!     THE ALPHAS NEED TO BE PREMULTIPLIED BY [U] INVERSE.
!
            DO ig = 1 , 3
               talpha(ig+morb) = Alpha(ig+morb)
            ENDDO
            morb = morb + 1
            CALL inverd(3,u,3,bdum,0,detu,isngu,index)
            CALL gmmatd(u,3,3,0,talpha(morb),3,1,0,Alpha(morb))
         ENDIF
      ENDIF
   ENDDO
!
!     SAVE PSEUDO E'S AND G'S FOR SHEAR FACTOR CALCULATIONS
!
   IF ( Bendng ) THEN
      Egnor(1) = G(10)
      Egnor(2) = G(14)
      Egnor(3) = G(19)
      Egnor(4) = G(22)
   ENDIF
!
END SUBROUTINE shgmgd
