
SUBROUTINE lprops(G)
   IMPLICIT NONE
   REAL E1 , E2 , Nu12 , Rmtout(25)
   COMMON /matout/ Rmtout
   DOUBLE PRECISION D(25)
   REAL G(25)
   REAL const , mtype , nu21
   DOUBLE PRECISION donst
   INTEGER i , ii , mtyp
!     &    ENTRY LPROPD (D)
!
!     THIS ROUTINE RETURNS INTRINSIC LAYER PROPERTIES FOR
!     ALL LAYERS REFERENCING MAT1, MAT2 OR MAT8 PROPERTY
!     ENTRIES IN A STANDARD FORMAT AS REQUIRED FOR FILE PCOMPS
!
   EQUIVALENCE (Rmtout(1),E1) , (Rmtout(2),Nu12) , (Rmtout(3),E2)
!
!     SINGLE PRECISION -
!
   DO i = 1 , 25
      G(i) = 0.0
   ENDDO
   mtype = Rmtout(25)
   mtyp = ifix(mtype+.05) - 2
   IF ( mtyp<0 ) THEN
   ELSEIF ( mtyp/=0 ) THEN
!
!****
!     ORTHOTROPIC MATERIALS, MAT8
!
!****  LAYER PROPERTY MATRIX
!
      nu21 = Nu12*E2/E1
      const = 1.0 - (nu21*Nu12)
      G(1) = E1/const
      G(2) = Nu12*E2/const
      G(5) = E2/const
      G(4) = G(2)
      G(9) = Rmtout(4)
!
!**** TRANSVERSE SHEAR FLEXIBILITY MATRIX
      G(10) = Rmtout(6)
      G(13) = Rmtout(5)
!
!**** THERMAL COEFFICIENTS OF EXPANSION
      G(14) = Rmtout(8)
      G(15) = Rmtout(9)
!
!**** ULTIMATE STRENGTH VALUES
      G(17) = Rmtout(11)
      G(18) = Rmtout(12)
      G(19) = Rmtout(13)
      G(20) = Rmtout(14)
      G(21) = Rmtout(15)
      G(22) = Rmtout(17)
!
!*** RHO, TREF, GE
      G(23) = Rmtout(7)
      G(24) = Rmtout(10)
      G(25) = Rmtout(16)
      GOTO 99999
   ENDIF
!
!****
!     ISOTROPIC MATERIALS, MAT1 IN MAT2 FORMAT
!
!****  LAYER PROPERTY MATRIX
!
!
!****
!     ANISOTROPIC MATERIALS, MAT2
!
!****  LAYER PROPERTY MATRIX
!
   DO i = 1 , 3
      G(i) = Rmtout(i)
   ENDDO
   G(5) = Rmtout(4)
   G(6) = Rmtout(5)
   G(9) = Rmtout(6)
   G(4) = G(2)
   G(7) = G(3)
   G(8) = G(6)
!
!**** TRANSVERSE SHEAR FLEXIBILITY MATRIX
   DO i = 10 , 13
      ii = i - 9
      G(i) = Rmtout(ii)
   ENDDO
   G(12) = G(11)
!
!**** THERMAL COEFFICIENTS OF EXPANSION
   G(14) = Rmtout(8)
   G(15) = Rmtout(9)
   G(16) = Rmtout(10)
!
!**** ULTIMATE STRENGTH VALUES
   G(17) = Rmtout(13)
   G(18) = Rmtout(13)
   G(19) = Rmtout(14)
   G(20) = Rmtout(14)
   G(21) = Rmtout(15)
   G(22) = 0.0
!
!*** RHO, TREF, GE
   G(23) = Rmtout(7)
   G(24) = Rmtout(11)
   G(25) = Rmtout(12)
   GOTO 99999
!
   ENTRY lpropd(D)
!     ================
!
!     DOUBLE PRECISION -
!
   DO i = 1 , 25
      D(i) = 0.0D0
   ENDDO
   mtype = Rmtout(25)
   mtyp = ifix(mtype+.05) - 2
   IF ( mtyp<0 ) THEN
   ELSEIF ( mtyp/=0 ) THEN
!
!****
!     ORTHOTROPIC MATERIALS, MAT8
!
!****  LAYER PROPERTY MATRIX
!
      nu21 = Nu12*E2/E1
      donst = 1.0D0 - dble(nu21*Nu12)
      D(1) = E1/donst
      D(2) = Nu12*E2/donst
      D(5) = E2/donst
      D(4) = D(2)
      D(9) = Rmtout(4)
!
!**** TRANSVERSE SHEAR FLEXIBILITY MATRIX
      D(10) = Rmtout(6)
      D(13) = Rmtout(5)
!
!**** THERMAL COEFFICIENTS OF EXPANSION
      D(14) = Rmtout(8)
      D(15) = Rmtout(9)
!
!**** ULTIMATE STRENGTH VALUES
      D(17) = Rmtout(11)
      D(18) = Rmtout(12)
      D(19) = Rmtout(13)
      D(20) = Rmtout(14)
      D(21) = Rmtout(15)
      D(22) = Rmtout(17)
!
!*** RHO, TREF, GE
      D(23) = Rmtout(7)
      D(24) = Rmtout(10)
      D(25) = Rmtout(16)
      GOTO 99999
   ENDIF
!
!****
!     ISOTROPIC MATERIALS, MAT1 IN MAT2 FORMAT
!
!****  LAYER PROPERTY MATRIX
!
!
!****
!     ANISOTROPIC MATERIALS, MAT2
!
!****  LAYER PROPERTY MATRIX
!
   DO i = 1 , 3
      D(i) = Rmtout(i)
   ENDDO
   D(5) = Rmtout(4)
   D(6) = Rmtout(5)
   D(9) = Rmtout(6)
   D(4) = D(2)
   D(7) = D(3)
   D(8) = D(6)
!
!**** TRANSVERSE SHEAR FLEXIBILITY MATRIX
   DO i = 10 , 13
      ii = i - 9
      D(i) = Rmtout(ii)
   ENDDO
   D(12) = D(11)
!
!**** THERMAL COEFFICIENTS OF EXPANSION
   D(14) = Rmtout(8)
   D(15) = Rmtout(9)
   D(16) = Rmtout(10)
!
!**** ULTIMATE STRENGTH VALUES
   D(17) = Rmtout(13)
   D(18) = Rmtout(13)
   D(19) = Rmtout(14)
   D(20) = Rmtout(14)
   D(21) = Rmtout(15)
   D(22) = 0.0D0
!
!*** RHO, TREF, GE
   D(23) = Rmtout(7)
   D(24) = Rmtout(11)
   D(25) = Rmtout(12)
!
99999 RETURN
END SUBROUTINE lprops
