
SUBROUTINE q4gmgs(Mid,Factor,G)
   IMPLICIT NONE
   REAL E1 , E2 , Nu12 , Rmtout(25)
   COMMON /matout/ Rmtout
   DOUBLE PRECISION Factod
   REAL Factor
   INTEGER Mid
   DOUBLE PRECISION D(9)
   REAL G(9)
   REAL const , mtype , nu21
   DOUBLE PRECISION donst
   INTEGER i , mtyp
!     &    ENTRY Q4GMGD (MID,FACTOD,D)
!
!
!     MATERIAL PROPERTY MATRIX GENERATOR ROUTINE FOR QUAD4 ELEMENT
!
!     THIS ROUTINE BUILDS THE MATERIAL PROPERTY MATRIX, G, USING THE
!     OUTPUT OF SUBROUTINE 'MAT' (/MATOUT/).
!
!     ALL THE MATERIAL OPTIONS, ISOTROPIC, ORTHOTROPIC, AND ANISOTROPIC,
!     ARE AVAILABLE.
!
!     OUTPUT WILL BE G(9) OR D(9) FOR MID1, MID2 AND MID4.
!     FOR MID3,  G(4) OR D(4) IS SENT BACK.
!
   !>>>>EQUIVALENCE (Rmtout(1),E1) , (Rmtout(2),Nu12) , (Rmtout(3),E2)
!
!     SINGLE PRECISION SECTION -
!
   DO i = 1 , 9
      G(i) = 0.0
   ENDDO
   mtype = Rmtout(25)
   mtyp = ifix(mtype+.05) - 2
   IF ( mtyp<0 ) THEN
!
!     ISOTROPIC MATERIALS (MAT1)
!
      IF ( Mid==3 ) THEN
         G(1) = Rmtout(6)
         G(4) = G(1)
         GOTO 100
      ENDIF
   ELSEIF ( mtyp==0 ) THEN
!
!     ANISOTROPIC MATERIALS (MAT2)
!
      IF ( Mid==3 ) THEN
!
         DO i = 1 , 4
            G(i) = Rmtout(i)
         ENDDO
         G(3) = G(2)
         GOTO 100
      ENDIF
!
!     ORTHOTROPIC MATERIALS (MAT8)
!
   ELSEIF ( Mid==3 ) THEN
!
      G(1) = Rmtout(6)
      G(4) = Rmtout(5)
      IF ( G(1)/=0.0 .OR. G(4)/=0.0 ) GOTO 100
      GOTO 200
   ELSE
      nu21 = Nu12*E2/E1
      const = 1.0 - (nu21*Nu12)
      G(1) = E1/const
      G(2) = Nu12*E2/const
      G(4) = G(2)
      G(5) = E2/const
      G(9) = Rmtout(4)
      GOTO 100
   ENDIF
   DO i = 1 , 3
      G(i) = Rmtout(i)
   ENDDO
   G(4) = G(2)
   G(5) = Rmtout(4)
   G(6) = Rmtout(5)
   G(7) = G(3)
   G(8) = G(6)
   G(9) = Rmtout(6)
!
!     STANDARD RETURN
!
 100  DO i = 1 , 9
      G(i) = G(i)*Factor
   ENDDO
   GOTO 99999
!
!     FATAL RETURN
!
 200  Mid = -Mid
   GOTO 99999
!
   ENTRY q4gmgd(Mid,Factod,D)
!     ===========================
!
   DO i = 1 , 9
      D(i) = 0.0D0
   ENDDO
   mtype = Rmtout(25)
   mtyp = ifix(mtype+.05) - 2
   IF ( mtyp<0 ) THEN
!
!     ISOTROPIC MATERIALS (MAT1)
!
      IF ( Mid==3 ) THEN
         D(1) = Rmtout(6)
         D(4) = D(1)
         GOTO 300
      ENDIF
   ELSEIF ( mtyp==0 ) THEN
!
!     ANISOTROPIC MATERIALS (MAT2)
!
      IF ( Mid==3 ) THEN
!
         DO i = 1 , 4
            D(i) = Rmtout(i)
         ENDDO
         D(3) = D(2)
         GOTO 300
      ENDIF
!
!     ORTHOTROPIC MATERIALS (MAT8)
!
   ELSEIF ( Mid==3 ) THEN
!
      D(1) = Rmtout(6)
      D(4) = Rmtout(5)
      IF ( D(1)/=0.0D0 .OR. D(4)/=0.0D0 ) GOTO 300
      GOTO 200
   ELSE
      nu21 = Nu12*E2/E1
      donst = 1.0D0 - dble(nu21*Nu12)
      D(1) = E1/donst
      D(2) = Nu12*E2/donst
      D(4) = D(2)
      D(5) = E2/donst
      D(9) = Rmtout(4)
      GOTO 300
   ENDIF
   DO i = 1 , 3
      D(i) = Rmtout(i)
   ENDDO
   D(4) = D(2)
   D(5) = Rmtout(4)
   D(6) = Rmtout(5)
   D(7) = D(3)
   D(8) = D(6)
   D(9) = Rmtout(6)
!
!     STANDARD RETURN
!
 300  DO i = 1 , 9
      D(i) = D(i)*Factod
   ENDDO
!
99999 RETURN
END SUBROUTINE q4gmgs