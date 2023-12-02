!*==q4gmgs.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE q4gmgs(Mid,Factor,G)
   USE C_MATOUT
   IMPLICIT NONE
   REAL e1 , e2 , nu12 , Rmtout(25)
   COMMON /matout/ Rmtout
   DOUBLE PRECISION Factod
   REAL Factor
   INTEGER Mid
   DOUBLE PRECISION D(9)
   REAL G(9)
   REAL const , mtype , nu21
   DOUBLE PRECISION donst
   INTEGER i , mtyp
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
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
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     ORTHOTROPIC MATERIALS (MAT8)
!
         ELSEIF ( Mid==3 ) THEN
!
            G(1) = Rmtout(6)
            G(4) = Rmtout(5)
            IF ( G(1)==0.0 .AND. G(4)==0.0 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSE
            nu21 = nu12*e2/e1
            const = 1.0 - (nu21*nu12)
            G(1) = e1/const
            G(2) = nu12*e2/const
            G(4) = G(2)
            G(5) = e2/const
            G(9) = Rmtout(4)
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
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
         spag_nextblock_1 = 2
      CASE (2)
!
!     STANDARD RETURN
!
         DO i = 1 , 9
            G(i) = G(i)*Factor
         ENDDO
         RETURN
      CASE (3)
!
!     FATAL RETURN
!
         Mid = -Mid
         RETURN
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
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
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
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     ORTHOTROPIC MATERIALS (MAT8)
!
         ELSEIF ( Mid==3 ) THEN
!
            D(1) = Rmtout(6)
            D(4) = Rmtout(5)
            IF ( D(1)==0.0D0 .AND. D(4)==0.0D0 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSE
            nu21 = nu12*e2/e1
            donst = 1.0D0 - dble(nu21*nu12)
            D(1) = e1/donst
            D(2) = nu12*e2/donst
            D(4) = D(2)
            D(5) = e2/donst
            D(9) = Rmtout(4)
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
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
         spag_nextblock_1 = 4
      CASE (4)
!
!     STANDARD RETURN
!
         DO i = 1 , 9
            D(i) = D(i)*Factod
         ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE q4gmgs
