
SUBROUTINE angtrs(Theta,K,Trans)
   IMPLICIT NONE
   INTEGER K
   REAL Theta
   DOUBLE PRECISION Thetad
   DOUBLE PRECISION Trand(9)
   REAL Trans(9)
   INTEGER i
!     &    ENTRY ANGTRD
!
!     ROUTINE TO CALCULATE AND OUTPUT THE INPLANE ROTATION
!     TRANSFORMATION IN 3-D USING THE ANGLE OF ROTATION.
!
!     IF K=1, TRANS OR TRAND WILL BE TRANSPOSED AND THEN RETURNED.
!
!     SINGLE PRECISION -
!
!
   DO i = 1 , 9
      Trans(i) = 0.0
   ENDDO
!
   Trans(1) = cos(Theta)
   Trans(2) = sin(Theta)
   Trans(4) = -Trans(2)
   Trans(5) = Trans(1)
   Trans(9) = 1.0
!
   IF ( K/=1 ) GOTO 99999
   Trans(2) = -Trans(2)
   Trans(4) = -Trans(4)
   RETURN
!
   ENTRY angtrd(Thetad,K,Trand)
!     =============================
!
!     DOUBLE PRECISION -
!
   DO i = 1 , 9
      Trand(i) = 0.0D0
   ENDDO
!
   Trand(1) = dcos(Thetad)
   Trand(2) = dsin(Thetad)
   Trand(4) = -Trand(2)
   Trand(5) = Trand(1)
   Trand(9) = 1.0D0
!
   IF ( K==1 ) THEN
      Trand(2) = -Trand(2)
      Trand(4) = -Trand(4)
   ENDIF
99999 RETURN
END SUBROUTINE angtrs
