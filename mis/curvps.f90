
SUBROUTINE curvps(Sigs,Prin)
   IMPLICIT NONE
   REAL Prin(4) , Sigs(3)
   REAL delta , temp
!*****
!  COMPUTES PRINCIPAL STRESSES OR STRAINS AND ANGLE OF MAXIMUM.
!*****
!
   temp = Sigs(1) - Sigs(2)
   Prin(4) = sqrt((temp/2.0)**2+Sigs(3)**2)
   delta = (Sigs(1)+Sigs(2))/2.0
   Prin(2) = delta + Prin(4)
   Prin(3) = delta - Prin(4)
   delta = 2.0*Sigs(3)
   IF ( abs(delta)<1.0E-15 .AND. abs(temp)<1.0E-15 ) THEN
!
      Prin(1) = 0.0
      GOTO 99999
   ENDIF
   Prin(1) = atan2(delta,temp)*28.6478898E0
   RETURN
99999 RETURN
END SUBROUTINE curvps