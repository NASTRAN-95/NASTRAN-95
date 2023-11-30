
SUBROUTINE plamat
   IMPLICIT NONE
   REAL Costh , Dummy(14) , Elid , Eltemp , G11 , G12 , G13 , G22 , G23 , G33 , Gp(9) , Plaarg , Sinth
   INTEGER Inflag , Matid , Midgp
   COMMON /matin / Matid , Inflag , Eltemp , Plaarg , Sinth , Costh
   COMMON /matout/ G11 , G12 , G13 , G22 , G23 , G33 , Dummy
   COMMON /plagp / Gp , Midgp , Elid
   REAL x(27)
! THIS ROUTINE RETURNS GP ROTATED FOR PLA3 AND PLA4
!
!
!  TEST TO SEE IF INCOMING MATERIAL ID IS EQUAL TO MATERIAL ID IN
!  PLAGP.  IF NOT USE REGULAR CALL TO MAT TO GET GP
!
   IF ( Midgp/=Matid ) THEN
      Inflag = 2
      CALL mat(Elid)
      GOTO 99999
   ENDIF
!
!                           T
!  TRANSFORM G   ,  G  =   U  *  G   * U
!             P      P            P
!
   x(1) = Costh**2
   x(2) = Sinth**2
   x(3) = Costh*Sinth
   x(4) = x(2)
   x(5) = x(1)
   x(6) = -x(3)
   x(7) = 2.0*x(6)
   x(8) = -x(7)
   x(9) = x(1) - x(2)
   CALL gmmats(Gp(1),3,3,0,x(1),3,3,0,x(19))
   CALL gmmats(x(1),3,3,1,x(19),3,3,0,x(10))
   G11 = x(10)
   G12 = x(11)
   G13 = x(12)
   G22 = x(14)
   G23 = x(15)
   G33 = x(18)
   RETURN
99999 RETURN
END SUBROUTINE plamat
