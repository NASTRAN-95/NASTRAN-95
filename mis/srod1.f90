
SUBROUTINE srod1
   IMPLICIT NONE
   REAL Alpha , Area , Costh , Dummy1(83) , E , Ecpt(17) , Eltemp , Eoverl , Fjovrc , Forvec(25) , G , Gsube , Rho , Sar(3) , Sat(3)&
      & , Sbr(3) , Sbt(3) , Sdelta , Sigc , Sigmac , Sigmas , Sigmat , Sigs , Sigt , Sigvec(77) , Sinth , St , Stress , Ti(9) ,     &
      & Tsub0 , Tsubc0 , Xl , Xn(6)
   INTEGER Ibase , Iecpt(13) , Ielid , Isilno(2) , Matflg , Matidc , Nu
   COMMON /matin / Matidc , Matflg , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ E , G , Nu , Rho , Alpha , Tsub0 , Gsube , Sigt , Sigc , Sigs
   COMMON /sdr2x5/ Ecpt , Dummy1 , Ielid , Isilno , Sat , Sbt , Sar , Sbr , St , Sdelta , Area , Fjovrc , Tsubc0 , Sigmat , Sigmac ,&
                 & Sigmas , Sigvec , Forvec
   COMMON /sdr2x6/ Xn , Ti , Xl , Eoverl , Ibase
   REAL gcovrl
!*****
! THIS ROUTINE IS PHASE I OF STRESS DATA RECOVERY FOR THE ROD.
!*****
!
!
!
!
!
!
!
! INPUT AND OUTPUT BLOCK
!
!
! SCRATCH BLOCK
!
!
! INPUT AND OUTPUT BLOCKS FOR SUBROUTINE MAT
!
!
!
!
!
!
!
   !>>>>EQUIVALENCE (Iecpt(1),Ecpt(1))
!
! CALL MAT TO GET MATERIAL PROPERTIES
!
   Matidc = Iecpt(4)
   Matflg = 1
   Eltemp = Ecpt(17)
   CALL mat(Iecpt(1))
!
! SET UP VECTOR ALONG THE ROD, COMPUTE LENGTH AND NORMALIZE
!
   Xn(1) = Ecpt(10) - Ecpt(14)
   Xn(2) = Ecpt(11) - Ecpt(15)
   Xn(3) = Ecpt(12) - Ecpt(16)
   Xl = Xn(1)**2 + Xn(2)**2 + Xn(3)**2
   Xl = sqrt(Xl)
   Xn(1) = Xn(1)/Xl
   Xn(2) = Xn(2)/Xl
   Xn(3) = Xn(3)/Xl
   Eoverl = E/Xl
   gcovrl = G*Ecpt(6)/Xl
   Ibase = 0
!
! TRANSFORM XN VECTOR IF POINT A IS NOT IN BASIC COORDINATES.
!
   IF ( Iecpt(9)/=0 ) THEN
      Ibase = 3
      CALL transs(Iecpt(9),Ti)
      CALL gmmats(Xn(1),3,1,1,Ti(1),3,3,0,Xn(4))
   ENDIF
   Sat(1) = Xn(Ibase+1)*Eoverl
   Sat(2) = Xn(Ibase+2)*Eoverl
   Sat(3) = Xn(Ibase+3)*Eoverl
   Sar(1) = Xn(Ibase+1)*gcovrl
   Sar(2) = Xn(Ibase+2)*gcovrl
   Sar(3) = Xn(Ibase+3)*gcovrl
!
! TRANSFORM XN VECTOR IF POINT B IS NOT IN BASIC COORDINATES.
!
   Ibase = 0
   IF ( Iecpt(13)/=0 ) THEN
      Ibase = 3
      CALL transs(Iecpt(13),Ti)
      CALL gmmats(Xn(1),3,1,1,Ti(1),3,3,0,Xn(4))
   ENDIF
   Sbt(1) = -Xn(Ibase+1)*Eoverl
   Sbt(2) = -Xn(Ibase+2)*Eoverl
   Sbt(3) = -Xn(Ibase+3)*Eoverl
   Sbr(1) = -Xn(Ibase+1)*gcovrl
   Sbr(2) = -Xn(Ibase+2)*gcovrl
   Sbr(3) = -Xn(Ibase+3)*gcovrl
!
! FILL REMAINDER OF OUTPUT BLOCK
!
   St = -Alpha*E
   Sdelta = -Eoverl
   Area = Ecpt(5)
   IF ( Ecpt(6)/=0 ) THEN
      Fjovrc = Ecpt(7)/Ecpt(6)
   ELSE
      Fjovrc = 0.0
   ENDIF
   Tsubc0 = Tsub0
   Sigmat = Sigt
   Sigmac = Sigc
   Sigmas = Sigs
   Ielid = Iecpt(1)
   Isilno(1) = Iecpt(2)
   Isilno(2) = Iecpt(3)
END SUBROUTINE srod1