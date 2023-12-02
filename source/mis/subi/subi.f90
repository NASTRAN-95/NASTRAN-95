!*==subi.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE subi(Da,Dzb,Dyb,Dar,Deta,Dzeta,Dcgam,Dsgam,Dee,Dxi,Tl,Detai,Dzetai,Dcgami,Dsgami,Deei,Dtlami,Dmuy,Dmuz,Infl,Ioutfl)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: Da
   REAL :: Dzb
   REAL :: Dyb
   REAL :: Dar
   REAL :: Deta
   REAL :: Dzeta
   REAL :: Dcgam
   REAL :: Dsgam
   REAL :: Dee
   REAL :: Dxi
   REAL :: Tl
   REAL :: Detai
   REAL :: Dzetai
   REAL :: Dcgami
   REAL :: Dsgami
   REAL :: Deei
   REAL :: Dtlami
   REAL :: Dmuy
   REAL :: Dmuz
   INTEGER :: Infl
   INTEGER :: Ioutfl
!
! Local variable declarations rewritten by SPAG
!
   REAL :: abar , abar2 , costh , ct2 , ct3 , delxi , dxi1 , dxi2 , dybm , dybp , dzbm , dzbp , eps , eta1 , eta2 , etai1 , etai2 , &
         & par3 , paren , part1 , part2 , psqr , rho12 , rho2 , rho22 , rho4 , sinth , st2 , st3 , tedif , trm1 , trm2 , xetai ,    &
         & xzetai , ycbar , zcbar , zeta1 , zeta2 , zeti1 , zeti2
   INTEGER :: igo
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     COMPUTES THE IMAGE POINT COORDINATES INSIDE ASSOCIATED BODIES AND
!     THE  MU-Z  MU-Y  ELEMENTS USED IN SUBROUTINE FWMW
!
   eps = 0.1*Dee
   Dmuy = 0.0
   Dmuz = 0.0
   igo = 1
   psqr = sqrt(((Deta-Dyb)*Dar)**2+(Dzeta-Dzb)**2)
   costh = (Deta-Dyb)*Dar/psqr
   sinth = (Dzeta-Dzb)/psqr
   ct2 = costh*costh
   st2 = sinth*sinth
   ct3 = costh*ct2
   st3 = sinth*st2
   ycbar = Da*(1.0-Dar*Dar)*ct3 + Dyb
   zcbar = Da*(Dar*Dar-1.0)*st3/Dar + Dzb
   paren = st2 + Dar*Dar*ct2
   par3 = paren*paren**2
   abar = Da*sqrt(par3)/Dar
   abar2 = abar*abar
   IF ( Infl/=0 ) THEN
      rho2 = (Deta-ycbar)**2 + (Dzeta-zcbar)**2
      rho4 = rho2*rho2
      Detai = ycbar + (Deta-ycbar)*abar2/rho2
      Dzetai = zcbar + (Dzeta-zcbar)*abar2/rho2
   ELSE
      eta1 = Deta - Dee*Dcgam
      eta2 = Deta + Dee*Dcgam
      zeta1 = Dzeta - Dee*Dsgam
      zeta2 = Dzeta + Dee*Dsgam
      rho12 = (eta1-ycbar)**2 + (zeta1-zcbar)**2
      rho22 = (eta2-ycbar)**2 + (zeta2-zcbar)**2
      etai1 = ycbar + (eta1-ycbar)*abar2/rho12
      etai2 = ycbar + (eta2-ycbar)*abar2/rho22
      zeti1 = zcbar + (zeta1-zcbar)*abar2/rho12
      zeti2 = zcbar + (zeta2-zcbar)*abar2/rho22
      Deei = sqrt((etai2-etai1)**2+(zeti2-zeti1)**2)/2.0
      Detai = (etai1+etai2)/2.0
      Dzetai = (zeti1+zeti2)/2.0
      Dcgami = -(etai2-etai1)/(2.0*Deei)
      Dsgami = -(zeti2-zeti1)/(2.0*Deei)
      dxi1 = Dxi - Dee*Tl
      dxi2 = Dxi + Dee*Tl
      delxi = dxi1 - dxi2
      Dtlami = delxi/(2.0*Deei)
      IF ( abs(Dar-1.0)<=0.0001 ) THEN
         Ioutfl = 1
         RETURN
      ENDIF
   ENDIF
   SPAG_Loop_1_1: DO
      IF ( igo==2 ) THEN
         xetai = etai1
         xzetai = zeti1
      ELSEIF ( igo==3 ) THEN
         xetai = etai2
         xzetai = zeti2
      ELSE
         xetai = Detai
         xzetai = Dzetai
      ENDIF
      IF ( Dar<1.0 ) THEN
         dzbm = Dzb - eps
         dzbp = Dzb + eps
         IF ( Dzeta>=Dzb .AND. xzetai<dzbm ) THEN
            Ioutfl = 0
            EXIT SPAG_Loop_1_1
         ELSEIF ( Dzeta<=Dzb .AND. xzetai>dzbp ) THEN
            Ioutfl = 0
            EXIT SPAG_Loop_1_1
         ENDIF
      ELSE
         dybm = Dyb - eps
         dybp = Dyb + eps
         IF ( Deta>=Dyb .AND. xetai<dybm ) THEN
            Ioutfl = 0
            EXIT SPAG_Loop_1_1
         ELSEIF ( Deta<=Dyb .AND. xetai>dybp ) THEN
            Ioutfl = 0
            EXIT SPAG_Loop_1_1
         ENDIF
      ENDIF
      part1 = ((xetai-Dyb)/Da)**2
      part2 = ((xzetai-Dzb)/(Da*Dar))**2
      tedif = part1 + part2 - 1.0
      IF ( Infl==0 ) THEN
         IF ( tedif>eps ) THEN
            Ioutfl = 0
         ELSEIF ( igo==3 ) THEN
            Ioutfl = 1
         ELSE
            igo = igo + 1
            CYCLE
         ENDIF
      ELSEIF ( tedif<=eps ) THEN
         Ioutfl = 1
         trm1 = (Deta-ycbar)**2 - (Dzeta-zcbar)**2
         trm2 = 2.0*(Deta-ycbar)*(Dzeta-zcbar)
         Dmuy = -(-Dsgam*trm1+Dcgam*trm2)*abar2/rho4
         Dmuz = -(-Dsgam*trm2-Dcgam*trm1)*abar2/rho4
      ELSE
         Ioutfl = 0
      ENDIF
      EXIT SPAG_Loop_1_1
   ENDDO SPAG_Loop_1_1
END SUBROUTINE subi
