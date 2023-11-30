
SUBROUTINE subi(Da,Dzb,Dyb,Dar,Deta,Dzeta,Dcgam,Dsgam,Dee,Dxi,Tl,Detai,Dzetai,Dcgami,Dsgami,Deei,Dtlami,Dmuy,Dmuz,Infl,Ioutfl)
   IMPLICIT NONE
   REAL Da , Dar , Dcgam , Dcgami , Dee , Deei , Deta , Detai , Dmuy , Dmuz , Dsgam , Dsgami , Dtlami , Dxi , Dyb , Dzb , Dzeta ,   &
      & Dzetai , Tl
   INTEGER Infl , Ioutfl
   REAL abar , abar2 , costh , ct2 , ct3 , delxi , dxi1 , dxi2 , dybm , dybp , dzbm , dzbp , eps , eta1 , eta2 , etai1 , etai2 ,    &
      & par3 , paren , part1 , part2 , psqr , rho12 , rho2 , rho22 , rho4 , sinth , st2 , st3 , tedif , trm1 , trm2 , xetai ,       &
      & xzetai , ycbar , zcbar , zeta1 , zeta2 , zeti1 , zeti2
   INTEGER igo
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
         GOTO 99999
      ENDIF
   ENDIF
 100  IF ( igo==2 ) THEN
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
         GOTO 99999
      ELSEIF ( Dzeta<=Dzb .AND. xzetai>dzbp ) THEN
         Ioutfl = 0
         GOTO 99999
      ENDIF
   ELSE
      dybm = Dyb - eps
      dybp = Dyb + eps
      IF ( Deta>=Dyb .AND. xetai<dybm ) THEN
         Ioutfl = 0
         GOTO 99999
      ELSEIF ( Deta<=Dyb .AND. xetai>dybp ) THEN
         Ioutfl = 0
         GOTO 99999
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
         GOTO 100
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
99999 RETURN
END SUBROUTINE subi