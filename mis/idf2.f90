
SUBROUTINE idf2(Ee,E2,Eta01,Zet01,A2r,A2i,B2r,B2i,C2r,C2i,R1sqx,Diijr,Diiji)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   REAL A2i , A2r , B2i , B2r , C2i , C2r , Diiji , Diijr , E2 , Ee , Eta01 , R1sqx , Zet01
!
! Local variable declarations
!
   REAL alpha , arga , argt , atana , azet , coef , den2 , den3 , dena , denb , deno , eps , eta02 , fac2a , fac2b , fac3a , fac3b ,&
      & faci , facr , funct , parn , s , ser , test , test0 , trm1i , trm1r , trm2i , trm2r , trm3i , trm3r , up1i , up1r , up2i ,  &
      & up2r , zet02
!
! End of declarations
!
!   ***   INTEGRATES THE NONPLANAR PARTS OF THE INCREMENTAL
!         OSCILLATORY KERNELS FOR UNSTEADY CASES
   eps = 0.0001
   azet = abs(Zet01)
   deno = R1sqx - E2
   parn = Eta01**2 + Zet01**2
   facr = parn*A2r + Eta01*B2r + C2r
   faci = parn*A2i + Eta01*B2i + C2i
   eta02 = Eta01**2
   zet02 = Zet01**2
   IF ( (azet/Ee)>0.001 ) THEN
      test0 = abs((R1sqx-E2)/(2.0*Ee*azet))
      IF ( test0<=0.1 ) THEN
         den2 = (Eta01+Ee)**2 + zet02
         den3 = (Eta01-Ee)**2 + zet02
         fac2a = R1sqx*Eta01 + (eta02-zet02)*Ee
         fac3a = R1sqx*Eta01 - (eta02-zet02)*Ee
         fac2b = R1sqx + Eta01*Ee
         fac3b = R1sqx - Eta01*Ee
         trm2r = (fac2a*A2r+fac2b*B2r+(Eta01+Ee)*C2r)/den2
         trm2i = (fac2a*A2i+fac2b*B2i+(Eta01+Ee)*C2i)/den2
         trm3r = -(fac3a*A2r+fac3b*B2r+(Eta01-Ee)*C2r)/den3
         trm3i = -(fac3a*A2i+fac3b*B2i+(Eta01-Ee)*C2i)/den3
         IF ( test0<=0.0001 ) THEN
            funct = 0.0
         ELSE
            coef = (2.0*Ee)/(R1sqx-E2)
            arga = coef*Zet01
            test = abs(arga)
            IF ( test>0.3 ) THEN
               argt = coef*azet
               atana = atan(argt)
               funct = atana/azet
            ELSE
               s = arga**2
               ser = 1./3. + s*(-1./5.+s*(1./7.+s*(-1./9.+s*(1./11.-s/13.))))
               alpha = E2*(coef**2)*ser
               funct = coef*(1.0-alpha*(Zet01**2)/E2)
            ENDIF
         ENDIF
         trm1r = facr*funct
         trm1i = faci*funct
         Diijr = (trm1r+trm2r+trm3r)/(2.0*zet02)
         Diiji = (trm1i+trm2i+trm3i)/(2.0*zet02)
         GOTO 99999
      ENDIF
   ENDIF
   dena = (Eta01+Ee)**2 + Zet01**2
   denb = (Eta01-Ee)**2 + Zet01**2
   up1r = 2.0*(E2*A2r+C2r)
   up1i = 2.0*(E2*A2i+C2i)
   up2r = 4.0*E2*Eta01*B2r
   up2i = 4.0*E2*Eta01*B2i
   trm1r = (up1r*(R1sqx+E2)+up2r)/(dena*denb)
   trm1i = (up1i*(R1sqx+E2)+up2i)/(dena*denb)
   IF ( (azet/Ee)<=0.001 ) THEN
      alpha = ((2.0*E2)/(eta02-E2))**2
   ELSE
      coef = (2.0*Ee)/(R1sqx-E2)
      arga = coef*Zet01
      test = abs(arga)
      IF ( test>0.3 ) THEN
         argt = coef*azet
         atana = atan(argt)
         funct = atana/azet
         alpha = (E2/zet02)*(1.0-funct*(deno/(2.0*Ee)))
      ELSE
         s = arga**2
         ser = 1./3. + s*(-1./5.+s*(1./7.+s*(-1./9.+s*(1./11.-s/13.))))
         alpha = E2*(coef**2)*ser
         funct = coef*(1.0-alpha*(Zet01**2)/E2)
      ENDIF
   ENDIF
   trm2r = -alpha*facr/E2
   trm2i = -alpha*faci/E2
   Diijr = Ee*(trm1r+trm2r)/deno
   Diiji = Ee*(trm1i+trm2i)/deno
99999 END SUBROUTINE idf2
