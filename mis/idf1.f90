
SUBROUTINE idf1(Ee,E2,Eta01,Zet01,Are,Aim,Bre,Bim,Cre,Cim,R1sqx,Xiijr,Xiiji)
   IMPLICIT NONE
   REAL Aim , Are , Bim , Bre , Cim , Cre , E2 , Ee , Eta01 , R1sqx , Xiiji , Xiijr , Zet01
   REAL alarg2 , alpha , arg2 , arga , argt , atana , azet , coef , down , faci , facr , funct , parn , parni , parnr , pi , s ,    &
      & ser , test , test0 , trm1i , trm1r , trm2i , trm2r , trm3i , trm3r , up
!   ***   INTEGRATES THE PLANAR PARTS OF THE INCREMENTAL
!         OSCILLATORY KERNELS FOR UNSTEADY CASES
   pi = 3.1415926
   parn = Eta01**2 - Zet01**2
   facr = parn*Are + Eta01*Bre + Cre
   faci = parn*Aim + Eta01*Bim + Cim
   parnr = Bre/2.0 + Eta01*Are
   parni = Bim/2.0 + Eta01*Aim
   up = (Eta01-Ee)**2 + Zet01**2
   down = (Eta01+Ee)**2 + Zet01**2
   arg2 = up/down
   alarg2 = alog(arg2)
   trm2r = parnr*alog(arg2)
   trm2i = parni*alog(arg2)
   trm3r = 2.0*Ee*Are
   trm3i = 2.0*Ee*Aim
   azet = abs(Zet01)
   IF ( (azet/Ee)<=0.001 ) THEN
      funct = (2.0*Ee)/(Eta01**2-E2)
   ELSE
      test0 = abs((R1sqx-E2)/(2.0*Ee*azet))
      IF ( test0<=0.0001 ) THEN
         funct = 0.0
      ELSE
         coef = (2.0*Ee)/(R1sqx-E2)
         arga = coef*Zet01
         test = abs(arga)
         IF ( test<=0.3 ) THEN
            s = arga**2
            ser = 1./3. + s*(-1./5.+s*(1./7.+s*(-1./9.+s*(1./11.-s/13.))))
            alpha = E2*(coef**2)*ser
            funct = coef*(1.0-alpha*(Zet01**2)/E2)
         ELSE
            argt = coef*azet
            atana = atan(argt)
            funct = atana/azet
         ENDIF
      ENDIF
   ENDIF
   trm1r = facr*funct
   trm1i = faci*funct
   Xiijr = trm1r + trm2r + trm3r
   Xiiji = trm1i + trm2i + trm3i
END SUBROUTINE idf1