!*==subbb.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE subbb
   USE c_blk1
   USE c_blk2
   USE c_blk3
   USE c_blk4
   USE c_system
   USE c_xmssg
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: aa , aa1 , aa2 , carg , ckap1 , s1 , temp , temp2 , temp3 , xl2
   COMPLEX :: c1a , c3a , c4a , cexp2a , cexp2b , cexp2c , cexp3 , cexp3a , cexp3b , cexp3c , cexp4 , cexp4a , cexp4b , cexp4c ,    &
            & const , t1 , t2 , t3 , t4
   EXTERNAL akapm , akappa , mesage , subc
!
! End of declarations rewritten by SPAG
!
!
!
   s1 = 2.0 + sns - sps
   t1 = cexp(-ai*sigma)
   t2 = cexp(+ai*sigma)
   temp = s1/rl2
   c1a = ai*gl
   const = b*ai*(del-amu)*blam2/bkap1
   cexp3 = cexp(c1a*sps)
   cexp4 = cexp(c1a*temp)
   xl = sps
   DO jl = 1 , nl2
      pres3(jl) = (ft2t*cexp3+ft3t+const*xl)*t1
      cexp3 = cexp3*cexp4
      xl = xl + temp
   ENDDO
   ft3tst = 0.0
   ft2 = 0.0
   ft3 = 0.0
   ft2t = 0.0
   ft3t = 0.0
   fqa = bkdel1/(bc*beta)*(a*ai*bkdel2/bkdel1-b*blkap1)*cexp(-ai*(del*sps-sigma)/2.0)
   DO i = 1 , 50
      rt = 0.0
      r = i - 1
      ri = (-1.0)**(i-1)
!WKBR ALP = SQRT((R*PI/SNS)**2+SCRK**2)
      alp = sqrt((r*pi/sns)**2+scrk**2)
      aln = -alp
      CALL akapm(alp,bkdel3)
      t3 = alp - del
      svkl1(i) = bkdel3
      IF ( i==1 ) rt = 1.0
      sum1 = (alp-amu)/(t3)*(ri-cexp(ai*(t3)*sps)*t2)/(beta*(1.0+rt))*ri/(sns*alp)*bkdel1/bkdel3*(a*ai*bkdel2/bkdel1*(t3)/(t3+gl)   &
           & -b*blkap1-b/(t3))
      sum1t = (alp-amu)/(t3)*(1.0-cexp(ai*(t3)*sps)*t2*ri)/(beta*(1.0+rt))*ri/(sns*alp)                                             &
            & *bkdel1/bkdel3*(a*ai*bkdel2/bkdel1*(t3)/(t3+gl)-b*blkap1-b/(t3))
      sumsv1(i) = (alp-amu)/(t3)*(1.0-ccos((t3)*sps+sigma+r*pi))/(beta*(1.0+rt)*sns*alp)*bkdel1/bkdel3*cexp(-2.0*ai*(alp-del))      &
                & *(a*bkdel2/bkdel1*(t3)/(t3+gl)+b*ai*blkap1+b*ai/(t3))
      ft2 = sum1*ai/(t3)*(cexp(-2.0*ai*(t3))-cexp(-ai*(sps-sns)*(t3))) + ft2
      ft3 = sum1*(2.0*ai*cexp(-2.0*ai*(t3))/(t3)-ai*(sps-sns)/(t3)*cexp(-ai*(t3)*(sps-sns))+cexp(-2.0*ai*(t3))/((t3)**2)            &
          & -cexp(-ai*(t3)*(sps-sns))/((t3)**2)) + ft3
      ft2t = sum1t*t1*cexp(-ai*(t3)*sps)*ai/(t3)*(cexp(-ai*(t3)*(s1))-1.0) + ft2t
      ft3t = sum1t*t1*cexp(-ai*(t3)*sps)*((s1)*ai/(t3)*cexp(-ai*(t3)*(s1))+1.0/((t3)**2)*(cexp(-ai*(t3)*(s1))-1.0)) + ft3t
      CALL akapm(aln,bkdel3)
      t4 = aln - del
      svkl2(i) = bkdel3
      sum2 = (aln-amu)/(t4)*(ri-cexp(ai*(t4)*sps)*t2)/(beta*(1.0+rt))*ri/(sns*aln)*bkdel1/bkdel3*(a*ai*bkdel2/bkdel1*(t4)/(t4+gl)   &
           & -b*blkap1-b/(t4))
      sum2t = (aln-amu)/(t4)*(1.0-cexp(ai*(t4)*sps)*t2*ri)/(beta*(1.0+rt))*ri/(sns*aln)                                             &
            & *bkdel1/bkdel3*(a*ai*bkdel2/bkdel1*(t4)/(t4+gl)-b*blkap1-b/(t4))
      sumsv2(i) = (aln-amu)/(t4)*(1.0-ccos((t4)*sps+sigma+r*pi))/(beta*(1.0+rt)*sns*aln)*bkdel1/bkdel3*cexp(-2.0*ai*(t4))           &
                & *(a*bkdel2/bkdel1*(t4)/(t4+gl)+b*ai*blkap1+b*ai/(t4))
      ft2 = ft2 + sum2*ai/(t4)*(cexp(-2.0*ai*(t4))-cexp(-ai*(sps-sns)*(t4)))
      ft2t = sum2t*t1*cexp(-ai*(t4)*sps)*ai/(t4)*(cexp(-ai*(t4)*(s1))-1.0) + ft2t
      ft3 = ft3 + sum2*(2.0*ai*cexp(-2.0*ai*(t4))/(t4)-ai*(sps-sns)/(t4)*cexp(-ai*(t4)*(sps-sns))+cexp(-2.0*ai*(t4))/((t4)**2)      &
          & -cexp(-ai*(t4)*(sps-sns))/((t4)**2))
      ft3t = ft3t + sum2t*t1*cexp(-ai*(t4)*sps)*((s1)*ai/(t4)*cexp(-ai*(t4)*(s1))+1./((t4)**2)*(cexp(-ai*(t4)*(s1))-1.))
      i7 = i
      aa = sps - sns
      temp = s1/rl2
      temp2 = r*pi/sns
      const = 4.0/pi*fqa
      temp3 = r + rt
      c3a = -ai*t3
      c4a = -ai*t4
      c1a = ai*del
      cexp3a = cexp(c3a*aa)
      cexp3b = cexp(c3a*sps)
      cexp3c = cexp(c3a*temp)
      cexp4a = cexp(c4a*aa)
      cexp4b = cexp(c4a*sps)
      cexp4c = cexp(c4a*temp)
      cexp2a = cexp(c1a*aa)
      cexp2b = cexp(c1a*sps)
      cexp2c = cexp(c1a*temp)
      xl1 = aa
      DO jl = 1 , nl2
         pres2(jl) = sum1*cexp3a + sum2*cexp4a + pres2(jl)
         pres2(jl) = pres2(jl) + const*cexp2a*ri/temp3*sin(temp2*(xl1-sps))
         xl2 = xl1 + sns
         pres3(jl) = (sum1t*cexp3b+sum2t*cexp4b)*t1 + pres3(jl)
         pres3(jl) = pres3(jl) + const*cexp2b/temp3*sin(temp2*(xl2-sps))*t1
         xl1 = xl1 + temp
         cexp3a = cexp3a*cexp3c
         cexp4a = cexp4a*cexp4c
         cexp2a = cexp2a*cexp2c
         cexp3b = cexp3b*cexp3c
         cexp4b = cexp4b*cexp4c
         cexp2b = cexp2b*cexp2c
      ENDDO
      IF ( cabs((ft3-ft3tst)/ft3)<0.0006 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
      ft3tst = ft3
   ENDDO
!
   WRITE (ibbout,99001) ufm
99001 FORMAT (A23,' - AMG MODULE -SUBROUTINE SUBC.  AM4 LOOP DID NOT ','CONVERGE.')
   CALL mesage(-61,0,0)
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
      Ft3tst = Ft3
      f2 = f2 + Ft2
      am2 = am2 + Ft3
      f2p = f2p + Ft2t
      am2p = am2p + Ft3t
      Aa = Sps - Sns
      Aa1 = Sps + Sns
      Aa2 = Sps + 2.0*Sns
      Temp = S1/Rl2
      Xl = Aa
      C1a = Ai*Del
      Cexp3 = cexp(C1a*Aa)
      Cexp3c = cexp(C1a*Temp)
      Cexp4 = cexp(C1a*Sps)
      Const = 2.0*Fqa
      Cexp2a = T1*Const
      DO Jl = 1 , Nl2
         step = 0.0
         IF ( Xl>=Aa1 ) step = 1.0
         pres2(Jl) = pres2(Jl) + Const*Cexp3*((Xl-Sps)/Sns-2.0*step)
         Xl2 = Xl + Sns
         step = 0.0
         IF ( Xl2>=Aa2 ) step = 1.0
         pres3(Jl) = pres3(Jl) - Cexp2a*Cexp4*(1.0-(Xl2-Sps)/Sns+2.0*step)
         Cexp3 = Cexp3*Cexp3c
         Cexp4 = Cexp4*Cexp3c
         Xl = Xl + Temp
      ENDDO
      gam = Sps*Del - Sigma
      c1p = (gam/dstr) - Scrk
      c2p = (gam/dstr) + Scrk
      Alp = gam*Sps/(dstr**2) - Sns/dstr*csqrt(c1p)*csqrt(c2p)
      T3 = Alp - Del
      f4 = cexp(Ai*(Alp*Sps-gam))*(Alp*Sps-gam)/((Alp*dstr**2-gam*Sps)*(T3))
      CALL akapm(Alp,Bkdel3)
      sbkde1(1) = Bkdel3
      sbkde2(1) = 0.0
      CALL akappa(Del,Bkap1)
      Carg = Del - Gl
      CALL akappa(Carg,Ckap1)
      f4 = f4*Bkdel3/(Bkdel1*Bkap1)*(A*(Bkdel1/Bkdel2*(T3)/(T3+Gl)*(Del-Gl-Amu)*cexp(2.0*Ai*Gl)*Bkap1/Ckap1)                        &
         & +B*Ai*(1.0-2.0*Ai*(Del-Amu)-(Del-Amu)*res)-B*Ai*(Del-Amu)*(Blkap1-1.0/(T3)))
      f5s = B*Ai/(Bkdel1*Bkap1)*(1.0-2.0*Ai*(Del-Amu)-(Del-Amu)*res-(Del-Amu)*Blkap1)
      f6s = A/(Bkdel1*Bkap1)*(Bkdel1/Bkdel2*(Del-Gl-Amu)*cexp(2.0*Ai*Gl)*Bkap1/Ckap1)
      f4s = f4
      fq7 = Bc*(f6s+f5s)
      Temp = (Sps-Sns)/rl1
      Temp2 = 2.0 - Sps
      Const = -T1*f4s
      C1a = -Ai*T3
      Cexp3a = cexp(C1a*Sns)
      Cexp3b = cexp(C1a*Temp)
      DO Jl = 1 , nl
         pres4(Jl) = Const*Cexp3a
         Cexp3a = Cexp3a*Cexp3b
      ENDDO
      c1 = cexp(-Ai*(T3)*Sps)
      c2 = cexp(-Ai*(T3)*Sns)
      f4 = f4*Ai*T1/(T3)*(c1-c2)
      am4 = f4s*T1*(Ai*Sps*c1/(T3)-Ai*Sns*c2/(T3)+(c1-c2)/((T3)**2)) + f4s*Ai*(2.0-Sps)*T1/(T3)*(c1-c2)
      CALL subc
   END SUBROUTINE spag_block_1
END SUBROUTINE subbb
