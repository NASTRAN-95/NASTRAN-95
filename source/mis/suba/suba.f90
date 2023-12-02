!*==suba.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE suba
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
   REAL :: aa , s0 , s1 , s2 , s3 , s4 , s5 , s6 , temp
   COMPLEX :: c1a , c2a , cexp3 , cexp4 , cexp5 , const , t1 , t2 , t3 , t4
   INTEGER :: nnl1
   EXTERNAL akapm , akappa , alamda , dlkapm , drkapm , mesage , subbb
!
! End of declarations rewritten by SPAG
!
!
!     UNSTEADY FLOW ANAYSIS OF A SUPERSONIC CASCADE
!
!     LIFT AND MOMENT COEFICIENT
!
!
   s1 = sps - sns
   s2 = sps*del - sigma
   s3 = sps/(dstr**2)
   s4 = sns/dstr
   s0 = 2.0 - sps + sns
   t1 = cexp(-ai*sigma)
   t2 = cexp(ai*sigma)
   a1 = 2.0*pi/s1
   b1 = s2/s1
   gam = s2
   c1p = gam/dstr - scrk
   c1n = gam/dstr + scrk
   alp = gam*s3 + s4*csqrt(c1p)*csqrt(c1n)
   bc = -b1/alp*bsycon/sin(pi*b1/a1)
   t3 = alp - del
   f1 = (alp-amu)/t3*ai*sns/(beta*(gam-alp*sps))
   arg2 = del
   CALL akapm(arg2,bkdel1)
   arg = del - gl
   CALL akapm(arg,bkdel2)
   CALL dlkapm(arg2,blkap1)
   inx = 0
   CALL drkapm(alp,inx,blkapm)
   f1 = f1*bkdel1/blkapm*(-t3/(t3+gl)*a*ai*bkdel2/bkdel1+b*blkap1+b/t3)
   f1s = f1
   nl = 10
   rl1 = nl - 1
   cexp3 = cexp(-ai*t3/rl1*s1)
   pres1(1) = f1s
   nnl1 = nl - 1
   DO jl = 1 , nnl1
      pres1(jl+1) = pres1(jl)*cexp3
   ENDDO
   f1 = f1*ai/t3*(cexp(-ai*t3*s1)-1.0)
   am1 = f1/(ai*t3) - f1s/(ai*t3)*s1*cexp(-ai*t3*s1)
   amtest = 0.0
   fqb = bkdel1/(beta*bc)*cexp(ai*s2/2.0)*(-a*ai*bkdel2/bkdel1+b*blkap1)
   DO i = 1 , 200
      r = i
      gamp = 2.0*pi*r + s2
      gamn = -2.0*pi*r + s2
      c1p = (gamp/dstr) - scrk
      c2p = (gamp/dstr) + scrk
      alp = gamp*s3 + s4*csqrt(c1p)*csqrt(c2p)
      t3 = alp - del
      idx = i
      CALL drkapm(alp,idx,blkapm)
      c1 = (alp-amu)/t3*ai*sns/(beta*(gamp-alp*sps))*bkdel1/(blkapm)*(-t3/(t3+gl)*a*ai*bkdel2/bkdel1+b*blkap1+b/t3)
      c1n = (gamn/dstr) - scrk
      c2n = (gamn/dstr) + scrk
      aln = gamn*s3 + s4*csqrt(c1n)*csqrt(c2n)
      t4 = aln - del
      idx = -i
      CALL drkapm(aln,idx,blkapm)
      c2 = (aln-amu)/t4*ai*sns/(beta*(gamn-aln*sps))*bkdel1/(blkapm)*(-t4/(t4+gl)*a*ai*bkdel2/bkdel1+b*blkap1+b/t4)
      f1 = f1 + c1*ai/t3*(cexp(-ai*t3*s1)-1.0) + c2*ai/t4*(cexp(-ai*t4*s1)-1.0)
      am1 = am1 + c1/(ai*t3)*(-s1*cexp(-ai*t3*s1)+ai/t3*(cexp(-ai*t3*s1)-1.0)) + c2/(ai*t4)                                         &
          & *(-s1*cexp(-ai*t4*s1)+ai/t4*(cexp(-ai*t4*s1)-1.0))
      c2a = c2
      c1a = c1
      aa = s1/rl1
      cexp3 = cexp(-ai*t3*aa)
      cexp4 = cexp(-ai*t4*aa)
      temp = 2.0*pi*r
      cexp5 = cexp(ai*(sigma-sns*del)/s1*aa)
      const = 4.0*fqb/temp
      pres1(1) = pres1(1) + c1 + c2
      DO jl = 1 , nnl1
         const = const*cexp5
         c1a = c1a*cexp3
         c2a = c2a*cexp4
         pres1(jl+1) = pres1(jl+1) + c1a + c2a
         pres1(jl+1) = pres1(jl+1) + const*sin(temp*jl/rl1)
      ENDDO
      IF ( cabs((am1-amtest)/am1)<0.0005 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
      amtest = am1
   ENDDO
   WRITE (ibbout,99001) ufm
99001 FORMAT (A23,' FROM AMG MODULE. AM1 LOOP IN SUBROUTINE SUBA DID ','NOT CONVERGE.')
   CALL mesage(-61,0,0)
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      Aa = S1/Rl1
      Cexp3 = cexp(Ai*(Sigma-Sns*Del)/Rl1)
      Const = Fqb
      Temp = 2.0*Aa/(Sps-Sns)
      pres1(1) = pres1(1) - Fqb
      DO Jl = 1 , Nnl1
         Const = Const*Cexp3
         pres1(Jl+1) = pres1(Jl+1) - Const*(1.0-Jl*Temp)
      ENDDO
      y = 0.0
      y1 = Sns
      Arg = Del - Gl
      CALL alamda(Arg,y,blam1)
      CALL alamda(Arg,y1,blam2)
      CALL akappa(Arg,bkap1)
      ft2 = A*Ai*(Del-Gl-Amu)*blam1/bkap1
      ft2t = A*Ai*(Del-Gl-Amu)*blam2/bkap1
      Arg = Del
      CALL alamda(Arg,y,blam1)
      CALL alamda(Arg,y1,blam2)
      CALL akappa(Arg,bkap1)
      Gam = sqrt(Del**2-Scrk**2)
      S5 = sin(Sns*Gam)
      S6 = cos(Sns*Gam)
      C1 = -1.0/(Beta*Gam*S5)
      c1t = C1*(Ai*Sps*T2*S6-Sns*Del/Gam*T2*S5) - blam2/bkap1*Del/Gam*(S5+Gam*Sns*S6)/(Gam*S5)
      C1 = C1*(Arg/Gam*Sns*S5+Ai*Sps*T2) - blam1/bkap1*Del/(Gam*S5)*(S5/Gam+Sns*S6)
      ft3 = -B*(blam1/bkap1+(Del-Amu)*C1)
      ft3t = -B*(blam2/bkap1+(Del-Amu)*c1t)
      IF ( Gl==0.0 ) THEN
         f2 = ft2*S0 + ft3*S0 + B*Ai*(Del-Amu)*blam1/bkap1*(4.-S1**2)/2.
         am2 = ft2*(4.0-S1**2)/2.0 + ft3*(4.0-S1**2)/2.0 + B*Ai*(Del-Amu)*blam1/bkap1*(8.0-S1**3)/3.0
         f2p = ft2t*T1*S0 + ft3t*T1*S0 + B*Ai*(Del-Amu)*T1*blam2/bkap1*(S0**2/2.0+Sps*S0)
         am2p = ft2t*T1*S0**2/2.0 + ft3t*T1*S0**2/2.0 + B*Ai*(Del-Amu)*T1*blam2/bkap1*(S0**3/3.0+Sps*S0**2/2.0)
      ELSE
         f2 = ft2*(cexp(2.0*Ai*Gl)-cexp(Ai*Gl*S1))/(Ai*Gl) + ft3*S0 + B*Ai*(Del-Amu)*blam1/bkap1*(4.0-S1**2)/2.0
         am2 = ft2*(2.0*cexp(2.0*Ai*Gl)/(Ai*Gl)-S1/(Ai*Gl)*cexp(Gl*Ai*S1)+(cexp(2.0*Ai*Gl)-cexp(Ai*S1*Gl))/Gl**2) + ft3*(4.0-S1**2) &
             & /2.0 + B*Ai*(Del-Amu)*blam1/bkap1*(8.0-S1**3)/3.0
         f2p = ft2t*T1*cexp(Ai*Gl*Sns)/(Ai*Gl)*(cexp(2.0*Ai*Gl)-cexp(Ai*Gl*S1)) + ft3t*T1*S0 + B*Ai*(Del-Amu)                       &
             & *T1*blam2/bkap1*(S0**2/2.0+Sps*S0)
         am2p = ft2t*T1*(cexp(Ai*Gl*Sps)/(Ai*Gl)*S0*cexp(Ai*Gl*S0)+cexp(Ai*Gl*Sps)/(Gl**2)*(cexp(Ai*Gl*S0)-1.0))                    &
              & + ft3t*T1*S0**2/2.0 + B*Ai*(Del-Amu)*T1*blam2/bkap1*(S0**3/3.0+Sps*S0**2/2.0)
      ENDIF
      nl2 = 20
      rl2 = nl2 - 1
      Aa = Sps - Sns
      Const = B*Ai*(Del-Amu)*blam1/bkap1
      Temp = S0/rl2
      C1a = Ai*Gl
      Cexp3 = cexp(C1a*Aa)
      Cexp4 = cexp(C1a*Temp)
      DO Jl = 1 , nl2
         xl = Aa + Temp*(Jl-1)
         pres2(Jl) = ft2*Cexp3 + ft3 + Const*xl
         Cexp3 = Cexp3*Cexp4
      ENDDO
      CALL subbb
   END SUBROUTINE spag_block_1
END SUBROUTINE suba
