
SUBROUTINE subbb
   IMPLICIT NONE
   COMPLEX A , Ai , Aln , Alp , Am1 , Am2 , Am2p , Am4 , Am4tst , Am5 , Am5t , Am5tt , Am6 , Amtest , Arg , Arg2 , B , Bc , Bc2 ,   &
         & Bc3 , Bc4 , Bc5 , Bkdel1 , Bkdel2 , Bkdel3 , Blam1 , Blam2 , Blkap1 , Blkapm , Bsycon , C1 , C1n , C1p , C1t , C2 , C2n ,&
         & C2p , Ca1 , Ca2 , Ca3 , Ca4 , Clift , Cmomt , F1 , F1s , F2 , F2p , F4 , F4s , F5 , F5s , F5t , F6s , Fq7 , Fqa , Fqb ,  &
         & Ft2 , Ft2t , Ft3 , Ft3t , Ft3tst , Gusamp , Pres1(21) , Pres2(21) , Pres3(21) , Pres4(21) , Qres4(21) , Sbkde1(201) ,    &
         & Sbkde2(201) , Sum1 , Sum1t , Sum2 , Sum2t , Sum3 , Sum4 , Sumsv1(201) , Sumsv2(201) , Svkl1(201) , Svkl2(201)
   REAL A1 , Alp1 , Alp2 , Alpamp , Amach , Amoaxs , Amu , B1 , Beta , Betnn , Betnp , Bkap1 , C4 , C5 , Del , Disamp , Dstr , Gam ,&
      & Gamn , Gamp , Gl , Pi , Pitaxs , Pitcor , R , R5 , Redf , Res , Ri , Rl1 , Rl2 , Rq1 , Rq2 , Rt , Scrk , Sigma , Sn , Sns , &
      & Sp , Sps , Stag , Step , Sysbuf , Xl , Xl1 , Xlsv1(21) , Xlsv2(21) , Xlsv3(21) , Xlsv4(21) , Y , Y1
   INTEGER I , I6 , I7 , Ibbout , Idx , Iner , Inx , Iout , Jl , Nl , Nl2
   CHARACTER*23 Ufm
   COMMON /blk1  / Scrk , Sps , Sns , Dstr , Ai , Pi , Del , Sigma , Beta , Res
   COMMON /blk2  / Bsycon
   COMMON /blk3  / Sbkde1 , Sbkde2 , F4 , F4s , Am4 , F5s , F6s , Am4tst , Sum3 , Sum4 , Am5tt , Am6 , Sumsv1 , Sumsv2 , Svkl1 ,    &
                 & Svkl2 , F5 , F5t , Am5 , Am5t , A , B , Alp , F1 , Am1 , Aln , Blkapm , Bkdel3 , F1s , C1 , C2p , C2n , C2 ,     &
                 & Amtest , Ft2 , Blam1 , Ft3 , Am2 , Sum1 , Sum2 , F2 , Blam2 , Ft2t , C1t , Ft3t , F2p , Am2p , Sum1t , Sum2t ,   &
                 & C1p , C1n , Bkdel1 , Bkdel2 , Blkap1 , Arg , Arg2 , Ft3tst , Bc , Bc2 , Bc3 , Bc4 , Bc5 , Ca1 , Ca2 , Ca3 , Ca4 ,&
                 & Clift , Cmomt , Pres1 , Pres2 , Pres3 , Pres4 , Qres4 , Fqa , Fqb , Fq7
   COMMON /blk4  / I , R , Y , A1 , B1 , C4 , C5 , Gl , I6 , I7 , Jl , Nl , Ri , Rt , R5 , Sn , Sp , Xl , Y1 , Amu , Gam , Idx ,    &
                 & Inx , Nl2 , Rl1 , Rl2 , Rq1 , Rq2 , Xl1 , Alp1 , Alp2 , Gamn , Gamp , Iner , Iout , Redf , Stag , Step , Amach , &
                 & Betnn , Betnp , Bkap1 , Xlsv1 , Xlsv2 , Xlsv3 , Xlsv4 , Alpamp , Amoaxs , Gusamp , Disamp , Pitaxs , Pitcor
   COMMON /system/ Sysbuf , Ibbout
   COMMON /xmssg / Ufm
   REAL aa , aa1 , aa2 , carg , ckap1 , s1 , temp , temp2 , temp3 , xl2
   COMPLEX c1a , c3a , c4a , cexp2a , cexp2b , cexp2c , cexp3 , cexp3a , cexp3b , cexp3c , cexp4 , cexp4a , cexp4b , cexp4c ,       &
         & const , t1 , t2 , t3 , t4
!
!
   s1 = 2.0 + Sns - Sps
   t1 = cexp(-Ai*Sigma)
   t2 = cexp(+Ai*Sigma)
   temp = s1/Rl2
   c1a = Ai*Gl
   const = B*Ai*(Del-Amu)*Blam2/Bkap1
   cexp3 = cexp(c1a*Sps)
   cexp4 = cexp(c1a*temp)
   Xl = Sps
   DO Jl = 1 , Nl2
      Pres3(Jl) = (Ft2t*cexp3+Ft3t+const*Xl)*t1
      cexp3 = cexp3*cexp4
      Xl = Xl + temp
   ENDDO
   Ft3tst = 0.0
   Ft2 = 0.0
   Ft3 = 0.0
   Ft2t = 0.0
   Ft3t = 0.0
   Fqa = Bkdel1/(Bc*Beta)*(A*Ai*Bkdel2/Bkdel1-B*Blkap1)*cexp(-Ai*(Del*Sps-Sigma)/2.0)
   DO I = 1 , 50
      Rt = 0.0
      R = I - 1
      Ri = (-1.0)**(I-1)
!WKBR ALP = SQRT((R*PI/SNS)**2+SCRK**2)
      Alp = sqrt((R*Pi/Sns)**2+Scrk**2)
      Aln = -Alp
      CALL akapm(Alp,Bkdel3)
      t3 = Alp - Del
      Svkl1(I) = Bkdel3
      IF ( I==1 ) Rt = 1.0
      Sum1 = (Alp-Amu)/(t3)*(Ri-cexp(Ai*(t3)*Sps)*t2)/(Beta*(1.0+Rt))*Ri/(Sns*Alp)*Bkdel1/Bkdel3*(A*Ai*Bkdel2/Bkdel1*(t3)/(t3+Gl)   &
           & -B*Blkap1-B/(t3))
      Sum1t = (Alp-Amu)/(t3)*(1.0-cexp(Ai*(t3)*Sps)*t2*Ri)/(Beta*(1.0+Rt))*Ri/(Sns*Alp)                                             &
            & *Bkdel1/Bkdel3*(A*Ai*Bkdel2/Bkdel1*(t3)/(t3+Gl)-B*Blkap1-B/(t3))
      Sumsv1(I) = (Alp-Amu)/(t3)*(1.0-ccos((t3)*Sps+Sigma+R*Pi))/(Beta*(1.0+Rt)*Sns*Alp)*Bkdel1/Bkdel3*cexp(-2.0*Ai*(Alp-Del))      &
                & *(A*Bkdel2/Bkdel1*(t3)/(t3+Gl)+B*Ai*Blkap1+B*Ai/(t3))
      Ft2 = Sum1*Ai/(t3)*(cexp(-2.0*Ai*(t3))-cexp(-Ai*(Sps-Sns)*(t3))) + Ft2
      Ft3 = Sum1*(2.0*Ai*cexp(-2.0*Ai*(t3))/(t3)-Ai*(Sps-Sns)/(t3)*cexp(-Ai*(t3)*(Sps-Sns))+cexp(-2.0*Ai*(t3))/((t3)**2)            &
          & -cexp(-Ai*(t3)*(Sps-Sns))/((t3)**2)) + Ft3
      Ft2t = Sum1t*t1*cexp(-Ai*(t3)*Sps)*Ai/(t3)*(cexp(-Ai*(t3)*(s1))-1.0) + Ft2t
      Ft3t = Sum1t*t1*cexp(-Ai*(t3)*Sps)*((s1)*Ai/(t3)*cexp(-Ai*(t3)*(s1))+1.0/((t3)**2)*(cexp(-Ai*(t3)*(s1))-1.0)) + Ft3t
      CALL akapm(Aln,Bkdel3)
      t4 = Aln - Del
      Svkl2(I) = Bkdel3
      Sum2 = (Aln-Amu)/(t4)*(Ri-cexp(Ai*(t4)*Sps)*t2)/(Beta*(1.0+Rt))*Ri/(Sns*Aln)*Bkdel1/Bkdel3*(A*Ai*Bkdel2/Bkdel1*(t4)/(t4+Gl)   &
           & -B*Blkap1-B/(t4))
      Sum2t = (Aln-Amu)/(t4)*(1.0-cexp(Ai*(t4)*Sps)*t2*Ri)/(Beta*(1.0+Rt))*Ri/(Sns*Aln)                                             &
            & *Bkdel1/Bkdel3*(A*Ai*Bkdel2/Bkdel1*(t4)/(t4+Gl)-B*Blkap1-B/(t4))
      Sumsv2(I) = (Aln-Amu)/(t4)*(1.0-ccos((t4)*Sps+Sigma+R*Pi))/(Beta*(1.0+Rt)*Sns*Aln)*Bkdel1/Bkdel3*cexp(-2.0*Ai*(t4))           &
                & *(A*Bkdel2/Bkdel1*(t4)/(t4+Gl)+B*Ai*Blkap1+B*Ai/(t4))
      Ft2 = Ft2 + Sum2*Ai/(t4)*(cexp(-2.0*Ai*(t4))-cexp(-Ai*(Sps-Sns)*(t4)))
      Ft2t = Sum2t*t1*cexp(-Ai*(t4)*Sps)*Ai/(t4)*(cexp(-Ai*(t4)*(s1))-1.0) + Ft2t
      Ft3 = Ft3 + Sum2*(2.0*Ai*cexp(-2.0*Ai*(t4))/(t4)-Ai*(Sps-Sns)/(t4)*cexp(-Ai*(t4)*(Sps-Sns))+cexp(-2.0*Ai*(t4))/((t4)**2)      &
          & -cexp(-Ai*(t4)*(Sps-Sns))/((t4)**2))
      Ft3t = Ft3t + Sum2t*t1*cexp(-Ai*(t4)*Sps)*((s1)*Ai/(t4)*cexp(-Ai*(t4)*(s1))+1./((t4)**2)*(cexp(-Ai*(t4)*(s1))-1.))
      I7 = I
      aa = Sps - Sns
      temp = s1/Rl2
      temp2 = R*Pi/Sns
      const = 4.0/Pi*Fqa
      temp3 = R + Rt
      c3a = -Ai*t3
      c4a = -Ai*t4
      c1a = Ai*Del
      cexp3a = cexp(c3a*aa)
      cexp3b = cexp(c3a*Sps)
      cexp3c = cexp(c3a*temp)
      cexp4a = cexp(c4a*aa)
      cexp4b = cexp(c4a*Sps)
      cexp4c = cexp(c4a*temp)
      cexp2a = cexp(c1a*aa)
      cexp2b = cexp(c1a*Sps)
      cexp2c = cexp(c1a*temp)
      Xl1 = aa
      DO Jl = 1 , Nl2
         Pres2(Jl) = Sum1*cexp3a + Sum2*cexp4a + Pres2(Jl)
         Pres2(Jl) = Pres2(Jl) + const*cexp2a*Ri/temp3*sin(temp2*(Xl1-Sps))
         xl2 = Xl1 + Sns
         Pres3(Jl) = (Sum1t*cexp3b+Sum2t*cexp4b)*t1 + Pres3(Jl)
         Pres3(Jl) = Pres3(Jl) + const*cexp2b/temp3*sin(temp2*(xl2-Sps))*t1
         Xl1 = Xl1 + temp
         cexp3a = cexp3a*cexp3c
         cexp4a = cexp4a*cexp4c
         cexp2a = cexp2a*cexp2c
         cexp3b = cexp3b*cexp3c
         cexp4b = cexp4b*cexp4c
         cexp2b = cexp2b*cexp2c
      ENDDO
      IF ( cabs((Ft3-Ft3tst)/Ft3)<0.0006 ) GOTO 100
      Ft3tst = Ft3
   ENDDO
!
   WRITE (Ibbout,99001) Ufm
99001 FORMAT (A23,' - AMG MODULE -SUBROUTINE SUBC.  AM4 LOOP DID NOT ','CONVERGE.')
   CALL mesage(-61,0,0)
   GOTO 99999
 100  Ft3tst = Ft3
   F2 = F2 + Ft2
   Am2 = Am2 + Ft3
   F2p = F2p + Ft2t
   Am2p = Am2p + Ft3t
   aa = Sps - Sns
   aa1 = Sps + Sns
   aa2 = Sps + 2.0*Sns
   temp = s1/Rl2
   Xl = aa
   c1a = Ai*Del
   cexp3 = cexp(c1a*aa)
   cexp3c = cexp(c1a*temp)
   cexp4 = cexp(c1a*Sps)
   const = 2.0*Fqa
   cexp2a = t1*const
   DO Jl = 1 , Nl2
      Step = 0.0
      IF ( Xl>=aa1 ) Step = 1.0
      Pres2(Jl) = Pres2(Jl) + const*cexp3*((Xl-Sps)/Sns-2.0*Step)
      xl2 = Xl + Sns
      Step = 0.0
      IF ( xl2>=aa2 ) Step = 1.0
      Pres3(Jl) = Pres3(Jl) - cexp2a*cexp4*(1.0-(xl2-Sps)/Sns+2.0*Step)
      cexp3 = cexp3*cexp3c
      cexp4 = cexp4*cexp3c
      Xl = Xl + temp
   ENDDO
   Gam = Sps*Del - Sigma
   C1p = (Gam/Dstr) - Scrk
   C2p = (Gam/Dstr) + Scrk
   Alp = Gam*Sps/(Dstr**2) - Sns/Dstr*csqrt(C1p)*csqrt(C2p)
   t3 = Alp - Del
   F4 = cexp(Ai*(Alp*Sps-Gam))*(Alp*Sps-Gam)/((Alp*Dstr**2-Gam*Sps)*(t3))
   CALL akapm(Alp,Bkdel3)
   Sbkde1(1) = Bkdel3
   Sbkde2(1) = 0.0
   CALL akappa(Del,Bkap1)
   carg = Del - Gl
   CALL akappa(carg,ckap1)
   F4 = F4*Bkdel3/(Bkdel1*Bkap1)*(A*(Bkdel1/Bkdel2*(t3)/(t3+Gl)*(Del-Gl-Amu)*cexp(2.0*Ai*Gl)*Bkap1/ckap1)                           &
      & +B*Ai*(1.0-2.0*Ai*(Del-Amu)-(Del-Amu)*Res)-B*Ai*(Del-Amu)*(Blkap1-1.0/(t3)))
   F5s = B*Ai/(Bkdel1*Bkap1)*(1.0-2.0*Ai*(Del-Amu)-(Del-Amu)*Res-(Del-Amu)*Blkap1)
   F6s = A/(Bkdel1*Bkap1)*(Bkdel1/Bkdel2*(Del-Gl-Amu)*cexp(2.0*Ai*Gl)*Bkap1/ckap1)
   F4s = F4
   Fq7 = Bc*(F6s+F5s)
   temp = (Sps-Sns)/Rl1
   temp2 = 2.0 - Sps
   const = -t1*F4s
   c1a = -Ai*t3
   cexp3a = cexp(c1a*Sns)
   cexp3b = cexp(c1a*temp)
   DO Jl = 1 , Nl
      Pres4(Jl) = const*cexp3a
      cexp3a = cexp3a*cexp3b
   ENDDO
   C1 = cexp(-Ai*(t3)*Sps)
   C2 = cexp(-Ai*(t3)*Sns)
   F4 = F4*Ai*t1/(t3)*(C1-C2)
   Am4 = F4s*t1*(Ai*Sps*C1/(t3)-Ai*Sns*C2/(t3)+(C1-C2)/((t3)**2)) + F4s*Ai*(2.0-Sps)*t1/(t3)*(C1-C2)
   CALL subc
   RETURN
99999 RETURN
END SUBROUTINE subbb
