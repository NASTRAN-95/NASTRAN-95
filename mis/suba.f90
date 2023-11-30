
SUBROUTINE suba
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
   REAL aa , s0 , s1 , s2 , s3 , s4 , s5 , s6 , temp
   COMPLEX c1a , c2a , cexp3 , cexp4 , cexp5 , const , t1 , t2 , t3 , t4
   INTEGER nnl1
!
!     UNSTEADY FLOW ANAYSIS OF A SUPERSONIC CASCADE
!
!     LIFT AND MOMENT COEFICIENT
!
!
   s1 = Sps - Sns
   s2 = Sps*Del - Sigma
   s3 = Sps/(Dstr**2)
   s4 = Sns/Dstr
   s0 = 2.0 - Sps + Sns
   t1 = cexp(-Ai*Sigma)
   t2 = cexp(Ai*Sigma)
   A1 = 2.0*Pi/s1
   B1 = s2/s1
   Gam = s2
   C1p = Gam/Dstr - Scrk
   C1n = Gam/Dstr + Scrk
   Alp = Gam*s3 + s4*csqrt(C1p)*csqrt(C1n)
   Bc = -B1/Alp*Bsycon/sin(Pi*B1/A1)
   t3 = Alp - Del
   F1 = (Alp-Amu)/t3*Ai*Sns/(Beta*(Gam-Alp*Sps))
   Arg2 = Del
   CALL akapm(Arg2,Bkdel1)
   Arg = Del - Gl
   CALL akapm(Arg,Bkdel2)
   CALL dlkapm(Arg2,Blkap1)
   Inx = 0
   CALL drkapm(Alp,Inx,Blkapm)
   F1 = F1*Bkdel1/Blkapm*(-t3/(t3+Gl)*A*Ai*Bkdel2/Bkdel1+B*Blkap1+B/t3)
   F1s = F1
   Nl = 10
   Rl1 = Nl - 1
   cexp3 = cexp(-Ai*t3/Rl1*s1)
   Pres1(1) = F1s
   nnl1 = Nl - 1
   DO Jl = 1 , nnl1
      Pres1(Jl+1) = Pres1(Jl)*cexp3
   ENDDO
   F1 = F1*Ai/t3*(cexp(-Ai*t3*s1)-1.0)
   Am1 = F1/(Ai*t3) - F1s/(Ai*t3)*s1*cexp(-Ai*t3*s1)
   Amtest = 0.0
   Fqb = Bkdel1/(Beta*Bc)*cexp(Ai*s2/2.0)*(-A*Ai*Bkdel2/Bkdel1+B*Blkap1)
   DO I = 1 , 200
      R = I
      Gamp = 2.0*Pi*R + s2
      Gamn = -2.0*Pi*R + s2
      C1p = (Gamp/Dstr) - Scrk
      C2p = (Gamp/Dstr) + Scrk
      Alp = Gamp*s3 + s4*csqrt(C1p)*csqrt(C2p)
      t3 = Alp - Del
      Idx = I
      CALL drkapm(Alp,Idx,Blkapm)
      C1 = (Alp-Amu)/t3*Ai*Sns/(Beta*(Gamp-Alp*Sps))*Bkdel1/(Blkapm)*(-t3/(t3+Gl)*A*Ai*Bkdel2/Bkdel1+B*Blkap1+B/t3)
      C1n = (Gamn/Dstr) - Scrk
      C2n = (Gamn/Dstr) + Scrk
      Aln = Gamn*s3 + s4*csqrt(C1n)*csqrt(C2n)
      t4 = Aln - Del
      Idx = -I
      CALL drkapm(Aln,Idx,Blkapm)
      C2 = (Aln-Amu)/t4*Ai*Sns/(Beta*(Gamn-Aln*Sps))*Bkdel1/(Blkapm)*(-t4/(t4+Gl)*A*Ai*Bkdel2/Bkdel1+B*Blkap1+B/t4)
      F1 = F1 + C1*Ai/t3*(cexp(-Ai*t3*s1)-1.0) + C2*Ai/t4*(cexp(-Ai*t4*s1)-1.0)
      Am1 = Am1 + C1/(Ai*t3)*(-s1*cexp(-Ai*t3*s1)+Ai/t3*(cexp(-Ai*t3*s1)-1.0)) + C2/(Ai*t4)                                         &
          & *(-s1*cexp(-Ai*t4*s1)+Ai/t4*(cexp(-Ai*t4*s1)-1.0))
      c2a = C2
      c1a = C1
      aa = s1/Rl1
      cexp3 = cexp(-Ai*t3*aa)
      cexp4 = cexp(-Ai*t4*aa)
      temp = 2.0*Pi*R
      cexp5 = cexp(Ai*(Sigma-Sns*Del)/s1*aa)
      const = 4.0*Fqb/temp
      Pres1(1) = Pres1(1) + C1 + C2
      DO Jl = 1 , nnl1
         const = const*cexp5
         c1a = c1a*cexp3
         c2a = c2a*cexp4
         Pres1(Jl+1) = Pres1(Jl+1) + c1a + c2a
         Pres1(Jl+1) = Pres1(Jl+1) + const*sin(temp*Jl/Rl1)
      ENDDO
      IF ( cabs((Am1-Amtest)/Am1)<0.0005 ) GOTO 100
      Amtest = Am1
   ENDDO
   WRITE (Ibbout,99001) Ufm
99001 FORMAT (A23,' FROM AMG MODULE. AM1 LOOP IN SUBROUTINE SUBA DID ','NOT CONVERGE.')
   CALL mesage(-61,0,0)
 100  aa = s1/Rl1
   cexp3 = cexp(Ai*(Sigma-Sns*Del)/Rl1)
   const = Fqb
   temp = 2.0*aa/(Sps-Sns)
   Pres1(1) = Pres1(1) - Fqb
   DO Jl = 1 , nnl1
      const = const*cexp3
      Pres1(Jl+1) = Pres1(Jl+1) - const*(1.0-Jl*temp)
   ENDDO
   Y = 0.0
   Y1 = Sns
   Arg = Del - Gl
   CALL alamda(Arg,Y,Blam1)
   CALL alamda(Arg,Y1,Blam2)
   CALL akappa(Arg,Bkap1)
   Ft2 = A*Ai*(Del-Gl-Amu)*Blam1/Bkap1
   Ft2t = A*Ai*(Del-Gl-Amu)*Blam2/Bkap1
   Arg = Del
   CALL alamda(Arg,Y,Blam1)
   CALL alamda(Arg,Y1,Blam2)
   CALL akappa(Arg,Bkap1)
   Gam = sqrt(Del**2-Scrk**2)
   s5 = sin(Sns*Gam)
   s6 = cos(Sns*Gam)
   C1 = -1.0/(Beta*Gam*s5)
   C1t = C1*(Ai*Sps*t2*s6-Sns*Del/Gam*t2*s5) - Blam2/Bkap1*Del/Gam*(s5+Gam*Sns*s6)/(Gam*s5)
   C1 = C1*(Arg/Gam*Sns*s5+Ai*Sps*t2) - Blam1/Bkap1*Del/(Gam*s5)*(s5/Gam+Sns*s6)
   Ft3 = -B*(Blam1/Bkap1+(Del-Amu)*C1)
   Ft3t = -B*(Blam2/Bkap1+(Del-Amu)*C1t)
   IF ( Gl==0.0 ) THEN
      F2 = Ft2*s0 + Ft3*s0 + B*Ai*(Del-Amu)*Blam1/Bkap1*(4.-s1**2)/2.
      Am2 = Ft2*(4.0-s1**2)/2.0 + Ft3*(4.0-s1**2)/2.0 + B*Ai*(Del-Amu)*Blam1/Bkap1*(8.0-s1**3)/3.0
      F2p = Ft2t*t1*s0 + Ft3t*t1*s0 + B*Ai*(Del-Amu)*t1*Blam2/Bkap1*(s0**2/2.0+Sps*s0)
      Am2p = Ft2t*t1*s0**2/2.0 + Ft3t*t1*s0**2/2.0 + B*Ai*(Del-Amu)*t1*Blam2/Bkap1*(s0**3/3.0+Sps*s0**2/2.0)
   ELSE
      F2 = Ft2*(cexp(2.0*Ai*Gl)-cexp(Ai*Gl*s1))/(Ai*Gl) + Ft3*s0 + B*Ai*(Del-Amu)*Blam1/Bkap1*(4.0-s1**2)/2.0
      Am2 = Ft2*(2.0*cexp(2.0*Ai*Gl)/(Ai*Gl)-s1/(Ai*Gl)*cexp(Gl*Ai*s1)+(cexp(2.0*Ai*Gl)-cexp(Ai*s1*Gl))/Gl**2) + Ft3*(4.0-s1**2)    &
          & /2.0 + B*Ai*(Del-Amu)*Blam1/Bkap1*(8.0-s1**3)/3.0
      F2p = Ft2t*t1*cexp(Ai*Gl*Sns)/(Ai*Gl)*(cexp(2.0*Ai*Gl)-cexp(Ai*Gl*s1)) + Ft3t*t1*s0 + B*Ai*(Del-Amu)                          &
          & *t1*Blam2/Bkap1*(s0**2/2.0+Sps*s0)
      Am2p = Ft2t*t1*(cexp(Ai*Gl*Sps)/(Ai*Gl)*s0*cexp(Ai*Gl*s0)+cexp(Ai*Gl*Sps)/(Gl**2)*(cexp(Ai*Gl*s0)-1.0)) + Ft3t*t1*s0**2/2.0 + &
           & B*Ai*(Del-Amu)*t1*Blam2/Bkap1*(s0**3/3.0+Sps*s0**2/2.0)
   ENDIF
   Nl2 = 20
   Rl2 = Nl2 - 1
   aa = Sps - Sns
   const = B*Ai*(Del-Amu)*Blam1/Bkap1
   temp = s0/Rl2
   c1a = Ai*Gl
   cexp3 = cexp(c1a*aa)
   cexp4 = cexp(c1a*temp)
   DO Jl = 1 , Nl2
      Xl = aa + temp*(Jl-1)
      Pres2(Jl) = Ft2*cexp3 + Ft3 + const*Xl
      cexp3 = cexp3*cexp4
   ENDDO
   CALL subbb
END SUBROUTINE suba