
SUBROUTINE subc
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
   COMPLEX c1a , c2a , c3a , c4a , cexp1 , cexp1a , cexp2 , cexp2a , cexp3 , cexp3a , cexp4 , cexp4a , cexp5 , cexp5a , const , t1 ,&
         & t2 , t3
   REAL s1 , s2 , s3 , s4 , temp , temp1 , temp2
!
!
   Am4tst = 0.0
   s1 = Sps*Del - Sigma
   s2 = Sps/(Dstr**2)
   s3 = Sns/Dstr
   s4 = Sps + Sns
   t3 = cexp(-Ai*Sigma)
   DO I = 1 , 200
      R = I
      Gamp = 2.0*Pi*R + s1
      Gamn = -2.0*Pi*R + s1
      C1p = (Gamp/Dstr) - Scrk
      C2p = (Gamp/Dstr) + Scrk
      Alp = Gamp*s2 - s3*csqrt(C1p)*csqrt(C2p)
      t1 = Alp - Del
      CALL akapm(Alp,Bkdel3)
      Sbkde1(I+1) = Bkdel3
      Sum1 = cexp(Ai*(Alp*Sps-Gamp))*(Alp*Sps-Gamp)*Bkdel3/((Alp*Dstr**2-Gamp*Sps)*t1)                                              &
           & *(F6s*t1/(t1+Gl)+F5s+B*Ai/(Bkdel1*Bkap1)*(Del-Amu)/(Alp-Del))
      C1n = (Gamn/Dstr) - Scrk
      C2n = (Gamn/Dstr) + Scrk
      Aln = Gamn*s2 - s3*csqrt(C1n)*csqrt(C2n)
      t2 = Aln - Del
      CALL akapm(Aln,Bkdel3)
      Sbkde2(I+1) = Bkdel3
      Sum2 = cexp(Ai*(Aln*Sps-Gamn))*(Aln*Sps-Gamn)*Bkdel3/((Aln*Dstr**2-Gamn*Sps)*t2)*(F6s*(t2)/(t2+Gl)+F5s+B*Ai/(Bkdel1*Bkap1)    &
           & *(Del-Amu)/(t2))
      C1p = cexp(-Ai*(t1)*Sps)
      C2p = cexp(-Ai*(t1)*Sns)
      C1n = cexp(-Ai*(t2)*Sps)
      C2n = cexp(-Ai*(t2)*Sns)
      F4 = F4 + Sum1*t3*Ai/(t1)*(C1p-C2p) + Sum2*t3*Ai/(t2)*(C1n-C2n)
      Am4 = Am4 + Sum1*t3*(Ai*Sps*C1p/(t1)-Ai*Sns*C2p/(t1)+1.0/((t1)**2)*(C1p-C2p)+Ai*(2.0-Sps)/(t1)*(C1p-C2p))                     &
          & + Sum2*t3*(Ai*Sps*C1n/(t2)-Ai*Sns*C2n/(t2)+1.0/((t2)**2)*(C1n-C2n)+Ai*(2.0-Sps)/(t2)*(C1n-C2n))
      I6 = I + 1
      temp = (Sps-Sns)/Rl1
      c1a = -Ai*t1
      c2a = -Ai*t2
      c3a = Ai*Del
      cexp1 = cexp(c1a*Sns)
      cexp2 = cexp(c2a*Sns)
      cexp3 = cexp(c3a*Sns)
      cexp1a = cexp(c1a*temp)
      cexp2a = cexp(c2a*temp)
      cexp3a = cexp(c3a*temp)
      const = Fq7/(2.0*Pi)
      temp2 = 2.0*Pi*R/s4
      c4a = -Ai*s1
      cexp4 = cexp(c4a*(2.0*Sns/s4+0.5))
      cexp5 = cexp(c4a*0.5)
      cexp4a = cexp(c4a*temp/s4)
      cexp5a = cexp(c4a*temp/(Sps+Sns))
      Xl = Sns
      DO Jl = 1 , Nl
         Pres4(Jl) = Pres4(Jl) - t3*(Sum1*cexp1+Sum2*cexp2+const*cexp3*(cexp4*sin(temp2*(Sns+Xl))/R-cexp5*sin(temp2*(Sps+Xl))/R))
         Xl = Xl + temp
         cexp1 = cexp1*cexp1a
         cexp2 = cexp2*cexp2a
         cexp3 = cexp3*cexp3a
         cexp4 = cexp4*cexp4a
         cexp5 = cexp5*cexp5a
      ENDDO
      IF ( cabs((Am4-Am4tst)/Am4)<0.0006 ) GOTO 100
      Am4tst = Am4
   ENDDO
!
   WRITE (Ibbout,99001) Ufm
99001 FORMAT (A23,' - AMG MODULE -SUBROUTINE SUBC.  AM4 LOOP DID NOT ','CONVERGE.')
   CALL mesage(-61,0,0)
   GOTO 99999
 100  temp = (Sps-Sns)/Rl1
   temp1 = 2.0*Sns/s4 + 0.5
   temp2 = 0.5 - (Sps+Sns)/s4
   c1a = Ai*Del
   c2a = -Ai*s1
   c3a = -c2a
   cexp1 = cexp(c1a*Sns)
   cexp2 = cexp(c2a*temp1)
   cexp3 = cexp(c3a*temp2)
   cexp1a = cexp(c1a*temp)
   cexp2a = cexp(c2a*temp/s4)
   const = t3*Fq7/2.0
   Xl = Sns
   DO Jl = 1 , Nl
      Pres4(Jl) = Pres4(Jl) - const*cexp1*(cexp2*((Sns+Xl)/s4-0.5)-cexp3*((Sps+Xl)/s4-1.5))
      Xl = Xl + temp
      cexp1 = cexp1*cexp1a
      cexp2 = cexp2*cexp2a
      cexp3 = cexp3*cexp2a
   ENDDO
   CALL subcc
   RETURN
99999 RETURN
END SUBROUTINE subc