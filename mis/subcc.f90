
SUBROUTINE subcc
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
   COMPLEX c1a , c2a , cexp1 , cexp1a , cexp2 , cexp2a , const , const2 , const3 , const4 , const5 , const6 , ss , t1 , t2 , t3 , t4
   REAL s1 , s2 , s3 , s4 , s5 , temp
!
!     THIS ROUTINE WAS ORIGINALLY CALLED SUBD
!
!
   Am6 = 0.0
   F5 = 0.0
   Am5 = 0.0
   s1 = Sps + Sns
   s2 = Sigma - Sps*Del
   s3 = Sps/(Dstr**2)
   s4 = Sns/Dstr
   s5 = Del*Sns + Sigma
   ss = cexp(-Ai*Sigma)
   DO Iout = 1 , 200
      IF ( Iout>I7 ) GOTO 300
      R5 = Iout - 1
      Rq1 = sqrt((R5*Pi/Sns)**2+Scrk**2)
      Rq2 = -Rq1
      C4 = (Rq1*s1+s2)/(2.0*Pi)
      C5 = (Rq2*s1+s2)/(2.0*Pi)
      Bc2 = Bc/(2.0*Svkl1(Iout))*cexp(-Ai*(-s2)*(Sps+3.0*Sns)/(2.0*s1))/(2.0*Pi*Ai)
      Bc3 = Bc2*Svkl1(Iout)/Svkl2(Iout)
      Bc4 = Bc/(2.0*Svkl1(Iout))*cexp(Ai*(-s2)*(Sns-Sps)/(2.0*s1))/(2.0*Pi*Ai)
      Bc5 = Bc4*Svkl1(Iout)/Svkl2(Iout)
      F5t = 0.0
      Am5t = 0.0
      Am5tt = 0.0
      DO Jl = 1 , Nl
         Qres4(Jl) = 0.0
      ENDDO
      DO Iner = 1 , 200
         R = Iner - 1
         Gamp = 2.0*Pi*R - s2
         Gamn = -2.0*Pi*R - s2
         C1p = (Gamp/Dstr) - Scrk
         C2p = (Gamp/Dstr) + Scrk
         Alp = Gamp*s3 - s4*csqrt(C1p)*csqrt(C2p)
         Bkdel3 = Sbkde1(Iner)
         IF ( Iner>I6 ) THEN
            CALL akapm(Alp,Bkdel3)
            Sbkde1(Iner) = Bkdel3
         ENDIF
         t1 = Alp*Sps - Gamp
         t2 = Alp*Dstr**2 - Gamp*Sps
         Sum1 = Sumsv1(Iout)*cexp(Ai*t1)*Bkdel3*t1/(t2*Svkl1(Iout)*(Alp-Rq1))
         Sum3 = Sumsv2(Iout)*cexp(Ai*t1)*Bkdel3*t1/(t2*Svkl2(Iout)*(Alp-Rq2))
         IF ( Iner/=1 ) THEN
            C1n = (Gamn/Dstr) - Scrk
            C2n = (Gamn/Dstr) + Scrk
            Aln = Gamn*s3 - s4*csqrt(C1n)*csqrt(C2n)
            Bkdel3 = Sbkde2(Iner)
            IF ( Iner>I6 ) THEN
               CALL akapm(Aln,Bkdel3)
               Sbkde2(Iner) = Bkdel3
            ENDIF
            t1 = Aln*Sps - Gamn
            t2 = Aln*Dstr**2 - Gamn*Sps
            Sum2 = Sumsv1(Iout)*cexp(Ai*t1)*Bkdel3*t1/(t2*Svkl1(Iout)*(Aln-Rq1))
            Sum4 = Sumsv2(Iout)*cexp(Ai*t1)*Bkdel3*t1/(t2*Svkl2(Iout)*(Aln-Rq2))
         ENDIF
         IF ( Iner==1 ) Sum2 = 0.0
         IF ( Iner==1 ) Sum4 = 0.0
         C1p = cexp(-Ai*(Alp-Del)*Sps)
         C2p = cexp(-Ai*(Alp-Del)*Sns)
         C1n = cexp(-Ai*(Aln-Del)*Sps)
         C2n = cexp(-Ai*(Aln-Del)*Sns)
         F5t = F5t + (Sum1+Sum3)*Ai*ss/(Alp-Del)*(C1p-C2p) + (Sum2+Sum4)*ss*Ai/(Aln-Del)*(C1n-C2n)
         Am5t = Am5t + (Sum1+Sum3)*ss*(Ai*Sps*C1p/(Alp-Del)-Ai*Sns*C2p/(Alp-Del)+1.0/((Alp-Del)**2)*(C1p-C2p)+Ai*(2.0-Sps)/(Alp-Del)&
              & *(C1p-C2p)) + (Sum2+Sum4)*ss*(Ai*Sps*C1n/(Aln-Del)-Ai*Sns*C2n/(Aln-Del)+1.0/((Aln-Del)**2)*(C1n-C2n)+Ai*(2.0-Sps)   &
              & /(Aln-Del)*(C1n-C2n))
         temp = (Sps-Sns)/Rl1
         const = (Sum1+Sum3)*ss
         const2 = (Sum2+Sum4)*ss
         c1a = -Ai*(Alp-Del)
         c2a = -Ai*(Aln-Del)
         cexp1 = cexp(c1a*Sns)
         cexp2 = cexp(c2a*Sns)
         cexp1a = cexp(c1a*temp)
         cexp2a = cexp(c2a*temp)
         DO Jl = 1 , Nl
            Qres4(Jl) = Qres4(Jl) - (const*cexp1+const2*cexp2)
            cexp1 = cexp1*cexp1a
            cexp2 = cexp2*cexp2a
         ENDDO
         Betnp = (2.0*R*Pi-s5)/s1
         Betnn = (-2.0*R*Pi-s5)/s1
         C1p = cexp(-2.0*Pi*R*Ai*Sns/s1)
         C2p = cexp(-2.0*Pi*R*Ai*Sps/s1)
         C1n = cexp(2.0*Pi*R*Ai*Sns/s1)
         C2n = cexp(2.0*Pi*R*Ai*Sps/s1)
         t1 = cexp(-Ai*Betnp*Sps)
         t2 = cexp(-Ai*Betnp*Sns)
         t3 = cexp(-Ai*Betnn*Sps)
         t4 = cexp(-Ai*Betnn*Sns)
         Ca1 = Ai*ss/Betnp*(t1-t2)
         Ca2 = Ai*ss/Betnn*(t3-t4)
         Ca3 = ss*(Ai*Sps/Betnp*t1-Ai*Sns*t2/Betnp+(t1-t2)/Betnp**2+(2.0-Sps)*Ai/Betnp*(t1-t2))
         Ca4 = ss*(Ai*Sps*t3/Betnn-Ai*Sns*t4/Betnn+(t3-t4)/Betnn**2+(2.0-Sps)*Ai/Betnn*(t3-t4))
         IF ( Iner>1 ) THEN
            F5t = F5t - Sumsv1(Iout)*((Bc2*C1p-Bc4*C2p)/(R-C4)*Ca1-(Bc2*C1n-Bc4*C2n)/(R+C4)*Ca2) - Sumsv2(Iout)                     &
                & *((Bc3*C1p-Bc5*C2p)/(R-C5)*Ca1-(Bc3*C1n-Bc5*C2n)/(R+C5)*Ca2)
            Am5t = Am5t - Sumsv1(Iout)*((Bc2*C1p-Bc4*C2p)/(R-C4)*Ca3-(Bc2*C1n-Bc4*C2n)/(R+C4)*Ca4) - Sumsv2(Iout)                   &
                 & *((Bc3*C1p-Bc5*C2p)/(R-C5)*Ca3-(Bc3*C1n-Bc5*C2n)/(R+C5)*Ca4)
            temp = (Sps-Sns)/Rl1
            const = (Bc2*C1p-Bc4*C2p)/(R-C4)
            const2 = (Bc2*C1n-Bc4*C2n)/(R+C4)
            const3 = (Bc3*C1p-Bc5*C2p)/(R-C5)
            const4 = (Bc3*C1n-Bc5*C2n)/(R+C5)
            const5 = ss*Sumsv1(Iout)
            const6 = ss*Sumsv2(Iout)
            c1a = -Ai*Betnp
            c2a = -Ai*Betnn
            cexp1 = cexp(c1a*Sns)
            cexp2 = cexp(c2a*Sns)
            cexp1a = cexp(c1a*temp)
            cexp2a = cexp(c2a*temp)
            DO Jl = 1 , Nl
               Qres4(Jl) = Qres4(Jl) + const5*(const*cexp1-const2*cexp2) + const6*(const3*cexp1-const4*cexp2)
               cexp1 = cexp1*cexp1a
               cexp2 = cexp2*cexp2a
            ENDDO
         ELSE
            F5t = F5t - Sumsv1(Iout)*(Bc2*C1p-Bc4*C2p)/(R-C4)*Ca1 - Sumsv2(Iout)*(Bc3*C1p-Bc5*C2p)/(R-C5)*Ca1
            Am5t = Am5t - Sumsv1(Iout)*(Bc2*C1p-Bc4*C2p)/(R-C4)*Ca3 - Sumsv2(Iout)*(Bc3*C1p-Bc5*C2p)/(R-C5)*Ca3
            temp = (Sps-Sns)/Rl1
            const = ss*Sumsv1(Iout)*(Bc2*C1p-Bc4*C2p)/(R-C4)
            const2 = ss*Sumsv2(Iout)*(Bc3*C1p-Bc5*C2p)/(R-C5)
            c1a = -Ai*Betnp
            cexp1 = cexp(c1a*Sns)
            cexp1a = cexp(c1a*temp)
            DO Jl = 1 , Nl
               Qres4(Jl) = Qres4(Jl) + const*cexp1 + const2*cexp1
               cexp1 = cexp1*cexp1a
            ENDDO
         ENDIF
         IF ( cabs((Am5tt-Am5t)/Am5t)<0.001 ) GOTO 50
         Am5tt = Am5t
      ENDDO
      GOTO 200
 50   IF ( Iner>I6 ) I6 = Iner
      F5 = F5 + F5t
      Am5 = Am5 + Am5t
      DO Jl = 1 , Nl
         Pres4(Jl) = Pres4(Jl) + Qres4(Jl)
      ENDDO
      Alp1 = (2.0*Pi*C4-Del*Sns-Sigma)/s1
      Alp2 = (2.0*Pi*C5-Del*Sns-Sigma)/s1
      t1 = 1.0 - cexp(-2.0*Pi*Ai*C4)
      t2 = 1.0 - cexp(-2.0*Pi*Ai*C5)
      C1p = cexp(-2.0*Pi*Ai*C4*Sns/s1)/(t1)
      C2p = cexp(2.0*Pi*Ai*C4*Sns/s1)/(t1)
      C1n = cexp(-2.0*Pi*Ai*C5*Sns/s1)/(t2)
      C2n = cexp(2.0*Pi*Ai*C5*Sns/s1)/(t2)
      t1 = cexp(-Ai*Sps*Alp1)
      t2 = cexp(-Ai*Sns*Alp1)
      t3 = cexp(-Ai*Sps*Alp2)
      t4 = cexp(-Ai*Sns*Alp2)
      Ca1 = Ai*ss/Alp1*(t1-t2)
      Ca2 = Ai*ss/Alp2*(t3-t4)
      Ca3 = ss*(Ai*Sps*t1/Alp1-Ai*Sns*t2/Alp1+(t1-t2)/Alp1**2+(2.0-Sps)*Ai/Alp1*(t1-t2))
      Ca4 = ss*(Ai*Sps*t3/Alp2-Ai*Sns*t4/Alp2+(t3-t4)/Alp2**2+(2.0-Sps)*Ai/Alp2*(t3-t4))
      F5 = F5 - 2.0*Pi*Ai*Sumsv1(Iout)*(Bc2*C1p-Bc4*C2p)*Ca1 - 2.0*Pi*Ai*Sumsv2(Iout)*(Bc3*C1n-Bc5*C2n)*Ca2
      Am5 = Am5 - 2.0*Pi*Ai*Sumsv1(Iout)*(Bc2*C1p-Bc4*C2p)*Ca3 - 2.0*Pi*Ai*Sumsv2(Iout)*(Bc3*C1n-Bc5*C2n)*Ca4
      temp = (Sps-Sns)/Rl1
      const = ss*2.0*Pi*Ai
      const2 = const*Sumsv1(Iout)*(Bc2*C1p-Bc4*C2p)
      const3 = const*Sumsv2(Iout)*(Bc3*C1n-Bc5*C2n)
      c1a = -Ai*Alp1
      c2a = -Ai*Alp2
      cexp1 = cexp(c1a*Sns)
      cexp2 = cexp(c2a*Sns)
      cexp1a = cexp(c1a*temp)
      cexp2a = cexp(c2a*temp)
      DO Jl = 1 , Nl
         Pres4(Jl) = Pres4(Jl) + const2*cexp1 + const3*cexp2
         cexp1 = cexp1*cexp1a
         cexp2 = cexp2*cexp2a
      ENDDO
      IF ( cabs((Am5-Am6)/Am5)<0.0009 ) GOTO 100
      Am6 = Am5
   ENDDO
   WRITE (Ibbout,99001) Ufm
99001 FORMAT (A23,' - AMG MODULE -SUBROUTINE SUBCC.  AM5 LOOP DID NOT',' CONVERGE.')
   GOTO 400
 100  Clift = F1 + F2 - F2p + F4 + F5
   Cmomt = Am1 + Am2 - Am2p + Am4 + Am5 - Amoaxs*Clift
   GOTO 99999
!
 200  WRITE (Ibbout,99002) Ufm
99002 FORMAT (A23,' - AMG MODULE -SUBROUTINE SUBCC.  AM5T LOOP DID NOT',' CONVERGE.')
   GOTO 400
 300  WRITE (Ibbout,99003) Ufm , I7
99003 FORMAT (A23,' - AMG MODULE -SUBROUTINE SUBCC.  OUTER LOOP OF AM5',' EXCEEDED I7 (',I6,1H))
 400  CALL mesage(-61,0,0)
99999 RETURN
END SUBROUTINE subcc