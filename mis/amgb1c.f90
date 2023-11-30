
SUBROUTINE amgb1c(Q)
   IMPLICIT NONE
   COMPLEX A , Ai , Aln , Alp , Am1 , Am2 , Am2p , Am4 , Am4tst , Am5 , Am5t , Am5tt , Am6 , Amtest , Arg , Arg2 , B , Bc , Bc2 ,   &
         & Bc3 , Bc4 , Bc5 , Bkdel1 , Bkdel2 , Bkdel3 , Blam1 , Blam2 , Blkap1 , Blkapm , Bsycon , C1 , C1n , C1p , C1t , C2 , C2n ,&
         & C2p , Ca1 , Ca2 , Ca3 , Ca4 , Clift , Cmomt , F1 , F1s , F2 , F2p , F4 , F4s , F5 , F5s , F5t , F6s , Fq7 , Fqa , Fqb ,  &
         & Ft2 , Ft2t , Ft3 , Ft3t , Ft3tst , Gusamp , Pres1(21) , Pres2(21) , Pres3(21) , Pres4(21) , Qres4(21) , Sbkde1(201) ,    &
         & Sbkde2(201) , Sum1 , Sum1t , Sum2 , Sum2t , Sum3 , Sum4 , Sumsv1(201) , Sumsv2(201) , Svkl1(201) , Svkl2(201)
   REAL A1 , Alp1 , Alp2 , Alpamp , Amach , Amachd , Amachr , Amoaxs , Amu , B1 , Beta , Betnn , Betnp , Bkap1 , Blspc , Bspace ,   &
      & C4 , C5 , Chord , Del , Den , Disamp , Dstr , Dum(2) , Flowa , Gam , Gamn , Gamp , Gl , Pi , Pitaxs , Pitcor , R , R5 ,     &
      & Radius , Redf , Redfd , Refc , Refcrd , Refden , Refflo , Refmac , Refstg , Refvel , Res , Rfreq , Ri , Rl1 , Rl2 , Rq1 ,   &
      & Rq2 , Rt , Scrk , Sigm , Sigma , Sn , Sns , Sp , Sps , Stag , Step , Stg , Sysbuf , Tsonic , Vel , Xl , Xl1 , Xlsv1(21) ,   &
      & Xlsv2(21) , Xlsv3(21) , Xlsv4(21) , Y , Y1
   INTEGER I , I6 , I7 , Ibbout , Idx , Iner , Inx , Iout , Iref , Jl , Mach , Maxmac , Mcb(7) , Minmac , Nl , Nl2 , Nlines , Nrow ,&
         & Nstns , Nstnsx , Sln
   CHARACTER*23 Ufm
   COMMON /amgmn / Mcb , Nrow , Dum , Refc , Sigm , Rfreq
   COMMON /bamg1l/ Iref , Minmac , Maxmac , Nlines , Nstns , Refstg , Refcrd , Refmac , Refden , Refvel , Refflo , Sln , Nstnsx ,   &
                 & Stg , Chord , Radius , Bspace , Mach , Den , Vel , Flowa , Amachd , Redfd , Blspc , Amachr , Tsonic
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
   COMPLEX Q(Nstns,Nstns)
   REAL aa , aye(10,29) , con , const , determ , gee(29,40) , geetmp(29,20) , gye(29,29) , s1 , sps1 , sumi , sumr , temp , xlow(29)&
      & , xtemp(29) , xup(29)
   INTEGER index(29,3) , ising , j , k , n1n , nf , nm , nm2 , nm3 , nm4 , nmm , nmmm , nsns2 , nstns2 , nstns4 , ntimes , nx , nxx
   COMPLEX presl(29) , presu(29)
!
!     UNSTEADY FLOW ANALYSIS OF A SUPERSONIC CASCADE
!
!
!     THEORY DEPENDENT RESTRICTION OF NO MORE THAN 10 COMPUTING
!     STATIONS PER STREAMLINE IS REFLECTED IN CODING.
!
   IF ( Nstns>10 ) THEN
      WRITE (Ibbout,99001) Ufm , Sln , Nstns
99001 FORMAT (A23,' - AMG MODULE - NUMBER OF COMPUTING STATIONS ON ','STREAMLINE',I8,4H IS ,I3,1H.,/39X,'SUPERSONIC CASCADE',       &
             &' ROUTINE AMGB1C ALLOWS ONLY A MAXIMUM OF 10.')
   ELSE
!
      Redf = Redfd
      Amach = Amachd
      Ai = cmplx(0.0,1.0)
      Pi = 3.1415927
      Pitcor = Blspc
      Stag = 90.0 - Stg
      Sigma = -Sigm*Pi/180.0
      Beta = sqrt(Amach**2-1.0)
      Scrk = Redf*Amach/(Beta**2)
      Del = Scrk*Amach
      Amu = Redf/(Beta**2)
      Sp = Pitcor*cos(Stag*Pi/180.0)*2.0
      Sn = Pitcor*sin(Stag*Pi/180.0)*2.0
      Sps = Sp
      Sns = Sn*Beta
      Dstr = sqrt(Sps**2-Sns**2)
      sps1 = abs(Sps-Sns)
      IF ( sps1<.00001 ) THEN
!
         WRITE (Ibbout,99002) Ufm
!
99002    FORMAT (A23,' - AMG MODULE -SUBROUTINE AMGB1C',/39X,'AXIAL MACH NUMB. IS EQUAL TO OR GREATER THAN ONE.')
      ELSE
!
!     ZERO OUT GEE
!
         nstns2 = 2*Nstns
         nstns4 = 4*Nstns
         DO I = 1 , 29
            DO j = 1 , nstns4
               gee(I,j) = 0.0
            ENDDO
         ENDDO
         Pitaxs = 0.0
         Amoaxs = 0.
         CALL asycon
         CALL akp2
         Rl1 = 9
         s1 = Sps - Sns
         aa = s1/Rl1
         Xlsv1(1) = 0.0
         DO Jl = 1 , 9
            Xlsv1(Jl+1) = Jl*aa
         ENDDO
         aa = Sps - Sns
         Rl2 = 19
         s1 = 2.0 + Sns - Sps
         temp = s1/Rl2
         Xl = aa
         DO Jl = 1 , 20
            Xlsv2(Jl) = Xl
            Xlsv3(Jl) = Xl + Sns - Sps
            Xl = Xl + temp
         ENDDO
         Xl = Sns + 2.0 - Sps
         temp = (Sps-Sns)/Rl1
         DO Jl = 1 , 10
            Xlsv4(Jl) = Xl
            Xl = Xl + temp
         ENDDO
!
!     ACCUMULATE PRESSURE VECTORS INTO G-MATRIX
!
         DO nm = 1 , Nstns
            ntimes = 1
            IF ( nm>2 ) ntimes = 2
            DO nmm = 1 , ntimes
!
!     DEFINE -----------------------------
!            ALPAMP - PITCHING AMP
!            DISAMP - PLUNGING AMP
!            GUSAMP - GUST AMP
!            GL -GUST WAVE NUMBER
!
               Alpamp = 0.0
               IF ( nm==2 ) Alpamp = 1.0
               Disamp = 0.0
               IF ( nm==1 ) Disamp = 1.0
               Gusamp = 0.0
               Gl = 0.0
               IF ( nm>2 .AND. nmm==1 ) Gusamp = -Redf/2.0 + (nm-2)*Pi/4.0
               IF ( nm>2 .AND. nmm==1 ) Gl = (nm-2)*Pi/2.0
               IF ( nm>2 .AND. nmm==2 ) Gusamp = Redf/2.0 + (nm-2)*Pi/4.0
               IF ( nm>2 .AND. nmm==2 ) Gl = -(nm-2)*Pi/2.0
!
               A = (1.0+Ai*Redf*Pitaxs)*Alpamp - Ai*Redf*Disamp
               B = -Ai*Redf*Alpamp
               IF ( Gl/=0.0 ) THEN
                  A = Gusamp
                  B = 0.0
               ENDIF
               CALL suba
!
!     FIND  DELTA P(LOWER-UPPER)
!
               DO nx = 1 , 10
                  presu(nx) = Pres1(nx)
                  xup(nx) = Xlsv1(nx)
                  IF ( nx==10 ) THEN
                     presu(nx) = (Pres1(10)+Pres2(1))/2.0
                     xup(10) = (Xlsv1(10)+Xlsv2(1))/2.0
                  ELSE
                     nxx = nx + 20
                     presl(nxx) = Pres4(nx+1)
                     xlow(nxx) = Xlsv4(nx+1)
                  ENDIF
               ENDDO
               DO nx = 1 , 20
                  nxx = nx + 10
                  IF ( nx==20 ) THEN
                     presl(20) = (Pres3(20)+Pres4(1))/2.0
                     xlow(20) = (Xlsv3(20)+Xlsv4(1))/2.0
                  ELSE
                     presu(nxx) = Pres2(nx+1)
                     xup(nxx) = Xlsv2(nx+1)
                     presl(nx) = Pres3(nx)
                     xlow(nx) = Xlsv3(nx)
                  ENDIF
               ENDDO
               nm2 = nm + Nstns
               nm3 = nm + 2*Nstns
               nm4 = nm + 3*Nstns
               DO nmmm = 1 , 29
                  gee(nmmm,nm) = gee(nmmm,nm) + real(presl(nmmm))
                  gee(nmmm,nm2) = gee(nmmm,nm2) + aimag(presl(nmmm))
                  gee(nmmm,nm3) = gee(nmmm,nm3) + real(presu(nmmm))
                  gee(nmmm,nm4) = gee(nmmm,nm4) + aimag(presu(nmmm))
               ENDDO
            ENDDO
         ENDDO
!
!     NOW DEFINE  I-MATRIX (NSTNS X 29)
!
         aye(1,1) = 2.0
         con = 1.0
         aye(1,2) = 2.0
         n1n = 27
         DO j = 1 , n1n
            aye(1,j+2) = con*4.0/j/Pi
            con = 1.0 - con
         ENDDO
         aye(2,1) = 2.0
         aye(2,2) = 2.66666667
         con = 1.0
         DO j = 1 , n1n
            aye(2,j+2) = con*4/j/Pi
            con = -con
         ENDDO
         DO I = 3 , Nstns
            DO j = 2 , 28
               con = 0.0
               IF ( (I-1)==j ) con = 1.0
               aye(I,j+1) = con
            ENDDO
         ENDDO
         DO j = 3 , Nstns
            aye(j,1) = aye(1,j)
            aye(j,2) = aye(2,j)
         ENDDO
!
!     Q DUE TO PRESL ONLY
!
!     NOW DEFINE LARGE G MATRIX
!
         DO I = 1 , 29
            gye(1,I) = 0.0
            gye(I,1) = 1.0
         ENDDO
!
!     PUT XLOW IN XTEMP
!
         DO I = 1 , 29
            xtemp(I) = xlow(I)
         ENDDO
         DO j = 3 , 29
            const = (j-2)*Pi/2.0
            DO I = 2 , 29
               gye(I,j) = sin(const*xtemp(I))
            ENDDO
         ENDDO
         DO I = 2 , 29
            gye(I,2) = xtemp(I)
         ENDDO
!
!     PUT PRESL PART OF GEE IN GEETMP
!
         DO I = 1 , 29
            DO j = 1 , nstns2
               geetmp(I,j) = gee(I,j)
            ENDDO
         ENDDO
!
!     SOLVE FOR G-INVERSE G IN GEE MATRIV
!     ISING = 1 NON-SINGULAR (GYE)
!     ISING = 2  SIGULAR     (GYE)
!     INDEX IS WORK STORAGE FOR ROUTINE INVERS
!
         ising = -1
         CALL invers(29,gye,29,geetmp,nstns2,determ,ising,index)
         IF ( ising/=2 ) THEN
!
!     NOW  MULTIPLY  I*G-INVERSE*G(DELTA P'S)
!
            DO j = 1 , Nstns
               DO k = 1 , Nstns
                  nf = k + Nstns
                  sumi = 0.0
                  sumr = 0.0
                  DO I = 1 , 29
                     sumr = aye(j,I)*geetmp(I,k) + sumr
                     sumi = aye(j,I)*geetmp(I,nf) + sumi
                  ENDDO
!
!  NOTE - NOTE THAT DUE TO CEXP( - I*OMEGA*T) TYPE OF TIME DEPENDENCE
!         IN UCAS DEVELOPMENT, Q IS DEFINED AS THE COMPLEX CONJUGATE
!         OF 'USUAL' Q
!
                  Q(j,k) = 2.0*cmplx(sumr,-sumi)
               ENDDO
            ENDDO
!
!     FINALLY, Q DUE TO (PRESL-PRESU) IS COMPUTED BY SUBTRACTING Q DUE
!     TO PRESU FROM Q DUE TO PRESL ABOVE
!
!     LARGE G MATRIX
!
            DO I = 1 , 29
               gye(1,I) = 0.0
               gye(I,1) = 1.0
            ENDDO
!
!     PUT XUP IN XTEMP
!
            DO I = 1 , 29
               xtemp(I) = xup(I)
            ENDDO
            DO j = 3 , 29
               const = (j-2)*Pi/2.0
               DO I = 2 , 29
                  gye(I,j) = sin(const*xtemp(I))
               ENDDO
            ENDDO
            DO I = 2 , 29
               gye(I,2) = xtemp(I)
            ENDDO
!
!     PUT PRESU PART OF GEE IN GEETMP
!
            DO I = 1 , 29
               DO j = 1 , nstns2
!
                  nsns2 = nstns2 + j
                  geetmp(I,j) = gee(I,nsns2)
               ENDDO
            ENDDO
!
!     SOLVE FOR G-INVERSE G IN GEETMP MATRIX
!     ISING = 1  NON-SINGULAR (GYE)
!     ISING = 2  SINGULAR GYE
!     INDEX IS WORK STORAGE FOR ROUTINE INVERS
!
            ising = -1
            CALL invers(29,gye,29,geetmp,nstns2,determ,ising,index)
!
            IF ( ising/=2 ) THEN
!
!     MULTIPLY I*G-INVERS*G
!
               DO j = 1 , Nstns
                  DO k = 1 , Nstns
                     nf = k + Nstns
                     sumi = 0.0
                     sumr = 0.0
                     DO I = 1 , 29
!
                        sumr = aye(j,I)*geetmp(I,k) + sumr
                        sumi = aye(j,I)*geetmp(I,nf) + sumi
!
                     ENDDO
!
                     Q(j,k) = Q(j,k) - 2.0*cmplx(sumr,-sumi)
                  ENDDO
               ENDDO
!
               RETURN
            ENDIF
         ENDIF
         WRITE (Ibbout,99003) Ufm
99003    FORMAT (A23,' - AMG MODULE - LARGE G-MATRIX IS SINGULAR IN ','ROUTINE AMGBIC.')
      ENDIF
   ENDIF
   CALL mesage(-61,0,0)
   RETURN
END SUBROUTINE amgb1c