!*==amgt1c.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE amgt1c(Q,Nstns2,C1sbar,C2sbar)
   IMPLICIT NONE
   USE C_AMGMN
   USE C_BLK1
   USE C_BLK2
   USE C_BLK3
   USE C_BLK4
   USE C_SYSTEM
   USE C_TAMG1L
   USE C_XMSSG
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nstns2
   COMPLEX , DIMENSION(Nstns2,Nstns2) :: Q
   REAL :: C1sbar
   REAL :: C2sbar
!
! Local variable declarations rewritten by SPAG
!
   REAL :: aa , c2ssch , const , conz1 , conz2 , conz3 , conz4 , conz5 , conz6 , csbar , csbar1 , csblsb , csbm2s , determ ,        &
         & dlsdzb , m2sbar , s1 , sps1 , sumi1 , sumi2 , sumr1 , sumr2 , tanlam , td , temp
   REAL , DIMENSION(10,29) :: aye
   REAL , DIMENSION(29,80) :: gee
   REAL , DIMENSION(29,40) :: geetmp
   REAL , DIMENSION(29,29) :: gye
   INTEGER , DIMENSION(29,3) :: index
   INTEGER :: ising , j , jndx , jx , k , nm , nm2z , nm3z , nm4z , nmm , nmmm , nmz , nsns2 , nstns4 , nstns8 , ntimes , nx , nxx
   COMPLEX , DIMENSION(29) :: presl , presu
   REAL , DIMENSION(29) :: xlow , xtemp , xup
   EXTERNAL akp2 , asycon , invers , mesage , suba
!
! End of declarations rewritten by SPAG
!
!
!     SUPERSONIC CASCADE CODE FOR SWEPT TURBOPROPS.
!
!
!
!     THEORY DEPENDENT RESTRICTION OF NO MORE THAN 10 COMPUTING
!     STATIONS PER STREAMLINE IS REFLECTED IN CODING.
!
   IF ( Nstns>10 ) THEN
      WRITE (Ibbout,99001) Ufm , Sln , Nstns
99001 FORMAT (A23,' - AMG MODULE - NUMBER OF COMPUTING STATIONS ON ','STREAMLINE',I8,4H IS ,I3,1H.,/39X,'SUPERSONIC CASCADE ',      &
             &'ROUTINE AMGT1C ALLOWS ONLY A MAXIMUM OF 10.')
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
99002    FORMAT (A23,' - AMG MODULE -SUBROUTINE AMGT1C',/39X,'AXIAL MACH NUMB. IS EQUAL TO OR GREATER THAN ONE.')
      ELSE
!
!     PARAMETERS RELATED TO SWEEP CHANGES
!
         csbar = .25*(Den*Vel**2*Chord**2)/(Refden*Refvel**2)
         csbar1 = 2.0/Chord
         m2sbar = -Dcbdzb/Chord
         c2ssch = csbar1*C2sbar
         csblsb = csbar*csbar1
         csbm2s = csbar*m2sbar
         tanlam = tan(Sweep*Pi/180.)
         dlsdzb = Dcbdzb/2.0
         td = tanlam*dlsdzb
!
!     ZERO OUT GEE
!
         nstns4 = 4*Nstns
         nstns8 = 8*Nstns
         DO I = 1 , 29
            DO j = 1 , nstns8
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
               jndx = 0
               SPAG_Loop_3_1: DO
                  IF ( jndx==0 ) THEN
!
!     DEFINE -----------------------------
!              ALPAMP - PITCHING AMP
!              DISAMP - PLUNGING AMP
!              GUSAMP - GUST AMP
!              GL -GUST WAVE NUMBER
                     Alpamp = 0.0
                     IF ( nm==2 ) Alpamp = 1.0
                     Disamp = 0.0
                     IF ( nm==1 ) Disamp = 1.0
                     Gusamp = 0.0
                     Gl = 0.0
                     IF ( nm>2 .AND. nmm==1 ) Gusamp = -(Redf+Ai*td)/2. + (nm-2)*Pi/4.
                     IF ( nm>2 .AND. nmm==1 ) Gl = (nm-2)*Pi/2.0
                     IF ( nm>2 .AND. nmm==2 ) Gusamp = (Redf+Ai*td)/2. + (nm-2)*Pi/4.
                     IF ( nm>2 .AND. nmm==2 ) Gl = -(nm-2)*Pi/2.0
!
                     A = (1.0+Ai*Redf*Pitaxs)*Alpamp - (Ai*Redf-td)*Disamp
                     B = -(Ai*Redf-td)*Alpamp
                     IF ( Gl/=0.0 ) THEN
                        A = Gusamp
                        B = 0.0
                     ENDIF
!
                  ELSEIF ( nm>2 ) THEN
!
                     IF ( nmm==1 ) Gusamp = -Ai*tanlam/csbar1/2.0
                     IF ( nmm==1 ) Gl = (nm-2)*Pi/2.0
                     IF ( nmm==2 ) Gusamp = Ai*tanlam/csbar1/2.0
                     IF ( nmm==2 ) Gl = -(nm-2)*Pi/2.0
!
                     A = Gusamp
!
                     B = 0.0
                  ELSE
!
                     Gl = 0.0
                     IF ( nm==1 ) A = tanlam/csbar1
                     IF ( nm==1 ) B = 0.0
                     IF ( nm==2 ) A = 0.0
!
                     IF ( nm==2 ) B = tanlam/csbar1
                  ENDIF
!
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
!
                  jx = jndx*4*Nstns
                  nmz = nm + jx
                  nm2z = nm + Nstns + jx
                  nm3z = nm + 2*Nstns + jx
                  nm4z = nm + 3*Nstns + jx
!
                  DO nmmm = 1 , 29
                     gee(nmmm,nmz) = gee(nmmm,nmz) + real(presl(nmmm))
                     gee(nmmm,nm2z) = gee(nmmm,nm2z) + aimag(presl(nmmm))
                     gee(nmmm,nm3z) = gee(nmmm,nm3z) + real(presu(nmmm))
                     gee(nmmm,nm4z) = gee(nmmm,nm4z) + aimag(presu(nmmm))
!
                  ENDDO
!
                  IF ( jndx/=0 ) EXIT SPAG_Loop_3_1
                  jndx = 1
               ENDDO SPAG_Loop_3_1
!
            ENDDO
         ENDDO
!
!     NOW DEFINE  I-MATRIX (NSTNS X 29)
!
         aye(1,1) = C1sbar*2.0 + c2ssch*2.0
         aye(1,2) = C1sbar*8.0/3.0 + c2ssch*2.0
         aye(2,1) = C1sbar*8.0/3.0 + c2ssch*2.0
         aye(2,2) = C1sbar*4.0 + c2ssch*8.0/3.0
!
         conz1 = 1.0
!
         DO I = 3 , Nstns
            conz4 = (1.+conz1)*2./(Pi*(j-2))
            conz5 = conz1*4./(Pi*(j-2))
            conz6 = conz1*8./(Pi*(j-2)) - (1.+conz1)*16./(Pi*(j-2))**3
!
            aye(I,1) = C1sbar*conz5 + c2ssch*conz4
            aye(I,2) = C1sbar*conz6 + c2ssch*conz5
            conz1 = -conz1
         ENDDO
!
         conz1 = 1.0
!
         DO j = 3 , 29
            conz4 = (1.+conz1)*2./(Pi*(j-2))
            conz5 = conz1*4./(Pi*(j-2))
            conz6 = conz1*8./(Pi*(j-2)) - (1.+conz1)*16./(Pi*(j-2))**3
!
            aye(1,j) = C1sbar*conz5 + c2ssch*conz4
            aye(2,j) = C1sbar*conz6 + c2ssch*conz5
            conz1 = -conz1
         ENDDO
!
         DO I = 3 , Nstns
!
            DO j = 3 , 29
               conz1 = 0.0
               IF ( j==I ) THEN
                  conz1 = 1.0
                  conz2 = 1.0
               ELSE
                  IF ( (I+j)/2*2/=(I+j) ) conz1 = -16.*(I-2)*(j-2)/(Pi*Pi*(I-j)*(I-j)*(I+j-4)**2)
                  conz2 = 0.0
               ENDIF
               aye(I,j) = C1sbar*conz1 + c2ssch*conz2
            ENDDO
         ENDDO
!
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
!     PUT PRESL PARTS OF GEE IN GEETMP (UNPRIMED AND PRIMED TERMS)
!
         DO I = 1 , 29
            DO j = 1 , Nstns2
               geetmp(I,j) = gee(I,j)
               geetmp(I,j+Nstns2) = gee(I,j+nstns4)
            ENDDO
         ENDDO
!
!     SOLVE FOR G-INVERSE G IN GEE MATRIV
!     ISING = 1  NON-SINGULAR (GYE)
!     ISING = 2  SIGULAR      (GYE)
!     INDEX IS WORK STORAGE FOR ROUTINE INVERS
!
         ising = -1
         CALL invers(29,gye,29,geetmp,nstns4,determ,ising,index)
         IF ( ising/=2 ) THEN
!
!     NOW  MULTIPLY  I*G-INVERSE*G(DELTA P'S)
!
            DO j = 1 , Nstns
               DO k = 1 , Nstns
!
                  sumr1 = 0.0
                  sumi1 = 0.0
                  sumr2 = 0.0
                  sumi2 = 0.0
!
                  DO I = 1 , 29
                     sumr1 = sumr1 + aye(j,I)*geetmp(I,k)
                     sumi1 = sumi1 + aye(j,I)*geetmp(I,k+Nstns)
                     sumr2 = sumr2 + aye(j,I)*geetmp(I,k+nstns4)
                     sumi2 = sumi2 + aye(j,I)*geetmp(I,k+Nstns+nstns4)
                  ENDDO
!
                  conz1 = csblsb*sumr1 + csbm2s*sumr2
                  conz2 = csblsb*sumi1 + csbm2s*sumi2
                  conz3 = csbar*sumr2
                  conz4 = csbar*sumi2
!
                  Q(j,k) = 2.0*cmplx(conz1,-conz2)
                  Q(j,k+Nstns) = 2.0*cmplx(conz3,-conz4)
                  Q(j+Nstns,k) = (0.0,0.0)
                  Q(j+Nstns,k+Nstns) = (0.0,0.0)
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
!     PUT PRESU PARTS OF GEE IN GEETMP (UNPRIMED AND PRIMED TERMS)
!
            DO I = 1 , 29
               DO j = 1 , Nstns2
!
                  nsns2 = Nstns2 + j
                  geetmp(I,j) = gee(I,nsns2)
                  geetmp(I,nsns2) = gee(I,nsns2+nstns4)
               ENDDO
            ENDDO
!
!     SOLVE FOR G-INVERSE G IN GEETMP MATRIX
!     ISING = 1  NON-SINGULAR (GYE)
!     ISING = 2  SINGULAR GYE
!     INDEX IS WORK STORAGE FOR ROUTINE INVERS
!
            ising = -1
            CALL invers(29,gye,29,geetmp,nstns4,determ,ising,index)
!
            IF ( ising/=2 ) THEN
!
!    MULTIPLY I*G-INVERS*G
!
               DO j = 1 , Nstns
                  DO k = 1 , Nstns
!
                     sumr1 = 0.0
                     sumi1 = 0.0
                     sumr2 = 0.0
                     sumi2 = 0.0
!
                     DO I = 1 , 29
                        sumr1 = sumr1 + aye(j,I)*geetmp(I,k)
                        sumi1 = sumi1 + aye(j,I)*geetmp(I,k+Nstns)
                        sumr2 = sumr2 + aye(j,I)*geetmp(I,k+nstns4)
                        sumi2 = sumi2 + aye(j,I)*geetmp(I,k+Nstns+nstns4)
                     ENDDO
!
                     conz1 = csblsb*sumr1 + csbm2s*sumr2
                     conz2 = csblsb*sumi1 + csbm2s*sumi2
                     conz3 = csbar*sumr2
                     conz4 = csbar*sumi2
!
                     Q(j,k) = Q(j,k) - 2.0*cmplx(conz1,-conz2)
                     Q(j,k+Nstns) = Q(j,k+Nstns) - 2.0*cmplx(conz3,-conz4)
                  ENDDO
               ENDDO
               RETURN
            ENDIF
         ENDIF
         WRITE (Ibbout,99003) Ufm
99003    FORMAT (A23,' - AMG MODULE - LARGE G-MATRIX IS SINGULAR IN ','ROUTINE AMGT1C.')
      ENDIF
   ENDIF
   CALL mesage(-61,0,0)
END SUBROUTINE amgt1c
