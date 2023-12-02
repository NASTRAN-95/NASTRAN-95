!*==amgt1c.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE amgt1c(Q,Nstns2,C1sbar,C2sbar)
   USE c_amgmn
   USE c_blk1
   USE c_blk2
   USE c_blk3
   USE c_blk4
   USE c_system
   USE c_tamg1l
   USE c_xmssg
   IMPLICIT NONE
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
   IF ( nstns>10 ) THEN
      WRITE (ibbout,99001) ufm , sln , nstns
99001 FORMAT (A23,' - AMG MODULE - NUMBER OF COMPUTING STATIONS ON ','STREAMLINE',I8,4H IS ,I3,1H.,/39X,'SUPERSONIC CASCADE ',      &
             &'ROUTINE AMGT1C ALLOWS ONLY A MAXIMUM OF 10.')
   ELSE
!
      redf = redfd
      amach = amachd
      ai = cmplx(0.0,1.0)
      pi = 3.1415927
      pitcor = blspc
      stag = 90.0 - stg
      sigma = -sigm*pi/180.0
      beta = sqrt(amach**2-1.0)
      scrk = redf*amach/(beta**2)
      del = scrk*amach
      amu = redf/(beta**2)
      sp = pitcor*cos(stag*pi/180.0)*2.0
      sn = pitcor*sin(stag*pi/180.0)*2.0
      sps = sp
      sns = sn*beta
      dstr = sqrt(sps**2-sns**2)
      sps1 = abs(sps-sns)
      IF ( sps1<.00001 ) THEN
!
         WRITE (ibbout,99002) ufm
!
99002    FORMAT (A23,' - AMG MODULE -SUBROUTINE AMGT1C',/39X,'AXIAL MACH NUMB. IS EQUAL TO OR GREATER THAN ONE.')
      ELSE
!
!     PARAMETERS RELATED TO SWEEP CHANGES
!
         csbar = .25*(den*vel**2*chord**2)/(refden*refvel**2)
         csbar1 = 2.0/chord
         m2sbar = -dcbdzb/chord
         c2ssch = csbar1*C2sbar
         csblsb = csbar*csbar1
         csbm2s = csbar*m2sbar
         tanlam = tan(sweep*pi/180.)
         dlsdzb = dcbdzb/2.0
         td = tanlam*dlsdzb
!
!     ZERO OUT GEE
!
         nstns4 = 4*nstns
         nstns8 = 8*nstns
         DO i = 1 , 29
            DO j = 1 , nstns8
               gee(i,j) = 0.0
            ENDDO
         ENDDO
         pitaxs = 0.0
         amoaxs = 0.
         CALL asycon
         CALL akp2
         rl1 = 9
         s1 = sps - sns
         aa = s1/rl1
         xlsv1(1) = 0.0
         DO jl = 1 , 9
            xlsv1(jl+1) = jl*aa
         ENDDO
         aa = sps - sns
         rl2 = 19
         s1 = 2.0 + sns - sps
         temp = s1/rl2
         xl = aa
         DO jl = 1 , 20
            xlsv2(jl) = xl
            xlsv3(jl) = xl + sns - sps
            xl = xl + temp
         ENDDO
         xl = sns + 2.0 - sps
         temp = (sps-sns)/rl1
         DO jl = 1 , 10
            xlsv4(jl) = xl
            xl = xl + temp
         ENDDO
!
!     ACCUMULATE PRESSURE VECTORS INTO G-MATRIX
!
         DO nm = 1 , nstns
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
                     alpamp = 0.0
                     IF ( nm==2 ) alpamp = 1.0
                     disamp = 0.0
                     IF ( nm==1 ) disamp = 1.0
                     gusamp = 0.0
                     gl = 0.0
                     IF ( nm>2 .AND. nmm==1 ) gusamp = -(redf+ai*td)/2. + (nm-2)*pi/4.
                     IF ( nm>2 .AND. nmm==1 ) gl = (nm-2)*pi/2.0
                     IF ( nm>2 .AND. nmm==2 ) gusamp = (redf+ai*td)/2. + (nm-2)*pi/4.
                     IF ( nm>2 .AND. nmm==2 ) gl = -(nm-2)*pi/2.0
!
                     a = (1.0+ai*redf*pitaxs)*alpamp - (ai*redf-td)*disamp
                     b = -(ai*redf-td)*alpamp
                     IF ( gl/=0.0 ) THEN
                        a = gusamp
                        b = 0.0
                     ENDIF
!
                  ELSEIF ( nm>2 ) THEN
!
                     IF ( nmm==1 ) gusamp = -ai*tanlam/csbar1/2.0
                     IF ( nmm==1 ) gl = (nm-2)*pi/2.0
                     IF ( nmm==2 ) gusamp = ai*tanlam/csbar1/2.0
                     IF ( nmm==2 ) gl = -(nm-2)*pi/2.0
!
                     a = gusamp
!
                     b = 0.0
                  ELSE
!
                     gl = 0.0
                     IF ( nm==1 ) a = tanlam/csbar1
                     IF ( nm==1 ) b = 0.0
                     IF ( nm==2 ) a = 0.0
!
                     IF ( nm==2 ) b = tanlam/csbar1
                  ENDIF
!
                  CALL suba
!
!     FIND  DELTA P(LOWER-UPPER)
!
                  DO nx = 1 , 10
                     presu(nx) = pres1(nx)
                     xup(nx) = xlsv1(nx)
                     IF ( nx==10 ) THEN
                        presu(nx) = (pres1(10)+pres2(1))/2.0
                        xup(10) = (xlsv1(10)+xlsv2(1))/2.0
                     ELSE
                        nxx = nx + 20
                        presl(nxx) = pres4(nx+1)
                        xlow(nxx) = xlsv4(nx+1)
                     ENDIF
                  ENDDO
                  DO nx = 1 , 20
                     nxx = nx + 10
                     IF ( nx==20 ) THEN
                        presl(20) = (pres3(20)+pres4(1))/2.0
                        xlow(20) = (xlsv3(20)+xlsv4(1))/2.0
                     ELSE
                        presu(nxx) = pres2(nx+1)
                        xup(nxx) = xlsv2(nx+1)
                        presl(nx) = pres3(nx)
                        xlow(nx) = xlsv3(nx)
                     ENDIF
                  ENDDO
!
                  jx = jndx*4*nstns
                  nmz = nm + jx
                  nm2z = nm + nstns + jx
                  nm3z = nm + 2*nstns + jx
                  nm4z = nm + 3*nstns + jx
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
         DO i = 3 , nstns
            conz4 = (1.+conz1)*2./(pi*(j-2))
            conz5 = conz1*4./(pi*(j-2))
            conz6 = conz1*8./(pi*(j-2)) - (1.+conz1)*16./(pi*(j-2))**3
!
            aye(i,1) = C1sbar*conz5 + c2ssch*conz4
            aye(i,2) = C1sbar*conz6 + c2ssch*conz5
            conz1 = -conz1
         ENDDO
!
         conz1 = 1.0
!
         DO j = 3 , 29
            conz4 = (1.+conz1)*2./(pi*(j-2))
            conz5 = conz1*4./(pi*(j-2))
            conz6 = conz1*8./(pi*(j-2)) - (1.+conz1)*16./(pi*(j-2))**3
!
            aye(1,j) = C1sbar*conz5 + c2ssch*conz4
            aye(2,j) = C1sbar*conz6 + c2ssch*conz5
            conz1 = -conz1
         ENDDO
!
         DO i = 3 , nstns
!
            DO j = 3 , 29
               conz1 = 0.0
               IF ( j==i ) THEN
                  conz1 = 1.0
                  conz2 = 1.0
               ELSE
                  IF ( (i+j)/2*2/=(i+j) ) conz1 = -16.*(i-2)*(j-2)/(pi*pi*(i-j)*(i-j)*(i+j-4)**2)
                  conz2 = 0.0
               ENDIF
               aye(i,j) = C1sbar*conz1 + c2ssch*conz2
            ENDDO
         ENDDO
!
!
!     Q DUE TO PRESL ONLY
!
!     NOW DEFINE LARGE G MATRIX
!
         DO i = 1 , 29
            gye(1,i) = 0.0
            gye(i,1) = 1.0
         ENDDO
!
!     PUT XLOW IN XTEMP
!
         DO i = 1 , 29
            xtemp(i) = xlow(i)
         ENDDO
         DO j = 3 , 29
            const = (j-2)*pi/2.0
            DO i = 2 , 29
               gye(i,j) = sin(const*xtemp(i))
            ENDDO
         ENDDO
         DO i = 2 , 29
            gye(i,2) = xtemp(i)
         ENDDO
!
!     PUT PRESL PARTS OF GEE IN GEETMP (UNPRIMED AND PRIMED TERMS)
!
         DO i = 1 , 29
            DO j = 1 , Nstns2
               geetmp(i,j) = gee(i,j)
               geetmp(i,j+Nstns2) = gee(i,j+nstns4)
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
            DO j = 1 , nstns
               DO k = 1 , nstns
!
                  sumr1 = 0.0
                  sumi1 = 0.0
                  sumr2 = 0.0
                  sumi2 = 0.0
!
                  DO i = 1 , 29
                     sumr1 = sumr1 + aye(j,i)*geetmp(i,k)
                     sumi1 = sumi1 + aye(j,i)*geetmp(i,k+nstns)
                     sumr2 = sumr2 + aye(j,i)*geetmp(i,k+nstns4)
                     sumi2 = sumi2 + aye(j,i)*geetmp(i,k+nstns+nstns4)
                  ENDDO
!
                  conz1 = csblsb*sumr1 + csbm2s*sumr2
                  conz2 = csblsb*sumi1 + csbm2s*sumi2
                  conz3 = csbar*sumr2
                  conz4 = csbar*sumi2
!
                  Q(j,k) = 2.0*cmplx(conz1,-conz2)
                  Q(j,k+nstns) = 2.0*cmplx(conz3,-conz4)
                  Q(j+nstns,k) = (0.0,0.0)
                  Q(j+nstns,k+nstns) = (0.0,0.0)
               ENDDO
            ENDDO
!
!     FINALLY, Q DUE TO (PRESL-PRESU) IS COMPUTED BY SUBTRACTING Q DUE
!     TO PRESU FROM Q DUE TO PRESL ABOVE
!
!     LARGE G MATRIX
!
            DO i = 1 , 29
               gye(1,i) = 0.0
               gye(i,1) = 1.0
            ENDDO
!
!     PUT XUP IN XTEMP
!
            DO i = 1 , 29
               xtemp(i) = xup(i)
            ENDDO
            DO j = 3 , 29
               const = (j-2)*pi/2.0
               DO i = 2 , 29
                  gye(i,j) = sin(const*xtemp(i))
               ENDDO
            ENDDO
            DO i = 2 , 29
               gye(i,2) = xtemp(i)
            ENDDO
!
!     PUT PRESU PARTS OF GEE IN GEETMP (UNPRIMED AND PRIMED TERMS)
!
            DO i = 1 , 29
               DO j = 1 , Nstns2
!
                  nsns2 = Nstns2 + j
                  geetmp(i,j) = gee(i,nsns2)
                  geetmp(i,nsns2) = gee(i,nsns2+nstns4)
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
               DO j = 1 , nstns
                  DO k = 1 , nstns
!
                     sumr1 = 0.0
                     sumi1 = 0.0
                     sumr2 = 0.0
                     sumi2 = 0.0
!
                     DO i = 1 , 29
                        sumr1 = sumr1 + aye(j,i)*geetmp(i,k)
                        sumi1 = sumi1 + aye(j,i)*geetmp(i,k+nstns)
                        sumr2 = sumr2 + aye(j,i)*geetmp(i,k+nstns4)
                        sumi2 = sumi2 + aye(j,i)*geetmp(i,k+nstns+nstns4)
                     ENDDO
!
                     conz1 = csblsb*sumr1 + csbm2s*sumr2
                     conz2 = csblsb*sumi1 + csbm2s*sumi2
                     conz3 = csbar*sumr2
                     conz4 = csbar*sumi2
!
                     Q(j,k) = Q(j,k) - 2.0*cmplx(conz1,-conz2)
                     Q(j,k+nstns) = Q(j,k+nstns) - 2.0*cmplx(conz3,-conz4)
                  ENDDO
               ENDDO
               RETURN
            ENDIF
         ENDIF
         WRITE (ibbout,99003) ufm
99003    FORMAT (A23,' - AMG MODULE - LARGE G-MATRIX IS SINGULAR IN ','ROUTINE AMGT1C.')
      ENDIF
   ENDIF
   CALL mesage(-61,0,0)
END SUBROUTINE amgt1c
