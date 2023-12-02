!*==amgb1c.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE amgb1c(Q)
   USE c_amgmn
   USE c_bamg1l
   USE c_blk1
   USE c_blk2
   USE c_blk3
   USE c_blk4
   USE c_system
   USE c_xmssg
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   COMPLEX , DIMENSION(nstns,nstns) :: Q
!
! Local variable declarations rewritten by SPAG
!
   REAL :: aa , con , const , determ , s1 , sps1 , sumi , sumr , temp
   REAL , DIMENSION(10,29) :: aye
   REAL , DIMENSION(29,40) :: gee
   REAL , DIMENSION(29,20) :: geetmp
   REAL , DIMENSION(29,29) :: gye
   INTEGER , DIMENSION(29,3) :: index
   INTEGER :: ising , j , k , n1n , nf , nm , nm2 , nm3 , nm4 , nmm , nmmm , nsns2 , nstns2 , nstns4 , ntimes , nx , nxx
   COMPLEX , DIMENSION(29) :: presl , presu
   REAL , DIMENSION(29) :: xlow , xtemp , xup
   EXTERNAL akp2 , asycon , invers , mesage , suba
!
! End of declarations rewritten by SPAG
!
!
!     UNSTEADY FLOW ANALYSIS OF A SUPERSONIC CASCADE
!
!
!     THEORY DEPENDENT RESTRICTION OF NO MORE THAN 10 COMPUTING
!     STATIONS PER STREAMLINE IS REFLECTED IN CODING.
!
   IF ( nstns>10 ) THEN
      WRITE (ibbout,99001) ufm , sln , nstns
99001 FORMAT (A23,' - AMG MODULE - NUMBER OF COMPUTING STATIONS ON ','STREAMLINE',I8,4H IS ,I3,1H.,/39X,'SUPERSONIC CASCADE',       &
             &' ROUTINE AMGB1C ALLOWS ONLY A MAXIMUM OF 10.')
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
99002    FORMAT (A23,' - AMG MODULE -SUBROUTINE AMGB1C',/39X,'AXIAL MACH NUMB. IS EQUAL TO OR GREATER THAN ONE.')
      ELSE
!
!     ZERO OUT GEE
!
         nstns2 = 2*nstns
         nstns4 = 4*nstns
         DO i = 1 , 29
            DO j = 1 , nstns4
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
!     DEFINE -----------------------------
!            ALPAMP - PITCHING AMP
!            DISAMP - PLUNGING AMP
!            GUSAMP - GUST AMP
!            GL -GUST WAVE NUMBER
!
               alpamp = 0.0
               IF ( nm==2 ) alpamp = 1.0
               disamp = 0.0
               IF ( nm==1 ) disamp = 1.0
               gusamp = 0.0
               gl = 0.0
               IF ( nm>2 .AND. nmm==1 ) gusamp = -redf/2.0 + (nm-2)*pi/4.0
               IF ( nm>2 .AND. nmm==1 ) gl = (nm-2)*pi/2.0
               IF ( nm>2 .AND. nmm==2 ) gusamp = redf/2.0 + (nm-2)*pi/4.0
               IF ( nm>2 .AND. nmm==2 ) gl = -(nm-2)*pi/2.0
!
               a = (1.0+ai*redf*pitaxs)*alpamp - ai*redf*disamp
               b = -ai*redf*alpamp
               IF ( gl/=0.0 ) THEN
                  a = gusamp
                  b = 0.0
               ENDIF
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
               nm2 = nm + nstns
               nm3 = nm + 2*nstns
               nm4 = nm + 3*nstns
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
            aye(1,j+2) = con*4.0/j/pi
            con = 1.0 - con
         ENDDO
         aye(2,1) = 2.0
         aye(2,2) = 2.66666667
         con = 1.0
         DO j = 1 , n1n
            aye(2,j+2) = con*4/j/pi
            con = -con
         ENDDO
         DO i = 3 , nstns
            DO j = 2 , 28
               con = 0.0
               IF ( (i-1)==j ) con = 1.0
               aye(i,j+1) = con
            ENDDO
         ENDDO
         DO j = 3 , nstns
            aye(j,1) = aye(1,j)
            aye(j,2) = aye(2,j)
         ENDDO
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
!     PUT PRESL PART OF GEE IN GEETMP
!
         DO i = 1 , 29
            DO j = 1 , nstns2
               geetmp(i,j) = gee(i,j)
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
            DO j = 1 , nstns
               DO k = 1 , nstns
                  nf = k + nstns
                  sumi = 0.0
                  sumr = 0.0
                  DO i = 1 , 29
                     sumr = aye(j,i)*geetmp(i,k) + sumr
                     sumi = aye(j,i)*geetmp(i,nf) + sumi
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
!     PUT PRESU PART OF GEE IN GEETMP
!
            DO i = 1 , 29
               DO j = 1 , nstns2
!
                  nsns2 = nstns2 + j
                  geetmp(i,j) = gee(i,nsns2)
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
               DO j = 1 , nstns
                  DO k = 1 , nstns
                     nf = k + nstns
                     sumi = 0.0
                     sumr = 0.0
                     DO i = 1 , 29
!
                        sumr = aye(j,i)*geetmp(i,k) + sumr
                        sumi = aye(j,i)*geetmp(i,nf) + sumi
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
         WRITE (ibbout,99003) ufm
99003    FORMAT (A23,' - AMG MODULE - LARGE G-MATRIX IS SINGULAR IN ','ROUTINE AMGBIC.')
      ENDIF
   ENDIF
   CALL mesage(-61,0,0)
END SUBROUTINE amgb1c
