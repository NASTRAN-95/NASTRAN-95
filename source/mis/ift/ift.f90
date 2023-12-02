!*==ift.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ift
   IMPLICIT NONE
   USE C_BLANK
   USE C_CONDAS
   USE C_PACKX
   USE C_SYSTEM
   USE C_UNPAKX
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: casecc , fol , tol , trl , uhvf , uhvt
   REAL :: ck , cmnc , cmnr , cp , delt , delw , dmnc , dmnr , epsi , fbig , r1 , r2 , rp , sk , t , tt , wn , wnm1 , wnp1
   INTEGER :: file , i , iap , ibuf1 , ibuf2 , ick , iequal , ifreq , ihop , im , im1 , im2 , isk , itstp , iudot , iuhvf , iuvt ,  &
            & j , k , kk , l , ll , lll , lx , m , m1 , m2 , n , n1 , n2 , nbig , nfreq , ngroup , nload , nmodes , nstep , nz
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(7) :: mcb , mcb1
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL bldpk , bldpkn , close , fname , fread , gopen , ifte2 , iftg , korsz , mesage , open , pack , pexit , rdtrl , read ,   &
          & skprec , unpack , write , wrttrl , zeroc
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     INVERSE FOURIER TRANSFORM MODULE (IFT)
!
!     DMAP CALLING SEQ.
!
!     IFT   UHVF,CASECC,TRL,FOL/UHVT,TOL/C,Y,IFTM
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
   DATA uhvf , casecc , trl , fol , uhvt , tol/101 , 102 , 103 , 104 , 201 , 202/
   DATA name/4HIFT  , 1H /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     VARIABLE CORE
!
!        CONTENT         LENGTH         POINTER
!        -------         ------         -------
!     FOL                NFREQ          IFREQ
!     TSTEP              NGROUP*3       ITSTP
!     UHVF               NMODES*NFREQ*2 IUHVF
!     CK                 NBIG           ICK
!     SK                 NBIG           ISK
!     UDOT               NMODES*NFREW*2 IUDOT
!     UHVT               NMODES         IUVT
!
!
!
!     PUT FOL INTO CORE
!
         nz = korsz(iz)
         ibuf1 = nz - Sysbuf + 1
         ibuf2 = ibuf1 - Sysbuf
         nz = nz - 2*Sysbuf
         file = fol
         CALL open(*60,fol,iz(ibuf1),0)
         CALL fread(fol,iz,-2,0)
         CALL read(*80,*20,fol,iz,nz,0,nfreq)
         CALL mesage(-8,0,name)
 20      CALL close(fol,1)
         ifreq = 1
         nz = nz - nfreq
         itstp = nfreq + 1
!
!     DEFINE BASIC SIZES
!
         mcb(1) = uhvf
         CALL rdtrl(mcb)
         nload = mcb(2)/nfreq
         nmodes = mcb(3)
         mcb(1) = uhvt
         mcb(2) = 0
         mcb(5) = 1
         mcb(6) = 0
         mcb(7) = 0
         k = nfreq + 2*(nmodes*nfreq*2) + nmodes
         IF ( k>nz ) CALL mesage(-8,0,name)
!
!     DETERMINE IF EQUAL FREQ - CONVERT TO W'S
!
         delw = Z(ifreq+1) - Z(ifreq)
         epsi = delw*1.E-6
         j = nfreq - 1
         iequal = 1
         DO i = 1 , j
            m = ifreq + i - 1
            IF ( abs(Z(m+1)-Z(m)-delw)>=epsi ) iequal = 0
            Z(m) = Z(m)*Twopi
         ENDDO
         Z(ifreq+nfreq-1) = Z(ifreq+nfreq-1)*Twopi
         delw = delw*Twopi
!
!     FIRST FREQUENCY MUST BE MULTIPLE OF DELW
!
         nbig = abs(Z(ifreq)/delw) + .1
         IF ( abs(float(nbig)*delw-abs(Z(ifreq)))>epsi ) iequal = 0
         lll = nbig - 1
!
!     FIND TSTEP IN TRL
!
         CALL gopen(casecc,iz(ibuf1),0)
         CALL fread(casecc,0,-37,0)
         CALL fread(casecc,j,1,0)
         CALL close(casecc,1)
         file = trl
         CALL open(*60,trl,iz(ibuf1),0)
         CALL fread(trl,mcb1,3,1)
         m = mcb1(3)
         CALL skprec(trl,m)
         SPAG_Loop_1_1: DO
            CALL fread(trl,m,1,0)
            IF ( m==j ) THEN
!
!     FOUND TSTEP
!
               CALL read(*80,*40,trl,iz(itstp),nz,0,ngroup)
               CALL mesage(-8,0,name)
               EXIT SPAG_Loop_1_1
            ELSE
               CALL fread(trl,0,0,1)
            ENDIF
         ENDDO SPAG_Loop_1_1
 40      nz = nz - ngroup
         iuhvf = itstp + ngroup
         CALL close(trl,1)
         ngroup = ngroup/3
         IF ( ngroup/=1 ) iequal = 0
         IF ( iequal/=0 ) THEN
!
!     FORCE WAT TO BE INTEGER MULTIPLE OF TWOPI/N
!
            fbig = Twopi/(delw*Z(itstp+1))
            nbig = fbig + .9
            Z(itstp+1) = Twopi/(float(nbig)*delw)
         ENDIF
!
!     BUILD / WRITE TOL
!
         file = tol
         CALL open(*60,tol,iz(ibuf1),1)
         CALL fname(tol,mcb1)
         CALL write(tol,mcb1,2,0)
         delt = Z(itstp+1)
         t = 0.0
         n = 0
         m = itstp
         DO i = 1 , ngroup
            nstep = iz(m)
            IF ( i==1 ) nstep = nstep + 1
            m = m + 3
            DO j = 1 , nstep
               CALL write(tol,t,1,0)
               n = n + 1
               IF ( j==nstep .AND. i/=ngroup ) delt = Z(m+1)
               t = t + delt
            ENDDO
         ENDDO
         CALL write(tol,0,0,1)
         CALL close(tol,1)
         mcb1(1) = tol
         mcb1(2) = ngroup
         mcb1(3) = n
         mcb1(4) = 0
         mcb1(5) = 0
         mcb1(6) = 0
         mcb1(7) = 0
         CALL wrttrl(mcb1)
!
!     BUILD TABLE OF CK, SK
!
         ick = iuhvf + 2*nmodes*nfreq
         isk = ick
         iudot = isk
         IF ( iequal/=0 ) THEN
            isk = ick + nbig
            iudot = isk + nbig
            m = ick
            m1 = isk
            m2 = isk
            j = iudot
            rp = cos(Twopi/float(nbig))
            cp = sin(Twopi/float(nbig))
            i = m
            n = m1 + 1
            l = m2
            kk = j
            Z(i) = 1.0
            Z(l) = 0.0
            SPAG_Loop_1_2: DO
               IF ( m1-i<2 ) EXIT SPAG_Loop_1_2
               IF ( m1-i==2 ) THEN
                  cmnr = -1.
                  cmnc = 0.
               ELSE
                  cmnr = rp*Z(i) - cp*Z(l)
                  cmnc = cp*Z(i) + rp*Z(l)
               ENDIF
               i = i + 1
               l = l + 1
               m1 = m1 - 1
               kk = kk - 1
               Z(i) = cmnr
               Z(l) = cmnc
               Z(m1) = cmnr
               Z(kk) = -cmnc
            ENDDO SPAG_Loop_1_2
         ENDIF
!     GET READY FOR OUTPUTS
!
         CALL gopen(uhvf,iz(ibuf1),0)
         CALL gopen(uhvt,iz(ibuf2),1)
         It1 = 1
         It2 = 1
         Ii = 1
         Jj = nmodes
         Incr = 1
         It3 = 3
         Ii1 = 1
         Jj1 = nmodes
         Incr1 = 1
         iuvt = iudot
         IF ( Iftm==2 ) iuvt = iuvt + 2*nfreq*nmodes
         ASSIGN 48 TO ihop
!
!     BEGIN LOOP ON LOADS
!
         DO i = 1 , nload
!
!     PUT UHVF INTO CORE
!
            DO j = 1 , nfreq
               m = iuhvf + (j-1)*nmodes*2
               CALL unpack(*45,uhvf,Z(m))
               CYCLE
 45            CALL zeroc(Z(m),2*nmodes)
            ENDDO
            IF ( Iftm==2 ) THEN
               ASSIGN 46 TO ihop
!
!     COMPUTE SPLINE FIT FOR U DOT
!
!
!     COMPUTE A'S
!
               iap = iuvt + nmodes
               m = nfreq + iap - 1
               Z(m) = 0.0
               l = nfreq - 2
               IF ( l>0 ) THEN
                  DO j = 1 , l
                     m = iap + nfreq - j - 1
                     n = ifreq + nfreq - j - 1
                     Z(m) = (Z(n)-Z(n-1))/(2.*(Z(n+1)-Z(n-1))-(Z(n+1)-Z(n))*Z(m+1))
                  ENDDO
               ENDIF
!
!     COMPUTE U DOT DOT
!
               DO m1 = 1 , nmodes
                  m = iudot + (nfreq-1)*nmodes*2 + (m1-1)*2
                  Z(m) = 0.0
                  Z(m+1) = 0.0
!
!     BEGIN BACKWARD PASS
!
                  m2 = iuhvf + (nfreq-1)*nmodes*2 + (m1-1)*2
                  IF ( l>0 ) THEN
                     DO j = 1 , l
                        n2 = m
                        m = m - nmodes*2
                        n = ifreq + nfreq - j - 1
                        m2 = m2 - nmodes*2
                        kk = iap + nfreq - j
                        ll = m2 + 2*nmodes
                        rp = Z(n+1) - Z(n)
                        cp = Z(n) - Z(n-1)
                        n1 = m2 - 2*nmodes
                        Z(m) = (6.*((Z(ll)-Z(m2))/rp-(Z(m2)-Z(n1))/cp)-rp*Z(kk)*Z(n2))/cp
                        Z(m+1) = (6.*((Z(ll+1)-Z(m2+1))/rp-(Z(m2+1)-Z(n1+1))/cp)-rp*Z(kk)*Z(n2+1))/cp
                     ENDDO
                  ENDIF
               ENDDO
!
!     BEGIN FORWARD PASS
!
               DO m1 = 1 , nmodes
                  m = iudot + (m1-1)*2
                  m2 = iuhvf + (m1-1)*2
                  n1 = m2 + 2*nmodes
                  ll = m + 2*nmodes
                  rp = Z(ifreq+1) - Z(ifreq)
                  Z(m) = (6.*(Z(n1)-Z(m2))/rp-rp*Z(iap+1)*Z(ll))/(6.*Z(ifreq)+(rp)*(2.-Z(iap+1)))
                  Z(m+1) = 0.0
                  DO j = 2 , nfreq
                     kk = iap + j - 1
                     m2 = m
                     m = m + 2*nmodes
                     Z(m) = Z(kk)*(Z(ll)-Z(m2))
                     Z(m+1) = Z(kk)*(Z(ll+1)-Z(m2+1))
                     ll = ll + 2*nmodes
                  ENDDO
               ENDDO
            ENDIF
            t = 0.0
            n = 0
            m = itstp
            delt = Z(itstp+1)
!
!     BEGIN LOOP ON TIMES
!
            DO l = 1 , ngroup
               nstep = iz(m)
               IF ( l==1 ) nstep = nstep + 1
               m = m + 3
               DO j = 1 , nstep
                  tt = t
                  CALL zeroc(Z(iuvt),nmodes)
!
!     BEGIN LOOP ON FREQUENCIES
!
                  lx = lll
                  DO ll = 1 , nfreq
                     spag_nextblock_2 = 1
                     SPAG_DispatchLoop_2: DO
                        SELECT CASE (spag_nextblock_2)
                        CASE (1)
                           lx = lx + 1
                           wn = Z(ifreq+ll-1)
                           IF ( ll/=1 ) wnm1 = Z(ifreq+ll-2)
                           IF ( ll/=nfreq ) wnp1 = Z(ifreq+ll)
                           IF ( iequal==0 ) THEN
                              ck = cos(wn*tt)
                              sk = sin(wn*tt)
                           ELSE
                              kk = mod(lx*n,nbig)
                              ck = Z(ick+kk)
                              sk = Z(isk+kk)
                           ENDIF
!
!     COMPUTE CMN, DMN
!
                           IF ( Iftm/=0 ) THEN
!
!     IFTM = 1
!
                              IF ( ll==1 ) THEN
                                 cmnr = 0.
                                 cmnc = 0.
                              ELSE
                                 IF ( ll>2 .AND. iequal/=0 .AND. ll/=nfreq ) THEN
                                    spag_nextblock_2 = 2
                                    CYCLE SPAG_DispatchLoop_2
                                 ENDIF
                                 r1 = wn - wnm1
                                 CALL ifte2(-tt*r1,rp,cp)
                                 cmnr = r1*.5*rp
                                 cmnc = r1*.5*cp
                              ENDIF
                              IF ( ll/=nfreq ) THEN
                                 r2 = wnp1 - wn
                                 CALL ifte2(tt*r2,rp,cp)
                                 cmnr = cmnr + r2*.5*rp
                                 cmnc = cmnc + r2*.5*cp
                              ENDIF
                           ELSE
!
!     IFTM  =0
!
                              cmnc = 0.0
                              IF ( ll==1 ) THEN
                                 cmnr = wnp1 - wn
                                 IF ( wn==0.0 ) cmnr = cmnr*.5
                              ELSEIF ( ll==nfreq ) THEN
                                 cmnr = wn - wnm1
                              ELSE
                                 cmnr = (wnp1-wnm1)*.5
                              ENDIF
                              spag_nextblock_2 = 3
                              CYCLE SPAG_DispatchLoop_2
                           ENDIF
                           spag_nextblock_2 = 2
                        CASE (2)
                           IF ( Iftm==2 ) THEN
!
!     IFTM = 2
!
                              im2 = iudot - 2 + (ll-1)*nmodes*2
                              IF ( ll==1 ) THEN
                                 dmnr = 0.0
                                 dmnc = 0.0
                              ELSE
                                 IF ( ll>2 .AND. iequal/=0 .AND. ll/=nfreq ) THEN
                                    spag_nextblock_2 = 3
                                    CYCLE SPAG_DispatchLoop_2
                                 ENDIF
                                 CALL iftg(-tt*r1,rp,cp)
                                 r1 = -r1*r1*r1/24.
                                 dmnr = r1*rp
                                 dmnc = r1*cp
                              ENDIF
                              IF ( ll/=nfreq ) THEN
                                 CALL iftg(tt*r2,rp,cp)
                                 r2 = -r2*r2*r2/24.
                                 dmnr = dmnr + r2*rp
                                 dmnc = dmnc + r2*cp
                              ENDIF
                           ELSE
                              dmnr = 0.0
                              dmnc = 0.0
                           ENDIF
                           spag_nextblock_2 = 3
                        CASE (3)
                           im1 = iuhvf - 2 + (ll-1)*nmodes*2
!
!     BEGIN LOOP ON MODES
!
                           DO kk = 1 , nmodes
                              im = im1 + 2*kk
                              rp = cmnr*Z(im) - cmnc*Z(im+1)
                              cp = cmnc*Z(im) + cmnr*Z(im+1)
                              GOTO ihop
 46                           im = im2 + 2*kk
                              rp = rp + dmnr*Z(im) - dmnc*Z(im+1)
                              cp = cp + dmnc*Z(im) + dmnr*Z(im+1)
 48                           Z(iuvt+kk-1) = Z(iuvt+kk-1) + rp*ck - cp*sk
!
!     END LOOP ON MODES
!
                           ENDDO
                           EXIT SPAG_DispatchLoop_2
                        END SELECT
                     ENDDO SPAG_DispatchLoop_2
!
!     END LOOP ON FREQUENCIES
!
                  ENDDO
                  DO kk = 1 , nmodes
                     Z(iuvt+kk-1) = Z(iuvt+kk-1)/Phi
                  ENDDO
                  CALL pack(Z(iuvt),uhvt,mcb)
                  DO kk = 1 , 2
                     CALL bldpk(1,1,uhvt,0,0)
                     CALL bldpkn(uhvt,0,mcb)
                  ENDDO
                  IF ( j==nstep ) delt = Z(m+1)
                  t = t + delt
                  n = n + 1
               ENDDO
!
!     END LOOP ON TIME
!
            ENDDO
!
!     END LOOP ON LOADS
!
         ENDDO
         CALL close(uhvf,1)
         CALL close(uhvt,1)
         CALL wrttrl(mcb)
         RETURN
!
!     ERROR MESSAGES
!
 60      n1 = -1
         spag_nextblock_1 = 2
      CASE (2)
         CALL mesage(n1,file,name)
         CALL pexit
 80      n1 = -2
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ift
