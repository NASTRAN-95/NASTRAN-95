
SUBROUTINE prolat
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Buf2 , Hest , Ii , Iii , Incr , Ist , Itypin , Itypou , Iz(6) , Jncr , Load , Mcore , Ng1 , Ng2 , Nn , Nnn , Nslt ,      &
         & Ntot , Otpe , Scr1 , Subcas , Sysbuf , Typout
   LOGICAL Remfl
   CHARACTER*25 Sfm , Uwm
   CHARACTER*31 Sim
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   REAL X1 , X2 , Y1 , Y2 , Z(1) , Z1 , Z2
   COMMON /biot  / Ng1 , Ng2 , Ist , Subcas , X1 , Y1 , Z1 , X2 , Y2 , Z2 , Buf2 , Remfl , Mcore , Load , Nslt , Scr1 , Hest , Ntot
   COMMON /packx / Itypin , Itypou , Iii , Nnn , Jncr
   COMMON /system/ Sysbuf , Otpe
   COMMON /unpakx/ Typout , Ii , Nn , Incr
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm , Sim
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   REAL ab , add , ang , area , areaep , c , cm , cn , con , detj , dfoc , dxi , em , eps , eta , etai(4) , etaint(4) , fden ,      &
      & fnum , fo(7) , hcdl , inter(4,4) , j11 , j12 , j21 , j22 , phi , phiint(4) , pi , pnmv(4) , poti(4) , potv(4) , pt1 , pt2 , &
      & reff , semaj , semin , sum , suma , sumb , sumep , sump , title(96) , tpi , trig1 , trig2 , trigc(4) , trigs(4) , v , v13(3)&
      & , v24(3) , vx(3) , xdetj(4) , xeta(4) , xi , xii(4) , xn(4) , xphi(4) , xx(4) , yy(4) , zz(4)
   LOGICAL anom , onlyar , writ
   INTEGER bgpdt , buf1 , casecc , eqexin , file , geom1 , hugv , i , iacoef , iam , ian , ibcoef , ibg , idx , ieqex , igrid ,     &
         & igrid1 , ihcpot , ij , inext , info(7) , iop , iop1 , ipot , ipt(4) , ipt1 , ipt2 , ipt3 , ipt4 , ipz , isub , isub1 ,   &
         & isump , isym , j , jloc , k , kount , lcore , llcore , m , mcb(7) , mcirc , mlong , ms , msegs , n , n2 , nam(2) , nbg , &
         & ncoefs , ncol , neqex , ngrids , nmharm , nmm , nmp1 , nnharm , nnp1 , npm , nrow , ns , nsegs , nsym , procof , procos ,&
         & prolte(2) , trail(7)
   INTEGER korsz
!
! End of declarations
!
!
!     PROLATE COMPUTES COEFFICIENTS FOR A PROLATE SPHEROIDAL HARMONIC
!     EXPANSION FOR MAGNETOSTATICS PROBLEMS. A PROLATE SPHEROID IA
!     ASSUMED TO ENCLOSE THE FERROMAGNETIC BODY AND ALL MAGNETIC
!     SOURCES. A PROLATE BULK DATA CARD DEFINES THE GRIDS ON HTE SURFACE
!     OF TEH PROLATE SPHEROID, THE NUMBER OF TERMS IN THE SERIES
!     EXPANSION,ETC. CASE CONTROL CARD AXISYM CONTROLS SYMMETRY OR ANTI-
!     SYMMETRY(OR LACK OF) OF THE POTENTIAL W.R.T. THE X-Y PLANE FOR
!     EACH SUBCASE.
!
!     PROLATE  GEOM1,EQEXIN,BGPDT,CASECC,NSLT,HUGV,REMFLD,HEST,MPT,DIT/
!              PROCF
!
!     INTEGER         DIT,REMFLD,MPT
   EQUIVALENCE (Z(1),Iz(1)) , (info(1),fo(1))
   DATA nam/4HPROL , 4HATE /
   DATA procos/302/
   DATA prolte/4101 , 41/
   DATA geom1 , eqexin , bgpdt , casecc/101 , 102 , 103 , 104/
   DATA hugv , procof/106 , 201/
!     DATA    REMFLD, MPT,DIT/107,109,110/
   DATA pt1 , pt2/.211324865 , .788675135/
   DATA inter(1,1) , inter(2,2) , inter(3,3) , inter(4,4)/4*.622008469/
   DATA inter(1,2) , inter(1,4) , inter(2,1) , inter(2,3)/4*.16666667/
   DATA inter(3,2) , inter(3,4) , inter(4,1) , inter(4,3)/4*.16666667/
   DATA inter(1,3) , inter(2,4) , inter(3,1) , inter(4,2)/4*.044658199/
   DATA pi/3.1415927/
!
   tpi = 2.*pi
   writ = .FALSE.
   Nslt = 105
   Hest = 108
   Scr1 = 301
   lcore = korsz(Z)
   llcore = lcore
   buf1 = lcore - Sysbuf
   lcore = buf1 - 1
   IF ( lcore>0 ) THEN
!
      xii(1) = pt1
      xii(2) = pt1
      xii(3) = pt2
      xii(4) = pt2
      etai(1) = pt2
      etai(2) = pt1
      etai(3) = pt1
      etai(4) = pt2
!
!     CHECK TO SEE IF PROLATE CARD EXISTS.IF NOT, WARNING AND OUT
!
      file = geom1
      CALL preloc(*1000,Z(buf1),geom1)
      CALL locate(*100,Z(buf1),prolte,idx)
!
!     THERE IS ONLY ONE PROLAT CARD IN THE DECK-READ IT IN
!
      CALL read(*1100,*200,geom1,Z,lcore,0,ngrids)
   ENDIF
   GOTO 1200
 100  WRITE (Otpe,99001) Sim
99001 FORMAT (A31,', NO PROLAT CARD FOUND')
   CALL close(geom1,1)
   RETURN
 200  CALL close(geom1,1)
   semaj = Z(1)
   j = 2
   semin = Z(j)
   nsegs = Iz(3)
   msegs = Iz(4)
   nnharm = Iz(5)
   nmharm = Iz(6)
   igrid = 6
!
!     CREATE A LIST OF COORDINATES FOR THE GRID POINTS. WE WILL NEED
!     BOTH INTERNAL AND SIL VALUES FOR THE GRIDS.BUT THESE ARE THE SAME
!     IN HEAT TRANSFER. SO READ IN ONLY THE 1ST RECORD OF EQEXIN
!
   Mcore = lcore - ngrids
   ieqex = ngrids
   CALL gopen(eqexin,Z(buf1),0)
   file = eqexin
   CALL read(*1100,*300,eqexin,Z(ieqex+1),Mcore,0,neqex)
   GOTO 1200
 300  CALL close(eqexin,1)
!
!     CREATE A LIST OF INTERNAL VALUES OF THE GRIDS ON PROLAT-CHECK CORE
!
   inext = ieqex + neqex
   IF ( inext+ngrids-6>lcore ) GOTO 1200
!
   igrid1 = igrid + 1
   k = 0
   DO i = igrid1 , ngrids
      k = k + 1
      CALL bisloc(*400,Iz(i),Iz(ieqex+1),2,neqex/2,jloc)
!
!     STORE  THE INTERNAL VALUE
!
      Iz(inext+k) = Iz(ieqex+jloc+1)
   ENDDO
   GOTO 500
 400  WRITE (Otpe,99002) Ufm , Iz(i)
99002 FORMAT (A23,', GRID',I8,' ON PROLAT CARD DOES NOT EXIST')
   CALL mesage(-61,0,0)
!
!     MOVE THIS LIST UP IN CORE  / ALL ELSE IN OPEN CORE IS EXPENDABLE
!
 500  DO i = 1 , k
      Iz(i) = Iz(inext+i)
   ENDDO
   ngrids = ngrids - 6
!
!     CREATE SCRATCH FILE OF HC VALUES FOR EACH REMFLUX CARD(FOR LATER
!     USE IN HC LINE INTEGRALS)
!
   Buf2 = buf1
   Mcore = lcore
   CALL remflx(ngrids)
!
!     NOW PICK UP COORDINATES OF THESE POINTS-OPEN CORE 1-NGRIDS GIVES
!     THE POINTERS
!
   ibg = ngrids
   CALL gopen(bgpdt,Z(buf1),0)
   file = bgpdt
   CALL read(*1100,*600,bgpdt,Z(ibg+1),lcore-ngrids,0,nbg)
   GOTO 1200
 600  CALL close(bgpdt,1)
!
   k = ibg + nbg
   IF ( k+3*ngrids>lcore ) GOTO 1200
   DO i = 1 , ngrids
      ipz = Iz(i)
      isub = 4*(ipz-1) + ibg
      isub1 = isub + 1
      DO j = 1 , 3
         Z(k+j) = Z(isub1+j)
      ENDDO
      k = k + 3
   ENDDO
!
!     MOVE THESE UP IN CORE SO THAT TOTAL WORDS OF OPEN CORE IS NOW
!     4*NGRIDS
!
   k = ibg + nbg
   DO i = 1 , ngrids
      ij = ngrids + 3*(i-1)
      DO j = 1 , 3
         Z(ij+j) = Z(k+j)
      ENDDO
      k = k + 3
   ENDDO
   ibg = ngrids
   iop = 0
   iop1 = 1
!
!     NOW PICK UP POTENTIAL VALUES AY THESE GRIDS
!
   mcb(1) = hugv
   CALL rdtrl(mcb)
   ncol = mcb(2)
   nrow = mcb(3)
   Typout = 1
   Ii = 1
   Nn = nrow
   Incr = 1
   Subcas = 0
 700  inext = 4*ngrids
   ipot = inext
   IF ( inext+nrow+ngrids>lcore ) GOTO 1200
   Subcas = Subcas + 1
   CALL gopen(hugv,Z(buf1),iop)
   CALL unpack(*800,hugv,Z(inext+1))
   CALL close(hugv,2)
!
!     PICK UP POTENTIALS OF ASSOCIATED POINTS
!
   isub = inext + nrow
   DO i = 1 , ngrids
      ipz = Iz(i)
      Z(isub+i) = Z(inext+ipz)
   ENDDO
!
!     MOVE THESE  UP
!
   DO i = 1 , ngrids
      Z(ipot+i) = Z(isub+i)
   ENDDO
   GOTO 900
!
!     ZERO POTENTIALS
!
 800  DO i = 1 , ngrids
      Z(ipot+i) = 0.
   ENDDO
   CALL close(hugv,2)
!
!     OPEN CORE ARRANGEMENT
!
!     1 - NGRIDS                            SIL VALUES
!     NGRIDS + 1 - 4*NGRIDS                 BGPDT VALUES OF THESE POINTS
!     4*NGRIDS + 1 - 5*NGRIDS               ANOMALY POTENTIALS
!     5*NGRIDS + 1 - 5*NGRIDS + NTOT        LOAD INFO IF NEEDED
!     5*NGRIDS + NTOT + 1 - 6*NGRIDS + NTOT HC POTENTIALS
!
!     PICK UP SYMMETRY INDICATOR FOR THIS CASE. 0 MEANS NO SYMMETRY,
!     1(SINE) MEANS ANTI-SYMMETRY, 2(COSINE) MEANS SYMMETRY. 0 IMPLIES
!     360 DEGREE MODELING, 1 AND 2 IMPLY 180 DEGREE MODELING(SKIP SUBCOM
!     REPCASE. IF LOAD=0, THEN BIOT SAVART LOADS ARE ZERO)
!
!     INDICATOR=10,20, OR 30 MEANS ANOMALY ONLY, IE DO NOT INCLUDE
!     EFFECTS OF   APPLIED FIELD(USED MAINLY IN INDUCING FIELDS)
!     IF THIS IS THE CASE, SET ANOM TO TRUE AND GO  BACK TO 0,1,2
!
 900  inext = 5*ngrids
   IF ( inext+136>lcore ) GOTO 1200
   CALL gopen(casecc,Z(buf1),iop)
   DO
      CALL fread(casecc,Z(inext+1),136,1)
      nsym = Iz(inext+16)
      Load = Iz(inext+4)
      isym = Iz(inext+136)
      DO i = 1 , 96
         title(i) = Z(inext+38+i)
      ENDDO
      anom = .FALSE.
      IF ( isym>2 ) THEN
         anom = .TRUE.
         IF ( isym==30 ) isym = 0
         isym = isym/10
      ENDIF
      IF ( nsym==0 ) THEN
         CALL close(casecc,2)
!
!     PROLATE SPHEROID COORDINATE XI IS CONSTANT OVER THE REFERNCE
!     SPHEROID. DFOC=DISTANCE BETWEEN FOCI
!
         inext = 5*ngrids
         dfoc = 2.*sqrt(semaj**2-semin**2)
         xi = 2.*semaj/dfoc
         dxi = 2./dfoc/xi
!
!     IF BIOT-SAVART LOAD ARE ZERO OR ANOMALY ONLY, SKIP HC POTENTIALS
!
         IF ( Load/=0 ) THEN
            IF ( .NOT.(anom) ) THEN
!
!     SET UP LOADS FOR LINE INTEGRAL COMPUTATIONS
!
               Buf2 = buf1
               Mcore = lcore
               Ist = inext
!
               CALL loadsu
!
               inext = inext + Ntot
               IF ( inext+ngrids>lcore ) GOTO 1200
!
!     CHECK CORE FOR BIOTSV
!
               IF ( Remfl .AND. inext+4*ngrids>lcore ) GOTO 1200
!
!     FIRST INTEGRATE THE BIOT-SAVART FIELD ON THE PROLATE SPHEROID TO
!     COME UP WITH AN EQUIVALENT POTENTIAL AT EACH POINT TO BE ADDED TO
!     THE ANOMALY POTENTIAL. STORE THESE POTENTIALS IN 5*NGRIDS+NTOT+1
!     THRU 6*NGRIDS+NTOT
!
!
               DO i = 1 , ngrids
                  Z(inext+i) = 0.
               ENDDO
               ihcpot = inext
               mlong = msegs + 1
               IF ( isym==0 ) mlong = msegs
               mcirc = msegs
               IF ( isym==0 ) mcirc = msegs - 1
!
               DO n = 1 , nsegs
!
                  DO m = 1 , mlong
!
!     INTEGRATE HC LONGITUDINALLY. PERFORM LINE INTEGRAL OF HC.DL
!     RETRIEVE COORDINATES OF POINTS
!
                     ipt1 = (m-1)*(nsegs-1) + 2 + (n-1)
                     ipt2 = ipt1 + 1
                     IF ( n==1 ) ipt1 = 1
                     IF ( n==nsegs ) ipt2 = 2
!
                     isub = 3*(ipt1-1) + ibg
                     X1 = Z(isub+1)
                     Y1 = Z(isub+2)
                     Z1 = Z(isub+3)
                     isub = 3*(ipt2-1) + ibg
                     X2 = Z(isub+1)
                     Y2 = Z(isub+2)
                     Z2 = Z(isub+3)
                     Ng1 = Iz(ipt1)
                     Ng2 = Iz(ipt2)
!
                     CALL linein(X1,Y1,Z1,X2,Y2,Z2,hcdl)
!
!     NOW ADD POTENTIAL FROM 1ST POINT TO INTEGRAL AT 2ND TO GIVE
!     INITIAL POTENTIAL AT 2ND POINT. IF 2ND POINT IS RIGHT END POINT
!     (POINT 2 ON PROLAT), ACCUMULATE FOR AVERAGING
!
                     add = 0.
                     IF ( ipt2==2 ) add = Z(ihcpot+2)
                     Z(ihcpot+ipt2) = Z(ihcpot+ipt1) + hcdl + add
!
!     GET ANOTHER CIRCUMFERENTIAL SEGMENT
!
                  ENDDO
!
!     AVERAGE THE INTEGRALS AT RIGHT END POINT
!
                  Z(ihcpot+2) = Z(ihcpot+2)/float(mlong)
!
!     LONGITUDINAL INTEGRATIONS FOR THIS LONGITUDINAL SEGMENT ARE
!     COMPLETE.
!     NOW INTEGRATE CIRCUMFERENTIALLY DOWN THE RIGHT HAND SIDE OF THE
!     LONGITUDINAL SEGMENT AND AVERGAE WITH THE LONGITUDINAL RESULTS.
!     IF WE ARE AT THE LAST SET OF LONGITUDINAL SEGMENTS, DO NOT DO ANY
!     CIRCUMFERENTIAL INTEGRATIONS SINCE WE HAVE ONLY THE RIGHT END
!     POINT.
!
                  IF ( n/=nsegs ) THEN
                     DO m = 1 , mcirc
                        ipt1 = (m-1)*(nsegs-1) + 2 + n
                        ipt2 = ipt1 + (nsegs-1)
                        isub = 3*(ipt1-1) + ibg
                        X1 = Z(isub+1)
                        Y1 = Z(isub+2)
                        Z1 = Z(isub+3)
                        isub = 3*(ipt2-1) + ibg
                        X2 = Z(isub+1)
                        Y2 = Z(isub+2)
                        Z2 = Z(isub+3)
                        Ng1 = Iz(ipt1)
                        Ng2 = Iz(ipt2)
!
                        CALL linein(X1,Y1,Z1,X2,Y2,Z2,hcdl)
!
!     TO GET FINAL HC POTENTIAL AT 2ND POINT, ADD PRESENT POTENTIAL AT
!     POINT 2(WHICH RESULTED FROM LONGITUDINAL INTEGRATION) TO THE SUM
!     OF THE POTENTIAL AT POINT 1 AND PRESENT INTEGRAL. THEN AVERAGE
!
                        Z(ihcpot+ipt2) = (Z(ihcpot+ipt2)+Z(ihcpot+ipt1)+hcdl)/2.
!
                     ENDDO
                  ENDIF
!
!     GET ANOTHER SET OF LONGITUDINAL SEGMENTS
!
               ENDDO
!
               CALL close(Nslt,1)
!
!     USING THE POTENTIALS JUST COMPUTED, COMPUTE AN AVERAGE REFERNCE
!     POTENTIAL TO BE SUBTRACTED FROM THESE POTENTIALS SO THAT THE
!     AVERAGE POTENTIAL IS ZERO GIVING A ZERO MONOPOLE.
!     (AVERAGE POTENTIAL=(U/AREA)*(INTEGRAL OF PHI*D(AREA))-INTEGRATE
!     OVER EACH SURFACE PATCH. ALSO COMPARE COMPUTED AREA TO ANALYTICAL
!     AREA. IF THIS SUBCASE IS A SINE CASE, THEN AVERGAE IS
!     AUTOMATICALLY ZERO AND WE CAN SKIP THIS.(THE AREA IN THE
!     INTEGRATION IS 4*PI-MORSE+FESCHBACH-PAGES 1265 AND 1285--OR THE
!     A00 TE-M OF THE EXPANSION)
!     THE REASON FOR THE REFERENCE POTENTIAL IS THAT WE MUST ARBITRARILY
!     SET PHI=0 AT SOME POINT AND THEN THEN INTEGRATE TO GET PHIC.
!     REFF COMPENSATES FOR THAT
!
               reff = 0.
            ENDIF
         ENDIF
         onlyar = .FALSE.
         IF ( Load==0 .OR. isym==1 ) onlyar = .TRUE.
         IF ( anom ) onlyar = .TRUE.
!
         sump = 0.
         suma = 0.
         sumep = 0.
         DO n = 1 , nsegs
            DO m = 1 , msegs
!
!     GET THE COORDINATES OF THE 4 CORNERS OF THE PATCH(3 CORNERS IF
!     1ST OR LAST SET OF SEGMENTS)
!
               ipt(1) = (m-1)*(nsegs-1) + 2 + (n-1)
               ipt(2) = ipt(1) + (nsegs-1)
               ipt(3) = ipt(2) + 1
               ipt(4) = ipt(1) + 1
               IF ( m==msegs ) THEN
                  IF ( isym==0 ) THEN
                     ipt(2) = n + 1
                     ipt(3) = ipt(2) + 1
                  ENDIF
               ENDIF
               IF ( n==1 ) THEN
                  ipt(1) = 1
                  ipt(2) = 1
               ELSEIF ( n==nsegs ) THEN
                  ipt(3) = 2
                  ipt(4) = 2
               ENDIF
!
!     COMPITE VECTOR COMPONENTS FOR THE DIAGONALS AND TAKE 1/2 THE CROSS
!     PRODUCT TO GET THE PATCH AREA
!
               DO i = 1 , 4
                  isub = 3*(ipt(i)-1) + ibg
                  xx(i) = Z(isub+1)
                  yy(i) = Z(isub+2)
                  zz(i) = Z(isub+3)
                  xeta(i) = dxi*xx(i)
                  IF ( zz(i)/=0. .OR. yy(i)/=0. ) THEN
                     xphi(i) = atan2(zz(i),yy(i))
                     IF ( xphi(i)<0. ) xphi(i) = xphi(i) + tpi
                  ENDIF
               ENDDO
               IF ( isym==0 .AND. m==msegs ) THEN
                  xphi(2) = tpi
                  xphi(3) = tpi
               ENDIF
               IF ( n==1 ) THEN
                  xphi(1) = xphi(4)
                  xphi(2) = xphi(3)
               ELSEIF ( n==nsegs ) THEN
                  xphi(4) = xphi(1)
                  xphi(3) = xphi(2)
               ENDIF
!
               v13(1) = xx(3) - xx(1)
               v13(2) = yy(3) - yy(1)
               v13(3) = zz(3) - zz(1)
               v24(1) = xx(4) - xx(2)
               v24(2) = yy(4) - yy(2)
               v24(3) = zz(4) - zz(2)
!
               vx(1) = v13(2)*v24(3) - v13(3)*v24(2)
               vx(2) = v13(3)*v24(1) - v13(1)*v24(3)
               vx(3) = v13(1)*v24(2) - v13(2)*v24(1)
!
               area = .5*sqrt(vx(1)**2+vx(2)**2+vx(3)**2)
               areaep = .5*((xeta(4)-xeta(2))*(xphi(1)-xphi(3))-(xeta(1)-xeta(3))*(xphi(4)-xphi(2)))
!
!     FOLLOWING IS BECAUSE OF BACKWARDS DEFINITION OF XPHI
!
               areaep = -areaep
               IF ( .NOT.(onlyar) ) THEN
!
!     PERFORM LINEAR INTERPOLATION OF TEH POTENTIALS FROM THE VERTICES
!     TO THE INTEGRATION POINTS USING ISOPARAMETRIC SHAPE FUNCTIONS AND
!     THEN INTEGRATE. 1ST PICK UP VERTEX POTENTIALS
!
!
                  ipt1 = ipt(1)
                  ipt2 = ipt(2)
                  ipt3 = ipt(3)
                  ipt4 = ipt(4)
                  potv(1) = Z(ihcpot+ipt1)
                  potv(2) = Z(ihcpot+ipt2)
                  potv(3) = Z(ihcpot+ipt3)
                  potv(4) = Z(ihcpot+ipt4)
!
                  DO i = 1 , 4
                     poti(i) = 0.
                     DO j = 1 , 4
                        poti(i) = poti(i) + inter(i,j)*potv(j)
                     ENDDO
                  ENDDO
!
                  sum = 0.
                  DO i = 1 , 4
!
!     COMPUTE DETREMINANT OF JACOBIAN
!
                     j11 = etai(i)*(xeta(4)-xeta(1)) + (1.-etai(i))*(xeta(3)-xeta(2))
                     j12 = etai(i)*(xphi(4)-xphi(1)) + (1.-etai(i))*(xphi(3)-xphi(2))
                     j21 = xii(i)*(xeta(4)-xeta(3)) + (1.-xii(i))*(xeta(1)-xeta(2))
                     j22 = xii(i)*(xphi(4)-xphi(3)) + (1.-xii(i))*(xphi(1)-xphi(2))
                     j12 = -j12
                     j22 = -j22
                     detj = j11*j22 - j12*j21
                     sum = sum + poti(i)*detj*.25
                  ENDDO
!
!     NOTE---  .25 * SUM OF THE 4 DETJ-S EQUALS AREAEP
!
                  sump = sump + sum
               ENDIF
               suma = suma + area
               sumep = sumep + areaep
!
!     GET ANOTHER PATCH
!
            ENDDO
         ENDDO
!
         IF ( suma<=0. ) THEN
            WRITE (Otpe,99003) Ufm
99003       FORMAT (A23,', AREA OF PROLATE SPHEROID IS ZERO')
            CALL mesage(-61,0,0)
         ENDIF
!
!     COMPUTE ANALYTICAL AREA
!
         eps = .5*dfoc/semaj
         area = 2.*pi*(semin**2+semaj*semin*asin(eps)/eps)
         IF ( isym/=0 ) suma = 2.*suma
!
         IF ( .NOT.writ ) WRITE (Otpe,99004) Uim , area , suma
99004    FORMAT (A29,', THE EXACT SURFACE AREA OF THE PROLATE SPHEROID IS',1X,1P,E15.3,',  THE COMPUTED AREA IS ',1P,E15.3)
         writ = .TRUE.
         IF ( Load/=0 ) THEN
            IF ( .NOT.(anom) ) THEN
!
!     GET REFERNCE POTENTIAL AND SUBTRACT FROM SUM OF ANOMALY AND HC
!     POTENTI
!
               IF ( isym/=1 ) reff = sump/sumep
               DO i = 1 , ngrids
                  Z(ipot+i) = Z(ipot+i) + Z(ihcpot+i) - reff
               ENDDO
            ENDIF
         ENDIF
!
!     FINALLY NOW WE CAN COMPUTE THE COEFFICIENTS A(M,N) AND B(M,N).
!     MORSE ABD FESCHBACH P. 1285--CHAECK FOR ENOUGH OPEN VORE SPACE TO
!     STORE THE A-S AND B-S. FOR EACH TYPE, THE NUMBER OF COEFFICIENTS
!     IS THE SUM OF TH+ INTEGERS FROM 1 TO (\+1), UNLESS M HAS A MAXIMUM
!     LESS THAN N, IN WHICH CASE, THE COUNT IS (M+1)*(N+1-M)+SUM OF
!     INTEGERS FROM 1 TO M. THE COEFFICIENTS WE NEED ARE
!
!              M=0       M=1       M=2       M=4       ETC
!        N=0   A00
!        N=1   A01       A11
!        N=2   A02       A12       A22
!        N=3   A03       A13       A23       A33
!         .
!        ETC
!
!     WE NO LONGER NEED THE HC POTENTIALS OR LOAD INFO. SO STORE
!     COEFFICIENTS STARTING AT 5*NGRIDS+1
!
         iacoef = ipot + ngrids
         ncoefs = ((nnharm+1)*(nnharm+2))/2
         IF ( nmharm<nnharm ) ncoefs = (nmharm+1)*(nnharm+1-nmharm) + (nmharm*(nmharm+1))/2
         ibcoef = iacoef + ncoefs
         IF ( ibcoef+ncoefs>lcore ) GOTO 1200
         n2 = 2*ncoefs
         DO i = 1 , n2
            Z(iacoef+i) = 0.
         ENDDO
!
!     START THE INTEGRATIONS - FOR EACH PATCH IN TURN, DO ALL THE M-S
!     AND N-
!
         DO ns = 1 , nsegs
            DO ms = 1 , msegs
!
!     INITIAL PART IS SAME AS FOR REFERNEC POTENTIAL
!
               ipt(1) = (ms-1)*(nsegs-1) + 2 + (ns-1)
               ipt(2) = ipt(1) + (nsegs-1)
               ipt(3) = ipt(2) + 1
               ipt(4) = ipt(1) + 1
               IF ( ms==msegs ) THEN
                  IF ( isym==0 ) THEN
                     ipt(2) = ns + 1
                     ipt(3) = ipt(2) + 1
                  ENDIF
               ENDIF
               IF ( ns==1 ) THEN
                  ipt(1) = 1
                  ipt(2) = 1
               ELSEIF ( ns==nsegs ) THEN
                  ipt(3) = 2
                  ipt(4) = 2
               ENDIF
               DO i = 1 , 4
                  isub = 3*(ipt(i)-1) + ibg
                  xx(i) = Z(isub+1)
                  yy(i) = Z(isub+2)
                  zz(i) = Z(isub+3)
                  xeta(i) = dxi*xx(i)
                  IF ( zz(i)/=0. .OR. yy(i)/=0. ) THEN
                     xphi(i) = atan2(zz(i),yy(i))
                     IF ( xphi(i)<0. ) xphi(i) = xphi(i) + tpi
                  ENDIF
               ENDDO
               IF ( isym==0 .AND. ms==msegs ) THEN
                  xphi(2) = tpi
                  xphi(3) = tpi
               ENDIF
               IF ( ns==1 ) THEN
                  xphi(1) = xphi(4)
                  xphi(2) = xphi(3)
               ELSEIF ( ns==nsegs ) THEN
                  xphi(4) = xphi(1)
                  xphi(3) = xphi(2)
               ENDIF
!
!     GET POTENTAILS AT VERTICES
!
               DO i = 1 , 4
                  isub = ipt(i)
                  potv(i) = Z(ipot+isub)
               ENDDO
!
!     INTERPOLATE TO GET POTENTIALS AT EACH INTEGRATION POINT
!
               DO i = 1 , 4
                  poti(i) = 0.
                  DO j = 1 , 4
                     poti(i) = poti(i) + inter(i,j)*potv(j)
                  ENDDO
               ENDDO
!
!     SAVE JACOBIAN DETERMINA5TS AT THE INTEGRATION POINTS
!
               DO i = 1 , 4
                  j11 = etai(i)*(xeta(4)-xeta(1)) + (1.-etai(i))*(xeta(3)-xeta(2))
                  j12 = etai(i)*(xphi(4)-xphi(1)) + (1.-etai(i))*(xphi(3)-xphi(2))
                  j21 = xii(i)*(xeta(4)-xeta(3)) + (1.-xii(i))*(xeta(1)-xeta(2))
                  j22 = xii(i)*(xphi(4)-xphi(3)) + (1.-xii(i))*(xphi(1)-xphi(2))
!
!     BECAUSE OF MY INCONSISTENCY IN DIRECTIONS BETWEEN PROLATE SPHEROID
!     COORDINATES IN ANGLE DIRECTION AND ISOPARAMETRIC COORDINATES IN
!     THAT DIRECTION, WE MUST SWITCH SIGNS FOR XPHI DIFFERENCES- OR ELSE
!     WE WE WILL GET NEGATIVE AREAS
!
                  j12 = -j12
                  j22 = -j22
!
                  xdetj(i) = j11*j22 - j12*j21
               ENDDO
!
!     COMPUTE 4 (ETA,PHI) COORDINATES AT THE INTEGRATION POINTS. USE
!     SHAPE FUNCTIONS FOR UNIT SQUARE. (ETAINT AND PHIINT ARE PROLATE
!     SPHEROIDAL COORDINATES AT INTEGRATION POINTS. XETA,XHPI ARE
!     PROLATE SPHEROIDAL COORDS. AT VERTICES. XII,ETAI ARE ISOPARAMETRIC
!     COORDS AT INTEGRATION POINTS FOR UNIT ISOPARAMEQRIC SPUARE.
!
               DO i = 1 , 4
                  xn(1) = (1.-xii(i))*etai(i)
                  xn(2) = (1.-xii(i))*(1.-etai(i))
                  xn(3) = xii(i)*(1.-etai(i))
                  xn(4) = xii(i)*etai(i)
                  etaint(i) = 0.
                  phiint(i) = 0.
                  DO j = 1 , 4
                     etaint(i) = etaint(i) + xn(j)*xeta(j)
                     phiint(i) = phiint(i) + xn(j)*xphi(j)
                  ENDDO
               ENDDO
!
!     START ACTUAL INTEGRATION FOR A GIVEN N,M
!
               kount = 0
               nnp1 = nnharm + 1
               DO n = 1 , nnp1
                  ian = n - 1
                  cn = ian
!
!     SINCE M SUMMATION GOES ONLY TO N, COMPUTE MIN(N,NNHARM)
!
                  nmp1 = nmharm + 1
                  IF ( nmp1>n ) nmp1 = n
!
                  DO m = 1 , nmp1
                     iam = m - 1
                     cm = iam
                     kount = kount + 1
!
!     COMPUTE ASSOCIATED LEGENDRE FUNCTION OF 1ST KIND AT EAC
!     INTEGRATION POINT
!
                     DO i = 1 , 4
                        CALL pnm(iam,ian,etaint(i),0,pnmv(i))
                     ENDDO
!
!     COMPUTE TRIG FUNCTION AT EAC INTEGRATION POINT
!
                     DO i = 1 , 4
                        ang = cm*phiint(i)
                        trigs(i) = sin(ang)
                        trigc(i) = cos(ang)
                     ENDDO
!
                     suma = 0.
                     sumb = 0.
                     DO i = 1 , 4
                        IF ( isym==0 .OR. isym==1 ) sumb = sumb + trigs(i)*pnmv(i)*poti(i)*xdetj(i)*.25
                        IF ( isym==0 .OR. isym==2 ) suma = suma + trigc(i)*pnmv(i)*poti(i)*xdetj(i)*.25
                     ENDDO
!
!     NOW FORM MULTIPLICATICE CONSTANT BASED ON N,M
!
                     em = 1.
                     IF ( iam>0 ) em = 2.
!
!     ADJUST EM FOR 1/2 MODEL IF NECESSARY
!
                     IF ( isym>0 ) em = 2.*em
!
!     COMPUTE FACTORIALS
!
                     nmm = ian - iam
                     IF ( nmm/=0 ) THEN
                        fnum = 1.
                        c = 1.
                        DO i = 1 , nmm
                           fnum = fnum*c
                           c = c + 1.
                        ENDDO
                     ELSE
                        fnum = 1.
                     ENDIF
                     npm = ian + iam
                     IF ( npm/=0 ) THEN
                        fden = 1.
                        c = 1.
                        DO i = 1 , npm
                           fden = fden*c
                           c = c + 1.
                        ENDDO
                     ELSE
                        fden = 1.
                     ENDIF
                     con = em*(2.*cn+1.)*fnum/fden/4./pi
!
                     suma = suma*con
                     sumb = sumb*con
!
!     STORE THE COEFFICIENTS
!
                     Z(iacoef+kount) = suma + Z(iacoef+kount)
                     Z(ibcoef+kount) = sumb + Z(ibcoef+kount)
!
!     GET ANOTHER N OR M
!
                  ENDDO
               ENDDO
!
!     GET ANOTHER AREA PATCH
!
            ENDDO
         ENDDO
!
!     DONE - THE SCARATCH DATA BLOCK PROCOS WILL HAVE 5 RECORDS FOR EACH
!     SUBCASE. 1ST IS 7 WORD INFO ARRAY, 2ND IS A(M,N) 3RD IS B(M,N)
!     4TH IS POTENTIALS ON SURFACE FROM ANOMALY+HC POTENTIALS-REFF,
!     5TH IS POTENTAILS ON SURFACE USING EXPANSION(WHICH WE WILL DO NOW)
!
         isump = ibcoef + ncoefs
         IF ( isump+ngrids>lcore ) GOTO 1200
!
         DO i = 1 , ngrids
!
!     PICK UP COORDINATES OF POINT
!
            isub = 3*(i-1) + ibg
            X1 = Z(isub+1)
            Y1 = Z(isub+2)
            Z1 = Z(isub+3)
!
!     COMPUTE PROLATE SPHEROIDAL COORDINATES
!
            eta = dxi*X1
            phi = 0.
            IF ( Z1/=0. .OR. Y1/=0. ) phi = atan2(Z1,Y1)
!
!     START SUMMATION
!
            kount = 0
            sum = 0.
            DO n = 1 , nnp1
               ian = n - 1
               cn = ian
               nmp1 = nmharm + 1
               IF ( nmp1>n ) nmp1 = n
               DO m = 1 , nmp1
                  iam = m - 1
                  cm = iam
                  kount = kount + 1
!
!     GET LEGENDRE AND TRIG FUNCTIONS
!
                  CALL pnm(iam,ian,eta,0,v)
                  ang = cm*phi
                  trig1 = cos(ang)
                  trig2 = sin(ang)
                  ab = 0.
                  IF ( isym==0 .OR. isym==1 ) ab = ab + Z(ibcoef+kount)*trig2
                  IF ( isym==0 .OR. isym==2 ) ab = ab + Z(iacoef+kount)*trig1
!
                  sum = sum + ab*v
               ENDDO
            ENDDO
!
!     STORE VALUE
!
            Z(isump+i) = sum
!
!     GET ANOTHER POINT
!
         ENDDO
!
!     WRITE RESULTS TO PROCOS
!
         fo(1) = semaj
         fo(2) = semin
         info(3) = nnharm
         info(4) = nmharm
         info(5) = ncoefs
         info(6) = isym
         info(7) = ngrids
         CALL gopen(procos,Z(buf1),iop1)
         CALL write(procos,info,7,0)
         CALL write(procos,title,96,1)
         CALL write(procos,Z(iacoef+1),ncoefs,1)
         CALL write(procos,Z(ibcoef+1),ncoefs,1)
         CALL write(procos,Z(ipot+1),ngrids,1)
         CALL write(procos,Z(isump+1),ngrids,1)
         CALL close(procos,2)
!
!     NOW THAT WE ARE FINISHED ALL THIS WORK, WE SHOULD SEE IF THERE
!     ARE OTHER SUBCASES WE MUST DO IT FOR
!
         IF ( Subcas>=ncol ) THEN
!
!     DONE
!
            trail(1) = procos
            trail(2) = Subcas
            DO i = 3 , 7
               trail(i) = 0
            ENDDO
            CALL wrttrl(trail)
!
!     CHECK FOR SUBCOMS AND REPCASES AND WRITE ( TO OUTPUT FILE
!
            CALL procom(procos,procof,casecc,ncoefs,ngrids)
!
            RETURN
         ELSE
            iop = 2
            iop1 = 3
            GOTO 700
         ENDIF
      ENDIF
   ENDDO
!
 1000 n = -1
   GOTO 1300
 1100 n = -2
   GOTO 1300
 1200 n = -8
   file = 0
 1300 CALL mesage(n,file,nam)
END SUBROUTINE prolat
