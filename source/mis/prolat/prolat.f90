!*==prolat.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE prolat
   USE c_biot
   USE c_packx
   USE c_system
   USE c_unpakx
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: ab , add , ang , area , areaep , c , cm , cn , con , detj , dfoc , dxi , em , eps , eta , fden , fnum , hcdl , j11 ,     &
         & j12 , j21 , j22 , phi , reff , semaj , semin , sum , suma , sumb , sumep , sump , tpi , trig1 , trig2 , v , xi
   LOGICAL :: anom , onlyar , writ
   INTEGER , SAVE :: bgpdt , casecc , eqexin , geom1 , hugv , procof , procos
   INTEGER :: buf1 , file , i , iacoef , iam , ian , ibcoef , ibg , idx , ieqex , igrid , igrid1 , ihcpot , ij , inext , iop ,      &
            & iop1 , ipot , ipt1 , ipt2 , ipt3 , ipt4 , ipz , isub , isub1 , isump , isym , j , jloc , k , kount , lcore , llcore , &
            & m , mcirc , mlong , ms , msegs , n , n2 , nbg , ncoefs , ncol , neqex , ngrids , nmharm , nmm , nmp1 , nnharm , nnp1 ,&
            & npm , nrow , ns , nsegs , nsym
   REAL , DIMENSION(4) :: etai , etaint , phiint , pnmv , poti , potv , trigc , trigs , xdetj , xeta , xii , xn , xphi , xx , yy ,  &
                        & zz
   REAL , DIMENSION(7) :: fo
   INTEGER , DIMENSION(7) :: info , mcb , trail
   REAL , DIMENSION(4,4) , SAVE :: inter
   INTEGER , DIMENSION(4) :: ipt
   INTEGER , DIMENSION(6) :: iz
   INTEGER , DIMENSION(2) , SAVE :: nam , prolte
   REAL , SAVE :: pi , pt1 , pt2
   REAL , DIMENSION(96) :: title
   REAL , DIMENSION(3) :: v13 , v24 , vx
   EXTERNAL bisloc , close , fread , gopen , korsz , linein , loadsu , locate , mesage , pnm , preloc , procom , rdtrl , read ,     &
          & remflx , unpack , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   !>>>>EQUIVALENCE (Z(1),Iz(1)) , (info(1),fo(1))
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         tpi = 2.*pi
         writ = .FALSE.
         nslt = 105
         hest = 108
         scr1 = 301
         lcore = korsz(z)
         llcore = lcore
         buf1 = lcore - sysbuf
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
            CALL preloc(*140,z(buf1),geom1)
            CALL locate(*20,z(buf1),prolte,idx)
!
!     THERE IS ONLY ONE PROLAT CARD IN THE DECK-READ IT IN
!
            CALL read(*160,*40,geom1,z,lcore,0,ngrids)
         ENDIF
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 20      WRITE (otpe,99001) sim
99001    FORMAT (A31,', NO PROLAT CARD FOUND')
         CALL close(geom1,1)
         RETURN
 40      CALL close(geom1,1)
         semaj = z(1)
         j = 2
         semin = z(j)
         nsegs = iz(3)
         msegs = iz(4)
         nnharm = iz(5)
         nmharm = iz(6)
         igrid = 6
!
!     CREATE A LIST OF COORDINATES FOR THE GRID POINTS. WE WILL NEED
!     BOTH INTERNAL AND SIL VALUES FOR THE GRIDS.BUT THESE ARE THE SAME
!     IN HEAT TRANSFER. SO READ IN ONLY THE 1ST RECORD OF EQEXIN
!
         mcore = lcore - ngrids
         ieqex = ngrids
         CALL gopen(eqexin,z(buf1),0)
         file = eqexin
         CALL read(*160,*60,eqexin,z(ieqex+1),mcore,0,neqex)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 60      CALL close(eqexin,1)
!
!     CREATE A LIST OF INTERNAL VALUES OF THE GRIDS ON PROLAT-CHECK CORE
!
         inext = ieqex + neqex
         IF ( inext+ngrids-6>lcore ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
         igrid1 = igrid + 1
         k = 0
         DO i = igrid1 , ngrids
            k = k + 1
            CALL bisloc(*80,iz(i),iz(ieqex+1),2,neqex/2,jloc)
!
!     STORE  THE INTERNAL VALUE
!
            iz(inext+k) = iz(ieqex+jloc+1)
         ENDDO
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 80      WRITE (otpe,99002) ufm , iz(i)
99002    FORMAT (A23,', GRID',I8,' ON PROLAT CARD DOES NOT EXIST')
         CALL mesage(-61,0,0)
         spag_nextblock_1 = 2
      CASE (2)
!
!     MOVE THIS LIST UP IN CORE  / ALL ELSE IN OPEN CORE IS EXPENDABLE
!
         DO i = 1 , k
            iz(i) = iz(inext+i)
         ENDDO
         ngrids = ngrids - 6
!
!     CREATE SCRATCH FILE OF HC VALUES FOR EACH REMFLUX CARD(FOR LATER
!     USE IN HC LINE INTEGRALS)
!
         buf2 = buf1
         mcore = lcore
         CALL remflx(ngrids)
!
!     NOW PICK UP COORDINATES OF THESE POINTS-OPEN CORE 1-NGRIDS GIVES
!     THE POINTERS
!
         ibg = ngrids
         CALL gopen(bgpdt,z(buf1),0)
         file = bgpdt
         CALL read(*160,*100,bgpdt,z(ibg+1),lcore-ngrids,0,nbg)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 100     CALL close(bgpdt,1)
!
         k = ibg + nbg
         IF ( k+3*ngrids>lcore ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO i = 1 , ngrids
            ipz = iz(i)
            isub = 4*(ipz-1) + ibg
            isub1 = isub + 1
            DO j = 1 , 3
               z(k+j) = z(isub1+j)
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
               z(ij+j) = z(k+j)
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
         typout = 1
         ii = 1
         nn = nrow
         incr = 1
         subcas = 0
         spag_nextblock_1 = 3
      CASE (3)
         inext = 4*ngrids
         ipot = inext
         IF ( inext+nrow+ngrids>lcore ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         subcas = subcas + 1
         CALL gopen(hugv,z(buf1),iop)
         CALL unpack(*120,hugv,z(inext+1))
         CALL close(hugv,2)
!
!     PICK UP POTENTIALS OF ASSOCIATED POINTS
!
         isub = inext + nrow
         DO i = 1 , ngrids
            ipz = iz(i)
            z(isub+i) = z(inext+ipz)
         ENDDO
!
!     MOVE THESE  UP
!
         DO i = 1 , ngrids
            z(ipot+i) = z(isub+i)
         ENDDO
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
!
!     ZERO POTENTIALS
!
 120     DO i = 1 , ngrids
            z(ipot+i) = 0.
         ENDDO
         CALL close(hugv,2)
         spag_nextblock_1 = 4
      CASE (4)
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
         inext = 5*ngrids
         IF ( inext+136>lcore ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL gopen(casecc,z(buf1),iop)
         DO
            CALL fread(casecc,z(inext+1),136,1)
            nsym = iz(inext+16)
            load = iz(inext+4)
            isym = iz(inext+136)
            DO i = 1 , 96
               title(i) = z(inext+38+i)
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
               IF ( load/=0 ) THEN
                  IF ( .NOT.(anom) ) THEN
!
!     SET UP LOADS FOR LINE INTEGRAL COMPUTATIONS
!
                     buf2 = buf1
                     mcore = lcore
                     ist = inext
!
                     CALL loadsu
!
                     inext = inext + ntot
                     IF ( inext+ngrids>lcore ) THEN
                        spag_nextblock_1 = 5
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
!
!     CHECK CORE FOR BIOTSV
!
                     IF ( remfl .AND. inext+4*ngrids>lcore ) THEN
                        spag_nextblock_1 = 5
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
!
!     FIRST INTEGRATE THE BIOT-SAVART FIELD ON THE PROLATE SPHEROID TO
!     COME UP WITH AN EQUIVALENT POTENTIAL AT EACH POINT TO BE ADDED TO
!     THE ANOMALY POTENTIAL. STORE THESE POTENTIALS IN 5*NGRIDS+NTOT+1
!     THRU 6*NGRIDS+NTOT
!
!
                     DO i = 1 , ngrids
                        z(inext+i) = 0.
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
                           x1 = z(isub+1)
                           y1 = z(isub+2)
                           z1 = z(isub+3)
                           isub = 3*(ipt2-1) + ibg
                           x2 = z(isub+1)
                           y2 = z(isub+2)
                           z2 = z(isub+3)
                           ng1 = iz(ipt1)
                           ng2 = iz(ipt2)
!
                           CALL linein(x1,y1,z1,x2,y2,z2,hcdl)
!
!     NOW ADD POTENTIAL FROM 1ST POINT TO INTEGRAL AT 2ND TO GIVE
!     INITIAL POTENTIAL AT 2ND POINT. IF 2ND POINT IS RIGHT END POINT
!     (POINT 2 ON PROLAT), ACCUMULATE FOR AVERAGING
!
                           add = 0.
                           IF ( ipt2==2 ) add = z(ihcpot+2)
                           z(ihcpot+ipt2) = z(ihcpot+ipt1) + hcdl + add
!
!     GET ANOTHER CIRCUMFERENTIAL SEGMENT
!
                        ENDDO
!
!     AVERAGE THE INTEGRALS AT RIGHT END POINT
!
                        z(ihcpot+2) = z(ihcpot+2)/float(mlong)
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
                              x1 = z(isub+1)
                              y1 = z(isub+2)
                              z1 = z(isub+3)
                              isub = 3*(ipt2-1) + ibg
                              x2 = z(isub+1)
                              y2 = z(isub+2)
                              z2 = z(isub+3)
                              ng1 = iz(ipt1)
                              ng2 = iz(ipt2)
!
                              CALL linein(x1,y1,z1,x2,y2,z2,hcdl)
!
!     TO GET FINAL HC POTENTIAL AT 2ND POINT, ADD PRESENT POTENTIAL AT
!     POINT 2(WHICH RESULTED FROM LONGITUDINAL INTEGRATION) TO THE SUM
!     OF THE POTENTIAL AT POINT 1 AND PRESENT INTEGRAL. THEN AVERAGE
!
                              z(ihcpot+ipt2) = (z(ihcpot+ipt2)+z(ihcpot+ipt1)+hcdl)/2.
!
                           ENDDO
                        ENDIF
!
!     GET ANOTHER SET OF LONGITUDINAL SEGMENTS
!
                     ENDDO
!
                     CALL close(nslt,1)
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
               IF ( load==0 .OR. isym==1 ) onlyar = .TRUE.
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
                        xx(i) = z(isub+1)
                        yy(i) = z(isub+2)
                        zz(i) = z(isub+3)
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
                        potv(1) = z(ihcpot+ipt1)
                        potv(2) = z(ihcpot+ipt2)
                        potv(3) = z(ihcpot+ipt3)
                        potv(4) = z(ihcpot+ipt4)
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
                  WRITE (otpe,99003) ufm
99003             FORMAT (A23,', AREA OF PROLATE SPHEROID IS ZERO')
                  CALL mesage(-61,0,0)
               ENDIF
!
!     COMPUTE ANALYTICAL AREA
!
               eps = .5*dfoc/semaj
               area = 2.*pi*(semin**2+semaj*semin*asin(eps)/eps)
               IF ( isym/=0 ) suma = 2.*suma
!
               IF ( .NOT.writ ) WRITE (otpe,99004) uim , area , suma
99004          FORMAT (A29,', THE EXACT SURFACE AREA OF THE PROLATE SPHEROID IS',1X,1P,E15.3,',  THE COMPUTED AREA IS ',1P,E15.3)
               writ = .TRUE.
               IF ( load/=0 ) THEN
                  IF ( .NOT.(anom) ) THEN
!
!     GET REFERNCE POTENTIAL AND SUBTRACT FROM SUM OF ANOMALY AND HC
!     POTENTI
!
                     IF ( isym/=1 ) reff = sump/sumep
                     DO i = 1 , ngrids
                        z(ipot+i) = z(ipot+i) + z(ihcpot+i) - reff
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
               IF ( ibcoef+ncoefs>lcore ) THEN
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               n2 = 2*ncoefs
               DO i = 1 , n2
                  z(iacoef+i) = 0.
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
                        xx(i) = z(isub+1)
                        yy(i) = z(isub+2)
                        zz(i) = z(isub+3)
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
                        potv(i) = z(ipot+isub)
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
                           z(iacoef+kount) = suma + z(iacoef+kount)
                           z(ibcoef+kount) = sumb + z(ibcoef+kount)
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
               IF ( isump+ngrids>lcore ) THEN
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!
               DO i = 1 , ngrids
!
!     PICK UP COORDINATES OF POINT
!
                  isub = 3*(i-1) + ibg
                  x1 = z(isub+1)
                  y1 = z(isub+2)
                  z1 = z(isub+3)
!
!     COMPUTE PROLATE SPHEROIDAL COORDINATES
!
                  eta = dxi*x1
                  phi = 0.
                  IF ( z1/=0. .OR. y1/=0. ) phi = atan2(z1,y1)
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
                        IF ( isym==0 .OR. isym==1 ) ab = ab + z(ibcoef+kount)*trig2
                        IF ( isym==0 .OR. isym==2 ) ab = ab + z(iacoef+kount)*trig1
!
                        sum = sum + ab*v
                     ENDDO
                  ENDDO
!
!     STORE VALUE
!
                  z(isump+i) = sum
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
               CALL gopen(procos,z(buf1),iop1)
               CALL write(procos,info,7,0)
               CALL write(procos,title,96,1)
               CALL write(procos,z(iacoef+1),ncoefs,1)
               CALL write(procos,z(ibcoef+1),ncoefs,1)
               CALL write(procos,z(ipot+1),ngrids,1)
               CALL write(procos,z(isump+1),ngrids,1)
               CALL close(procos,2)
!
!     NOW THAT WE ARE FINISHED ALL THIS WORK, WE SHOULD SEE IF THERE
!     ARE OTHER SUBCASES WE MUST DO IT FOR
!
               IF ( subcas>=ncol ) THEN
!
!     DONE
!
                  trail(1) = procos
                  trail(2) = subcas
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
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDDO
!
 140     n = -1
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 160     n = -2
         spag_nextblock_1 = 6
      CASE (5)
         n = -8
         file = 0
         spag_nextblock_1 = 6
      CASE (6)
         CALL mesage(n,file,nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE prolat
