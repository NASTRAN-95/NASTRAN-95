!*==tria3s.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE tria3s
   USE c_emgdic
   USE c_emgest
   USE c_emgprm
   USE c_hmtout
   USE c_matin
   USE c_system
   USE c_terms
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: adamp , area , avgthk , determ , detjac , dheat , dvol , elth , gsube , htcp , jog , jok , k11 , k22 , lx , ly , mominr ,&
         & nsm , offset , reali , rho , tempel , th , thetam , ts , tsi , tsm , tsub0 , weight , weitc , wtmass , wtstif , xmasso , &
         & zoff , zoff1
   REAL , DIMENSION(18) :: aic , htflx
   REAL , DIMENSION(1) :: akgg , alpha , amgg
   REAL , DIMENSION(3) :: bdum , cente , dgpth , edglen , gpth , shpt
   REAL , DIMENSION(4,3) :: bgpdt , egpdt , epnorm , gpnorm
   REAL , DIMENSION(486) :: bmat1
   REAL , DIMENSION(162) :: bmatrx
   REAL , DIMENSION(54) :: bmtrx
   REAL , DIMENSION(6) :: bterms
   INTEGER , DIMENSION(11) :: dict
   INTEGER , SAVE :: dmat , hunmeg , kmat , mmat
   REAL , DIMENSION(4) :: ecpt , egnor
   REAL , SAVE :: eps
   REAL , DIMENSION(9,9) :: g
   REAL , DIMENSION(36) :: gi , htcap , htcon , tmptrn
   INTEGER :: i , i3 , iec , ieoe , ierr , ig , ig1 , ig2 , ig3 , ig4 , ii , ijk , int , ip , ipoint , ipt , ising , ix , j ,       &
            & jcored , jend , jg , jj , jpoint , k , kk , kpt , length , mcsid , nd2 , nd6 , nd7 , nd8 , nd9 , ndof , ndof33 ,      &
            & ndof66 , ndofp1 , nnod2 , nnode , npart
   INTEGER , DIMENSION(4,3) :: igpdt
   INTEGER , DIMENSION(3,3) :: index
   INTEGER , DIMENSION(3) :: iorder , sil
   INTEGER , DIMENSION(4) :: mid , necpt
   INTEGER , DIMENSION(2) , SAVE :: name
   LOGICAL :: needk , needm , noalfa , sheart
   INTEGER , DIMENSION(39) :: nest
   REAL , DIMENSION(9) :: tbg , teb , tem , teu , tub , tum , xmass , zz
   REAL , DIMENSION(324) :: tottrn , transk
   REAL , DIMENSION(27) :: trans
   EXTERNAL emgout , gmmats , hmat , invers , mesage , mpya3s , shcsgs , shgmgs , t3bgbs , t3bmgs , t3gems , t3sets , tldrs , transs
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     SINGLE PRECISION ROUTINE TO FORM STIFFNESS, MASS, AND DAMPING
!     MATRICES FOR THE CTRIA3 ELEMENT
!
!                 EST  LISTING
!
!        WORD     TYP       DESCRIPTION
!     ----------------------------------------------------------------
!     ECT:
!         1        I   ELEMENT ID, EID
!         2-4      I   SIL LIST, GRIDS 1,2,3
!         5-7      R   MEMBRANE THICKNESSES T, AT GRIDS 1,2,3
!         8        R   MATERIAL PROPERTY ORIENTAION ANGLE, THETA
!               OR I   COORD. SYSTEM ID (SEE TM ON CTRIA3 CARD)
!         9        I   TYPE FLAG FOR WORD 8
!        10        R   GRID OFFSET, ZOFF
!    EPT:
!        11        I   MATERIAL ID FOR MEMBRANE, MID1
!        12        R   ELEMENT THICKNESS,T (MEMBRANE, UNIFORMED)
!        13        I   MATERIAL ID FOR BENDING, MID2
!        14        R   MOMENT OF INERTIA FACTOR, I (BENDING)
!        15        I   MATERIAL ID FOR TRANSVERSE SHEAR, MID3
!        16        R   TRANSV. SHEAR CORRECTION FACTOR, TS/T
!        17        R   NON-STRUCTURAL MASS, NSM
!        18-19     R   STRESS FIBER DISTANCES, Z1,Z2
!        20        I   MATERIAL ID FOR MEMBRANE-BENDING COUPLING, MID4
!        21        R   MATERIAL ANGLE OF ROTATION, THETA
!               OR I   COORD. SYSTEM ID (SEE MCSID ON PSHELL CARD)
!                      (DEFAULT FOR WORD 8)
!        22        I   TYPE FLAG FOR WORD 21 (DEFAULT FOR WORD 9)
!        23        I   INTEGRATION ORDER FLAG
!        24        R   STRESS ANGLE OF RATATION, THETA
!               OR I   COORD. SYSTEM ID (SEE SCSID ON PSHELL CARD)
!        25        I   TYPE FLAG FOR WORD 24
!        26        R   OFFSET, ZOFF1 (DEFAULT FOR WORD 10)
!    BGPDT:
!        27-38   I/R   CID,X,Y,Z  FOR GRIDS 1,2,3
!    ETT:
!        39        I   ELEMENT TEMPERATURE
!
!
   !>>>>EQUIVALENCE (Est(1),Nest(1)) , (Est(2),Sil(1)) , (Est(5),Gpth(1)) , (Est(10),Zoff) , (Est(12),Elth) , (Est(17),Nsm) ,            &
!>>>>    & (Est(23),Int) , (Est(26),Zoff1) , (Est(27),Bgpdt(1,1),Igpdt(1,1)) , (Est(39),Tempel) , (dict(5),adamp) , (necpt(1),ecpt(1)) , &
!>>>>    & (Z(1),Amgg(1),Akgg(1)) , (Kheat(4),Htcp) , (htcap(1),xmass(1))
   DATA hunmeg , eps/100000000 , 1.0E-7/
   DATA name , kmat , mmat , dmat/4HCTRI , 4HA3   , 1 , 2 , 3/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     INITIALIZE
!
         elid = nest(1)
         nnode = 3
         mominr = 0.0
         ts = 0.0
         weight = 1.0/6.0
         eltemp = tempel
         needk = kgg1/=0 .OR. ibgg1/=0
         noalfa = .TRUE.
         sheart = .TRUE.
         ieoe = 1
         offset = zoff
         IF ( zoff==0.0 ) offset = zoff1
!
!     CHECK FOR SUFFICIENT OPEN CORE FOR ELEMENT STIFFNESS
!
!     OPEN CORE BEGINS AT JCORE
!     OPEN CORE ENDS   AT NCORE
!     LENGTH OF AVAILABLE WORDS = (NCORE-JCORE-1)/PREC
!
         jcored = jcore/prec + 1
         length = (ncore-jcore-1)/prec
         IF ( length<324 .AND. (.NOT.heat .AND. needk) ) THEN
!
!
!     FATAL ERRORS
!
!     INSUFFICIENT MEMORY IS AVAILABLE
!
            CALL mesage(-30,228,name)
         ELSE
!
!     SET UP THE ELEMENT FORMULATION
!
            CALL t3sets(ierr,sil,igpdt,elth,gpth,dgpth,egpdt,gpnorm,epnorm,iorder,teb,tub,cente,avgthk,lx,ly,edglen,elid)
            IF ( ierr==0 ) THEN
               CALL gmmats(teb,3,3,0,tub,3,3,1,teu)
               area = lx*ly/2.0
!
!     SET THE NUMBER OF DOF'S
!
               nnod2 = nnode*nnode
               ndof = nnode*6
               npart = ndof*ndof
               nd2 = ndof*2
               nd6 = ndof*6
               nd7 = ndof*7
               nd8 = ndof*8
               nd9 = ndof*9
               jend = jcored + npart - 1
!
!     OBTAIN MATERIAL INFORMATION
!
!     PASS THE LOCATION OF THE ELEMENT CENTER FOR MATERIAL
!     TRANSFORMATIONS.
!
               DO iec = 2 , 4
                  ecpt(iec) = cente(iec-1)
               ENDDO
!
!     SET MATERIAL FLAGS
!     5.0/6.0 = 0.833333333
!
               IF ( nest(13)/=0 ) mominr = est(14)
               IF ( nest(13)/=0 ) ts = est(16)
               IF ( est(16)==0.0 ) ts = 0.83333333
               IF ( nest(13)==0 .AND. nest(11)>hunmeg ) ts = 0.833333333
!
               mid(1) = nest(11)
               mid(2) = nest(13)
               mid(3) = nest(15)
               mid(4) = nest(20)
!
               membrn = mid(1)>0
               bendng = mid(2)>0 .AND. mominr>0.0
               shrflx = mid(3)>0
               mbcoup = mid(4)>0
               norpth = mid(1)==mid(2) .AND. mid(1)==mid(3) .AND. mid(4)==0 .AND. abs(mominr-1.0)<=eps
!
!     SET UP TRANSFORMATION MATRIX FROM MATERIAL TO ELEMENT COORD.SYSTEM
!
               CALL shcsgs(*20,nest(9),nest(8),nest(8),nest(21),nest(20),nest(20),necpt,tub,mcsid,thetam,tum)
!
!     BRANCH ON FORMULATION TYPE.
!
               IF ( heat ) THEN
!
!     HEAT CALCULATIONS
!
                  inflag = 2
                  sinmat = sin(thetam)
                  cosmat = cos(thetam)
                  matid = nest(11)
!
                  CALL hmat(elid)
!
                  gi(1) = kheat(1)
                  gi(2) = kheat(2)
                  gi(3) = gi(2)
                  gi(4) = kheat(3)
!
                  DO i = 1 , 18
                     htcon(i) = 0.0
                     htcap(i) = 0.0
                  ENDDO
!
!     BEGIN LOOP ON INTEGRATION POINTS
!
                  DO ipt = 1 , 3
                     CALL t3bmgs(ierr,sheart,ipt,iorder,egpdt,dgpth,aic,th,detjac,shpt,bterms,bmatrx)
                     IF ( ierr/=0 ) GOTO 10
!
                     dvol = weight*detjac*th
                     DO i = 1 , 4
                        g(i,1) = gi(i)*dvol
                     ENDDO
                     weitc = dvol*htcp
!
                     ip = 1
                     DO i = 1 , nnode
                        htflx(ip) = g(1,1)*bterms(i) + g(2,1)*bterms(i+nnode)
                        htflx(ip+1) = g(3,1)*bterms(i) + g(4,1)*bterms(i+nnode)
                        ip = ip + 2
                     ENDDO
                     CALL gmmats(bterms,2,nnode,-1,htflx,nnode,2,1,htcon)
!
!     FINISHED WITH HEAT CONDUCTIVITY MATRIX, DO HEAT CAPACITY IF
!     REQUIRED.
!
                     IF ( htcp/=0.0 ) THEN
                        ip = 1
                        DO i = 1 , nnode
                           dheat = weitc*shpt(i)
                           DO j = 1 , nnode
                              htcap(ip) = htcap(ip) + dheat*shpt(j)
                              ip = ip + 1
                           ENDDO
                        ENDDO
                     ENDIF
!
                  ENDDO
!
!     END OF INTEGRATION LOOP, SHIP OUT THE RESULTS.
!
                  dict(1) = estid
                  dict(2) = 1
                  dict(3) = nnode
                  dict(4) = 1
                  IF ( weitc/=0.0 ) THEN
                     adamp = 1.0
                     CALL emgout(htcap,htcap,nnod2,ieoe,dict,dmat,prec)
                  ENDIF
                  adamp = 0.0
!
                  CALL emgout(htcon,htcon,nnod2,ieoe,dict,kmat,prec)
               ELSE
!
!     FETCH MATERIAL PROPERTIES
!
                  CALL gmmats(teu,3,3,0,tum,3,3,0,tem)
                  CALL shgmgs(*40,elid,tem,mid,ts,noalfa,gi,rho,gsube,tsub0,egnor,alpha)
!
!     TURN OFF THE COUPLING FLAG WHEN MID4 IS PRESENT WITH ALL
!     CALCULATED ZERO TERMS.
!
                  IF ( mbcoup ) THEN
                     DO i = 28 , 36
                        IF ( abs(gi(i))>eps ) GOTO 2
                     ENDDO
                     mbcoup = .FALSE.
                  ENDIF
!
!     GET THE GEOMETRY CORRECTION TERMS
!
 2                IF ( bendng ) THEN
                     CALL t3gems(ierr,egpdt,iorder,gi(10),gi(19),lx,ly,edglen,shrflx,aic,jog,jok,k11,k22)
                     IF ( ierr/=0 ) GOTO 10
                  ENDIF
!
!     REDUCED INTEGRATION LOOP FOR STIFFNESS
!
                  IF ( .NOT.(.NOT.needk .OR. int/=0) ) THEN
!
!     DETERMINE THE AVERAGE [B] FOR OUT-OF-PLANE SHEAR
!
                     DO ipt = 1 , 3
                        kpt = (ipt-1)*nd9 + 1
                        CALL t3bmgs(ierr,sheart,ipt,iorder,egpdt,dgpth,aic,th,detjac,shpt,bterms,bmat1(kpt))
                        IF ( ierr/=0 ) GOTO 10
                     ENDDO
!
                     DO i = 1 , ndof
                        bmtrx(i) = bmat1(i+nd6) + bmat1(i+nd6+nd9) + bmat1(i+nd6+2*nd9)
                        bmtrx(i+ndof) = bmat1(i+nd7) + bmat1(i+nd7+nd9) + bmat1(i+nd7+2*nd9)
                        bmtrx(i+nd2) = bmat1(i+nd8) + bmat1(i+nd8+nd9) + bmat1(i+nd8+2*nd9)
                     ENDDO
                  ENDIF
!
!     INITIALIZE FOR THE MAIN INTEGRATION LOOP
!
                  needm = mgg1/=0 .AND. (nsm>0.0 .OR. rho>0.0)
                  IF ( .NOT.(.NOT.needk .AND. .NOT.needm) ) THEN
                     DO i = jcored , jend
                        akgg(i) = 0.0
                     ENDDO
!
                     DO i = 1 , 9
                        xmass(i) = 0.0
                     ENDDO
                  ENDIF
!
!     MAIN INTEGRATION LOOP
!
                  DO ipt = 1 , 3
!
                     CALL t3bmgs(ierr,sheart,ipt,iorder,egpdt,dgpth,aic,th,detjac,shpt,bterms,bmatrx)
                     IF ( ierr/=0 ) GOTO 10
!
!     PERFORM STIFFNESS CALCULATIONS IF REQUIRED
!
                     IF ( needk ) THEN
                        wtstif = detjac*weight
                        reali = mominr*th*th*th/12.0
                        tsi = ts*th
!
                        IF ( int==0 ) THEN
                           DO ix = 1 , ndof
                              bmatrx(ix+nd6) = bmtrx(ix)
                              bmatrx(ix+nd7) = bmtrx(ix+ndof)
                              bmatrx(ix+nd8) = bmtrx(ix+nd2)
                           ENDDO
                        ENDIF
!
!     FILL IN THE 9X9 [G]
!
                        DO ig = 1 , 9
                           DO jg = 1 , 9
                              g(ig,jg) = 0.0
                           ENDDO
                        ENDDO
!
                        IF ( membrn ) THEN
                           DO ig = 1 , 3
                              ig1 = (ig-1)*3
                              DO jg = 1 , 3
                                 g(ig,jg) = gi(ig1+jg)*th*wtstif
                              ENDDO
                           ENDDO
                        ENDIF
!
                        IF ( bendng ) THEN
                           DO ig = 4 , 6
                              ig2 = (ig-2)*3
                              DO jg = 4 , 6
                                 g(ig,jg) = gi(ig2+jg)*reali*wtstif
                              ENDDO
                           ENDDO
!
                           tsm = 1.0/(2.0*12.0*reali)
                           zz(1) = (jog/tsi)*gi(22) + tsm*jok*k22
                           zz(2) = -(jog/tsi)*(gi(20)+gi(21))/2.0
                           zz(3) = 0.0
                           zz(4) = zz(2)
                           zz(5) = (jog/tsi)*gi(19) + tsm*jok*k11
                           zz(6) = 0.0
                           zz(7) = 0.0
                           zz(8) = 0.0
                           zz(9) = (jog/tsi)*(gi(22)+gi(19))/2.0 + tsm*12.0*area/sqrt(gi(10)*gi(14))
                           CALL invers(3,zz,3,bdum,0,determ,ising,index)
                           IF ( ising/=1 ) GOTO 10
!
                           DO ig = 7 , 9
                              ig3 = (ig-7)*3
                              DO jg = 7 , 9
                                 g(ig,jg) = zz(ig3+jg-6)*wtstif
                              ENDDO
                           ENDDO
!
                           IF ( mbcoup ) THEN
                              DO ig = 1 , 3
                                 ig4 = (ig+8)*3
                                 DO jg = 1 , 3
                                    g(ig,jg+3) = gi(ig4+jg)*th*th*wtstif
                                    g(ig+3,jg) = g(ig,jg+3)
                                 ENDDO
                              ENDDO
                           ENDIF
                        ENDIF
!
!     COMPUTE THE CONTRIBUTION TO THE STIFFNESS MATRIX FROM THIS
!     INTEGRATION POINT.
!
                        CALL t3bgbs(9,ndof,g,bmatrx,akgg(jcored))
                     ENDIF
!
!
!     END OF STIFFNESS CALCULATIONS.
!     SKIP MASS CALCULATIONS IF NOT REQUIRED
!
!
                     IF ( needm ) THEN
                        wtmass = (rho*th+nsm)*detjac*weight
                        IF ( cpmass<=0 ) THEN
!
!     LUMPED MASS FORMULATION (DEFAULT)
!
                           i3 = 1
                           DO i = 1 , nnode
                              xmass(i3) = xmass(i3) + shpt(i)*wtmass
                              i3 = i3 + 1 + nnode
                           ENDDO
                        ELSE
!
!     CONSISTENT MASS FORMULATION (OPTION)
!
                           DO i = 1 , nnode
                              ii = (i-1)*nnode
                              DO j = 1 , nnode
                                 xmass(ii+j) = xmass(ii+j) + shpt(i)*shpt(j)*wtmass
                              ENDDO
                           ENDDO
                        ENDIF
                     ENDIF
!
!     END OF MAIN INTEGRATION LOOP
!
                  ENDDO
!
!     PICK UP THE ELEMENT TO GLOBAL TRANSFORMATION FOR EACH NODE.
!
                  DO i = 1 , nnode
                     ipoint = 9*(i-1) + 1
                     CALL transs(igpdt(1,i),tbg)
                     CALL gmmats(teb,3,3,0,tbg,3,3,0,trans(ipoint))
                  ENDDO
!
!     SHIP OUT THE STIFFNESS AND DAMPING MATRICES
!
                  IF ( needk ) THEN
!
                     dict(1) = estid
                     dict(2) = 1
                     dict(3) = ndof
                     dict(4) = 63
                     adamp = gsube
!
!     BUILD THE 18X18 TRANSFORMATION MATRIX FOR ONE-SHOT MULTIPLY
!
                     DO i = 1 , npart
                        transk(i) = 0.0
                        tottrn(i) = 0.0
                     ENDDO
!
                     ndof66 = 6*ndof + 6
                     ii = 1
                     DO i = 1 , npart , ndof66
                        CALL tldrs(offset,ii,trans,tmptrn)
                        DO jj = 1 , 36 , 6
                           j = jj - 1
                           kk = i - 1 + j*nnode
                           DO k = 1 , 6
                              tottrn(kk+k) = tmptrn(j+k)
                           ENDDO
                        ENDDO
                        ii = ii + 1
                     ENDDO
!
!     PERFORM THE TRIPLE MULTIPLY.
!
                     CALL mpya3s(tottrn,akgg(jcored),ndof,6,transk)
!
                     CALL emgout(transk,transk,npart,ieoe,dict,kmat,prec)
                  ENDIF
!
!     SHIP OUT THE MASS MATRIX
!
                  IF ( needm ) THEN
                     ndof = nnode*3
                     npart = ndof*ndof
                     dict(2) = 1
                     dict(3) = ndof
                     dict(4) = 7
                     adamp = 0.0
                     jend = jcored + npart - 1
!
!     ZERO OUT THE POSITIONS, THEN LOOP ON I AND J TO LOAD THE MASS
!     MATRIX.
!
                     DO ijk = jcored , jend
                        amgg(ijk) = 0.0
                     ENDDO
!
                     ndofp1 = ndof + 1
                     DO ii = 1 , nnod2 , nnode
                        i = ii - 1
                        DO j = 1 , nnode
                           xmasso = xmass(i+j)
                           ipoint = (j-1)*3 + i*9 + jcored
                           jpoint = ipoint + 3*ndof
                           DO k = ipoint , jpoint , ndofp1
                              amgg(k) = xmasso
                           ENDDO
                        ENDDO
                     ENDDO
!
!     BYPASS TRANSFORMATIONS IF LUMPED MASS.
!
                     IF ( cpmass<=0 ) THEN
!
!     JUST COPY THE LUMPED MASS MATRIX OUT
!
                        ii = jcored
                        DO i = 1 , npart
                           transk(i) = amgg(ii)
                           ii = ii + 1
                        ENDDO
                     ELSE
!
!     BUILD THE 9X9 TRANSFORMATION MATRIX FOR ONE-SHOT MULTIPLY
!
                        DO i = 1 , npart
                           transk(i) = 0.0
                           tottrn(i) = 0.0
                        ENDDO
!
                        ndof33 = 3*ndof + 3
                        DO i = 1 , npart , ndof33
                           ii = ((i-1)/(3*ndof))*9
                           DO jj = 1 , 9 , 3
                              j = jj - 1
                              kk = i - 1 + j*nnode
                              DO k = 1 , 3
                                 tottrn(kk+k) = trans(ii+j+k)
                              ENDDO
                           ENDDO
                        ENDDO
!
!     PERFORM THE TRIPLE MULTIPLY.
!
                        CALL mpya3s(tottrn,amgg(jcored),ndof,3,transk)
                     ENDIF
!
                     CALL emgout(transk,transk,npart,ieoe,dict,mmat,prec)
                  ENDIF
               ENDIF
               RETURN
            ENDIF
!
!     CTRIA3 ELEMENT HAS ILLEGAL GEOMETRY OR CONNECTIONS
!
 10         j = 224
         ENDIF
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     THE X-AXIS OF THE MATERIAL COORDINATE SYSTEM HAS NO PROJECTION
!     ON TO THE PLANE OF CTRIA3 ELEMENT
!
 20      j = 225
         nest(2) = mcsid
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     ILLEGAL DATA DETECTED ON MATERIAL ID REFERENCED BY CTRIA3 ELEMENT
!     FOR MID3 APPLICATION
!
 40      j = 226
         nest(2) = mid(3)
         spag_nextblock_1 = 2
      CASE (2)
!
         CALL mesage(30,j,nest(1))
         IF ( l38==1 ) CALL mesage(-61,0,0)
         nogo = 1
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE tria3s
