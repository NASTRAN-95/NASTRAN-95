!*==tria3d.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE tria3d
USE C_EMGDIC
USE C_EMGEST
USE C_EMGPRM
USE C_HMTOUT
USE C_MATIN
USE C_SYSTEM
USE C_TERMS
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: adamp , elth , gsube , htcp , nsm , tempel , tsub0 , zoff , zoff1
   REAL(REAL64) , DIMENSION(18) :: aic , htflx
   REAL(REAL64) , DIMENSION(1) :: akgg , alpha , amgg
   REAL(REAL64) :: area , avgthk , determ , detjac , dheat , dvol , jog , jok , k11 , k22 , lx , ly , mominr , offset , reali ,     &
                 & rho , th , thetam , ts , tsi , tsm , weight , weitc , wtmass , wtstif , xmasso
   REAL(REAL64) , DIMENSION(3) :: bdum , cente , dgpth , edglen , shpt
   REAL , DIMENSION(4,3) :: bgpdt
   REAL(REAL64) , DIMENSION(486) :: bmat1
   REAL(REAL64) , DIMENSION(162) :: bmatrx
   REAL(REAL64) , DIMENSION(54) :: bmtrx
   REAL(REAL64) , DIMENSION(6) :: bterms
   INTEGER , DIMENSION(11) :: dict
   INTEGER , SAVE :: dmat , hunmeg , kmat , mmat
   REAL , DIMENSION(4) :: ecpt
   REAL(REAL64) , DIMENSION(4) :: egnor
   REAL(REAL64) , DIMENSION(4,3) :: egpdt , epnorm , gpnorm
   REAL(REAL64) , SAVE :: eps
   REAL(REAL64) , DIMENSION(9,9) :: g
   REAL(REAL64) , DIMENSION(36) :: gi , htcap , htcon , tmptrn
   REAL , DIMENSION(3) :: gpth
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
   REAL(REAL64) , DIMENSION(9) :: tbg , teb , tem , teu , tub , tum , xmass , zz
   REAL(REAL64) , DIMENSION(324) :: tottrn , transk
   REAL(REAL64) , DIMENSION(27) :: trans
   EXTERNAL emgout , gmmatd , hmat , inverd , mesage , mpya3d , shcsgd , shgmgd , t3bgbd , t3bmgd , t3gemd , t3setd , tldrd , transd
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     DOUBLE PRECISION ROUTINE TO FORM STIFFNESS, MASS, AND DAMPING
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
   DATA hunmeg , eps/100000000 , 1.0D-7/
   DATA name , kmat , mmat , dmat/4HCTRI , 4HA3   , 1 , 2 , 3/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     INITIALIZE
!
         Elid = nest(1)
         nnode = 3
         mominr = 0.0D0
         ts = 0.0D0
         weight = 1.0D0/6.0D0
         Eltemp = tempel
         needk = Kgg1/=0 .OR. Ibgg1/=0
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
         jcored = Jcore/Prec + 1
         length = (Ncore-Jcore-1)/Prec
         IF ( length<324 .AND. (.NOT.Heat .AND. needk) ) THEN
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
            CALL t3setd(ierr,sil,igpdt,elth,gpth,dgpth,egpdt,gpnorm,epnorm,iorder,teb,tub,cente,avgthk,lx,ly,edglen,Elid)
            IF ( ierr==0 ) THEN
               CALL gmmatd(teb,3,3,0,tub,3,3,1,teu)
               area = lx*ly/2.0D0
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
!     5.0D0/6.0D0 = 0.833333333D0
!
               IF ( nest(13)/=0 ) mominr = Est(14)
               IF ( nest(13)/=0 ) ts = Est(16)
               IF ( Est(16)==0.0 ) ts = 0.833333333D0
               IF ( nest(13)==0 .AND. nest(11)>hunmeg ) ts = 0.833333333D0
!
               mid(1) = nest(11)
               mid(2) = nest(13)
               mid(3) = nest(15)
               mid(4) = nest(20)
!
               Membrn = mid(1)>0
               Bendng = mid(2)>0 .AND. mominr>0.0D0
               Shrflx = mid(3)>0
               Mbcoup = mid(4)>0
               Norpth = mid(1)==mid(2) .AND. mid(1)==mid(3) .AND. mid(4)==0 .AND. dabs(mominr-1.0D0)<=eps
!
!     SET UP TRANSFORMATION MATRIX FROM MATERIAL TO ELEMENT COORD.SYSTEM
!
               CALL shcsgd(*20,nest(9),nest(8),nest(8),nest(21),nest(20),nest(20),necpt,tub,mcsid,thetam,tum)
!
!     BRANCH ON FORMULATION TYPE.
!
               IF ( Heat ) THEN
!
!     HEAT CALCULATIONS
!
                  Inflag = 2
                  Sinmat = dsin(thetam)
                  Cosmat = dcos(thetam)
                  Matid = nest(11)
!
                  CALL hmat(Elid)
!
                  gi(1) = Kheat(1)
                  gi(2) = Kheat(2)
                  gi(3) = gi(2)
                  gi(4) = Kheat(3)
!
                  DO i = 1 , 18
                     htcon(i) = 0.0D0
                     htcap(i) = 0.0D0
                  ENDDO
!
!     BEGIN LOOP ON INTEGRATION POINTS
!
                  DO ipt = 1 , 3
                     CALL t3bmgd(ierr,sheart,ipt,iorder,egpdt,dgpth,aic,th,detjac,shpt,bterms,bmatrx)
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
                     CALL gmmatd(bterms,2,nnode,-1,htflx,nnode,2,1,htcon)
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
                  dict(1) = Estid
                  dict(2) = 1
                  dict(3) = nnode
                  dict(4) = 1
                  IF ( weitc/=0.0D0 ) THEN
                     adamp = 1.0
                     CALL emgout(htcap,htcap,nnod2,ieoe,dict,dmat,Prec)
                  ENDIF
                  adamp = 0.0
!
                  CALL emgout(htcon,htcon,nnod2,ieoe,dict,kmat,Prec)
               ELSE
!
!     FETCH MATERIAL PROPERTIES
!
                  CALL gmmatd(teu,3,3,0,tum,3,3,0,tem)
                  CALL shgmgd(*40,Elid,tem,mid,ts,noalfa,gi,rho,gsube,tsub0,egnor,alpha)
!
!     TURN OFF THE COUPLING FLAG WHEN MID4 IS PRESENT WITH ALL
!     CALCULATED ZERO TERMS.
!
                  IF ( Mbcoup ) THEN
                     DO i = 28 , 36
                        IF ( dabs(gi(i))>eps ) GOTO 2
                     ENDDO
                     Mbcoup = .FALSE.
                  ENDIF
!
!     GET THE GEOMETRY CORRECTION TERMS
!
 2                IF ( Bendng ) THEN
                     CALL t3gemd(ierr,egpdt,iorder,gi(10),gi(19),lx,ly,edglen,Shrflx,aic,jog,jok,k11,k22)
                     IF ( ierr/=0 ) GOTO 10
                  ENDIF
!
!     REDUCED INTEGRATION LOOP FOR STIFFNESS
!
                  IF ( .NOT.(.NOT.needk .OR. int/=0) ) THEN
!
!     DETERMINE THE AVERAGE B-MATRIX FOR OUT-OF-PLANE SHEAR
!
                     DO ipt = 1 , 3
                        kpt = (ipt-1)*nd9 + 1
                        CALL t3bmgd(ierr,sheart,ipt,iorder,egpdt,dgpth,aic,th,detjac,shpt,bterms,bmat1(kpt))
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
                  needm = Mgg1/=0 .AND. (nsm>0.0 .OR. rho>0.0D0)
                  IF ( .NOT.(.NOT.needk .AND. .NOT.needm) ) THEN
                     DO i = jcored , jend
                        akgg(i) = 0.0D0
                     ENDDO
!
                     DO i = 1 , 9
                        xmass(i) = 0.0D0
                     ENDDO
                  ENDIF
!
!     MAIN INTEGRATION LOOP
!
                  DO ipt = 1 , 3
!
                     CALL t3bmgd(ierr,sheart,ipt,iorder,egpdt,dgpth,aic,th,detjac,shpt,bterms,bmatrx)
                     IF ( ierr/=0 ) GOTO 10
!
!     PERFORM STIFFNESS CALCULATIONS IF REQUIRED
!
                     IF ( needk ) THEN
                        wtstif = detjac*weight
                        reali = mominr*th*th*th/12.0D0
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
!     FILL IN THE 9X9 G-MATRIX
!
                        DO ig = 1 , 81
                           g(ig,1) = 0.0D0
                        ENDDO
!
                        IF ( Membrn ) THEN
                           DO ig = 1 , 3
                              ig1 = (ig-1)*3
                              DO jg = 1 , 3
                                 g(ig,jg) = gi(ig1+jg)*th*wtstif
                              ENDDO
                           ENDDO
                        ENDIF
!
                        IF ( Bendng ) THEN
                           DO ig = 4 , 6
                              ig2 = (ig-2)*3
                              DO jg = 4 , 6
                                 g(ig,jg) = gi(ig2+jg)*reali*wtstif
                              ENDDO
                           ENDDO
!
                           tsm = 1.0D0/(2.0D0*12.0D0*reali)
                           zz(1) = (jog/tsi)*gi(22) + tsm*jok*k22
                           zz(2) = -(jog/tsi)*(gi(20)+gi(21))/2.0D0
                           zz(3) = 0.0D0
                           zz(4) = zz(2)
                           zz(5) = (jog/tsi)*gi(19) + tsm*jok*k11
                           zz(6) = 0.0D0
                           zz(7) = 0.0D0
                           zz(8) = 0.0D0
                           zz(9) = (jog/tsi)*(gi(22)+gi(19))/2.0D0 + tsm*12.0D0*area/dsqrt(gi(10)*gi(14))
                           CALL inverd(3,zz,3,bdum,0,determ,ising,index)
                           IF ( ising/=1 ) GOTO 10
!
                           DO ig = 7 , 9
                              ig3 = (ig-7)*3
                              DO jg = 7 , 9
                                 g(ig,jg) = zz(ig3+jg-6)*wtstif
                              ENDDO
                           ENDDO
!
                           IF ( Mbcoup ) THEN
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
                        CALL t3bgbd(9,ndof,g,bmatrx,akgg(jcored))
                     ENDIF
!
!
!     END OF STIFFNESS CALCULATIONS.
!     SKIP MASS CALCULATIONS IF NOT REQUIRED
!
!
                     IF ( needm ) THEN
                        wtmass = (rho*th+nsm)*detjac*weight
                        IF ( Cpmass<=0 ) THEN
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
                     CALL transd(igpdt(1,i),tbg)
                     CALL gmmatd(teb,3,3,0,tbg,3,3,0,trans(ipoint))
                  ENDDO
!
!     SHIP OUT THE STIFFNESS AND DAMPING MATRICES
!
                  IF ( needk ) THEN
!
                     dict(1) = Estid
                     dict(2) = 1
                     dict(3) = ndof
                     dict(4) = 63
                     adamp = gsube
!
!     BUILD THE 18X18 TRANSFORMATION MATRIX FOR ONE-SHOT MULTIPLY
!
                     DO i = 1 , npart
                        transk(i) = 0.0D0
                        tottrn(i) = 0.0D0
                     ENDDO
!
                     ndof66 = 6*ndof + 6
                     ii = 1
                     DO i = 1 , npart , ndof66
                        CALL tldrd(offset,ii,trans,tmptrn)
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
                     CALL mpya3d(tottrn,akgg(jcored),ndof,6,transk)
!
                     CALL emgout(transk,transk,npart,ieoe,dict,kmat,Prec)
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
                        amgg(ijk) = 0.0D0
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
                     IF ( Cpmass<=0 ) THEN
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
                           transk(i) = 0.0D0
                           tottrn(i) = 0.0D0
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
                        CALL mpya3d(tottrn,amgg(jcored),ndof,3,transk)
                     ENDIF
!
!
                     CALL emgout(transk,transk,npart,ieoe,dict,mmat,Prec)
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
         IF ( L38==1 ) CALL mesage(-61,0,0)
         Nogo = 1
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE tria3d
