!*==tlqd4d.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE tlqd4d
   IMPLICIT NONE
   USE C_BLANK
   USE C_COMPST
   USE C_CONDAD
   USE C_MATIN
   USE C_MATOUT
   USE C_Q4COMD
   USE C_Q4DT
   USE C_SGTMPD
   USE C_SYSTEM
   USE C_TERMS
   USE C_TRIMEX
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   REAL*8 :: aa , bb , c , c2 , cc , coeff , delta , deltat , determ , detg2 , detu , eta , exi , exj , mintr , mominr , offset ,   &
           & reali , s , s2 , tbar , tempel , tgrad , theta , thetae , thetam , thk , tlam , tmean , tsubo , ttbar , v12dk , vjl ,  &
           & vkl , vp12l , x31 , x42 , xi , xm , y31 , y42 , ym , zk , zk1 , zref , zsubi
   REAL*8 , DIMENSION(6,6) :: abbd , g
   REAL*8 , DIMENSION(3) :: alfab , alfam , alphae , alphal , cent , cente , galpha , talfab , talfam , thrmom , v12 , v41 , vd1 ,  &
                          & vd2 , vis , vjs , vkn , vks , vp12
   REAL*8 , DIMENSION(6) :: alpha , alphad , epslnt , epsubt , ftherm , gepsbt , gtemps
   REAL , DIMENSION(6) :: alpham
   REAL :: bdum , dum , elth , gdum , matset , pa , sysbuf , ti , tsub0 , zoff , zoff1 , zta
   REAL , DIMENSION(3,4) :: bgpdm
   REAL , DIMENSION(4,4) :: bgpdt , egpdt , epnorm , gpnorm , tgrid
   REAL*8 , DIMENSION(144) :: bmatrx
   LOGICAL :: compos , pcmp , pcmp1 , pcmp2 , tempp1 , tempp2
   REAL*8 , SAVE :: const , eps1
   REAL*8 , DIMENSION(4) :: dgpth , shp , tmpshp
   REAL*8 , DIMENSION(8) :: dshp , dshptp
   REAL , DIMENSION(4) :: ecpt , gpth , tmpthk
   INTEGER :: elid , flag , i , i1 , ibot , iec , ieta , iflag , ig , ig1 , ig2 , ig4 , ii , ij , imat , imt , ip , ipc11 , ipc21 , &
            & ipnt , ipoint , ipt , ir , isil , ising , isngg2 , isngu , itemp , itype , ix , ixsi , izta , j , jg , jg1 , jg2 ,    &
            & jg4 , jj , jpt1 , jpt2 , k , k1 , kg , kk , kpt , kpt1 , l , lamopt , lg , ll , lpc11 , lpcomp , lpoint , ltypfl , m ,&
            & mcsid , mm , morb , nd2 , nd3 , nd4 , nd5 , ndof , nlay , nn , nogo , nout , pid , pidloc
   REAL*8 , DIMENSION(9) :: g2 , g2i , gbar , gge , ggu , glay , glayt , gt , jacob , phi , tbg , tbm , teb , tem , teu , transl ,  &
                          & tub , tum , u
   REAL*8 , DIMENSION(36) :: gi , stiff , trans
   REAL*8 , DIMENSION(25) :: gprop
   INTEGER , DIMENSION(4,4) :: igpdt
   INTEGER , DIMENSION(3,3) :: index
   INTEGER , DIMENSION(6,3) :: indx
   INTEGER , DIMENSION(1) :: intz
   INTEGER , DIMENSION(4) :: kcid , ksil , mid , necpt , sil
   INTEGER , SAVE :: mem , pcomp , pcomp1 , pcomp2 , sym , symmem
   INTEGER , DIMENSION(2) , SAVE :: nam
   INTEGER , DIMENSION(45) :: nest
   REAL*8 , DIMENSION(24) :: pt , ptg
   REAL*8 , DIMENSION(2) :: ptint
   REAL*8 , DIMENSION(3,4) :: ugpdm
   REAL*8 , DIMENSION(96) :: xybmat
!
! End of declarations rewritten by SPAG
!
!
!     ELEMENT THERMAL LOAD GENERATOR FOR 4-NODE ISOPARAMETRIC
!     QUADRILATERAL SHELL ELEMENT (QUAD4)
!     (DOUBLE PRECISION VERSION)
!
!     COMPLETELY RESTRUCTURED FOR COMPOSITES WITH THE FOLLOWING
!     LIMITATION -
!     1. FOR DIFFERENT GRID POINT TEMPERATURES AN AVERAGE
!        VALUE IS TAKEN.                       HEMANT  2/24/86
!
!
!                 EST LISTING
!     ---------------------------------------------------------
!      1          EID
!      2 THRU 5   SILS, GRIDS 1 THRU 4
!      6 THRU 9   T (MEMBRANE), GRIDS 1 THRU 4
!     10          THETA (MATERIAL)
!     11          TYPE FLAG FOR WORD 10
!     12          ZOFF  (OFFSET)
!     13          MATERIAL ID FOR MEMBRANE
!     14          T (MEMBRANE)
!     15          MATERIAL ID FOR BENDING
!     16          I FACTOR (BENDING)
!     17          MATERIAL ID FOR TRANSVERSE SHEAR
!     18          FACTOR FOR T(S)
!     19          NSM (NON-STRUCTURAL MASS)
!     20 THRU 21  Z1, Z2  (STRESS FIBRE DISTANCES)
!     22          MATERIAL ID FOR MEMBRANE-BENDING COUPLING
!     23          THETA (MATERIAL) FROM PSHELL CARD
!     24          TYPE FLAG FOR WORD 23
!     25          INTEGRATION ORDER
!     26          THETA (STRESS)
!     27          TYPE FLAG FOR WORD 26
!     28          ZOFF1 (OFFSET)  OVERRIDDEN BY EST(12)
!     29 THRU 44  CID,X,Y,Z - GRIDS 1 THRU 4
!     45          ELEMENT TEMPERATURE
!
!
!WKBNB 11/93 SPR 93020
!WKBNE 11/93 SPR 93020
!WKBI 9/94 SPR93020
!
!ZZ   COMMON /ZZSSB1/  Z(1)
!
   !>>>>EQUIVALENCE (Z(1),Intz(1)) , (igpdt(1,1),Bgpdt(1,1))
   !>>>>EQUIVALENCE (Est(1),Nest(1)) , (Matset,Rmtout(25))
   !>>>>EQUIVALENCE (Gpth(1),Est(6)) , (Bgpdt(1,1),Est(29))
   !>>>>EQUIVALENCE (Elth,Est(14)) , (Sil(1),Nest(2))
   !>>>>EQUIVALENCE (Zoff,Est(12)) , (Zoff1,Est(28))
   !>>>>EQUIVALENCE (necpt(1),ecpt(1)) , (Buffer(1),Sysbuf)
   !>>>>EQUIVALENCE (Buffer(2),Nout) , (Buffer(3),Nogo)
   !>>>>EQUIVALENCE (Stemp(7),Flag) , (alfam(1),alpha(1))
   !>>>>EQUIVALENCE (alfab(1),alpha(4))
!
   DATA eps1/1.0D-7/
   DATA pcomp/0/
   DATA pcomp1/1/
   DATA pcomp2/2/
   DATA sym/1/
   DATA mem/2/
   DATA symmem/3/
   DATA const/0.57735026918962D+0/
   DATA nam/4HTLQD , 4H4D  /
!
!     ZERO THE VARIOUS ALPHA ARRAYS
!
   DO i = 1 , 6
      alpham(i) = 0.0
      alpha(i) = 0.0D0
      alphad(i) = 0.0D0
   ENDDO
   DO i = 1 , 3
      talfam(i) = 0.0D0
      talfab(i) = 0.0D0
   ENDDO
!
   elid = nest(1)
   ltypfl = 1
   offset = zoff
   IF ( zoff==0.0 ) offset = zoff1
!
!     TEST FOR COMPOSITE ELEMENT
!
   compos = .FALSE.
!
   pid = nest(13) - 100000000
   compos = Comps== - 1 .AND. pid>0
!
!     CHECK FOR THE TYPE OF TEMPERATURE DATA
!     NOTES-  1- TYPE TEMPP1 ALSO INCLUDES TYPE TEMPP3
!             2- IF NO TEMPPI CARDS, GRID POINT TEMPERATURES
!                ONLY ARE PRESENT
!
   tempp1 = flag==13
   tempp2 = flag==2
!
   N1 = 4
   Nnode = 4
   ndof = Nnode*6
   nd2 = ndof*2
   nd3 = ndof*3
   nd4 = ndof*4
   nd5 = ndof*5
!
!     FILL IN ARRAY GGU WITH THE COORDINATES OF GRID POINTS
!     1, 2 AND 4. THIS ARRAY WILL BE USED LATER TO DEFINE
!     THE USER COORDINATE SYSTEM WHILE CALCULATING
!     TRANSFORMATIONS INVOLVING THIS COORDINATE SYSTEM.
!
   DO i = 1 , 3
      ii = (i-1)*3
      ij = i
      IF ( ij==3 ) ij = 4
      DO j = 1 , 3
         jj = j + 1
         ggu(ii+j) = bgpdt(jj,ij)
      ENDDO
   ENDDO
!WKBD 11/93 SPR93020      CALL BETRND (TUB,GGU,0,ELID)
!WKBNB 11/93 SPR93020
!    ADD FROM SHEAR ELEMENT
!
!    COMPUTE DIAGONAL VECTORS
!
   DO i = 1 , 3
      ii = i + 1
      vd1(i) = bgpdt(ii,3) - bgpdt(ii,1)
      vd2(i) = bgpdt(ii,4) - bgpdt(ii,2)
   ENDDO
!
!    COMPUTE THE NORMAL VECTOR VKN, NORMALIZE, AND COMPUTE THE PROJECTED
!    AREA, PA
!
   vkn(1) = vd1(2)*vd2(3) - vd1(3)*vd2(2)
   vkn(2) = vd1(3)*vd2(1) - vd1(1)*vd2(3)
   vkn(3) = vd1(1)*vd2(2) - vd1(2)*vd2(1)
   vkl = dsqrt(vkn(1)**2+vkn(2)**2+vkn(3)**2)
   IF ( vkl==0. ) WRITE (nout,99002) Est(1)
   vks(1) = vkn(1)/vkl
   vks(2) = vkn(2)/vkl
   vks(3) = vkn(3)/vkl
   pa = vkl/2.
!
!  COMPUTE SIDES -12- AND -41-
   DO i = 1 , 3
      ii = i + 1
      v12(i) = bgpdt(ii,2) - bgpdt(ii,1)
      v41(i) = bgpdt(ii,1) - bgpdt(ii,4)
   ENDDO
!
!  COMPUTE DOT PRODUCT, V12DK, OR V12 AND VK, THE VECTORS VP12, VI, VJ
!
   v12dk = v12(1)*vks(1) + v12(2)*vks(2) + v12(3)*vks(3)
   vp12(1) = v12(1) - v12dk*vks(1)
   vp12(2) = v12(2) - v12dk*vks(2)
   vp12(3) = v12(3) - v12dk*vks(3)
   vp12l = dsqrt(vp12(1)**2+vp12(2)**2+vp12(3)**2)
   IF ( vp12l==0. ) WRITE (nout,99002) Est(1)
   vis(1) = vp12(1)/vp12l
   vis(2) = vp12(2)/vp12l
   vis(3) = vp12(3)/vp12l
   vjs(1) = vks(2)*vis(3) - vks(3)*vis(2)
   vjs(2) = vks(3)*vis(1) - vks(1)*vis(3)
   vjs(3) = vks(1)*vis(2) - vks(2)*vis(1)
!
!   NORMALIZE J FOR GOOD MEASURE
!
   vjl = dsqrt(vjs(1)**2+vjs(2)**2+vjs(3)**2)
   IF ( vjl==0. ) WRITE (nout,99002) Est(1)
   vjs(1) = vjs(1)/vjl
   vjs(2) = vjs(2)/vjl
   vjs(3) = vjs(3)/vjl
   DO i = 1 , 3
      tub(i) = vis(i)
      tub(i+3) = vjs(i)
      tub(i+6) = vks(i)
   ENDDO
!WKBNE 11/93 SPR93020
!
!     STORE INCOMING BGPDT FOR ELEMENT C.S. CALCULATION
!
   DO i = 1 , 3
      i1 = i + 1
      DO j = 1 , 4
         bgpdm(i,j) = bgpdt(i1,j)
      ENDDO
   ENDDO
!
!     TRANSFORM BGPDM FROM BASIC TO USER C.S.
!
   DO i = 1 , 3
      ip = (i-1)*3
      DO j = 1 , 4
         ugpdm(i,j) = 0.0
         DO k = 1 , 3
            kk = ip + k
            ugpdm(i,j) = ugpdm(i,j) + tub(kk)*(dble(bgpdm(k,j))-ggu(k))
         ENDDO
      ENDDO
   ENDDO
!
!
!     THE ORIGIN OF THE ELEMENT C.S. IS IN THE MIDDLE OF THE ELEMENT
!
   DO j = 1 , 3
      cent(j) = 0.0D0
      DO i = 1 , 4
         cent(j) = cent(j) + ugpdm(j,i)/Nnode
      ENDDO
   ENDDO
!
!     STORE THE CORNER NODE DIFF. IN THE USER C. S.
!
   x31 = ugpdm(1,3) - ugpdm(1,1)
   y31 = ugpdm(2,3) - ugpdm(2,1)
   x42 = ugpdm(1,4) - ugpdm(1,2)
   y42 = ugpdm(2,4) - ugpdm(2,2)
   aa = dsqrt(x31*x31+y31*y31)
   bb = dsqrt(x42*x42+y42*y42)
!
!     NORMALIZE XIJ'S
!
   x31 = x31/aa
   y31 = y31/aa
   x42 = x42/bb
   y42 = y42/bb
   exi = x31 - x42
   exj = y31 - y42
!
!     STORE GGE ARRAY, THE OFFSET BETWEEN ELEMENT C.S. AND USER C.S.
!
   gge(1) = cent(1)
   gge(2) = cent(2)
   gge(3) = cent(3)
!
   gge(4) = gge(1) + exi
   gge(5) = gge(2) + exj
   gge(6) = gge(3)
!
   gge(7) = gge(1) - exj
   gge(8) = gge(2) + exi
   gge(9) = gge(3)
!
!
!     THE ARRAY IORDER STORES THE ELEMENT NODE ID IN
!     INCREASING SIL ORDER.
!
!     IORDER(1) = NODE WITH LOWEST  SIL NUMBER
!     IORDER(4) = NODE WITH HIGHEST SIL NUMBER
!
!     ELEMENT NODE NUMBER IS THE INTEGER FROM THE NODE
!     LIST  G1,G2,G3,G4 .  THAT IS, THE 'I' PART
!     OF THE 'GI' AS THEY ARE LISTED ON THE CONNECTIVITY
!     BULK DATA CARD DESCRIPTION.
!
!
   DO i = 1 , 4
      Iorder(i) = 0
      ksil(i) = sil(i)
   ENDDO
!
   DO i = 1 , 4
      itemp = 1
      isil = ksil(1)
      DO j = 2 , 4
         IF ( isil>ksil(j) ) THEN
            itemp = j
            isil = ksil(j)
         ENDIF
      ENDDO
      Iorder(i) = itemp
      ksil(itemp) = 99999999
   ENDDO
!
!     ADJUST EST DATA
!
!
!     USE THE POINTERS IN IORDER TO COMPLETELY REORDER THE
!     GEOMETRY DATA INTO INCREASING SIL ORDER.
!     DON'T WORRY!! IORDER ALSO KEEPS TRACK OF WHICH SHAPE
!     FUNCTIONS GO WITH WHICH GEOMETRIC PARAMETERS!
!
!
   DO i = 1 , 4
      ksil(i) = sil(i)
      tmpthk(i) = gpth(i)
      kcid(i) = igpdt(1,i)
      DO j = 2 , 4
         tgrid(j,i) = bgpdt(j,i)
      ENDDO
   ENDDO
   DO i = 1 , 4
      ipoint = Iorder(i)
      sil(i) = ksil(ipoint)
      gpth(i) = tmpthk(ipoint)
      igpdt(1,i) = kcid(ipoint)
      DO j = 2 , 4
         bgpdt(j,i) = tgrid(j,ipoint)
      ENDDO
   ENDDO
!
!     SORT THE GRID POINT TEMPERATURES (IN STEMP(1-4)). IF PRESENT AND
!     MAKE DOUBLE PRECISION THE OTHER KINDS OF TEMPERATURE DATA IF
!     TEMPPI CARDS PRESENT
!
   IF ( .NOT.(tempp1 .OR. tempp2) ) THEN
!
      tempel = 0.0D0
      DO i = 1 , 4
         ipnt = Iorder(i)
         gtemps(i) = Stemp(ipnt)
         tempel = tempel + gtemps(i)
      ENDDO
      tempel = tempel*0.25D0
!
   ELSEIF ( tempp2 ) THEN
!
      tbar = Stemp(1)
      thrmom(1) = Stemp(2)
      thrmom(2) = Stemp(3)
      thrmom(3) = Stemp(4)
   ELSE
!
      tbar = Stemp(1)
      tgrad = Stemp(2)
   ENDIF
!
!     COMPUTE NODE NORMALS
!
   CALL q4nrmd(bgpdt,gpnorm,Iorder,iflag)
   IF ( iflag==0 ) THEN
!
!     DETERMINE NODAL THICKNESSES
!
      DO i = 1 , Nnode
         IF ( gpth(i)==0.0 ) gpth(i) = elth
         IF ( gpth(i)>0.0 ) THEN
            dgpth(i) = gpth(i)
         ELSE
            WRITE (nout,99001) elid
!
99001       FORMAT ('0*** SYSTEM FATAL ERROR. THE ELEMENT THICKNESS FOR ',' QUAD4 EID = ',I8,' IS NOT COMPLETELY DEFINED.')
            nogo = 1
            GOTO 300
         ENDIF
      ENDDO
!
      mominr = 0.0D0
      IF ( nest(15)/=0 ) mominr = Est(16)
!
!     THE COORDINATES OF THE ELEMENT GRID POINTS HAVE TO BE
!     TRANSFORMED FROM THE BASIC C.S. TO THE ELEMENT C.S.
!
      CALL betrnd(teu,gge,0,elid)
      CALL gmmatd(teu,3,3,0,tub,3,3,0,teb)
      CALL gmmatd(tub,3,3,1,cent,3,1,0,cente)
!
      ip = -3
      DO ii = 2 , 4
         ip = ip + 3
         DO j = 1 , Nnode
            epnorm(ii,j) = 0.0
            egpdt(ii,j) = 0.0
            DO k = 1 , 3
               kk = ip + k
               k1 = k + 1
               cc = dble(bgpdt(k1,j)) - ggu(k) - cente(k)
               epnorm(ii,j) = epnorm(ii,j) + teb(kk)*gpnorm(k1,j)
               egpdt(ii,j) = egpdt(ii,j) + sngl(teb(kk)*cc)
            ENDDO
         ENDDO
      ENDDO
!WKBNB 11/93 SPR93020
      DO j = 1 , 4
         egpdt(4,j) = cent(3)
      ENDDO
!WKBNE 11/93 SPR93020
!
!     BEGIN INITIALIZING MATERIAL VARIABLES
!
!     SET INFLAG = 12 SO THAT SUBROUTINE MAT WILL SEARCH FOR -
!     ISOTROPIC MATERIAL PROPERTIES AMONG THE MAT1 CARDS,
!     ORTHOTROPIC MATERIAL PROPERTIES AMONG THE MAT8 CARDS, AND
!     ANISOTROPIC MATERIAL PROPERTIES AMONG THE MAT2 CARDS.
!
      Inflag = 12
      Eltemp = Est(45)
      mid(1) = nest(13)
      mid(2) = nest(15)
      mid(3) = 0
      mid(4) = nest(22)
      Membrn = mid(1)>0
      Bendng = mid(2)>0 .AND. mominr>0.0D0
      Shrflx = mid(3)>0
      Mbcoup = mid(4)>0
      Norpth = .FALSE.
!
!     SET THE INTEGRATION POINTS
!
      ptint(1) = -const
      ptint(2) = const
!
!     IN PLANE SHEAR REDUCTION
!
      xi = 0.0D0
      eta = 0.0D0
      kpt = 1
      kpt1 = nd2
!
      CALL q4shpd(xi,eta,shp,dshp)
!
!     SORT THE SHAPE FUNCTIONS AND THEIR DERIVATIVES INTO SIL ORDER.
!
      DO i = 1 , 4
         tmpshp(i) = shp(i)
         dshptp(i) = dshp(i)
         dshptp(i+4) = dshp(i+4)
      ENDDO
      DO i = 1 , 4
         kk = Iorder(i)
         shp(i) = tmpshp(kk)
         dshp(i) = dshptp(kk)
         dshp(i+4) = dshptp(kk+4)
      ENDDO
!
      DO izta = 1 , 2
         zta = ptint(izta)
         Hzta = zta/2.0D0
         CALL jacob2(elid,shp,dshp,dgpth,egpdt,epnorm,jacob)
         IF ( Badjac ) GOTO 300
!
         CALL gmmatd(Psitrn,3,3,0,jacob,3,3,1,phi)
!
!     CALL Q4BMGD TO GET B MATRIX
!     SET THE ROW FLAG TO 2. IT WILL SAVE THE 3RD ROW OF B AT
!     THE TWO INTEGRATION POINTS.
!
         Rowflg = 2
         CALL q4bmgd(dshp,dgpth,egpdt,epnorm,phi,xybmat(kpt))
         kpt = kpt + kpt1
      ENDDO
!
!     SET THE ARRAY OF LENGTH 4 TO BE USED IN CALLING TRANSD.
!     NOTE THAT THE FIRST WORD IS THE COORDINATE SYSTEM ID WHICH
!     WILL BE SET IN POSITION LATER.
!
      DO iec = 2 , 4
         ecpt(iec) = 0.0
      ENDDO
!
!     FETCH MATERIAL PROPERTIES
!
!     EACH MATERIAL PROPERTY MATRIX G HAS TO BE TRANSFORMED FROM
!     THE MATERIAL COORDINATE SYSTEM TO THE ELEMENT COORDINATE
!     SYSTEM. THESE STEPS ARE TO BE FOLLOWED-
!
!     1- IF MCSID HAS BEEN SPECIFIED, SUBROUTINE TRANSD IS CALLED
!        TO CALCULATE TBM MATRIX (MATERIAL TO BASIC TRANSFORMATION).
!        THIS WILL BE FOLLOWED BY A CALL TO SUBROUTINE BETRND
!        TO CALCULATE TEB MATRIX (BASIC TO ELEMENT TRANSFORMATION).
!        TBM IS THEN PREMULTIPLIED BY TEB TO OBTAIN TEM MATRIX.
!        THEN USING THE PROJECTION OF X-AXIS, AN ANGLE IS CALCULATED
!        UPON WHICH STEP 2 IS TAKEN.
!
!     2- IF THETAM HAS BEEN SPECIFIED, SUBROUTINE ANGTRD IS CALLED
!        TO CALCULATE TEM MATRIX (MATERIAL TO ELEMENT TRANSFORMATION).
!
!                          T
!     3-          G   =   U   G    U
!                  E           M
!
!
      IF ( nest(11)==0 ) THEN
!
!     CALCULATE TEM USING THETAM
!
         thetam = dble(Est(10))*Degrad
         IF ( thetam/=0.0D0 ) GOTO 100
!
!     DEFAULT IS CHOSEN, LOOK FOR VALUES OF MCSID AND/OR THETAM
!     ON THE PSHELL CARD.
!
         IF ( nest(24)==0 ) THEN
!
            thetam = dble(Est(23))*Degrad
            GOTO 100
         ELSE
            mcsid = nest(23)
         ENDIF
      ELSE
         mcsid = nest(10)
      ENDIF
!
!     CALCULATE TEM USING MCSID
!
      IF ( mcsid>0 ) THEN
         necpt(1) = mcsid
         CALL transd(ecpt,tbm)
!
!     MULTIPLY TEB AND TBM
!
         CALL gmmatd(teb,3,3,0,tbm,3,3,0,tem)
      ELSE
         DO i = 1 , 9
            tem(i) = teb(i)
         ENDDO
      ENDIF
!
!     CALCULATE THETAM FROM THE PROJECTION OF THE X-AXIS OF THE
!     MATERIAL C.S. ON TO THE XY PLANE OF THE ELEMENT C.S.
!
      imt = -1
      xm = tem(1)
      ym = tem(4)
      IF ( dabs(xm)<=eps1 ) imt = imt + 1
      IF ( dabs(ym)<=eps1 ) imt = imt + 2
      IF ( imt<2 ) THEN
         thetam = datan2(ym,xm)
      ELSE
         nest(2) = mcsid
         j = -231
         GOTO 200
      ENDIF
   ELSE
      j = -230
      GOTO 200
   ENDIF
 100  CALL angtrd(thetam,1,tum)
   CALL gmmatd(teu,3,3,0,tum,3,3,0,tem)
!
!     BEGIN THE LOOP TO FETCH PROPERTIES FOR EACH MATERIAL ID
!
   m = 0
   DO
      m = m + 1
      IF ( m>4 ) THEN
!
         IF ( compos ) THEN
!
!      IF LAMINATED COMPOSITE ELEMENT, DETERMINE THE THERMAL
!      STRAIN VECTOR DUE TO THE APPLIED THERMAL LOADING.
!      NOTE THE FOLLOWING -
!         1. DIFFERENT GRID POINT TEMPERATURES ARE NOT SUPPORTED
!
!     LOCATE PID BY CARRYING OUT A SEQUENTIAL SEARCH
!     OF THE PCOMPS DATA BLOCK, AND ALSO DETERMINE
!     THE TYPE OF 'PCOMP' BULK DATA ENTRY.
!
!     POINTER DESCRIPITION
!     --------------------
!     IPCMP  - LOCATION OF START OF PCOMP DATA IN CORE
!     NPCMP  - NUMBER OF WORDS OF PCOMP DATA
!     IPCMP1 - LOCATION OF START OF PCOMP1 DATA IN CORE
!     NPCMP1 - NUMBER OF WORDS OF PCOMP1 DATA
!     IPCMP2 - LOCATION OF START OF PCOMP2 DATA IN CORE
!     NPCMP2 - NUMBER OF WORDS OF PCOMP2 DATA
!
!     ITYPE  - TYPE OF PCOMP BULK DATA ENTRY
!
!
!     LAMOPT - LAMINATION GENERATION OPTION
!            = SYM  (SYMMETRIC)
!            = MEM  (MEMBRANE)
!            = SYMMEM  (SYMMETRIC-MEMBRANE)
!
!
!     SET POINTER LPCOMP
            lpcomp = Ipcmp + Npcmp + Npcmp1 + Npcmp2
!
!     SET POINTERS
!
            itype = -1
!
            pcmp = .FALSE.
            pcmp1 = .FALSE.
            pcmp2 = .FALSE.
!
            pcmp = Npcmp>0
            pcmp1 = Npcmp1>0
            pcmp2 = Npcmp2>0
!
!     CHECK IF NO 'PCOMP' DATA HAS BEEN READ INTO CORE
!
            IF ( pcmp .OR. pcmp1 .OR. pcmp2 ) THEN
!
!     SEARCH FOR PID IN PCOMP DATA
!
               IF ( pcmp ) THEN
!
                  ip = Ipcmp
                  IF ( intz(ip)==pid ) THEN
                     itype = pcomp
                     GOTO 120
                  ELSE
                     ipc11 = Ipcmp1 - 1
                     DO ip = Ipcmp , ipc11
                        IF ( intz(ip)==-1 .AND. ip<(Ipcmp1-1) ) THEN
                           IF ( intz(ip+1)==pid ) GOTO 102
                        ENDIF
                     ENDDO
                     GOTO 105
                  ENDIF
!
 102              ip = ip + 1
                  itype = pcomp
                  GOTO 120
               ENDIF
!
!     SEARCH FOR PID IN PCOMP1 DATA
!
 105           IF ( pcmp1 ) THEN
                  ip = Ipcmp1
                  IF ( intz(ip)==pid ) THEN
                     itype = pcomp1
                     GOTO 120
                  ELSE
                     ipc21 = Ipcmp2 - 1
                     DO ip = Ipcmp1 , ipc21
                        IF ( intz(ip)==-1 .AND. ip<(Ipcmp2-1) ) THEN
                           IF ( intz(ip+1)==pid ) GOTO 106
                        ENDIF
                     ENDDO
                     GOTO 110
                  ENDIF
!
 106              ip = ip + 1
                  itype = pcomp1
                  GOTO 120
               ENDIF
!
!     SEARCH FOR PID IN PCOMP2 DATA
!
 110           ip = Ipcmp2
               IF ( intz(ip)/=pid ) THEN
                  lpc11 = lpcomp - 1
                  DO ip = Ipcmp2 , lpc11
                     IF ( intz(ip)==-1 .AND. ip<(lpcomp-1) ) THEN
                        IF ( intz(ip+1)==pid ) GOTO 112
                     ENDIF
                  ENDDO
!
!     CHECK IF PID HAS NOT BEEN LOCATED
!
                  IF ( itype/=-1 ) GOTO 120
                  j = -229
                  EXIT
!
 112              ip = ip + 1
               ENDIF
               itype = pcomp2
            ELSE
               j = -229
               EXIT
            ENDIF
!
!     LOCATION OF PID
!
 120        pidloc = ip
            lamopt = intz(pidloc+8)
!
!     DETERMINE INTRINSIC LAMINATE PROPERTIES
!
!     LAMINATE THICKNESS
!
            tlam = elth
!
!     LAMINATE EXTENSIONAL, BENDING AND MEMBRANE-BENDING MATRICES
!
            DO ll = 1 , 6
               DO mm = 1 , 6
                  abbd(ll,mm) = 0.0D0
               ENDDO
            ENDDO
!
!     EXTENSIONAL
!
            Matid = mid(1)
            CALL mat(elid)
!
            CALL lpropd(gprop)
!
            DO ll = 1 , 3
               DO mm = 1 , 3
                  ii = mm + 3*(ll-1)
                  abbd(ll,mm) = gprop(ii)*tlam
               ENDDO
            ENDDO
!
            IF ( lamopt/=mem .AND. lamopt/=symmem ) THEN
!
!     BENDING
!
               Matid = mid(2)
               CALL mat(elid)
!
               CALL lpropd(gprop)
!
!     MOMENT OF INERTIA OF LAMINATE
!
               mintr = (tlam**3)/12.0D0
!
               DO ll = 1 , 3
                  DO mm = 1 , 3
                     ii = mm + 3*(ll-1)
                     abbd(ll+3,mm+3) = gprop(ii)*mintr
                  ENDDO
               ENDDO
!
               IF ( lamopt/=sym ) THEN
!
!     MEMBRANE-BENDING
!
                  Matid = mid(4)
                  CALL mat(elid)
!
                  CALL lpropd(gprop)
!
                  DO ll = 1 , 3
                     DO mm = 1 , 3
                        ii = mm + 3*(ll-1)
                        abbd(ll,mm+3) = gprop(ii)*tlam*tlam
                        abbd(ll+3,mm) = gprop(ii)*tlam*tlam
                     ENDDO
                  ENDDO
               ENDIF
            ENDIF
!
!     REFERENCE SURFACE
!
            zref = -tlam/2.0D0
!
!     NUMBER OF LAYERS
!
            nlay = intz(pidloc+1)
!
!     SET POINTER
!
            IF ( itype==pcomp ) ipoint = (pidloc+8+4*nlay)
            IF ( itype==pcomp1 ) ipoint = (pidloc+8+nlay)
            IF ( itype==pcomp2 ) ipoint = (pidloc+8+2*nlay)
!
!     ALLOW FOR THE ORIENTATION OF THE MATERIAL AXIS FROM
!     THE ELEMENT AXIS
!
            thetae = datan(tem(2)/tem(1))
            thetae = thetae*Degrad
!
!     LAMINATE REFERENCE (OR LAMINATION) TEMPERATURE
!
            tsubo = Z(ipoint+24)
!
            IF ( tempp1 .OR. tempp2 ) THEN
!
               tmean = Stemp(1)
            ELSE
               tmean = tempel
            ENDIF
!
            delta = tmean - tsubo
!
            DO ll = 1 , 6
               ftherm(ll) = 0.0D0
            ENDDO
!
!     ALLOW FOR APPLIED THERMAL MOMENTS
!
            IF ( tempp2 ) THEN
!
               DO ll = 1 , 3
                  ftherm(ll+3) = thrmom(ll)
               ENDDO
            ENDIF
!
!     LOOP OVER NLAY
!
            DO k = 1 , nlay
!
               zk1 = zk
               IF ( k==1 ) zk1 = zref
               IF ( itype==pcomp ) zk = zk1 + Z(pidloc+6+4*k)
               IF ( itype==pcomp1 ) zk = zk1 + Z(pidloc+7)
               IF ( itype==pcomp2 ) zk = zk1 + Z(pidloc+7+2*k)
!
               zsubi = (zk+zk1)/2.0D0
!
!     LAYER THICKNESS
!
               ti = zk - zk1
!
!     LAYER ORIENTATION
!
               IF ( itype==pcomp ) theta = Z(pidloc+7+4*k)
               IF ( itype==pcomp1 ) theta = Z(pidloc+8+k)
               IF ( itype==pcomp2 ) theta = Z(pidloc+8+2*k)
!
!
               theta = theta*Degrad
!
               IF ( thetae>0.0D0 ) theta = theta + thetae
!
               c = dcos(theta)
               c2 = c*c
               s = dsin(theta)
               s2 = s*s
!
               transl(1) = c2
               transl(2) = s2
               transl(3) = c*s
               transl(4) = s2
               transl(5) = c2
               transl(6) = -c*s
               transl(7) = -2.0D0*c*s
               transl(8) = 2.0D0*c*s
               transl(9) = c2 - s2
!
!     CALCULATE GBAR = TRANST X GLAY X TRANS
!
               DO ir = 1 , 9
                  glay(ir) = Z(ipoint+ir)
               ENDDO
!
               CALL gmmatd(glay(1),3,3,0,transl(1),3,3,0,glayt(1))
               CALL gmmatd(transl(1),3,3,1,glayt(1),3,3,0,gbar(1))
!
!     CALCULATE ALPHAE = TRANSL X ALPHA
!
!     MODIFY TRANSL FOR TRANSFORMATIONS OF ALPHAS
!
               transl(3) = -transl(3)
               transl(6) = -transl(6)
               transl(7) = -transl(7)
               transl(8) = -transl(8)
!
               DO ir = 1 , 3
                  alphal(ir) = Z(ipoint+13+ir)
               ENDDO
!
               CALL gmmatd(transl(1),3,3,0,alphal(1),3,1,0,alphae(1))
!
!     CALCULATE LAMINATE OPERATING TEMPERATURE (ALLOWING FOR
!     TEMPERATURE GRADIENT IF APPLIED)
!
               deltat = delta
               IF ( tempp1 ) deltat = delta + zsubi*tgrad
!
!     CALCULATE THERMAL FORCES AND MOMENTS
!
               CALL gmmatd(gbar(1),3,3,0,alphae(1),3,1,0,galpha(1))
!
               DO ir = 1 , 3
                  ftherm(ir) = ftherm(ir) + galpha(ir)*deltat*(zk-zk1)
                  IF ( lamopt/=mem .AND. lamopt/=symmem ) ftherm(ir+3) = ftherm(ir+3) - galpha(ir)*deltat*((zk**2)-(zk1**2))/2.0D0
               ENDDO
!
               IF ( lamopt==sym .OR. lamopt==symmem ) THEN
!
!     CALCULATE CONTRIBUTION FROM SYMMETRIC LAYERS
!
                  deltat = delta
                  IF ( tempp1 ) deltat = delta - zsubi*tgrad
!
                  DO ir = 1 , 3
                     ftherm(ir) = ftherm(ir) + galpha(ir)*deltat*(zk-zk1)
                     IF ( lamopt/=symmem ) ftherm(ir+3) = ftherm(ir+3) - galpha(ir)*deltat*((zk1**2)-(zk**2))/2.0D0
                  ENDDO
               ENDIF
!
               IF ( itype==pcomp ) ipoint = ipoint + 27
!
            ENDDO
!
!     COMPUTE THERMAL STRAIN VECTOR
!                 -1
!     EPSLN = ABBD   X FTHERM
!
            CALL inverd(6,abbd,6,dum,0,determ,ising,indx)
!
            DO ll = 1 , 6
               DO mm = 1 , 6
                  nn = mm + 6*(ll-1)
                  stiff(nn) = abbd(ll,mm)
               ENDDO
            ENDDO
!
            CALL gmmatd(stiff(1),6,6,0,ftherm(1),6,1,0,epslnt(1))
         ENDIF
!
!     INITIALIZE NECESSARY ARRAYS BEFORE STARTING THE
!     DOUBLE INTEGRATION LOOP
!
         DO i = 1 , 9
            g2(i) = 0.0D0
         ENDDO
         DO i = 1 , 6
            epsubt(i) = 0.0D0
         ENDDO
         DO i = 1 , ndof
            pt(i) = 0.0D0
            ptg(i) = 0.0D0
         ENDDO
!
!     FILL IN THE 6X6 MATERIAL PROPERTY MATRIX G
!
         DO ig = 1 , 6
            DO jg = 1 , 6
               g(ig,jg) = 0.0D0
            ENDDO
         ENDDO
!
         IF ( Membrn ) THEN
            DO ig = 1 , 3
               ig1 = (ig-1)*3
               DO jg = 1 , 3
                  jg1 = jg + ig1
                  g(ig,jg) = gi(jg1)
               ENDDO
            ENDDO
         ENDIF
!
         IF ( Bendng ) THEN
            i = 0
            DO ig = 4 , 6
               ig2 = (ig-2)*3
               DO jg = 4 , 6
                  jg2 = jg + ig2
                  g(ig,jg) = gi(jg2)*mominr
!
!     SAVE THE G-MATRIX FOR BENDING IN G2
!
                  i = i + 1
                  g2(i) = g(ig,jg)
               ENDDO
            ENDDO
!
            IF ( Membrn ) THEN
               IF ( .NOT.(Mbcoup) ) THEN
                  DO ig = 1 , 3
                     ig1 = (ig-1)*3
                     kg = ig + 3
                     DO jg = 1 , 3
                        jg1 = jg + ig1
                        lg = jg + 3
                        g(ig,lg) = gi(jg1)
                        g(kg,jg) = gi(jg1)
                     ENDDO
                  ENDDO
               ENDIF
            ENDIF
         ENDIF
!
!     HERE BEGINS THE DOUBLE LOOP ON STATEMENT 1470 TO
!     GAUSS INTEGRATE FOR THE ELEMENT STIFFNESS MATRIX.
!
         DO ixsi = 1 , 2
            xi = ptint(ixsi)
!
            DO ieta = 1 , 2
               eta = ptint(ieta)
!
               CALL q4shpd(xi,eta,shp,dshp)
!
!     SORT THE SHAPE FUNCTIONS AND THEIR DERIVATIVES INTO SIL ORDER.
!
               DO i = 1 , 4
                  tmpshp(i) = shp(i)
                  dshptp(i) = dshp(i)
                  dshptp(i+4) = dshp(i+4)
               ENDDO
               DO i = 1 , 4
                  kk = Iorder(i)
                  shp(i) = tmpshp(kk)
                  dshp(i) = dshptp(kk)
                  dshp(i+4) = dshptp(kk+4)
               ENDDO
!
!     CALCULATE THE ELEMENT THICKNESS AT THIS POINT
!
               thk = 0.0D0
               DO i = 1 , Nnode
                  thk = thk + dgpth(i)*shp(i)
               ENDDO
               reali = thk*thk*thk/12.0D0
!
!     CALCULATE T-BAR FOR THIS INTEGRATION POINT. SKIP OVER IF TEMPPI
!     CARDS ARE PRESENT, THEN CALCULATE ALPHA*T FOR EACH CASE
!
               IF ( .NOT.(compos) ) THEN
!
                  IF ( .NOT.(tempp1 .OR. tempp2) ) THEN
                     tbar = 0.0D0
                     DO i = 1 , Nnode
                        tbar = tbar + shp(i)*gtemps(i)
                     ENDDO
                  ENDIF
!
                  ttbar = tbar - tsub0
                  IF ( Membrn ) THEN
                     DO i = 1 , 3
                        talfam(i) = ttbar*alfam(i)
                     ENDDO
                  ENDIF
!
                  IF ( Bendng ) THEN
                     IF ( .NOT.(.NOT.tempp1 .AND. .NOT.tempp2) ) THEN
                        IF ( tempp2 ) THEN
!
                           DO ig2 = 1 , 9
                              g2i(ig2) = g2(ig2)*reali
                           ENDDO
                           CALL inverd(3,g2i,3,gdum,0,detg2,isngg2,index)
                           CALL gmmatd(g2i,3,3,0,thrmom,3,1,0,talfab)
                        ELSE
                           DO i = 1 , 3
                              talfab(i) = -tgrad*alfab(i)
                           ENDDO
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
!
!     START THE THIRD INTEGRATION LOOP (THRU THE THICKNESS)
!
               DO izta = 1 , 2
                  zta = ptint(izta)
                  Hzta = zta/2.0D0
                  ibot = (izta-1)*nd2
!
                  CALL jacob2(elid,shp,dshp,dgpth,egpdt,epnorm,jacob)
                  IF ( Badjac ) GOTO 300
!
                  CALL gmmatd(Psitrn,3,3,0,jacob,3,3,1,phi)
!
!     CALL Q4BMGD TO GET B MATRIX
!     SET THE ROW FLAG TO 3. IT WILL RETURN THE FIRST 6 ROWS.
!
                  Rowflg = 3
                  CALL q4bmgd(dshp,dgpth,egpdt,epnorm,phi,bmatrx(1))
                  DO ix = 1 , ndof
                     bmatrx(ix+nd2) = xybmat(ibot+ix)
                  ENDDO
!
                  IF ( Bendng ) THEN
                     DO ix = 1 , ndof
                        bmatrx(ix+nd5) = xybmat(ibot+ix+ndof)
                     ENDDO
!
!     NOW COMPLETE THE G-MATRIX IF COUPLING EXISTS.
!
                     IF ( Mbcoup ) THEN
                        DO ig = 1 , 3
                           ig4 = (ig+8)*3
                           kg = ig + 3
                           DO jg = 1 , 3
                              jg4 = jg + ig4
                              jg1 = jg4 - 27
                              lg = jg + 3
                              g(ig,lg) = -gi(jg4)*zta*6.0D0 + gi(jg1)
                              g(kg,jg) = -gi(jg4)*zta*6.0D0 + gi(jg1)
                           ENDDO
                        ENDDO
                     ENDIF
                  ENDIF
!
!     MULTIPLY DETERMINANT, B-TRANSPOSE, G-MATRIX, & THERMAL
!     STRAIN MATRIX.
!                         T
!         P  =  DETERM * B  * G * EPSILON
!          T                             T
!
                  IF ( compos ) THEN
!
                     DO ir = 1 , 3
                        epsubt(ir) = Detj*epslnt(ir)
                        epsubt(ir+3) = -Detj*epslnt(ir+3)*thk*Hzta
                     ENDDO
                  ELSE
                     DO i = 1 , 3
                        epsubt(i) = Detj*talfam(i)
                        epsubt(i+3) = -Detj*talfab(i)*Hzta*thk
                     ENDDO
                  ENDIF
!
                  CALL gmmatd(g,6,6,0,epsubt,6,1,0,gepsbt)
                  CALL gmmatd(bmatrx,6,ndof,-1,gepsbt,6,1,0,pt)
!
               ENDDO
            ENDDO
         ENDDO
!
!     TRIPLE INTEGRATION LOOP IS NOW FINISHED
!
!     PICK UP THE BASIC TO GLOBAL TRANSFORMATION FOR EACH NODE.
!
         DO i = 1 , 36
            trans(i) = 0.0D0
         ENDDO
!
         DO i = 1 , Nnode
            ipoint = 9*(i-1) + 1
            IF ( igpdt(1,i)<=0 ) THEN
               DO j = 1 , 9
                  tbg(j) = 0.0D0
               ENDDO
               tbg(1) = 1.0D0
               tbg(5) = 1.0D0
               tbg(9) = 1.0D0
            ELSE
               CALL transd(bgpdt(1,i),tbg)
            ENDIF
!
            CALL gmmatd(teb,3,3,0,tbg,3,3,0,trans(ipoint))
         ENDDO
!
!     TRANSFORM THE THERMAL LOAD VECTOR INTO THE INDIVIDUAL
!     GLOBAL COORDINATE SYSTEMS OF EACH NODE. NOTE THAT THE
!     TRANSFORMATION MATRICES ARE STORED IN  TRANS = TEG,
!     AND THAT THE 6-DOF LOAD VECTOR FOR EACH NODE USES THE
!     SAME 3X3 TRANSFORMATION MATRIX FOR THE TRANSLATIONAL
!     DOF'S (1-3) AND THE ROTATIONAL DOF'S (4-6).
!
!                        T
!              PT  =  TEG   *  PT
!                G               E
!
         DO i = 1 , Nnode
            ipt = (i-1)*9 + 1
            jpt1 = (i-1)*6 + 1
            jpt2 = jpt1 + 3
            CALL gmmatd(trans(ipt),3,3,1,pt(jpt1),3,1,0,ptg(jpt1))
            CALL gmmatd(trans(ipt),3,3,1,pt(jpt2),3,1,0,ptg(jpt2))
         ENDDO
!
!     WE NOW HAVE THE THERMAL LOAD VECTOR IN GLOBAL COORDINATES,
!     IN PTG. THE NEXT AND LAST STEP IS TO COMBINE IT WITH THE
!     SYSTEM LOAD VECTOR CONTAINED IN Z.
!
         l = 0
         DO i = 1 , Nnode
            k = sil(i) - 1
            DO j = 1 , 6
               k = k + 1
               l = l + 1
               Z(k) = Z(k) + sngl(ptg(l))
            ENDDO
         ENDDO
         GOTO 300
      ELSE
         Matid = mid(m)
         IF ( Matid/=0 ) THEN
            IF ( m<1 ) THEN
            ELSEIF ( m==1 ) THEN
               CALL mat(elid)
            ELSEIF ( Matid/=mid(m-1) ) THEN
               CALL mat(elid)
            ENDIF
!
            tsub0 = Rmtout(11)
            IF ( matset==8.0 ) tsub0 = Rmtout(10)
!
            coeff = 1.0D0
            lpoint = (m-1)*9 + 1
!
            CALL q4gmgd(m,coeff,gi(lpoint))
!
            IF ( thetam/=0.0D0 ) THEN
!
               u(1) = tem(1)*tem(1)
               u(2) = tem(4)*tem(4)
               u(3) = tem(1)*tem(4)
               u(4) = tem(2)*tem(2)
               u(5) = tem(5)*tem(5)
               u(6) = tem(2)*tem(5)
               u(7) = tem(1)*tem(2)*2.0D0
               u(8) = tem(4)*tem(5)*2.0D0
               u(9) = tem(1)*tem(5) + tem(2)*tem(4)
               l = 3
!
               CALL gmmatd(u(1),l,l,1,gi(lpoint),l,l,0,gt(1))
               CALL gmmatd(gt(1),l,l,0,u(1),l,l,0,gi(lpoint))
            ENDIF
!
            IF ( .NOT.(compos) ) THEN
!
!     TRANSFORM THERMAL EXPANSION COEFFICIENTS AND STORE THEM IN ALPHA
!
               IF ( m<=2 ) THEN
                  morb = (m-1)*3
                  IF ( matset==2.0 ) THEN
!
!     MAT2
!
                     DO imat = 1 , 3
                        alpham(imat+morb) = Rmtout(7+imat)
                     ENDDO
                  ELSEIF ( matset==8.0 ) THEN
!
!     MAT8
!
                     alpham(morb+1) = Rmtout(8)
                     alpham(morb+2) = Rmtout(9)
                     alpham(morb+3) = 0.0
                  ELSE
!
!     MAT1
!
                     DO imat = 1 , 2
                        alpham(imat+morb) = Rmtout(8)
                     ENDDO
                     alpham(3+morb) = 0.0
                  ENDIF
!
!     SKIP THE TRANSFORMATION OF ALPHAM IF MATSET = 1. OR THETAM = 0.D0
!
                  IF ( matset/=1.0 ) THEN
                     IF ( thetam/=0.0D0 ) THEN
!
!     THE ALPHAS NEED TO BE PREMULTIPLIED BY U INVERSE. INCREMENT MORB
!     BY 1 TO INDICATE WHERE TO FILL THE ARRAYS, AND PUT THE SINGLE
!     PREC. ARRAY OF ALPHAM INTO THE DOUBLE PREC. ARRAY OF ALPHAD FOR
!     THE CALL TO GMMATD.
!
                        morb = morb + 1
                        DO i = 1 , 6
                           alphad(i) = alpham(i)
                        ENDDO
                        CALL inverd(3,u,3,bdum,0,detu,isngu,index)
                        CALL gmmatd(u,3,3,0,alphad(morb),3,1,0,alpha(morb))
                        CYCLE
                     ENDIF
                  ENDIF
!
                  DO ig = 1 , 3
                     alpha(ig+morb) = alpham(ig+morb)
                  ENDDO
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDDO
!
 200  CALL mesage(30,j,nam)
   nogo = 1
 300  RETURN
99002 FORMAT (//,' ILLEGAL GEOMETRY FOR QUAD4 ELEMENT, ID=',I10)
END SUBROUTINE tlqd4d
