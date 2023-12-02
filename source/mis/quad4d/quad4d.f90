!*==quad4d.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE quad4d
   IMPLICIT NONE
   USE c_cjacob
   USE c_comjac
   USE c_condad
   USE c_emgdic
   USE c_emgest
   USE c_emgprm
   USE c_hmtout
   USE c_matin
   USE c_matout
   USE c_q4comd
   USE c_q4dt
   USE c_system
   USE c_terms
   USE c_trplm
   USE c_xmssg
   USE c_zzzzzz
!
! Local variable declarations rewritten by SPAG
!
   REAL*8 :: a , aa , area , area2 , aspctx , aspcty , aspect , avgthk , b , bb , cc , coeff , csubb4 , csubt , csubtx , csubty ,   &
           & csubx , csuby , dheat , dnux , dnuy , dsub , dsub4 , dvol , enorx , enory , exi , exj , gnorx , gnory , mominr ,       &
           & nunorx , nunory , offset , psiinx , psiiny , reali , rho , rhox , sfctr1 , sfctr2 , sfctx1 , sfctx2 , sfcty1 , sfcty2 ,&
           & sineax , sineay , temp , thetam , thk , thlen , ts , tsi , tsmfx , tsmfy , v12dk , vjl , vkl , vol , voli , vp12l ,    &
           & w1 , weitc , x31 , x42 , xm , xmasso , y31 , y42 , ym , zta
   REAL :: adamp , ax , by , coeft , elth , eps , epsi , epst , factor , gsube , htcp , matset , nsm , pa , sysbuf , tsfact , zoff ,&
         & zoff1
   REAL*8 , DIMENSION(1) :: amgg
   LOGICAL :: anis , nocsub , nogo
   REAL*8 , DIMENSION(240) :: bfour
   REAL , DIMENSION(3,4) :: bgpdm
   REAL , DIMENSION(4,4) :: bgpdt , egpdt , epnorm , gpnorm
   REAL*8 , DIMENSION(384) :: bmat1
   REAL*8 , DIMENSION(32) :: bterms
   REAL*8 , DIMENSION(3) :: cent , cente , curve , v12 , v41 , vd1 , vd2 , vis , vjs , vkn , vks , vp12
   REAL*8 , DIMENSION(144) :: colstf , coltmp
   REAL*8 , SAVE :: const , eps1
   REAL*8 , DIMENSION(3,4) :: curvtr , ugpdm , vnt
   REAL*8 , DIMENSION(7,7) :: dfour
   REAL*8 , DIMENSION(4) :: dgpth , shp , tmpshp , tmpthk , xa , yb , zc
   INTEGER , DIMENSION(9) :: dict
   REAL*8 , DIMENSION(8) :: dshp , dshptp
   REAL , DIMENSION(4) :: ecpt , gpth
   REAL*8 , DIMENSION(10,10) :: gfour
   REAL*8 , DIMENSION(9) :: gge , ggu , gt , phi , tbg , tbm , teb , tem , teu , tmpmas , tub , tum , u
   REAL*8 , DIMENSION(36) :: gi , trans , trans1
   INTEGER , DIMENSION(8) :: horder , hsil
   REAL*8 , DIMENSION(16) :: htcap , htcon , xmass , xmtmp
   REAL*8 , DIMENSION(12) :: htflx
   INTEGER :: i , i1 , i4 , ibegin , ic , identt , idn , iec , ieoe , ieta , iflag , ifrom , ig , ig1 , ig2 , ig4 , igobk , ii ,    &
            & iicore , iipnt , ij , ijk , ik , im1 , int , ioj , iok , iord , ip , ip1 , ipoint , iprec , ir , is , isil , itemp ,  &
            & ixsi , izta , j , j4 , jcored , jg , jg1 , jg2 , jg4 , jj , jo , jpoint , jtemp , k , k1 , kg , kgg1 , kk , kpnt ,    &
            & kpoint , kpt , ktemp , l , lg , limit , lpoint , m , mattyp , mcsid , mgg1 , mpoint , mtype , ncored , nd2 , nd3 ,    &
            & nd4 , nd5 , nd6 , nd7 , ndof3 , ndofm1 , nodesq , nout , npart
   INTEGER , DIMENSION(4,4) :: iegpdt , igpdt
   INTEGER , DIMENSION(4) :: igpth , kcid , ksil , mid , necpt , notran , sil
   REAL*8 , DIMENSION(3,3) :: jacob , v
   INTEGER , DIMENSION(2) , SAVE :: nam
   INTEGER , DIMENSION(45) :: nest
   REAL*8 , DIMENSION(2) :: ptint
   REAL , DIMENSION(20) :: save
   REAL*8 , DIMENSION(4,4) :: tgrid
   REAL*8 , DIMENSION(96) :: xybmat
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     FORMS STIFFNESS AND MASS MATRICES FOR THE QUAD4 PLATE ELEMENT
!
!     DOUBLE PRECISION VERSION
!
!     EST  LISTING
!
!     WORD       TYPE         DESCRIPTION
!     --------------------------------------------------------------
!       1          I    ELEMENT ID, EID
!       2 THRU 5   I    SILS, GRIDS 1 THRU 4
!       6 THRU 9   R    MEMBRANE THICKNESSES T AT GRIDS 1 THRU 4
!      10          R    MATERIAL PROPERTY ORIENTATION ANGLE, THETA
!               OR I    COORD. SYSTEM ID (SEE TM ON CQUAD4 CARD)
!      11          I    TYPE FLAG FOR WORD 10
!      12          R    GRID ZOFF  (OFFSET)
!      13          I    MATERIAL ID FOR MEMBRANE, MID1
!      14          R    ELEMENT THICKNESS, T (MEMBRANE, UNIFORMED)
!      15          I    MATERIAL ID FOR BENDING, MID2
!      16          R    BENDING INERTIA FACTOR, I
!      17          I    MATERIAL ID FOR TRANSVERSE SHEAR, MID3
!      18          R    TRANSV. SHEAR CORRECTION FACTOR TS/T
!      19          R    NON-STRUCTURAL MASS, NSM
!      20 THRU 21  R    Z1, Z2  (STRESS FIBRE DISTANCES)
!      22          I    MATERIAL ID FOR MEMBRANE-BENDING COUPLING, MID4
!      23          R    MATERIAL ANGLE OF ROTATION, THETA
!               OR I    COORD. SYSTEM ID (SEE MCSID ON PSHELL CARD)
!      24          I    TYPE FLAG FOR WORD 23
!      25          I    INTEGRATION ORDER
!      26          R    STRESS ANGLE OF ROTATION, THETA
!               OR I    COORD. SYSTEM ID (SEE SCSID ON PSHELL CARD)
!      27          I    TYPE FLAG FOR WORD 26
!      28          R    ZOFF1 (OFFSET)  OVERRIDDEN BY EST(12)
!      29 THRU 44  I/R  CID,X,Y,Z - GRIDS 1 THRU 4
!      45          R    ELEMENT TEMPERATURE
!
!
!WKBD 2/94 SPR93020      DOUBLE PRECISION EIX,EIY,TGX,TGY
!WKBI 9/94 SPR93020
!WKBI 2/94 SPR93020
!
!     DOUBLE PRECISION PTINTZ(2),BMATRX(144),STRESR(240)
!
!     DATA FOR ADDING ELEMENT, USER AND MATERIAL COORDINATE SYSTEMS
!
!
!     DATA FOR ADDING CSUBB, MIDI, MATERIAL TRANS., AND HEAT
!
!WKBNB 11/93 SPR 93020
!WKBNE 11/93 SPR 93020
!
!     DATA FOR IRREGULAR 4-NODE
!
!
!     ICORE = FIRST WORD OF OPEN CORE
!     JCORE = NEXT AVAILABLE LOCATION IN OPEN CORE.
!     NCORE = CURRENT LAST AVAILABLE LOCATION IN OPEN CORE
!
!ZZ   COMMON /ZZEMGX/  AKGG(1)
   !>>>>EQUIVALENCE (Sys(01),Sysbuf) , (Sys(02),Nout) , (Sys(03),Nogo) , (Sys(55),Iprec)
!     EQUIVALENCE      (SYS(48) ,ICSUB4  ), (SYS(49) ,ICSUBB    ),
!    1                 (SYS(50) ,ICSUBT  ), (SYS(75) ,ICSUB8    )
   !>>>>EQUIVALENCE (Flags(1),Kgg1) , (Flags(2),Mgg1) , (adamp,dict(5)) , (igpth(1),Gpth(1)) , (Est(1),Nest(1)) , (Int,Nest(25)) ,       &
!>>>>    & (Elth,Est(14)) , (Gpth(1),Est(6)) , (Zoff,Est(12)) , (Zoff1,Est(28)) , (Sil(1),Nest(2)) , (Matset,Matout(25)) , (Nsm,Est(19)) &
!>>>>    & , (Amgg(1),Akgg(1)) , (Htcp,Kheat(4)) , (htflx(1),tmpmas(1)) , (htcap(1),xmass(1)) , (htcon(1),xmtmp(1)) , (necpt(1),ecpt(1)) &
!>>>>    & , (Bgpdt(1,1),Est(29)) , (iegpdt(1,1),egpdt(1,1)) , (Igpdt(1,1),Bgpdt(1,1))
   DATA eps1/1.0D-7/
   DATA const/0.57735026918962D0/
   DATA nam/4HQUAD , 4H4D  /
!
   elid = nest(1)
   ltypfl = 1
   offset = zoff
   IF ( zoff==0.0 ) offset = zoff1
!
!     CHECK FOR SUFFICIENT OPEN CORE FOR ELEMENT STIFFNESS
!
   jcored = jcore/iprec + 1
   ncored = ncore/iprec - 1
   IF ( (jcored+576)<=ncored .OR. heat .OR. kgg1==0 ) THEN
!
!     COPY THE SILS AND BGPDT DATA INTO SAVE ARRAY SINCE THE DATA
!     WILL BE REORDERED BASED ON INCREASING SILS.
!
      j = 1
      DO i = 1 , 20
         save(i) = est(i+j)
         IF ( i==4 ) j = 24
      ENDDO
!
      nnode = 4
      n1 = 4
      nodesq = nnode*nnode
      ndof = nnode*6
      ndof3 = nnode*3
      nd2 = ndof*2
      nd3 = ndof*3
      nd4 = ndof*4
      nd5 = ndof*5
      nd6 = ndof*6
      nd7 = ndof*7
!
!     FILL IN ARRAY GGU WITH THE COORDINATES OF GRID POINTS 1, 2 AND 4.
!     THIS ARRAY WILL BE USED LATER TO DEFINE THE USER COORD. SYSTEM
!     WHILE CALCULATING TRANSFORMATIONS INVOLVING THIS COORD. SYSTEM.
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
      IF ( vkl==0. ) WRITE (nout,99004) nest(1)
      vks(1) = vkn(1)/vkl
      vks(2) = vkn(2)/vkl
      vks(3) = vkn(3)/vkl
      pa = vkl/2.D0
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
      IF ( vp12l==0. ) WRITE (nout,99004) nest(1)
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
      IF ( vjl==0. ) WRITE (nout,99004) nest(1)
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
!     STORE INCOMING BGPDT FOR LUMPED MASS AND ELEMENT C.S.
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
            ugpdm(i,j) = 0.0D0
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
            cent(j) = cent(j) + ugpdm(j,i)/nnode
         ENDDO
      ENDDO
!
!     STORE THE CORNER NODE DIFF. IN THE USER C.S.
!
      x31 = ugpdm(1,3) - ugpdm(1,1)
      y31 = ugpdm(2,3) - ugpdm(2,1)
      x42 = ugpdm(1,4) - ugpdm(1,2)
      y42 = ugpdm(2,4) - ugpdm(2,2)
      aa = dsqrt(x31*x31+y31*y31)
      bb = dsqrt(x42*x42+y42*y42)
      IF ( aa==0.D0 .OR. bb==0.D0 ) GOTO 300
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
      DO i = 1 , 4
         iorder(i) = 0
         horder(i) = 0
         ksil(i) = sil(i)
         hsil(i) = sil(i)
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
         iorder(i) = itemp
         horder(i) = itemp
         ksil(itemp) = 99999999
      ENDDO
!
!     ADJUST EST DATA
!
!     USE THE POINTERS IN IORDER TO COMPLETELY REORDER THE
!     GEOMETRY DATA INTO INCREASING SIL ORDER.
!     DON'T WORRY!! IORDER ALSO KEEPS TRACK OF WHICH SHAPE
!     FUNCTIONS GO WITH WHICH GEOMETRIC PARAMETERS!
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
         ipoint = iorder(i)
         sil(i) = ksil(ipoint)
         gpth(i) = tmpthk(ipoint)
         igpdt(1,i) = kcid(ipoint)
         DO j = 2 , 4
            bgpdt(j,i) = tgrid(j,ipoint)
         ENDDO
      ENDDO
!
!     COMPUTE NODE NORMALS
!
      CALL q4nrmd(bgpdt,gpnorm,iorder,iflag)
      IF ( iflag/=0 ) GOTO 300
!
!     DETERMINE NODAL THICKNESSES
!
      avgthk = 0.0D0
      DO i = 1 , nnode
         iord = iorder(i)
         DO ic = 1 , 3
            curvtr(ic,iord) = gpnorm(ic+1,i)
         ENDDO
!
         IF ( gpth(i)==0.0 ) gpth(i) = elth
         IF ( nest(13)==0 .AND. elth==0.0 ) gpth(i) = 1.0E-14
         IF ( gpth(i)>0.0 ) THEN
            dgpth(i) = gpth(i)
            avgthk = avgthk + dgpth(i)/nnode
         ELSE
            WRITE (nout,99001) ufm , elid
!
99001       FORMAT (A23,', THE ELEMENT THICKNESS FOR QUAD4 EID =',I9,' IS NOT COMPLETELY DEFINED.')
            nogo = .TRUE.
            GOTO 400
         ENDIF
      ENDDO
!
!     NEST(13) = MID1 ID FOR MEMBRANE
!     NEST(15) = MID2 ID FOR BENDING
!     NEST(17) = MID3 ID FOR TRANSVERSE SHEAR
!     NEST(22) = MID4 ID FOR MEMBRANE-BENDING COUPLING
!                MID4 MUST BE BLANK UNLESS MID1 AND MID2 ARE NON-ZERO
!                MID4 ID MUST NOT EQUAL MID1 OR MID2 ID
!     (WHEN LAYER COMPOSITE IS USED, MID ID IS RAISED TO ID*100000000)
!      EST(14) = MEMBRANE THICKNESS, T
!      EST(16) = BENDING STIFFNESS PARAMETER, 12I/T**3
!      EST(18) = TRANSVERSE SHEAR  PARAMETER, TS/T
!
!     0.8333333 = 5.0/6.0
!
      mominr = 0.0D0
      tsfact = .8333333
      nocsub = .FALSE.
      IF ( nest(15)/=0 ) mominr = est(16)
      IF ( nest(17)/=0 ) ts = est(18)
      IF ( est(18)==.0 ) ts = .833333D0
!
!     FIX FOR LAMINATED COMPOSITE WITH MEMBRANE BEHAVIOUR ONLY.
!     REQUIRED TO PREVENT ZERO DIVIDE ERRORS.
!
      IF ( nest(15)==0 .AND. nest(13)>100000000 ) ts = .833333D0
!
!     SET LOGICAL NOCSUB IF EITHER MOMINR OR TS ARE NOT DEFAULT
!     VALUES. THIS WILL BE USED TO OVERRIDE ALL CSUBB COMPUTATIONS.
!     I.E. DEFAULT VALUES OF UNITY ARE USED.
!
      epsi = abs(mominr-1.0)
      epst = abs(ts-tsfact)
      eps = .05
!     NOCSUB = EPSI.GT.EPS .OR. EPST.GT.EPS
      IF ( nest(13)>100000000 ) nocsub = .FALSE.
!
!     THE COORDINATES OF THE ELEMENT GRID POINTS HAVE TO BE
!     TRANSFORMED FROM THE BASIC C.S. TO THE ELEMENT C.S.
!
      CALL betrnd(teu,gge,0,elid)
      CALL gmmatd(teu,3,3,0,tub,3,3,0,teb)
      CALL gmmatd(tub,3,3,1,cent,3,1,0,cente)
      identt = 0
      IF ( teb(1)==1.D0 .AND. teb(5)==1.D0 .AND. teb(9)==1.D0 .AND. teb(2)==0.D0 .AND. teb(3)==0.D0 .AND. teb(4)==0.D0 .AND. teb(6) &
         & ==0.D0 .AND. teb(7)==0.D0 .AND. teb(8)==0.D0 ) identt = 1
      ip = -3
      DO ii = 2 , 4
         ip = ip + 3
         DO j = 1 , nnode
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
!
!     BEGIN INITIALIZING MATERIAL VARIABLES
!
!     SET INFLAG = 12 SO THAT SUBROUTINE MAT WILL SEARCH FOR-
!     ISOTROPIC MATERIAL PROPERTIES AMONG THE MAT1 CARDS,
!     ORTHOTROPIC MATERIAL PROPERTIES AMONG THE MAT8 CARDS, AND
!     ANISOTROPIC MATERIAL PROPERTIES AMONG THE MAT2 CARDS.
!
      inflag = 12
      rho = 0.0D0
      eltemp = est(45)
      mid(1) = nest(13)
      mid(2) = nest(15)
      mid(3) = nest(17)
      mid(4) = nest(22)
      membrn = mid(1)>0
      bendng = mid(2)>0 .AND. mominr>0.0D0
      shrflx = mid(3)>0
      mbcoup = mid(4)>0
!
!     FIGURE OUT PATH OF THE TRIPLE MULTIPLY AND THE NO. OF ROWS IN
!     THE B-MATRIX (I.E. STRAIN-NODAL DISPLACEMENT MATRIX)
!
!     NORPTH = MID(1).EQ.MID(2) .AND. MID(1).EQ.MID(3) .AND. MID(4).EQ.0
!    1        .AND. DABS(MOMINR-1.0D0).LE.EPS1
!
      norpth = .FALSE.
!
!     DETERMINE FACTORS TO BE USED IN CSUBB CALCULATIONS
!
!     IF (.NOT.BENDNG) GO TO 290
      DO i = 1 , 4
         DO j = 1 , nnode
            jo = iorder(j)
            IF ( i==jo ) THEN
               xa(i) = egpdt(2,j)
               yb(i) = egpdt(3,j)
               zc(i) = egpdt(4,j)
               vnt(1,i) = epnorm(2,j)
               vnt(2,i) = epnorm(3,j)
               vnt(3,i) = epnorm(4,j)
            ENDIF
         ENDDO
      ENDDO
!
      a = 0.5D0*dabs(xa(2)+xa(3)-xa(1)-xa(4))
      b = 0.5D0*dabs(yb(4)+yb(3)-yb(1)-yb(2))
      IF ( a>b ) aspect = b/a
      IF ( a<=b ) aspect = a/b
      thlen = avgthk/a
      IF ( a<b ) thlen = avgthk/b
!
!     TORSION-RELATED SHEAR CORRECTION FOR 4-NODE-
!     PRELIMINARY FACTORS
!
      aspctx = a/b
      aspcty = b/a
      csubb4 = 1.6D0
      csubt = 71.D0*aspect*(1.6D0/csubb4)*(1.D0+415.D0*aspect*thlen**2)
      csubtx = csubt*aspctx**2
      csubty = csubt*aspcty**2
!
      i = 2
      j = 2
      jj = 3
      sineax = 0.0D0
      sineay = 0.0D0
   ELSE
      CALL mesage(-30,234,nam)
      GOTO 600
   ENDIF
   DO
      CALL daxb(curvtr(1,i-1),curvtr(1,i),curve)
      cc = curve(1)*curve(1) + curve(2)*curve(2) + curve(3)*curve(3)
      IF ( cc>=eps1 ) cc = 0.5D0*dsqrt(cc)
      sineax = sineax + cc
      IF ( i/=2 ) EXIT
      i = 4
   ENDDO
   DO
!
      CALL daxb(curvtr(1,j),curvtr(1,jj),curve)
      cc = curve(1)*curve(1) + curve(2)*curve(2) + curve(3)*curve(3)
      IF ( cc>=eps1 ) cc = 0.5D0*dsqrt(cc)
      sineay = sineay + cc
      IF ( j/=2 ) THEN
         cc = 28.0D0
         sineax = cc*sineax + 1.0D0
         sineay = cc*sineay + 1.0D0
         IF ( sineax>sineay ) sineay = sineax
         IF ( sineay>sineax ) sineax = sineay
!
!     IRREGULAR 4-NODE CODE-  GEOMETRIC VARIABLES
!
!     CALCULATE AND NORMALIZE- UNIT EDGE VECTORS, UNIT NORMAL VECTORS
!
         DO i = 1 , 4
            j = i + 1
            IF ( j==5 ) j = 1
            uev(1,i) = xa(j) - xa(i)
            uev(2,i) = yb(j) - yb(i)
            uev(3,i) = zc(j) - zc(i)
            unv(1,i) = (vnt(1,j)+vnt(1,i))*0.50D0
            unv(2,i) = (vnt(2,j)+vnt(2,i))*0.50D0
            unv(3,i) = (vnt(3,j)+vnt(3,i))*0.50D0
            cc = uev(1,i)**2 + uev(2,i)**2 + uev(3,i)**2
            IF ( cc==0.D0 ) GOTO 300
            IF ( cc>=eps1 ) cc = dsqrt(cc)
            edgel(i) = cc
            uev(1,i) = uev(1,i)/cc
            uev(2,i) = uev(2,i)/cc
            uev(3,i) = uev(3,i)/cc
            cc = unv(1,i)**2 + unv(2,i)**2 + unv(3,i)**2
            IF ( cc==0.D0 ) GOTO 300
            IF ( cc>=eps1 ) cc = dsqrt(cc)
            unv(1,i) = unv(1,i)/cc
            unv(2,i) = unv(2,i)/cc
            unv(3,i) = unv(3,i)/cc
         ENDDO
!
!     CALCULATE INTERNAL NODAL ANGLES
!
         DO i = 1 , 4
            j = i - 1
            IF ( j==0 ) j = 4
            anglei(i) = -uev(1,i)*uev(1,j) - uev(2,i)*uev(2,j) - uev(3,i)*uev(3,j)
            IF ( dabs(anglei(i))<eps1 ) anglei(i) = 0.0D0
         ENDDO
!
!     SET THE INTEGRATION POINTS
!
! 290 CONTINUE
         ptint(1) = -const
         ptint(2) = const
!     PTINTZ(1) = -CONST
!     PTINTZ(2) =  CONST
!     JZTA = 2
!     IF (.NOT.BENDNG) PTINTZ(1) = 0.0D0
!     IF (.NOT.BENDNG) JZTA = 1
         IF ( heat ) GOTO 600
!
!     TRIPLE LOOP TO SAVE THE LAST 2 ROWS OF B-MATRIX AT 2X2X2
!     INTEGRATION POINTS FOR LATER MANIPULATION.
!
         IF ( kgg1/=0 ) THEN
!     IF (.NOT.BENDNG) GO TO 360
            i = 1
            kpt = 1
!
            DO ixsi = 1 , 2
               xi = ptint(ixsi)
!
               DO ieta = 1 , 2
                  eta = ptint(ieta)
!
                  CALL q4shpd(xi,eta,shp,dshp)
!
!     IRREGULAR 4-NODE CODE-  CALCULATION OF NODAL EDGE SHEARS
!                             AT THIS INTEGRATION POINT
!
                  DO ij = 1 , 4
                     ii = ij - 1
                     IF ( ii==0 ) ii = 4
                     ik = ij + 1
                     IF ( ik==5 ) ik = 1
                     aa = shp(ij)
                     bb = shp(ik)
!
                     DO is = 1 , 3
                        edgshr(is,ij) = (uev(is,ij)+anglei(ij)*uev(is,ii))*aa/(1.0D0-anglei(ij)*anglei(ij))                         &
                                      & + (uev(is,ij)+anglei(ik)*uev(is,ik))*bb/(1.0D0-anglei(ik)*anglei(ik))
                     ENDDO
                  ENDDO
!
!     SORT THE SHAPE FUNCTIONS AND THEIR DERIVATIVES INTO SIL ORDER.
!
                  DO is = 1 , 4
                     tmpshp(is) = shp(is)
                     dshptp(is) = dshp(is)
                     dshptp(is+4) = dshp(is+4)
                  ENDDO
                  DO is = 1 , 4
                     kk = iorder(is)
                     shp(is) = tmpshp(kk)
                     dshp(is) = dshptp(kk)
                     dshp(is+4) = dshptp(kk+4)
                  ENDDO
!
                  DO izta = 1 , 2
                     zta = ptint(izta)
!
!     COMPUTE THE JACOBIAN AT THIS GAUSS POINT,
!     ITS INVERSE AND ITS DETERMINANT.
!
                     hzta = zta/2.0D0
                     CALL jacob2(elid,shp,dshp,dgpth,egpdt,epnorm,jacob)
                     IF ( badjac ) GOTO 400
!
!     COMPUTE PSI TRANSPOSE X JACOBIAN INVERSE.
!     HERE IS THE PLACE WHERE THE INVERSE JACOBIAN IS FLAGED TO BE
!     TRANSPOSED BECAUSE OF OPPOSITE MATRIX LOADING CONVENTION
!     BETWEEN INVER AND GMMAT.
!
                     CALL gmmatd(psitrn,3,3,0,jacob,3,3,1,phi)
!
!     CALL Q4BMGD TO GET B MATRIX
!     SET THE ROW FLAG TO 1. IT SIGNALS SAVING THE LAST 2 ROWS.
!
                     rowflg = 1
                     CALL q4bmgd(dshp,dgpth,egpdt,epnorm,phi,bmat1(kpt))
                     kpt = kpt + nd2
                  ENDDO
               ENDDO
            ENDDO
!
!     IN PLANE SHEAR REDUCTION
!
!     IF (.NOT.MEMBRN) GO TO 400
! 360 CONTINUE
            xi = 0.0D0
            eta = 0.0D0
            kpt = 1
            kpnt = nd2
!     IF (NORPTH) KPNT = NDOF
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
               kk = iorder(i)
               shp(i) = tmpshp(kk)
               dshp(i) = dshptp(kk)
               dshp(i+4) = dshptp(kk+4)
            ENDDO
!
!     DO 390 IZTA = 1,JZTA
            DO izta = 1 , 2
!     ZTA  = PTINTZ(IZTA)
               zta = ptint(izta)
               hzta = zta/2.0D0
               CALL jacob2(elid,shp,dshp,dgpth,egpdt,epnorm,jacob)
               IF ( badjac ) GOTO 400
!
               CALL gmmatd(psitrn,3,3,0,jacob,3,3,1,phi)
!
!     CALL Q4BMGD TO GET B-MATRIX
!     SET THE ROW FLAG TO 2. IT WILL SAVE THE 3RD ROW OF B-MATRIX AT
!     THE TWO INTEGRATION POINTS.
!
               rowflg = 2
               CALL q4bmgd(dshp,dgpth,egpdt,epnorm,phi,xybmat(kpt))
               kpt = kpt + kpnt
            ENDDO
         ENDIF
         EXIT
      ELSE
         j = 1
         jj = 4
      ENDIF
   ENDDO
!
!     SET THE ARRAY OF LENGTH 4 TO BE USED IN CALLING TRANSD.
!     NOTE THAT THE FIRST WORD IS THE COORDINATE SYSTEM ID WHICH
!     WILL BE SET IN POSITION LATER.
!
 100  DO iec = 2 , 4
      ecpt(iec) = 0.0
   ENDDO
!
!     FETCH MATERIAL PROPERTIES
!
!
!     EACH MATERIAL PROPERTY MATRIX G HAS TO BE TRANSFORMED FROM
!     THE MATERIAL COORDINATE SYSTEM TO THE ELEMENT COORDINATE
!     SYSTEM. THESE STEPS ARE TO BE FOLLOWED-
!
!     1- IF MCSID HAS BEEN SPECIFIED, SUBROUTINE TRANSD IS CALLED
!        TO CALCULATE TBM-MATRIX (MATERIAL TO BASIC TRANSFORMATION).
!        TBM-MATRIX IS THEN PREMULTIPLIED BY TEB-MATRIX TO OBTAIN
!        TEM-MATRIX.
!        THEN USING THE PROJECTION OF X-AXIS, AN ANGLE IS CALCULATED
!        UPON WHICH STEP 2 IS TAKEN.
!
!     2- IF THETAM HAS BEEN SPECIFIED, SUBROUTINE ANGTRD IS CALLED
!        TO CALCULATE TEM-MATRIX (MATERIAL TO ELEMENT TRANSFORMATION).
!
!                          T
!     3-           G  =   U   G   U
!                   E          M
!
!
   IF ( nest(11)==0 ) THEN
!
!     CALCULATE TEM-MATRIX USING THETAM
!
      thetam = dble(est(10))*degrad
!     IF (THETAM .EQ. 0.0D0) GO TO 490
      IF ( thetam/=0.0D0 ) GOTO 200
!
!     DEFAULT IS CHOSEN, LOOK FOR VALUES OF MCSID AND/OR THETAM
!     ON THE PSHELL CARD.
!
      IF ( nest(24)==0 ) THEN
!
         thetam = dble(est(23))*degrad
         GOTO 200
      ELSE
         mcsid = nest(23)
      ENDIF
   ELSE
      mcsid = nest(10)
   ENDIF
!
!     CALCULATE TEM-MATRIX USING MCSID
!
   IF ( mcsid>0 ) THEN
      necpt(1) = mcsid
      CALL transd(ecpt,tbm)
!
!     MULTIPLY TEB AND TBM MATRICES
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
   xm = tem(1)
   ym = tem(4)
   IF ( dabs(xm)>eps1 .OR. dabs(ym)>eps1 ) THEN
      thetam = datan2(ym,xm)
   ELSE
      nest(2) = mcsid
      j = 231
      GOTO 500
   ENDIF
 200  CALL angtrd(thetam,1,tum)
   CALL gmmatd(teu,3,3,0,tum,3,3,0,tem)
!
   IF ( heat ) THEN
      tem(3) = tem(4)
      tem(4) = tem(5)
      CALL gmmatd(tem,2,2,0,gi,2,2,0,gt)
      CALL gmmatd(gt,2,2,0,tem,2,2,1,gi)
      GOTO 700
   ELSE
!
      DO m = 1 , 36
         gi(m) = 0.0D0
      ENDDO
      sinmat = 0.
      cosmat = 0.
      igobk = 0
!
!     BEGIN M-LOOP TO FETCH PROPERTIES FOR EACH MATERIAL ID
!
      m = 0
      DO
         m = m + 1
         IF ( m>4 ) THEN
!
!     END OF M-LOOP
!
            IF ( mid(3)<100000000 ) EXIT
            IF ( gi(19)/=0.D0 .OR. gi(20)/=0.D0 .OR. gi(21)/=0.D0 .OR. gi(22)/=0.D0 ) EXIT
            igobk = 1
            m = 2
            mid(3) = mid(2)
         ELSE
            IF ( m==4 .AND. igobk==1 ) EXIT
            matid = mid(m)
            IF ( matid/=0 .OR. m==3 ) THEN
               IF ( .NOT.(matid==0 .AND. m==3 .AND. .NOT.bendng) ) THEN
                  IF ( matid==0 .AND. m==3 .AND. bendng ) matid = mid(2)
!
                  IF ( m<1 ) THEN
                  ELSEIF ( m==1 ) THEN
                     CALL mat(elid)
                  ELSEIF ( matid/=mid(m-1) .OR. igobk/=0 ) THEN
                     CALL mat(elid)
                  ENDIF
!
                  IF ( membrn .AND. m==1 ) rho = matout(7)
                  rhox = rho
                  IF ( rho==0.0D0 ) rhox = 1.0D0
                  IF ( kgg1/=0 ) THEN
!
                     IF ( .NOT.(membrn .AND. m/=1 .OR. .NOT.membrn .AND. m/=2) ) THEN
                        gsube = matout(12)
                        IF ( matset==8. ) gsube = matout(16)
                     ENDIF
!
                     IF ( .NOT.(m==2 .AND. norpth) ) THEN
                        coeff = 1.0D0
                        lpoint = (m-1)*9 + 1
!
                        CALL q4gmgd(m,coeff,gi(lpoint))
!
!WKBDB 11/93 SPR93020
!      IF (M .GT. 0) GO TO 670
!      IF (.NOT.SHRFLX .AND. BENDNG) GO TO 660
!      NEST(2) = MATID
!      J = 232
!      GO TO 1720
!
!  660 M = -M
! ALREADY DELETED BEFORE SPR93020 670 IF (.NOT.BENDNG) GO TO 760
!  670 CONTINUE
!      MTYPE = IFIX(MATSET+.05) - 2
!      IF (NOCSUB) GO TO 760
!      GO TO (760,680,720,760), M
!
!  680 IF (MTYPE) 690,700,710
!  690 ENORX = MATOUT(16)
!      ENORY = MATOUT(16)
!      GO TO 760
!  700 ENORX = MATOUT(1)
!      ENORY = MATOUT(4)
!      GO TO 760
!  710 ENORX = MATOUT(1)
!      ENORY = MATOUT(3)
!      GO TO 760
!
!  720 IF (MTYPE) 730,740,750
!  730 GNORX = MATOUT(6)
!      GNORY = MATOUT(6)
!      GO TO 760
!
!  740 GNORX = MATOUT(1)
!      GNORY = MATOUT(4)
!      GO TO 760
!
!  750 GNORX = MATOUT(6)
!      GNORY = MATOUT(5)
!      IF (GNORX .EQ. 0.0D0) GNORX = MATOUT(4)
!      IF (GNORY .EQ. 0.0D0) GNORY = MATOUT(4)
!  760 CONTINUE
!
!WKBDE 11/93 SPR93020
!     IF (MATSET .EQ. 1.0) GO TO 610
                        IF ( m==3 ) THEN
!
                           u(1) = tem(5)*tem(9) + tem(6)*tem(8)
                           u(2) = tem(2)*tem(9) + tem(8)*tem(3)
                           u(3) = tem(4)*tem(9) + tem(7)*tem(6)
                           u(4) = tem(1)*tem(9) + tem(3)*tem(7)
                           l = 2
                        ELSE
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
                        ENDIF
!
                        CALL gmmatd(u(1),l,l,1,gi(lpoint),l,l,0,gt(1))
                        CALL gmmatd(gt(1),l,l,0,u(1),l,l,0,gi(lpoint))
!WKBNB 11/93 SPR93020
                        IF ( m<=0 ) THEN
                           IF ( .NOT.shrflx .AND. bendng ) THEN
                              m = -m
                           ELSE
                              nest(2) = matid
                              j = 232
                              GOTO 500
                           ENDIF
                        ENDIF
                     ENDIF
                     mtype = ifix(matset+.05) - 2
                     IF ( .NOT.(nocsub) ) THEN
                        IF ( m==1 .OR. m==4 ) THEN
                        ELSEIF ( m==3 ) THEN
                           IF ( mtype<0 ) THEN
                              gnorx = matout(6)
                              gnory = matout(6)
                           ELSEIF ( mtype==0 ) THEN
                              gnorx = matout(1)
                              gnory = matout(4)
                           ELSE
                              gnorx = matout(6)
                              gnory = matout(5)
                              IF ( gnorx==0.0D0 ) gnorx = matout(4)
                              IF ( gnory==0.0D0 ) gnory = matout(4)
                           ENDIF
!WKBNE 11/93 SPR93020
!WKBNB 2/94 SPR93020
                        ELSEIF ( mtype<0 ) THEN
                           enorx = matout(16)
                           enory = matout(16)
                           dnux = gi(lpoint+1)/gi(lpoint)
                           dnuy = gi(lpoint+3)/gi(lpoint+4)
                        ELSEIF ( mtype==0 ) THEN
                           enorx = matout(1)
                           enory = matout(4)
                           dnux = gi(lpoint+1)/gi(lpoint)
                           dnuy = gi(lpoint+3)/gi(lpoint+4)
                        ELSE
                           enorx = matout(1)
                           enory = matout(3)
                           dnux = gi(lpoint+1)/gi(lpoint)
!WKBNE 2/94 SPR93020
                           dnuy = gi(lpoint+3)/gi(lpoint+4)
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDDO
!
      nocsub = enorx==0.0D0 .OR. enory==0.0D0 .OR. gnorx==0.0D0 .OR. gnory==0.0D0 .OR. mominr==0.0D0
!
      mattyp = ifix(matset+.05)
!
!     IF MGG1 IS NON-ZERO AND RHO IS GREATER THAN 0.0,
!     THEN COMPUTE THE MASS MATRIX.
!
      IF ( mgg1/=0 ) THEN
         IF ( jcored+144<=ncored ) THEN
         ENDIF
      ENDIF
!
      limit = jcored + ndof*ndof
      DO i = jcored , limit
         akgg(i) = 0.0D0
      ENDDO
      DO i = 1 , nodesq
         xmass(i) = 0.0D0
         xmtmp(i) = 0.0D0
      ENDDO
      area = 0.0D0
      vol = 0.0D0
!
!
!     HERE BEGINS THE TRIPLE LOOP ON STATEMENTS 1310 AND 1300 TO
!     GAUSS INTEGRATE FOR THE ELEMENT MASS AND STIFFNESS MATRICES.
!     -----------------------------------------------------------
!
      DO ixsi = 1 , 2
         xi = ptint(ixsi)
         DO ieta = 1 , 2
            eta = ptint(ieta)
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
               kk = iorder(i)
               shp(i) = tmpshp(kk)
               dshp(i) = dshptp(kk)
               dshp(i+4) = dshptp(kk+4)
            ENDDO
            CALL gmmatd(shp,1,nnode,0,dgpth,1,nnode,1,thk)
            reali = mominr*thk*thk*thk/12.0D0
!     REALI =        THK*THK*THK/12.0D0
            tsi = ts*thk
!
!     SKIP MASS CALCULATIONS IF NOT REQUESTED
!
            IF ( nsm==0.0 ) THEN
               IF ( mgg1==0 ) GOTO 210
               IF ( rho==0.D0 ) GOTO 210
               IF ( rho<=0.D0 ) THEN
                  WRITE (nout,99002) uwm , rho , mid(1) , nest(1)
99002             FORMAT (A25,', RHO = ',1P,D12.4,' IS ILLEGAL FROM MATERIAL ID =',I9,' FOR QUAD4 EID =',I9)
               ENDIF
            ENDIF
!     NOGO = .TRUE.
!     GO TO 1710
!
!     COMPUTE S AND T VECTORS AT THE MID-SURFACE
!     FOR MASS CALCULATIONS ONLY.
!
            DO i = 1 , 2
               ipoint = 4*(i-1)
               DO j = 1 , 3
                  v(i,j) = 0.0D0
                  DO k = 1 , nnode
                     ktemp = k + ipoint
                     jtemp = j + 1
                     v(i,j) = v(i,j) + dshp(ktemp)*bgpdt(jtemp,k)
                  ENDDO
               ENDDO
            ENDDO
!
!     COMPUTE S CROSS T AT THE MID-SURFACE FOR MASS CALCULATIONS.
!
            v(3,1) = v(1,2)*v(2,3) - v(2,2)*v(1,3)
            v(3,2) = v(1,3)*v(2,1) - v(2,3)*v(1,1)
            v(3,3) = v(1,1)*v(2,2) - v(2,1)*v(1,2)
            area2 = v(3,1)*v(3,1) + v(3,2)*v(3,2) + v(3,3)*v(3,3)
!
!     AREA2 = NORM OF S CROSS T IS THE AREA OF THE ELEMENT
!     AS COMPUTED AT THIS GAUSS POINT.
!
!WKBR 11/93 SPR 93015 IF (AREA2 .LT. EPS1) GO TO 1700
            IF ( area2<=0.0 ) GOTO 300
!
            area2 = dsqrt(area2)
            area = area + area2
            voli = area2*thk
            vol = vol + voli
!
            IF ( mgg1/=0 ) THEN
               IF ( cpmass>0 ) THEN
!
!     COMPUTE CONSISTENT MASS MATRIX
!
!     COMPUTE THE CONTRIBUTION TO THE MASS MATRIX
!     FROM THIS INTEGRATION POINT.
!
                  CALL gmmatd(shp,1,nnode,1,shp,1,nnode,0,xmtmp)
!
!     ADD MASS CONTRIBUTION FROM THIS INTEGRATION POINT
!     TO THE ELEMENT MASS MATRIX.
!
                  DO i = 1 , nodesq
                     xmass(i) = xmass(i) + voli*rhox*xmtmp(i)
                  ENDDO
               ELSE
                  i4 = 1
                  DO j4 = 1 , nnode
                     xmass(i4) = xmass(i4) + voli*rhox*shp(j4)
                     i4 = i4 + nnode + 1
                  ENDDO
               ENDIF
            ENDIF
!
 210        IF ( kgg1==0 ) GOTO 250
!
!     BEGIN STIFFNESS COMPUTATIONS
!
!     SET DEFAULT VALUES OF CSUBB FACTORS
!
            sfcty1 = 1.0D0
            sfcty2 = 1.0D0
            sfctx1 = 1.0D0
            sfctx2 = 1.0D0
            tsmfx = 1.0D0
            tsmfy = 1.0D0
            IF ( .NOT.(nocsub) ) THEN
               IF ( bendng ) THEN
!      NUNORX = MOMINR*ENORX/(2.0D0*GNORX) - 1.0D0
!      NUNORY = MOMINR*ENORY/(2.0D0*GNORY) - 1.0D0
!WKBNB 2/94 SPR93020
                  nunorx = mominr*enorx/(2.0D0*gnorx) - 1.0D0
                  nunory = mominr*enory/(2.0D0*gnory) - 1.0D0
!WKBNE 2/94 SPR93020
!
!     NOTE- THE ABOVE EXPRESSIONS FOR NUNORX AND NUNORY WERE MODIFIED
!           BY G.CHAN/UNISYS    1988
!
!WKBDB 2/94 SPR93020
!      EIX = MOMINR*ENORX
!      EIY = MOMINR*ENORY
!      TGX = 2.0D0*GNORX
!      TGY = 2.0D0*GNORY
!      NUNORX = EIX/TGX - 1.0D0
!      NUNORY = EIY/TGY - 1.0D0
!      IF (EIX .GT. TGX) NUNORX= 1.0D0 - TGX/EIX
!      IF (EIY .GT. TGY) NUNORY= 1.0D0 - TGY/EIY
!WKBDE 2/94 SPR93020
                  IF ( nunorx>0.999999D0 ) nunorx = 0.999999D0
                  IF ( nunory>0.999999D0 ) nunory = 0.999999D0
!WKBNB 2/94 SPR93020
                  IF ( nunorx<=0. ) nunorx = dnux
                  IF ( nunory<=0. ) nunory = dnuy
!WKBNE 2/94 SPR93020
!     IF (NUNORX .GT. .49D0) NUNORX = 0.49D0
!     IF (NUNORY .GT. .49D0) NUNORY = 0.49D0
                  cc = aspect
!
!     NOTE- THE FOLLOWING 2 FORMULATIONS WERE PUT IN ON 4/30/85 IN
!           CONJUNCTION WITH THE OUT-OF-PLANE SHEAR CORRECTION BASED
!           ON T.J.R HUGHES. THE FLEXIBLE SOLUTION PROVIDES MORE
!           ACCURATE RESULTS FOR PLATES, ALTHOUGH IT MIGHT CONVERGE
!           SLOWLY. THE STIFFER SOLUTION (COMMENTED OUT) IS O.K. FOR
!           PLATES AND SHOULD HAVE A BETTER CONVERGENCE.
!
!           THEY WERE MODIFIED ON 5/3/85
!
!     4-NODE CSUBB FORMULATION AS OF 5/3/85 (FLEXIBLE SOLUTION)
!     REPLACES THE ONE COMMENTED OUT IMMEDIATELY ABOVE
!
                  w1 = 1.0D0 + 4400.0D0*thlen*thlen*thlen*thlen
                  IF ( cc<0.2D0 ) THEN
                     dsub4 = (159.85D0*cc-15.97D0)*w1
                  ELSE
                     dsub4 = (18.375D0-11.875D0*cc)*w1
                  ENDIF
!
!     4-NODE CSUBB FORMULATION AS OF 5/3/85 (STIFFER SOLUTION)
!
!     W1 = 1.0D0 + 2.5D0*THLEN + 1.0D04*THLEN**5
!     IF (CC .LT. 0.2D0) GO TO 1030
!     DSUB4 = 18.0D0*W1
!     GO TO 1040
!1030 DSUB4 = (179.85D0*CC-17.97D0)*W1
                  IF ( dsub4<.01D0 ) dsub4 = 0.01D0
                  IF ( dsub4>2.0D3 ) dsub4 = 2000.0D0
                  dsub = dsub4
                  coeft = const
                  ax = a
                  IF ( eta<0.0D0 ) ax = a + coeft*(xa(2)-xa(1)-a)
                  IF ( eta>0.0D0 ) ax = a + coeft*(xa(3)-xa(4)-a)
                  psiinx = 20.0D0*dsub*reali*sineax*(1.0D0+aspect*aspect)/(tsi*(1.0D0-nunorx)*ax*ax)
                  dsub = dsub4
                  coeft = const
                  by = b
                  IF ( xi<0.0D0 ) by = b + coeft*(yb(4)-yb(1)-b)
                  IF ( xi>0.0D0 ) by = b + coeft*(yb(3)-yb(2)-b)
                  psiiny = 20.0D0*dsub*reali*sineay*(1.0D0+aspect*aspect)/(tsi*(1.0D0-nunory)*by*by)
                  IF ( .NOT.shrflx ) THEN
                     tsmfx = psiinx
                     tsmfy = psiiny
                  ELSE
                     tsmfx = psiinx/(1.0D0+psiinx)
                     tsmfy = psiiny/(1.0D0+psiiny)
                  ENDIF
!
                  IF ( tsmfx<=0.0D0 ) tsmfx = eps1
                  IF ( tsmfy<=0.0D0 ) tsmfy = eps1
!
!     FILL IN THE 7X7 MATERIAL PROPERTY MATRIX D FOR NORPTH
!
                  IF ( norpth ) THEN
                     DO ig = 1 , 7
                        DO jg = 1 , 7
                           dfour(ig,jg) = 0.0D0
                        ENDDO
                     ENDDO
!
                     DO ig = 1 , 3
                        ig1 = (ig-1)*3
                        DO jg = 1 , 3
                           jg1 = jg + ig1
                           dfour(ig,jg) = gi(jg1)
                        ENDDO
                     ENDDO
                     GOTO 220
                  ENDIF
               ENDIF
            ENDIF
!
!     FILL IN THE 10X10 G-MATRIX WHEN MID4 IS NOT PRESENT
!
            DO ig = 1 , 10
               DO jg = 1 , 10
                  gfour(ig,jg) = 0.0D0
               ENDDO
            ENDDO
            IF ( .NOT.(mbcoup) ) THEN
!
               IF ( membrn ) THEN
                  DO ig = 1 , 3
                     ig1 = (ig-1)*3
                     DO jg = 1 , 3
                        jg1 = jg + ig1
                        gfour(ig,jg) = gi(jg1)
                     ENDDO
                  ENDDO
               ENDIF
!
               IF ( .NOT.bendng ) GOTO 230
               DO ig = 4 , 6
                  ig2 = (ig-2)*3
                  DO jg = 4 , 6
                     jg2 = jg + ig2
                     gfour(ig,jg) = gi(jg2)*mominr
                  ENDDO
               ENDDO
!
               IF ( membrn ) THEN
                  DO ig = 1 , 3
                     ig1 = (ig-1)*3
                     kg = ig + 3
                     DO jg = 1 , 3
                        jg1 = jg + ig1
                        lg = jg + 3
                        gfour(ig,lg) = gi(jg1)
                        gfour(kg,jg) = gi(jg1)
                     ENDDO
                  ENDDO
               ENDIF
            ENDIF
!
!     IRREGULAR 4-NODE CODE-  CALCULATION OF NODAL EDGE SHEARS
!                             AT THIS INTEGRATION POINT
!
 220        DO ij = 1 , 4
               ii = ij - 1
               IF ( ii==0 ) ii = 4
               ik = ij + 1
               IF ( ik==5 ) ik = 1
!
               DO ir = 1 , 4
                  IF ( ij==iorder(ir) ) THEN
                     ioj = ir
                     EXIT
                  ENDIF
               ENDDO
               DO ir = 1 , 4
                  IF ( ik==iorder(ir) ) THEN
                     iok = ir
                     EXIT
                  ENDIF
               ENDDO
               aa = shp(ioj)
               bb = shp(iok)
!
               DO is = 1 , 3
                  edgshr(is,ij) = (uev(is,ij)+anglei(ij)*uev(is,ii))*aa/(1.0D0-anglei(ij)*anglei(ij))                               &
                                & + (uev(is,ij)+anglei(ik)*uev(is,ik))*bb/(1.0D0-anglei(ik)*anglei(ik))
               ENDDO
            ENDDO
!
!     TORSION-RELATED SHEAR CORRECTION FOR 4-NODE-
!     SET-UP OF EXPANDED SHEAR MATERIAL PROPERTY MATRICES (G OR D)
!
            csubx = 20.0D0*reali/(tsi*(1.0D0-nunorx)*a*a)
            csuby = 20.0D0*reali/(tsi*(1.0D0-nunory)*b*b)
            sfctr1 = csubb4*csubx
            sfctr2 = csubtx*csubx
            IF ( shrflx ) THEN
               sfctr1 = sfctr1/(1.0D0+sfctr1)
               sfctr2 = sfctr2/(1.0D0+sfctr2)
            ENDIF
            sfctx1 = sfctr1 + sfctr2
            sfctx2 = sfctr1 - sfctr2
            sfctr1 = csubb4*csuby
            sfctr2 = csubty*csuby
            IF ( shrflx ) THEN
               sfctr1 = sfctr1/(1.0D0+sfctr1)
               sfctr2 = sfctr2/(1.0D0+sfctr2)
            ENDIF
            sfcty1 = sfctr1 + sfctr2
            sfcty2 = sfctr1 - sfctr2
!
!     FILL IN THE EXPANDED MATERIAL PROPERTY MATRIX
!
            IF ( norpth ) THEN
!
               dfour(4,4) = 0.25D0*sfcty1*ts*gi(19)
               dfour(5,5) = 0.25D0*sfcty1*ts*gi(19)
               dfour(5,4) = 0.25D0*sfcty2*ts*gi(19)
               dfour(4,5) = dfour(5,4)
               dfour(6,6) = 0.25D0*sfctx1*ts*gi(22)
               dfour(7,7) = 0.25D0*sfctx1*ts*gi(22)
               dfour(7,6) = 0.25D0*sfctx2*ts*gi(22)
               dfour(6,7) = dfour(7,6)
               dfour(4,6) = dsqrt(tsmfx*tsmfy)*ts*gi(20)
               dfour(6,4) = dfour(4,6)
            ELSE
               gfour(7,7) = 0.25D0*sfcty1*ts*gi(19)
               gfour(8,8) = 0.25D0*sfcty1*ts*gi(19)
               gfour(8,7) = 0.25D0*sfcty2*ts*gi(19)
               gfour(7,8) = gfour(8,7)
               gfour(9,9) = 0.25D0*sfctx1*ts*gi(22)
               gfour(10,10) = 0.25D0*sfctx1*ts*gi(22)
               gfour(10,9) = 0.25D0*sfctx2*ts*gi(22)
               gfour(9,10) = gfour(10,9)
               gfour(7,9) = dsqrt(tsmfx*tsmfy)*ts*gi(20)
               gfour(9,7) = gfour(7,9)
            ENDIF
!
!     DO 1300 IZTA = 1,JZTA
 230        DO izta = 1 , 2
               zta = ptint(izta)
               ibot = (izta-1)*nd2
!
               hzta = zta/2.0D0
!
!     TORSION-RELATED SHEAR CORRECTION FOR 4-NODE-
!     SET-UP OF POINTERS TO THE SAVED B-MATRIX
!
               iptx1 = ((ixsi-1)*2+ieta-1)*2*nd2 + ibot
               iptx2 = ((ixsi-1)*2+2-ieta)*2*nd2 + ibot
               ipty1 = ((ixsi-1)*2+ieta-1)*2*nd2 + ibot
               ipty2 = ((2-ixsi)*2+ieta-1)*2*nd2 + ibot
!     IF (NORPTH) IBOT = IBOT/2
!
!     FILL IN THE 10X10 G-MATRIX IF MID4 IS PRESENT
!
               IF ( mbcoup ) THEN
                  DO ig = 1 , 3
                     ig1 = (ig-1)*3
                     DO jg = 1 , 3
                        jg1 = jg + ig1
                        jg4 = jg1 + 27
                        gfour(ig,jg) = gi(jg1)
                     ENDDO
                  ENDDO
!
                  DO ig = 4 , 6
                     ig2 = (ig-2)*3
                     DO jg = 4 , 6
                        jg2 = jg + ig2
                        jg4 = jg2 + 18
                        gfour(ig,jg) = gi(jg2)*mominr
                     ENDDO
                  ENDDO
!
                  DO ig = 1 , 3
                     ig4 = (ig+8)*3
                     kg = ig + 3
                     DO jg = 1 , 3
                        jg4 = jg + ig4
                        jg1 = jg4 - 27
                        lg = jg + 3
                        gfour(ig,lg) = -gi(jg4)*zta*6.0D0 + gi(jg1)
                        gfour(kg,jg) = -gi(jg4)*zta*6.0D0 + gi(jg1)
                     ENDDO
                  ENDDO
               ENDIF
!
!     COMPUTE THE JACOBIAN AT THIS GAUSS POINT,
!     ITS INVERSE AND ITS DETERMINANT.
!
               CALL jacob2(elid,shp,dshp,dgpth,egpdt,epnorm,jacob)
               IF ( badjac ) GOTO 400
!
!     COMPUTE PSI TRANSPOSE X JACOBIAN INVERSE.
!     HERE IS THE PLACE WHERE THE INVERSE JACOBIAN IS FLAGED TO BE
!     TRANSPOSED BECAUSE OF OPPOSITE MATRIX LOADING CONVENTION
!     BETWEEN INVER AND GMMAT.
!
               CALL gmmatd(psitrn,3,3,0,jacob,3,3,1,phi)
!
!     CALL Q4BMGD TO GET B-MATRIX. SET THE ROW FLAG TO 3.
!     IT WILL RETURN THE FIRST 6 ROWS OF B-MATRIX.
!
               rowflg = 3
               CALL q4bmgd(dshp,dgpth,egpdt,epnorm,phi,bfour(1))
!
!     REPLACE ABOVE Q4BMGD BY THE FOLLOWING LINE IF TRPLMD IS NOT USED
!     CALL Q4BMGD (DSHP,DGPTH,EGPDT,EPNORM,PHI,BMATRX)
!
!     TORSION-RELATED SHEAR CORRECTION FOR 4-NODE -
!     SET-UP OF B-MATRIX AND TRIPLE MULTIPLY
!
!
               CALL trplmd(gfour,dfour,bfour,bmat1,xybmat,mattyp,jcored,detj)
!     (TRPLMD CAN BE REPLACED BY NEXT 40 (APROX.) LINES)
!
!     ND63 = ND6
!     ND74 = ND7
!     IF (.NOT.NORPTH) GO TO 1291
!     ND63 = ND3
!     ND74 = ND4
!1291 DO 1292 IX = 1,NDOF
!     BFOUR(IX) = BMATRX(IX)
!     BFOUR(IX+NDOF) = BMATRX(IX+NDOF)
!     BFOUR(IX+ND2 ) = XYBMAT(IX+IBOT)
!     BFOUR(IX+ND5 ) = XYBMAT(IX+IBOT+NDOF)
!     BFOUR(IX+ND63) = BMAT1(IX+IPTY1)
!     BFOUR(IX+ND74) = BMAT1(IX+IPTY2)
!     BFOUR(IX+ND74+NDOF) = BMAT1(IX+IPTX1+NDOF)
!1292 BFOUR(IX+ND74+ND2 ) = BMAT1(IX+IPTX2+NDOF)
!
!     IF (NORPTH) GO TO 1294
!     DO 1293 IX = 1,NDOF
!     BFOUR(IX+ND3) = BMATRX(IX+ND3)
!     BFOUR(IX+ND4) = BMATRX(IX+ND4)
!1293 CONTINUE
!     NNX = 10
!     CALL GMMATD (GFOUR,NNX,NNX,0,BFOUR,NNX,NDOF,0,STRESR)
!     GO TO 1295
!
!1294 NNX = 7
!     CALL GMMATD (DFOUR,NNX,NNX,0,BFOUR,NNX,NDOF,0,STRESR)
!1295 NNY = NNX*NDOF
!     DO 1296 KBAR = 1,NNY
!1296 BFOUR(KBAR) = BFOUR(KBAR)*DETJ
!
!     COMPUTE THE CONTRIBUTION TO THE STIFFNESS MATRIX FROM THIS GAUSS
!     INTEGRATION POINT.  NOTE THAT THE -1 IN THE GMMATD CALL KEEPS A
!     RUNNING SUM ON AKGG.
!
!     CALL GMMATD (BFOUR,NNX,NDOF,-1,STRESR,NNX,NDOF,0,AKGG(JCORED))
!
            ENDDO
         ENDDO
      ENDDO
!
!     EQUALIZE THE OFF- DIAGNOAL TERMS TO GUARANTEE PERFECT SYMMETRIC
!     MATRIX IF NO DAMPING INVLOVED
!
      IF ( gsube==0.0 ) THEN
         ij = jcored - 1
         ndofm1 = ndof - 1
         DO ii = 1 , ndofm1
            ip1 = ii + 1
            im1 = (ii-1)*ndof + ij
            DO jj = ip1 , ndof
               i = im1 + jj
               j = (jj-1)*ndof + ii + ij
               temp = (akgg(i)+akgg(j))*.5D0
               IF ( dabs(temp)<1.0D-17 ) temp = 0.0D0
               akgg(i) = temp
               akgg(j) = temp
            ENDDO
         ENDDO
      ENDIF
!
!     END OF STIFFNESS LOOP
!
!     ADD NON-STRUCTURAL MASS
!
 250  IF ( mgg1/=0 ) THEN
         IF ( rho/=0.D0 .OR. nsm/=0.0 ) THEN
!     IF (CPMASS .GT. 0) GO TO 1410
            IF ( nsm/=0.0 ) THEN
               IF ( vol==0.D0 .OR. rhox==0.D0 ) WRITE (nout,99003) sfm , elid , area , vol , rhox , mgg1 , kgg1
99003          FORMAT (A25,', ZERO VOLUME OR DENSITY FOR QUAD4 ELEMENT ID =',I9,', AREA,VOL,RHO =',3D12.3,/70X,'MGG1,KGG1 =',2I8)
               factor = (vol*rho+nsm*area)/(vol*rhox)
               DO i = 1 , nodesq
                  xmass(i) = xmass(i)*factor
               ENDDO
            ENDIF
         ENDIF
      ENDIF
!
!     PICK UP THE GLOBAL TO BASIC TRANSFORMATIONS FROM THE CSTM.
!
      DO i = 1 , 36
         trans(i) = 0.0D0
      ENDDO
!     DO 1414 I = 2,8
!1414 TRANS1(I) = 0.0D0
!     TRANS1(1) = 1.0D0
!     TRANS1(5) = 1.0D0
!     TRANS1(9) = 1.0D0
!
      DO i = 1 , nnode
         notran(i) = 0
         ipoint = 9*(i-1) + 1
         IF ( igpdt(1,i)>0 ) THEN
            igpth(1) = igpdt(1,i)
            gpth(2) = bgpdt(2,i)
            gpth(3) = bgpdt(3,i)
            gpth(4) = bgpdt(4,i)
!
!     NOTE THAT THE 6X6 TRANSFORMATION WHICH WILL BE USED LATER
!     IN THE TRIPLE MULTIPLICATION TO TRANSFORM THE ELEMENT
!     STIFFNESS MATRIX FROM BASIC TO GLOBAL COORDINATES, IS BUILT
!     UPON THE 3X3 TRANSFORMATION FROM GLOBAL TO BASIC TBG-MATRIX.
!     THIS IS DUE TO THE DIFFERENCE IN TRANSFORMATION OF ARRAYS
!     AND MATRICES.
!
            CALL transd(gpth,tbg)
            CALL gmmatd(teb,3,3,0,tbg,3,3,0,trans(ipoint))
!
         ELSEIF ( identt/=1 .OR. offset/=0.0D0 ) THEN
!
            DO j = 1 , 9
               trans(ipoint+j-1) = teb(j)
            ENDDO
         ELSE
            notran(i) = 1
         ENDIF
      ENDDO
!
!
!     HERE WE SHIP OUT THE STIFFNESS AND DAMPING MATRICES.
!     ----------------------------------------------------
!
      IF ( kgg1/=0 ) THEN
!
!     SET UP I-LOOP TO DUMP OUT BASIC TO GLOBAL TRANSFORMED, NODAL
!     PARTITIONED (6 D.O.F. PER NODE) COLUMNS OF THE ELEM. STIFFNESS.
!
!     THIS MEANS WE ARE SENDING TO EMGOUT 6 COLUMNS OF THE ELEMENT
!     STIFFNESS MATRIX AT A TIME.  EACH BUNCH OF 6 COLUMNS CORRESPOND
!     TO ONE PARTICULAR NODE OF THE ELEMENT.  FOR THE MASS MATRIX, WE
!     ONLY SEND 3 COLUMNS PER NODE TO EMGOUT SINCE THE OTHER 3 D.O.F.
!     ARE ZERO ANYWAY.  THE CODE WORD (DICT(4)) TELLS EMGOUT WHICH
!     COLUMNS ARE THE NON ZERO ONES THAT WE ARE SENDING. (SEE SECTION
!     6.8.3.5.1 OF THE PROGRAMMER MANUAL)
!
!
         dict(1) = estid
         dict(2) = 1
         dict(3) = ndof
         dict(4) = 63
         npart = ndof*6
         DO i = 1 , nnode
            ibegin = 6*(i-1) + jcored - 1
!
!     DUMP AN UNTRANSFORMED NODAL COLUMN PARTITION.
!
            DO j = 1 , ndof
               kpoint = ndof*(j-1) + ibegin
               lpoint = 6*(j-1)
               DO k = 1 , 6
                  colstf(lpoint+k) = akgg(kpoint+k)
               ENDDO
            ENDDO
            IF ( notran(i)/=1 ) THEN
!
!     THIS COLUMN PARTITION NEEDS TO BE TRANSFORMED TO GLOBAL
!     COORDINATES. (SEE PAGE 2.3-43 OF THE PROGRAMMER MANUAL)
!
!     LOAD THE 6X6 TRANSFORMATION
!
               CALL tldrd(offset,i,trans,trans1)
!
!     TRANSFORM THE NODAL COLUMN PARTITION.
!
               CALL gmmatd(colstf,ndof,6,0,trans1,6,6,0,coltmp)
               DO ii = 1 , npart
                  colstf(ii) = coltmp(ii)
               ENDDO
            ENDIF
!
!     NOW TRANSFORM THE ROWS OF THIS PARTITION.
!
            DO m = 1 , nnode
               IF ( notran(m)/=1 ) THEN
                  mpoint = 36*(m-1) + 1
!
!     LOAD THE 6X6 TRANSFORMATION
!
                  CALL tldrd(offset,m,trans,trans1)
!
!     TRANSFORM THE 6 ROWS FOR THIS SUBPARTITION
!
                  CALL gmmatd(trans1,6,6,1,colstf(mpoint),6,6,0,coltmp)
                  iipnt = mpoint - 1
                  DO ii = 1 , 36
                     colstf(iipnt+ii) = coltmp(ii)
                  ENDDO
               ENDIF
            ENDDO
!
!     HERE WE MUST CHANGE FROM THE ROW LOADING CONVENTION
!     FOR GMMATD TO THE COLUMN LOADING CONVENTION FOR EMGOUT.
!
            DO ii = 1 , 6
               ipoint = ndof*(ii-1)
               DO jj = 1 , ndof
                  jpoint = 6*(jj-1)
                  coltmp(ipoint+jj) = colstf(jpoint+ii)
               ENDDO
            ENDDO
!
!     DUMP THE TRANSFORMED NODAL COLUMN PARTITION
!
            ieoe = 0
            IF ( i==nnode ) ieoe = 1
            adamp = gsube
!
!     INTEGER 1 IN THE NEXT TO LAST FORMAL PARAMETER OF
!     EMGOUT MEANS WE ARE SENDING STIFFNESS DATA.
!
            CALL emgout(coltmp,coltmp,npart,ieoe,dict,1,iprec)
         ENDDO
      ENDIF
!
!
!     HERE WE SHIP OUT THE MASS MATRIX.
!     ---------------------------------
!
      IF ( mgg1/=0 ) THEN
!
         ndof = nnode*3
         npart = ndof*3
         dict(3) = ndof
         dict(4) = 7
         adamp = 0.0D0
!
!     SET UP I-LOOP TO PROCESS AND DUMP THE NODAL COLUMN PARTITIONS.
!
         DO i = 1 , nnode
            DO ijk = 1 , npart
               amgg(jcored-1+ijk) = 0.0D0
            ENDDO
!
!     SET UP J-LOOP TO LOAD THE UNTRANSFORMED NODAL COLUMN PARTITION.
!
            DO j = 1 , nnode
               ipoint = 9*(j-1) + jcored
               jpoint = ipoint + 4
               kpoint = ipoint + 8
               ifrom = nnode*(j-1) + i
               xmasso = xmass(ifrom)
               amgg(ipoint) = xmasso
               amgg(jpoint) = xmasso
               amgg(kpoint) = xmasso
            ENDDO
            IF ( notran(i)/=1 ) THEN
!
!     THIS COLUMN PARTITION NEEDS TO BE TRANSFORMED
!     TO GLOBAL COORDINATES.
!
               DO m = 1 , nnode
                  mpoint = 9*(m-1) + jcored
                  CALL gmmatd(amgg(mpoint),3,3,0,trans(9*i-8),3,3,0,tmpmas)
                  iicore = mpoint - 1
                  DO k = 1 , 9
                     amgg(iicore+k) = tmpmas(k)
                  ENDDO
               ENDDO
!
!     SET UP M-LOOP TO TRANSFORM THE NODAL ROW PARTITIONS
!     OF THIS NODAL COLUMN PARTITION.
!
               DO m = 1 , nnode
                  mpoint = 9*(m-1) + jcored
!
!     TRANSFORM THE 3 ROWS FOR THIS SUBPARTITION.  THIS IS CORRECT
!     (3 ROWS).  REMEMBER THAT FOR THE MASS MATIIX FOR THIS ELEMENT
!     THERE ARE NO MASS MOMENT OF INERTIA TERMS.  THIS GIVES THREE
!     ROWS OF ZERO TERMS INTERSPERSED BETWEEN 3 ROWS OF NONZERO
!     TRANSLATIONAL MASS TERMS FOR EACH NODE.
!
                  CALL gmmatd(trans(9*m-8),3,3,1,amgg(mpoint),3,3,0,tmpmas)
                  iicore = mpoint - 1
                  DO k = 1 , 9
                     amgg(iicore+k) = tmpmas(k)
                  ENDDO
               ENDDO
            ENDIF
!
!     HERE WE MUST CHANGE FROM THE ROW LOADING CONVENTION
!     FOR GMMATD TO THE COLUMN LOADING CONVENTION FOR EMGOUT.
!
            DO ii = 1 , 3
               ipoint = ndof*(ii-1)
               DO jj = 1 , ndof
                  jpoint = 3*(jj-1) + jcored - 1
                  coltmp(ipoint+jj) = amgg(jpoint+ii)
               ENDDO
            ENDDO
!
!     DUMP THIS TRANSFORMED MASS NODAL COLUMN PARTITION.
!
            ieoe = 0
            IF ( i==nnode ) ieoe = 1
!
!     INTEGER 2 IN THE NEXT TO LAST FORMAL PARAMETER OF
!     EMGOUT MEANS WE ARE SENDING MASS DATA.
!
            CALL emgout(coltmp,coltmp,npart,ieoe,dict,2,iprec)
         ENDDO
      ENDIF
      GOTO 400
   ENDIF
!
 300  j = 230
   GOTO 500
!
 400  RETURN
!
 500  CALL mesage(30,j,nest)
   IF ( l38==1 ) CALL mesage(-61,0,0)
   nogo = .TRUE.
   GOTO 400
!
!
!     HEAT FLOW OPTION STARTS HERE.
!
!     WE NEED TO RESTORE THE ORIGINAL ORDER OF SILS AND BGPDT DATA
!
 600  j = 1
   DO i = 1 , 20
      est(i+j) = save(i)
      IF ( i==4 ) j = 24
   ENDDO
!
   inflag = 2
   cosmat = 1.0
   sinmat = 0.0
   matid = nest(13)
   CALL hmat(elid)
   gi(1) = dble(kheat(1))
   gi(2) = dble(kheat(2))
   gi(3) = gi(2)
   gi(4) = dble(kheat(3))
   anis = type/=4 .AND. type/= - 1
!     COMMENT-  ANIS = .FALSE. MEANS ISOTROPIC THERMAL CONDUCTIVITY.
!
   IF ( anis ) GOTO 100
 700  DO i = 1 , 16
      htcon(i) = 0.0D0
      htcap(i) = 0.0D0
   ENDDO
   DO i = 5 , 8
      hsil(i) = 0
      horder(i) = 0
   ENDDO
!
   DO ixsi = 1 , 2
      xi = ptint(ixsi)
      DO ieta = 1 , 2
         eta = ptint(ieta)
         DO izta = 1 , 2
            zeta = ptint(izta)
!
            CALL termsd(nnode,dgpth,epnorm,egpdt,horder,hsil,bterms)
            dvol = determ
!
            DO i = 1 , 4
               ecpt(i) = gi(i)*dvol
            ENDDO
            weitc = dvol*htcp
!
            ip = 1
            DO i = 1 , nnode
               idn = i + nnode
               htflx(ip+1) = ecpt(3)*bterms(i) + ecpt(4)*bterms(idn)
               htflx(ip) = ecpt(1)*bterms(i) + ecpt(2)*bterms(idn)
               ip = ip + 2
            ENDDO
            CALL gmmatd(bterms,2,nnode,-1,htflx,nnode,2,1,htcon)
!
         ENDDO
         IF ( htcp/=0.0 ) THEN
            ip = 0
            DO i = 1 , nnode
               dheat = weitc*shp(i)
               DO j = 1 , nnode
                  ip = ip + 1
                  htcap(ip) = htcap(ip) + dheat*shp(j)
               ENDDO
            ENDDO
         ENDIF
      ENDDO
   ENDDO
   dict(1) = estid
   dict(2) = 1
   dict(3) = nnode
   dict(4) = 1
   IF ( htcp/=0.0 ) THEN
      adamp = 1.0
      CALL emgout(htcap,htcap,nodesq,1,dict,3,iprec)
   ENDIF
   adamp = 0.0
   CALL emgout(htcon,htcon,nodesq,1,dict,1,iprec)
   GOTO 400
99004 FORMAT (//,' ILLEGAL GEOMETRY FOR QUAD4 ELEMENT, ID=',I10)
END SUBROUTINE quad4d
