!*==quad4s.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE quad4s
   IMPLICIT NONE
   USE C_CJACOB
   USE C_COMJAC
   USE C_CONDAS
   USE C_EMGDIC
   USE C_EMGEST
   USE C_EMGPRM
   USE C_HMTOUT
   USE C_MATIN
   USE C_MATOUT
   USE C_Q4COMS
   USE C_Q4DT
   USE C_SYSTEM
   USE C_TERMS
   USE C_TRPLM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a , aa , adamp , area , area2 , aspctx , aspcty , aspect , avgthk , ax , b , bb , by , cc , coeff , coeft , csubb4 ,     &
         & csubt , csubtx , csubty , csubx , csuby , dheat , dnux , dnuy , dsub , dsub4 , dvol , elth , enorx , enory , eps , epsi ,&
         & epst , exi , exj , factor , gnorx , gnory , gsube , htcp , matset , mominr , nsm , nunorx , nunory , offset , pa ,       &
         & psiinx , psiiny , reali , rho , rhox , sfctr1 , sfctr2 , sfctx1 , sfctx2 , sfcty1 , sfcty2 , sineax , sineay , sysbuf ,  &
         & temp , thetam , thk , thlen , ts , tsfact , tsi , tsmfx , tsmfy , v12dk , vjl , vkl , vol , voli , vp12l , w1 , weitc ,  &
         & x31
   REAL , DIMENSION(1) :: amgg
   LOGICAL :: anis , nocsub , nogo
   REAL , DIMENSION(240) :: bfour
   REAL , DIMENSION(3,4) :: bgpdm , curvtr , ugpdm , vnt
   REAL , DIMENSION(4,4) :: bgpdt , egpdt , epnorm , gpnorm , tgrid
   REAL , DIMENSION(384) :: bmat1
   REAL , DIMENSION(32) :: bterms
   REAL , DIMENSION(3) :: cent , cente , curve , v12 , v41 , vd1 , vd2 , vis , vjs , vkn , vks , vp12
   REAL , DIMENSION(144) :: colstf , coltmp
   REAL , SAVE :: const , eps1
   REAL , DIMENSION(7,7) :: dfour
   REAL , DIMENSION(4) :: dgpth , ecpt , gpth , shp , tmpshp , tmpthk , xa , yb , zc
   INTEGER , DIMENSION(9) :: dict
   REAL , DIMENSION(8) :: dshp , dshptp
   REAL , DIMENSION(10,10) :: gfour
   REAL , DIMENSION(9) :: gge , ggu , gt , phi , tbg , tbm , teb , tem , teu , tmpmas , tub , tum , u
   REAL , DIMENSION(36) :: gi , trans , trans1
   INTEGER , DIMENSION(8) :: horder , hsil
   REAL , DIMENSION(16) :: htcap , htcon , xmass , xmtmp
   REAL , DIMENSION(12) :: htflx
   INTEGER :: i , i1 , i4 , ibegin , ic , identt , idn , iec , ieoe , ieta , iflag , ifrom , ig , ig1 , ig2 , ig4 , igobk , ii ,    &
            & iicore , iipnt , ij , ijk , ik , im1 , int , ioj , iok , iord , ip , ip1 , ipoint , iprec , ir , is , isil , itemp ,  &
            & ixsi , izta , j , j4 , jcored , jg , jg1 , jg2 , jg4 , jj , jo , jpoint , jtemp , k , k1 , kg , kgg1 , kk , kpnt ,    &
            & kpoint , kpt , ktemp , l , lg , limit , lpoint , m , mattyp , mcsid , mgg1 , mpoint , mtype , ncored , nd2 , nd3 ,    &
            & nd4 , nd5 , nd6 , nd7 , ndof3 , ndofm1 , nodesq , nout , npart
   INTEGER , DIMENSION(4,4) :: iegpdt , igpdt
   INTEGER , DIMENSION(4) :: igpth , kcid , ksil , mid , necpt , notran , sil
   REAL , DIMENSION(3,3) :: jacob , v
   INTEGER , DIMENSION(2) , SAVE :: nam
   INTEGER , DIMENSION(45) :: nest
   REAL , DIMENSION(2) :: ptint
   REAL , DIMENSION(20) :: save
   REAL :: x42 , xm , xmasso , y31 , y42 , ym , zoff , zoff1 , zta
   REAL , DIMENSION(96) :: xybmat
!
! End of declarations rewritten by SPAG
!
!
!     FORMS STIFFNESS AND MASS MATRICES FOR THE QUAD4 PLATE ELEMENT
!
!     SINGLE PRECISION VERSION
!
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
!WKBD 2/94 SPR93020      REAL            EIX,EIY,TGX,TGY
!WKBI 2/94 SPR 93020
!WKBNB 11/93 SPR 93020
!WKBNE 11/93 SPR 93020
 
!
!     ICORE = FIRST WORD OF OPEN CORE
!     JCORE = NEXT AVAILABLE LOCATION IN OPEN CORE.
!     NCORE = CURRENT LAST AVAILABLE LOCATION IN OPEN CORE
!
!ZZ   COMMON /ZZEMGX/ AKGG(1)
   !>>>>EQUIVALENCE (Sys(01),Sysbuf) , (Sys(02),Nout) , (Sys(03),Nogo) , (Sys(55),Iprec)
!     EQUIVALENCE     (SYS(48)   ,ICSUB4  ), (SYS(49) ,ICSUBB    ),
!    1                (SYS(50)   ,ICSUBT  ), (SYS(75) ,ICSUB8    )
   !>>>>EQUIVALENCE (Flags(1),Kgg1) , (Flags(2),Mgg1) , (adamp,dict(5)) , (igpth(1),Gpth(1)) , (Est(1),Nest(1)) , (Int,Nest(25)) ,       &
!>>>>    & (Bgpdt(1,1),Est(29)) , (Gpth(1),Est(6)) , (Elth,Est(14)) , (Sil(1),Nest(2)) , (Zoff,Est(12)) , (Zoff1,Est(28)) ,              &
!>>>>    & (Amgg(1),Akgg(1)) , (necpt(1),ecpt(1)) , (Htcp,Kheat(4)) , (htflx(1),tmpmas(1)) , (htcap(1),xmass(1)) , (htcon(1),xmtmp(1)) , &
!>>>>    & (Nsm,Est(19)) , (Matset,Matout(25)) , (iegpdt(1,1),egpdt(1,1)) , (Igpdt(1,1),Bgpdt(1,1))
   DATA eps1/1.0E-7/
   DATA const/0.57735026918962/
   DATA nam/4HQUAD , 4H4S  /
!
   Elid = nest(1)
   Ltypfl = 1
   offset = zoff
   IF ( zoff==0.0 ) offset = zoff1
!
!     CHECK FOR SUFFICIENT OPEN CORE FOR ELEMENT STIFFNESS
!
   jcored = Jcore
   ncored = Ncore - 1
   IF ( jcored+576<=ncored .OR. Heat .OR. kgg1==0 ) THEN
!
!     COPY THE SILS AND BGPDT DATA INTO SAVE ARRAY SINCE THE DATA
!     WILL BE REORDERED BASED ON INCREASING SILS.
!
      j = 1
      DO i = 1 , 20
         save(i) = Est(i+j)
         IF ( i==4 ) j = 24
      ENDDO
!
      Nnode = 4
      N1 = 4
      nodesq = Nnode*Nnode
      Ndof = Nnode*6
      ndof3 = Nnode*3
      nd2 = Ndof*2
      nd3 = Ndof*3
      nd4 = Ndof*4
      nd5 = Ndof*5
      nd6 = Ndof*6
      nd7 = Ndof*7
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
!WKBD 11/93 SPR93020      CALL BETRNS (TUB,GGU,0,ELID)
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
      vkl = sqrt(vkn(1)**2+vkn(2)**2+vkn(3)**2)
      IF ( vkl==0. ) WRITE (nout,99004) nest(1)
      vks(1) = vkn(1)/vkl
      vks(2) = vkn(2)/vkl
      vks(3) = vkn(3)/vkl
!WKBR 9/94      PA = VKL/2.D0
      pa = vkl/2.0
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
      vp12l = sqrt(vp12(1)**2+vp12(2)**2+vp12(3)**2)
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
      vjl = sqrt(vjs(1)**2+vjs(2)**2+vjs(3)**2)
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
            ugpdm(i,j) = 0.0
            DO k = 1 , 3
               kk = ip + k
               ugpdm(i,j) = ugpdm(i,j) + tub(kk)*((bgpdm(k,j))-ggu(k))
            ENDDO
         ENDDO
      ENDDO
!
!
!     THE ORIGIN OF THE ELEMENT C.S. IS IN THE MIDDLE OF THE ELEMENT
!
      DO j = 1 , 3
         cent(j) = 0.0
         DO i = 1 , 4
            cent(j) = cent(j) + ugpdm(j,i)/Nnode
         ENDDO
      ENDDO
!
!     STORE THE CORNER NODE DIFF. IN THE USER C.S.
!
      x31 = ugpdm(1,3) - ugpdm(1,1)
      y31 = ugpdm(2,3) - ugpdm(2,1)
      x42 = ugpdm(1,4) - ugpdm(1,2)
      y42 = ugpdm(2,4) - ugpdm(2,2)
      aa = sqrt(x31*x31+y31*y31)
      bb = sqrt(x42*x42+y42*y42)
      IF ( aa==0.0 .OR. bb==0.0 ) GOTO 300
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
         Iorder(i) = 0
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
         Iorder(i) = itemp
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
         ipoint = Iorder(i)
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
      CALL q4nrms(bgpdt,gpnorm,Iorder,iflag)
      IF ( iflag/=0 ) GOTO 300
!
!     DETERMINE NODAL THICKNESSES
!
      avgthk = 0.0
      DO i = 1 , Nnode
         iord = Iorder(i)
         DO ic = 1 , 3
            curvtr(ic,iord) = gpnorm(ic+1,i)
         ENDDO
!
         IF ( gpth(i)==0.0 ) gpth(i) = elth
         IF ( nest(13)==0 .AND. elth==0. ) gpth(i) = 1.0E-14
         IF ( gpth(i)>0.0 ) THEN
            dgpth(i) = gpth(i)
            avgthk = avgthk + dgpth(i)/Nnode
         ELSE
            WRITE (nout,99001) Ufm , Elid
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
!      EST(18) = TRNASVERSE SHEAR  PARAMETER, TS/T
!
!     0.8333333 = 5.0/6.0
!
      mominr = 0.0
      tsfact = .8333333
      nocsub = .FALSE.
      IF ( nest(15)/=0 ) mominr = Est(16)
      IF ( nest(17)/=0 ) ts = Est(18)
      IF ( Est(18)==0. ) ts = .8333333
!
!     FIX FOR LAMINATED COMPOSITE WITH MEMBRANE BEHAVIOUR ONLY.
!     REQUIRED TO PREVENT ZERO DIVIDE ERRORS.
!
      IF ( nest(15)==0 .AND. nest(13)>100000000 ) ts = .8333333
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
!     SET IDENTT FLAG TO 1 IF TEB IS AN IDENTITY MATRIX
!
      CALL betrns(teu,gge,0,Elid)
      CALL gmmats(teu,3,3,0,tub,3,3,0,teb)
      CALL gmmats(tub,3,3,1,cent,3,1,0,cente)
      identt = 0
      IF ( teb(1)==1.0 .AND. teb(5)==1.0 .AND. teb(9)==1.0 .AND. teb(2)==0.0 .AND. teb(3)==0.0 .AND. teb(4)==0.0 .AND. teb(6)       &
         & ==0.0 .AND. teb(7)==0.0 .AND. teb(8)==0.0 ) identt = 1
      ip = -3
      DO ii = 2 , 4
         ip = ip + 3
         DO j = 1 , Nnode
            epnorm(ii,j) = 0.0
            egpdt(ii,j) = 0.0
            DO k = 1 , 3
               kk = ip + k
               k1 = k + 1
               cc = bgpdt(k1,j) - ggu(k) - cente(k)
               epnorm(ii,j) = epnorm(ii,j) + teb(kk)*gpnorm(k1,j)
               egpdt(ii,j) = egpdt(ii,j) + (teb(kk)*cc)
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
      Inflag = 12
      rho = 0.0
      Eltemp = Est(45)
      mid(1) = nest(13)
      mid(2) = nest(15)
      mid(3) = nest(17)
      mid(4) = nest(22)
      Membrn = mid(1)>0
      Bendng = mid(2)>0 .AND. mominr>0.0
      Shrflx = mid(3)>0
      Mbcoup = mid(4)>0
!
!     FIGURE OUT PATH OF THE TRIPLE MULTIPLY AND THE NO. OF ROWS
!     IN B-MATRIX
!
!     NORPTH = MID(1).EQ.MID(2).AND.MID(1).EQ.MID(3).AND.MID(4).EQ.0
!    1        .AND. ABS(MOMINR-1.0).LE.EPS1
      Norpth = .FALSE.
!
!     DETERMINE FACTORS TO BE USED IN CSUBB CALCULATIONS
!
!     IF (.NOT.BENDNG) GO TO 290
      DO i = 1 , 4
         DO j = 1 , Nnode
            jo = Iorder(j)
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
      a = 0.5*abs(xa(2)+xa(3)-xa(1)-xa(4))
      b = 0.5*abs(yb(4)+yb(3)-yb(1)-yb(2))
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
      csubb4 = 1.6
!     IF (ICSUBB .NE. 0) CSUBB4 = SYS(49)
      csubt = 71.0*aspect*(1.60/csubb4)*(1.0+415.0*aspect*thlen**2)
      csubtx = csubt*aspctx**2
      csubty = csubt*aspcty**2
!
      i = 2
      j = 2
      jj = 3
      sineax = 0.0
      sineay = 0.0
   ELSE
      CALL mesage(-30,234,nam)
      GOTO 600
   ENDIF
   DO
      CALL saxb(curvtr(1,i-1),curvtr(1,i),curve)
      cc = curve(1)*curve(1) + curve(2)*curve(2) + curve(3)*curve(3)
      IF ( cc>=eps1 ) cc = 0.5*sqrt(cc)
      sineax = sineax + cc
      IF ( i/=2 ) EXIT
      i = 4
   ENDDO
   DO
!
      CALL saxb(curvtr(1,j),curvtr(1,jj),curve)
      cc = curve(1)*curve(1) + curve(2)*curve(2) + curve(3)*curve(3)
      IF ( cc>=eps1 ) cc = 0.5*sqrt(cc)
      sineay = sineay + cc
      IF ( j/=2 ) THEN
         cc = 28.0
         sineax = cc*sineax + 1.0
         sineay = cc*sineay + 1.0
         IF ( sineax>sineay ) sineay = sineax
         IF ( sineay>sineax ) sineax = sineay
!
!     IRREGULAR 4-NODE CODE-  GEOMETRIC VARIABLES
!
!     CALCULATE AND NORMALIZE- UNIT EDGE VECTORS,UNIT NORMAL VECTORS
!
         DO i = 1 , 4
            j = i + 1
            IF ( j==5 ) j = 1
            Uev(1,i) = xa(j) - xa(i)
            Uev(2,i) = yb(j) - yb(i)
            Uev(3,i) = zc(j) - zc(i)
            Unv(1,i) = (vnt(1,j)+vnt(1,i))*0.50
            Unv(2,i) = (vnt(2,j)+vnt(2,i))*0.50
            Unv(3,i) = (vnt(3,j)+vnt(3,i))*0.50
            cc = Uev(1,i)**2 + Uev(2,i)**2 + Uev(3,i)**2
            IF ( cc==0.0 ) GOTO 300
            IF ( cc>=eps1 ) cc = sqrt(cc)
            Edgel(i) = cc
            Uev(1,i) = Uev(1,i)/cc
            Uev(2,i) = Uev(2,i)/cc
            Uev(3,i) = Uev(3,i)/cc
            cc = Unv(1,i)**2 + Unv(2,i)**2 + Unv(3,i)**2
            IF ( cc==0.0 ) GOTO 300
            IF ( cc>=eps1 ) cc = sqrt(cc)
            Unv(1,i) = Unv(1,i)/cc
            Unv(2,i) = Unv(2,i)/cc
            Unv(3,i) = Unv(3,i)/cc
         ENDDO
!
!     CALCULATE INTERNAL NODAL ANGLES
!
         DO i = 1 , 4
            j = i - 1
            IF ( j==0 ) j = 4
            Anglei(i) = -Uev(1,i)*Uev(1,j) - Uev(2,i)*Uev(2,j) - Uev(3,i)*Uev(3,j)
            IF ( abs(Anglei(i))<eps1 ) Anglei(i) = 0.0
         ENDDO
! 290 CONTINUE
!
!     SET THE INTEGRATION POINTS
!
         ptint(1) = -const
         ptint(2) = const
!     JZTA = 2
!     IF (.NOT.BENDNG) PTINTZ(1) = 0.0
!     IF (.NOT.BENDNG) JZTA = 1
         IF ( Heat ) GOTO 600
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
               Xi = ptint(ixsi)
!
               DO ieta = 1 , 2
                  Eta = ptint(ieta)
!
                  CALL q4shps(Xi,Eta,shp,dshp)
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
                        Edgshr(is,ij) = (Uev(is,ij)+Anglei(ij)*Uev(is,ii))*aa/(1.0-Anglei(ij)*Anglei(ij))                           &
                                      & + (Uev(is,ij)+Anglei(ik)*Uev(is,ik))*bb/(1.0-Anglei(ik)*Anglei(ik))
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
                     kk = Iorder(is)
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
                     Hzta = zta/2.0
                     CALL jacobs(Elid,shp,dshp,dgpth,egpdt,epnorm,jacob)
                     IF ( Badjac ) GOTO 400
!
!     COMPUTE PSI TRANSPOSE X JACOBIAN INVERSE.
!     HERE IS THE PLACE WHERE THE INVERSE JACOBIAN IS FLAGED TO BE
!     TRANSPOSED BECAUSE OF OPPOSITE MATRIX LOADING CONVENTION
!     BETWEEN INVER AND GMMAT.
!
                     CALL gmmats(Psitrn,3,3,0,jacob,3,3,1,phi)
!
!     CALL Q4BMGS TO GET B MATRIX
!     SET THE ROW FLAG TO 1. IT SIGNALS SAVING THE LAST 2 ROWS.
!
                     Rowflg = 1
                     CALL q4bmgs(dshp,dgpth,egpdt,epnorm,phi,bmat1(kpt))
                     kpt = kpt + nd2
                  ENDDO
               ENDDO
            ENDDO
!
!     IN PLANE SHEAR REDUCTION
!
!     IF (.NOT.MEMBRN) GO TO 400
! 360 CONTINUE
            Xi = 0.0
            Eta = 0.0
            kpt = 1
            kpnt = nd2
!     IF (NORPTH) KPNT = NDOF
!
            CALL q4shps(Xi,Eta,shp,dshp)
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
!     DO 390 IZTA = 1,JZTA
            DO izta = 1 , 2
               zta = ptint(izta)
               Hzta = zta/2.0
               CALL jacobs(Elid,shp,dshp,dgpth,egpdt,epnorm,jacob)
               IF ( Badjac ) GOTO 400
!
               CALL gmmats(Psitrn,3,3,0,jacob,3,3,1,phi)
!
!     CALL Q4BMGS TO GET B-MATRIX
!     SET THE ROW FLAG TO 2. IT WILL SAVE THE 3RD ROW OF B-MATRIX AT
!     THE TWO INTEGRATION POINTS.
!
               Rowflg = 2
               CALL q4bmgs(dshp,dgpth,egpdt,epnorm,phi,xybmat(kpt))
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
!     SET THE ARRAY OF LENGTH 4 TO BE USED IN CALLING TRANSS.
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
!     1- IF MCSID HAS BEEN SPECIFIED, SUBROUTINE TRANSS IS CALLED
!        TO CALCULATE TBM-MATRIX (MATERIAL TO BASIC TRANSFORMATION).
!        TBM-MATRIX IS THEN PREMULTIPLIED BY TEB-MATRIX TO OBTAIN
!        TEM-MATRIX.
!        THEN USING THE PROJECTION OF X-AXIS, AN ANGLE IS CALCULATED
!        UPON WHICH STEP 2 IS TAKEN.
!
!     2- IF THETAM HAS BEEN SPECIFIED, SUBROUTINE ANGTRS IS CALLED
!        TO CALCULATE TEM-MATRIX (MATERIAL TO ELEMENT TRANSFORMATION).
!
!                         T
!     3-           G  =  U   G   U
!                   E         M
!
!
   IF ( nest(11)==0 ) THEN
!
!     CALCULATE TEM-MATRIX USING THETAM
!
      thetam = Est(10)*Degrad
      IF ( thetam/=0.0 ) GOTO 200
!
!     DEFAULT IS CHOSEN, LOOK FOR VALUES OF MCSID AND/OR THETAM
!     ON THE PSHELL CARD.
!
      IF ( nest(24)==0 ) THEN
!
         thetam = Est(23)*Degrad
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
      CALL transs(ecpt,tbm)
!
!     MULTIPLY TEB AND TBM MATRICES
!
      CALL gmmats(teb,3,3,0,tbm,3,3,0,tem)
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
   IF ( abs(xm)>eps1 .OR. abs(ym)>eps1 ) THEN
      thetam = atan2(ym,xm)
   ELSE
      nest(2) = mcsid
      j = 231
      GOTO 500
   ENDIF
 200  CALL angtrs(thetam,1,tum)
   CALL gmmats(teu,3,3,0,tum,3,3,0,tem)
!
   IF ( Heat ) THEN
      tem(3) = tem(4)
      tem(4) = tem(5)
      CALL gmmats(tem,2,2,0,gi,2,2,0,gt)
      CALL gmmats(gt,2,2,0,tem,2,2,1,gi)
      GOTO 700
   ELSE
!
      DO m = 1 , 36
         gi(m) = 0.0
      ENDDO
      Sinmat = 0.0
      Cosmat = 0.0
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
            IF ( gi(19)/=0.0 .OR. gi(20)/=0.0 .OR. gi(21)/=0.0 .OR. gi(22)/=0.0 ) EXIT
            igobk = 1
            m = 2
            mid(3) = mid(2)
         ELSE
            IF ( m==4 .AND. igobk==1 ) EXIT
            Matid = mid(m)
            IF ( Matid/=0 .OR. m==3 ) THEN
               IF ( .NOT.(Matid==0 .AND. m==3 .AND. .NOT.Bendng) ) THEN
                  IF ( Matid==0 .AND. m==3 .AND. Bendng ) Matid = mid(2)
!
                  IF ( m<1 ) THEN
                  ELSEIF ( m==1 ) THEN
                     CALL mat(Elid)
                  ELSEIF ( Matid/=mid(m-1) .OR. igobk/=0 ) THEN
                     CALL mat(Elid)
                  ENDIF
!
                  IF ( Membrn .AND. m==1 ) rho = Matout(7)
                  rhox = rho
                  IF ( rho==0.0 ) rhox = 1.0
                  IF ( kgg1/=0 ) THEN
!
                     IF ( .NOT.(Membrn .AND. m/=1 .OR. .NOT.Membrn .AND. m/=2) ) THEN
                        gsube = Matout(12)
                        IF ( matset==8. ) gsube = Matout(16)
                     ENDIF
!
                     IF ( .NOT.(m==2 .AND. Norpth) ) THEN
                        coeff = 1.0
                        lpoint = (m-1)*9 + 1
!
                        CALL q4gmgs(m,coeff,gi(lpoint))
!
!WKBDB 11/93 SPR93020
!      IF (M .GT. 0) GO TO 670
!      IF (.NOT.SHRFLX .AND. BENDNG) GO TO 660
!      NEST(2) = MATID
!      J = 232
!      GO TO 1720
!
!  660 M = -M
! 11/93 ALREADY DELETED 670 IF (.NOT.BENDNG) GO TO 760
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
!      IF (GNORX .EQ. 0.0) GNORX = MATOUT(4)
!      IF (GNORY .EQ. 0.0) GNORY = MATOUT(4)
!  760 CONTINUE
!WKBDE 11/93 SPR93020
!
!
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
                           u(7) = tem(1)*tem(2)*2.0
                           u(8) = tem(4)*tem(5)*2.0
                           u(9) = tem(1)*tem(5) + tem(2)*tem(4)
                           l = 3
                        ENDIF
!
                        CALL gmmats(u(1),l,l,1,gi(lpoint),l,l,0,gt(1))
                        CALL gmmats(gt(1),l,l,0,u(1),l,l,0,gi(lpoint))
!WKBNB 11/93 SPR93020
                        IF ( m<=0 ) THEN
                           IF ( .NOT.Shrflx .AND. Bendng ) THEN
                              m = -m
                           ELSE
                              nest(2) = Matid
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
                              gnorx = Matout(6)
                              gnory = Matout(6)
                           ELSEIF ( mtype==0 ) THEN
                              gnorx = Matout(1)
                              gnory = Matout(4)
                           ELSE
                              gnorx = Matout(6)
                              gnory = Matout(5)
!WKBDB 9/94
!      IF ( GNORX .EQ. 0.0D0 ) GNORX = MATOUT(4)
!      IF ( GNORY .EQ. 0.0D0 ) GNORY = MATOUT(4)
!WKBDE 9/94
!WKBNB 9/94
                              IF ( gnorx==0.0 ) gnorx = Matout(4)
!WKBNE 9/94
!WKBNE 2/94 SPR93020
                              IF ( gnory==0.0 ) gnory = Matout(4)
                           ENDIF
!WKBNE 11/93 SPR93020
!WKBNB 2/94 SPR93020
                        ELSEIF ( mtype<0 ) THEN
                           enorx = Matout(16)
                           enory = Matout(16)
                           dnux = gi(lpoint+1)/gi(lpoint)
                           dnuy = gi(lpoint+3)/gi(lpoint+4)
                        ELSEIF ( mtype==0 ) THEN
                           enorx = Matout(1)
                           enory = Matout(4)
                           dnux = gi(lpoint+1)/gi(lpoint)
                           dnuy = gi(lpoint+3)/gi(lpoint+4)
                        ELSE
                           enorx = Matout(1)
                           enory = Matout(3)
                           dnux = gi(lpoint+1)/gi(lpoint)
                           dnuy = gi(lpoint+3)/gi(lpoint+4)
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDDO
!
      nocsub = enorx==0.0 .OR. enory==0.0 .OR. gnorx==0.0 .OR. gnory==0.0 .OR. mominr==0.0
!
      mattyp = ifix(matset+.05)
!
!     IF MGG1 IS NON-ZERO AND RHO IS GREATER THAN 0.0,
!     THEN COMPUTE THE MASS MATRIX.
!
      IF ( mgg1/=0 ) THEN
         IF ( jcored+144>ncored ) THEN
            CALL mesage(-30,234,nam)
            GOTO 600
         ENDIF
      ENDIF
!
      limit = jcored + Ndof*Ndof
      DO i = jcored , limit
         Akgg(i) = 0.0
      ENDDO
      DO i = 1 , nodesq
         xmass(i) = 0.0
         xmtmp(i) = 0.0
      ENDDO
      area = 0.0
      vol = 0.0
!
!
!     HERE BEGINS THE TRIPLE LOOP ON STATEMENTS 1310 AND 1300 TO
!     GAUSS INTEGRATE FOR THE ELEMENT MASS AND STIFFNESS MATRICES.
!     -----------------------------------------------------------
!
      DO ixsi = 1 , 2
         Xi = ptint(ixsi)
         DO ieta = 1 , 2
            Eta = ptint(ieta)
            CALL q4shps(Xi,Eta,shp,dshp)
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
            CALL gmmats(shp,1,Nnode,0,dgpth,1,Nnode,1,thk)
            reali = mominr*thk*thk*thk/12.0
!     REALI =        THK*THK*THK/12.0
            tsi = ts*thk
!
!     SKIP MASS CALCULATIONS IF NOT REQUESTED
!
            IF ( nsm==0. ) THEN
               IF ( mgg1==0 ) GOTO 210
               IF ( rho==0. ) GOTO 210
               IF ( rho<=0. ) THEN
                  WRITE (nout,99002) Uwm , rho , mid(1) , nest(1)
99002             FORMAT (A25,', RHO = ',1PD12.4,' IS ILLEGAL FROM MATERIAL ID =',I9,' FOR QUAD4 EID =',I9)
               ENDIF
            ENDIF
!     NOGO =.TRUE.
!     GO TO 1710
!
!     COMPUTE S AND T VECTORS AT THE MID-SURFACE
!     FOR MASS CALCULATIONS ONLY.
!
            DO i = 1 , 2
               ipoint = 4*(i-1)
               DO j = 1 , 3
                  v(i,j) = 0.0
                  DO k = 1 , Nnode
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
!WKBR spr 93015  IF (AREA2 .LT. EPS1) GO TO 1700
            IF ( area2<=0.0 ) GOTO 300
!
            area2 = sqrt(area2)
            area = area + area2
            voli = area2*thk
            vol = vol + voli
!
            IF ( mgg1/=0 ) THEN
               IF ( Cpmass>0 ) THEN
!
!     COMPUTE CONSISTENT MASS MATRIX
!
!     COMPUTE THE CONTRIBUTION TO THE MASS MATRIX
!     FROM THIS INTEGRATION POINT.
!
                  CALL gmmats(shp,1,Nnode,1,shp,1,Nnode,0,xmtmp)
!
!     ADD MASS CONTRIBUTION FROM THIS INTEGRATION POINT
!     TO THE ELEMENT MASS MATRIX.
!
                  DO i = 1 , nodesq
                     xmass(i) = xmass(i) + voli*rhox*xmtmp(i)
                  ENDDO
               ELSE
                  i4 = 1
                  DO j4 = 1 , Nnode
                     xmass(i4) = xmass(i4) + voli*rhox*shp(j4)
                     i4 = i4 + Nnode + 1
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
            sfcty1 = 1.0
            sfcty2 = 1.0
            sfctx1 = 1.0
            sfctx2 = 1.0
            tsmfx = 1.0
            tsmfy = 1.0
            IF ( .NOT.(nocsub) ) THEN
               IF ( Bendng ) THEN
!      NUNORX = MOMINR*ENORX/(2.0*GNORX) - 1.0
!      NUNORY = MOMINR*ENORY/(2.0*GNORY) - 1.0
!WKBNB 2/94 SPR93020
                  nunorx = mominr*enorx/(2.0*gnorx) - 1.0
                  nunory = mominr*enory/(2.0*gnory) - 1.0
!WKBNE 2/94 SPR93020
!
!     NOTE- THE ABOVE EXPRESSIONS FOR NUNORX AND NUNORY WERE MODIFIED
!           BY G.CHAN/UNISYS    1988
!
!WKBDB 2/94 SPR93020
!      EIX = MOMINR*ENORX
!      EIY = MOMINR*ENORY
!      TGX = 2.0*GNORX
!      TGY = 2.0*GNORY
!      NUNORX = EIX/TGX - 1.0
!      NUNORY = EIY/TGY - 1.0
!      IF (EIX .GT. TGX) NUNORX = 1.0 - TGX/EIX
!      IF (EIY .GT. TGY) NUNORY = 1.0 - TGY/EIY
!WKBDE 2/94 SPR93020
                  IF ( nunorx>0.999999 ) nunorx = 0.999999
                  IF ( nunory>0.999999 ) nunory = 0.999999
!WKBNB 2/94 SPR93020
                  IF ( nunorx<=0. ) nunorx = dnux
                  IF ( nunory<=0. ) nunory = dnuy
!WKBNE 2/94 SPR93020
!     IF (NUNORX .GT. .49) NUNORX = 0.49
!     IF (NUNORY .GT. .49) NUNORY = 0.49
                  cc = aspect
!
!     NOTE- THE FOLLOWING 2 FORMULATIONS WERE PUT IN ON 4/30/85 IN
!           CONJUNCTION WITH THE OUT-OF-PLANE SHEAR CORRECTION A LA
!           HUGHES. THE FLEXIBLE SOLUTION PROVIDES MORE ACCURATE
!           RESULTS FOR PLATES, ALTHOUGH IT MIGHT CONVERGE SLOWLY.
!           THE STIFFER SOLUTION (COMMENTED OUT) IS O.K. FOR PLATES
!           AND SHOULD HAVE A BETTER CONVERGENCE.
!
!           THEY WERE MODIFIED ON 5/3/85
!
!
!     4-NODE CSUBB FORMULATION AS OF 5/3/85 (FLEXIBLE SOLUTION)
!     REPLACES THE ONE COMMENTED OUT IMMEDIATELY ABOVE
!
                  w1 = 1.0 + 4400.0*thlen*thlen*thlen*thlen
                  IF ( cc<0.2 ) THEN
                     dsub4 = (159.85*cc-15.97)*w1
                  ELSE
                     dsub4 = (18.375-11.875*cc)*w1
                  ENDIF
!
!     4-NODE CSUBB FORMULATION AS OF 5/3/85 (STIFFER SOLUTION)
!
!     W1 = 1.0 + 2.5*THLEN + 1.04*THLEN**5
!     IF (CC .LT. 0.2) GO TO 1030
!     DSUB4 = 18.0*W1
!     GO TO 1040
!1030 DSUB4 = (179.85*CC-17.97)*W1
                  IF ( dsub4<0.01 ) dsub4 = 0.01
                  IF ( dsub4>2000.0 ) dsub4 = 2000.0
                  dsub = dsub4
                  coeft = const
                  ax = a
                  IF ( Eta<0.0 ) ax = a + coeft*(xa(2)-xa(1)-a)
                  IF ( Eta>0.0 ) ax = a + coeft*(xa(3)-xa(4)-a)
                  psiinx = 20.0*dsub*reali*sineax*(1.0+aspect*aspect)/(tsi*(1.0-nunorx)*ax*ax)
                  dsub = dsub4
                  coeft = const
                  by = b
                  IF ( Xi<0.0 ) by = b + coeft*(yb(4)-yb(1)-b)
                  IF ( Xi>0.0 ) by = b + coeft*(yb(3)-yb(2)-b)
                  psiiny = 20.0*dsub*reali*sineay*(1.0+aspect*aspect)/(tsi*(1.0-nunory)*by*by)
                  IF ( .NOT.Shrflx ) THEN
                     tsmfx = psiinx
                     tsmfy = psiiny
                  ELSE
                     tsmfx = psiinx/(1.0+psiinx)
                     tsmfy = psiiny/(1.0+psiiny)
                  ENDIF
!
                  IF ( tsmfx<=0.0 ) tsmfx = eps1
                  IF ( tsmfy<=0.0 ) tsmfy = eps1
!
!     FILL IN THE 7X7 MATERIAL PROPERTY MATRIX D FOR NORPTH
!
                  IF ( Norpth ) THEN
                     DO ig = 1 , 7
                        DO jg = 1 , 7
                           dfour(ig,jg) = 0.0
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
                  gfour(ig,jg) = 0.0
               ENDDO
            ENDDO
            IF ( .NOT.(Mbcoup) ) THEN
!
               IF ( Membrn ) THEN
                  DO ig = 1 , 3
                     ig1 = (ig-1)*3
                     DO jg = 1 , 3
                        jg1 = jg + ig1
                        gfour(ig,jg) = gi(jg1)
                     ENDDO
                  ENDDO
               ENDIF
!
               IF ( .NOT.Bendng ) GOTO 230
               DO ig = 4 , 6
                  ig2 = (ig-2)*3
                  DO jg = 4 , 6
                     jg2 = jg + ig2
                     gfour(ig,jg) = gi(jg2)*mominr
                  ENDDO
               ENDDO
!
               IF ( Membrn ) THEN
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
                  IF ( ij==Iorder(ir) ) THEN
                     ioj = ir
                     EXIT
                  ENDIF
               ENDDO
               DO ir = 1 , 4
                  IF ( ik==Iorder(ir) ) THEN
                     iok = ir
                     EXIT
                  ENDIF
               ENDDO
               aa = shp(ioj)
               bb = shp(iok)
!
               DO is = 1 , 3
                  Edgshr(is,ij) = (Uev(is,ij)+Anglei(ij)*Uev(is,ii))*aa/(1.0-Anglei(ij)*Anglei(ij))                                 &
                                & + (Uev(is,ij)+Anglei(ik)*Uev(is,ik))*bb/(1.0-Anglei(ik)*Anglei(ik))
               ENDDO
            ENDDO
!
!     TORSION-RELATED SHEAR CORRECTION FOR 4-NODE-
!     SET-UP OF EXPANDED SHEAR MATERIAL PROPERTY MATRICES (G OR D)
!
            csubx = 20.0*reali/(tsi*(1.0-nunorx)*a*a)
            csuby = 20.0*reali/(tsi*(1.0-nunory)*b*b)
            sfctr1 = csubb4*csubx
            sfctr2 = csubtx*csubx
            IF ( Shrflx ) THEN
               sfctr1 = sfctr1/(1.0+sfctr1)
               sfctr2 = sfctr2/(1.0+sfctr2)
            ENDIF
            sfctx1 = sfctr1 + sfctr2
            sfctx2 = sfctr1 - sfctr2
            sfctr1 = csubb4*csuby
            sfctr2 = csubty*csuby
            IF ( Shrflx ) THEN
               sfctr1 = sfctr1/(1.0+sfctr1)
               sfctr2 = sfctr2/(1.0+sfctr2)
            ENDIF
            sfcty1 = sfctr1 + sfctr2
            sfcty2 = sfctr1 - sfctr2
!
!     FILL IN THE EXPANDED MATERIAL PROPERTY MATRIX
!
            IF ( Norpth ) THEN
!
               dfour(4,4) = 0.25*sfcty1*ts*gi(19)
               dfour(5,5) = 0.25*sfcty1*ts*gi(19)
               dfour(5,4) = 0.25*sfcty2*ts*gi(19)
               dfour(4,5) = dfour(5,4)
               dfour(6,6) = 0.25*sfctx1*ts*gi(22)
               dfour(7,7) = 0.25*sfctx1*ts*gi(22)
               dfour(7,6) = 0.25*sfctx2*ts*gi(22)
               dfour(6,7) = dfour(7,6)
               dfour(4,6) = sqrt(tsmfx*tsmfy)*ts*gi(20)
               dfour(6,4) = dfour(4,6)
            ELSE
               gfour(7,7) = 0.25*sfcty1*ts*gi(19)
               gfour(8,8) = 0.25*sfcty1*ts*gi(19)
               gfour(8,7) = 0.25*sfcty2*ts*gi(19)
               gfour(7,8) = gfour(8,7)
               gfour(9,9) = 0.25*sfctx1*ts*gi(22)
               gfour(10,10) = 0.25*sfctx1*ts*gi(22)
               gfour(10,9) = 0.25*sfctx2*ts*gi(22)
               gfour(9,10) = gfour(10,9)
               gfour(7,9) = sqrt(tsmfx*tsmfy)*ts*gi(20)
               gfour(9,7) = gfour(7,9)
            ENDIF
!
!     DO 1300 IZTA = 1,JZTA
 230        DO izta = 1 , 2
               zta = ptint(izta)
               Ibot = (izta-1)*nd2
!
               Hzta = zta/2.0
!
!     TORSION-RELATED SHEAR CORRECTION FOR 4-NODE-
!     SET-UP OF POINTERS TO THE SAVED B-MATRIX
!
               Iptx1 = ((ixsi-1)*2+ieta-1)*2*nd2 + Ibot
               Iptx2 = ((ixsi-1)*2+2-ieta)*2*nd2 + Ibot
               Ipty1 = ((ixsi-1)*2+ieta-1)*2*nd2 + Ibot
               Ipty2 = ((2-ixsi)*2+ieta-1)*2*nd2 + Ibot
!     IF (NORPTH) IBOT = IBOT/2
!
!     FILL IN THE 10X10 G-MATRIX IF MID4 IS PRESENT
!
               IF ( Mbcoup ) THEN
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
                        gfour(ig,lg) = -gi(jg4)*zta*6.0 + gi(jg1)
                        gfour(kg,jg) = -gi(jg4)*zta*6.0 + gi(jg1)
                     ENDDO
                  ENDDO
               ENDIF
!
!     COMPUTE THE JACOBIAN AT THIS GAUSS POINT,
!     ITS INVERSE AND ITS DETERMINANT.
!
               CALL jacobs(Elid,shp,dshp,dgpth,egpdt,epnorm,jacob)
               IF ( Badjac ) GOTO 400
!
!     COMPUTE PSI TRANSPOSE X JACOBIAN INVERSE.
!     HERE IS THE PLACE WHERE THE INVERSE JACOBIAN IS FLAGED TO BE
!     TRANSPOSED BECAUSE OF OPPOSITE MATRIX LOADING CONVENTION
!     BETWEEN INVER AND GMMAT.
!
               CALL gmmats(Psitrn,3,3,0,jacob,3,3,1,phi)
!
!     CALL Q4BMGS TO GET B-MATRIX.  SET THE ROW FLAG TO 3.
!     IT WILL RETURN THE FIRST 6 ROWS OF B-MATRIX.
!
               Rowflg = 3
               CALL q4bmgs(dshp,dgpth,egpdt,epnorm,phi,bfour(1))
!
!     SET-UP OF B-MATRIX AND TRIPLE MULTIPLY
!
               CALL trplms(gfour,dfour,bfour,bmat1,xybmat,mattyp,jcored,Detj)
            ENDDO
         ENDDO
      ENDDO
!
!     EQUALIZE THE OFF-DIAGONAL TERMS TO GUARANTEE PERFECT SYMMETRIC
!     MATRIX IF NO DAMPING INVOLVED
!
      IF ( gsube==0.0 ) THEN
         ij = jcored - 1
         ndofm1 = Ndof - 1
         DO ii = 1 , ndofm1
            ip1 = ii + 1
            im1 = (ii-1)*Ndof + ij
            DO jj = ip1 , Ndof
               i = im1 + jj
               j = (jj-1)*Ndof + ii + ij
               temp = (Akgg(i)+Akgg(j))*.5
               IF ( abs(temp)<1.0E-17 ) temp = 0.0
               Akgg(i) = temp
               Akgg(j) = temp
            ENDDO
         ENDDO
      ENDIF
!
!     END OF STIFFNESS LOOP
!
!     ADD NON-STRUCTURAL MASS
!
 250  IF ( mgg1/=0 ) THEN
         IF ( rho/=0.0 .OR. nsm/=0.0 ) THEN
!     IF (CPMASS .GT. 0) GO TO 1410
            IF ( nsm/=0.0 ) THEN
               IF ( vol==0. .OR. rhox==0. ) WRITE (nout,99003) Sfm , Elid , area , vol , rhox , mgg1 , kgg1
99003          FORMAT (A25,', ZERO VOLUME OR DENSITY FOR QUAD4 ELEMENT ID =',I9,', AREA,VOL,RHO=',3E12.3,/70X,'MGG1,KGG1=',2I8)
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
         trans(i) = 0.0
      ENDDO
!     DO 1414 I = 2,8
!1414 TRANS1(I) = 0.0
!     TRANS1(1) = 1.0
!     TRANS1(5) = 1.0
!     TRANS1(9) = 1.0
!
      DO i = 1 , Nnode
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
            CALL transs(gpth,tbg)
            CALL gmmats(teb,3,3,0,tbg,3,3,0,trans(ipoint))
!
         ELSEIF ( identt/=1 .OR. offset/=0.0 ) THEN
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
!     ---------------------------------------------------
!
      IF ( kgg1/=0 ) THEN
!
!     SET UP I-LOOP TO DUMP OUT BASIC TO GLOBAL TRANSFORMED, NODAL
!     PARTITIONED (6 D.O.F. PER NODE) COLUMNS OF THE ELEMENT STIFFNESS.
!
!     THIS MEANS WE ARE SENDING TO EMGOUT 6 COLUMNS OF THE ELEMENT
!     STIFFNESS MATRIX AT TIME.  EACH BUNCH OF 6 COLUMNS CORRESPOND
!     TO ONE PARTICULAR NODE OF THE ELEMENT. FOR THE MASS MATRIX, WE
!     ONLY SEND 3 COLUMNS PER NODE TO EMGOUT SINCE THE OTHER 3 D.O.F.
!     ARE ZERO ANYWAY.  THE CODE WORD (DICT(4)) TELLS EMGOUT WHICH
!     COLUMNS ARE THE NON ZERO ONES THAT WE ARE SENDING. (SEE SECTION
!     6.8.3.5.1 OF THE PROGRAMMER MANUAL)
!
!
         dict(1) = Estid
         dict(2) = 1
         dict(3) = Ndof
         dict(4) = 63
         npart = Ndof*6
         DO i = 1 , Nnode
            ibegin = 6*(i-1) + jcored - 1
!
!     DUMP AN UNTRANSFORMED NODAL COLUMN PARTITION.
!
            DO j = 1 , Ndof
               kpoint = Ndof*(j-1) + ibegin
               lpoint = 6*(j-1)
               DO k = 1 , 6
                  colstf(lpoint+k) = Akgg(kpoint+k)
               ENDDO
            ENDDO
            IF ( notran(i)/=1 ) THEN
!
!     THIS COLUMN PARTITION NEEDS TO BE TRANSFORMED TO GLOBAL
!     COORDINATES. (SEE PAGE 2.3-43 OF THE PROGRAMMER)
!
!     LOAD THE 6X6 TRANSFORMATION
!
               CALL tldrs(offset,i,trans,trans1)
!
!     TRANSFORM THE NODAL COLUMN PARTITION.
!
               CALL gmmats(colstf,Ndof,6,0,trans1,6,6,0,coltmp)
               DO ii = 1 , npart
                  colstf(ii) = coltmp(ii)
               ENDDO
            ENDIF
!
!     NOW TRANSFORM THE ROWS OF THIS PARTITION.
!
            DO m = 1 , Nnode
               IF ( notran(m)/=1 ) THEN
                  mpoint = 36*(m-1) + 1
!
!     LOAD THE 6X6 TRANSFORMATION
!
                  CALL tldrs(offset,m,trans,trans1)
!
!     TRANSFORM THE 6 ROWS FOR THIS SUBPARTITION
!
                  CALL gmmats(trans1,6,6,1,colstf(mpoint),6,6,0,coltmp)
                  iipnt = mpoint - 1
                  DO ii = 1 , 36
                     colstf(iipnt+ii) = coltmp(ii)
                  ENDDO
               ENDIF
            ENDDO
!
!     HERE WE MUST CHANGE FROM THE ROW LOADING CONVENTION
!     FOR GMMATS TO THE COLUMN LOADING CONVENTION FOR EMGOUT.
!
            DO ii = 1 , 6
               ipoint = Ndof*(ii-1)
               DO jj = 1 , Ndof
                  jpoint = 6*(jj-1)
                  coltmp(ipoint+jj) = colstf(jpoint+ii)
               ENDDO
            ENDDO
!
!     DUMP THE TRANSFORMED NODAL COLUMN PARTITION
!
            ieoe = 0
            IF ( i==Nnode ) ieoe = 1
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
!     --------------------------------
!
      IF ( mgg1/=0 ) THEN
!
         Ndof = Nnode*3
         npart = Ndof*3
         dict(3) = Ndof
         dict(4) = 7
         adamp = 0.0
!
!     SET UP I-LOOP TO PROCESS AND DUMP THE NODAL COLUMN PARTITIONS.
!
         DO i = 1 , Nnode
            DO ijk = 1 , npart
               amgg(jcored-1+ijk) = 0.0
            ENDDO
!
!     SET UP J-LOOP TO LOAD THE UNTRANSFORMED NODAL COLUMN PARTITION.
!
            DO j = 1 , Nnode
               ipoint = 9*(j-1) + jcored
               jpoint = ipoint + 4
               kpoint = ipoint + 8
               ifrom = Nnode*(j-1) + i
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
               DO m = 1 , Nnode
                  mpoint = 9*(m-1) + jcored
                  CALL gmmats(amgg(mpoint),3,3,0,trans(9*i-8),3,3,0,tmpmas)
                  iicore = mpoint - 1
                  DO k = 1 , 9
                     amgg(iicore+k) = tmpmas(k)
                  ENDDO
               ENDDO
!
!     SET UP M-LOOP TO TRANSFORM THE NODAL ROW PARTITIONS
!     OF THIS NODAL COLUMN PARTITION.
!
               DO m = 1 , Nnode
                  mpoint = 9*(m-1) + jcored
!
!     TRANSFORM THE 3 ROWS FOR THIS SUBPARTITION.  THIS IS CORRECT
!     (3 ROWS).  REMEMBER THAT FOR THE MASS MATIIX FOR THIS ELEMENT
!     THERE ARE NO MASS MOMENT OF INERTIA TERMS.  THIS GIVES THREE
!     ROWS OF ZERO TERMS INTERSPERSED BETWEEN 3 ROWS OF NONZERO
!     TRANSLATIONAL MASS TERMS FOR EACH NODE.
!
                  CALL gmmats(trans(9*m-8),3,3,1,amgg(mpoint),3,3,0,tmpmas)
                  iicore = mpoint - 1
                  DO k = 1 , 9
                     amgg(iicore+k) = tmpmas(k)
                  ENDDO
               ENDDO
            ENDIF
!
!     HERE WE MUST CHANGE FROM THE ROW LOADING CONVENTION
!     FOR GMMATS TO THE COLUMN LOADING CONVENTION FOR EMGOUT.
!
            DO ii = 1 , 3
               ipoint = Ndof*(ii-1)
               DO jj = 1 , Ndof
                  jpoint = 3*(jj-1) + jcored - 1
                  coltmp(ipoint+jj) = amgg(jpoint+ii)
               ENDDO
            ENDDO
!
!     DUMP THIS TRANSFORMED MASS NODAL COLUMN PARTITION.
!
            ieoe = 0
            IF ( i==Nnode ) ieoe = 1
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
   IF ( L38==1 ) CALL mesage(-61,0,0)
   nogo = .TRUE.
   GOTO 400
!
!
!     HEAT FLOW OPTION STARTS HERE.
!
!     WE NEED TO RESTORE THE ORIGIANL ORDER OF SILS AND BGPDT DATA
!
 600  j = 1
   DO i = 1 , 20
      Est(i+j) = save(i)
      IF ( i==4 ) j = 24
   ENDDO
!
   Inflag = 2
   Cosmat = 1.0
   Sinmat = 0.0
   Matid = nest(13)
   CALL hmat(Elid)
   gi(1) = Kheat(1)
   gi(2) = Kheat(2)
   gi(3) = gi(2)
   gi(4) = Kheat(3)
   anis = Type/=4 .AND. Type/= - 1
!     OMMENT . ANIS = .FALSE. MEANS ISOTROPIC THERMAL CONDUCTIVITY.
   IF ( anis ) GOTO 100
 700  DO i = 1 , 16
      htcon(i) = 0.0
      htcap(i) = 0.0
   ENDDO
   DO i = 5 , 8
      hsil(i) = 0
      horder(i) = 0
   ENDDO
!
   DO ixsi = 1 , 2
      Xi = ptint(ixsi)
      DO ieta = 1 , 2
         Eta = ptint(ieta)
!
         DO izta = 1 , 2
            Zeta = ptint(izta)
!
            CALL termss(Nnode,dgpth,epnorm,egpdt,horder,hsil,bterms)
            dvol = Determ
!
            DO i = 1 , 4
               ecpt(i) = gi(i)*dvol
            ENDDO
            weitc = dvol*htcp
!
            ip = 1
            DO i = 1 , Nnode
               idn = i + Nnode
               htflx(ip+1) = ecpt(3)*bterms(i) + ecpt(4)*bterms(idn)
               htflx(ip) = ecpt(1)*bterms(i) + ecpt(2)*bterms(idn)
               ip = ip + 2
            ENDDO
            CALL gmmats(bterms,2,Nnode,-1,htflx,Nnode,2,1,htcon)
!
         ENDDO
         IF ( htcp/=0.0 ) THEN
            ip = 0
            DO i = 1 , Nnode
               dheat = weitc*shp(i)
               DO j = 1 , Nnode
                  ip = ip + 1
                  htcap(ip) = htcap(ip) + dheat*shp(j)
               ENDDO
            ENDDO
         ENDIF
      ENDDO
   ENDDO
   dict(1) = Estid
   dict(2) = 1
   dict(3) = Nnode
   dict(4) = 1
   IF ( htcp/=0.0 ) THEN
      adamp = 1.0
      CALL emgout(htcap,htcap,nodesq,1,dict,3,iprec)
   ENDIF
   adamp = 0.0
   CALL emgout(htcon,htcon,nodesq,1,dict,1,iprec)
   GOTO 400
99004 FORMAT (//,' ILLEGAL GEOMETRY FOR QUAD4 ELEMENT, ID=',I10)
END SUBROUTINE quad4s
