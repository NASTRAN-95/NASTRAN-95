
SUBROUTINE squd41
   IMPLICIT NONE
   REAL Anglei(4) , Bgpdt(4,4) , Degrad , Detj , Edgel(4) , Edgshr(3,4) , Eltemp , Elth , Est(100) , Gpth(4) , Gpth2(4) , Htcp ,    &
      & Hzta , Kheat(7) , Matset , Phiout(2395) , Pi , Psitrn(9) , Raddeg , Relout(300) , Rmtout(25) , Systm(100) , Twopi , Uev(3,4)&
      & , Unv(3,4) , Zoff , Zoff1
   LOGICAL Badjac , Bendng , Mbcoup , Membrn , Norpth , Shrflx
   INTEGER Ielout(300) , Inflag , Int , Iorder(4) , Itherm , Matid , Nest(45) , Nnode , Node , Nogo , Nout , Nphi(2395) , Rowflg ,  &
         & Sil(4) , Type
   CHARACTER*23 Ufm
   COMMON /condas/ Pi , Twopi , Raddeg , Degrad
   COMMON /hmtout/ Kheat , Type
   COMMON /matin / Matid , Inflag , Eltemp
   COMMON /matout/ Rmtout
   COMMON /q4coms/ Anglei , Edgshr , Edgel , Unv , Uev , Rowflg , Iorder
   COMMON /q4dt  / Detj , Hzta , Psitrn , Nnode , Badjac , Node
   COMMON /sdr2x5/ Est , Phiout
   COMMON /sdr2x6/ Ielout
   COMMON /system/ Systm
   COMMON /terms / Membrn , Bendng , Shrflx , Mbcoup , Norpth
   COMMON /xmssg / Ufm
   REAL a , aa , alfa(3) , aspect , avgthk , ax , b , bb , bdum , bgpdm(3,4) , bmatrx(192) , by , cc , cent(3) , cente(3) , coeff , &
      & const , cosmat , detu , dnux , dnuy , dq(24) , dshp(8) , dshptp(8) , ecpt(4) , egpdt(4,4) , enorx , enory , epnorm(4,4) ,   &
      & eps , eps1 , epsi , epst , eta , exi , exj , g(6,6) , gge(9) , ggu(9) , gi(36) , gnorx , gnory , gpc(3) , gpnorm(4,4) ,     &
      & gt(9) , jacbs(9) , jacob(3,3) , jacobe(9) , jacobu(9) , mominr , nunorx , nunory , offset , pa , phi(9) , psiinx , psiiny , &
      & ptint(2) , ptintp(3) , reali , rho , shp(4) , sinmat , tbg(9) , tbm(9) , tbs(9) , teb(9) , tem(9) , teu(9) , tgrid(4,4) ,   &
      & th , thetam , thetas , tmi(9) , tmpshp(4) , tmpthk(4) , tms(9) , ts , tse(9) , tsfact
   INTEGER elid , flagm , flags , hunmeg , i , i1 , ialf , ibmx , ibot , icount , iec , iem , ieta , iflag , ig , ig1 , ig2 , ig4 , &
         & igobk , igpdt(4,4) , ii , ij , ik , imat , img , index(3,3) , io , ioj , iok , iop , ip , ip1 , ip2 , iph , ipn(4) ,     &
         & ipoint , ir , is , isil , isngu , it0 , itemp , ith , ix , ixsi , izta , j , jg , jg1 , jg2 , jj , jn , jo , k , k1 ,    &
         & kcid(8) , kcount , kg , kk , kpt , ksil(4) , l , lg , ll , lpoint , m , mcsid , mid(4) , mp , mpoint , mtype , name(2) , &
         & nd2 , nd3 , nd4 , nd5 , nd6 , nd7 , nd8 , ndof
   INTEGER necpt(4) , q4strs , scsid
   LOGICAL nocsub
   REAL tsi , tsmfx , tsmfy , tsu(9) , tsub0 , tub(9) , tum(9) , u(9) , ugpdm(3,4) , v12(3) , v12dk , v41(3) , vd1(3) , vd2(3) ,    &
      & vis(3) , vjl , vjs(3) , vkl , vkn(3) , vks(3) , vnt(3,4) , vp12(3) , vp12l , x31 , x42 , xa(4) , xi , xm , xs , xybmat(96) ,&
      & y31 , y42 , yb(4) , ym , ys , zc(4) , zeta
!
!     PHASE 1  STRESS DATA RECOVERY FOR CQUAD4 ELEMENT
!
!                EST  LISTING
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
!WKBNB 11/93 SPR 93020
!WKBNE 11/93 SPR 93020
   EQUIVALENCE (igpdt(1,1),Bgpdt(1,1)) , (Est(1),Nest(1)) , (Bgpdt(1,1),Est(29)) , (Gpth(1),Est(6)) , (Elth,Est(14)) ,              &
    & (Sil(1),Nest(2)) , (Nphi(1),Phiout(1)) , (Int,Nest(25)) , (Zoff,Nest(12)) , (Zoff1,Est(28)) , (Ielout(1),Relout(1)) ,         &
    & (Matset,Rmtout(25)) , (necpt(1),ecpt(1)) , (Systm(2),Nout) , (Phiout(65),Gpth2(1)) , (Systm(3),Nogo) , (Htcp,Kheat(4)) ,      &
    & (Itherm,Systm(56))
   DATA eps1/1.0E-16/ , ipn/1 , 4 , 2 , 3/
   DATA name/4HQUAD , 4H4   /
   DATA hunmeg/100000000/
   DATA const/0.57735026918962/
!
!     PHIOUT DATA BLOCK
!     --------------------------------------------------------------
!     PHIOUT(1)                 = ELID (ELEMENT ID)
!     PHIOUT(2-9)               = SIL NUMBERS
!     PHIOUT(10-17)             = ARRAY IORDER
!     PHIOUT(18)                = TSUB0 (REFERENCE TEMP.)
!     PHIOUT(19-20)             = Z1 & Z2 (FIBER DISTANCES)
!     PHIOUT(21)                = AVGTHK  (AVERAGE THICKNESS)
!     PHIOUT(22)                = MOMINR  (MOMENT OF INER. FACTOR)
!     PHIOUT(23-58)             = GBAR (BASIC MAT. PROP. MATRIX)
!                                 (W/O SHEAR)
!     PHIOUT(59-61)             = THERMAL EXPANSION COEFFICIENTS
!                                 FOR MEMBRANE MATERIAL
!     PHIOUT(62-64)             = THERMAL EXPANSION COEFFICIENTS
!                                 FOR BENDING MATERIAL
!     PHIOUT(65-68)             = CORNER NODE THICKNESSES
!     PHIOUT(69-77)             = 3X3 TRANSFORMATION FROM USER TO
!                                 MATERIAL COORD. SYSTEM
!     PHIOUT(78)                = OFFSET OF ELEMENT FROM GP PLANE
!     PHIOUT(79)                = ID OF THE ORIGINAL PCOMP(I)
!                                 PROPERTY ENTRY FOR COMPOSITES
!     PHIOUT(80-(79+9*NNODE))   = 3X3 TRANSFORMATIONS FROM GLOBAL
!                                 TO ELEMENT COORDINATE SYSTEM
!                                 FOR EACH EXISTING NODE
!
!     THE FOLLOWING IS REPEATED FOR EACH EVALUATION POINT AND THE
!     CENTER POINT (10 TIMES). THE EVALUATION POINTS ARE AT THE
!     STANDARD 2X2X2 GAUSSIAN POINTS. THE CHOICE OF THE
!     FINAL STRESS AND FORCE OUTPUT POINTS IS MADE AT THE SUBCASE
!     LEVEL (PHASE 2.)
!
!              1                  THICKNESS OF THE ELEMENT AT THIS
!                                 EVALUATION POINT
!            2 - 10               3X3 TRANSFORMATION FROM TANGENT
!                                 TO STRESS C.S. AT THIS EVAL. PT.
!           11 - 19               CORRECTION TO GBAR-MATRIX FOR
!                                 MEMBRANE-BENDING COUPLING AT THIS
!                                 EVALUATION POINT
!           20 - 28               3X3 TRANSFORMATION FROM MATERIAL
!                                 TO INTEGRATION PT. COORDINATE
!                                 SYSTEM
!           29 - 32               2X2 PROPERTY MATRIX FOR OUT-OF-
!                                 PLANE SHEAR (G3)
!         32+1 - 32+NNODE         ELEMENT SHAPE FUNCTIONS
!   32+NNODE+1 - 32+NNODE+8*NDOF  STRAIN RECOVERY MATRIX
!
!
!              IELOUT DATA BLOCK      (TOTAL OF NWORDS = 102)
!     --------------------------------------------------------------
!              1                  ELEMENT ID
!              2                  AVERAGE THICKNESS
!
!     THE FOLLOWING IS REPEATED FOR EACH CORNER POINT.
!
!         WORD  1                 SIL NUMBER
!         WORD  2-10              TBS TRANSFORMATION FOR Z1
!         WORD 11-19              TBS TRANSFORMATION FOR Z2
!         WORD 20-22              NORMAL VECTOR IN BASIC C.S.
!         WORD 23-25              GRID COORDS IN BASIC C.S.
!
!
   q4strs = 0
   elid = Nest(1)
   Nphi(1) = elid
   Norpth = .FALSE.
   Node = 4
   Nnode = 4
   ndof = Nnode*6
   nd2 = ndof*2
   nd3 = ndof*3
   nd4 = ndof*4
   nd5 = ndof*5
   nd6 = ndof*6
   nd7 = ndof*7
   nd8 = ndof*8
!
!     FILL IN ARRAY GGU WITH THE COORDINATES OF GRID POINTS 1, 2 AND 4.
!     THIS ARRAY WILL BE USED LATER TO DEFINE THE USER COORD. SYSTEM
!     WHILE CALCULATING  TRANSFORMATIONS INVOLVING THIS COORD. SYSTEM.
!
   DO i = 1 , 3
      ii = (i-1)*3
      ij = i
      IF ( ij==3 ) ij = 4
      DO j = 1 , 3
         jj = j + 1
         ggu(ii+j) = Bgpdt(jj,ij)
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
      vd1(i) = Bgpdt(ii,3) - Bgpdt(ii,1)
      vd2(i) = Bgpdt(ii,4) - Bgpdt(ii,2)
   ENDDO
!
!    COMPUTE THE NORMAL VECTOR VKN, NORMALIZE, AND COMPUTE THE PROJECTED
!    AREA, PA
!
   vkn(1) = vd1(2)*vd2(3) - vd1(3)*vd2(2)
   vkn(2) = vd1(3)*vd2(1) - vd1(1)*vd2(3)
   vkn(3) = vd1(1)*vd2(2) - vd1(2)*vd2(1)
   vkl = sqrt(vkn(1)**2+vkn(2)**2+vkn(3)**2)
   IF ( vkl==0. ) WRITE (Nout,99003) Est(1)
   vks(1) = vkn(1)/vkl
   vks(2) = vkn(2)/vkl
   vks(3) = vkn(3)/vkl
   pa = vkl/2.
!
!  COMPUTE SIDES -12- AND -41-
   DO i = 1 , 3
      ii = i + 1
      v12(i) = Bgpdt(ii,2) - Bgpdt(ii,1)
      v41(i) = Bgpdt(ii,1) - Bgpdt(ii,4)
   ENDDO
!
!  COMPUTE DOT PRODUCT, V12DK, OR V12 AND VK, THE VECTORS VP12, VI, VJ
!
   v12dk = v12(1)*vks(1) + v12(2)*vks(2) + v12(3)*vks(3)
   vp12(1) = v12(1) - v12dk*vks(1)
   vp12(2) = v12(2) - v12dk*vks(2)
   vp12(3) = v12(3) - v12dk*vks(3)
   vp12l = sqrt(vp12(1)**2+vp12(2)**2+vp12(3)**2)
   IF ( vp12l==0. ) WRITE (Nout,99003) Est(1)
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
   IF ( vjl==0. ) WRITE (Nout,99003) Est(1)
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
!     STORE INCOMING BGPDT FOR ELEMENT C.S.
!
   DO i = 1 , 3
      i1 = i + 1
      DO j = 1 , 4
         bgpdm(i,j) = Bgpdt(i1,j)
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
!     START FILLING IN IELOUT ARRAY WITH DATA TO BE STORED IN GPSRN
!
   Ielout(1) = elid
   DO i = 1 , 4
      Ielout(3+(i-1)*25) = Sil(i)
      DO j = 1 , 3
         Relout(25*i+j-1) = Bgpdt(j+1,i)
      ENDDO
   ENDDO
!
!     THE ARRAY IORDER STORES THE ELEMENT NODE ID IN
!     INCREASING SIL ORDER.
!
!     IORDER(1) = NODE WITH LOWEST  SIL NUMBER
!     IORDER(4) = NODE WITH HIGHEST SIL NUMBER
!
!     ELEMENT NODE NUMBER IS THE INTEGER FROM THE NODE LIST G1,G2,G3,G4.
!     THAT IS, THE 'I' PART OF THE 'GI' AS THEY ARE LISTED ON THE
!     CONNECTIVITY BULK DATA CARD DESCRIPTION.
!
!
   DO i = 1 , 4
      Iorder(i) = 0
      ksil(i) = Sil(i)
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
!     USE THE POINTERS IN IORDER TO COMPLETELY REORDER THE
!     GEOMETRY DATA INTO INCREASING SIL ORDER.
!     DON'T WORRY!! IORDER ALSO KEEPS TRACK OF WHICH SHAPE
!     FUNCTIONS GO WITH WHICH GEOMETRIC PARAMETERS!
!
   DO i = 1 , 4
      ksil(i) = Sil(i)
      tmpthk(i) = Gpth(i)
      kcid(i) = igpdt(1,i)
      DO j = 2 , 4
         tgrid(j,i) = Bgpdt(j,i)
      ENDDO
   ENDDO
   DO i = 1 , 4
      ipoint = Iorder(i)
      Gpth(i) = tmpthk(ipoint)
      igpdt(1,i) = kcid(ipoint)
      Sil(i) = ksil(ipoint)
      Nphi(i+1) = ksil(ipoint)
      Nphi(i+5) = 0
      Nphi(i+9) = ipoint
      Nphi(i+13) = 0
      DO j = 2 , 4
         Bgpdt(j,i) = tgrid(j,ipoint)
      ENDDO
   ENDDO
!
   Nphi(19) = Nest(20)
   Nphi(20) = Nest(21)
   Phiout(18) = 0.0
   offset = Zoff
   IF ( Zoff==0.0 ) offset = Zoff1
   Phiout(78) = offset
!
!     COMPUTE NODE NORMALS
!
   CALL q4nrms(Bgpdt,gpnorm,Iorder,iflag)
   IF ( iflag==0 ) THEN
!
!     PUT NORMALS IN IELOUT
!
      DO i = 1 , Nnode
         io = Iorder(i)
         iop = (io-1)*25 + 21
         Relout(iop+1) = gpnorm(2,i)
         Relout(iop+2) = gpnorm(3,i)
         Relout(iop+3) = gpnorm(4,i)
      ENDDO
!
!     COMPUTE NODE NORMALS
!
      avgthk = 0.0
      DO i = 1 , Nnode
         io = Iorder(i)
         IF ( Gpth(i)==0.0 ) Gpth(i) = Elth
         IF ( Gpth(i)>0.0 ) THEN
            avgthk = avgthk + Gpth(i)/Nnode
            Gpth2(io) = Gpth(i)
         ELSE
            WRITE (Nout,99001) Ufm , elid , Sil(i)
!
99001       FORMAT (A23,', QUAD4 ELEMENT HAS UNDEFINED THICKNESS.  ELEMENT',' ID =',I8,', SIL ID =',I8)
            GOTO 500
         ENDIF
      ENDDO
!
      mominr = 0.0
      tsfact = 5.0/6.0
      nocsub = .FALSE.
      IF ( Nest(15)/=0 ) mominr = Est(16)
      IF ( Nest(17)/=0 ) ts = Est(18)
      IF ( Est(18)==.0 ) ts = 5.0/6.0
      Phiout(21) = avgthk
      Phiout(22) = mominr
!
!     SET LOGICAL NOCSUB IF EITHER MOMINR OR TS ARE NOT DEFAULT
!     VALUES. THIS WILL BE USED TO OVERRIDE ALL CSUBB COMPUTATIONS.
!     I.E. DEFAULT VALUES OF UNITY ARE USED.
!
      epsi = abs(mominr-1.0)
      epst = abs(ts-tsfact)
      eps = .05
!     NOCSUB = EPSI.GT.EPS .OR. EPST.GT.EPS
!
!     PUT THE AVERAGE THICKNESS IN RELOUT
!
      Relout(2) = avgthk
!
!     THE COORDINATES OF THE ELEMENT GRID POINTS HAVE TO BE
!     TRANSFORMED FROM THE BASIC C.S. TO THE ELEMENT C.S.
!
      CALL betrns(teu,gge,0,elid)
      CALL gmmats(teu,3,3,0,tub,3,3,0,teb)
      CALL gmmats(tub,3,3,1,cent,3,1,0,cente)
!
      DO i = 1 , 3
         ii = i + 1
         ip = (i-1)*3
         DO j = 1 , Nnode
            epnorm(ii,j) = 0.0
            egpdt(ii,j) = 0.0
            DO k = 1 , 3
               kk = ip + k
               k1 = k + 1
               cc = Bgpdt(k1,j) - ggu(k) - cente(k)
               epnorm(ii,j) = epnorm(ii,j) + teb(kk)*gpnorm(k1,j)
               egpdt(ii,j) = egpdt(ii,j) + teb(kk)*cc
            ENDDO
         ENDDO
      ENDDO
!
!     INITIALIZE MATERIAL VARIABLES
!
!     SET INFLAG = 12 SO THAT SUBROUTINE MAT WILL SEARCH FOR-
!     ISOTROPIC MATERIAL PROPERTIES AMONG THE MAT1 CARDS,
!     ORTHOTROPIC MATERIAL PROPERTIES AMONG THE MAT8 CARDS, AND
!     ANISOTROPIC MATERIAL PROPERTIES AMONG THE MAT2 CARDS.
!
      Inflag = 12
      rho = 0.0
      Eltemp = Est(45)
      mid(1) = Nest(13)
      mid(2) = Nest(15)
      mid(3) = Nest(17)
      mid(4) = Nest(22)
      Membrn = mid(1)>0
      Bendng = mid(2)>0 .AND. mominr>0.0
      Shrflx = mid(3)>0
      Mbcoup = mid(4)>0
!
!     CHECK FOR COMPOSITE MATERIAL
!
      Nphi(79) = 0
      DO img = 1 , 4
         IF ( mid(img)>hunmeg ) GOTO 100
      ENDDO
      GOTO 200
   ELSE
      WRITE (Nout,99002) Ufm , elid
99002 FORMAT (A23,', MODULE SDR2 DETECTS BAD OR REVERSE GEOMETRY FOR ','ELEMENT ID =',I8)
      GOTO 500
   ENDIF
 100  Nphi(79) = mid(img) - img*hunmeg
!
!     DETERMINE FACTORS TO BE USED IN CSUBB CALCULATIONS
!
 200  IF ( Bendng ) THEN
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
      a = 0.5*(xa(2)+xa(3)-xa(1)-xa(4))
      b = 0.5*(yb(4)+yb(3)-yb(1)-yb(2))
      IF ( a>b ) aspect = b/a
      IF ( a<=b ) aspect = a/b
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
         IF ( cc>=1.0E-8 ) cc = sqrt(cc)
         Edgel(i) = cc
         Uev(1,i) = Uev(1,i)/cc
         Uev(2,i) = Uev(2,i)/cc
         Uev(3,i) = Uev(3,i)/cc
         cc = sqrt(Unv(1,i)**2+Unv(2,i)**2+Unv(3,i)**2)
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
         IF ( abs(Anglei(i))<1.0E-8 ) Anglei(i) = 0.0
      ENDDO
   ENDIF
!
!     SET THE INTEGRATION POINTS
!
   ptint(1) = -const
   ptint(2) = const
!
   IF ( Itherm/=0 ) THEN
!
!     BEGINNING OF HEAT RECOVERY.
!
      Matid = Nest(13)
      Inflag = 2
      Nphi(22) = 2
      Nphi(23) = Nnode
      Nphi(24) = name(1)
      Nphi(25) = name(2)
      xi = 0.0
      eta = 0.0
      CALL q4shps(xi,eta,shp,dshp)
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
      Hzta = 0.0
      CALL jacobs(elid,shp,dshp,Gpth,egpdt,epnorm,jacobe)
      IF ( Badjac ) GOTO 500
!
      DO i = 2 , 4
         ecpt(i) = 0.0
         DO j = 1 , Nnode
            ecpt(i) = ecpt(i) + shp(j)*Bgpdt(i,j)
         ENDDO
      ENDDO
!
      flags = Nest(27)
      IF ( flags==0 ) THEN
         thetas = Est(26)*Degrad
         GOTO 700
      ELSE
         scsid = Nest(26)
         IF ( scsid<=0 ) THEN
            DO i = 1 , 3
               ii = (i-1)*3
               DO j = 1 , 3
                  jj = (j-1)*3
                  tsu(ii+j) = tub(i+jj)
               ENDDO
            ENDDO
         ELSE
            necpt(1) = scsid
            CALL transs(ecpt,tbs)
            CALL gmmats(tbs,3,3,1,tub,3,3,1,tsu)
         ENDIF
         xs = tsu(1)
         ys = tsu(2)
         IF ( abs(xs)>eps1 .OR. abs(ys)>eps1 ) THEN
            thetas = atan2(ys,xs)
            GOTO 700
         ELSE
            Nest(2) = scsid
            j = 233
            GOTO 600
         ENDIF
      ENDIF
   ELSE
!
!     IN PLANE SHEAR REDUCTION
!
      xi = 0.0
      eta = 0.0
      kpt = 1
!
      CALL q4shps(xi,eta,shp,dshp)
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
         zeta = ptint(izta)
!
!     COMPUTE THE JACOBIAN AT THIS GAUSS POINT,
!     ITS INVERSE AND ITS DETERMINANT.
!
         Hzta = zeta/2.0
!
         CALL jacobs(elid,shp,dshp,Gpth,egpdt,epnorm,jacob)
         IF ( Badjac ) GOTO 500
!
!     COMPUTE PSI TRANSPOSE X JACOBIAN INVERSE.
!     HERE IS THE PLACE WHERE THE INVERSE JACOBIAN IS FLAGED TO BE
!     TRANSPOSED BECAUSE OF OPPOSITE MATRIX LOADING CONVENTION BETWEEN
!     INVER AND GMMAT.
!
         CALL gmmats(Psitrn,3,3,0,jacob,3,3,1,phi)
!
!     CALL Q4BMGS TO GET B MATRIX
!     SET THE ROW FLAG TO 2. IT WILL SAVE THE 3RD ROW OF B-MATRIX AT
!     THE TWO INTEGRATION POINTS.
!
         Rowflg = 2
         CALL q4bmgs(dshp,Gpth,egpdt,epnorm,phi,xybmat(kpt))
         kpt = kpt + nd2
      ENDDO
   ENDIF
!
!     FETCH MATERIAL PROPERTIES
!
!     SET THE ARRAY OF LENGTH 4 TO BE USED IN CALLING TRANSS.
!     NOTE THAT THE FIRST WORD IS THE COORDINATE SYSTEM ID WHICH
!     WILL BE SET IN POSITION LATER.
!
 300  DO iec = 2 , 4
      ecpt(iec) = 0.0
   ENDDO
!
!
!     EACH MATERIAL PROPERTY MATRIX G HAS TO BE TRANSFORMED FROM
!     THE MATERIAL COORDINATE SYSTEM TO THE ELEMENT COORDINATE
!     SYSTEM. THESE STEPS ARE TO BE FOLLOWED-
!
!     1- IF MCSID HAS BEEN SPECIFIED, SUBROUTINE TRANSS IS CALLED
!        TO CALCULATE TBM-MATRIX (MATERIAL TO BASIC TRANSFORMATION).
!        THIS WILL BE FOLLOWED BY A CALL TO SUBROUTINE BETRNS
!        TO CALCULATE TEB-MATRIX (BASIC TO ELEMENT TRANSFORMATION).
!        TBM-MATRIX IS THEN PREMULTIPLIED BY TEB-MATRIX TO OBTAIN
!        TEM-MATRIX. THEN STEP 3 WILL BE TAKEN.
!
!     2- IF THETAM HAS BEEN SPECIFIED, SUBROUTINE ANGTRS IS CALLED
!        TO CALCULATE TEM-MATRIX (MATERIAL TO ELEMENT TRANSFORMATION).
!
!                          T
!     3-           G   =  U   G   U
!                   E          M
!
!
   flagm = Nest(11)
   IF ( flagm==0 ) THEN
!
!     CALCULATE TEM-MATRIX USING THETAM
!
      thetam = Est(10)*Degrad
      IF ( thetam/=0.0 ) GOTO 400
!
!     DEFAULT IS CHOSEN, LOOK FOR VALUES OF MCSID AND/OR THETAM
!     ON THE PSHELL CARD.
!
      flagm = Nest(24)
      IF ( flagm==0 ) THEN
!
         thetam = Est(23)*Degrad
         GOTO 400
      ELSE
         mcsid = Nest(23)
      ENDIF
   ELSE
      mcsid = Nest(10)
   ENDIF
!
!     CALCULATE TUM-MATRIX USING MCSID
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
      Nest(2) = mcsid
      j = 231
      GOTO 600
   ENDIF
 400  CALL angtrs(thetam,1,tum)
   CALL gmmats(teu,3,3,0,tum,3,3,0,tem)
!
!
!     STORE TUM IN PHIOUT
!
   DO iem = 1 , 9
      Phiout(68+iem) = tum(iem)
   ENDDO
!
   IF ( Itherm/=0 ) THEN
      CALL gmmats(tum,3,3,1,tsu,3,3,1,tms)
      tms(3) = tms(4)
      tms(4) = tms(5)
      CALL gmmats(tms,2,2,1,Phiout(26),2,2,0,tum)
      CALL gmmats(tum,2,2,0,tms,2,2,0,Phiout(26))
      GOTO 800
   ELSE
!
!     BEGIN THE LOOP TO FETCH PROPERTIES FOR EACH MATERIAL ID
!
      DO ll = 1 , 36
         gi(ll) = 0.0
      ENDDO
!
      m = 0
      it0 = 0
      igobk = 0
      DO
         m = m + 1
         IF ( m>4 ) THEN
            IF ( mid(3)<hunmeg ) EXIT
            IF ( gi(19)/=0. .OR. gi(20)/=0. .OR. gi(21)/=0. .OR. gi(22)/=0. ) EXIT
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
                     CALL mat(elid)
                  ELSEIF ( Matid/=mid(m-1) .OR. igobk/=0 ) THEN
                     CALL mat(elid)
                  ENDIF
!
                  IF ( it0<=0 ) THEN
                     tsub0 = Rmtout(11)
                     IF ( Matset==8.0 ) tsub0 = Rmtout(10)
                     Phiout(18) = tsub0
                     it0 = 1
                  ENDIF
!
                  coeff = 1.0
!     IF (M .EQ. 2) COEFF = MOMINR
                  IF ( m==3 ) coeff = ts
                  lpoint = (m-1)*9 + 1
!
                  CALL q4gmgs(m,coeff,gi(lpoint))
!
!WKBDB 11/93 SPR93020
!      IF (M .GT. 0) GO TO 490
!      IF (.NOT.SHRFLX .AND. BENDNG) GO TO 480
!      NEST(2) = MATID
!      J = 231
!      GO TO 1440
!
!  480 M = -M
!  490 CONTINUE
!      MTYPE = IFIX(MATSET+.05) - 2
!      IF (NOCSUB) GO TO 580
!      GO TO (580,500,540,580), M
!C
!  500 IF (MTYPE) 510,520,530
!  510 ENORX = RMTOUT(16)
!      ENORY = RMTOUT(16)
!      GO TO 580
!  520 ENORX = RMTOUT(1)
!      ENORY = RMTOUT(4)
!      GO TO 580
!  530 ENORX = RMTOUT(1)
!      ENORY = RMTOUT(3)
!      GO TO 580
!
!  540 IF (MTYPE) 550,560,570
!  550 GNORX = RMTOUT(6)
!      GNORY = RMTOUT(6)
!      GO TO 580
!  560 GNORX = RMTOUT(1)
!      GNORY = RMTOUT(4)
!      GO TO 580
!  570 GNORX = RMTOUT(6)
!      GNORY = RMTOUT(5)
!      IF (GNORX .EQ. 0.0) GNORX = RMTOUT(4)
!      IF (GNORY .EQ. 0.0) GNORY = RMTOUT(4)
!  580 CONTINUE
!WKBDE 11/93 SPR93020
!WKBNB 11/93 SPR93020
                  IF ( m<=0 ) THEN
                     IF ( .NOT.Shrflx .AND. Bendng ) THEN
                        m = -m
                     ELSE
                        Nest(2) = Matid
                        j = 231
                        GOTO 600
                     ENDIF
                  ENDIF
                  mtype = ifix(Matset+.05) - 2
                  IF ( .NOT.(nocsub) ) THEN
                     IF ( m==1 .OR. m==4 ) THEN
                     ELSEIF ( m==3 ) THEN
                        IF ( mtype<0 ) THEN
                           gnorx = Rmtout(6)
                           gnory = Rmtout(6)
                        ELSEIF ( mtype==0 ) THEN
                           gnorx = Rmtout(1)
                           gnory = Rmtout(4)
                        ELSE
                           gnorx = Rmtout(6)
                           gnory = Rmtout(5)
                           IF ( gnorx==0.0D0 ) gnorx = Rmtout(4)
                           IF ( gnory==0.0D0 ) gnory = Rmtout(4)
                        ENDIF
!WKBNE 11/93 SPR93020
!WKBNB 2/94 SPR93020
                     ELSEIF ( mtype<0 ) THEN
                        enorx = Rmtout(16)
                        enory = Rmtout(16)
                        dnux = gi(lpoint+1)/gi(lpoint)
                        dnuy = gi(lpoint+3)/gi(lpoint+4)
                     ELSEIF ( mtype==0 ) THEN
                        enorx = Rmtout(1)
                        enory = Rmtout(4)
                        dnux = gi(lpoint+1)/gi(lpoint)
                        dnuy = gi(lpoint+3)/gi(lpoint+4)
                     ELSE
                        enorx = Rmtout(1)
                        enory = Rmtout(3)
                        dnux = gi(lpoint+1)/gi(lpoint)
                        dnuy = gi(lpoint+3)/gi(lpoint+4)
                     ENDIF
                  ENDIF
!WKBNE 2/94 SPR93020
                  IF ( Matset/=1.0 ) THEN
                     IF ( m==3 ) THEN
!
                        u(1) = tem(5)*tem(9) + tem(6)*tem(8)
                        u(2) = tem(4)*tem(9) + tem(6)*tem(7)
                        u(3) = tem(2)*tem(9) + tem(3)*tem(8)
                        u(4) = tem(1)*tem(9) + tem(3)*tem(7)
                        l = 2
                     ELSE
                        u(1) = tem(1)*tem(1)
                        u(2) = tem(2)*tem(2)
                        u(3) = tem(1)*tem(2)
                        u(4) = tem(4)*tem(4)
                        u(5) = tem(5)*tem(5)
                        u(6) = tem(4)*tem(5)
                        u(7) = tem(1)*tem(4)*2.0
                        u(8) = tem(2)*tem(5)*2.0
                        u(9) = tem(1)*tem(5) + tem(2)*tem(4)
                        l = 3
                     ENDIF
!
                     CALL gmmats(u(1),l,l,1,gi(lpoint),l,l,0,gt(1))
                     CALL gmmats(gt(1),l,l,0,u(1),l,l,0,gi(lpoint))
                  ENDIF
!
!     TRANSFORM THERMAL EXPANSION COEFF'S AND STORE THEM IN PHIOUT
!
                  IF ( m<=2 ) THEN
                     IF ( Matset==2. ) THEN
!
!     MAT2
!
                        DO imat = 1 , 3
                           alfa(imat) = Rmtout(7+imat)
                        ENDDO
                     ELSEIF ( Matset==8. ) THEN
!
!     MAT8
!
                        alfa(1) = Rmtout(8)
                        alfa(2) = Rmtout(9)
                        alfa(3) = 0.0
                     ELSE
!
!     MAT1
!
                        alfa(1) = Rmtout(8)
                        alfa(2) = Rmtout(8)
                        alfa(3) = 0.0
                     ENDIF
!
                     mpoint = (m-1)*3 + 59
                     IF ( Matset==1.0 ) THEN
                        DO ialf = 1 , 3
                           mp = mpoint - 1 + ialf
                           Phiout(mp) = alfa(ialf)
                        ENDDO
                     ELSE
                        CALL invers(3,u,3,bdum,0,detu,isngu,index)
                        CALL gmmats(u,3,3,0,alfa,3,1,0,Phiout(mpoint))
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDDO
!
      nocsub = enorx==0.0 .OR. enory==0.0 .OR. gnorx==0.0 .OR. gnory==0.0 .OR. mominr==0.0
!
!
!     FILL IN THE BASIC 6X6 MATERIAL PROPERTY MATRIX G
!
      DO ig = 1 , 6
         DO jg = 1 , 6
            g(ig,jg) = 0.0
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
         DO ig = 4 , 6
            ig2 = (ig-2)*3
            DO jg = 4 , 6
               jg2 = jg + ig2
               g(ig,jg) = gi(jg2)
            ENDDO
         ENDDO
!
         IF ( Membrn ) THEN
            DO ig = 1 , 3
               kg = ig + 3
               ig1 = (ig-1)*3
               DO jg = 1 , 3
                  lg = jg + 3
                  jg1 = jg + ig1
                  g(ig,lg) = gi(jg1)
                  g(kg,jg) = gi(jg1)
               ENDDO
            ENDDO
         ENDIF
      ENDIF
!
!     STORE 6X6 GBAR-MATRIX IN PHIOUT
!
      ig1 = 22
      DO ig = 1 , 6
         DO jg = 1 , 6
            ig1 = ig1 + 1
            Phiout(ig1) = g(ig,jg)
         ENDDO
      ENDDO
!
!
!     STRESS TRANSFORMATIONS
!     ----------------------
!
!     THE NECESSARY TRANSFORMATIONS ARE PERFORMED IN THE FOLLOWING
!     MANNER-
!
!     1- ALL THE TRANSFORMATIONS ARE CALCULATED IN PHASE I AND THEN
!        TRANSFERED THRU DATA BLOCK 'PHIOUT' TO PHASE II WHERE THE
!        ACTUAL MULTIPLICATIONS ARE PERFORMED.
!
!     2- THE STRAIN RECOVERY MATRIX B
!        IS EVALUATED IN THE ELEMENT COORDINATE SYSTEM IN PHASE I
!        AND TRANSFERED TO PHASE II. THE DISPLACEMENTS, HOWEVER,
!        ENTER PHASE II IN GLOBAL COORDINATES. THEREFORE,
!        2A) 3X3 TRANSFORMATIONS FROM GLOBAL TO ELEMENT COORDINATE
!            SYSTEM (TEG) FOR EACH GRID POINT ARE CALCULATED AND
!            STORED IN  PHIOUT (80 - (79+9*NNODE)).
!            USING THESE TRANSFORMATIONS THE DISPLACEMENTS AT
!            EACH GRID POINT WILL BE EVALUATED IN THE ELEMENT
!            COORDINATE SYSTEM AFTER ENTERING PHASE II.
!
!        2B) A 3X3 TRANSFORMATION FROM THE TANGENT TO THE USER-
!            DEFINED STRESS COORDINATE SYSTEM (TSI) IS CALCULATED
!            FOR EACH INTEGRATION POINT AND STORED ALONG WITH OTHER
!            DATA FOR THAT INTEGRATION POINT AT POSITIONS 2-10 OF
!            THE REPEATED DATA FOR EACH EVALUATION POINT.
!            IT WILL BE USED TO TRANSFORM THE STRESS OUTPUT TO
!            ANY DESIRED COORDINATE SYSTEM.
!            NOTE THAT THESE CALCULATIONS WILL BE PERFORMED INSIDE
!            THE DOUBLE LOOP.
!
!     CALCULATIONS FOR TEG-MATRIX
!
!     CALCULATE  TBG-MATRIX (GLOBAL TO BASIC), THEN
!     MULTIPLY  TEB AND TBG MATRICES  TO GET  TEG-MATRIX
!     FOR THIS GRID POINT AND STORE IT IN PHIOUT.
!
      DO i = 1 , Nnode
         ip = 80 + (i-1)*9
         IF ( igpdt(1,i)<=0 ) THEN
!
            DO j = 1 , 9
               Phiout(ip+j-1) = teb(j)
            ENDDO
         ELSE
            CALL transs(igpdt(1,i),tbg)
            CALL gmmats(teb,3,3,0,tbg,3,3,0,Phiout(ip))
         ENDIF
      ENDDO
!
!     INITIALIZE THE ARRAYS USED IN THE DOUBLE LOOP CALCULATION.
!     EVALUATION OF STRESSES IS DONE AT 2X2 POINTS AND AT THE
!     CENTER OF THE ELEMENT, AT THE MID-SURFACE.
!
      IF ( .NOT.(Bendng) ) THEN
         j = nd3 + 1
         DO ibmx = j , nd8
            bmatrx(ibmx) = 0.0
         ENDDO
      ENDIF
!
      icount = -(8*ndof+Nnode+32) + 79 + 9*Nnode
!
      ptintp(1) = -const
      ptintp(2) = const
      ptintp(3) = 0.0
!
!
!     HERE BEGINS THE TRIPLE LOOP ON STATEMENTS 835 AND 840
!     -----------------------------------------------------
!
      DO ixsi = 1 , 3
         xi = ptintp(ixsi)
!
         DO ieta = 1 , 3
            eta = ptintp(ieta)
            IF ( ixsi/=3 .OR. ieta==3 ) THEN
               IF ( ixsi/=3 .AND. ieta==3 ) CYCLE
!
               CALL q4shps(xi,eta,shp,dshp)
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
               th = 0.0
               DO ith = 1 , Nnode
                  th = th + shp(ith)*Gpth(ith)
               ENDDO
               reali = mominr*th*th*th/12.0
               tsi = ts*th
!
               IF ( .NOT.(nocsub) ) THEN
                  IF ( Bendng ) THEN
!      NUNORX = MOMINR*ENORX/(2.0*GNORX) - 1.0
!      NUNORY = MOMINR*ENORY/(2.0*GNORY) - 1.0
!WKBNB 2/94 SPR93020
                     nunorx = mominr*enorx/(2.0*gnorx) - 1.0
                     nunory = mominr*enory/(2.0*gnory) - 1.0
                     IF ( nunorx<0. ) nunorx = dnux
                     IF ( nunory<0. ) nunory = dnuy
!WKBNE 2/94 SPR93020
!WKBDB 2/94 SPR93020
!      EIX = MOMINR*ENORX
!      EIY = MOMINR*ENORY
!      TGX = 2.0*GNORX
!      TGY = 2.0*GNORY
!      NUNORX = EIX/TGX - 1.0
!      IF (EIX .GT. TGX) NUNORX = 1.0 - TGX/EIX
!      NUNORY = EIY/TGY - 1.0
!      IF (EIY .GT. TGY) NUNORY = 1.0 - TGY/EIY
!      IF (NUNORX .GT. 0.999999) NUNORX = 0.999999
!      IF (NUNORY .GT. 0.999999) NUNORY = 0.999999
!WKBDE 2/94 SPR93020
!     IF (NUNORX .GT. .49) NUNORX = 0.49
!     IF (NUNORY .GT. .49) NUNORY = 0.49
                     cc = aspect
                     ax = a
                     IF ( eta<0.0 ) ax = a + const*(xa(2)-xa(1)-a)
                     IF ( eta>0.0 ) ax = a + const*(xa(3)-xa(4)-a)
                     psiinx = 32.0*reali/((1.0-nunorx)*tsi*ax*ax)
                     by = b
                     IF ( xi<0.0 ) by = b + const*(yb(4)-yb(1)-b)
                     IF ( xi>0.0 ) by = b + const*(yb(3)-yb(2)-b)
                     psiiny = 32.0*reali/((1.0-nunory)*tsi*by*by)
                     IF ( .NOT.Shrflx ) THEN
                        IF ( psiinx>=1.0 ) THEN
                           tsmfx = 1.0
                        ELSE
                           tsmfx = psiinx/(1.0-psiinx)
                           IF ( tsmfx>1.0 ) tsmfx = 1.0
                        ENDIF
                        IF ( psiiny>=1.0 ) THEN
                           tsmfy = 1.0
                        ELSE
                           tsmfy = psiiny/(1.0-psiiny)
                           IF ( tsmfy>1.0 ) tsmfy = 1.0
                        ENDIF
                     ELSE
                        tsmfx = psiinx
                        tsmfy = psiiny
                        IF ( tsmfx>1.0 ) tsmfx = 1.0
                        IF ( tsmfy>1.0 ) tsmfy = 1.0
                     ENDIF
                     GOTO 405
                  ENDIF
               ENDIF
!
               tsmfx = 1.0
               tsmfy = 1.0
!
!     IRREGULAR 4-NODE CODE-  CALCULATION OF NODAL EDGE SHEARS
!                             AT THIS INTEGRATION POINT
!
!
 405           DO ij = 1 , 4
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
                     Edgshr(is,ij) = (Uev(is,ij)+Anglei(ij)*Uev(is,ii))*aa/(1.0-Anglei(ij)*Anglei(ij))                              &
                                   & + (Uev(is,ij)+Anglei(ik)*Uev(is,ik))*bb/(1.0-Anglei(ik)*Anglei(ik))
                  ENDDO
               ENDDO
!
               DO izta = 1 , 2
                  zeta = ptint(izta)
                  Hzta = zeta/2.0
                  ibot = (izta-1)*nd2
!
!     SET THE PHIOUT POINTER
!
                  icount = icount + 32 + Nnode + 8*ndof
!
                  Phiout(icount+1) = th
!
!     STORE SHAPE FUNCTION VALUES IN PHIOUT
!
                  DO i = 1 , Nnode
                     Phiout(icount+32+i) = shp(i)
                  ENDDO
!
!     STORE THE CORRECTION TO GBAR-MATRIX IN PHIOUT
!
                  ig1 = icount + 10
                  ig4 = 28
                  DO ig = 1 , 9
                     ig1 = ig1 + 1
                     Phiout(ig1) = -gi(ig4)*zeta*6.0
                     ig4 = ig4 + 1
                  ENDDO
!
!     STORE G3-MATRIX IN PHIOUT
!
                  iph = icount + 28
                  Phiout(iph+1) = tsmfy*gi(19)
                  Phiout(iph+2) = sqrt(tsmfx*tsmfy)*gi(20)
                  Phiout(iph+3) = sqrt(tsmfx*tsmfy)*gi(21)
                  Phiout(iph+4) = tsmfx*gi(22)
!
!     COMPUTE THE JACOBIAN AT THIS GAUSS POINT,
!     ITS INVERSE AND ITS DETERMINANT.
!
                  CALL jacobs(elid,shp,dshp,Gpth,egpdt,epnorm,jacob)
                  IF ( Badjac ) GOTO 500
!
!     COMPUTE PSI TRANSPOSE X JACOBIAN INVERSE.
!     HERE IS THE PLACE WHERE THE INVERSE JACOBIAN IS FLAGED TO BE
!     TRANSPOSED BECAUSE OF OPPOSITE MATRIX LOADING CONVENTION BETWEEN
!     INVER AND GMMAT.
!
                  CALL gmmats(Psitrn,3,3,0,jacob,3,3,1,phi)
!
                  CALL gmmats(tem,3,3,1,Psitrn,3,3,1,tmi)
!
!     STORE TMI-MATRIX IN PHIOUT
!
                  iph = icount + 20
                  DO i = 1 , 9
                     Phiout(iph) = tmi(i)
                     iph = iph + 1
                  ENDDO
!
!     ARRAY ECPT(4) WHICH IS USED IN TRANSS CONSISTS OF THE C.S. ID
!     AND THE COORDINATES (IN BASIC C.S.) OF THE POINT FROM (OR TO)
!     WHICH THE TRANSFORMATION IS BEING PERFORMED. THE COORDINATES
!     ARE NOT USED IF THE DESIGNATED COORDINATE SYSTEM IS RECTANGULAR.
!
                  DO i = 1 , 3
                     gpc(i) = 0.0
                     ii = i + 1
                     DO j = 1 , Nnode
                        gpc(i) = gpc(i) + shp(j)*(Bgpdt(ii,j)+Hzta*Gpth(j)*gpnorm(ii,j))
                     ENDDO
                     ecpt(ii) = gpc(i)
                  ENDDO
!
!     CALCULATIONS FOR TSE-MATRIX
!
                  flags = Nest(27)
                  IF ( flags==0 ) THEN
!
!     FLAGS IS 0, I.E. THETAS HAS BEEN SPECIFIED.
!     SUBROUTINE ANGTRS RETURNS THE 3X3 TRANSFORMATION USING THETAS.
!     NOTE THAT IF THETAS IS LEFT BLANK (DEFAULT), THE TRANSFORMATION
!     WILL BE IDENTITY,  I.E. THE STRESSES WILL BE OUTPUT IN THE
!     ELEMENT COORDINATE SYSTEM.
!     IF Q4STRS IS SET EQUAL TO 1, STRESSES WILL BE OUTPUT IN THE E C.S.
!     WHICH COOINCIDES WITH MSC'S  VERSION OF ELEMENT COORDINATE SYSTEM.
!
                     thetas = Est(26)*Degrad
                  ELSE
!
!     FLAGS IS 1, I.E. SCSID HAS BEEN SPECIFIED.
!     CALCULATE TBS-MATRIX (STRESS TO BASIC)
!
                     scsid = Nest(26)
                     IF ( scsid<=0 ) THEN
                        DO i = 1 , 3
                           ii = (i-1)*3
                           DO j = 1 , 3
                              jj = (j-1)*3
                              tsu(ii+j) = tub(i+jj)
                           ENDDO
                        ENDDO
                     ELSE
                        necpt(1) = scsid
                        CALL transs(ecpt,tbs)
!
!     MULTIPLY
!               T         T
!            TBS  AND  TUB  TO GET TSU-MATRIX (USER TO STRESS)
!
                        CALL gmmats(tbs,3,3,1,tub,3,3,1,tsu)
                     ENDIF
!
!     CALCULATE THETAS FROM THE PROJECTION OF THE X-AXIS OF THE
!     STRESS C.S. ON TO THE XY PLANE OF THE ELEMENT C.S.
!
                     xs = tsu(1)
                     ys = tsu(2)
                     IF ( abs(xs)>eps1 .OR. abs(ys)>eps1 ) THEN
                        thetas = atan2(ys,xs)
                     ELSE
                        Nest(2) = scsid
                        j = 233
                        GOTO 600
                     ENDIF
                  ENDIF
                  IF ( q4strs==1 ) THEN
                     CALL angtrs(thetas,0,tse)
                  ELSE
                     CALL angtrs(thetas,0,tsu)
                     CALL gmmats(tsu,3,3,0,teu,3,3,1,tse)
                  ENDIF
!                                   T
!     CALCULATE  TSI  = TSE X PSITRN  AND STORE IT IN PHIOUT
!
                  CALL gmmats(tse,3,3,0,Psitrn,3,3,1,Phiout(icount+2))
!
!     FOR CORNER POINTS (THE STRESS EVALUATION POINTS EXCEPT FOR THE
!     ONES AT THE CENTER), CALCULATE TSB-MATRIX AND STORE IT IN IELOUT.
!
                  IF ( ixsi+ieta<=4 ) THEN
                     ip = (ixsi-1)*2 + ieta
                     ip1 = ipn(ip)
                     ip2 = (ip1-1)*25 + 4 + (izta-1)*9
                     CALL gmmats(tse,3,3,0,teb,3,3,0,Relout(ip2))
                  ENDIF
!
!     CALL Q4BMGS TO GET B MATRIX
!     SET THE ROW FLAG TO 3 TO CREATE THE FIRST 6 ROWS. THEN SET IT
!     TO 1 FOR THE LAST 2 ROWS.
!
                  Rowflg = 3
                  CALL q4bmgs(dshp,Gpth,egpdt,epnorm,phi,bmatrx(1))
                  DO ix = 1 , ndof
                     bmatrx(ix+nd2) = xybmat(ibot+ix)
                  ENDDO
!
                  IF ( Bendng ) THEN
                     Rowflg = 1
                     CALL q4bmgs(dshp,Gpth,egpdt,epnorm,phi,bmatrx(1+nd6))
                     DO ix = 1 , ndof
                        bmatrx(ix+nd5) = xybmat(ibot+ix+ndof)
                     ENDDO
                  ENDIF
!
!
!     HERE WE SHIP OUT THE STRAIN RECOVERY MATRIX.
!     --------------------------------------------
!
                  kcount = icount + 32 + Nnode
                  DO iph = 1 , nd8
                     Phiout(kcount+iph) = bmatrx(iph)
                  ENDDO
               ENDDO
            ENDIF
         ENDDO
      ENDDO
      RETURN
   ENDIF
!
 500  Nogo = 1
   RETURN
!
 600  CALL mesage(30,j,name)
   GOTO 500
 700  CALL angtrs(thetas,0,tsu)
   sinmat = 0.0
   cosmat = 1.0
   CALL hmat(elid)
   Phiout(26) = Kheat(1)
   Phiout(27) = Kheat(2)
   Phiout(28) = Kheat(2)
   Phiout(29) = Kheat(3)
!
!     BRANCH IF THERMAL CONDUCTIVITY KHEAT IS ISOTROPIC.
!     OTHERWISE, FIND TBM, TBS AND TMS AND COMPUTE THE KHEAT
!     TENSOR IN 2-DIMENSIONAL STRESS COORDINATE SYSTEM.
!
!     COMMENTS FROM G.CHAN/UNISYS     10/88
!     HMAT ROUTINE DOES NOT RETURN 'TYPE' IN COSMIC NASTRAN
!     SO WE CAN ONLY ASSUME THERMAL CONDUCTIVITY IS ISOTROPIC AND
!     BRANCH TO 1610 UNCONDITIOANLLY BY SETTING TYPE =-1
!
   Type = -1
!
   IF ( Type/=4 .AND. Type/=-1 ) GOTO 300
 800  CALL gmmats(teu,3,3,1,jacobe,3,3,0,jacobu)
   CALL gmmats(tsu,3,3,0,jacobu,3,3,0,jacbs)
   DO j = 1 , Nnode
      dq(j) = dshp(j)
      jn = j + Nnode
      dq(jn) = dshp(j+4)
      jn = jn + Nnode
      dq(jn) = 0.0
   ENDDO
   CALL gmmats(jacbs,3,3,0,dq,3,Nnode,0,Phiout(35))
   RETURN
99003 FORMAT (//,' ILLEGAL GEOMETRY FOR QUAD4 ELEMENT, ID=',I10)
END SUBROUTINE squd41
