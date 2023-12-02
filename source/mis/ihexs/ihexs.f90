!*==ihexs.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE ihexs(Type)
   IMPLICIT NONE
   USE c_blank
   USE c_emgdic
   USE c_emgest
   USE c_emgprm
   USE c_hmtout
   USE c_matin
   USE c_matiso
   USE c_matout
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Type
!
! Local variable declarations rewritten by SPAG
!
   REAL :: alfa , beta , bmag , detj , dstld , e1 , e2 , e3 , maxar , prt1 , sfact , smag , sx , sxy , sy , syz , sz , szx , tmag , &
         & tvol
   LOGICAL :: anis , diag , nocstm , rect , tdep
   INTEGER :: back , bcord , bgpdt , cid , edge , eid , face , gpt , heat , i , icode , icp , id , idon , idstld , ig , igcs , ii , &
            & ijk , ijkl , ijl , ik , il , im , in , inc , irp , it , ix , iz , j , k , kgg , l , m , mgg , mz , n , nd , ngg ,     &
            & ngp , nip , nk , nm , nz , ugv
   REAL , SAVE :: balfa , bar , bbeta , bcd1 , dtor
   REAL , DIMENSION(3) , SAVE :: bcd2 , dalfa , dmaxar
   REAL , DIMENSION(3,3) :: c , jacob , part , tf , tk
   REAL , DIMENSION(6) :: dalpha , sig
   REAL , DIMENSION(2) , SAVE :: dbeta
   INTEGER , DIMENSION(40) :: dict
   INTEGER , DIMENSION(3) , SAVE :: elno , excd
   REAL , DIMENSION(3,12) :: evec
   REAL , DIMENSION(8) , SAVE :: gauss
   REAL , DIMENSION(36) :: gmat
   REAL , DIMENSION(32) :: gptld
   REAL , DIMENSION(4) :: h , s
   INTEGER , DIMENSION(46) :: ib
   INTEGER , DIMENSION(1) :: iest , iwork , jz , sil
   INTEGER , SAVE :: ihex , nerr1 , nerr2 , scr4
   INTEGER , DIMENSION(8) :: nc
   INTEGER , DIMENSION(5) , SAVE :: rvrs
   REAL , DIMENSION(45) :: store
   REAL , DIMENSION(18) :: str
   INTEGER , DIMENSION(9) , SAVE :: twins
   REAL , DIMENSION(3,2) :: vn
   REAL , DIMENSION(66) :: work
   REAL , DIMENSION(1) :: z
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     SINGLE PRECISION VERSION
!
!     THIS ROUTINE PROCESSES IHEX1, IHEX2, AND IHEX3 ELEMENT DATA TO
!     PRODUCE STIFFNESS AND MASS MATRICES.  IF THE HEAT TRANSFER OPTION
!     IS ON, CONDUCTIVITY AND CAPACITY MATRICES ARE PRODUCED.  IF THE
!     DISPLACEMENT VECTOR POINTER IS NON-ZERO, THE DIFFERENTIAL
!     STIFFNESS MATRIX ONLY IS PRODUCED.
!
!           TYPE = 1    IHEX1
!           TYPE = 2    IHEX2
!           TYPE = 3    IHEX3
!
!           THE EST ENTRIES ARE
!
!     NAME  ----------INDEX----------   DESCRIPTION
!            IHEX1    IHEX2    IHEX3
!
!     EID        1        1        1    ELEMENT ID NO.
!     SIL      2-9     2-21     2-33    SCALAR INDEX LIST
!     MID       10       22       34    MATERIAL ID NO.
!     CID       11       23       35    MATERIAL COORD. SYSTEM ID NO.
!     NIP       12       24       36    NO. INTEGRATION POINTS PER EDGE
!     MAXAR     13       25       37    MAX ASPECT RATIO
!     ALFA      14       26       38    MAX ANGLE FOR NORMALS
!     BETA      15       27       39    MAX ANGLE FOR MIDSIDE POINTS
!     BGPDT  16-47   28-107   40-167    BASIC GRID POINT DATA
!     GPT    48-55  108-127  168-199    GRID POINT TEMPERATURES
!
!     - INSTALLATION NOTE --
!     GPTLD IS SUPPOSED TO CONTAIN GRID POINT TEMPERATURE LOADS FOR
!     COMPUTING DIFFERENTIAL STIFFNESS.  FOR INSTALLATION, GPTLD MUST
!     BE LOADED WITH DATA BY EMG.  IF GPTLD(1)=-1, NO TEMP LOAD IS
!     ASSUMED.
!
!     OMMON  /MATISO/  G11,G12,G13,...,G46,G56,G66,RHO,AXX,AYY,AZZ,AXY,
!                      AYZ,AZX,TREF,GE,IER
!     COMMON /EMG***/  ...,UGV,...
!
!     - INSTALLATION NOTE --
!     UGV POINTS TO BEGINNING OF SINGLE PRECISION GLOBAL DISPLACEMENT
!     VECTOR IN OPEN CORE ARRAY RZ.
!
!
!     RZ IS OPEN CORE.  USE ONLY RZ(IZS) TO RZ(NZS).
!
   !>>>>EQUIVALENCE (Z(1),Jz(1),Rz(1)) , (Eid,Est(1),Iest(1)) , (Sil(1),Est(2)) , (work(1),iwork(1)) , (sig(1),sx) , (sig(2),sy) ,       &
!>>>>    & (sig(3),sz) , (sig(4),sxy) , (sig(5),syz) , (sig(6),szx) , (dstld,idstld)
   !>>>>EQUIVALENCE (work(1),evec(1,1)) , (work(37),vn(1,1)) , (work(43),nc(1))
   !>>>>EQUIVALENCE (work(1),jacob(1,1)) , (work(19),h(1)) , (work(27),s(1)) , (work(35),part(1,1)) , (work(53),sig(1)) ,                &
!>>>>    & (work(1),c(1,1))
   !>>>>EQUIVALENCE (work(1),tf(1,1)) , (work(35),tk(1,1))
   !>>>>EQUIVALENCE (Ib(1),Bufm6(1))
   DATA scr4/304/
   DATA bcd1 , bcd2/4HCIHE , 4HX1   , 4HX2   , 4HX3  /
   DATA dmaxar , dalfa , dbeta/5.0 , 10.0 , 15.0 , 45.0 , 45.0 , 45.0 , 45.0 , 45.0/
   DATA dtor , gauss/0.01745329251994 , 0.57735026918962 , 0.55555555555555 , 0.77459666924148 , 0.88888888888889 ,                 &
      & 0.34785484513745 , 0.86113631159405 , 0.65214515486254 , 0.33998104358485/
   DATA ihex , elno/4HIHEX , 4H ELE , 4HMENT , 4H NO./
   DATA bar , balfa , bbeta/4H  AR , 4HALFA , 4HBETA/
   DATA excd/4H EXC , 4HEEDE , 4HD.  /
   DATA rvrs/4HREVE , 4HRSED , 4H NUM , 4HBERI , 4HNG. /
   DATA twins/4HCOOR , 4HDINA , 4HTES  , 4HOF T , 4HWO P , 4HOINT , 4HS AR , 4HE SA , 4HME. /
   DATA nerr1 , nerr2/3301 , 3302/
!
!     IZ AND NZ ARE OPEN CORE POINTERS
!
   iz = izs
   nz = nzs
!
!     THIS ROUTINE OPERATES IN DOUBLE PRECISION.
!     EMGOUT WILL PRODUCE THE REQUIRED MATRIX IN THE REQUESTED PRECISION
!
!     ALLOCATE LARGE ARRAYS IN OPEN CORE
!
   ngp = 12*Type - 4
   heat = 0
   kgg = 0
   mgg = 0
   IF ( heat1 ) heat = 1
   IF ( kgg1/=0 ) kgg = 1
   IF ( mgg1/=0 ) mgg = 1
   ngrids = ngp
   ugv = 0
   ngg = 3*ngp
   dict(1) = iestid
   dict(2) = 1
   IF ( .NOT.heat1 ) THEN
      dict(3) = ngg
      dict(4) = 7
      IF ( kgg<=0 ) THEN
         ik = iz
         nk = ik + 3*ngg - 1
         im = nk + 1
         nm = (ngp+1)*ngp/2 + nk
      ELSE
         ik = iz + 3*ngg
         nk = ik - 1 + (ngg+1)*ngg/2
         nm = nk
         IF ( mgg>0 ) THEN
            im = nk + 1
            nm = nk + (ngp+1)*ngp/2
         ENDIF
      ENDIF
   ELSE
      dict(3) = ngp
      dict(4) = 1
      ik = iz + 17
      nk = ik - 1 + ngp**2
      im = nk + 1
      nm = im - 1 + ngp**2
      ngg = ngp
   ENDIF
   in = nm + 1
   ig = in + ngp
   ix = ig + 3*ngp
   nd = nm + 9*ngp
   IF ( ugv/=0 ) THEN
      id = nd + 1
      nd = id + ngg - 1
   ENDIF
   IF ( nd>nz ) THEN
      WRITE (otpt,99001) ufm , nerr1 , ihex , Type , elno , eid
!
99001 FORMAT (A23,I5,2H, ,A4,I1,3A4,I9,' INSUFFICIENT CORE TO COMPUTE',' ELEMENT MATRIX')
      nogo = .TRUE.
   ENDIF
!
!     ***** OPEN CORE MAP *****
!
!     DOUBLE PRECISION Z(1)
!     COMMON /EMGZZZ/  Z
!
!     NGG = ORDER OF ELEMENT MATRIX
!
!     INDEX      STIFFNESS             MASS                HEAT
!                AND MASS              ONLY              TRANSFER
!
!     IZ    NGG BY 3 PARTITION  NGG BY 3 PARTITION  FOUR WORD COORDINATE
!           OF MATRIX           OF MATRIX           VECTOR.  INPUT TO
!                                                   TRANSD
!
!     IZ+2                                          TRANSFORMED THERMAL
!                                                   CONDUCTANCE MATRIX
!
!     IT                                            MATERIAL TRANSFOR-
!                                                   MATION MATRIX
!
!     IK    SYMMETRIC HALF OF   SAME AS IZ          FULL CONDUCTANCE
!           STIFFNESS
!
!     IM    SYMMETRIC HALF OF   SYMMETRIC HALF OF   FULL CAPACITANCE
!           MASS                MASS
!
!     IN    --------------------SHAPE FUNCTIONS-------------------------
!
!     IG    --------------------D(SHAPE)/D(GREEK)-----------------------
!
!     IX    --------------------D(SHAPE)/D(BASIC XYZ)-------------------
!
!     ID    DISPLACEMENT
!           VECTOR IN BASIC
!           COORDINATES
!
!     CHECK GEOMETRY.  THE FOLLOWING CHECKS ARE MADE
!           1.  ASPECT RATIO
!           2.  ANGLES BETWEEN NORMALS OF SUB-TRIANGLES ON EACH FACE
!           3.  ANGLES BETWEEN VECTORS BETWEEN POINTS ALONG EACH EDGE
!           4.  REVERSE SEQUENCING
!           5.  DUPLICATE COORDINATE VALUES
!
!     FETCH EPT DATA, COMPUTE EST POINTERS
!
   mid = 10 + 12*(Type-1)
   cid = iest(mid+1)
   nip = iest(mid+2)
   maxar = est(mid+3)
   alfa = est(mid+4)
   beta = est(mid+5)
   bgpdt = mid + 6
   gpt = bgpdt + ngp*4
   mid = iest(mid)
   IF ( nip<2 .OR. nip>4 ) nip = Type/2 + 2
   IF ( maxar<=0.0 ) maxar = dmaxar(Type)
   IF ( alfa<0.0 ) alfa = dalfa(Type)
   IF ( beta<0.0 .AND. Type/=1 ) beta = dbeta(Type-1)
   alfa = cos(dtor*alfa)
   beta = cos(dtor*beta)
   IF ( ugv/=0 ) THEN
!
!     TRANSFORM DISPLACEMENT VECTOR TO BASIC COORDINATES
!     MULTIPLY BY 1/4 TO AVOID MULTIPLYING STRAIN-DISPLACEMENT
!     RELATIONS BY 1/2 UNDER THE INTEGRAL.  DITTO FOR LOADING TEMP-S.
!
      dstld = gptld(1)
      DO i = 1 , ngp
         m = bgpdt + 4*i - 4
         j = ugv + sil(i) - 1
         k = id + 3*i - 3
         IF ( iest(m)==0 ) THEN
            DO l = 1 , 3
               z(n+l-1) = rz(j+l-1)*0.25
            ENDDO
            gptld(i) = 0.25*gptld(i)
         ELSE
            CALL transs(est(m),tk)
            DO l = 1 , 3
               z(iz+l-1) = rz(j+l-1)*0.25
            ENDDO
            CALL gmmats(tk,3,3,0,z(iz),3,1,0,z(n))
            gptld(i) = 0.25*gptld(i)
         ENDIF
      ENDDO
   ENDIF
!
!     REARRANGE BGPDT
!
   DO i = 1 , ngp
      jz(izs+i) = iest(bgpdt+i*4-4)
   ENDDO
   bcord = gpt - 3
   DO i = 2 , ngp
      DO j = 1 , 3
         k = bgpdt + 4*(ngp-i) + 4 - j
         bcord = bcord - 1
         est(bcord) = est(k)
      ENDDO
   ENDDO
   DO i = 2 , ngp
      iest(bgpdt+i-1) = jz(izs+i)
   ENDDO
!
!     IF COMPUTING DIFFERENTIAL STIFFNESS, SKIP CHECKS
!
   IF ( ugv<=0 ) THEN
!
!     FIND 8 POINTERS TO CORNER COORDINATES IN EST
!
!     EDGE        CORNERS
!       1         1     2
!       2         2     3
!       3         3     4
!       4         4     1
!       5         1     5
!       6         2     6
!       7         3     7
!       8         4     8
!       9         5     6
!      10         6     7
!      11         7     8
!      12         8     5
!
      nc(1) = bcord
      j = 3*Type
      IF ( Type==2 ) THEN
         nc(5) = bcord + 36
      ELSEIF ( Type==3 ) THEN
         nc(5) = bcord + 60
      ELSE
         nc(5) = bcord + 12
      ENDIF
      DO i = 2 , 4
         nc(i) = nc(i-1) + j
         nc(i+4) = nc(i+3) + j
      ENDDO
!
!     COMPUTE 12 EDGE VECTORS, FIND SMALLEST AND LARGEST MAGNITUDES
!
      i = 0
      j = 1
      smag = 1.0E+20
      bmag = 0.0
      DO edge = 1 , 12
         IF ( edge==4 .OR. edge==12 ) THEN
            l = m
            m = nc(j-3) - 1
            GOTO 20
         ELSEIF ( edge==5 ) THEN
            i = 0
            j = 4
         ELSEIF ( edge==9 ) THEN
            i = 4
            j = 5
         ENDIF
         i = i + 1
         j = j + 1
         l = nc(i) - 1
         m = nc(j) - 1
 20      tmag = 0.0
         DO k = 1 , 3
            evec(k,edge) = est(m+k) - est(l+k)
            tmag = tmag + evec(k,edge)**2
         ENDDO
         IF ( tmag<smag ) smag = tmag
         IF ( tmag>bmag ) bmag = tmag
      ENDDO
!
!     CHECK ASPECT RATIO
!
      IF ( smag<=0.0 ) smag = 1.0E-10
      IF ( bmag/smag>maxar**2 ) THEN
         WRITE (otpt,99002) ufm , nerr2 , ihex , Type , elno , eid , bar , excd
         nogo = .TRUE.
      ENDIF
!
!     CHECK ANGLES BETWEEN FACE NORMALS
!
!     FACE              CORNERS
!       1         1     4     3     2
!       2         1     2     6     5
!       3         2     3     7     6
!       4         3     4     8     7
!       5         4     1     5     8
!       6         5     6     7     8
!
      DO face = 1 , 6
         IF ( face==2 ) THEN
            i = 1
            j = 6
            k = 9
            l = 5
         ELSEIF ( face==3 .OR. face==4 ) THEN
            i = i + 1
            j = j + 1
            k = k + 1
            l = l + 1
         ELSEIF ( face==5 ) THEN
            i = 4
            j = 5
            k = 12
            l = 8
         ELSEIF ( face==6 ) THEN
            i = 12
            j = 9
            k = 10
            l = 11
         ELSE
            i = 1
            j = 4
            k = 3
            l = 2
         ENDIF
         DO n = 1 , 2
            vn(1,1) = evec(2,i)*evec(3,j) - evec(3,i)*evec(2,j)
            vn(2,1) = evec(3,i)*evec(1,j) - evec(1,i)*evec(3,j)
            vn(3,1) = evec(1,i)*evec(2,j) - evec(2,i)*evec(1,j)
            vn(1,2) = evec(2,k)*evec(3,l) - evec(3,k)*evec(2,l)
            vn(2,2) = evec(3,k)*evec(1,l) - evec(1,k)*evec(3,l)
            vn(3,2) = evec(1,k)*evec(2,l) - evec(2,k)*evec(1,l)
            smag = 0.0
            bmag = 0.0
            tmag = 0.0
            DO m = 1 , 3
               smag = smag + vn(m,1)**2
               bmag = bmag + vn(m,2)**2
               tmag = vn(m,1)*vn(m,2) + tmag
            ENDDO
            smag = sqrt(smag*bmag)
            IF ( smag/=0.0 ) THEN
!
!     EPSILON INTRODUCED TO OVERCOME ROUNDOUT ERROR
!
               IF ( tmag/smag<0.99*alfa ) THEN
                  WRITE (otpt,99002) ufm , nerr2 , ihex , Type , elno , eid , balfa , excd
                  nogo = .TRUE.
               ENDIF
            ENDIF
            m = i
            i = l
            l = k
            k = j
            j = m
         ENDDO
      ENDDO
!
!     CHECK MID-EDGE POINTS
!
      IF ( Type/=1 ) THEN
         m = 1
         DO edge = 1 , 12
            IF ( edge==5 ) THEN
               m = 0
               GOTO 30
            ELSEIF ( edge==6 .OR. edge==7 .OR. edge==8 ) THEN
               GOTO 30
            ELSEIF ( edge==9 ) THEN
               m = 5
            ENDIF
            i = nc(m)
            j = i + 3
            k = j + 3
            l = k + 3
            m = m + 1
            IF ( edge==4 .OR. edge==12 ) THEN
               IF ( Type==2 ) k = nc(m-4)
               IF ( Type==3 ) l = nc(m-4)
            ENDIF
            GOTO 40
 30         m = m + 1
            i = nc(m)
            j = i + 12*Type - 3*(m-1)*(Type-1)
            k = j + 12
            k = k + 3*(m-1)*(3-Type)
            l = nc(m+4)
 40         smag = 0.0
            bmag = 0.0
            tmag = 0.0
            DO n = 1 , 3
               vn(n,1) = est(j+n-1) - est(i+n-1)
               vn(n,2) = est(k+n-1) - est(j+n-1)
               tmag = tmag + vn(n,1)*vn(n,2)
               smag = smag + vn(n,1)**2
               bmag = bmag + vn(n,2)**2
            ENDDO
            smag = sqrt(smag*bmag)
            IF ( smag/=0.0 ) THEN
               IF ( tmag/smag<beta ) GOTO 50
            ENDIF
            IF ( Type==2 ) CYCLE
            tmag = 0.0
            smag = 0.0
            DO n = 1 , 3
               vn(n,1) = est(l+n-1) - est(k+n-1)
               tmag = tmag + vn(n,1)*vn(n,2)
               smag = smag + vn(n,1)**2
            ENDDO
            smag = sqrt(smag*bmag)
            IF ( smag==0.0 ) CYCLE
            IF ( tmag/smag>=beta ) CYCLE
 50         WRITE (otpt,99002) ufm , nerr2 , ihex , Type , elno , eid , bbeta , excd
            nogo = .TRUE.
         ENDDO
      ENDIF
!
!     CHECK FOR LEFT-HANDED ELEMENT COORDINATE SYSTEM
!
!     VOL = EVEC(5)*(EVEC(1) X -EVEC(4))
!
      vn(1,1) = evec(2,4)*evec(3,1) - evec(3,4)*evec(2,1)
      vn(2,1) = evec(3,4)*evec(1,1) - evec(1,4)*evec(3,1)
      vn(3,1) = evec(1,4)*evec(2,1) - evec(2,4)*evec(1,1)
      tmag = 0.0
      DO i = 1 , 3
         tmag = tmag + evec(i,5)*vn(i,1)
      ENDDO
      IF ( tmag<=0.0 ) THEN
         WRITE (otpt,99002) ufm , nerr2 , ihex , Type , elno , eid , rvrs
         nogo = .TRUE.
      ENDIF
!
!     CHECK FOR DUPLICATE COORDINATE VALUES
!
      l = ngp - 1
      DO i = 1 , l
         m = bcord + 3*(i-1)
         k = i + 1
         DO j = k , ngp
            n = bcord + 3*(j-1)
            IF ( est(m)==est(n) ) THEN
               IF ( est(m+1)==est(n+1) ) THEN
                  IF ( est(m+2)==est(n+2) ) THEN
                     WRITE (otpt,99002) ufm , nerr2 , ihex , Type , elno , eid , twins
                     nogo = .TRUE.
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
      ENDDO
!
!     IF NOGO FLAG ON, DON T COMPUTE ELEMENT MATRICES
!
      IF ( nogo ) RETURN
   ENDIF
!
!     INITIALIZE FOR NUMERICAL INTEGRATION
!
!     ABSCISSAE AND WEIGHT COEFFICIENTS FOR GAUSSIAN QUADRATURE
!
   i = nip - 1
   IF ( i==2 ) THEN
      h(1) = gauss(2)
      s(1) = gauss(3)
      h(2) = gauss(4)
      s(2) = 0.0
      h(3) = gauss(2)
      s(3) = -gauss(3)
   ELSEIF ( i==3 ) THEN
      h(1) = gauss(5)
      s(1) = gauss(6)
      h(2) = gauss(7)
      s(2) = gauss(8)
      h(3) = gauss(7)
      s(3) = -gauss(8)
      h(4) = gauss(5)
      s(4) = -gauss(6)
   ELSE
      h(1) = 1.0
      s(1) = gauss(1)
      h(2) = 1.0
      s(2) = -gauss(1)
   ENDIF
!
!     GENERATE TABLE OF EQUIVALENTS IN SIL ARRAY SO MATRIX WILL BE
!     ORDERED ACCORDING TO INCREASING SIL NUMBERS
!
   i = -ngp
   DO
      j = 0
      DO k = 1 , ngp
         IF ( sil(k)>=j ) THEN
            j = sil(k)
            l = k
         ENDIF
      ENDDO
      sil(l) = i
      i = i + 1
      IF ( i>=0 ) THEN
         DO i = 1 , ngp
            sil(i) = -sil(i)
         ENDDO
!
!     NOW SIL(I) = PARTITION NUMBER OF ELEMENT GRID POINT I
!
!     ZERO OUT OPEN CORE FOR MATRIX SUMMATION
!
         DO i = ik , nm
            z(i) = 0.0
         ENDDO
!
!     BRANCH ON HEAT TRANSFER FLAG
!
         IF ( heat==1 ) THEN
!
!     HEAT TRANSFER SECTION
!
            inflag = 3
            CALL hmat(eid)
            anis = .FALSE.
            IF ( kgg<=0 ) GOTO 700
!
!     CHECK FOR ANISOTROPY
!
            IF ( kheat(1)/=kheat(4) .OR. kheat(1)/=kheat(6) ) GOTO 600
            IF ( kheat(2)==0.0 .AND. kheat(3)==0.0 .AND. kheat(5)==0.0 ) GOTO 700
            GOTO 600
         ELSE
!
!     FETCH MATERIAL PROPERTIES
!
!     =============================================================
!     THIS SECTION OF CODE MUST BE UPDATED WHEN GENERAL ANISOTROPIC
!     MATERIAL IS ADDED.
!
!     TEST FOR ANISOTROPIC MATERIAL
!
            inflag = 10
            anis = .FALSE.
!
!     TEST FOR RECTANGULAR COORDINATE SYSTEM IN WHICH THE ANISOTROPIC
!     MATERIAL IS DEFINED
!
            rect = .TRUE.
!     ===============================================================
!
!     CHECK FOR TEMPERATURE DEPENDENCE
!
            tdep = .TRUE.
            DO i = 2 , ngp
               IF ( est(gpt)/=est(gpt+i-1) ) GOTO 60
            ENDDO
            tdep = .FALSE.
 60         temp = est(gpt)
            CALL mat(eid)
            IF ( .NOT.mtdep ) tdep = .FALSE.
            IF ( ib(46)==6 ) anis = .TRUE.
            IF ( kgg>0 ) THEN
!
!     IF ISOTROPIC, TEMPERATURE INDEPENDENT MATERIAL, COMPUTE CONSTANTS
!
               IF ( .NOT.(anis .OR. tdep) ) THEN
                  IF ( ib(46)/=0 ) THEN
!
!     SET UP FOR EASY MULTIPLICATION IF MATERIALS ARE ON MAT1
!
                     e1 = bufm6(1)
                     e2 = bufm6(2)
                     e3 = bufm6(22)
                  ELSE
                     WRITE (otpt,99003) ufm , mid , eid
                     nogo = .TRUE.
                     RETURN
                  ENDIF
               ENDIF
            ENDIF
!
!     ============================================================
!     CODE TO TRANSFORM GENERAL ANISOTROPIC MATERIAL PROPERTIES TO
!     BASIC COORDINATE SYSTEM MUST BE ADDED HERE.
!     ============================================================
!
!     ALL SET TO BEGIN INTEGRATION LOOPS.  DO IT.
!
            tvol = 0.0
            DO i = 1 , nip
               DO j = 1 , nip
                  DO k = 1 , nip
!
!     GENERATE SHAPE FUNCTIONS AND JACOBIAN MATRIX INVERSE
!
                     CALL ihexss(Type,z(in),z(ig),jacob,detj,eid,s(i),s(j),s(k),est(bcord))
                     IF ( detj/=0.0 ) THEN
!
                        sfact = h(i)*h(j)*h(k)*detj
                        tvol = tvol + sfact
!
!     STIFFNESS
!
!     COMPUTE STRAIN-DISPLACEMENT RELATIONS
!
!     MUST REVERSE CALLING ORDER SINCE MATRICES ARE STORED BY COLUMNS
!
                        IF ( kgg>0 ) CALL gmmats(z(ig),ngp,3,0,jacob,3,3,0,z(ix))
!
!     IF MATERIAL IS TEMPERATURE DEPENDENT, MUST COMPUTE TEMPERATURE
!     AT THIS INTEGRATION POINT AND FETCH MATERIAL PROPERTIES AGAIN
!
                        IF ( .NOT.tdep ) THEN
                           IF ( kgg<=0 ) GOTO 62
!
!     IF MATERIAL IS ANISOTROPIC AND NOT DEFINED IN RECTANGULAR COOR-
!     DINATE SYSTEM, MUST TRANSFORM TO BASIC COORDINATE SYSTEM AT THIS
!     INTEGRATION POINT
!     IN THIS VERSION, ANISOTROPIC MATERIAL SYSTEMS MUST BE RECTANGULAR.
!     THEREFORE, NO FURTHER TRANSFORMATIONS ARE NECESSARY
!
!
!     ================================================================
!     THIS CODE MUST BE COMPLETED WHEN GENERAL ANISOTROPIC MATERIAL IS
!     ADDED
!
                           IF ( .NOT.anis ) GOTO 62
                        ELSE
                           temp = 0.0
                           DO l = 1 , ngp
                              temp = temp + z(in+l-1)*est(gpt+l-1)
                           ENDDO
                           CALL mat(eid)
                           IF ( kgg<=0 ) GOTO 62
                           IF ( .NOT.(anis) ) THEN
                              IF ( ib(46)/=0 ) THEN
!
                                 e1 = bufm6(1)
                                 e2 = bufm6(2)
                                 e3 = bufm6(22)
                                 GOTO 62
                              ELSE
                                 WRITE (otpt,99003) ufm , mid , eid
                                 nogo = .TRUE.
                                 RETURN
                              ENDIF
                           ENDIF
                        ENDIF
!
!     INSERT GLOBAL TO BASIC TRANSFORMATION OPERATIONS HERE FOR
!     ANISOTROPIC MATERIAL MATRIX
!     =============+==================================================
!
                        DO ijk = 1 , 36
                           gmat(ijk) = bufm6(ijk)
                        ENDDO
                        IF ( rect ) THEN
                        ENDIF
                     ELSE
!
!     BAD ELEMENT IF FALL HERE.  JACOBIAN MATRIX WAS SINGULAR.
!
                        nogo = .TRUE.
                        RETURN
                     ENDIF
!
!     MATERIAL HAS BEEN EVALUATED FOR THIS INTEGRATION POINT WHEN
!     FALL HERE.
!
 62                  IF ( ugv/=0 ) THEN
!
!     COMPUTE STRESSES FOR DIFFERENTIAL STIFFNESS MATRIX
!
!     THERMAL EFFECTS
!
                        IF ( idstld==-1 ) THEN
!     ===========================================================
                           DO l = 1 , 6
                              sig(l) = 0.0
                           ENDDO
                        ELSE
                           temp = 0.0
                           DO l = 1 , ngp
                              temp = temp + z(in+l-1)*gptld(l)
                           ENDDO
                           temp = temp - tref
                           IF ( anis ) THEN
!     ===========================================================
!
!     ADD THERMAL STRESS COMPUTATIONS FOR ANISOTROPIC MATERIAL
!
!     STORE ALPHA IN DOUBLE PRECISION
!
                              DO ijk = 1 , 6
                                 dalpha(ijk) = bufm6(ijk+37)
                              ENDDO
!
                              CALL gmmats(gmat,6,6,0,dalpha,6,1,0,sig)
                              DO ijk = 1 , 6
                                 sig(ijk) = -sig(ijk)*temp
                              ENDDO
                           ELSE
                              sig(1) = -talpha*(e1+2.0*e2)*temp
                              sig(2) = sig(1)
                              sig(3) = sig(1)
                              sig(4) = 0.0
                              sig(5) = 0.0
                              sig(6) = 0.0
                           ENDIF
                        ENDIF
!
!     DISPLACEMENT EFFECTS, COMPUTE STRESS MATRIX AND MULTIPLY BY DISPL.
!
                        str(12) = 0.0
                        str(13) = 0.0
                        str(17) = 0.0
                        DO l = 1 , ngp
                           ii = ix + 3*l - 4
                           IF ( anis ) THEN
!     =========================================================
!
!
!     ADD STRESS MATRIX COMPUTATION FOR ANISOTROPIC MATERIAL
!
                              DO ijk = 1 , 18
                                 store(ijk) = 0.
                              ENDDO
                              store(1) = z(ii+1)
                              store(5) = z(ii+2)
                              store(9) = z(ii+3)
                              store(10) = z(ii+2)
                              store(11) = z(ii+1)
                              store(14) = z(ii+3)
                              store(15) = z(ii+2)
                              store(16) = z(ii+3)
                              store(18) = z(ii+1)
!
                              CALL gmmats(gmat,6,6,0,store(1),6,3,0,str)
                           ELSE
                              str(1) = e1*z(ii+1)
                              str(2) = e2*z(ii+2)
                              str(3) = e2*z(ii+3)
                              str(4) = e2*z(ii+1)
                              str(5) = e1*z(ii+2)
                              str(6) = e2*z(ii+3)
                              str(7) = e2*z(ii+1)
                              str(8) = e2*z(ii+2)
                              str(9) = e1*z(ii+3)
                              str(10) = e3*z(ii+2)
                              str(11) = e3*z(ii+1)
                              str(14) = e3*z(ii+3)
                              str(15) = e3*z(ii+2)
                              str(16) = e3*z(ii+3)
                              str(18) = e3*z(ii+1)
                           ENDIF
!
!     ============================================================
!
                           CALL gmmats(str,6,3,-2,z(id+3*l-3),3,1,0,sig)
                        ENDDO
                        str(1) = sx
                        sx = sx + sy
                        sy = sy + sz
                        sz = sz + str(1)
                     ENDIF
!
!     NOW BEGIN LOOPS OVER GRID POINTS ALONG ROWS AND COLUMNS
!
                     DO n = 1 , ngp
                        DO m = n , ngp
!
!     COMPUTE PARTITION FOR POINTWISE ROW M AND COLUMN N
!
                           IF ( kgg>0 ) THEN
                              IF ( .NOT.anis ) THEN
                              ENDIF
!
!     =================================================================
!     MUST ADD CODE TO COMPUTE THE CONTRIBUTION TO THE STIFFNESS MATRIX
!     FOR ANISOTROPIC MATERIAL HERE
!     =================================================================
!
                              IF ( sil(m)>=sil(n) ) THEN
                                 mz = ix + (m-1)*3
                                 nz = ix + (n-1)*3
                              ELSE
!
!     MUST COMPUTE TRANSPOSE OF THIS PARTITION FOR SUMMATION IN ELEMENT
!     MATRIX
!
                                 mz = ix + (n-1)*3
                                 nz = ix + (m-1)*3
                              ENDIF
                              IF ( ugv/=0 ) THEN
!
!     DIFFERENTIAL STIFFNESS
!
                                 DO l = 1 , 3
                                    DO inc = 1 , 3
                                       c(l,inc) = z(mz+inc-1)*z(nz+l-1)
                                    ENDDO
                                 ENDDO
                                 part(1,1) = sx*c(2,2) + syz*(c(2,3)+c(3,2)) + sz*c(3,3)
                                 part(2,2) = sy*c(3,3) + szx*(c(3,1)+c(1,3)) + sx*c(1,1)
                                 part(3,3) = sz*c(1,1) + sxy*(c(1,2)+c(2,1)) + sy*c(2,2)
                                 part(2,1) = -sx*c(2,1) + sxy*c(3,3) - syz*c(1,3) - szx*c(2,3)
                                 part(3,1) = -sz*c(3,1) - sxy*c(3,2) - syz*c(2,1) + szx*c(2,2)
                                 part(1,2) = -sx*c(1,2) + sxy*c(3,3) - syz*c(3,1) - szx*c(3,2)
                                 part(3,2) = -sy*c(3,2) - sxy*c(3,1) + syz*c(1,1) - szx*c(1,2)
                                 part(1,3) = -sz*c(1,3) - sxy*c(2,3) - syz*c(1,2) + szx*c(2,2)
                                 part(2,3) = -sy*c(2,3) - sxy*c(1,3) + syz*c(1,1) - szx*c(2,1)
!
!     ELASTIC STIFFNESS
!
                              ELSEIF ( .NOT.anis ) THEN
                                 part(1,1) = e1*z(nz)*z(mz) + e3*(z(nz+1)*z(mz+1)+z(nz+2)*z(mz+2))
                                 part(2,2) = e1*z(nz+1)*z(mz+1) + e3*(z(nz)*z(mz)+z(nz+2)*z(mz+2))
                                 part(3,3) = e1*z(nz+2)*z(mz+2) + e3*(z(nz)*z(mz)+z(nz+1)*z(mz+1))
                                 part(2,1) = e2*z(nz)*z(mz+1) + e3*z(nz+1)*z(mz)
                                 part(3,1) = e2*z(nz)*z(mz+2) + e3*z(nz+2)*z(mz)
                                 part(1,2) = e2*z(nz+1)*z(mz) + e3*z(nz)*z(mz+1)
                                 part(3,2) = e2*z(nz+1)*z(mz+2) + e3*z(nz+2)*z(mz+1)
                                 part(1,3) = e2*z(nz+2)*z(mz) + e3*z(nz)*z(mz+2)
                                 part(2,3) = e2*z(nz+2)*z(mz+1) + e3*z(nz+1)*z(mz+2)
                              ELSE
!
!     STORE CI MATRIX
!
                                 DO ijk = 1 , 18
                                    store(ijk) = 0.
                                 ENDDO
                                 store(1) = z(mz)
                                 store(4) = z(mz+1)
                                 store(6) = z(mz+2)
                                 store(8) = z(mz+1)
                                 store(10) = z(mz)
                                 store(11) = z(mz+2)
                                 store(15) = z(mz+2)
                                 store(17) = z(mz+1)
                                 store(18) = z(mz)
!
                                 CALL gmmats(store(1),3,6,0,gmat(1),6,6,0,store(19))
!
!     STORE CJ
!
                                 DO ijk = 1 , 18
                                    store(ijk) = 0.
                                 ENDDO
                                 store(1) = z(nz)
                                 store(5) = z(nz+1)
                                 store(9) = z(nz+2)
                                 store(10) = z(nz+1)
                                 store(11) = z(nz)
                                 store(14) = z(nz+2)
                                 store(15) = z(nz+1)
                                 store(16) = z(nz+2)
                                 store(18) = z(nz)
!
                                 CALL gmmats(store(19),3,6,0,store(1),6,3,0,store(37))
                                 ijkl = 0
                                 DO ijk = 1 , 3
                                    DO ijl = 1 , 3
                                       ijkl = ijkl + 1
                                       part(ijk,ijl) = store(ijkl+36)
                                    ENDDO
                                 ENDDO
                              ENDIF
!
!     ADD STIFFNESS PARTITION TO ELEMENT MATRIX
!
!     COMPUTE INDEX INTO OPEN CORE WHERE PART(1,1) IS TO BE ADDED.
!
                              IF ( sil(m)<sil(n) ) THEN
                                 mz = sil(n)
                                 nz = sil(m)
                                 diag = .FALSE.
                              ELSEIF ( sil(m)==sil(n) ) THEN
                                 mz = sil(m)
                                 nz = sil(n)
                                 diag = .TRUE.
                              ELSE
                                 mz = sil(m)
                                 nz = sil(n)
                                 diag = .FALSE.
                              ENDIF
!
!     COLUMN NUMBER
!
                              l = (nz-1)*3 + 1
!
!     INCREMENT BETWEEN COLUMNS
!
                              inc = ngg - l
!
!     FIRST WORD OF COLUMN
!
                              l = ik + ((l-1)*l)/2 + (inc+1)*(l-1)
!
!     WORD IN COLUMN FOR THIS ROW
!
                              l = l + 3*(mz-nz)
!
!     ADD PARTITION
!
                              DO nz = 1 , 3
                                 DO mz = 1 , 3
                                    IF ( .NOT.(diag .AND. mz<nz) ) z(l+mz-1) = z(l+mz-1) + part(mz,nz)*sfact
                                 ENDDO
                                 l = l + inc
                                 inc = inc - 1
                              ENDDO
                           ENDIF
                           IF ( mgg>0 ) THEN
!
!     MASS
!
!     COMPUTE TERM FOR MASS MATRIX
!
                              rho = bufm6(37)
                              mz = sil(m)
                              nz = sil(n)
                              IF ( mz<nz ) THEN
                                 mz = sil(n)
                                 nz = sil(m)
                              ENDIF
!
!     COMPUTE INDEX INTO OPEN CORE FOR THIS MASS TERM
!
                              l = (nz*(nz+1))/2 + (nz-1)*(ngp-nz) + mz - nz + im - 1
!
!     COMPUTE AND ADD MASS TERM TO ELEMENT MATRIX
!
                              z(l) = z(l) + rho*sfact*z(in+m-1)*z(in+n-1)
                           ENDIF
                        ENDDO
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
!
!     END OF INTEGRATION LOOPS
!
            icode = 7
!
!     LOOK FOR NON-BASIC COORDINATE SYSTEM
!
            nocstm = .FALSE.
            DO i = 1 , ngp
               IF ( iest(bgpdt+i-1)/=0 ) GOTO 80
            ENDDO
            nocstm = .TRUE.
            GOTO 300
         ENDIF
!
!     RESTORE GRID POINT DATA TO ORIGINAL FORM FOR DOING TRANSFORM
!     TO GLOBAL COORDINATES
!
!     FIRST, TRANSFER IT TO OPEN CORE AT IN
!
 80      k = (in-1)*2 + 1
         j = ngp*4
         DO i = 1 , j
            rz(k+i-1) = est(bgpdt+i-1)
         ENDDO
!
!     NOW MOVE IT BACK AND REARRANGE IT
!
         DO i = 1 , ngp
            iest(bgpdt+4*i-4) = jz(k+i-1)
            DO j = 1 , 3
               est(bgpdt+4*i-4+j) = rz(k+ngp+3*i+j-4)
            ENDDO
         ENDDO
!
!     FETCH GLOBAL TO BASIC TRANSFORMATION MATRICES
!
         DO i = 1 , ngp
            j = in + (i-1)*9
            CALL transs(est(bgpdt+4*i-4),z(j))
         ENDDO
         IF ( kgg<=0 ) GOTO 400
!
!     TRANSFORM STIFFNESS TO GLOBAL COORDINATES
!
         i = 0
         EXIT
      ENDIF
   ENDDO
 100  i = i + 1
   icp = sil(i)
!
!     COLUMN INDICES
!
   k = (icp-1)*3 + 1
   inc = ngg - k + 1
   l = ik + ((k-1)*k)/2 + inc*(k-1)
   m = l + inc
   n = m + inc - 1
!
!     TRANSFORMATION MATRIX INDEX
!
   igcs = iest(bgpdt+4*i-4)
   nz = in + (i-1)*9
   IF ( igcs/=0 ) THEN
!
!     TERMS ON DIAGONAL PARTITION
!
      ASSIGN 200 TO back
      GOTO 1600
   ENDIF
!
!     OFF-DIAGONAL PARTITIONS
!
 200  l = l + 3
   m = m + 2
   n = n + 1
   irp = icp + 1
   IF ( irp<=ngp ) THEN
      mz = nz
      DO j = irp , ngp
         DO k = 1 , ngp
            IF ( j==sil(k) ) EXIT
         ENDDO
         IF ( igcs==0 ) THEN
            IF ( iest(bgpdt+4*k-4)==0 ) GOTO 220
         ENDIF
         nz = in + (k-1)*9
         DO k = 1 , 3
            tk(k,1) = 0.0
            tk(k,2) = 0.0
            tk(k,3) = 0.0
            DO ii = 1 , 3
               tk(k,1) = tk(k,1) + z(l+ii-1)*z(nz+3*ii+k-4)
               tk(k,2) = tk(k,2) + z(m+ii-1)*z(nz+3*ii+k-4)
               tk(k,3) = tk(k,3) + z(n+ii-1)*z(nz+3*ii+k-4)
            ENDDO
         ENDDO
         DO k = 1 , 3
            z(l+k-1) = 0.0
            z(m+k-1) = 0.0
            z(n+k-1) = 0.0
            DO ii = 1 , 3
               z(l+k-1) = z(l+k-1) + tk(k,ii)*z(mz+3*ii-3)
               z(m+k-1) = z(m+k-1) + tk(k,ii)*z(mz+3*ii-2)
               z(n+k-1) = z(n+k-1) + tk(k,ii)*z(mz+3*ii-1)
            ENDDO
         ENDDO
 220     l = l + 3
         m = m + 3
         n = n + 3
      ENDDO
   ENDIF
   IF ( i<ngp ) GOTO 100
!
!     BUILD STIFFNESS PARTITIONS AND PASS TO EMGOUT
!
 300  idon = 0
   DO i = 1 , ngp
      IF ( i==ngp ) idon = 1
      DO j = 1 , 3
!
!     COLUMN NUMBER
!
         k = (i-1)*3 + j
!
!     NUMBER OF TERMS TO FETCH TO COMPLETE THIS COLUMN IN PARTITION
!
         l = k - 1
         IF ( l/=0 ) THEN
!
!     FETCH TERMS AND LOAD INTO J-TH COLUMN OF PARTITION
!
            n = ik + l
            inc = ngg - 1
            DO m = 1 , l
               z(iz+ngg*j-ngg+m-1) = z(n)
               n = n + inc
               inc = inc - 1
            ENDDO
         ENDIF
!
!     FILL OUT PARTITION WITH COLUMNS OF STIFFNESS MATRIX
!
!     COMPUTE INDEX IN OPEN CORE OF FIRST TERM OF COLUMN K
!
         n = ik + ((k-1)*k)/2 + (ngg-k+1)*(k-1)
!
!     INSERT THIS COLUMN IN PARTITION
!
         DO m = k , ngg
            z(iz+ngg*j-ngg+m-1) = z(n)
            n = n + 1
         ENDDO
      ENDDO
      dict(5) = ib(45)
      CALL emgout(z(iz),z(iz),3*ngg,idon,dict,1,1)
   ENDDO
!
!     EXPAND AND TRANSFORM MASS MATRIX AND PASS TO EMGOUT
!
   IF ( mgg<=0 ) GOTO 500
 400  idon = 0
   DO i = 1 , ngp
      IF ( i==ngp ) idon = 1
      DO j = 1 , ngp
!
!     COMPUTE INDEX INTO OPEN CORE FOR MASS TERM
!
         k = i
         l = j
         IF ( i>j ) THEN
            k = j
            l = i
         ENDIF
         n = ((k-1)*k)/2 + (k-1)*(ngp-k+1) + l - k + im
!
!     MULTIPLY GLOBAL TO BASIC TRANSFORMATIONS
!
         m = iz - ngg + 3*j - 4
         IF ( .NOT.(i==j .OR. nocstm) ) THEN
            IF ( iest(bgpdt+4*i-4)/=0 ) GOTO 420
            IF ( iest(bgpdt+4*j-4)/=0 ) GOTO 420
         ENDIF
         z(m+ngg+1) = z(n)
         z(m+ngg+2) = 0.0
         z(m+ngg+3) = 0.0
         z(m+ngg*2+1) = 0.0
         z(m+ngg*2+2) = z(n)
         z(m+ngg*2+3) = 0.0
         z(m+ngg*3+1) = 0.0
         z(m+ngg*3+2) = 0.0
         z(m+ngg*3+3) = z(n)
         CYCLE
 420     DO k = 1 , ngp
            IF ( i==sil(k) ) mz = in + 9*(k-1)
            IF ( j==sil(k) ) nz = in + 9*(k-1)
         ENDDO
         CALL gmmats(z(mz),3,3,1,z(nz),3,3,0,tf)
!
!     MULTIPLY BY MASS SCALAR FOR THIS 3 BY 3 PARTITION AND STORE
!     IN NGG BY 3 PARTITION
!
         DO k = 1 , 3
            DO l = 1 , 3
               z(m+ngg*l+k) = tf(k,l)*z(n)
            ENDDO
         ENDDO
      ENDDO
      dict(5) = 0
      CALL emgout(z(iz),z(iz),3*ngg,idon,dict,2,1)
   ENDDO
!
!     SAVE ELEMENT BCD NAME, ID, VOLUME, MASS, NO. OF GRID POINTS, AND
!     GRID POINT DATA IN SCR4 IF USER REQUESTED VOLUME/AREA PRINTOUT
!     (NOTE - MAKE SURE THE GRID POINT DATA, BGPDT, IS IN ISTS ORIGIANL
!      FORM)
!
 500  IF ( volume>0.0 .OR. surfac>0.0 ) THEN
      il = iz*2
      rz(il+1) = bcd1
      rz(il+2) = bcd2(Type)
      jz(il+3) = eid
      rz(il+4) = tvol*volume
      rz(il+5) = tvol
      IF ( rho>0.0 ) rz(il+5) = tvol*rho
      jz(il+6) = ngp
      k = il + 6
      DO i = 1 , ngp
         k = k + 1
         rz(k) = est(1+i)
      ENDDO
      IF ( surfac>0.0 ) THEN
         IF ( .NOT.nocstm ) THEN
            j = ngp*4
            DO i = 1 , j
               k = k + 1
               rz(k) = est(bgpdt+i-1)
            ENDDO
         ELSE
            l = bgpdt + ngp
            DO i = 1 , ngp
               k = k + 1
               jz(k) = iest(bgpdt+i-1)
               DO j = 1 , 3
                  k = k + 1
                  rz(k) = est(l)
                  l = l + 1
               ENDDO
            ENDDO
         ENDIF
      ENDIF
      l = k - il
      CALL write(scr4,rz(il+1),l,1)
   ENDIF
   GOTO 1500
 600  anis = .TRUE.
   it = iz + 8
!
!     CHECK FOR RECTANGULAR COORDINATE SYSTEM FOR MATERIAL
!
   rect = .TRUE.
   IF ( cid/=0 ) THEN
      jz(izs) = cid
      DO i = 1 , 3
         rz(izs+i) = est(bcord+i-1)
      ENDDO
      CALL transs(rz(izs),z(it))
      DO i = 1 , 3
         rz(izs+i) = -rz(izs+i)
      ENDDO
      CALL transs(rz(izs),z(in))
      DO i = 1 , 9
         IF ( z(it+i-1)/=z(in+i-1) ) rect = .FALSE.
      ENDDO
!
!     IF NOT DEFINED IN A RECTANGULAR SYSTEM, MUST TRANSFORM INSIDE
!     INTEGRATION LOOPS
!
      IF ( rect ) THEN
!
!     TRANSFORM MATERIAL MATRIX TO BASIC SYSTEM
!
         DO i = 1 , 6
            z(iz+i+1) = kheat(i)
         ENDDO
         l = iz + 2
         m = l + 3
         n = m + 2
         nz = it
         ASSIGN 700 TO back
         GOTO 1600
      ENDIF
   ENDIF
!
!     ANISOTROPIC CONDUCTIVITY MATERIAL MATRIX NOW STORED AT RZ(IZ+2)
!     TO RZ(IZ+7)
!
!     ALL SET FOR DOING INTEGRATION.  DO IT.
!
 700  i = 0
 800  i = i + 1
   j = 0
 900  j = j + 1
   k = 0
 1000 k = k + 1
!
!     GENERATE SHAPE FUNCTIONS AND JACOBIAN MATRIX INVERSE
!
   CALL ihexss(Type,z(in),z(ig),jacob,detj,eid,s(i),s(j),s(k),est(bcord))
   IF ( detj/=0.0 ) THEN
!
      sfact = h(i)*h(j)*h(k)*detj
!
!     COMPUTE DERIVATIVES OF SHAPE FUNCTION W.R.T. BASIC SYSTEM.
!
!     MUST REVERSE CALLING ORDER SINCE MATRICES ARE STORED BY COLUMNS
!
      IF ( kgg>0 ) CALL gmmats(z(ig),ngp,3,0,jacob,3,3,0,z(ix))
!
!     IF MATERIAL IS ANISOTROPIC AND NOT DEFINED IN A RECTANGULAR
!     CORDINATE SYSTEM, MUST TRANSFORM TO BASIC SYSTEM AT THIS
!     INTEGRATION POINT
!
      IF ( anis ) THEN
         IF ( .NOT.(rect) ) THEN
!
!     COMPUTE BASIC COORDINATES VECTOR AT THIS POINT
!
            DO l = 1 , 3
               rz(izs+l) = 0.0
            ENDDO
            DO l = 1 , ngp
               DO m = 1 , 3
                  rz(izs+m) = rz(izs+m) + z(in+l-1)*est(bcord+3*l+m-4)
               ENDDO
            ENDDO
!
!     FETCH TRANSFORMATION AND CONDUCTIVITY MATRICES AND PERFORM
!     TRANSFORMATION OPERATIONS
!
            CALL transs(rz(izs),z(it))
            DO l = 1 , 6
               z(iz+l+1) = kheat(l)
            ENDDO
            nz = it
            l = iz + 2
            m = l + 3
            n = m + 2
            ASSIGN 1100 TO back
            GOTO 1600
         ENDIF
      ENDIF
   ELSE
!
!     FALL HERE IF JACOBIAN MATRIX WAS SINGULAR
!
      nogo = .TRUE.
      RETURN
   ENDIF
!
!     MATERIAL HAS BEEN EVALUATED FOR THIS INTEGRATION POINT WHEN
!     FALL HERE
!
!     NOW BEGIN LOOPS OVER GRID POINTS ALONG ROWS AND COLUMNS
!
 1100 DO n = 1 , ngp
      DO m = n , ngp
!
!     COMPUTE 1 BY 1 PARTITION FOR ROW M AND COLUMN N
!
         IF ( kgg>0 ) THEN
!
!     CONDUCTIVITY
!
            IF ( anis ) THEN
!
!     ANISOTROPIC CASE
!
               l = ix + 3*(m-1)
               e1 = z(l)*z(iz+2) + z(l+1)*z(iz+3) + z(l+2)*z(iz+4)
               e2 = z(l)*z(iz+3) + z(l+1)*z(iz+5) + z(l+2)*z(iz+6)
               e3 = z(l)*z(iz+4) + z(l+1)*z(iz+6) + z(l+2)*z(iz+7)
               l = ix + 3*(n-1)
               prt1 = sfact*(z(l)*e1+z(l+1)*e2+z(l+2)*e3)
            ELSE
!
!     ISOTROPIC CASE
!
               prt1 = 0.0
               DO l = 1 , 3
                  prt1 = prt1 + z(ix+3*m+l-4)*z(ix+3*n+l-4)
               ENDDO
               prt1 = sfact*kheat(1)*prt1
            ENDIF
!
!     COMPUTE INDEX INTO OPEN CORE FOR THIS TERM
!
            l = sil(m)
            mz = sil(n)
            IF ( l>mz ) THEN
               l = mz
               mz = sil(m)
            ENDIF
            l = (l-1)*ngg + mz + ik - 1
!
!     ADD TERM TO MATRIX
!
            z(l) = z(l) + prt1
         ENDIF
!
!     CAPACITANCE
!
         IF ( mgg>0 ) THEN
!
!     COMPUTE INDEX INTO OPEN CORE FOR THIS TERM
!
            l = sil(m)
            mz = sil(n)
            IF ( l>mz ) THEN
               l = mz
               mz = sil(m)
            ENDIF
            l = (l-1)*ngg + mz + im - 1
!
!     COMPUTE AND ADD TERM
!
            z(l) = z(l) + sfact*cp*z(in+m-1)*z(in+n-1)
         ENDIF
      ENDDO
   ENDDO
   IF ( k<nip ) GOTO 1000
   IF ( j<nip ) GOTO 900
   IF ( i<nip ) GOTO 800
!
!     END OF HEAT TRANSFER INTEGRATION LOOPS
!
   icode = 1
!
!     FILL IN THE UPPER TRIANGLES OF THE MATRICES
!
   IF ( kgg>0 ) THEN
      mz = ik
      GOTO 1300
   ENDIF
 1200 IF ( mgg<=0 ) GOTO 1400
   mz = im
 1300 l = ngg - 1
   DO i = 1 , l
      j = i + 1
      DO k = j , ngg
         m = (i-1)*ngg + k + mz - 1
         n = (k-1)*ngg + i + mz - 1
         z(n) = z(m)
      ENDDO
   ENDDO
   IF ( mz==ik ) GOTO 1200
!
!     PASS MATRICES TO EMGOUT
!
 1400 k = ngg**2
   dict(5) = 0
   IF ( kgg>0 ) CALL emgout(z(ik),z(ik),k,1,dict,1,1)
   IF ( mgg>0 ) CALL emgout(z(im),z(im),k,1,dict,3,1)
!
!     ALL DONE, NO ERRORS
!
 1500 RETURN
!
!
!     INTERNAL SUBROUTINE
!
!     TRANSFORM COORDINATE SYSTEM OF SYMMETRIC HALF OF A 3 BY 3 MATRIX
!
 1600 tk(1,1) = z(nz)*z(l) + z(nz+3)*z(l+1) + z(nz+6)*z(l+2)
   tk(2,1) = z(nz+1)*z(l) + z(nz+4)*z(l+1) + z(nz+7)*z(l+2)
   tk(3,1) = z(nz+2)*z(l) + z(nz+5)*z(l+1) + z(nz+8)*z(l+2)
   tk(1,2) = z(nz)*z(l+1) + z(nz+3)*z(m) + z(nz+6)*z(m+1)
   tk(2,2) = z(nz+1)*z(l+1) + z(nz+4)*z(m) + z(nz+7)*z(m+1)
   tk(3,2) = z(nz+2)*z(l+1) + z(nz+5)*z(m) + z(nz+8)*z(m+1)
   tk(1,3) = z(nz)*z(l+2) + z(nz+3)*z(m+1) + z(nz+6)*z(n)
   tk(2,3) = z(nz+1)*z(l+2) + z(nz+4)*z(m+1) + z(nz+7)*z(n)
   tk(3,3) = z(nz+2)*z(l+2) + z(nz+5)*z(m+1) + z(nz+8)*z(n)
   z(l) = z(nz)*tk(1,1) + z(nz+3)*tk(1,2) + z(nz+6)*tk(1,3)
   z(l+1) = z(nz)*tk(2,1) + z(nz+3)*tk(2,2) + z(nz+6)*tk(2,3)
   z(l+2) = z(nz)*tk(3,1) + z(nz+3)*tk(3,2) + z(nz+6)*tk(3,3)
   z(m) = z(nz+1)*tk(2,1) + z(nz+4)*tk(2,2) + z(nz+7)*tk(2,3)
   z(m+1) = z(nz+1)*tk(3,1) + z(nz+4)*tk(3,2) + z(nz+7)*tk(3,3)
   z(n) = z(nz+2)*tk(3,1) + z(nz+5)*tk(3,2) + z(nz+8)*tk(3,3)
   GOTO back
99002 FORMAT (A23,I5,2H, ,A4,I1,3A4,I9,3X,18HILLEGAL GEOMETRY, ,9A4)
99003 FORMAT (A23,' 4005. AN ILLEGAL VALUE OF -NU- HAS BEEN SPECIFIED ','UNDER MATERIAL ID =',I10,17H FOR ELEMENT ID =,I10)
!
END SUBROUTINE ihexs
