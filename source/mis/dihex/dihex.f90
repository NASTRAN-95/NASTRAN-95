!*==dihex.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE dihex(Type)
   IMPLICIT NONE
   USE c_ds1aaa
   USE c_ds1adp
   USE c_ds1aet
   USE c_matin
   USE c_matiso
   USE c_matout
   USE c_system
   USE c_zzzzzz
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Type
!
! Local variable declarations rewritten by SPAG
!
   REAL :: alfa , beta , maxar
   LOGICAL :: anis , diag , rect , tdep
   INTEGER :: bcord , bgpdt , cid , eid , gpt , heat , i , id , ig , igp , ii , ijk , ik , im , in , inc , iprec , irp , ix , iz ,  &
            & izs , j , k , l , m , mz , n , nd , ngg , ngp , nip , nk , nm , nz , nzs
   REAL*8 , DIMENSION(3,3) :: c , jacob , part , tf , tk
   REAL , DIMENSION(3) , SAVE :: dalfa , dmaxar
   REAL*8 , DIMENSION(6) :: dalpha , sig
   REAL , DIMENSION(2) , SAVE :: dbeta
   REAL*8 :: detj , e1 , e2 , e3 , sfact , sv , sx , sxy , sy , syz , sz , szx
   REAL , SAVE :: dtor
   INTEGER , DIMENSION(3) , SAVE :: elno
   REAL , DIMENSION(3,12) :: evec
   REAL*8 , DIMENSION(8) , SAVE :: gauss
   REAL*8 , DIMENSION(36) :: gmat
   REAL , DIMENSION(128) :: grid
   REAL*8 , DIMENSION(4) :: h , s
   INTEGER , DIMENSION(46) :: ib
   INTEGER , DIMENSION(1) :: iest , iwork , jz , sil
   INTEGER , DIMENSION(128) :: igrid
   INTEGER , SAVE :: ihex , kgg , mgg , nerr1
   INTEGER , DIMENSION(8) :: nc
   REAL*8 , DIMENSION(18) :: store
   INTEGER , DIMENSION(6) , SAVE :: ufm
   REAL , DIMENSION(3,2) :: vn
   REAL*8 , DIMENSION(1) :: z
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
!     THIS ROUTINE PROCESSES IHEX1, IHEX2, AND IHEX3 ELEMENT DATA TO
!     PRODUCE THE DIFFERENTIAL STIFFNESS MATRIX
!
!           TYPE = 1    IHEX1
!           TYPE = 2    IHEX2
!           TYPE = 3    IHEX3
!
!     THE EST ENTRIES ARE
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
!     DEF       56      128      200    NOT USED
!     GPTLD  57-64  129-148  201-232    GRID POINT TEMPERATURE LOADS
!     UGV    65-88  149-208  233-328    GLOBAL DISPLACEMENT VECTOR
!
   !>>>>EQUIVALENCE (Z(1),Jz(1),Zs(1)) , (Eid,Est(1),Iest(1)) , (Sil(1),Est(2)) , (Work(1),Iwork(1)) , (Sig(1),sx) , (Sig(2),sy) ,       &
!>>>>    & (Sig(3),sz) , (Sig(4),sxy) , (Sig(5),syz) , (Sig(6),szx)
   !>>>>EQUIVALENCE (Work(1),Evec(1,1)) , (Work(37),Vn(1,1)) , (Work(43),Nc(1))
   !>>>>EQUIVALENCE (Work(1),Jacob(1,1)) , (Work(19),H(1)) , (Work(27),S(1)) , (Work(35),Part(1,1)) , (Work(53),Sig(1)) ,                &
!>>>>    & (Work(1),C(1,1))
   !>>>>EQUIVALENCE (Work(1),Tf(1,1)) , (Work(35),Tk(1,1))
   !>>>>EQUIVALENCE (Ib(1),Bufm6(1)) , (grid(1),igrid(1))
   DATA kgg/101/ , mgg/ - 1/
   DATA dmaxar , dalfa , dbeta/5.0 , 10.0 , 15.0 , 45.0 , 45.0 , 45.0 , 45.0 , 45.0/
   DATA dtor , gauss/0.017453292519943E0 , 0.577350269189626D0 , 0.555555555555556D0 , 0.774596669241483D0 , 0.888888888888889D0 ,  &
      & 0.347854845137454D0 , 0.861136311594053D0 , 0.652145154862546D0 , 0.339981043584856D0/
   DATA ufm/4H0*** , 4H USE , 4HR FA , 4HTAL  , 4HMESS , 4HAGE /
   DATA ihex , elno/4HIHEX , 4H ELE , 4HMENT , 4H NO./
   DATA nerr1/2141/
!
   heat = 0
!
!     FOR DOUBLE PRECISION, OPEN CORE POINTERS MUST BE MODIFIED
!
   izs = 1 + 2*(i6x6k+jmax*nrowsc)
   nzs = izs + 10655
   iz = izs/2 + 1
   nz = nzs/2 + 1
   iprec = 2
!
!     ALLOCATE LARGE ARRAYS IN OPEN CORE
!
   ngp = 12*Type - 4
   DO i = 1 , ngp
      IF ( sil(i)==npvt ) GOTO 100
   ENDDO
   nogo = 1
 100  igp = i
   IF ( heat==1 ) THEN
      ik = iz + 17
      nk = ik - 1 + ngp**2
      im = nk + 1
      nm = im - 1 + ngp**2
      ngg = ngp
   ELSE
      ngg = 3*ngp
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
   ENDIF
   in = nm + 1
   ig = in + ngp
   ix = ig + 3*ngp
   nd = nm + 9*ngp
   id = nd + 1
   nd = id + ngg - 1
   IF ( nd>nz ) THEN
      WRITE (otpt,99001) ufm , nerr1 , ihex , Type , elno , eid
!
!
99001 FORMAT (6A4,I4,2H, ,A4,I1,3A4,I9,' INSUFFICIENT CORE TO COMPUTE',' ELEMENT MATRIX')
      nogo = 1
   ENDIF
!
!     OPEN CORE MAP
!     =============
!
!     DOUBLE PRECISION  Z(1)
!     COMMON  /ZZZZZZ/  Z
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
   gpt = bgpdt + 4*ngp
   mid = iest(mid)
   IF ( nip<2 .OR. nip>4 ) nip = Type/2 + 2
   IF ( maxar<=0.0 ) maxar = dmaxar(Type)
   IF ( alfa<0.0 ) alfa = dalfa(Type)
   IF ( beta<0.0 ) beta = dbeta(Type-1)
   alfa = cos(dtor*alfa)
   beta = cos(dtor*beta)
!
!     TRANSFORM DISPLACEMENT VECTOR TO BASIC COORDINATES
!
   DO i = 1 , ngp
      m = bgpdt + 4*i - 4
      n = id + 3*i - 3
      j = gpt + 2*ngp + 3*(i-1) + 1
      IF ( iest(m)==0 ) THEN
         DO l = 1 , 3
            z(n+l-1) = dble(est(j+l-1)*0.25)
         ENDDO
      ELSE
         CALL transd(est(m),z(iz))
         DO l = 1 , 3
            z(ik+l-1) = dble(est(j+l-1)*0.25)
         ENDDO
         CALL gmmatd(z(iz),3,3,0,z(ik),3,1,0,z(n))
      ENDIF
   ENDDO
!
!     REARRANGE BGPDT
!
   DO i = 1 , ngp
      igrid(i) = iest(bgpdt+4*i-4)
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
      iest(bgpdt+i-1) = igrid(i)
   ENDDO
!
!     INITIALIZE FOR NUMERICAL INTEGRATION
!
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
   DO i = 1 , ngp
      isil(i) = sil(i)
      sil(i) = i
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
!     FETCH MATERIAL PROPERTIES
!
!     THIS SECTION OF CODE MUST BE UPDATED WHEN GENERAL ANISOTROPIC
!     MATERIAL IS ADDED.
!
!     TEST FOR ANISOTROPIC MATERIAL
!
   anis = .FALSE.
!
!     TEST FOR RECTANGULAR COORDINATE SYSTEM IN WHICH THE ANISOTROPIC
!     MATERIAL IS DEFINED
!
   rect = .TRUE.
!
!     CHECK FOR TEMPERATURE DEPENDENCE
!
   tdep = .TRUE.
   DO i = 2 , ngp
      IF ( est(gpt)/=est(gpt+i-1) ) GOTO 200
   ENDDO
   tdep = .FALSE.
 200  temp = est(gpt)
   inflag = 10
   CALL mat(eid)
   IF ( .NOT.mtdep ) tdep = .FALSE.
   IF ( ib(46)==6 ) anis = .TRUE.
   tref = bufm6(44)
!
   IF ( kgg>0 ) THEN
!
!     IF ISOTROPIC, TEMPERATURE INDEPENDENT MATERIAL, COMPUTE CONSTANTS
!
      IF ( .NOT.(tdep) ) THEN
         IF ( anis ) THEN
!
!     IF MATERIAL IS ANISOTROPIC, DEFINED IN A RECTANGULAR COORDINATE
!     SYSTEM, AND NOT TEMPERATURE DEPENDENT, TRANSFORM IT TO BASIC
!     SYSTEM
!
            DO ijk = 1 , 36
               gmat(ijk) = bufm6(ijk)
            ENDDO
         ELSEIF ( ib(46)/=0 ) THEN
!
            e1 = bufm6(1)
            e2 = bufm6(2)
            e3 = bufm6(22)
            talpha = bufm6(38)
         ELSE
            WRITE (otpt,99002) ufm , mid , eid
            nogo = 1
            RETURN
         ENDIF
      ENDIF
   ENDIF
!
!     CODE TO TRANSFORM GENERAL ANISOTROPIC MATERIAL PROPERTIES TO
!     BASIC COORDINATE SYSTEM MUST BE ADDED HERE.
!
!     ALL SET TO BEGIN INTEGRATION LOOPS.  DO IT.
!
   DO i = 1 , nip
      DO j = 1 , nip
         DO k = 1 , nip
!
!     GENERATE SHAPE FUNCTIONS AND JACOBIAN MATRIX INVERSE
!
            CALL ihexsd(Type,z(in),z(ig),jacob,detj,eid,s(i),s(j),s(k),est(bcord))
            IF ( detj/=0.0 ) THEN
!
               sfact = h(i)*h(j)*h(k)*detj
!
!     STIFFNESS
!
!     COMPUTE STRAIN-DISPLACEMENT RELATIONS
!
!     MUST REVERSE CALLING ORDER SINCE MATRICES ARE STORED BY COLUMNS
!
               IF ( kgg>0 ) CALL gmmatd(z(ig),ngp,3,0,jacob,3,3,0,z(ix))
!
!     IF MATERIAL IS TEMPERATURE DEPENDENT, MUST COMPUTE TEMPERATURE
!     AT THIS INTEGRATION POINT AND FETCH MATERIAL PROPERTIES AGAIN
!
               IF ( .NOT.tdep ) THEN
                  IF ( kgg<=0 ) GOTO 210
!
!     IF MATERIAL IS ANISOTROPIC AND NOT DEFINED IN RECTANGULAR COOR-
!     DINATE SYSTEM, MUST TRANSFORM TO BASIC COORDINATE SYSTEM AT THIS
!     INTEGRATION POINT
!
!     THIS CODE MUST BE COMPLETED WHEN GENERAL ANISOTROPIC MATERIAL IS
!     ADDED
!
                  IF ( .NOT.anis ) GOTO 210
                  IF ( rect ) GOTO 210
               ELSE
                  temp = 0.0
                  DO l = 1 , ngp
                     temp = temp + z(in+l-1)*est(gpt+l-1)
                  ENDDO
                  CALL mat(eid)
                  IF ( kgg<=0 ) GOTO 210
                  IF ( .NOT.(anis) ) THEN
                     IF ( ib(46)/=0 ) THEN
!
                        e1 = bufm6(1)
                        e2 = bufm6(2)
                        e3 = bufm6(22)
                        talpha = bufm6(38)
                        GOTO 210
                     ELSE
                        WRITE (otpt,99002) ufm , mid , eid
                        nogo = 1
                        RETURN
                     ENDIF
                  ENDIF
               ENDIF
!
!     INSERT GLOBAL TO BASIC TRANSFORMATION OPERATIONS HERE FOR
!     ANISOTROPIC MATERIAL MATRIX
!
               DO ijk = 1 , 36
                  gmat(ijk) = bufm6(ijk)
               ENDDO
            ELSE
!
!     BAD ELEMENT IF FALL HERE.  JACOBIAN MATRIX WAS SINGULAR.
!
               nogo = 1
               RETURN
            ENDIF
!
!     MATERIAL HAS BEEN EVALUATED FOR THIS INTEGRATION POINT WHEN
!     FALL HERE.
!
!
!     COMPUTE STRESSES FOR DIFFERENTIAL STIFFNESS MATRIX
!
!     THERMAL EFFECTS
!
 210        IF ( iest(gpt+ngp+1)==-1 ) THEN
               DO l = 1 , 6
                  sig(l) = 0.0
               ENDDO
            ELSE
!
!     COMPUTE LOADING TEMPERATURE AT THIS POINT
!
               temp = 0.0
               DO l = 1 , ngp
                  temp = temp + z(in+l-1)*est(gpt+ngp+l)
               ENDDO
               temp = 0.25*(temp-tref)
               IF ( anis ) THEN
!
!     ANISOTROPIC
!
                  DO ijk = 1 , 6
                     dalpha(ijk) = bufm6(ijk+37)
                  ENDDO
!
                  CALL gmmatd(gmat(1),6,6,0,dalpha(1),6,1,0,sig)
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
!
!     ANISOTROPIC
!
                  DO ijk = 1 , 18
                     store(ijk) = 0.D0
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
                  CALL gmmatd(gmat(1),6,6,0,store(1),6,3,0,str(1))
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
               CALL gmmatd(str,6,3,-2,z(id+3*l-3),3,1,0,sig)
            ENDDO
            sv = sx
            sx = sx + sy
            sy = sy + sz
            sz = sz + sv
!
!     NOW BEGIN LOOPS OVER GRID POINTS ALONG ROWS AND COLUMNS
!
            DO n = 1 , ngp
               DO m = n , ngp
                  IF ( n==igp .OR. m==igp ) THEN
!
!     COMPUTE PARTITION FOR POINTWISE ROW M AND COLUMN N
!
                     IF ( kgg>0 ) THEN
                        IF ( .NOT.anis ) THEN
                        ENDIF
!
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
                     IF ( mgg<=0 ) THEN
                     ENDIF
                  ENDIF
               ENDDO
            ENDDO
         ENDDO
      ENDDO
   ENDDO
!
!     END OF INTEGRATION LOOPS
!
!
!     LOOK FOR NON-BASIC COORDINATE SYSTEM
!
   DO i = 1 , ngp
      IF ( iest(bgpdt+i-1)/=0 ) GOTO 300
   ENDDO
   GOTO 400
!
!     RESTORE GRID POINT DATA TO ORIGINAL FORM FOR DOING TRANSFORM
!     TO GLOBAL COORDINATES
!
!     FIRST, TRANSFER IT TO OPEN CORE AT IN
!
 300  k = (in-1)*2 + 1
   j = ngp*4
   DO i = 1 , j
      grid(i) = est(bgpdt+i-1)
   ENDDO
!
!     NOW MOVE IT BACK AND REARRANGE IT
!
   DO i = 1 , ngp
      iest(bgpdt+4*i-4) = igrid(i)
      DO j = 1 , 3
         est(bgpdt+4*i-4+j) = grid(ngp+3*i+j-3)
      ENDDO
   ENDDO
!
!     FETCH GLOBAL TO BASIC TRANSFORMATION MATRICES
!
   DO i = 1 , ngp
      j = in + (i-1)*9
      CALL transd(est(bgpdt+4*i-4),z(j))
   ENDDO
!
!     TRANSFORM STIFFNESS TO GLOBAL COORDINATES
!
   DO i = 1 , ngp
!
!     COLUMN INDICES
!
      k = (i-1)*3 + 1
      inc = ngg - k + 1
      l = ik + ((k-1)*k)/2 + inc*(k-1)
      m = l + inc
      n = m + inc - 1
!
!     TRANSFORMATION MATRIX INDEX
!
      nz = in + (i-1)*9
!
!     TERMS ON DIAGONAL PARTITION
!
      CALL tktztk(tk,z,nz,l,m,n)
!
!     OFF-DIAGONAL PARTITIONS
!
      l = l + 3
      m = m + 2
      n = n + 1
      irp = i + 1
      IF ( irp<=ngp ) THEN
         DO j = irp , ngp
            nz = in + 9*(j-1)
            DO k = 1 , 3
               tk(k,1) = 0.0
               tk(k,2) = 0.0
               tk(k,3) = 0.0
               DO mz = 1 , 3
                  tk(k,1) = tk(k,1) + z(l+mz-1)*z(nz+3*mz+k-4)
                  tk(k,2) = tk(k,2) + z(m+mz-1)*z(nz+3*mz+k-4)
                  tk(k,3) = tk(k,3) + z(n+mz-1)*z(nz+3*mz+k-4)
               ENDDO
            ENDDO
            mz = in + 9*(i-1)
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
            l = l + 3
            m = m + 3
            n = n + 3
         ENDDO
      ENDIF
   ENDDO
!
!     BUILD STIFFNESS PARTITIONS AND PASS TO EMGOUT
!
 400  DO i = 1 , 36
      sk(i,1) = 0.0D0
   ENDDO
   i = igp
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
   DO i = 1 , ngp
      j = (i-1)*3 + iz - 1
      DO m = 1 , 3
         DO n = 1 , 3
            k = j + n + (m-1)*ngg
            sk(n,m) = z(k)
         ENDDO
      ENDDO
      CALL ds1b(sk,isil(i))
   ENDDO
!
!     ALL DONE, NO ERRORS
!
   RETURN
99002 FORMAT (6A4,'4005. AN ILLEGAL VALUE OF -NU- HAS BEEN SPECIFIED ','UNDER MATERIAL ID =',I10,17H FOR ELEMENT ID =,I10)
!
END SUBROUTINE dihex
