
SUBROUTINE dihex(Type)
   IMPLICIT NONE
   REAL Bufm6(46) , Cdamp , Dum12(12) , Dum2(2) , Dum6(6) , E , Est(328) , Evec(3,12) , G , Nu , Rho , Space(18) , Sys(6) , Sysbuf ,&
      & Talpha , Temp , Tref , Vn(3,2) , Work(66) , Zs(1)
   DOUBLE PRECISION C(3,3) , H(4) , Jacob(3,3) , Part(3,3) , S(4) , Sig(6) , Sk(6,6) , Str(18) , Tf(3,3) , Tk(3,3) , Z(1)
   INTEGER Eid , I6x6k , Ib(46) , Iest(1) , Inflag , Isil(32) , Iwork(1) , Jmax , Jz(1) , Mid , Mtemp , Nc(8) , Nogo , Npvt ,       &
         & Nrowsc , Otpt , Sil(1)
   LOGICAL Mtdep
   COMMON /ds1aaa/ Npvt , Dum6 , I6x6k , Dum12 , Jmax , Dum2 , Nrowsc
   COMMON /ds1adp/ Isil , Sk , Work , Str
   COMMON /ds1aet/ Est
   COMMON /matin / Mid , Inflag , Temp
   COMMON /matiso/ Bufm6
   COMMON /matout/ E , G , Nu , Rho , Talpha , Tref , Cdamp , Space , Mtdep
   COMMON /system/ Sysbuf , Otpt , Nogo , Sys , Mtemp
   COMMON /zzzzzz/ Zs
   INTEGER Type
   REAL alfa , beta , dalfa(3) , dbeta(2) , dmaxar(3) , dtor , grid(128) , maxar
   LOGICAL anis , diag , rect , tdep
   INTEGER bcord , bgpdt , cid , elno(3) , gpt , heat , i , id , ig , igp , igrid(128) , ihex , ii , ijk , ik , im , in , inc ,     &
         & iprec , irp , ix , iz , izs , j , k , kgg , l , m , mgg , mz , n , nd , nerr1 , ngg , ngp , nip , nk , nm , nz , nzs ,   &
         & ufm(6)
   DOUBLE PRECISION dalpha(6) , detj , e1 , e2 , e3 , gauss(8) , gmat(36) , sfact , store(18) , sv , sx , sxy , sy , syz , sz , szx
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
   EQUIVALENCE (Z(1),Jz(1),Zs(1)) , (Eid,Est(1),Iest(1)) , (Sil(1),Est(2)) , (Work(1),Iwork(1)) , (Sig(1),sx) , (Sig(2),sy) ,       &
    & (Sig(3),sz) , (Sig(4),sxy) , (Sig(5),syz) , (Sig(6),szx)
   EQUIVALENCE (Work(1),Evec(1,1)) , (Work(37),Vn(1,1)) , (Work(43),Nc(1))
   EQUIVALENCE (Work(1),Jacob(1,1)) , (Work(19),H(1)) , (Work(27),S(1)) , (Work(35),Part(1,1)) , (Work(53),Sig(1)) ,                &
    & (Work(1),C(1,1))
   EQUIVALENCE (Work(1),Tf(1,1)) , (Work(35),Tk(1,1))
   EQUIVALENCE (Ib(1),Bufm6(1)) , (grid(1),igrid(1))
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
   izs = 1 + 2*(I6x6k+Jmax*Nrowsc)
   nzs = izs + 10655
   iz = izs/2 + 1
   nz = nzs/2 + 1
   iprec = 2
!
!     ALLOCATE LARGE ARRAYS IN OPEN CORE
!
   ngp = 12*Type - 4
   DO i = 1 , ngp
      IF ( Sil(i)==Npvt ) GOTO 100
   ENDDO
   Nogo = 1
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
      WRITE (Otpt,99001) ufm , nerr1 , ihex , Type , elno , Eid
!
!
99001 FORMAT (6A4,I4,2H, ,A4,I1,3A4,I9,' INSUFFICIENT CORE TO COMPUTE',' ELEMENT MATRIX')
      Nogo = 1
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
   Mid = 10 + 12*(Type-1)
   cid = Iest(Mid+1)
   nip = Iest(Mid+2)
   maxar = Est(Mid+3)
   alfa = Est(Mid+4)
   beta = Est(Mid+5)
   bgpdt = Mid + 6
   gpt = bgpdt + 4*ngp
   Mid = Iest(Mid)
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
      IF ( Iest(m)==0 ) THEN
         DO l = 1 , 3
            Z(n+l-1) = dble(Est(j+l-1)*0.25)
         ENDDO
      ELSE
         CALL transd(Est(m),Z(iz))
         DO l = 1 , 3
            Z(ik+l-1) = dble(Est(j+l-1)*0.25)
         ENDDO
         CALL gmmatd(Z(iz),3,3,0,Z(ik),3,1,0,Z(n))
      ENDIF
   ENDDO
!
!     REARRANGE BGPDT
!
   DO i = 1 , ngp
      igrid(i) = Iest(bgpdt+4*i-4)
   ENDDO
   bcord = gpt - 3
   DO i = 2 , ngp
      DO j = 1 , 3
         k = bgpdt + 4*(ngp-i) + 4 - j
         bcord = bcord - 1
         Est(bcord) = Est(k)
      ENDDO
   ENDDO
   DO i = 2 , ngp
      Iest(bgpdt+i-1) = igrid(i)
   ENDDO
!
!     INITIALIZE FOR NUMERICAL INTEGRATION
!
!
!     ABSCISSAE AND WEIGHT COEFFICIENTS FOR GAUSSIAN QUADRATURE
!
   i = nip - 1
   IF ( i==2 ) THEN
      H(1) = gauss(2)
      S(1) = gauss(3)
      H(2) = gauss(4)
      S(2) = 0.0
      H(3) = gauss(2)
      S(3) = -gauss(3)
   ELSEIF ( i==3 ) THEN
      H(1) = gauss(5)
      S(1) = gauss(6)
      H(2) = gauss(7)
      S(2) = gauss(8)
      H(3) = gauss(7)
      S(3) = -gauss(8)
      H(4) = gauss(5)
      S(4) = -gauss(6)
   ELSE
      H(1) = 1.0
      S(1) = gauss(1)
      H(2) = 1.0
      S(2) = -gauss(1)
   ENDIF
!
!     GENERATE TABLE OF EQUIVALENTS IN SIL ARRAY SO MATRIX WILL BE
!     ORDERED ACCORDING TO INCREASING SIL NUMBERS
!
   DO i = 1 , ngp
      Isil(i) = Sil(i)
      Sil(i) = i
   ENDDO
!
!     NOW SIL(I) = PARTITION NUMBER OF ELEMENT GRID POINT I
!
!     ZERO OUT OPEN CORE FOR MATRIX SUMMATION
!
   DO i = ik , nm
      Z(i) = 0.0
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
      IF ( Est(gpt)/=Est(gpt+i-1) ) GOTO 200
   ENDDO
   tdep = .FALSE.
 200  Temp = Est(gpt)
   Inflag = 10
   CALL mat(Eid)
   IF ( .NOT.Mtdep ) tdep = .FALSE.
   IF ( Ib(46)==6 ) anis = .TRUE.
   Tref = Bufm6(44)
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
               gmat(ijk) = Bufm6(ijk)
            ENDDO
         ELSEIF ( Ib(46)/=0 ) THEN
!
            e1 = Bufm6(1)
            e2 = Bufm6(2)
            e3 = Bufm6(22)
            Talpha = Bufm6(38)
         ELSE
            WRITE (Otpt,99002) ufm , Mid , Eid
            Nogo = 1
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
            CALL ihexsd(Type,Z(in),Z(ig),Jacob,detj,Eid,S(i),S(j),S(k),Est(bcord))
            IF ( detj/=0.0 ) THEN
!
               sfact = H(i)*H(j)*H(k)*detj
!
!     STIFFNESS
!
!     COMPUTE STRAIN-DISPLACEMENT RELATIONS
!
!     MUST REVERSE CALLING ORDER SINCE MATRICES ARE STORED BY COLUMNS
!
               IF ( kgg>0 ) CALL gmmatd(Z(ig),ngp,3,0,Jacob,3,3,0,Z(ix))
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
                  Temp = 0.0
                  DO l = 1 , ngp
                     Temp = Temp + Z(in+l-1)*Est(gpt+l-1)
                  ENDDO
                  CALL mat(Eid)
                  IF ( kgg<=0 ) GOTO 210
                  IF ( .NOT.(anis) ) THEN
                     IF ( Ib(46)/=0 ) THEN
!
                        e1 = Bufm6(1)
                        e2 = Bufm6(2)
                        e3 = Bufm6(22)
                        Talpha = Bufm6(38)
                        GOTO 210
                     ELSE
                        WRITE (Otpt,99002) ufm , Mid , Eid
                        Nogo = 1
                        RETURN
                     ENDIF
                  ENDIF
               ENDIF
!
!     INSERT GLOBAL TO BASIC TRANSFORMATION OPERATIONS HERE FOR
!     ANISOTROPIC MATERIAL MATRIX
!
               DO ijk = 1 , 36
                  gmat(ijk) = Bufm6(ijk)
               ENDDO
            ELSE
!
!     BAD ELEMENT IF FALL HERE.  JACOBIAN MATRIX WAS SINGULAR.
!
               Nogo = 1
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
 210        IF ( Iest(gpt+ngp+1)==-1 ) THEN
               DO l = 1 , 6
                  Sig(l) = 0.0
               ENDDO
            ELSE
!
!     COMPUTE LOADING TEMPERATURE AT THIS POINT
!
               Temp = 0.0
               DO l = 1 , ngp
                  Temp = Temp + Z(in+l-1)*Est(gpt+ngp+l)
               ENDDO
               Temp = 0.25*(Temp-Tref)
               IF ( anis ) THEN
!
!     ANISOTROPIC
!
                  DO ijk = 1 , 6
                     dalpha(ijk) = Bufm6(ijk+37)
                  ENDDO
!
                  CALL gmmatd(gmat(1),6,6,0,dalpha(1),6,1,0,Sig)
                  DO ijk = 1 , 6
                     Sig(ijk) = -Sig(ijk)*Temp
                  ENDDO
               ELSE
                  Sig(1) = -Talpha*(e1+2.0*e2)*Temp
                  Sig(2) = Sig(1)
                  Sig(3) = Sig(1)
                  Sig(4) = 0.0
                  Sig(5) = 0.0
                  Sig(6) = 0.0
               ENDIF
            ENDIF
!
!     DISPLACEMENT EFFECTS, COMPUTE STRESS MATRIX AND MULTIPLY BY DISPL.
!
            Str(12) = 0.0
            Str(13) = 0.0
            Str(17) = 0.0
            DO l = 1 , ngp
               ii = ix + 3*l - 4
               IF ( anis ) THEN
!
!     ANISOTROPIC
!
                  DO ijk = 1 , 18
                     store(ijk) = 0.D0
                  ENDDO
                  store(1) = Z(ii+1)
                  store(5) = Z(ii+2)
                  store(9) = Z(ii+3)
                  store(10) = Z(ii+2)
                  store(11) = Z(ii+1)
                  store(14) = Z(ii+3)
                  store(15) = Z(ii+2)
                  store(16) = Z(ii+3)
                  store(18) = Z(ii+1)
!
                  CALL gmmatd(gmat(1),6,6,0,store(1),6,3,0,Str(1))
               ELSE
                  Str(1) = e1*Z(ii+1)
                  Str(2) = e2*Z(ii+2)
                  Str(3) = e2*Z(ii+3)
                  Str(4) = e2*Z(ii+1)
                  Str(5) = e1*Z(ii+2)
                  Str(6) = e2*Z(ii+3)
                  Str(7) = e2*Z(ii+1)
                  Str(8) = e2*Z(ii+2)
                  Str(9) = e1*Z(ii+3)
                  Str(10) = e3*Z(ii+2)
                  Str(11) = e3*Z(ii+1)
                  Str(14) = e3*Z(ii+3)
                  Str(15) = e3*Z(ii+2)
                  Str(16) = e3*Z(ii+3)
                  Str(18) = e3*Z(ii+1)
               ENDIF
!
               CALL gmmatd(Str,6,3,-2,Z(id+3*l-3),3,1,0,Sig)
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
                        IF ( Sil(m)>=Sil(n) ) THEN
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
                              C(l,inc) = Z(mz+inc-1)*Z(nz+l-1)
                           ENDDO
                        ENDDO
                        Part(1,1) = sx*C(2,2) + syz*(C(2,3)+C(3,2)) + sz*C(3,3)
                        Part(2,2) = sy*C(3,3) + szx*(C(3,1)+C(1,3)) + sx*C(1,1)
                        Part(3,3) = sz*C(1,1) + sxy*(C(1,2)+C(2,1)) + sy*C(2,2)
                        Part(2,1) = -sx*C(2,1) + sxy*C(3,3) - syz*C(1,3) - szx*C(2,3)
                        Part(3,1) = -sz*C(3,1) - sxy*C(3,2) - syz*C(2,1) + szx*C(2,2)
                        Part(1,2) = -sx*C(1,2) + sxy*C(3,3) - syz*C(3,1) - szx*C(3,2)
                        Part(3,2) = -sy*C(3,2) - sxy*C(3,1) + syz*C(1,1) - szx*C(1,2)
                        Part(1,3) = -sz*C(1,3) - sxy*C(2,3) - syz*C(1,2) + szx*C(2,2)
                        Part(2,3) = -sy*C(2,3) - sxy*C(1,3) + syz*C(1,1) - szx*C(2,1)
!
!     ADD STIFFNESS PARTITION TO ELEMENT MATRIX
!
!     COMPUTE INDEX INTO OPEN CORE WHERE PART(1,1) IS TO BE ADDED.
!
                        IF ( Sil(m)<Sil(n) ) THEN
                           mz = Sil(n)
                           nz = Sil(m)
                           diag = .FALSE.
                        ELSEIF ( Sil(m)==Sil(n) ) THEN
                           mz = Sil(m)
                           nz = Sil(n)
                           diag = .TRUE.
                        ELSE
                           mz = Sil(m)
                           nz = Sil(n)
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
                              IF ( .NOT.(diag .AND. mz<nz) ) Z(l+mz-1) = Z(l+mz-1) + Part(mz,nz)*sfact
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
      IF ( Iest(bgpdt+i-1)/=0 ) GOTO 300
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
      grid(i) = Est(bgpdt+i-1)
   ENDDO
!
!     NOW MOVE IT BACK AND REARRANGE IT
!
   DO i = 1 , ngp
      Iest(bgpdt+4*i-4) = igrid(i)
      DO j = 1 , 3
         Est(bgpdt+4*i-4+j) = grid(ngp+3*i+j-3)
      ENDDO
   ENDDO
!
!     FETCH GLOBAL TO BASIC TRANSFORMATION MATRICES
!
   DO i = 1 , ngp
      j = in + (i-1)*9
      CALL transd(Est(bgpdt+4*i-4),Z(j))
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
      CALL tktztk(Tk,Z,nz,l,m,n)
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
               Tk(k,1) = 0.0
               Tk(k,2) = 0.0
               Tk(k,3) = 0.0
               DO mz = 1 , 3
                  Tk(k,1) = Tk(k,1) + Z(l+mz-1)*Z(nz+3*mz+k-4)
                  Tk(k,2) = Tk(k,2) + Z(m+mz-1)*Z(nz+3*mz+k-4)
                  Tk(k,3) = Tk(k,3) + Z(n+mz-1)*Z(nz+3*mz+k-4)
               ENDDO
            ENDDO
            mz = in + 9*(i-1)
            DO k = 1 , 3
               Z(l+k-1) = 0.0
               Z(m+k-1) = 0.0
               Z(n+k-1) = 0.0
               DO ii = 1 , 3
                  Z(l+k-1) = Z(l+k-1) + Tk(k,ii)*Z(mz+3*ii-3)
                  Z(m+k-1) = Z(m+k-1) + Tk(k,ii)*Z(mz+3*ii-2)
                  Z(n+k-1) = Z(n+k-1) + Tk(k,ii)*Z(mz+3*ii-1)
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
      Sk(i,1) = 0.0D0
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
            Z(iz+ngg*j-ngg+m-1) = Z(n)
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
         Z(iz+ngg*j-ngg+m-1) = Z(n)
         n = n + 1
      ENDDO
   ENDDO
   DO i = 1 , ngp
      j = (i-1)*3 + iz - 1
      DO m = 1 , 3
         DO n = 1 , 3
            k = j + n + (m-1)*ngg
            Sk(n,m) = Z(k)
         ENDDO
      ENDDO
      CALL ds1b(Sk,Isil(i))
   ENDDO
!
!     ALL DONE, NO ERRORS
!
   RETURN
99002 FORMAT (6A4,'4005. AN ILLEGAL VALUE OF -NU- HAS BEEN SPECIFIED ','UNDER MATERIAL ID =',I10,17H FOR ELEMENT ID =,I10)
!
END SUBROUTINE dihex
