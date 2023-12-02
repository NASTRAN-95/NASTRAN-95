!*==cfeer4.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cfeer4
   USE c_feeraa
   USE c_feerxc
   USE c_names
   USE c_system
   USE c_unpakx
   USE c_xmssg
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(2) , SAVE :: accept , reject
   REAL , DIMENSION(2) :: alam , dm , dmp1 , status
   REAL(REAL64) , DIMENSION(4) :: d
   LOGICAL :: decrem , dpmach
   REAL :: denom , rms , rsqrt , sumi , sumr , theta2 , unidum
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER :: eor , i , i1 , i2 , ia , ia1 , ib , ibuf1 , ibuf2 , ih , ihl , ihl1 , il , im , inidum , inth , intq , iopn , iprec , &
            & ishft , iv , iv1 , iv1x , iv2 , izz , j , ji , jj , jr , k , ki , kk , kr , l , l1 , lim1 , lim2 , ll , lll , m , mi ,&
            & mid , mm , mr , mrd , msave , n3 , nfound , nord8 , nout , nrow , nrow1 , nrow2 , nrow22 , nrowsq , nw , nz
   INTEGER , DIMENSION(1) :: iz
   REAL(REAL64) , DIMENSION(2) :: lam1
   LOGICAL , DIMENSION(1) :: lz
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL , DIMENSION(8) :: s
   EXTERNAL allmat , close , cnorm1 , gopen , korsz , mesage , open , read , skprec , unpack , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     CFEER4 OBTAINS THE EIGENVALUES AND EIGENVECTORS FROM THE
!     REDUCED TRIDIAGONAL MATRIX FOR THE COMPLEX FEER METHOD
!
   !>>>>EQUIVALENCE (Ksystm(2),Nout) , (Nrow,Mreduc) , (Ksystm(55),Iprec) , (d(1),s(1)) , (Z(1),Iz(1),Lz(1),Dz(1))
   DATA name/4HCFEE , 4HR4  /
   DATA accept , reject/4H  AC , 4HCEPT , 4H -RE , 4HJECT/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     CORE ALLOCATION FOR ALLMAT
!
!     CONTENTS                SIZE             POINTER   TYPE   NAME
!     --------                ----             -------   ----   ----
!     INPUT MATRIX--VECTORS   2*NROW*NROW      IA        COMP   A
!     EIGENVALUES             2*NROW           IL        COMP   LAM
!     H  MATRIX               2*NROW*NROW      IH        COMP   H
!     HL MATRIX               2*NROW*NROW      IHL       COMP   HL
!     VECTOR STORAGE          2*NROW           IV        COMP   VEC
!     MULTIPLIERS             2*NROW           IM        COMP   MULT
!     INTH                    NROW             INTH      INTG   INTH
!     INTQ                    NROW             INTQ      LOGL   INTQ
!
!     CORE ALLOCATION AFTER ALLMAT IS FINISHED
!
!     ALLMAT OUTPUT EIGENVECTORS               IA
!     EIGENVALUES                              IL
!     ORDER OF EXTRACTION                      IH
!     THEORETICAL ERRORS                       IHL
!     NOT USED                                 IV,IM
!     STATUS OF SOLUTIONS                      INTH
!     DISTANCES FROM CENTER                    INTQ
!     VARIABLE PRECISION PHYSICAL EIGENVECTORS IV1
!     VARIABLE PRECISION ORTHOGONAL VECTORS    IV2
!
!     DEFINITION OF INTERNAL PARAMETERS
!
!     DMP1     = D-SUB-M-PLUS-1 = EXTRANEOUS OFF-DIAGONAL ELEMENT
!                OF REDUCED TRIDIAGONAL MATRIX, USED FOR COMPUTING
!                THEORETICAL ERRORS
!     DM       = FINAL OFF-DIAGONAL ELEMENT OF REDUCED TRIDIAGONAL
!                MATRIX
!     NO B     = LOGICAL INDICATOR FOR ABSENCE OF DAMPING MATRIX B
!     DECREM   = LOGICAL INDICATOR FOR DECREMENTED SIZE OF REDUCED
!                PROBLEM
!     NROW     = SIZE OF THE REDUCED PROBLEM (EQUIVALENT TO MREDUC)
!     RMS      = ROOT-MEAN-SQUARE OF EIGENVALUES, USED IN RIGID-BODY
!                ERROR TEST
!     NOTE.....SEE LISTING OF CFCNTL FOR ADDITIONAL DEFINITIONS
!
         IF ( qpr ) WRITE (nout,99001)
99001    FORMAT (1H0,//7H CFEER4,//)
         dpmach = iprec==2
         nord8 = 2*nord4
         decrem = .FALSE.
         SPAG_Loop_1_1: DO
            nrow2 = 2*nrow
            nrowsq = nrow*nrow2
!
!     ALLOCATE CORE FOR ALLMAT
!
            ia = 1
            il = ia + nrowsq
            ih = il + nrow2
            ihl = ih + nrowsq
            iv = ihl + nrowsq
            im = iv + nrow2
            inth = im + nrow2
            intq = inth + nrow
!
!     ALLOCATE CORE FOR PHYSICAL EIGENVECTORS (LEFT FOLLOWS RIGHT)
!
            iv1 = intq + nrow
            iv2 = iv1 + nord8
            IF ( dpmach .AND. mod(iv2,2)==0 ) iv2 = iv2 + 1
            iv1x = iv1 - 1
!
!     TEST FOR INSUFFICIENT CORE
!
            nz = korsz(z(1))
            ibuf1 = nz - ksystm(1)
            ibuf2 = ibuf1 - ksystm(1)
            iopn = ibuf2 - (iv2+nord8)
            IF ( idiag/=0 ) WRITE (nout,99002) iopn
99002       FORMAT (1H ,I10,36H SINGLE PRECISION WORDS OF OPEN CORE,29H NOT USED (SUBROUTINE CFEER4))
            IF ( iopn<=0 ) CALL mesage(-8,0,name(1))
            IF ( iopn<minopn ) minopn = iopn
            IF ( nswp(2)<0 ) EXIT SPAG_Loop_1_1
!
!     CONSTRUCT REDUCED TRIDIAGONAL MATRIX
!
            DO i = ia , il
               z(i) = 0.
            ENDDO
            nrow22 = nrow2 + 2
            CALL gopen(iscr(5),z(ibuf1),rdrew)
            nw = 4*iprec
            eor = 1
            m = 0
            nrow1 = nrow - 1
!
!     ENTER LOOP
!
            DO i = 1 , nrow
               i1 = i - 1
               CALL read(*40,*60,iscr(5),s(1),nw,eor,m)
               IF ( qpr .AND. .NOT.dpmach ) WRITE (nout,99003) i , (s(j),j=1,4)
99003          FORMAT (4H ROW,I5,2(4X,2E16.8))
               IF ( qpr .AND. dpmach ) WRITE (nout,99004) i , (d(j),j=1,4)
99004          FORMAT (4H ROW,I5,2(4X,2D16.8))
!
!     ALLMAT ACCEPTS ONLY SINGLE PRECISION ARRAY
!
               j = ia + nrow22*i1
               IF ( .NOT.dpmach ) THEN
!
!     LOAD MAIN DIAGONAL ELEMENT
!
                  z(j) = s(3)
                  z(j+1) = s(4)
                  IF ( i==nrow1 ) THEN
!
!     SAVE LAST OFF-DIAGONAL ELEMENT
!
                     dm(1) = s(1)
                     dm(2) = s(2)
                  ENDIF
                  IF ( i/=nrow ) THEN
!
!     LOAD OFF-DIAGONAL ELEMENTS
!
                     z(j+2) = s(1)
                     z(j+3) = s(2)
                     j = j + nrow2
                     z(j) = s(1)
                     z(j+1) = s(2)
                  ENDIF
               ELSE
!
!     LOAD MAIN DIAGONAL ELEMENT
!
                  z(j) = d(3)
                  z(j+1) = d(4)
                  IF ( i==nrow1 ) THEN
!
!     SAVE LAST OFF-DIAGONAL ELEMENT
!
                     dm(1) = d(1)
                     dm(2) = d(2)
                  ENDIF
                  IF ( i/=nrow ) THEN
!
!     LOAD OFF-DIAGONAL ELEMENTS
!
                     z(j+2) = d(1)
                     z(j+3) = d(2)
                     j = j + nrow2
                     z(j) = d(1)
                     z(j+1) = d(2)
                  ENDIF
               ENDIF
            ENDDO
!
!     SAVE ERROR ELEMENT FROM TRIDIAGONAL MATRIX
!
            IF ( .NOT.dpmach ) THEN
               dmp1(1) = s(1)
               dmp1(2) = s(2)
            ELSE
               dmp1(1) = d(1)
               dmp1(2) = d(2)
            ENDIF
            IF ( qpr ) WRITE (nout,99005) (z(i),i=1,nrowsq)
99005       FORMAT (1H0,26HREDUCED TRIDIAGONAL MATRIX,/(1H ,6E16.8))
            CALL close(iscr(5),rew)
            IF ( .NOT.(decrem) ) THEN
!
!     DECREMENT THE REDUCED PROBLEM SIZE IF THE ERROR ELEMENT IS NULL
!
               IF ( dmp1(1)==0. .AND. dmp1(2)==0. ) THEN
                  mreduc = mreduc - 1
                  WRITE (nout,99006) uwm , mreduc
99006             FORMAT (A25,' 3154',//5X,'SIZE OF REDUCED PROBLEM DECREMENTED ','ONCE (NOW',I6,') DUE TO NULL ERROR ELEMENT.',//)
                  IF ( mreduc==0 ) THEN
                     WRITE (nout,99007) uwm
99007                FORMAT (A25,' 3155',//5X,'REDUCED PROBLEM HAS VANISHED. NO ','ROOTS FOUND.',//)
                     IF ( nzero>0 .AND. jreg==noreg ) nswp(2) = -1
                     spag_nextblock_1 = 5
                     CYCLE SPAG_DispatchLoop_1
                  ELSEIF ( dm(1)/=0. .OR. dm(2)/=0. ) THEN
                     decrem = .TRUE.
                     CYCLE
                  ELSE
!
!     NEW ERROR ELEMENT IS ALSO NULL. RESTORE ORIGINAL REDUCED SIZE.
!
                     mreduc = mreduc + 1
                     dmp1(1) = sngl(eps)
                     WRITE (nout,99008) uwm , mreduc , dmp1
99008                FORMAT (A25,' 3156',//5X,'SIZE OF REDUCED PROBLEM RESTORED TO',I8,' BECAUSE NEXT ERROR ELEMENT WAS ALSO NULL.',&
                           & /5X,'ERROR ELEMENT SET = ',2E16.8,//)
                  ENDIF
               ENDIF
            ENDIF
!
            CALL allmat(z(ia),z(il),z(ih),z(ihl),z(iv),z(im),z(inth),z(intq),nrow,nrow,inidum)
!
!     --------------- SPECIAL PRINT -------------------------
!
            IF ( qpr ) THEN
               WRITE (nout,99009)
99009          FORMAT (1H0,10X,15HALLMAT EXECUTED,/,1H0)
               j = ih - 1
               WRITE (nout,99025) (z(i),i=il,j)
               WRITE (nout,99026)
               DO i = 1 , nrow
                  l = ia + nrow2*(i-1)
                  k = l + nrow2 - 1
                  WRITE (nout,99028) (z(j),j=l,k)
!
!     CHECK NORMALITY
!
                  sumr = 0.
                  sumi = 0.
                  DO j = l , k , 2
                     jj = j + 1
                     sumr = sumr + z(j)**2 - z(jj)**2
                     sumi = sumi + 2.*z(j)*z(jj)
                  ENDDO
                  WRITE (nout,99027) sumr , sumi
               ENDDO
            ENDIF
!     -------------------------------------------------------
!
!     NORMALIZE THE EIGENVECTORS OUTPUT FROM ALLMAT
!
            IF ( qpr ) WRITE (nout,99026)
            DO i = 1 , nrow
               l = ia + nrow2*(i-1)
               k = l + nrow2 - 1
               sumr = 0.
               sumi = 0.
               DO j = l , k , 2
                  jj = j + 1
                  sumr = sumr + z(j)**2 - z(jj)**2
                  sumi = sumi + 2.*z(j)*z(jj)
               ENDDO
               rsqrt = sqrt(sqrt(sumr**2+sumi**2))
               IF ( rsqrt>0. ) THEN
                  theta2 = .5*atan2(sumi,sumr)
                  sumr = rsqrt*cos(theta2)
                  sumi = rsqrt*sin(theta2)
                  theta2 = 1./(sumr**2+sumi**2)
                  sumr = sumr*theta2
                  sumi = -sumi*theta2
                  DO j = l , k , 2
                     jj = j + 1
                     theta2 = z(j)
                     z(j) = sumr*z(j) - sumi*z(jj)
                     z(jj) = sumi*theta2 + sumr*z(jj)
                  ENDDO
!
!     -------------- SPECIAL PRINT --------------------------
!
                  IF ( qpr ) THEN
                     WRITE (nout,99028) (z(j),j=l,k)
!
!     CHECK NORMALITY
!
                     sumr = 0.
                     sumi = 0.
                     DO j = l , k , 2
                        jj = j + 1
                        sumr = sumr + z(j)**2 - z(jj)**2
                        sumi = sumi + 2.*z(j)*z(jj)
                     ENDDO
                     WRITE (nout,99027) sumr , sumi
                  ENDIF
               ELSE
                  WRITE (nout,99010) uwm , name
99010             FORMAT (A25,' 3153',//5X,'ATTEMPT TO NORMALIZE NULL VECTOR IN ','SUBROUTINE ',A4,A2,'. NO ACTION TAKEN.',//)
               ENDIF
!     -------------------------------------------------------
!
            ENDDO
!
!     COMPUTE THEORETICAL EIGENVALUE ERRORS
!
            IF ( qpr ) WRITE (nout,99011) dmp1
99011       FORMAT (1H0,//30H THEORETICAL EIGENVALUE ERRORS,20X,18HD-SUB-M-PLUS-ONE =,2E16.8,/)
            ihl1 = ihl - 1
            DO i = 1 , nrow
               k = il + 2*(i-1)
               denom = sqrt(z(k)**2+z(k+1)**2)
               IF ( denom<=0. ) THEN
                  WRITE (nout,99012) uim , i
99012             FORMAT (A29,' 3152',//5X,'SUBROUTINE ALLMAT OUTPUT EIGENVALUE',I4,' IS NULL.',//)
                  denom = 1.E-10
               ENDIF
               denom = 1./denom
               k = ia + nrow2*i - 2
               kk = k + 1
               j = ihl1 + i
               z(j) = denom*sqrt((dmp1(1)*z(k)-dmp1(2)*z(kk))**2+(dmp1(1)*z(kk)+dmp1(2)*z(k))**2)
               IF ( qpr ) WRITE (nout,99013) i , z(j) , z(k) , z(kk) , denom
99013          FORMAT (1H ,I5,E16.8,20X,2E16.8,10X,E16.8)
            ENDDO
!
!     RECOVER PHYSICAL EIGENVALUES
!
            rms = 0.
            IF ( nob ) THEN
               alam(1) = lambda(1)**2 - lambda(2)**2
               alam(2) = 2.D0*lambda(1)*lambda(2)
            ELSE
               alam(1) = lambda(1)
               alam(2) = lambda(2)
            ENDIF
            DO i = 1 , nrow
               k = il + 2*(i-1)
               kk = k + 1
               denom = z(k)**2 + z(kk)**2
               IF ( denom==0. ) denom = 1.E-20
               denom = 1./denom
               z(k) = denom*z(k) + alam(1)
               z(kk) = -denom*z(kk) + alam(2)
               IF ( nob ) THEN
!
!     DAMPING MATRIX ABSENT
!
                  rsqrt = sqrt(sqrt(z(k)**2+z(kk)**2))
                  theta2 = .5*atan2(z(kk),z(k))
                  z(k) = rsqrt*cos(theta2)
                  z(kk) = rsqrt*sin(theta2)
                  IF ( z(kk)<0. ) THEN
                     z(k) = -z(k)
                     z(kk) = -z(kk)
                  ENDIF
               ENDIF
!
!     COMPUTE RMS FOR RIGID-BODY ERROR TEST
!
               rms = rms + sqrt((z(k)**2-z(kk)**2)**2+4.*(z(k)*z(kk))**2)
            ENDDO
            rms = sqrt(rms)/float(nrow)
            IF ( qpr ) WRITE (nout,99014) rms
99014       FORMAT (1H ,10X,5HRMS =,E16.8)
            j = ih - 1
            IF ( qpr ) WRITE (nout,99025) (z(i),i=il,j)
!
!     PERFORM RIGID-BODY ERROR TEST
!
            IF ( rms<1.E-20 ) rms = 1.E-20
            rms = 1./rms
            DO i = 1 , nrow
               k = il + 2*(i-1)
               j = ihl1 + i
               IF ( rms*sqrt(z(k)**2+z(k+1)**2)<=tenmtt ) z(j) = 0.
            ENDDO
!
!     COMPUTE DISTANCES OF EIGENVALUES TO CENTER OF NEIGHBORHOOD
!
            alam(1) = lambda(1)
            alam(2) = lambda(2)
            jj = intq - 1
            kk = ih - 1
            ll = inth - 1
            DO i = 1 , nrow
               j = jj + i
               k = il + 2*(i-1)
               z(j) = sqrt((alam(1)-z(k))**2+(alam(2)-z(k+1))**2)
!
!     LOAD ORDER OF EXTRACTION
!
               k = kk + i
               iz(k) = i
!
!     LOAD STATUS OF EACH SOLUTION
!
               k = ll + i
               lz(k) = .FALSE.
               j = ihl1 + i
               IF ( z(j)<sngl(eps) ) lz(k) = .TRUE.
            ENDDO
!
!     SORT EIGENVALUES ACCORDING TO DISTANCE FROM CURRENT CENTER
!
            IF ( nrow/=1 ) THEN
               ll = nrow - 1
               DO i = 1 , ll
                  k = jj + i
                  i1 = kk + i
                  lll = i + 1
                  DO j = lll , nrow
                     l = jj + j
                     IF ( z(k)>=z(l) ) THEN
                        unidum = z(l)
                        z(l) = z(k)
                        z(k) = unidum
                        i2 = kk + j
                        inidum = iz(i1)
                        iz(i1) = iz(i2)
                        iz(i2) = inidum
                     ENDIF
                  ENDDO
               ENDDO
            ENDIF
            lll = il - 1
            ll = inth - 1
            IF ( idiag/=0 ) THEN
!
!     PRINT OUT FULL SUMMARY FOR CURRENT NEIGHBORHOOD
!
               WRITE (nout,99029) jreg , noreg , alam
               WRITE (nout,99015)
99015          FORMAT (4X,43HALL SOLUTIONS FOUND IN CURRENT NEIGHBORHOOD,12H ARE LISTED.,/)
               WRITE (nout,99030)
               DO i = 1 , nrow
                  k = kk + i
                  izz = 2*iz(k) - 1
                  j = jj + i
                  l = lll + izz
                  l1 = l + 1
                  i1 = ihl1 + iz(k)
                  z(i1) = 100.*z(i1)
                  i2 = ll + iz(k)
                  status(1) = accept(1)
                  status(2) = accept(2)
                  IF ( .NOT.(lz(i2)) ) THEN
                     status(1) = reject(1)
                     status(2) = reject(2)
                  ENDIF
                  WRITE (nout,99031) i , iz(k) , z(j) , z(l) , z(l1) , z(i1) , status
               ENDDO
            ENDIF
!
!     DECREMENT COUNTERS SO THAT ONLY ACCEPTABLE SOLUTIONS ARE RETAINED
!
            msave = nrow
            DO i = 1 , msave
               i2 = ll + i
               IF ( .NOT.(lz(i2)) ) THEN
                  nrow = nrow - 1
                  northo = northo - 1
                  IF ( nrow==0 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
            ENDDO
            nfound = nzero + nrow
            IF ( nrow==msave ) WRITE (nout,99016) uim , msave
99016       FORMAT (A29,' 3164',//5X,'ALL',I6,' SOLUTIONS ARE ACCEPTABLE.',//)
            IF ( idiag/=0 .AND. nrow/=msave ) THEN
!
!     PRINT OUT SUMMARY WITH REJECTED SOLUTIONS DELETED
!
               WRITE (nout,99029) jreg , noreg , alam
               WRITE (nout,99017)
99017          FORMAT (4X,37HREJECTED SOLUTIONS HAVE BEEN DELETED.,/)
               WRITE (nout,99030)
               m = 0
               DO i = 1 , msave
                  k = kk + i
                  i2 = ll + iz(k)
                  IF ( lz(i2) ) THEN
                     m = m + 1
                     izz = 2*iz(k) - 1
                     j = jj + i
                     l = lll + izz
                     l1 = l + 1
                     i1 = ihl1 + iz(k)
                     WRITE (nout,99031) m , iz(k) , z(j) , z(l) , z(l1) , z(i1) , accept
                  ENDIF
               ENDDO
            ENDIF
            m = msave - nrow
            IF ( m>0 ) WRITE (nout,99018) uim , nrow , m
99018       FORMAT (A29,' 3165',//4X,I6,' SOLUTIONS HAVE BEEN ACCEPTED AND',I4,' SOLUTIONS HAVE BEEN REJECTED.',//)
!
!     WRITE EIGENVALUES TO OUTPUT FILE
!
            CALL gopen(ilam(1),z(ibuf1),wrt)
            DO i = 1 , msave
               k = kk + i
               i2 = ll + iz(k)
               IF ( lz(i2) ) THEN
                  izz = 2*iz(k) - 1
                  l = lll + izz
                  lam1(1) = dble(z(l))
                  lam1(2) = dble(z(l+1))
                  CALL write(ilam(1),lam1(1),4,1)
               ENDIF
            ENDDO
            CALL close(ilam(1),eofnrw)
            IF ( jreg>=noreg .OR. nfound>=nord ) THEN
               IF ( nzero/=0 ) EXIT SPAG_Loop_1_1
            ENDIF
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDDO SPAG_Loop_1_1
!
!     IF THIS IS THE FINAL (BUT NOT THE FIRST) NEIGHBORHOOD, THEN
!     RE-WRITE THE EIGENVECTOR FILE PERTAINING TO ALL PRIOR
!     NEIGHBORHOODS (ELIMINATE LEFT-HAND VECTORS)
!
         IF ( idiag/=0 ) WRITE (nout,99019) nzero , northo
99019    FORMAT (1H ,33HLEFT-HAND EIGENVECTORS ELIMINATED,20X,2I8)
         inidum = iscr(10)
         CALL open(*80,iscr(10),z(ibuf2),wrtrew)
         CALL close(iscr(10),rew)
         j = nord2
         IF ( nob ) j = 2*j
         inidum = iphi(1)
         CALL open(*80,iphi(1),z(ibuf1),0)
         DO i = 1 , nzero
            CALL read(*100,*10,iphi(1),z(iv2),nord8+10,0,n3)
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
 10         CALL gopen(iscr(10),z(ibuf2),wrt)
            CALL write(iscr(10),z(iv2),j,1)
            CALL close(iscr(10),norew)
         ENDDO
         CALL close(iphi(1),norew)
         CALL open(*80,iphi(1),z(ibuf1),wrtrew)
         CALL close(iphi(1),rew)
         inidum = iscr(10)
         CALL open(*80,iscr(10),z(ibuf2),0)
         DO i = 1 , nzero
            CALL read(*100,*20,iscr(10),z(iv2),j+10,0,n3)
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
 20         CALL gopen(iphi(1),z(ibuf1),wrt)
            CALL write(iphi(1),z(iv2),j,1)
            CALL close(iphi(1),eofnrw)
         ENDDO
         CALL close(iscr(10),norew)
         IF ( nswp(2)<0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     RECOVER PHYSICAL EIGENVECTORS, PRINT, AND WRITE TO OUTPUT FILE
!
         iprc = iprec + 2
         ii = 1
         nn = nord2
         incr = 1
         ia1 = ia - 1
         IF ( qpr ) WRITE (nout,99020)
99020    FORMAT (1H1,27X,39H*****  F E E R  *****  (FAST EIGENVALUE,27H EXTRACTION ROUTINE)  *****,//42X,                           &
                &37HE I G E N V E C T O R   S U M M A R Y,//1H ,32(4H----),2H--)
         ishft = nord2*iprec
         i1 = 0
!
!     ENTER LOOP
!
         DO i = 1 , msave
            k = kk + i
            i2 = ll + iz(k)
            IF ( lz(i2) ) THEN
               CALL gopen(iscr(7),z(ibuf2),rdrew)
               IF ( nzero>0 ) CALL skprec(iscr(7),nzero)
               DO j = 1 , nord8
                  m = iv1x + j
                  z(m) = 0.
               ENDDO
!
!     SET POINTER TO ALLMAT OUTPUT VECTOR
!
               ib = ia1 + 2*msave*(iz(k)-1)
!
!     CYCLE THRU ALL ORTHOGONAL VECTORS
!
               DO j = 1 , msave
!
!     NOTE.... Z(IV2) MAY BE LOADED DOUBLE-PRECISION....HIGHER DIGITS
!              ARE NOT USED
!     (HIGHER DIGITS MUST BE INCLUDED FOR THE D.P.MACHINES.  G.C/UNISYS)
!
                  CALL unpack(*25,iscr(7),z(iv2))
                  kr = ib + 2*j - 1
                  ki = kr + 1
                  DO mm = 1 , nord2 , 2
                     mr = iv2 + (mm-1)*iprec
                     mi = mr + iprec
                     jr = iv1x + mm
                     ji = jr + 1
                     IF ( .NOT.dpmach ) THEN
                        z(jr) = z(jr) + z(mr)*z(kr) - z(mi)*z(ki)
                        z(ji) = z(ji) + z(mi)*z(kr) + z(mr)*z(ki)
                     ELSE
                        mrd = (mr+1)/2
                        mid = mrd + 1
!
!     RECOVER RIGHT-HAND PHYSICAL EIGENVECTOR
!
                        z(jr) = z(jr) + dz(mrd)*z(kr) - dz(mid)*z(ki)
                        z(ji) = z(ji) + dz(mid)*z(kr) + dz(mrd)*z(ki)
                     ENDIF
                     mr = mr + ishft
                     mi = mr + iprec
                     jr = jr + nord4
                     ji = jr + 1
                     IF ( .NOT.dpmach ) THEN
                        z(jr) = z(jr) + z(mr)*z(kr) - z(mi)*z(ki)
                        z(ji) = z(ji) + z(mi)*z(kr) + z(mr)*z(ki)
                     ELSE
                        mrd = (mr+1)/2
                        mid = mrd + 1
!
!     RECOVER LEFT-HAND PHYSICAL EIGENVECTOR
!
                        z(jr) = z(jr) + dz(mrd)*z(kr) - dz(mid)*z(ki)
                        z(ji) = z(ji) + dz(mid)*z(kr) + dz(mrd)*z(ki)
                     ENDIF
                  ENDDO
 25            ENDDO
               CALL close(iscr(7),eofnrw)
               IF ( qpr ) THEN
                  i1 = i1 + 1
                  izz = 2*iz(k) - 1
                  l = lll + izz
                  mm = iv1x + nord8
                  WRITE (nout,99021) i1 , iz(k) , z(l) , z(l+1) , (z(j),j=iv1,mm)
99021             FORMAT (1H ,8HSOLUTION,I4,8X,16HEXTRACTION ORDER,I4,10X,10HEIGENVALUE,2X,1P,2E16.8,/(1H ,3(4X,1P,2E16.8)))
                  WRITE (nout,99032)
               ENDIF
!
!     EXPAND PHYSICAL EIGENVECTORS TO DOUBLE PRECISION FOR OUTPUT
!
               lim1 = iv1 + nord2
               lim2 = lim1 + nord4
               inidum = iv1x + nord4
               DO j = 1 , nord2
                  ki = lim1 - j
                  mi = 2*ki - iv1x
                  mr = mi - 1
                  mrd = (mr+1)/2
!
!     EXPAND RIGHT-HAND VECTOR
!
                  z(mi) = 0.
                  z(mr) = z(ki)
                  IF ( dpmach ) dz(mrd) = z(ki)
                  ki = lim2 - j
                  mi = 2*ki - inidum
                  mr = mi - 1
                  mrd = (mr+1)/2
!
!     EXPAND LEFT -HAND VECTOR
!
                  z(mi) = 0.
                  z(mr) = z(ki)
                  IF ( dpmach ) dz(mrd) = z(ki)
               ENDDO
               IF ( qpr ) THEN
                  WRITE (nout,99032)
                  lim1 = iv1x + nord4
                  WRITE (nout,99033) (z(j),j=iv1,lim1)
                  WRITE (nout,99032)
                  lim2 = lim1 + nord4
                  lim1 = lim1 + 1
                  WRITE (nout,99033) (z(j),j=lim1,lim2)
                  WRITE (nout,99032)
               ENDIF
!
!     PERFORM SPECIAL NORMALIZATION OF VECTORS FOR OUTPUT
!
               CALL cnorm1(z(iv1),ikmb(2))
               IF ( qpr ) WRITE (nout,99034)
               inidum = inidum + 1
               CALL cnorm1(z(inidum),ikmb(2))
               IF ( qpr ) WRITE (nout,99034)
               CALL gopen(iphi(1),z(ibuf1),wrt)
               IF ( jreg<noreg .AND. nfound<nord ) THEN
!
!     MUST USE NORD8 TO WRITE FULL RIGHT AND LEFT EIGENVECTORS
!
                  CALL write(iphi(1),z(iv1),nord8,1)
                  CALL close(iphi(1),norew)
               ELSE
                  j = nord2
                  IF ( nob ) j = 2*j
                  CALL write(iphi(1),z(iv1),j,1)
                  CALL close(iphi(1),eofnrw)
               ENDIF
            ENDIF
         ENDDO
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 40      WRITE (nout,99022) name
!
99022    FORMAT (27H UNEXPECTED EOF ENCOUNTERED,2X,2A4)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 60      WRITE (nout,99023) m , name
99023    FORMAT (22H UNEXPECTED WORD COUNT,I5,2X,2A4)
         spag_nextblock_1 = 5
      CASE (3)
         WRITE (nout,99024) uwm , msave
99024    FORMAT (A25,' 3163',//5X,'ALL',I6,' SOLUTIONS HAVE FAILED ','ACCURACY TEST. NO ROOTS FOUND.',//)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 80      CALL mesage(-1,inidum,name)
 100     CALL mesage(-2,inidum,name)
         spag_nextblock_1 = 4
      CASE (4)
         CALL mesage(-8,inidum,name)
         spag_nextblock_1 = 5
      CASE (5)
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99025 FORMAT (1H0,11HEIGENVALUES,//,(1H ,2E16.8))
99026 FORMAT (1H0,12HEIGENVECTORS,//)
99027 FORMAT (//,35H SELF INNER-PRODUCT OF ABOVE VECTOR,/,1H ,6X,11HREAL PART =,E16.8,8X,16HIMAGINARY PART =,E16.8)
99028 FORMAT (//,(1H ,6E16.8))
99029 FORMAT (1H1,27X,39H*****  F E E R  *****  (FAST EIGENVALUE,27H EXTRACTION ROUTINE)  *****,//4X,24HSUMMARY FOR NEIGHBORHOOD,I3,&
             &3H OF,I3,1H.,10X,21HNEIGHBORHOOD CENTER =,2E16.8,/)
99030 FORMAT (4X,7X,8HSOLUTION,7X,8HORDER OF,7X,8HDISTANCE,10X,10HEIGENVALUE,14X,11HTHEORETICAL,/4X,9X,6HNUMBER,5X,10HEXTRACTION,4X,&
             &11HFROM CENTER,6X,4HREAL,9X,9HIMAGINARY,9X,5HERROR,12X,6HSTATUS,/)
99031 FORMAT (4X,I12,I15,1P,E18.8,1P,3E15.7,7X,2A4)
99032 FORMAT (3H --,32(4H----))
99033 FORMAT ((1H ,3(3X,2E16.8)))
99034 FORMAT (1H ,12HAFTER CNORM1)
END SUBROUTINE cfeer4
