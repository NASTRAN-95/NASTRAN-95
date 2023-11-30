
SUBROUTINE cfeer4
   IMPLICIT NONE
   REAL Dumaa(84) , Eofnrw , Rd , Rdrew , Rew , Swdum , Ten2mt , Tenmht , Tenmtt , Wrt , Z(1)
   DOUBLE PRECISION Dz(1) , Eps , Lambda(2)
   INTEGER Idiag , Idmpfl , Ii , Ikmb(21) , Ilam(7) , Incr , Iphi(7) , Iprc , Iprec , Iscr(11) , It , Iz(1) , Jreg , Ksystm(65) ,   &
         & Mcbvec(7) , Minopn , Mreduc , Nn , Nord , Nord2 , Nord4 , Nordp1 , Noreg , Norew , Northo , Nout , Nrow , Nstart ,       &
         & Nswp(2) , Nzero , Wrtrew
   LOGICAL Lz(1) , Nob , Qpr
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /feeraa/ Ikmb , Ilam , Iphi , Idmpfl , Iscr , Dumaa , Mcbvec
   COMMON /feerxc/ Lambda , Swdum , Mreduc , Nord , Idiag , Eps , Northo , Nord2 , Nord4 , Nordp1 , Nswp , Nob , It , Ten2mt ,      &
                 & Tenmht , Nstart , Qpr , Jreg , Noreg , Nzero , Tenmtt , Minopn
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw
   COMMON /system/ Ksystm
   COMMON /unpakx/ Iprc , Ii , Nn , Incr
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zzzzzz/ Z
   REAL accept(2) , alam(2) , denom , dm(2) , dmp1(2) , reject(2) , rms , rsqrt , s(8) , status(2) , sumi , sumr , theta2 , unidum
   DOUBLE PRECISION d(4) , lam1(2)
   LOGICAL decrem , dpmach
   INTEGER eor , i , i1 , i2 , ia , ia1 , ib , ibuf1 , ibuf2 , ih , ihl , ihl1 , il , im , inidum , inth , intq , iopn , ishft ,    &
         & iv , iv1 , iv1x , iv2 , izz , j , ji , jj , jr , k , ki , kk , kr , l , l1 , lim1 , lim2 , ll , lll , m , mi , mid , mm ,&
         & mr , mrd , msave , n3 , name(2) , nfound , nord8 , nrow1 , nrow2 , nrow22 , nrowsq , nw , nz
   INTEGER korsz
!
!     CFEER4 OBTAINS THE EIGENVALUES AND EIGENVECTORS FROM THE
!     REDUCED TRIDIAGONAL MATRIX FOR THE COMPLEX FEER METHOD
!
   EQUIVALENCE (Ksystm(2),Nout) , (Nrow,Mreduc) , (Ksystm(55),Iprec) , (d(1),s(1)) , (Z(1),Iz(1),Lz(1),Dz(1))
   DATA name/4HCFEE , 4HR4  /
   DATA accept , reject/4H  AC , 4HCEPT , 4H -RE , 4HJECT/
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
   IF ( Qpr ) WRITE (Nout,99001)
99001 FORMAT (1H0,//7H CFEER4,//)
   dpmach = Iprec==2
   nord8 = 2*Nord4
   decrem = .FALSE.
   DO
      nrow2 = 2*Nrow
      nrowsq = Nrow*nrow2
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
      intq = inth + Nrow
!
!     ALLOCATE CORE FOR PHYSICAL EIGENVECTORS (LEFT FOLLOWS RIGHT)
!
      iv1 = intq + Nrow
      iv2 = iv1 + nord8
      IF ( dpmach .AND. mod(iv2,2)==0 ) iv2 = iv2 + 1
      iv1x = iv1 - 1
!
!     TEST FOR INSUFFICIENT CORE
!
      nz = korsz(Z(1))
      ibuf1 = nz - Ksystm(1)
      ibuf2 = ibuf1 - Ksystm(1)
      iopn = ibuf2 - (iv2+nord8)
      IF ( Idiag/=0 ) WRITE (Nout,99002) iopn
99002 FORMAT (1H ,I10,36H SINGLE PRECISION WORDS OF OPEN CORE,29H NOT USED (SUBROUTINE CFEER4))
      IF ( iopn<=0 ) CALL mesage(-8,0,name(1))
      IF ( iopn<Minopn ) Minopn = iopn
      IF ( Nswp(2)<0 ) EXIT
!
!     CONSTRUCT REDUCED TRIDIAGONAL MATRIX
!
      DO i = ia , il
         Z(i) = 0.
      ENDDO
      nrow22 = nrow2 + 2
      CALL gopen(Iscr(5),Z(ibuf1),Rdrew)
      nw = 4*Iprec
      eor = 1
      m = 0
      nrow1 = Nrow - 1
!
!     ENTER LOOP
!
      DO i = 1 , Nrow
         i1 = i - 1
         CALL read(*300,*400,Iscr(5),s(1),nw,eor,m)
         IF ( Qpr .AND. .NOT.dpmach ) WRITE (Nout,99003) i , (s(j),j=1,4)
99003    FORMAT (4H ROW,I5,2(4X,2E16.8))
         IF ( Qpr .AND. dpmach ) WRITE (Nout,99004) i , (d(j),j=1,4)
99004    FORMAT (4H ROW,I5,2(4X,2D16.8))
!
!     ALLMAT ACCEPTS ONLY SINGLE PRECISION ARRAY
!
         j = ia + nrow22*i1
         IF ( .NOT.dpmach ) THEN
!
!     LOAD MAIN DIAGONAL ELEMENT
!
            Z(j) = s(3)
            Z(j+1) = s(4)
            IF ( i==nrow1 ) THEN
!
!     SAVE LAST OFF-DIAGONAL ELEMENT
!
               dm(1) = s(1)
               dm(2) = s(2)
            ENDIF
            IF ( i/=Nrow ) THEN
!
!     LOAD OFF-DIAGONAL ELEMENTS
!
               Z(j+2) = s(1)
               Z(j+3) = s(2)
               j = j + nrow2
               Z(j) = s(1)
               Z(j+1) = s(2)
            ENDIF
         ELSE
!
!     LOAD MAIN DIAGONAL ELEMENT
!
            Z(j) = d(3)
            Z(j+1) = d(4)
            IF ( i==nrow1 ) THEN
!
!     SAVE LAST OFF-DIAGONAL ELEMENT
!
               dm(1) = d(1)
               dm(2) = d(2)
            ENDIF
            IF ( i/=Nrow ) THEN
!
!     LOAD OFF-DIAGONAL ELEMENTS
!
               Z(j+2) = d(1)
               Z(j+3) = d(2)
               j = j + nrow2
               Z(j) = d(1)
               Z(j+1) = d(2)
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
      IF ( Qpr ) WRITE (Nout,99005) (Z(i),i=1,nrowsq)
99005 FORMAT (1H0,26HREDUCED TRIDIAGONAL MATRIX,/(1H ,6E16.8))
      CALL close(Iscr(5),Rew)
      IF ( .NOT.(decrem) ) THEN
!
!     DECREMENT THE REDUCED PROBLEM SIZE IF THE ERROR ELEMENT IS NULL
!
         IF ( dmp1(1)==0. .AND. dmp1(2)==0. ) THEN
            Mreduc = Mreduc - 1
            WRITE (Nout,99006) Uwm , Mreduc
99006       FORMAT (A25,' 3154',//5X,'SIZE OF REDUCED PROBLEM DECREMENTED ','ONCE (NOW',I6,') DUE TO NULL ERROR ELEMENT.',//)
            IF ( Mreduc==0 ) THEN
               WRITE (Nout,99007) Uwm
99007          FORMAT (A25,' 3155',//5X,'REDUCED PROBLEM HAS VANISHED. NO ','ROOTS FOUND.',//)
               IF ( Nzero>0 .AND. Jreg==Noreg ) Nswp(2) = -1
               GOTO 900
            ELSEIF ( dm(1)/=0. .OR. dm(2)/=0. ) THEN
               decrem = .TRUE.
               CYCLE
            ELSE
!
!     NEW ERROR ELEMENT IS ALSO NULL. RESTORE ORIGINAL REDUCED SIZE.
!
               Mreduc = Mreduc + 1
               dmp1(1) = sngl(Eps)
               WRITE (Nout,99008) Uwm , Mreduc , dmp1
99008          FORMAT (A25,' 3156',//5X,'SIZE OF REDUCED PROBLEM RESTORED TO',I8,' BECAUSE NEXT ERROR ELEMENT WAS ALSO NULL.',/5X,  &
                      &'ERROR ELEMENT SET = ',2E16.8,//)
            ENDIF
         ENDIF
      ENDIF
!
      CALL allmat(Z(ia),Z(il),Z(ih),Z(ihl),Z(iv),Z(im),Z(inth),Z(intq),Nrow,Nrow,inidum)
!
!     --------------- SPECIAL PRINT -------------------------
!
      IF ( Qpr ) THEN
         WRITE (Nout,99009)
99009    FORMAT (1H0,10X,15HALLMAT EXECUTED,/,1H0)
         j = ih - 1
         WRITE (Nout,99025) (Z(i),i=il,j)
         WRITE (Nout,99026)
         DO i = 1 , Nrow
            l = ia + nrow2*(i-1)
            k = l + nrow2 - 1
            WRITE (Nout,99028) (Z(j),j=l,k)
!
!     CHECK NORMALITY
!
            sumr = 0.
            sumi = 0.
            DO j = l , k , 2
               jj = j + 1
               sumr = sumr + Z(j)**2 - Z(jj)**2
               sumi = sumi + 2.*Z(j)*Z(jj)
            ENDDO
            WRITE (Nout,99027) sumr , sumi
         ENDDO
      ENDIF
!     -------------------------------------------------------
!
!     NORMALIZE THE EIGENVECTORS OUTPUT FROM ALLMAT
!
      IF ( Qpr ) WRITE (Nout,99026)
      DO i = 1 , Nrow
         l = ia + nrow2*(i-1)
         k = l + nrow2 - 1
         sumr = 0.
         sumi = 0.
         DO j = l , k , 2
            jj = j + 1
            sumr = sumr + Z(j)**2 - Z(jj)**2
            sumi = sumi + 2.*Z(j)*Z(jj)
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
               theta2 = Z(j)
               Z(j) = sumr*Z(j) - sumi*Z(jj)
               Z(jj) = sumi*theta2 + sumr*Z(jj)
            ENDDO
!
!     -------------- SPECIAL PRINT --------------------------
!
            IF ( Qpr ) THEN
               WRITE (Nout,99028) (Z(j),j=l,k)
!
!     CHECK NORMALITY
!
               sumr = 0.
               sumi = 0.
               DO j = l , k , 2
                  jj = j + 1
                  sumr = sumr + Z(j)**2 - Z(jj)**2
                  sumi = sumi + 2.*Z(j)*Z(jj)
               ENDDO
               WRITE (Nout,99027) sumr , sumi
            ENDIF
         ELSE
            WRITE (Nout,99010) Uwm , name
99010       FORMAT (A25,' 3153',//5X,'ATTEMPT TO NORMALIZE NULL VECTOR IN ','SUBROUTINE ',A4,A2,'. NO ACTION TAKEN.',//)
         ENDIF
!     -------------------------------------------------------
!
      ENDDO
!
!     COMPUTE THEORETICAL EIGENVALUE ERRORS
!
      IF ( Qpr ) WRITE (Nout,99011) dmp1
99011 FORMAT (1H0,//30H THEORETICAL EIGENVALUE ERRORS,20X,18HD-SUB-M-PLUS-ONE =,2E16.8,/)
      ihl1 = ihl - 1
      DO i = 1 , Nrow
         k = il + 2*(i-1)
         denom = sqrt(Z(k)**2+Z(k+1)**2)
         IF ( denom<=0. ) THEN
            WRITE (Nout,99012) Uim , i
99012       FORMAT (A29,' 3152',//5X,'SUBROUTINE ALLMAT OUTPUT EIGENVALUE',I4,' IS NULL.',//)
            denom = 1.E-10
         ENDIF
         denom = 1./denom
         k = ia + nrow2*i - 2
         kk = k + 1
         j = ihl1 + i
         Z(j) = denom*sqrt((dmp1(1)*Z(k)-dmp1(2)*Z(kk))**2+(dmp1(1)*Z(kk)+dmp1(2)*Z(k))**2)
         IF ( Qpr ) WRITE (Nout,99013) i , Z(j) , Z(k) , Z(kk) , denom
99013    FORMAT (1H ,I5,E16.8,20X,2E16.8,10X,E16.8)
      ENDDO
!
!     RECOVER PHYSICAL EIGENVALUES
!
      rms = 0.
      IF ( Nob ) THEN
         alam(1) = Lambda(1)**2 - Lambda(2)**2
         alam(2) = 2.D0*Lambda(1)*Lambda(2)
      ELSE
         alam(1) = Lambda(1)
         alam(2) = Lambda(2)
      ENDIF
      DO i = 1 , Nrow
         k = il + 2*(i-1)
         kk = k + 1
         denom = Z(k)**2 + Z(kk)**2
         IF ( denom==0. ) denom = 1.E-20
         denom = 1./denom
         Z(k) = denom*Z(k) + alam(1)
         Z(kk) = -denom*Z(kk) + alam(2)
         IF ( Nob ) THEN
!
!     DAMPING MATRIX ABSENT
!
            rsqrt = sqrt(sqrt(Z(k)**2+Z(kk)**2))
            theta2 = .5*atan2(Z(kk),Z(k))
            Z(k) = rsqrt*cos(theta2)
            Z(kk) = rsqrt*sin(theta2)
            IF ( Z(kk)<0. ) THEN
               Z(k) = -Z(k)
               Z(kk) = -Z(kk)
            ENDIF
         ENDIF
!
!     COMPUTE RMS FOR RIGID-BODY ERROR TEST
!
         rms = rms + sqrt((Z(k)**2-Z(kk)**2)**2+4.*(Z(k)*Z(kk))**2)
      ENDDO
      rms = sqrt(rms)/float(Nrow)
      IF ( Qpr ) WRITE (Nout,99014) rms
99014 FORMAT (1H ,10X,5HRMS =,E16.8)
      j = ih - 1
      IF ( Qpr ) WRITE (Nout,99025) (Z(i),i=il,j)
!
!     PERFORM RIGID-BODY ERROR TEST
!
      IF ( rms<1.E-20 ) rms = 1.E-20
      rms = 1./rms
      DO i = 1 , Nrow
         k = il + 2*(i-1)
         j = ihl1 + i
         IF ( rms*sqrt(Z(k)**2+Z(k+1)**2)<=Tenmtt ) Z(j) = 0.
      ENDDO
!
!     COMPUTE DISTANCES OF EIGENVALUES TO CENTER OF NEIGHBORHOOD
!
      alam(1) = Lambda(1)
      alam(2) = Lambda(2)
      jj = intq - 1
      kk = ih - 1
      ll = inth - 1
      DO i = 1 , Nrow
         j = jj + i
         k = il + 2*(i-1)
         Z(j) = sqrt((alam(1)-Z(k))**2+(alam(2)-Z(k+1))**2)
!
!     LOAD ORDER OF EXTRACTION
!
         k = kk + i
         Iz(k) = i
!
!     LOAD STATUS OF EACH SOLUTION
!
         k = ll + i
         Lz(k) = .FALSE.
         j = ihl1 + i
         IF ( Z(j)<sngl(Eps) ) Lz(k) = .TRUE.
      ENDDO
!
!     SORT EIGENVALUES ACCORDING TO DISTANCE FROM CURRENT CENTER
!
      IF ( Nrow/=1 ) THEN
         ll = Nrow - 1
         DO i = 1 , ll
            k = jj + i
            i1 = kk + i
            lll = i + 1
            DO j = lll , Nrow
               l = jj + j
               IF ( Z(k)>=Z(l) ) THEN
                  unidum = Z(l)
                  Z(l) = Z(k)
                  Z(k) = unidum
                  i2 = kk + j
                  inidum = Iz(i1)
                  Iz(i1) = Iz(i2)
                  Iz(i2) = inidum
               ENDIF
            ENDDO
         ENDDO
      ENDIF
      lll = il - 1
      ll = inth - 1
      IF ( Idiag/=0 ) THEN
!
!     PRINT OUT FULL SUMMARY FOR CURRENT NEIGHBORHOOD
!
         WRITE (Nout,99029) Jreg , Noreg , alam
         WRITE (Nout,99015)
99015    FORMAT (4X,43HALL SOLUTIONS FOUND IN CURRENT NEIGHBORHOOD,12H ARE LISTED.,/)
         WRITE (Nout,99030)
         DO i = 1 , Nrow
            k = kk + i
            izz = 2*Iz(k) - 1
            j = jj + i
            l = lll + izz
            l1 = l + 1
            i1 = ihl1 + Iz(k)
            Z(i1) = 100.*Z(i1)
            i2 = ll + Iz(k)
            status(1) = accept(1)
            status(2) = accept(2)
            IF ( .NOT.(Lz(i2)) ) THEN
               status(1) = reject(1)
               status(2) = reject(2)
            ENDIF
            WRITE (Nout,99031) i , Iz(k) , Z(j) , Z(l) , Z(l1) , Z(i1) , status
         ENDDO
      ENDIF
!
!     DECREMENT COUNTERS SO THAT ONLY ACCEPTABLE SOLUTIONS ARE RETAINED
!
      msave = Nrow
      DO i = 1 , msave
         i2 = ll + i
         IF ( .NOT.(Lz(i2)) ) THEN
            Nrow = Nrow - 1
            Northo = Northo - 1
            IF ( Nrow==0 ) GOTO 500
         ENDIF
      ENDDO
      nfound = Nzero + Nrow
      IF ( Nrow==msave ) WRITE (Nout,99016) Uim , msave
99016 FORMAT (A29,' 3164',//5X,'ALL',I6,' SOLUTIONS ARE ACCEPTABLE.',//)
      IF ( Idiag/=0 .AND. Nrow/=msave ) THEN
!
!     PRINT OUT SUMMARY WITH REJECTED SOLUTIONS DELETED
!
         WRITE (Nout,99029) Jreg , Noreg , alam
         WRITE (Nout,99017)
99017    FORMAT (4X,37HREJECTED SOLUTIONS HAVE BEEN DELETED.,/)
         WRITE (Nout,99030)
         m = 0
         DO i = 1 , msave
            k = kk + i
            i2 = ll + Iz(k)
            IF ( Lz(i2) ) THEN
               m = m + 1
               izz = 2*Iz(k) - 1
               j = jj + i
               l = lll + izz
               l1 = l + 1
               i1 = ihl1 + Iz(k)
               WRITE (Nout,99031) m , Iz(k) , Z(j) , Z(l) , Z(l1) , Z(i1) , accept
            ENDIF
         ENDDO
      ENDIF
      m = msave - Nrow
      IF ( m>0 ) WRITE (Nout,99018) Uim , Nrow , m
99018 FORMAT (A29,' 3165',//4X,I6,' SOLUTIONS HAVE BEEN ACCEPTED AND',I4,' SOLUTIONS HAVE BEEN REJECTED.',//)
!
!     WRITE EIGENVALUES TO OUTPUT FILE
!
      CALL gopen(Ilam(1),Z(ibuf1),Wrt)
      DO i = 1 , msave
         k = kk + i
         i2 = ll + Iz(k)
         IF ( Lz(i2) ) THEN
            izz = 2*Iz(k) - 1
            l = lll + izz
            lam1(1) = dble(Z(l))
            lam1(2) = dble(Z(l+1))
            CALL write(Ilam(1),lam1(1),4,1)
         ENDIF
      ENDDO
      CALL close(Ilam(1),Eofnrw)
      IF ( Jreg<Noreg .AND. nfound<Nord ) GOTO 200
      IF ( Nzero/=0 ) EXIT
      GOTO 200
   ENDDO
!
!     IF THIS IS THE FINAL (BUT NOT THE FIRST) NEIGHBORHOOD, THEN
!     RE-WRITE THE EIGENVECTOR FILE PERTAINING TO ALL PRIOR
!     NEIGHBORHOODS (ELIMINATE LEFT-HAND VECTORS)
!
   IF ( Idiag/=0 ) WRITE (Nout,99019) Nzero , Northo
99019 FORMAT (1H ,33HLEFT-HAND EIGENVECTORS ELIMINATED,20X,2I8)
   inidum = Iscr(10)
   CALL open(*600,Iscr(10),Z(ibuf2),Wrtrew)
   CALL close(Iscr(10),Rew)
   j = Nord2
   IF ( Nob ) j = 2*j
   inidum = Iphi(1)
   CALL open(*600,Iphi(1),Z(ibuf1),0)
   DO i = 1 , Nzero
      CALL read(*700,*50,Iphi(1),Z(iv2),nord8+10,0,n3)
      GOTO 800
 50   CALL gopen(Iscr(10),Z(ibuf2),Wrt)
      CALL write(Iscr(10),Z(iv2),j,1)
      CALL close(Iscr(10),Norew)
   ENDDO
   CALL close(Iphi(1),Norew)
   CALL open(*600,Iphi(1),Z(ibuf1),Wrtrew)
   CALL close(Iphi(1),Rew)
   inidum = Iscr(10)
   CALL open(*600,Iscr(10),Z(ibuf2),0)
   DO i = 1 , Nzero
      CALL read(*700,*100,Iscr(10),Z(iv2),j+10,0,n3)
      GOTO 800
 100  CALL gopen(Iphi(1),Z(ibuf1),Wrt)
      CALL write(Iphi(1),Z(iv2),j,1)
      CALL close(Iphi(1),Eofnrw)
   ENDDO
   CALL close(Iscr(10),Norew)
   IF ( Nswp(2)<0 ) GOTO 900
!
!     RECOVER PHYSICAL EIGENVECTORS, PRINT, AND WRITE TO OUTPUT FILE
!
 200  Iprc = Iprec + 2
   Ii = 1
   Nn = Nord2
   Incr = 1
   ia1 = ia - 1
   IF ( Qpr ) WRITE (Nout,99020)
99020 FORMAT (1H1,27X,39H*****  F E E R  *****  (FAST EIGENVALUE,27H EXTRACTION ROUTINE)  *****,//42X,                              &
             &37HE I G E N V E C T O R   S U M M A R Y,//1H ,32(4H----),2H--)
   ishft = Nord2*Iprec
   i1 = 0
!
!     ENTER LOOP
!
   DO i = 1 , msave
      k = kk + i
      i2 = ll + Iz(k)
      IF ( Lz(i2) ) THEN
         CALL gopen(Iscr(7),Z(ibuf2),Rdrew)
         IF ( Nzero>0 ) CALL skprec(Iscr(7),Nzero)
         DO j = 1 , nord8
            m = iv1x + j
            Z(m) = 0.
         ENDDO
!
!     SET POINTER TO ALLMAT OUTPUT VECTOR
!
         ib = ia1 + 2*msave*(Iz(k)-1)
!
!     CYCLE THRU ALL ORTHOGONAL VECTORS
!
         DO j = 1 , msave
!
!     NOTE.... Z(IV2) MAY BE LOADED DOUBLE-PRECISION....HIGHER DIGITS
!              ARE NOT USED
!     (HIGHER DIGITS MUST BE INCLUDED FOR THE D.P.MACHINES.  G.C/UNISYS)
!
            CALL unpack(*220,Iscr(7),Z(iv2))
            kr = ib + 2*j - 1
            ki = kr + 1
            DO mm = 1 , Nord2 , 2
               mr = iv2 + (mm-1)*Iprec
               mi = mr + Iprec
               jr = iv1x + mm
               ji = jr + 1
               IF ( .NOT.dpmach ) THEN
                  Z(jr) = Z(jr) + Z(mr)*Z(kr) - Z(mi)*Z(ki)
                  Z(ji) = Z(ji) + Z(mi)*Z(kr) + Z(mr)*Z(ki)
               ELSE
                  mrd = (mr+1)/2
                  mid = mrd + 1
!
!     RECOVER RIGHT-HAND PHYSICAL EIGENVECTOR
!
                  Z(jr) = Z(jr) + Dz(mrd)*Z(kr) - Dz(mid)*Z(ki)
                  Z(ji) = Z(ji) + Dz(mid)*Z(kr) + Dz(mrd)*Z(ki)
               ENDIF
               mr = mr + ishft
               mi = mr + Iprec
               jr = jr + Nord4
               ji = jr + 1
               IF ( .NOT.dpmach ) THEN
                  Z(jr) = Z(jr) + Z(mr)*Z(kr) - Z(mi)*Z(ki)
                  Z(ji) = Z(ji) + Z(mi)*Z(kr) + Z(mr)*Z(ki)
               ELSE
                  mrd = (mr+1)/2
                  mid = mrd + 1
!
!     RECOVER LEFT-HAND PHYSICAL EIGENVECTOR
!
                  Z(jr) = Z(jr) + Dz(mrd)*Z(kr) - Dz(mid)*Z(ki)
                  Z(ji) = Z(ji) + Dz(mid)*Z(kr) + Dz(mrd)*Z(ki)
               ENDIF
            ENDDO
 220     ENDDO
         CALL close(Iscr(7),Eofnrw)
         IF ( Qpr ) THEN
            i1 = i1 + 1
            izz = 2*Iz(k) - 1
            l = lll + izz
            mm = iv1x + nord8
            WRITE (Nout,99021) i1 , Iz(k) , Z(l) , Z(l+1) , (Z(j),j=iv1,mm)
99021       FORMAT (1H ,8HSOLUTION,I4,8X,16HEXTRACTION ORDER,I4,10X,10HEIGENVALUE,2X,1P,2E16.8,/(1H ,3(4X,1P,2E16.8)))
            WRITE (Nout,99032)
         ENDIF
!
!     EXPAND PHYSICAL EIGENVECTORS TO DOUBLE PRECISION FOR OUTPUT
!
         lim1 = iv1 + Nord2
         lim2 = lim1 + Nord4
         inidum = iv1x + Nord4
         DO j = 1 , Nord2
            ki = lim1 - j
            mi = 2*ki - iv1x
            mr = mi - 1
            mrd = (mr+1)/2
!
!     EXPAND RIGHT-HAND VECTOR
!
            Z(mi) = 0.
            Z(mr) = Z(ki)
            IF ( dpmach ) Dz(mrd) = Z(ki)
            ki = lim2 - j
            mi = 2*ki - inidum
            mr = mi - 1
            mrd = (mr+1)/2
!
!     EXPAND LEFT -HAND VECTOR
!
            Z(mi) = 0.
            Z(mr) = Z(ki)
            IF ( dpmach ) Dz(mrd) = Z(ki)
         ENDDO
         IF ( Qpr ) THEN
            WRITE (Nout,99032)
            lim1 = iv1x + Nord4
            WRITE (Nout,99033) (Z(j),j=iv1,lim1)
            WRITE (Nout,99032)
            lim2 = lim1 + Nord4
            lim1 = lim1 + 1
            WRITE (Nout,99033) (Z(j),j=lim1,lim2)
            WRITE (Nout,99032)
         ENDIF
!
!     PERFORM SPECIAL NORMALIZATION OF VECTORS FOR OUTPUT
!
         CALL cnorm1(Z(iv1),Ikmb(2))
         IF ( Qpr ) WRITE (Nout,99034)
         inidum = inidum + 1
         CALL cnorm1(Z(inidum),Ikmb(2))
         IF ( Qpr ) WRITE (Nout,99034)
         CALL gopen(Iphi(1),Z(ibuf1),Wrt)
         IF ( Jreg<Noreg .AND. nfound<Nord ) THEN
!
!     MUST USE NORD8 TO WRITE FULL RIGHT AND LEFT EIGENVECTORS
!
            CALL write(Iphi(1),Z(iv1),nord8,1)
            CALL close(Iphi(1),Norew)
         ELSE
            j = Nord2
            IF ( Nob ) j = 2*j
            CALL write(Iphi(1),Z(iv1),j,1)
            CALL close(Iphi(1),Eofnrw)
         ENDIF
      ENDIF
   ENDDO
   GOTO 900
 300  WRITE (Nout,99022) name
!
99022 FORMAT (27H UNEXPECTED EOF ENCOUNTERED,2X,2A4)
   GOTO 900
 400  WRITE (Nout,99023) m , name
99023 FORMAT (22H UNEXPECTED WORD COUNT,I5,2X,2A4)
   GOTO 900
 500  WRITE (Nout,99024) Uwm , msave
99024 FORMAT (A25,' 3163',//5X,'ALL',I6,' SOLUTIONS HAVE FAILED ','ACCURACY TEST. NO ROOTS FOUND.',//)
   GOTO 900
 600  CALL mesage(-1,inidum,name)
 700  CALL mesage(-2,inidum,name)
 800  CALL mesage(-8,inidum,name)
 900  RETURN
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
