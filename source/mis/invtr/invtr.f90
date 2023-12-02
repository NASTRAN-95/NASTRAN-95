!*==invtr.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE invtr(*,X,Dx)
   IMPLICIT NONE
   USE c_invtrx
   USE c_names
   USE c_packx
   USE c_system
   USE c_type
   USE c_unpakx
   USE c_zntpkx
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: X
   REAL*8 , DIMENSION(1) :: Dx
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a1 , temp
   INTEGER :: bakskp , cmplx , core , forma , forskp , i , i1 , in1 , in2 , inbuf , incr , iobuf , j , jj , k , kk , l , l1 , lend ,&
            & ll , m , no , nrow , outbuf , tra , tra1 , tra2 , typea , typear , typeb
   REAL*8 , DIMENSION(2) :: da
   REAL*8 :: dtemp
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , DIMENSION(7) , SAVE :: t
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
!     INVTR WILL INVERT A LOWER OR UPPER TRIANGULAR MATRIX
!
!
!     FILEA    =  MATRIX CONTROL BLOCK FOR THE INPUT FILE A
!     FILEB    =  MATRIX CONTROL BLOCK FOR THE OUTPUT MATRIX B
!     SCRFIL   =  SCRATCH FILE (NEEDED ONLY FOR AN UPPER TRIANGLE)
!     NX       =  NUMBER OF CELLS OF CORE AVAILABLE AT X
!     PREC     =  DESIRED PRECISION OF ARITHMETIC OPERATIONS
!     X        =  BLOCK OF AVAILABLE CORE
!     DX       =  SAME BLOCK AS X, BUT TYPED DOUBLE PRECISION
!
!     COMMON /DESCRP/ LENGTH,MAJOR(1)
   !>>>>EQUIVALENCE (A(1),Da(1)) , (Filea(3),Nrow) , (Filea(4),Forma) , (Filea(5),Typea) , (Fileb(5),Typeb)
   DATA name/4HINVT , 4HR   / , t/7*0/
!
!     INITIALIZE
!
   typear = rc(typea)
   typear = rc(typear) + prec - 1
   incr = nwds(typear)
   it1 = typear
   it2 = typeb
   itx1 = typear
   incrx = 1
   incry = 1
   fileb(2) = 0
   fileb(6) = 0
   fileb(7) = 0
   iobuf = nx - sysbuf
   cmplx = rc(typear)
   core = iobuf - 1
   CALL gopen(fileb,X(iobuf),1)
   CALL close(fileb,norew)
   IF ( forma==5 ) THEN
!
!     INVERT UPPER TRIANGULAR MATRIX
!
      IF ( typear==2 ) THEN
         ASSIGN 4 TO tra
         ASSIGN 10 TO tra1
         ASSIGN 20 TO tra2
      ELSEIF ( typear==3 ) THEN
         ASSIGN 4 TO tra
         ASSIGN 12 TO tra1
         ASSIGN 22 TO tra2
      ELSEIF ( typear==4 ) THEN
         ASSIGN 6 TO tra
         ASSIGN 14 TO tra1
         ASSIGN 24 TO tra2
      ELSE
         ASSIGN 2 TO tra
         ASSIGN 8 TO tra1
         ASSIGN 18 TO tra2
      ENDIF
!
!     REWRITE UPPER TRIANGULAR MATRIX ON SCRATCH FILE
!
      inbuf = iobuf
      forskp = nrow + 1
      bakskp = 0
      outbuf = inbuf - sysbuf
      IF ( outbuf<nrow+1 ) THEN
         no = -8
         CALL mesage(no,0,name)
         GOTO 99999
      ELSE
         CALL gopen(filea,X(iobuf),0)
!
!     POSITION FILE AT LAST RECORD
!
         CALL skprec(filea,nrow)
!
!     REWRITE THE INPUT MATRIX ON A SCRATCH FILE WITH THE RECORDS
!     WRITTEN IN THE REVERSE ORDER AND THE COLUMNS INVERTED
!
         CALL gopen(scrfil,X(outbuf),1)
         it2 = typear
         DO i = 1 , nrow
            ix = 1
            jx = 0
            CALL bckrec(filea)
            CALL unpack(*100,filea,X)
            CALL bckrec(filea)
            kk = jx - ix + 1
            k = kk/2
            IF ( k/=0 ) THEN
               kk = kk + 1
               DO j = 1 , k
                  l = kk - j
                  GOTO tra
 2                temp = X(j)
                  X(j) = X(l)
                  X(l) = temp
                  CYCLE
 4                dtemp = Dx(j)
                  Dx(j) = Dx(l)
                  Dx(l) = dtemp
                  CYCLE
 6                dtemp = Dx(j)
                  Dx(j) = Dx(l)
                  Dx(l) = dtemp
                  dtemp = Dx(j+1)
                  Dx(j+1) = Dx(l+1)
                  Dx(l+1) = dtemp
               ENDDO
            ENDIF
            iy = nrow - jx + 1
            jy = nrow - ix + 1
            CALL pack(X,scrfil,t)
         ENDDO
         it1 = typear
         it2 = typeb
         CALL close(filea,rew)
         CALL close(scrfil,eofnrw)
         CALL gopen(scrfil,X(iobuf),0)
         CALL skprec(scrfil,nrow)
         CALL close(scrfil,norew)
!
!     ALLOCATE CORE
!
         j = 0
         DO
            m = j + 1
            CALL gopen(scrfil,X(iobuf),rd)
            k = nrow - j
            IF ( k*m+k*(k-1)/2>=core/incr ) THEN
               a1 = (2*m-1)**2 + 8*core/incr
               k = sqrt(a1)
               k = (-(2*m-1)+k)/2
               IF ( k<=0 ) THEN
                  no = -8
                  CALL mesage(no,0,name)
                  GOTO 99999
               ENDIF
            ENDIF
            bakskp = bakskp + k
            forskp = forskp - k
!
!     POSITION SCRATCH FILE
!
            IF ( forskp>bakskp ) THEN
               CALL skprec(scrfil,-bakskp)
            ELSE
               CALL rewind(scrfil)
               CALL skprec(scrfil,forskp)
            ENDIF
!
!     GENERATE UPPER TRIANGLE OF THE IDENTITY MATRIX
!
            lend = (m*k+k*(k-1)/2)*incr
            DO i = 1 , lend
               X(i) = 0.
            ENDDO
            l = m
            IF ( prec==2 ) THEN
               DO i = 1 , k
                  Dx(l) = 1.D0
                  l = l + (i+m)*cmplx
               ENDDO
            ELSE
               DO i = 1 , k
                  X(l) = 1.
                  l = l + (i+m)*incr
               ENDDO
            ENDIF
!
!     READ UPPER TRIANGLE ONE ELEMENT AT A TIME, ADDING IN
!     APPROPIATE TERMS TO THE IDENTITY MATRIX
!
            IF ( prec==2 ) lend = lend/2
            j = j + k
            l = 1
            DO jj = 1 , j
               CALL intpk(*100,scrfil,0,typear,0)
               CALL zntpki
               i = nrow - ii + 1
               IF ( i/=j-jj+1 ) GOTO 100
               l1 = 0
               DO i1 = 1 , l
                  in1 = lend - l*cmplx - l1 + 1
                  GOTO tra1
 8                X(in1) = X(in1)/a(1)
                  GOTO 16
 10               Dx(in1) = Dx(in1)/da(1)
                  GOTO 16
 12               temp = (a(1)*X(in1)+a(2)*X(in1+1))/(a(1)*a(1)+a(2)*a(2))
                  in2 = in1 + 1
                  X(in2) = (a(1)*X(in1+1)-a(2)*X(in1))/(a(1)*a(1)+a(2)*a(2))
                  X(in1) = temp
                  GOTO 16
 14               dtemp = (da(1)*Dx(in1)+da(2)*Dx(in1+1))/(da(1)**2+da(2)**2)
                  in2 = in1 + 1
                  Dx(in2) = (da(1)*Dx(in1+1)-da(2)*Dx(in1))/(da(1)**2+da(2)**2)
                  Dx(in1) = dtemp
 16               l1 = l1 + (m+k-1-i1)*cmplx
               ENDDO
               DO WHILE ( eol/=1 )
                  CALL zntpki
                  l1 = 0
                  i = j - jj - nrow + ii
                  DO i1 = 1 , l
                     in2 = lend - l*cmplx - l1 + 1
                     in1 = in2 - i*cmplx
                     GOTO tra2
 18                  X(in1) = X(in1) - a(1)*X(in2)
                     GOTO 26
 20                  Dx(in1) = Dx(in1) - da(1)*Dx(in2)
                     GOTO 26
 22                  X(in1) = X(in1) - a(1)*X(in2) + a(2)*X(in2+1)
                     X(in1+1) = X(in1+1) - a(1)*X(in2+1) - a(2)*X(in2)
                     GOTO 26
 24                  Dx(in1) = Dx(in1) - da(1)*Dx(in2) + da(2)*Dx(in2+1)
                     Dx(in1+1) = Dx(in1+1) - da(1)*Dx(in2+1) - da(2)*Dx(in2)
 26                  l1 = l1 + (m+k-1-i1)*cmplx
                  ENDDO
               ENDDO
               l = l + 1
            ENDDO
            CALL close(scrfil,norew)
            CALL gopen(fileb,X(iobuf),wrt)
            l = j - k + 1
            ll = 1
            DO i = 1 , k
               iy = 1
               jy = l
               CALL pack(X(ll),fileb,fileb)
               l = l + 1
               ll = ll + (m+i-1)*incr
            ENDDO
            CALL close(fileb,norew)
            IF ( j>=nrow ) THEN
               CALL gopen(fileb,X(iobuf),wrt)
               CALL close(fileb,rew)
               CALL gopen(scrfil,X(iobuf),rd)
               CALL close(scrfil,rew)
               GOTO 99999
            ENDIF
         ENDDO
      ENDIF
   ELSEIF ( forma/=4 ) THEN
!
      no = -7
      CALL mesage(no,0,name)
      GOTO 99999
   ELSE
!
!     INVERT A LOWER TRIANGULAR MATRIX
!
      bakskp = nrow
      forskp = 1
      IF ( typear==2 ) THEN
         ASSIGN 30 TO tra
         ASSIGN 40 TO tra1
      ELSEIF ( typear==3 ) THEN
         ASSIGN 32 TO tra
         ASSIGN 42 TO tra1
      ELSEIF ( typear==4 ) THEN
         ASSIGN 34 TO tra
         ASSIGN 44 TO tra1
      ELSE
         ASSIGN 28 TO tra
         ASSIGN 38 TO tra1
      ENDIF
!
!     ALLOCATE CORE STORAGE
!
      CALL gopen(filea,X(iobuf),0)
      j = 1
   ENDIF
   DO
!
!     SOLVE QUADRATIC FOR K
!
      m = nrow - j + 1
      l = 2*m + 1
      k = m
      IF ( l*l>8/incr*core ) THEN
         a1 = l*l - 8/incr*core
         k = sqrt(a1)
         k = k + 1
         k = (l-k)/2
         IF ( k<=0 ) THEN
            no = -8
            CALL mesage(no,0,name)
            GOTO 99999
         ENDIF
      ENDIF
!
!     GENERATE COLUMNS J THROUGH J+K OF THE IDENTITY MATRIX (STORE
!     ONLY THE LOWER TRIANGLE IN CORE)
!
      l = (m*k-(k*(k-1))/2)*incr
      DO i = 1 , l
         X(i) = 0.
      ENDDO
      l = 1
      IF ( prec==2 ) THEN
         DO i = 1 , k
            Dx(l) = 1.D0
            l = l + (m-i+1)*cmplx
         ENDDO
      ELSE
         DO i = 1 , k
            X(l) = 1.
            l = l + (m-i+1)*incr
         ENDDO
      ENDIF
!
!     READ MATRIX A ONE ELEMENT AT A TIME, ADDING IN TERMS TO THE
!     IDENTITY MATRIX
!
      l = 1
      ll = 1
!
!     II =  COLUMN INDEX
!     M  =  HEIGTH OF TRAPAZOID
!     K  =  LENGTH OF TRAPAZOID
!
      DO i = j , nrow
         CALL intpk(*100,filea,0,typear,0)
         DO WHILE ( eol==0 )
            CALL zntpki
            IF ( i==ii ) THEN
               l1 = 0
               DO i1 = 1 , ll
                  in1 = (l-1)*cmplx + 1 + l1
                  GOTO tra
 28               X(in1) = X(in1)/a(1)
                  GOTO 36
 30               Dx(in1) = Dx(in1)/da(1)
                  GOTO 36
 32               temp = (a(1)*X(in1)+a(2)*X(in1+1))/(a(1)*a(1)+a(2)*a(2))
                  X(in1+1) = (a(1)*X(in1+1)-a(2)*X(in1))/(a(1)*a(1)+a(2)*a(2))
                  X(in1) = temp
                  GOTO 36
 34               dtemp = (da(1)*Dx(in1)+da(2)*Dx(in1+1))/(da(1)**2+da(2)**2)
                  Dx(in1+1) = (da(1)*Dx(in1+1)-da(2)*Dx(in1))/(da(1)**2+da(2)**2)
                  Dx(in1) = dtemp
 36               l1 = l1 + (m-i1)*cmplx
               ENDDO
               DO WHILE ( eol/=1 )
                  CALL zntpki
                  l1 = 0
                  DO i1 = 1 , ll
                     in2 = (l-1)*cmplx + 1 + l1
                     in1 = in2 + (ii-i)*cmplx
                     GOTO tra1
 38                  X(in1) = X(in1) - a(1)*X(in2)
                     GOTO 46
 40                  Dx(in1) = Dx(in1) - da(1)*Dx(in2)
                     GOTO 46
 42                  X(in1) = X(in1) - a(1)*X(in2) + a(2)*X(in2+1)
                     X(in1+1) = X(in1+1) - a(1)*X(in2+1) - a(2)*X(in2)
                     GOTO 46
 44                  Dx(in1) = Dx(in1) - da(1)*Dx(in2) + da(2)*Dx(in2+1)
                     Dx(in1+1) = Dx(in1+1) - da(1)*Dx(in2+1) - da(2)*Dx(in2)
 46                  l1 = l1 + (m-i1)*cmplx
                  ENDDO
               ENDDO
               ll = ll + 1
               IF ( ll>k ) ll = k
               l = l + 1
               GOTO 50
            ENDIF
         ENDDO
         GOTO 100
 50   ENDDO
      forskp = forskp + k
      bakskp = bakskp - k
      i1 = rew
      IF ( bakskp<forskp ) i1 = norew
      CALL close(filea,i1)
      CALL gopen(fileb,X(iobuf),wrt)
      l = 1
      iy = j
      jy = nrow
      DO i = 1 , k
         CALL pack(X(l),fileb,fileb)
         iy = iy + 1
         l = l + (m-i+1)*incr
      ENDDO
      CALL close(fileb,norew)
      j = j + k
      IF ( j<=nrow ) THEN
!
         CALL gopen(filea,X(iobuf),rd)
         IF ( forskp>bakskp ) THEN
            CALL skprec(filea,-bakskp)
         ELSE
            CALL skprec(filea,forskp)
         ENDIF
      ELSE
         CALL gopen(fileb,X(iobuf),wrt)
         CALL close(fileb,rew)
         RETURN
      ENDIF
   ENDDO
 100  RETURN 1
!
99999 END SUBROUTINE invtr
