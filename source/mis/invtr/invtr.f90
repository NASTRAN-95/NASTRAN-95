!*==invtr.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE invtr(X,Dx) !HIDESTARS (*,X,Dx)
   IMPLICIT NONE
   USE C_INVTRX
   USE C_NAMES
   USE C_PACKX
   USE C_SYSTEM
   USE C_TYPE
   USE C_UNPAKX
   USE C_ZNTPKX
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
   typear = Rc(typea)
   typear = Rc(typear) + Prec - 1
   incr = Nwds(typear)
   It1 = typear
   It2 = typeb
   Itx1 = typear
   Incrx = 1
   Incry = 1
   Fileb(2) = 0
   Fileb(6) = 0
   Fileb(7) = 0
   iobuf = Nx - Sysbuf
   cmplx = Rc(typear)
   core = iobuf - 1
   CALL gopen(Fileb,X(iobuf),1)
   CALL close(Fileb,Norew)
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
      outbuf = inbuf - Sysbuf
      IF ( outbuf<nrow+1 ) THEN
         no = -8
         CALL mesage(no,0,name)
         GOTO 99999
      ELSE
         CALL gopen(Filea,X(iobuf),0)
!
!     POSITION FILE AT LAST RECORD
!
         CALL skprec(Filea,nrow)
!
!     REWRITE THE INPUT MATRIX ON A SCRATCH FILE WITH THE RECORDS
!     WRITTEN IN THE REVERSE ORDER AND THE COLUMNS INVERTED
!
         CALL gopen(Scrfil,X(outbuf),1)
         It2 = typear
         DO i = 1 , nrow
            Ix = 1
            Jx = 0
            CALL bckrec(Filea)
            CALL unpack(*100,Filea,X)
            CALL bckrec(Filea)
            kk = Jx - Ix + 1
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
            Iy = nrow - Jx + 1
            Jy = nrow - Ix + 1
            CALL pack(X,Scrfil,t)
         ENDDO
         It1 = typear
         It2 = typeb
         CALL close(Filea,Rew)
         CALL close(Scrfil,Eofnrw)
         CALL gopen(Scrfil,X(iobuf),0)
         CALL skprec(Scrfil,nrow)
         CALL close(Scrfil,Norew)
!
!     ALLOCATE CORE
!
         j = 0
         DO
            m = j + 1
            CALL gopen(Scrfil,X(iobuf),Rd)
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
               CALL skprec(Scrfil,-bakskp)
            ELSE
               CALL rewind(Scrfil)
               CALL skprec(Scrfil,forskp)
            ENDIF
!
!     GENERATE UPPER TRIANGLE OF THE IDENTITY MATRIX
!
            lend = (m*k+k*(k-1)/2)*incr
            DO i = 1 , lend
               X(i) = 0.
            ENDDO
            l = m
            IF ( Prec==2 ) THEN
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
            IF ( Prec==2 ) lend = lend/2
            j = j + k
            l = 1
            DO jj = 1 , j
               CALL intpk(*100,Scrfil,0,typear,0)
               CALL zntpki
               i = nrow - Ii + 1
               IF ( i/=j-jj+1 ) GOTO 100
               l1 = 0
               DO i1 = 1 , l
                  in1 = lend - l*cmplx - l1 + 1
                  GOTO tra1
 8                X(in1) = X(in1)/A(1)
                  GOTO 16
 10               Dx(in1) = Dx(in1)/da(1)
                  GOTO 16
 12               temp = (A(1)*X(in1)+A(2)*X(in1+1))/(A(1)*A(1)+A(2)*A(2))
                  in2 = in1 + 1
                  X(in2) = (A(1)*X(in1+1)-A(2)*X(in1))/(A(1)*A(1)+A(2)*A(2))
                  X(in1) = temp
                  GOTO 16
 14               dtemp = (da(1)*Dx(in1)+da(2)*Dx(in1+1))/(da(1)**2+da(2)**2)
                  in2 = in1 + 1
                  Dx(in2) = (da(1)*Dx(in1+1)-da(2)*Dx(in1))/(da(1)**2+da(2)**2)
                  Dx(in1) = dtemp
 16               l1 = l1 + (m+k-1-i1)*cmplx
               ENDDO
               DO WHILE ( Eol/=1 )
                  CALL zntpki
                  l1 = 0
                  i = j - jj - nrow + Ii
                  DO i1 = 1 , l
                     in2 = lend - l*cmplx - l1 + 1
                     in1 = in2 - i*cmplx
                     GOTO tra2
 18                  X(in1) = X(in1) - A(1)*X(in2)
                     GOTO 26
 20                  Dx(in1) = Dx(in1) - da(1)*Dx(in2)
                     GOTO 26
 22                  X(in1) = X(in1) - A(1)*X(in2) + A(2)*X(in2+1)
                     X(in1+1) = X(in1+1) - A(1)*X(in2+1) - A(2)*X(in2)
                     GOTO 26
 24                  Dx(in1) = Dx(in1) - da(1)*Dx(in2) + da(2)*Dx(in2+1)
                     Dx(in1+1) = Dx(in1+1) - da(1)*Dx(in2+1) - da(2)*Dx(in2)
 26                  l1 = l1 + (m+k-1-i1)*cmplx
                  ENDDO
               ENDDO
               l = l + 1
            ENDDO
            CALL close(Scrfil,Norew)
            CALL gopen(Fileb,X(iobuf),Wrt)
            l = j - k + 1
            ll = 1
            DO i = 1 , k
               Iy = 1
               Jy = l
               CALL pack(X(ll),Fileb,Fileb)
               l = l + 1
               ll = ll + (m+i-1)*incr
            ENDDO
            CALL close(Fileb,Norew)
            IF ( j>=nrow ) THEN
               CALL gopen(Fileb,X(iobuf),Wrt)
               CALL close(Fileb,Rew)
               CALL gopen(Scrfil,X(iobuf),Rd)
               CALL close(Scrfil,Rew)
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
      CALL gopen(Filea,X(iobuf),0)
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
      IF ( Prec==2 ) THEN
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
         CALL intpk(*100,Filea,0,typear,0)
         DO WHILE ( Eol==0 )
            CALL zntpki
            IF ( i==Ii ) THEN
               l1 = 0
               DO i1 = 1 , ll
                  in1 = (l-1)*cmplx + 1 + l1
                  GOTO tra
 28               X(in1) = X(in1)/A(1)
                  GOTO 36
 30               Dx(in1) = Dx(in1)/da(1)
                  GOTO 36
 32               temp = (A(1)*X(in1)+A(2)*X(in1+1))/(A(1)*A(1)+A(2)*A(2))
                  X(in1+1) = (A(1)*X(in1+1)-A(2)*X(in1))/(A(1)*A(1)+A(2)*A(2))
                  X(in1) = temp
                  GOTO 36
 34               dtemp = (da(1)*Dx(in1)+da(2)*Dx(in1+1))/(da(1)**2+da(2)**2)
                  Dx(in1+1) = (da(1)*Dx(in1+1)-da(2)*Dx(in1))/(da(1)**2+da(2)**2)
                  Dx(in1) = dtemp
 36               l1 = l1 + (m-i1)*cmplx
               ENDDO
               DO WHILE ( Eol/=1 )
                  CALL zntpki
                  l1 = 0
                  DO i1 = 1 , ll
                     in2 = (l-1)*cmplx + 1 + l1
                     in1 = in2 + (Ii-i)*cmplx
                     GOTO tra1
 38                  X(in1) = X(in1) - A(1)*X(in2)
                     GOTO 46
 40                  Dx(in1) = Dx(in1) - da(1)*Dx(in2)
                     GOTO 46
 42                  X(in1) = X(in1) - A(1)*X(in2) + A(2)*X(in2+1)
                     X(in1+1) = X(in1+1) - A(1)*X(in2+1) - A(2)*X(in2)
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
      i1 = Rew
      IF ( bakskp<forskp ) i1 = Norew
      CALL close(Filea,i1)
      CALL gopen(Fileb,X(iobuf),Wrt)
      l = 1
      Iy = j
      Jy = nrow
      DO i = 1 , k
         CALL pack(X(l),Fileb,Fileb)
         Iy = Iy + 1
         l = l + (m-i+1)*incr
      ENDDO
      CALL close(Fileb,Norew)
      j = j + k
      IF ( j<=nrow ) THEN
!
         CALL gopen(Filea,X(iobuf),Rd)
         IF ( forskp>bakskp ) THEN
            CALL skprec(Filea,-bakskp)
         ELSE
            CALL skprec(Filea,forskp)
         ENDIF
      ELSE
         CALL gopen(Fileb,X(iobuf),Wrt)
         CALL close(Fileb,Rew)
         RETURN
      ENDIF
   ENDDO
 100  RETURN 1
!
99999 END SUBROUTINE invtr
