
SUBROUTINE invtr(*,X,Dx)
   IMPLICIT NONE
   REAL A(4) , Prc(2)
   DOUBLE PRECISION Da(2)
   INTEGER Eofnrw , Eol , Filea(7) , Fileb(7) , Forma , Ii , Incrx , Incry , It1 , It2 , Itx1 , Ix , Iy , Jx , Jy , Norew , Nrow ,  &
         & Nwds(4) , Nx , Prec , Rc(10) , Rd , Rdrew , Rew , Scrfil , Sysbuf , Typea , Typeb , Wrt , Wrtrew
   COMMON /invtrx/ Filea , Fileb , Scrfil , Nx , Prec
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw
   COMMON /packx / It1 , It2 , Iy , Jy , Incry
   COMMON /system/ Sysbuf
   COMMON /type  / Prc , Nwds , Rc
   COMMON /unpakx/ Itx1 , Ix , Jx , Incrx
   COMMON /zntpkx/ A , Ii , Eol
   DOUBLE PRECISION Dx(1)
   REAL X(1)
   REAL a1 , temp
   INTEGER bakskp , cmplx , core , forskp , i , i1 , in1 , in2 , inbuf , incr , iobuf , j , jj , k , kk , l , l1 , lend , ll , m ,  &
         & name(2) , no , outbuf , t(7) , tra , tra1 , tra2 , typear
   DOUBLE PRECISION dtemp
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
   typear = Rc(Typea)
   typear = Rc(typear) + Prec - 1
   incr = Nwds(typear)
   It1 = typear
   It2 = Typeb
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
   IF ( Forma==5 ) THEN
!
!     INVERT UPPER TRIANGULAR MATRIX
!
      IF ( typear==2 ) THEN
         ASSIGN 4 TO tra
         ASSIGN 340 TO tra1
         ASSIGN 420 TO tra2
      ELSEIF ( typear==3 ) THEN
         ASSIGN 4 TO tra
         ASSIGN 360 TO tra1
         ASSIGN 430 TO tra2
      ELSEIF ( typear==4 ) THEN
         ASSIGN 6 TO tra
         ASSIGN 380 TO tra1
         ASSIGN 440 TO tra2
      ELSE
         ASSIGN 2 TO tra
         ASSIGN 320 TO tra1
         ASSIGN 410 TO tra2
      ENDIF
!
!     REWRITE UPPER TRIANGULAR MATRIX ON SCRATCH FILE
!
      inbuf = iobuf
      forskp = Nrow + 1
      bakskp = 0
      outbuf = inbuf - Sysbuf
      IF ( outbuf<Nrow+1 ) THEN
         no = -8
         CALL mesage(no,0,name)
         GOTO 99999
      ELSE
         CALL gopen(Filea,X(iobuf),0)
!
!     POSITION FILE AT LAST RECORD
!
         CALL skprec(Filea,Nrow)
!
!     REWRITE THE INPUT MATRIX ON A SCRATCH FILE WITH THE RECORDS
!     WRITTEN IN THE REVERSE ORDER AND THE COLUMNS INVERTED
!
         CALL gopen(Scrfil,X(outbuf),1)
         It2 = typear
         DO i = 1 , Nrow
            Ix = 1
            Jx = 0
            CALL bckrec(Filea)
            CALL unpack(*500,Filea,X)
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
            Iy = Nrow - Jx + 1
            Jy = Nrow - Ix + 1
            CALL pack(X,Scrfil,t)
         ENDDO
         It1 = typear
         It2 = Typeb
         CALL close(Filea,Rew)
         CALL close(Scrfil,Eofnrw)
         CALL gopen(Scrfil,X(iobuf),0)
         CALL skprec(Scrfil,Nrow)
         CALL close(Scrfil,Norew)
!
!     ALLOCATE CORE
!
         j = 0
         GOTO 300
      ENDIF
   ELSEIF ( Forma/=4 ) THEN
!
      no = -7
      CALL mesage(no,0,name)
      GOTO 99999
   ELSE
!
!     INVERT A LOWER TRIANGULAR MATRIX
!
      bakskp = Nrow
      forskp = 1
      IF ( typear==2 ) THEN
         ASSIGN 110 TO tra
         ASSIGN 128 TO tra1
      ELSEIF ( typear==3 ) THEN
         ASSIGN 115 TO tra
         ASSIGN 130 TO tra1
      ELSEIF ( typear==4 ) THEN
         ASSIGN 120 TO tra
         ASSIGN 132 TO tra1
      ELSE
         ASSIGN 105 TO tra
         ASSIGN 126 TO tra1
      ENDIF
!
!     ALLOCATE CORE STORAGE
!
      CALL gopen(Filea,X(iobuf),0)
      j = 1
   ENDIF
!
!     SOLVE QUADRATIC FOR K
!
 100  m = Nrow - j + 1
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
   DO i = j , Nrow
      CALL intpk(*500,Filea,0,typear,0)
      DO WHILE ( Eol==0 )
         CALL zntpki
         IF ( i==Ii ) THEN
            l1 = 0
            DO i1 = 1 , ll
               in1 = (l-1)*cmplx + 1 + l1
               GOTO tra
 105           X(in1) = X(in1)/A(1)
               GOTO 125
 110           Dx(in1) = Dx(in1)/Da(1)
               GOTO 125
 115           temp = (A(1)*X(in1)+A(2)*X(in1+1))/(A(1)*A(1)+A(2)*A(2))
               X(in1+1) = (A(1)*X(in1+1)-A(2)*X(in1))/(A(1)*A(1)+A(2)*A(2))
               X(in1) = temp
               GOTO 125
 120           dtemp = (Da(1)*Dx(in1)+Da(2)*Dx(in1+1))/(Da(1)**2+Da(2)**2)
               Dx(in1+1) = (Da(1)*Dx(in1+1)-Da(2)*Dx(in1))/(Da(1)**2+Da(2)**2)
               Dx(in1) = dtemp
 125           l1 = l1 + (m-i1)*cmplx
            ENDDO
            DO WHILE ( Eol/=1 )
               CALL zntpki
               l1 = 0
               DO i1 = 1 , ll
                  in2 = (l-1)*cmplx + 1 + l1
                  in1 = in2 + (Ii-i)*cmplx
                  GOTO tra1
 126              X(in1) = X(in1) - A(1)*X(in2)
                  GOTO 134
 128              Dx(in1) = Dx(in1) - Da(1)*Dx(in2)
                  GOTO 134
 130              X(in1) = X(in1) - A(1)*X(in2) + A(2)*X(in2+1)
                  X(in1+1) = X(in1+1) - A(1)*X(in2+1) - A(2)*X(in2)
                  GOTO 134
 132              Dx(in1) = Dx(in1) - Da(1)*Dx(in2) + Da(2)*Dx(in2+1)
                  Dx(in1+1) = Dx(in1+1) - Da(1)*Dx(in2+1) - Da(2)*Dx(in2)
 134              l1 = l1 + (m-i1)*cmplx
               ENDDO
            ENDDO
            ll = ll + 1
            IF ( ll>k ) ll = k
            l = l + 1
            GOTO 200
         ENDIF
      ENDDO
      GOTO 500
 200  ENDDO
   forskp = forskp + k
   bakskp = bakskp - k
   i1 = Rew
   IF ( bakskp<forskp ) i1 = Norew
   CALL close(Filea,i1)
   CALL gopen(Fileb,X(iobuf),Wrt)
   l = 1
   Iy = j
   Jy = Nrow
   DO i = 1 , k
      CALL pack(X(l),Fileb,Fileb)
      Iy = Iy + 1
      l = l + (m-i+1)*incr
   ENDDO
   CALL close(Fileb,Norew)
   j = j + k
   IF ( j<=Nrow ) THEN
!
      CALL gopen(Filea,X(iobuf),Rd)
      IF ( forskp>bakskp ) THEN
         CALL skprec(Filea,-bakskp)
      ELSE
         CALL skprec(Filea,forskp)
      ENDIF
      GOTO 100
   ELSE
      CALL gopen(Fileb,X(iobuf),Wrt)
      CALL close(Fileb,Rew)
      RETURN
   ENDIF
 300  m = j + 1
   CALL gopen(Scrfil,X(iobuf),Rd)
   k = Nrow - j
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
      CALL intpk(*500,Scrfil,0,typear,0)
      CALL zntpki
      i = Nrow - Ii + 1
      IF ( i/=j-jj+1 ) GOTO 500
      l1 = 0
      DO i1 = 1 , l
         in1 = lend - l*cmplx - l1 + 1
         GOTO tra1
 320     X(in1) = X(in1)/A(1)
         GOTO 400
 340     Dx(in1) = Dx(in1)/Da(1)
         GOTO 400
 360     temp = (A(1)*X(in1)+A(2)*X(in1+1))/(A(1)*A(1)+A(2)*A(2))
         in2 = in1 + 1
         X(in2) = (A(1)*X(in1+1)-A(2)*X(in1))/(A(1)*A(1)+A(2)*A(2))
         X(in1) = temp
         GOTO 400
 380     dtemp = (Da(1)*Dx(in1)+Da(2)*Dx(in1+1))/(Da(1)**2+Da(2)**2)
         in2 = in1 + 1
         Dx(in2) = (Da(1)*Dx(in1+1)-Da(2)*Dx(in1))/(Da(1)**2+Da(2)**2)
         Dx(in1) = dtemp
 400     l1 = l1 + (m+k-1-i1)*cmplx
      ENDDO
      DO WHILE ( Eol/=1 )
         CALL zntpki
         l1 = 0
         i = j - jj - Nrow + Ii
         DO i1 = 1 , l
            in2 = lend - l*cmplx - l1 + 1
            in1 = in2 - i*cmplx
            GOTO tra2
 410        X(in1) = X(in1) - A(1)*X(in2)
            GOTO 450
 420        Dx(in1) = Dx(in1) - Da(1)*Dx(in2)
            GOTO 450
 430        X(in1) = X(in1) - A(1)*X(in2) + A(2)*X(in2+1)
            X(in1+1) = X(in1+1) - A(1)*X(in2+1) - A(2)*X(in2)
            GOTO 450
 440        Dx(in1) = Dx(in1) - Da(1)*Dx(in2) + Da(2)*Dx(in2+1)
            Dx(in1+1) = Dx(in1+1) - Da(1)*Dx(in2+1) - Da(2)*Dx(in2)
 450        l1 = l1 + (m+k-1-i1)*cmplx
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
   IF ( j<Nrow ) GOTO 300
   CALL gopen(Fileb,X(iobuf),Wrt)
   CALL close(Fileb,Rew)
   CALL gopen(Scrfil,X(iobuf),Rd)
   CALL close(Scrfil,Rew)
   GOTO 99999
 500  RETURN 1
!
99999 RETURN
END SUBROUTINE invtr