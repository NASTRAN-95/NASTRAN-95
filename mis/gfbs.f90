
SUBROUTINE gfbs(X,Dx)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL A(4) , Cdp , Csp , Diag , Prc(2) , Rd , Rdp , Rdrew , Rect , Row , Rsp , Sqr , Sym , Uprtri , Wrt , Wrtrew
   DOUBLE PRECISION Da(2)
   INTEGER Eofnrw , Eol , Fileb(7) , Filel(7) , Fileu(7) , Filex(7) , Formb , Identy , Ii , Incrx , Incry , Isign , Ixy , Iy , Jxy ,&
         & Jy , Lowtri , Norew , Nout , Nrow , Nwds(4) , Nx , Prec , Rc(10) , Rew , Sysbuf , Type1 , Type2 , Typea , Typeb , Typel ,&
         & Typex
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /gfbsx / Filel , Fileu , Fileb , Filex , Nx , Prec , Isign
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp , Csp , Cdp , Sqr , Rect , Diag , Lowtri , Uprtri , &
                 & Sym , Row , Identy
   COMMON /packx / Type1 , Type2 , Iy , Jy , Incrx
   COMMON /system/ Sysbuf , Nout
   COMMON /type  / Prc , Nwds , Rc
   COMMON /unpakx/ Typea , Ixy , Jxy , Incry
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zntpkx/ A , Ii , Eol
!
! Dummy argument declarations
!
   DOUBLE PRECISION Dx(1)
   REAL X(1)
!
! Local variable declarations
!
   REAL begn , buf(2) , end , subnam(2) , temp , zeros(4)
   INTEGER clsop , cmplx , col , fstcol , i , icol , ident , ijk , in1 , in2 , incr , intchn , iobuf , ioff , j , j1 , j2 , k ,     &
         & kerr , khr , kk , l , lstcol , lstlod , ncol , nm , nn , noload , nxtnz , parm(4) , tra1 , tra2 , tra3 , tra4 , tra5 ,   &
         & typear
   DOUBLE PRECISION dtemp
!
! End of declarations
!
!
!     GIVEN THE TRIANGULAR FACTORS FOR A GENERAL MATRIX, GFBS WILL
!     PERFORM THE FORWARD-BACKWARD SUBSTITUTION NECESSARY TO SOLVE
!     A SYSTEM OF EQUATIONS
!
!     DEFINITION OF INPUT PARAMETERS
!
!     FILEL    =  MATRIX CONTROL BLOCK FOR THE LOWER TRIANGLE L
!     FILEU    =  MATRIX CONTROL BLOCK FOR THE UPPER TRIANGLE U
!     FILEB    =  MATRIX CONTROL BLOCK FOR THE LOAD   VECTORS B
!     FILEX    =  MATRIX CONTROL BLOCK FOR THE SOLUTION VECTORS X
!     NX       =  NUMBER OF CELLS OF CORE AVAILABLE AT X
!     PREC     =  DESIRED PRECISION OF ARITHMETIC OPERATIONS
!                (1 = SINGLE PRECISION, 2 = DOUBLE PRECISION)
!     ISIGN    =  SIGN TO BE APPLIED TO THE LOAD VECTORS
!     X        =  BLOCK OF CORE AVAILABLE AS WORKING STORAGE
!     DX       =  SAME BLOCK AS X, BUT TYPED DOUBLE PRECISION
!
!     COMMON   /DESCRP/  LENGTH    ,MAJOR
   EQUIVALENCE (A(1),Da(1)) , (Filel(5),Typel) , (Filel(3),Nrow) , (Filex(5),Typex) , (Fileb(4),Formb) , (Fileb(5),Typeb)
   DATA parm(3) , parm(4)/4HGFBS , 4H    /
   DATA zeros/0. , 0. , 0. , 0./
   DATA subnam/4HGFBS , 4H    / , begn/4HBEGN/ , end/4HEND /
!
   buf(1) = subnam(1)
   buf(2) = begn
   CALL conmsg(buf,2,0)
!
!     INITIALIZE
!
   IF ( Formb==Identy ) Typeb = 1
   typear = Prec
   IF ( Rc(Typel)+Rc(Typeb)-1>1 ) typear = Prec + 2
   incr = Nwds(typear)*Nrow
   Typea = typear*Isign
   Type1 = typear
   Type2 = Typex
   Incrx = 1
   Incry = 1
   cmplx = Rc(typear)
   iobuf = Nx - Sysbuf
   icol = iobuf - 1
   col = 1
   clsop = Eofnrw
!
!     SET UP TRANSFER VECTORS FOR THE ARITHMETIC TYPES
!
   IF ( typear==2 ) THEN
      ASSIGN 600 TO tra1
      ASSIGN 1200 TO tra2
      ASSIGN 2000 TO tra3
      ASSIGN 2900 TO tra4
      ASSIGN 3700 TO tra5
   ELSEIF ( typear==3 ) THEN
      ASSIGN 650 TO tra1
      ASSIGN 1300 TO tra2
      ASSIGN 2100 TO tra3
      ASSIGN 3000 TO tra4
      ASSIGN 3800 TO tra5
   ELSEIF ( typear==4 ) THEN
      ASSIGN 700 TO tra1
      ASSIGN 1400 TO tra2
      ASSIGN 2200 TO tra3
      ASSIGN 3100 TO tra4
      ASSIGN 3900 TO tra5
   ELSE
      ASSIGN 550 TO tra1
      ASSIGN 1100 TO tra2
      ASSIGN 1900 TO tra3
      ASSIGN 2800 TO tra4
      ASSIGN 3600 TO tra5
   ENDIF
   nm = (iobuf-1)/incr
   IF ( nm<=0 ) THEN
      parm(1) = -8
      GOTO 4300
   ELSE
      noload = Fileb(2)
      IF ( Formb==Identy ) noload = Nrow
      ident = 1
      lstlod = noload
!
!     WRITE OUTPUT HEADER RECORDS AND INITIALIZE MATRIX CONTROL BLOCKS
!
      CALL gopen(Filex,X(iobuf),1)
      CALL close(Filex(1),Norew)
      Filex(2) = 0
      Filex(6) = 0
      Filex(7) = 0
      IF ( Formb==Identy ) GOTO 500
!
!     OPEN THE LOAD FILE AND FILL CORE WITH LOAD VECTORS
!
      CALL gopen(Fileb,X(iobuf),0)
   ENDIF
 100  nn = 0
   khr = icol
   fstcol = col
   l = 1
   Ixy = 1
   Jxy = Nrow
 200  IF ( l+incr>=khr ) THEN
      col = col - 1
      GOTO 400
   ELSE
      CALL unpack(*300,Fileb,X(l))
      nn = nn + 1
      X(khr) = col
      khr = khr - 1
      l = l + incr
   ENDIF
 300  IF ( col/=lstlod ) THEN
      col = col + 1
      GOTO 200
   ENDIF
 400  ncol = khr
   X(ncol) = lstlod + 1
   lstcol = col
   IF ( lstcol==lstlod ) clsop = Rew
   CALL close(Fileb,clsop)
   IF ( nn/=0 ) GOTO 800
   GOTO 4100
!
!     GENERATE COLUMNS OF THE IDENTITY MATRIX
!
 500  nn = min0(nm,noload)
   l = 1
   DO i = 1 , nn
      j1 = l
      j2 = j1 + incr - 1
      DO k = j1 , j2
         X(k) = 0.
      ENDDO
      k = l + ident - 1
      GOTO tra1
 550  X(k) = 1.
      GOTO 750
 600  k = (l-1)/2 + ident
      Dx(k) = 1.D0
      GOTO 750
 650  kk = k + ident - 1
      X(kk) = 1.
      GOTO 750
 700  kk = (l-1)/2 + 2*ident - 1
      Dx(kk) = 1.D0
 750  ident = ident + 1
      l = l + incr
   ENDDO
   fstcol = col
   col = ident - 1
   lstcol = col
 800  ijk = 0
!
!     OPEN FILE FOR THE LOWER TRIANGLE
!
   parm(2) = Filel(1)
   CALL gopen(Filel,X(iobuf),0)
!
!     BEGIN FORWARD PASS
!
   j = 1
 900  CALL intpk(*2400,Filel(1),0,typear,0)
   DO WHILE ( Eol==0 )
      CALL zntpki
      IF ( j<Ii ) GOTO 1700
      IF ( j==Ii ) THEN
!
!     PERFORM THE REQUIRED ROW INTERCHANGE
!
         intchn = A(1)
         k = 0
         IF ( Prec==2 ) intchn = Da(1)
         in1 = j*cmplx
         in2 = in1 + intchn*cmplx
         GOTO 1000
      ENDIF
   ENDDO
   GOTO 4200
 1000 GOTO tra2
 1100 temp = X(in1)
   X(in1) = X(in2)
   X(in2) = temp
   GOTO 1500
 1200 dtemp = Dx(in1)
   Dx(in1) = Dx(in2)
   Dx(in2) = dtemp
   GOTO 1500
 1300 temp = X(in1)
   X(in1) = X(in2)
   X(in2) = temp
   temp = X(in1-1)
   X(in1-1) = X(in2-1)
   X(in2-1) = temp
   GOTO 1500
 1400 dtemp = Dx(in1)
   Dx(in1) = Dx(in2)
   Dx(in2) = dtemp
   dtemp = Dx(in1-1)
   Dx(in1-1) = Dx(in2-1)
   Dx(in2-1) = dtemp
 1500 in1 = in1 + Nrow*cmplx
   in2 = in2 + Nrow*cmplx
   k = k + 1
   IF ( k<nn ) GOTO 1000
 1600 IF ( Eol/=0 ) GOTO 2400
   CALL zntpki
 1700 k = 0
   in2 = j*cmplx
   in1 = Ii*cmplx
 1800 k = k + 1
   GOTO tra3
 1900 X(in1) = X(in1) - X(in2)*A(1)
   GOTO 2300
 2000 Dx(in1) = Dx(in1) - Dx(in2)*Da(1)
   GOTO 2300
 2100 X(in1-1) = X(in1-1) - A(1)*X(in2-1) + A(2)*X(in2)
   X(in1) = X(in1) - A(1)*X(in2) - A(2)*X(in2-1)
   GOTO 2300
 2200 Dx(in1-1) = Dx(in1-1) - Da(1)*Dx(in2-1) + Da(2)*Dx(in2)
   Dx(in1) = Dx(in1) - Da(1)*Dx(in2) - Da(2)*Dx(in2-1)
 2300 in1 = in1 + Nrow*cmplx
   in2 = in2 + Nrow*cmplx
   IF ( k>=nn ) GOTO 1600
   GOTO 1800
 2400 j = j + 1
   IF ( j<Nrow ) GOTO 900
   CALL close(Filel(1),Rew)
!
!     BEGIN BACKWARD PASS
!
   ioff = Fileu(7) - 1
   parm(2) = Fileu(1)
   CALL gopen(Fileu,X(iobuf),0)
   j = Nrow
 2500 CALL intpk(*4200,Fileu(1),0,typear,0)
   IF ( Eol/=0 ) GOTO 4200
 2600 CALL zntpki
   i = Nrow - Ii + 1
   IF ( i/=j ) GOTO 3400
!
!     DIVIDE BY THE DIAGONAL
!
   in1 = i*cmplx
   k = 0
 2700 GOTO tra4
 2800 X(in1) = X(in1)/A(1)
   GOTO 3200
 2900 Dx(in1) = Dx(in1)/Da(1)
   GOTO 3200
 3000 temp = (A(1)*X(in1-1)+A(2)*X(in1))/(A(1)*A(1)+A(2)*A(2))
   X(in1) = (A(1)*X(in1)-A(2)*X(in1-1))/(A(1)*A(1)+A(2)*A(2))
   X(in1-1) = temp
   GOTO 3200
 3100 dtemp = (Da(1)*Dx(in1-1)+Da(2)*Dx(in1))/(Da(1)**2+Da(2)**2)
   Dx(in1) = (Da(1)*Dx(in1)-Da(2)*Dx(in1-1))/(Da(1)**2+Da(2)**2)
   Dx(in1-1) = dtemp
 3200 k = k + 1
   in1 = in1 + Nrow*cmplx
   IF ( k<nn ) GOTO 2700
 3300 IF ( Eol/=0 ) THEN
      j = j - 1
      IF ( j>0 ) GOTO 2500
      CALL close(Fileu(1),Rew)
      GOTO 4100
   ELSE
      CALL zntpki
      i = Nrow - Ii + 1
   ENDIF
 3400 in1 = i*cmplx
   in2 = j*cmplx
   IF ( i>=j ) THEN
      k = in1
      in1 = in2 - ioff*cmplx
      in2 = k
   ENDIF
   k = 0
 3500 GOTO tra5
 3600 X(in1) = X(in1) - A(1)*X(in2)
   GOTO 4000
 3700 Dx(in1) = Dx(in1) - Dx(in2)*Da(1)
   GOTO 4000
 3800 X(in1-1) = X(in1-1) - A(1)*X(in2-1) + A(2)*X(in2)
   X(in1) = X(in1) - A(1)*X(in2) - A(2)*X(in2-1)
   GOTO 4000
 3900 Dx(in1-1) = Dx(in1-1) - Da(1)*Dx(in2-1) + Da(2)*Dx(in2)
   Dx(in1) = Dx(in1) - Da(1)*Dx(in2) - Da(2)*Dx(in2-1)
 4000 in1 = in1 + Nrow*cmplx
   in2 = in2 + Nrow*cmplx
   k = k + 1
   IF ( k<nn ) GOTO 3500
!
!     SUBTRACT OFF REMAINING TERMS
!
   IF ( i<=j ) GOTO 3300
   GOTO 2600
!
!     OUTPUT LOAD VECTORS
!
 4100 CALL gopen(Filex,X(iobuf),Wrt)
   l = 1
   Iy = 1
   IF ( Formb/=Identy ) nxtnz = X(icol)
   khr = icol
   DO col = fstcol , lstcol
      IF ( Formb/=Identy ) THEN
! 593 CONTINUE
         IF ( col<nxtnz ) THEN
            Jy = 1
            CALL pack(zeros,Filex,Filex)
            CYCLE
         ELSEIF ( col/=nxtnz ) THEN
            GOTO 4500
         ENDIF
      ENDIF
      Jy = Nrow
      CALL pack(X(l),Filex,Filex)
      l = l + incr
      khr = khr - 1
      IF ( Formb/=Identy ) nxtnz = X(khr)
   ENDDO
   IF ( Formb/=Identy .AND. khr/=ncol ) THEN
      kerr = 600
      GOTO 4600
   ELSE
      IF ( lstcol==lstlod ) clsop = Rew
      CALL close(Filex,clsop)
      noload = noload - (lstcol-fstcol+1)
      IF ( lstcol==lstlod ) GOTO 4400
      col = lstcol + 1
      IF ( Formb==Identy ) GOTO 500
      CALL gopen(Fileb,X(iobuf),Rd)
      GOTO 100
   ENDIF
 4200 parm(1) = -5
 4300 CALL mesage(parm(1),parm(2),parm(3))
 4400 IF ( Filex(2)/=lstlod ) THEN
      kerr = 670
      GOTO 4600
   ELSE
      buf(1) = subnam(1)
      buf(2) = end
      CALL conmsg(buf,2,0)
      RETURN
   ENDIF
!
!     LOGIC ERRORS LAND HERE
!
 4500 kerr = 593
 4600 WRITE (Nout,99001) Sfm , kerr
99001 FORMAT (A25,I4,' - LOGIC ERROR IN GFBS')
   CALL mesage(-61,0,0)
END SUBROUTINE gfbs
