!*==gfbs.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE gfbs(X,Dx)
   IMPLICIT NONE
   USE C_GFBSX
   USE C_NAMES
   USE C_PACKX
   USE C_SYSTEM
   USE C_TYPE
   USE C_UNPAKX
   USE C_XMSSG
   USE C_ZNTPKX
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: X
   REAL*8 , DIMENSION(1) :: Dx
!
! Local variable declarations rewritten by SPAG
!
   REAL , SAVE :: begn , end
   REAL , DIMENSION(2) :: buf
   INTEGER :: clsop , cmplx , col , formb , fstcol , i , icol , ident , ijk , in1 , in2 , incr , intchn , iobuf , ioff , j , j1 ,   &
            & j2 , k , kerr , khr , kk , l , lstcol , lstlod , ncol , nm , nn , noload , nrow , nxtnz , tra1 , tra2 , tra3 , tra4 , &
            & tra5 , typear , typeb , typel , typex
   REAL*8 , DIMENSION(2) :: da
   REAL*8 :: dtemp
   INTEGER , DIMENSION(4) , SAVE :: parm
   REAL , DIMENSION(2) , SAVE :: subnam
   REAL :: temp
   REAL , DIMENSION(4) , SAVE :: zeros
!
! End of declarations rewritten by SPAG
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
   !>>>>EQUIVALENCE (A(1),Da(1)) , (Filel(5),Typel) , (Filel(3),Nrow) , (Filex(5),Typex) , (Fileb(4),Formb) , (Fileb(5),Typeb)
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
   IF ( formb==Identy ) typeb = 1
   typear = Prec
   IF ( Rc(typel)+Rc(typeb)-1>1 ) typear = Prec + 2
   incr = Nwds(typear)*nrow
   Typea = typear*Isign
   Type1 = typear
   Type2 = typex
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
      IF ( formb==Identy ) noload = nrow
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
      IF ( formb==Identy ) GOTO 500
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
   Jxy = nrow
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
   IF ( nn==0 ) GOTO 4100
   GOTO 800
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
         IF ( Prec==2 ) intchn = da(1)
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
 1500 in1 = in1 + nrow*cmplx
   in2 = in2 + nrow*cmplx
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
 2000 Dx(in1) = Dx(in1) - Dx(in2)*da(1)
   GOTO 2300
 2100 X(in1-1) = X(in1-1) - A(1)*X(in2-1) + A(2)*X(in2)
   X(in1) = X(in1) - A(1)*X(in2) - A(2)*X(in2-1)
   GOTO 2300
 2200 Dx(in1-1) = Dx(in1-1) - da(1)*Dx(in2-1) + da(2)*Dx(in2)
   Dx(in1) = Dx(in1) - da(1)*Dx(in2) - da(2)*Dx(in2-1)
 2300 in1 = in1 + nrow*cmplx
   in2 = in2 + nrow*cmplx
   IF ( k<nn ) GOTO 1800
   GOTO 1600
 2400 j = j + 1
   IF ( j<nrow ) GOTO 900
   CALL close(Filel(1),Rew)
!
!     BEGIN BACKWARD PASS
!
   ioff = Fileu(7) - 1
   parm(2) = Fileu(1)
   CALL gopen(Fileu,X(iobuf),0)
   j = nrow
 2500 CALL intpk(*4200,Fileu(1),0,typear,0)
   IF ( Eol/=0 ) GOTO 4200
 2600 CALL zntpki
   i = nrow - Ii + 1
   IF ( i/=j ) GOTO 3400
!
!     DIVIDE BY THE DIAGONAL
!
   in1 = i*cmplx
   k = 0
 2700 GOTO tra4
 2800 X(in1) = X(in1)/A(1)
   GOTO 3200
 2900 Dx(in1) = Dx(in1)/da(1)
   GOTO 3200
 3000 temp = (A(1)*X(in1-1)+A(2)*X(in1))/(A(1)*A(1)+A(2)*A(2))
   X(in1) = (A(1)*X(in1)-A(2)*X(in1-1))/(A(1)*A(1)+A(2)*A(2))
   X(in1-1) = temp
   GOTO 3200
 3100 dtemp = (da(1)*Dx(in1-1)+da(2)*Dx(in1))/(da(1)**2+da(2)**2)
   Dx(in1) = (da(1)*Dx(in1)-da(2)*Dx(in1-1))/(da(1)**2+da(2)**2)
   Dx(in1-1) = dtemp
 3200 k = k + 1
   in1 = in1 + nrow*cmplx
   IF ( k<nn ) GOTO 2700
 3300 IF ( Eol/=0 ) THEN
      j = j - 1
      IF ( j>0 ) GOTO 2500
      CALL close(Fileu(1),Rew)
      GOTO 4100
   ELSE
      CALL zntpki
      i = nrow - Ii + 1
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
 3700 Dx(in1) = Dx(in1) - Dx(in2)*da(1)
   GOTO 4000
 3800 X(in1-1) = X(in1-1) - A(1)*X(in2-1) + A(2)*X(in2)
   X(in1) = X(in1) - A(1)*X(in2) - A(2)*X(in2-1)
   GOTO 4000
 3900 Dx(in1-1) = Dx(in1-1) - da(1)*Dx(in2-1) + da(2)*Dx(in2)
   Dx(in1) = Dx(in1) - da(1)*Dx(in2) - da(2)*Dx(in2-1)
 4000 in1 = in1 + nrow*cmplx
   in2 = in2 + nrow*cmplx
   k = k + 1
   IF ( k<nn ) GOTO 3500
!
!     SUBTRACT OFF REMAINING TERMS
!
   IF ( i>j ) GOTO 2600
   GOTO 3300
!
!     OUTPUT LOAD VECTORS
!
 4100 CALL gopen(Filex,X(iobuf),Wrt)
   l = 1
   Iy = 1
   IF ( formb/=Identy ) nxtnz = X(icol)
   khr = icol
   DO col = fstcol , lstcol
      IF ( formb/=Identy ) THEN
! 593 CONTINUE
         IF ( col<nxtnz ) THEN
            Jy = 1
            CALL pack(zeros,Filex,Filex)
            CYCLE
         ELSEIF ( col/=nxtnz ) THEN
            GOTO 4500
         ENDIF
      ENDIF
      Jy = nrow
      CALL pack(X(l),Filex,Filex)
      l = l + incr
      khr = khr - 1
      IF ( formb/=Identy ) nxtnz = X(khr)
   ENDDO
   IF ( formb/=Identy .AND. khr/=ncol ) THEN
      kerr = 600
      GOTO 4600
   ELSE
      IF ( lstcol==lstlod ) clsop = Rew
      CALL close(Filex,clsop)
      noload = noload - (lstcol-fstcol+1)
      IF ( lstcol==lstlod ) GOTO 4400
      col = lstcol + 1
      IF ( formb==Identy ) GOTO 500
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
