
SUBROUTINE detgbs(Iy,Iobuf,Kcount)
   IMPLICIT NONE
   REAL A(4) , Core(1) , Dum1(23) , Dum2(11) , Dum3(36) , Fa(7) , Fl(7) , Rd , Rdrew , Rew , Wrt , Wrtrew , X(1) , Y(1)
   DOUBLE PRECISION Da(2) , Dx(1) , Dy(1)
   INTEGER Eol , Fc(7) , Ii , Incr , Ipdeta , Itypex , Iunpak , Junpak , Option , Scr3 , Scr4 , Scr6
   COMMON /dcompx/ Fa , Fl , Fc
   COMMON /detmx / Dum3 , Ipdeta
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew
   COMMON /regean/ Dum1 , Scr3 , Scr4 , Dum2 , Scr6
   COMMON /reigkr/ Option
   COMMON /unpakx/ Itypex , Iunpak , Junpak , Incr
   COMMON /zntpkx/ A , Ii , Eol
   COMMON /zzzzzz/ Core
   INTEGER Iy , Kcount
   INTEGER Iobuf(7)
   REAL aden , ai , anum , avalue , sdiag , xmin
   DOUBLE PRECISION dsdiag , dxmin
   INTEGER fileu(7) , i , in1 , in2 , ind , index , ioff , iprec , j , k , lcore , nfile , nrow , parm(4)
!
!     DETGFBS IS A SPECIAL VERSION OF THE DETFBS ROUTINE AND IS USED BY
!     THE REAL DETERMINANT METHOD FOR UNSYMMETRIC DECOMPOSITION.
!     IT IS SUITABLE FOR BOTH SINGLE AND DOUBLE PRECISION OPERATION.
!
!
!     DEFINITION OF PARAMETERS
!     ------------------------
!
!     FILEU  = MATRIX CONTROL BLOCK FOR THE UPPER TRIANGLE
!     FILEV  = SAME AS FILEU
!     FILEVT = MATRIX CONTROL BLOCK FOR THE TRANSPOSE OF THE UPPER
!              TRIANGLE
!     X, DX  = THE SOLUTION VECTOR
!     Y, DY  = REGION USED FOR UNPACKING
!     IY     = POINTER TO Y (DY) RELATIVE TO X (DX)
!     IOBUF  = THE INPUT BUFFER
!     NROW   = MATRIX SIZE
!     KCOUNT = EIGENVALUE COUNTER
!
   !>>>>EQUIVALENCE (Core(1),X(1),Dx(1),Y(1),Dy(1)) , (xmin,dxmin) , (sdiag,dsdiag) , (A(1),Da(1))
   DATA parm(3) , parm(4)/4HDETG , 4HFBS /
!
   fileu(1) = Fc(1)
   CALL rdtrl(fileu)
   Itypex = fileu(5)
   nrow = fileu(2)
   ioff = fileu(7) - 1
   iprec = 1
   IF ( Itypex==2 ) iprec = 2
   index = -1
   Incr = 1
   nfile = fileu(1)
   index = 1
   lcore = Ipdeta - Iy*Itypex - 1
   IF ( lcore<0 ) CALL mesage(-8,0,parm(3))
   nfile = fileu(1)
   parm(2) = nfile
   CALL gopen(nfile,Iobuf,Rdrew)
   xmin = 1.0E20
   IF ( Itypex/=1 ) dxmin = 1.0D20
   DO i = 1 , nrow
      Iunpak = i
      Junpak = i
      ind = nrow - i + 1
      IF ( Itypex/=1 ) THEN
         CALL unpack(*500,nfile,Dx(ind))
         IF ( dxmin>dabs(Dx(ind)) ) dxmin = dabs(Dx(ind))
      ELSE
         CALL unpack(*500,nfile,X(ind))
         IF ( xmin>abs(X(ind)) ) xmin = abs(X(ind))
      ENDIF
   ENDDO
   IF ( Itypex/=1 .OR. xmin==0.0 ) THEN
      IF ( Itypex==1 .OR. dxmin==0.0D0 ) THEN
         xmin = 1.0E20
         IF ( Itypex/=1 ) dxmin = 1.0D20
         DO i = 1 , nrow
            IF ( Itypex/=1 ) THEN
               IF ( Dx(i)/=0.0D0 ) THEN
                  IF ( dxmin>dabs(Dx(i)) ) dxmin = dabs(Dx(i))
               ENDIF
            ELSEIF ( X(i)/=0.0 ) THEN
               IF ( xmin>abs(X(i)) ) xmin = abs(X(i))
            ENDIF
         ENDDO
         IF ( Itypex/=1 ) THEN
            IF ( dxmin>1.0D-8 ) dxmin = 1.0D-8
         ELSE
            IF ( xmin>1.0E-8 ) xmin = 1.0E-8
         ENDIF
      ENDIF
   ENDIF
!
!     BUILD LOAD VECTOR FOR BACKWARD PASS
!
   sdiag = 1.0
   IF ( Itypex/=1 ) dsdiag = 1.0D0
   DO i = 1 , nrow
      anum = (-1)**(i*Kcount)
      ai = i
      aden = 1.0 + (1.0-ai/nrow)*Kcount
      avalue = anum/aden
      IF ( Itypex/=1 ) THEN
         Dx(i) = dxmin*avalue/dsdiag
      ELSE
         X(i) = xmin*avalue/sdiag
      ENDIF
   ENDDO
!
!     BEGIN BACKWARD PASS
!
   CALL rewind(fileu)
   CALL skprec(fileu,1)
   j = nrow
 100  CALL intpk(*600,fileu(1),0,iprec,0)
   IF ( Eol/=0 ) GOTO 600
 200  CALL zntpki
   i = nrow - Ii + 1
   IF ( i/=j ) GOTO 400
!
!     DIVIDE BY THE DIAGONAL
!
   in1 = i
   k = 0
   IF ( iprec==2 ) THEN
      IF ( Da(1)>=0.0D0 .AND. dabs(Da(1))<dxmin ) Da(1) = dxmin
      IF ( Da(1)<0.0D0 .AND. dabs(Da(1))<dxmin ) Da(1) = -dxmin
      Dx(in1) = Dx(in1)/Da(1)
   ELSE
      IF ( A(1)>=0.0 .AND. abs(A(1))<xmin ) A(1) = xmin
      IF ( A(1)<0.0 .AND. abs(A(1))<xmin ) A(1) = -xmin
      X(in1) = X(in1)/A(1)
   ENDIF
 300  IF ( Eol/=0 ) THEN
      j = j - 1
      IF ( j>0 ) GOTO 100
! END OF BACKWARD SUBSTITUTION, NEGATE TERMS AND RETURN
      IF ( iprec==2 ) THEN
         DO k = 1 , nrow
            Dx(k) = -Dx(k)
         ENDDO
      ELSE
         DO k = 1 , nrow
            X(k) = -X(k)
         ENDDO
      ENDIF
      CALL close(fileu,Rew)
      RETURN
   ELSE
      CALL zntpki
      i = nrow - Ii + 1
   ENDIF
 400  in1 = i
   in2 = j
   IF ( i>=j ) THEN
      k = in1
      in1 = in2 - ioff
      in2 = k
   ENDIF
   IF ( iprec==2 ) THEN
      Dx(in1) = Dx(in1) - Dx(in2)*Da(1)
   ELSE
      X(in1) = X(in1) - A(1)*X(in2)
   ENDIF
   in1 = in1 + nrow
   in2 = in2 + nrow
!
!     SUBTRACT OFF REMAINING TERMS
!
   IF ( i<=j ) GOTO 300
   GOTO 200
!
!     ATTEMPT TO OPERATE ON SINGULAR MATRIX
!
 500  parm(1) = -5
   CALL mesage(parm(1),parm(2),parm(3))
 600  CALL mesage(-5,parm(2),parm(3))
END SUBROUTINE detgbs