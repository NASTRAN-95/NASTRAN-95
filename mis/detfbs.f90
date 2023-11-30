
SUBROUTINE detfbs(Iy,Iobuf,Fileu,Nrow,Kcount)
   IMPLICIT NONE
   REAL Core(1) , Dum1(23) , Dum2(11) , Dum3(36) , Rd , Rdrew , Rew , Wrt , Wrtrew , X(1) , Y(1)
   DOUBLE PRECISION Dx(1) , Dy(1)
   INTEGER Filev(7) , Filevt(7) , Incr , Ipdeta , Itypex , Iunpak , Junpak , Lcore , Ncr , Option , Scr(2) , Scr3 , Scr4 , Scr6
   COMMON /detmx / Dum3 , Ipdeta
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew
   COMMON /regean/ Dum1 , Scr3 , Scr4 , Dum2 , Scr6
   COMMON /reigkr/ Option
   COMMON /trnspx/ Filev , Filevt , Lcore , Ncr , Scr
   COMMON /unpakx/ Itypex , Iunpak , Junpak , Incr
   COMMON /zzzzzz/ Core
   INTEGER Iy , Kcount , Nrow
   INTEGER Fileu(7) , Iobuf(7)
   REAL aden , ai , anum , avalue , sdiag , xmin
   DOUBLE PRECISION dsdiag , dxmin
   INTEGER i , index , isd , ising , ius , j , k , nfile , parm(4) , sdet
!
!     DETFBS IS A SPECIAL VERSION OF THE GFBS ROUTINE AND IS USED BY
!     THE REAL DETERMINANT METHOD.  IT IS SUITABLE FOR BOTH SINGLE
!     AND DOUBLE PRECISION OPERATION.
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
   EQUIVALENCE (Core(1),X(1),Dx(1),Y(1),Dy(1)) , (xmin,dxmin) , (sdiag,dsdiag)
   DATA sdet/4HSDET/
   DATA parm(3) , parm(4)/4HDETF , 4HBS  /
!
!WKBI SPR 94011 10/94
   IF ( Option/=sdet ) THEN
!WKBNB 10/94 SPR94011
      CALL detgbs(Iy,Iobuf,Kcount)
   ELSE
      Itypex = Fileu(5)
      index = -1
      Incr = 1
      nfile = Fileu(1)
      IF ( Option/=sdet ) THEN
         index = 1
         Lcore = Ipdeta - Iy*Itypex - 1
         IF ( Lcore<0 ) CALL mesage(-8,0,parm(3))
         Ncr = 2
         Scr(1) = Scr3
         Scr(2) = Scr4
         DO i = 1 , 7
            Filev(i) = Fileu(i)
            Filevt(i) = Fileu(i)
         ENDDO
      ENDIF
      Filevt(1) = Scr6
      nfile = Filevt(1)
      IF ( Itypex==1 ) CALL trnsp(Y(Iy))
      IF ( Itypex/=1 ) CALL trnsp(Dy(Iy))
      IF ( Itypex==1 ) THEN
         ASSIGN 60 TO isd
         ASSIGN 120 TO ius
      ELSE
         ASSIGN 40 TO isd
         ASSIGN 100 TO ius
      ENDIF
      parm(2) = nfile
      CALL gopen(nfile,Iobuf,Rdrew)
      xmin = 1.0E20
      IF ( Itypex/=1 ) dxmin = 1.0D20
      DO i = 1 , Nrow
         Iunpak = 0
         IF ( Itypex/=1 ) THEN
            CALL unpack(*200,nfile,Dx(i))
            IF ( dxmin>dabs(Dx(i)) ) dxmin = dabs(Dx(i))
         ELSE
            CALL unpack(*200,nfile,X(i))
            IF ( xmin>abs(X(i)) ) xmin = abs(X(i))
         ENDIF
      ENDDO
      IF ( Itypex/=1 .OR. xmin==0.0 ) THEN
         IF ( Itypex==1 .OR. dxmin==0.0D0 ) THEN
            xmin = 1.0E20
            IF ( Itypex/=1 ) dxmin = 1.0D20
            DO i = 1 , Nrow
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
      DO i = 1 , Nrow
         anum = (-1)**(i*Kcount)
         ai = i
         aden = 1.0 + (1.0-ai/Nrow)*Kcount
         avalue = anum/aden
         IF ( Itypex/=1 ) THEN
            IF ( Option==sdet ) THEN
               dsdiag = Dx(i)
               IF ( Dx(i)>=0.0 .AND. dabs(Dx(i))<dxmin ) dsdiag = dxmin
               IF ( Dx(i)<0.0 .AND. dabs(Dx(i))<dxmin ) dsdiag = -dxmin
            ENDIF
            Dx(i) = dxmin*avalue/dsdiag
         ELSE
            IF ( Option==sdet ) THEN
               sdiag = X(i)
               IF ( X(i)>=0.0 .AND. abs(X(i))<xmin ) sdiag = xmin
               IF ( X(i)<0.0 .AND. abs(X(i))<xmin ) sdiag = -xmin
            ENDIF
            X(i) = xmin*avalue/sdiag
         ENDIF
      ENDDO
!
!
!     BEGIN BACKWARD PASS
!
      DO i = 1 , Nrow
         Iunpak = 0
         j = Nrow - i + 1
         CALL bckrec(nfile)
         IF ( Itypex==1 ) CALL unpack(*200,nfile,Y(Iy))
         IF ( Itypex/=1 ) CALL unpack(*200,nfile,Dy(Iy))
         CALL bckrec(nfile)
         ising = 0
         k = Junpak - Iunpak + Iy
         GOTO isd
 20      k = k - 1
         Junpak = Junpak - 1
         IF ( k<Iy ) GOTO 140
         GOTO isd
 40      IF ( Dy(k)==0.0D0 ) GOTO 20
         IF ( Junpak<j ) GOTO 140
         IF ( Junpak==j ) THEN
!
!     DIVIDE BY THE DIAGONAL TERM
!
            IF ( Option/=sdet ) THEN
               IF ( Dy(k)>=0.0D0 .AND. dabs(Dy(k))<dxmin ) Dy(k) = dxmin
               IF ( Dy(k)<0.0D0 .AND. dabs(Dy(k))<dxmin ) Dy(k) = -dxmin
               Dx(j) = Dx(j)/Dy(k)
            ENDIF
            CYCLE
         ELSE
            GOTO 80
         ENDIF
 60      IF ( Y(k)==0.0 ) GOTO 20
         IF ( Junpak<j ) GOTO 140
         IF ( Junpak==j ) THEN
            IF ( Option/=sdet ) THEN
               IF ( Y(k)>=0.0 .AND. abs(Y(k))<xmin ) Y(k) = xmin
               IF ( Y(k)<0.0 .AND. abs(Y(k))<xmin ) Y(k) = -xmin
               X(j) = X(j)/Y(k)
            ENDIF
            CYCLE
         ENDIF
 80      GOTO ius
 100     Dx(j) = Dx(j) - index*Dx(Junpak)*Dy(k)
         GOTO 20
 120     X(j) = X(j) - index*X(Junpak)*Y(k)
         GOTO 20
 140     IF ( ising==0 ) GOTO 200
      ENDDO
!
      IF ( Option/=sdet ) THEN
         IF ( Itypex==1 ) THEN
            DO i = 1 , Nrow
               X(i) = -X(i)
            ENDDO
         ELSE
            DO i = 1 , Nrow
               Dx(i) = -Dx(i)
            ENDDO
         ENDIF
      ENDIF
      CALL close(nfile,Rew)
   ENDIF
!WKBI 10/94 SPR94011
   RETURN
!WKBNE 10/94 SPR94011
!
!     ATTEMPT TO OPERATE ON SINGULAR MATRIX
!
 200  parm(1) = -5
   CALL mesage(parm(1),parm(2),parm(3))
END SUBROUTINE detfbs
