!*==detfbs.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE detfbs(Iy,Iobuf,Fileu,Nrow,Kcount)
   USE c_detmx
   USE c_names
   USE c_regean
   USE c_reigkr
   USE c_trnspx
   USE c_unpakx
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iy
   INTEGER , DIMENSION(7) :: Iobuf
   INTEGER , DIMENSION(7) :: Fileu
   INTEGER :: Nrow
   INTEGER :: Kcount
!
! Local variable declarations rewritten by SPAG
!
   REAL :: aden , ai , anum , avalue , sdiag , xmin
   REAL(REAL64) :: dsdiag , dxmin
   REAL(REAL64) , DIMENSION(1) :: dx , dy
   INTEGER :: i , index , isd , ising , ius , j , k , nfile
   INTEGER , DIMENSION(4) , SAVE :: parm
   INTEGER , SAVE :: sdet
   REAL , DIMENSION(1) :: x , y
   EXTERNAL bckrec , close , detgbs , gopen , mesage , trnsp , unpack
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   !>>>>EQUIVALENCE (Core(1),X(1),Dx(1),Y(1),Dy(1)) , (xmin,dxmin) , (sdiag,dsdiag)
   DATA sdet/4HSDET/
   DATA parm(3) , parm(4)/4HDETF , 4HBS  /
!
!WKBI SPR 94011 10/94
   IF ( option/=sdet ) THEN
!WKBNB 10/94 SPR94011
      CALL detgbs(Iy,Iobuf,Kcount)
   ELSE
      itypex = Fileu(5)
      index = -1
      incr = 1
      nfile = Fileu(1)
      IF ( option/=sdet ) THEN
         index = 1
         lcore = ipdeta - Iy*itypex - 1
         IF ( lcore<0 ) CALL mesage(-8,0,parm(3))
         ncr = 2
         scr(1) = scr3
         scr(2) = scr4
         DO i = 1 , 7
            filev(i) = Fileu(i)
            filevt(i) = Fileu(i)
         ENDDO
      ENDIF
      filevt(1) = scr6
      nfile = filevt(1)
      IF ( itypex==1 ) CALL trnsp(y(Iy))
      IF ( itypex/=1 ) CALL trnsp(dy(Iy))
      IF ( itypex==1 ) THEN
         ASSIGN 10 TO isd
         ASSIGN 20 TO ius
      ELSE
         ASSIGN 5 TO isd
         ASSIGN 15 TO ius
      ENDIF
      parm(2) = nfile
      CALL gopen(nfile,Iobuf,rdrew)
      xmin = 1.0E20
      IF ( itypex/=1 ) dxmin = 1.0D20
      DO i = 1 , Nrow
         iunpak = 0
         IF ( itypex/=1 ) THEN
            CALL unpack(*100,nfile,dx(i))
            IF ( dxmin>dabs(dx(i)) ) dxmin = dabs(dx(i))
         ELSE
            CALL unpack(*100,nfile,x(i))
            IF ( xmin>abs(x(i)) ) xmin = abs(x(i))
         ENDIF
      ENDDO
      IF ( itypex/=1 .OR. xmin==0.0 ) THEN
         IF ( itypex==1 .OR. dxmin==0.0D0 ) THEN
            xmin = 1.0E20
            IF ( itypex/=1 ) dxmin = 1.0D20
            DO i = 1 , Nrow
               IF ( itypex/=1 ) THEN
                  IF ( dx(i)/=0.0D0 ) THEN
                     IF ( dxmin>dabs(dx(i)) ) dxmin = dabs(dx(i))
                  ENDIF
               ELSEIF ( x(i)/=0.0 ) THEN
                  IF ( xmin>abs(x(i)) ) xmin = abs(x(i))
               ENDIF
            ENDDO
            IF ( itypex/=1 ) THEN
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
      IF ( itypex/=1 ) dsdiag = 1.0D0
      DO i = 1 , Nrow
         anum = (-1)**(i*Kcount)
         ai = i
         aden = 1.0 + (1.0-ai/Nrow)*Kcount
         avalue = anum/aden
         IF ( itypex/=1 ) THEN
            IF ( option==sdet ) THEN
               dsdiag = dx(i)
               IF ( dx(i)>=0.0 .AND. dabs(dx(i))<dxmin ) dsdiag = dxmin
               IF ( dx(i)<0.0 .AND. dabs(dx(i))<dxmin ) dsdiag = -dxmin
            ENDIF
            dx(i) = dxmin*avalue/dsdiag
         ELSE
            IF ( option==sdet ) THEN
               sdiag = x(i)
               IF ( x(i)>=0.0 .AND. abs(x(i))<xmin ) sdiag = xmin
               IF ( x(i)<0.0 .AND. abs(x(i))<xmin ) sdiag = -xmin
            ENDIF
            x(i) = xmin*avalue/sdiag
         ENDIF
      ENDDO
!
!
!     BEGIN BACKWARD PASS
!
      DO i = 1 , Nrow
         iunpak = 0
         j = Nrow - i + 1
         CALL bckrec(nfile)
         IF ( itypex==1 ) CALL unpack(*100,nfile,y(Iy))
         IF ( itypex/=1 ) CALL unpack(*100,nfile,dy(Iy))
         CALL bckrec(nfile)
         ising = 0
         k = junpak - iunpak + Iy
         GOTO isd
         spag_nextblock_1 = 1
         SPAG_DispatchLoop_1: DO
            SELECT CASE (spag_nextblock_1)
            CASE (1)
               k = k - 1
               junpak = junpak - 1
               IF ( k<Iy ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               GOTO isd
 5             IF ( dy(k)==0.0D0 ) THEN
                  spag_nextblock_1 = 1
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( junpak<j ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( junpak/=j ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!
!     DIVIDE BY THE DIAGONAL TERM
!
               IF ( option/=sdet ) THEN
                  IF ( dy(k)>=0.0D0 .AND. dabs(dy(k))<dxmin ) dy(k) = dxmin
                  IF ( dy(k)<0.0D0 .AND. dabs(dy(k))<dxmin ) dy(k) = -dxmin
                  dx(j) = dx(j)/dy(k)
               ENDIF
               CYCLE
 10            IF ( y(k)==0.0 ) THEN
                  spag_nextblock_1 = 1
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( junpak<j ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( junpak==j ) THEN
                  IF ( option/=sdet ) THEN
                     IF ( y(k)>=0.0 .AND. abs(y(k))<xmin ) y(k) = xmin
                     IF ( y(k)<0.0 .AND. abs(y(k))<xmin ) y(k) = -xmin
                     x(j) = x(j)/y(k)
                  ENDIF
                  CYCLE
               ENDIF
               spag_nextblock_1 = 2
            CASE (2)
               GOTO ius
 15            dx(j) = dx(j) - index*dx(junpak)*dy(k)
               spag_nextblock_1 = 1
               CYCLE SPAG_DispatchLoop_1
 20            x(j) = x(j) - index*x(junpak)*y(k)
               spag_nextblock_1 = 1
            CASE (3)
               IF ( ising/=0 ) EXIT SPAG_DispatchLoop_1
               GOTO 100
            END SELECT
         ENDDO SPAG_DispatchLoop_1
      ENDDO
!
      IF ( option/=sdet ) THEN
         IF ( itypex==1 ) THEN
            DO i = 1 , Nrow
               x(i) = -x(i)
            ENDDO
         ELSE
            DO i = 1 , Nrow
               dx(i) = -dx(i)
            ENDDO
         ENDIF
      ENDIF
      CALL close(nfile,rew)
   ENDIF
!WKBI 10/94 SPR94011
   RETURN
!WKBNE 10/94 SPR94011
!
!     ATTEMPT TO OPERATE ON SINGULAR MATRIX
!
 100  parm(1) = -5
   CALL mesage(parm(1),parm(2),parm(3))
END SUBROUTINE detfbs
