!*==detgbs.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE detgbs(Iy,Iobuf,Kcount)
USE C_DCOMPX
USE C_DETMX
USE C_NAMES
USE C_REGEAN
USE C_REIGKR
USE C_UNPAKX
USE C_ZNTPKX
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iy
   INTEGER , DIMENSION(7) :: Iobuf
   INTEGER :: Kcount
!
! Local variable declarations rewritten by SPAG
!
   REAL :: aden , ai , anum , avalue , sdiag , xmin
   REAL(REAL64) , DIMENSION(2) :: da
   REAL(REAL64) :: dsdiag , dxmin
   REAL(REAL64) , DIMENSION(1) :: dx
   INTEGER , DIMENSION(7) :: fileu
   INTEGER :: i , in1 , in2 , ind , index , ioff , iprec , j , k , lcore , nfile , nrow
   INTEGER , DIMENSION(4) , SAVE :: parm
   REAL , DIMENSION(1) :: x
   EXTERNAL close , gopen , intpk , mesage , rdtrl , rewind , skprec , unpack , zntpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
               CALL unpack(*20,nfile,dx(ind))
               IF ( dxmin>dabs(dx(ind)) ) dxmin = dabs(dx(ind))
            ELSE
               CALL unpack(*20,nfile,x(ind))
               IF ( xmin>abs(x(ind)) ) xmin = abs(x(ind))
            ENDIF
         ENDDO
         IF ( Itypex/=1 .OR. xmin==0.0 ) THEN
            IF ( Itypex==1 .OR. dxmin==0.0D0 ) THEN
               xmin = 1.0E20
               IF ( Itypex/=1 ) dxmin = 1.0D20
               DO i = 1 , nrow
                  IF ( Itypex/=1 ) THEN
                     IF ( dx(i)/=0.0D0 ) THEN
                        IF ( dxmin>dabs(dx(i)) ) dxmin = dabs(dx(i))
                     ENDIF
                  ELSEIF ( x(i)/=0.0 ) THEN
                     IF ( xmin>abs(x(i)) ) xmin = abs(x(i))
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
               dx(i) = dxmin*avalue/dsdiag
            ELSE
               x(i) = xmin*avalue/sdiag
            ENDIF
         ENDDO
!
!     BEGIN BACKWARD PASS
!
         CALL rewind(fileu)
         CALL skprec(fileu,1)
         j = nrow
         spag_nextblock_1 = 2
      CASE (2)
         CALL intpk(*40,fileu(1),0,iprec,0)
         IF ( Eol/=0 ) GOTO 40
         spag_nextblock_1 = 3
      CASE (3)
         CALL zntpki
         i = nrow - Ii + 1
         IF ( i/=j ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     DIVIDE BY THE DIAGONAL
!
         in1 = i
         k = 0
         IF ( iprec==2 ) THEN
            IF ( da(1)>=0.0D0 .AND. dabs(da(1))<dxmin ) da(1) = dxmin
            IF ( da(1)<0.0D0 .AND. dabs(da(1))<dxmin ) da(1) = -dxmin
            dx(in1) = dx(in1)/da(1)
         ELSE
            IF ( A(1)>=0.0 .AND. abs(A(1))<xmin ) A(1) = xmin
            IF ( A(1)<0.0 .AND. abs(A(1))<xmin ) A(1) = -xmin
            x(in1) = x(in1)/A(1)
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
         IF ( Eol/=0 ) THEN
            j = j - 1
            IF ( j>0 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
! END OF BACKWARD SUBSTITUTION, NEGATE TERMS AND RETURN
            IF ( iprec==2 ) THEN
               DO k = 1 , nrow
                  dx(k) = -dx(k)
               ENDDO
            ELSE
               DO k = 1 , nrow
                  x(k) = -x(k)
               ENDDO
            ENDIF
            CALL close(fileu,Rew)
            RETURN
         ELSE
            CALL zntpki
            i = nrow - Ii + 1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         in1 = i
         in2 = j
         IF ( i>=j ) THEN
            k = in1
            in1 = in2 - ioff
            in2 = k
         ENDIF
         IF ( iprec==2 ) THEN
            dx(in1) = dx(in1) - dx(in2)*da(1)
         ELSE
            x(in1) = x(in1) - A(1)*x(in2)
         ENDIF
         in1 = in1 + nrow
         in2 = in2 + nrow
!
!     SUBTRACT OFF REMAINING TERMS
!
         IF ( i>j ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
!
!     ATTEMPT TO OPERATE ON SINGULAR MATRIX
!
 20      parm(1) = -5
         CALL mesage(parm(1),parm(2),parm(3))
 40      CALL mesage(-5,parm(2),parm(3))
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE detgbs
