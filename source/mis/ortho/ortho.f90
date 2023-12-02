!*==ortho.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ortho(U,V,X1,X2,X3,X4,X5,Nz,Ibuf1,Ibuf2,Ibuf3,Ibuf4)
USE C_CINVPX
USE C_CINVXX
USE C_NAMES
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: U
   REAL(REAL64) , DIMENSION(1) :: V
   REAL(REAL64) , DIMENSION(1) :: X1
   REAL(REAL64) , DIMENSION(1) :: X2
   REAL(REAL64) , DIMENSION(1) :: X3
   REAL(REAL64) , DIMENSION(1) :: X4
   REAL(REAL64) , DIMENSION(1) :: X5
   INTEGER :: Nz
   INTEGER , DIMENSION(1) :: Ibuf1
   INTEGER , DIMENSION(1) :: Ibuf2
   INTEGER , DIMENSION(1) :: Ibuf3
   INTEGER , DIMENSION(1) :: Ibuf4
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(2) :: alpha , beta , const1 , const2 , pj
   REAL :: flag
   INTEGER :: i , ifile , k , l7 , ncol , ncol2 , ncol4 , no , sr0fil , sr5fil
   INTEGER , DIMENSION(2) , SAVE :: sub
   EXTERNAL close , cmtimu , csub , cxtrny , mesage , open , read , sswtch
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     ORTHO WILL ORTHOGONALIZE THE CURRENT ITERANT WITH RESPECT TO
!     THE PREVIOUSLY EXTRACTED EIGENVECTORS
!
   !>>>>EQUIVALENCE (Sr0fil,Scrfil(10)) , (Sr5fil,Scrfil(5)) , (Ncol,Filek(2))
   DATA sub/4HORTH , 4HO   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         ncol2 = ncol + ncol
         ncol4 = ncol2 + ncol2
         IF ( Fileb(1)==0 ) THEN
            DO i = 1 , ncol2
               X2(i) = 0.D0
            ENDDO
         ELSE
            CALL cmtimu(V,X1,0,Ibuf4)
            CALL cmtimu(U,X2,Fileb,Ibuf4)
         ENDIF
         CALL cmtimu(U,X3,0,Ibuf4)
         CALL sswtch(12,l7)
         const1(1) = 1.0D0
         const1(2) = 0.
         const2(1) = -1.0D0
         const2(2) = 0.
         CALL csub(X1,X2,X2,const1,const2)
!
!     REWIND EIGENVALUE AND EIGENVECTOR FILES
!
         ifile = Filelm(1)
         CALL open(*20,ifile,Ibuf1,Rdrew)
         ifile = Filevc(1)
         CALL open(*20,ifile,Ibuf2,Rdrew)
         ifile = sr0fil
         CALL open(*20,ifile,Ibuf3,Rdrew)
         DO k = 1 , Northo
!
!     READ AN EIGENVALUE
!
            ifile = Filelm(1)
            CALL read(*40,*60,ifile,pj(1),4,1,flag)
            const1(1) = -1.D0
            const1(2) = 0.
            CALL csub(X3,X2,X5,pj,const1)
!
!     READ THE RIGHT EIGENVECTOR
!
            ifile = Filevc(1)
            CALL read(*40,*60,ifile,X1(1),ncol4,1,flag)
!
!     READ THE LEFT EIGENVECTOR
!
            ifile = sr0fil
            CALL read(*40,*60,ifile,X4(1),ncol4,1,flag)
!
            IF ( Fileb(1)/=0 ) THEN
               CALL cxtrny(X4(1),X5(1),const1(1))
            ELSE
!
!    COMPUTE ALPHA USING REAL FORMULA
!
               CALL cxtrny(X4,X3,const1)
            ENDIF
            alpha(1) = const1(1)
            alpha(2) = const1(2)
            beta(1) = alpha(1)*pj(1) - alpha(2)*pj(2)
            beta(2) = alpha(1)*pj(2) + alpha(2)*pj(1)
            IF ( l7/=0 ) THEN
               WRITE (6,99001) const1 , const2 , alpha
99001          FORMAT (4H NUM,2D12.5,6H DENOM,2D12.5,6H ALPHA,2D12.5)
            ENDIF
            DO i = 1 , ncol2 , 2
               U(i) = U(i) - alpha(1)*X1(i) + alpha(2)*X1(i+1)
               U(i+1) = U(i+1) - alpha(2)*X1(i) - alpha(1)*X1(i+1)
               IF ( Fileb(1)/=0 ) THEN
                  V(i) = V(i) - beta(1)*X1(i) + beta(2)*X1(i+1)
                  V(i+1) = V(i+1) - beta(1)*X1(i+1) - beta(2)*X1(i)
               ENDIF
            ENDDO
         ENDDO
         CALL close(Filelm,Norew)
         CALL close(Filevc,Norew)
         CALL close(sr0fil,Norew)
         RETURN
!
 20      no = -1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 40      no = -2
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 60      no = -3
         spag_nextblock_1 = 2
      CASE (2)
         CALL mesage(no,ifile,sub)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ortho
