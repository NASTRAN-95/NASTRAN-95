
SUBROUTINE ortho(U,V,X1,X2,X3,X4,X5,Nz,Ibuf1,Ibuf2,Ibuf3,Ibuf4)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Cdp , Fileb(7) , Filek(7) , Filelm(7) , Filem(7) , Filevc(7) , Ncol , Norew , Northo , Real , Sqr , Sr0fil , Sr5fil
   REAL Csp , Dmpfil , Dum(17) , Eofnrw , Rd , Rdp , Rdrew , Rew , Rsp , Scrfil(10) , Wrt , Wrtrew , Xxxx
   COMMON /cinvpx/ Filek , Filem , Fileb , Filelm , Filevc , Dmpfil , Scrfil
   COMMON /cinvxx/ Dum , Real , Xxxx , Northo
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp , Csp , Cdp , Sqr
!
! Dummy argument declarations
!
   INTEGER Nz
   INTEGER Ibuf1(1) , Ibuf2(1) , Ibuf3(1) , Ibuf4(1)
   DOUBLE PRECISION U(1) , V(1) , X1(1) , X2(1) , X3(1) , X4(1) , X5(1)
!
! Local variable declarations
!
   DOUBLE PRECISION alpha(2) , beta(2) , const1(2) , const2(2) , pj(2)
   REAL flag
   INTEGER i , ifile , k , l7 , ncol2 , ncol4 , no , sub(2)
!
! End of declarations
!
!
!     ORTHO WILL ORTHOGONALIZE THE CURRENT ITERANT WITH RESPECT TO
!     THE PREVIOUSLY EXTRACTED EIGENVECTORS
!
   EQUIVALENCE (Sr0fil,Scrfil(10)) , (Sr5fil,Scrfil(5)) , (Ncol,Filek(2))
   DATA sub/4HORTH , 4HO   /
!
   ncol2 = Ncol + Ncol
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
   CALL open(*100,ifile,Ibuf1,Rdrew)
   ifile = Filevc(1)
   CALL open(*100,ifile,Ibuf2,Rdrew)
   ifile = Sr0fil
   CALL open(*100,ifile,Ibuf3,Rdrew)
   DO k = 1 , Northo
!
!     READ AN EIGENVALUE
!
      ifile = Filelm(1)
      CALL read(*200,*300,ifile,pj(1),4,1,flag)
      const1(1) = -1.D0
      const1(2) = 0.
      CALL csub(X3,X2,X5,pj,const1)
!
!     READ THE RIGHT EIGENVECTOR
!
      ifile = Filevc(1)
      CALL read(*200,*300,ifile,X1(1),ncol4,1,flag)
!
!     READ THE LEFT EIGENVECTOR
!
      ifile = Sr0fil
      CALL read(*200,*300,ifile,X4(1),ncol4,1,flag)
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
99001    FORMAT (4H NUM,2D12.5,6H DENOM,2D12.5,6H ALPHA,2D12.5)
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
   CALL close(Sr0fil,Norew)
   RETURN
!
 100  no = -1
   GOTO 400
 200  no = -2
   GOTO 400
 300  no = -3
 400  CALL mesage(no,ifile,sub)
END SUBROUTINE ortho
