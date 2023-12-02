!*==clvec.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE clvec(Lamd,Nvect,Phidl,Ih,Ibuf,Ibuf1)
USE C_CDCMPX
USE C_CINVPX
USE C_CINVXX
USE C_NAMES
USE C_PACKX
USE C_SYSTEM
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Lamd
   INTEGER :: Nvect
   INTEGER :: Phidl
   INTEGER , DIMENSION(7) :: Ih
   INTEGER :: Ibuf
   INTEGER :: Ibuf1
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(6) :: buf
   REAL(REAL64) :: di1 , dnrow
   REAL(REAL64) , DIMENSION(1) :: dz
   REAL :: f , fi1 , fnrow
   INTEGER :: flag , i , ibuf2 , j , j2 , n , nrow
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL cdifbs , cinvp1 , cinvp2 , close , cnorm1 , gopen , mesage , pack , read , skprec
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!*****
!     CLVEC CACLULATES THE LEFT EIGENVECTORS FOR THE DETERMINANT AND
!     UPPER HESSENBERG APPROACHES TO THE COMPLEX EIGENVALUE PROBLEM
!*****
   !>>>>EQUIVALENCE (Nrow,Filek(3))
   !>>>>EQUIVALENCE (Dz(1),Z(1))
   DATA name/4HCLVE , 4HC   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!*****
!     INITIALIZATION
!*****
         ibuf2 = Ibuf1 - Sysbuf
         IF ( Fileb(1)<0 ) Fileb(1) = 0
         IF ( Fileb(6)==0 ) Fileb(1) = 0
         DO i = 1 , 11
            Scr(i) = 300 + i
         ENDDO
         Switch = -204
         fnrow = float(nrow)
         dnrow = fnrow
!*****
!     OPEN SORTED EIGENVALUE FILE
!*****
         CALL gopen(Lamd,Z(Ibuf),Rdrew)
         CALL skprec(Lamd,1)
!*****
!     LOOP TO CALCULATE LEFT EIGENVECTORS
!*****
         DO i = 1 , Nvect
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
! READ EIGENVALUE
                  CALL read(*20,*40,Lamd,buf,6,0,flag)
                  Lambda(1) = buf(3)
                  Lambda(2) = buf(4)
                  spag_nextblock_2 = 2
               CASE (2)
! CREATE DYNAMIC MATRIX
                  CALL cinvp1
! DECOMPOSE DYNAMIC MATRIX
                  CALL cinvp2(*2)
! BUILD LOAD FOR FBS
                  fi1 = float(i-1)
                  di1 = fi1
                  j2 = 2*nrow
                  DO j = 1 , j2 , 2
                     f = float((j+1)/2)
                     dz(j) = Mindia/(1.0D0+(1.0D0-f/dnrow)*di1)
                     dz(j+1) = 0.0D0
                  ENDDO
! PERFORM FORWARD-BACKWARD SUBSTITUTION - U(T)*L(T)*PHI
                  CALL cdifbs(dz(1),Z(ibuf2))
! NORMALIZE LEFT EIGENVECTOR
                  CALL cnorm1(dz(1),nrow)
! PACK LEFT EIGENVECTOR ONTO PHIDL
                  It1 = 4
                  It2 = 3
                  Ii = 1
                  Jj = nrow
                  Inc = 1
                  CALL pack(dz(1),Phidl,Ih)
                  CYCLE
! ADJUST CURRENT EIGENVALUE
 2                Lambda(1) = 1.01D0*Lambda(1)
                  Lambda(2) = 1.01D0*Lambda(2)
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
! END OF LOOP
         ENDDO
         CALL close(Lamd,Clsrew)
         RETURN
!*****
!     ERRORS
!*****
 20      n = -2
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 40      n = -3
         spag_nextblock_1 = 2
      CASE (2)
         CALL mesage(n,Lamd,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE clvec
