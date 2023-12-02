!*==smleig.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE smleig(D,O,Val)
   USE c_givn
   USE c_packx
   USE c_system
   USE c_unpakx
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(2) :: D
   REAL(REAL64) , DIMENSION(2) :: O
   REAL(REAL64) , DIMENSION(2) :: Val
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: entry , i , ibuf1 , im1 , itra , lama , loc , md , mo , n , nfound , phia , xentry
   INTEGER , DIMENSION(7) , SAVE :: mcb
   REAL(REAL64) :: p , q
   REAL :: valx
   REAL , DIMENSION(30) :: vcom
   EXTERNAL close , gopen , korsz , pack , unpack , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     COMPUTES EIGENVALUES AND VECTORS FOR 1X1 AND 2X2
!
!
!
   !>>>>EQUIVALENCE (Mo,Title(2)) , (Md,Title(3)) , (Entry,Title(11)) , (Xentry,Title(20)) , (Vcom(1),Title(101)) , (N,Vcom(1)) ,        &
!>>>>    & (Lama,Vcom(6)) , (Phia,Vcom(12)) , (Nfound,Vcom(10))
!
   DATA mcb/7*0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     D        ARRAY OF DIAGONALS
!     O        ARRAY OF OFF DIAGONALS
!     VAL      ARRAY OF EIGENVALUES
!     LAMA     FILE OF EIGENVALUES--HEADER,VALUES,ORDER FOUND
!     PHIA     FILE OF  VECTORS   --......,VECTORS-D.P.
!     MO       RESTART TAPE FOR MORE EIGENVALUES
!     MD       INPUT MATRIX
!     N        ORDER OF  PROBLEM
!     NFOUND   NUMBER OF EIGENVALUES/VECTOR PREVIOUSLY FOUND
!
         ibuf1 = (korsz(O)-sysbuf+1)/2 - 1
!
!     OPEN INPUT MATRIX
!
         CALL gopen(md,O(ibuf1),0)
!
!     SETUP FOR UNPACK
!
         it3 = 2
         iii = 1
         jjj = n
         incr1 = 1
         ASSIGN 20 TO itra
         CALL unpack(*60,md,D)
 20      IF ( n==2 ) THEN
!
!     THE MATRIX IS A 2X2
!
            O(1) = D(2)
            O(2) = 0.0D0
            ASSIGN 40 TO itra
            iii = 2
            CALL unpack(*60,md,D(2))
         ELSE
!
!     THE MATRIX IS A 1X1
!
            O(1) = 0.0D0
            Val(1) = D(1)
            loc = 1
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 40      p = D(1) + D(2)
         q = dsqrt(p*p-4.0D0*(D(1)*D(2)-O(1)**2))
         Val(1) = (p+q)/2.0D0
         Val(2) = (p-q)/2.0D0
         loc = 0
         spag_nextblock_1 = 2
      CASE (2)
!
!     WRAP UP ROUTINE
!
         CALL close(md,1)
!
!     COPY D,O,LOC ONTO MO FOR RESTART
!
         CALL gopen(mo,O(ibuf1),1)
!
!     SETUP FOR PACK
!
         im1 = 1
         it1 = 2
         it2 = 2
         ii = 1
         jj = n
         incr = 1
         CALL pack(D,mo,mcb)
         CALL pack(O,mo,mcb)
         CALL write(mo,loc,1,1)
         CALL close(mo,1)
         IF ( n==1 ) THEN
!
!     1X1 WRITE OUT VECTORS AND VALUES
!
            mcb(1) = phia
            mcb(2) = 0
            mcb(3) = 1
            mcb(4) = 2
            mcb(5) = 2
            mcb(6) = 0
            CALL gopen(phia,O(ibuf1),1)
            jj = 1
            CALL pack(1.0D0,phia,mcb)
            CALL close(phia,1)
            CALL wrttrl(mcb(1))
            CALL gopen(lama,O(ibuf1),1)
            IF ( nfound/=0 ) THEN
               DO i = 1 , nfound
                  CALL write(lama,0.0,1,0)
               ENDDO
            ENDIF
            valx = Val(1)
            CALL write(lama,valx,1,1)
            IF ( nfound/=0 ) THEN
               DO i = 1 , nfound
                  CALL write(lama,i,1,0)
               ENDDO
            ENDIF
            CALL write(lama,nfound+1,1,1)
            CALL close(lama,1)
            mcb(1) = lama
            CALL wrttrl(mcb)
         ENDIF
         xentry = -entry
         RETURN
 60      DO i = iii , jjj
            D(i) = 0.0D0
         ENDDO
         GOTO itra
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE smleig
