!*==sdcout.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdcout(Block,Irw,Ac,N,Vecs,Vecd)
USE C_TYPE
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(15) :: Block
   INTEGER :: Irw
   INTEGER , DIMENSION(1) :: Ac
   INTEGER :: N
   REAL , DIMENSION(1) :: Vecs
   REAL(REAL64) , DIMENSION(1) :: Vecd
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ii , j , jj , jstr , k , nbrstr , nstr , prec , rc , type
   REAL , DIMENSION(1) :: xns
   EXTERNAL endput , putstr
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     SDCOUT WRITES A ROW OF A MATRIX IN STRING FORMAT USING
!     PUTSTR/ENDPUT.
!
!     BLOCK = A 15-WORD ARRAY IN WHICH BLOCK(1),(2),(3) HAVE ALREADY
!             BEEN COMPLETED WITH GINO NAME, TYPE AND FORMAT
!     IRW   = ZERO -- ROW NBR OF VECTOR = AC(1)
!           = N.Z. -- ROW NBR OF VECTOR IS IRW
!     AC    = A VECTOR OF N COLUMN POSITIONS (COL NBRS MAY BE .LT. 0)
!     N     = NUMBER OF WORDS IN AC AND NUMBER OF TERMS IN VECS
!     VECS  = A VECTOR OF N TERMS. THE POS OF EACH TERM IS DEFINED
!             BY THE NUMBER STORED IN THE CORRESPONDING POSITION IN AC
!     VECD  = SAME VECTOR AS VECS
!
   !>>>>EQUIVALENCE (Xnd(1),Xns(1))
!
         Block(8) = -1
         Block(12) = Irw
         IF ( Irw==0 ) Block(12) = iabs(Ac(1))
         ii = 0
         type = Block(2)
         rc = Rlcmpx(type)
         prec = Prc(type)
         i = 1
         spag_nextblock_1 = 2
      CASE (2)
!
!     DETERMINE LENGTH OF A STRING BY SCANNING AC
!
         Block(4) = iabs(Ac(i))
         j = Block(4) - i
         k = i + 1
         DO WHILE ( k<=N )
            IF ( iabs(Ac(k))/=j+k ) THEN
               nbrstr = k - i
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ELSE
               k = k + 1
            ENDIF
         ENDDO
         nbrstr = k - i
         spag_nextblock_1 = 3
      CASE (3)
         SPAG_Loop_1_1: DO
!
!     WRITE STRING WITH PUTSTR/ENDPUT
!
            CALL putstr(Block)
            Block(7) = min0(Block(6),nbrstr)
            jstr = Block(5)
            nstr = jstr + rc*Block(7) - 1
            IF ( prec==2 ) THEN
!
               DO jj = jstr , nstr
                  ii = ii + 1
                  Xnd(jj) = Vecd(ii)
               ENDDO
            ELSE
!
               DO jj = jstr , nstr
                  ii = ii + 1
                  xns(jj) = Vecs(ii)
               ENDDO
            ENDIF
!
!     TEST FOR COMPLETION
!
            i = i + Block(7)
            IF ( i>N ) THEN
!
!     END LAST STRING
!
               Block(8) = 1
               CALL endput(Block)
               EXIT SPAG_Loop_1_1
            ELSE
               CALL endput(Block)
               IF ( nbrstr==Block(7) ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               nbrstr = nbrstr - Block(7)
               Block(4) = iabs(Ac(i))
            ENDIF
         ENDDO SPAG_Loop_1_1
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE sdcout
