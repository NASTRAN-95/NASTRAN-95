!*==sdcin.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdcin(Block,Ac,N,Vecs,Vecd)
   USE c_system
   USE c_type
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(15) :: Block
   INTEGER , DIMENSION(1) :: Ac
   INTEGER :: N
   REAL , DIMENSION(1) :: Vecs
   REAL(REAL64) , DIMENSION(1) :: Vecd
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ii , j , jj , jstr , kerr , nn , nstr , prec , rc , type
   REAL , DIMENSION(1) :: xns
   EXTERNAL endget , getstr , mesage
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     SDCIN USES GETSTR/ENDGET TO READ A ROW OF A MATRIX AND ADD THE
!     TERMS OF THE ROW INTO A VECTOR
!
!     BLOCK = A 15-WORD ARRAY IN WHICH BLOCK (1) = GINO NAME
!     AC    = A VECTOR OF N COLUMN POSITIONS (COL NBRS MAY BE .LT. 0)
!     N     = NUMBER OF WORDS IN AC AND NUMBER OF TERMS IN VECS
!     VECS  = A VECTOR OF N TERMS. THE POS OF EACH TERM IS DEFINED BY
!     THE NUMBER STORED IN THE CORRESPONDING POSITION IN AC
!     VECD  = SAME VECTOR AS VECS
!
   !>>>>EQUIVALENCE (Xnd(1),Xns(1))
!
!     PERFORM GENERAL INITIALIZATION
!
         type = Block(2)
         prec = prc(type)
         rc = rlcmpx(type)
         i = 1
         spag_nextblock_1 = 2
      CASE (2)
!
!     LOCATE POSITION IN VECTOR CORRESPONDING TO STRING
!
         IF ( i>N ) THEN
            kerr = 3
         ELSE
            DO j = i , N
               IF ( iabs(Ac(j))==Block(4) ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
!
!     LOGIC ERRORS
!
            kerr = 1
         ENDIF
         spag_nextblock_1 = 4
      CASE (3)
         i = j + Block(6)
         nn = Block(4) + Block(6) - 1
         IF ( iabs(Ac(i-1))/=nn ) THEN
            kerr = 2
         ELSE
!
!     ADD TERMS FROM STRING INTO VECTOR
!
            ii = rc*(j-1)
            jstr = Block(5)
            nstr = jstr + rc*Block(6) - 1
            IF ( prec==2 ) THEN
!
               DO jj = jstr , nstr
                  ii = ii + 1
                  Vecd(ii) = Vecd(ii) + xnd(jj)
               ENDDO
            ELSE
!
               DO jj = jstr , nstr
                  ii = ii + 1
                  Vecs(ii) = Vecs(ii) + xns(jj)
               ENDDO
            ENDIF
!
!     CLOSE CURRENT STRING AND GET NEXT STRING
!
            CALL endget(Block)
            CALL getstr(*99999,Block)
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
         WRITE (nout,99001) kerr
99001    FORMAT (22H0*** SDCIN FATAL ERROR,I2)
         CALL mesage(-61,0,0)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99999 END SUBROUTINE sdcin
