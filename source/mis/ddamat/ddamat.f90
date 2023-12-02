!*==ddamat.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ddamat
USE C_BLANK
USE C_PACKX
USE C_SYSTEM
USE C_UNPAKX
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: a , b , c
   INTEGER :: buf1 , buf2 , buf3 , i , inull , iprec , isub , j , k , lcore , ncola , ncolb , nrowa , nrowb
   REAL(REAL64) :: dgg , ggdz
   REAL(REAL64) , DIMENSION(1) :: dz
   REAL :: ggz
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: nam
   EXTERNAL close , gopen , korsz , mesage , pack , rdtrl , unpack , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     DDAMAT  A,B/C/C,Y,GG=1.  $
!
!     DDAMAT  TAKES THE OUTER PRODUCT OF MATRICES A AND B, AND MULTIPLES
!     BY GG TO GET C, I.E.  CIJ=GG*(AIJ*BIJ).  ALSO, IF B HAS ONLY ONE
!     COLUMN, AND NUMBER OF COLUMNS OF A .GT. 1, THEN USE THAT COLUMN
!     ON EACH COLUMN OF A.
!
   !>>>>EQUIVALENCE (Iprec,Ibuf(55)) , (Z(1),Dz(1))
   DATA a , b , c/101 , 102 , 201/
   DATA nam/4HDDAM , 4HAT  /
!
!     SET PACK AND UNPACK PARAMETER
!
   Jout = iprec
   Iin = iprec
   Iout = iprec
   Incr = 1
   Jncr = 1
   Ii = 1
   Iii = 1
!
!     SET OPEN CORE
!
   lcore = korsz(Z)
   buf1 = lcore - Ibuf(1) + 1
   buf2 = buf1 - Ibuf(1)
   buf3 = buf2 - Ibuf(1)
   lcore = buf3 - 1
   IF ( lcore<=0 ) THEN
      k = -8
      CALL mesage(k,0,nam)
   ELSE
!
      mcb(1) = a
      CALL rdtrl(mcb)
      ncola = mcb(2)
      nrowa = mcb(3)
      mcb(1) = b
      CALL rdtrl(mcb)
      ncolb = mcb(2)
      nrowb = mcb(3)
      IF ( nrowa/=nrowb ) THEN
!
!     FATAL ERROR MESSAGE
!
         k = -7
         CALL mesage(k,0,nam)
      ELSEIF ( lcore<2*nrowa*iprec ) THEN
         k = -8
         CALL mesage(k,0,nam)
      ELSE
!
!     NO. OF COLUMNS OF A AND B MUST BE EQUAL OR
!     NO. OF COLUMNS OF B MUST BE 1
!
         IF ( ncola/=ncolb ) THEN
            IF ( ncolb/=1 ) THEN
               k = -7
               CALL mesage(k,0,nam)
               RETURN
            ENDIF
         ENDIF
!
         Nn = nrowa
         Nnn = nrowa
         mcb(1) = c
         mcb(2) = 0
         mcb(3) = nrowa
         mcb(6) = 0
         mcb(7) = 0
         IF ( iprec==2 ) dgg = Gg
!
         CALL gopen(a,Z(buf1),0)
         CALL gopen(b,Z(buf2),0)
         CALL gopen(c,Z(buf3),1)
!
!     UNPACK A COLUMN OF A AND B, COMPUTE PRODUCTS, AND PACK TO C.
!     IF I.GT.1 AND B=1, USE THE ONE COLUMN OF B OVER AGAIN.
!
         DO i = 1 , ncola
            spag_nextblock_1 = 1
            SPAG_DispatchLoop_1: DO
               SELECT CASE (spag_nextblock_1)
               CASE (1)
!
                  inull = 0
                  IF ( iprec==2 ) THEN
                     ggdz = dgg
                     CALL unpack(*6,a,dz(1))
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     ggz = Gg
                     CALL unpack(*2,a,Z(1))
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
 2                inull = 1
                  spag_nextblock_1 = 2
               CASE (2)
                  IF ( i<=1 .OR. ncolb/=1 ) CALL unpack(*4,b,Z(nrowa+1))
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
 4                inull = 1
                  DO j = 1 , nrowa
                     Z(nrowa+j) = 0.
                  ENDDO
                  spag_nextblock_1 = 3
               CASE (3)
                  IF ( inull==1 ) ggz = 0.
                  DO j = 1 , nrowa
                     Z(j) = ggz*Z(j)*Z(nrowa+j)
                  ENDDO
                  CALL pack(Z(1),c,mcb)
                  CYCLE
 6                inull = 1
                  spag_nextblock_1 = 4
               CASE (4)
                  IF ( i<=1 .OR. ncolb/=1 ) CALL unpack(*8,b,dz(nrowa+1))
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
 8                inull = 1
                  DO j = 1 , nrowa
                     dz(nrowa+j) = 0.D0
                  ENDDO
                  spag_nextblock_1 = 5
               CASE (5)
                  IF ( inull==1 ) ggdz = 0.D0
                  DO j = 1 , nrowa
                     isub = nrowa + j
                     dz(j) = ggdz*dz(j)*dz(isub)
                  ENDDO
                  CALL pack(dz(1),c,mcb)
                  EXIT SPAG_DispatchLoop_1
               END SELECT
            ENDDO SPAG_DispatchLoop_1
!
!     DO ANOTHER COLUMN
!
         ENDDO
!
         CALL wrttrl(mcb)
         CALL close(a,1)
         CALL close(b,1)
         CALL close(c,1)
         RETURN
      ENDIF
   ENDIF
END SUBROUTINE ddamat
