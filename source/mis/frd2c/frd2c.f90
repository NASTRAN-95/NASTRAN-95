!*==frd2c.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE frd2c(A,B,X,Scr1,Scr2,Scr3,Scr4,Scr5,Nload,Nfreq)
   USE c_frd2bc
   USE c_packx
   USE c_system
   USE c_unpakx
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: A
   INTEGER :: B
   INTEGER :: X
   INTEGER :: Scr1
   INTEGER :: Scr2
   INTEGER :: Scr3
   INTEGER :: Scr4
   INTEGER :: Scr5
   INTEGER :: Nload
   INTEGER :: Nfreq
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ia , ib , ibuf1 , ibuf2 , icore , iopt , ix , j , k , l , m , n , n1 , na , nb , ncore
   INTEGER , DIMENSION(7) :: ta , tb , tx
   REAL , DIMENSION(1) :: zz
   EXTERNAL cfactr , cfbsor , close , cyct2b , gopen , incore , korsz , makmcb , pack , rdtrl , skprec , unpack , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     SOLVE A X = B
!     USE INCORE DECOMP IF POSSIBLE
!
   !>>>>EQUIVALENCE (Zz(1),Z(1))
!
   icore = korsz(z)
   incr = 1
   ii = 1
   inn = 1
   incr1 = 1
   iout = 3
   IF ( ih==0 .AND. iprec==2 ) iout = 4
!
!     IH IN /FRD2BC/ IS INITIALIZED BY ROUTINE FRRD2.
!     (COMPLEX D.P. ARITHMETIC IS USED IF IH=0)
!
   ito = iout
   iti = ito
!
!     DECIDE IF INCORE IS POSSIBLE
!
   ta(1) = A
   CALL rdtrl(ta)
   tb(1) = B
   CALL rdtrl(tb)
   na = ta(2)
   nb = tb(3)*Nload
   ibuf1 = icore - sysbuf
   ncore = na*na*2 + nb*2 + nb*2 + sysbuf
!
!     IF IH=0, COMPLEX D.P. COMPUTATION WILL BE USED.  NOTICE THAT THE
!     ROUTINE INCORE IS WRITTEN ONLY FOR COMPLEX S.P. OPERATION.
!
   IF ( ih/=0 ) THEN
      IF ( ncore<=icore ) THEN
!
!     DO INCORE
!
         ia = 1
         CALL gopen(A,z(ibuf1),0)
         nnn = ta(3)
         incr1 = nnn
         n = na + na
         DO i = 1 , n , 2
            CALL unpack(*10,A,z(i))
            CYCLE
 10         DO k = 1 , n , 2
               l = (k-1)*nnn
               z(i+l) = 0.0
               z(i+l+1) = 0.0
            ENDDO
         ENDDO
         CALL close(A,1)
!
!     GET FREQ FROM B
!
         ib = nnn*nnn*2 + 1
         nnn = tb(3)
         incr1 = Nload
         n1 = nnn + nnn
         j = tb(2)/Nload - 1
         m = 0
         CALL gopen(B,z(ibuf1),0)
         CALL skprec(B,Nfreq-1)
         DO i = 1 , Nload
            spag_nextblock_1 = 1
            SPAG_DispatchLoop_1: DO
               SELECT CASE (spag_nextblock_1)
               CASE (1)
                  CALL unpack(*12,B,z(ib+m))
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
 12               DO k = 1 , n1 , 2
                     l = (k-1)*Nload + ib + m
                     z(l) = 0.0
                     z(l+1) = 0.0
                  ENDDO
                  spag_nextblock_1 = 2
               CASE (2)
                  IF ( i/=Nload ) CALL skprec(B,j)
                  m = m + 2
                  EXIT SPAG_DispatchLoop_1
               END SELECT
            ENDDO SPAG_DispatchLoop_1
         ENDDO
         CALL close(B,1)
         ix = Nload*nnn*2 + ib
         CALL incore(z(ia),na,z(ib),z(ix),Nload)
         nn = na
         CALL gopen(X,z(ibuf1),1)
         CALL makmcb(tx,X,nn,tb(4),ito)
         incr = Nload
         j = ix
         DO i = 1 , Nload
            CALL pack(z(j),X,tx)
            j = j + 2
         ENDDO
         CALL close(X,1)
         CALL wrttrl(tx)
         RETURN
!
!     USE FILE SOLVE
!
      ELSEIF ( ip==0 ) THEN
         ip = ncore - icore
         WRITE (out,99001) uim , ip
99001    FORMAT (A29,' 2437, ADDITIONAL CORE NEEDED FOR IN-CORE ','DECOMPOSITION IN FRRD2 MODULE IS',I8,' WORDS.')
      ENDIF
   ENDIF
   CALL cfactr(A,Scr1,Scr2,Scr3,Scr4,Scr5,iopt)
   icore = korsz(zz)
   ibuf1 = icore - sysbuf
   ibuf2 = ibuf1 - sysbuf
   CALL gopen(B,zz(ibuf1),0)
   CALL gopen(Scr3,zz(ibuf2),1)
   iout = 3
   IF ( ih==0 .AND. iprec==2 ) iout = 4
   incr1 = 1
   j = tb(2)/Nload - 1
   nn = tb(3)
   CALL makmcb(tx,Scr3,nn,tb(4),ito)
   CALL skprec(B,Nfreq-1)
   DO i = 1 , Nload
      CALL cyct2b(B,Scr3,1,zz,tx)
      IF ( i/=Nload ) CALL skprec(B,j)
   ENDDO
   CALL close(Scr3,1)
   CALL close(B,1)
   CALL wrttrl(tx)
   CALL cfbsor(Scr1,Scr2,Scr3,X,iopt)
END SUBROUTINE frd2c
