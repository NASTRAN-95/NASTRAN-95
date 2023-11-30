
SUBROUTINE frd2c(A,B,X,Scr1,Scr2,Scr3,Scr4,Scr5,Nload,Nfreq)
   IMPLICIT NONE
   REAL Dum(52) , Z(1) , Zz(1)
   INTEGER Ih , Ii , Incr , Incr1 , Inn , Iout , Ip , Iprec , Iti , Ito , Nn , Nnn , Out , Sysbuf
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /frd2bc/ Ih , Ip
   COMMON /packx / Iti , Ito , Ii , Nn , Incr
   COMMON /system/ Sysbuf , Out , Dum , Iprec
   COMMON /unpakx/ Iout , Inn , Nnn , Incr1
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zzzzzz/ Z
   INTEGER A , B , Nfreq , Nload , Scr1 , Scr2 , Scr3 , Scr4 , Scr5 , X
   INTEGER i , ia , ib , ibuf1 , ibuf2 , icore , iopt , ix , j , k , l , m , n , n1 , na , nb , ncore , ta(7) , tb(7) , tx(7)
   INTEGER korsz
!
!     SOLVE A X = B
!     USE INCORE DECOMP IF POSSIBLE
!
   !>>>>EQUIVALENCE (Zz(1),Z(1))
!
   icore = korsz(Z)
   Incr = 1
   Ii = 1
   Inn = 1
   Incr1 = 1
   Iout = 3
   IF ( Ih==0 .AND. Iprec==2 ) Iout = 4
!
!     IH IN /FRD2BC/ IS INITIALIZED BY ROUTINE FRRD2.
!     (COMPLEX D.P. ARITHMETIC IS USED IF IH=0)
!
   Ito = Iout
   Iti = Ito
!
!     DECIDE IF INCORE IS POSSIBLE
!
   ta(1) = A
   CALL rdtrl(ta)
   tb(1) = B
   CALL rdtrl(tb)
   na = ta(2)
   nb = tb(3)*Nload
   ibuf1 = icore - Sysbuf
   ncore = na*na*2 + nb*2 + nb*2 + Sysbuf
!
!     IF IH=0, COMPLEX D.P. COMPUTATION WILL BE USED.  NOTICE THAT THE
!     ROUTINE INCORE IS WRITTEN ONLY FOR COMPLEX S.P. OPERATION.
!
   IF ( Ih/=0 ) THEN
      IF ( ncore<=icore ) THEN
!
!     DO INCORE
!
         ia = 1
         CALL gopen(A,Z(ibuf1),0)
         Nnn = ta(3)
         Incr1 = Nnn
         n = na + na
         DO i = 1 , n , 2
            CALL unpack(*10,A,Z(i))
            CYCLE
 10         DO k = 1 , n , 2
               l = (k-1)*Nnn
               Z(i+l) = 0.0
               Z(i+l+1) = 0.0
            ENDDO
         ENDDO
         CALL close(A,1)
!
!     GET FREQ FROM B
!
         ib = Nnn*Nnn*2 + 1
         Nnn = tb(3)
         Incr1 = Nload
         n1 = Nnn + Nnn
         j = tb(2)/Nload - 1
         m = 0
         CALL gopen(B,Z(ibuf1),0)
         CALL skprec(B,Nfreq-1)
         DO i = 1 , Nload
            CALL unpack(*20,B,Z(ib+m))
            GOTO 30
 20         DO k = 1 , n1 , 2
               l = (k-1)*Nload + ib + m
               Z(l) = 0.0
               Z(l+1) = 0.0
            ENDDO
 30         IF ( i/=Nload ) CALL skprec(B,j)
            m = m + 2
         ENDDO
         CALL close(B,1)
         ix = Nload*Nnn*2 + ib
         CALL incore(Z(ia),na,Z(ib),Z(ix),Nload)
         Nn = na
         CALL gopen(X,Z(ibuf1),1)
         CALL makmcb(tx,X,Nn,tb(4),Ito)
         Incr = Nload
         j = ix
         DO i = 1 , Nload
            CALL pack(Z(j),X,tx)
            j = j + 2
         ENDDO
         CALL close(X,1)
         CALL wrttrl(tx)
         GOTO 99999
!
!     USE FILE SOLVE
!
      ELSEIF ( Ip==0 ) THEN
         Ip = ncore - icore
         WRITE (Out,99001) Uim , Ip
99001    FORMAT (A29,' 2437, ADDITIONAL CORE NEEDED FOR IN-CORE ','DECOMPOSITION IN FRRD2 MODULE IS',I8,' WORDS.')
      ENDIF
   ENDIF
   CALL cfactr(A,Scr1,Scr2,Scr3,Scr4,Scr5,iopt)
   icore = korsz(Zz)
   ibuf1 = icore - Sysbuf
   ibuf2 = ibuf1 - Sysbuf
   CALL gopen(B,Zz(ibuf1),0)
   CALL gopen(Scr3,Zz(ibuf2),1)
   Iout = 3
   IF ( Ih==0 .AND. Iprec==2 ) Iout = 4
   Incr1 = 1
   j = tb(2)/Nload - 1
   Nn = tb(3)
   CALL makmcb(tx,Scr3,Nn,tb(4),Ito)
   CALL skprec(B,Nfreq-1)
   DO i = 1 , Nload
      CALL cyct2b(B,Scr3,1,Zz,tx)
      IF ( i/=Nload ) CALL skprec(B,j)
   ENDDO
   CALL close(Scr3,1)
   CALL close(B,1)
   CALL wrttrl(tx)
   CALL cfbsor(Scr1,Scr2,Scr3,X,iopt)
99999 RETURN
END SUBROUTINE frd2c