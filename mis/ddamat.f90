
SUBROUTINE ddamat
   IMPLICIT NONE
!
! COMMON variable declarations
!
   DOUBLE PRECISION Dz(1)
   REAL Gg , Z(1)
   INTEGER Ibuf(80) , Ii , Iii , Iin , Incr , Iout , Iprec , Jncr , Jout , Nn , Nnn
   COMMON /blank / Gg
   COMMON /packx / Iin , Iout , Ii , Nn , Incr
   COMMON /system/ Ibuf
   COMMON /unpakx/ Jout , Iii , Nnn , Jncr
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER a , b , buf1 , buf2 , buf3 , c , i , inull , isub , j , k , lcore , mcb(7) , nam(2) , ncola , ncolb , nrowa , nrowb
   DOUBLE PRECISION dgg , ggdz
   REAL ggz
   INTEGER korsz
!
! End of declarations
!
!
!     DDAMAT  A,B/C/C,Y,GG=1.  $
!
!     DDAMAT  TAKES THE OUTER PRODUCT OF MATRICES A AND B, AND MULTIPLES
!     BY GG TO GET C, I.E.  CIJ=GG*(AIJ*BIJ).  ALSO, IF B HAS ONLY ONE
!     COLUMN, AND NUMBER OF COLUMNS OF A .GT. 1, THEN USE THAT COLUMN
!     ON EACH COLUMN OF A.
!
   EQUIVALENCE (Iprec,Ibuf(55)) , (Z(1),Dz(1))
   DATA a , b , c/101 , 102 , 201/
   DATA nam/4HDDAM , 4HAT  /
!
!     SET PACK AND UNPACK PARAMETER
!
   Jout = Iprec
   Iin = Iprec
   Iout = Iprec
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
      ELSEIF ( lcore<2*nrowa*Iprec ) THEN
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
               GOTO 99999
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
         IF ( Iprec==2 ) dgg = Gg
!
         CALL gopen(a,Z(buf1),0)
         CALL gopen(b,Z(buf2),0)
         CALL gopen(c,Z(buf3),1)
!
!     UNPACK A COLUMN OF A AND B, COMPUTE PRODUCTS, AND PACK TO C.
!     IF I.GT.1 AND B=1, USE THE ONE COLUMN OF B OVER AGAIN.
!
         DO i = 1 , ncola
!
            inull = 0
            IF ( Iprec==2 ) THEN
               ggdz = dgg
               CALL unpack(*50,a,Dz(1))
               GOTO 60
            ELSE
               ggz = Gg
               CALL unpack(*10,a,Z(1))
               GOTO 20
            ENDIF
 10         inull = 1
 20         IF ( i<=1 .OR. ncolb/=1 ) CALL unpack(*30,b,Z(nrowa+1))
            GOTO 40
 30         inull = 1
            DO j = 1 , nrowa
               Z(nrowa+j) = 0.
            ENDDO
 40         IF ( inull==1 ) ggz = 0.
            DO j = 1 , nrowa
               Z(j) = ggz*Z(j)*Z(nrowa+j)
            ENDDO
            CALL pack(Z(1),c,mcb)
            CYCLE
 50         inull = 1
 60         IF ( i<=1 .OR. ncolb/=1 ) CALL unpack(*70,b,Dz(nrowa+1))
            GOTO 80
 70         inull = 1
            DO j = 1 , nrowa
               Dz(nrowa+j) = 0.D0
            ENDDO
 80         IF ( inull==1 ) ggdz = 0.D0
            DO j = 1 , nrowa
               isub = nrowa + j
               Dz(j) = ggdz*Dz(j)*Dz(isub)
            ENDDO
            CALL pack(Dz(1),c,mcb)
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
99999 END SUBROUTINE ddamat
