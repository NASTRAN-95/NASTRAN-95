
SUBROUTINE sdcout(Block,Irw,Ac,N,Vecs,Vecd)
   IMPLICIT NONE
   INTEGER Prc(2) , Rlcmpx(4) , Words(4)
   DOUBLE PRECISION Xnd(1)
   REAL Xns(1)
   COMMON /type  / Prc , Words , Rlcmpx
   COMMON /zzzzzz/ Xnd
   INTEGER Irw , N
   INTEGER Ac(1) , Block(15)
   DOUBLE PRECISION Vecd(1)
   REAL Vecs(1)
   INTEGER i , ii , j , jj , jstr , k , nbrstr , nstr , prec , rc , type
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
   EQUIVALENCE (Xnd(1),Xns(1))
!
   Block(8) = -1
   Block(12) = Irw
   IF ( Irw==0 ) Block(12) = iabs(Ac(1))
   ii = 0
   type = Block(2)
   rc = Rlcmpx(type)
   prec = Prc(type)
   i = 1
!
!     DETERMINE LENGTH OF A STRING BY SCANNING AC
!
 100  Block(4) = iabs(Ac(i))
   j = Block(4) - i
   k = i + 1
   DO WHILE ( k<=N )
      IF ( iabs(Ac(k))/=j+k ) THEN
         nbrstr = k - i
         GOTO 200
      ELSE
         k = k + 1
      ENDIF
   ENDDO
   nbrstr = k - i
 200  DO
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
            Xns(jj) = Vecs(ii)
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
         EXIT
      ELSE
         CALL endput(Block)
         IF ( nbrstr==Block(7) ) GOTO 100
         nbrstr = nbrstr - Block(7)
         Block(4) = iabs(Ac(i))
      ENDIF
   ENDDO
END SUBROUTINE sdcout
