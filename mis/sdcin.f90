
SUBROUTINE sdcin(Block,Ac,N,Vecs,Vecd)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Nout , Prc(2) , Rlcmpx(4) , Words(4)
   REAL Sysbuf , Xns(1)
   DOUBLE PRECISION Xnd(1)
   COMMON /system/ Sysbuf , Nout
   COMMON /type  / Prc , Words , Rlcmpx
   COMMON /zzzzzz/ Xnd
!
! Dummy argument declarations
!
   INTEGER N
   INTEGER Ac(1) , Block(15)
   DOUBLE PRECISION Vecd(1)
   REAL Vecs(1)
!
! Local variable declarations
!
   INTEGER i , ii , j , jj , jstr , kerr , nn , nstr , prec , rc , type
!
! End of declarations
!
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
   EQUIVALENCE (Xnd(1),Xns(1))
!
!     PERFORM GENERAL INITIALIZATION
!
   type = Block(2)
   prec = Prc(type)
   rc = Rlcmpx(type)
   i = 1
!
!     LOCATE POSITION IN VECTOR CORRESPONDING TO STRING
!
 100  IF ( i>N ) THEN
      kerr = 3
   ELSE
      DO j = i , N
         IF ( iabs(Ac(j))==Block(4) ) GOTO 200
      ENDDO
!
!     LOGIC ERRORS
!
      kerr = 1
   ENDIF
   GOTO 300
 200  i = j + Block(6)
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
            Vecd(ii) = Vecd(ii) + Xnd(jj)
         ENDDO
      ELSE
!
         DO jj = jstr , nstr
            ii = ii + 1
            Vecs(ii) = Vecs(ii) + Xns(jj)
         ENDDO
      ENDIF
!
!     CLOSE CURRENT STRING AND GET NEXT STRING
!
      CALL endget(Block)
      CALL getstr(*99999,Block)
      GOTO 100
   ENDIF
 300  WRITE (Nout,99001) kerr
99001 FORMAT (22H0*** SDCIN FATAL ERROR,I2)
   CALL mesage(-61,0,0)
99999 RETURN
END SUBROUTINE sdcin
