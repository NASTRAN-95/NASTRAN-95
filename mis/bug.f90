
SUBROUTINE bug(Name,Loc,Buf,Nwds)
   IMPLICIT NONE
   INTEGER Ibuf , Nout
   COMMON /system/ Ibuf , Nout
   INTEGER Loc , Nwds
   REAL Buf(1) , Name(3)
   CHARACTER*4 a(28) , blank , xloc
   CHARACTER*8 b(14) , err , zero
   INTEGER i , j , l , limit , line , nwpl
   INTEGER numtyp
!
!     THIS ROUTINE PRINTS NAME,LOC, AND CONTENT OF BUF ARRAY
!     E.G.   CALL BUG ('SUBR ABC',105,CORE(1),120)
!     LIMITED TO 5000 LINES EACH CALL,  14 VALUES PER LINE
!
!     (THIS ROUTINE REPLACES THE OLD ONE IN NASTRAN)
!     WRITTEN BY G.CHAN/SPERRY     MARCH 1986
!
   !>>>>EQUIVALENCE (a(1),b(1))
   DATA line , nwpl , limit/0 , 14 , 5000/
   DATA zero , blank , xloc , err/' 00 ' , '    ' , 'LOC' , '(ERR)'/
!
   CALL sswtch(20,l)
   IF ( l==0 ) RETURN
!
   ENTRY bug1(Name,Loc,Buf,Nwds)
!     ==============================
!
   IF ( Nwds<0 ) RETURN
   l = 2
   i = 0
   CALL a42k8(Name(1),Name(2),b(1))
   CALL int2k8(*300,Loc,a(3))
   a(4) = a(3)
   a(3) = xloc
!
 100  IF ( i>=Nwds ) GOTO 500
 200  i = i + 1
   l = l + 1
   j = numtyp(Buf(i)) + 1
   IF ( j==1 ) THEN
      b(l) = zero
   ELSEIF ( j==2 ) THEN
      CALL int2k8(*300,Buf(i),b(l))
   ELSEIF ( j==3 ) THEN
      CALL fp2k8(*300,Buf(i),b(l))
   ELSEIF ( j==4 ) THEN
      CALL a42k8(Buf(i),Buf(i+1),b(l))
      IF ( numtyp(Buf(i+1))/=3 ) THEN
         a(l*2) = blank
      ELSE
         i = i + 1
      ENDIF
      IF ( i>=Nwds ) GOTO 500
   ELSE
      GOTO 300
   ENDIF
   GOTO 400
!            ZERO,INT,REAL,BCD
 300  b(l) = err
 400  IF ( l<nwpl ) GOTO 100
 500  IF ( l>0 ) WRITE (Nout,99001) (b(j),j=1,l)
99001 FORMAT (2X,14(A8,1X))
   line = line + 1
   IF ( line>limit ) THEN
!
      WRITE (Nout,99002) limit
99002 FORMAT (/2X,'PRINT LINES IN BUG EXCEEDS LIMIT OF',I6)
      GOTO 99999
   ELSE
      l = 0
      IF ( i<Nwds ) GOTO 200
   ENDIF
   RETURN
99999 RETURN
END SUBROUTINE bug