
SUBROUTINE exfort(Rw,U,F,Buf,Nwds,Prec,Dbuf)
   IMPLICIT NONE
   INTEGER Fmt(1) , Fp(5,1) , Lbuf , Nf
   REAL X1(26)
   COMMON /blank / X1 , Lbuf
   COMMON /exio2f/ Fmt
   COMMON /exio2p/ Nf , Fp
   INTEGER F , Nwds , Prec , Rw , U
   INTEGER Buf(Nwds)
   DOUBLE PRECISION Dbuf(1)
   INTEGER frmt(10) , i , ifmt , leof , n
!*****
!
!         *** IBM 360/370, VAX/780 VERSION ***
!
!     EXFORT PERFORMS FORTRAN FORMATTED IO FOR MODULE EXIO
!
!*****
!     COMMON /EXIO2X/  ==> /ZZEXO2/ UNIVAC ONLY
!
   DATA leof/4H&EOF/
!
   IF ( Nwds<=0 ) RETURN
   IF ( F>0 ) THEN
      ifmt = Fp(1,F) - 1
      DO i = 1 , 10
         frmt(i) = Fmt(ifmt+i)
      ENDDO
   ENDIF
   IF ( Rw==2 ) THEN
   ELSEIF ( Rw==3 ) THEN
!
!     POSITION THE FILE
!
      IF ( Nwds==2 .OR. Nwds==3 ) THEN
         n = Lbuf/33 + 1
         DO i = 1 , n
            BACKSPACE U
         ENDDO
         DO
            READ (U,99001) n
            IF ( n==leof ) THEN
               BACKSPACE U
               RETURN
            ENDIF
         ENDDO
      ELSE
         REWIND U
         RETURN
      ENDIF
   ELSEIF ( Rw==4 ) THEN
!
!     WRITE LOGICAL EOF
!
      n = Lbuf/33
      DO i = 1 , n
         WRITE (U,99001) leof
      ENDDO
      GOTO 99999
   ELSEIF ( Prec==1 ) THEN
!
!     READ -- SINGLE PRECISION
!
      READ (U,frmt,ERR=100) Buf
      GOTO 100
   ELSEIF ( Prec==2 ) THEN
!
!     READ -- DOUBLE PRECISION
!
      n = Nwds/3
      READ (U,frmt) (Buf(4*i-3),Dbuf(2*i),i=1,n)
      RETURN
   ENDIF
   IF ( Prec==1 ) THEN
!
!     WRITE -- SINGLE PRECISION
!
      WRITE (U,frmt,ERR=200) Buf
      GOTO 200
   ELSEIF ( Prec==2 ) THEN
!
!     WRITE -- DOUBLE PRECISION
!
      n = Nwds/3
      WRITE (U,frmt) (Buf(4*i-3),Dbuf(2*i),i=1,n)
      RETURN
   ELSE
      READ (U,frmt,ERR=100) Buf
   ENDIF
 100  IF ( Buf(1)==leof ) GOTO 300
   RETURN
 200  RETURN
!
!     END OF FILE
!
 300  Buf(3) = -1
   RETURN
99001 FORMAT (A4,128X)
99999 RETURN
END SUBROUTINE exfort