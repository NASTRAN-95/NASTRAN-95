!*==exfort.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE exfort(Rw,U,F,Buf,Nwds,Prec,Dbuf)
   IMPLICIT NONE
   USE c_blank
   USE c_exio2f
   USE c_exio2p
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Rw
   INTEGER :: U
   INTEGER :: F
   INTEGER , DIMENSION(Nwds) :: Buf
   INTEGER :: Nwds
   INTEGER :: Prec
   REAL*8 , DIMENSION(1) :: Dbuf
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(10) :: frmt
   INTEGER :: i , ifmt , n
   INTEGER , SAVE :: leof
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
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
      ifmt = fp(1,F) - 1
      DO i = 1 , 10
         frmt(i) = fmt(ifmt+i)
      ENDDO
   ENDIF
   IF ( Rw==2 ) THEN
   ELSEIF ( Rw==3 ) THEN
!
!     POSITION THE FILE
!
      IF ( Nwds==2 .OR. Nwds==3 ) THEN
         n = lbuf/33 + 1
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
      n = lbuf/33
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
 100  IF ( Buf(1)==leof ) THEN
!
!     END OF FILE
!
      Buf(3) = -1
      RETURN
   ELSE
      RETURN
   ENDIF
 200  RETURN
99001 FORMAT (A4,128X)
99999 END SUBROUTINE exfort
