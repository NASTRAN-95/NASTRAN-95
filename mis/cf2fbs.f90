
SUBROUTINE cf2fbs(Tpose,Xout,Iobuf)
   IMPLICIT NONE
   REAL Aadum(117) , Csp , Eofnrw , Rd , Rdp , Rdrew , Rew , Rsp , Wrt , Wrtrew , Xcd01(4) , Xcd02(9) , Xcd03(6)
   INTEGER Cdp , Eol , Ii , Iscr6 , Ksystm , Mcblt(7) , Mcbut(7) , Norew , Nout , Nswp
   DOUBLE PRECISION Da(2)
   LOGICAL Qpr , Symmet
   COMMON /feeraa/ Aadum , Mcblt , Mcbut
   COMMON /feerxc/ Xcd01 , Symmet , Xcd02 , Nswp , Xcd03 , Qpr
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp , Csp , Cdp
   COMMON /system/ Ksystm , Nout
   COMMON /zntpkx/ Da , Ii , Eol
   INTEGER Iobuf(1)
   LOGICAL Tpose(1)
   DOUBLE PRECISION Xout(1)
   DOUBLE PRECISION dtemp , unidum
   INTEGER i , i1 , i2 , ii1 , ii2 , in1 , in2 , intchn , ioff , j , j1 , j2 , junk , k , mcsave , name(2)
!*******
!     CF2FBS PERFORMS THE DOUBLE-PRECISION FORWARD AND BACKWARD SWEEPS
!     FOR THE COMPLEX FEER METHOD. THESE SWEEPS CONSTITUTE THE
!     OPERATIONAL INVERSE (MATRIX INVERSION).
!*******
!     DEFINITION OF INPUT AND OUTPUT PARAMETERS
!*******
!     TPOSE    = .FALSE. --- PERFORM OPERATION L * U
!              = .TRUE.  --- PERFORM OPERATION U-TRANSPOSE * L-TRANSPOSE
!     XOUT     = INPUT VECTOR GETS TRANSFORMED TO OUTPUT VECTOR
!     IOBUF    = INPUT  GINO BUFFER
!*******
   !>>>>EQUIVALENCE (Aadum(42),Iscr6)
   DATA name/4HCF2F , 4HBS  /
!
   IF ( Qpr ) WRITE (Nout,99001) Tpose(1) , Symmet , Nswp , Iscr6
99001 FORMAT (1H0,12HENTER CF2FBS,8X,11HTRANSPOSE =,L2,L9,2I10)
   junk = 0
   IF ( Tpose(1) .AND. .NOT.Symmet ) THEN
!*******
!     BELOW FOR OPERATION U-TRANSPOSE * L-TRANSPOSE
!     (LOGIC COPIED FROM SUBROUTINE CDIFBS)
!*******
!     BEGIN THE FORWARD PASS USING THE UPPER TRIANGLE
!*******
      ioff = Mcbut(7) - 1
      IF ( Qpr ) WRITE (Nout,99002) ioff
99002 FORMAT (1H ,30X,6HIOFF =,I10)
      mcsave = Mcbut(1)
      Mcbut(1) = Iscr6
      CALL gopen(Mcbut(1),Iobuf(1),Rdrew)
      DO i = 1 , Nswp
         IF ( Qpr ) WRITE (Nout,99010) i
         j = i + i
         CALL intpk(*50,Mcbut(1),0,Cdp,0)
 20      CALL zntpki
         IF ( Qpr ) WRITE (Nout,99011) Ii , Eol , Da
         IF ( Ii<i ) THEN
!*******
!     SUBTRACT OFF NORMAL TERM
!*******
            i2 = Ii + Ii
            i1 = i2 - 1
            j1 = j - 1
            Xout(j1) = Xout(j1) - Xout(i1)*Da(1) + Xout(i2)*Da(2)
            Xout(j) = Xout(j) - Xout(i1)*Da(2) - Xout(i2)*Da(1)
         ELSEIF ( Ii==i ) THEN
!*******
!     DIVIDE BY THE DIAGONAL
!*******
            i1 = j - 1
            unidum = 1.D0/(Da(1)**2+Da(2)**2)
            dtemp = (Xout(i1)*Da(1)+Xout(j)*Da(2))*unidum
            Xout(j) = (Xout(j)*Da(1)-Xout(i1)*Da(2))*unidum
            Xout(i1) = dtemp
            IF ( Qpr ) WRITE (Nout,99013)
         ELSE
!*******
!     SUBTRACT OFF ACTIVE COLUMN TERMS
!*******
            k = (i-ioff)*2
            junk = 1
            in1 = k
            IF ( in1<=0 ) GOTO 1000
            i2 = Ii + Ii
            i1 = i2 - 1
            j1 = k - 1
            Xout(i1) = Xout(i1) - Xout(j1)*Da(1) + Xout(k)*Da(2)
            Xout(i2) = Xout(i2) - Xout(k)*Da(1) - Xout(j1)*Da(2)
         ENDIF
         IF ( Eol==0 ) GOTO 20
 50   ENDDO
      CALL close(Mcbut(1),Rew)
      Mcbut(1) = mcsave
!*******
!     BEGIN BACKWARD PASS USING THE LOWER TRIANGLE
!*******
      CALL gopen(Mcblt(1),Iobuf(1),Rdrew)
      CALL skprec(Mcblt(1),Nswp)
      DO i = 1 , Nswp
         IF ( Qpr ) WRITE (Nout,99010) i
         CALL bckrec(Mcblt(1))
         intchn = 0
         CALL intpk(*80,Mcblt(1),0,Cdp,0)
         j = (Nswp-i+1)*2
 60      CALL zntpki
         IF ( Qpr ) WRITE (Nout,99011) Ii , Eol , Da
         IF ( Ii/=Nswp-i+1 ) THEN
            j1 = j - 1
            i2 = Ii + Ii
            i1 = i2 - 1
            Xout(j1) = Xout(j1) - Xout(i1)*Da(1) + Xout(i2)*Da(2)
            Xout(j) = Xout(j) - Xout(i1)*Da(2) - Xout(i2)*Da(1)
         ELSE
            IF ( Ii<j/2 ) GOTO 900
!*******
!     PERFORM THE INTERCHANGE
!*******
            intchn = ifix(sngl(Da(1)))*2
            IF ( Qpr ) WRITE (Nout,99003) intchn
99003       FORMAT (1H ,4X,11HINTERCHANGE,I6)
         ENDIF
         IF ( Eol==0 ) GOTO 60
         IF ( intchn>0 ) THEN
            in1 = j + intchn
            IF ( Qpr ) WRITE (Nout,99004) j , intchn , in1
99004       FORMAT (1H ,15X,3I6)
            dtemp = Xout(j)
            Xout(j) = Xout(in1)
            Xout(in1) = dtemp
            j1 = j - 1
            i1 = in1 - 1
            dtemp = Xout(j1)
            Xout(j1) = Xout(i1)
            Xout(i1) = dtemp
         ENDIF
 80      CALL bckrec(Mcblt(1))
      ENDDO
      CALL close(Mcblt(1),Rew)
      GOTO 1200
   ELSE
!*******
!     BELOW FOR OPERATION L * U
!     (LOGIC COPIED FROM SUBROUTINE CINFBS)
!*******
!     BEGIN FORWARD PASS USING THE LOWER TRIANGLE
!*******
      CALL gopen(Mcblt(1),Iobuf(1),Rdrew)
      j = 1
      CALL intpk(*400,Mcblt(1),0,Cdp,0)
   ENDIF
 100  DO WHILE ( Eol==0 )
      CALL zntpki
      IF ( Qpr ) WRITE (Nout,99012) Da , Ii , Eol , j
      IF ( j<Ii ) GOTO 300
      IF ( j==Ii ) THEN
!*******
!     PERFORM THE REQUIRED ROW INTERCHANGE
!*******
         in1 = (j+ifix(sngl(Da(1))))*2 - 1
         IF ( Qpr ) WRITE (Nout,99005) in1 , Eol
99005    FORMAT (1H ,3X,5HIN1 =,I6,4X,5HEOL =,I2)
         in2 = in1 + 1
         j2 = 2*j
         unidum = Xout(j2)
         Xout(j2) = Xout(in2)
         Xout(in2) = unidum
         j2 = j2 - 1
         unidum = Xout(j2)
         Xout(j2) = Xout(in1)
         Xout(in1) = unidum
         GOTO 200
      ENDIF
   ENDDO
   GOTO 900
 200  IF ( Eol/=0 ) GOTO 400
   CALL zntpki
   IF ( Qpr ) WRITE (Nout,99012) Da , Ii , Eol , j
 300  ii2 = 2*Ii
   ii1 = ii2 - 1
   j2 = 2*j
   j1 = j2 - 1
   Xout(ii1) = Xout(ii1) - Da(1)*Xout(j1) + Da(2)*Xout(j2)
   Xout(ii2) = Xout(ii2) - Da(2)*Xout(j1) - Da(1)*Xout(j2)
   GOTO 200
 400  j = j + 1
   IF ( j<Nswp ) THEN
      CALL intpk(*400,Mcblt(1),0,Cdp,0)
      GOTO 100
   ELSE
      CALL close(Mcblt(1),Rew)
!*******
!     BEGIN BACKWARD PASS USING THE UPPER TRIANGLE
!*******
      ioff = Mcbut(7) - 1
      IF ( Qpr ) WRITE (Nout,99006) ioff , Mcblt , Mcbut
99006 FORMAT (1H ,15(1X,I7))
      CALL gopen(Mcbut(1),Iobuf(1),Rdrew)
      j = Nswp
   ENDIF
 500  CALL intpk(*1000,Mcbut(1),0,Cdp,0)
   IF ( Eol/=0 ) GOTO 1000
 600  CALL zntpki
   IF ( Qpr ) WRITE (Nout,99012) Da , Ii , Eol , j
   i = Nswp - Ii + 1
   IF ( i/=j ) GOTO 800
!*******
!     DIVIDE BY THE DIAGONAL
!*******
   i2 = 2*i
   i1 = i2 - 1
   unidum = 1.D0/(Da(1)**2+Da(2)**2)
   dtemp = (Da(1)*Xout(i1)+Da(2)*Xout(i2))*unidum
   Xout(i2) = (Da(1)*Xout(i2)-Da(2)*Xout(i1))*unidum
   Xout(i1) = dtemp
   IF ( Qpr ) WRITE (Nout,99013)
!*******
!     SUBTRACT OFF REMAINING TERMS
!*******
 700  IF ( i>j ) GOTO 600
   IF ( Eol/=0 ) THEN
      j = j - 1
      IF ( j>0 ) GOTO 500
      CALL close(Mcbut(1),Rew)
      GOTO 1200
   ELSE
      CALL zntpki
      IF ( Qpr ) WRITE (Nout,99012) Da , Ii , Eol , j
      i = Nswp - Ii + 1
   ENDIF
 800  in1 = i
   in2 = j
   IF ( i>=j ) THEN
      k = in1
      in1 = in2 - ioff
      junk = 1
      IF ( in1<=0 ) GOTO 1000
      in2 = k
   ENDIF
   in1 = 2*in1
   in2 = 2*in2
   ii1 = in1 - 1
   ii2 = in2 - 1
   IF ( Qpr ) WRITE (Nout,99007) i , j , ii1 , ii2
99007 FORMAT (1H ,3HI =,I6,6X,3HJ =,I6,6X,5HII1 =,I6,6X,5HII2 =,I6)
   Xout(ii1) = Xout(ii1) - Da(1)*Xout(ii2) + Da(2)*Xout(in2)
   Xout(in1) = Xout(in1) - Da(2)*Xout(ii2) - Da(1)*Xout(in2)
   GOTO 700
 900  j = Mcblt(1)
   GOTO 1100
 1000 j = Mcbut(1)
 1100 CALL mesage(-5,j,name)
 1200 IF ( Qpr .AND. junk==0 ) WRITE (Nout,99008)
99008 FORMAT (1H0,30X,13HIOFF NOT USED,/1H )
   IF ( Qpr .AND. junk/=0 ) WRITE (Nout,99009)
99009 FORMAT (1H0,30X,13HIOFF WAS USED,/1H )
99010 FORMAT (1H ,12HLOOP INDEX =,I6)
99011 FORMAT (1H ,4HII =,I14,6X,5HEOL =,I2,8X,4HDA =,2D16.8)
99012 FORMAT (1H ,4HDA =,2D16.8,4X,4HII =,I6,4X,5HEOL =,I2,4X,3HJ =,I4)
99013 FORMAT (1H ,6X,8HDIAGONAL)
END SUBROUTINE cf2fbs