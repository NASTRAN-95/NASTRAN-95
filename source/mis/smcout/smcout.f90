!*==smcout.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE smcout(Zi,Zr,Zd,Zrs,Zrd)
   IMPLICIT NONE
   USE I_SMCOMX
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(10) :: Zi
   REAL , DIMENSION(10) :: Zr
   REAL*8 , DIMENSION(10) :: Zd
   REAL , DIMENSION(10) :: Zrs
   REAL*8 , DIMENSION(10) :: Zrd
!
! Local variable declarations rewritten by SPAG
!
   REAL :: akk , akk2 , akki , akkr , rs
   REAL*8 :: dakk , dakk2 , dakki , dakkr , dr , dsc , dsr , minds
   INTEGER :: i , istore , itwrds , jj , jstr , kdir , km2 , kmidx , kridn , kvidx , nrowm , nrowm1 , nrows , nstr , nwds
   REAL , SAVE :: rzero
   REAL*8 , DIMENSION(10) :: xnd
!
! End of declarations rewritten by SPAG
!
!
! SMCOUT DOES THE FINAL DIVISION OF THE TERMS OF THE PIVOTAL COLUMN
! AND WRITES THE COLUMN DATA TO THE LOWER TRIANGULAR MATRIX.
!
! THE FOLLOWING CALCULATIONS ARE DONE IN SUBROUTINE SMC2-RS,RD,CS,CD
!
!      do 100 k = 1,n
!         do 10  i = k,n
!         temp = 0.
!         do 5  l = 1,k-1
!            temp = temp + a(i,l)*a(k,l) / a(l,l)
!    5       continue
!         a(i,k) = a(i,k) - temp
!   10    continue
!
!            THE FOLLOWING LAST COMPUTATION TAKES PLACE IN THIS SUBROUTINE.
!            THE RESULTS OF THE DIVISION ARE WRITTEN TO THE OUTPUT FILE BUT
!            THE RESULTS OF THE ABOVE (WITHOUT THE DIVISION BELOW) IS
!            MAINTAINED IN MEMORY FOR REMAINING COLUMN COMPUTATIONS.
!
!         do 11 j = k+1,n
!           a(k,j) = a(j,k) / a( k,k )
!   11      continue
!  100 continue
!
!  THE FINAL COMPUTATIONS ARE WRITTEN TO THE LLL MATRIX USING PUTSTR/ENDPUT.
!
   !>>>>EQUIVALENCE (Xns,Xnd)
   !>>>>EQUIVALENCE (Minds,Mindd) , (Dsr,Ddr) , (Dsc,Ddc)
   DATA rzero/1.0E-10/
!      PRINT *,' SMCOUT-ENTER,KCOL=',KCOL
   kdir = (kcol-1)*4 + 1
   kmidx = Zi(kdir)
   kridx = kmidx + 4
   km2 = Zi(kmidx+1)
   kridn = kridx + km2
   nrows = 0
   DO i = 1 , km2 , 2
      nrows = nrows + Zi(kridx+i)
   ENDDO
   kvidx = kridx + km2
   IF ( ktype==2 ) THEN
!
! DO DIVISION IN REAL DOUBLE PRECISION AND COMPUTE THE  DETERMINANT DDR.
! CHECK FOR THE SMALLEST VALUE OF ANY DIAGONAL ELEMENT ("MINDD")
!
      kvidx = (kvidx/2) + 1
      dakk = Zd(kvidx)
!WKBI  7/95 SPR95005
      IF ( dakk==0.D0 ) GOTO 700
      GOTO 200
   ELSEIF ( ktype==3 ) THEN
!
! DO DIVISION IN COMPLEX SINGLE PRECISION AND COMPUTE THE DETERMINANT
! DSR AND DSC.
! CHECK FOR THE SMALLEST VALUE OF ANY DIAGONAL ELEMENT ("MINDS")
!
!   (A+Bi) / (C+Di) = (AC + DB + ( CB-AD )i ) / (C**2 + D**2)
      akkr = Zr(kvidx)
      akki = Zr(kvidx+1)
      akk2 = akkr*akkr + akki*akki
!WKBI  7/95 SPR95005
      IF ( akk2==0. ) GOTO 700
      GOTO 300
   ELSEIF ( ktype==4 ) THEN
!
! DO DIVISION IN COMPLEX DOUBLE PRECISION AND COMPUTE THE DETERMINANT
! DDR AND DDC.
! CHECK FOR THE SMALLEST VALUE OF ANY DIAGONAL ELEMENT ("MINDD")
!
      kvidx = (kvidx/2) + 1
      dakkr = Zd(kvidx)
      dakki = Zd(kvidx+1)
      dakk2 = dakkr*dakkr + dakki*dakki
!WKBI  7/95 SPR95005
      IF ( dakk2==0. ) GOTO 700
      GOTO 400
   ELSE
!
! DO DIVISION IN REAL SINGLE PRECISION AND COMPUTE THE  DETERMINANT DDS.
! CHECK FOR THE SMALLEST VALUE OF ANY DIAGONAL ELEMENT ("MINDS")
!
      akk = Zr(kvidx)
!WKBI  7/95 SPR95005
      IF ( akk==0.0 ) GOTO 700
   ENDIF
 100  DO WHILE ( abs(dsr)>=10. )
      dsr = dsr/10.
      power = power + 1
   ENDDO
   DO WHILE ( abs(dsr)<=0.1 )
      dsr = dsr*10.
      power = power - 1
   ENDDO
   dsr = dsr*akk
   minds = amin1(abs(akk),minds)
   IF ( chlsky/=0 ) THEN
      IF ( akk<=0 ) GOTO 600
      akk = sqrt(akk)
   ENDIF
   IF ( akk<0. ) sturm = sturm + 1
!WKBD  7/95 SPR95005
!      IF ( AKK .EQ. 0. ) GO TO 7002
   Zrs(1) = akk
!WKBR 7/95 SPR95005
!      AKK      = 1. / AKK
   akk = -1./akk
   DO i = 2 , nrows
!WKBR 7/95 SPR95005
!      ZRS( I ) = -1.0 * ZR( KVIDX + I - 1 ) * AKK
      Zrs(i) = Zr(kvidx+i-1)*akk
   ENDDO
   GOTO 500
 200  DO WHILE ( dabs(ddr)>=10.D0 )
      ddr = ddr/10.D0
      power = power + 1
   ENDDO
   DO WHILE ( dabs(ddr)<=0.1 )
      ddr = ddr*10.D0
      power = power - 1
   ENDDO
   ddr = ddr*dakk
   mindd = dmin1(dabs(dakk),mindd)
   IF ( chlsky/=0 ) THEN
      IF ( dakk<=0 ) GOTO 600
      dakk = dsqrt(dakk)
   ENDIF
   IF ( dakk<0.D0 ) sturm = sturm + 1
!WKBD 7/95 SPR95005
!      IF ( DAKK .EQ. 0.D0 ) GO TO 7002
   Zrd(1) = dakk
!WKBR 7/95 SPR95005
!      DAKK        = 1.D0 / DAKK
   dakk = -1.D0/dakk
   DO i = 2 , nrows
!WKBR 7/95 SPR95005
!      ZRD( I ) = -1.0D0 * ZD( KVIDX + I - 1 ) * DAKK
      Zrd(i) = Zd(kvidx+i-1)*dakk
   ENDDO
   GOTO 500
 300  DO WHILE ( abs(dsr**2+dsc**2)>=10. )
      dsr = dsr/10.
      dsc = dsc/10.
      power = power + 1
   ENDDO
   DO WHILE ( abs(dsr**2+dsc**2)<=0.1 )
      dsr = dsr*10.
      dsc = dsc*10.
      power = power - 1
   ENDDO
   rs = dsr*akkr - dsc*akki
   dsc = dsr*akki + dsc*akkr
   dsr = rs
   minds = amin1(abs(akk2),minds)
   IF ( chlsky/=0 ) THEN
      IF ( akk2<=0 ) GOTO 600
      akk2 = sqrt(akk2)
   ENDIF
   IF ( akkr<0. ) sturm = sturm + 1
!WKBD  7/95 SPR95005
!      IF ( AKK2 .EQ. 0. ) GO TO 7002
   Zrs(1) = akkr
   Zrs(2) = akki
   nrowm = nrows*2 - 1
!WKBR 7/95 SPR95005
!      AKK2    = 1. / AKK2
   akk2 = -1./akk2
   kvidx = kvidx + 1
   DO i = 2 , nrowm , 2
!WKBDB 7/95 SPR95005
!      ZRS( I+1 ) =  -1.0 * ( ZR( KVIDX+I-1   ) * AKKR  +
!     &                         ZR( KVIDX+I   ) * AKKI  ) * AKK2
!      ZRS( I+2 ) =  -1.0 * ( ZR( KVIDX+I     ) * AKKR  -
!     &                         ZR( KVIDX+I-1 ) * AKKI  ) * AKK2
!WKBDE 7/95 SPR95005
!WKBIB 7/95 SPR95005
      Zrs(i+1) = (Zr(kvidx+i-1)*akkr+Zr(kvidx+i)*akki)*akk2
      Zrs(i+2) = (Zr(kvidx+i)*akkr-Zr(kvidx+i-1)*akki)*akk2
!WKBIE 7/95 SPR95005
   ENDDO
   GOTO 500
 400  DO WHILE ( dabs(ddr**2+ddc**2)>=10.D0 )
      ddr = ddr/10.
      ddc = ddc/10.
      power = power + 1
   ENDDO
   DO WHILE ( dabs(ddr**2+ddc**2)<=0.1D0 )
      ddr = ddr*10.
      ddc = ddc*10.
      power = power - 1
   ENDDO
   dr = ddr*dakkr - ddc*dakki
   ddc = ddr*dakki + ddc*dakkr
   ddr = dr
   mindd = dmin1(dabs(dakk2),mindd)
   IF ( chlsky/=0 ) THEN
      IF ( dakk2<=0 ) GOTO 600
      dakk2 = dsqrt(dakk2)
   ENDIF
   IF ( dakkr<0. ) sturm = sturm + 1
!WKBD  7/95 SPR95005
!      IF ( DAKK2 .EQ. 0. ) GO TO 7002
   Zrd(1) = dakkr
   Zrd(2) = dakki
   nrowm1 = nrows*2 - 1
!WKBR 7/95 SPR95005
!      DAKK2   = 1.D0 / (DAKK2 )
   dakk2 = -1.D0/(dakk2)
   kvidx = kvidx + 1
   DO i = 2 , nrowm1 , 2
!WKBDB 7/95 SPR95005
!      ZRD( I+1 ) = -1.0D0 * ( ZD( KVIDX+I-1 ) * DAKKR  +
!     &                          ZD( KVIDX+I ) * DAKKI  ) * DAKK2
!      ZRD( I+2 ) = -1.0D0 * ( ZD( KVIDX+I )   * DAKKR  -
!     &                        ZD( KVIDX+I-1 ) * DAKKI  ) * DAKK2
!WKBDE 7/95 SPR95005
!WKBIB 7/95 SPR95005
      Zrd(i+1) = (Zd(kvidx+i-1)*dakkr+Zd(kvidx+i)*dakki)*dakk2
      Zrd(i+2) = (Zd(kvidx+i)*dakkr-Zd(kvidx+i-1)*dakki)*dakk2
!WKBIE 7/95 SPR95005
   ENDDO
!
! NOW WRITE THE COLUMN OUT TO THE OUTPUT MATRIX
!
 500  itwrds = 0
   moblk(8) = -1
   moblk(12) = kcol
   krow = Zi(kridx)
   krows = Zi(kridx+1)
   IF ( ktype<=2 ) nwds = 1
   IF ( ktype>2 ) nwds = 2
   iol = 1
   DO
      CALL putstr(moblk)
      moblk(4) = krow
      moblk(7) = min0(krows,moblk(6))
      jstr = moblk(5)
      nstr = jstr + (moblk(7)-1)*nwds
      IF ( ktype>=3 ) nstr = nstr + 1
      IF ( kprec==2 ) THEN
!
! MOVE REAL DOUBLE AND DOUBLE COMPLEX VALUES INTO BUFFER
!
         DO jj = jstr , nstr
            xnd(jj) = Zrd(iol)
!      PRINT *,' SMCOUT,ROW,NUM,TERM=',MOBLK(4),MOBLK(7),XND(JJ)
            iol = iol + 1
         ENDDO
         itwrds = (nstr-jstr+1)*2
      ELSE
!
! MOVE REAL SINGLE AND SINGLE COMPLEX VALUES INTO BUFFER
!
         DO jj = jstr , nstr
            Xns(jj) = Zrs(iol)
            iol = iol + 1
         ENDDO
         itwrds = nstr - jstr + 1
      ENDIF
!
! CHECK TO SEE IF ALL CONSECUTIVE ROWS CAN BE STORED IN THE BUFFER
! I.E., ARE THERE ENOUGH WORDS IN THE AVAILABLE STRING
!
      IF ( moblk(7)==krows ) THEN
!
! ALL OF THE CURRENT CONSECUTIVE ROWS WERE STORED IN THE BUFFER.
! GO AND GET THE NEXT SET OF CONSECUTIVE ROWS, IF ANY EXIST.
!
         kridx = kridx + 2
         IF ( kridx>=kridn ) THEN
!
! ALL ROWS OF THIS COLUMN HAVE BEEN STORED, CLOSE OUT THE COLUMN
!
            moblk(8) = 1
            CALL endput(moblk)
!WKBIE 7/95 SPR95005
!WKBD  7/95 SPR95005
!      GO TO ( 1050, 2050, 3050, 4050 ), KTYPE
            lll(6) = max0(lll(6),itwrds)
            lll(7) = lll(7) + itwrds
            GOTO 99999
         ELSE
            CALL endput(moblk)
            krow = Zi(kridx)
            krows = Zi(kridx+1)
         ENDIF
      ELSE
         istore = moblk(7)
         krows = krows - istore
         krow = krow + istore
         CALL endput(moblk)
      ENDIF
   ENDDO
 600  WRITE (nout,99001) Ufm , kcol
99001 FORMAT (A23,' 3181, ATTEMPT TO PERFORM CHOLESKY DECOMPOSITION',' ON A NEGATIVE DEFINITE MATRIX IN SUBROUTINE SMCOMP.',/,      &
             &' NEGATIVE DIAGONAL TERM FOUND ON COLUMN ',I6)
   ierror = 4
   CALL mesage(-61,0,0)
 700  WRITE (nout,99002) Uwm , kcol , rzero
99002 FORMAT (A25,' 2396, SMCOMP COMPUTED A ZERO ON THE DIAGONAL ','DURING DECOMPOSITION OF ROW NUMBER ',I6,'.',/,                  &
             &' USE OF DIAG 22 OUTPUT SHOULD PERMIT YOU TO CORRELATE THE',' ROW WITH A MODEL D.O.F.',/,' A VALUE OF ',E13.6,        &
             &' WILL BE USED IN PLACE OF THE ZERO, HOWEVER',/,' THE ACCURACY OF THE DECOMPOSITION MAY BE IN DOUBT.')
   akk = rzero
   dakk = rzero
   akkr = rzero
   akki = rzero
   dakkr = rzero
   dakki = rzero
   akk2 = akkr*akkr + akki*akki
   dakk2 = dakkr*dakkr + dakki*dakki
!WKBIB 7/95 SPR95005
   IF ( ktype==2 ) THEN
      Zd(kvidx) = dakk
      GOTO 200
   ELSEIF ( ktype==3 ) THEN
      Zr(kvidx) = akkr
      Zr(kvidx+1) = akki
      GOTO 300
   ELSEIF ( ktype==4 ) THEN
      Zd(kvidx) = dakkr
      Zd(kvidx+1) = dakki
      GOTO 400
   ELSE
      Zr(kvidx) = akk
      GOTO 100
   ENDIF
99999 END SUBROUTINE smcout
