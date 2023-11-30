
SUBROUTINE smcout(Zi,Zr,Zd,Zrs,Zrd)
   IMPLICIT NONE
   INCLUDE 'SMCOMX.COM'
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   DOUBLE PRECISION Xnd(10)
   REAL Xns(10)
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ Xns
   DOUBLE PRECISION Zd(10) , Zrd(10)
   INTEGER Zi(10)
   REAL Zr(10) , Zrs(10)
   REAL akk , akk2 , akki , akkr , rs , rzero
   DOUBLE PRECISION dakk , dakk2 , dakki , dakkr , dr, Dsr, Dsc, Minds
   INTEGER i , istore , itwrds , jj , jstr , kdir , km2 , kmidx , kridn , kvidx , nrowm , nrowm1 , nrows , nstr , nwds
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
   kdir = (Kcol-1)*4 + 1
   kmidx = Zi(kdir)
   Kridx = kmidx + 4
   km2 = Zi(kmidx+1)
   kridn = Kridx + km2
   nrows = 0
   DO i = 1 , km2 , 2
      nrows = nrows + Zi(Kridx+i)
   ENDDO
   kvidx = Kridx + km2
   IF ( Ktype==2 ) THEN
!
! DO DIVISION IN REAL DOUBLE PRECISION AND COMPUTE THE  DETERMINANT DDR.
! CHECK FOR THE SMALLEST VALUE OF ANY DIAGONAL ELEMENT ("MINDD")
!
      kvidx = (kvidx/2) + 1
      dakk = Zd(kvidx)
!WKBI  7/95 SPR95005
      IF ( dakk/=0.D0 ) GOTO 200
      GOTO 700
   ELSEIF ( Ktype==3 ) THEN
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
      IF ( akk2/=0. ) GOTO 300
      GOTO 700
   ELSEIF ( Ktype==4 ) THEN
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
      IF ( dakk2/=0. ) GOTO 400
      GOTO 700
   ELSE
!
! DO DIVISION IN REAL SINGLE PRECISION AND COMPUTE THE  DETERMINANT DDS.
! CHECK FOR THE SMALLEST VALUE OF ANY DIAGONAL ELEMENT ("MINDS")
!
      akk = Zr(kvidx)
!WKBI  7/95 SPR95005
      IF ( akk==0.0 ) GOTO 700
   ENDIF
 100  DO WHILE ( abs(Dsr)>=10. )
      Dsr = Dsr/10.
      Power = Power + 1
   ENDDO
   DO WHILE ( abs(Dsr)<=0.1 )
      Dsr = Dsr*10.
      Power = Power - 1
   ENDDO
   Dsr = Dsr*akk
   Minds = amin1(abs(akk),Minds)
   IF ( Chlsky/=0 ) THEN
      IF ( akk<=0 ) GOTO 600
      akk = sqrt(akk)
   ENDIF
   IF ( akk<0. ) Sturm = Sturm + 1
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
 200  DO WHILE ( dabs(Ddr)>=10.D0 )
      Ddr = Ddr/10.D0
      Power = Power + 1
   ENDDO
   DO WHILE ( dabs(Ddr)<=0.1 )
      Ddr = Ddr*10.D0
      Power = Power - 1
   ENDDO
   Ddr = Ddr*dakk
   Mindd = dmin1(dabs(dakk),Mindd)
   IF ( Chlsky/=0 ) THEN
      IF ( dakk<=0 ) GOTO 600
      dakk = dsqrt(dakk)
   ENDIF
   IF ( dakk<0.D0 ) Sturm = Sturm + 1
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
 300  DO WHILE ( abs(Dsr**2+Dsc**2)>=10. )
      Dsr = Dsr/10.
      Dsc = Dsc/10.
      Power = Power + 1
   ENDDO
   DO WHILE ( abs(Dsr**2+Dsc**2)<=0.1 )
      Dsr = Dsr*10.
      Dsc = Dsc*10.
      Power = Power - 1
   ENDDO
   rs = Dsr*akkr - Dsc*akki
   Dsc = Dsr*akki + Dsc*akkr
   Dsr = rs
   Minds = amin1(abs(akk2),Minds)
   IF ( Chlsky/=0 ) THEN
      IF ( akk2<=0 ) GOTO 600
      akk2 = sqrt(akk2)
   ENDIF
   IF ( akkr<0. ) Sturm = Sturm + 1
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
 400  DO WHILE ( dabs(Ddr**2+Ddc**2)>=10.D0 )
      Ddr = Ddr/10.
      Ddc = Ddc/10.
      Power = Power + 1
   ENDDO
   DO WHILE ( dabs(Ddr**2+Ddc**2)<=0.1D0 )
      Ddr = Ddr*10.
      Ddc = Ddc*10.
      Power = Power - 1
   ENDDO
   dr = Ddr*dakkr - Ddc*dakki
   Ddc = Ddr*dakki + Ddc*dakkr
   Ddr = dr
   Mindd = dmin1(dabs(dakk2),Mindd)
   IF ( Chlsky/=0 ) THEN
      IF ( dakk2<=0 ) GOTO 600
      dakk2 = dsqrt(dakk2)
   ENDIF
   IF ( dakkr<0. ) Sturm = Sturm + 1
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
   Moblk(8) = -1
   Moblk(12) = Kcol
   Krow = Zi(Kridx)
   Krows = Zi(Kridx+1)
   IF ( Ktype<=2 ) nwds = 1
   IF ( Ktype>2 ) nwds = 2
   Iol = 1
   DO
      CALL putstr(Moblk)
      Moblk(4) = Krow
      Moblk(7) = min0(Krows,Moblk(6))
      jstr = Moblk(5)
      nstr = jstr + (Moblk(7)-1)*nwds
      IF ( Ktype>=3 ) nstr = nstr + 1
      IF ( Kprec==2 ) THEN
!
! MOVE REAL DOUBLE AND DOUBLE COMPLEX VALUES INTO BUFFER
!
         DO jj = jstr , nstr
            Xnd(jj) = Zrd(Iol)
!      PRINT *,' SMCOUT,ROW,NUM,TERM=',MOBLK(4),MOBLK(7),XND(JJ)
            Iol = Iol + 1
         ENDDO
         itwrds = (nstr-jstr+1)*2
      ELSE
!
! MOVE REAL SINGLE AND SINGLE COMPLEX VALUES INTO BUFFER
!
         DO jj = jstr , nstr
            Xns(jj) = Zrs(Iol)
            Iol = Iol + 1
         ENDDO
         itwrds = nstr - jstr + 1
      ENDIF
!
! CHECK TO SEE IF ALL CONSECUTIVE ROWS CAN BE STORED IN THE BUFFER
! I.E., ARE THERE ENOUGH WORDS IN THE AVAILABLE STRING
!
      IF ( Moblk(7)==Krows ) THEN
!
! ALL OF THE CURRENT CONSECUTIVE ROWS WERE STORED IN THE BUFFER.
! GO AND GET THE NEXT SET OF CONSECUTIVE ROWS, IF ANY EXIST.
!
         Kridx = Kridx + 2
         IF ( Kridx>=kridn ) THEN
!
! ALL ROWS OF THIS COLUMN HAVE BEEN STORED, CLOSE OUT THE COLUMN
!
            Moblk(8) = 1
            CALL endput(Moblk)
!WKBIE 7/95 SPR95005
!WKBD  7/95 SPR95005
!      GO TO ( 1050, 2050, 3050, 4050 ), KTYPE
            Lll(6) = max0(Lll(6),itwrds)
            Lll(7) = Lll(7) + itwrds
            GOTO 99999
         ELSE
            CALL endput(Moblk)
            Krow = Zi(Kridx)
            Krows = Zi(Kridx+1)
         ENDIF
      ELSE
         istore = Moblk(7)
         Krows = Krows - istore
         Krow = Krow + istore
         CALL endput(Moblk)
      ENDIF
   ENDDO
 600  WRITE (Nout,99001) Ufm , Kcol
99001 FORMAT (A23,' 3181, ATTEMPT TO PERFORM CHOLESKY DECOMPOSITION',' ON A NEGATIVE DEFINITE MATRIX IN SUBROUTINE SMCOMP.',/,      &
             &' NEGATIVE DIAGONAL TERM FOUND ON COLUMN ',I6)
   Ierror = 4
   CALL mesage(-61,0,0)
 700  WRITE (Nout,99002) Uwm , Kcol , rzero
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
   IF ( Ktype==2 ) THEN
      Zd(kvidx) = dakk
      GOTO 200
   ELSEIF ( Ktype==3 ) THEN
      Zr(kvidx) = akkr
      Zr(kvidx+1) = akki
      GOTO 300
   ELSEIF ( Ktype==4 ) THEN
      Zd(kvidx) = dakkr
      Zd(kvidx+1) = dakki
      GOTO 400
   ELSE
      Zr(kvidx) = akk
      GOTO 100
   ENDIF
99999 RETURN
END SUBROUTINE smcout