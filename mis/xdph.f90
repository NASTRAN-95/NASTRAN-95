
SUBROUTINE xdph
!
!     DATA POOL HOUSEKEEPER (XDPH)
!
!     THIS SUBROUTINE SCANS THE DATA POOL DICT AND TO DETERMINE THE
!     NUMBER AND SIZE OF ANY FILES NO LONGER NEEDED.  IF A SUFFICIENT
!     QUANTITY IS NOT NEEDED, THE FILE IS RECOPIED WITH THE DEAD FILES
!     DELETED.
!
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Almsk , Apndmk , Comm(20) , Cursno , Dculg , Ddbn(2) , Dfnu(1) , Dmxlg , Dnaf , Dpd(1) , Dum(36) , Endsfa(1) , Entn1 ,   &
         & Entn2 , Entn3 , Entn4 , Exfiat , Fculg , Fcum(1) , Fcus(1) , Fdbn(2) , Fequ(1) , Fiat(1) , File(1) , Fist(2) , Fknd(1) , &
         & Flag , Fmat(1) , Fmxlg , Fntu(1) , Fnx , Fon(1) , Ford(1) , Fpun(1) , Funlg , Ibufsz , Lmsk , Lxmsk , Md(401) , Minp(1) ,&
         & Mlsn(1) , Mout(1) , Mscr(1) , Nbpc , Nbpw , Ncpw , Ndpd(1) , Npfist , Outtap , Rmsk , Rxmsk , S , Sal(1) , Scornt ,      &
         & Sdbn(1) , Slgn , Sntu(1) , Sord(1) , Sos(1501) , Tapmsk , Thcrmk , Xf1at(1) , Zap
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /system/ Ibufsz , Outtap , Dum , Nbpc , Nbpw , Ncpw
   COMMON /xdpl  / Dpd , Dmxlg , Dculg , Ddbn , Dfnu
   COMMON /xfiat / Fiat , Fmxlg , Fculg , File , Fdbn , Fmat
   COMMON /xfist / Fist
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /xpfist/ Npfist
   COMMON /xsfa1 / Md , Sos , Comm , Xf1at , Fpun , Fcum , Fcus , Fknd
   COMMON /xxfiat/ Exfiat
   COMMON /zzzzzz/ Endsfa
!
! Local variable declarations
!
   INTEGER andf , korsz , orf , rshift
   INTEGER fn , i , ii , iprt1 , iprt2 , iprt3 , isav , istart , iwkbuf , ix , j , k , kk , lmt , lmt2 , lmt3 , m , ncnt , nconst , &
         & nculg , ndph(2) , nfile , ngcnt , npol , pool , scrn1 , scrn2 , trial
   EXTERNAL andf , orf , rshift
!
! End of declarations
!
   EQUIVALENCE (Dpd(1),Dnaf) , (Fiat(1),Funlg) , (File(1),Fequ(1)) , (File(1),Ford(1)) , (Endsfa(1),Ndpd(1))
   EQUIVALENCE (Md(2),Mlsn(1)) , (Md(3),Minp(1)) , (Md(4),Mout(1)) , (Md(5),Mscr(1)) , (Sos(1),Slgn) , (Sos(2),Sdbn(1)) ,           &
    & (Sos(4),Sal(1)) , (Sos(4),Sntu(1)) , (Sos(4),Sord(1)) , (Comm(1),Almsk) , (Comm(2),Apndmk) , (Comm(3),Cursno) ,               &
    & (Comm(4),Entn1) , (Comm(5),Entn2) , (Comm(6),Entn3) , (Comm(7),Entn4) , (Comm(8),Flag) , (Comm(9),Fnx) , (Comm(10),Lmsk) ,    &
    & (Comm(11),Lxmsk) , (Comm(13),Rmsk) , (Comm(14),Rxmsk) , (Comm(15),S) , (Comm(16),Scornt) , (Comm(17),Tapmsk) ,                &
    & (Comm(18),Thcrmk) , (Comm(19),Zap) , (Xf1at(1),Fntu(1)) , (Xf1at(1),Fon(1))
!
   DATA nconst/100/
   DATA scrn1/4HSCRA/ , scrn2/4HTCH*/
   DATA pool , npol/4HPOOL , 4HNPOL/ , ndph/4HXDPH , 4H    /
!
!
   Flag = 0
   DO
      lmt3 = Dculg*Entn4
      lmt = (Dculg-1)*Entn4 + 1
      ncnt = 0
      ngcnt = 0
      trial = Dnaf - 1
!
!     COUNT DEAD FILE SIZE, PUT SIZE IN NCNT
!
      DO i = 1 , lmt3 , Entn4
         IF ( Ddbn(i)/=0 .OR. Ddbn(i+1)/=0 ) THEN
!
!     COUNT GOOD STUFF ALSO
!
            ngcnt = ngcnt + rshift(andf(Lmsk,Dfnu(i)),16)
            CYCLE
         ELSE
            IF ( Dfnu(i)<0 ) THEN
!
!     DEAD FILE IS EQUIV
!
               Flag = -1
               kk = andf(Rmsk,Dfnu(i))
               DO j = 1 , lmt3 , Entn4
                  IF ( Dfnu(j)<0 .AND. i/=j ) THEN
                     IF ( kk==andf(Rmsk,Dfnu(j)) ) THEN
                        IF ( Ddbn(j)/=0 .OR. Ddbn(j+1)/=0 ) GOTO 10
                        Dfnu(j) = 0
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
            IF ( kk==trial ) THEN
               Dnaf = trial
            ELSE
               IF ( Dfnu(i)/=0 ) ncnt = ncnt + rshift(andf(Lmsk,Dfnu(i)),16)
               GOTO 20
            ENDIF
 10         Dfnu(i) = 0
         ENDIF
 20      IF ( i==lmt ) THEN
            Dculg = Dculg - 1
            Flag = -1
            GOTO 100
         ENDIF
      ENDDO
!
!     CHECK FOR BREAKING OF EQUIV
!
      IF ( Flag/=0 ) THEN
         DO i = 1 , lmt3 , Entn4
            IF ( Dfnu(i)<0 ) THEN
               kk = andf(Rmsk,Dfnu(i))
               DO j = 1 , lmt3 , Entn4
                  IF ( Dfnu(j)<0 .AND. i/=j ) THEN
                     IF ( kk==andf(Rmsk,Dfnu(j)) ) GOTO 40
                  ENDIF
               ENDDO
               Dfnu(i) = andf(Almsk,Dfnu(i))
            ENDIF
 40      ENDDO
      ENDIF
!
!     IS NCNT OF SUFFICIENT SIZE TO WARRANT RECOPYING POOL
!
      CALL sswtch(3,ix)
      IF ( ix==1 ) THEN
         CALL page1
         WRITE (Outtap,99001) ncnt
99001    FORMAT (21H0DPH DEAD FILE COUNT=,I6)
         WRITE (Outtap,99002) (Dpd(ix),ix=1,3)
99002    FORMAT (16H0DPD BEFORE DPH ,3I4)
         ii = Dculg*3 + 3
         DO ix = 4 , ii , 3
            iprt1 = rshift(Dpd(ix+2),Nbpw-1)
            iprt2 = rshift(andf(Lxmsk,Dpd(ix+2)),16)
            iprt3 = andf(Rxmsk,Dpd(ix+2))
            WRITE (Outtap,99005) Dpd(ix) , Dpd(ix+1) , iprt1 , iprt2 , iprt3
         ENDDO
      ENDIF
!
!     RECOPY POOL IF THERE ARE MORE THAN 500,000 WORD DEAD AND
!     THE GOOD STUFF IS TWICE AS BIG AS THE DEAD STUFF
!
      IF ( ncnt>nconst .AND. ncnt>2*ngcnt ) EXIT
      IF ( ncnt>0 .AND. Dculg+5>=Dmxlg ) EXIT
      RETURN
 100  ENDDO
!
!     RECOPY POOL, SWITCH POOL FILE POINTERS
!
   lmt2 = Funlg*Entn1
   kk = andf(Thcrmk,scrn2)
   DO i = 1 , lmt2 , Entn1
      IF ( Fdbn(i)==0 .AND. Fdbn(i+1)==0 ) GOTO 200
      IF ( Fdbn(i)==scrn1 .AND. andf(Thcrmk,Fdbn(i+1))==kk ) GOTO 200
   ENDDO
!
!     NO FILE AVAILABLE TO COPY ONTO, FORGET IT
!
   RETURN
!
!     SET-UP FOR A RECOPY
!
 200  isav = i
   CALL open(*300,pool,Endsfa,0)
   Fnx = 1
   Fist(2*Npfist+4) = isav + 2
   Fist(2) = Npfist + 1
   Fist(2*Npfist+3) = npol
   CALL open(*300,npol,Endsfa(Ibufsz+1),1)
   m = 2*Ibufsz
   i = m + 1
   istart = i
   m = m + Dculg*3 + 3
   iwkbuf = korsz(Endsfa) - m
   IF ( iwkbuf<100 ) CALL mesage(-8,0,ndph)
   m = m + 1
   nfile = 1
   nculg = 0
   DO j = 1 , lmt3 , Entn4
      IF ( Ddbn(j)/=0 .OR. Ddbn(j+1)/=0 ) THEN
         IF ( Ddbn(j)/=63 .OR. Ddbn(j+1)/=63 ) THEN
!
!     RECOPY DICTIONARY
!
            Ndpd(i) = Ddbn(j)
            Ndpd(i+1) = Ddbn(j+1)
            Ndpd(i+2) = orf(andf(Lxmsk,Dfnu(j)),nfile)
            IF ( Dfnu(j)<0 ) THEN
               Ndpd(i+2) = orf(S,Ndpd(i+2))
               kk = andf(Rmsk,Dfnu(j))
               DO k = 1 , lmt3 , Entn4
                  IF ( Dfnu(k)<0 .AND. j/=k ) THEN
                     IF ( kk==andf(Rmsk,Dfnu(k)) ) THEN
                        i = i + 3
                        nculg = nculg + 1
                        Ndpd(i) = Ddbn(k)
                        Ddbn(k) = 63
                        Ndpd(i+1) = Ddbn(k+1)
                        Ddbn(k+1) = 63
                        Ndpd(i+2) = Ndpd(i-1)
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
            i = i + 3
            nculg = nculg + 1
!
!     RECOPY NECESSARY FILE
!
            fn = andf(Rmsk,Dfnu(j))
            CALL xfilps(fn)
            CALL cpyfil(pool,npol,Endsfa(m),iwkbuf,Flag)
            CALL eof(npol)
            nfile = nfile + 1
            Fnx = fn + 1
         ENDIF
      ENDIF
   ENDDO
!
!     COPY TEMPORARY DPD INTO ACTUAL DPD
!
   i = i - 1
   ix = 0
   DO j = istart , i
      ix = ix + 1
      Ddbn(ix) = Ndpd(j)
   ENDDO
   Dnaf = nfile
   Dculg = nculg
   CALL close(pool,1)
   CALL close(npol,1)
   Fnx = 1
!
!     COPY POOL BACK TO POOL UNIT
!
   CALL open(*300,npol,Endsfa,0)
   CALL open(*300,pool,Endsfa(Ibufsz+1),1)
   nfile = nfile - 1
   DO ix = 1 , nfile
      CALL cpyfil(npol,pool,Endsfa(m),iwkbuf,Flag)
      CALL eof(pool)
   ENDDO
   CALL close(pool,1)
   CALL close(npol,1)
!
!     THE FOLLOWING 3 LINES OF CODE WILL FREE DISK AREA ON SOME CONFIG.
!
   CALL open(*300,npol,Endsfa,1)
   CALL write(npol,ndph,2,1)
   CALL close(npol,1)
   CALL sswtch(3,ix)
   IF ( ix/=1 ) RETURN
!
   WRITE (Outtap,99003) (Dpd(ix),ix=1,3)
99003 FORMAT (15H0DPD AFTER DPH ,3I4)
   ii = Dculg*3 + 3
   DO ix = 4 , ii , 3
      iprt1 = rshift(Dpd(ix+2),Nbpw-1)
      iprt2 = rshift(andf(Lxmsk,Dpd(ix+2)),16)
      iprt3 = andf(Rxmsk,Dpd(ix+2))
      WRITE (Outtap,99005) Dpd(ix) , Dpd(ix+1) , iprt1 , iprt2 , iprt3
   ENDDO
   RETURN
!
 300  WRITE (Outtap,99004) Sfm
99004 FORMAT (A25,' 1041, OLD/NEW POOL COULD NOT BE OPENED.')
   CALL mesage(-37,0,ndph)
99005 FORMAT (1H ,2A4,3I6)
END SUBROUTINE xdph
