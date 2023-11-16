
SUBROUTINE dpd3
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Buf(24) , Buf1 , Buf2 , Buf3 , Buf4 , Dload(2) , Dlt , Dpool , Eed , Eigb(2) , Eigc(2) , Eigr(2) , Epoint(2) , Eqdyn ,   &
         & Freq(2) , Freq1(2) , Frl , Gpl , Gpld , Ineq , Kn , L , Loads(32) , Luset , Lusetd , Mcb(7) , Msg(3) , Nam(2) , Neqdyn , &
         & Ngrid , Nlft , Nodlt , Noeed , Nofrl , Nogo , Nolin(21) , Nonlft , Nopsdl , Notfl , Notrl , Psd(2) , Psdl , Scr1 , Scr2 ,&
         & Scr3 , Scr4 , Sdt , Seqep(2) , Sil , Sild , Tf(2) , Tfl , Tic(2) , Trl , Tstep(2) , Uset , Usetd , Z(1)
   REAL Bufr(20) , Clsrew , Consts(5) , Rd , Rdrew , Twopi , Wrt , Wrtrew , Zz(1)
   COMMON /blank / Luset , Lusetd , Notfl , Nodlt , Nopsdl , Nofrl , Nonlft , Notrl , Noeed
   COMMON /condas/ Consts
   COMMON /dpdcom/ Dpool , Gpl , Sil , Uset , Gpld , Sild , Usetd , Dlt , Frl , Nlft , Tfl , Trl , Psdl , Eed , Scr1 , Scr2 , Scr3 ,&
                 & Scr4 , Buf , Buf1 , Buf2 , Buf3 , Buf4 , Epoint , Seqep , L , Kn , Neqdyn , Loads , Dload , Freq1 , Freq ,       &
                 & Nolin , Nogo , Msg , Tic , Tstep , Tf , Psd , Eigr , Eigb , Eigc , Mcb , Nam , Eqdyn , Sdt , Ineq
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   REAL delf , delt , delta , f , f0 , fe , fi , fn , t0 , ti
   INTEGER file , flag , freq2(2) , i , ifrq , ifrq1 , ifrq2 , ilist , irps , irt1 , irt2 , ix , j , j1 , jn , jx , k , n , nfrq1 , &
         & nfrq2 , nlist , nofrq , nofrq1 , nofrq2 , nort , nort1 , nort2 , nrps , nrt1 , randps(2) , randt1(2) , randt2(2)
!
! End of declarations
!
!
!     DPD3 ASSEMBLES THE FREQUENCY RESPONSE LIST (FRL)
!     AND THE POWER SPECTRAL DENSITY LIST (PSDL).
!
   EQUIVALENCE (Consts(2),Twopi) , (Z(1),Zz(1)) , (Buf(1),Bufr(1)) , (Msg(2),Ngrid)
   DATA freq2 , randps , randt1 , randt2/1107 , 11 , 2107 , 21 , 2207 , 22 , 2307 , 23/
!
!     OPEN DYNAMICS POOL. SET POINTERS.
!
   file = Dpool
   CALL preloc(*1700,Z(Buf1),Dpool)
   nofrq1 = 0
   nofrq2 = 0
   nofrq = 0
   ifrq1 = 1
   ifrq2 = ifrq1
   ifrq = ifrq1
   i = ifrq1
   j = i
!
!     READ FREQ1 CARDS. CONVERT F1 AND DELTA F TO RADIANS.
!
   CALL locate(*200,Z(Buf1),Freq1,flag)
   nofrq1 = 1
   DO
      CALL read(*1800,*100,Dpool,Z(i),4,0,flag)
      Zz(i+1) = Twopi*Zz(i+1)
      Zz(i+2) = Twopi*Zz(i+2)
      i = i + 4
   ENDDO
 100  nfrq1 = i - 4
   ifrq2 = i
   ifrq = i
   j = i
!
!     READ FREQ2 CARDS. CONVERT FREQUENCIES TO RADIANS.
!
 200  CALL locate(*400,Z(Buf1),freq2,flag)
   nofrq2 = 1
   DO
      CALL read(*1800,*300,Dpool,Z(i),4,0,flag)
      Zz(i+1) = Twopi*Zz(i+1)
      Zz(i+2) = Twopi*Zz(i+2)
      i = i + 4
   ENDDO
 300  nfrq2 = i - 4
   ifrq = i
   j = i
!
!     READ FREQ CARDS. CONVERT FREQUENCIES TO RADIANS.
!
 400  CALL locate(*600,Z(Buf1),Freq,flag)
   nofrq = 1
 500  CALL read(*1800,*600,Dpool,Z(j+1),1,0,flag)
   j = j + 2
   DO
      CALL read(*1800,*1900,Dpool,Z(j),1,0,flag)
      IF ( Z(j)==-1 ) THEN
         Z(i) = j - (i+1)
         i = j
         GOTO 500
      ELSE
         Zz(j) = Twopi*Zz(j)
         j = j + 1
      ENDIF
   ENDDO
!
!     TEST FOR ANY FREQ TYPE CARDS.
!
 600  Nofrl = nofrq1 + nofrq2 + nofrq
   IF ( Nofrl==0 ) GOTO 800
!
!     COLLECT LIST OF FREQUENCY SET IDS AND POINTERS TO CARDS.
!     SORT THIS LIST ON SET ID.
!
   ilist = j + 1
   i = ilist
   IF ( nofrq1/=0 ) THEN
!
!     FOR FREQ1 SET STORE SET ID, POINTER TO SET, 0.
!
      DO k = ifrq1 , nfrq1 , 4
         Z(i) = Z(k)
         Z(i+1) = k
         Z(i+2) = 0
         i = i + 3
      ENDDO
      nlist = i - 3
   ENDIF
   IF ( nofrq2/=0 ) THEN
!
!     FOR FREQ2 SET STORE SET ID, POINTER TO SET, -1.
!
      DO k = ifrq2 , nfrq2 , 4
         Z(i) = Z(k)
         Z(i+1) = k
         Z(i+2) = -1
         i = i + 3
      ENDDO
      nlist = i - 3
   ENDIF
   IF ( nofrq==0 ) GOTO 900
!
!     FOR FREQ SET STORE SET ID, POINTER TO SET, NO. OF WORDS IN SET.
!
   j = ifrq
   DO
      n = Z(j)
      IF ( n==-1 ) THEN
         nlist = i - 3
         GOTO 900
      ELSE
         j = j + 1
         Z(i) = Z(j)
         Z(i+1) = j
         Z(i+2) = n
         i = i + 3
         j = j + n
      ENDIF
   ENDDO
 700  Ineq = 0
 800  Nofrl = -1
   GOTO 1000
 900  n = i - ilist
   CALL sort(0,0,3,1,Z(ilist),n)
!
!     OPEN THE FRL. WRITE NAME + SET IDS IN HEADER.
!
   file = Frl
   CALL open(*700,Frl,Z(Buf2),Wrtrew)
   CALL fname(Frl,Buf)
   CALL write(Frl,Buf,2,0)
   DO i = ilist , nlist , 3
      Buf(1) = Z(i)
      CALL write(Frl,Buf,1,0)
   ENDDO
   CALL write(Frl,0,0,1)
!
!     WRITE THE FRL ONE RECORD PER FREQUENCY SET.
!     CONVERT FREQ1 SETS TO LOOK LIKE FREQ SETS.
!     CONVERT FREQ2 SETS TO LOOK LIKE FREQ SETS.
!
   DO i = ilist , nlist , 3
      j = Z(i+1)
      n = Z(i+2)
      IF ( n<0 ) THEN
!
!     FREQ2 SET-- FORM F = F0*10.0**((I-1)*DELTA)
!     WHERE DELTA = (LOG10(FE/F0))/N AND I = 1 THRU N+1.
!
         f0 = Zz(j+1)
         fe = Zz(j+2)
         n = Z(j+3)
         fn = n
         delta = (alog10(fe/f0))/fn
         fi = 0.
         n = n + 1
         DO k = 1 , n
            f = f0*10.0**(fi*delta)
            CALL write(Frl,f,1,0)
            fi = fi + 1.0
         ENDDO
         CALL write(Frl,0,0,1)
      ELSEIF ( n==0 ) THEN
!
!     FREQ1 SET-- FORM F = F0 + (I-1)*DELTA F, WHERE I = 1 THRU N+1.
!
         f0 = Zz(j+1)
         delf = Zz(j+2)
         n = Z(j+3) + 1
         fi = 0.
         DO k = 1 , n
            f = f0 + fi*delf
            CALL write(Frl,f,1,0)
            fi = fi + 1.0
         ENDDO
         CALL write(Frl,0,0,1)
      ELSE
!
!     FREQ SET ---  SORT FREQUENCY LIST AND DISCARD ANY DUPLICATES.
!     THEN WRITE FREQUENCIES ON THE FRL
!
         n = n - 1
         IF ( n/=1 ) THEN
            CALL sortf(0,0,1,1,Z(j+1),n)
            j1 = j + 2
            jn = j + n
            ix = j + 1
            DO jx = j1 , jn
               IF ( Z(jx)/=Z(ix) ) THEN
                  ix = ix + 1
                  Z(ix) = Z(jx)
               ENDIF
            ENDDO
            n = ix - j
         ENDIF
         CALL write(Frl,Z(j+1),n,1)
      ENDIF
   ENDDO
!
!     CLOSE FRL AND WRITE TRAILER.
!
   Mcb(1) = Frl
   Mcb(2) = (nlist-ilist)/3 + 1
   CALL wrttrl(Mcb)
   CALL close(Frl,Clsrew)
   Ineq = 0
!
!     OPEN PSDL. IF PURGED, BYPASS PSDL PROCESSING.
!     OTHERWISE, LOCATE RANDPS CARDS. IF ABSENT, BYPASS PSDL PROCESSING.
!
 1000 file = Psdl
   CALL open(*1600,Psdl,Z(Buf2),Wrtrew)
   CALL locate(*1600,Z(Buf1),randps,flag)
!
!     READ RANDPS CARDS INTO CORE.
!
   irps = 1
   file = Dpool
   CALL read(*1800,*1100,Dpool,Z(irps),Buf2-irps,1,nrps)
   n = -8
   CALL mesage(n,file,Nam)
   GOTO 99999
 1100 irt1 = irps + nrps
   irt2 = irt1
   i = irt1
   j = i
   nort1 = 0
   nort2 = 0
!
!     READ RANDT1 CARDS.
!
   CALL locate(*1300,Z(Buf1),randt1,flag)
   CALL read(*1800,*1200,Dpool,Z(irt1),Buf2-irt1,1,nort1)
   n = -8
   CALL mesage(n,file,Nam)
   GOTO 99999
 1200 irt2 = irt1 + nort1
   nrt1 = irt2 - 4
   i = irt2
   j = i
!
!     READ RANDT2 CARDS.
!
 1300 CALL locate(*1500,Z(Buf1),randt2,flag)
   nort2 = 1
 1400 CALL read(*1800,*1500,Dpool,Z(j+1),1,0,flag)
   j = j + 2
   DO
      CALL read(*1800,*1900,Dpool,Z(j),1,0,flag)
      IF ( Z(j)==-1 ) THEN
         Z(i) = j - (i+1)
         i = j
         GOTO 1400
      ELSE
         j = j + 1
         IF ( j>=Buf2 ) THEN
            n = -8
            CALL mesage(n,file,Nam)
            GOTO 99999
         ENDIF
      ENDIF
   ENDDO
!
!     COLLECT LIST OF RANDT1 AND RANDT2 SET IDS AND POINTERS TO DATA.
!
 1500 nort = nort1 + nort2
   IF ( nort/=0 ) THEN
      ilist = j + 1
      i = ilist
      IF ( nort1/=0 ) THEN
!
!     FOR RANDT1 SETS STORE SET ID, POINTER TO SET, 0.
!
         DO k = irt1 , nrt1 , 4
            Z(i) = Z(k)
            Z(i+1) = k
            Z(i+2) = 0
            i = i + 3
         ENDDO
         nlist = i - 3
         IF ( i>Buf2 ) THEN
            n = -8
            CALL mesage(n,file,Nam)
            GOTO 99999
         ENDIF
      ENDIF
      IF ( nort2/=0 ) THEN
!
!     FOR RANDT2 SETS STORE SET ID, POINTER TO SET, NO. OF WORDS IN SET.
!
         j = irt2
         DO
            n = Z(j)
            IF ( n==-1 ) THEN
               nlist = i - 3
               EXIT
            ELSE
               Z(i) = Z(j)
               Z(i+1) = j
               Z(i+2) = n
               i = i + 3
               j = j + n
               IF ( i>=Buf2 ) THEN
                  n = -8
                  CALL mesage(n,file,Nam)
                  GOTO 99999
               ENDIF
            ENDIF
         ENDDO
      ENDIF
!
!     SORT LIST ON SET ID.
!
      n = i - ilist
      CALL sort(0,0,3,1,Z(ilist),n)
   ENDIF
!
!     WRITE SET IDS FOR RANDT1 AND RANDT2 CARDS IN HEADER RECORD OF
!     PSDL. THEN WRITE RANDPS DATA AS FIRST RECORD OF PSDL.
!
   CALL fname(Psdl,Buf)
   CALL write(Psdl,Buf,2,0)
   IF ( nort/=0 ) THEN
      DO i = ilist , nlist , 3
         CALL write(Psdl,Z(i),1,0)
      ENDDO
   ENDIF
   CALL write(Psdl,0,0,1)
   CALL write(Psdl,Z(irps),nrps,1)
   IF ( nort/=0 ) THEN
!
!     WRITE ONE RECORD ON PSDL FOR EACH RANDT1 OR RANDT2 SET.
!
      DO i = ilist , nlist , 3
         j = Z(i+1)
         n = Z(i+2)
         IF ( n==0 ) THEN
!
!     RANDT1 SET-- WRITE TI = T0 + (I-1)*DELTA T, WHERE I = 1 THRU N+1.
!
            n = Z(j+1)
            fn = n
            delt = (Zz(j+3)-Zz(j+2))/fn
            t0 = Zz(j+2)
            fi = 0.
            n = n + 1
            DO k = 1 , n
               ti = t0 + fi*delt
               CALL write(Psdl,ti,1,0)
               fi = fi + 1.0
            ENDDO
            CALL write(Psdl,0,0,1)
         ELSE
!
!     RANDT2 SET--  SORT DATA AND DISCARD ANY DUPLICATES. THEN WRITE SET
!
            n = n - 1
            IF ( n/=1 ) THEN
               CALL sortf(0,0,1,1,Z(j+1),n)
               j1 = j + 2
               jn = j + n
               ix = j + 1
               DO jx = j1 , jn
                  IF ( Z(jx)/=Z(ix) ) THEN
                     ix = ix + 1
                     Z(ix) = Z(jx)
                  ENDIF
               ENDDO
               n = ix - j
            ENDIF
            CALL write(Psdl,Z(j+1),n,1)
         ENDIF
      ENDDO
   ENDIF
!
!     CLOSE FILES, WRITE TRAILER AND EXIT.
!
   Mcb(1) = Psdl
   Mcb(2) = (nlist-ilist)/3 + 1
!      2147483647  = 2**31 - 1
   IF ( nort==0 ) Mcb(2) = 2147483647
   CALL wrttrl(Mcb)
   Ineq = 0
   Nopsdl = 1
 1600 CALL close(Dpool,Clsrew)
   CALL close(Psdl,Clsrew)
   RETURN
!
!     FATAL FILE ERRORS
!
 1700 n = -1
   CALL mesage(n,file,Nam)
   GOTO 99999
 1800 n = -2
   CALL mesage(n,file,Nam)
   GOTO 99999
 1900 n = -3
   CALL mesage(n,file,Nam)
99999 RETURN
END SUBROUTINE dpd3
