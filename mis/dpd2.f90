
SUBROUTINE dpd2
   IMPLICIT NONE
   INTEGER Buf(24) , Buf1 , Buf2 , Buf3 , Buf4 , Bufx(3) , Dload(2) , Dlt , Dpool , Eed , Eigb(2) , Eigc(2) , Eigr(2) , Epoint(2) , &
         & Eqdyn , Freq(2) , Freq1(2) , Frl , Gpl , Gpld , Idummy(55) , Ineq , Ithrml , Kn , L , Loads(32) , Luset , Lusetd , Mcb(7)&
         & , Msg(3) , Nam(2) , Neqdyn , Ngrid , Nlft , Nodlt , Noeed , Nofrl , Nogo , Nolin(21) , Nonlft , Nopsdl , Nosdt , Notfl , &
         & Notrl , Noue , Psd(2) , Psdl , Scr(4) , Scr1 , Scr2 , Scr3 , Scr4 , Sdt , Seqep(2) , Sil , Sild , Tf(2) , Tfl , Tic(2) , &
         & Trl , Tstep(2) , Uset , Usetd , Z(1)
   REAL Bufr(20) , Clsrew , Rd , Rdrew , Wrt , Wrtrew , Zz(1)
   COMMON /blank / Luset , Lusetd , Notfl , Nodlt , Nopsdl , Nofrl , Nonlft , Notrl , Noeed , Nosdt , Noue
   COMMON /dpdcom/ Dpool , Gpl , Sil , Uset , Gpld , Sild , Usetd , Dlt , Frl , Nlft , Tfl , Trl , Psdl , Eed , Scr1 , Scr2 , Scr3 ,&
                 & Scr4 , Buf , Buf1 , Buf2 , Buf3 , Buf4 , Epoint , Seqep , L , Kn , Neqdyn , Loads , Dload , Freq1 , Freq ,       &
                 & Nolin , Nogo , Msg , Tic , Tstep , Tf , Psd , Eigr , Eigb , Eigc , Mcb , Nam , Eqdyn , Sdt , Ineq
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew
   COMMON /system/ Idummy , Ithrml
   COMMON /zzzzzz/ Z
   INTEGER file , flag , i , ibuf , id , idarea , idload , iemf , ii , iii , itabl , itemp , j , j2 , jj , k , kk , m , n , ncore , &
         & nlist , nn , nodld , nx
!
!     DPD2 ASSEMBLES THE DYNAMIC LOADS TABLE (DLT).
!
   EQUIVALENCE (Z(1),Zz(1)) , (Buf(1),Bufr(1)) , (Msg(2),Ngrid) , (Scr1,Scr(1)) , (Buf2,Bufx(1))
!
!     OPEN DYNAMICS POOL. SET POINTERS TO LOOP THRU DAREA, DELAY
!     AND DPHASE TABLES.
!
   file = Dpool
   CALL preloc(*1300,Z(Buf1),Dpool)
   ii = 1
   iii = 1
   itabl = Neqdyn + 2
   L = 2
   j = Buf4 - 1
   Msg(1) = 66
!
!     LOCATE CARD TYPE. IF PRESENT--
!     STORE POINTER TO 1ST TABLE NO. IN LOADS TABLE, OPEN SCRATCH FILE
!     FOR TABLES, SET ID = 0.
!
 100  CALL locate(*400,Z(Buf1),Loads(ii),flag)
   Loads(ii+2) = j
   file = Scr(iii)
   CALL open(*1300,file,Z(Buf2),Wrtrew)
   id = 0
!
!     READ A CARD. IF TABLE NO. IS DIFFERENT, STORE TABLE NO. IN TABLE
!     LIST.  IF NOT FIRST CARD, SORT TABLE ON SIL NO. AND WRITE ON
!     SCRATCH FILE.
!
 200  CALL read(*1400,*300,Dpool,Buf,4,0,flag)
   IF ( Buf(1)/=id ) THEN
      IF ( id/=0 ) THEN
         n = i - itabl
         CALL sort(0,0,2,1,Z(itabl),n)
         CALL write(file,Z(itabl),n,1)
      ENDIF
      id = Buf(1)
      Z(j) = id
      j = j - 1
      i = itabl
      Msg(3) = id
   ENDIF
!
!     CONVERT POINT AND COMPONENT TO SIL NO.
!     STORE SIL NO. AND VALUE IN CORE.
!
   CALL dpdaa
   Z(i) = Buf(2)
   Z(i+1) = Buf(4)
   i = i + 2
   IF ( i<j ) GOTO 200
   CALL mesage(-8,0,Nam)
!
!     HERE WHEN LAST CARD OF CURRENT TYPE HAS BEEN READ--
!     SORT AND WRITE LAST RECORD. CLOSE SCRATCH FILE.  STORE
!     NUMBER OF TABLES IN TABLE LIST. TEST FOR ALL CARD TYPES PROCESSED.
!
 300  n = i - itabl
   CALL sort(0,0,2,1,Z(itabl),n)
   CALL write(file,Z(itabl),n,1)
   CALL close(file,Clsrew)
   Loads(ii+3) = Loads(ii+2) - j
 400  ii = ii + 4
   iii = iii + 1
   IF ( iii<=3 ) GOTO 100
!
!     SET POINTERS TO LOOP THRU RLOAD1,2 AND TLOAD1,2 CARDS
!
   ncore = j
   j = 1
   iii = 1
   Ineq = 0
!
!     LOCATE A CARD TYPE. IF PRESENT--
!     READ ALL CARDS OF TYPE INTO CORE.
!
 500  CALL locate(*600,Z(Buf1),Loads(ii),flag)
   m = Loads(ii+2)
   DO
      Z(j) = iii
      CALL read(*1400,*600,Dpool,Z(j+1),m,0,flag)
      j = j + 11
      IF ( j>=ncore ) THEN
         CALL mesage(-8,0,Nam)
         EXIT
      ENDIF
   ENDDO
!
!     TEST FOR ALL CARD TYPES PROCESSED.
!     IF SO, SORT CARDS ON LOAD SET ID.
!
 600  ii = ii + 4
   iii = iii + 1
   IF ( iii<=4 ) GOTO 500
   n = j - 1
   IF ( n/=0 ) THEN
!
      CALL sort(0,0,11,2,Z,n)
      nlist = j - 11
!
!     LOCATE DLOAD CARDS ON DYNAMICS POOL.
!     IF PRESENT READ INTO CORE. SORT EACH DLOAD CARD ON REFERENCED SET
!     ID.
!
      nodld = 0
      CALL locate(*800,Z(Buf1),Dload,flag)
      idload = j
      i = idload
      j = i
   ELSE
      CALL close(Dpool,Clsrew)
      RETURN
   ENDIF
 700  CALL read(*1400,*800,Dpool,Z(j+1),2,0,flag)
   j = j + 3
   nodld = nodld + 1
   DO
      CALL read(*1400,*1500,Dpool,Z(j),2,0,flag)
      IF ( Z(j)==-1 ) THEN
         n = j - (i+3)
         CALL sort(0,0,2,2,Z(i+3),n)
!
!     CHECK FOR DLOAD SET ID UNIQUENESS
!
         DO kk = 2 , n , 2
            jj = i + 2 + kk
            IF ( kk<n ) THEN
               IF ( Z(jj)==Z(jj+2) ) THEN
                  Nogo = 1
                  Msg(2) = Z(i+1)
                  Msg(3) = Z(jj)
                  CALL mesage(30,135,Msg(2))
               ENDIF
            ENDIF
         ENDDO
         Z(i) = n + 2
         i = j
         GOTO 700
      ELSE
         j = j + 2
         IF ( j>=ncore ) CALL mesage(-8,0,Nam)
      ENDIF
   ENDDO
 800  CALL close(Dpool,Clsrew)
!
!     OPEN THE DLT. WRITE NAME IN HEADER RECORD.
!     THEN WRITE NO. OF DLOAD CARDS FOLLOWED BY DLOAD SET IDS.
!     THEN WRITE SET IDS FOR EACH RECORD OF THE DLT (FOLLOWING DLOAD
!     RECORD)
!
   file = Dlt
   CALL open(*1200,Dlt,Z(Buf1),Wrtrew)
   CALL fname(Dlt,Buf)
   Buf(3) = nodld
   CALL write(Dlt,Buf,3,0)
   IF ( nodld/=0 ) THEN
      i = idload
      j = 1
      DO
         CALL write(Dlt,Z(i+1),1,0)
         i = i + Z(i) + 1
         j = j + 1
         IF ( j>nodld ) THEN
!
!     CHECK DLOAD SID  VS  RLOAD1,2 AND TLOAD1,2 FOR UNIQUENESS
!
            i = idload
            DO jj = 1 , nodld
               itemp = Z(i+1)
               DO kk = 1 , nlist , 11
                  IF ( itemp==Z(kk+1) ) THEN
                     Nogo = 1
                     Msg(2) = itemp
                     CALL mesage(30,136,Msg(2))
                  ENDIF
               ENDDO
               i = i + Z(i) + 1
            ENDDO
            EXIT
         ENDIF
      ENDDO
   ENDIF
   DO i = 1 , nlist , 11
      Buf(1) = Z(i+1)
!
!     CHECK FOR UNIQUE SET IDS ON TLOAD1,2 AND RLOAD1,2 CARDS  THEN WRIT
!
      IF ( i<nlist ) THEN
         IF ( Z(i+1)==Z(i+12) ) THEN
            Nogo = 1
            Msg(2) = itemp
            CALL mesage(30,136,Msg(2))
         ENDIF
      ENDIF
      CALL write(Dlt,Buf,1,0)
   ENDDO
   CALL write(Dlt,0,0,1)
!
!     IF DLOAD CARDS PRESENT, WRITE THE DLOAD RECORD.
!
   IF ( nodld==0 ) THEN
!
!     INITIALIZE TO LOOP THRU ALL LOAD SETS. THE REMAINDER OF THE DLT
!     WILL CONSIST OF ONE LOGICAL RECORD PER LOAD SET.
!
      i = 1
   ELSE
      Buf(1) = -1
      Buf(2) = -1
      i = idload
      j = 1
      DO
         n = Z(i)
         CALL write(Dlt,Z(i+1),n,0)
         CALL write(Dlt,Buf,2,0)
         i = i + n + 1
         j = j + 1
         IF ( j>nodld ) THEN
            CALL write(Dlt,0,0,1)
            i = 1
            EXIT
         ENDIF
      ENDDO
   ENDIF
!
!     WRITE FIXED SECTION OF DLT RECORD.
!
 900  Buf(1) = Z(i)
   Buf(2) = Z(i+2)
!
!     SAVE INFORCED MOTION FLAG ON TLOAD CARDS
!
   IF ( Z(i)>=3 .AND. Z(i)<=4 ) THEN
      iemf = Z(i+4)
      Z(i+4) = 0
   ENDIF
   CALL write(Dlt,Buf,2,0)
   CALL write(Dlt,Z(i+5),6,0)
!
!     POSITION SCRATCH FILES TO SELECTED TABLES.
!
   idarea = 0
   DO j = 1 , 3
      Buf(2*j-1) = 16777215
!                  16777215 =2**24 - 1
      k = i + j
      Buf(j+16) = Z(k+1)
      IF ( Buf(j+16)/=0 ) THEN
         jj = Loads(4*j-1)
         nn = Loads(4*j)
         IF ( nn/=0 ) THEN
            DO nx = 1 , nn
               IF ( Z(jj)==Buf(j+16) ) GOTO 950
               jj = jj - 1
            ENDDO
         ENDIF
         IF ( Ithrml/=1 .OR. j/=1 ) THEN
            Buf(10) = Z(i+1)
            Buf(11) = Buf(j+16)
            Buf(11) = Buf(11) + 100000000*j
            Nogo = 1
            CALL mesage(30,71,Buf(10))
            Buf(j+16) = 0
         ELSE
            idarea = -1
            Buf(17) = 0
         ENDIF
      ENDIF
      CYCLE
 950  nn = nx - 1
      file = Scr(j)
      ibuf = Bufx(j)
      CALL open(*1300,file,Z(ibuf),Rdrew)
      IF ( nn/=0 ) THEN
         DO nx = 1 , nn
            CALL fwdrec(*1400,file)
         ENDDO
      ENDIF
   ENDDO
!
!     INITIALIZE TABLE READ.
!
   Buf(14) = Buf(17)
   Buf(15) = Buf(18)
   Buf(16) = Buf(19)
!
!     READ AN ENTRY FROM APPROPRIATE TABLE/S).
!     IF ALL ENTRIES HAVE BEEN READ, GO TO CLOSE DLT RECORD.
!
 1000 DO j = 1 , 3
      IF ( Ithrml==1 .AND. j==1 ) THEN
         IF ( idarea/=0 ) THEN
            IF ( idarea==-2 ) GOTO 1050
            idarea = -2
            Buf(1) = 1
            Buf(2) = 0
            Buf(14) = 0
         ENDIF
      ENDIF
      IF ( Buf(j+13)/=0 ) THEN
         file = Scr(j)
         j2 = 2*j
         CALL read(*1400,*1050,file,Buf(j2-1),2,0,flag)
      ENDIF
      CYCLE
 1050 Buf(2*j-1) = 16777215
      Buf(j+13) = 0
   ENDDO
   IF ( Buf(1)+Buf(3)+Buf(5)==3*16777215 ) THEN
!
!     CLOSE DLT RECORD,  CLOSE TABLES AND TEST FOR COMPLETION OF DLT.
!
      CALL write(Dlt,0,0,1)
      DO j = 1 , 3
         IF ( Buf(j+16)/=0 ) CALL close(Scr(j),Clsrew)
      ENDDO
      i = i + 11
      IF ( i<=nlist ) GOTO 900
!
!     CLOSE DLT, WRITE TRAILER AND RETURN.
!
      CALL close(Dlt,Clsrew)
      Mcb(1) = Dlt
      Mcb(2) = Dlt
      CALL wrttrl(Mcb)
      Nodlt = 1
      GOTO 1200
   ELSE
!
!     SELECT MINIMUM SIL NO(S) AND FORMAT OUTPUT.
!
      DO j = 1 , 6
         Buf(j+10) = 0
      ENDDO
      Buf(7) = 1
      Buf(8) = 2
      Buf(9) = 3
      IF ( Buf(1)>Buf(3) ) THEN
!
!     1 .GT. 2--SWITCH 1 AND 2 THEN COMPARE 2 AND 3. IF 2 .GT. 3, SWITCH
!
         k = Buf(7)
         Buf(7) = Buf(8)
         Buf(8) = k
         IF ( Buf(1)<=Buf(5) ) GOTO 1100
         k = Buf(8)
         Buf(8) = Buf(9)
         Buf(9) = k
      ELSE
!
!     1 .LE. 2--COMPARE 2 TO 3. IF 2 .GT. 3, SWITCH 2 AND 3.
!
         IF ( Buf(3)<=Buf(5) ) GOTO 1100
         k = Buf(8)
         Buf(8) = Buf(9)
         Buf(9) = k
      ENDIF
!
!     COMPARE 1 TO 2--IF 1 .GT. 2, SWITCH 1 AND 2.
!
      k = Buf(7)
      L = Buf(8)
      IF ( Buf(2*k-1)>Buf(2*L-1) ) THEN
         Buf(7) = L
         Buf(8) = k
      ENDIF
   ENDIF
!
!     PICK UP 1. SET TO READ 1.
!
 1100 k = Buf(7)
   Buf(10) = Buf(2*k-1)
   Buf(k+10) = Buf(2*k)
   Buf(k+13) = k
!
!     IF 1 .EQ. 2, PICK UP 2 AND SET TO READ 2.
!
   L = Buf(8)
   IF ( Buf(2*k-1)==Buf(2*L-1) ) THEN
      Buf(L+10) = Buf(2*L)
      Buf(L+13) = L
!
!     IF 1 .EQ. 2 .EQ. 3, PICK UP 3 AND SET TO READ 3.
!
      m = Buf(9)
      IF ( Buf(2*L-1)==Buf(2*m-1) ) THEN
         Buf(m+10) = Buf(2*m)
         Buf(m+13) = m
      ENDIF
   ENDIF
!
!     WRITE SIL NO., A, TAU, THETA. THEN GO TO READ ANOTHER TABLE
!     ENTRY(S).
!
   IF ( Z(i)>=3 .AND. Z(i)<=4 ) Buf(13) = iemf
   CALL write(Dlt,Buf(10),4,0)
   GOTO 1000
 1200 RETURN
!
!     FATAL FILE ERRORS
!
 1300 n = -1
   GOTO 1600
 1400 n = -2
   GOTO 1600
 1500 n = -3
 1600 CALL mesage(n,file,Nam)
END SUBROUTINE dpd2
