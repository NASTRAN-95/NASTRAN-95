
SUBROUTINE dpd4
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Buf(24) , Buf1 , Buf2 , Buf3 , Buf4 , Dload(2) , Dlt , Dpool , Eed , Eigb(2) , Eigc(2) , Eigr(2) , Epoint(2) , Eqdyn ,   &
         & Freq(2) , Freq1(2) , Frl , Gpl , Gpld , Ineq , Kn , L , Loads(32) , Luset , Lusetd , Mcb(7) , Msg(3) , Nam(2) , Neqdyn , &
         & Ngrid , Nlft , Nodlt , Noeed , Nofrl , Nogo , Nolin(21) , Nonlft , Nopsdl , Notfl , Notrl , Psd(2) , Psdl , Scr1 , Scr2 ,&
         & Scr3 , Scr4 , Sdt , Seqep(2) , Sil , Sild , Tf(2) , Tfl , Tic(2) , Trl , Tstep(2) , Two(32) , Ud , Ue , Uset , Usetd ,   &
         & Z(1)
   REAL Bufr(20) , Clsrew , Rd , Rdrew , Ua , Uf , Ufe , Ug , Ul , Um , Un , Une , Uo , Up , Ur , Us , Usb , Usg , Wrt , Wrtrew ,   &
      & Zz(1)
   COMMON /bitpos/ Um , Uo , Ur , Usg , Usb , Ul , Ua , Uf , Us , Un , Ug , Ue , Up , Une , Ufe , Ud
   COMMON /blank / Luset , Lusetd , Notfl , Nodlt , Nopsdl , Nofrl , Nonlft , Notrl , Noeed
   COMMON /dpdcom/ Dpool , Gpl , Sil , Uset , Gpld , Sild , Usetd , Dlt , Frl , Nlft , Tfl , Trl , Psdl , Eed , Scr1 , Scr2 , Scr3 ,&
                 & Scr4 , Buf , Buf1 , Buf2 , Buf3 , Buf4 , Epoint , Seqep , L , Kn , Neqdyn , Loads , Dload , Freq1 , Freq ,       &
                 & Nolin , Nogo , Msg , Tic , Tstep , Tf , Psd , Eigr , Eigb , Eigc , Mcb , Nam , Eqdyn , Sdt , Ineq
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew
   COMMON /two   / Two
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER andf
   INTEGER file , flag , i , id , ii , iii , incore , inolin , ipoint , iset , itic , iusetd , j , k , k1 , k2 , kk , kkk , kset ,  &
         & mskud , mskue , n , nn , nnolin , nolinr , notic , notstp , nset , nusetd , nwdin
   EXTERNAL andf
!
! End of declarations
!
!
!     DPD4 ASSEMBLES THE NON-LINEAR FORCING TABLE (NLFT)
!     AND THE TRANSIENT RESPONSE LIST (TRL).
!
   EQUIVALENCE (Z(1),Zz(1)) , (Buf(1),Bufr(1)) , (Msg(2),Ngrid)
   DATA nolinr/7/
!
!     INITIALIZE POINTERS. OPEN SCR1. OPEN DYNAMICS POOL.
!
   inolin = Neqdyn + 2
   j = inolin
   mskud = Two(Ud)
   mskue = Two(Ue)
   incore = 0
   ii = 1
   i = 1
   Msg(1) = 67
   CALL preloc(*2400,Z(Buf1),Dpool)
   CALL open(*2400,Scr1,Z(Buf2),Wrtrew)
   Ineq = 0
!
!     LOCATE NOLINI CARD. IF PRESENT, TURN NONLFT FLAG OFF,
!
 100  CALL locate(*500,Z(Buf1),Nolin(i),flag)
   Nonlft = 1
   nwdin = Nolin(i+2)
!
!     READ A NOLINI CARD. CONVERT POINTS ON CARD TO SIL NOS.
!     STORE DATA IN CORE. IF SPILL, WRITE ON SCRATCH FILE.
!
 200  CALL read(*2500,*500,Dpool,Buf,nwdin,0,flag)
   Msg(3) = 100000000*ii + Buf(1)
   IF ( ii<5 ) THEN
!                             NOLIN5,NFTUBE,NOLIN6
      iii = ii
      IF ( Buf(6)>=10 ) THEN
         iii = ii + 4
         Buf(6) = Buf(6) - 10
      ENDIF
      IF ( ii==2 ) THEN
         IF ( Buf(8)>=10 ) THEN
            Buf(8) = Buf(8) - 10
            IF ( iii==2 ) iii = 10
            IF ( iii==6 ) iii = 9
         ENDIF
      ENDIF
   ELSEIF ( ii<6 ) THEN
!
!     SPECIAL HANDLING OF NOLIN5 CARD
!     CARD FORMAT AS RECEIVED FROM IFP
!        SID  AA   AB   FAB  EA/TEA  EB/TEB  ALPA/TALPA  ALPB/TALPB
!        GA1  GA2  GA3  GA4  GB1     GB2     GB3         GB4
!
!     WE CONVERT THIS CARD INTO THE FOLLOWING 6-WORD ENTRY FORMAT
!
!        SID  12  SILA1  AA          SILA2  AB
!        SID  12  SILA3  FAB         SIL4   0
!        SID  12  SILB1  EA/TEA      SILB2  EB/TEB
!        SID  12  SILB3  ALPA/TALPA  SILB4  ALPB/TALPB
!
      L = 23
      kk = 16
      DO k = 1 , 8
         Buf(L+1) = 0
         Buf(L) = Buf(kk)
         IF ( Buf(L)/=0 ) CALL dpdaa
         kk = kk - 1
         L = L - 2
      ENDDO
      Buf(24) = Buf(8)
      Buf(22) = Buf(7)
      Buf(18) = Buf(6)
      Buf(16) = Buf(5)
      Buf(12) = 0
      Buf(10) = Buf(4)
      Buf(6) = Buf(3)
      Buf(4) = Buf(2)
      Buf(3) = Buf(9)
      Buf(5) = Buf(11)
      Buf(9) = Buf(13)
      Buf(11) = Buf(15)
      Buf(17) = Buf(19)
      DO k = 1 , 24 , 6
         Buf(k) = Buf(1)
         Buf(k+1) = 12
      ENDDO
      nn = 24
      GOTO 400
   ELSEIF ( ii==6 ) THEN
!
      L = 7
      Buf(7) = Buf(2)
      Buf(8) = 1
      CALL dpdaa
      Buf(3) = Buf(7)
      Buf(7) = Buf(3)
      Buf(8) = 1
      CALL dpdaa
      Buf(5) = Buf(7)
      Buf(6) = Buf(5)
      Buf(2) = 11
      Msg(3) = Buf(1)
      GOTO 300
   ELSE
      iii = 13
      IF ( Buf(6)>=10 ) THEN
         iii = 14
         Buf(6) = Buf(6) - 10
      ENDIF
   ENDIF
   L = 2
   CALL dpdaa
   Buf(3) = Buf(2)
   L = 5
   CALL dpdaa
   L = 7
   IF ( ii==2 ) CALL dpdaa
   Buf(6) = Buf(7)
   Buf(2) = iii
 300  nn = 6
 400  IF ( incore/=0 ) THEN
      CALL write(Scr1,Buf,nn,0)
   ELSEIF ( j+nn>=Buf2 ) THEN
      CALL write(Scr1,Z(inolin),j-inolin,0)
      incore = 1
      CALL write(Scr1,Buf,nn,0)
   ELSE
      DO k = 1 , nn
         Z(j) = Buf(k)
         j = j + 1
      ENDDO
   ENDIF
   GOTO 200
!
!     HERE WHEN ALL CARDS OF CURRENT TYPE HAVE BEEN READ.
!     TEST FOR ALL CARDS READ.
!
 500  i = i + 3
   ii = ii + 1
   IF ( ii<=nolinr ) GOTO 100
   CALL write(Scr1,0,0,1)
   CALL close(Scr1,Clsrew)
   IF ( Nonlft==-1 ) GOTO 1100
!
!     SORT THE DATA ON SET ID.
!
   IF ( incore/=0 ) THEN
      CALL open(*2400,Scr1,Z(Buf2),Rdrew)
      CALL read(*2500,*600,Scr1,Z,Buf1,1,n)
      CALL mesage(-8,0,Nam)
   ELSE
      nnolin = j - 6
      n = j - inolin
      GOTO 700
   ENDIF
 600  CALL close(Scr1,Clsrew)
   inolin = 1
   nnolin = n - 5
 700  CALL sort(0,0,6,1,Z(inolin),n)
!
!     READ USETD INTO CORE.
!
   file = Usetd
   CALL open(*2400,Usetd,Z(Buf2),Rdrew)
   CALL fwdrec(*2500,Usetd)
   iusetd = nnolin + 7
   CALL read(*2500,*800,Usetd,Z(iusetd),Buf2-iusetd,1,n)
   CALL mesage(-8,0,Nam)
 800  CALL close(Usetd,Clsrew)
!
!     OPEN THE NLFT. WRITE SET IDS IN HEADER RECORD.
!
   file = Nlft
   CALL open(*1000,Nlft,Z(Buf2),Wrtrew)
   CALL fname(Nlft,Buf)
   CALL write(Nlft,Buf,2,0)
   Z(nnolin+6) = 0
   DO i = inolin , nnolin , 6
      IF ( Z(i+6)/=Z(i) ) CALL write(Nlft,Z(i),1,0)
   ENDDO
   CALL write(Nlft,0,0,1)
!
!     WRITE ONE RECORD PER SET. WITHIN EACH SET, SORT DATA ON SIL NO.
!     CONVERT SIL NOS. TO SIL NOS. IN UD AND UE SETS
!
   i = inolin
 900  j = i
   DO WHILE ( Z(i+6)==Z(i) )
      i = i + 6
   ENDDO
   n = i + 6 - j
!
! ... THE FOLLOWING SORT WAS REMOVED DUE TO THE INSTALLATION OF NOLIN5
!     CALL SORT (0,0,6,3,Z(J),N)
!
!WKBR SPR94005 6/94   DO 1387 KC = J,I,6
   DO k = j , i , 6
      Buf(1) = Z(k+1)
      Buf(2) = Z(k+2)
      Buf(4) = Z(k+3)
      Buf(5) = Z(k+4)
      Buf(8) = Z(k+5)
      Buf(9) = 0
      DO kk = 2 , 8 , 3
         IF ( kk<8 .OR. Buf(1)==2 .OR. Buf(1)==6 .OR. Buf(1)==9 .OR. Buf(1)==10 .OR. kk/=8 ) THEN
            k1 = 0
            k2 = 0
            nusetd = iusetd + Buf(kk) - 1
            IF ( nusetd>=iusetd ) THEN
               DO kkk = iusetd , nusetd
                  Buf(10) = Z(kkk)
                  IF ( andf(Buf(10),mskud)/=0 ) k1 = k1 + 1
                  IF ( andf(Buf(10),mskue)/=0 ) k2 = k2 + 1
               ENDDO
            ENDIF
            Buf(kk) = k1
            Buf(kk+1) = k2
            IF ( nusetd>=iusetd ) THEN
               IF ( andf(Buf(10),mskue)==0 ) Buf(kk+1) = 0
               IF ( andf(Buf(10),mskud)==0 ) THEN
                  Nogo = 1
                  Buf(1) = Z(k)
                  Buf(2) = k1
                  CALL mesage(30,93,Buf)
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      Buf(7) = Buf(8)
      Buf(8) = Buf(9)
      CALL write(Nlft,Buf,8,0)
   ENDDO
   CALL write(Nlft,0,0,1)
   i = i + 6
   IF ( Z(i)/=0 ) GOTO 900
!
!     CLOSE FILE AND WRITE TRAILER.
!
   CALL close(Nlft,Clsrew)
   Mcb(1) = Nlft
   Mcb(2) = (nnolin-inolin)/6 + 1
   CALL wrttrl(Mcb)
   IF ( incore/=0 ) Ineq = 0
   GOTO 1100
 1000 Nonlft = -1
!
!     LOCATE TIC CARDS IN DYNAMICS POOL.
!
 1100 Notrl = -1
   notic = 0
   notstp = 0
   CALL locate(*2300,Z(Buf1),Tic,flag)
   Notrl = 1
!
!     OPEN SCR1. INITIALIZE TO READ TIC CARDS.
!
   file = Scr1
   CALL open(*2400,Scr1,Z(Buf2),Wrtrew)
   itic = Neqdyn + 2
   nset = Buf3 - 1
   j = nset
   L = 2
   Msg(1) = 69
   id = 0
!
!     READ A TIC CARD. IF SET ID IS DIFFERENT, STORE IT IN LIST.
!     IF NOT FIRST CARD, SORT DATA ON SIL NO. AND WRITE IT IN SCR1.
!
 1200 CALL read(*2500,*1300,Dpool,Buf,5,0,flag)
   IF ( Buf(1)/=id ) THEN
      IF ( id/=0 ) THEN
         n = i - itic
         CALL sort(0,0,3,1,Z(itic),n)
         CALL write(Scr1,Z(itic),n,1)
      ENDIF
      id = Buf(1)
      Z(j) = id
      j = j - 1
      i = itic
      Msg(3) = id
   ENDIF
!
!     CONVERT POINT AND COMPONENT TO SIL NO.
!     STORE SIL NO., UO, VO IN CORE.
!
   CALL dpdaa
   Z(i) = Buf(2)
   Z(i+1) = Buf(4)
   Z(i+2) = Buf(5)
   i = i + 3
   IF ( i<j ) GOTO 1200
   CALL mesage(-8,0,Nam)
!
!     HERE WHEN LAST CARD READ - SORT AND WRITE LAST RECORD.
!
 1300 n = i - itic
   CALL sort(0,0,3,1,Z(itic),n)
   CALL write(Scr1,Z(itic),n,1)
   CALL close(Scr1,Clsrew)
   iset = j + 1
!
!     OPEN TRL. WRITE SET IDS IN HEADER.
!
   file = Trl
   CALL open(*2200,Trl,Z(Buf2),Wrtrew)
   CALL fname(Trl,Buf)
   n = nset - iset + 1
   Buf(3) = n
   notic = n
   CALL write(Trl,Buf,3,0)
   i = iset
   j = nset
   DO
      id = Z(j)
      Z(j) = Z(i)
      Z(i) = id
      i = i + 1
      j = j - 1
      IF ( i>=j ) THEN
         CALL write(Trl,Z(iset),n,0)
         EXIT
      ENDIF
   ENDDO
!
!     READ USETD INTO CORE.
!     COMPUTE NO. OF POINTS UN UD SET. WRITE NO. AS LAST WORD OF HEADER.
!
 1400 file = Usetd
   CALL open(*2400,Usetd,Z(Buf3),Rdrew)
   CALL fwdrec(*2500,Usetd)
   iusetd = 1
   Ineq = 0
   CALL read(*2500,*1500,Usetd,Z(iusetd),Buf3-iusetd,1,n)
   CALL mesage(-8,0,Nam)
 1500 CALL close(Usetd,Clsrew)
   nusetd = iusetd + n - 1
   k = 0
   DO i = iusetd , nusetd
      IF ( andf(Z(i),mskud)/=0 ) k = k + 1
   ENDDO
   CALL write(Trl,k,1,1)
   IF ( notic==0 ) GOTO 1900
!
!     READ SCR1. CONVERT SIL NO. TO AN SIL NO. IN THE D-SET.
!     WRITE TRL ONE RECORD PER SET.
!
   file = Scr1
   kset = iset
   CALL open(*2400,Scr1,Z(Buf3),Rdrew)
 1600 k = 0
   ipoint = iusetd
   DO
      CALL read(*1800,*1700,Scr1,Buf,3,0,flag)
      nusetd = iusetd + Buf(1) - 1
      DO i = ipoint , nusetd
         IF ( andf(Z(i),mskud)/=0 ) k = k + 1
      ENDDO
      Buf(1) = k
      IF ( andf(Z(nusetd),mskud)==0 ) THEN
         Nogo = 1
         CALL mesage(30,133,Z(kset))
      ENDIF
      CALL write(Trl,Buf,3,0)
      ipoint = nusetd + 1
   ENDDO
 1700 CALL write(Trl,0,0,1)
   kset = kset + 1
   GOTO 1600
 1800 CALL close(Scr1,Clsrew)
!
!     IF TSTEP CARDS PRESENT, COPY THEM ONTO TRL.
!
   CALL locate(*2000,Z(Buf1),Tstep,flag)
 1900 CALL read(*2500,*2000,Dpool,Buf,1,0,flag)
   notstp = notstp + 1
   CALL write(Trl,Buf,1,0)
   DO
      CALL read(*2500,*2600,Dpool,Buf,3,0,flag)
      IF ( Buf(1)==-1 ) THEN
         CALL write(Trl,0,0,1)
         GOTO 1900
      ELSE
         CALL write(Trl,Buf,3,0)
      ENDIF
   ENDDO
!
!     CLOSE FILES AND WRITE TRAILER.
!
 2000 CALL close(Trl,Clsrew)
   Mcb(1) = Trl
   Mcb(2) = notic
   Mcb(3) = notstp
   CALL wrttrl(Mcb)
 2100 CALL close(Dpool,Clsrew)
   RETURN
!
 2200 Notrl = -1
   GOTO 2100
!
!     HERE IF NO TIC CARDS - LOCATE TSTEP CARDS IN DYNAMICS POOL.
!     IF ABSENT, RETURN. OTHERWISE OPEN TRL AND WRTIE HEADER.
!
 2300 CALL locate(*2100,Z(Buf1),Tstep,flag)
   Notrl = 1
   file = Trl
   CALL open(*2200,Trl,Z(Buf2),Wrtrew)
   CALL fname(Trl,Buf)
   Buf(3) = 0
   CALL write(Trl,Buf,3,0)
   GOTO 1400
!
!     FATAL FILE ERRORS
!
 2400 n = -1
   GOTO 2700
 2500 n = -2
   GOTO 2700
 2600 n = -3
 2700 CALL mesage(n,file,Nam)
END SUBROUTINE dpd4
