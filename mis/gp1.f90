
SUBROUTINE gp1
   IMPLICIT NONE
   INTEGER Axic , Cls , Clsrew , Elem(1) , Iaxif , Icfiat , Ifl , Incrx , Iout , Isubs , Itherm , Ksystm(100) , Lastx , Luset ,     &
         & Nbpw , Nelem , Nfile(6) , Nocstm , Nogpdt , Ptr , Rd , Rdrew , Sysbuf , Wrt , Wrtrew , Z(1)
   REAL Degra , Pi , Radeg , S4pisq , Twopi , Zz(1)
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /blank / Luset , Nogpdt , Nocstm
   COMMON /condas/ Pi , Twopi , Radeg , Degra , S4pisq
   COMMON /gpta1 / Nelem , Lastx , Incrx , Elem
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Cls
   COMMON /setup / Nfile , Ptr
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zzzzzz/ Z
   INTEGER a(34) , bgpdt , buf1 , buf2 , buf3 , cord(6) , cordij(12) , cstm , eqexin , file , flag , geom1 , geom2 , geomp ,        &
         & gp1ah(2) , gpdt , gpfl , gpl , grid(2) , i , icsdt , icsgp , idseq1 , idseq2 , idseq3 , ierr , ifail , igpdt , ii , ij , &
         & ijk , ilist , ilist0 , imax , incr , irmndr , itype , j , j1 , jerr , jj , k , khi , khr , kk , klo , kn , l , large ,   &
         & last , lmt1 , m , m8 , maxa1 , maxdof , mcb(7) , mult , n , n1 , n2 , nam , ncode , ncore , ncsdt , ncsgp , ndx , neqex ,&
         & nerr , nlist , nn , nogmp1 , nogo , nogrid , nolist , nosclr , noseq , nread , nskip , nwds , nz
   REAL aa(34) , ab(3) , ac(3) , ai(3) , aj(3) , ak(3) , ar(3) , ax(3) , axi , length , r , rcth , rsth
   INTEGER korsz , rshift
   CHARACTER*29 lvl1 , lvl2
   INTEGER offset , scalpt(2) , scr1 , scr2 , seqgp(2) , sil , solv , solvp , spoint(2) , type
   EXTERNAL rshift
!
!     GP1  BUILDS THE FOLLOWING DATA BLOCKS--
!       1. GRID POINT LIST (GPL)
!       2. EXTERNAL INTERNAL GRID POINT EQUIVALENCE TABLE (EQEXIN)
!       3. GRID POINT DEFINITION TABLE (GPDT)
!       4. COORDINATE SYSTEM TRANSFORMATION MATRICES (CSTM)
!       5. BASIC GRID POINT DEFINITION TABLE (BGPDT)
!       6. SCALAR INDEX LIST (SIL)
!
!     THE FOLLOWING CARDS ARE READ BY GP1--
!       1. GRID
!       2. CELASI, CDAMPI, CMASSI  (I=1,2,3,4)
!       3. SPOINT
!       4. SEQGP   (SEQEP IS PROCESSED IN DPD1)
!       5. CORDIJ  (I=1,2,  J=R,S,C)
!
!     IMPORTANT
!     =========
!     REVISED  7/89 BY G.CHAN/UNISYS, TO ALLOW GRID, SCALAR AND EXTRA
!     POINT EXTERNAL ID UP TO 8 DIGITS FOR ALL 32-BIT MACHINES
!     PREVIOUSLY, ID OF 2000000 IS THE UPPER LIMIT FOR IBM AND VAX
!
!     REVISED  8/89 BY G.CHAN/UNISYS, AS PART OF THE EFFORT TO ALLOW A
!     NASTRAN JOB TO EXCEED 65535 LIMIT.
!     NORMALLY, IF GRID POINTS OR SCALAR POINTS DO NOT HAVE VERY LARGE
!     EXTERNAL ID NUMBERS, THEIR ID NOS. ARE MULTIPLIED BY 1000, SO THAT
!     999 ADDITIONAL POINTS CAN SQUEEZE IN VIA SEQGP CARDS. (NOTE - A
!     7- OR 8-DIGIT ID NO., TIMES 1000, EXCEEDS A 32-BIT WORD COMPUTER
!     HARDWARE LIMIT). THIS MULTIPLY FACTOR IS NOW ADJUSTABLE, 1000,100,
!     OR 10, SO THAT ADDITIONAL DIGITS CAN BE USED FOR THE EXTERNAL GRID
!     OR SCALAR POINTS IN CASE THERE ARE LIMITTED SEQGP CARDS PRESENT.
!     THIS VARIABLE MULTIPLIER (10,100, OR 1000) IS ALSO RECORDED IN THE
!     3RD WORD OF THE HEADER RECORD OF THE GPL DATA BLOCK FOR LATER USE.
!     THE ACTUAL FACTOR OF THE MULTIPLIER IS ALSO MACHINE DEPENDENT.
!     UNIVAC, A 36-BIT MACHINE, CAN HAVE A MULTIPLIER OF 100 OR 1000.
!     OTHER 60- OR 64- BIT MACHINES, THE MULTIPLIER REMAINS AT 1000
!     IF THE MULTIPLIER IS 1000, THE SEQGP AND SEQEP CARDS, AS BEFORE,
!     CAN HAVE 4 SEQID LEVELS, SUCH AS XXX.X.X.X
!     IF THE MULTIPLIER IS 100, SEQGP AND SEQEP CARDS ARE LIMITED TO
!     3 SEQID LEVELS, XXX.X.X
!     FINALLY, IF MULTIPLIER IS 10, SEQGP AND SEQEP ARE LIMITED TO XXX.X
!
!     SPECIAL CONSIDERATION FOR THE AXISYM. AND HYDROELAS. PROBLEMS - 10
!     IS USED FOR THE MULTIPLIER, AND THEREFOR A ONE SEQID LEVEL IS
!     AVAILABLE. PREVIOUSLY, SEQGP CARDS WERE NOT USED IN AXISYM. AND
!     HYDROELAS. PROBLEMS, AND NO USER WARNING MESSAGE PRINTED
!
!     NO ADJUSTABLE MULTIPLY FACTOR FOR SUBRSTRUCTURING (MULT=1000,
!     SEE ALSO SGEN)
!
!     THE 65535 LIMITATION INVOLVES ONLY A SAMLL CHANGE IN STA 973
!
   EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Iout) , (Ksystm(24),Icfiat) , (Ksystm(27),Axic) , (Ksystm(38),Iaxif) ,               &
    & (Ksystm(40),Nbpw) , (Ksystm(56),Itherm) , (Ksystm(69),Isubs)
   EQUIVALENCE (Z(1),Zz(1)) , (a(1),aa(1)) , (a(4),ab(1)) , (a(7),ac(1)) , (a(10),ai(1)) , (a(13),aj(1)) , (a(16),ak(1)) ,          &
    & (a(19),ax(1)) , (a(22),ar(1)) , (Nocstm,Ifl) , (geomp,geom1) , (mcb(2),kn)
   EQUIVALENCE (igpdt,icsdt)
   DATA geom1/101/ , geom2/102/ , gpl/201/ , eqexin/202/ , gpdt/203/ , cstm/204/ , bgpdt/205/ , sil/206/ , scr1/301/ , scr2/302/
   DATA gp1ah/4HGP1  , 4H    / , cord/6 , 6 , 6 , 13 , 13 , 13/ , grid/4501 , 45/ , seqgp/5301 , 53/ , cordij/1701 , 17 , 1801 ,    &
      & 18 , 1901 , 19 , 2001 , 20 , 2101 , 21 , 2201 , 22/ , scalpt/5551 , 49/
   DATA mcb/7*0/ , large/100000000/ , lvl1/'3  I.E.  XXX.X.X.X TO XXX.X.X'/ , lvl2/'2  I.E.  XXX.X.X.X TO XXX.X  '/
!
!     PERFORM GENERAL INITIALIZATION
!
   CALL delset
   nz = korsz(Z)
   buf1 = nz - Sysbuf - 2
   buf2 = buf1 - Sysbuf
   buf3 = buf2 - Sysbuf
   nogo = 0
   Nocstm = 0
   Nogpdt = -1
   nogmp1 = 1
   maxa1 = 0
   mult = 1000
   axi = 0
   IF ( Axic/=0 .OR. Iaxif/=0 ) axi = 1
   IF ( axi/=0 ) mult = 10
   IF ( Isubs/=0 ) mult = 1000
   imax = large
   IF ( Nbpw==32 ) imax = 2147483
   IF ( Nbpw==36 ) imax = 34359738
!         2147483=2**31/1000   34359738=2**35/1000
!
!     READ SCALAR ELEMENT CONNECTION CARDS (IF PRESENT).
!     EXTRACT SCALAR POINTS AND WRITE THEM ON SCR2.
!
   file = scr2
   CALL open(*5900,scr2,Z(buf2),Wrtrew)
   nosclr = 0
   m8 = -8
   a(11) = -1
   DO k = 12 , 16
      a(k) = 0
   ENDDO
   CALL preloc(*300,Z(buf1),geom2)
   i = 1
   DO i = 1 , Lastx , Incrx
      kk = Elem(i+10)
      IF ( kk/=0 ) THEN
         CALL locate(*100,Z(buf1),Elem(i+3),flag)
         nn = Elem(i+5)
         DO
            CALL read(*6000,*100,geom2,a,nn,0,flag)
            DO k = 3 , 4
               IF ( .NOT.(a(k)==0 .OR. (kk==1 .AND. a(k+2)/=0)) ) THEN
                  a(10) = a(k)
                  nosclr = 1
                  CALL write(scr2,a(10),1,0)
               ENDIF
            ENDDO
         ENDDO
      ENDIF
 100  ENDDO
!
!     COPY SCALAR POINTS DEFINED ON SPOINT CARDS (IF PRESENT) ONTO SCR2.
!
   CALL locate(*300,Z(buf1),scalpt,flag)
   nosclr = 1
   CALL read(*6000,*200,geom2,Z,buf2-1,1,n)
   CALL mesage(m8,0,gp1ah)
 200  CALL write(scr2,Z,n,0)
!
!     CLOSE FILES. IF SCALAR POINTS PRESENT, SORT LIST.
!     THEN DISCARD DUPLICATES AND WRITE UNIQUE LIST ON SCR2.
!
 300  CALL write(scr2,0,0,1)
   CALL close(scr2,Clsrew)
   CALL close(geom2,Clsrew)
   IF ( nosclr==0 ) GOTO 500
   Nfile(1) = gpdt
   Nfile(2) = bgpdt
   Nfile(3) = sil
   CALL open(*5900,scr2,Z(buf1),Rdrew)
   CALL sorti(scr2,0,1,1,Z,buf1-1)
   CALL close(scr2,Clsrew)
   file = Nfile(6)
   CALL open(*5900,file,Z(buf1),Rdrew)
   CALL open(*5900,scr2,Z(buf2),Wrtrew)
   last = -1
   DO
      CALL read(*6000,*400,file,a(10),1,0,flag)
      IF ( a(10)/=last ) THEN
         CALL write(scr2,a(10),1,0)
         last = a(10)
      ENDIF
   ENDDO
 400  CALL write(scr2,0,0,1)
   CALL close(scr2,Clsrew)
   CALL close(file,Clsrew)
   CALL open(*5900,scr2,Z(buf3),Rdrew)
!
!     READ GRID ENTRIES (IF PRESENT).
!     MERGE GRID AND SCALAR NOS.
!     CREATING LIST IN CORE OF EXTERNAL NO., MULT * EXTERNAL NO.
!     WRITE 7-WORD GRID AND SCALAR ENTRIES ON SCR1.
!
 500  a(1) = large
   a(10) = large
   file = scr1
   IF ( maxa1==0 ) CALL open(*5900,scr1,Z(buf2),Wrtrew)
   i = -1
   nogrid = 0
   IF ( maxa1==0 ) CALL preloc(*1300,Z(buf1),geom1)
   CALL locate(*1400,Z(buf1),grid,flag)
   nogrid = 1
   CALL read(*6000,*6100,geom1,a,8,0,flag)
   CALL write(scr1,a,7,0)
 600  IF ( nosclr==0 ) GOTO 800
   CALL read(*6000,*6100,scr2,a(10),1,0,flag)
   CALL write(scr1,a(10),7,0)
 700  IF ( nogrid==0 ) GOTO 1000
   IF ( nosclr/=0 ) THEN
      IF ( a(1)<a(10) ) THEN
      ELSEIF ( a(1)==a(10) ) THEN
         spoint(1) = a(1)
         spoint(2) = 0
         ierr = 12
         CALL mesage(-30,ierr,spoint)
         GOTO 6100
      ELSE
         GOTO 1000
      ENDIF
   ENDIF
!
!     GRID NO. .LT. SCALAR NO.
!
 800  i = i + 2
   Z(i) = a(1)
!
!     GRID POINT EXTERNAL ID * MULT IS LIMITED TO COMPUTER MAXIMUM
!     INTEGER SIZE
!
   IF ( a(1)<=imax .OR. axi/=0 ) THEN
      Z(i+1) = mult*a(1)
   ELSE
      IF ( a(1)>maxa1 ) maxa1 = a(1)
   ENDIF
   CALL read(*6000,*900,geom1,a,8,0,flag)
   CALL write(scr1,a,7,0)
   GOTO 700
 900  nogrid = 0
   a(1) = large
   IF ( nosclr==0 ) GOTO 1200
!
!     SCALAR NO. .LT. GRID NO.
!
 1000 i = i + 2
   Z(i) = a(10)
!
!     SCALAR POINT EXTERNAL ID * MULT IS LIMITED TO COMPUTER MAXIMUM
!     INTEGER SIZE
!
   IF ( a(10)<=imax .OR. axi/=0 ) THEN
      Z(i+1) = mult*a(10)
   ELSE
      IF ( a(10)>maxa1 ) maxa1 = a(10)
   ENDIF
   CALL read(*6000,*1100,scr2,a(10),1,0,flag)
   CALL write(scr1,a(10),7,0)
   GOTO 700
 1100 nosclr = 0
   a(10) = large
   IF ( nogrid/=0 ) GOTO 800
!
!     LIST COMPLETE ONLY IF MAXA1 .LE. ZERO
!
!     IF MAXA1 IS .GT. ZERO, SOME LARGE GRID OR SCALAR POINTS HAD BEEN
!     LEFT OUT IN LIST. MAXA1 IS THE LARGEST GRID OR SCALAR POINT
!     EXTERNAL ID.  RESET MULT AND REPEAT COMPILING LIST
!
 1200 IF ( maxa1>0 ) THEN
      IF ( Isubs/=0 ) THEN
!
         WRITE (Iout,99001) Ufm
99001    FORMAT (A23,' 2140B, EXTERNAL GRID OR SCALAR POINT ID TOO BIG')
         CALL mesage(-61,0,0)
      ELSE
         CALL rewind(scr1)
         CALL rewind(geom1)
         IF ( nosclr/=0 ) CALL rewind(scr2)
         mult = 100
         IF ( maxa1>imax*10 ) mult = 10
         imax = (imax/mult)*1000
         maxa1 = -1
!WKBR CALL PAGE (-3)
         CALL page2(-3)
         IF ( mult==100 ) WRITE (Iout,99014) Uwm , lvl1
         IF ( mult==10 ) WRITE (Iout,99014) Uwm , lvl2
         GOTO 500
      ENDIF
   ENDIF
!
   n = i
   neqex = n
   n1 = n + 1
   n2 = n + 2
   igpdt = n2
   ilist = n2
   kn = n1/2
   CALL close(scr1,Clsrew)
   CALL close(scr2,Clsrew)
!
!     READ THE SEQGP TABLE (IF PRESENT)
!     FOR EACH ENTRY, FIND MATCH IN THE SORTED EXTERNAL GRID POINTS
!     AND REPLACE SEQUENCE NO. WITH SEQGP NO.
!
   noseq = 0
   Nogpdt = 1
   IF ( nogmp1==0 ) GOTO 1900
   ASSIGN 1700 TO ndx
   spoint(2) = 0
   ierr = 1
   ASSIGN 1600 TO nerr
   CALL locate(*1800,Z(buf1),seqgp,flag)
   noseq = 1
   ifail = 0
   DO
      CALL read(*6000,*1500,geomp,Z(n2),buf1-1,1,flag)
      ifail = ifail + 1
   ENDDO
!
!     NO GRID CARDS PRESENT-- TEST FOR ANY SCALAR PTS.
!
 1300 nogmp1 = 0
 1400 IF ( nosclr/=0 ) GOTO 600
!
!     ABNORMAL EXIT FROM GP1
!
   CALL close(scr1,Clsrew)
   CALL close(geom1,Clsrew)
   Nocstm = -1
   RETURN
 1500 IF ( ifail/=0 ) THEN
      nwds = (ifail-1)*(buf1-1) + flag
      WRITE (Iout,99002) Ufm , nwds
99002 FORMAT (A23,' 3135, UNABLE TO PROCESS SEQGP DATA IN SUBROUTINE ','GP1 DUE TO INSUFFICIENT CORE.',//5X,                        &
             &'ADDITIONAL CORE REQUIRED =',I10,7H  WORDS)
      CALL mesage(-61,0,0)
   ENDIF
!
!     CHECK FOR MULTIPLE REFERENCES TO GRID (OR SCALAR) POINT ID NOS.
!     AND SEQUENCE ID NOS. ON SEQGP CARDS
!
   k = n2
   kk = n2 + flag - 1
   jj = kk - 2
   DO
      DO i = k , jj , 2
         IF ( Z(i)>=0 .AND. i<kk ) THEN
            ii = i + 2
            ifail = 0
            DO j = ii , kk , 2
               IF ( Z(i)==Z(j) ) THEN
                  IF ( ifail==0 ) THEN
                     ifail = 1
                     nogo = 1
                     IF ( k/=n2 ) THEN
                        idseq1 = Z(i)/1000
                        irmndr = Z(i) - 1000*idseq1
                        IF ( irmndr/=0 .AND. mult>=10 ) THEN
                           idseq2 = irmndr/100
                           irmndr = irmndr - 100*idseq2
                           IF ( irmndr/=0 .AND. mult>=100 ) THEN
                              idseq3 = irmndr/10
                              irmndr = irmndr - 10*idseq3
                              IF ( irmndr/=0 ) THEN
                                 WRITE (Iout,99003) Ufm , idseq1 , idseq2 , idseq3 , irmndr
99003                            FORMAT (A23,' 3137, MULTIPLE REFERENCES TO SEQUENCE ID NO.',I6,1H.,I1,1H.,I1,1H.,I1,               &
                                   &'  ON SEQGP CARDS.')
                              ELSE
                                 WRITE (Iout,99004) Ufm , idseq1 , idseq2 , idseq3
99004                            FORMAT (A23,' 3137, MULTIPLE REFERENCES TO SEQUENCE ID NO.',I6,1H.,I1,1H.,I1,4X,'ON SEQGP CARDS.')
                              ENDIF
                           ELSE
                              WRITE (Iout,99005) Ufm , idseq1 , idseq2
99005                         FORMAT (A23,' 3137, MULTIPLE REFERENCES TO SEQUENCE ID NO.',I6,1H.,I1,5X,'ON SEQGP CARDS.')
                           ENDIF
                        ELSEIF ( axi/=0 ) THEN
                           IF ( axi==1 ) WRITE (Iout,99015) Ufm
                           axi = 2
                           nogo = 1
                        ELSE
                           WRITE (Iout,99006) Ufm , idseq1
99006                      FORMAT (A23,' 3137, MULTIPLE REFERENCES TO SEQUENCE ID NO.',I6,6X,' ON SEQGP CARDS.')
                        ENDIF
                     ELSE
                        WRITE (Iout,99007) Ufm , Z(i)
99007                   FORMAT (A23,' 3136, MULTIPLE REFERENCES TO GRID (OR SCALAR) POINT',' ID NO.',I9,'  ON SEQGP CARDS.')
                     ENDIF
                  ENDIF
                  Z(j) = -Z(j)
               ENDIF
            ENDDO
         ENDIF
!
         IF ( jj>=kk .AND. mult/=1000 ) THEN
            l = Z(i)
            IF ( mult<=10 ) THEN
               IF ( mult==1 ) THEN
                  IF ( axi==0 ) CALL mesage(-37,0,nam)
                  IF ( mod(l,1000)/=0 ) THEN
                     IF ( axi==1 ) WRITE (Iout,99015) Ufm
                     axi = 2
                     nogo = 1
                  ENDIF
                  CYCLE
               ELSEIF ( mod(l,100)==0 ) THEN
                  Z(i) = l/100
                  CYCLE
               ENDIF
            ELSEIF ( mod(l,10)==0 ) THEN
               Z(i) = l/10
               CYCLE
            ENDIF
            IF ( maxa1/=0 ) THEN
               maxa1 = 0
               nogo = 1
               WRITE (Iout,99008) Ufm
99008          FORMAT (A23,' 2140B, ILLEGAL DATA IN SEQGP CARD, POSSIBLY CAUSED',' BY LARGE GRID OR SCALAR POINTS')
            ENDIF
         ENDIF
      ENDDO
!
      IF ( k/=n2 ) THEN
!
         DO i = n2 , kk , 2
            IF ( Z(i)<0 ) Z(i) = -Z(i)
         ENDDO
         IF ( nogo/=1 ) THEN
!
!     CHECK TO SEE IF ANY SEQUENCE ID NO. ON SEQGP CARDS IS THE SAME
!     AS A GRID (OR SCALAR) POINT ID NO. THAT HAS NOT BEEN RESEQUENCED
!
            DO i = k , kk , 2
               IF ( Z(i)>=0 ) THEN
                  idseq1 = Z(i)/mult
                  irmndr = Z(i) - mult*idseq1
                  IF ( irmndr==0 ) THEN
                     DO j = n2 , kk , 2
                        IF ( idseq1==Z(j) ) GOTO 1510
                     ENDDO
                     DO j = 1 , n1 , 2
                        IF ( idseq1==Z(j) ) GOTO 1502
                     ENDDO
                  ENDIF
                  CYCLE
 1502             nogo = 1
                  WRITE (Iout,99009) Ufm , idseq1
99009             FORMAT (A23,' 3138, SEQUENCE ID NO.',I6,' ON SEQGP CARDS IS THE ','SAME AS A',/5X,                                &
                         &'GRID (OR SCALAR) POINT ID NO. THAT HAS ','NOT BEEN RESEQUENCED.')
               ENDIF
 1510       ENDDO
         ENDIF
         i = -1
         EXIT
      ELSE
         jj = kk
         k = k + 1
      ENDIF
   ENDDO
 1600 i = i + 2
   IF ( i<=flag ) THEN
      a(1) = Z(n2+i-1)
      a(2) = Z(n2+i)
      GOTO 5600
!
!     SORT THE CORE TABLE BY INTERNAL GRID PT NO
!     THUS FORMING THE GPL (EXTERNAL GRID PT NOS IN SORT BY INTERNAL NO)
!
   ELSEIF ( nogo/=0 ) THEN
      CALL mesage(-61,0,0)
      GOTO 5900
   ELSE
      CALL sorti(0,0,2,2,Z,n1)
      GOTO 1800
   ENDIF
 1700 Z(2*k) = a(2)
   GOTO 1600
!
!     CLOSE GEOM1. WRITE THE GPL. FIRST RECORD IS A SINGE ENTRIED LIST
!     OF EXTERNAL GRID NOS. IN INTERNAL SORT. SECOND RECORD IS A DOUBLE
!     ENTRIED LIST OF EXTERAL GRID NO., SEQUENCE NO. (SORT IS INTERNAL).
!     ADD THE MULTIPLIER, MULT, TO THE 3RD WORD OF GPL HEADER RECORD
!
 1800 IF ( nogmp1/=0 ) CALL close(geom1,Clsrew)
 1900 CALL fname(gpl,a)
   file = gpl
   CALL open(*5900,gpl,Z(buf1),Wrtrew)
   a(3) = mult
   CALL write(gpl,a,3,1)
   DO i = 1 , n , 2
      CALL write(gpl,Z(i),1,0)
   ENDDO
   CALL write(gpl,0,0,1)
   CALL write(gpl,Z,n1,1)
   CALL close(gpl,Clsrew)
   mcb(1) = gpl
   CALL wrttrl(mcb)
!
!     FORM INTERNAL INDEX FOR EACH EXTERNAL GRID PT. NO.
!
   i = 2
   Z(i) = 1
   IF ( n/=1 ) THEN
      DO i = 3 , n , 2
         Z(i+1) = Z(i-1) + 1
      ENDDO
!
!     TEST TO SEE IF EXTERNAL GRID PT NOS ARE STILL IN EXTERNAL SORT
!     I.E., IF NO SEQGP TABLE, THEN SORT IS MAINTAINED
!     OTHERWISE, SORT ON EXTERNAL GRID NO.
!
      IF ( noseq/=0 ) CALL sorti(0,0,2,1,Z,n1)
   ENDIF
!
!     DETERMINE IF THE GPDT CAN BE HELD IN CORE
!     NWDS= TOTAL NO OF WORDS IN THE GPDT
!     M= MAX NO OF ENTRIES CORE CAN HOLD WITH ONE BUFFER OPEN
!     IF NWDS/7.LE.M,CORE WILL HOLD THE GPDT
!     OTHERWISE THE FILE SORT ROUTINE WILL BE USED
!
   nwds = 7*kn
   m = (buf1-n1)/7
   gpfl = 0
   IF ( kn>m ) gpfl = 7
   file = scr1
!
!     READ THE GRID AND SPOINT TABLES FROM SCR1
!     REPLACE THE EXTERNAL GRID PT NO WITH THE INTERNAL INDEX
!     IF CORE WILL HOLD THE GPDT, USE THE INTERNAL INDEX AS A POINTER
!     OTHERWISE, WRITE THE UNSORTED GPDT ON SCR2
!
   CALL open(*5900,scr1,Z(buf1),Rdrew)
   file = scr2
   IF ( gpfl/=0 ) CALL open(*5900,scr2,Z(buf2),Wrtrew)
   file = scr1
   ASSIGN 2100 TO ndx
   ierr = 2
   ASSIGN 2000 TO nerr
 2000 CALL read(*6000,*2200,scr1,a,7,0,flag)
   GOTO 5600
 2100 IF ( gpfl/=0 ) THEN
      CALL write(scr2,a,7,0)
   ELSE
      j = n1 + 7*(a(1)-1)
      DO k = 1 , 7
         i = j + k
         Z(i) = a(k)
      ENDDO
   ENDIF
   GOTO 2000
 2200 IF ( nogo/=0 ) THEN
      CALL mesage(-61,0,0)
      GOTO 5900
   ELSE
      CALL close(scr1,Clsrew)
!
!     OPEN OUTPUT FILE FOR GPDT AND WRITE HEADER DATA
!     IF GPDT IS IN CORE, WRITE IT OUT
!
      file = gpdt
      CALL fname(gpdt,a)
      CALL open(*5900,gpdt,Z(buf1),Wrtrew)
      CALL write(gpdt,a,2,1)
      IF ( gpfl/=0 ) THEN
!
!     IF GPDT NOT IN CORE, CALL SORT
!
         Nfile(1) = scr1
         Nfile(2) = cstm
         Nfile(3) = bgpdt
         CALL close(scr2,Clsrew)
         file = scr2
         CALL open(*5900,scr2,Z(buf2),Rdrew)
         CALL sorti(scr2,gpdt,7,1,Z(igpdt),buf2-igpdt)
         CALL close(scr2,Clsrew)
      ELSE
         CALL write(gpdt,Z(igpdt),nwds,1)
      ENDIF
      CALL close(gpdt,Clsrew)
      mcb(1) = gpdt
      CALL wrttrl(mcb)
!
!     READ THE CORDIJ TABLES INTO CORE (IF PRESENT)
!
      Ifl = -1
      m = icsdt
      nolist = 0
      IF ( nogmp1==0 ) GOTO 4000
      ndx = buf1 - 15
      ncore = buf1 - 15
      DO i = icsdt , buf1
         Z(i) = 0
      ENDDO
      file = geomp
      CALL preloc(*5900,Z(buf1),geomp)
      DO i = 1 , 6
         ij = i + i - 1
         CALL locate(*2250,Z(buf1),cordij(ij),flag)
         Ifl = 1
         DO
            CALL read(*6000,*2250,geomp,Z(m),cord(i),0,flag)
            m = m + 16
            IF ( m>ncore ) CALL mesage(-8,0,gp1ah)
         ENDDO
 2250 ENDDO
      CALL close(geomp,Clsrew)
      m = m - 16
      ncsdt = m
!
!     TEST FOR PRESENCE OF ANY CORDIJ TABLES
!
      IF ( Ifl==-1 ) GOTO 4000
!
!     REPLACE EXTERNAL GRID PT NO IN CORD1J ENTRIES (IF ANY)
!     WITH CORRESPONDING INTERNAL INDEX
!     SAVE A TABLE OF GRID PTS REFERENCED ON CORD1J ENTRIES
!
      jj = icsdt
      ilist = ncsdt + 16
      ii = ilist - 1
      ncore = buf1 - 3
      ierr = 3
   ENDIF
 2300 IF ( Z(jj+2)/=1 ) GOTO 3000
   nolist = 1
   ASSIGN 2400 TO ndx
   ASSIGN 2500 TO nerr
   a(1) = Z(jj+3)
   spoint(2) = Z(jj+1)
   GOTO 5600
 2400 Z(jj+3) = a(1)
   Z(ii+1) = a(1)
 2500 ASSIGN 2600 TO ndx
   ASSIGN 2700 TO nerr
   a(1) = Z(jj+4)
   GOTO 5600
 2600 Z(jj+4) = a(1)
   Z(ii+2) = a(1)
 2700 ASSIGN 2800 TO ndx
   ASSIGN 2900 TO nerr
   a(1) = Z(jj+5)
   GOTO 5600
 2800 Z(jj+5) = a(1)
   Z(ii+3) = a(1)
 2900 ii = ii + 3
   IF ( ii>ncore ) CALL mesage(-8,0,gp1ah)
 3000 jj = jj + 16
   IF ( jj<=ncsdt ) GOTO 2300
   IF ( nogo/=0 ) THEN
      CALL mesage(-61,0,0)
      GOTO 5900
   ELSE
!
!     IF ANY CORD1J ENTRIES, PASS THE GPDT AND CREATE A TABLE OF THE
!     REFERENCED GRID PTS. THIS TABLE IS CALLED CSGP
!
      IF ( nolist/=0 ) THEN
         nlist = ii
         icsgp = nlist + 1
         CALL sorti(0,0,1,1,Z(ilist),icsgp-ilist)
         Z(icsgp) = 0
         jj = ilist
         DO kk = ilist , nlist
            IF ( Z(kk+1)/=Z(kk) ) THEN
               Z(jj) = Z(kk)
               jj = jj + 1
            ENDIF
         ENDDO
         nlist = jj - 1
         icsgp = jj
         file = gpdt
         CALL open(*5900,gpdt,Z(buf1),Rdrew)
         CALL fwdrec(*6000,gpdt)
         ncore = buf1 - 5
         i = ilist
         DO
            CALL read(*6000,*6100,gpdt,Z(jj),7,0,flag)
            IF ( Z(jj)==Z(i) ) THEN
               jj = jj + 5
               IF ( jj>ncore ) CALL mesage(-8,0,gp1ah)
               i = i + 1
               IF ( i>nlist ) THEN
                  ncsgp = jj - 5
                  CALL close(gpdt,Clsrew)
                  EXIT
               ENDIF
            ENDIF
         ENDDO
      ENDIF
!
!     LOOP THRU THE CSDT SOLVING AS MANY COORDINATE SYSTEMS AS POSSIBLE
!     ON EACH PASS.
!
      nn = (ncsdt-icsdt)/16 + 1
      solv = 0
      solvp = 0
      ii = icsdt
   ENDIF
 3100 IF ( Z(ii+2)<2 ) THEN
!
!     *****  TYPE = 1 *****
!     CHECK TO SEE IF EACH OF THE 3 REFERENCE GRID PTS IS IN BASIC SYS
!     IF SO,CALCULATE THE TRANSFORMATION TO BASIC AND SET COORD SYSTEM
!     AS SOLVED, IF NOT CONTINUE TO NEXT COORDINATE SYSTEM
!
      i = 0
   ELSEIF ( Z(ii+2)==2 ) THEN
!
!     ***** TYPE = 2 *****
!     CHECK THE DEFINING LOCAL COORDINATE SYSTEM
!     IF BASIC, SOLVE AS IN TYPE=1
!     IF NOT BASIC, FIND THE REFERENCED COORD SYSTEM AND TEST IF THAT
!     SYSTEM IS SOLVED. IF YES, CALCULATE THE TRANSFORMATION TO BASIC
!     IF NO, CONTINUE THRU THE CSDT
!
      IF ( Z(ii+3)/=0 ) THEN
         i = icsdt
         DO WHILE ( Z(i)/=Z(ii+3) )
            i = i + 16
            IF ( i>ncsdt ) THEN
               spoint(1) = Z(ii)
               spoint(2) = Z(ii+3)
               ierr = 4
               CALL mesage(-30,ierr,spoint)
               GOTO 6100
            ENDIF
         ENDDO
         IF ( Z(i+2)/=3 .OR. Z(i+3)/=0 ) GOTO 3500
         k = 0
         ASSIGN 3400 TO ndx
         GOTO 3300
      ELSE
         DO i = 1 , 9
            k = ii + i
            aa(i) = Zz(k+3)
         ENDDO
         GOTO 5500
      ENDIF
   ELSE
!
!     ***** TYPE = 3 *****
!     CHECK THE DEFINING LOCAL COORDINATE SYSTEM
!     IF BASIC, CONTINUE THRU CSDT
!     IF NOT BASIC, ERROR CONDITION
!
      IF ( Z(ii+3)==0 ) GOTO 3500
      CALL mesage(-30,ierr,spoint)
      GOTO 6100
   ENDIF
 3200 k = ii + i
   j = icsgp - 1
   DO WHILE ( Z(j+1)/=Z(k+3) )
      j = j + 5
      IF ( j>=ncsgp ) GOTO 6200
   ENDDO
   IF ( Z(j+2)/=0 ) GOTO 3500
   k = i*3
   aa(k+1) = Zz(j+3)
   aa(k+2) = Zz(j+4)
   aa(k+3) = Zz(j+5)
   i = i + 1
   IF ( i>2 ) GOTO 5500
   GOTO 3200
 3300 l = k + ii
   ax(1) = Zz(l+4)
   ax(2) = Zz(l+5)
   ax(3) = Zz(l+6)
   IF ( Z(i+1)<2 ) GOTO 5200
   IF ( Z(i+1)==2 ) GOTO 5300
   GOTO 5400
 3400 aa(k+1) = ar(1)
   aa(k+2) = ar(2)
   aa(k+3) = ar(3)
   k = k + 3
   IF ( k>6 ) GOTO 5500
   GOTO 3300
!
!     TEST FOR COMPLETION OF PASS THRU CSDT
!
 3500 ii = ii + 16
   IF ( ii<=ncsdt ) GOTO 3100
!
!     LOOP THRU THE CSGP (IFPRESENT) AND TRANSFORM ALL
!     POSSIBLE GRID PTS TO BASIC
!
   IF ( nolist==0 ) GOTO 3900
   jj = icsgp
 3600 IF ( Z(jj+1)==0 ) GOTO 3800
   i = icsdt
   DO WHILE ( Z(jj+1)/=Z(i) )
      i = i + 16
      IF ( i>ncsdt ) THEN
         ierr = 6
         spoint(1) = Z(jj)
         spoint(2) = Z(jj+1)
         CALL mesage(-30,ierr,spoint)
         GOTO 6100
      ENDIF
   ENDDO
   IF ( Z(i+2)/=3 .OR. Z(i+3)/=0 ) GOTO 3800
   ax(1) = Zz(jj+2)
   ax(2) = Zz(jj+3)
   ax(3) = Zz(jj+4)
   ASSIGN 3700 TO ndx
   IF ( Z(i+1)<2 ) GOTO 5200
   IF ( Z(i+1)==2 ) GOTO 5300
   GOTO 5400
 3700 Zz(jj+2) = ar(1)
   Zz(jj+3) = ar(2)
   Zz(jj+4) = ar(3)
   Zz(jj+1) = 0
 3800 jj = jj + 5
   IF ( jj<=ncsgp ) GOTO 3600
!
!     TEST TO SEE IF ALL COORDINATE SYSTEMS SOLVED
!     IF NOT, TEST TO SEE IF ANY NEW SOLUTIONS ON LAST PASS
!     IF NONE, INCONSISTANT DEFINITION OF COORDINATE SYSTEMS
!     OTHERWISE LOOP BACK THRU THE CSDT
!
 3900 IF ( solv==nn ) THEN
!
!     WRITE THE CSTM
!
      CALL fname(cstm,a)
      file = cstm
      CALL open(*5900,cstm,Z(buf1),Wrtrew)
      CALL write(cstm,a,2,1)
      DO ii = icsdt , ncsdt , 16
         CALL write(cstm,Z(ii),2,0)
         CALL write(cstm,Z(ii+4),12,0)
      ENDDO
      CALL close(cstm,Clsrew)
      Nocstm = nn
      mcb(3) = nn
      mcb(1) = cstm
      CALL wrttrl(mcb)
   ELSEIF ( solv==solvp ) THEN
      spoint(1) = 0
      spoint(2) = 0
      ierr = 5
      CALL mesage(-30,ierr,spoint)
      GOTO 6100
   ELSE
      solvp = solv
      ii = icsdt
      GOTO 3100
   ENDIF
!
!     OPEN EQEXIN AND WRITE HEADER RECORD.
!     THEN WRITE FIRST RECORD (PAIRS OF EXTERNAL GRID NO., INTERNAL NO.
!     IN EXTERNAL SORT).
!
 4000 file = eqexin
   CALL open(*5900,eqexin,Z(buf1),Wrtrew)
   CALL fname(eqexin,a)
   CALL write(eqexin,a,2,1)
   CALL write(eqexin,Z,n1,1)
   CALL close(eqexin,Cls)
!
!     A LIST OF DEGREES OF FREEDOM FOR EACH GRID OR SCALAR POINT IS
!     FORMED BEGINNING AT Z(ILIST) BY READING GEOM2 AND USING THE
!     CONNECTION INFORMATION IN CONJUNCTION WITH THE ELEM TABLE IN
!     /GPTA1/.
!
   file = geom2
   ilist0 = ilist - 1
   nlist = ilist + (neqex+1)/2
   IF ( nlist>=buf3 ) CALL mesage(-8,0,gp1ah)
   DO i = ilist , nlist
      Z(i) = 0
   ENDDO
   jerr = 0
   CALL open(*4300,geom2,Z(buf1),Rdrew)
 4100 CALL fwdrec(*6000,geom2)
 4200 CALL ectloc(*4300,geom2,a,i)
!
!     ELEMENT TYPE LOCATED--PREPARE TO PROCESS EACH ELEMENT
!
   IF ( Elem(i+9)==0 ) GOTO 4100
   j1 = Elem(i+12)
   nread = j1 + Elem(i+9) - 1
   nskip = -(Elem(i+5)-nread)
   maxdof = Elem(i+24)
   itype = Elem(i+2)
   DO
!
!     READ CONNECTION DATA FOR ELEMENT AND LOCATE EXT. GRID NBR IN
!     EQEXIN UPDATE DOF LIST FOR EACH GRID NBR
!
      CALL read(*6000,*4200,geom2,a,nread,0,m)
      DO i = j1 , nread
         IF ( a(i)==0 ) CYCLE
         CALL bisloc(*4220,a(i),Z,2,kn,k)
         j = ilist0 + Z(k+1)
         IF ( itype>=76 .AND. itype<=79 ) THEN
!
!     FLUID ELEMENT (CFHEX1,CFHEX2,CFWEDGE,CFTETRA)
!
            IF ( Z(j)>0 ) GOTO 4240
!
            Z(j) = -1
            CYCLE
         ELSE
!
!     STRUCTURE ELEMENT AND OTHERS
!
            IF ( Z(j)<0 ) GOTO 4240
            Z(j) = max0(Z(j),maxdof)
            CYCLE
         ENDIF
 4220    WRITE (Iout,99010) Ufm , a(1) , a(i)
99010    FORMAT (A23,' 2007, ELEMENT',I8,' REFERENCES UNDEFINED GRID ','POINT',I8)
         jerr = jerr + 1
         CYCLE
 4240    WRITE (Iout,99011) Ufm , a(i)
99011    FORMAT (A23,' 8011, GRID POINT',I8,' HAS BOTH STRUCTURE AND ','FLUID ELEMENTS CONNECTED')
         jerr = jerr + 1
      ENDDO
      CALL read(*6000,*4200,geom2,a,nskip,0,m)
   ENDDO
!
!     END-OF-FILE ON GEOM2---IF FATAL ERRORS, TERMINATE
!
 4300 IF ( jerr/=0 ) CALL mesage(-61,a,Z)
!
!     OPEN BGPDT AND SIL. WRITE HEADER RECORDS. OPEN GPDT. SKIP HEADER.
!
   offset = rshift(kn,5)
   CALL fname(bgpdt,a)
   CALL fname(sil,a(3))
   file = bgpdt
   CALL open(*5900,bgpdt,Z(buf1),Wrtrew)
   file = sil
   CALL open(*5900,sil,Z(buf2),Wrtrew)
   file = gpdt
   CALL open(*5900,gpdt,Z(buf3),Rdrew)
   CALL fwdrec(*6000,gpdt)
   CALL write(bgpdt,a,2,1)
   CALL write(sil,a(3),2,1)
   Luset = 1
!
!     READ AN ENTRY FROM THE GPDT.
!     TEST FOR DEFINING COORDINATE SYSTEM.
!
 4400 CALL read(*6000,*5100,gpdt,a,7,0,flag)
   IF ( a(2)<0 ) GOTO 4800
   IF ( a(2)==0 ) GOTO 4700
!
!     COORDINATE SYSTEM NOT BASIC--
!     USE CSDT IN CORE TO TRANSFORM TO BASIC.
!
   IF ( Nocstm/=-1 ) THEN
      i = icsdt
      DO WHILE ( Z(i)/=a(2) )
         i = i + 16
         IF ( i>ncsdt ) GOTO 4500
      ENDDO
      ax(1) = aa(3)
      ax(2) = aa(4)
      ax(3) = aa(5)
      ASSIGN 4600 TO ndx
      IF ( Z(i+1)<2 ) GOTO 5200
      IF ( Z(i+1)==2 ) GOTO 5300
      GOTO 5400
   ENDIF
 4500 ierr = 6
   spoint(1) = a(1)
   spoint(2) = a(2)
   CALL mesage(-30,ierr,spoint)
   GOTO 6100
 4600 aa(3) = ar(1)
   aa(4) = ar(2)
   aa(5) = ar(3)
!
!     GRID POINT NOW BASIC--
!     STORE DISPLACEMENT SYSTEM COORD. SYSTEM ID AND SET TYPE.
!     MAKE SURE DISPLACEMENT COORD. SYSTEM IS DEFINED.
!
 4700 a(2) = a(6)
   type = 1
   khr = ilist0 + a(1)
   incr = Z(khr)
!
!     IF INCR NEGATIVE - SPECIAL HYDROELASTIC GRID POINT WITH SINGLE
!     DEGREE OF FREEDOM
!
   IF ( incr>=0 ) THEN
      IF ( incr==0 ) incr = 6
!
!     ///////////////////////////////
!
!     TEMP PATCH
!
      incr = max0(incr,6)
!
!     ///////////////////////////////
!
      IF ( a(2)==0 .AND. Itherm==0 ) GOTO 4900
!
!     IF A(2) WHICH EQUALS A(6) IS EQUAL TO -1 THEN A FLUID GRID POINT
!     AS CREATED BY IFP4 IS AT HAND AND HAS ONLY 1 DEGREE OF FREEDOM
!     ..... IF -HEAT- PROBLEM THEN ALL GRIDS HAVE 1 DEGREE OF FREEDOM.
!
      IF ( a(2)/=(-1) .AND. Itherm<=0 ) THEN
         IF ( Nocstm/=-1 ) THEN
            DO ijk = icsdt , ncsdt , 16
               IF ( a(2)==Z(ijk) ) GOTO 4900
            ENDDO
         ENDIF
         nogo = 1
         CALL mesage(30,104,a(2))
         GOTO 4900
      ENDIF
   ENDIF
!
!     SCALAR POINT-- SET TYPE.
!
   a(2) = 0
   a(6) = 0
 4800 type = 2
   incr = 1
!
!     WRITE ENTRY ON BGPDT AND SIL.
!
 4900 CALL write(bgpdt,a(2),4,0)
   CALL write(sil,Luset,1,0)
!
!     REPLACE INTERNAL NO. IN EQEXIN WITH CODED SIL NO.
!     THEN INCREMENT SIL NO.
!
   ncode = 10*Luset + type
   IF ( noseq/=0 ) THEN
      ncode = -ncode
      lmt1 = max0(2*(a(1)-offset),2)
      DO k = lmt1 , n1 , 2
         IF ( Z(k)==a(1) ) GOTO 5000
      ENDDO
      DO k = 2 , lmt1 , 2
         IF ( Z(k)==a(1) ) GOTO 5000
      ENDDO
      CALL mesage(-30,2,a)
   ELSE
      k = 2*a(1)
      IF ( Z(k)/=a(1) ) CALL mesage(-30,2,a)
   ENDIF
 5000 Z(k) = ncode
   Luset = Luset + incr
   GOTO 4400
!
!     CLOSE BGPDT AND SIL. WRITE TRAILERS.
!
 5100 CALL close(bgpdt,Clsrew)
   CALL close(sil,Clsrew)
   CALL close(gpdt,Clsrew)
   Luset = Luset - 1
!    2147483647   = 2**31-1
   IF ( Luset>2147483647 ) THEN
      WRITE (Iout,99012) Ufm , Luset
99012 FORMAT (A23,' 3175, TOTAL NUMBER OF DEGREES OF FREEDOM IN THE ','PROBLEM (',I11,' ) EXCEEDS 2,147,483,647 (I.E., ''2**31 - 1)'&
            & )
99013 FORMAT (A29,' 3175, PROBLEM SIZE,',I8,' DOF''S, EXCEEDS THE OLD ','LIMIT OF 65535.',/5X,'GOOD NEWS, JOB WILL CONTINUE')
      CALL mesage(-61,0,0)
   ENDIF
   mcb(1) = bgpdt
   mcb(3) = 0
   CALL wrttrl(mcb)
   mcb(1) = sil
   mcb(3) = Luset
   CALL wrttrl(mcb)
!
!     IF GRID NOS. ARE RESEQUENCED, SWITCH SIGN ON CODED SIL NO.
!     WRITE SECOND RECORD OF EQEXIN. CLOSE FILE AND WRITE TRAILER.
!
   IF ( noseq/=0 ) THEN
      DO k = 2 , n1 , 2
         Z(k) = -Z(k)
      ENDDO
   ENDIF
   file = eqexin
   CALL open(*5900,eqexin,Z(buf1),Wrt)
   CALL write(eqexin,Z,n1,1)
   CALL close(eqexin,Clsrew)
   mcb(1) = eqexin
   mcb(3) = 0
   CALL wrttrl(mcb)
   CALL sswtch(36,k)
   IF ( k==1 ) CALL diag36(Z,buf1,gpl,sil,eqexin)
   IF ( nogo/=0 ) CALL mesage(-61,0,0)
   RETURN
!
!     ===============================================================
!
!     INTERNAL SUBROUTINE TO TRANSFORM A RECTANGULAR GRID PT TO BASIC
!     I POINTS TO THE CSDT ENTRY WHERE THE TRANSFORMATION IS DEFINED
!     THE GRID PT TO BE TRANSFORMED IS STORED AT AX(1,2,3)
!     THE TRANSFORMED GRID PT WILL BE STORED AT AR(1,2,3)
!
 5200 ar(1) = Zz(i+7)*ax(1) + Zz(i+8)*ax(2) + Zz(i+9)*ax(3) + Zz(i+4)
   ar(2) = Zz(i+10)*ax(1) + Zz(i+11)*ax(2) + Zz(i+12)*ax(3) + Zz(i+5)
   ar(3) = Zz(i+13)*ax(1) + Zz(i+14)*ax(2) + Zz(i+15)*ax(3) + Zz(i+6)
   GOTO ndx
!
!     INTERNAL SUBROUTINE TO TRANSFORM A CYLINDRICAL GRID PT TO BASIC
!     R,THETA,Z IS STORED AX(1,2,3)
!
 5300 r = ax(1)
   ax(2) = Degra*ax(2)
   ax(1) = r*cos(ax(2))
   ax(2) = r*sin(ax(2))
   GOTO 5200
!
!
!     INTERNAL SUBROUTINE TO TRANSFORM A SPHERICAL GRID PT TO BASIC
!     RHO,THETA,PHI IS STORED AT AX(1,2,3)
!
 5400 ax(2) = Degra*ax(2)
   ax(3) = Degra*ax(3)
   rsth = ax(1)*sin(ax(2))
   rcth = ax(1)*cos(ax(2))
   ax(1) = rsth*cos(ax(3))
   ax(2) = rsth*sin(ax(3))
   ax(3) = rcth
   GOTO 5200
!
!
!     INTERNAL SUBROUTINE TO CALCULATE THE 3X3 TRANSFORMATION MATRIX
!     AND 3X1 TRANSLATION VECTOR GIVEN THREE POINTS IN THE BASIC SYSTEM
!     THE RESULTS ARE STORED BACK IN THE CSDT
!
!     STORE R0 = A IN THE CSDT
!
 5500 Zz(ii+4) = aa(1)
   Zz(ii+5) = aa(2)
   Zz(ii+6) = aa(3)
!
!     FORM B - A
!
   DO i = 1 , 3
      ak(i) = ab(i) - aa(i)
   ENDDO
!
!     FORM K = (B - A)/LENGTH(B - A)
!     FORM C - A
!
   length = sqrt(ak(1)**2+ak(2)**2+ak(3)**2)
   DO i = 1 , 3
      ak(i) = ak(i)/length
      ac(i) = ac(i) - aa(i)
   ENDDO
!
!     FORM K X (C - A)
!
   aj(1) = ak(2)*ac(3) - ak(3)*ac(2)
   aj(2) = ak(3)*ac(1) - ak(1)*ac(3)
   aj(3) = ak(1)*ac(2) - ak(2)*ac(1)
!
!     FORM J = (K X (C-A))/LENGTH(K X (C-A))
!
   length = sqrt(aj(1)**2+aj(2)**2+aj(3)**2)
   DO i = 1 , 3
      aj(i) = aj(i)/length
   ENDDO
!
!     FORM I = J X K
!
   ai(1) = aj(2)*ak(3) - aj(3)*ak(2)
   ai(2) = aj(3)*ak(1) - aj(1)*ak(3)
   ai(3) = aj(1)*ak(2) - aj(2)*ak(1)
!
!     STORE 3X3 ROTATION MATRIX = ((IX,JX,KX),(IY,JY,KY),(IZ,JZ,KZ))
!     IN THE CSDT
!
   Zz(ii+7) = ai(1)
   Zz(ii+8) = aj(1)
   Zz(ii+9) = ak(1)
   Zz(ii+10) = ai(2)
   Zz(ii+11) = aj(2)
   Zz(ii+12) = ak(2)
   Zz(ii+13) = ai(3)
   Zz(ii+14) = aj(3)
   Zz(ii+15) = ak(3)
!
!     SET WD 3 OF CSDT = 3 AND WD 4 = 0 TO INDICATE  SOLVED SYSTEM
!     INCREMENT SOLVED SYSTEM COUNT
!
   Z(ii+2) = 3
   Z(ii+3) = 0
   solv = solv + 1
   GOTO 3500
!
!
!     INTERNAL SUBROUTINE TO PERFORM BINARY SEARCH ON FIRST ENTRY
!     OF A DOUBLE ENTRIED TABLE STORED AT Z(1) THRU Z(N+1)
!
 5600 klo = 1
   khi = kn
 5700 k = (klo+khi+1)/2
 5800 IF ( a(1)<Z(2*k-1) ) THEN
      khi = k
   ELSEIF ( a(1)==Z(2*k-1) ) THEN
      a(1) = Z(2*k)
      GOTO ndx
   ELSE
      klo = k
   ENDIF
   IF ( khi-klo<1 ) THEN
      CALL mesage(30,ierr,a(1))
      nogo = 1
      GOTO nerr
   ELSEIF ( khi-klo==1 ) THEN
      IF ( k==klo ) THEN
         k = khi
      ELSE
         k = klo
      ENDIF
      klo = khi
      GOTO 5800
   ELSE
      GOTO 5700
   ENDIF
!
!
!     FATAL ERROR MESAGES
!
 5900 ndx = -1
   CALL mesage(ndx,file,gp1ah)
   GOTO 6200
 6000 ndx = -2
   CALL mesage(ndx,file,gp1ah)
   GOTO 6200
 6100 ndx = -3
   CALL mesage(ndx,file,gp1ah)
 6200 spoint(1) = Z(k+3)
   spoint(2) = Z(ii)
   ierr = 3
   CALL mesage(-30,ierr,spoint)
   GOTO 6100
99014 FORMAT (A25,' 2140A, DUE TO THE PRESENCE OF ONE OR MORE GRID OR ','SCALAR POINTS WITH VERY LARGE EXTERNAL ID''S, THE SEQGP',  &
            & /5X,'AND SEQEP CARDS, IF USED, ARE FORCED TO REDUCE FROM ','ALLOWABLE 4 SEQID LEVELS TO ',A29,/)
99015 FORMAT (A23,' 3137A, SEQGP CARDS WITH MORE THAN ONE SEQID LEVEL ','ARE ILLEGAL FOR AXISYSM. OR HYDROELAS. PROBLEM')
END SUBROUTINE gp1
