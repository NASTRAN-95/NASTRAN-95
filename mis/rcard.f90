
SUBROUTINE rcard(Out,Fmt,Nflag,In)
!DIR$ INTEGER=64
!
!     CDIR$ IS CRAY COMPILER DIRECTIVE. 64 BIT INTEGER IS USED LOCALLY
!
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Dum1(7) , Dum2(10) , F6 , Highpw , Ibufsz , Lowpw , Lsystm , Nlines , Npages
   LOGICAL Nogo
   CHARACTER*23 Ufm
   COMMON /lhpwx / Lowpw , Highpw
   COMMON /system/ Ibufsz , F6 , Nogo , Dum1 , Npages , Nlines , Dum2 , Lsystm
   COMMON /xmssg / Ufm
!
! Dummy argument declarations
!
   INTEGER Nflag
   INTEGER Fmt(1) , In(1) , Out(1)
!
! Local variable declarations
!
   INTEGER a67777 , bcd(16) , blank , blanks , charac , chars(7) , d , e , field , i , ichek , ifmt , int1 , iout , iretrn , it ,   &
         & izero , j , k , line(20) , max , minus , n , n8or16 , ndoubl(2) , nnn , npoint , nt(16) , num(10) , number , nwords , p ,&
         & period , places , plus , power , seq , sign , star , stars , type(16) , val(16) , word , word1 , word2 , zero
   LOGICAL blkon , deciml , double , expont , lminus , pass , seqgp
   INTEGER complf , khrfn1 , khrfn3 , lshift , rshift
   REAL fl1
   DOUBLE PRECISION xdoubl
   EXTERNAL complf , lshift , rshift
!
! End of declarations
!
   EQUIVALENCE (fl1,int1) , (xdoubl,ndoubl(1)) , (num(10),zero) , (chars(1),blank) , (chars(2),star) , (chars(3),plus) ,            &
    & (chars(4),minus) , (chars(5),period) , (chars(6),e) , (chars(7),d)
   DATA pass/.FALSE./ , blanks/4H    / , stars/4H****/ , line/20*4H    / , blank/1H / , star/1H*/ , plus/1H+/ , minus/1H-/ ,        &
       &period/1H./ , e/1HE/ , d/1HD/ , seq/3HSEQ/ , p/4HP   / , izero/0/
   DATA num/1H1 , 1H2 , 1H3 , 1H4 , 1H5 , 1H6 , 1H7 , 1H8 , 1H9 , 1H0/
!
   IF ( .NOT.(pass) ) THEN
      pass = .TRUE.
      a67777 = complf(0)
      a67777 = rshift(lshift(a67777,1),1)
!
      DO i = 1 , 10
         num(i) = khrfn1(izero,4,num(i),1)
      ENDDO
!
      DO i = 1 , 7
         chars(i) = khrfn1(izero,4,chars(i),1)
      ENDDO
      seq = khrfn3(izero,seq,1,0)
      p = khrfn3(izero,p,0,0)
   ENDIF
!
   field = 0
   nwords = 2
   n8or16 = 8
   word = 0
   iout = 0
   ifmt = 0
   seqgp = .FALSE.
 100  IF ( word==18 ) THEN
      Nflag = iout
      Fmt(ifmt+1) = -1
      RETURN
   ELSE
!
!     OPERATE ON 1 FIELD  (2 OR 4 WORDS), GET FIRST NON-BLANK CHARACTER.
!
      field = field + 1
      deciml = .FALSE.
      lminus = .FALSE.
      expont = .FALSE.
      double = .FALSE.
      blkon = .FALSE.
      places = 0
      it = 0
      sign = 0
      power = 0
!
!     READ 8 OR 16 CHARACTERS OF ONE FIELD
!
!     TYPE AS 0 = BLANK, -1 = BCD, +1 = INTEGER
!
      n = 0
      word1 = word + 1
      word = word + nwords
      DO i = word1 , word
         DO j = 1 , 4
            n = n + 1
            charac = khrfn1(izero,4,In(i),j)
            IF ( charac==blank ) THEN
               type(n) = 0
               val(n) = blank
            ELSE
               IF ( charac==zero ) THEN
                  k = 0
               ELSE
                  DO k = 1 , 9
                     IF ( charac==num(k) ) GOTO 105
                  ENDDO
                  type(n) = -1
                  val(n) = charac
                  GOTO 110
               ENDIF
 105           type(n) = 1
               val(n) = k
            ENDIF
 110        bcd(n) = charac
         ENDDO
      ENDDO
!
!     BCD, INTEGER TRANSFER ON FIRST NON-BLANK CHARACTER
!
      IF ( seqgp ) THEN
         IF ( field==3 .OR. field==5 .OR. field==7 .OR. field==9 ) THEN
!
!     **********************************************
!
!     FIRST CHARACTER OF FIELD 3, 5, 7,  OR 9 ON SEQGP CARD ENCOUNTERED.
!
!     IT HAS TO BE A 1 TO 9 FOR NO ERROR
!
            DO n = 1 , n8or16
               IF ( type(n)<0 ) GOTO 2100
               IF ( type(n)/=0 ) GOTO 1400
            ENDDO
            GOTO 200
         ENDIF
      ENDIF
!
      DO n = 1 , n8or16
         IF ( type(n)<0 ) GOTO 300
         IF ( type(n)/=0 ) GOTO 700
      ENDDO
!
!     ALL BLANK FIELD IF FALL HERE
!
      IF ( field==1 ) GOTO 400
   ENDIF
 200  iout = iout + 1
   Out(iout) = 0
   ifmt = ifmt + 1
   Fmt(ifmt) = 0
   GOTO 100
!
!     **********************************************
!
!     ALPHA HIT FOR FIRST CHARACTER
!
 300  IF ( field==1 .AND. val(n)==star ) THEN
!
!     **********************************************
!
!     FIRST CHARACTER  ON CARD IS A STAR
!
      nwords = 4
      n8or16 = 16
      GOTO 600
   ELSEIF ( val(n)==plus ) THEN
!
!     **********************************************
!
!     FIRST CHARACTER IN FIELD IS A PLUS
!
!
!     IGNORING PLUS SIGN AND NOW ASSUMING FIELD IS NUMBERIC
!
      IF ( field/=1 ) GOTO 800
      GOTO 600
   ELSEIF ( val(n)==period ) THEN
!
!     **********************************************
!
!     FIRST CHARACTER IN FIELD IS A PERIOD
!
      deciml = .TRUE.
      places = 0
      GOTO 800
   ELSEIF ( val(n)==minus ) THEN
!
!     **********************************************
!
!     FIRST CHARACTER IN FIELD IS A MINUS
!
      lminus = .TRUE.
      GOTO 800
!
!     PLAIN ALPHA BCD FIELD NOW ASSUMED.
!     CHECKING FOR DOULBE-FIELD * IF WE ARE IN FIELD 1
!
   ELSEIF ( field==1 ) THEN
      IF ( bcd(8)==star .AND. type(8)==-1 ) THEN
         nwords = 4
         n8or16 = 16
!
!     REMOVE STAR BEFORE PUTTING 2 BCD WORDS INTO OUT
!
         bcd(8) = blank
      ENDIF
   ENDIF
 400  iout = iout + 2
   IF ( type(1)/=0 ) THEN
      IF ( nwords/=4 .OR. field/=1 ) THEN
         n = word - nwords
         Out(iout-1) = In(n+1)
         Out(iout) = In(n+2)
         GOTO 500
      ENDIF
   ENDIF
!
!     CHARACTER N WAS FIRST NON-BLANK CHARACTER
!
   max = n8or16 - n + 1
   DO i = 1 , max
      bcd(i) = bcd(n)
      n = n + 1
   ENDDO
   DO WHILE ( max<8 )
      max = max + 1
      bcd(max) = blank
   ENDDO
   word1 = 0
   word2 = 0
   DO i = 1 , 4
      word1 = khrfn3(bcd(i),word1,1,1)
      word2 = khrfn3(bcd(i+4),word2,1,1)
   ENDDO
   Out(iout-1) = word1
   Out(iout) = word2
 500  ifmt = ifmt + 1
   Fmt(ifmt) = 3
   IF ( field==1 ) THEN
      IF ( khrfn3(izero,Out(iout-1),1,0)==seq .AND. khrfn3(izero,Out(iout),0,0)==p ) seqgp = .TRUE.
   ENDIF
   GOTO 100
 600  iout = iout + 2
   Out(iout-1) = 0
   Out(iout) = 0
   ifmt = ifmt + 1
   Fmt(ifmt) = 3
   GOTO 100
!
!     **********************************************
!
!     0 TO 9 NUMERIC HIT
!
 700  IF ( val(n)/=0 ) THEN
!
!     NON-ZERO NUMBER.  SAVING IT NOW IN TABLE NT
!
      nt(1) = val(n)
      it = 1
   ENDIF
 800  IF ( n/=n8or16 ) THEN
!
!     PROCESS REMAINING DIGITS
!
      nnn = n + 1
      DO n = nnn , n8or16
         IF ( .NOT.((type(n)==0 .OR. val(n)==zero) .AND. it==0 .AND. .NOT.deciml) ) THEN
            IF ( type(n)<=0 ) THEN
!
!     FALL THRU IMPLIES NON 0 TO 9 CHARACTER
!
               IF ( val(n)/=period ) GOTO 1100
               IF ( deciml ) GOTO 1900
               places = 0
               deciml = .TRUE.
            ELSE
!
!     0 TO 9 CHARACTER HIT. SAVE IT.
!
               it = it + 1
               nt(it) = val(n)
               IF ( deciml ) places = places + 1
            ENDIF
         ENDIF
      ENDDO
   ENDIF
!
!     NUMERIC WORD COMPLETED
!     IF DECIML IS .FALSE. NUMERIC IS A SIMPLE INTEGER
!
   IF ( deciml ) GOTO 1300
!
!     **********************************************
!
!     SIMPLE INTEGER
!
 900  number = 0
   IF ( it/=0 ) THEN
      DO i = 1 , it
         IF ( ((a67777-nt(i))/10)<number ) GOTO 1800
         number = number*10 + nt(i)
      ENDDO
   ENDIF
   IF ( lminus ) number = -number
 1000 iout = iout + 1
   Out(iout) = number
   ifmt = ifmt + 1
   Fmt(ifmt) = 1
   GOTO 100
!
!     **********************************************
!
!     PROBABLE (E, D, +, -) EXPONENT HIT OR BLANK
!
 1100 IF ( type(n)==0 ) THEN
!
!     BLANK HIT THUS ONLY AN EXPONENT OR BLANKS PERMITTED FOR BALANCE
!     OF FIELD
!
      DO WHILE ( n/=n8or16 )
         n = n + 1
         IF ( type(n)<0 ) GOTO 1200
         IF ( type(n)/=0 ) GOTO 2000
      ENDDO
!
!     FALL THRU ABOVE LOOP IMPLIES BALANCE OF FIELD WAS BLANKS
!
      IF ( .NOT.(deciml) ) GOTO 900
      GOTO 1300
   ENDIF
!
!     **********************************************
!
!     COMING HERE IMPLIES A NON-BLANK CHARACTER HAS BEEN HIT BEGINNING
!     AN EXPONENT. IT HAS TO BE A (+, -, D, OR E ) FOR NO ERROR
!
 1200 IF ( val(n)==plus ) THEN
      expont = .TRUE.
      sign = plus
   ELSEIF ( val(n)==e ) THEN
      expont = .TRUE.
   ELSEIF ( val(n)/=minus ) THEN
      IF ( val(n)/=d ) GOTO 2000
      expont = .TRUE.
      double = .TRUE.
   ELSE
      expont = .TRUE.
      sign = minus
   ENDIF
!
!     READ INTEGER POWER, WITH OR WITHOUT SIGN
!
   DO WHILE ( n/=n8or16 )
      n = n + 1
!
      IF ( type(n)<0 ) THEN
         IF ( val(n)==plus .OR. val(n)==minus ) THEN
            IF ( sign/=0 ) EXIT
            sign = val(n)
            CYCLE
         ENDIF
      ELSEIF ( type(n)==0 ) THEN
         CYCLE
      ENDIF
!
!     FIRST DIGIT OF INTEGER POWER AT HAND NOW
!
      power = 0
      blkon = .FALSE.
!
      DO WHILE ( type(n)>0 )
         power = power*10 + val(n)
!
!     GET ANY MORE DIGITS IF PRESENT
!
         DO WHILE ( n/=n8or16 )
            n = n + 1
            IF ( .NOT.(blkon) ) THEN
               IF ( type(n)/=0 ) GOTO 1250
!
!     BLANK HIT, BALANCE OF FIELD MUST BE BLANKS
!
               blkon = .TRUE.
            ELSEIF ( type(n)/=0 ) THEN
               WRITE (F6,99011) Ufm
               WRITE (F6,99001)
99001          FORMAT (10X,23HPOSSIBLE IMBEDDED BLANK)
               GOTO 2300
            ENDIF
         ENDDO
         GOTO 1300
 1250 ENDDO
      EXIT
   ENDDO
   GOTO 2000
!
!     **********************************************
!
!     SINGLE OR DOUBLE PRECISION FLOATING POINT NUMBER
!     COMPLETE AND OUTPUT IT.
!
!     15 SIGNIFICANT FIGURES POSSIBLE ON INPUT
!     CONSIDERED SINGLE PRECISION UNLESS D EXPONENT IS PRESENT
!
 1300 IF ( sign==minus ) power = -power
   power = power - places
!
   number = 0
   IF ( it/=0 ) THEN
      IF ( it<7 ) THEN
         n = it
      ELSE
         n = 7
      ENDIF
      DO i = 1 , n
         number = number*10 + nt(i)
      ENDDO
   ENDIF
   xdoubl = dble(float(number))
   IF ( it>7 ) THEN
      number = 0
      DO i = 8 , it
         number = number*10 + nt(i)
      ENDDO
      xdoubl = xdoubl*10.0D0**(it-7) + dble(float(number))
   ENDIF
   IF ( lminus ) xdoubl = -xdoubl
!
!     CHECK FOR POWER IN RANGE OF MACHINE
!
   ichek = power + it
   IF ( xdoubl/=0.0D0 ) THEN
      IF ( ichek<Lowpw+1 .OR. ichek>Highpw-1 .OR. power<Lowpw+1 .OR. power>Highpw-1 ) THEN
!
!
         WRITE (F6,99011) Ufm
         WRITE (F6,99002)
99002    FORMAT (10X,42HFLOATING POINT NUMBER OUT OF MACHINE RANGE)
         GOTO 2300
      ELSE
!
         xdoubl = xdoubl*10.0D0**power
      ENDIF
   ENDIF
   ifmt = ifmt + 1
   IF ( double ) THEN
      iout = iout + 2
      Out(iout-1) = ndoubl(1)
      Out(iout) = ndoubl(2)
      Fmt(ifmt) = 4
   ELSE
      fl1 = xdoubl
      iout = iout + 1
      Out(iout) = int1
      Fmt(ifmt) = 2
   ENDIF
   GOTO 100
!
!     STORE NUMBER IN NT
!
 1400 npoint = 0
 1500 it = it + 1
   nt(it) = val(n)
 1600 DO WHILE ( n/=n8or16 )
      n = n + 1
!
!     GET NEXT CHARACTER
!
      IF ( npoint>0 .AND. .NOT.deciml .AND. .NOT.blkon ) THEN
!
         IF ( val(n)==period .AND. type(n)<0 ) GOTO 1700
      ELSEIF ( deciml ) THEN
!
         IF ( type(n)<=0 ) GOTO 2100
!
         deciml = .FALSE.
         GOTO 1500
      ELSEIF ( .NOT.(blkon) ) THEN
         IF ( type(n)>0 ) GOTO 1500
         IF ( val(n)==period ) GOTO 1700
      ENDIF
      IF ( type(n)/=0 ) GOTO 2100
      blkon = .TRUE.
   ENDDO
!
!     READY TO COMPUTE INTEGER VALUE OF SPECIAL SEQGP INTEGER
!
   npoint = 3 - npoint
   IF ( npoint<0 ) GOTO 2100
   IF ( npoint/=0 ) THEN
      DO k = 1 , npoint
         it = it + 1
         nt(it) = 0
      ENDDO
   ENDIF
!
!     COMPUTE NUMBER
!
   number = 0
   IF ( it/=0 ) THEN
      DO k = 1 , it
         IF ( ((a67777-nt(k))/10)<number ) GOTO 2200
         number = number*10 + nt(k)
      ENDDO
   ENDIF
   GOTO 1000
!
 1700 deciml = .TRUE.
   npoint = npoint + 1
   GOTO 1600
 1800 WRITE (F6,99011) Ufm
   WRITE (F6,99003)
99003 FORMAT (10X,38HINTEGER MAGNITUDE OUT OF MACHINE RANGE)
   GOTO 2300
 1900 WRITE (F6,99011) Ufm
   WRITE (F6,99004)
99004 FORMAT (10X,22HDATA NOT RECOGNIZEABLE)
   GOTO 2300
 2000 WRITE (F6,99011) Ufm
   WRITE (F6,99005)
99005 FORMAT (10X,26HPOSSIBLE ERROR IN EXPONENT)
   GOTO 2300
 2100 WRITE (F6,99011) Ufm
   WRITE (F6,99006)
99006 FORMAT (10X,30HINCORRECT DEWEY DECIMAL NUMBER)
   GOTO 2300
 2200 WRITE (F6,99011) Ufm
   WRITE (F6,99007)
99007 FORMAT (10X,49HINTERNAL CONVERSION OF DEWEY DECIMAL IS TOO LARGE)
 2300 word = (field-1)*nwords + 2
   ASSIGN 2500 TO iretrn
   word2 = stars
 2400 line(word) = word2
   line(word-1) = word2
   IF ( nwords/=2 .AND. field/=1 ) THEN
      line(word-2) = word2
      line(word-3) = word2
   ENDIF
   GOTO iretrn
 2500 IF ( nwords==4 ) THEN
      WRITE (F6,99008)
99008 FORMAT (10X,80H.   1  ..   2  AND  3  ..   4  AND  5  ..   6  AND  7  ..   8  AND  9  ..  10  .)
   ELSE
      WRITE (F6,99009)
99009 FORMAT (10X,80H.   1  ..   2  ..   3  ..   4  ..   5  ..   6  ..   7  ..   8  ..   9  ..  10  .)
   ENDIF
   WRITE (F6,99010) (In(i),i=1,20) , line
99010 FORMAT (10X,20A4)
   ASSIGN 2600 TO iretrn
   word2 = blanks
   GOTO 2400
 2600 iout = iout + 1
   Nlines = Nlines + 7
   Out(iout) = 0
   ifmt = ifmt + 1
   Fmt(ifmt) = -1
   Nogo = .TRUE.
   GOTO 100
99011 FORMAT (A23,' 300, DATA ERROR IN FIELD UNDERLINED.')
END SUBROUTINE rcard
