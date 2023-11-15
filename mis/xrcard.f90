
SUBROUTINE xrcard(Out,Nflag,In)
!
!     MODIFIED BY G.CHAN/UNISYS FOR EFFICIENCY,        2/1988
!     LAST REVISED, 8/1989, IMPROVED EFFICIENCY BY REDUCING CHARACTER
!     OPERATIONS (VERY IMPORTANT FOR CDC MACHINE)
!
!     REVERT TO NASTRAN ORIGIANL XRCARD ROUTINE (NOW CALLED YRCARD)
!     IF DIAG 42 IS TURNED ON
!     (THIS NEW XRCARD IS SEVERAL TIMES FASTER THAN YRCARD)
!
!
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Bufsz , Highpw , Lowpw , Nout
   LOGICAL Nogo
   CHARACTER*23 Ufm
   COMMON /lhpwx / Lowpw , Highpw
   COMMON /system/ Bufsz , Nout , Nogo
   COMMON /xmssg / Ufm
!
! Dummy argument declarations
!
   INTEGER Nflag
   INTEGER In(18) , Out(2)
!
! Local variable declarations
!
   INTEGER a67777 , a77777 , asave , astk1 , astk4 , blank1 , blank4 , char1(72) , char1i , char1n , charn2 , chars(23) , comma1 ,  &
         & cparn1 , d1 , diag , dollr1 , dot1 , e1 , equal1 , equal4 , i , ichar , ichek , idoubl(2) , imhere , intg , iout ,       &
         & ipower , irtn , it , k , l42 , last , minus1 , n , n1 , n2 , nt(15) , num1(10) , number , oparn1 , oparn4 , places ,     &
         & plus1 , precis , prev , psign , slash1 , slash4 , type(72) , zero1
   LOGICAL alpha , debug , delim , expont , minus , power
   CHARACTER*8 blank8 , numric , save8
   CHARACTER*23 char23
   CHARACTER*72 char72
   INTEGER complf , lshift , rshift
   DOUBLE PRECISION ddoubl
   REAL flpt
   CHARACTER*1 khar1(72) , save1(8)
   EXTERNAL complf , lshift , rshift
!
! End of declarations
!
   EQUIVALENCE (khar1(1),char72) , (save8,save1(1)) , (flpt,intg) , (ddoubl,idoubl(1)) , (chars(1),dollr1) , (chars(8),cparn1) ,    &
    & (chars(2),plus1) , (chars(9),e1) , (chars(3),equal1) , (chars(10),d1) , (chars(4),minus1) , (chars(11),dot1) ,                &
    & (chars(5),comma1) , (chars(12),blank1) , (chars(6),slash1) , (chars(13),astk1) , (chars(7),oparn1) , (chars(14),num1(1)) ,    &
    & (num1(10),zero1)
   DATA char23/'$+=-,/()ED. *1234567890'/ , blank8/'       '/
   DATA dollr1 , blank4 , diag , debug , numric/0 , 4H     , 4HDIAG , .FALSE. , 'NUMERIC'/
   DATA equal4 , slash4 , oparn4 , astk4/4H=    , 4H/    , 4H(    , 4H*   /
!
   IF ( dollr1==0 ) THEN
      CALL k2b(char23,chars,23)
      a77777 = complf(0)
      a67777 = rshift(lshift(a77777,1),1)
      prev = blank4
      CALL sswtch(42,l42)
      IF ( debug ) WRITE (Nout,99001)
99001 FORMAT (//5X,'INPUT DEBUG IN XRCARD ROUTINE')
   ENDIF
!
!         KXX=0
!
!     USE ORIGINAL XRCARD ROUTINE IF DIAG 42 IS TURNED ON
!
   IF ( prev==diag ) CALL sswtch(42,l42)
   IF ( l42==0 ) THEN
!
!     CONVERT 18 BCD WORDS IN 'IN' TO 72 CHARACTER STRING, AND SET TYPE
!
      CALL bcdkh7(In,char72)
      CALL k2b(char72,char1,72)
      IF ( debug ) WRITE (Nout,99002) char72
99002 FORMAT (/,' INPUT- ',A72)
!
!
      DO n = 1 , 72
         char1n = char1(n)
         IF ( char1n==blank1 ) THEN
            type(n) = 0
         ELSE
            DO k = 1 , 10
               IF ( char1n==num1(k) ) GOTO 20
            ENDDO
            type(n) = -1
         ENDIF
         CYCLE
 20      type(n) = 1
      ENDDO
      n = 73
      DO
         n = n - 1
         IF ( type(n)/=0 ) THEN
            last = n + 1
            IF ( last>72 ) last = 72
            alpha = .FALSE.
            delim = .TRUE.
            iout = 0
            n = 0
            asave = 1
            Out(asave) = 0
            save8 = blank8
            EXIT
         ENDIF
      ENDDO
   ELSE
      CALL yrcard(Out,Nflag,In)
      RETURN
   ENDIF
 100  DO WHILE ( n/=last )
      IF ( Nflag-iout<5 ) THEN
         WRITE (Nout,99003) Ufm
99003    FORMAT (A23,'300, ROUTINE XRCARD FINDS OUTPUT BUFFER TOO SMALL ','TO PROCESS CARD COMPLETELY')
         GOTO 1900
      ELSE
         minus = .FALSE.
         n = n + 1
         char1n = char1(n)
         IF ( type(n)<0 ) THEN
!                  BCD  BLANK NUMERIC
!
            IF ( char1n==plus1 .OR. char1n==minus1 .OR. char1n==dot1 ) THEN
!
!     PLUS, MINUS, OR DOT ENCOUNTERED
!
               IF ( char1n==minus1 ) minus = .TRUE.
               IF ( char1n/=dot1 ) n = n + 1
               IF ( n<=last ) GOTO 800
               GOTO 1400
            ELSE
               IF ( char1n==dollr1 ) GOTO 600
!
!     GOOD ALPHA FIELD OR DELIMITER
!
               IF ( .NOT.(alpha) ) THEN
                  IF ( (char1n==comma1 .OR. char1n==dollr1) .AND. (.NOT.delim) ) GOTO 600
                  IF ( char1n==cparn1 .AND. .NOT.delim ) GOTO 600
                  iout = iout + 1
                  asave = iout
                  Out(asave) = 0
                  alpha = .TRUE.
               ENDIF
               IF ( char1n==oparn1 .OR. char1n==slash1 .OR. char1n==equal1 .OR. char1n==comma1 .OR. char1n==astk1 .OR.              &
                  & char1n==dollr1 ) GOTO 600
               IF ( char1n==cparn1 ) GOTO 600
               ASSIGN 200 TO irtn
               imhere = 125
               GOTO 500
            ENDIF
         ELSEIF ( type(n)/=0 ) THEN
            GOTO 800
         ENDIF
      ENDIF
   ENDDO
   GOTO 1200
 200  Out(asave) = Out(asave) + 1
   iout = iout + 2
   delim = .FALSE.
   Out(iout-1) = blank4
   Out(iout) = blank4
   ichar = 0
   GOTO 400
 300  IF ( n==last ) GOTO 1200
   n = n + 1
   char1n = char1(n)
   IF ( type(n)<0 ) THEN
!                  BCD BLANK NUMERIC
!
      IF ( char1n==oparn1 .OR. char1n==slash1 .OR. char1n==equal1 .OR. char1n==comma1 .OR. char1n==astk1 .OR. char1n==dollr1 )      &
         & GOTO 600
      IF ( char1n==cparn1 ) GOTO 600
   ELSEIF ( type(n)==0 ) THEN
!
!     A BLANK CHARACTER IS ENCOUNTERED WHILE PROCESSING ALPHA STRING
!     IF THIS IS AT THE BEGINNING OF A NEW BCD WORD, GO TO 100
!     IF THIS IS AT THE END OF A BCD WORD, GO TO 170 TO WRAP IT UP
!
      IF ( ichar==0 ) GOTO 100
      ASSIGN 100 TO irtn
      imhere = 160
      GOTO 500
   ENDIF
!
!     RECONSTRUCT CHARACTERS INTO SAVE1 SPACE, UP TO 8 CHARACTERS ONLY
!
 400  IF ( ichar==8 ) GOTO 300
   ichar = ichar + 1
   save1(ichar) = khar1(n)
   imhere = 150
   IF ( debug ) WRITE (Nout,99014) save8 , imhere , ichar , iout
!
!     GO FOR NEXT CHARACTER
!
   IF ( ichar<8 ) GOTO 300
   ASSIGN 300 TO irtn
   imhere = 155
!
!     MOVE CHARACTER DATA IN SAVE8 TO OUT(IOUT-1) AND OUT(IOUT) IN BCD
!     WORDS
!
 500  IF ( save8/=blank8 ) CALL khrbc2(save8,Out(iout-1))
   IF ( debug ) THEN
      WRITE (Nout,99014) save8 , imhere , ichar , iout
      WRITE (Nout,99004) iout , Out(iout-1) , Out(iout) , delim , iout , Out(iout-1) , Out(iout)
99004 FORMAT ('   IOUT,OUT  =',I4,2H /,2A4,'/  DELIM=',L1,/14X,'=',I4,2H /,2I25,'/')
   ENDIF
   save8 = blank8
   GOTO irtn
!
!     DELIMITER HIT
!
 600  ASSIGN 700 TO irtn
   imhere = 180
   GOTO 500
 700  IF ( delim ) THEN
      IF ( iout==0 ) iout = 1
      iout = iout + 2
      Out(asave) = Out(asave) + 1
      Out(iout-1) = blank4
      Out(iout) = blank4
   ENDIF
   IF ( char1n==dollr1 ) THEN
      Out(iout+1) = a67777
      GOTO 1300
   ELSE
      delim = .TRUE.
      IF ( char1n==cparn1 ) delim = .FALSE.
      IF ( char1n==comma1 ) GOTO 100
      IF ( char1n==cparn1 ) GOTO 100
!
!     OUTPUT DELIMITER
!
      iout = iout + 2
      Out(asave) = Out(asave) + 1
      Out(iout) = blank4
      IF ( char1n==oparn1 ) Out(iout) = oparn4
      IF ( char1n==slash1 ) Out(iout) = slash4
      IF ( char1n==equal1 ) Out(iout) = equal4
      IF ( char1n==astk1 ) Out(iout) = astk4
      IF ( Out(iout)==blank4 ) THEN
         WRITE (Nout,99005) Ufm , char1n
99005    FORMAT (A23,'300, FORGOTTEN DELIMITER - ',A1,',  PROGRAM ERROR')
         GOTO 1900
      ELSE
         Out(iout-1) = a77777
         IF ( debug ) WRITE (Nout,99006) iout , Out(iout) , delim , char1n
99006    FORMAT (5X,'IOUT,OUT/@195 =',I4,2H ',A4,8H' DELIM=,L1,2H ',A1,1H')
         save8 = blank8
         GOTO 100
      ENDIF
   ENDIF
!
!     NUMERIC
!
 800  alpha = .FALSE.
   delim = .FALSE.
   it = 0
   nt(1) = 0
   DO i = n , last
      IF ( type(i)<0 ) GOTO 1000
      IF ( type(i)==0 ) EXIT
!
!     INTEGER CHARACTER
!
      char1i = char1(i)
      DO k = 1 , 9
         IF ( char1i==num1(k) ) GOTO 850
      ENDDO
      k = 0
 850  it = it + 1
      IF ( it<16 ) nt(it) = k
   ENDDO
!
!     FALL HERE IMPLIES WE HAVE A SIMPLE INTEGER
!
 900  number = 0
   DO i = 1 , it
      IF ( ((a67777-nt(i))/10)<number ) GOTO 1500
      number = number*10 + nt(i)
   ENDDO
   IF ( minus ) number = -number
   iout = iout + 2
   Out(iout-1) = -1
   Out(iout) = number
   IF ( debug ) THEN
      imhere = 280
      WRITE (Nout,99014) numric , imhere
      WRITE (Nout,99007) iout , Out(iout-1) , Out(iout) , delim
99007 FORMAT (10X,I4,1H),2I8,'    DELIM=',L1)
   ENDIF
   n = n + it - 1
   GOTO 100
!
!     FLOATING PT. NUMBER, DELIMITER, OR ERROR IF FALL HERE
!
!     COUNT THE NUMBER OF DIGITS LEFT BEFORE CARD END OR DELIMITER HIT
!
 1000 n1 = i
   DO n2 = n1 , last
      charn2 = char1(n2)
      IF ( charn2==oparn1 .OR. charn2==slash1 .OR. charn2==equal1 .OR. charn2==comma1 .OR. charn2==dollr1 .OR. type(n2)==0 )        &
         & GOTO 1100
      IF ( charn2==cparn1 ) GOTO 1100
   ENDDO
   n2 = last + 1
 1100 IF ( n1==n2 ) GOTO 900
!
!     CHARACTER N1 NOW MUST BE A DECIMAL FOR NO ERROR
!
   IF ( char1(n1)/=dot1 ) GOTO 1600
   power = .FALSE.
   n1 = n1 + 1
   n2 = n2 - 1
   places = 0
   expont = .FALSE.
   ipower = 0
   psign = zero1
   precis = zero1
   IF ( n2>=n1 ) THEN
      DO i = n1 , n2
         char1i = char1(i)
         IF ( type(i)<0 ) THEN
!
!     START EXPONENTS HERE
!
            IF ( expont ) THEN
!
!     SIGN OF POWER
!
               IF ( power ) GOTO 1700
               IF ( psign/=zero1 .OR. (char1i/=plus1 .AND. char1i/=minus1) ) GOTO 1700
               psign = char1i
               power = .TRUE.
            ELSE
               expont = .TRUE.
               IF ( char1i/=plus1 .AND. char1i/=minus1 ) THEN
                  IF ( char1i/=e1 .AND. char1i/=d1 ) GOTO 1700
                  precis = char1i
               ELSE
                  precis = e1
                  psign = char1i
               ENDIF
            ENDIF
            IF ( i==last ) GOTO 1400
         ELSEIF ( type(i)==0 ) THEN
            GOTO 1600
         ELSE
!
!     FLOATING PT. NUMBER
!
            DO k = 1 , 9
               IF ( char1i==num1(k) ) GOTO 1110
            ENDDO
            k = 0
 1110       IF ( expont ) THEN
!
!     BUILD POWER HERE
!
               power = .TRUE.
               ipower = ipower*10 + k
               IF ( ipower>1000 ) GOTO 1800
            ELSE
               it = it + 1
               IF ( it<16 ) nt(it) = k
               places = places + 1
            ENDIF
         ENDIF
      ENDDO
   ENDIF
   n = n2
!
!     ALL DATA COMPLETE FOR FLOATING POINT NUMBER
!     ONLY 15 FIGURES WILL BE ACCEPTED
!
   IF ( it>15 ) THEN
      ipower = ipower + it - 15
      it = 15
   ENDIF
   IF ( psign==minus1 ) ipower = -ipower
   ipower = ipower - places
   number = 0
   IF ( it<7 ) THEN
      n2 = it
   ELSE
      n2 = 7
   ENDIF
   DO i = 1 , n2
      number = number*10 + nt(i)
   ENDDO
   ddoubl = dble(float(number))
   IF ( it>7 ) THEN
      number = 0
      n2 = it - 7
      DO i = 1 , n2
         it = i + 7
         number = number*10 + nt(it)
      ENDDO
      ddoubl = ddoubl*10.0D0**n2 + dble(float(number))
   ENDIF
   IF ( minus ) ddoubl = -ddoubl
!
!     POWER HAS TO BE WITHIN RANGE OF MACHINE
!
   ichek = ipower + it
   IF ( ddoubl/=0.0D0 ) THEN
      IF ( ichek<Lowpw+1 .OR. ichek>Highpw-1 .OR. ipower<Lowpw+1 .OR. ipower>Highpw-1 ) GOTO 1800
      ddoubl = ddoubl*10.0D0**ipower
   ENDIF
   IF ( precis==d1 ) THEN
      iout = iout + 3
      Out(iout-2) = -4
      Out(iout-1) = idoubl(1)
      Out(iout) = idoubl(2)
   ELSE
      flpt = ddoubl
      iout = iout + 2
      Out(iout-1) = -2
      Out(iout) = intg
   ENDIF
   GOTO 100
!
!     PREPARE TO RETURN
!
 1200 IF ( .NOT.delim ) THEN
      Out(iout+1) = a67777
   ELSE
      IF ( save8/=blank8 ) CALL khrbc2(save8,Out(iout-1))
      Out(iout+1) = 0
   ENDIF
 1300 prev = Out(2)
   RETURN
!
!     ERRORS
!
 1400 WRITE (Nout,99008) Ufm
99008 FORMAT (A23,'300, INVALID DATA COLUMN 72')
   GOTO 1900
 1500 WRITE (Nout,99009) Ufm
99009 FORMAT (A23,'300, INTEGER DATA OUT OF MACHINE RANGE')
   GOTO 1900
 1600 WRITE (Nout,99010) Ufm , n1
99010 FORMAT (A23,'300, INVALID CHARACTER FOLLOWING INTEGER IN COLUMN',I4)
   GOTO 1900
 1700 WRITE (Nout,99011) Ufm , i
99011 FORMAT (A23,'300, DATA ERROR-UNANTICIPATED CHARACTER IN COLUMN',I4)
   GOTO 1900
 1800 WRITE (Nout,99012) Ufm
99012 FORMAT (A23,'300, DATA ERROR - MISSING DELIMITER OR REAL POWER ','OUT OF MACHINE RANGE')
 1900 Nogo = .TRUE.
   WRITE (Nout,99013) char72
99013 FORMAT (/5X,1H',A72,1H','  ERROR IN XRCARD ROUTINE')
   Out(1) = 0
99014 FORMAT ('   SAVE8= /',A8,'/   @',I3,',  ICHAR,IOUT=',2I3)
!
END SUBROUTINE xrcard
