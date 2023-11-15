
SUBROUTINE yrcard(Out,Nflag,In)
!
!     THIS WAS NASTRAN ORIGINAL XRCARD ROUTINE, AND IS NOW RENAMED
!     YRCARD
!     THIS ROUTINE IS CALLED ONLY BY XRCARD
!     THIS ROUTINE CAN BE DELETED IF THE NEW XRCARD ROUTINE PASSES
!     ALL RELIABILITY TESTS                 G.CHAN/UNISYS,  2/1988
!
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Dum1(7) , F6 , Highpw , Ibufsz , Lowpw , Nlines , Npages
   LOGICAL Nogo
   CHARACTER*23 Ufm
   COMMON /lhpwx / Lowpw , Highpw
   COMMON /system/ Ibufsz , F6 , Nogo , Dum1 , Npages , Nlines
   COMMON /xmssg / Ufm
!
! Dummy argument declarations
!
   INTEGER Nflag
   INTEGER In(18) , Out(1)
!
! Local variable declarations
!
   INTEGER a67777 , a77777 , asave , astk , blank , blanks , char(72) , charac , chars(13) , comma , cparen , d , dollar , e ,      &
         & equal , i , ichar , ichek , int1 , iout , ipos , ipower , it , j , k , minus , n , n1 , n2 , nchar , ndoubl(2) , nt(15) ,&
         & num(10) , number , oparen , period , places , plus , precis , psign , slash , type(72) , word , zero
   LOGICAL alpha , delim , expont , lminus , pass , power
   INTEGER complf , khrfn1 , lshift , rshift
   REAL fl1
   DOUBLE PRECISION xdoubl
   EXTERNAL complf , lshift , rshift
!
! End of declarations
!
   EQUIVALENCE (fl1,int1) , (xdoubl,ndoubl(1)) , (num(10),zero) , (chars(1),dollar) , (chars(2),plus) , (chars(3),equal) ,          &
    & (chars(4),minus) , (chars(5),comma) , (chars(6),slash) , (chars(7),oparen) , (chars(8),cparen) , (chars(9),e) , (chars(10),d) &
    & , (chars(11),period) , (chars(12),blank) , (chars(13),astk)
   DATA blanks/4H    / , blank/4H    / , dollar/4H$   / , equal/1H=/ , astk/1H*/ , comma/1H,/ , slash/1H// , cparen/1H)/ ,          &
       &oparen/1H(/ , plus/1H+/ , minus/1H-/ , period/1H./ , e/1HE/ , d/1HD/ , pass/.FALSE./ , num/1H1 , 1H2 , 1H3 , 1H4 , 1H5 ,    &
      & 1H6 , 1H7 , 1H8 , 1H9 , 1H0/
!
   IF ( .NOT.(pass) ) THEN
      pass = .TRUE.
      a77777 = complf(0)
      a67777 = rshift(lshift(a77777,1),1)
   ENDIF
!
!     READ AND TYPE 72 CHARACTERS
!
   n = 0
   DO i = 1 , 18
      DO j = 1 , 4
         n = n + 1
         charac = khrfn1(blanks,1,In(i),j)
         IF ( charac==blank ) THEN
            type(n) = 0
         ELSE
            DO k = 1 , 10
               IF ( charac==num(k) ) GOTO 20
            ENDDO
            type(n) = -1
         ENDIF
         GOTO 40
 20      type(n) = 1
 40      char(n) = charac
      ENDDO
   ENDDO
   alpha = .FALSE.
   delim = .TRUE.
   iout = 0
   n = 0
   asave = 1
   Out(asave) = 0
 100  DO WHILE ( n/=72 )
      IF ( Nflag-iout<5 ) THEN
         WRITE (F6,99001) Ufm
99001    FORMAT (A23,' 300 *** ROUTINE XRCARD FINDS OUTPUT BUFFER TOO ','SMALL TO PROCESS CARD COMPLETELY')
         GOTO 1500
      ELSE
         lminus = .FALSE.
         n = n + 1
         nchar = char(n)
         IF ( type(n)<0 ) THEN
            IF ( nchar==plus .OR. nchar==minus .OR. nchar==period ) THEN
!
!
               IF ( nchar==minus ) lminus = .TRUE.
               IF ( nchar/=period ) n = n + 1
               IF ( n<=72 ) GOTO 400
               GOTO 1000
            ELSE
               IF ( nchar==dollar ) GOTO 300
!
!     GOOD ALPHA FIELD OR DELIMETER
!
               IF ( .NOT.(alpha) ) THEN
                  IF ( (nchar==comma .OR. nchar==dollar) .AND. (.NOT.delim) ) GOTO 300
                  IF ( nchar==cparen .AND. .NOT.delim ) GOTO 300
                  iout = iout + 1
                  asave = iout
                  Out(asave) = 0
                  alpha = .TRUE.
               ENDIF
               IF ( nchar==oparen .OR. nchar==slash .OR. nchar==equal .OR. nchar==comma .OR. nchar==astk .OR. nchar==dollar )       &
                  & GOTO 300
               IF ( nchar==cparen ) GOTO 300
               Out(asave) = Out(asave) + 1
               iout = iout + 2
               delim = .FALSE.
               Out(iout-1) = blanks
               Out(iout) = blanks
               ichar = 0
               GOTO 200
            ENDIF
         ELSEIF ( type(n)/=0 ) THEN
            GOTO 400
         ENDIF
      ENDIF
   ENDDO
   GOTO 800
 200  IF ( ichar/=8 ) THEN
      ichar = ichar + 1
      IF ( ichar<=4 ) THEN
         ipos = ichar
         word = iout - 1
      ELSE
         ipos = ichar - 4
         word = iout
      ENDIF
!
!     CLEAR SPOT IN WORD FOR CHAR(N) AND PUT CHAR(N) IN IT
!
!
!     GO FOR NEXT CHARACTER
!
      Out(word) = khrfn1(Out(word),ipos,nchar,1)
   ENDIF
   IF ( n==72 ) GOTO 800
   n = n + 1
   nchar = char(n)
   IF ( type(n)<0 ) THEN
      IF ( nchar/=oparen .AND. nchar/=slash .AND. nchar/=equal .AND. nchar/=comma .AND. nchar/=astk .AND. nchar/=dollar ) THEN
         IF ( nchar/=cparen ) GOTO 200
      ENDIF
   ELSEIF ( type(n)==0 ) THEN
      GOTO 100
   ELSE
      GOTO 200
   ENDIF
!
!
!     DELIMETER HIT
!
 300  IF ( delim ) THEN
      IF ( iout==0 ) iout = 1
      iout = iout + 2
      Out(asave) = Out(asave) + 1
      Out(iout-1) = blanks
      Out(iout) = blanks
   ENDIF
   IF ( nchar==dollar ) GOTO 900
   delim = .TRUE.
   IF ( nchar==cparen ) delim = .FALSE.
   IF ( nchar/=comma ) THEN
      IF ( nchar/=cparen ) THEN
!
!     OUTPUT DELIMETER
!
         iout = iout + 2
         Out(asave) = Out(asave) + 1
         Out(iout-1) = a77777
         Out(iout) = khrfn1(blanks,1,nchar,1)
      ENDIF
   ENDIF
   GOTO 100
!
 400  alpha = .FALSE.
   delim = .FALSE.
   it = 0
   nt(1) = 0
   DO i = n , 72
      IF ( type(i)<0 ) GOTO 600
      IF ( type(i)==0 ) EXIT
!
!     INTEGER CHARACTER
!
      DO k = 1 , 9
         IF ( char(i)==num(k) ) GOTO 450
      ENDDO
      k = 0
 450  it = it + 1
      IF ( it<16 ) nt(it) = k
   ENDDO
!
!     FALL HERE IMPLIES WE HAVE A SIMPLE INTEGER
!
 500  number = 0
   DO i = 1 , it
      IF ( ((a67777-nt(i))/10)<number ) GOTO 1100
      number = number*10 + nt(i)
   ENDDO
   IF ( lminus ) number = -number
   iout = iout + 2
   Out(iout-1) = -1
   Out(iout) = number
   n = n + it - 1
   GOTO 100
!
!     REAL NUMBER, DELIMETER, OR ERROR IF FALL HERE
!
!     COUNT THE NUMBER OF DIGITS LEFT BEFORE CARD END OR DELIMETER HIT
!
 600  n1 = i
   DO n2 = n1 , 72
      IF ( char(n2)==oparen .OR. char(n2)==slash .OR. char(n2)==equal .OR. char(n2)==comma .OR. char(n2)==dollar .OR. type(n2)==0 ) &
         & GOTO 700
      IF ( char(n2)==cparen ) GOTO 700
   ENDDO
   n2 = 73
 700  IF ( n1==n2 ) GOTO 500
!
!     CHARACTER N1 NOW MUST BE A DECIMAL FOR NO ERROR
!
   IF ( char(n1)/=period ) GOTO 1200
   power = .FALSE.
   n1 = n1 + 1
   n2 = n2 - 1
   places = 0
   psign = 0
   expont = .FALSE.
   ipower = 0
   precis = 0
   IF ( n2>=n1 ) THEN
      DO i = n1 , n2
         IF ( type(i)<0 ) THEN
!
!     START EXPONENTS HERE
!
            IF ( expont ) THEN
!
!     SIGN OF POWER
!
               IF ( power ) GOTO 1300
               IF ( psign/=0 .OR. (char(i)/=plus .AND. char(i)/=minus) ) GOTO 1300
               psign = char(i)
               power = .TRUE.
            ELSE
               expont = .TRUE.
               IF ( char(i)/=plus .AND. char(i)/=minus ) THEN
                  IF ( char(i)/=e .AND. char(i)/=d ) GOTO 1300
                  precis = char(i)
               ELSE
                  precis = e
                  psign = char(i)
               ENDIF
            ENDIF
            IF ( i==72 ) GOTO 1000
         ELSEIF ( type(i)==0 ) THEN
            GOTO 1200
         ELSE
!
!     NUMERIC
!
            DO k = 1 , 9
               IF ( char(i)==num(k) ) GOTO 710
            ENDDO
            k = 0
 710        IF ( expont ) THEN
!
!     BUILD IPOWER HERE
!
               power = .TRUE.
               ipower = ipower*10 + k
               IF ( ipower>1000 ) GOTO 1400
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
!     15 FIGURES WILL BE ACCEPTED ONLY
!
   IF ( it>15 ) THEN
      ipower = ipower + it - 15
      it = 15
   ENDIF
   IF ( psign==minus ) ipower = -ipower
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
   xdoubl = dble(float(number))
   IF ( it>7 ) THEN
      number = 0
      n2 = it - 7
      DO i = 1 , n2
         it = i + 7
         number = number*10 + nt(it)
      ENDDO
      xdoubl = xdoubl*10.0D0**n2 + dble(float(number))
   ENDIF
   IF ( lminus ) xdoubl = -xdoubl
!
!     POWER HAS TO BE WITHIN RANGE OF MACHINE
!
   ichek = ipower + it
   IF ( xdoubl/=0.0D0 ) THEN
      IF ( ichek<Lowpw+1 .OR. ichek>Highpw-1 .OR. ipower<Lowpw+1 .OR. ipower>Highpw-1 ) GOTO 1400
      xdoubl = xdoubl*10.0D0**ipower
   ENDIF
   IF ( precis==d ) THEN
      iout = iout + 3
      Out(iout-2) = -4
      Out(iout-1) = ndoubl(1)
      Out(iout) = ndoubl(2)
   ELSE
      fl1 = xdoubl
      iout = iout + 2
      Out(iout-1) = -2
      Out(iout) = int1
   ENDIF
   GOTO 100
!
!
!     PREPARE TO RETURN
!
 800  IF ( delim ) THEN
      Out(iout+1) = 0
      RETURN
   ENDIF
 900  Out(iout+1) = a67777
   RETURN
!
!     ERRORS
!
 1000 WRITE (F6,99002) Ufm
99002 FORMAT (A23,' 300 *** INVALID DATA COLUMN 72')
   GOTO 1500
 1100 WRITE (F6,99003) Ufm
99003 FORMAT (A23,' 300 *** INTEGER DATA OUT OF MACHINE RANGE')
   GOTO 1500
 1200 WRITE (F6,99004) Ufm , n1
99004 FORMAT (A23,' 300 *** INVALID CHARACTER FOLLOWING INTEGER IN ','COLUMN',I3)
   GOTO 1500
 1300 WRITE (F6,99005) Ufm , i
99005 FORMAT (A23,' 300 *** DATA ERROR-UNANTICIPATED CHARACTER IN ','COLUMN',I3)
   GOTO 1500
 1400 WRITE (F6,99006) Ufm
99006 FORMAT (A23,' 300 *** DATA ERROR - MISSING DELIMITER OR REAL ','POWER OUT OF MACHINE RANGE')
 1500 Nogo = .TRUE.
   WRITE (F6,99007) char
99007 FORMAT (/5X,1H',72A1,1H')
   Out(1) = 0
!
END SUBROUTINE yrcard
