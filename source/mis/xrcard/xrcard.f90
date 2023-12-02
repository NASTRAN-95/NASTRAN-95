!*==xrcard.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
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
USE C_LHPWX
USE C_SYSTEM
USE C_XMSSG
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: Out
   INTEGER :: Nflag
   INTEGER , DIMENSION(18) :: In
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: a67777 , a77777 , asave , astk1 , blank1 , char1i , char1n , charn2 , comma1 , cparn1 , d1 , dot1 , e1 , equal1 , i , &
            & ichar , ichek , imhere , intg , iout , ipower , irtn , it , k , l42 , last , minus1 , n , n1 , n2 , number , oparn1 , &
            & places , plus1 , precis , prev , psign , slash1 , zero1
   LOGICAL :: alpha , delim , expont , minus , power
   INTEGER , SAVE :: astk4 , blank4 , diag , dollr1 , equal4 , oparn4 , slash4
   CHARACTER(8) , SAVE :: blank8 , numric
   INTEGER , DIMENSION(72) :: char1 , type
   CHARACTER(23) , SAVE :: char23
   CHARACTER(72) :: char72
   INTEGER , DIMENSION(23) :: chars
   REAL(REAL64) :: ddoubl
   LOGICAL , SAVE :: debug
   REAL :: flpt
   INTEGER , DIMENSION(2) :: idoubl
   CHARACTER(1) , DIMENSION(72) :: khar1
   INTEGER , DIMENSION(15) :: nt
   INTEGER , DIMENSION(10) :: num1
   CHARACTER(1) , DIMENSION(8) :: save1
   CHARACTER(8) :: save8
   EXTERNAL bcdkh7 , complf , k2b , khrbc2 , lshift , rshift , sswtch , yrcard
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
   !>>>>EQUIVALENCE (khar1(1),char72) , (save8,save1(1)) , (flpt,intg) , (ddoubl,idoubl(1)) , (chars(1),dollr1) , (chars(8),cparn1) ,    &
!>>>>    & (chars(2),plus1) , (chars(9),e1) , (chars(3),equal1) , (chars(10),d1) , (chars(4),minus1) , (chars(11),dot1) ,                &
!>>>>    & (chars(5),comma1) , (chars(12),blank1) , (chars(6),slash1) , (chars(13),astk1) , (chars(7),oparn1) , (chars(14),num1(1)) ,    &
!>>>>    & (num1(10),zero1)
   DATA char23/'$+=-,/()ED. *1234567890'/ , blank8/'       '/
   DATA dollr1 , blank4 , diag , debug , numric/0 , 4H     , 4HDIAG , .FALSE. , 'NUMERIC'/
   DATA equal4 , slash4 , oparn4 , astk4/4H=    , 4H/    , 4H(    , 4H*   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         IF ( dollr1==0 ) THEN
            CALL k2b(char23,chars,23)
            a77777 = complf(0)
            a67777 = rshift(lshift(a77777,1),1)
            prev = blank4
            CALL sswtch(42,l42)
            IF ( debug ) WRITE (Nout,99001)
99001       FORMAT (//5X,'INPUT DEBUG IN XRCARD ROUTINE')
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
99002       FORMAT (/,' INPUT- ',A72)
!
!
            DO n = 1 , 72
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     char1n = char1(n)
                     IF ( char1n==blank1 ) THEN
                        type(n) = 0
                     ELSE
                        DO k = 1 , 10
                           IF ( char1n==num1(k) ) THEN
                              spag_nextblock_2 = 2
                              CYCLE SPAG_DispatchLoop_2
                           ENDIF
                        ENDDO
                        type(n) = -1
                     ENDIF
                     CYCLE
                  CASE (2)
                     type(n) = 1
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
            ENDDO
            n = 73
            SPAG_Loop_1_1: DO
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
                  EXIT SPAG_Loop_1_1
               ENDIF
            ENDDO SPAG_Loop_1_1
         ELSE
            CALL yrcard(Out,Nflag,In)
            RETURN
         ENDIF
 20      DO WHILE ( n/=last )
            IF ( Nflag-iout<5 ) THEN
               WRITE (Nout,99003) Ufm
99003          FORMAT (A23,'300, ROUTINE XRCARD FINDS OUTPUT BUFFER TOO SMALL ','TO PROCESS CARD COMPLETELY')
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
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
                     IF ( n>last ) THEN
                        spag_nextblock_1 = 11
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     spag_nextblock_1 = 5
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     IF ( char1n==dollr1 ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
!
!     GOOD ALPHA FIELD OR DELIMITER
!
                     IF ( .NOT.(alpha) ) THEN
                        IF ( (char1n==comma1 .OR. char1n==dollr1) .AND. (.NOT.delim) ) THEN
                           spag_nextblock_1 = 4
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        IF ( char1n==cparn1 .AND. .NOT.delim ) THEN
                           spag_nextblock_1 = 4
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        iout = iout + 1
                        asave = iout
                        Out(asave) = 0
                        alpha = .TRUE.
                     ENDIF
                     IF ( char1n==oparn1 .OR. char1n==slash1 .OR. char1n==equal1 .OR. char1n==comma1 .OR. char1n==astk1 .OR.        &
                        & char1n==dollr1 ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( char1n==cparn1 ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     ASSIGN 40 TO irtn
                     imhere = 125
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ELSEIF ( type(n)/=0 ) THEN
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDDO
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 40      Out(asave) = Out(asave) + 1
         iout = iout + 2
         delim = .FALSE.
         Out(iout-1) = blank4
         Out(iout) = blank4
         ichar = 0
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 60      IF ( n==last ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         n = n + 1
         char1n = char1(n)
         IF ( type(n)<0 ) THEN
!                  BCD BLANK NUMERIC
!
            IF ( char1n==oparn1 .OR. char1n==slash1 .OR. char1n==equal1 .OR. char1n==comma1 .OR. char1n==astk1 .OR. char1n==dollr1 )&
               & THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( char1n==cparn1 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSEIF ( type(n)==0 ) THEN
!
!     A BLANK CHARACTER IS ENCOUNTERED WHILE PROCESSING ALPHA STRING
!     IF THIS IS AT THE BEGINNING OF A NEW BCD WORD, GO TO 100
!     IF THIS IS AT THE END OF A BCD WORD, GO TO 170 TO WRAP IT UP
!
            IF ( ichar==0 ) GOTO 20
            ASSIGN 20 TO irtn
            imhere = 160
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     RECONSTRUCT CHARACTERS INTO SAVE1 SPACE, UP TO 8 CHARACTERS ONLY
!
         IF ( ichar==8 ) GOTO 60
         ichar = ichar + 1
         save1(ichar) = khar1(n)
         imhere = 150
         IF ( debug ) WRITE (Nout,99014) save8 , imhere , ichar , iout
!
!     GO FOR NEXT CHARACTER
!
         IF ( ichar<8 ) GOTO 60
         ASSIGN 60 TO irtn
         imhere = 155
         spag_nextblock_1 = 3
      CASE (3)
!
!     MOVE CHARACTER DATA IN SAVE8 TO OUT(IOUT-1) AND OUT(IOUT) IN BCD
!     WORDS
!
         IF ( save8/=blank8 ) CALL khrbc2(save8,Out(iout-1))
         IF ( debug ) THEN
            WRITE (Nout,99014) save8 , imhere , ichar , iout
            WRITE (Nout,99004) iout , Out(iout-1) , Out(iout) , delim , iout , Out(iout-1) , Out(iout)
99004       FORMAT ('   IOUT,OUT  =',I4,2H /,2A4,'/  DELIM=',L1,/14X,'=',I4,2H /,2I25,'/')
         ENDIF
         save8 = blank8
         GOTO irtn
      CASE (4)
!
!     DELIMITER HIT
!
         ASSIGN 80 TO irtn
         imhere = 180
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 80      IF ( delim ) THEN
            IF ( iout==0 ) iout = 1
            iout = iout + 2
            Out(asave) = Out(asave) + 1
            Out(iout-1) = blank4
            Out(iout) = blank4
         ENDIF
         IF ( char1n==dollr1 ) THEN
            Out(iout+1) = a67777
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ELSE
            delim = .TRUE.
            IF ( char1n==cparn1 ) delim = .FALSE.
            IF ( char1n==comma1 ) GOTO 20
            IF ( char1n==cparn1 ) GOTO 20
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
99005          FORMAT (A23,'300, FORGOTTEN DELIMITER - ',A1,',  PROGRAM ERROR')
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ELSE
               Out(iout-1) = a77777
               IF ( debug ) WRITE (Nout,99006) iout , Out(iout) , delim , char1n
99006          FORMAT (5X,'IOUT,OUT/@195 =',I4,2H ',A4,8H' DELIM=,L1,2H ',A1,1H')
               save8 = blank8
               GOTO 20
            ENDIF
         ENDIF
      CASE (5)
!
!     NUMERIC
!
         alpha = .FALSE.
         delim = .FALSE.
         it = 0
         nt(1) = 0
         SPAG_Loop_1_2: DO i = n , last
            spag_nextblock_3 = 1
            SPAG_DispatchLoop_3: DO
               SELECT CASE (spag_nextblock_3)
               CASE (1)
                  IF ( type(i)<0 ) THEN
                     spag_nextblock_1 = 7
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( type(i)==0 ) EXIT SPAG_Loop_1_2
!
!     INTEGER CHARACTER
!
                  char1i = char1(i)
                  DO k = 1 , 9
                     IF ( char1i==num1(k) ) THEN
                        spag_nextblock_3 = 2
                        CYCLE SPAG_DispatchLoop_3
                     ENDIF
                  ENDDO
                  k = 0
                  spag_nextblock_3 = 2
               CASE (2)
                  it = it + 1
                  IF ( it<16 ) nt(it) = k
                  EXIT SPAG_DispatchLoop_3
               END SELECT
            ENDDO SPAG_DispatchLoop_3
         ENDDO SPAG_Loop_1_2
         spag_nextblock_1 = 6
      CASE (6)
!
!     FALL HERE IMPLIES WE HAVE A SIMPLE INTEGER
!
         number = 0
         DO i = 1 , it
            IF ( ((a67777-nt(i))/10)<number ) THEN
               spag_nextblock_1 = 12
               CYCLE SPAG_DispatchLoop_1
            ENDIF
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
99007       FORMAT (10X,I4,1H),2I8,'    DELIM=',L1)
         ENDIF
         n = n + it - 1
         GOTO 20
      CASE (7)
!
!     FLOATING PT. NUMBER, DELIMITER, OR ERROR IF FALL HERE
!
!     COUNT THE NUMBER OF DIGITS LEFT BEFORE CARD END OR DELIMITER HIT
!
         n1 = i
         DO n2 = n1 , last
            charn2 = char1(n2)
            IF ( charn2==oparn1 .OR. charn2==slash1 .OR. charn2==equal1 .OR. charn2==comma1 .OR. charn2==dollr1 .OR. type(n2)==0 )  &
               & THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( charn2==cparn1 ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         n2 = last + 1
         spag_nextblock_1 = 8
      CASE (8)
         IF ( n1==n2 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     CHARACTER N1 NOW MUST BE A DECIMAL FOR NO ERROR
!
         IF ( char1(n1)/=dot1 ) THEN
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ENDIF
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
                     IF ( power ) THEN
                        spag_nextblock_1 = 14
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( psign/=zero1 .OR. (char1i/=plus1 .AND. char1i/=minus1) ) THEN
                        spag_nextblock_1 = 14
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     psign = char1i
                     power = .TRUE.
                  ELSE
                     expont = .TRUE.
                     IF ( char1i/=plus1 .AND. char1i/=minus1 ) THEN
                        IF ( char1i/=e1 .AND. char1i/=d1 ) THEN
                           spag_nextblock_1 = 14
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        precis = char1i
                     ELSE
                        precis = e1
                        psign = char1i
                     ENDIF
                  ENDIF
                  IF ( i==last ) THEN
                     spag_nextblock_1 = 11
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ELSEIF ( type(i)==0 ) THEN
                  spag_nextblock_1 = 13
                  CYCLE SPAG_DispatchLoop_1
               ELSE
!
!     FLOATING PT. NUMBER
!
                  DO k = 1 , 9
                     IF ( char1i==num1(k) ) GOTO 82
                  ENDDO
                  k = 0
 82               IF ( expont ) THEN
!
!     BUILD POWER HERE
!
                     power = .TRUE.
                     ipower = ipower*10 + k
                     IF ( ipower>1000 ) THEN
                        spag_nextblock_1 = 15
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
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
            IF ( ichek<Lowpw+1 .OR. ichek>Highpw-1 .OR. ipower<Lowpw+1 .OR. ipower>Highpw-1 ) THEN
               spag_nextblock_1 = 15
               CYCLE SPAG_DispatchLoop_1
            ENDIF
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
         GOTO 20
      CASE (9)
!
!     PREPARE TO RETURN
!
         IF ( .NOT.delim ) THEN
            Out(iout+1) = a67777
         ELSE
            IF ( save8/=blank8 ) CALL khrbc2(save8,Out(iout-1))
            Out(iout+1) = 0
         ENDIF
         spag_nextblock_1 = 10
      CASE (10)
         prev = Out(2)
         RETURN
      CASE (11)
!
!     ERRORS
!
         WRITE (Nout,99008) Ufm
99008    FORMAT (A23,'300, INVALID DATA COLUMN 72')
         spag_nextblock_1 = 16
         CYCLE SPAG_DispatchLoop_1
      CASE (12)
         WRITE (Nout,99009) Ufm
99009    FORMAT (A23,'300, INTEGER DATA OUT OF MACHINE RANGE')
         spag_nextblock_1 = 16
         CYCLE SPAG_DispatchLoop_1
      CASE (13)
         WRITE (Nout,99010) Ufm , n1
99010    FORMAT (A23,'300, INVALID CHARACTER FOLLOWING INTEGER IN COLUMN',I4)
         spag_nextblock_1 = 16
         CYCLE SPAG_DispatchLoop_1
      CASE (14)
         WRITE (Nout,99011) Ufm , i
99011    FORMAT (A23,'300, DATA ERROR-UNANTICIPATED CHARACTER IN COLUMN',I4)
         spag_nextblock_1 = 16
         CYCLE SPAG_DispatchLoop_1
      CASE (15)
         WRITE (Nout,99012) Ufm
99012    FORMAT (A23,'300, DATA ERROR - MISSING DELIMITER OR REAL POWER ','OUT OF MACHINE RANGE')
         spag_nextblock_1 = 16
      CASE (16)
         Nogo = .TRUE.
         WRITE (Nout,99013) char72
99013    FORMAT (/5X,1H',A72,1H','  ERROR IN XRCARD ROUTINE')
         Out(1) = 0
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99014 FORMAT ('   SAVE8= /',A8,'/   @',I3,',  ICHAR,IOUT=',2I3)
!
END SUBROUTINE xrcard
