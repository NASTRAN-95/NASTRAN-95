!*==rcard2.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rcard2(Out,Fmt,Nflag,In)
!DIR$ INTEGER=64
!
!     CDIR$ IS CRAY COMPILE DIRECTIVE. 64-BIT INTEGER IS USED LOCALLY
!
!     THIS ROUTINE IS MUCH MORE EFFICIENT THAN THE OLD ROUTINE RCARD
!     IT CAN SAFELY REPLACE THE OLD RCARD ROUTINE
!     WRITTEN BY G.CHAN/UNISYS            10/1987
!     REVISED, 8/1989, IMPROVED EFFICIENCY BY REDUCING CHARACTER
!     OPERATIONS (VERY IMPORTANT FOR CDC MACHINE)
!     LAST REVISED, 8/1991, SETTING UP REAL NO. UPPER AND LOWER BOUNDS
!     FOR VARIOUS MACHINES
!
!     RCARD2 ASSUMES ALL INPUT FIELDS IN 'IN' ARE LEFT-ADJUSTED.
!
USE c_lhpwx
USE c_system
USE c_xechox
USE c_xmssg
USE C_LHPWX
USE C_SYSTEM
USE C_XECHOX
USE C_XMSSG
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Out
   INTEGER , DIMENSION(1) :: Fmt
   INTEGER :: Nflag
   INTEGER , DIMENSION(20) :: In
   *0() :: 
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(80) :: a1
   INTEGER :: a1nb , a67777 , base , blank1 , check , chr11 , d1 , dot1 , e1 , field , i , ifmt , intgr , iout , it , j , k ,       &
            & minus1 , n , n8or16 , npoint , number , nwords , places , power , sign1 , spag_nextblock_1 , spag_nextblock_2 ,       &
            & spag_nextblock_3 , star1 , word , zero1
   INTEGER , SAVE :: blank , plus1 , stars
   CHARACTER(1) , SAVE :: blankc , dc , dotc , ec , minusc , plusc , starc , zeroc
   LOGICAL :: blkon , deciml , double , expont , minus , seqgp
   CHARACTER(4) , DIMENSION(1) :: C4
   INTEGER , DIMENSION(16) :: chr1 , nt , type , value
   CHARACTER(4) , DIMENSION(4) :: chr4
   CHARACTER(5) :: d5
   REAL*8 :: ddoubl
   CHARACTER(80) :: e80
   REAL :: fpt
   INTEGER , DIMENSION(2) :: idoubl
   CHARACTER(4) , DIMENSION(40) :: in4
   CHARACTER(1) , DIMENSION(80) :: k1
   CHARACTER(1) , DIMENSION(16) :: khr1
   INTEGER , DIMENSION(9) :: num1
   CHARACTER(9) , SAVE :: num9
   CHARACTER(100) , DIMENSION(4) :: out100
   CHARACTER(4) , DIMENSION(100) :: out4
   INTEGER , DIMENSION(100) :: outx
   CHARACTER(5) , SAVE :: seqep5 , seqgp5
   CHARACTER(100) :: tmp100
!
! End of declarations rewritten by SPAG
!
   !>>>>EQUIVALENCE (chr11,chr1(1)) , (k1(1),in4(1),d5,e80) , (fpt,intgr) , (khr1(1),chr4(1)) , (ddoubl,idoubl(1)) , (out4(1),out100(1))
   DATA blankc , starc , plusc , minusc , dotc , ec , dc/' ' , '*' , '+' , '-' , '.' , 'E' , 'D'/
   DATA blank , stars , seqgp5 , seqep5 , zeroc , num9/4H     , 4H==== , 'SEQGP' , 'SEQEP' , '0' , '123456789'/
   DATA plus1/0/
   spag_nextblock_1 = 1
   SPAG_DISPATCHLOOP_1:DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         IF ( plus1==0 ) THEN
            CALL k2b(blankc,blank1,1)
            CALL k2b(starc,star1,1)
            CALL k2b(plusc,plus1,1)
            CALL k2b(minusc,minus1,1)
            CALL k2b(dotc,dot1,1)
            CALL k2b(ec,e1,1)
            CALL k2b(dc,d1,1)
            CALL k2b(zeroc,zero1,1)
            CALL k2b(num9,num1,9)
         ENDIF
!
         CALL bcdkh8(In,e80)
         CALL k2b(e80,a1,80)
         spag_nextblock_1 = 2
         CYCLE SPAG_DISPATCHLOOP_1
!
!
         ENTRY rcard3(Out,Fmt,Nflag,C4)
!     ===============================
!
!     IN RCARD2, 'IN' IS 4-BYTE BCD  AND 'OUT' IS 4-BYTE BCD
!     IN RCARD3, 'C4' IS CHARACTER*4 AND 'OUT' IS 4-BYTE BCD
!     'IN' AND 'C4' ARE INPUT, AND 'OUT' IS OUTPUT
!
         DO i = 1 , 20
            in4(i) = C4(i)
         ENDDO
         CALL k2b(C4,a1,80)
         spag_nextblock_1 = 2
      CASE (2)
!
         field = 0
         iout = 0
         ifmt = 0
         word = 0
         nwords = 2
         seqgp = .FALSE.
         a67777 = rshift(lshift(complf(0),1),1)/10 - 10
         n8or16 = 8
         DO i = 1 , 100
            outx(i) = blank
         ENDDO
         spag_nextblock_1 = 3
      CASE (3)
!
!     PROCESS ONE FIELD (2 OR 4 WORDS) AT A TIME,
!     GET FIRST NON-BLANK CHARATER
!
         DO WHILE ( word/=18 )
            spag_nextblock_2 = 1
            SPAG_DISPATCHLOOP_2:DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  field = field + 1
                  deciml = .FALSE.
                  minus = .FALSE.
                  expont = .FALSE.
                  double = .FALSE.
                  blkon = .FALSE.
                  sign1 = blank1
                  places = 0
                  it = 0
                  power = 0
!
!     READ 8 OR 16 CHARATERS OF ONE FIELD
!     FOR EACH CHARACTER, SET TYPE TO
!            0 IF IT IS A BLANK
!           -1 IF IT IS BCD CHARACTER, AND
!           +1 IF IT IS NUMERIC
!
                  base = word*4
                  word = word + nwords
                  DO n = 1 , n8or16
                     spag_nextblock_3 = 1
                     SPAG_DISPATCHLOOP_3:DO
                        SELECT CASE (spag_nextblock_3)
                        CASE (1)
                           a1nb = a1(n+base)
                           IF ( a1nb==blank1 ) THEN
                              type(n) = 0
                           ELSE
                              IF ( a1nb==zero1 ) THEN
                                 k = 0
                              ELSE
                                 DO k = 1 , 9
                                    IF ( a1nb==num1(k) ) GOTO 2
                                 ENDDO
                                 type(n) = -1
                                 spag_nextblock_3 = 2
                                 CYCLE SPAG_DISPATCHLOOP_3
                              ENDIF
 2                            type(n) = 1
                              value(n) = k
                           ENDIF
                           spag_nextblock_3 = 2
                        CASE (2)
                           chr1(n) = a1nb
                           khr1(n) = k1(n+base)
                           EXIT SPAG_DISPATCHLOOP_3
                        END SELECT
                     ENDDO SPAG_DISPATCHLOOP_3
                  ENDDO
!
                  IF ( seqgp ) THEN
                     IF ( field==3 .OR. field==5 .OR. field==7 .OR. field==9 ) THEN
!
!     FIRST CHARATER OF FIELD 3, 5, 7,  OR 9 ON SEQGP/SEQEP CARD
!     ENCOUNTERED. IT HAS TO BE A 1 TO 9 FOR NO ERROR
!
                        DO n = 1 , n8or16
                           IF ( type(n)<0 ) THEN
                              spag_nextblock_1 = 21
                              GOTO 100
                           ENDIF
                           IF ( type(n)/=0 ) THEN
                              spag_nextblock_1 = 14
                              GOTO 100
                           ENDIF
                        ENDDO
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DISPATCHLOOP_2
                     ENDIF
                  ENDIF
!
!     BRANCH ON BCD, BLANK, OR NUMERIC
!
                  IF ( type(1)<0 ) THEN
!
!     BCD FIELD -
!     ===========
!
!     FIRST NON-BLANK CHARATER IS ALPHA, STAR, DOT, PLUS, OR MINUS
!
                     IF ( field==1 .AND. chr11==star1 ) THEN
!
!     FIRST CHARATER ON CARD IS AN ASTERISK (*)
!
                        nwords = 4
                        n8or16 = 16
                        spag_nextblock_1 = 7
                     ELSEIF ( chr11==plus1 ) THEN
!
!     FIRST CHARATER IN FIELD IS A PLUS (+)
!     IGNOR IT AND ASSUMING REMAINING FIELD IS NUMBERIC
!
                        IF ( field/=1 ) THEN
                           spag_nextblock_1 = 8
                           GOTO 100
                        ENDIF
                        spag_nextblock_1 = 7
                     ELSEIF ( chr11==dot1 ) THEN
!
!     FIRST CHARATER IN FIELD IS A DOT (.)
!
                        deciml = .TRUE.
                        places = 0
                        spag_nextblock_1 = 8
                     ELSEIF ( chr11==minus1 ) THEN
!
!     FIRST CHARATER IN FIELD IS A MINUS (-)
!
                        minus = .TRUE.
                        spag_nextblock_1 = 8
                     ELSE
!
!     TRUE ALPHA BCD-CHARACTER FIELD
!
!     CHECKING FOR DOULBE-FIELD ASTERISK (*) IF WE ARE IN FIELD 1
!     SET DOUBLE FLAGS N8OR16, NWORDS, AND REMOVE THE ASTERISK
!
                        IF ( field==1 ) THEN
                           j = 8
                           DO i = 2 , 8
                              IF ( chr1(j)==star1 .AND. type(j)==-1 ) THEN
                                 spag_nextblock_1 = 4
                                 GOTO 100
                              ENDIF
                              j = j - 1
                           ENDDO
                        ENDIF
                        spag_nextblock_1 = 5
                     ENDIF
                     GOTO 100
                  ELSEIF ( type(1)==0 ) THEN
!                  BCD BLANK NUMERIC
!
!     A BLANK FIELD -
!     ===============
!
                     IF ( field==1 ) THEN
                        spag_nextblock_1 = 5
                        GOTO 100
                     ENDIF
                  ELSE
!
!     NUMERIC -  0 TO 9
!     =================
!
                     IF ( value(1)/=0 ) THEN
                        nt(1) = value(1)
                        it = 1
                     ENDIF
                     spag_nextblock_1 = 8
                     GOTO 100
                  ENDIF
                  spag_nextblock_2 = 2
               CASE (2)
                  iout = iout + 1
                  outx(iout) = 0
                  ifmt = ifmt + 1
                  Fmt(ifmt) = 0
                  EXIT SPAG_DISPATCHLOOP_2
               END SELECT
            ENDDO SPAG_DISPATCHLOOP_2
         ENDDO
!
!     ALL FIELDS PROCESSED
!
         Nflag = iout
         Fmt(ifmt+1) = -1
!
!     CONVERT CHARACTERS TO BCD, AND INSERT NUMERIC VALUES IF
!     APPLICABLE
!
         n = 1
         DO i = 1 , Nflag , 25
            k = i + 24
            tmp100 = out100(n)
            CALL khrbc1(tmp100,Out(i))
            DO j = i , k
               IF ( outx(j)/=blank ) Out(j) = outx(j)
            ENDDO
            n = n + 1
         ENDDO
         RETURN
      CASE (4)
         nwords = 4
         n8or16 = 16
         chr1(j) = blank1
         khr1(j) = blankc
         spag_nextblock_1 = 5
      CASE (5)
!
         iout = iout + 2
         IF ( type(1)/=0 ) THEN
            IF ( nwords/=4 .OR. field/=1 ) THEN
               n = word - nwords
               out4(iout-1) = in4(n+1)
               out4(iout) = in4(n+2)
               spag_nextblock_1 = 6
               CYCLE SPAG_DISPATCHLOOP_1
            ENDIF
         ENDIF
!
         out4(iout-1) = chr4(1)
         out4(iout) = chr4(2)
         spag_nextblock_1 = 6
      CASE (6)
         ifmt = ifmt + 1
         Fmt(ifmt) = 3
!
!     IF FIRST FIELD IS SEQGP OR SEQEP, SET SEQGP FLAG TO TRUE
!
         IF ( field==1 .AND. (d5==seqgp5 .OR. d5==seqep5) ) seqgp = .TRUE.
         spag_nextblock_1 = 3
      CASE (7)
         iout = iout + 2
         outx(iout-1) = 0
         outx(iout) = 0
         ifmt = ifmt + 1
         Fmt(ifmt) = 3
         spag_nextblock_1 = 3
      CASE (8)
!
!     PROCESS REMAINING DIGITS
!
         DO n = 2 , n8or16
            IF ( type(n)>0 ) THEN
!
!     A NUMERIC CHARACTER, 0 TO 9, SAVE IT IN NT
!
               it = it + 1
               nt(it) = value(n)
               IF ( deciml ) places = places + 1
            ELSE
!
!     A NON-NUMERIC CHARACTER ENCOUNTERED
!
               IF ( chr1(n)/=dot1 ) THEN
                  spag_nextblock_1 = 11
                  GOTO 100
               ENDIF
               IF ( deciml ) THEN
                  spag_nextblock_1 = 19
                  GOTO 100
               ENDIF
               places = 0
               deciml = .TRUE.
            ENDIF
         ENDDO
!
!     IF DECIML IS .FALSE. NUMERIC IS AN INTEGER
!
         IF ( deciml ) THEN
            spag_nextblock_1 = 13
            CYCLE SPAG_DISPATCHLOOP_1
         ENDIF
         spag_nextblock_1 = 9
      CASE (9)
!
!     INTEGER FOUND.  NASTRAN INTEGER LIMIT = 10*A67777
!
         number = 0
         IF ( it/=0 ) THEN
            DO i = 1 , it
               IF ( number>a67777 ) THEN
                  spag_nextblock_1 = 18
                  GOTO 100
               ENDIF
               number = number*10 + nt(i)
            ENDDO
         ENDIF
         IF ( minus ) number = -number
         spag_nextblock_1 = 10
      CASE (10)
         iout = iout + 1
         outx(iout) = number
         ifmt = ifmt + 1
         Fmt(ifmt) = 1
         spag_nextblock_1 = 3
      CASE (11)
!
!     PROBABLY WE JUST ENCOUNTERED (E, D, +, -) EXPONENT, OR BLANK
!
         IF ( type(n)==0 ) THEN
!
!     IT IS A BLANK
!     THUS ONLY AN EXPONENT OR BLANKS PERMITTED FOR BALANCE OF FIELD
!
            DO WHILE ( n/=n8or16 )
               n = n + 1
               IF ( type(n)<0 ) THEN
                  spag_nextblock_1 = 12
                  GOTO 100
               ENDIF
               IF ( type(n)/=0 ) THEN
                  spag_nextblock_1 = 20
                  GOTO 100
               ENDIF
            ENDDO
!
!     FALL THRU ABOVE LOOP IMPLIES BALANCE OF FIELD WAS BLANKS
!
            IF ( deciml ) THEN
               spag_nextblock_1 = 13
               CYCLE SPAG_DISPATCHLOOP_1
            ENDIF
            spag_nextblock_1 = 9
            CYCLE SPAG_DISPATCHLOOP_1
         ENDIF
         spag_nextblock_1 = 12
      CASE (12)
!
!     A NON-BLANK CHARACTER -
!     IT HAS TO BE A (+, -, D, OR E ) OF THE EXPONENT STRING
!
         expont = .TRUE.
         IF ( chr1(n)==plus1 ) THEN
            sign1 = plus1
         ELSEIF ( chr1(n)/=e1 ) THEN
            IF ( chr1(n)/=minus1 ) THEN
               IF ( chr1(n)/=d1 ) THEN
                  spag_nextblock_1 = 20
                  CYCLE SPAG_DISPATCHLOOP_1
               ENDIF
               double = .TRUE.
            ELSE
               sign1 = minus1
            ENDIF
         ENDIF
!
!     READ INTEGER POWER, WITH OR WITHOUT SIGN
!
         SPAG_LOOP_1_1:DO WHILE ( n/=n8or16 )
            n = n + 1
!
            IF ( type(n)<0 ) THEN
               IF ( chr1(n)==plus1 .OR. chr1(n)==minus1 ) THEN
                  IF ( sign1/=blank1 ) EXIT SPAG_LOOP_1_1
                  sign1 = chr1(n)
                  CYCLE SPAG_LOOP_1_1
               ENDIF
            ELSEIF ( type(n)==0 ) THEN
               CYCLE SPAG_LOOP_1_1
            ENDIF
!
!     FIRST DIGIT OF INTEGER POWER AT HAND NOW
!
            power = 0
            blkon = .FALSE.
!
            SPAG_LOOP_2_2:DO WHILE ( type(n)>0 )
               power = power*10 + value(n)
!
!     GET ANY MORE DIGITS IF PRESENT
!
               DO WHILE ( n/=n8or16 )
                  n = n + 1
                  IF ( .NOT.(blkon) ) THEN
                     IF ( type(n)/=0 ) GOTO 10
!
!     IS A BLANK.  BALANCE OF FIELD MUST BE BLANKS
!
                     blkon = .TRUE.
                  ELSEIF ( type(n)/=0 ) THEN
                     IF ( Xsort2==2 ) THEN
                        spag_nextblock_1 = 3
                        GOTO 100
                     ENDIF
                     WRITE (Nout,99012) Ufm
                     WRITE (Nout,99001)
99001                FORMAT (10X,'POSSIBLE IMBEDDED BLANK')
                     spag_nextblock_1 = 23
                     GOTO 100
                  ENDIF
               ENDDO
               spag_nextblock_1 = 13
               GOTO 100
 10         ENDDO SPAG_LOOP_2_2
            EXIT SPAG_LOOP_1_1
         ENDDO SPAG_LOOP_1_1
         spag_nextblock_1 = 20
      CASE (13)
!
!     SINGLE OR DOUBLE PRECISION FLOATING POINT NUMBER
!     COMPLETE AND OUTPUT IT
!
!     15 SIGNIFICANT FIGURES POSSIBLE ON INPUT
!     CONSIDERED SINGLE PRECISION UNLESS D EXPONENT IS PRESENT
!
         IF ( sign1==minus1 ) power = -power
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
         ddoubl = dble(float(number))
         IF ( it>7 ) THEN
            number = 0
            DO i = 8 , it
               number = number*10 + nt(i)
            ENDDO
            ddoubl = ddoubl*10.0D0**(it-7) + dble(float(number))
         ENDIF
         IF ( minus ) ddoubl = -ddoubl
!
!     CHECK FOR POWER IN RANGE OF MACHINE
!
         check = power + it
         IF ( ddoubl/=0.0D0 ) THEN
            IF ( check<Lowpw+1 .OR. check>Highpw-1 .OR. power<Lowpw+1 .OR. power>Highpw-1 ) THEN
!
!     ERROR
!
               WRITE (Nout,99012) Ufm
               WRITE (Nout,99002)
99002          FORMAT (10X,'FLOATING POINT NUMBER OUT OF MACHINE RANGE')
               WRITE (Nout,99003) power , it , check , Lowpw , Highpw
99003          FORMAT (10X,'POWER,IT,CHECK,LOWPW,HIGHPW =',5I5)
               spag_nextblock_1 = 23
               CYCLE SPAG_DISPATCHLOOP_1
            ELSE
!
               ddoubl = ddoubl*10.0D0**power
            ENDIF
         ENDIF
         ifmt = ifmt + 1
         IF ( double ) THEN
            iout = iout + 2
            outx(iout-1) = idoubl(1)
            outx(iout) = idoubl(2)
            Fmt(ifmt) = 4
         ELSE
            fpt = ddoubl
            iout = iout + 1
            outx(iout) = intgr
            Fmt(ifmt) = 2
         ENDIF
         spag_nextblock_1 = 3
      CASE (14)
!
!     STORE NUMBER IN NT
!
         npoint = 0
         spag_nextblock_1 = 15
      CASE (15)
         it = it + 1
         nt(it) = value(n)
         spag_nextblock_1 = 16
      CASE (16)
         DO WHILE ( n/=n8or16 )
            n = n + 1
!
!     GET NEXT CHARATER
!
            IF ( npoint>0 .AND. .NOT.deciml .AND. .NOT.blkon ) THEN
!
               IF ( chr1(n)==dot1 .AND. type(n)<0 ) THEN
                  spag_nextblock_1 = 17
                  GOTO 100
               ENDIF
            ELSEIF ( deciml ) THEN
!
               IF ( type(n)<=0 ) THEN
                  spag_nextblock_1 = 21
                  GOTO 100
               ENDIF
!
               deciml = .FALSE.
               spag_nextblock_1 = 15
               GOTO 100
            ELSEIF ( .NOT.(blkon) ) THEN
               IF ( type(n)>0 ) THEN
                  spag_nextblock_1 = 15
                  GOTO 100
               ENDIF
               IF ( chr1(n)==dot1 ) THEN
                  spag_nextblock_1 = 17
                  GOTO 100
               ENDIF
            ENDIF
            IF ( type(n)/=0 ) THEN
               spag_nextblock_1 = 21
               GOTO 100
            ENDIF
            blkon = .TRUE.
         ENDDO
!
!     READY TO COMPUTE INTEGER VALUE OF SPECIAL SEQGP/SEQEP INTEGER
!
         npoint = 3 - npoint
         IF ( npoint<0 ) THEN
            spag_nextblock_1 = 21
            CYCLE SPAG_DISPATCHLOOP_1
         ENDIF
         IF ( npoint/=0 ) THEN
            DO k = 1 , npoint
               it = it + 1
               nt(it) = 0
            ENDDO
         ENDIF
!
!     COMPUTE NUMBER.  NASTRAN INTEGER LIMIT = 10*A67777
!
         number = 0
         IF ( it/=0 ) THEN
            DO k = 1 , it
               IF ( number>a67777 ) THEN
                  spag_nextblock_1 = 22
                  GOTO 100
               ENDIF
               number = number*10 + nt(k)
            ENDDO
         ENDIF
         spag_nextblock_1 = 10
      CASE (17)
!
         deciml = .TRUE.
         npoint = npoint + 1
         spag_nextblock_1 = 16
      CASE (18)
         WRITE (Nout,99012) Ufm
         WRITE (Nout,99004)
99004    FORMAT (10X,'INTEGER MAGNITUDE OUT OF MACHINE RANGE')
         spag_nextblock_1 = 23
      CASE (19)
         IF ( Xsort2==2 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DISPATCHLOOP_1
         ENDIF
         WRITE (Nout,99012) Ufm
         WRITE (Nout,99005)
99005    FORMAT (10X,'DATA NOT RECOGNIZEABLE')
         spag_nextblock_1 = 23
      CASE (20)
         expont = .FALSE.
         IF ( Xsort2==2 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DISPATCHLOOP_1
         ENDIF
         WRITE (Nout,99012) Ufm
         WRITE (Nout,99006)
99006    FORMAT (10X,'POSSIBLE ERROR IN EXPONENT')
         spag_nextblock_1 = 23
      CASE (21)
         IF ( Xsort2==2 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DISPATCHLOOP_1
         ENDIF
         WRITE (Nout,99012) Ufm
         WRITE (Nout,99007)
99007    FORMAT (10X,'INCORRECT DEWEY DECIMAL NUMBER')
         spag_nextblock_1 = 23
      CASE (22)
         IF ( Xsort2==2 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DISPATCHLOOP_1
         ENDIF
         WRITE (Nout,99012) Ufm
         WRITE (Nout,99008)
99008    FORMAT (10X,'INTERNAL CONVERSION OF DEWEY DECIMAL IS TOO LARGE')
         spag_nextblock_1 = 23
      CASE (23)
         DO j = 1 , 20
            IF ( outx(j)/=stars ) outx(j) = blank
         ENDDO
         word = (field-1)*nwords + 2
         k = stars
         DO
            outx(word) = k
            outx(word-1) = k
            IF ( nwords/=2 .AND. field/=1 ) THEN
               outx(word-2) = k
               outx(word-3) = k
            ENDIF
            IF ( k==0 ) THEN
               iout = iout + 1
               outx(iout) = 0
               ifmt = ifmt + 1
               Fmt(ifmt) = -1
               Nogo = .TRUE.
               spag_nextblock_1 = 3
               GOTO 100
            ELSE
               IF ( nwords==4 ) THEN
                  WRITE (Nout,99009)
99009             FORMAT (10X,'---1--- +++++2+&+3+++++ -----4-&-5----- +++++6+&','+7+++++ -----8-&-9----- +++10+++')
               ELSE
                  WRITE (Nout,99010)
99010             FORMAT (10X,'---1--- +++2+++ ---3--- +++4+++ ---5--- +++6+++ ','---7--- +++8+++ ---9--- +++10+++')
               ENDIF
               WRITE (Nout,99011) (in4(i),i=1,20) , outx
99011          FORMAT (10X,20A4)
               Nlines = Nlines + 7
               k = 0
            ENDIF
         ENDDO
         EXIT SPAG_DISPATCHLOOP_1
      END SELECT
 100  ENDDO SPAG_DISPATCHLOOP_1
99012 FORMAT (A23,' 300, DATA ERROR IN FIELD UNDERLINED.')
!
END SUBROUTINE rcard2
