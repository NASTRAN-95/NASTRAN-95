!*==unpscr.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE unpscr(In,Out,Z,Buf1,Buf2,Maxz,Tysign,Flag)
!
!     THIS ROUTINE UNPACKS A MATRIX (IN), AND TRANSFER THE DATA FROM
!     FIRST TO LAST NON-ZERO TERMS TO A SCRATCH FILE (OUT) IN VERY LARGE
!     RECORD(S), PRECEEDED BY THE FIRST AND LAST NON-ZERO TERM POINTERS.
!
!     INPPUT  - IN, + 7 TRAILER WORDS (WORDS 4,5,6, AND 7 WILL BE
!               OVERWRITTEN)
!               Z, BUF1, BUF2, MAXZ, TYSIGN, AND FLAG
!     OUTPUT  - OUT, NO TRAILER WORD WRITTEN
!               IN(4) = 10*(NO. OF RECONDS WRITTEN, HEADER RECORD
!                       EXCLUDED) + FLAG
!               IN(5) = DATA WORD TYPE UNPACKED (= 1,2,OR 4)
!               IN(6) = TOTAL NO. OF S.P. WORDS USED FOR INPUT MATRIX
!                       IN FORWARD UNPACK PASS
!               IN(7) = OUTPUT GINO NUMBER
!
!     FLAG = 1, THE MATRIX IS UNPACKED ONCE, IN FORWARD DIRECTION, THIS
!               MATRIX CAN BE IN GENERAL FORM; NEEDS NOT BE TRIANGULAR.
!     FLAG = 2, THE MATRIX IS UNPACKED FORWARD AND BACKWARD
!     FLAG = 3, THE MATRIX IS ADVANCED TO THE END AND UNPACKED BACKWARD
!               ONCE AND THEN FORWARD
!     MAXZ = n, WHERE n IS THE UPER LIMIT OF THE RECORD SIZE TO BE
!               WRITTEN (5000 MINIMUM).
!          = 0  OR LESS, OUTPUT WILL BE WRITTEN OUT IN EITHER ONE OR TWO
!               LONG RECORDS (ONE EACH FOR FORWARD AND BACKWARD UNPACK)
!     Z    =    WORKING SPACE, MINIMUM SIZE = ROW + 2 WORDS
!     TYSIGN =  (-4,-3,...,+4), IS TYPE AND SIGN FOR INPUT MATRIX UNPACK
!               NO TYPE AND SIGN CHANGE IF TYSIGN = 0.
!     BUF1, BUF2 = TWO GINO BUFFERS
!     SUBROUTINE DEBUG CAN BE ACTIVATED BY DIAG 11 OR 16
!
!     ASSUME MATRIX IN(5x5) =  a  0  0  0  0
!                              b  e  0  0  0
!                              c  f  g  0  0
!                              d  0  h  j  0
!                              0  0  i  k  l
!
!     OUTPUT FILE OUT WILL HAVE THE FOLLOWING DATA (PRECEEDED BY HEADER
!     RECORD)
!
!     FLAG 1 -  1 4 a b c d 2 3 e f 3 5 g h i 4 5 j k 5 5 l <EOF>
!     FLAG 2 -  1 4 a b c d 2 3 e f 3 5 g h i 4 5 j k 5 5 l <EOR>
!               5 5 l 4 5 j k 3 5 g h i 2 4 e f 1 4 a b d c <EOF>
!     FLAG 3 -  5 5 l 4 5 j k 3 5 g h i 2 3 e f 1 4 a b c d <EOR>
!               1 4 a b c d 2 3 e f 3 5 g h i 4 5 j k 5 5 l <EOF>
!
!     WHERE a thru l MAY BE SP, DP, CSP, OR CDP DATA
!
!     IF INPUT MATRIX IS VERY LARGE, THERE WILL BE SEVERAL LONG RECORDS
!     FOR EACH UNPACK PASS, AND EACH RECORD WILL NOT EXCEED MAXZ IN
!     LENGTH. MINIMUM OF MAXZ IS 5000. IF MAXZ IS NOT GIVEN, EACH UNPACK
!     PASS WILL GO TO ONE VERY VERY LONG RECORD. IN THIS CASE, MAXZ IS
!     SET TO 2**31
!
!     THE PURPOSE OF THIS ROUTINE IS TO AVOID UNPACKING A MATRIX TOO
!     MANY TIMES, WHILE THE MATRIX IS BEING USED REPEATEDLY.
!     SEE FBSII (REPEATEDLY CALLED BY FBS), FRBK2 (REPEATEDLY CALLED
!     BY FNXTVC), AND FRMLTD (REPEATED CALLED BY FRBK2 AND FNXTVC) IN
!     USING THIS NEW DATA FORMAT.
!
!     WRITTEN BY G.CHAN/UNISYS   11/1991
!
!     COMMENTS FROM G.C.  3/93
!     THE PRESENT UNPSCR ASSUMES THE MATRIX IS QUIT DENSE, SUCH AS THE
!     LOWER OR UPPER TRIANGULAR FACTORS. IF MATRIX IS SPARSE, SAY 33
!     PERCENT OF LESS, WE COULD WRITE THE MATRIX OUT ANOTHER WAY AND
!     SAVE LOTS OF DISC SPACE. WE COULD WRITE THE FIRST TO LAST NON-ZERO
!     TERMS IN STRING FORMS SIMILAR TO OUTPUT4 MODULE. THIS IMPROVEMENT
!     WILL BE LEFT FOR NEXT PROJECT.
!
   IMPLICIT NONE
   USE C_NAMES
   USE C_SYSTEM
   USE C_TYPE
   USE C_UNPAKX
   USE C_XMSSG
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(7) :: In
   INTEGER :: Out
   INTEGER , DIMENSION(3) :: Z
   INTEGER :: Buf1
   INTEGER :: Buf2
   INTEGER :: Maxz
   INTEGER :: Tysign
   INTEGER :: Flag
!
! Local variable declarations rewritten by SPAG
!
   CHARACTER(8) , SAVE :: backwd , forwd
   LOGICAL :: debug , flag23
   CHARACTER(8) :: fbwd
   INTEGER :: file , form , i , j , k , ll , max , nl , nrec , nwds , sum , tot
   INTEGER , DIMENSION(2) , SAVE :: nam
   INTEGER , DIMENSION(4) :: save , tyiijj
   EXTERNAL bckrec , close , errtrc , fname , fwdrec , gopen , mesage , open , skprec , sswtch , unpack , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   !>>>>EQUIVALENCE (Type,Tyiijj(1))
   DATA forwd , backwd/'FORWARD' , 'BACKWARD'/
   DATA nam/4HUNPS , 2HCR/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         IF ( Flag<1 .OR. Flag>3 .OR. In(1)==Out ) THEN
            WRITE (Nout,99001) Sfm , Flag , In(1) , Out
99001       FORMAT (A25,',  FLAG,IN(1),OUT =',3I5)
            j = -37
            CALL mesage(j,file,nam)
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ELSE
            CALL sswtch(11,i)
            CALL sswtch(16,j)
            debug = .FALSE.
            IF ( i+j>=1 ) debug = .TRUE.
            max = Maxz
            IF ( max<=0 ) max = 1073741824
            IF ( debug ) WRITE (Nout,99002) Uim
99002       FORMAT (A29,', UNPSCR DEBUG, ACTIVATED BY DIAG 11 AND/OR 16')
            IF ( max<5000 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            flag23 = Flag==2 .OR. Flag==3
            DO i = 1 , 4
               save(i) = tyiijj(i)
            ENDDO
            Type = In(5)
            nl = In(2)
            IF ( Tysign/=0 .AND. iabs(Tysign)<=4 ) Type = Tysign
            nwds = Words(iabs(Type))
            IF ( debug ) WRITE (Nout,99003) In(1) , Out , Maxz , max , Flag , nl , Type , nwds
99003       FORMAT (5X,'UNPSCR/@15  IN,OUT,MAXZ,MAX,FLAG,NL,TYPE,NWDS = ',2I5,2I12,I4,I7,2I4)
            Incr = 1
            form = In(4)
            IF ( flag23 .AND. form/=4 .AND. form/=5 ) THEN
               CALL fname(In(1),In(2))
               WRITE (Nout,99004) In(2) , In(3) , form , Flag
99004          FORMAT ('0*** INPUT MATRTIX ',2A4,' IS NOT A TRIANGULAR FACTOR.','   FORM,FLAG =',2I4)
               CALL errtrc('UNPSCR  ',270)
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ELSE
!                          LOWER  AND      UPPER  TRIANGULAR FACTORS
!
               file = Out
               CALL gopen(Out,Z(Buf2),Wrtrew)
               file = In(1)
               CALL open(*20,In,Z(Buf1),Rdrew)
               nrec = 0
               IF ( Flag==3 ) THEN
!
                  CALL skprec(In,nl)
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         CALL fwdrec(*40,In)
!
!     UNPACK FORWARD
!
         fbwd = forwd
         tot = 0
         sum = 0
         DO i = 1 , nl
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  Ii = 0
                  CALL unpack(*2,In,Z(3))
                  IF ( flag23 .AND. Ii/=i ) THEN
                     spag_nextblock_1 = 5
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  spag_nextblock_2 = 2
               CASE (2)
                  Z(1) = Ii
                  Z(2) = Jj
                  ll = (Jj-Ii+1)*nwds + 2
                  tot = tot + ll
                  sum = sum + ll
                  IF ( sum>max ) THEN
                     nrec = nrec + 1
                     CALL write(Out,0,0,1)
                     sum = sum - ll
                     IF ( debug ) WRITE (Nout,99010) nrec , sum , fbwd
                     sum = ll
                  ENDIF
                  CALL write(Out,Z(1),ll,0)
                  CYCLE
 2                IF ( flag23 ) GOTO 60
                  Ii = i
                  Jj = i
                  DO k = 3 , 6
                     Z(k) = 0
                  ENDDO
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         nrec = nrec + 1
         CALL write(Out,0,0,1)
         IF ( debug ) WRITE (Nout,99010) nrec , sum , fbwd
         IF ( Flag/=2 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL bckrec(In)
         spag_nextblock_1 = 3
      CASE (3)
!
!     UNPACK BACKWARD
!
         fbwd = backwd
         sum = 0
         i = nl
         DO j = 1 , nl
            Ii = 0
            CALL unpack(*60,In,Z(3))
            IF ( Ii/=i ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            Z(1) = Ii
            Z(2) = Jj
            ll = (Jj-Ii+1)*nwds + 2
            sum = sum + ll
            IF ( sum>max ) THEN
               nrec = nrec + 1
               CALL write(Out,0,0,1)
               sum = sum - ll
               IF ( debug ) WRITE (Nout,99010) nrec , sum , fbwd
               sum = ll
            ENDIF
            CALL write(Out,Z(1),ll,0)
            CALL bckrec(In)
            CALL bckrec(In)
            i = i - 1
         ENDDO
         nrec = nrec + 1
         CALL write(Out,0,0,1)
         IF ( debug ) WRITE (Nout,99010) nrec , sum , fbwd
         IF ( Flag==3 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
!
!     END OF UNPACKING
!
!     CHANGE LAST 4 WORDS OF THE INPUT MATRIX TRAILER. PARTICULARY, SET
!     THE 7TH WORD TO NEGATIVE. NOTE, IF FLAG IS 2 OR 3, IN(4) AND IN(6)
!     TRAILER WORDS HOLD HALF OF THE ACTUAL VALUES.
!     NOTE - SINCE WRTTRL IS NOT CALLED TO REGISTER THESE TRAILER WORD
!     CHANGES, THE TRAILER WORDS ARE INTENDED FOR THE ROUTINE TO BE
!     EXECUTE NEXT.  ALSO NOTE THAT OUTPUT FILE HAS NO TRAILER.
!     LASTLY, WE NEED TO RESTORE ORIGINAL WORDS IN /UNPAKX/ PREVIOUSLY
!     SAVED.
!
         CALL close(In,Rew)
         CALL close(Out,Rew)
         In(7) = -Out
         In(6) = tot
         In(5) = nwds
         i = nrec
         IF ( flag23 ) THEN
            i = nrec/2
            tot = tot*2
         ENDIF
         In(4) = 10*i + Flag
         DO i = 1 , 4
            tyiijj(i) = save(i)
         ENDDO
         IF ( debug ) THEN
            WRITE (Nout,99005) Uim , tot , nrec , nl , In(3)
99005       FORMAT (A29,1H,,I10,' S.P. WORDS MOVED TO SCRATCH FILE BY UNPSCR',/5X,'IN',I5,' RECORDS.',5X,'INPUT MATRIX =',I8,3H BY, &
                  & I7)
         ENDIF
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
!
 20      j = -1
         CALL mesage(j,file,nam)
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 40      j = -2
         CALL mesage(j,file,nam)
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
      CASE (5)
         WRITE (Nout,99006) Sfm , i , Ii , Jj , fbwd , Flag
99006    FORMAT (A25,',  I & II MISMATCH ',3I6,3H  /,A8,I9)
         j = -37
         CALL mesage(j,file,nam)
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 60      WRITE (Nout,99007) i , fbwd , Flag
99007    FORMAT ('0*** NULL COLUMN ENCOUNTERED IN TRIANGULAR FACTOR.  ','COLUMN',I7,3X,A8,I9)
         j = -37
         CALL mesage(j,file,nam)
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
      CASE (6)
         WRITE (Nout,99008) Maxz
99008    FORMAT ('0*** MAXZ ERROR ',I9,'  (TOO SMALL)')
         CALL errtrc('UNPSCR  ',290)
         j = -37
         CALL mesage(j,file,nam)
         spag_nextblock_1 = 7
      CASE (7)
!
         IF ( debug ) WRITE (Nout,99009)
99009    FORMAT (' ... UNPSCR DEBUG ENDS',/)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99010 FORMAT (5X,'UNPSCR WROTE RECORD',I5,',  NO. OF WORDS =',I9,2X,A8)
END SUBROUTINE unpscr
