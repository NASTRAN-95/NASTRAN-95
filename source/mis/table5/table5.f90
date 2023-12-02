!*==table5.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE table5(*,In,Out,Trl,Ibuf,Wrt,Lfn,Fn)
!
!     THIS ROUTINE IS CALLED ONLY BY OUTPT5 TO COPY A TABLE FILE IN 'IN'
!     TO AN OUPUT FILE 'OUT', BY FORTRAN WRITE, FORMATTED OR UNFORMATTED
!
!     IN,OUT = INPUT AND OUTPUT FILE, INTEGERS
!     TRL    = TRAILER OF INPUT FILE, INTEGERS
!     P4     = 0, OUTPUT FILE IS TO BE WRITTEN UNFORMATTED, BINARY, INT.
!            = 1, OUTPUT FILE IS TO BE WRITTEN FORMATTED, INTEGER
!     TI     = ARRAY TO OVERRIDE DATA TYPE OUTPUT. INTEGERS
!              SEE RULES BELOW.
!     Z,IBUF = OPEN CORE AND GINO BUFFER POINTER, INTEGER
!     WRT,LFN= ARE COMMUNICATION FLAGS BETWEEN TABLE5 AND OUTPT5
!     FN     = ARRAY FOR INPUT FILE NAME
!
!     THE FOLLOWING CONVENTIONS ARE USED FOR FORMATTED TAPE -
!
!       A   '/'+A4  FORMAT FOR BCD WORD               ( 5 BYTES)
!       AN  'I'+I9  FORMAT FOR INTEGER                (10 BYTES)
!       A 'R'+E14.7 FORMAT FOR S.P. REAL NUMBER.      (15 BYTES)
!       A 'D'+D14.7 FORMAT FOR D.P. REAL NUMBER.      (15 BYTES)
!       A 'X'+4 BLANKS IS A FILLER, AT END OF A LINE  ( 5 BYTES)
!
!       EACH RECORD IS PRECEEDED BY L5 (IN I10 FORMAT) WHERE L5 IS THE
!       TOTAL NO. OF CHARACTERS OF THIS CURRENT RECORD DIVIDED BY 5.
!
!       EACH RECORD IS WRITTEN IN MULTIPLE LINES OF 130 CHARACTERS EACH.
!       (131 CHARACTERS TO BE EXACTLY - 130 PLUS A BLANK)
!
!       ONE OR TWO FILLERS MAY ATTACH TO THE END OF A LINE TO MAKE UP
!       130 CHARACTERS. THAT IS, INTEGER AND S.P.REAL NUMBER AT THE END
!       OF A LINE WILL NOT BE SPLITTED BETWEEN TWO LINES
!
!       IF A ZERO IS PRECEEDED BY A F.P. REAL NUMBER, IT WILL BE WRITTEN
!       OUT AS A REAL ZERO (0.0), INTEGER ZERO (0) OTHERWISE.
!
!       DUE TO THE FACTS THAT FLOATING POINT ZEROS ARE ALWAYS TREATED AS
!       INTEGERS, DOUBLE PRECISION CAN NOT BE DETECTED, AND OCCATIONALLY
!       AUTOMATIC DATA TYPE CHECKING MAY ERR, THE USER CAN OVERRIDE THE
!       OUTPUT DATA FORMAT BY DEFINING TI ARRAYS WITH THE FOLLOWING
!       RULES -
!
!          EACH TI PARAMETER MUST HOLD 9 DIGITS, FROM LEFT TO RIGHT.
!               ZEROS-FILLED IF NECCESSARY.
!               TOTALLY THERE ARE 10 TI PARAMETERS. THEREFORE, THERE ARE
!               UP TO 90 CONTINUOUS DIGITS CAN BE USED.
!               (DEFAULT IS 90 ZEROS)
!          EACH DIGIT HOLDS VALUE FROM 0 THROUGH 9, VALUE
!               0 MEANS DATA TYPE WILL BE SET AUTOMATICALLY BY TABLE5
!               1 MEANS DATA TYPE IS INTEGER
!               2 MEANS DATA TYPE IS REAL, SINGLE PRECISION
!               3 MEANS DATA TYPE IS BCD WORD (4 BYTES PER WORD)
!               4 MEANS DATA TYPE IS REAL, DOUGLE PRECISION
!             5-9 HAS SPECIAL MEANING. IT MEANS THERE ARE (5-9) VALUES
!                 OF DATA TYPE DEFINED BY THE NEXT VALUE FOLLOWING.
!          EACH DIGIT IN TI, EXCEPT 5 THRU 9, DEFINES THE CORRESPODING
!               DATA TYPE IN THE TABLE BLOCK DATA, STARTING FROM THE
!               FIRST DATA WORD AND CONTINUE TO THE LAST.
!          IF TI(1) IS NEGATIVE, INTERMEDIATE STEPS IN FORMAT GENERATION
!               ARE PRINTED OUT.
!     E.G.
!     TABLE- 3  4  3.4  5.0E-3  TESTING  .6D+7  9  G  3.2  8  0.  0  4
!            12 13  14  15  28  61   88   14   44 .7D+7
!     TI   - TI(1) =-112233413, TI(2) = 212516140  OR
!            TI(1) = 604000025, TI(2) = 060400000 (7TH AND 24 WORDS ARE
!                                            D.P. AND 12TH WORD IS REAL)
!     NOTE - 2 BCD WORDS IN 'TESTING',
!            ALL OTHERS ARE 1 COMPUTER WORD PER DATA ENTRY
!            TI(2), THE LAST TI USED HERE, MUST FILL UP WITH ZEROS TO
!               MAKE UP A 9-DIGIT WORD.
!
!     TO READ THE OUTPUT FILE, USE TABLE-V SUBROUTINE AS REFERENCE
!
!     NOTE - THE FORMATTED OUTPUT FILE CAN BE VIEWED AND/OR EDITTED BY
!            THE SYSTEM EDITOR
!
!     WRITTEN BY G.CHAN/UNISYS,  1989
!
!  $MIXED_FORMATS
!
   USE c_blank
   USE c_machin
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: In
   INTEGER :: Out
   INTEGER , DIMENSION(7) :: Trl
   INTEGER :: Ibuf
   INTEGER :: Wrt
   INTEGER :: Lfn
   INTEGER , DIMENSION(3,1) :: Fn
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL , SAVE :: debug
   INTEGER , SAVE :: del , end , tble
   LOGICAL :: dp , tion
   REAL(REAL64) :: dtemp
   CHARACTER(10) , DIMENSION(30) :: fmt
   CHARACTER(10) , SAVE :: fmtb , fmtd , fmti , fmtr , fmtx , lpren , lpri10 , rpren
   INTEGER :: i , ib , isave , j , ji , jj , jz , k , kk , kk1 , kk2 , kk3 , kore , l , l5 , ll , pjj , til , til10
   INTEGER , DIMENSION(2) :: name
   REAL , DIMENSION(1) :: rz
   INTEGER , DIMENSION(2) , SAVE :: sub
   REAL , DIMENSION(2) :: temp
   EXTERNAL close , fname , forwrt , fwdrec , mesage , numtyp , open , page1 , page2 , read , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!WKBI 7/94
   !>>>>EQUIVALENCE (Z(1),Rz(1)) , (dtemp,temp(1))
   DATA sub/4HTABL , 4HE5  / , debug/.FALSE./
   DATA fmti , fmtr/'1HI,I9,' , '1HR,E14.7,'/
   DATA fmtb , fmtd/'1H/,A4,' , '1HD,D14.7,'/
   DATA fmtx , lpri10/'1HX,4X,' , '(I10,'/
   DATA lpren , rpren , del/'(' , '1X)' , 4H),.)/
   DATA end , tble/4H*END , 4HTBLE/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         debug = .FALSE.
         IF ( ti(1)<0 ) debug = .TRUE.
         ti(1) = iabs(ti(1))
         tion = .FALSE.
         DO l = 1 , 10
            IF ( ti(l)/=0 ) tion = .TRUE.
         ENDDO
         IF ( debug ) CALL page1
         IF ( debug ) WRITE (nout,99001)
99001    FORMAT (///5X,'*** IN TABLE5/OUTPUT5 ***')
         kore = Ibuf - 2
!
!     OPEN INPUT FILE, AND READ FILE NAME IN THE FILE HEADER RECORD
!     WRITE ONE HEADER RECORD, IN OUTPT5 MATRIX HEADER FORMAT, TO
!     OUTPUT TAPE
!
         CALL open(*120,In,z(Ibuf),0)
         CALL read(*140,*160,In,name,2,1,kk)
         IF ( debug ) WRITE (nout,99002) name
99002    FORMAT (/5X,'PROCESSING...',2A4,/)
         i = 0
         j = 1
         Trl(7) = 0
         IF ( p4==0 ) WRITE (Out) i , j , j , dtemp , (Trl(k),k=2,7) , name
         IF ( p4==1 ) WRITE (Out,99003) i , j , j , dtemp , (Trl(k),k=2,7) , name
99003    FORMAT (3I8,/,D26.17,6I8,2A4)
         spag_nextblock_1 = 2
      CASE (2)
!
         IF ( p4==1 ) THEN
!
!     FORMATTED WRITE
!
            j = 2
            CALL read(*100,*40,In,z(j),kore,1,kk)
            j = 0
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     UNFORMATED WRITE
!
            j = 2
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         CALL read(*100,*20,In,z(j),kore,1,kk)
         j = 0
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
 20      IF ( j/=1 ) THEN
            j = 1
            z(1) = kk
         ENDIF
         CALL write(Out,z(1),kk,1)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     SET UP USER DIRECTED TI TABLE IN Z(KK2) THRU Z(KK3)
!
 40      IF ( debug ) WRITE (nout,99004) (ti(j),j=1,10)
99004    FORMAT (//5X,'TI PARAMETERS =',/4X,10(1X,I9))
         kk1 = kk + 2
         kk2 = kk1 + kk
         kk3 = kk2 + kk
         j = kore - kk3 - 9
         IF ( j<0 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO k = kk1 , kk3
            z(k) = 0
         ENDDO
         IF ( .NOT.tion ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         k = kk1 - 9
         ll = 0
         l = -1
         spag_nextblock_1 = 4
      CASE (4)
         SPAG_Loop_1_1: DO WHILE ( l<0 )
            l = 8
            ll = ll + 1
            k = k + 9
            IF ( k>=kk2 .OR. ll>10 ) THEN
!
               k = kk2 - 1
               IF ( debug ) WRITE (nout,99005) (z(j),j=kk1,k)
99005          FORMAT (//5X,'DIGITIZED TI PARAMTERS =',/,(3X,25I3))
               i = kk2
               DO j = kk1 , k
                  jz = z(j)
                  IF ( jz>4 ) THEN
                     ji = jz + i - 1
                     jj = z(j+1)
                     IF ( jj>4 ) THEN
                        spag_nextblock_1 = 10
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     DO l = i , ji
                        z(l) = jj
                     ENDDO
                     i = ji + 1
                     z(j+1) = -1
                  ELSEIF ( jz/=-1 ) THEN
                     z(i) = jz
                     i = i + 1
                  ENDIF
               ENDDO
               i = kk3 - 1
               IF ( debug ) WRITE (nout,99006) (z(j),j=kk2,i)
99006          FORMAT (//,5X,'DECODED TI PARAMETERS =',/,(3X,25I3))
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSE
               til = ti(ll)
               IF ( til>0 ) EXIT SPAG_Loop_1_1
               l = -1
            ENDIF
         ENDDO SPAG_Loop_1_1
         til10 = til/10
         z(k+l) = til - til10*10
         til = til10
         l = l - 1
         spag_nextblock_1 = 4
      CASE (5)
!
!     COUNT HOW MANY 5-BYTE WORDS TO BE GENERATED, FILLERS INCLUDED
!
         kk2 = kk2 - 1
         k = kk1
         pjj = 1
         l5 = 10
!
         IF ( debug ) CALL page1
         DO i = 1 , kk
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  k = k + 1
                  pjj = jj
                  IF ( tion ) THEN
                     jj = z(kk2+i) + 1
                     IF ( jj==1 ) jj = numtyp(z(i+1)) + 1
                  ELSE
                     jj = numtyp(z(i+1)) + 1
                  ENDIF
                  IF ( jj==2 ) THEN
                  ELSEIF ( jj==3 .OR. jj==5 ) THEN
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
                  ELSEIF ( jj==4 ) THEN
!
!     BCD
!
                     z(k) = jj
                     l5 = l5 + 5
                     CYCLE
                  ELSE
!              0,  I,  R,  B,  D
!
!     ZERO
!
                     jj = 3
                     IF ( pjj==3 .OR. pjj==5 ) THEN
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                     jj = 2
                  ENDIF
!
!     INTEGER
!
                  IF ( mod(l5,130)>120 ) THEN
                     z(k) = 6
                     k = k + 1
                     l5 = l5 + 5
                  ENDIF
                  z(k) = jj
                  l5 = l5 + 10
               CASE (2)
!
!     REAL, S.P. OR D.P.
!
                  j = mod(l5,130)
                  IF ( j>=120 ) THEN
                     IF ( j==120 ) THEN
                        l5 = l5 + 5
                        z(k) = 6
                        k = k + 1
                     ENDIF
                     l5 = l5 + 5
                     z(k) = 6
                     k = k + 1
                  ENDIF
                  z(k) = jj
                  l5 = l5 + 15
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
!
         ENDDO
!
!     NOW WE FORM THE FORMAT
!
         dp = .FALSE.
         kk = k
         z(1) = (l5-10)/5
         fmt(1) = lpri10
!
         l5 = 10
         l = 1
         i = 1
         ib = 1
         k = kk1
         spag_nextblock_1 = 6
      CASE (6)
         IF ( l5<130 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         l = l + 1
         fmt(l) = rpren
         IF ( debug ) THEN
            CALL page2(-5)
            WRITE (nout,99012) (fmt(j),j=1,l)
         ENDIF
!WKBD 7/94   520 WRITE  (OUT,FMT,ERR=530) (RZ(J),J=IB,I)
!WKBNB 7/94
         IF ( mach/=5 .AND. mach/=2 ) THEN
            isave = nout
            nout = Out
            CALL forwrt(fmt,rz(ib),i-ib+1)
            nout = isave
         ELSE
            WRITE (Out,fmt,ERR=60) (rz(j),j=ib,i)
         ENDIF
!WKBNE 7/94
 60      ib = i + 1
         l5 = 0
         l = 1
         fmt(1) = lpren
         spag_nextblock_1 = 7
      CASE (7)
!
         k = k + 1
         IF ( k>kk ) THEN
!
            l = l + 1
            fmt(l) = rpren
            IF ( debug ) THEN
               CALL page2(-5)
               WRITE (nout,99012) (fmt(j),j=1,l)
            ENDIF
!
!     REMOVED SECOND HALVES OF ALL D.P. NUMBERS IF THEY ARE PRESENT
!     THEN WRITE THE ARRAY OUT WITH THE GENERATED FORMAT
!
            IF ( dp ) THEN
               k = ib - 1
               DO j = ib , i
                  IF ( z(j)/=del ) THEN
                     k = k + 1
                     z(k) = z(j)
                  ENDIF
               ENDDO
               i = k
            ENDIF
!WKBD 7/94  680 WRITE (OUT,FMT,ERR=690) (RZ(J),J=IB,I)
!WKBNB 7/94
            IF ( mach/=2 .AND. mach/=5 ) THEN
               isave = nout
               nout = Out
               CALL forwrt(fmt,rz(ib),i-ib+1)
               nout = isave
            ELSE
               WRITE (Out,fmt,ERR=80) (rz(j),j=ib,i)
            ENDIF
         ELSE
            i = i + 1
            l = l + 1
            j = z(k)
            IF ( j==3 ) THEN
!
!     S.P. REAL NUMBERS
!
               fmt(l) = fmtr
               l5 = l5 + 15
            ELSEIF ( j==4 ) THEN
!
               fmt(l) = fmtb
               l5 = l5 + 5
            ELSEIF ( j==5 ) THEN
!
!     D.P. NUMBERS
!
               fmt(l) = fmtd
               l5 = l5 + 15
               temp(1) = rz(l)
               temp(2) = rz(l+1)
               z(l) = sngl(dtemp)
               z(l+1) = del
               dp = .TRUE.
            ELSEIF ( j==6 ) THEN
!
!     FILLER
!
               fmt(l) = fmtx
               l5 = l5 + 5
               i = i - 1
            ELSE
!              0,  I,  R,  B,  D, FL
               fmt(l) = fmti
               l5 = l5 + 10
            ENDIF
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!WKBNE 7/94
!
!     RETURN TO PROCESS ANOTHER RECORD ON INPUT FILE
!
 80      debug = .FALSE.
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     ALL DONE. SET WRT FLAG, UPDATE LFN AND FN, AND CLOSE INPUT FILE
!     AND ECHO USER MESSAGES
!
 100     Wrt = 1
         IF ( Lfn<0 ) Lfn = 0
         Lfn = Lfn + 1
         Fn(1,Lfn) = name(1)
         Fn(2,Lfn) = name(2)
         Fn(3,Lfn) = tble
         CALL close(In,1)
         IF ( p4==1 ) THEN
            i = 1
            WRITE (Out,99007) i , end
99007       FORMAT (1X,I9,1X,A4)
            CALL page2(-13)
            WRITE (nout,99013) uim , name
            WRITE (nout,99008)
99008       FORMAT (5X,'FORTRAN FORMATTED WRITE, 130 CHARACTERS PER LINE -',/10X,'(''/'',A4 FOR BCD WORD       ( 5 BYTES)',/11X,    &
                   &'''I'',I9 FOR INTEGER        (10 BYTES)',/11X,'''R'',E14.7 FOR S.P. REAL   (15 BYTES)',/11X,                    &
                   &'''D'',D14.7 FOR D.P. NUMBER (15 BYTES)',/11X,'''X    '', FOR FILLER       ( 5 BYTES)')
         ELSE
            CALL page2(-7)
            WRITE (Out) i , end
            WRITE (nout,99013) uim , name
            WRITE (nout,99009)
99009       FORMAT (5X,'FORTRAN UNFORMATTED (BINARY) WRITE')
         ENDIF
         GOTO 180
!
!     ERROR
!
 120     j = 1
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 140     j = 2
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 160     j = 3
         spag_nextblock_1 = 9
      CASE (8)
         In = j
         j = 8
         spag_nextblock_1 = 9
      CASE (9)
         CALL mesage(j,In,sub)
         spag_nextblock_1 = 11
      CASE (10)
         WRITE (nout,99010) uwm , ji , jj
99010    FORMAT (A25,', OUTPTT5 MODULE PARAMETER ERROR.  WRONG INDEX ','VALUES',2I3)
         spag_nextblock_1 = 11
      CASE (11)
         CALL fname(In,name)
         WRITE (nout,99011) name
99011    FORMAT (/5X,'TABLE DATA BLOCK ',2A4,' WAS NOT COPIED TO OUTPUT',' TAPE')
         DO
            CALL fwdrec(*180,In)
         ENDDO
!
 180     RETURN 1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99012 FORMAT (/,' DYNAMICALLY GENERATED FORMAT =',/,(1X,7A10))
99013 FORMAT (A29,' FROM OUTPUT5 MODULE, SUCCESSFUL TABLE-DATA ','TRANSFERED FROM INPUT FILE ',2A4,' TO OUTPUT TAPE',//5X,          &
             &'A HEADER RECORD WAS FIRST WRITTEN, THEN FOLLOWED BY')
END SUBROUTINE table5
