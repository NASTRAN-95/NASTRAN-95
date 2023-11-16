
SUBROUTINE outpt2
!
!     COPY DATA BLOCK(S) ONTO FORTRAN UNIT.
!
!     CALL TO THIS MODULE IS
!
!     OUTPUT2    IN1,IN2,IN3,IN4,IN5/ /V,N,P1/V,N,P2/V,N,P3/
!                                      V,N,P4/V,N,P5/V,N,P6 $
!
!             P1 = 0, NO ACTION TAKEN BEFORE WRITE
!                     (DEFAULT P1=0)
!                =+N, SKIP FORWARD N DATA BLOCKS BEFORE WRITE
!                =-1, BEFORE WRITE, FORTRAN TAPE IS REWOUND AND A
!                     HEADER RECORD (RECORD NUMBER 0) ADDED TO TAPE
!                =-3, THE NAMES OF ALL DATA BLOCKS ON FORTRAN TAPE
!                     ARE PRINTED AND WRITE OCCURS AT THE END OF TAPE
!                =-9, WRITE A NULL FILE, ENDFILE AND REWIND FORTRAN
!                     TAPE.
!
!             P2 =    THE FORTRAN UNIT NO. ON WHICH THE DATA BLOCKS WILL
!                     BE WRITTEN. (DEFAULT P2=11).
!
!             P3 =    TAPE ID CODE FOR FORTRAN TAPE, AN ALPHANUMERIC
!                     VARIABLE WHOSE VALUE WILL BE WRITTEN ON A FORTRAN
!                     TAPE.
!                     THE WRITING OF THIS ITEM IS DEPENDENT ON THE
!                     VALUE OF P1 AS FOLLOWS.
!                          *P1*             *TAPE ID WRITTEN*
!                           +N                     NO
!                            0                     NO
!                           -1                    YES
!                           -3                     NO (WARNING CHECK)
!                    (DEFAULT P3 = XXXXXXXX).
!
!             P4 = 0, FORTRAN WRITTEN RECORD SIZE IS UNLIMITTED
!                     (DEFAULT FOR ALL MACHINES, EXECPT IBM)
!                =-N, MAXIMUM FORTRAN WRITTEN RECORD SIZE IS N TIMES
!                     THE SYSTEM BUFFER SIZE, N*BUFFSIZE
!                = N, MAXIMUM FORTRAN WRITTEN RECORD SIZE IS N WORDS.
!                -    IN ALL CASES, THE MAXIMUM FORTRAN WRITTEN RECORD
!                     SIZE SHOLD BE .GE. BUFFSIZE, AND .LE. AVAILABLE
!                     CORE
!                IBM, IF P4=0, AND SINCE IBM CAN NOT HANDLE UNLIMITED
!                     RECORD SIZE, RECORD SIZE P4 OF 1024 WORDS IS USED
!
!             P5 = 0  FOR NON-SPARSE, AND NON-ZERO FOR SPARSE MATRIX
!                     OUTPUT
!                = 0, KEY-WORD RECORD CONTAINS EFFECTIVELY ONE SINGLE
!                     WORD OF DATA (THIS IS THE ORIGINAL COSMIC/OUTPT2)
!                = NOT 0, KEY-WORD RECORD CONTAINS 2 WORDS, THUS ALLOW
!                     SPARSE MATRIX TO BE COPIED OUT.
!                     FIRST KEY WORD:
!                        >0, DEFINES THE LENGTH OF NEXT DATA RECORD
!                        =0, END-OF-FILE
!                        <0, END-OF-RECORD WITH ANOTHER RECORD TO FOLLOW
!                     SECOND KEY WORD:
!                        =0, TABLE DATA, OR P5 SPARSE MATRIX OPTION NOT
!                            REQUESTED
!                        >0, ROW-BASE FOR NEXT RECORD. FOR EXAMPLE:
!                            KEYS = 10,200 INDICATE NEXT DATA RECORD IS
!                            FOR ROW(200+1) THRU ROW(200+10)
!                            i.e. (ROW(KEY2+J),J=1,KEY1)
!
!             P6 = BLANK (DEFAULT)
!                = *MSC*,    OUTPUT2 WILL ISSUE RECORDS IN MSC/OUTPUT2
!                            FORMAT WHICH IS SLIGHTLY DIFFERENT FROM
!                            COSMIC/OUTPUT2.
!                            (P5 OPTION IS NOT AVAILABLE)
!
!     NOTES ABOUT P5
!             (1) P5 IS IGNORED IN TABLE DATA
!             (2) POSSIBLY, NON-ZERO ROW ELEMENT MAY START AT 2ND HALF
!                 OF A COMPLEX WORD
!             (3) UP TO 3 ZEROS MAY BE IMBEDDED IN NON-ZERO STRING
!             (4) THE CHOICE OF 2 KEY WORDS IN ONE KEY RECORD OVER 2 KEY
!                 WORDS IN TWO RECORDS (AS IN MSC/NASTRAN), IS NOT TO
!                 MAKE THE ORIGINAL COSMIC OUTPT2/INPTT2 OBSOLETE.
!                 (i.e. WE DON'T FOLLOW OTHER PEOPLE BLINDLY SO TO MAKE
!                 OURSELVES OBSOLETE)
!             (5) ALTHOUGH OUTPT2 ALWAYS WRITES 2 KEY WORDS OUT IN A
!                 RECORD. ONE MAY CHOOSE TO READ BACK ONE OR BOTH KEYS.
!
!     REVISED  11/90 BY G.CHAN/UNISYS TO INCLUDE P4 AND P5 PARAMETERS
!     LAST REVISED  2/93 BY G.CHAN    TO INCLUDE P6 PARAMETER
!
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Buffsz , D(3) , Dum(2) , Dum2(2) , Dum6(6) , Incr , Irow , Itype , Line , Mach , Nlpp , Nout , Nrow , P1 , P2 , P3(2) ,  &
         & P4 , P5 , P6(2) , X(1)
   CHARACTER*80 Dsnames(80)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / P1 , P2 , P3 , P4 , P5 , P6
   COMMON /dsname/ Dsnames
   COMMON /machin/ Mach
   COMMON /system/ Buffsz , Nout , Dum6 , Nlpp , Dum2 , Line , Dum , D
   COMMON /unpakx/ Itype , Irow , Nrow , Incr
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ X
!
! Local variable declarations
!
   LOGICAL dp , sparse
   INTEGER dsp , dx(3) , endfil , endrec , i , icrq , idhdr(7) , idhdrx(7) , ifirst , ii , inbuf , index , inp(3) , input , irec1 , &
         & irec2 , j , j12 , k , k1 , k2 , kb , kbe , ke , key , keyx , kf , kk , l , lcor , lrec , mm , mnin , mone , msc , mtre , &
         & mtwo , name(2) , namex(2) , ncol , nf , ns , nskip , nwds , out , p3x(2) , ret , subnam(2) , tapcod(2) , trl(8) , zero
   INTEGER korsz
   CHARACTER*6 matrix , mt , table
!
! End of declarations
!
!WKBNB
!WKBNE
   DATA subnam/4HOUTP , 4HT2  / , matrix , table/'MATRIX' , ' TABLE'/
   DATA inp/1HT , 1H1 , 1H2/ , msc/4HMSC /
   DATA zero , mone , mtwo , mtre , mnin/0 , -1 , -2 , -3 , -9/
   DATA idhdr/4HNAST , 4HRAN  , 4HFORT , 4H TAP , 4HE ID , 4H COD , 4HE - /
!WKBI
   DATA ifirst/0/
!
!     CHECK P2 AND P4 PARAMETERS
!
!WKBI 3/95 SPR94016
   lcor = korsz(X) - Buffsz
   IF ( P2<11 .OR. P2>21 ) THEN
      j = 11
      WRITE (Nout,99001) Uwm , P2 , j , inp(i)
99001 FORMAT (A25,' FROM OUTPUT2 MODULE. UNACCEPTABLE FORTRAN UNIT',I3,' WAS CHANGED TO',I3,' (INP',A1,1H))
      P2 = j
   ENDIF
   IF ( P4<0 ) THEN
      lrec = -P4*Buffsz
   ELSEIF ( P4==0 ) THEN
      lrec = lcor
      IF ( Mach==2 ) lrec = 1024
      IF ( P6(1)==msc ) lrec = 2*Buffsz
   ELSE
      lrec = P4
   ENDIF
   IF ( lrec>lcor ) lrec = lcor
   IF ( lrec<Buffsz ) lrec = Buffsz
   IF ( P4/=0 ) WRITE (Nout,99002) Uim , lrec
99002 FORMAT (A29,' 4116, MAXIMUM FORTRAN RECORD SIZE USED IN OUTPUT2 ','WAS',I8,' WORDS')
   P4 = lrec
!WKBNB
   IF ( ifirst==0 ) THEN
      CLOSE (UNIT=P2)
      OPEN (UNIT=P2,FILE=Dsnames(P2),FORM='UNFORMATTED',STATUS='UNKNOWN')
      ifirst = 1
   ENDIF
!WKBNE
   IF ( P6(1)==msc ) CALL outmsc(*99999,*700)
!
   sparse = .FALSE.
   IF ( P5/=0 ) sparse = .TRUE.
   endfil = 0
   endrec = 0
!WKBD 3/95 SPR94016      LCOR   = KORSZ(X) - BUFFSZ
   icrq = -lcor
   IF ( lcor<=0 ) GOTO 1600
   inbuf = lcor + 1
   tapcod(1) = P3(1)
   tapcod(2) = P3(2)
   out = P2
   IF ( P1==mnin ) THEN
!
!     FINAL CALL TO OUTPUT2. (P1 = -9)
!
      WRITE (out) endfil , zero
      GOTO 700
   ELSEIF ( P1<mtre .OR. P1==mtwo ) THEN
!
      WRITE (Nout,99003) Ufm , P1
99003 FORMAT (A23,' 4120, MODULE OUTPUT2 - ILLEGAL VALUE FOR FIRST ','PARAMETER =',I20)
      Line = Line + 2
!
      mm = -37
      CALL mesage(mm,input,subnam)
      GOTO 99999
!
   ELSEIF ( P1==mtre ) THEN
!
!     OBTAIN LIST OF DATA BLOCKS ON FORTRAN TAPE.  (P1 = -3)
!
      REWIND out
      READ (out) key
      keyx = 3
      IF ( key/=keyx ) GOTO 1700
      READ (out) dx
      READ (out) key
      keyx = 7
      IF ( key/=keyx ) GOTO 1700
      READ (out) idhdrx
      DO kf = 1 , 7
         IF ( idhdrx(kf)/=idhdr(kf) ) GOTO 1500
      ENDDO
      READ (out) key
      keyx = 2
      IF ( key/=keyx ) GOTO 1700
      READ (out) p3x
      IF ( p3x(1)/=P3(1) .OR. p3x(2)/=P3(2) ) THEN
         WRITE (Nout,99004) Uwm , p3x , P3
99004    FORMAT (A25,' 4131, FORTRAN TAPE ID CODE -',2A4,'- DOES NOT MATCH',' THIRD OUTPUT2 DMAP PARAMETER -',2A4,2H-.)
         Line = Line + 2
      ENDIF
      ASSIGN 800 TO ret
      nskip = 1
      GOTO 1200
   ELSE
      IF ( P1<=zero ) GOTO 300
!
      i = 1
   ENDIF
 100  READ (out) key
   keyx = 2
   IF ( key/=keyx ) GOTO 1700
   READ (out) namex
   READ (out) key
   IF ( key>=0 ) THEN
      WRITE (Nout,99013) Sfm , key
      Line = Line + 2
      mm = -37
      CALL mesage(mm,input,subnam)
      GOTO 99999
   ELSE
      ASSIGN 200 TO ret
      nskip = 1
      GOTO 1200
   ENDIF
 200  i = i + 1
   IF ( i<=P1 ) GOTO 100
!
 300  IF ( P1==mone ) THEN
!
!     REWIND OUTPUT TAPE. (P1 = -1)
!
      REWIND out
      key = 3
      WRITE (out) key , zero
      WRITE (out) D
      key = 7
      WRITE (out) key , zero
      WRITE (out) idhdr
      key = 2
      WRITE (out) key , zero
      WRITE (out) P3
      endrec = endrec - 1
      WRITE (out) endrec , zero
      WRITE (out) endfil , zero
      endrec = 0
   ENDIF
!
 400  DO i = 1 , 5
      input = 100 + i
      trl(1) = input
      CALL rdtrl(trl)
      IF ( trl(1)<=0 ) CYCLE
      CALL fname(input,name)
!
!     OPEN INPUT DATA BLOCK TO READ WITH REWIND.
!
      CALL open(*1400,input,X(inbuf),0)
      CALL skprec(input,1)
      trl(8) = 1
      CALL rectyp(input,irec1)
      IF ( irec1==0 ) THEN
         trl(8) = 0
         CALL read(*450,*450,input,X(1),1,1,nf)
         CALL rectyp(input,irec2)
         IF ( irec2/=0 ) trl(8) = 2
      ENDIF
 450  CALL rewind(input)
      key = 2
      WRITE (out) key , zero
      WRITE (out) name
      endrec = endrec - 1
      WRITE (out) endrec , zero
      key = 8
      WRITE (out) key , zero
      WRITE (out) trl
      endrec = endrec - 1
      WRITE (out) endrec , zero
      index = 0
 500  DO
!
!     COPY CONTENTS OF INPUT DATA BLOCK ONTO FILE.
!     (OR THE HEADER RECORD OF A MATRIX DATA BLOCK)
!
!     COMMENTS FROM G.CHAN/UNISYS  2/93
!     THE WRITES IN LOOP 110 AND 120 SEEM DATA TYPE (S.P. OR D.P.)
!     INCENSITIVE. THE D.P. DATA IN KELM, MELM AND BELM TABLES SHOULD
!     WORK OK.
!
         CALL read(*650,*550,input,X(1),lrec,0,nf)
         WRITE (out) lrec , zero
         WRITE (out) (X(l),l=1,lrec)
      ENDDO
!
 550  WRITE (out) nf , zero
      WRITE (out) (X(l),l=1,nf)
      endrec = endrec - 1
      WRITE (out) endrec , zero
      IF ( trl(8)==0 ) GOTO 500
      IF ( trl(8)/=1 ) THEN
         IF ( index<=0 ) THEN
            index = 1
            GOTO 500
         ENDIF
      ENDIF
!
!     COPY STRING FORMATTED MATRIX
!
      IF ( trl(8)/=2 .OR. index/=2 ) THEN
         index = 2
         nwds = trl(5)
         dp = .FALSE.
         IF ( nwds==2 .OR. nwds==4 ) dp = .TRUE.
         dsp = 1
         IF ( dp ) dsp = 2
         IF ( nwds==3 ) nwds = 2
!         NWDS=1,SP  -  =2,DP,CS  -  =4,CDP
!
         Incr = 1
         nwds = trl(3)*nwds
!
!     CHECK FOR NULL MATRIX
!
         IF ( trl(2)==0 .OR. trl(3)==0 ) GOTO 650
!
!     NWDS HAS NUMBER WORDS NEEDED PER COLUMN
!
         icrq = nwds - lcor
         IF ( nwds>lcor ) GOTO 1600
         Itype = trl(5)
         Irow = 1
         Nrow = trl(3)
         ncol = trl(2)
         IF ( trl(8)==2 ) ncol = 1
      ENDIF
      DO l = 1 , ncol
         CALL unpack(*600,input,X)
         IF ( sparse ) THEN
!
!     SPARSE MASTRIX OUT
!
            j12 = -1
            DO j = 1 , nwds , dsp
               IF ( j12<+1 ) THEN
                  IF ( X(j)==0.0 ) THEN
                     IF ( .NOT.(dp) ) CYCLE
                     IF ( X(j+1)==0 ) CYCLE
                  ENDIF
                  j12 = +1
                  k2 = j - 1
               ELSEIF ( X(j)==0.0 ) THEN
                  IF ( dp ) THEN
                     IF ( X(j+1)/=0 ) CYCLE
                  ENDIF
                  IF ( j12==-1 ) CALL mesage(-37,0,subnam)
                  j12 = j12 + 1
!
!     ALLOW UP TO 3 IMBEDDED ZEROS
!
                  IF ( j12>3 ) THEN
                     IF ( X(j-1)==0.0 .AND. X(j-2)==0.0 ) THEN
                        j12 = -1
                        k1 = j - k2
                        IF ( k1>lrec ) THEN
                           ke = j
                           kb = k2 + 1
                           DO kk = kb , ke , lrec
                              k2 = kk - 1
                              k1 = k2 + lrec
                              IF ( k1>ke ) k1 = ke
                              WRITE (out) k1 , k2
                              WRITE (out) (X(k2+k),k=1,k1)
                           ENDDO
                        ELSE
                           WRITE (out) k1 , k2
                           WRITE (out) (X(k2+k),k=1,k1)
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
!
            IF ( j12/=-1 ) THEN
               j12 = -1
               k1 = nwds - k2
               IF ( k1>=lrec ) THEN
                  ke = nwds
                  kb = k2 + 1
                  DO kk = kb , ke , lrec
                     k2 = kk - 1
                     k1 = k2 + lrec
                     IF ( k1>ke ) k1 = ke
                     WRITE (out) k1 , k2
                     WRITE (out) (X(k2+k),k=1,k1)
                  ENDDO
               ELSE
                  WRITE (out) k1 , k2
                  WRITE (out) (X(k2+k),k=1,k1)
               ENDIF
            ENDIF
!
            endrec = endrec - 1
            WRITE (out) endrec , zero
            CYCLE
         ENDIF
 560     DO kb = 1 , nwds , lrec
            ke = kb + lrec - 1
            IF ( ke>nwds ) ke = nwds
            kbe = ke - kb + 1
            WRITE (out) kbe , zero
            WRITE (out) (X(k),k=kb,ke)
         ENDDO
!
 580     endrec = endrec - 1
         WRITE (out) endrec , zero
         CYCLE
 600     IF ( sparse ) GOTO 580
         DO k = 1 , nwds
            X(k) = 0
         ENDDO
         GOTO 560
!
      ENDDO
!
      IF ( trl(8)==2 ) GOTO 500
!
!     CLOSE INPUT DATA BLOCK WITH REWIND
!
 650  CALL close(input,1)
!
      WRITE (out) endfil , zero
      endrec = 0
      CALL page2(-4)
      mt = matrix
      IF ( trl(8)==0 ) mt = table
      WRITE (Nout,99005) Uim , mt , name , out , (trl(ii),ii=2,7)
99005 FORMAT (A29,' 4114, ',A6,' DATA BLOCK ',2A4,' WRITTEN ON FORTRAN UNIT',I4,/5X,'TRAILR =',5I6,I9)
      IF ( sparse .AND. trl(8)/=0 ) WRITE (Nout,99006)
99006 FORMAT (1H+,55X,'(SPARSE MATRIX)')
!
   ENDDO
   GOTO 99999
 700  endrec = 0
   ENDFILE out
   REWIND out
   WRITE (Nout,99007) Uim
99007 FORMAT (A29,'. OUTPUT2 MODULE WROTE AN E-O-F RECORD, A SYSTEM ','E-O-F MARK, AND REWOUND THE OUTPUT TAPE. (P1=-9)')
   GOTO 99999
 800  kf = 0
 900  CALL page1
   Line = Line + 8
   WRITE (Nout,99008) out
99008 FORMAT (1H0,50X,30HFILE CONTENTS ON FORTRAN UNIT ,I2,/51X,32(1H-),///54X,4HFILE,18X,4HNAME/1H0)
 1000 READ (out) key
   IF ( key<0 ) THEN
      WRITE (Nout,99009) Sfm
99009 FORMAT (A25,' 4115, MODULE OUTPUT2 - SHORT RECORD.')
      Line = Line + 2
      mm = -37
      CALL mesage(mm,input,subnam)
      GOTO 99999
   ELSEIF ( key==0 ) THEN
      ASSIGN 400 TO ret
      nskip = -(kf+1)
   ELSE
      READ (out) namex
      ASSIGN 1100 TO ret
      nskip = 1
   ENDIF
   GOTO 1200
 1100 kf = kf + 1
   Line = Line + 1
   WRITE (Nout,99010) kf , namex
99010 FORMAT (53X,I5,18X,2A4)
   IF ( Line>=Nlpp ) GOTO 900
   GOTO 1000
!
!     SIMULATION OF SKPFIL (OUT,NSKIP)
!
 1200 IF ( nskip<0 ) THEN
      REWIND out
!
!     NSKIP IS THE NEGATIVE OF THE NUMBER OF FILES TO BE SKIPPED
!
      nskip = -nskip
   ELSEIF ( nskip==0 ) THEN
      GOTO 1300
   ENDIF
   DO ns = 1 , nskip
      DO
         READ (out) key
         IF ( key<0 ) THEN
         ELSEIF ( key==0 ) THEN
            EXIT
         ELSE
!     ICRQ = KEY - LCOR
!     IF (KEY .GT. LCOR) GO TO 9917
            READ (out) l
         ENDIF
      ENDDO
   ENDDO
 1300 GOTO ret
!
!
!     ERRORS
!
 1400 mm = -1
   CALL mesage(mm,input,subnam)
   GOTO 99999
 1500 WRITE (Nout,99011) Ufm , (idhdrx(kf),kf=1,7)
99011 FORMAT (A23,' 4130, MODULE OUTPUT2 - ILLEGAL TAPE CODE HEADER = ',7A4)
   Line = Line + 2
   mm = -37
   CALL mesage(mm,input,subnam)
   GOTO 99999
 1600 mm = -8
   input = icrq
   CALL mesage(mm,input,subnam)
   GOTO 99999
 1700 WRITE (Nout,99013) Sfm , key
   WRITE (Nout,99012) keyx
99012 FORMAT (10X,17HEXPECTED VALUE = ,I10,1H.)
   Line = Line + 3
   mm = -37
   CALL mesage(mm,input,subnam)
99013 FORMAT (A25,' 2190, ILLEGAL VALUE FOR KEY =',I10,1H.)
!
99999 RETURN
END SUBROUTINE outpt2