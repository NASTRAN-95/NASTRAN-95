
SUBROUTINE input4(Nmat,Unitx,Tape,Bcdopt)
!
!     THIS SUBROUTINE IS CALLED ONLY BY INPTT4. IT READS USER-SUPPLIED
!     TAPE (OR DISC FILE), AS GENERATED BY COSMIC or MSC/OUTPUT4 MODULE,
!     AND CREATES THE CORRESPONDING MATRIX DATA BLOCKS.
!
!     INPUTT4 MODULE DOES NOT HANDLE TABLE DATA BLOCKS.
!
!     DUE TO INSUFFICEINT DOCUMENTATION IN MSC USER MANUAL, THIS INPUT4
!     MAY NOT WORK WITH BCD/ASCII DATA AS GENERATED BY MSC/OUTPUT4
!
!     MATRICES CAN BE IN S.P. OR D.P.; DENSE OR SPARSE.
!     NO MATRIX CONVERSION IN THIS ROUTINE
!     i.e. TYPE OF MATRIX OUT = TYPE OF MATRIX IN
!
!     DEFINITION OF DENSE AND SPARSE MATRICES IN THIS SUBROUTINE -
!     DENSE MATRIX IS PROCESSED FROM FIRST TO LAST NONZERO TERMS OF
!     COLUMNS, AND SPARSE MATRIX IS PROCESSED BY STRINGS.
!
!     WRITTEN BY G.CHAN/UNISYS            JUNE  1989
!     LAST REVISION WITH MAJOR CHANGES    MARCH 1993
!
!     NMAT   = NUMBER OF MATRICES (5 MAX) WRITTEN ON USER'S TAPE
!     UNITX  = INPUT TAPE LOGICAL UNIT*, INTEGER, NO DEFAULT
!     TAPE   = TAPE READ CONTROL
!            = 0  NO ADDITIONAL ACTION BEFORE READ
!            =-1  REWIND UNITX  BEFORE READ
!            =-2  REWIND UNITX  AT END
!            =-3  BOTH
!     BCDOPT = 1  INPUT TAPE IN BINARY FORMAT
!            = 2  INPUT TAPE IN ASCII  FORMAT
!                 IF INPUT MATRIX IS IN S.P., I13 IS USED FOR INTEGER,
!                 AND 10E13.6 FOR S.P.REAL DATA
!                 IF INPUT MATRIX IS IN D.P., I16 IS USED FOR INTEGER,
!                 AND 8D16.9 FOR D.P.REAL DATA
!            = 3  SAME AS BCDOPT=2, EXECPT THAT I16 AND 8E16.9 ARE USED
!                 FOR INTEGERS AND S.P.REAL DATA. (BCDOPT=3 IS USED ONLY
!                 IN MACHINES WITH LONG WORDS (60 OR MORE BITS PER WORD)
!           NOTE- MATRIX HEADER RECORD IS NOT AFFECTED BY ABOVE FORMAT
!                 CHANGES. IT IS WRITTEN OUT BY (1X,4I13,5X,2A4)
!     P4     =-4,-2,-1,0,.GE.1, SEE P4 IN INPTT4
!
!     OUTFIL = UP TO 5 OUTPUT GINO DATA BLOCKS (MATRIX ONLY)
!              IF ANY OF THE OUTPUT DB IS PURGED, THE CORRESPONDING
!              MATRIX ON INPUT TAPE WILL BE SKIPPED.
!
!     * LOGICAL UNIT  vs.  GINO FILE NAME
!              ------     ----------------------
!                11        UT1  (CDC ONLY)
!                12        UT2  (CDC ONLY
!                14        INPT (VAX,UNIVAC)
!                15        INP1 (VAX,UNIVAC,IBM)
!                16        INP2      :
!                17        INP3      :
!                 :          :       :
!                23        INP9      :
!                24        INPT (IBM ONLY)
!
!
!     EACH MATRIX WAS WRITTEN AS FOLLOWS (IN BINARY OR ASCII), 4 INTEGER
!     WORDS + FILE NAME
!                1) NO. OF COLUMNS
!                2) NO. OF ROWS
!                3) FORM (NASTRAN  1 TO 8)
!                4) TYPE (NASTRAN  1 TO 4)
!              5,6) FILE NAME (BCD)
!
!     A  RECORD WAS WRITTEN FOR EACH NON-ZERO COLUMN
!        A) DENSE MATRIX:
!                1) COLUMN NO.
!                2) ROW POSITION OF FIRST NON-ZERO ELEMENT
!                3) NO. OF WORDS IN THIS COLUMN, ZEROS INCLUDED, FROM
!                   THE FIRST TO LAST NON-ZERO TERMS.
!                4) DATA VALUES FOR THIS COLUMN (REAL)
!        B) SPARSE MATRIX:
!                1) COLUMN NO.
!                2) ZERO (THIS ZERO IS THE SPARSE MATRIX FLAG)
!                3) NO. OF WORDS IN THIS COLUMN
!                4) DATA OF ONE OR MORE STRINGS.
!
!
!     EXAMPLE 1 - INPUT TAPE INP1 (UNIT 15) CONTAINS 5 MATRICES,
!     =========   WRITTEN BY COSMIC/OUTPUT4, BINARY.
!                 WE WANT TO COPY
!                 FILE 3 TO A,
!                 FILE 4 TO B
!
!     INPUTT4  /,,A,B,/-1/15      $ REWIND, READ & ECHO HEADER RECORD
!
!
!     EXAMPLE 2 - TO COPY THE FIRST 2 FILES OF A UNFORMATTED TAPE INP2
!     =========   (UNIT 16), WRITTEN BY MSC/OUTPUT4, DENSE MATRIX
!
!     INPUTT4  /A,B,,,/-3/16//-4  $
!
!     EXAMPLE 3 - TO COPY THE FIRST 2 FILES OF A FORMATTED ASCII TAPE
!     =========   INPT (UNIT 14), WRITTEN BY COSMIC/OUTPUT4, SPARSE
!                 MATRIX
!
!     INPUTT4  /A,B,,,/-3/-14//1  $
!
!     EXAMPLE 4 - SEE DEMO PROBLEM T00001A TO INPUT VARIOUS DATA BLOCKS
!     =========   (SQUARE, RECTANGULAR, ROW-VECTOR, 'COLUMN' VECOR,
!                 DIAGONAL, IDENTITY, SYMMETRIC) INTO NASTRAN SYSTEM
!                 USING MSC, ASCII FORMAT FILES.
!
!     A NOTE FOR FUTURE IMPROVEMENT, G.CHAN 4/93 -
!     IF INPUT MATRIX IS SYMMETRIC, MAKE AN OPTION TO INPUT ONLY THE
!     LOWER TRIANGULAR PORTION OF THE MATRIX, AND OBTAIN THE UPPER
!     PROTION THRU SYMMETRY.
!
!
   IMPLICIT NONE
   INTEGER Core(1) , Dum36(36) , Ii , Incr , Iz(1) , Jj , Mach , Nbpw , Nogo , Nout , Nwds(4) , P1 , P2 , P3(2) , P4 , Prec(2) ,    &
         & Sysbuf , Typin , Typout
   CHARACTER*80 Dsnames(80)
   DOUBLE PRECISION Dz(1)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   REAL Z(1)
   COMMON /blank / P1 , P2 , P3 , P4
   COMMON /dsname/ Dsnames
   COMMON /machin/ Mach
   COMMON /packx / Typin , Typout , Ii , Jj , Incr
   COMMON /system/ Sysbuf , Nout , Nogo , Dum36 , Nbpw
   COMMON /type  / Prec , Nwds
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   COMMON /zzzzzz/ Core
   INTEGER Bcdopt , Nmat , Tape , Unitx
   INTEGER base , blnk , buf1 , flag , i , icol , ifirst , imhere , iname(2,5) , iold , irow , irowp , j , j1 , j2 , k , kpb ,      &
         & lcor , lcore , ln , name(2) , ncol , ncol1 , nform , nn , nrow , ntype , nw , nwords , oname(2,5) , outfil(5) , output , &
         & skip(2) , subnam(2) , t(2,5) , trl(7) , ty(4) , typ(5)
   LOGICAL bo , cp , debug , dp , ms , sp
   REAL d , dr(2) , zero(4)
   DOUBLE PRECISION dd
   CHARACTER*11 fm , fmd , unf
   INTEGER korsz
!WKBR LOGICAL          BO,SP,CP,DP,MS,TAPEUP,TAPBIT,DEBUG
!WKBI
!WKBI
   !>>>>EQUIVALENCE (Iz(1),Z(1),Dz(1),Core(1)) , (dr(1),d,dd)
   DATA outfil/201 , 202 , 203 , 204 , 205/ , skip/4H(SKI , 4HP)  /
   DATA iname , oname , typ/25*4H    / , subnam/4HINPT , 2HT4/
   DATA ty/4HRSP  , 4HRDP  , 4HCSP  , 4HCDP / , zero/4*0.0/
   DATA fmd , unf/'FORMATTED  ' , 'UNFORMATTED'/blnk/4H    /
   DATA debug/.FALSE./
!WKBI
   DATA ifirst/0/
!
   sp = .FALSE.
   cp = .FALSE.
   dp = .FALSE.
   ms = P4== - 4
   bo = Bcdopt/=1
   lcore = korsz(Z(1))
   buf1 = lcore - Sysbuf
   lcor = buf1 - 1
   IF ( lcor<=0 ) CALL mesage(-8,lcore,subnam)
   IF ( Unitx>=10 .AND. Unitx<=24 ) THEN
      IF ( Unitx/=13 ) THEN
         IF ( Mach/=4 .OR. Unitx<13 ) THEN
            IF ( Mach==4 .OR. Unitx>13 ) THEN
!
               fm = unf
               IF ( bo ) fm = fmd
!WKBR WRITE  (NOUT,10) UIM,UNITX,INP(UNITX-10),FM
               WRITE (Nout,99001) Uim , Unitx , Dsnames(Unitx) , fm
!    1,                BCDOPT,P1,P2,P3,P4
!WKBR10 FORMAT (A29,'. INPUTT4 MODULE OPENING FORTRAN TAPE',I4,' (',A4,
99001          FORMAT (A29,'. INPUTT4 MODULE OPENING FORTRAN TAPE',I4,/,' (',A44,')',/,' FOR ',A11,' READ.')
!    2,      /5X,'BCDOPT,P1,P2,P3,P4 =',3I3,1X,2A4,I4)
!
!WKBR IF (MACH .GE. 5) GO TO 50
!WKBI
               CLOSE (UNIT=Unitx)
!WKBR OPEN (UNIT=UNITX,ACCESS='SEQUENTIAL',STATUS='OLD',FORM=FM,ERR=920)
!WKBI
               OPEN (UNIT=Unitx,ACCESS='SEQUENTIAL',STATUS='OLD',FORM=fm,ERR=200,FILE=Dsnames(Unitx))
!
               IF ( Tape==-1 .OR. Tape==-3 ) REWIND Unitx
!WKBI
               ifirst = 1
!
!     SET UP LOOP TO READ MATRIX FILES
!
               Incr = 1
               Ii = 1
               DO nn = 1 , Nmat
!
!     CHECK OUTPUT FILE REQUEST
!
                  output = outfil(nn)
                  trl(1) = output
                  CALL rdtrl(trl)
                  IF ( trl(1)>0 ) THEN
!
!     TRANSFER DATA FROM INPUT TAPE TO OUTPUT FILE
!
                     imhere = 210
                     IF ( bo ) THEN
                        IF ( .NOT.ms ) READ (Unitx,99016,ERR=400,END=300) ncol , nrow , nform , ntype , name
                        IF ( ms ) READ (Unitx,99017,ERR=400,END=300) ncol , nrow , nform , ntype , name
                     ELSE
                        imhere = 200
                        READ (Unitx,ERR=400,END=300) ncol , nrow , nform , ntype , name
                     ENDIF
!
                     IF ( debug ) WRITE (Nout,99016) ncol , nrow , nform , ntype , name
                     IF ( .NOT.debug ) WRITE (Nout,99002) nn , name
99002                FORMAT (5X,'READING DATA BLOCK NO.',I4,' - ',2A4,' FROM INPUT TAPE')
!
                     IF ( ms ) nform = -nform
                     IF ( bo .AND. nform>0 ) GOTO 100
!
!     THE ABOVE CHECK ON NFORM AND BO MAY BE ALREADY TOO LATE
!
                     IF ( .NOT.(ms) ) THEN
                        dp = .FALSE.
                        IF ( ntype==2 .OR. ntype==4 ) dp = .TRUE.
                        sp = .NOT.dp
                        cp = P4>=1 .AND. Nbpw>=60
                        IF ( cp ) sp = .FALSE.
                        IF ( cp ) dp = .FALSE.
                     ENDIF
                     flag = 0
                     IF ( ms ) flag = 1
                     IF ( sp ) flag = 2
                     IF ( cp ) flag = 3
                     IF ( dp ) flag = 4
                     IF ( flag==0 ) CALL mesage(-37,0,subnam)
                     nform = iabs(nform)
                     Jj = nrow
                     Typin = ntype
                     IF ( ms .AND. (Typin==2 .OR. Typin==4) ) Typin = Typin - 1
                     Typout = ntype
                     nwords = Nwds(Typin)
                     base = nrow*nwords
                     IF ( base>lcor ) CALL mesage(-8,lcore,subnam)
                     CALL makmcb(trl(1),output,nrow,nform,Typout)
                     iname(1,nn) = name(1)
                     iname(2,nn) = name(2)
                     CALL fname(output,name)
                     CALL open(*2,output,Iz(buf1),1)
                     CALL write(output,name,2,1)
                     oname(1,nn) = name(1)
                     oname(2,nn) = name(2)
                     typ(nn) = ty(ntype)
                     t(1,nn) = ncol
                     t(2,nn) = nrow
!
!     PROCESS EACH COLUMN (NON-ZERO OR NULL COLUMN ON FILE)
!     PLUS ONE EXTRA COLUMN, NCOL+1, AT THE END
!
                     iold = -1
                     Ii = 1
                     Jj = nrow
                     ncol1 = ncol + 1
                     i = 0
                     GOTO 4
                  ELSE
!
!     IF OUTPUT FILE IS PURGED, PURGE THE CORRESPONDING FILE ON INPUT
!     TAPE. CHECK IF THERE ARE MORE OUTPUT DATA BLOCK REQUESTED ON THE
!     SAME OUTPUT2 DMAP. QUIT IF THERE ARE NONE
!
                     i = nn
                     DO
                        i = i + 1
                        IF ( i>5 ) GOTO 20
                        trl(1) = outfil(i)
                        CALL rdtrl(trl)
                        IF ( trl(1)>0 ) THEN
!
!     SKIP PRESENT MATRIX DATA BLOCK ON INPUT TAPE
!
                           imhere = 120
                           IF ( bo ) THEN
!
!     SKIP ASCII FILES
!
                              IF ( .NOT.ms ) READ (Unitx,99016,ERR=400,END=300) ncol , j1 , j2 , ntype , name
                              IF ( ms ) READ (Unitx,99017,ERR=400,END=300) ncol , j1 , j2 , ntype , name
                              IF ( .NOT.(ms) ) THEN
                                 dp = ntype==2 .OR. ntype==4
                                 sp = .NOT.dp
                                 cp = P4>=1 .AND. Nbpw>=60
                                 IF ( cp ) THEN
                                    sp = .FALSE.
                                    dp = .FALSE.
                                 ENDIF
                              ENDIF
                              DO
                                 IF ( ms ) READ (Unitx,99019) icol , irow , nw
                                 IF ( sp ) READ (Unitx,99020) icol , irow , nw
                                 IF ( cp .OR. dp ) READ (Unitx,99021) icol , irow , nw
                                 IF ( icol>ncol ) THEN
!
                                    READ (Unitx,99018) j
                                    EXIT
                                 ELSE
                                    IF ( irow==0 ) nw = nw/65536
!
!     COMPUTE NO. OF RECORDS TO SKIP.
!
!     S.P. DATA ARE WRITTEN IN 10 VALUES PER RECORD (5 FOR MSC RECORD)
!     D.P. DATA, AND DATA FROM LONG WORD MACHINE, ARE IN 8 VALUES PER
!     RECORD (SEE FORMAT 650, 660, 670 AND 680)
!
                                    IF ( ms ) nw = (nw+4)/5
                                    IF ( sp ) nw = (nw+9)/10
                                    IF ( cp .OR. dp ) nw = (nw+7)/8
                                    DO j = 1 , nw
                                       READ (Unitx,99018) k
                                    ENDDO
                                 ENDIF
                              ENDDO
                           ELSE
!
!     SKIP BINARY FILES
!
                              imhere = 105
                              READ (Unitx,ERR=400,END=300) ncol , j1 , j2 , ntype , name
                              imhere = -110
                              DO
                                 READ (Unitx,ERR=10,END=300) icol
                                 IF ( icol>ncol ) EXIT
                              ENDDO
                           ENDIF
!
                           iname(1,nn) = name(1)
                           iname(2,nn) = name(2)
                           oname(1,nn) = skip(1)
                           oname(2,nn) = skip(2)
                           typ(nn) = ty(ntype)
                           t(1,nn) = j1
                           t(2,nn) = j2
                           GOTO 15
                        ENDIF
                     ENDDO
                  ENDIF
!
 2                WRITE (Nout,99003) Ufm , Dsnames(Unitx)
99003             FORMAT (A23,'. CANNOT OPEN OUTPUT FILE - ',/,A80)
                  GOTO 500
 4                DO
!
                     i = i + 1
                     IF ( debug ) WRITE (Nout,99004) i , ncol1
99004                FORMAT ('   INPUT4/@290   I,NCOL1 =',2I5)
                     IF ( i>ncol1 ) GOTO 8
                     DO j = 1 , base
                        Z(j) = 0.0
                     ENDDO
                     imhere = -400
                     IF ( bo ) EXIT
!
!     BINARY (UNFORMATTED) READ
!     -------------------------
!
                     imhere = -315
                     READ (Unitx,ERR=10,END=300) icol , irow , nw , (Z(k+base),k=1,nw)
!
                     IF ( icol>ncol ) GOTO 8
                     IF ( nw+base>lcor ) CALL mesage(-8,lcore,subnam)
                     DO WHILE ( i<icol )
!
!     NULL COLUMN(S) ENCOUNTERED
!
                        Jj = 1
                        CALL pack(Z(1),output,trl)
                        Jj = nrow
                        i = i + 1
                     ENDDO
!
                     IF ( irow==0 ) THEN
!
!     SPARSE INCOMING MATRIX.
!     THIS RECORD CONATINS ONE OR MORE STRINGS.
!
!     DATA ARE WRITTEN IN MULTIPLE STRINGS OF NON-ZERO TERMS. EACH
!     STRING IS PRECEED BY A CONTROL WORD
!       LN   = LENGTH OF STRING, LEFT HALF OF WORD
!       IROW = ROW POSITION,    RIGHT HALF OF WORD
!       LN AND IROW ARE DATA TYPE DEPENDENT
!     AND
!       K    = A RUNNING POINTER, POINTS TO THE CONTROL WORD OF EACH
!              STRING IN ARRAY Z HOLDING LN AND IROW INFORMATION
!
                        k = 1
                        DO
                           kpb = k + base
                           ln = Iz(kpb)/65536
                           irow = Iz(kpb) - ln*65536
                           irow = (irow-1)*nwords
                           ln = ln*nwords
!
!     S.P. OR D.P. MATRIX IN, S.P. OR. D.P. MATRIX OUT. THAT INCLUDE
!     REAL AND COMPLEX
!
                           DO j = 1 , ln
                              Z(j+irow) = Z(j+kpb)
                           ENDDO
                           k = k + ln + 1
                           IF ( k>=nw ) THEN
!
!     PACK ONE COLUMN OUT
!
                              CALL pack(Z(1),output,trl)
                              EXIT
                           ENDIF
                        ENDDO
                     ELSE
!
!     DENSE MATRIX FORMAT
!
!     DATA WERE WRITTEN FROM FIRST NON-ZERO TERM TO LAST NON-ZERO TERM
!     INCLUDING POSSIBLE ZERO TERMS.
!     IROW IS THE FIRST NON-ZERO TERM ROW POSITION
!
!     S.P. OR D.P. MATRIX IN, S.P. OR. D.P. MATRIX OUT. THAT INCLUDE
!     REAL AND COMPLEX.
!
                        irowp = (irow-1)*nwords
                        DO j = 1 , nw
                           Z(j+irowp) = Z(j+base)
                        ENDDO
                        CALL pack(Z(1),output,trl)
                     ENDIF
                  ENDDO
!
!     ASCII (FORMATTED) READ
!     ----------------------
!
!     THIS ASCII OPTION WORKS WELL WITH INPUT TAPE GENERATED FROM
!     COSMIC/OUTPUT4 MODULE. HOWEVER IT MAY OR MAY NOT WORK WITH INPUT
!     TAPE FROM MSC/OUTPUT4.
!
!     ASSUMPTIONS HERE FOR MSC/OUTPUT4 TAPE ARE -
!     1. INTEGER RECORDS AND FLOATING POINT RECORDS DO NOT MIXED
!     2. ONE OR MORE RECORDS HOLD A MATRIX COLUMN, EACH RECORD IS LESS
!        THAN 80 BYTES LONG.
!        INTEGER IN 3I8, BCD IN 2A4, AND S.P. REAL DATA IN 5E16.9
!
 6                IF ( flag==2 ) THEN
                     READ (Unitx,99020,ERR=10,END=300) icol , irow , nw
                  ELSEIF ( flag==3 .OR. flag==4 ) THEN
                     READ (Unitx,99021,ERR=10,END=300) icol , irow , nw
                  ELSE
                     READ (Unitx,99019,ERR=10,END=300) icol , irow , nw
                     IF ( debug ) WRITE (Nout,99020) icol , irow , nw
                  ENDIF
!
!     ICOL IS MATRIX COLUMN NUMBER READ IN FROM THE INPUT TAPE.
!          REPEATED ICOL FOR MULTIPLE STRINGS.
!     IROW IS .LT. 0, AND IABS(IROW) IS THE ROW POSITION OF STRING.
!     NW   IS LENGTH OF STRING.
!     I    IS THE CURRENT COLUMN NUMBER OF THE OUTPUT MATRIX.
!
!     POSSIBILITIES AT THIS POINT ARE -
!
!     1. ICOL = IOLD, ADD NEW STRING TO CURRENT COLUMN OF OUTPUT MATRIX.
!     2. ICOL = IOLD+1, PREVIOUS COLUMN JUST FINISHED, PACK IT OUT.
!     3. ICOL.GT.NCOL, OUTPUT MATRIX FINISH. ALL COLUMNS HAVE BEEN READ.
!               READ ONE MORE DUMMY RECORD BEFORE WRAP UP THIS MATRIX
!     4. IN ALL CASES, ZERO OUT Z ARRAY FOR NEW DATA, AND INCREASE
!        COLUMN COUNTER I BY 1
!     5. ICOL .LT. I, LOGIC ERROR
!     6. ICOL .GT. I, PACK NULL COLUMN(S) OUT.
!     7. ICOL .EQ. I, CURRENT INPUT RECORD IS FOR THE I-TH COLUMN.
!
                  IF ( nw*nwords>lcor ) CALL mesage(-8,lcore,subnam)
                  IF ( icol/=iold ) THEN
                     IF ( icol==iold+1 ) CALL pack(Z(1),output,trl)
                     imhere = -550
                     IF ( icol>ncol ) THEN
!
                        READ (Unitx,99018,ERR=10,END=300) j
                        GOTO 8
                     ELSE
                        DO j = 1 , base
                           Z(j) = 0.0
                        ENDDO
                        DO
! 490 I = I + 1
                           IF ( icol<i ) THEN
                              WRITE (Nout,99005) Sfm , i , icol , iold , ncol , irow , nw , sp , cp , dp , ms , flag
99005                         FORMAT (A25,'. LOGIC ERROR @470, I,ICOL =',2I6,/5X,'  IOLD,NCOL,IROW,NW =',4I6,'  SP,CP,DP,MS,FLAG =',&
                                    & 4L2,I4)
                              CALL mesage(-37,0,subnam)
                              READ (Unitx,99018,ERR=10,END=300) j
                              GOTO 8
                           ELSEIF ( icol==i ) THEN
!
                              IF ( irow<=0 ) THEN
!
!     SPARSE INCOMING MATRIX
!
!     OUTPUT4 WRITES OUT THE ASCII STRING DATA IN FOLLOWING FORMATS -
!     EACH STRING, PRECEEDED BY A 3-INTEGER - ICOL,IROW,NW - CONTROL
!     RECORD, AND CONTINUE INTO ONE OR MORE DATA RECORDS OF 130 OR
!     128 BYTES EACH. (80 BYTES MSC RECORD)
!     NW   = LENGTH OF STRING IN THE FOLLOWING DATA RECORDS, S.P. OR
!            D.P. DEPENDENT.
!     IROW = IABS(IROW) IS ROW POSITION IF FIRST WORD OF STRING
!     ICOL = COLUMN NUMBER OF MATRIX
!
!     NOTICE THAT OUTPUT4 MAY WRITE OUT A MATRIX COLUMN IN MULTI-STRING
!     RECORDS, WITH THE SAME COLUMN VALUE ICOL IN THE EACH 3-INTEGER
!     CONTROL RECORD. IN THIS CASE, MROW IS ALWAYS NEGATIVE.
!     (IF IROW IS ZERO, MATRIX WAS WRITTEN OUT IN DENSE FORMAT)
!
                                 iold = icol
                                 EXIT
                              ELSE
!
!     DENSE MATRIX FORMAT
!
                                 irow = irow - 1
                                 imhere = 605
                                 IF ( flag==2 ) THEN
                                    READ (Unitx,99023,ERR=10,END=300) (Z(k+irow),k=1,nw)
                                 ELSEIF ( flag==3 ) THEN
                                    READ (Unitx,99024,ERR=10,END=300) (Z(k+irow),k=1,nw)
                                 ELSEIF ( flag==4 ) THEN
                                    READ (Unitx,99025,ERR=10,END=300) (Dz(k+irow),k=1,nw)
                                 ELSE
                                    READ (Unitx,99022,ERR=10,END=300) (Z(k+irow),k=1,nw)
                                    IF ( debug ) WRITE (Nout,99023) (Z(k+irow),k=1,nw)
                                 ENDIF
                                 CALL pack(Z(1),output,trl)
                                 GOTO 4
                              ENDIF
                           ELSE
                              CALL pack(Z(1),output,trl)
                              i = i + 1
                           ENDIF
                        ENDDO
                     ENDIF
                  ENDIF
                  irow = iabs(irow) - 1
                  IF ( Typin>=3 ) irow = irow*2
                  imhere = 715
                  IF ( flag==2 ) THEN
                     READ (Unitx,99023,ERR=10,END=300) (Z(k+irow),k=1,nw)
                  ELSEIF ( flag==3 ) THEN
                     READ (Unitx,99024,ERR=10,END=300) (Z(k+irow),k=1,nw)
                  ELSEIF ( flag==4 ) THEN
                     READ (Unitx,99025,ERR=10,END=300) (Dz(k+irow),k=1,nw)
                  ELSE
                     READ (Unitx,99022,ERR=10,END=300) (Z(k+irow),k=1,nw)
                  ENDIF
                  GOTO 6
!
 8                CALL close(output,1)
                  CALL wrttrl(trl)
                  IF ( debug ) WRITE (Nout,99006) Uim , name , Dsnames(Unitx) , trl
99006             FORMAT (A29,' FROM INPUTT4 MODULE. ',2A4,' WAS RECOVERED FROM ',/,A44,' INPUT TAPE SUCCESSFULLY.',/5X,'TRAIL =',  &
                        & 6I6,I9)
                  CYCLE
!
!     BAD DATA ON INPUT TAPE
!
 10               WRITE (Nout,99007) Ufm , Dsnames(Unitx) , Unitx , nn , imhere
99007             FORMAT (A23,'. BAD DATA ENCOUNTERED WHILE READING INPUT TAPE ',/,A80,/,' FORTRAN UNIT',I4,',  DATA BLOCK',I4,/5X, &
                         &'IMHERE =',I5)
                  Nogo = 1
!
 15            ENDDO
!
 20            IF ( Tape<=-2 ) REWIND Unitx
               CALL page2(-8)
!WKBR WRITE  (NOUT,820) UIM,FM,INP(UNITX-10)
               WRITE (Nout,99008) Uim , fm , Dsnames(Unitx)
!WKBR2       /A80/,' TO NASTRAN GINO FILES')
99008          FORMAT (A29,' FROM INPUTT4 MODULE. THE FOLLOWING FILES WERE ','SUCCESSFULLY RECOVERED FROM USER ',/5X,A11,           &
                      &' INPUT TAPE ',/,A44,' TO NASTRAN GINO FILES')
               DO j = 1 , 5
                  IF ( iname(1,j)/=blnk ) WRITE (Nout,99009) iname(1,j) , iname(2,j) , oname(1,j) , oname(2,j) , typ(j) , t(1,j) ,  &
                     & t(2,j)
99009             FORMAT (5X,2A4,' ==COPIED TO== ',2A4,4X,'MATRIX TYPE = ',A4,',  SIZE (',I6,2H X,I6,1H))
               ENDDO
               GOTO 99999
            ENDIF
         ENDIF
      ENDIF
   ENDIF
!WKBD FILE   = INP(UNITX-10)
!WKBD TAPEUP = TAPBIT(FILE)
!WKBD IF (TAPEUP) GO TO 50
!WKBD WRITE  (NOUT,20) UFM,FILE,UNITX
!WKBD 20 FORMAT (A23,'. ',A4,' (TAPE UNIT',I4,') NOT ASSIGNED')
!WKBD GO TO 990
!
   WRITE (Nout,99010) Ufm , Unitx
99010 FORMAT (A23,', TAPE UNIT',I4,' SPEC. ERROR')
   GOTO 500
!
!     ERRORS
!
 100  WRITE (Nout,99011) Ufm , Dsnames(Unitx) , bo , ncol , nrow , nform , ntype , name , Bcdopt
99011 FORMAT (A23,'. PARAMETER P3 ERROR. FORTRAN INPUT TAPE ',A4,' WAS',' WRITTEN IN BINARY RECORDS, NOT ASCII.',/5X,'BO =',L2,2X,  &
             &'NCOL,NROW,NFORM,NTYPE,NAME =',4I8,1X,2A4,'   BCDOPT =',I3)
   GOTO 500
 200  WRITE (Nout,99012) Ufm , Unitx
99012 FORMAT (A23,'. INPUTT4 MODULE CANNOT OPEN FORTRAN INPUT TAPE',I4)
   GOTO 500
 300  WRITE (Nout,99013) Ufm , Dsnames(Unitx) , Unitx , nn , imhere
99013 FORMAT (A23,' 3001, EOF ENCOUNTERED WHILE READING INPUT TAPE ',/,A80,/,' FORTRAN UNIT',I4,',  DATA BLOCK',I4,/5X,'IMHERE =',  &
            & I4)
   IF ( imhere==210 .OR. imhere==220 ) WRITE (Nout,99026)
   GOTO 500
 400  WRITE (Nout,99014) Ufm , Dsnames(Unitx) , Unitx , nn , imhere
99014 FORMAT (A23,'. BAD DATA IN HEADER RECORD ON INPUT TAPE ',/,A80,/,' FORTRAN UNIT',I4,',  DATA BLOCK',I4,/5X,'IMHERE =',I5)
   IF ( imhere==105 .OR. imhere==120 ) WRITE (Nout,99026)
   IF ( imhere<0 ) WRITE (Nout,99015)
99015 FORMAT (1H+,22X,'POSSIBLY ERROR IN CONTRL RECORD 3 WORDS')
!
 500  Nogo = 1
99016 FORMAT (1X,4I13,5X,2A4)
99017 FORMAT (4I8,2A4)
99018 FORMAT (A1)
99019 FORMAT (3I8)
99020 FORMAT (1X,3I13)
99021 FORMAT (1X,3I16)
99022 FORMAT (5E16.9)
99023 FORMAT (1X,10E13.6)
99024 FORMAT (1X,8E16.9)
99025 FORMAT (1X,8D16.9)
99026 FORMAT (1H+,22X,'POSSIBLY TAPE UNIT NOT CORRECTLY ASSIGNED')
!
!WKBR 1000 CLOSE (UNIT=UNITX)
99999 RETURN
END SUBROUTINE input4