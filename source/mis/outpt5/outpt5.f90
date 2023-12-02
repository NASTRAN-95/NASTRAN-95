!*==outpt5.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE outpt5
!
!     DRIVER OF OUTPUT5 MODULE
!     COPIES UP TO 5 GINO DATA BLOCKS TO TAPE, BY FORTRAN WRITE,
!     FORMATTED (ASCII), OR UNFORMATTED (BINARY)
!
!     THIS MODULE HAS BEEN EXPANDED TO INCLUDE TABLE DATA BLOCKS.
!     ORIGINALLY IT HANDLES ONLY MATRIX DATA BLOCKS.  G.CHAN/MAY 88
!
!     ==== TABLE  ====
!     OUTPT5 CALLS TABLE5 TO PROCESS TABLE DATA BLOCKS
!      . UNFORMATTED (BINARY) OR FORMATTED (UNDER P4 CONTROL)
!      . IF BINARY, EACH RECORD IS WRITTEN OUT BY -
!           WRITE (OUT) L,(Z(J),J=1,L)
!      . IF FORMATTED,  5 BYTES ARE USED FOR BCD WORD,
!                      10 BYTES FOR INTEGER,
!                      15 BYTES FOR REAL, S.P. OR D.P.
!      . A HEADER RECORD, WHICH CONFORMS TO OUTPT5 HEADER STANDARD,
!           IS WRITTEN OUT FIRST, PRECEEDING THE TABLE DATA RECORDS.
!
!     ==== MATRIX ====
!     COPY GINO MATRIX DATA BLOCK(S) ONTO FORTRAN UNIT IN
!      . UNPACKED BANDED RECORD
!      . BANDED COLUMN  RECORD (FIRST TO LAST NON-ZERO ELEMENTS),
!      . UNFORMATTED (BINARY) OR FORMATTED
!      . SINGLE PRECISION OR DOUBLE, REAL OR COMPLEX DATA
!      . OUTPUT FORTRAN TAPE INPI (I=T,1,2,..,9) FOR UNIVAC, IBM, VAX
!                    OR TAPE UTI  (I=  1,2,..,5) FOR CDC
!        (DEFAULT=INP1, UNIT 15, OR UT1, UNIT 11)
!
!     THIS MODULE HANDLES ONLY MATRIX DATA BLOCKS, NOT TRUE ANY MORE
!
!     UNFORMATTED RECORDS CAN ONLY BE USED BY THE SAME COMPUTER SYSTEM,
!     WHILE FORMATTED RECORDS CAN BE USED ACROSS COMPUTER BOUNDARY
!     (E.G. WRITTEN BY CDC MACHINE AND READ BY IBM) AND ALSO, CAN BE
!     EDITED BY SYSTEM EDITOR, OR PRINTED OUT BY SYSTEM PRINT COMMAND.
!
!     CALL TO THIS MODULE IS
!
!     OUTPUT5  IN1,IN2,IN3,IN4,IN5//C,N,P1/C,N,P2/C,N,P3/C,N,P4
!                                  /C,N,T1/C,N,T2/C,N,T3/C,N,T4... $
!
!              P1=+N, SKIP FORWARD N MATRIX DATA BLOCKS OR TABLES BEFORE
!                     WRITE. (EXCEPT THE FIRST HEADER RECORD. EACH
!                     MATRIX DATA BLOCK OR TABLE, PRECEEDED BY A HEADER
!                     RECORD, IS A COMPLETE MATRIX OR TABLE, MADE UP OF
!                     MANY PHYSICAL RECORDS.
!                     SKIP TO THE END OF TAPE IF P1 EXCEEDS THE
!                     NO. OF DATA BLOCKS AVAILABLE ON THE OUTPUT FILE)
!              P1= 0, NO ACTION TAKEN BEFORE WRITE. (DEFAULT)
!              P1=-1, FORTRAN TAPE IS REWOUND, A TAPE HEADER RECORD IS
!                     WRITTEN TO TAPE. DATA IN FIRST GINO DATA BLOCK IS
!                     COPIED TO TAPE, FOLLOWED BY 4 MORE GINO DATA
!                     BLOCKS IF THEY ARE PRESENT.
!                     AT END, NO EOF WRITTEN, AND TAPE NOT REWOUND
!              P1=-3, THE NAMES OF ALL DATA BLOCKS ON FORTRAN TAPE
!                     ARE PRINTED AND WRITE OCCURS AT THE END OF TAPE
!              P1=-9, WRITE AN INTERNAL END-OF-FILE RECORD, FOLLOWED BY
!                     A SYSTEM ENDFILE MARK, AND REWIND FORTRAN TAPE
!              P2  IS THE FORTRAN UNIT NO. ON WHICH THE DATA BLOCKS WILL
!                     BE WRITTEN.  DEFAULT IS 15 (INP1 FOR UNIVAC, IBM,
!                     VAX), OR UNIT 11 (UT1 FOR CDC)
!
!              P3  IS TAPE ID IF GIVEN BY USER. DEFAULT IS XXXXXXXX
!
!              P4= 0, OUTPUT FILE IS FORTRAN WRITTEN, UNFORMATTED
!              P4= 1, OUTPUT FILE IS FORTRAN WRITTEN, FORMATTED
!                     (BCD IN 2A4, INTEGER IN I8, REAL IN 10E13.6 AND
!                      D.P. IN 5D26.17)
!              P4= 2, SAME AS P4=1, EXECPT 5E26.17 IS USED FOR S.P. REAL
!                     DATA. P4=2 IS USED ONLY IN MACHINES WITH LONG WORD
!                     FOR ACCURACY (60 OR MORE BITS PER WORD)
!
!              TI     10 WORD ARRAY USED ONLY BY TABLE BLOCK DATA.
!                     TO OVERRIDE AUTOMATIC FORMAT TYPE SETTING.
!
!     OUTPT5 LOGIC -
!                                                       (P4=0)   (P4=1)
!     RECORD  WORD        CONTENTS                      BINARY   FORMAT
!     ------  ----  --------------------------------   -------  -------
!        0            TAPE HEADER RECORD -
!              1,2    TAPEID                             2*BCD      2A4
!              3,4    MACHINE (2ND WORD BLANK)           2*BCD      2A4
!              5-7    DATE                               3*INT      3I8
!               8     SYSTEM BUFFSIZE                      INT       I8
!               9     P4 (0,1, OR 2)                       INT       I8
!      1A,1B%         FIRST MATRIX HEADER RECORD -
!               1     ZERO                                 INT       I8
!              2,3    ONE,ONE                            2*INT      2I8
!               4     D.P. ZERO                           F.P.   D26.17
!              5-10   MATRIX TRAILER                     6*INT      6I8
!                     (COL,ROW,FORM,TYPE,MAX,DENSITY)
!             11,12   DMAP NAME OF FIRST INPUT MATRIX    2*BCD      2A4
!      2A,2B    1     1 (FIRST MATRIX COLUMN ID)           INT       I8
!               2     COLUMN LOC. OF FIRST NON-ZERO ELEM.  INT       I8
!               3     COLUMN LOC. OF LAST  NON-ZERO ELEM.  INT       I8
!              1-W    FIRST BANDED COLUMN DATA            F.P.     (**)
!                     (W=WORD3-WORD2)
!      3A,3B    1     2 (SECOND MATRIX COLUMN ID)          INT       I8
!              2-3    FIRST AND LAST NON-ZERO ELEM LOC.  2*INT      2I8
!              1-W    SECOND BANDED COLUMN DATA           F.P.     (**)
!      4A,4B   1-3    THIRD  MATRIX COLUMN, SAME FORMAT  3*INT      3I8
!              1-W    AS RECORD 1                         F.P.     (**)
!        :      :       :
!      ZA,ZB    1     (A NULL COLUMN ID)                   INT       I8
!              2,3    1,1                                2*INT      2I8
!               1     0.0                                 F.P.     (**)
!        :      :       :
!      MA,MB   1-3    LAST MATRIX COLUMN, SAME AS REC #2 3*INT      3I8
!              1-W    LAST BANDED COLUMN DATA             F.P.     (**)
!
!      SA,SB    :     SECOND MATRIX HEADER RECORD   3*INT+F.P. 3I8+D26.
!                                                       +2*BCD   +2*BCD
!                                                       +6*INT     +6I8
!    S+1A,S+1B 1-W    FIRST THRU LAST COLS OF 2ND MATRIX
!        :      :     REPEAT FOR MORE MATRICES
!        :      :     (UP TO 5 MATRIX DATA BLOCKS PER ONE OUTPUT FILE)
!
!    EOFA,EOFB  1     -1                                   INT       I8
!              2,3    1,1                                2*INT      2I8
!               1     D.P. ZERO                           F.P.   D26.17
!
!                                                               - NOTE -
!                                                  BCD AND INTEGERS IN 8
!                                         SINGLE PRECISION REAL IN  13.6
!                                         DOUBLE PRECISION DATA IN 26.17
!                                         S.P. LOGN WORD MACHINE   26.17
!
!     WHERE   %  RECORDS A AND B ARE 2 (OR MORE) RECORDS ON FORMATTED
!                OUTPUT FILE, WHILE
!                A & B ARE 1 CONTINUOUS RECORD IN UNFORMATTED TAPE
!           (**) IS (10E13.6) FOR S.P.REAL, OR (5D26.17) FOR D.P. DATA.
!                OR (5E26.17) FOR S.P. AND D.P. DATA (P4=2 ONLY)
!     NOTE -
!     NO SYSTEM END-OF-FILE MARK WRITTEN BETWEEN MATRICES.
!
!     TO READ BINARY TAPE              TO READ FORMATTED TAPE
!     ----------------------------     --------------------------------
!                LOGICAL SP,DP
!                INTEGER COL,ROW,FORM,TYPE,DENS,FILE(2),IZ(M,N)
!    *                   TAPEID(2),MAC(2),DATE(3),BUFSZ,P4
!                DOUBLE PRECISION DZ(M/2,N/2),DTEMP
!                COMMON  /ZZZZZZ/ Z(M,N)
!                EQUIVALENCE      (Z,IZ,DZ)
!                DATA     SP,DP / .TRUE.,.FALSE./
!     READ (TAPE,ERR=7)                READ (TAPE,10,ERR=7)
!    *           TAPEID,MAC,DATE,BUFSZ,P4
!   1            K = 0
!   2            K = K + 1
!     READ (TAPE,ERR=7,END=3) I,JB,    IF (SP) READ (TAPE,8,ERR=7,END=3)
!    *                        JE               I,JB,JE,( Z(J,K),J=JB,JE)
!                                      IF (DP) READ (TAPE,9,ERR=7,END=3)
!    *                                         I,JB,JE,(DZ(J,K),J=JB,JE)
!                IF (I)   3,            4,     6
!C                      EOF,MATRIX-HEADER,COLUMN
!   3            CONTINUE
!C               (EOF ENCOUNTERED, COMPLETE TAPE READ)
!                CALL EXIT
!   4            BACKSPACE TAPE
!                                      BACKSPACE TAPE
!C               (MATRIX-HEADER READ)
!     READ (TAPE) J,J,J,               READ (TAPE,11) J,J,J
!    *           DTEMP,COL,ROW,FORM,TYPE,MAX,DENS,FILE
!                DP = .FALSE.
!                IF (TYPE.EQ.2 .OR. TYPE.EQ.4) DP=.TRUE.
!                SP = .NOT.DP
!                JTYP = TYPE
!                IF (TYPE .EQ. 3) JTYP = 2
!                IF (COL*JTYP.GT.M .OR. ROW*JTYP.GT.N) STOP 'Z DIM ERR'
!                J = COL*ROW*JTYP
!                DO 5 I = 1,J
!   5            Z(I,1) = 0.0
!                GO TO 1
!   6            CONTINUE
!C               (A COLUMN OF MATRIX READ)
!                IF (I .NE. K) STOP 'COLUMN COUNTER MISSMATCH'
!                GO TO 2
!   7            STOP 'READ ERROR. CHECK TAPE FORMAT TYPE'
!                                     8 FORMAT (3I8,/,(10E13.6))
!                                     9 FORMAT (3I8,/,(5D26.17))
!                                    10 FORMAT (4A4,5I8)
!                                    11 FORMAT (3I8,/,D26.17,6I8,2A4)
!C             FOR LONG WORD MACHINE  8 FORMAT (3I8,/,(5E26.17))
!
!     SEE SUBROUTINE INPTT5 FOR MORE COMPREHENSIVE DETAILS IN RECOVERING
!     MATRIX DATA FROM THE TAPE GENERATED IN THIS OUTPT5 ROUTINE.
!     OR SUBROUTINE TABLE-V FOR TABLE DATA BLOCK RECOVERY.
!
!     WRITTEN BY G.CHAN/UNISYS   1987
!
   IMPLICIT NONE
   USE c_blank
   USE c_dsname
   USE c_machin
   USE c_system
   USE c_unpakx
   USE c_xmssg
   USE c_zzzzzz
!
! Local variable declarations rewritten by SPAG
!
   CHARACTER(8) :: bf
   CHARACTER(8) , SAVE :: binary , formtd
   INTEGER , SAVE :: blank , izero , mone , mtrx , one , tble
   INTEGER :: buf1 , col , err , i , input , j , jb , je , k , l , lfn , mx , nc , nwds , out , row , table , type , wrt
   LOGICAL :: complx , p40 , p40d , p40s , p41 , p41c , p41d , p41s
   INTEGER , DIMENSION(3) :: dt
   REAL*8 :: dx
   REAL*8 , DIMENSION(1) :: dz
   REAL*8 , SAVE :: dzero
   INTEGER , DIMENSION(3,10) , SAVE :: fn
   INTEGER , DIMENSION(2) :: name , tapeid
   INTEGER , DIMENSION(2) , SAVE :: none , subnam
   REAL , DIMENSION(1) :: rz
   INTEGER , DIMENSION(9) :: trl
   REAL :: x
   REAL , SAVE :: zero
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!WKBNB
!WKBNE
   !>>>>EQUIVALENCE (Rz(1),Dz(1),Iz(1))
   DATA binary , formtd , subnam/'BINARY  ' , 'FORMATTD' , 4HOUTP , 2HT5/
   DATA zero , dzero , izero , one , mone , fn/0.0 , 0.0D0 , 0 , 1 , -1 , 30*4H    /
   DATA mtrx , tble , blank/4HMTRX , 4HTBLE , 4H    /
   DATA none/4H (NO , 4HNE) /
!
!     IF MACHINE IS CDC OR UNIVAC, CALL CDCOPN OR UNVOPN TO OPEN OUTPUT
!     FILE, A FORMATTED SEQUENTIAL TAPE.  NO CONTROL WORDS ARE TO BE
!     ADDED TO EACH FORMATTED RECORD. RECORD LENGTH IS 132 CHARACTERS,
!     AN ANSI STANDARD.
!
!WKBD IF (MACH .EQ. 3) CALL UNVOPN (P2)
!WKBD IF (MACH .EQ. 4) CALL CDCOPN (P2)
   bf = binary
   IF ( p4>=1 ) bf = formtd
   CALL page
   CALL page2(1)
   WRITE (nout,99001) uim , bf , p1
99001 FORMAT (A29,', MODULE OUTPUT5 CALLED BY USER DMAP ALTER, ON ',A8,' TAPE,',/5X,'WITH FOLLOWING REQUEST  (P1=',I2,1H))
   IF ( p1==-9 ) WRITE (nout,99002)
99002 FORMAT (5X,'WRITE AN INTERNAL E-O-F RECORD, FOLLOWED BY A SYSTEM',' E-O-F MARK, AND REWIND OUTPUT TAPE')
   IF ( p1==-3 ) WRITE (nout,99003)
99003 FORMAT (5X,'REWIND TAPE, PRINT DATA BLOCK NAMES AND THEN WRITE ','AFTER THE LAST DATA BLOCK ON TAPE')
   IF ( p1==-1 ) WRITE (nout,99004)
99004 FORMAT (5X,'REWIND, WRITE A TAPE HEADER RECORD, THEN FOLLOWED BY ','DATA BLOCKS WRITING.',/5X,'AT END, NO EOF AND NO REWIND')
   IF ( p1==0 ) WRITE (nout,99005)
99005 FORMAT (5X,'DATA BLOCKS ARE WRITTEN STARTING AT CURRENT TAPE ','POSITION. AT END, NO EOF AND NO REWIND')
   IF ( p1>0 ) WRITE (nout,99006) p1
99006 FORMAT (5X,'SKIP FORWARD',I4,' DATA BLOCKS BEFORE WRITING (TAPE ','HEADER RECORD NOT COUNTED AS A DATA BLOCK).',/5X,          &
             &'NO REWIND BEFORE SKIPPING. AT END, NO EOF AND NO REWIND')
!
   buf1 = korsz(rz(1)) - ibuf - 1
   IF ( buf1<=0 ) CALL mesage(-8,0,subnam)
   out = p2
   wrt = 0
   lfn = -1
   IF ( p1==-3 ) lfn = 0
!
!     SET P4 FLAGS
!
!     SET P40  TO .TRUE. IF USER SPECIFIES P4 TO ZERO (BINARY)
!     SET P41  TO .TRUE. IF USER SPECIFIES P4 TO ONE  (FORMATTED)
!     SET P40D TO .TRUE. IF P40 IS TRUE AND DATA IS IN D.P.
!     SET P40S TO .TRUE. IF P40 IS TRUE AND DATA IS IN S.P.
!     SET P41D TO .TRUE. IF P41 IS TRUE AND DATA IS IN D.P.
!     SET P41S TO .TRUE. IF P41 IS TRUE AND DATA IS IN S.P.
!     SET P41C TO .TRUE. IF P4=2, AND RESET P41S AND P41D TO .FALSE.
!
   p40d = .FALSE.
   p41s = .FALSE.
   p41d = .FALSE.
   p41c = p4==2 .AND. nbpw>=60
   p41 = .FALSE.
   IF ( p4>=1 ) p41 = .TRUE.
!WKBNB
   CLOSE (UNIT=out)
   IF ( p4/=0 ) THEN
      OPEN (UNIT=out,FILE=dsnames(out),STATUS='UNKNOWN')
   ELSE
      OPEN (UNIT=out,FILE=dsnames(out),FORM='UNFORMATTED',STATUS='UNKNOWN')
   ENDIF
!WKBNE
   IF ( .NOT.(p41c) ) THEN
      p41s = p41
      IF ( p41 ) p41d = .NOT.p41s
   ENDIF
   p40 = .NOT.p41
   p40s = p40
   IF ( p40 ) p40d = .NOT.p40s
   IF ( p1==-9 ) THEN
!
!     FINAL CALL TO OUTPUT5
!
      IF ( p40 ) WRITE (out) mone , one , one , dzero
      IF ( p41 ) WRITE (out,99030) mone , one , one , dzero
      ENDFILE out
      REWIND out
      RETURN
!
   ELSEIF ( p1==-3 ) THEN
!
!     OLD TAPE. CHECK TAPE ID
!
      REWIND out
      GOTO 200
   ELSEIF ( p1==-1 ) THEN
!
!     NEW TAPE (P1=-1)
!
!     WRITE A TAPE IDENTIFICATION RECORD (NOTE -THIS IS THE ONLY TIME
!     A TAPE HEADER RECORD IS WRITTEN)
!
      IF ( p1==-1 ) THEN
         REWIND out
         trl(1) = p3(1)
         trl(2) = p3(2)
         trl(3) = mchnam
         trl(4) = blank
         trl(5) = date(1)
         trl(6) = date(2)
         trl(7) = date(3)
         IF ( p40 ) WRITE (out) (trl(j),j=1,7) , ibuf , p4
         IF ( p41 ) WRITE (out,99025) (trl(j),j=1,7) , ibuf , p4
         lfn = 0
      ENDIF
      GOTO 700
   ELSEIF ( p1<0 ) THEN
!
      WRITE (nout,99007) ufm , p1
99007 FORMAT (A23,' 4120, MODULE OUTPUT5 - ILLEGAL VALUE FOR FIRST ','PARAMETER = ',I8)
      err = -37
   ELSEIF ( p1==0 ) THEN
      lfn = 0
      GOTO 700
   ELSE
      GOTO 200
   ENDIF
 100  CALL mesage(err,input,subnam)
   RETURN
 200  IF ( p40 ) READ (out,END=500) tapeid , name , dt , i , k
   IF ( p41 ) READ (out,99025,END=500) tapeid , name , dt , i , k
   IF ( tapeid(1)==p3(1) .AND. tapeid(2)==p3(2) ) THEN
      CALL page2(6)
      WRITE (nout,99008) tapeid , name , dt , i
99008 FORMAT (/5X,'MODULE OUTPUT5 IS PROCESSING TAPE ',2A4,/5X,'WRITTEN BY ',2A4,/5X,'ON ',I2,1H/,I2,1H/,I2,/5X,'BUFFSIZE USED =',  &
            & I7,/)
      IF ( k==0 ) WRITE (nout,99026) binary
      IF ( k>=1 ) WRITE (nout,99026) formtd
      IF ( k/=p4 ) THEN
         WRITE (nout,99009) ufm , p4
99009    FORMAT (A23,', THE 4TH PARAMETER TO OUTPUT5 DOES NOT AGREE WITH ','ORIG. TAPE FORMAT    P4=',I5,/)
         CALL mesage(-37,0,subnam)
      ENDIF
!
!     TO SKIP P1 MATRIX DATA BLOCKS OR TABLES ON THE OLD OUTPUT FILE
!     OR TO TABULATE TAPE CONTENTS IF P1 = -3
!
      lfn = 0
   ELSE
      WRITE (nout,99010) tapeid , p3
99010 FORMAT ('0*** WRONG TAPE MOUNTED - TAPEID =',2A4,', NOT ',2A4)
      err = -37
      GOTO 100
   ENDIF
 300  DO
      IF ( p40 ) READ (out,ERR=600,END=500) nc , jb , je
      IF ( p41s ) READ (out,99028,ERR=300,END=500) nc , jb , je , (x,j=jb,je)
      IF ( p41c ) READ (out,99029,ERR=300,END=500) nc , jb , je , (x,j=jb,je)
      IF ( p41d ) READ (out,99030,ERR=300,END=500) nc , jb , je , (dx,j=jb,je)
      IF ( nc<0 ) THEN
         IF ( p41 ) BACKSPACE out
         EXIT
      ELSEIF ( nc==0 ) THEN
         DO WHILE ( p1==-3 .OR. lfn<p1 )
            lfn = lfn + 1
            BACKSPACE out
            IF ( p41 ) BACKSPACE out
            IF ( p40 ) READ (out) i , i , i , dx , j , j , j , j , k , k , fn(1,lfn) , fn(2,lfn)
            IF ( p41 ) READ (out,99027) i , i , i , dx , j , j , j , j , k , k , fn(1,lfn) , fn(2,lfn)
            IF ( k>0 .AND. j>=1 .AND. j<=4 ) THEN
               fn(3,lfn) = mtrx
               IF ( .NOT.(p40) ) THEN
                  p41s = .FALSE.
                  p41d = .FALSE.
                  p41c = p4==2 .AND. nbpw>=60
                  IF ( .NOT.(p41c) ) THEN
                     IF ( j==1 .OR. j==3 ) p41s = .TRUE.
                     p41d = .NOT.p41s
                  ENDIF
               ENDIF
               GOTO 400
            ELSE
               fn(3,lfn) = tble
               DO
                  IF ( p40 ) READ (out,ERR=600,END=500) l
                  IF ( p41 ) READ (out,99011,ERR=300,END=500) l , (table,j=1,l)
99011             FORMAT (I10,24A5,/,(26A5))
                  IF ( l<0 ) THEN
                     IF ( p41 ) BACKSPACE out
                     GOTO 500
                  ELSEIF ( l==0 ) THEN
                     EXIT
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
         IF ( p41 ) BACKSPACE out
         EXIT
      ENDIF
 400  ENDDO
 500  BACKSPACE out
   IF ( p1/=-3 .OR. lfn<=0 ) GOTO 700
   GOTO 900
!
 600  WRITE (nout,99012) uwm , tapeid
99012 FORMAT (A25,' FROM OUTPUT5 MODULE. ERROR WHILE READING ',2A4)
   err = -37
   GOTO 100
!
!     COPY MATRICES OR TABLES OUT TO TAPE
!
 700  DO mx = 1 , 5
      input = mx + 100
      CALL fname(input,name)
      IF ( name(1)/=none(1) .OR. name(2)/=none(2) ) THEN
         trl(1) = input
         CALL rdtrl(trl)
         IF ( trl(1)>0 ) THEN
            IF ( trl(1)>0 ) THEN
               IF ( trl(4)>8 .OR. trl(5)>4 .OR. trl(6)<=0 .OR. trl(7)<=0 ) CALL table5(*800,input,out,trl,buf1,wrt,lfn,fn)
               col = trl(2)
               row = trl(3)
               type = trl(5)
               complx = .FALSE.
               IF ( type>=3 ) complx = .TRUE.
!
!     CHECK FOR NULL MATRIX
!
               IF ( row==0 .OR. col==0 .OR. type==0 ) THEN
!
!     NULL MATRIX, OR GINO DATA BLOCK IS NOT A MTRIX FILE
!
                  CALL page2(5)
                  WRITE (nout,99013) uwm , name
99013             FORMAT (A25,' FROM OUTPUT5 MODULE. ',2A4,' IS EITHER A NULL ','MATRIX OR NOT A MATRIX DATA BLOCK',/5X,            &
                         &'NO DATA WERE COPIED TO OUTPUT FILE',/)
               ELSE
!
!     SET FLAGS FOR FORMATTED OR UNFORMATTED WRITE, SINGLE OR DOUBLE
!     PRECISION DATA, THEN WRITE THE MATRIX HEADER WITH PROPER FORMAT.
!     MATRIX HEADER CONSISTS OF ONE SCRATCH WORD, ORIGINAL MATRIX
!     TRAILER, AND MATRIX DMAP NAME
!
                  p40s = .FALSE.
                  p40d = .FALSE.
                  p41s = .FALSE.
                  p41d = .FALSE.
                  p41c = p4==2 .AND. nbpw>=60
                  IF ( .NOT.(p41) ) THEN
                     IF ( type==1 .OR. type==3 ) p40s = .TRUE.
                     p40d = .NOT.p40s
                  ELSEIF ( .NOT.(p41c) ) THEN
                     IF ( type==1 .OR. type==3 ) p41s = .TRUE.
                     p41d = .NOT.p41s
                  ENDIF
                  IF ( p40 ) WRITE (out) izero , one , one , dzero , (trl(k),k=2,7) , name
                  IF ( p41 ) WRITE (out,99027) izero , one , one , dzero , (trl(k),k=2,7) , name
                  wrt = 1
!
!     OPEN INPUT DATA BLOCK AND SAVE DMAP NAME IN FN ARRAY
!
                  err = -1
                  CALL open(*100,input,rz(buf1),0)
                  CALL fwdrec(*100,input)
                  IF ( lfn/=-1 .AND. lfn<10 ) THEN
                     lfn = lfn + 1
                     fn(1,lfn) = name(1)
                     fn(2,lfn) = name(2)
                     fn(3,lfn) = mtrx
                  ENDIF
!
!     UNPACK A MATRIX COLUMN, AND WRITE TO OUTPUT FILE THE BANDED DATA
!     (FROM FIRST TO LAST NON-ZERO ELEMENTS)
!
                  ityp = type
                  incr = 1
                  DO nc = 1 , col
                     ii = 0
                     jj = 0
                     CALL unpack(*702,input,rz)
                     jb = ii
                     je = jj
                     nwds = jj - ii + 1
                     IF ( complx ) THEN
                        nwds = nwds + nwds
                        je = nwds + jb - 1
                     ENDIF
                     IF ( nwds>buf1 ) CALL mesage(-8,0,subnam)
                     IF ( p40s ) WRITE (out) nc , jb , je , (rz(j),j=1,nwds)
                     IF ( p40d ) WRITE (out) nc , jb , je , (dz(j),j=1,nwds)
                     IF ( p41s ) WRITE (out,99028,ERR=1100) nc , jb , je , (rz(j),j=1,nwds)
                     IF ( p41c ) WRITE (out,99029,ERR=1100) nc , jb , je , (rz(j),j=1,nwds)
                     IF ( p41d ) WRITE (out,99030,ERR=1100) nc , jb , je , (dz(j),j=1,nwds)
                     CYCLE
!
!     A NULL COLUMN
!
 702                 je = 1
                     IF ( complx ) je = 2
                     IF ( p40s ) WRITE (out) nc , one , je , (zero,i=1,je)
                     IF ( p40d ) WRITE (out) nc , one , je , (dzero,i=1,je)
                     IF ( p41s ) WRITE (out,99028) nc , one , je , (zero,i=1,je)
                     IF ( p41c ) WRITE (out,99029) nc , one , je , (zero,i=1,je)
                     IF ( p41d ) WRITE (out,99030) nc , one , je , (dzero,i=1,je)
                  ENDDO
!
!     CLOSE INPUT DATA BLOCK WITH REWIND.
!
                  CALL close(input,1)
                  CALL page2(10)
                  WRITE (nout,99014) name , out , (trl(j),j=2,5) , ibuf
99014             FORMAT (/5X,'MODULE OUTPUT5 UNPACKED MATRIX DATA BLOCK ',2A4,' AND WROTE IT OUT TO',/5X,'FORTRAN UNIT',I4,        &
                         &', IN BANDED DATA FORM (FIRST TO LAST NON-ZERO ELEMENTS)',/9X,'NO. OF COLS =',I8,/9X,'NO. OF ROWS =',I8,  &
                        & /16X,'FORM =',I8,/16X,'TYPE =',I8,/5X,'SYSTEM BUFFSIZE =',I8)
                  IF ( p40 ) WRITE (nout,99015)
99015             FORMAT (5X,'IN FORTRAN BINARY RECORDS')
                  IF ( p41s ) WRITE (nout,99016)
99016             FORMAT (5X,'IN FORTRAN FORMATTED RECORDS - (3I8,/,(10E13.6))')
                  IF ( p41c ) WRITE (nout,99017)
99017             FORMAT (5X,'IN FORTRAN FORMATTED RECORDS - (3I8,/,(5E26.17))')
                  IF ( p41d ) WRITE (nout,99018)
99018             FORMAT (5X,'IN FORTRAN FORMATTED RECORDS - (3I8,/,(5D26.17))')
               ENDIF
            ELSE
               CALL page2(3)
               WRITE (nout,99019) input , name
99019          FORMAT (/5X,'INPUT FILE ',2A4,'(',I3,') IS PURGED. NO DATA ','TRANSFERRED TO OUTPUT FILE')
            ENDIF
            CYCLE
         ENDIF
      ENDIF
!
      trl(1) = input + 1
      CALL rdtrl(trl)
      IF ( trl(1)>0 ) WRITE (nout,99020) uwm , input , name
99020 FORMAT (A25,' FROM OUTPUT5 MODULE. INPUT DATA BLOCK',I5,2H, ,2A4,' IS EITHER PURGED OR DOES NOT EXIST')
!
 800  ENDDO
!
   IF ( wrt==0 ) WRITE (nout,99021) uwm
99021 FORMAT (A25,' FROM OUTPUT5 MODULE. NO DATA BLOCK WRITTEN TO ','OUTPUT FILE')
   ENDFILE out
   BACKSPACE out
   IF ( p1==-3 ) GOTO 1000
!
!     PRINT LIST OF DATA BLOCKS ON FORTRAN TAPE (P1=-3).
!
   IF ( lfn<=0 ) RETURN
 900  CALL page2(lfn+10)
   WRITE (nout,99022) out , mchnam , bf , (j,fn(1,j),fn(2,j),fn(3,j),j=1,lfn)
99022 FORMAT (/5X,'SUMMARY FROM OUTPUT5 MODULE',//16X,'DATA BLOCKS ','WRITTEN TO FORTRAN UNIT',I4,/17X,'(BY ',A4,' MACHINE, ',A8,   &
             &' RECORDS)',///22X,'FILE',8X,'NAME',8X,'TYPE'/17X,9(4H----),/,(22X,I3,9X,2A4,4X,A4))
   IF ( p1==-3 ) GOTO 700
   IF ( .NOT.(p40) ) THEN
      CALL page2(2)
      WRITE (nout,99023)
99023 FORMAT (/5X,'THIS FORMATTED OUTPUT FILE CAN BE VIEWED OR EDITED',' VIA SYSTEM EDITOR',/)
   ENDIF
!
 1000 IF ( mach==3 ) CALL unvcls(p2)
   IF ( mach==4 ) CALL cdccls(p2)
   RETURN
!
!     WRITE ERROR
!
 1100 WRITE (nout,99024) sfm
99024 FORMAT (A25,' IN WRITING OUTPUT FILE',/5X,'IBM USER - CHECK FILE',' ASSIGNMENT FOR DCB PARAMETER OF 132 BYTES')
   CALL mesage(-37,0,subnam)
99025 FORMAT (4A4,5I8)
99026 FORMAT (5X,'ORIGINAL TAPE IS ',A8)
99027 FORMAT (3I8,/,D26.17,6I8,2A4)
99028 FORMAT (3I8,/,(10E13.6))
99029 FORMAT (3I8,/,(5E26.17))
99030 FORMAT (3I8,/,(5D26.17))
END SUBROUTINE outpt5
