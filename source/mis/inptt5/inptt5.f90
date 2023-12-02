!*==inptt5.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE inptt5
!
!     DRIVER OF INPUTT5 MODULE
!     THIS MODULE HANDLES BOTH TABLE AND MATRIX DATA BLOCKS     5/88
!
!     THIS IS A COMPANION MODULE TO OUTPUT5
!
!     ==== TABLE  ====
!     CALLS TABLE-V ROUTINE TO COPY FROM A FORTRAN UNIT (FORMATTED OR
!     BINARY TAPE) TABLE DATA TO NASTRAN GINO TABLE DATA BLOCKS
!
!     ==== MATRIX ====
!     COPIES FROM A FORTRAN UNIT (BINARY OR FORMATTED TAPE) OF BANDED
!     MATRICES ONTO NASTRAN GINO MATRIX DATA BLOCKS, IN GINO PACKED
!     FORMAT
!
!     UNFORMATTED RECORDS CAN ONLY BE USED BY THE SAME COMPUTER SYSTEM,
!     WHILE FORMATTED RECORDS CAN BE USED ACROSS COMPUTER BOUNDARY
!     (E.G. WRITTEN BY CDC MACHINE AND READ BY IBM), AND ASLO, IT CAN
!     BE EDITED BY SYSTEM EDITOR, OR PRINTED OUT BY SYSTEM PRINT COMMAND
!
!     ******************************************************************
!     *                                                                *
!     *                       - IMPORTANT -                            *
!     *                                                                *
!     *  IF USER ASSEMBLES HIS OWN MATRIX IN INPUTT5 FORMAT, AND USES  *
!     *  INPUTT5 MODULE TO READ IT INTO NASTRAN, BE SURE THAT THE      *
!     *  DENSITY TERM (DENS) OF THE MATRIX TRAILER IS SET TO NON-ZERO  *
!     *  (NEED NOT BE EXACT) AND THE PRECISION TERM (TYPE) IS 1,2,3,   *
!     *  OR 4. OTHERWISE, HIS MATRIX WILL BE TREATED AS TABLE AND      *
!     *  EVERYTHING GOES HAYWIRE.                                      *
!     *                                                                *
!     ******************************************************************
!
!     CALL TO THIS MODULE IS
!
!     INPUTT5  /O1,O2,O3,O4,O5/C,N,P1/C,N,P2/C,N,P3/C,N,P4 $
!
!              P1=+N, SKIP FORWARD N MATRIX DATA BLOCKS OR TABLES BEFORE
!                     COPYING (EXCEPT THE FIRST HEADER RECORD. EACH
!                     MATRIX DATA BLOCK OR TABLE, PRECEEDED BY A HEADER
!                     RECORD, IS A COMPLETE MATRIX OR TABLE, MADE UP OF
!                     MANY PHYSICAL RECORDS.
!                     SKIP TO THE END OF TAPE IF P1 EXCEEDS THE NO. OF
!                     DATA BLOCKS AVAILABLE ON THE OUTPUT TAPE
!                     NO REWIND BEFORE SKIPPING)
!              P1= 0, NO ACTION TAKEN BEFORE COPYING. (DEFAULT)
!                     HOWEVER, IF TAPE IS POSITIONED AT THE BEGINNING,
!                     THE TAPE ID RECORD IS SKIPPED FIRST.
!              P1=-1, INPUT TAPE IS REWOUND, AND TAPEID CHECKED. IF
!                     OUTPUT GINO FILES ARE PRESENT, DATA FROM TAPE ARE
!                     THEN COPIED TO GINO FILES - IN PACKED MATRIX FORM
!                     IF MATRIX DATA, OR TABLE FORM IF TABLE DATA.
!              P1=-3, TAPE IS REWOUND AND READ. THE NAMES OF ALL DATA
!                     BLOCKS ON FORTRAN TAPE ARE PRINTED. AT END, TAPE
!                     IS REWOUND AND POSITIONED AFTER TAPE HEADER RECORD
!                     (NOTE - SERVICE UP TO 15 FILE NAMES ON ONE INPUT
!                     TAPE. AND THE 'AT END' TREATMENT IS NOT THE SAME
!                     AS IN OUTPUT5)
!              P1=-4  THRU -8 ARE NOT USED
!              P1=-9, REWIND TAPE
!
!              P2  IS THE FORTRAN UNIT NO. ON WHICH THE DATA BLOCKS WILL
!                     WRITTEN.  DEFAULT IS 16 (INP2 FOR UNIVAC,IBM,VAX),
!                     OR 12 (UT2 FOR CDC)
!
!              P3  IS TAPE ID IF GIVEN BY USER. DEFAULT IS XXXXXXXX
!
!              P4=0, OUTPUT TAPE IS FORTRAN WRITTEN, UNFORMATTED RECORDS
!              P4=1, OUTPUT TAPE IS FORTRAN WRITTED, FORMATTED
!                    BCD IN 2A4, INTEGER IN I8, S.P. REAL IN 10E13.6,
!                    AND D.P. IN 5D26.17.
!              P4=2, SAME AS P4=1, EXECPT FORMAT 5E26.17 IS USED FOR
!                    S.P. REAL DATA. (THIS OPTION IS USED ONLY IN
!                    MACHINES WITH 60 OR MORE BITS PER WORD)
!
!
!     CONTENTS OF INPUT TAPE, AS WRITTEN BY OUTPUT5
!                                                       (P4=0)   (P4=1)
!     RECORD  WORD        CONTENTS                      BINARY   FORMAT
!     ------  ----  --------------------------------   -------  -------
!        0            TAPE HEADER RECORD -
!              1,2    TAPEID                             2*BCD      2A4
!              3,4    MACHINE                            2*BCD      2A4
!              5-7    DATE                               3*INT      3I8
!               8     BUFFSIZE                             INT       I8
!               9     0 (BINARY), OR 1 OR 2 (FORMATTED)    INT       I8
!       1/2@          FIRST MATRIX HEADER RECORD -
!               1     ZERO                                 INT       I8
!              2,3    1,1                                2*INT      2I8
!               1     DUMMY (D.P.)                        F.P.   D26.17
!              2-7    MATRIX TRAILER                     6*INT      6I8
!                     (COL,ROW,FORM,TYPE,MAX,DENS)
!              8-9    MATRIX DMAP NAME                   2*BCD      2A4
!       3/4     1     1 (FIRST COLUMN ID)                  INT       I8
!               2     LOC. OF FIRST NON-ZERO ELEMENT, L1   INT       I8
!               3     LOC. OF LAST  NON-ZERO ELEMENT, L2   INT       I8
!              1-W    FIRST MATRIX COLUMN DATA            F.P.     (**)
!                     (W=L2-L1+1)
!       5/6     1     2 (SECOND COLUMN ID)                 INT       I8
!              2-3    LOC. OF FIRST AND LAST NON-ZERO    2*INT      2I8
!                     ELEMENTS
!              1-W    SECOND MATRIX COLUMN DATA           F.P.     (**)
!       7/8    1-3    THIRD MATRIX COLUMN, SAME FORMAT   3*INT      3I8
!              1-W    AS RECORD 1                         F.P.     (**)
!        :      :       :
!       M/M+1  1-3    LAST MATRIX COLUMN, SAME FORMAT    3*INT      3I8
!                     AS RECORD 1                         F.P.     (**)
!     M+2/M+3  1-3    SECOND MATRIX HEADER RECORD        3*INT      3I8
!               1     DUMMY                               F.P.     (**)
!              2-7    MATRIX TRAILER                     6*INT      6I8
!              8,9    MATRIX DMAP NAME                   2*BCD      2A4
!     M+4-N     :     FIRST THRU LAST COLUMNS OF MATRIX  3*INT      3I8
!                                                        +F.P.    +(**)
!        :      :     REPEAT FOR 3RD,4TH,5TH MATRICES
!        :      :     (UP TO 5 MATRIX DATA BLOCKS PER ONE OUTPUT TAPE)
!
!       EOF    1-3    -1,1,1                              3*INT     3I8
!               1     ZEROS (D.P.)                         F.P.  D26.17
!
!     @  RECORDS 1/2 (3/4, 5/6, ETC) ARE TWO RECORDS IN THE FORMATTED
!        TAPE, AND ARE PHYSICALLY ONE RECORD IN THE BINARY TAPE (AND
!        THE WORD COUNT SHOULD BE ADDED)
!     ** IS (10E13.6) FOR S.P.REAL OR (5D26.17) FOR D.P.DATA
!        (5E26.17) FOR LONG WORD MACHINE
!
!                                                               - NOTE -
!                                                  BCD AND INTEGERS IN 8
!                                                     S.P. REAL IN  13.7
!                                                     D.P. DATA IN 26.17
!                                             LONG WORD MACHINE IN 26.17
!
!     NO SYSTEM END-OF-FILE MARK WRITTEN BETWEEN MATRICES
!     EXCEPT FOR THE TAPE HEADER RECORD, AND THE MATRIX HEADERS, THE
!     ENTIRE FORMATTED INPUT TAPE CAN BE READ BY A STANDARD FORMAT
!     (3I8,/,(10E13.6)), (3I8,/,(5D26.17)), OR (3I8,/,(5E26.17))
!
!     ALSO, USER MAY OR MAY NOT CALL OUTPUT5 WITH P1=-9 TO WRITE AN
!     'OUPUT5 E-O-F' MARK ON TAPE. THIS CAUSED PROBLEM BEFORE.
!
!     THE PROCEDURE TO READ AND/OR WRITE THE TAPE IS COMMONLY USED
!     AMONG INPUTT5, OUTPUT5, AND DUMOD5. ANY PROCEDURE CHANGE SHOULD
!     BE MADE TO ALL THREE SUBROUTINES.
!
!     WRITTEN BY G.CHAN/UNISYS   1987
!     MAJOR REVISED 12/1992 BY G.C.
!
   IMPLICIT NONE
   USE c_blank
   USE c_dsname
   USE c_input5
   USE c_machin
   USE c_packx
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
!
! Local variable declarations rewritten by SPAG
!
   CHARACTER(8) :: bf
   CHARACTER(8) , SAVE :: binary , formtd
   INTEGER , SAVE :: bk , mtrx , skip , tble
   INTEGER :: buf1 , col12 , err , formx , i , imhere , input , j , jb , je , jtyp , k , l , ll , nc , nck , nwds , output , p1n ,  &
            & rowx , tabel
   LOGICAL , SAVE :: debug
   INTEGER , DIMENSION(3) :: dt
   REAL*8 :: dx
   REAL*8 , DIMENSION(7) :: dz
   INTEGER , DIMENSION(3,15) , SAVE :: fn
   INTEGER , DIMENSION(2) :: mac , name , tapeid
   LOGICAL :: opn , p40 , p40d , p40s , p41 , p41c , p41d , p41s
   INTEGER , DIMENSION(2) , SAVE :: subnam
   REAL :: x
   CHARACTER(5) , DIMENSION(1) :: z5
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!WKBI
!WKBNB
!WKBNE
!WKBR EQUIVALENCE      (RZ(1),IZ(1),DZ(1))
   !>>>>EQUIVALENCE (Rz(1),Iz(1),Dz(1),Z5)
   DATA binary , formtd , subnam , fn , bk/'BINARY' , 'FORMATTD' , 4HINPT , 2HT5 , 46*2H  /
   DATA mtrx , tble , skip/4HMTRX , 4HTBLE , 4HSKIP/
   DATA debug/.FALSE./
!
!     IF MACHINE IS CDC OR UNIVAC, CALL CDCOPN OR UNVOPN TO OPEN OUTPUT
!     FILE, A SEQUENTIAL FORMATTED TAPE. NO CONTROL WORDS ARE TO BE
!     ADDED TO EACH SEQUENTIAL RECORD. RECORD LENGTH IS 132 CHARACTERS,
!     AN ANSI STANDARD.
!
   bf = binary
   IF ( p4>=1 ) bf = formtd
   CALL page1
   WRITE (nout,99001) uim , bf , p1
99001 FORMAT (A29,', MODULE INPUTT5 CALLED BY USER DMAP ALTER, ON ',A8,' INPUT FILE,',/5X,'WITH THE FOLLOWING REQUEST.  (P1=',I2,   &
            & 1H))
   IF ( p1==-9 ) WRITE (nout,99002)
99002 FORMAT (5X,'REWIND TAPE ONLY')
   IF ( p1==-3 ) WRITE (nout,99003)
99003 FORMAT (5X,'REWIND AND READ TAPE. PRINT ALL DATA BLOCK NAMES ON ','TAPE. AT END, TAPE IS REWOUND',/5X,'AND POSITIONED ',      &
             &'PASS TAPE HEADER RECORD')
   IF ( p1==-1 ) WRITE (nout,99004)
99004 FORMAT (5X,'REWIND, POSITION PAST TAPE HEADER RECORD, THEN READ ','TAPE. AT END, NO REWIND')
   IF ( p1==0 ) WRITE (nout,99005)
99005 FORMAT (5X,'READ TAPE STARTING AT CURRENT POSITION, OR POSITION ','PAST THE TAPE HEADER RECORD (FIRST USE OF TAPE).',/5X,     &
             &' NO REWIND AT BEGINNING AND AT END')
   IF ( p1>0 ) WRITE (nout,99006) p1
99006 FORMAT (5X,'SKIP FORWARD',I4,' DATA BLOCKS (NOT COUNTING TAPE ','HEADER RECORD) BEFORE READING, AT END NO REWIND')
!
   buf1 = korsz(rz(1)) - ibuf - 1
   IF ( buf1<=0 ) CALL mesage(-8,0,subnam)
   input = p2
   opn = .FALSE.
   ll = 0
   p41 = .FALSE.
   IF ( p4>=1 ) p41 = .TRUE.
   p40 = .NOT.p41
   p40s = .FALSE.
   p41s = .FALSE.
   p40d = p40
   p41d = p41
   p41c = p4==2 .AND. nbpw>=60
   IF ( p41c ) p40d = .FALSE.
   IF ( p41c ) p41d = .FALSE.
   col12 = 0
   p1n = p1
   IF ( p1<0 ) p1n = 0
!WKBNB
   CLOSE (UNIT=input)
   IF ( p4/=0 ) THEN
      OPEN (UNIT=input,FILE=dsnames(input),STATUS='UNKNOWN')
   ELSE
      OPEN (UNIT=input,FILE=dsnames(input),FORM='UNFORMATTED',STATUS='UNKNOWN')
   ENDIF
!WKBNE
   IF ( p1/=-9 ) THEN
!
      DO i = 1 , 15
         fn(3,i) = bk
      ENDDO
      IF ( p1<-1 .AND. p1/=-3 .AND. p1/=-9 ) THEN
!
         WRITE (nout,99007) ufm , p1
99007    FORMAT (A23,', MODULE INPUTT5 - ILLEGAL VALUE FOR FIRST PARAMETER',' = ',I8,/5X,'ONLY -9, -3 AND GREATER THAN -1 ALLOWED')
         err = -37
!
      ELSEIF ( p1==0 ) THEN
!
!     P1 = 0,
!     MUST SKIP TAPE HEADER RECORD IF CURRENT TAPE POSITION IS AT THE
!     VERY BEGINNING
!
         ll = 0
         imhere = 500
         IF ( p40 ) READ (input,ERR=1300,END=600) tapeid
         IF ( p41 ) READ (input,99029,ERR=1300,END=600) tapeid
         IF ( tapeid(1)/=p3(1) .OR. tapeid(2)/=p3(2) ) BACKSPACE input
!
!     COPY MATRIX TO TAPE
!
         imhere = 510
         GOTO 700
      ELSE
!
!     CHECK TAPE ID
!
         REWIND input
         err = -1
         IF ( p40 ) READ (input,END=600) tapeid , mac , dt , i , k
         IF ( p41 ) READ (input,99029,END=600) tapeid , mac , dt , i , k
         IF ( tapeid(1)==p3(1) .AND. tapeid(2)==p3(2) ) GOTO 200
         WRITE (nout,99008) tapeid , p3 , mac , dt
99008    FORMAT ('0*** WRONG TAPE MOUNTED - TAPEID =',2A4,', NOT ',2A4,/5X,'MACHINE=',2A4,' DATE WRITTEN-',I4,1H/,I2,1H/,I2)
         IF ( p1/=-1 ) GOTO 200
         err = -37
      ENDIF
   ELSE
      REWIND input
      GOTO 2000
   ENDIF
 100  CALL mesage(err,output,subnam)
   RETURN
 200  IF ( k/=p4 ) THEN
      WRITE (nout,99009) uwm , p4
99009 FORMAT (A25,', MODULE INPUTT5 4TH PARAMETER SPECIFIED WRONG TAPE',' FORMAT.   P4=',I5,/5X,                                    &
             &'INPUTT5 WILL RESET P4 AND TRY TO READ THE TAPE AGAIN.',/)
      p4 = k
      p40 = .NOT.p40
      p41 = .NOT.p41
   ENDIF
   CALL page2(4)
   WRITE (nout,99010) tapeid , mac , dt , i
99010 FORMAT (/5X,'MODULE INPUTT5 IS NOW PROCESSING TAPE ',2A4,' WHICH WAS WRITTEN BY ',2A4,'MACHINE',/5X,'ON',I4,1H/,I2,1H/,I2,4X, &
             &'SYSTEM BUFFSIZE=',I8)
   IF ( p40 ) WRITE (nout,99011)
99011 FORMAT (5X,'TAPE IN BINARY RECORDS',/)
   IF ( p41 ) WRITE (nout,99012)
99012 FORMAT (5X,'TAPE IN FORMATTED RECORDS',/)
   ll = 0
   IF ( p1>0 .OR. p1==-3 ) THEN
      DO
!
!     TO SKIP P1 MATRIX DATA BLOCKS OR TABLES ON INPUT TAPE (P1 = +N)
!     OR PRINT CONTENTS OF INPUT TAPE (P1 = -3)
!
         IF ( p40 ) READ (input,ERR=500,END=600) nc , jb , je
         IF ( p41s ) READ (input,99033,ERR=500,END=600) nc , jb , je , (x,j=jb,je)
         IF ( p41c ) READ (input,99034,ERR=500,END=600) nc , jb , je , (x,j=jb,je)
         IF ( p41d ) READ (input,99035,ERR=500,END=600) nc , jb , je , (dx,j=jb,je)
         IF ( debug .AND. (nc<=15 .OR. nc>=col12) ) WRITE (nout,99036) nc , jb , je , ll
         IF ( nc<0 ) EXIT
         IF ( nc==0 ) THEN
            DO WHILE ( p1==-3 .OR. ll<p1 )
               imhere = 340
               ll = ll + 1
               BACKSPACE input
               IF ( p41 ) BACKSPACE input
               IF ( ll>15 ) GOTO 400
               IF ( p40 ) READ (input) i , i , i , dx , j , j , j , j , k , k , fn(1,ll) , fn(2,ll)
               IF ( p41 ) READ (input,99037) i , i , i , dx , j , j , j , j , k , k , fn(1,ll) , fn(2,ll)
               IF ( p1/=-3 .OR. ll<=p1 ) fn(3,ll) = skip
               IF ( k>0 .AND. j>=1 .AND. j<=4 ) THEN
!
!     FILE IS A MATRIX
!
                  IF ( ll>p1 ) fn(3,ll) = mtrx
                  IF ( .NOT.(p40) ) THEN
                     p41s = .FALSE.
                     p41d = .FALSE.
                     p41c = p4==2 .AND. nbpw>=60
                     IF ( .NOT.(p41c) ) THEN
                        IF ( j==1 .OR. j==3 ) p41s = .TRUE.
                        p41d = .NOT.p41s
                     ENDIF
                  ENDIF
                  GOTO 250
               ELSE
!
!     FILE IS A TABLE
!
                  IF ( ll>p1 ) fn(3,ll) = tble
                  imhere = 345
                  DO
!
                     IF ( p40 ) READ (input,ERR=500,END=600) l
                     IF ( p41 ) READ (input,99013,ERR=500,END=600) l , (tabel,j=1,l)
99013                FORMAT (I10,24A,/,(26A5))
                     IF ( debug ) WRITE (nout,99014) l , ll
99014                FORMAT (30X,'L AND LL=',2I6)
                     imhere = 330
                     IF ( l<0 ) GOTO 300
                     IF ( l==0 ) EXIT
                  ENDDO
               ENDIF
            ENDDO
            EXIT
         ENDIF
 250  ENDDO
!
 300  IF ( p1==-3 ) GOTO 1700
      IF ( p41 ) BACKSPACE input
      BACKSPACE input
      GOTO 700
   ELSE
      IF ( p1==-1 ) GOTO 700
      imhere = 290
      WRITE (nout,99030) sfm , imhere , p1
      err = -37
      GOTO 100
   ENDIF
!
 400  WRITE (nout,99015) uim
99015 FORMAT (A29,', INPUTT5, WITH P1= -3, CAN ONLY PRINT UP TO 15 ',' FILE NAMES ON ONE INPUT TAPE.',/5X,'TAPE IS POSITIONED',     &
             &' AFTER THE 15TH FILE')
   ll = ll - 1
   GOTO 1900
!
 500  WRITE (nout,99031) ufm , p3 , ll , nc , imhere
   imhere = 405
   IF ( p41 .AND. mach==2 ) WRITE (nout,99032) imhere
   err = -37
   GOTO 100
 600  IF ( p1==-3 ) THEN
      WRITE (nout,99016) uwm , p3
99016 FORMAT (A25,', EOF ENCOUNTERED ON INPUT TAPE ',2A4,'. TAPE DOES ','NOT CONTAIN AN ''OUTPUT5 E-O-F'' MARK')
      IF ( debug ) WRITE (nout,99017) imhere , ll , nc
99017 FORMAT (5X,'IMHERE,LL,NC =',3I5)
   ELSE
      WRITE (nout,99018) ufm , p3 , imhere , ll , nc
99018 FORMAT (A23,', EOF ENCOUNTERED ON INPUT TAPE ',2A4,5X,'IMHERE,LL,NC =',3I5)
      IF ( p1/=-3 ) nogo = 1
   ENDIF
   GOTO 1700
 700  IF ( p40s ) READ (input,ERR=1300,END=1800) nc , jb , je , (rz(j),j=jb,je)
   IF ( p40d ) READ (input,ERR=1300,END=1800) nc , jb , je , (dz(j),j=jb,je)
   IF ( p41s ) READ (input,99033,ERR=1300,END=1800) nc , jb , je , (rz(j),j=jb,je)
   IF ( p41c ) READ (input,99034,ERR=1300,END=1800) nc , jb , je , (rz(j),j=jb,je)
   IF ( p41d ) READ (input,99035,ERR=1300,END=1800) nc , jb , je , (dz(j),j=jb,je)
   IF ( debug .AND. (nc<=15 .OR. nc>=col12) ) WRITE (nout,99036) nc , jb , je , ll , imhere
   IF ( nc<0 ) GOTO 1400
   IF ( nc/=0 ) GOTO 1100
!             EOF, MATRIX-HEADER, COLUMN-DATA
!
!     MATRIX OR TABLE HEADER
!
 800  IF ( opn ) GOTO 1500
   ll = ll + 1
   IF ( ll>15 ) GOTO 400
   BACKSPACE input
   IF ( p41 ) BACKSPACE input
   j = -1
   IF ( p40 ) READ (input,ERR=900) k , j , j , dx , col , row , form , type , max , dens , fn(1,ll) , fn(2,ll)
   IF ( p41 ) READ (input,99037,ERR=900) k , j , j , dx , col , row , form , type , max , dens , fn(1,ll) , fn(2,ll)
   col12 = col - 12
   IF ( col12<0 ) col12 = 0
   IF ( .NOT.debug ) GOTO 1000
 900  WRITE (nout,99019) col , row , form , type , max , dens , dx , fn(1,ll) , fn(2,ll)
99019 FORMAT (' COL,ROW,FORM,TYPE,MAX,DENS,DX,FILE=',6I6,D12.3,3X,2A4)
   IF ( j==-1 ) CALL mesage(-37,0,subnam)
!
!WKBR1    CALL TABLE V (*510,INPUT,LL,MCB,FN(1,LL),P4,BUF1,RZ)
 1000 IF ( k==0 .AND. (dens==0 .OR. type<=0 .OR. type>4) ) CALL tablev(*700,input,ll,mcb,fn(1,ll),p4,buf1,z5)
!
   fn(3,ll) = mtrx
   p40s = .FALSE.
   p40d = .FALSE.
   p41s = .FALSE.
   p41d = .FALSE.
   p41c = p4==2 .AND. nbpw>=60
   IF ( .NOT.(p41c) ) THEN
      IF ( p41 ) THEN
         IF ( type==1 .OR. type==3 ) p41s = .TRUE.
         p41d = .NOT.p41s
      ELSE
         IF ( type==1 .OR. type==3 ) p40s = .TRUE.
         p40d = .NOT.p40s
      ENDIF
   ENDIF
   IF ( debug ) WRITE (nout,99020) p40 , p40s , p40d , p41 , p41s , p41d , p41c
99020 FORMAT ('0  P40,P40S,P40D,P41,P41S,P41D,P41C = ',7L4)
   typin = type
   typout = type
   jtyp = type
   IF ( type==3 ) jtyp = 2
   ii = 1
   jj = row
   incr = 1
   nwds = row*jtyp
   IF ( nwds>buf1 ) CALL mesage(-8,0,subnam)
!
!     OPEN GINO FILE FOR OUTPUT
!
   imhere = 640
   IF ( p1==-3 ) THEN
!
      WRITE (nout,99030) sfm , imhere , p1
      CALL mesage(-37,0,subnam)
   ELSE
      rowx = row
      formx = form
      output = 200 + ll - p1n
      mcb(1) = output
      CALL rdtrl(mcb(1))
      IF ( mcb(1)<=0 ) GOTO 1200
      err = -1
      CALL open(*100,output,rz(buf1),1)
      CALL fname(output,name)
      CALL write(output,name,2,1)
      opn = .TRUE.
      col = 0
      row = rowx
      form = formx
      type = typout
      max = 0
      dens = 0
      nck = 0
      GOTO 700
   ENDIF
!
!     RECOVER INPUT MATRIX, AND WRITE IT OUT BY COLUMN
!
 1100 imhere = 700
   IF ( p1==-3 ) GOTO 700
   nck = nck + 1
   IF ( nc/=nck ) GOTO 500
   IF ( jb>1 ) THEN
      jb = (jb-1)*jtyp
      DO j = 1 , jb
         rz(j) = 0.0
      ENDDO
   ENDIF
   IF ( je<nwds ) THEN
      je = (je*jtyp) + 1
      DO j = je , nwds
         rz(j) = 0.0
      ENDDO
   ENDIF
   CALL pack(rz,output,mcb)
   GOTO 700
 1200 DO
!
!     OUTPUT FILE PURGED, SKIP FORWARD FOR NEXT MATRIX ON TAPE
!
      IF ( p40 ) READ (input,ERR=500,END=600) nc , jb , je
      IF ( p41s ) READ (input,99033,ERR=500,END=600) nc , jb , je , (x,j=jb,jb)
      IF ( p41c ) READ (input,99034,ERR=500,END=600) nc , jb , je , (x,j=jb,jb)
      IF ( p41d ) READ (input,99035,ERR=500,END=600) nc , jb , je , (dx,j=jb,jb)
      IF ( nc<=0 ) THEN
         CALL page2(2)
         WRITE (nout,99021) uwm , fn(1,ll) , fn(2,ll)
99021    FORMAT (A25,', OUTPUT FILE PURGED.  ',2A4,' FROM INPUT TAPE NOT ','COPIED')
!     LL = LL + 1
         GOTO 800
      ENDIF
   ENDDO
!
 1300 imhere = -imhere
   WRITE (nout,99031) ufm , p3 , ll , nc , imhere
   WRITE (nout,99022) p40 , p41 , p40s , p40d , p41s , p41d , p41c
99022 FORMAT ('  P40,P41,P40S,P40D,P41S,P41D,P41C =',7L2)
   imhere = 770
   IF ( p41 .AND. mach==2 ) WRITE (nout,99032) imhere
   GOTO 1200
!
!     END OF MATRIX ENCOUNTERED. CLOSE GINO DATA BLOCK WITH REWIND.
!
 1400 IF ( .NOT.opn ) GOTO 1600
 1500 CALL close(output,1)
   opn = .FALSE.
   IF ( form<1 .OR. form>6 ) THEN
      form = 1
      IF ( col/=row ) form = 2
   ENDIF
   CALL wrttrl(mcb)
   CALL fname(output,name)
   CALL page2(10)
   WRITE (nout,99023) fn(1,ll) , fn(2,ll) , input , name , (mcb(j),j=1,7)
99023 FORMAT (/5X,'MATRIX DATA BLOCK ',2A4,' WAS SUCESSFULLY RECOVERED',' FROM FORTRAN UNIT',I4,' TO ',2A4,/8X,'GINO UNIT =',I8,/6X,&
             &'NO. OF COLS =',I8,/6X,'NO. OF ROWS =',I8,/13X,'FORM =',I8,/13X,'TYPE =',I8,/3X,'NON-ZERO WORDS =',I8,/10X,           &
            & 'DENSITY =',I8)
 1600 imhere = 840
   IF ( ll>=5+p1n ) THEN
      BACKSPACE input
      IF ( p41 ) BACKSPACE input
      GOTO 1900
   ELSEIF ( nc<0 ) THEN
      IF ( p1==-3 ) GOTO 2000
   ELSEIF ( nc==0 ) THEN
      GOTO 800
   ELSE
      GOTO 500
   ENDIF
!
!     IF NC = -2, THIS IS AN ELEM/GRID ID RECORD WRITTEN BY DUMOD5
!
 1700 IF ( nc==-2 ) GOTO 700
 1800 nc = -3
   IF ( opn ) GOTO 1400
   IF ( fn(3,ll)==bk ) ll = ll - 1
   IF ( ll<=0 ) THEN
!
      IF ( p1==-3 ) WRITE (nout,99024) uim , input
99024 FORMAT (A29,' FROM INPUTT5 MODULE, INPUT TAPE (FORTRAN UNIT',I5,') CONTAINS NO DATA BLOCK')
      GOTO 2000
   ENDIF
!
!     PRINT LIST OF DATA BLOCKS ON FORTRAN TAPE (P1=-3).
!
 1900 CALL page2(ll+9)
   WRITE (nout,99025)
99025 FORMAT (/5X,'SUMMARY FROM INPUTT5 MODLUE -')
   IF ( p1/=-3 ) WRITE (nout,99026) input
99026 FORMAT (/34X,'FILES RECOVERED FROM FORTRAN UNIT',I5)
   IF ( p1==-3 ) WRITE (nout,99027) input
99027 FORMAT (/34X,'FILE CONTENTS ON FORTRAN UNIT',I5)
   WRITE (nout,99028) mac , bf , (j,fn(1,j),fn(2,j),fn(3,j),j=1,ll)
99028 FORMAT (28X,'(WRITTEN BY ',2A4,' MACHINE ',A8,' RECORDS)',//37X,'FILE',8X,'NAME',8X,'TYPE',/33X,9(4H----),/,                  &
            & (37X,I3,7X,2A4,6X,A4))
   IF ( nogo==1 ) THEN
      err = -37
      GOTO 100
!
   ELSEIF ( p1==-3 ) THEN
      REWIND input
      IF ( p40 ) READ (input)
      IF ( p41 ) READ (input,99029)
   ENDIF
!
 2000 IF ( mach==3 ) CALL unvcls(p2)
   IF ( mach==4 ) CALL cdccls(p2)
99029 FORMAT (4A4,5I8)
99030 FORMAT (A25,' @',I5,I10)
99031 FORMAT (A23,', TAPE ERROR DURING READ/INPUTT5  ',2A4,/5X,'LL,NC =',2I5,'   IMHERE =',I5)
99032 FORMAT (/5X,'IBM USER - CHECK FILE ASSIGNMENT FOR DCB PARAMETER ','OF 132 BYTES',I15)
99033 FORMAT (3I8,/,(10E13.6))
99034 FORMAT (3I8,/,(5E26.17))
99035 FORMAT (3I8,/,(5D26.17))
99036 FORMAT (30X,'NC,JB,JE,LL=',5I6,'=IMHERE')
99037 FORMAT (3I8,/,D26.17,6I8,2A4)
END SUBROUTINE inptt5
