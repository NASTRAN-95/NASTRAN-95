!*==inptt2.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE inptt2
!
!     READ DATA BLOCK(S) FROM A FORTRAN UNIT.
!
!     CALL TO THIS MODULE IS
!
!     INPUTT2   /O1,O2,O3,O4,O5/V,N,P1/V,N,P2/V,N,P3/V,N,P4/V,N,P5/
!                               V,N,P6 $
!
!     PARAMETERS P1, P2, P4, AND P5 ARE INTEGER INPUT. P3 AND P6 ARE BCD
!
!            P1 =+N, SKIP FORWARD N DATA BLOCKS BEFORE READ
!               = 0, NO ACTION TAKEN BEFORE READ (DEFAULT)
!               =-1, BEFORE READ, FORTRAN TAPE IS REWOUND AND TAPE
!                    HEADER RECORD (RECORD NUMBER ZERO) IS CHECKED
!               =-3, THE NAMES OF ALL DATA BLOCKS ON FORTRAN TAPE
!                    ARE PRINTED AND READ OCCURS AT BEGINNING OF TAPE
!               =-5, SEARCH FORTRAN TAPE FOR FIRST VERSION OF DATA
!                    BLOCKS REQUESTED.
!                    IF ANY ARE NOT FOUND, A FATAL TERMINATION OCCURS.
!               =-6, SEARCH FORTRAN TAPE FOR FINAL VERSION OF DATA
!                    BLOCKS REQUESTED.
!                    IF ANY ARE NOT FOUND, A FATAL TERMINATION OCCURS.
!               =-7, SEARCH FORTRAN TAPE FOR FIRST VERSION OF DATA
!                    BLOCKS REQUESTED.
!                    IF ANY ARE NOT FOUND, A WARNING OCCURS.
!               =-8, SEARCH FORTRAN TAPE FOR FINAL VERSION OF DATA
!                    BLOCKS REQUESTED.
!                    IF ANY ARE NOT FOUND, A WARNING OCCURS.
!
!            P2 =    THE FORTRAN UNIT FROM WHICH THE DATA BLOCK(S)
!                    WILL BE READ. (DEFAULT P2 = 11, OR 14)
!
!            P3 =    TAPE ID CODE FOR FORTRAN TAPE, AN ALPHANUMERIC
!                    VARIABLE WHOSE VALUE MUST MATCH A CORRESPONDING
!                    VALUE ON THE FORTRAN TAPE.
!                    THIS CHECK IS DEPENDENT ON THE VALUE OF P1 AS
!                    FOLLOWS..
!
!                    *P1*             *TAPE ID CHECKED*
!                     +N                     NO
!                      0                     NO
!                     -1                    YES
!                     -3                    YES (WARNING CHECK)
!                     -5                    YES
!                     -6                    YES
!                     -7                    YES
!                     -8                    YES
!                    THE MPL DEFAULT VALUE FOR P3 IS XXXXXXXX .
!
!            P4 =    NOT USED IN INPUTT2.
!                    (USED ONLY IN OUTPUT2 FOR MAXIMUM RECORD SIZE)
!
!            P5 = 0, NON-SPARSE MATRIX IF INPUT IS A MATRIX DATA BLOCK
!               = NON-0, SPARSE MATRIX IF INPUT IS A MATRIX DATA BLOCK
!                    (P4 IS IGNORED IF INPUT IS A TALBE DATA BLOCK.
!                     P4 IS EQUIVALENT TO P5 IN OUTPUT2 MODULE)
!
!            P6 = BLANK, (DEFAULT)
!               = 'MSC', THE INPUT TAPE WAS WRITTEN IN MSC/OUTPUT2
!                     COMPATIBEL RECORD FORMAT.
!
!     OUTPT2 DOES NOT AUTOMATICALLY OUTPUT THE MATRIX IN STRING OR
!     SPARSE FORM. UNLESS P5 IS REQUESTED.
!     SIMILARILY, INPUT2 DOES NOT AUTOMATICALLY PROCESS MATRIX IN SPARSE
!     MATRIX FORM, UNLESS P5 IS REQUESTED).
!
!     REVISED  11/90 BY G.CHAN/UNISYS
!              (1) TO ACCEPT MSC/OUTPUT2 DATA (CALLED FROM INPTT4, 11/90
!                  OR INPTT2, 2/93)
!              (2) TO ACCEPT SPARSE MATRIX COMING FORM COSMIC/OUTPT2
!                  (SEE P5 PARAMETER IN INPTT2 AND P5 IN OUTPT2)
!
   IMPLICIT NONE
   USE C_BLANK
   USE C_DSNAME
   USE C_PACKX
   USE C_SYSTEM
   USE C_TYPE
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: base , bcol , bflag , bform , bname , bpoint , brav , brow , btyp , bwrt , i , imhere , in , index , iptx , irecf ,   &
            & k , key , keyx , kf , krec , l , l1 , l2 , lcor , line , nb , ncol , nlpp , nnt , nout , ns , nskip , ntrl , nwd ,    &
            & nwdsx , oubuf , output , ret
   INTEGER , DIMENSION(4) , SAVE :: bcdbin
   INTEGER , DIMENSION(20) :: blk
   REAL , DIMENSION(1) :: core
   REAL*8 , DIMENSION(1) :: dcore
   LOGICAL :: dp , sparse
   INTEGER , DIMENSION(3) :: dx
   INTEGER , SAVE :: i3 , ifirst , ipt2 , ipt4 , mete , mfiv , mfor , mone , msc , msix , mtre , mtwo , zero
   INTEGER , DIMENSION(7) , SAVE :: idhdr
   INTEGER , DIMENSION(7) :: idhdrx , mcb
   INTEGER , DIMENSION(2) :: name , namex , p3x , tapcod
   INTEGER , DIMENSION(2) , SAVE :: none , subnam
   INTEGER , DIMENSION(5,3) :: nt
   INTEGER , DIMENSION(8) :: trl
!
! End of declarations rewritten by SPAG
!
!WKBNB
!WKBNE
   !>>>>EQUIVALENCE (Core(1),X(1))
   !>>>>EQUIVALENCE (Ksystm(1),Nb) , (Ksystm(2),Nout) , (Ksystm(9),Nlpp) , (Ksystm(12),Line) , (blk(1),bname) , (blk(2),btyp) ,          &
!>>>>    & (blk(3),bform) , (blk(4),brow) , (blk(5),bpoint) , (blk(6),brav) , (blk(7),bwrt) , (blk(8),bflag) , (blk(12),bcol) ,          &
!>>>>    & (Dcore(1),Core(1))
!WKBI
   DATA ifirst/0/
   DATA subnam/4HINPT , 4HT2  / , none/4H (NO , 4HNE) /
   DATA zero , mone , mtwo , mtre , mfor/0 , -1 , -2 , -3 , -4/ , i3/3/mfiv , msix , mete/ - 5 , -6 , -8/ , ipt2 , ipt4/1H2 , 1H4/
   DATA idhdr/4HNAST , 4HRAN  , 4HFORT , 4H TAP , 4HE ID , 4H COD , 3HE -/
   DATA bcdbin/4HBCD  , 4H     , 4HBINA , 4HRY  / , msc/4HMSC /
!
!
   iptx = ipt2
   ntrl = 8
   IF ( P4/=0 ) THEN
      IF ( P5==0 ) THEN
         WRITE (nout,99001) Uwm
99001    FORMAT (A25,'. THE 4TH PARAMETER IN INPUTT2 MODULE IS NO LONGER ','USED.',/5X,'SPARSE MATRIX FLAG IS NOW THE 5TH PARAMETER'&
               & ,', A MOVE TO SYNCHRONIZE THE PARAMETERS USED IN OUTPUT2')
         P5 = P4
      ENDIF
   ENDIF
   sparse = .FALSE.
   IF ( P5/=0 ) sparse = .TRUE.
   IF ( P6(1)/=msc ) GOTO 200
   GOTO 100
!
!
   ENTRY input2
!     ============
!
!     INPUT2 IS CALLED TO HANDLE MSC/OUTPUT2 DATA.
!     IT IS CALLED FROM INPTT2 WITH P6 PARAMETER = 'MSC', OR
!     FROM INPTT4
!
   iptx = ipt4
 100  WRITE (nout,99002) Uim , iptx
99002 FORMAT (A29,' FROM INPUTT',A1,'. USER INPUT TAPE IN MSC/OUTPUT2',' COMPATIBLE RECORDS')
   iptx = ipt4
   ntrl = 7
   irecf = 0
   sparse = .FALSE.
   IF ( P3(1)==bcdbin(1) .AND. P3(2)==bcdbin(2) ) GOTO 2400
   IF ( P3(1)==bcdbin(3) .AND. P3(2)==bcdbin(4) ) GOTO 2400
!
 200  lcor = korsz(X) - nb
   IF ( lcor<=0 ) CALL mesage(-8,lcor,subnam)
!WKBNB
   IF ( ifirst==0 ) THEN
      CLOSE (UNIT=P2)
      OPEN (UNIT=P2,FILE=Dsnames(P2),FORM='UNFORMATTED',STATUS='UNKNOWN')
      ifirst = 1
   ENDIF
!WKBNE
   oubuf = lcor + 1
   tapcod(1) = P3(1)
   tapcod(2) = P3(2)
   in = P2
   IF ( P1<mete .OR. P1==mtwo .OR. P1==mfor ) THEN
      WRITE (nout,99003) Ufm , iptx , P1
99003 FORMAT (A23,' 4113, MODULE INPUTT',A1,' - ILLEGAL VALUE FOR ','FIRST PARAMETER =',I20)
      GOTO 2500
!
   ELSEIF ( P1<mfor ) THEN
!
!     SEARCH MODE
!
!     EXAMINE OUTPUT REQUESTS AND FILL NAME TABLE.
!
      nnt = 0
      DO i = 1 , 5
         output = 200 + i
         trl(1) = output
         CALL rdtrl(trl)
         IF ( trl(1)>0 ) THEN
            CALL fname(output,name)
            IF ( name(1)/=none(1) .OR. name(2)/=none(2) ) THEN
               nt(i,1) = 0
               nt(i,2) = name(1)
               nt(i,3) = name(2)
               nnt = nnt + 1
               CYCLE
            ENDIF
         ENDIF
         nt(i,1) = -1
!     IF (IPTX .NE. IPT2) GO TO 3050
         nt(i,2) = none(1)
         nt(i,3) = none(2)
      ENDDO
!
      IF ( nnt>0 ) THEN
!
!     CHECK TAPE ID LABEL.
!
         REWIND in
         READ (in) key
         keyx = 3
         IF ( key/=keyx ) GOTO 2200
         READ (in) dx
         READ (in) key
         keyx = 7
         IF ( key/=keyx ) GOTO 2200
         READ (in) idhdrx
         DO kf = 1 , 7
            IF ( idhdrx(kf)/=idhdr(kf) ) GOTO 2000
         ENDDO
         READ (in) key
         keyx = 2
         IF ( key/=keyx ) GOTO 2200
         READ (in) p3x
         READ (in) key
         imhere = 815
         IF ( key>=0 ) GOTO 2300
         IF ( p3x(1)/=P3(1) .OR. p3x(2)/=P3(2) ) GOTO 1900
         ASSIGN 1100 TO ret
         nskip = 1
         GOTO 1600
      ELSE
         CALL page2(-2)
         WRITE (nout,99004) Uwm , iptx
99004    FORMAT (A25,' 4137, ALL OUTPUT DATA BLOCKS FOR INPUTT',A1,' ARE PURGED.')
!     CLOSE (UNIT=IN)
         RETURN
      ENDIF
   ELSEIF ( P1==mtre ) THEN
!
!     OBTAIN LIST OF DATA BLOCKS ON FORTRAN TAPE.
!
      REWIND in
      READ (in) key
      keyx = 3
      IF ( key/=keyx ) GOTO 2200
      READ (in) dx
      READ (in) key
      keyx = 7
      IF ( key/=keyx ) GOTO 2200
      READ (in) idhdrx
      DO kf = 1 , 7
         IF ( idhdrx(kf)/=idhdr(kf) ) GOTO 2000
      ENDDO
      READ (in) key
      keyx = 2
      IF ( key/=keyx ) GOTO 2200
      READ (in) p3x
      READ (in) key
      imhere = 515
      IF ( key>=0 ) GOTO 2300
      IF ( p3x(1)/=P3(1) .OR. p3x(2)/=P3(2) ) THEN
         WRITE (nout,99005) Uwm , p3x , iptx , P3
99005    FORMAT (A25,' 4135, USER TAPE ID CODE -',2A4,'- DOES NOT MATCH ','THIRD INPUTT',A1,' DMAP PARAMETER -',2A4,2H-.)
      ENDIF
      ASSIGN 700 TO ret
      nskip = 1
      GOTO 1600
   ELSEIF ( P1<=zero ) THEN
!
!     OPEN FORTRAN TAPE TO READ TAPE-LABEL WITHOUT REWIND.
!
      IF ( P1/=mone ) GOTO 500
      REWIND in
      READ (in) key
      keyx = 3
      IF ( key/=keyx ) GOTO 2200
      READ (in) dx
      READ (in) key
      keyx = 7
      IF ( key/=keyx ) GOTO 2200
      READ (in) idhdrx
      DO kf = 1 , 7
         IF ( idhdrx(kf)/=idhdr(kf) ) GOTO 2000
      ENDDO
      READ (in) key
      keyx = 2
      IF ( key/=keyx ) GOTO 2200
      READ (in) p3x
      READ (in) key
      imhere = 145
      IF ( key>=0 ) GOTO 2300
      IF ( p3x(1)/=P3(1) .OR. p3x(2)/=P3(2) ) GOTO 1900
      ASSIGN 500 TO ret
      nskip = 1
      GOTO 1600
   ELSE
!
      i = 1
   ENDIF
 300  READ (in) key
   keyx = 2
   IF ( key/=keyx ) GOTO 2200
   READ (in) namex
   READ (in) key
   imhere = 115
   IF ( key>=0 ) GOTO 2300
   ASSIGN 400 TO ret
   nskip = 1
   GOTO 1600
!
 400  i = i + 1
   IF ( i<=P1 ) GOTO 300
!
 500  DO i = 1 , 5
!
      output = 200 + i
      trl(1) = output
      CALL rdtrl(trl)
      IF ( trl(1)<=0 ) CYCLE
      CALL fname(output,name)
      IF ( name(1)==none(1) .AND. name(2)==none(2) ) CYCLE
!
!     READ FILE NAME HEADER RECORD.
!
      READ (in) key
      IF ( key==0 ) EXIT
      keyx = 2
      IF ( key/=keyx ) GOTO 2200
      READ (in) namex
      READ (in) key
      imhere = 163
      IF ( key>=0 ) GOTO 2300
!
!     READ TRAILER RECORD.
!
      READ (in) key
      keyx = ntrl
      IF ( key/=keyx ) GOTO 2200
      READ (in) (trl(l),l=1,ntrl)
      IF ( iptx==ipt2 ) irecf = trl(8)
      READ (in) key
      imhere = 165
      IF ( key>=0 ) GOTO 2300
!
!     OPEN OUTPUT DATA BLOCK TO WRITE WITH REWIND.
!
      CALL open(*1800,output,X(oubuf),1)
!
!     COPY CONTENTS OF FORTRAN TAPE ONTO OUTPUT DATA BLOCK.
!
!     TRL(8) = 0, DATA BLOCK IS A TALBE
!            = 1, DATA BLOCK IS A MATRIX, WRITTEN IN STRING FORMAT
!            = 2, DATA BLOCK IS A VECTOR (1ST RECORD IS REGULAR, 2ND
!                 RECORD IS A STRING)
!
      index = 0
      READ (in) key
      IF ( iptx/=ipt2 ) THEN
         bname = output
         keyx = 1
         IF ( key/=keyx ) GOTO 2200
         READ (in) krec
         imhere = 170
         IF ( krec/=0 ) GOTO 2300
!
         READ (in) key
      ENDIF
      keyx = 2
      IF ( key<keyx ) GOTO 2200
      IF ( key>lcor ) GOTO 2100
      READ (in) (X(l),l=1,key)
      CALL write(output,name,2,0)
      IF ( key/=keyx ) CALL write(output,X(i3),key-2,0)
!
 550  IF ( iptx==ipt2 ) GOTO 650
      READ (in) key
      imhere = 205
      IF ( key>=0 ) GOTO 2300
      btyp = trl(5)
      bform = 0
      bcol = 0
      nwd = Nwds(btyp)
      dp = btyp==2 .OR. btyp==4
      CALL write(output,X,0,1)
 600  DO
         READ (in) key
         keyx = 1
         IF ( key/=keyx ) GOTO 2200
         READ (in) krec
         IF ( krec==0 ) EXIT
!
!     MATRIX DATA BLOCK - MSC/STRING RECORD. (IPTX=IPT4)
!
!
         bflag = -1
         bcol = bcol + 1
         DO
            READ (in) key
            CALL putstr(blk)
            imhere = 360
            IF ( key<0 ) THEN
               bflag = +1
               bwrt = 0
               CALL endput(blk)
               EXIT
            ELSEIF ( key==0 ) THEN
               GOTO 2300
            ELSE
!       NULL or EOR, ERR, KEY
!
               bwrt = key/nwd
               imhere = 370
               IF ( bwrt>brav ) GOTO 2300
!
!     COMMENTS FROM G.C./UNISYS   3/93
!     UNLESS MSC/PUTSTR IS DIFFERENT FROM COSMIC/PUTSTR, THE FOLLOWING
!     3 LINES, ORIGINATED FROM MSC SOURCE CODE, DO NOT WORK FOR D.P.
!     DATA ON VAX, AND POSSIBLY SILICON-GRAHPICS. THEY ARE REPLACED BY
!     NEXT 17 LINES BELOW.
!     (I TRIED  SETTING L1=(BPOINT-1)*NWD+1, AND STILL DID NOT WORK.)
!     THE PROBLEM HERE IS D.P. DATA MAY FALL SHORT ON DOUBLE WORD
!     BOUNADRY, AND THEREFORE BECOME GARBAGE, WHICH MAY CAUSE FATAL
!     ERROR IN PRINTING.
!
!     L1 = BPOINT
!     L2 = L1 - 1 + KEY
!     READ (IN) BROW,(CORE(L),L=L1,L2)
!
               l1 = bpoint*nwd
               l2 = l1 - 1 + key
               IF ( dp ) THEN
                  l1 = l1/2
                  l2 = l2/2
!     L  = 382
!     WRITE  (NOUT,375) L,L1,L2,KEY,BROW,BTYP,BPOINT
                  READ (in) brow , (dcore(l),l=l1,l2)
               ELSE
!     L  = 375
!     WRITE  (NOUT,375) L,L1,L2,KEY,BROW,BTYP,BPOINT
! 375 FORMAT (' /@',I3,'  L1,L2,KEY,BROW,BTYP,BPOINT =',4I7,4I4)
!     WRITE  (NOUT,376,ERR=388) (CORE(L),L=L1,L2)
! 376 FORMAT (10X,' CORE =',/,(1X,11E11.3))
                  READ (in) brow , (core(l),l=l1,l2)
               ENDIF
!     WRITE  (NOUT,382,ERR=388) (DCORE(L),L=L1,L2)
! 382 FORMAT (10X,'DCORE =',/,(1X,11D11.3))
               CALL endput(blk)
            ENDIF
         ENDDO
      ENDDO
 650  DO
!
!     TABLE DATA BLOCK(S)
!
         READ (in) key
         IF ( key<0 ) THEN
            CALL write(output,X,0,1)
            IF ( iptx==ipt4 ) GOTO 600
            IF ( irecf==0 ) GOTO 550
            IF ( irecf==1 .OR. index>0 ) THEN
!
!     READ STRING FORMATTED MATRIX
!
               IF ( irecf/=2 .OR. index/=2 ) THEN
                  index = 2
                  CALL makmcb(mcb(1),output,trl(3),trl(4),trl(5))
                  Irow = 1
                  Nrow = trl(3)
                  Typin = trl(5)
                  Typout = trl(5)
                  nwdsx = Nwds(Typout)
                  ncol = trl(2)
!
!     CHECK FOR NULL MATRIX
!
                  IF ( Nrow==0 .OR. ncol==0 ) EXIT
                  IF ( irecf==2 ) ncol = 1
                  Incr = 1
                  nwdsx = Nrow*nwdsx
               ENDIF
               keyx = nwdsx
!
!     NWDSX IS NUMBER OF WORDS NEEDED PER COLUMN
!
               IF ( sparse ) THEN
!
!     SPARSE MATRIX INPUT (P5 = NON-ZERO)
!     (NOT CALLING FROM INPTT4 (IPTX=IPT2)
!
                  DO l = 1 , ncol
                     DO k = 1 , nwdsx
                        X(k) = 0.0
                     ENDDO
                     DO
                        READ (in) key , base
                        IF ( key<0 ) THEN
                           CALL pack(X,output,mcb)
                           EXIT
                        ELSE
                           READ (in) (X(k+base),k=1,key)
                        ENDIF
                     ENDDO
                  ENDDO
               ELSE
                  DO l = 1 , ncol
                     READ (in) key
                     IF ( key/=keyx ) GOTO 2200
                     IF ( key>lcor ) GOTO 2100
                     READ (in) (X(k),k=1,nwdsx)
                     CALL pack(X,output,mcb)
                     READ (in) key
                     imhere = 265
                     IF ( key>0 ) GOTO 2300
                  ENDDO
               ENDIF
               IF ( irecf==2 ) GOTO 550
               keyx = 0
               READ (in) key
               imhere = 285
               IF ( key==keyx ) EXIT
               GOTO 2200
            ELSE
               index = 1
            ENDIF
         ELSEIF ( key==0 ) THEN
            EXIT
         ELSE
!              EOR, EOF, KEY
!
            IF ( key>lcor ) GOTO 2100
            READ (in) (X(l),l=1,key)
            CALL write(output,X,key,0)
         ENDIF
      ENDDO
!
!     CLOSE OUTPUT DATA BLOCK WITH REWIND AND EOF.
!
      CALL close(output,1)
!
!     WRITE TRAILER.
!
      trl(1) = output
      CALL wrttrl(trl)
      CALL page2(-3)
      WRITE (nout,99006) Uim , name , in , namex
99006 FORMAT (A29,' 4105, DATA BLOCK ',2A4,' RETRIEVED FROM FORTRAN ','TAPE ',I2,/5X,'ORIGINAL NAME OF DATA BLOCK WAS ',2A4)
      IF ( sparse .AND. ntrl==8 .AND. trl(8)/=0 ) WRITE (nout,99007) trl(2) , trl(3)
99007 FORMAT (1H+,55X,'(A SPARSE MATRIX',I6,2H X,I6,')')
!
   ENDDO
!
!     CLOSE FORTRAN TAPE WITHOUT REWIND.
!
   RETURN
 700  kf = 0
 800  CALL page1
   line = line + 8
   WRITE (nout,99008) in
99008 FORMAT (//50X,'FILE CONTENTS ON FORTRAN UNIT ',I2,/51X,32(1H-),//54X,4HFILE,18X,4HNAME,//)
 900  READ (in) key
   IF ( key==0 ) THEN
      REWIND in
      ASSIGN 500 TO ret
      nskip = 1
   ELSE
!     KEYX = 2
!     IF (KEY .NE. KEYX) GO TO 9918
      READ (in) namex
!     READ (IN) KEY
!     IF (KEY .GE. 0) GO TO 9919
      ASSIGN 1000 TO ret
      nskip = 1
   ENDIF
   GOTO 1600
 1000 kf = kf + 1
   line = line + 1
   WRITE (nout,99009) kf , namex
99009 FORMAT (53X,I5,18X,2A4)
   IF ( line<nlpp ) GOTO 900
   GOTO 800
!
!     BEGIN SEARCH OF TAPE.
!
 1100 kf = 0
 1200 READ (in) key
   IF ( key==0 ) THEN
!
      IF ( nnt<=0 ) GOTO 1400
      CALL page2(-7)
      IF ( P1==mfiv .OR. P1==msix ) THEN
         WRITE (nout,99010) Ufm
99010    FORMAT (A23,' 4142, ONE OR MORE DATA BLOCKS NOT FOUND ON USER ','TAPE')
      ELSE
         WRITE (nout,99011) Uwm
99011    FORMAT (A25,' 4141, ONE OR MORE DATA BLOCKS NOT FOUND ON FORTRAN',' TAPE.')
      ENDIF
      DO i = 1 , 5
         IF ( nt(i,1)==0 ) THEN
            WRITE (nout,99012) nt(i,2) , nt(i,3)
99012       FORMAT (20X,21HNAME OF DATA BLOCK = ,2A4)
         ENDIF
         IF ( iptx==ipt4 ) GOTO 1400
      ENDDO
      IF ( P1==mfiv .OR. P1==msix ) GOTO 2500
      GOTO 1400
   ELSE
!     KEYX = 2
!     IF (KEY .NE. KEYX) GO TO 9918
      READ (in) namex
      READ (in) key
      imhere = 835
      IF ( key>=0 ) GOTO 2300
      kf = kf + 1
!
      DO i = 1 , 5
         name(1) = nt(i,2)
         name(2) = nt(i,3)
         IF ( nt(i,1)<0 ) CYCLE
         IF ( name(1)/=namex(1) .OR. name(2)/=namex(2) ) CYCLE
         nt(i,1) = nt(i,1) + 1
         IF ( nt(i,1)==1 .OR. P1==msix .OR. P1==mete ) THEN
            READ (in) key
            keyx = ntrl
            IF ( key/=keyx ) GOTO 2200
            READ (in) (trl(l),l=1,ntrl)
            IF ( iptx==ipt2 ) irecf = trl(8)
            READ (in) key
            imhere = 855
            IF ( key>=0 ) GOTO 2300
!
            output = 200 + i
            CALL open(*1800,output,X(oubuf),1)
!
            index = 0
!     IF (IPTX .EQ. IPT4) GO TO 890        ! FROM MSC/INPTT4
            READ (in) key
            IF ( iptx/=ipt2 ) THEN
               keyx = 1
               IF ( key==keyx ) GOTO 2200
               READ (in) krec
               imhere = 857
               IF ( krec<0 ) GOTO 2300
               READ (in) key
            ENDIF
            keyx = 2
            IF ( key<keyx ) GOTO 2200
            IF ( key>lcor ) GOTO 2100
            READ (in) (X(l),l=1,key)
            CALL write(output,name,2,0)
            IF ( key/=keyx ) CALL write(output,X(i3),key-2,0)
         ELSE
            CALL page2(-3)
            WRITE (nout,99013) Uwm , name , kf , in
99013       FORMAT (A25,' 4138, DATA BLOCK ,',2A4,' (DATA BLOCK COUNT =',I6,')  HAS PREVIOUSLY BEEN RETRIEVED FROM ',/36X,          &
                   &'FORTRAN TAPE ',I2,' AND WILL BE IGNORED.')
            EXIT
         ENDIF
!
 1220    IF ( iptx==ipt2 ) GOTO 1260
         READ (in) key
         imhere = 875
         IF ( key>0 ) GOTO 2300
         btyp = trl(5)
         bform = 0
         bcol = 0
         nwd = Nwds(btyp)
         dp = btyp==2 .OR. btyp==4
         CALL write(output,0,0,1)
 1240    DO
            READ (in) key
            keyx = 1
            IF ( key/=keyx ) GOTO 2200
            READ (in) krec
            IF ( krec==0 ) EXIT
!
!     MATRIX DATA BLOCK, IPTX = IPT4.  MSC/STRING RECORD
!
!
            bflag = -1
            bcol = bcol + 1
            DO
               READ (in) key
               CALL putstr(blk)
               imhere = 1025
               IF ( key<0 ) THEN
                  bflag = +1
                  bwrt = 0
                  CALL endput(blk)
                  EXIT
               ELSEIF ( key==0 ) THEN
                  GOTO 2300
               ELSE
!       NULL or EOR,  ERR, KEY
!
                  bwrt = key/nwd
                  imhere = 1030
                  IF ( bwrt>brav ) GOTO 2300
!
!     L1 = BPOINT
!     L2 = L1 - 1 + KEY
!     READ (IN) BROW,(CORE(L),L=L1,L2)
!
                  l1 = bpoint*nwd
                  l2 = l1 - 1 + key
                  IF ( dp ) THEN
                     l1 = l1/2
                     l2 = l2/2
                     READ (in) brow , (dcore(l),l=l1,l2)
                  ELSE
                     READ (in) brow , (core(l),l=l1,l2)
                  ENDIF
                  CALL endput(blk)
               ENDIF
            ENDDO
         ENDDO
 1260    DO
!
!     TABLE DATA BLOCK(S)
!
            READ (in) key
            IF ( key<0 ) THEN
               CALL write(output,X,0,1)
!     IF (IPTX .EQ. IPT4) GO TO 890
               IF ( iptx==ipt4 ) GOTO 1240
               IF ( irecf==0 ) GOTO 1220
               IF ( irecf/=1 ) THEN
                  IF ( index<=0 ) THEN
                     index = 1
                     GOTO 1220
                  ENDIF
               ENDIF
!
!     READ STRING FORMATTED MATRIX
!
               IF ( irecf/=2 .OR. index/=2 ) THEN
                  index = 2
                  CALL makmcb(mcb(1),output,trl(3),trl(4),trl(5))
                  Irow = 1
                  Nrow = trl(3)
                  Typin = trl(5)
                  Typout = trl(5)
                  nwdsx = Nwds(Typout)
                  ncol = trl(2)
!
!     CHECK FOR NULL MATRIX
!
                  IF ( Nrow==0 .OR. ncol==0 ) EXIT
                  IF ( irecf==2 ) ncol = 1
                  Incr = 1
                  nwdsx = Nrow*nwdsx
               ENDIF
               keyx = nwdsx
!
!     NWDSX IS NUMBER OF WORDS NEEDED PER COLUMN
!
               IF ( sparse ) THEN
!
!     SPARSE MATRIX INPUT (P4 = NON-ZERO)
!     (NOT CALLING FROM INPTT4 (IPTX=IPT2)
!
                  DO l = 1 , ncol
                     DO k = 1 , nwdsx
                        X(k) = 0.0
                     ENDDO
                     DO
                        READ (in) key , base
                        IF ( key<0 ) THEN
                           CALL pack(X,output,mcb)
                           EXIT
                        ELSE
                           READ (in) (X(k+base),k=1,key)
                        ENDIF
                     ENDDO
                  ENDDO
               ELSE
                  DO l = 1 , ncol
                     READ (in) key
                     IF ( key/=keyx ) GOTO 2200
                     IF ( key>lcor ) GOTO 2100
                     READ (in) (X(k),k=1,nwdsx)
                     CALL pack(X,output,mcb)
                     READ (in) key
                     imhere = 935
                     IF ( key>0 ) GOTO 2300
                  ENDDO
               ENDIF
               IF ( irecf==2 ) GOTO 1220
               keyx = 0
               READ (in) key
               IF ( key==keyx ) EXIT
               GOTO 2200
            ELSEIF ( key==0 ) THEN
               EXIT
            ELSE
!              EOR, EOF, KEY
!
               IF ( key>lcor ) GOTO 2100
               READ (in) (X(l),l=1,key)
               CALL write(output,X,key,0)
            ENDIF
         ENDDO
!
!     CLOSE OUTPUT DATA BLOCK WITH REWIND AND EOF
!
         CALL close(output,1)
!
!     WRITE TRAILER
!
         trl(1) = output
         CALL wrttrl(trl)
         CALL page2(-2)
         WRITE (nout,99014) Uim , name , in , kf
99014    FORMAT (A29,' 4139, DATA BLOCK ',2A4,' RETRIEVED FROM FORTRAN ','TAPE ',I2,' (DATA BLOCK COUNT =',I6,1H))
         IF ( nt(i,1)>1 ) THEN
            WRITE (nout,99015) Uwm
99015       FORMAT (A25,' 4140, SECONDARY VERSION OF DATA BLOCK HAS REPLACED',' EARLIER ONE.')
            CALL page2(-2)
         ELSE
            nnt = nnt - 1
         ENDIF
         GOTO 1300
      ENDDO
!
      ASSIGN 1300 TO ret
      nskip = 1
      GOTO 1600
   ENDIF
 1300 IF ( nnt>0 .OR. P1==msix .OR. P1==mete ) GOTO 1200
!
 1400 ASSIGN 1500 TO ret
   nskip = -1
   GOTO 1600
 1500 RETURN
!
!     SIMULATION OF SKPFIL (IN,NSKIP)
!
 1600 IF ( nskip<0 ) THEN
      REWIND in
   ELSEIF ( nskip==0 ) THEN
      GOTO 1700
   ENDIF
!
!     NSKIP = COMPLEMENT OF NSKIP.
!
   DO ns = 1 , nskip
      DO
         READ (in) key
         IF ( key<0 ) THEN
         ELSEIF ( key==0 ) THEN
            EXIT
         ELSE
!               EOR, EOF, KEY
!
            IF ( key>lcor ) GOTO 2100
            READ (in) (X(l),l=1,key)
         ENDIF
      ENDDO
   ENDDO
 1700 GOTO ret
!
!     ERRORS
!
 1800 WRITE (nout,99016) Ufm , iptx , output
99016 FORMAT (A23,' 4108, SUBROUTINE INPTT',A1,' UNABLE TO OPEN OUTPUT',' DATA BLOCK',I6)
   GOTO 2500
 1900 WRITE (nout,99017) Ufm , p3x , iptx , P3
99017 FORMAT (A23,' 4136, USER TAPE ID CODE -',2A4,'- DOES NOT MATCH ','THIRD INPUTT',A1,' DMAP PARAMETER -',2A4,2H-.)
   line = line + 1
   GOTO 2500
 2000 WRITE (nout,99018) Ufm , iptx , idhdrx
99018 FORMAT (A23,' 4134, MODULE INPUTT',A1,' - ILLEGAL TAPE CODE ','HEADER = ',7A4)
   GOTO 2500
 2100 WRITE (nout,99019) Ufm , lcor , key
99019 FORMAT (A23,' 2187, INSUFFICIENT WORKING CORE TO HOLD FORTRAN ','LOGICAL RECORD.',/5X,'LENGTH OF WORKING CORE =',I11,         &
             &',  LENGTH OF FORTRAN LOGICAL RECORD =',I11,1H.)
   line = line + 1
   GOTO 2500
 2200 WRITE (nout,99020) Sfm , key , keyx
99020 FORMAT (A25,' 2190, ILLEGAL VALUE FOR KEY =',I10,',   EXPECTED VALUE =',I11,1H.)
   IF ( key==2 .AND. keyx==3 ) WRITE (nout,99021)
99021 FORMAT (5X,'POSSIBLY DUE TO IMPROPER TAPE GENERATION PROCEDURE')
   GOTO 2500
 2300 WRITE (nout,99022) Sfm , key , imhere
99022 FORMAT (A25,' 2190, ILLEGAL VALUE FOR KEY =',I10,'.  IMHERE =',I4)
   GOTO 2500
 2400 WRITE (nout,99023) Ufm , P3
99023 FORMAT (A23,', ILLEGAL TAPE LABEL NAME -',2A4,'-  POSSIBLY ','THE 4TH PARAMETER OF INPTT4 IS IN ERROR')
!
 2500 line = line + 2
   CALL mesage(-61,lcor,subnam)
!
END SUBROUTINE inptt2
