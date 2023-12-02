!*==inptt2.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
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
   USE c_blank
   USE c_dsname
   USE c_packx
   USE c_system
   USE c_type
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
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
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
         iptx = ipt2
         ntrl = 8
         IF ( p4/=0 ) THEN
            IF ( p5==0 ) THEN
               WRITE (nout,99001) uwm
99001          FORMAT (A25,'. THE 4TH PARAMETER IN INPUTT2 MODULE IS NO LONGER ','USED.',/5X,                                       &
                      &'SPARSE MATRIX FLAG IS NOW THE 5TH PARAMETER',', A MOVE TO SYNCHRONIZE THE PARAMETERS USED IN OUTPUT2')
               p5 = p4
            ENDIF
         ENDIF
         sparse = .FALSE.
         IF ( p5/=0 ) sparse = .TRUE.
         IF ( p6(1)==msc ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
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
         spag_nextblock_1 = 2
      CASE (2)
         WRITE (nout,99002) uim , iptx
99002    FORMAT (A29,' FROM INPUTT',A1,'. USER INPUT TAPE IN MSC/OUTPUT2',' COMPATIBLE RECORDS')
         iptx = ipt4
         ntrl = 7
         irecf = 0
         sparse = .FALSE.
         IF ( p3(1)==bcdbin(1) .AND. p3(2)==bcdbin(2) ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( p3(1)==bcdbin(3) .AND. p3(2)==bcdbin(4) ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
         lcor = korsz(x) - nb
         IF ( lcor<=0 ) CALL mesage(-8,lcor,subnam)
!WKBNB
         IF ( ifirst==0 ) THEN
            CLOSE (UNIT=p2)
            OPEN (UNIT=p2,FILE=dsnames(p2),FORM='UNFORMATTED',STATUS='UNKNOWN')
            ifirst = 1
         ENDIF
!WKBNE
         oubuf = lcor + 1
         tapcod(1) = p3(1)
         tapcod(2) = p3(2)
         in = p2
         IF ( p1<mete .OR. p1==mtwo .OR. p1==mfor ) THEN
            WRITE (nout,99003) ufm , iptx , p1
99003       FORMAT (A23,' 4113, MODULE INPUTT',A1,' - ILLEGAL VALUE FOR ','FIRST PARAMETER =',I20)
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
!
         ELSEIF ( p1<mfor ) THEN
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
               IF ( key/=keyx ) THEN
                  spag_nextblock_1 = 14
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               READ (in) dx
               READ (in) key
               keyx = 7
               IF ( key/=keyx ) THEN
                  spag_nextblock_1 = 14
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               READ (in) idhdrx
               DO kf = 1 , 7
                  IF ( idhdrx(kf)/=idhdr(kf) ) THEN
                     spag_nextblock_1 = 12
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
               READ (in) key
               keyx = 2
               IF ( key/=keyx ) THEN
                  spag_nextblock_1 = 14
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               READ (in) p3x
               READ (in) key
               imhere = 815
               IF ( key>=0 ) THEN
                  spag_nextblock_1 = 15
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( p3x(1)/=p3(1) .OR. p3x(2)/=p3(2) ) THEN
                  spag_nextblock_1 = 11
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               ASSIGN 100 TO ret
               nskip = 1
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ELSE
               CALL page2(-2)
               WRITE (nout,99004) uwm , iptx
99004          FORMAT (A25,' 4137, ALL OUTPUT DATA BLOCKS FOR INPUTT',A1,' ARE PURGED.')
!     CLOSE (UNIT=IN)
               RETURN
            ENDIF
         ELSEIF ( p1==mtre ) THEN
!
!     OBTAIN LIST OF DATA BLOCKS ON FORTRAN TAPE.
!
            REWIND in
            READ (in) key
            keyx = 3
            IF ( key/=keyx ) THEN
               spag_nextblock_1 = 14
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            READ (in) dx
            READ (in) key
            keyx = 7
            IF ( key/=keyx ) THEN
               spag_nextblock_1 = 14
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            READ (in) idhdrx
            DO kf = 1 , 7
               IF ( idhdrx(kf)/=idhdr(kf) ) THEN
                  spag_nextblock_1 = 12
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            READ (in) key
            keyx = 2
            IF ( key/=keyx ) THEN
               spag_nextblock_1 = 14
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            READ (in) p3x
            READ (in) key
            imhere = 515
            IF ( key>=0 ) THEN
               spag_nextblock_1 = 15
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( p3x(1)/=p3(1) .OR. p3x(2)/=p3(2) ) THEN
               WRITE (nout,99005) uwm , p3x , iptx , p3
99005          FORMAT (A25,' 4135, USER TAPE ID CODE -',2A4,'- DOES NOT MATCH ','THIRD INPUTT',A1,' DMAP PARAMETER -',2A4,2H-.)
            ENDIF
            ASSIGN 60 TO ret
            nskip = 1
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( p1<=zero ) THEN
!
!     OPEN FORTRAN TAPE TO READ TAPE-LABEL WITHOUT REWIND.
!
            IF ( p1/=mone ) GOTO 40
            REWIND in
            READ (in) key
            keyx = 3
            IF ( key/=keyx ) THEN
               spag_nextblock_1 = 14
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            READ (in) dx
            READ (in) key
            keyx = 7
            IF ( key/=keyx ) THEN
               spag_nextblock_1 = 14
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            READ (in) idhdrx
            DO kf = 1 , 7
               IF ( idhdrx(kf)/=idhdr(kf) ) THEN
                  spag_nextblock_1 = 12
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            READ (in) key
            keyx = 2
            IF ( key/=keyx ) THEN
               spag_nextblock_1 = 14
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            READ (in) p3x
            READ (in) key
            imhere = 145
            IF ( key>=0 ) THEN
               spag_nextblock_1 = 15
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( p3x(1)/=p3(1) .OR. p3x(2)/=p3(2) ) THEN
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ASSIGN 40 TO ret
            nskip = 1
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
            i = 1
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
         READ (in) key
         keyx = 2
         IF ( key/=keyx ) THEN
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         READ (in) namex
         READ (in) key
         imhere = 115
         IF ( key>=0 ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ASSIGN 20 TO ret
         nskip = 1
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
!
 20      i = i + 1
         IF ( i<=p1 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
 40      SPAG_Loop_1_1: DO i = 1 , 5
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
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
                  IF ( key==0 ) EXIT SPAG_Loop_1_1
                  keyx = 2
                  IF ( key/=keyx ) THEN
                     spag_nextblock_1 = 14
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  READ (in) namex
                  READ (in) key
                  imhere = 163
                  IF ( key>=0 ) THEN
                     spag_nextblock_1 = 15
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
!
!     READ TRAILER RECORD.
!
                  READ (in) key
                  keyx = ntrl
                  IF ( key/=keyx ) THEN
                     spag_nextblock_1 = 14
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  READ (in) (trl(l),l=1,ntrl)
                  IF ( iptx==ipt2 ) irecf = trl(8)
                  READ (in) key
                  imhere = 165
                  IF ( key>=0 ) THEN
                     spag_nextblock_1 = 15
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
!
!     OPEN OUTPUT DATA BLOCK TO WRITE WITH REWIND.
!
                  CALL open(*160,output,x(oubuf),1)
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
                     IF ( key/=keyx ) THEN
                        spag_nextblock_1 = 14
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     READ (in) krec
                     imhere = 170
                     IF ( krec/=0 ) THEN
                        spag_nextblock_1 = 15
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
!
                     READ (in) key
                  ENDIF
                  keyx = 2
                  IF ( key<keyx ) THEN
                     spag_nextblock_1 = 14
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( key>lcor ) THEN
                     spag_nextblock_1 = 13
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  READ (in) (x(l),l=1,key)
                  CALL write(output,name,2,0)
                  IF ( key/=keyx ) CALL write(output,x(i3),key-2,0)
                  spag_nextblock_2 = 2
               CASE (2)
!
                  IF ( iptx==ipt2 ) THEN
                     spag_nextblock_2 = 4
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  READ (in) key
                  imhere = 205
                  IF ( key>=0 ) THEN
                     spag_nextblock_1 = 15
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  btyp = trl(5)
                  bform = 0
                  bcol = 0
                  nwd = nwds(btyp)
                  dp = btyp==2 .OR. btyp==4
                  CALL write(output,x,0,1)
                  spag_nextblock_2 = 3
               CASE (3)
                  SPAG_Loop_2_2: DO
                     READ (in) key
                     keyx = 1
                     IF ( key/=keyx ) THEN
                        spag_nextblock_1 = 14
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     READ (in) krec
                     IF ( krec==0 ) EXIT SPAG_Loop_2_2
!
!     MATRIX DATA BLOCK - MSC/STRING RECORD. (IPTX=IPT4)
!
!
                     bflag = -1
                     bcol = bcol + 1
                     SPAG_Loop_3_3: DO
                        READ (in) key
                        CALL putstr(blk)
                        imhere = 360
                        IF ( key<0 ) THEN
                           bflag = +1
                           bwrt = 0
                           CALL endput(blk)
                           EXIT SPAG_Loop_3_3
                        ELSEIF ( key==0 ) THEN
                           spag_nextblock_1 = 15
                           CYCLE SPAG_DispatchLoop_1
                        ELSE
!       NULL or EOR, ERR, KEY
!
                           bwrt = key/nwd
                           imhere = 370
                           IF ( bwrt>brav ) THEN
                              spag_nextblock_1 = 15
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
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
                     ENDDO SPAG_Loop_3_3
                  ENDDO SPAG_Loop_2_2
                  spag_nextblock_2 = 4
               CASE (4)
                  SPAG_Loop_2_4: DO
!
!     TABLE DATA BLOCK(S)
!
                     READ (in) key
                     IF ( key<0 ) THEN
                        CALL write(output,x,0,1)
                        IF ( iptx==ipt4 ) THEN
                           spag_nextblock_2 = 3
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                        IF ( irecf==0 ) THEN
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                        IF ( irecf==1 .OR. index>0 ) THEN
!
!     READ STRING FORMATTED MATRIX
!
                           IF ( irecf/=2 .OR. index/=2 ) THEN
                              index = 2
                              CALL makmcb(mcb(1),output,trl(3),trl(4),trl(5))
                              irow = 1
                              nrow = trl(3)
                              typin = trl(5)
                              typout = trl(5)
                              nwdsx = nwds(typout)
                              ncol = trl(2)
!
!     CHECK FOR NULL MATRIX
!
                              IF ( nrow==0 .OR. ncol==0 ) EXIT SPAG_Loop_2_4
                              IF ( irecf==2 ) ncol = 1
                              incr = 1
                              nwdsx = nrow*nwdsx
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
                                    x(k) = 0.0
                                 ENDDO
                                 SPAG_Loop_4_5: DO
                                    READ (in) key , base
                                    IF ( key<0 ) THEN
                                       CALL pack(x,output,mcb)
                                       EXIT SPAG_Loop_4_5
                                    ELSE
                                       READ (in) (x(k+base),k=1,key)
                                    ENDIF
                                 ENDDO SPAG_Loop_4_5
                              ENDDO
                           ELSE
                              DO l = 1 , ncol
                                 READ (in) key
                                 IF ( key/=keyx ) THEN
                                    spag_nextblock_1 = 14
                                    CYCLE SPAG_DispatchLoop_1
                                 ENDIF
                                 IF ( key>lcor ) THEN
                                    spag_nextblock_1 = 13
                                    CYCLE SPAG_DispatchLoop_1
                                 ENDIF
                                 READ (in) (x(k),k=1,nwdsx)
                                 CALL pack(x,output,mcb)
                                 READ (in) key
                                 imhere = 265
                                 IF ( key>0 ) THEN
                                    spag_nextblock_1 = 15
                                    CYCLE SPAG_DispatchLoop_1
                                 ENDIF
                              ENDDO
                           ENDIF
                           IF ( irecf==2 ) THEN
                              spag_nextblock_2 = 2
                              CYCLE SPAG_DispatchLoop_2
                           ENDIF
                           keyx = 0
                           READ (in) key
                           imhere = 285
                           IF ( key==keyx ) EXIT SPAG_Loop_2_4
                           spag_nextblock_1 = 14
                           CYCLE SPAG_DispatchLoop_1
                        ELSE
                           index = 1
                        ENDIF
                     ELSEIF ( key==0 ) THEN
                        EXIT SPAG_Loop_2_4
                     ELSE
!              EOR, EOF, KEY
!
                        IF ( key>lcor ) THEN
                           spag_nextblock_1 = 13
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        READ (in) (x(l),l=1,key)
                        CALL write(output,x,key,0)
                     ENDIF
                  ENDDO SPAG_Loop_2_4
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
                  WRITE (nout,99006) uim , name , in , namex
99006             FORMAT (A29,' 4105, DATA BLOCK ',2A4,' RETRIEVED FROM FORTRAN ','TAPE ',I2,/5X,'ORIGINAL NAME OF DATA BLOCK WAS ',&
                        & 2A4)
                  IF ( sparse .AND. ntrl==8 .AND. trl(8)/=0 ) WRITE (nout,99007) trl(2) , trl(3)
99007             FORMAT (1H+,55X,'(A SPARSE MATRIX',I6,2H X,I6,')')
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
!
         ENDDO SPAG_Loop_1_1
!
!     CLOSE FORTRAN TAPE WITHOUT REWIND.
!
         RETURN
 60      kf = 0
         spag_nextblock_1 = 5
      CASE (5)
         CALL page1
         line = line + 8
         WRITE (nout,99008) in
99008    FORMAT (//50X,'FILE CONTENTS ON FORTRAN UNIT ',I2,/51X,32(1H-),//54X,4HFILE,18X,4HNAME,//)
         spag_nextblock_1 = 6
      CASE (6)
         READ (in) key
         IF ( key==0 ) THEN
            REWIND in
            ASSIGN 40 TO ret
            nskip = 1
         ELSE
!     KEYX = 2
!     IF (KEY .NE. KEYX) GO TO 9918
            READ (in) namex
!     READ (IN) KEY
!     IF (KEY .GE. 0) GO TO 9919
            ASSIGN 80 TO ret
            nskip = 1
         ENDIF
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 80      kf = kf + 1
         line = line + 1
         WRITE (nout,99009) kf , namex
99009    FORMAT (53X,I5,18X,2A4)
         IF ( line>=nlpp ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
!
!     BEGIN SEARCH OF TAPE.
!
 100     kf = 0
         spag_nextblock_1 = 7
      CASE (7)
         READ (in) key
         IF ( key==0 ) THEN
!
            IF ( nnt<=0 ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL page2(-7)
            IF ( p1==mfiv .OR. p1==msix ) THEN
               WRITE (nout,99010) ufm
99010          FORMAT (A23,' 4142, ONE OR MORE DATA BLOCKS NOT FOUND ON USER ','TAPE')
            ELSE
               WRITE (nout,99011) uwm
99011          FORMAT (A25,' 4141, ONE OR MORE DATA BLOCKS NOT FOUND ON FORTRAN',' TAPE.')
            ENDIF
            DO i = 1 , 5
               IF ( nt(i,1)==0 ) THEN
                  WRITE (nout,99012) nt(i,2) , nt(i,3)
99012             FORMAT (20X,21HNAME OF DATA BLOCK = ,2A4)
               ENDIF
               IF ( iptx==ipt4 ) THEN
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            IF ( p1/=mfiv .AND. p1/=msix ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
         ELSE
!     KEYX = 2
!     IF (KEY .NE. KEYX) GO TO 9918
            READ (in) namex
            READ (in) key
            imhere = 835
            IF ( key>=0 ) THEN
               spag_nextblock_1 = 15
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            kf = kf + 1
!
            SPAG_Loop_1_6: DO i = 1 , 5
               spag_nextblock_3 = 1
               SPAG_DispatchLoop_3: DO
                  SELECT CASE (spag_nextblock_3)
                  CASE (1)
                     name(1) = nt(i,2)
                     name(2) = nt(i,3)
                     IF ( nt(i,1)<0 ) CYCLE
                     IF ( name(1)/=namex(1) .OR. name(2)/=namex(2) ) CYCLE
                     nt(i,1) = nt(i,1) + 1
                     IF ( nt(i,1)==1 .OR. p1==msix .OR. p1==mete ) THEN
                        READ (in) key
                        keyx = ntrl
                        IF ( key/=keyx ) THEN
                           spag_nextblock_1 = 14
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        READ (in) (trl(l),l=1,ntrl)
                        IF ( iptx==ipt2 ) irecf = trl(8)
                        READ (in) key
                        imhere = 855
                        IF ( key>=0 ) THEN
                           spag_nextblock_1 = 15
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
!
                        output = 200 + i
                        CALL open(*160,output,x(oubuf),1)
!
                        index = 0
!     IF (IPTX .EQ. IPT4) GO TO 890        ! FROM MSC/INPTT4
                        READ (in) key
                        IF ( iptx/=ipt2 ) THEN
                           keyx = 1
                           IF ( key==keyx ) THEN
                              spag_nextblock_1 = 14
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           READ (in) krec
                           imhere = 857
                           IF ( krec<0 ) THEN
                              spag_nextblock_1 = 15
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           READ (in) key
                        ENDIF
                        keyx = 2
                        IF ( key<keyx ) THEN
                           spag_nextblock_1 = 14
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        IF ( key>lcor ) THEN
                           spag_nextblock_1 = 13
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        READ (in) (x(l),l=1,key)
                        CALL write(output,name,2,0)
                        IF ( key/=keyx ) CALL write(output,x(i3),key-2,0)
                     ELSE
                        CALL page2(-3)
                        WRITE (nout,99013) uwm , name , kf , in
99013                   FORMAT (A25,' 4138, DATA BLOCK ,',2A4,' (DATA BLOCK COUNT =',I6,')  HAS PREVIOUSLY BEEN RETRIEVED FROM ',   &
                              & /36X,'FORTRAN TAPE ',I2,' AND WILL BE IGNORED.')
                        EXIT SPAG_Loop_1_6
                     ENDIF
                     spag_nextblock_3 = 2
                  CASE (2)
!
                     IF ( iptx==ipt2 ) THEN
                        spag_nextblock_3 = 4
                        CYCLE SPAG_DispatchLoop_3
                     ENDIF
                     READ (in) key
                     imhere = 875
                     IF ( key>0 ) THEN
                        spag_nextblock_1 = 15
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     btyp = trl(5)
                     bform = 0
                     bcol = 0
                     nwd = nwds(btyp)
                     dp = btyp==2 .OR. btyp==4
                     CALL write(output,0,0,1)
                     spag_nextblock_3 = 3
                  CASE (3)
                     SPAG_Loop_2_7: DO
                        READ (in) key
                        keyx = 1
                        IF ( key/=keyx ) THEN
                           spag_nextblock_1 = 14
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        READ (in) krec
                        IF ( krec==0 ) EXIT SPAG_Loop_2_7
!
!     MATRIX DATA BLOCK, IPTX = IPT4.  MSC/STRING RECORD
!
!
                        bflag = -1
                        bcol = bcol + 1
                        SPAG_Loop_3_8: DO
                           READ (in) key
                           CALL putstr(blk)
                           imhere = 1025
                           IF ( key<0 ) THEN
                              bflag = +1
                              bwrt = 0
                              CALL endput(blk)
                              EXIT SPAG_Loop_3_8
                           ELSEIF ( key==0 ) THEN
                              spag_nextblock_1 = 15
                              CYCLE SPAG_DispatchLoop_1
                           ELSE
!       NULL or EOR,  ERR, KEY
!
                              bwrt = key/nwd
                              imhere = 1030
                              IF ( bwrt>brav ) THEN
                                 spag_nextblock_1 = 15
                                 CYCLE SPAG_DispatchLoop_1
                              ENDIF
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
                        ENDDO SPAG_Loop_3_8
                     ENDDO SPAG_Loop_2_7
                     spag_nextblock_3 = 4
                  CASE (4)
                     SPAG_Loop_2_9: DO
!
!     TABLE DATA BLOCK(S)
!
                        READ (in) key
                        IF ( key<0 ) THEN
                           CALL write(output,x,0,1)
!     IF (IPTX .EQ. IPT4) GO TO 890
                           IF ( iptx==ipt4 ) THEN
                              spag_nextblock_3 = 3
                              CYCLE SPAG_DispatchLoop_3
                           ENDIF
                           IF ( irecf==0 ) THEN
                              spag_nextblock_3 = 2
                              CYCLE SPAG_DispatchLoop_3
                           ENDIF
                           IF ( irecf/=1 ) THEN
                              IF ( index<=0 ) THEN
                                 index = 1
                                 spag_nextblock_3 = 2
                                 CYCLE SPAG_DispatchLoop_3
                              ENDIF
                           ENDIF
!
!     READ STRING FORMATTED MATRIX
!
                           IF ( irecf/=2 .OR. index/=2 ) THEN
                              index = 2
                              CALL makmcb(mcb(1),output,trl(3),trl(4),trl(5))
                              irow = 1
                              nrow = trl(3)
                              typin = trl(5)
                              typout = trl(5)
                              nwdsx = nwds(typout)
                              ncol = trl(2)
!
!     CHECK FOR NULL MATRIX
!
                              IF ( nrow==0 .OR. ncol==0 ) EXIT SPAG_Loop_2_9
                              IF ( irecf==2 ) ncol = 1
                              incr = 1
                              nwdsx = nrow*nwdsx
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
                                    x(k) = 0.0
                                 ENDDO
                                 SPAG_Loop_4_10: DO
                                    READ (in) key , base
                                    IF ( key<0 ) THEN
                                       CALL pack(x,output,mcb)
                                       EXIT SPAG_Loop_4_10
                                    ELSE
                                       READ (in) (x(k+base),k=1,key)
                                    ENDIF
                                 ENDDO SPAG_Loop_4_10
                              ENDDO
                           ELSE
                              DO l = 1 , ncol
                                 READ (in) key
                                 IF ( key/=keyx ) THEN
                                    spag_nextblock_1 = 14
                                    CYCLE SPAG_DispatchLoop_1
                                 ENDIF
                                 IF ( key>lcor ) THEN
                                    spag_nextblock_1 = 13
                                    CYCLE SPAG_DispatchLoop_1
                                 ENDIF
                                 READ (in) (x(k),k=1,nwdsx)
                                 CALL pack(x,output,mcb)
                                 READ (in) key
                                 imhere = 935
                                 IF ( key>0 ) THEN
                                    spag_nextblock_1 = 15
                                    CYCLE SPAG_DispatchLoop_1
                                 ENDIF
                              ENDDO
                           ENDIF
                           IF ( irecf==2 ) THEN
                              spag_nextblock_3 = 2
                              CYCLE SPAG_DispatchLoop_3
                           ENDIF
                           keyx = 0
                           READ (in) key
                           IF ( key==keyx ) EXIT SPAG_Loop_2_9
                           spag_nextblock_1 = 14
                           CYCLE SPAG_DispatchLoop_1
                        ELSEIF ( key==0 ) THEN
                           EXIT SPAG_Loop_2_9
                        ELSE
!              EOR, EOF, KEY
!
                           IF ( key>lcor ) THEN
                              spag_nextblock_1 = 13
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           READ (in) (x(l),l=1,key)
                           CALL write(output,x,key,0)
                        ENDIF
                     ENDDO SPAG_Loop_2_9
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
                     WRITE (nout,99014) uim , name , in , kf
99014                FORMAT (A29,' 4139, DATA BLOCK ',2A4,' RETRIEVED FROM FORTRAN ','TAPE ',I2,' (DATA BLOCK COUNT =',I6,1H))
                     IF ( nt(i,1)>1 ) THEN
                        WRITE (nout,99015) uwm
99015                   FORMAT (A25,' 4140, SECONDARY VERSION OF DATA BLOCK HAS REPLACED',' EARLIER ONE.')
                        CALL page2(-2)
                     ELSE
                        nnt = nnt - 1
                     ENDIF
                     GOTO 120
                  END SELECT
               ENDDO SPAG_DispatchLoop_3
            ENDDO SPAG_Loop_1_6
!
            ASSIGN 120 TO ret
            nskip = 1
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 120     IF ( nnt>0 .OR. p1==msix .OR. p1==mete ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
!
         ASSIGN 140 TO ret
         nskip = -1
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 140     RETURN
      CASE (9)
!
!     SIMULATION OF SKPFIL (IN,NSKIP)
!
         IF ( nskip<0 ) THEN
            REWIND in
         ELSEIF ( nskip==0 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     NSKIP = COMPLEMENT OF NSKIP.
!
         DO ns = 1 , nskip
            SPAG_Loop_2_11: DO
               READ (in) key
               IF ( key<0 ) THEN
               ELSEIF ( key==0 ) THEN
                  EXIT SPAG_Loop_2_11
               ELSE
!               EOR, EOF, KEY
!
                  IF ( key>lcor ) THEN
                     spag_nextblock_1 = 13
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  READ (in) (x(l),l=1,key)
               ENDIF
            ENDDO SPAG_Loop_2_11
         ENDDO
         spag_nextblock_1 = 10
      CASE (10)
         GOTO ret
!
!     ERRORS
!
 160     WRITE (nout,99016) ufm , iptx , output
99016    FORMAT (A23,' 4108, SUBROUTINE INPTT',A1,' UNABLE TO OPEN OUTPUT',' DATA BLOCK',I6)
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
      CASE (11)
         WRITE (nout,99017) ufm , p3x , iptx , p3
99017    FORMAT (A23,' 4136, USER TAPE ID CODE -',2A4,'- DOES NOT MATCH ','THIRD INPUTT',A1,' DMAP PARAMETER -',2A4,2H-.)
         line = line + 1
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
      CASE (12)
         WRITE (nout,99018) ufm , iptx , idhdrx
99018    FORMAT (A23,' 4134, MODULE INPUTT',A1,' - ILLEGAL TAPE CODE ','HEADER = ',7A4)
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
      CASE (13)
         WRITE (nout,99019) ufm , lcor , key
99019    FORMAT (A23,' 2187, INSUFFICIENT WORKING CORE TO HOLD FORTRAN ','LOGICAL RECORD.',/5X,'LENGTH OF WORKING CORE =',I11,      &
                &',  LENGTH OF FORTRAN LOGICAL RECORD =',I11,1H.)
         line = line + 1
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
      CASE (14)
         WRITE (nout,99020) sfm , key , keyx
99020    FORMAT (A25,' 2190, ILLEGAL VALUE FOR KEY =',I10,',   EXPECTED VALUE =',I11,1H.)
         IF ( key==2 .AND. keyx==3 ) WRITE (nout,99021)
99021    FORMAT (5X,'POSSIBLY DUE TO IMPROPER TAPE GENERATION PROCEDURE')
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
      CASE (15)
         WRITE (nout,99022) sfm , key , imhere
99022    FORMAT (A25,' 2190, ILLEGAL VALUE FOR KEY =',I10,'.  IMHERE =',I4)
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
      CASE (16)
         WRITE (nout,99023) ufm , p3
99023    FORMAT (A23,', ILLEGAL TAPE LABEL NAME -',2A4,'-  POSSIBLY ','THE 4TH PARAMETER OF INPTT4 IS IN ERROR')
         spag_nextblock_1 = 17
      CASE (17)
!
         line = line + 2
         CALL mesage(-61,lcor,subnam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE inptt2
