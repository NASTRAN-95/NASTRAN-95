!*==xsort.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xsort
!
!     SORT READS BULK DATA CARDS FROM THE INPUT TAPE, ADJUSTS THE
!     FIELDS, PERFORMS AN ALPHA-NUMERIC SORT ON THE CARD IMAGES FROM
!     LEFT TO RIGHT, INSERTS CONTINUATION CARDS IN THEIR PROPER
!     POSITION, AND PLACES THE RESULTING SORTED IMAGES ON THE NEW
!     PROBLEM TAPE.
!
   IMPLICIT NONE
   USE C_MACHIN
   USE C_OUTPUT
   USE C_STAPID
   USE C_SYSTEM
   USE C_XECHOX
   USE C_XMSSG
   USE C_XSRTCM
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(4) :: bk , mk
   INTEGER :: blanx , ccnt , fcnt , i , ibk3 , ibk4 , ibrana , ibranb , ibranc , ibrand , ibrane , ibranf , ibufbg , ibuflg ,       &
            & iccbrk , iccflg , iccnt , iconlg , iend , iflg , ii , ik2 , imax , imin , in1 , index , inf , irestr , iseq , itape , &
            & itst , itst1 , itst2 , j , j1 , j1st , j2 , jj , jn , jo , jtape , k , k1 , k2 , keep , ki , kin , kk , kop , kp ,    &
            & ktape , ktarsw , kx , l47 , ldup , linf , m , mkb , mkc , mkd , mke , mx3 , my1 , my2 , my3 , my4 , my5 , my6 , mz1 , &
            & n1 , n2 , n3 , nbuf2 , nbuf3 , nbuf4 , ncnt , ni , nl , notsor , nshift
   INTEGER , DIMENSION(3) , SAVE :: cdcnt
   LOGICAL :: dec
   INTEGER , DIMENSION(32) , SAVE :: headn , heads , headu
   INTEGER , DIMENSION(2) , SAVE :: iblkda , nsort
   INTEGER , DIMENSION(20) :: ibuf1 , ibuf2
   INTEGER , DIMENSION(2) :: ibuf1a , ibuf2a , ibuf3 , iiend , kparnt
   INTEGER , SAVE :: idup , iend1 , iend2 , iend3 , iend4 , iend5 , iend6 , iok , iptp , itape1 , itape2 , itape3 , itape4 ,        &
                   & itape5 , nptp , umf
   INTEGER :: nx , ny , optp , pid , ptst , sfta , starsw , trial , tst
   EXTERNAL andf , close , crdflg , eof , extint , initco , intext , khrfn1 , khrfn3 , khrfn4 , korsz , mesage , open , orf , page ,&
          & page2 , read , rewind , skpfil , umffd , umftrn , write , xbcdbi , xfadj , xfadj1 , xprety , xread , xrecps
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
   INTEGER :: spag_nextblock_4
   !>>>>EQUIVALENCE (Bk(1),Bkmsk1(5)) , (Mk(1),Bimsk2(2)) , (Mkb,Bimsk5(1)) , (Inf,Bimsk2(1)) , (Sfta,Shifts(2)) , (Mkd,Bimsk2(2)) ,     &
!>>>>    & (Mke,Bimsk5(2)) , (Mkc,Bimsk4(1))
   !>>>>EQUIVALENCE (Blanx,Bkmsk1(8))
   DATA headu/10*4H     , 4H I N , 4H P U , 4H T   , 4H B U , 4H L K , 4H   D , 4H A T , 4H A   , 4H D E , 4H C K , 4H   E ,        &
      & 4H C H , 4H O   , 9*4H    /
   DATA heads/11*4H     , 4H S O , 4H R T , 4H E D , 4H   B , 4H U L , 4H K   , 4H D A , 4H T A , 4H   E , 4H C H , 4H O   ,        &
       &10*4H    /
   DATA headn/3*4H     , 4H     , 4H     , 4H     , 4H .   , 4H 1   , 4H..   , 4H 2   , 4H..   , 4H 3   , 4H..   , 4H 4   , 4H..   ,&
       &4H 5   , 4H..   , 4H 6   , 4H..   , 4H 7   , 4H..   , 4H 8   , 4H..   , 4H 9   , 4H..   , 4H10   , 4H.    , 5*4H    /
   DATA cdcnt/4HCARD , 4HCOUN , 4HT   / , nsort/4HXSOR , 4HT   /
!     DATA BK/4H000 ,4H00  ,4H0   ,4H    /
!     DATA (MK(I),I=1,4)/O777777007777,O777700007777,O770000007777,O0/
!     DATA MKA,MKB,INF,SFTA/O000000777777,O377777777777,O777777777777,6/
!     DATA MKC/O007777777777/,MKD/O777777007777/,MKE/O377777007777/
   DATA iend1 , iend2/4HENDD , 4HATA /
   DATA iend3 , iend4/4HENDA , 4HTA  /
   DATA iend5 , iend6/4HEND  , 4HDATA/
!     DATA STAR,PLUS,DOLLAR,STARL/4H000*,4H+000,4H$000,4H*000/
   DATA iblkda/4HBULK , 4HDATA/ , iptp/4HOPTP/ , nptp/4HNPTP/
   DATA itape1 , itape2 , itape3 , itape4 , itape5/301 , 302 , 303 , 304 , 305/
   DATA umf/4HUMF /
   DATA idup , iok/4HDUPL , 4HOK  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
!     XSORT MAY NOT WORK PROPERLY IN ALL UNIX MACHINES, WHICH FOLLOW
!     THE VAX LINE.
!
         dec = Mach==5 .OR. Mach==6 .OR. Mach==21
         IF ( dec .AND. Lpch/=77 ) WRITE (Outtap,99001) Uwm
99001    FORMAT (A25,', SWITCHING TO OLD XSORT VIA DIAG 42 HAS NOT BEEN ','THOROUGHLY TESTED',/5X,'FOR THE UNIX MACHINES.')
!
!     INITIALIZE XSORT AND TURN ON FREE-FIELD FLAG FOR XREAD
!
         Ffflag = 1234
         Echou = 0
         Echos = 0
         Echop = 0
         iend = 0
         iseq = 0
         iccbrk = 0
         notsor = 0
         optp = iptp
         kin = 0
         irestr = -Iapprc
         IF ( Kumf<=0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         kin = 1
         CALL open(*40,umf,Buf(1),2)
         SPAG_Loop_1_1: DO
!
!     FIND PARTICULAR BULK DATA FILE ON UMF AS REQUESTED BY USER
!
            CALL read(*20,*60,umf,pid,1,1,iflg)
            IF ( Kumf<pid ) EXIT SPAG_Loop_1_1
            IF ( Kumf==pid ) THEN
               CALL close(umf,2)
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ELSE
               CALL skpfil(umf,1)
            ENDIF
         ENDDO SPAG_Loop_1_1
 20      WRITE (Outtap,99002) Ufm , Kumf
99002    FORMAT (A23,' 201, REQUESTED BULK DATA DECK',I8,' NOT ON USER ','MASTER FILE.')
         CALL page2(2)
         Nogo = -1
         CALL close(umf,1)
         RETURN
!
 40      WRITE (Outtap,99003) Sfm
99003    FORMAT (A25,' 202, UMF COULD NOT BE OPENED')
         spag_nextblock_1 = 22
         CYCLE SPAG_DispatchLoop_1
 60      WRITE (Outtap,99004) Sfm
99004    FORMAT (A25,' 203, ILLEGAL EOR ON UMF')
         spag_nextblock_1 = 22
         CYCLE SPAG_DispatchLoop_1
      CASE (2)
!
         CALL initco
         IF ( Iecho>=0 ) THEN
            Echou = andf(Iecho,1)
            Echos = andf(Iecho,2)
            Echop = andf(Iecho,4)
            IF ( Icpflg/=0 ) Echos = 1
         ENDIF
         ASSIGN 540 TO ibrana
         ASSIGN 160 TO ibranb
         ASSIGN 480 TO ibranf
!
!     SET ASSIGN GO TO SWITCHES FOR MACHINE CONFIGURATIONS
!     THE 8 BIT CHARACTER BYTE OF THE 360 WILL HOLD THE INTERNAL
!     CHARACTER CODE (MAX=37) WITHOUT USE OF THE 1ST BIT POSITION -
!     THE OTHER 3 MACHINES HAVE 6 BIT CHARACTERS THEREFORE A SHIFT RIGHT
!     OF ONE MUST BE DONE TO REMOVE A POSSIBLE BIT FROM THE SIGN
!     POSITION THE FOLLOWING ASSIGNS SET THOSE BRANCHES BASED ON MACHINE
!
         IF ( Mach==2 .OR. dec ) THEN
            ASSIGN 68 TO mx3
            ASSIGN 140 TO my1
            ASSIGN 360 TO my2
            ASSIGN 360 TO my3
            ASSIGN 420 TO my4
            ASSIGN 420 TO my5
            ASSIGN 240 TO my6
            ASSIGN 100 TO mz1
            linf = orf(Is,1)
         ELSE
            ASSIGN 66 TO mx3
            ASSIGN 120 TO my1
            ASSIGN 180 TO my2
            ASSIGN 340 TO my3
            ASSIGN 380 TO my4
            ASSIGN 400 TO my5
            ASSIGN 220 TO my6
            ASSIGN 80 TO mz1
            linf = 0
            nshift = 1
!
!     SET NSHIFT TO ZERO FOR UNIVAC ASCII VERSION ONLY (NOT FORTRAN 5)
!
            IF ( Mach==3 ) nshift = 0
         ENDIF
!
!     START WORKING SORT BUFFER BELOW GINO I/O BUFFERS
!
         ii = 5*Ibufsz + 1
         ibufbg = ii + 42
         ibuflg = korsz(Buf) - 21
         IF ( ibuflg-ibufbg<210 ) CALL mesage(-8,ibufbg+210-ibuflg,nsort)
         itape = itape1
         jtape = itape2
!
!     OPEN ITAPE4 AND ITAPE5
!     (4 CONTAINS CONTINUATIONS, 5 CONTAINS ALTERS)
!
         nbuf3 = 3*Ibufsz + 1
         CALL open(*660,itape4,Buf(nbuf3),1)
         nbuf4 = 4*Ibufsz + 1
         CALL open(*660,itape5,Buf(nbuf4),1)
!
!     A BUFFER LINE IS 20 WORDS OF CARD IMAGE PLUS A 1 WORD POINTER TO
!     THE NEXT IMAGE IN THE SORT SEQUENCE - A ZERO POINTER INDICATES
!     THE LAST IMAGE (LARGEST IN SORT)
!     INITIALIZE WORKING BUFFER - 1ST LINE ZEROS, 2ND LINE ALL BITS
!
         k = ii + 19
         DO j = ii , k
            Buf(j) = linf
            Buf(j+21) = inf
         ENDDO
         Buf(ii+41) = 0
!
!     SET UP UNSORTED HEADING
!
         DO j = 1 , 32
            Head1(j) = headu(j)
            Head3(j) = headn(j)
         ENDDO
         Head2(4) = headn(1)
         iccnt = 0
         IF ( Echou/=0 ) CALL page
!
!     OPEN ITAPE (LOCATION FOR EACH SORTED CORE LOAD AS ITS FORCED TO
!     EMPTY
!
         CALL open(*660,itape,Buf(1),1)
         spag_nextblock_1 = 3
      CASE (3)
         Buf(ii+20) = 1
         k = ii
         ncnt = 2
!
!     LOOP TO INPUT AND SORT CARD IMAGES - USES OPEN CORE FOR SORTED
!     IMAGES
!
         DO n1 = ibufbg , ibuflg , 21
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  n2 = n1 + 19
                  n3 = n2 + 1
                  SPAG_Loop_2_2: DO
                     CALL xread(*780,Buf(n1))
                     iccnt = iccnt + 1
                     IF ( Echou/=0 ) THEN
                        CALL page2(-1)
                        WRITE (Outtap,99022) (Buf(i),i=n1,n2)
                     ENDIF
!
!     IGNORE BLANK CARDS
!
                     IF ( Buf(n1)/=blanx .OR. Buf(n1+1)/=blanx ) THEN
!
!     LEFT ADJUST FIELD 1
!
                        CALL xfadj1(Buf(n1),lshift,0)
!
!     TEST FOR END OF INPUT DATA STREAM (ENDDATA)
!
                        iiend(1) = iend1
                        iiend(2) = iend2
                        IF ( Buf(n1)==iend1 .AND. Buf(n1+1)==iend2 ) THEN
                           spag_nextblock_1 = 4
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        iiend(1) = iend3
                        iiend(2) = iend4
                        IF ( Buf(n1)==iend3 .AND. Buf(n1+1)==iend4 ) THEN
                           spag_nextblock_1 = 4
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        iiend(1) = iend5
                        iiend(2) = iend6
                        IF ( Buf(n1)==iend5 .AND. Buf(n1+1)==iend6 ) THEN
                           spag_nextblock_1 = 4
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
!
!     IS THIS A CONTINUATION, COMMENT, OR DELETE CARD
!
                        IF ( .NOT.dec ) tst = andf(mk(3),Buf(n1))
                        IF ( dec ) tst = khrfn1(Bkmsk2,1,Buf(n1),1)
!
!     WRITE CONTINUATIONS ON ITAPE4
!
                        IF ( tst==Starl .OR. tst==Plus ) THEN
!
!     CONTINUATION CARD - PUT ON ITAPE4
!
                           CALL write(itape4,Buf(n1),20,1)
                        ELSE
!
!     IGNORE COMMENT CARDS
!
                           IF ( tst==Dollar ) CYCLE
!
!     WRITE DELETES ON ITAPE5
!
                           IF ( tst==Slash ) THEN
!
!     BULK DATA DELETE CARD - PUT ON ITAPE5
!
!     TEST FOR EXTRANEOUS DATA IN FIELD 1 OF DELETE CARD
!     AND WRITE OUT TO SCRATCH FILE
!
                              IF ( .NOT.dec ) itst1 = andf(Buf(n1),Bimsk1(6))
                              IF ( dec ) itst1 = andf(Buf(n1),Bimsk1(1))
                              itst2 = andf(Buf(n1+1),Mbit4)
                              ibk3 = andf(bk(3),Mbit4)
                              ibk4 = andf(bk(4),Mbit4)
                              IF ( itst1/=ibk3 .OR. itst2/=ibk4 ) THEN
                                 CALL page2(2)
                                 WRITE (Outtap,99005) Ufm
99005                            FORMAT (A23,' 221, EXTRANEOUS DATA IN FIELD 1 OF BULK DATA ','DELETE CARD.')
                                 Nogo = -2
                              ENDIF
                              CALL xfadj1(Buf(n1+2),rshift,0)
                              CALL xbcdbi(Buf(n1+2))
                              CALL xfadj1(Buf(n1+4),rshift,0)
                              CALL xbcdbi(Buf(n1+4))
                              Buf(n1+4) = Buf(n1+5)
                              CALL write(itape5,Buf(n1+3),2,1)
                              CYCLE
                           ELSE
!
!     IF A STAR IS FOUND IN FIELD 1, MOVE IT TO COLUMN 8
!
                              ny = 4
                              DO j = 1 , 2
                                 nx = n1 + 2 - j
                                 tst = Buf(nx)
                                 DO i = 1 , ny
                                    IF ( .NOT.dec ) ptst = andf(Mka,tst)
                                    IF ( dec ) ptst = khrfn1(Bkmsk2,4,tst,4)
                                    IF ( ptst/=bk(1) ) GOTO 62
                                    IF ( .NOT.dec ) tst = rshift(tst,sfta)
                                    IF ( dec ) tst = khrfn3(Bkmsk2,tst,1,0)
                                 ENDDO
                                 ny = 3
                              ENDDO
                              GOTO 64
                           ENDIF
!
!     STARSW = 0 FOR A SINGLE FIELD CARD (NO STAR)
!            = 1 FOR A DOUBLE FIELD CARD (W/ STAR)
!
 62                        starsw = 0
                           IF ( ptst==Star ) THEN
                              starsw = 1
                              IF ( j/=1 .OR. i/=1 ) THEN
                                 IF ( dec ) THEN
                                    Buf(nx) = khrfn1(Buf(nx),5-i,bk(i),5-i)
                                    Buf(n1+1) = khrfn1(Buf(n1+1),4,Star,4)
                                 ELSE
                                    Buf(nx) = orf(andf(mk(i),Buf(nx)),bk(i))
                                    Buf(n1+1) = orf(andf(mk(1),Buf(n1+1)),Star)
                                 ENDIF
                              ENDIF
                           ENDIF
 64                        CALL xfadj(Buf(n1+2),starsw,ny)
                           CALL extint(Buf(n1))
!
!
!     START SORT LOGIC
!
!     WITHOUT THE FOLLOWING CARD, XSORT WILL ASSUME SOME DEGREE OF SORT
!     EXISTS (I.E.,THE NEXT CARD WILL FOLLOW THE PREVIOUS CARD, MORE
!     OFTEN THAN NOT)
!     K  = II  (THIS CARD WILL FORCE SORT TO BEGINNING OF CHAIN)
!
                           kp = 0
                           EXIT SPAG_Loop_2_2
                        ENDIF
                     ENDIF
                  ENDDO SPAG_Loop_2_2
                  spag_nextblock_2 = 2
               CASE (2)
!
!     K TYPE SUBSCRIPTS REFER TO POSITIONS AND ITEMS IN THE SORTED
!     TABLE CURRENTLY BEING BUILT
!     N TYPE SUBSCRIPTS REFER TO ITEMS ABOUT THE NEWEST CARD IN
!
                  fcnt = 1
                  ni = 0
                  ki = 0
                  nx = n1
                  spag_nextblock_2 = 3
               CASE (3)
!
!     THE RIGHT SHIFT IN THE FOLLOWING CODE IS USED TO AVOID THE
!     NEGATIVE SIGN PROBLEM WHICH WOULD REVERSE THE SORT ORDER ON SOME
!     MACHINES.
!     (NOTE THAT THE SORT COMPARES CAN BE MADE BOTH WITH OR WITHOUT
!     THE SIGN SHIFT DEPENDING ON THE MACHINES CHARACTER CONFIG)
!
                  kx = k
                  spag_nextblock_2 = 5
                  CYCLE SPAG_DispatchLoop_2
               CASE (4)
                  IF ( Buf(nx)==Buf(kx) ) THEN
                     spag_nextblock_2 = 8
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  IF ( Buf(nx)==bk(4) ) THEN
                     spag_nextblock_2 = 7
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  IF ( Buf(kx)==bk(4) ) THEN
                     spag_nextblock_2 = 6
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  spag_nextblock_2 = 5
               CASE (5)
                  GOTO mx3
 66               IF ( rshift(Buf(nx),nshift)<rshift(Buf(kx),nshift) ) THEN
                     spag_nextblock_2 = 7
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  IF ( rshift(Buf(nx),nshift)/=rshift(Buf(kx),nshift) ) THEN
                     spag_nextblock_2 = 6
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  spag_nextblock_2 = 8
                  CYCLE SPAG_DispatchLoop_2
 68               IF ( dec ) THEN
                     IF ( rshift(khrfn4(Buf(nx)),1)<rshift(khrfn4(Buf(kx)),1) ) THEN
                        spag_nextblock_2 = 7
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                     IF ( rshift(khrfn4(Buf(nx)),1)==rshift(khrfn4(Buf(kx)),1) ) THEN
                        IF ( rshift(lshift(khrfn4(Buf(nx)),1),1)<rshift(lshift(khrfn4(Buf(kx)),1),1) ) THEN
                           spag_nextblock_2 = 7
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                        IF ( rshift(lshift(khrfn4(Buf(nx)),1),1)==rshift(lshift(khrfn4(Buf(kx)),1),1) ) THEN
                           spag_nextblock_2 = 8
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDIF
                  ELSE
                     IF ( Buf(nx)<Buf(kx) ) THEN
                        spag_nextblock_2 = 7
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                     IF ( Buf(nx)<=Buf(kx) ) THEN
                        spag_nextblock_2 = 8
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                  ENDIF
                  spag_nextblock_2 = 6
               CASE (6)
!
!     GO ON, LOOK AT NEXT ITEM IN THE SORTED TABLE
!
                  kp = k
                  k = Buf(k+20)*21 + ii
                  IF ( nx==n1 ) THEN
                     spag_nextblock_2 = 3
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
               CASE (7)
!
!     CARD POSITION FOUND IN SORT, SET THE CHAINING POINTER
!
                  IF ( kp==0 ) THEN
                     k = ii
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
                  ELSE
                     Buf(n3) = Buf(kp+20)
                     Buf(kp+20) = ncnt
                     k = kp
                     ncnt = ncnt + 1
                     CYCLE
                  ENDIF
               CASE (8)
                  SPAG_Loop_2_3: DO
!
!     TWO FIELDS EQUAL - SLIDE TO NEXT FIELD ON CARD
!
                     fcnt = fcnt + 1
                     nx = nx + 1
                     kx = kx + 1
                     IF ( fcnt==1 ) THEN
                        spag_nextblock_1 = 21
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( fcnt==3 ) THEN
                        IF ( starsw==ktarsw ) THEN
                           spag_nextblock_2 = 4
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                        k1 = 0
                        k2 = 0
                        IF ( dec ) THEN
                           IF ( khrfn1(Bkmsk2,1,Buf(nx),1)/=Bkmsk1(4) ) k1 = 1
                           IF ( khrfn1(Bkmsk2,1,Buf(kx),1)/=Bkmsk1(4) ) k2 = 1
                        ELSE
                           IF ( andf(mk(3),Buf(nx))/=Bkmsk1(4) ) k1 = 1
                           IF ( andf(mk(3),Buf(kx))/=Bkmsk1(4) ) k2 = 1
                        ENDIF
                        spag_nextblock_2 = 9
                        CYCLE SPAG_DispatchLoop_2
                     ELSEIF ( fcnt==4 .OR. fcnt==6 .OR. fcnt==8 .OR. fcnt==10 .OR. fcnt==12 .OR. fcnt==14 .OR. fcnt==16 .OR.        &
                            & fcnt==18 ) THEN
                        spag_nextblock_2 = 4
                        CYCLE SPAG_DispatchLoop_2
                     ELSEIF ( fcnt==5 .OR. fcnt==9 .OR. fcnt==13 .OR. fcnt==17 ) THEN
                        IF ( starsw/=ktarsw ) EXIT SPAG_Loop_2_3
                        IF ( starsw==0 ) EXIT SPAG_Loop_2_3
                        spag_nextblock_2 = 4
                        CYCLE SPAG_DispatchLoop_2
                     ELSEIF ( fcnt==7 .OR. fcnt==15 ) THEN
                        EXIT SPAG_Loop_2_3
                     ELSEIF ( fcnt==11 ) THEN
                        IF ( starsw==ktarsw ) EXIT SPAG_Loop_2_3
                        spag_nextblock_2 = 7
                        CYCLE SPAG_DispatchLoop_2
                     ELSEIF ( fcnt==19 ) THEN
                        spag_nextblock_2 = 7
                        CYCLE SPAG_DispatchLoop_2
                     ELSE
                        ktarsw = 0
                        IF ( .NOT.dec ) itst = andf(Mka,Buf(k+1))
                        IF ( dec ) itst = khrfn1(Bkmsk2,4,Buf(k+1),4)
                        IF ( itst==Star ) ktarsw = 1
                        IF ( starsw==ktarsw ) THEN
                           spag_nextblock_2 = 5
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
!
!     IF ONE MEMBER OF THE 2ND FIELD HAS A STAR AND THE OTHER DOES NOT,
!     DELETE STARS FOR THE COMPARE
!
                        IF ( dec ) THEN
                           in1 = rshift(khrfn4(khrfn1(Buf(nx),4,Bkmsk2,1)),1)
                           ik2 = rshift(khrfn4(khrfn1(Buf(kx),4,Bkmsk2,1)),1)
                        ELSE
                           in1 = rshift(andf(mkd,Buf(nx)),1)
                           ik2 = rshift(andf(mkd,Buf(kx)),1)
                        ENDIF
                        IF ( in1==ik2 ) THEN
                           IF ( dec ) THEN
                              in1 = rshift(lshift(khrfn4(khrfn1(Buf(nx),4,Bkmsk2,1)),1),1)
                              ik2 = rshift(lshift(khrfn4(khrfn1(Buf(kx),4,Bkmsk2,1)),1),1)
                           ELSE
                              in1 = andf(mke,Buf(nx))
                              ik2 = andf(mke,Buf(kx))
                           ENDIF
                           IF ( in1==ik2 ) CYCLE
                        ENDIF
                        IF ( in1<ik2 ) THEN
                           spag_nextblock_2 = 7
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                        spag_nextblock_2 = 6
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                  ENDDO SPAG_Loop_2_3
!
!     INCREMENT FIELD LOCATIONS IF FIELD TYPES DID NOT MATCH
!
                  IF ( ni<ki ) THEN
                     kx = kx + ki
                     ki = 0
                  ELSEIF ( ni/=ki ) THEN
                     nx = nx + ni
                     ni = 0
                  ENDIF
!
!     ADJUST FIELDS RIGHT OR LEFT AS REQUIRED
!
                  CALL xfadj(Buf(nx),starsw,k1)
                  CALL xfadj(Buf(kx),ktarsw,k2)
                  spag_nextblock_2 = 9
               CASE (9)
                  IF ( starsw<ktarsw ) THEN
                     ki = 2
                     IF ( k1+k2/=2 ) THEN
                        kx = kx + 2
                        ki = 0
                     ENDIF
                  ELSEIF ( starsw/=ktarsw ) THEN
                     ni = 2
                     IF ( k1+k2/=2 ) THEN
                        nx = nx + 2
                        ni = 0
                     ENDIF
                  ENDIF
                  spag_nextblock_2 = 4
                  CYCLE SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
!
!     END OF BIG SORT LOOP
!
         ENDDO
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
!
!
!     SET (ENDDATA) CARD FOUND FLAG
!
         iend = -1
         IF ( Echou==1 ) THEN
            CALL page2(2)
            WRITE (Outtap,99006) iccnt
99006       FORMAT (//24X,12HTOTAL COUNT=,I5)
         ENDIF
!
!     TEST FOR COLD-START WITH NO BULK DATA
!
         IF ( iccnt<=1 .AND. irestr<=0 .AND. Kumf<=0 ) THEN
            IF ( Iapprc/=1 ) THEN
               IF ( Isubs==0 ) THEN
                  CALL page2(2)
                  WRITE (Outtap,99007) Ufm
99007             FORMAT (A23,' 204, COLD START NO BULK DATA.')
                  Nogo = -2
                  RETURN
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
!
!
!     IF MODIFIED RESTART - TURN ON SORT ECHO
!
!
!     THIS SECTION UNCHAINS THE SORTED TABLE AND WRITES A CORE LOAD,
!     IN ITS ACTUAL ORDER, ONTO A MERGE SCRATCH TAPE.
!
         j = Buf(ii+20)
         j1st = j*21 + ii
         keep = 1
         SPAG_Loop_1_4: DO
            j = j*21 + ii
            j1 = Buf(j+20)
            IF ( j1==0 ) THEN
               iseq = iseq + 1
               IF ( iseq==2 ) THEN
!
!     SET UP 1ST MERGE
!
                  CALL close(itape,1)
                  itape = itape1
                  kop = 0
               ELSEIF ( iseq<=2 ) THEN
                  IF ( iend/=0 ) THEN
!
!     NO MERGING IS REQUIRED, ALL CARDS FIT WITHIN ONE WORKING BUFFER
!     LOAD
!
                     ktape = itape
                     CALL close(ktape,1)
                     GOTO 540
                  ELSE
                     itape = itape2
                     CALL open(*660,itape,Buf(Ibufsz+1),1)
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
!
!     SET UP SUBSEQUENT MERGE TAPES
!
               IF ( mod(iseq,2)==0 ) THEN
                  jtape = itape2
                  ktape = itape3
               ELSE
                  jtape = itape3
                  ktape = itape2
               ENDIF
               CALL close(itape,1)
!
!     SPECIAL LOGIC TO AVOID MERGE IF NEW CORE LOAD FOLLOWS ALL PREVIOUS
!
               IF ( kop<1 ) THEN
                  CALL open(*660,itape,Buf(1),0)
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( kop/=1 ) THEN
                  DO j = 1 , 18
                     ibuf1(j) = ibuf2(j)
                  ENDDO
               ENDIF
               DO j = 1 , 18
                  IF ( Buf(j1st)/=ibuf1(j) ) EXIT SPAG_Loop_1_4
                  j1st = j1st + 1
               ENDDO
               CALL open(*660,itape,Buf(1),0)
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ELSE
!
!     ITAPE IS PRIMARY CORE UNLOAD TAPE
!
               CALL write(itape,Buf(j),20,1)
               IF ( j<keep ) notsor = 1
               keep = j
               j = j1
            ENDIF
         ENDDO SPAG_Loop_1_4
         GOTO mz1
 80      IF ( rshift(Buf(j1st),nshift)>=rshift(ibuf1(j),nshift) ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     THIS SECTION PERFORMS A 2 TAPE ALPHANUMERIC MERGE
!     (ITAPE+JTAPE=KTAPE)
!     SAME BASIC LOGIC AS ORIGINAL SORT COMPARES (COMMENT CARDS OMITTED)
!
         notsor = 1
         CALL open(*660,itape,Buf(1),0)
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 100     IF ( dec ) THEN
            IF ( khrfn4(Buf(j1st))<khrfn4(ibuf1(j)) ) THEN
               notsor = 1
               CALL open(*660,itape,Buf(1),0)
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSEIF ( Buf(j1st)<ibuf1(j) ) THEN
            notsor = 1
            CALL open(*660,itape,Buf(1),0)
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
         trial = ktape
         ktape = jtape
         jtape = trial
         iseq = iseq - 1
         CALL open(*660,itape,Buf(1),0)
         CALL open(*660,ktape,Buf(Ibufsz+1),3)
         GOTO 460
      CASE (7)
         CALL open(*660,jtape,Buf(Ibufsz+1),0)
         nbuf2 = 2*Ibufsz + 1
         CALL open(*660,ktape,Buf(nbuf2),1)
         ccnt = 0
         spag_nextblock_1 = 8
      CASE (8)
         CALL read(*440,*680,jtape,ibuf2,20,1,iflg)
         IF ( Mach==2 .AND. (jtape==umf .OR. jtape==iptp) ) CALL umftrn(ibuf2)
         IF ( Mach==3 .AND. kin==1 ) CALL umffd(ibuf2)
         ldup = 0
         IF ( itape==optp ) CALL crdflg(ibuf2)
         ktarsw = 0
         IF ( .NOT.dec ) itst = andf(Mka,ibuf2(2))
         IF ( dec ) itst = khrfn1(Bkmsk2,4,ibuf2(2),4)
         IF ( itst==Star ) ktarsw = 1
         GOTO my1
 120     ibuf2a(1) = rshift(ibuf2(1),nshift)
         ibuf2a(2) = rshift(ibuf2(2),nshift)
 140     CALL read(*500,*680,itape,ibuf1,20,1,iflg)
         IF ( Mach==2 .AND. (itape==umf .OR. itape==iptp) ) CALL umftrn(ibuf1)
         IF ( Mach==3 .AND. kin==1 ) CALL umffd(ibuf1)
         starsw = 0
         IF ( .NOT.dec ) itst = andf(Mka,ibuf1(2))
         IF ( dec ) itst = khrfn1(Bkmsk2,4,ibuf1(2),4)
         IF ( itst==Star ) starsw = 1
         GOTO ibranb
 160     GOTO my2
 180     ibuf1a(1) = rshift(ibuf1(1),nshift)
         ibuf1a(2) = rshift(ibuf1(2),nshift)
         GOTO 360
!
!     TEST IF CARD IS TO BE DELETED
!
 200     ccnt = ccnt + 1
         IF ( .NOT.dec ) tst = andf(mk(3),ibuf1(1))
         IF ( dec ) tst = khrfn1(Bkmsk2,1,ibuf1(1),1)
         iccflg = -1
         IF ( tst==Plus .OR. tst==Starl ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL extint(ibuf1(1))
         GOTO my6
 220     ibuf1a(1) = rshift(ibuf1(1),nshift)
         ibuf1a(2) = rshift(ibuf1(2),nshift)
 240     iccflg = 0
         kparnt(1) = ibuf1(1)
         kparnt(2) = ibuf1(2)
         spag_nextblock_1 = 9
      CASE (9)
         GOTO ibranc
 260     SPAG_Loop_1_5: DO
            CALL read(*320,*680,itape5,ibuf3,2,1,iflg)
            IF ( ibuf3(1)/=0 ) THEN
               ASSIGN 280 TO ibranc
               EXIT SPAG_Loop_1_5
            ENDIF
         ENDDO SPAG_Loop_1_5
 280     IF ( ibuf3(2)/=0 ) THEN
            IF ( ibuf3(2)==ccnt ) ASSIGN 260 TO ibranc
            IF ( ibuf3(1)<=ccnt .AND. ibuf3(2)>=ccnt ) THEN
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSEIF ( ibuf3(1)==ccnt ) THEN
            ASSIGN 260 TO ibranc
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     REMOVE ANY UNDELETED CONTINUATION CARDS DURING RESTART MERGE
!
 300     IF ( iccflg==0 ) GOTO ibrane
         CALL write(itape4,ibuf1(1),20,1)
         spag_nextblock_1 = 10
      CASE (10)
         GOTO ibrand
 320     ASSIGN 300 TO ibranc
         CALL close(itape5,1)
         GOTO 300
      CASE (11)
!
!     IF CONTINUATION WAS DELETED, FLAG PARENT
!
         IF ( iccflg==0 ) THEN
            CALL crdflg(ibuf1)
         ELSE
            CALL crdflg(kparnt)
         ENDIF
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 340     ibuf2a(1) = rshift(ibuf2(1),nshift)
         ibuf2a(2) = rshift(ibuf2(2),nshift)
 360     j = 1
         j1 = 1
         j2 = 1
         ni = 0
         ki = 0
         spag_nextblock_1 = 12
      CASE (12)
         GOTO my4
 380     IF ( ibuf1a(j1)<ibuf2a(j2) ) THEN
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ibuf1a(j1)/=ibuf2a(j2) ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 16
         CYCLE SPAG_DispatchLoop_1
      CASE (13)
         IF ( ibuf1(j1)==ibuf2(j2) ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ibuf1(j1)==bk(4) ) THEN
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ibuf2(j2)==bk(4) ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         GOTO my5
 400     IF ( rshift(ibuf1(j1),1)<rshift(ibuf2(j2),1) ) THEN
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( rshift(ibuf1(j1),1)/=rshift(ibuf2(j2),1) ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 16
         CYCLE SPAG_DispatchLoop_1
 420     IF ( dec ) THEN
            IF ( khrfn4(ibuf1(j1))<khrfn4(ibuf2(j2)) ) THEN
            ELSEIF ( khrfn4(ibuf1(j1))==khrfn4(ibuf2(j2)) ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ELSE
               spag_nextblock_1 = 15
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSEIF ( ibuf1(j1)>=ibuf2(j2) ) THEN
            IF ( ibuf1(j1)>ibuf2(j2) ) THEN
               spag_nextblock_1 = 15
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 14
      CASE (14)
         CALL write(ktape,ibuf1,20,1)
         kop = 1
         GOTO 140
      CASE (15)
         CALL write(ktape,ibuf2,20,1)
         kop = 2
         CALL read(*440,*680,jtape,ibuf2,20,1,iflg)
         IF ( Mach==2 .AND. (jtape==umf .OR. jtape==iptp) ) CALL umftrn(ibuf2)
         IF ( Mach==3 .AND. kin==1 ) CALL umffd(ibuf2)
         IF ( itape==optp ) CALL crdflg(ibuf2)
         ktarsw = 0
         IF ( .NOT.dec ) itst = andf(Mka,ibuf2(2))
         IF ( dec ) itst = khrfn1(Bkmsk2,4,ibuf2(2),4)
         IF ( itst==Star ) ktarsw = 1
         GOTO my3
      CASE (16)
         SPAG_Loop_1_6: DO
            j = j + 1
            j1 = j1 + 1
            j2 = j2 + 1
            IF ( j==1 ) THEN
               spag_nextblock_1 = 21
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( j==2 ) THEN
               IF ( starsw==ktarsw ) THEN
                  spag_nextblock_1 = 12
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( dec ) THEN
                  in1 = rshift(khrfn4(khrfn1(ibuf1(j1),4,Bkmsk2,1)),1)
                  ik2 = rshift(khrfn4(khrfn1(ibuf2(j2),4,Bkmsk2,1)),1)
               ELSE
                  in1 = rshift(andf(mkd,ibuf1(j1)),1)
                  ik2 = rshift(andf(mkd,ibuf2(j2)),1)
               ENDIF
               IF ( in1==ik2 ) THEN
                  IF ( dec ) THEN
                     in1 = rshift(lshift(khrfn4(khrfn1(ibuf1(j1),4,Bkmsk2,1)),1),1)
                     ik2 = rshift(lshift(khrfn4(khrfn1(ibuf2(j2),4,Bkmsk2,1)),1),1)
                  ELSE
                     in1 = andf(mke,ibuf1(j1))
                     ik2 = andf(mke,ibuf2(j2))
                  ENDIF
                  IF ( in1==ik2 ) CYCLE
               ENDIF
               IF ( in1<ik2 ) THEN
                  spag_nextblock_1 = 14
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               spag_nextblock_1 = 15
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( j==3 ) THEN
               IF ( starsw==ktarsw ) THEN
                  spag_nextblock_1 = 13
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               k1 = 0
               k2 = 0
               IF ( dec ) THEN
                  IF ( khrfn1(Bkmsk2,1,ibuf1(j1),1)/=Bkmsk1(4) ) k1 = 1
                  IF ( khrfn1(Bkmsk2,1,ibuf2(j2),1)/=Bkmsk1(4) ) k2 = 1
               ELSE
                  IF ( andf(mk(3),ibuf1(j1))/=Bkmsk1(4) ) k1 = 1
                  IF ( andf(mk(3),ibuf2(j2))/=Bkmsk1(4) ) k2 = 1
               ENDIF
               spag_nextblock_1 = 17
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( j==4 .OR. j==6 .OR. j==8 .OR. j==10 .OR. j==12 .OR. j==14 .OR. j==16 .OR. j==18 ) THEN
               spag_nextblock_1 = 13
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( j==5 .OR. j==9 .OR. j==13 .OR. j==17 ) THEN
               IF ( starsw/=ktarsw ) EXIT SPAG_Loop_1_6
               IF ( starsw==0 ) EXIT SPAG_Loop_1_6
               spag_nextblock_1 = 13
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( j==11 ) THEN
!
!     DUPLICATE CARD
!
               IF ( starsw==ktarsw ) EXIT SPAG_Loop_1_6
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( j==19 ) THEN
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ELSE
               EXIT SPAG_Loop_1_6
            ENDIF
         ENDDO SPAG_Loop_1_6
         IF ( ni<ki ) THEN
            j2 = j2 + ki
            ki = 0
         ELSEIF ( ni/=ki ) THEN
            j1 = j1 + ni
            ni = 0
         ENDIF
         CALL xfadj(ibuf1(j1),starsw,k1)
         CALL xfadj(ibuf2(j2),ktarsw,k2)
         spag_nextblock_1 = 17
      CASE (17)
         IF ( starsw<ktarsw ) THEN
            ki = 2
            IF ( k1+k2/=2 ) THEN
               j2 = j2 + 2
               ki = 0
            ENDIF
         ELSEIF ( starsw/=ktarsw ) THEN
            ni = 2
            IF ( k1+k2/=2 ) THEN
               j1 = j1 + 2
               ni = 0
            ENDIF
         ENDIF
         spag_nextblock_1 = 13
         CYCLE SPAG_DispatchLoop_1
      CASE (18)
         CALL write(ktape,ibuf1,20,1)
         CALL write(ktape,ibuf2,20,1)
         ldup = -1
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
!
!     ONE OF TWO TAPES BEING MERGED IS EXHAUSTED, OTHER TAPE IS COPIED
!     ONTO THE MERGE TAPE
!
 440     IF ( itape==optp ) THEN
            ASSIGN 460 TO ibrand
            ASSIGN 480 TO ibrane
            ASSIGN 200 TO ibranf
            IF ( ccnt==0 ) GOTO 460
         ENDIF
         IF ( ldup>=0 ) GOTO 480
 460     CALL read(*520,*680,itape,ibuf1,20,1,iflg)
         IF ( Mach==2 .AND. (itape==umf .OR. itape==iptp) ) CALL umftrn(ibuf1)
         IF ( Mach==3 .AND. kin==1 ) CALL umffd(ibuf1)
         GOTO ibranf
 480     CALL write(ktape,ibuf1,20,1)
         kop = 1
         GOTO 460
 500     DO
            CALL write(ktape,ibuf2,20,1)
            kop = 2
            CALL read(*520,*680,jtape,ibuf2,20,1,iflg)
            IF ( Mach==2 .AND. (jtape==umf .OR. jtape==iptp) ) CALL umftrn(ibuf2)
            IF ( Mach==3 .AND. kin==1 ) CALL umffd(ibuf2)
         ENDDO
 520     IF ( Iuedit==1 ) THEN
!
!     CLOSE TAPES ENVOLVED IN MERGE
!
            CALL close(itape,2)
         ELSEIF ( itape==optp ) THEN
            CALL close(itape,2)
         ELSE
            CALL close(itape,1)
         ENDIF
         CALL close(jtape,3)
         CALL close(ktape,3)
         GOTO ibrana
!
!     WAS THIS THE FINAL MERGE (LAST CORE LOAD OF CARDS)
!
 540     IF ( iend==0 ) THEN
            CALL open(*660,itape,Buf(1),1)
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSE
            CALL page2(2)
            WRITE (Outtap,99025)
            CALL close(itape5,1)
!
!     PROCESS DELETE CARDS (IF ANY)
!
            nbuf4 = 4*Ibufsz + 1
            CALL open(*660,itape5,Buf(nbuf4),0)
!
!     IF NOT RESTART - NO DELETES SHOULD EXIST
!
            IF ( irestr>0 .OR. kin>0 ) THEN
!
!     FORM DELETE CARD LIST
!
               ibuf3(1) = inf
               Buf(ii) = mkb
               Buf(ii+1) = mkb
               DO j = ii , ibuflg , 2
                  CALL read(*560,*680,itape5,ibuf3,2,1,iflg)
                  SPAG_Loop_2_7: DO i = ii , j , 2
                     IF ( ibuf3(1)<=Buf(i) ) EXIT SPAG_Loop_2_7
                  ENDDO SPAG_Loop_2_7
!
!     PUSH DOWN LIST - MAKE DOUBLE WORD SLOT
!
                  kk = j + 2
                  k1 = (j-i)/2 + 1
                  DO k = 1 , k1
                     Buf(kk+1) = Buf(kk-1)
                     Buf(kk) = Buf(kk-2)
                     kk = kk - 2
                  ENDDO
                  Buf(i) = ibuf3(1)
                  Buf(i+1) = ibuf3(2)
               ENDDO
!
!     IF DELETE CARD LIST WILL NOT FIT
!
               CALL mesage(-8,0,nsort)
            ELSE
!
               CALL read(*580,*680,itape5,ibuf3,1,1,iflg)
!
!     NOT RESTART AND DELETES DO EXIST - WARNING
!
               CALL close(itape5,1)
               CALL page2(2)
               WRITE (Outtap,99008) Uwm
99008          FORMAT (A25,' 205, COLD START,DELETE CARDS IGNORED.')
               GOTO 580
            ENDIF
         ENDIF
!
!     EOF ON ITAPE5, IF IBUF3(1)= INF, THERE ARE NO DELETE CARDS
!
 560     IF ( ibuf3(1)/=inf ) THEN
            j = j - 1
!
!     CHECK FOR AND ELIMINATE OVERLAPS AND REDUNDANCYS IN DELETES
!
            imin = 0
            DO i = ii , j , 2
               spag_nextblock_3 = 1
               SPAG_DispatchLoop_3: DO
                  SELECT CASE (spag_nextblock_3)
                  CASE (1)
                     IF ( Buf(i)/=0 ) THEN
                        IF ( Buf(i)<Buf(i+1) ) THEN
                           IF ( imin==0 ) THEN
                              spag_nextblock_3 = 2
                              CYCLE SPAG_DispatchLoop_3
                           ENDIF
                           IF ( Buf(i)>Buf(imax) ) THEN
                              spag_nextblock_3 = 2
                              CYCLE SPAG_DispatchLoop_3
                           ENDIF
                           IF ( Buf(i+1)>=Buf(imax) ) Buf(imax) = Buf(i+1)
                        ELSE
                           Buf(i+1) = 0
                           IF ( Buf(i)/=Buf(i+2) ) THEN
                              IF ( imin==0 ) CYCLE
                              IF ( Buf(i)>Buf(imax) ) THEN
                                 imin = 0
                                 CYCLE
                              ENDIF
                           ENDIF
                        ENDIF
                        Buf(i) = 0
                     ENDIF
                     CYCLE
                  CASE (2)
                     imin = i
                     imax = i + 1
                     EXIT SPAG_DispatchLoop_3
                  END SELECT
               ENDDO SPAG_DispatchLoop_3
            ENDDO
            CALL close(itape5,1)
!
!     PUT OUT SORTED DELETE CARD LIST
!
            nbuf4 = 4*Ibufsz + 1
            CALL open(*660,itape5,Buf(nbuf4),1)
            DO i = ii , j , 2
               IF ( Buf(i)/=0 ) CALL write(itape5,Buf(i),2,1)
            ENDDO
         ENDIF
         CALL close(itape5,1)
!
!     AT THIS POINT, IF THIS IS A RESTART, MERGE OPTP, FINAL KTAPE,
!     + DELETE
!
         ASSIGN 580 TO ibrana
         ASSIGN 200 TO ibranb
         ASSIGN 260 TO ibranc
         ASSIGN 140 TO ibrand
         ASSIGN 360 TO ibrane
         nbuf4 = 4*Ibufsz + 1
         CALL open(*660,itape5,Buf(nbuf4),0)
!
         IF ( kin>0 ) THEN
!
            optp = umf
            CALL open(*40,umf,Buf(1),2)
         ELSE
!
            CALL open(*740,optp,Buf(1),0)
            SPAG_Loop_1_8: DO
               CALL read(*720,*680,optp,ibuf3,2,1,iflg)
               IF ( ibuf3(1)==iblkda(1) .AND. ibuf3(2)==iblkda(2) ) EXIT SPAG_Loop_1_8
               CALL skpfil(optp,+1)
            ENDDO SPAG_Loop_1_8
         ENDIF
         itape = optp
         trial = jtape
         jtape = ktape
         ktape = trial
         IF ( iccnt/=1 ) THEN
            CALL write(itape4,mkb,20,1)
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     PROCESS CONTINUATION CARDS (IF ANY)
!
 580     CALL close(itape4,1)
         nbuf3 = 3*Ibufsz + 1
         CALL open(*660,itape4,Buf(nbuf3),0)
         IF ( iccnt==1 .AND. (irestr>0 .OR. kin>0) ) ktape = optp
         IF ( .NOT.(iccnt==1 .AND. (irestr>0 .OR. kin>0)) ) THEN
            nbuf2 = 2*Ibufsz + 1
            CALL open(*660,ktape,Buf(nbuf2),0)
         ENDIF
!
!     FORM CONTINUATION CARD DICTIONARY
!
         ibuf1(1) = 0
         DO j = ii , ibuflg , 4
            CALL read(*600,*680,itape4,ibuf1,20,1,iflg)
            IF ( Mach==2 .AND. ibuf1(1)/=mkb ) CALL umftrn(ibuf1)
            IF ( Mach==3 .AND. kin==1 ) CALL umffd(ibuf1)
            IF ( ibuf1(1)/=mkb ) THEN
               IF ( .NOT.dec ) Buf(j) = andf(mkc,ibuf1(1))
               IF ( dec ) Buf(j) = khrfn1(ibuf1(1),1,Bkmsk2,1)
            ELSE
               IF ( j/=ii ) iccbrk = j
               Buf(j) = Dollar
            ENDIF
            Buf(j+1) = ibuf1(2)
            IF ( .NOT.dec ) Buf(j+2) = andf(mkc,ibuf1(19))
            IF ( dec ) Buf(j+2) = khrfn1(ibuf1(19),1,Bkmsk2,1)
            Buf(j+3) = ibuf1(20)
         ENDDO
!
!
!     CORE INSUFFICIENT TO ACCOMMODATE 4-WORD PER CARD DICTIONARY
!     OF CONTINUATION CARDS
!
         CALL mesage(-8,0,nsort)
!
!     EOF ON ITAPE4, IF IBUF1(1)= 0, THERE ARE NO CONTINUATION CARDS
!
 600     IF ( ibuf1(1)/=0 ) THEN
            CALL rewind(itape4)
            jo = 1
            iconlg = j - 1
!
!     CHECK AND SET FLAGS FOR DUPLICATE CONTINUATION CARDS
!
            k = iconlg - 4
            IF ( k>ii ) THEN
               DO j = ii , k , 4
                  IF ( Buf(j)/=idup ) THEN
                     index = 0
                     m = j + 4
                     DO jj = m , iconlg , 4
                        IF ( Buf(jj)/=idup ) THEN
                           IF ( Buf(j)==Buf(jj) ) THEN
                              IF ( Buf(j+1)==Buf(jj+1) ) THEN
                                 Buf(jj) = idup
                                 index = 1
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDDO
                     IF ( index==1 ) Buf(j) = idup
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
!
!     SET UP AND PUT OUT SORTED HEADING
!
         IF ( notsor/=0 ) THEN
            CALL page2(2)
            WRITE (Outtap,99009) Uim
99009       FORMAT (A29,' 207, BULK DATA NOT SORTED, XSORT WILL RE-ORDER ','DECK.')
         ENDIF
         IF ( Echos/=0 ) THEN
            DO j = 1 , 32
               Head1(j) = heads(j)
            ENDDO
            Head2(4) = cdcnt(1)
            Head3(4) = cdcnt(2)
            Head3(5) = cdcnt(3)
            CALL page
            ccnt = 0
         ENDIF
         CALL close(itape5,1)
         j = ii
         nbuf4 = 4*Ibufsz + 1
         CALL open(*760,nptp,Buf(nbuf4),3)
         CALL write(nptp,iblkda,2,1)
         IF ( ibuf1(1)==0 ) THEN
            DO
!
!     NO CONTINUATION CARDS
!
               CALL read(*620,*680,ktape,ibuf2,20,1,iflg)
               IF ( iccnt/=1 ) CALL intext(ibuf2(1))
               IF ( Mach==2 ) CALL umftrn(ibuf2)
               IF ( Mach==3 .AND. kin==1 ) CALL umffd(ibuf2)
               CALL write(nptp,ibuf2,20,1)
               IF ( Echos/=0 ) THEN
                  CALL page2(-1)
                  ccnt = ccnt + 1
                  CALL xprety(ibuf2)
                  WRITE (Outtap,99023) ccnt , ibuf2
               ENDIF
               IF ( Echop/=0 ) THEN
                  IF ( Echos==0 ) CALL xprety(ibuf2)
                  WRITE (Lpch,99024) ibuf2
               ENDIF
            ENDDO
            GOTO 620
         ENDIF
         spag_nextblock_1 = 19
      CASE (19)
         SPAG_Loop_1_9: DO
!
!     MERGE CONTINUATION CARDS - PRODUCE DATA ON NPTP
!
            CALL read(*620,*680,ktape,ibuf1,20,1,iflg)
            IF ( iccbrk/=0 ) THEN
               kparnt(1) = ibuf1(1)
               kparnt(2) = ibuf1(2)
            ENDIF
            CALL intext(ibuf1(1))
            IF ( Mach==2 ) CALL umftrn(ibuf1)
            IF ( Mach==3 .AND. kin==1 ) CALL umffd(ibuf1)
            CALL write(nptp,ibuf1,20,1)
            IF ( Echos/=0 ) THEN
               CALL page2(-1)
               ccnt = ccnt + 1
               CALL xprety(ibuf1)
               WRITE (Outtap,99023) ccnt , ibuf1
            ENDIF
!
!      PUNCH OUT DECK
!
            IF ( Echop/=0 ) THEN
               IF ( Echos==0 ) CALL xprety(ibuf1)
               WRITE (Lpch,99024) ibuf1
            ENDIF
!
!     SEE IF PREVIOUS CARD HAS A CONTINUATION
!     IF CONTINUATION FIELD BLANK - CONTINUATION NOT POSSIBLE
!
            IF ( ibuf1(19)/=bk(4) .OR. ibuf1(20)/=bk(4) ) THEN
               IF ( .NOT.dec ) trial = andf(mkc,ibuf1(19))
               IF ( dec ) trial = khrfn1(ibuf1(19),1,Bkmsk2,1)
               jn = 0
               SPAG_Loop_2_11: DO
!
                  SPAG_Loop_3_10: DO k = ii , iconlg , 4
!
!     IGNORE DUPLICATE CONTINUATION CARDS
!
                     IF ( Buf(j)/=idup ) THEN
                        IF ( ibuf1(20)==Buf(j+1) ) THEN
                           IF ( .NOT.dec ) itst = andf(mkc,Buf(j))
                           IF ( dec ) itst = khrfn1(Buf(j),1,Bkmsk2,1)
                           IF ( itst==trial ) THEN
!
!     A CONTINUATION EXISTS, HAS IT ALREADY BEEN USED
!
                              IF ( .NOT.dec ) itst = andf(mk(3),Buf(j))
                              IF ( dec ) itst = khrfn1(Bkmsk2,1,Buf(j),1)
                              IF ( itst==Dollar ) EXIT SPAG_Loop_1_9
                              IF ( j<=iccbrk ) CALL crdflg(kparnt)
                              IF ( .NOT.dec ) Buf(j) = orf(Buf(j),Dollar)
                              IF ( dec ) Buf(j) = khrfn1(Buf(j),1,Dollar,1)
                              jn = (j-ii)/4 + 1
                              CALL xrecps(jn,jo)
                              CALL read(*700,*680,itape4,ibuf1,20,1,iflg)
                              IF ( Mach==2 ) CALL umftrn(ibuf1)
                              IF ( Mach==3 .AND. kin==1 ) CALL umffd(ibuf1)
                              CALL write(nptp,ibuf1,20,1)
                              IF ( Echos/=0 ) THEN
                                 CALL page2(-1)
                                 ccnt = ccnt + 1
                                 WRITE (Outtap,99023) ccnt , ibuf1
                              ENDIF
                              IF ( Echop/=0 ) WRITE (Lpch,99024) ibuf1
                              IF ( .NOT.dec ) trial = andf(mkc,ibuf1(19))
                              IF ( dec ) trial = khrfn1(ibuf1(19),1,Bkmsk2,1)
                              IF ( ibuf1(19)==bk(4) .AND. ibuf1(20)==bk(4) ) EXIT SPAG_Loop_3_10
                              CYCLE SPAG_Loop_2_11
                           ENDIF
                        ENDIF
                     ENDIF
                     j = j + 4
                     IF ( j>iconlg ) j = ii
                  ENDDO SPAG_Loop_3_10
                  EXIT SPAG_Loop_2_11
               ENDDO SPAG_Loop_2_11
            ENDIF
         ENDDO SPAG_Loop_1_9
!
!     DUPLICATE PARENT - ERROR
!
         nl = 0
         IF ( Echos==0 ) THEN
            nl = 1
            WRITE (Outtap,99022) ibuf1
         ENDIF
         nl = nl + 2
         CALL page2(-nl)
         WRITE (Outtap,99010) Ufm
99010    FORMAT (A23,' 208, PREVIOUS CARD IS A DUPLICATE PARENT.')
         Nogo = -1
         spag_nextblock_1 = 19
         CYCLE SPAG_DispatchLoop_1
!
!     CLOSE KTAPE AND WRITE (ENDDATA)
!
 620     CALL close(ktape,2)
         CALL eof(nptp)
         CALL close(nptp,1)
         IF ( Echos/=0 ) THEN
            CALL page2(-1)
            WRITE (Outtap,99022) iiend
         ENDIF
         IF ( ibuf1(1)/=0 ) THEN
            CALL page2(2)
            WRITE (Outtap,99025)
!
!     IDENTIFY DUPLICATE OR PARENTLESS CONTINUATION CARDS
!
            ncnt = 0
            DO j = ii , iconlg , 4
               spag_nextblock_4 = 1
               SPAG_DispatchLoop_4: DO
                  SELECT CASE (spag_nextblock_4)
                  CASE (1)
                     IF ( .NOT.dec ) itst = andf(mk(3),Buf(j))
                     IF ( dec ) itst = khrfn1(Bkmsk2,1,Buf(j),1)
                     IF ( itst/=Dollar ) THEN
!
!     CHECK FOR DUPLICATE CONTINUATION CARDS
!
                        IF ( Buf(j)/=idup ) THEN
!
!     CHECK FOR PARENTLESS CONTINUATION CARDS
!
                           DO jj = ii , iconlg , 4
                              IF ( j/=jj ) THEN
                                 IF ( Buf(j)==Buf(jj+2) .AND. Buf(j+1)==Buf(jj+3) ) THEN
                                    spag_nextblock_4 = 2
                                    CYCLE SPAG_DispatchLoop_4
                                 ENDIF
                              ENDIF
                           ENDDO
                        ENDIF
                        ncnt = ncnt + 1
                        jn = (j-ii)/4 + 1
                        CALL xrecps(jn,jo)
                        CALL read(*700,*680,itape4,ibuf2,20,1,iflg)
                        IF ( Mach==2 ) CALL umftrn(ibuf2)
                        IF ( Mach==3 .AND. kin==1 ) CALL umffd(ibuf2)
                        CALL page2(-1)
                        WRITE (Outtap,99022) ibuf2
                     ENDIF
                     CYCLE
                  CASE (2)
                     Buf(j) = iok
                     EXIT SPAG_DispatchLoop_4
                  END SELECT
               ENDDO SPAG_DispatchLoop_4
            ENDDO
            IF ( ncnt/=0 ) THEN
               CALL page2(3)
               WRITE (Outtap,99011) Ufm , ncnt
99011          FORMAT (A23,' 209, PREVIOUS',I7,' CONTINUATION MNEMONICS HAVE NO',' PARENTS AND/OR ARE DUPLICATES.',/)
               Nogo = -1
!
!     IDENTIFY THOSE CONTINUATION CARDS THAT ARE VALID, BUT YET CANNOT
!     BE PROCESSED BECAUSE OF ERRORS ON OTHER RELATED CONTINUATION CARDS
!
               ncnt = 0
               DO j = ii , iconlg , 4
                  IF ( Buf(j)==iok ) THEN
                     ncnt = ncnt + 1
                     jn = (j-ii)/4 + 1
                     CALL xrecps(jn,jo)
                     CALL read(*700,*680,itape4,ibuf2,20,1,iflg)
                     IF ( Mach==2 ) CALL umftrn(ibuf2)
                     IF ( Mach==3 .AND. kin==1 ) CALL umffd(ibuf2)
                     CALL page2(-1)
                     WRITE (Outtap,99022) ibuf2
                  ENDIF
               ENDDO
               IF ( ncnt/=0 ) THEN
                  CALL page2(4)
                  WRITE (Outtap,99012) Ufm , ncnt
99012             FORMAT (A23,' 206, PREVIOUS',I7,' CONTINUATION CARDS, THOUGH ','VALID, CANNOT BE PROCESSED',/5X,                  &
                         &'BECAUSE OF ERRORS ON OTHER RELATED CONTINUATION CARDS.',/)
               ENDIF
            ENDIF
         ENDIF
         CALL close(itape4,1)
!
!     REACTIVE DIAG 47 TO PRINT THE CONTENTS OF NTPT
!
         l47 = 0
         IF ( l47==0 ) THEN
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL open(*760,nptp,Buf(1),0)
         DO
            CALL skpfil(nptp,+1)
            CALL read(*640,*640,nptp,ibuf1(1),2,1,j)
            IF ( ibuf1(1)==iblkda(1) .AND. ibuf1(2)==iblkda(2) ) THEN
               DO
                  CALL read(*640,*640,nptp,ibuf1(1),20,1,j)
                  WRITE (Outtap,99013) (ibuf1(j),j=1,10) , (ibuf1(j),j=17,20)
99013             FORMAT (' ==NPTP==>',5(1X,2A4),'...',2(1X,2A4))
               ENDDO
            ENDIF
         ENDDO
 640     CALL close(nptp,1)
         spag_nextblock_1 = 20
      CASE (20)
!
!     DISABLE FREE-FIELD INPUT OPTION IN XREAD.
!
         Ffflag = 0
         RETURN
!
!     ERROR MESSAGES
!
 660     WRITE (Outtap,99014) Sfm
99014    FORMAT (A25,' 210, SCRATCH COULD NOT BE OPENED')
         spag_nextblock_1 = 22
         CYCLE SPAG_DispatchLoop_1
 680     WRITE (Outtap,99015) Sfm
99015    FORMAT (A25,' 211, ILLEGAL EOR ON SCRATCH')
         spag_nextblock_1 = 22
         CYCLE SPAG_DispatchLoop_1
 700     WRITE (Outtap,99016) Sfm
99016    FORMAT (A25,' 212, ILLEGAL EOF ON ITAPE4')
         spag_nextblock_1 = 22
         CYCLE SPAG_DispatchLoop_1
 720     WRITE (Outtap,99017) Sfm
99017    FORMAT (A25,' 213, ILLEGAL EOF ON OPTP')
         spag_nextblock_1 = 22
         CYCLE SPAG_DispatchLoop_1
 740     WRITE (Outtap,99018) Sfm
99018    FORMAT (A25,' 214, OPTP COULD NOT BE OPENED')
         spag_nextblock_1 = 22
         CYCLE SPAG_DispatchLoop_1
 760     WRITE (Outtap,99019) Sfm
99019    FORMAT (A25,' 215, NPTP COULD NOT BE OPENED')
         spag_nextblock_1 = 22
         CYCLE SPAG_DispatchLoop_1
      CASE (21)
         WRITE (Outtap,99020) Sfm
99020    FORMAT (A25,' 216, ILLEGAL INDEX')
         spag_nextblock_1 = 22
         CYCLE SPAG_DispatchLoop_1
 780     WRITE (Outtap,99021) Sfm
99021    FORMAT (A25,' 219, MISSING ENDDATA CARD.')
         spag_nextblock_1 = 22
      CASE (22)
         CALL page2(2)
         CALL mesage(-37,0,nsort)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99022 FORMAT (30X,20A4)
99023 FORMAT (13X,I8,1H-,8X,20A4)
99024 FORMAT (20A4)
99025 FORMAT (1H0)
END SUBROUTINE xsort
