
SUBROUTINE xsort
!
!     SORT READS BULK DATA CARDS FROM THE INPUT TAPE, ADJUSTS THE
!     FIELDS, PERFORMS AN ALPHA-NUMERIC SORT ON THE CARD IMAGES FROM
!     LEFT TO RIGHT, INSERTS CONTINUATION CARDS IN THEIR PROPER
!     POSITION, AND PLACES THE RESULTING SORTED IMAGES ON THE NEW
!     PROBLEM TAPE.
!
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Bimsk1(6) , Bimsk2(5) , Bimsk3(4) , Bimsk4(4) , Bimsk5(2) , Bimsk6 , Bk(4) , Bkmsk1(8) , Bkmsk2 , Blank , Blanx , Buf(1) &
         & , D , D1(14) , Dollar , Dum1(2) , Dum12(12) , Dum2(96) , Dum44(44) , Dum8(8) , Echop , Echos , Echou , Ffflag , Head1(32)&
         & , Head2(32) , Head3(32) , Iapprc , Ibufsz , Icon1 , Icon2 , Icpflg , Iecho , Inf , Intape , Is , Isubs , Iuedit ,        &
         & Krap(12) , Kumf , Lpch , Mach , Mask , Mbit4 , Mk(4) , Mka , Mkb , Mkc , Mkd , Mke , Nogo , Outtap , Plus , Sfta , Sftm ,&
         & Shifts(4) , Skip1 , Slash , Star , Starl
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /machin/ Mach
   COMMON /output/ Dum2 , Head1 , Head2 , Head3
   COMMON /stapid/ Krap , Kumf
   COMMON /system/ Ibufsz , Outtap , Nogo , Intape , D1 , Iecho , D , Iapprc , Dum1 , Iuedit , Dum44 , Isubs , Dum12 , Icpflg ,     &
                 & Dum8 , Lpch
   COMMON /xechox/ Ffflag , Echou , Echos , Echop
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /xsrtcm/ Bimsk1 , Bimsk2 , Bimsk3 , Bimsk4 , Bimsk5 , Bimsk6 , Bkmsk1 , Bkmsk2 , Shifts , Icon1 , Icon2 , Star , Plus ,   &
                 & Dollar , Starl , Slash , Sftm , Mask , Blank , Mka , Is , Mbit4
   COMMON /zzzzzz/ Skip1 , Buf
!
! Local variable declarations
!
   INTEGER andf , khrfn1 , khrfn3 , khrfn4 , korsz , lshift , orf , rshift
   INTEGER ccnt , cdcnt(3) , fcnt , headn(32) , heads(32) , headu(32) , i , ibk3 , ibk4 , iblkda(2) , ibrana , ibranb , ibranc ,    &
         & ibrand , ibrane , ibranf , ibuf1(20) , ibuf1a(2) , ibuf2(20) , ibuf2a(2) , ibuf3(2) , ibufbg , ibuflg , iccbrk , iccflg ,&
         & iccnt , iconlg , idup , iend , iend1 , iend2 , iend3 , iend4 , iend5 , iend6 , iflg , ii , iiend(2) , ik2 , imax , imin ,&
         & in1 , index , iok , iptp , irestr , iseq , itape , itape1 , itape2 , itape3 , itape4 , itape5 , itst , itst1 , itst2 ,   &
         & j , j1 , j1st , j2 , jj , jn , jo , jtape , k , k1 , k2 , keep , ki , kin , kk , kop , kp , kparnt(2) , ktape , ktarsw , &
         & kx , l47 , ldup , linf
   LOGICAL dec
   INTEGER m , mx3 , my1 , my2 , my3 , my4 , my5 , my6 , mz1 , n1 , n2 , n3 , nbuf2 , nbuf3 , nbuf4 , ncnt , ni , nl , notsor ,     &
         & nptp , nshift , nsort(2) , nx , ny , optp , pid , ptst , starsw , trial , tst , umf
   EXTERNAL andf , lshift , orf , rshift
!
! End of declarations
!
   EQUIVALENCE (Bk(1),Bkmsk1(5)) , (Mk(1),Bimsk2(2)) , (Mkb,Bimsk5(1)) , (Inf,Bimsk2(1)) , (Sfta,Shifts(2)) , (Mkd,Bimsk2(2)) ,     &
    & (Mke,Bimsk5(2)) , (Mkc,Bimsk4(1))
   EQUIVALENCE (Blanx,Bkmsk1(8))
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
!
!
!     XSORT MAY NOT WORK PROPERLY IN ALL UNIX MACHINES, WHICH FOLLOW
!     THE VAX LINE.
!
   dec = Mach==5 .OR. Mach==6 .OR. Mach==21
   IF ( dec .AND. Lpch/=77 ) WRITE (Outtap,99001) Uwm
99001 FORMAT (A25,', SWITCHING TO OLD XSORT VIA DIAG 42 HAS NOT BEEN ','THOROUGHLY TESTED',/5X,'FOR THE UNIX MACHINES.')
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
   IF ( Kumf<=0 ) GOTO 400
   kin = 1
   CALL open(*200,umf,Buf(1),2)
   DO
!
!     FIND PARTICULAR BULK DATA FILE ON UMF AS REQUESTED BY USER
!
      CALL read(*100,*300,umf,pid,1,1,iflg)
      IF ( Kumf<pid ) EXIT
      IF ( Kumf==pid ) THEN
         CALL close(umf,2)
         GOTO 400
      ELSE
         CALL skpfil(umf,1)
      ENDIF
   ENDDO
 100  WRITE (Outtap,99002) Ufm , Kumf
99002 FORMAT (A23,' 201, REQUESTED BULK DATA DECK',I8,' NOT ON USER ','MASTER FILE.')
   CALL page2(2)
   Nogo = -1
   CALL close(umf,1)
   RETURN
!
 200  WRITE (Outtap,99003) Sfm
99003 FORMAT (A25,' 202, UMF COULD NOT BE OPENED')
   GOTO 6800
 300  WRITE (Outtap,99004) Sfm
99004 FORMAT (A25,' 203, ILLEGAL EOR ON UMF')
   GOTO 6800
!
 400  CALL initco
   IF ( Iecho>=0 ) THEN
      Echou = andf(Iecho,1)
      Echos = andf(Iecho,2)
      Echop = andf(Iecho,4)
      IF ( Icpflg/=0 ) Echos = 1
   ENDIF
   ASSIGN 5000 TO ibrana
   ASSIGN 2100 TO ibranb
   ASSIGN 4700 TO ibranf
!
!     SET ASSIGN GO TO SWITCHES FOR MACHINE CONFIGURATIONS
!     THE 8 BIT CHARACTER BYTE OF THE 360 WILL HOLD THE INTERNAL
!     CHARACTER CODE (MAX=37) WITHOUT USE OF THE 1ST BIT POSITION -
!     THE OTHER 3 MACHINES HAVE 6 BIT CHARACTERS THEREFORE A SHIFT RIGHT
!     OF ONE MUST BE DONE TO REMOVE A POSSIBLE BIT FROM THE SIGN
!     POSITION THE FOLLOWING ASSIGNS SET THOSE BRANCHES BASED ON MACHINE
!
   IF ( Mach==2 .OR. dec ) THEN
      ASSIGN 800 TO mx3
      ASSIGN 2000 TO my1
      ASSIGN 3400 TO my2
      ASSIGN 3400 TO my3
      ASSIGN 3900 TO my4
      ASSIGN 3900 TO my5
      ASSIGN 2500 TO my6
      ASSIGN 1500 TO mz1
      linf = orf(Is,1)
   ELSE
      ASSIGN 750 TO mx3
      ASSIGN 1900 TO my1
      ASSIGN 2200 TO my2
      ASSIGN 3300 TO my3
      ASSIGN 3600 TO my4
      ASSIGN 3800 TO my5
      ASSIGN 2400 TO my6
      ASSIGN 1400 TO mz1
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
   CALL open(*6000,itape4,Buf(nbuf3),1)
   nbuf4 = 4*Ibufsz + 1
   CALL open(*6000,itape5,Buf(nbuf4),1)
!
!     A BUFFER LINE IS 20 WORDS OF CARD IMAGE PLUS A 1 WORD POINTER TO
!     THE NEXT IMAGE IN THE SORT SEQUENCE - A ZERO POINTER INDICATES
!     THE LAST IMAGE (LARGEST IN SORT)
!     INITIALIZE WORKING BUFFER - 1ST LINE ZEROS, 2ND LINE ALL BITS
!
   k = ii + 19
   DO j = ii , k
      Buf(j) = linf
      Buf(j+21) = Inf
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
   CALL open(*6000,itape,Buf(1),1)
 500  Buf(ii+20) = 1
   k = ii
   ncnt = 2
!
!     LOOP TO INPUT AND SORT CARD IMAGES - USES OPEN CORE FOR SORTED
!     IMAGES
!
   DO n1 = ibufbg , ibuflg , 21
      n2 = n1 + 19
      n3 = n2 + 1
      DO
         CALL xread(*6700,Buf(n1))
         iccnt = iccnt + 1
         IF ( Echou/=0 ) THEN
            CALL page2(-1)
            WRITE (Outtap,99022) (Buf(i),i=n1,n2)
         ENDIF
!
!     IGNORE BLANK CARDS
!
         IF ( Buf(n1)/=Blanx .OR. Buf(n1+1)/=Blanx ) THEN
!
!     LEFT ADJUST FIELD 1
!
            CALL xfadj1(Buf(n1),lshift,0)
!
!     TEST FOR END OF INPUT DATA STREAM (ENDDATA)
!
            iiend(1) = iend1
            iiend(2) = iend2
            IF ( Buf(n1)==iend1 .AND. Buf(n1+1)==iend2 ) GOTO 1100
            iiend(1) = iend3
            iiend(2) = iend4
            IF ( Buf(n1)==iend3 .AND. Buf(n1+1)==iend4 ) GOTO 1100
            iiend(1) = iend5
            iiend(2) = iend6
            IF ( Buf(n1)==iend5 .AND. Buf(n1+1)==iend6 ) GOTO 1100
!
!     IS THIS A CONTINUATION, COMMENT, OR DELETE CARD
!
            IF ( .NOT.dec ) tst = andf(Mk(3),Buf(n1))
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
                  ibk3 = andf(Bk(3),Mbit4)
                  ibk4 = andf(Bk(4),Mbit4)
                  IF ( itst1/=ibk3 .OR. itst2/=ibk4 ) THEN
                     CALL page2(2)
                     WRITE (Outtap,99005) Ufm
99005                FORMAT (A23,' 221, EXTRANEOUS DATA IN FIELD 1 OF BULK DATA ','DELETE CARD.')
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
                        IF ( ptst/=Bk(1) ) GOTO 505
                        IF ( .NOT.dec ) tst = rshift(tst,Sfta)
                        IF ( dec ) tst = khrfn3(Bkmsk2,tst,1,0)
                     ENDDO
                     ny = 3
                  ENDDO
                  GOTO 510
               ENDIF
!
!     STARSW = 0 FOR A SINGLE FIELD CARD (NO STAR)
!            = 1 FOR A DOUBLE FIELD CARD (W/ STAR)
!
 505           starsw = 0
               IF ( ptst==Star ) THEN
                  starsw = 1
                  IF ( j/=1 .OR. i/=1 ) THEN
                     IF ( dec ) THEN
                        Buf(nx) = khrfn1(Buf(nx),5-i,Bk(i),5-i)
                        Buf(n1+1) = khrfn1(Buf(n1+1),4,Star,4)
                     ELSE
                        Buf(nx) = orf(andf(Mk(i),Buf(nx)),Bk(i))
                        Buf(n1+1) = orf(andf(Mk(1),Buf(n1+1)),Star)
                     ENDIF
                  ENDIF
               ENDIF
 510           CALL xfadj(Buf(n1+2),starsw,ny)
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
               EXIT
            ENDIF
         ENDIF
      ENDDO
!
!     K TYPE SUBSCRIPTS REFER TO POSITIONS AND ITEMS IN THE SORTED
!     TABLE CURRENTLY BEING BUILT
!     N TYPE SUBSCRIPTS REFER TO ITEMS ABOUT THE NEWEST CARD IN
!
 550  fcnt = 1
      ni = 0
      ki = 0
      nx = n1
!
!     THE RIGHT SHIFT IN THE FOLLOWING CODE IS USED TO AVOID THE
!     NEGATIVE SIGN PROBLEM WHICH WOULD REVERSE THE SORT ORDER ON SOME
!     MACHINES.
!     (NOTE THAT THE SORT COMPARES CAN BE MADE BOTH WITH OR WITHOUT
!     THE SIGN SHIFT DEPENDING ON THE MACHINES CHARACTER CONFIG)
!
 600  kx = k
      GOTO 700
 650  IF ( Buf(nx)==Buf(kx) ) GOTO 950
      IF ( Buf(nx)==Bk(4) ) GOTO 900
      IF ( Buf(kx)==Bk(4) ) GOTO 850
 700  GOTO mx3
 750  IF ( rshift(Buf(nx),nshift)<rshift(Buf(kx),nshift) ) GOTO 900
      IF ( rshift(Buf(nx),nshift)==rshift(Buf(kx),nshift) ) GOTO 950
      GOTO 850
 800  IF ( dec ) THEN
         IF ( rshift(khrfn4(Buf(nx)),1)<rshift(khrfn4(Buf(kx)),1) ) GOTO 900
         IF ( rshift(khrfn4(Buf(nx)),1)==rshift(khrfn4(Buf(kx)),1) ) THEN
            IF ( rshift(lshift(khrfn4(Buf(nx)),1),1)<rshift(lshift(khrfn4(Buf(kx)),1),1) ) GOTO 900
            IF ( rshift(lshift(khrfn4(Buf(nx)),1),1)==rshift(lshift(khrfn4(Buf(kx)),1),1) ) GOTO 950
         ENDIF
      ELSE
         IF ( Buf(nx)<Buf(kx) ) GOTO 900
         IF ( Buf(nx)<=Buf(kx) ) GOTO 950
      ENDIF
!
!     GO ON, LOOK AT NEXT ITEM IN THE SORTED TABLE
!
 850  kp = k
      k = Buf(k+20)*21 + ii
      IF ( nx/=n1 ) GOTO 550
      GOTO 600
!
!     CARD POSITION FOUND IN SORT, SET THE CHAINING POINTER
!
 900  IF ( kp==0 ) THEN
         k = ii
         GOTO 550
      ELSE
         Buf(n3) = Buf(kp+20)
         Buf(kp+20) = ncnt
         k = kp
         ncnt = ncnt + 1
         CYCLE
      ENDIF
 950  DO
!
!     TWO FIELDS EQUAL - SLIDE TO NEXT FIELD ON CARD
!
         fcnt = fcnt + 1
         nx = nx + 1
         kx = kx + 1
         IF ( fcnt==1 ) GOTO 6600
         IF ( fcnt==3 ) THEN
            IF ( starsw==ktarsw ) GOTO 650
            k1 = 0
            k2 = 0
            IF ( dec ) THEN
               IF ( khrfn1(Bkmsk2,1,Buf(nx),1)/=Bkmsk1(4) ) k1 = 1
               IF ( khrfn1(Bkmsk2,1,Buf(kx),1)/=Bkmsk1(4) ) k2 = 1
            ELSE
               IF ( andf(Mk(3),Buf(nx))/=Bkmsk1(4) ) k1 = 1
               IF ( andf(Mk(3),Buf(kx))/=Bkmsk1(4) ) k2 = 1
            ENDIF
            GOTO 1000
         ELSEIF ( fcnt==4 .OR. fcnt==6 .OR. fcnt==8 .OR. fcnt==10 .OR. fcnt==12 .OR. fcnt==14 .OR. fcnt==16 .OR. fcnt==18 ) THEN
            GOTO 650
         ELSEIF ( fcnt==5 .OR. fcnt==9 .OR. fcnt==13 .OR. fcnt==17 ) THEN
            IF ( starsw/=ktarsw ) EXIT
            IF ( starsw/=0 ) GOTO 650
            EXIT
         ELSEIF ( fcnt==7 .OR. fcnt==15 ) THEN
            EXIT
         ELSEIF ( fcnt==11 ) THEN
            IF ( starsw/=ktarsw ) GOTO 900
            EXIT
         ELSEIF ( fcnt==19 ) THEN
            GOTO 900
         ELSE
            ktarsw = 0
            IF ( .NOT.dec ) itst = andf(Mka,Buf(k+1))
            IF ( dec ) itst = khrfn1(Bkmsk2,4,Buf(k+1),4)
            IF ( itst==Star ) ktarsw = 1
            IF ( starsw==ktarsw ) GOTO 700
!
!     IF ONE MEMBER OF THE 2ND FIELD HAS A STAR AND THE OTHER DOES NOT,
!     DELETE STARS FOR THE COMPARE
!
            IF ( dec ) THEN
               in1 = rshift(khrfn4(khrfn1(Buf(nx),4,Bkmsk2,1)),1)
               ik2 = rshift(khrfn4(khrfn1(Buf(kx),4,Bkmsk2,1)),1)
            ELSE
               in1 = rshift(andf(Mkd,Buf(nx)),1)
               ik2 = rshift(andf(Mkd,Buf(kx)),1)
            ENDIF
            IF ( in1==ik2 ) THEN
               IF ( dec ) THEN
                  in1 = rshift(lshift(khrfn4(khrfn1(Buf(nx),4,Bkmsk2,1)),1),1)
                  ik2 = rshift(lshift(khrfn4(khrfn1(Buf(kx),4,Bkmsk2,1)),1),1)
               ELSE
                  in1 = andf(Mke,Buf(nx))
                  ik2 = andf(Mke,Buf(kx))
               ENDIF
               IF ( in1==ik2 ) CYCLE
            ENDIF
            IF ( in1>=ik2 ) GOTO 850
            GOTO 900
         ENDIF
      ENDDO
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
 1000 IF ( starsw<ktarsw ) THEN
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
      GOTO 650
!
!     END OF BIG SORT LOOP
!
   ENDDO
   GOTO 1200
!
!
!     SET (ENDDATA) CARD FOUND FLAG
!
 1100 iend = -1
   IF ( Echou==1 ) THEN
      CALL page2(2)
      WRITE (Outtap,99006) iccnt
99006 FORMAT (//24X,12HTOTAL COUNT=,I5)
   ENDIF
!
!     TEST FOR COLD-START WITH NO BULK DATA
!
   IF ( iccnt<=1 .AND. irestr<=0 .AND. Kumf<=0 ) THEN
      IF ( Iapprc/=1 ) THEN
         IF ( Isubs==0 ) THEN
            CALL page2(2)
            WRITE (Outtap,99007) Ufm
99007       FORMAT (A23,' 204, COLD START NO BULK DATA.')
            Nogo = -2
            RETURN
         ENDIF
      ENDIF
   ENDIF
!
!
!     IF MODIFIED RESTART - TURN ON SORT ECHO
!
!
!     THIS SECTION UNCHAINS THE SORTED TABLE AND WRITES A CORE LOAD,
!     IN ITS ACTUAL ORDER, ONTO A MERGE SCRATCH TAPE.
!
 1200 j = Buf(ii+20)
   j1st = j*21 + ii
   keep = 1
   DO
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
               GOTO 5000
            ELSE
               itape = itape2
               CALL open(*6000,itape,Buf(Ibufsz+1),1)
               GOTO 500
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
            CALL open(*6000,itape,Buf(1),0)
            GOTO 1700
         ELSEIF ( kop/=1 ) THEN
            DO j = 1 , 18
               ibuf1(j) = ibuf2(j)
            ENDDO
         ENDIF
         DO j = 1 , 18
            IF ( Buf(j1st)/=ibuf1(j) ) GOTO 1300
            j1st = j1st + 1
         ENDDO
         CALL open(*6000,itape,Buf(1),0)
         GOTO 1700
      ELSE
!
!     ITAPE IS PRIMARY CORE UNLOAD TAPE
!
         CALL write(itape,Buf(j),20,1)
         IF ( j<keep ) notsor = 1
         keep = j
         j = j1
      ENDIF
   ENDDO
 1300 GOTO mz1
 1400 IF ( rshift(Buf(j1st),nshift)>=rshift(ibuf1(j),nshift) ) GOTO 1600
!
!     THIS SECTION PERFORMS A 2 TAPE ALPHANUMERIC MERGE
!     (ITAPE+JTAPE=KTAPE)
!     SAME BASIC LOGIC AS ORIGINAL SORT COMPARES (COMMENT CARDS OMITTED)
!
   notsor = 1
   CALL open(*6000,itape,Buf(1),0)
   GOTO 1700
 1500 IF ( dec ) THEN
      IF ( khrfn4(Buf(j1st))<khrfn4(ibuf1(j)) ) THEN
         notsor = 1
         CALL open(*6000,itape,Buf(1),0)
         GOTO 1700
      ENDIF
   ELSEIF ( Buf(j1st)<ibuf1(j) ) THEN
      notsor = 1
      CALL open(*6000,itape,Buf(1),0)
      GOTO 1700
   ENDIF
 1600 trial = ktape
   ktape = jtape
   jtape = trial
   iseq = iseq - 1
   CALL open(*6000,itape,Buf(1),0)
   CALL open(*6000,ktape,Buf(Ibufsz+1),3)
   GOTO 4600
 1700 CALL open(*6000,jtape,Buf(Ibufsz+1),0)
   nbuf2 = 2*Ibufsz + 1
   CALL open(*6000,ktape,Buf(nbuf2),1)
   ccnt = 0
 1800 CALL read(*4500,*6100,jtape,ibuf2,20,1,iflg)
   IF ( Mach==2 .AND. (jtape==umf .OR. jtape==iptp) ) CALL umftrn(ibuf2)
   IF ( Mach==3 .AND. kin==1 ) CALL umffd(ibuf2)
   ldup = 0
   IF ( itape==optp ) CALL crdflg(ibuf2)
   ktarsw = 0
   IF ( .NOT.dec ) itst = andf(Mka,ibuf2(2))
   IF ( dec ) itst = khrfn1(Bkmsk2,4,ibuf2(2),4)
   IF ( itst==Star ) ktarsw = 1
   GOTO my1
 1900 ibuf2a(1) = rshift(ibuf2(1),nshift)
   ibuf2a(2) = rshift(ibuf2(2),nshift)
 2000 CALL read(*4800,*6100,itape,ibuf1,20,1,iflg)
   IF ( Mach==2 .AND. (itape==umf .OR. itape==iptp) ) CALL umftrn(ibuf1)
   IF ( Mach==3 .AND. kin==1 ) CALL umffd(ibuf1)
   starsw = 0
   IF ( .NOT.dec ) itst = andf(Mka,ibuf1(2))
   IF ( dec ) itst = khrfn1(Bkmsk2,4,ibuf1(2),4)
   IF ( itst==Star ) starsw = 1
   GOTO ibranb
 2100 GOTO my2
 2200 ibuf1a(1) = rshift(ibuf1(1),nshift)
   ibuf1a(2) = rshift(ibuf1(2),nshift)
   GOTO 3400
!
!     TEST IF CARD IS TO BE DELETED
!
 2300 ccnt = ccnt + 1
   IF ( .NOT.dec ) tst = andf(Mk(3),ibuf1(1))
   IF ( dec ) tst = khrfn1(Bkmsk2,1,ibuf1(1),1)
   iccflg = -1
   IF ( tst==Plus .OR. tst==Starl ) GOTO 2600
   CALL extint(ibuf1(1))
   GOTO my6
 2400 ibuf1a(1) = rshift(ibuf1(1),nshift)
   ibuf1a(2) = rshift(ibuf1(2),nshift)
 2500 iccflg = 0
   kparnt(1) = ibuf1(1)
   kparnt(2) = ibuf1(2)
 2600 GOTO ibranc
 2700 DO
      CALL read(*3100,*6100,itape5,ibuf3,2,1,iflg)
      IF ( ibuf3(1)/=0 ) THEN
         ASSIGN 2800 TO ibranc
         EXIT
      ENDIF
   ENDDO
 2800 IF ( ibuf3(2)/=0 ) THEN
      IF ( ibuf3(2)==ccnt ) ASSIGN 2700 TO ibranc
      IF ( ibuf3(1)<=ccnt .AND. ibuf3(2)>=ccnt ) GOTO 3200
   ELSEIF ( ibuf3(1)==ccnt ) THEN
      ASSIGN 2700 TO ibranc
      GOTO 3200
   ENDIF
!
!     REMOVE ANY UNDELETED CONTINUATION CARDS DURING RESTART MERGE
!
 2900 IF ( iccflg==0 ) GOTO ibrane
   CALL write(itape4,ibuf1(1),20,1)
 3000 GOTO ibrand
 3100 ASSIGN 2900 TO ibranc
   CALL close(itape5,1)
   GOTO 2900
!
!     IF CONTINUATION WAS DELETED, FLAG PARENT
!
 3200 IF ( iccflg==0 ) THEN
      CALL crdflg(ibuf1)
   ELSE
      CALL crdflg(kparnt)
   ENDIF
   GOTO 3000
 3300 ibuf2a(1) = rshift(ibuf2(1),nshift)
   ibuf2a(2) = rshift(ibuf2(2),nshift)
 3400 j = 1
   j1 = 1
   j2 = 1
   ni = 0
   ki = 0
 3500 GOTO my4
 3600 IF ( ibuf1a(j1)<ibuf2a(j2) ) GOTO 4000
   IF ( ibuf1a(j1)==ibuf2a(j2) ) GOTO 4200
   GOTO 4100
 3700 IF ( ibuf1(j1)==ibuf2(j2) ) GOTO 4200
   IF ( ibuf1(j1)==Bk(4) ) GOTO 4000
   IF ( ibuf2(j2)==Bk(4) ) GOTO 4100
   GOTO my5
 3800 IF ( rshift(ibuf1(j1),1)<rshift(ibuf2(j2),1) ) GOTO 4000
   IF ( rshift(ibuf1(j1),1)==rshift(ibuf2(j2),1) ) GOTO 4200
   GOTO 4100
 3900 IF ( dec ) THEN
      IF ( khrfn4(ibuf1(j1))<khrfn4(ibuf2(j2)) ) THEN
      ELSEIF ( khrfn4(ibuf1(j1))==khrfn4(ibuf2(j2)) ) THEN
         GOTO 4200
      ELSE
         GOTO 4100
      ENDIF
   ELSEIF ( ibuf1(j1)>=ibuf2(j2) ) THEN
      IF ( ibuf1(j1)<=ibuf2(j2) ) GOTO 4200
      GOTO 4100
   ENDIF
 4000 CALL write(ktape,ibuf1,20,1)
   kop = 1
   GOTO 2000
 4100 CALL write(ktape,ibuf2,20,1)
   kop = 2
   CALL read(*4500,*6100,jtape,ibuf2,20,1,iflg)
   IF ( Mach==2 .AND. (jtape==umf .OR. jtape==iptp) ) CALL umftrn(ibuf2)
   IF ( Mach==3 .AND. kin==1 ) CALL umffd(ibuf2)
   IF ( itape==optp ) CALL crdflg(ibuf2)
   ktarsw = 0
   IF ( .NOT.dec ) itst = andf(Mka,ibuf2(2))
   IF ( dec ) itst = khrfn1(Bkmsk2,4,ibuf2(2),4)
   IF ( itst==Star ) ktarsw = 1
   GOTO my3
 4200 DO
      j = j + 1
      j1 = j1 + 1
      j2 = j2 + 1
      IF ( j==1 ) GOTO 6600
      IF ( j==2 ) THEN
         IF ( starsw==ktarsw ) GOTO 3500
         IF ( dec ) THEN
            in1 = rshift(khrfn4(khrfn1(ibuf1(j1),4,Bkmsk2,1)),1)
            ik2 = rshift(khrfn4(khrfn1(ibuf2(j2),4,Bkmsk2,1)),1)
         ELSE
            in1 = rshift(andf(Mkd,ibuf1(j1)),1)
            ik2 = rshift(andf(Mkd,ibuf2(j2)),1)
         ENDIF
         IF ( in1==ik2 ) THEN
            IF ( dec ) THEN
               in1 = rshift(lshift(khrfn4(khrfn1(ibuf1(j1),4,Bkmsk2,1)),1),1)
               ik2 = rshift(lshift(khrfn4(khrfn1(ibuf2(j2),4,Bkmsk2,1)),1),1)
            ELSE
               in1 = andf(Mke,ibuf1(j1))
               ik2 = andf(Mke,ibuf2(j2))
            ENDIF
            IF ( in1==ik2 ) CYCLE
         ENDIF
         IF ( in1>=ik2 ) GOTO 4100
         GOTO 4000
      ELSEIF ( j==3 ) THEN
         IF ( starsw==ktarsw ) GOTO 3700
         k1 = 0
         k2 = 0
         IF ( dec ) THEN
            IF ( khrfn1(Bkmsk2,1,ibuf1(j1),1)/=Bkmsk1(4) ) k1 = 1
            IF ( khrfn1(Bkmsk2,1,ibuf2(j2),1)/=Bkmsk1(4) ) k2 = 1
         ELSE
            IF ( andf(Mk(3),ibuf1(j1))/=Bkmsk1(4) ) k1 = 1
            IF ( andf(Mk(3),ibuf2(j2))/=Bkmsk1(4) ) k2 = 1
         ENDIF
         GOTO 4300
      ELSEIF ( j==4 .OR. j==6 .OR. j==8 .OR. j==10 .OR. j==12 .OR. j==14 .OR. j==16 .OR. j==18 ) THEN
         GOTO 3700
      ELSEIF ( j==5 .OR. j==9 .OR. j==13 .OR. j==17 ) THEN
         IF ( starsw/=ktarsw ) EXIT
         IF ( starsw/=0 ) GOTO 3700
         EXIT
      ELSEIF ( j==11 ) THEN
!
!     DUPLICATE CARD
!
         IF ( starsw/=ktarsw ) GOTO 4400
         EXIT
      ELSEIF ( j==19 ) THEN
         GOTO 4400
      ELSE
         EXIT
      ENDIF
   ENDDO
   IF ( ni<ki ) THEN
      j2 = j2 + ki
      ki = 0
   ELSEIF ( ni/=ki ) THEN
      j1 = j1 + ni
      ni = 0
   ENDIF
   CALL xfadj(ibuf1(j1),starsw,k1)
   CALL xfadj(ibuf2(j2),ktarsw,k2)
 4300 IF ( starsw<ktarsw ) THEN
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
   GOTO 3700
 4400 CALL write(ktape,ibuf1,20,1)
   CALL write(ktape,ibuf2,20,1)
   ldup = -1
   GOTO 1800
!
!     ONE OF TWO TAPES BEING MERGED IS EXHAUSTED, OTHER TAPE IS COPIED
!     ONTO THE MERGE TAPE
!
 4500 IF ( itape==optp ) THEN
      ASSIGN 4600 TO ibrand
      ASSIGN 4700 TO ibrane
      ASSIGN 2300 TO ibranf
      IF ( ccnt==0 ) GOTO 4600
   ENDIF
   IF ( ldup>=0 ) GOTO 4700
 4600 CALL read(*4900,*6100,itape,ibuf1,20,1,iflg)
   IF ( Mach==2 .AND. (itape==umf .OR. itape==iptp) ) CALL umftrn(ibuf1)
   IF ( Mach==3 .AND. kin==1 ) CALL umffd(ibuf1)
   GOTO ibranf
 4700 CALL write(ktape,ibuf1,20,1)
   kop = 1
   GOTO 4600
 4800 DO
      CALL write(ktape,ibuf2,20,1)
      kop = 2
      CALL read(*4900,*6100,jtape,ibuf2,20,1,iflg)
      IF ( Mach==2 .AND. (jtape==umf .OR. jtape==iptp) ) CALL umftrn(ibuf2)
      IF ( Mach==3 .AND. kin==1 ) CALL umffd(ibuf2)
   ENDDO
 4900 IF ( Iuedit==1 ) THEN
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
 5000 IF ( iend==0 ) THEN
      CALL open(*6000,itape,Buf(1),1)
      GOTO 500
   ELSE
      CALL page2(2)
      WRITE (Outtap,99025)
      CALL close(itape5,1)
!
!     PROCESS DELETE CARDS (IF ANY)
!
      nbuf4 = 4*Ibufsz + 1
      CALL open(*6000,itape5,Buf(nbuf4),0)
!
!     IF NOT RESTART - NO DELETES SHOULD EXIST
!
      IF ( irestr>0 .OR. kin>0 ) THEN
!
!     FORM DELETE CARD LIST
!
         ibuf3(1) = Inf
         Buf(ii) = Mkb
         Buf(ii+1) = Mkb
         DO j = ii , ibuflg , 2
            CALL read(*5100,*6100,itape5,ibuf3,2,1,iflg)
            DO i = ii , j , 2
               IF ( ibuf3(1)<=Buf(i) ) EXIT
            ENDDO
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
         CALL read(*5200,*6100,itape5,ibuf3,1,1,iflg)
!
!     NOT RESTART AND DELETES DO EXIST - WARNING
!
         CALL close(itape5,1)
         CALL page2(2)
         WRITE (Outtap,99008) Uwm
99008    FORMAT (A25,' 205, COLD START,DELETE CARDS IGNORED.')
         GOTO 5200
      ENDIF
   ENDIF
!
!     EOF ON ITAPE5, IF IBUF3(1)= INF, THERE ARE NO DELETE CARDS
!
 5100 IF ( ibuf3(1)/=Inf ) THEN
      j = j - 1
!
!     CHECK FOR AND ELIMINATE OVERLAPS AND REDUNDANCYS IN DELETES
!
      imin = 0
      DO i = ii , j , 2
         IF ( Buf(i)/=0 ) THEN
            IF ( Buf(i)<Buf(i+1) ) THEN
               IF ( imin==0 ) GOTO 5120
               IF ( Buf(i)>Buf(imax) ) GOTO 5120
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
 5120    imin = i
         imax = i + 1
      ENDDO
      CALL close(itape5,1)
!
!     PUT OUT SORTED DELETE CARD LIST
!
      nbuf4 = 4*Ibufsz + 1
      CALL open(*6000,itape5,Buf(nbuf4),1)
      DO i = ii , j , 2
         IF ( Buf(i)/=0 ) CALL write(itape5,Buf(i),2,1)
      ENDDO
   ENDIF
   CALL close(itape5,1)
!
!     AT THIS POINT, IF THIS IS A RESTART, MERGE OPTP, FINAL KTAPE,
!     + DELETE
!
   ASSIGN 5200 TO ibrana
   ASSIGN 2300 TO ibranb
   ASSIGN 2700 TO ibranc
   ASSIGN 2000 TO ibrand
   ASSIGN 3400 TO ibrane
   nbuf4 = 4*Ibufsz + 1
   CALL open(*6000,itape5,Buf(nbuf4),0)
!
   IF ( kin>0 ) THEN
!
      optp = umf
      CALL open(*200,umf,Buf(1),2)
   ELSE
!
      CALL open(*6400,optp,Buf(1),0)
      DO
         CALL read(*6300,*6100,optp,ibuf3,2,1,iflg)
         IF ( ibuf3(1)==iblkda(1) .AND. ibuf3(2)==iblkda(2) ) EXIT
         CALL skpfil(optp,+1)
      ENDDO
   ENDIF
   itape = optp
   trial = jtape
   jtape = ktape
   ktape = trial
   IF ( iccnt/=1 ) THEN
      CALL write(itape4,Mkb,20,1)
      GOTO 1700
   ENDIF
!
!     PROCESS CONTINUATION CARDS (IF ANY)
!
 5200 CALL close(itape4,1)
   nbuf3 = 3*Ibufsz + 1
   CALL open(*6000,itape4,Buf(nbuf3),0)
   IF ( iccnt==1 .AND. (irestr>0 .OR. kin>0) ) ktape = optp
   IF ( .NOT.(iccnt==1 .AND. (irestr>0 .OR. kin>0)) ) THEN
      nbuf2 = 2*Ibufsz + 1
      CALL open(*6000,ktape,Buf(nbuf2),0)
   ENDIF
!
!     FORM CONTINUATION CARD DICTIONARY
!
   ibuf1(1) = 0
   DO j = ii , ibuflg , 4
      CALL read(*5300,*6100,itape4,ibuf1,20,1,iflg)
      IF ( Mach==2 .AND. ibuf1(1)/=Mkb ) CALL umftrn(ibuf1)
      IF ( Mach==3 .AND. kin==1 ) CALL umffd(ibuf1)
      IF ( ibuf1(1)/=Mkb ) THEN
         IF ( .NOT.dec ) Buf(j) = andf(Mkc,ibuf1(1))
         IF ( dec ) Buf(j) = khrfn1(ibuf1(1),1,Bkmsk2,1)
      ELSE
         IF ( j/=ii ) iccbrk = j
         Buf(j) = Dollar
      ENDIF
      Buf(j+1) = ibuf1(2)
      IF ( .NOT.dec ) Buf(j+2) = andf(Mkc,ibuf1(19))
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
 5300 IF ( ibuf1(1)/=0 ) THEN
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
99009 FORMAT (A29,' 207, BULK DATA NOT SORTED, XSORT WILL RE-ORDER ','DECK.')
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
   CALL open(*6500,nptp,Buf(nbuf4),3)
   CALL write(nptp,iblkda,2,1)
   IF ( ibuf1(1)==0 ) GOTO 5600
!
!     MERGE CONTINUATION CARDS - PRODUCE DATA ON NPTP
!
 5400 CALL read(*5700,*6100,ktape,ibuf1,20,1,iflg)
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
   IF ( ibuf1(19)/=Bk(4) .OR. ibuf1(20)/=Bk(4) ) THEN
      IF ( .NOT.dec ) trial = andf(Mkc,ibuf1(19))
      IF ( dec ) trial = khrfn1(ibuf1(19),1,Bkmsk2,1)
      jn = 0
      DO
!
         DO k = ii , iconlg , 4
!
!     IGNORE DUPLICATE CONTINUATION CARDS
!
            IF ( Buf(j)/=idup ) THEN
               IF ( ibuf1(20)==Buf(j+1) ) THEN
                  IF ( .NOT.dec ) itst = andf(Mkc,Buf(j))
                  IF ( dec ) itst = khrfn1(Buf(j),1,Bkmsk2,1)
                  IF ( itst==trial ) THEN
!
!     A CONTINUATION EXISTS, HAS IT ALREADY BEEN USED
!
                     IF ( .NOT.dec ) itst = andf(Mk(3),Buf(j))
                     IF ( dec ) itst = khrfn1(Bkmsk2,1,Buf(j),1)
                     IF ( itst==Dollar ) GOTO 5500
                     IF ( j<=iccbrk ) CALL crdflg(kparnt)
                     IF ( .NOT.dec ) Buf(j) = orf(Buf(j),Dollar)
                     IF ( dec ) Buf(j) = khrfn1(Buf(j),1,Dollar,1)
                     jn = (j-ii)/4 + 1
                     CALL xrecps(jn,jo)
                     CALL read(*6200,*6100,itape4,ibuf1,20,1,iflg)
                     IF ( Mach==2 ) CALL umftrn(ibuf1)
                     IF ( Mach==3 .AND. kin==1 ) CALL umffd(ibuf1)
                     CALL write(nptp,ibuf1,20,1)
                     IF ( Echos/=0 ) THEN
                        CALL page2(-1)
                        ccnt = ccnt + 1
                        WRITE (Outtap,99023) ccnt , ibuf1
                     ENDIF
                     IF ( Echop/=0 ) WRITE (Lpch,99024) ibuf1
                     IF ( .NOT.dec ) trial = andf(Mkc,ibuf1(19))
                     IF ( dec ) trial = khrfn1(ibuf1(19),1,Bkmsk2,1)
                     IF ( ibuf1(19)/=Bk(4) .OR. ibuf1(20)/=Bk(4) ) GOTO 5450
                     EXIT
                  ENDIF
               ENDIF
            ENDIF
            j = j + 4
            IF ( j>iconlg ) j = ii
         ENDDO
         EXIT
 5450 ENDDO
   ENDIF
   GOTO 5400
!
!     DUPLICATE PARENT - ERROR
!
 5500 nl = 0
   IF ( Echos==0 ) THEN
      nl = 1
      WRITE (Outtap,99022) ibuf1
   ENDIF
   nl = nl + 2
   CALL page2(-nl)
   WRITE (Outtap,99010) Ufm
99010 FORMAT (A23,' 208, PREVIOUS CARD IS A DUPLICATE PARENT.')
   Nogo = -1
   GOTO 5400
 5600 DO
!
!     NO CONTINUATION CARDS
!
      CALL read(*5700,*6100,ktape,ibuf2,20,1,iflg)
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
!
!     CLOSE KTAPE AND WRITE (ENDDATA)
!
 5700 CALL close(ktape,2)
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
         IF ( .NOT.dec ) itst = andf(Mk(3),Buf(j))
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
                     IF ( Buf(j)==Buf(jj+2) .AND. Buf(j+1)==Buf(jj+3) ) GOTO 5720
                  ENDIF
               ENDDO
            ENDIF
            ncnt = ncnt + 1
            jn = (j-ii)/4 + 1
            CALL xrecps(jn,jo)
            CALL read(*6200,*6100,itape4,ibuf2,20,1,iflg)
            IF ( Mach==2 ) CALL umftrn(ibuf2)
            IF ( Mach==3 .AND. kin==1 ) CALL umffd(ibuf2)
            CALL page2(-1)
            WRITE (Outtap,99022) ibuf2
         ENDIF
         CYCLE
 5720    Buf(j) = iok
      ENDDO
      IF ( ncnt/=0 ) THEN
         CALL page2(3)
         WRITE (Outtap,99011) Ufm , ncnt
99011    FORMAT (A23,' 209, PREVIOUS',I7,' CONTINUATION MNEMONICS HAVE NO',' PARENTS AND/OR ARE DUPLICATES.',/)
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
               CALL read(*6200,*6100,itape4,ibuf2,20,1,iflg)
               IF ( Mach==2 ) CALL umftrn(ibuf2)
               IF ( Mach==3 .AND. kin==1 ) CALL umffd(ibuf2)
               CALL page2(-1)
               WRITE (Outtap,99022) ibuf2
            ENDIF
         ENDDO
         IF ( ncnt/=0 ) THEN
            CALL page2(4)
            WRITE (Outtap,99012) Ufm , ncnt
99012       FORMAT (A23,' 206, PREVIOUS',I7,' CONTINUATION CARDS, THOUGH ','VALID, CANNOT BE PROCESSED',/5X,                        &
                   &'BECAUSE OF ERRORS ON OTHER RELATED CONTINUATION CARDS.',/)
         ENDIF
      ENDIF
   ENDIF
   CALL close(itape4,1)
!
!     REACTIVE DIAG 47 TO PRINT THE CONTENTS OF NTPT
!
   l47 = 0
   IF ( l47==0 ) GOTO 5900
   CALL open(*6500,nptp,Buf(1),0)
   DO
      CALL skpfil(nptp,+1)
      CALL read(*5800,*5800,nptp,ibuf1(1),2,1,j)
      IF ( ibuf1(1)==iblkda(1) .AND. ibuf1(2)==iblkda(2) ) THEN
         DO
            CALL read(*5800,*5800,nptp,ibuf1(1),20,1,j)
            WRITE (Outtap,99013) (ibuf1(j),j=1,10) , (ibuf1(j),j=17,20)
99013       FORMAT (' ==NPTP==>',5(1X,2A4),'...',2(1X,2A4))
         ENDDO
      ENDIF
   ENDDO
 5800 CALL close(nptp,1)
!
!     DISABLE FREE-FIELD INPUT OPTION IN XREAD.
!
 5900 Ffflag = 0
   RETURN
!
!     ERROR MESSAGES
!
 6000 WRITE (Outtap,99014) Sfm
99014 FORMAT (A25,' 210, SCRATCH COULD NOT BE OPENED')
   GOTO 6800
 6100 WRITE (Outtap,99015) Sfm
99015 FORMAT (A25,' 211, ILLEGAL EOR ON SCRATCH')
   GOTO 6800
 6200 WRITE (Outtap,99016) Sfm
99016 FORMAT (A25,' 212, ILLEGAL EOF ON ITAPE4')
   GOTO 6800
 6300 WRITE (Outtap,99017) Sfm
99017 FORMAT (A25,' 213, ILLEGAL EOF ON OPTP')
   GOTO 6800
 6400 WRITE (Outtap,99018) Sfm
99018 FORMAT (A25,' 214, OPTP COULD NOT BE OPENED')
   GOTO 6800
 6500 WRITE (Outtap,99019) Sfm
99019 FORMAT (A25,' 215, NPTP COULD NOT BE OPENED')
   GOTO 6800
 6600 WRITE (Outtap,99020) Sfm
99020 FORMAT (A25,' 216, ILLEGAL INDEX')
   GOTO 6800
 6700 WRITE (Outtap,99021) Sfm
99021 FORMAT (A25,' 219, MISSING ENDDATA CARD.')
 6800 CALL page2(2)
   CALL mesage(-37,0,nsort)
99022 FORMAT (30X,20A4)
99023 FORMAT (13X,I8,1H-,8X,20A4)
99024 FORMAT (20A4)
99025 FORMAT (1H0)
END SUBROUTINE xsort
