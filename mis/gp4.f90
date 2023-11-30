
SUBROUTINE gp4
!
!     GP4  PERFORMS THE FOLLOWING FUNCTIONS--
!       1. READS CASECC AND MAKES ANALYSIS OF SUBCASE LOGIC
!       2. PROCESSES RIGID ELEMENTS AND ALL OTHER CONSTRAINT DATA (MPC,
!          SPC, OMIT, SUPORT, ASET, ETC.)
!       3. BUILDS THE USET FOR THE CURRENT SUBCASE
!       4. CALLS GP4SP TO EXAMINE GRID POINT SINGULARITIES
!       5. BUILDS THE RGT MATRIX AND YS VECTOR FOR CURRENT SUBCASE
!
   IMPLICIT NONE
   INTEGER Bgpdt , Buf(20) , Buf1 , Buf2 , Buf3 , Buf4 , Clsrew , Cstm , Geomp , Gpoint , Iautsp , Iaxic , Iaxif , Ib6 , Idsub ,    &
         & Ihalf , Ii , Ii1 , Incr , Incr1 , Iogpst , Ita1 , Itb , Itb1 , Ix , Jhalf , Jj , Jj1 , Kn , Knkl1 , Ksystm(65) , Luset , &
         & Mach , Mask16 , Mpcf1 , Mpcf2 , Mpcset , Msksng , Mskul , Mskum , Mskuo , Mskur , Mskus , Nauto , Noa , Nogo , Nol ,     &
         & Nosets , Nskip , Omit1 , Outtap , Rd , Rdrew , React , Repeat , Rgt , Scr1 , Single , Spcset , Sysbuf , Two(32) , Ua ,   &
         & Uf , Ug , Ul , Um , Un , Uo , Ur , Us , Usb , Usg , Wrt , Wrtrew , X(4) , Z(1)
   REAL Bufr(2) , Rz(1)
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /bitpos/ Um , Uo , Ur , Usg , Usb , Ul , Ua , Uf , Us , Un , Ug
   COMMON /blank / Luset , Mpcf1 , Mpcf2 , Single , Omit1 , React , Nskip , Repeat , Nosets , Nol , Noa , Idsub , Iautsp
   COMMON /gp4fil/ Geomp , Bgpdt , Cstm , Rgt , Scr1
   COMMON /gp4prm/ Buf , Buf1 , Buf2 , Buf3 , Buf4 , Knkl1 , Mask16 , Nogo , Gpoint , Kn
   COMMON /gp4spx/ Mskum , Mskuo , Mskur , Mskus , Mskul , Msksng , Spcset , Mpcset , Nauto , Iogpst
   COMMON /machin/ Mach , Ihalf , Jhalf
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew
   COMMON /packx / Ita1 , Itb1 , Ii1 , Jj1 , Incr1
   COMMON /system/ Ksystm
   COMMON /two   / Two
   COMMON /unpakx/ Itb , Ii , Jj , Incr
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zblpkx/ X , Ix
   COMMON /zzzzzz/ Z
   INTEGER andf , complf , korsz , lshift , orf , rshift
   INTEGER aset(2) , aset1(2) , asetx , casecc , ctype(18) , dup , eqexin , file , flag , gpdt , i , i1 , i2 , i3 , icode , icomp , &
         & icount , icrq , id , idepn , ifl , iflag , iflg , igotch , ijk , ik , iload , impc , impcad , imsk , index , insuff ,    &
         & intrnl , inys , iold , ioys , ipoint , irecn , isil , ispc , ispcad , iuset , iz138 , iz16 , iz2 , iz3 , iz5 , j , jjx , &
         & jxx , k , khi , kj , kk , klo , km , kn2 , komp , l21 , l22 , l51 , lastk , m , mak(4) , mask(6) , mask15 , mcb(7) ,     &
         & mcbust(7) , mcbys(7) , mpc(2) , mpcadd(2) , mpcax1 , mpcax2 , mpcold , mset , msk1 , msk2 , mskal , mskall , mskck
   INTEGER mskfng , mskrst , mskua , mskuf , mskug , mskums , mskun , mskung , mskuor , mskusb , mskusg , multi , n , n23 , n231 ,  &
         & name(2) , ncol , neqx , nmpc , nmpcad , nn , nn1 , nnx , nogeom , nogoo , nogoof , nold , noys , nrigid , nskp1 , nspc , &
         & nspcad , omit(2) , omitx1(2) , r , ret , ret1 , ret2 , ret3 , rigid , scr2 , sg , sild , spc(2) , spc1(2) , spcadd(2) ,  &
         & spcd(2) , spcold , suport(2) , ugset , uset , usgset , ys
   EXTERNAL andf , complf , lshift , orf , rshift
!WKBI 3/95 NCL94002
!WKBR 3/95 NCL94002      COMMON /XMSSG / UFM
   EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Outtap) , (Ksystm(27),Iaxic) , (Ksystm(38),Iaxif) , (Z(1),Rz(1)) , (Buf(1),Bufr(1)) ,&
    & (ugset,usgset) , (Ib6,Buf(6))
   DATA omit/5001 , 50/ , suport/5601 , 56/ , spc/5501 , 55/ , spc1/5481 , 58/ , spcadd/5491 , 59/ , omitx1/4951 , 63/ , aset/5561 ,&
      & 76/ , aset1/5571 , 77/ , spcd/5110 , 51/ , mpc/4901 , 49/ , mpcadd/4891 , 60/
   DATA name/4HGP4  , 4H    /
   DATA mset/4H M  / , sg/4H SG / , r/4H R  /
   DATA ys , uset/202 , 203/
   DATA scr2/302/
   DATA mpcax1 , mpcax2/101 , 102/
   DATA casecc , eqexin , gpdt/101 , 103 , 104/
   DATA ctype/4HMPC  , 4H     , 4HOMIT , 4H     , 4HOMIT , 4H1    , 4HSUPO , 4HRT   , 4HSPC1 , 4H     , 4HSPC  , 4H     , 4HSPCD ,  &
       &4H     , 4HASET , 4H     , 4HASET , 4H1   /
   DATA iz2 , iz3 , iz5 , iz16 , iz138/2 , 3 , 5 , 16 , 138/
!
!     PERFORM GENERAL INITIALIZATION
!
!WKBI 3/95 NCL94002
   CALL sswtch(51,l51)
   Geomp = 102
   Bgpdt = 105
   Cstm = 106
   Rgt = 201
   Scr1 = 301
   Nauto = 0
   Iogpst = -1
   Buf1 = korsz(Z) - Sysbuf - 2
   Buf2 = Buf1 - Sysbuf
   Buf3 = Buf2 - Sysbuf
   Buf4 = Buf3 - Sysbuf
   icrq = Luset - Buf4
   insuff = 10
   IF ( Luset>=Buf4 ) GOTO 9400
   Mask16 = Jhalf
   mask15 = Jhalf/2
   n23 = 2
   Mskum = Two(Um)
   Mskuo = Two(Uo)
   Mskur = Two(Ur)
   mskusg = Two(Usg)
   mskusb = Two(Usb)
   Mskul = Two(Ul)
   mskua = Two(Ua)
   mskuf = Two(Uf)
   Mskus = Two(Us)
   mskun = Two(Un)
   mskug = Two(Ug)
   mskung = orf(mskun,mskug)
   mskfng = orf(mskuf,mskung)
   Msksng = orf(Mskus,mskung)
   mask(1) = orf(Mskum,mskug)
   mask(2) = orf(Mskuo,mskfng)
   mask(3) = orf(Mskur,orf(mskua,mskfng))
   mask(4) = orf(mskusg,Msksng)
   mask(5) = orf(mskusb,Msksng)
   mask(6) = orf(Mskul,orf(mskua,mskfng))
   mak(1) = orf(Mskum,Mskul)
   mak(2) = orf(Mskus,Mskul)
   mak(3) = orf(Mskuo,Mskul)
   mak(4) = orf(Mskur,Mskul)
   CALL makmcb(mcbys,ys,0,2,1)
   CALL makmcb(mcbust,uset,Luset,0,0)
   multi = -1
   usgset = -1
   Single = -1
   Omit1 = -1
   Nosets = -1
   asetx = -1
   React = -1
   noys = 0
   nogeom = 0
   Nol = -1
   Noa = +1
   Nogo = 0
   nogoof = 0
   dup = 0
   iflag = 0
   flag = 0
   mskck = complf(lshift(complf(0),20))
   rigid = 0
   spcold = -1
   mpcold = -1
   l21 = 0
   l22 = 0
   mcb(1) = Geomp
   CALL rdtrl(mcb(1))
   IF ( mcb(1)>=0 ) THEN
!
!     BIT ASSIGNMENTS FOR RIGID ELEMENTS -
!     CRIGD1 - 53       CRROD    - 65       CRBE1 - 68
!     CRIGD2 - 54       CRBAR    - 66       CRBE2 - 69
!     CRIGD3 - 83       CRTRPLT  - 67       CRBE3 - 70
!     CRIGDR - 82       CRSPLINE - 71
!
      IF ( andf(mcb(5),Two(21))==Two(21) ) rigid = 1
      IF ( andf(mcb(5),Two(22))==Two(22) ) rigid = 1
      IF ( andf(mcb(7),Two(19))==Two(19) ) rigid = 1
      IF ( andf(mcb(7),Two(18))==Two(18) ) rigid = 1
      i = mcb(6)
      DO j = 17 , 23
         IF ( andf(i,Two(j))==Two(j) ) rigid = 1
      ENDDO
      CALL makmcb(mcb,Rgt,0,2,1)
   ENDIF
!
!     SUBCASE LOGIC -- NSKIP IS 0 (SET BY PARAM MODULE) IF FIRST
!     SUBCASE. OTHERWISE NSKIP IS THE NO. OF RECORDS TO SKIP ON CASE
!     CONTROL DATA BLOCK TO REACH THE LAST SUBCASE. GP4 SETS THE
!     FOLLOWING PARAMETERS -
!     (1) MPCF1 = +1 (DO NOT PURGE OR EQUIV MCE DATA BLOCKS) = -1 (PURGE
!                 AND EQUIV TO TAKE).
!     (2) MPCF2 = +1 (EXECUTE MCE1 AND MCE2) = -1 (DO NOT EXECUTE)
!     (3) REPEAT= +1 (MORE SUBCASES AFTER THIS ONE) = -1 (LAST SUBCASE).
!     (4) NSKIP = NO. OF RECORDS TO SKIP ON CASE CONTROL TO REACH THE
!                 CURRENT SUBCASE (FOR MODULES IN REMAINDER OF LOOP).
!
   Repeat = -1
   Mpcf1 = -1
   Mpcf2 = -1
   nskp1 = 1
   file = casecc
   CALL gopen(casecc,Z(Buf1),0)
   IF ( Nskip>1 ) CALL skprec(casecc,Nskip-1)
   CALL fread(casecc,Z,36,1)
   IF ( Nskip>0 ) THEN
!
!     SUBSEQUENT SUBCASE - POSITION CASE CONTROL AND INITIALIZE.
!
      mpcold = Z(iz2)
      spcold = Z(iz3)
      DO
         Nskip = Nskip + 1
         CALL fread(casecc,Z,36,1)
         IF ( Z(iz16)==0 ) THEN
            IF ( Z(iz2)/=mpcold .OR. Z(iz3)/=spcold ) THEN
               Mpcset = Z(iz2)
               Spcset = Z(iz3)
               EXIT
            ENDIF
         ENDIF
      ENDDO
   ELSE
!
!     FIRST SUBCASE - INITIALIZE.
!
      Mpcset = Z(iz2)
      Spcset = Z(iz3)
      Nskip = 1
   ENDIF
   DO
!
!     LOOK AHEAD TO END OF CURRENT SUBCASE AND SET PARAMETERS.
!
      CALL read(*100,*9300,casecc,Z,138,1,flag)
!
!     CHECK FOR SYMMETRY
!
      IF ( Z(iz16)==0 ) THEN
!
!     CHECK FOR BUCKLING OR DIFFERENTIAL STIFFNESS
!
         IF ( Z(iz5)/=0 .OR. Z(iz138)/=0 ) EXIT
         IF ( Z(iz2)==Mpcset .AND. Z(iz3)==Spcset ) THEN
!
            nskp1 = nskp1 + 1
         ELSE
            Repeat = 1
            EXIT
         ENDIF
      ENDIF
   ENDDO
!
!     CHECK TO SEE IF MPC SET IS SELECTED OR IF RIGID ELEMENTS EXIST
!
 100  IF ( Mpcset/=0 .OR. rigid/=0 ) THEN
      Mpcf1 = 1
      Mpcf2 = 1
      IF ( Nskip/=1 ) THEN
         IF ( Mpcset==mpcold ) Mpcf2 = -1
      ENDIF
   ENDIF
   CALL close(casecc,Clsrew)
   ASSIGN 400 TO ret
!
!     READ EQEXIN INTO CORE
!
 200  file = eqexin
   CALL gopen(eqexin,Z(Buf1),0)
   CALL read(*9200,*300,eqexin,Z,Buf4,1,Kn)
   insuff = 80
   icrq = Buf4
   GOTO 9400
 300  CALL read(*9200,*9300,eqexin,Z(Kn+1),Kn,1,flag)
   CALL close(eqexin,Clsrew)
   km = 2*Kn
   kn2 = Kn/2
!
!     FORM ARRAY OF SORTED SIL VALUES STARTING AT Z(KM+1)
!
   DO i = 1 , kn2
      j = 2*(i-1) + 2 + Kn
      Z(km+i) = Z(j)/10
   ENDDO
   CALL sort(0,0,1,1,Z(km+1),kn2)
   Z(km+kn2+1) = Luset + 1
   Knkl1 = km + kn2 + 2
!
!     SET DIAG-S 21 AND 22 FOR DEGREE-OF-FREEDOM PRINTER LATER.
!
   CALL sswtch(21,l21)
   CALL sswtch(22,l22)
   GOTO ret
!
!     OPEN INPUT DATA FILE
!
 400  file = Geomp
   CALL preloc(*500,Z(Buf1),Geomp)
   nogeom = 1
!
!     CHECK TO SEE IF MPC SET IS SELECTED OR IF RIGID ELEMENTS EXIST
!
   IF ( Mpcset/=0 .OR. rigid/=0 ) THEN
!
!     OPEN RGT FILE
!
      file = Rgt
      CALL gopen(Rgt,Z(Buf3),1)
!
!     IF RIGID ELEMENTS EXIST, GENERATE THEIR COEFFICIENTS
!
      nogoo = Nogo
      Nogo = 0
      IF ( rigid==1 ) CALL criggp(n23)
      IF ( Nogo/=0 ) GOTO 10400
      Nogo = nogoo
   ENDIF
!
!     OPEN SCRATCH DATA FILE
!
 500  file = Scr1
   CALL open(*9100,Scr1,Z(Buf2),Wrtrew)
!
!     CHECK TO SEE IF GEOMP FILE EXISTS
!
   IF ( nogeom==0 ) GOTO 3600
!
!     CHECK TO SEE IF MPC SET IS SELECTED OR IF RIGID ELEMENTS EXIST
!
   IF ( Mpcset==0 .AND. rigid==0 ) GOTO 2100
   IF ( Mpcset/=0 ) THEN
!
!     IF MPC SET IS SELECTED, DETERMINE IF SET IS ON MPCADD CARD.
!     IF NOT, SIMULATE AN MPCADD SET LIST WITH ONE SET = MPCSET.
!
      impcad = Knkl1
      nmpcad = Knkl1
      impc = impcad + 2
      i = impcad
      Z(i) = Mpcset
      Z(i+1) = 0
      file = Geomp
      CALL locate(*700,Z(Buf1),mpcadd,flag)
      DO
         CALL read(*9200,*700,Geomp,id,1,0,flag)
         IF ( id==Mpcset ) THEN
            DO
               CALL read(*9200,*600,Geomp,Buf,1,0,flag)
               IF ( Buf(1)==-1 ) THEN
                  CALL fwdrec(*9200,Geomp)
                  GOTO 600
               ELSE
                  Z(i) = Buf(1)
                  Z(i+1) = 0
                  i = i + 2
               ENDIF
            ENDDO
         ELSE
            DO
               CALL fread(Geomp,Buf,1,0)
               IF ( Buf(1)==-1 ) EXIT
            ENDDO
         ENDIF
      ENDDO
   ELSE
!
!     NO MPC SET IS SELECTED
!
      multi = 0
      impc = Knkl1
      i = impc
      j = Buf3 - 1
      GOTO 1400
   ENDIF
 600  impc = i
   nmpcad = i - 2
!
!     READ MPC CARDS. FOR EACH EQUATION WHOSE SET ID MATCHES A SET ID
!     IN THE MPCADD SET LIST, CONVERT THE GRID POINT AND COMPONENT NO.
!     (OR SCALAR NO.) TO A SIL VALUE. COMPUTE THE ROW AND COLUMN NO.
!     FOR THE POINT AND SAVE THIS ALONG WITH ITS VALUE.
!
 700  CALL locate(*1300,Z(Buf1),mpc,flag)
   j = Buf3 - 1
   i = impc
   multi = 0
   ASSIGN 1100 TO ret
   ASSIGN 9600 TO ret1
   ASSIGN 1000 TO ret2
   ASSIGN 1200 TO ret3
 800  CALL read(*9200,*1300,Geomp,id,1,0,flag)
   DO k = impcad , nmpcad , 2
      IF ( Z(k)==id ) GOTO 900
   ENDDO
   DO
      CALL fread(Geomp,Buf,3,0)
      IF ( Buf(1)==-1 ) GOTO 800
   ENDDO
 900  multi = multi + 1
   Z(k+1) = 1
   ifl = 0
 1000 CALL fread(Geomp,Buf,3,0)
   IF ( Buf(1)==-1 ) THEN
!
!     SAVE A LIST OF DEPENDENT SIL VALUES
!
      Z(j) = sild
      j = j - 1
      GOTO 800
   ELSE
      Gpoint = Buf(1)
      GOTO 8800
   ENDIF
 1100 index = 1
   icomp = Buf(2)
   GOTO 8900
 1200 IF ( icomp/=0 ) Gpoint = Gpoint + icomp - 1
   IF ( ifl==0 ) sild = Gpoint
   IF ( n23==3 ) THEN
      Z(i) = Gpoint
      Z(i+1) = sild
      Z(i+2) = Buf(3)
   ELSEIF ( Gpoint>mask15 ) THEN
!
!     GPOINT IS TOO BIG TO BE PACKED INTO HALF A WORD.  ABANDON COL.
!     AND ROW PACKING LOGIC, AND DO IT OVER AGAIN WITHOUT PACKING.
!
      n23 = 3
      CALL rewind(Geomp)
      CALL fwdrec(*9200,Geomp)
      GOTO 700
   ELSE
      Z(i) = orf(lshift(Gpoint,Ihalf),sild)
      Z(i+1) = Buf(3)
   ENDIF
   i = i + n23
   insuff = 236
   IF ( i>=j ) GOTO 9400
   ifl = 1
   GOTO 1000
!
!     DETERMINE IF ALL MPC SETS IN MPCADD SET LIST HAVE BEEN INPUT
!
 1300 IF ( Nogo/=0 ) GOTO 10400
   nogoo = Nogo
   Nogo = 0
   igotch = 0
   DO k = impcad , nmpcad , 2
      IF ( Z(k+1)/=0 ) THEN
         igotch = 1
      ELSE
         Nogo = -1
         IF ( Z(k)/=200000000 .OR. Iaxif==0 ) THEN
            IF ( Iaxic/=0 ) THEN
               IF ( Z(k)==mpcax1 .OR. Z(k)==mpcax2 ) CYCLE
               IF ( Z(k)==200000000 ) CYCLE
            ENDIF
            Nogo = +1
            Buf(1) = Z(k)
            Buf(2) = 0
            CALL mesage(30,47,Buf)
         ENDIF
      ENDIF
   ENDDO
   IF ( Nogo/=0 ) THEN
      IF ( Nogo==-1 .AND. igotch==1 ) THEN
         IF ( Nogo==-1 .AND. nogoo==0 ) Nogo = 0
      ELSE
         Mpcset = 0
         multi = -1
         Mpcf1 = -1
         Mpcf2 = -1
         IF ( Nogo==-1 .AND. nogoo==0 ) Nogo = 0
         CALL close(Rgt,Clsrew)
         GOTO 2100
      ENDIF
   ENDIF
!
!     CHECK TO SEE IF RIGID ELEMENTS EXIST
!
 1400 IF ( rigid==0 ) GOTO 1900
!
!     EXPAND THE DEPENDENT SET BY APPENDING RIGID ELEMENT
!     DATA TO MPC DATA
!
   CALL gopen(Rgt,Z(Buf3),0)
   CALL skprec(Rgt,1)
   i1 = Buf3 - i
   CALL read(*9200,*1500,Rgt,Z(i),i1,1,nrigid)
   insuff = 3020
   GOTO 9400
 1500 j = j - nrigid
   multi = multi + nrigid
   CALL skprec(Rgt,-2)
   CALL read(*9200,*1700,Rgt,Z(i),i1,1,flag)
   insuff = 3030
   i2 = i1
   DO
      CALL bckrec(Rgt)
      CALL read(*9200,*1600,Rgt,Z(i),-i2,0,flag)
      CALL read(*9200,*1600,Rgt,Z(i),i1,0,flag)
      i2 = i2 + i1
   ENDDO
 1600 flag = i2 + flag
   GOTO 1800
!
!     RE-CODE COLUMN-ROW PACKED WORD IF NECESSARY FOR DATA JUST BROUGHT
!     IN FROM RIGID ELEMENTS
!     THEN READ THE LAST RECORD FROM RGT
!
 1700 IF ( n23==3 ) THEN
!
      insuff = 3050
   ELSE
      i1 = i - 1
      i2 = i1
      i3 = i1 + flag
      DO
         Z(i2+1) = orf(lshift(Z(i1+1),Ihalf),Z(i1+2))
         Z(i2+2) = Z(i1+3)
         i1 = i1 + 3
         i2 = i2 + 2
         IF ( i1>=i3 ) THEN
            flag = i2 - i + 1
            insuff = 3050
            EXIT
         ENDIF
      ENDDO
   ENDIF
 1800 i3 = i + flag
   IF ( i3<j ) THEN
      i = i3
      CALL read(*9200,*9300,Rgt,Z(j+1),nrigid,1,flag)
      CALL close(Rgt,Clsrew)
      CALL gopen(Rgt,Z(Buf3),1)
   ELSE
      WRITE (Outtap,99001) i , i3 , j , flag , Buf3 , nrigid , n23
99001 FORMAT ('  GP4/3060 I,I3,J,FLAG,BUF3,NRIGID,N23 =',7I7)
      icrq = i - j
      GOTO 9400
   ENDIF
!
!     SORT THE LIST OF DEPENDENT SIL VALUES
!     THUS FORMING THE UM SUBSET
!
 1900 Ii = j + 1
   m = Buf3 - Ii
   nnx = Buf3 - 1
   IF ( m/=1 ) THEN
      CALL sort(0,0,1,1,Z(Ii),m)
!
!     CHECK FOR DEPENDENT COMPONENT ERRORS IN MPC/RIGID ELEMENT DATA
!
      Jj = nnx - 1
      nold = 0
      jxx = 0
      DO j = Ii , Jj
         IF ( Z(j)/=nold ) THEN
            IF ( Z(j)==Z(j+1) ) THEN
               nold = Z(j)
               Nogo = 1
               jxx = jxx + 1
               IF ( jxx<=50 ) THEN
                  CALL page2(2)
                  WRITE (Outtap,99002) Ufm , Z(j)
99002             FORMAT (A23,' 2423, DEPENDENT COMPONENT SPECIFIED MORE THAN ONCE',' ON MPC CARDS AND/OR IN RIGID ELEMENTS.  SIL ='&
                        & ,I9)
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      IF ( jxx>50 ) WRITE (Outtap,99003)
99003 FORMAT (//12X,12H... AND MORE,/)
   ENDIF
   IF ( Nogo/=0 ) GOTO 10400
   CALL write(Scr1,Z(Ii),m,1)
!
!     SORT THE LIST OF CODED COL AND ROW NOS (OR UNCODED NOS)
!     THEN BLDPK EACH COL THUS FORMING THE RG MATRIX
!
   n = i - impc
   nmpc = i - n23
   j = impc
   IF ( n23==3 ) CALL sort2k(0,0,3,1,Z(j),n)
   IF ( n23==2 ) CALL sort(0,0,2,1,Z(j),n)
!
!     CHECK FOR INDEPENDENT COMPONENT ERRORS IN MPC DATA
!
   kj = j + n - 2*n23
   nold = 0
   Nogo = 0
   DO kk = j , kj , n23
      IF ( Z(kk)/=nold ) THEN
         IF ( Z(kk)==Z(kk+n23) ) THEN
            IF ( n23/=3 .OR. Z(kk+1)==Z(kk+n23+1) ) THEN
               nold = Z(kk)
               Nogo = 1
               Jj = nold
               IF ( n23==2 ) Jj = rshift(nold,Ihalf)
               CALL page2(-2)
               WRITE (Outtap,99004) Ufm , Jj
99004          FORMAT (A23,' 3180, INDEPENDENT COMPONENT SPECIFIED MORE THAN ','ONCE IN AN MPC RELATIONSHIP.   SIL =',I6)
            ENDIF
         ENDIF
      ENDIF
   ENDDO
   IF ( Nogo/=0 ) GOTO 10400
   ncol = 1
   m = Buf3 - i
   n231 = n23 - 1
   CALL bldpk(1,1,Rgt,0,0)
 2000 DO WHILE ( j<=nmpc )
      Jj = Z(j)
      IF ( n23==2 ) Jj = rshift(Z(j),Ihalf)
      IF ( Jj>ncol ) EXIT
      Ix = Z(j+1)
      IF ( n23==2 ) Ix = andf(Z(j),Mask16)
      X(1) = Z(j+n231)
      DO nn1 = Ii , nnx
         IF ( Ix==Z(nn1) ) GOTO 2050
      ENDDO
      GOTO 10400
 2050 Ix = nn1 - Ii + 1
      CALL zblpki
      j = j + n23
   ENDDO
   CALL bldpkn(Rgt,0,mcb)
   ncol = ncol + 1
   IF ( ncol<=Luset ) THEN
      CALL bldpk(1,1,Rgt,0,0)
      GOTO 2000
   ELSE
      mcb(3) = multi
      CALL wrttrl(mcb)
      CALL close(Rgt,Clsrew)
   ENDIF
!
!     READ OMIT CARDS (IF PRESENT).
!
 2100 i = Knkl1
   CALL locate(*2500,Z(Buf1),omit,flag)
   ASSIGN 2300 TO ret
   ASSIGN 9700 TO ret1
   ASSIGN 2200 TO ret2
   ASSIGN 2400 TO ret3
   Omit1 = 1
 2200 CALL read(*9200,*2500,Geomp,Buf,2,0,flag)
   Gpoint = Buf(1)
   GOTO 8800
 2300 index = 3
   icomp = Buf(2)
   GOTO 8900
 2400 IF ( icomp/=0 ) Gpoint = Gpoint + icomp - 1
   Z(i) = Gpoint
   i = i + 1
   IF ( i<=Buf3 ) GOTO 2200
   icrq = i - Buf3
   insuff = 345
   GOTO 9400
!
!     READ OMIT1 CARDS (IF PRESENT).
!
 2500 IF ( Nogo/=0 ) GOTO 10400
   CALL locate(*3000,Z(Buf1),omitx1,flag)
   Omit1 = 1
   ASSIGN 2800 TO ret
   ASSIGN 9700 TO ret1
   ASSIGN 2700 TO ret2
   ASSIGN 2900 TO ret3
 2600 CALL read(*9200,*3000,Geomp,Buf,1,0,flag)
   IF ( Buf(1)/=0 ) CALL scalex(1,Buf(1),Buf(8))
 2700 CALL read(*9200,*3000,Geomp,Buf(2),1,0,flag)
   IF ( Buf(2)==-1 ) GOTO 2600
   Gpoint = Buf(2)
   GOTO 8800
 2800 index = 5
   icomp = Buf(1)
   GOTO 8900
 2900 IF ( icomp/=0 ) THEN
      Gpoint = Gpoint - 1
      DO ijk = 1 , 6
         IF ( Buf(ijk+7)==0 ) EXIT
         Z(i) = Gpoint + Buf(ijk+7)
         i = i + 1
      ENDDO
   ELSE
      Z(i) = Gpoint
      i = i + 1
   ENDIF
   GOTO 2700
 3000 IF ( Omit1==1 ) THEN
      IF ( Nogo/=0 ) GOTO 10400
!
!     SORT OMIT AND OMIT1 DATA AND WRITE IT ON SCR1.
!
      n = i - Knkl1
      i = Knkl1
      CALL sort(0,0,1,1,Z(i),n)
      CALL write(Scr1,Z(i),n,1)
   ENDIF
!
!     READ SUPORT CARDS (IF PRESENT)
!
   CALL locate(*3500,Z(Buf1),suport,flag)
   React = 1
   i = Knkl1
   ASSIGN 3200 TO ret
   ASSIGN 9800 TO ret1
   ASSIGN 3100 TO ret2
   ASSIGN 3300 TO ret3
 3100 CALL read(*9200,*3400,Geomp,Buf,2,0,flag)
   Gpoint = Buf(1)
   GOTO 8800
 3200 index = 7
   icomp = Buf(2)
   GOTO 8900
 3300 IF ( icomp/=0 ) Gpoint = Gpoint + icomp - 1
   Z(i) = Gpoint
   i = i + 1
   IF ( i<Buf3 ) GOTO 3100
   icrq = i - Buf3
   insuff = 445
   GOTO 9400
 3400 IF ( Nogo/=0 ) GOTO 10400
   n = i - Knkl1
   i = Knkl1
   CALL sort(0,0,1,1,Z(i),n)
   CALL write(Scr1,Z(i),n,1)
!
!     READ THE GPDT AND EXTRACT CONSTRAINED POINTS (IF ANY)
!
 3500 CALL close(Geomp,Clsrew)
 3600 file = gpdt
   ASSIGN 3800 TO ret
   CALL gopen(gpdt,Z(Buf1),0)
 3700 DO
      CALL read(*9100,*3900,gpdt,Buf,7,0,flag)
      IF ( Buf(7)/=0 ) THEN
         j = Buf(1) + km
         Buf(1) = Z(j)
         CALL scalex(Buf,Buf(7),Buf(8))
!
!
!     INTERNAL SUBROUTINE TO SORT THE SCALAR COMPONENTS
!
         DO Ii = 1 , 6
            IF ( Buf(Ii+7)==0 ) GOTO 3720
         ENDDO
         Ii = 7
 3720    n = Ii - 1
         IF ( n==0 ) GOTO ret
         DO Ii = 1 , n
            ijk = Luset + 1
            DO Jj = Ii , n
               IF ( Buf(Jj+7)<ijk ) THEN
                  ijk = Buf(Jj+7)
                  jjx = Jj
               ENDIF
            ENDDO
            Buf(jjx+7) = Buf(Ii+7)
            Buf(Ii+7) = ijk
         ENDDO
         GOTO ret
      ENDIF
   ENDDO
 3800 CALL write(Scr1,Buf(8),n,0)
   ugset = 1
   GOTO 3700
 3900 IF ( ugset>0 ) CALL write(Scr1,0,0,1)
   CALL close(gpdt,Clsrew)
   file = Geomp
   IF ( nogeom==0 ) THEN
      IF ( Mpcset/=0 ) CALL mesage(30,47,Mpcset)
      IF ( Spcset/=0 ) CALL mesage(30,53,Spcset)
      IF ( Mpcset/=0 .OR. Spcset/=0 ) Nogo = +1
      GOTO 6600
   ELSE
      CALL preloc(*9100,Z(Buf1),Geomp)
   ENDIF
!
!     IF SPC SET IS SELECTED, READ SPCADD CARDS (IF PRESENT).
!     DETERMINE IF SET ID IS ON SPCADD CARD.
!     IF NOT, SIMULATE AN SPCADD SET LIST WITH ONE SET = SPCSET.
!
 4000 IF ( Spcset==0 ) GOTO 5600
   ispcad = Knkl1
   nspcad = Knkl1
   ispc = ispcad + 2
   i = ispcad
   Z(i) = Spcset
   Z(i+1) = 0
   CALL locate(*4200,Z(Buf1),spcadd,flag)
   DO
      CALL read(*9200,*4200,Geomp,id,1,0,flag)
      IF ( id==Spcset ) THEN
         DO
            CALL read(*9200,*4100,Geomp,Buf,1,0,flag)
            IF ( Buf(1)==-1 ) THEN
               CALL fwdrec(*9200,Geomp)
               GOTO 4100
            ELSE
               Z(i) = Buf(1)
               Z(i+1) = 0
               i = i + 2
            ENDIF
         ENDDO
      ELSE
         DO
            CALL fread(Geomp,id,1,0)
            IF ( id==-1 ) EXIT
         ENDDO
      ENDIF
   ENDDO
 4100 ispc = i
   nspcad = i - 2
!
!     READ SPC1 AND SPC CARDS.
!     FOR EACH SET ID WHICH IS IN THE SPCADD SET LIST,
!     CONVERT THE GRID POINT NO. AND COMPONENT VALUE (OR SCALAR NO.)
!     TO AN SIL VALUE. SAVE A LIST IN CORE OF SIL VALUES AND
!     ENFORCED DISPLACEMENT (ON SPC1 CARDS, ENF. DISPL. = 0.)
!
 4200 i = ispc
!
!     PROCESSING OF SPC CARDS EXECUTES FIRST.
!
   CALL locate(*4300,Z(Buf1),spc,flag)
   ASSIGN 5100 TO ret
   ASSIGN 10300 TO ret1
   ASSIGN 4900 TO ret2
   ASSIGN 5200 TO ret3
   GOTO 4900
!
!     SPC1 PROCESSING EXECUTES AFTER SPC PROCESSING
!
 4300 IF ( Nogo/=0 ) GOTO 10400
   CALL locate(*5500,Z(Buf1),spc1,flag)
   ASSIGN 4700 TO ret
   ASSIGN 9900 TO ret1
   ASSIGN 4600 TO ret2
   ASSIGN 4800 TO ret3
 4400 CALL read(*9200,*5500,Geomp,id,1,0,flag)
   DO k = ispcad , nspcad , 2
      IF ( Z(k)==id ) GOTO 4500
   ENDDO
   DO
      CALL fread(Geomp,Buf,1,0)
      IF ( Buf(1)==-1 ) GOTO 4400
   ENDDO
 4500 Z(k+1) = 1
   CALL fread(Geomp,Buf,1,0)
   Single = 1
   IF ( Buf(1)/=0 ) CALL scalex(1,Buf(1),Buf(8))
 4600 CALL read(*9200,*4400,Geomp,Buf(2),1,0,flag)
   IF ( Buf(2)<0 ) GOTO 4400
   Gpoint = Buf(2)
   GOTO 8800
 4700 index = 9
   icomp = Buf(1)
   GOTO 8900
 4800 IF ( icomp/=0 ) THEN
      Gpoint = Gpoint - 1
      DO ijk = 1 , 6
         IF ( Buf(ijk+7)==0 ) EXIT
         Z(i) = Gpoint + Buf(ijk+7)
         Z(i+1) = 0
         i = i + 2
      ENDDO
   ELSE
      Z(i) = Gpoint
      Z(i+1) = 0
      i = i + 2
   ENDIF
   GOTO 4600
 4900 DO
      CALL read(*9200,*5300,Geomp,Buf,4,0,flag)
      DO k = ispcad , nspcad , 2
         IF ( Z(k)==Buf(1) ) GOTO 5000
      ENDDO
   ENDDO
 5000 Single = 1
   Z(k+1) = 1
   Gpoint = Buf(2)
   GOTO 8800
 5100 index = 11
   icomp = Buf(3)
   GOTO 8900
 5200 IF ( icomp/=0 ) THEN
      CALL scalex(Gpoint,Buf(3),Buf(8))
      DO ijk = 1 , 6
         IF ( Buf(ijk+7)==0 ) EXIT
         Z(i) = Buf(ijk+7)
         Z(i+1) = Buf(4)
         i = i + 2
      ENDDO
   ELSE
      Z(i) = Gpoint
      Z(i+1) = Buf(4)
      i = i + 2
   ENDIF
   GOTO 4900
 5300 IF ( Nogo/=0 ) GOTO 10400
   n = i - ispc
   IF ( n<=2 ) GOTO 4300
!
!     CHECK FOR DUPLICATELY DEFINED ENFORCED DISPLACEMENTS ON SPC CARDS
!
   CALL sort(0,0,2,1,Z(ispc),n)
   n = n - 2
   nold = 0
   DO k = 1 , n , 2
      IF ( Z(ispc+k-1)/=nold ) THEN
         IF ( Z(ispc+k-1)==Z(ispc+k+1) ) THEN
            IF ( Z(ispc+k)/=0 .OR. Z(ispc+k+2)/=0 ) THEN
               nold = Z(ispc+k-1)
               Nogo = 1
               CALL page2(3)
               WRITE (Outtap,99005) Ufm , nold
99005          FORMAT (A23,' 3147, ENFORCED DISPLACEMENT ON SPC CARDS SPECIFIED',' MORE THAN ONCE',/5X,                             &
                      &'FOR THE SAME COMPONENT.  SIL VALUE =',I10)
            ENDIF
         ENDIF
      ENDIF
   ENDDO
   IF ( Nogo==0 ) GOTO 4300
   GOTO 10400
!
!     FLUID PROBLEM AND NO SPC-S AT ALL.
!
 5400 Spcset = 0
   GOTO 4000
 5500 nspc = i - 2
   icrq = nspc - Buf3
   insuff = 740
   IF ( icrq>0 ) GOTO 9400
!
!     DETERMINE IF ALL SPC SETS IN SPCADD SET LIST HAVE BEEN DEFINED
!
   IF ( Nogo/=0 ) GOTO 10400
   DO k = ispcad , nspcad , 2
      IF ( Z(k+1)==0 ) THEN
         IF ( Iaxif/=0 .AND. Z(k)==200000000 ) GOTO 5400
         Nogo = 1
         Buf(1) = Z(k)
         Buf(2) = 0
         CALL mesage(30,53,Buf)
      ENDIF
   ENDDO
   IF ( Nogo/=0 ) GOTO 10400
!
!     SORT THE SPC LIST AND WRITE IT ON SCR1
!
   n = nspc - ispc + 2
   CALL sort(0,0,2,1,Z(ispc),n)
   CALL write(Scr1,Z(ispc),n,1)
!
!     READ ASET CARDS (IF PRESENT)
!
 5600 i = Knkl1
   CALL locate(*6000,Z(Buf1),aset,flag)
   ASSIGN 5800 TO ret
   ASSIGN 9700 TO ret1
   ASSIGN 5700 TO ret2
   ASSIGN 5900 TO ret3
   asetx = 1
 5700 CALL read(*9200,*6000,Geomp,Buf,2,0,flag)
   Gpoint = Buf(1)
   GOTO 8800
 5800 index = 15
   icomp = Buf(2)
   GOTO 8900
 5900 IF ( icomp/=0 ) Gpoint = Gpoint + icomp - 1
   Z(i) = Gpoint
   i = i + 1
   IF ( i<=Buf3 ) GOTO 5700
   icrq = i - Buf3
   insuff = 1445
   GOTO 9400
!
!     READ ASET1 CARDS (IF PRESENT)
!
 6000 IF ( Nogo/=0 ) GOTO 10400
   CALL locate(*6500,Z(Buf1),aset1,flag)
   asetx = 1
   ASSIGN 6300 TO ret
   ASSIGN 9700 TO ret1
   ASSIGN 6200 TO ret2
   ASSIGN 6400 TO ret3
 6100 CALL read(*9200,*6500,Geomp,Buf,1,0,flag)
   IF ( Buf(1)/=0 ) CALL scalex(1,Buf(1),Buf(8))
 6200 CALL read(*9200,*6500,Geomp,Buf(2),1,0,flag)
   IF ( Buf(2)==-1 ) GOTO 6100
   Gpoint = Buf(2)
   GOTO 8800
 6300 index = 17
   icomp = Buf(1)
   GOTO 8900
 6400 IF ( icomp/=0 ) THEN
      Gpoint = Gpoint - 1
      DO ijk = 1 , 6
         IF ( Buf(ijk+7)==0 ) EXIT
         Z(i) = Gpoint + Buf(ijk+7)
         i = i + 1
      ENDDO
   ELSE
      Z(i) = Gpoint
      i = i + 1
   ENDIF
   GOTO 6200
 6500 IF ( asetx/=1 ) THEN
      CALL close(Geomp,Clsrew)
   ELSE
      IF ( Nogo/=0 ) GOTO 10400
!
!     SORT ASET AND ASET1 DATA AND WRITE IT ON SCR1
!
      n = i - Knkl1
      i = Knkl1
      CALL sort(0,0,1,1,Z(i),n)
      CALL write(Scr1,Z(i),n,1)
      CALL close(Geomp,Clsrew)
   ENDIF
 6600 CALL close(Scr1,Clsrew)
!
!     FORM THE BASIC USET BY READING EACH OF THE SUBSETS AND
!     TURNING ON THE APPROPRIATE BIT IN THE APPROPRIATE WORD
!
   file = Scr1
   CALL open(*9100,Scr1,Z(Buf2),Rdrew)
   DO k = 1 , Luset
      Z(k) = 0
   ENDDO
   Buf(1) = multi
   Buf(2) = Omit1
   Buf(3) = React
   Buf(4) = usgset
   Buf(5) = Single
   Buf(6) = asetx
   icount = 0
   DO k = 1 , 6
      IF ( Buf(k)<0 ) CYCLE
      IF ( k<5 ) icount = icount + 1
      IF ( k/=2 .AND. k/=6 ) THEN
         mcbust(5) = orf(mcbust(5),mask(k))
         Nosets = 1
         IF ( k==5 ) THEN
            DO
               CALL read(*9200,*6700,Scr1,Buf(7),2,0,flag)
               j = Buf(7)
               Z(j) = orf(Z(j),mask(k))
            ENDDO
         ENDIF
      ENDIF
 6650 DO
         CALL read(*9200,*6700,Scr1,j,1,0,flag)
         IF ( k==2 ) EXIT
         IF ( k/=6 ) THEN
            IF ( andf(Z(j),mask(k))==mask(k) ) THEN
               dup = 1
               IF ( iflag==0 ) THEN
                  file = uset
                  CALL open(*9100,uset,Z(Buf1),Wrtrew)
                  iflag = 1
                  file = Scr1
               ENDIF
               Buf(1) = j
               Buf(2) = k
               CALL write(uset,Buf(1),2,0)
            ENDIF
            EXIT
         ELSEIF ( andf(Z(j),mskua)==0 ) THEN
            EXIT
         ENDIF
      ENDDO
      Z(j) = orf(Z(j),mask(k))
      GOTO 6650
 6700 ENDDO
   IF ( dup/=0 ) THEN
      CALL write(uset,0,0,1)
      CALL close(uset,Clsrew)
   ENDIF
   CALL close(Scr1,Clsrew)
!
!     THE FOLLOWING CONVENTION WILL BE USED WITH REGARD TO DEGREES OF
!     FREEDOM NOT SPECIFICALLY INCLUDED OR OMITTED-
!       1. IF ASET OR ASET1 CARDS ARE PRESENT, UNSPECIFIED DEGREES OF
!          FREEDOM WILL BE OMITTED.
!       2. IF ASET OR ASET1 CARDS ARE NOT PRESENT AND OMIT OR OMIT1
!          CARDS ARE PRESENT, UNSPECIFIED DEGREES OF FREEDOM WILL BE
!          INCLUDED IN THE ANALYSIS SET.
!       3. IF NO ASET, ASET1, OMIT, OR OMIT 1 CARDS ARE PRESENT ALL
!          UNSPECIFIED DEGREES OF FREEDOM WILL BE INCLUDED IN THE
!          ANALYSIS SET.
!       4. IF BOTH ASET OR ASET1 CARDS AND OMIT OR OMIT1 CARDS ARE
!          SUPPLIED, UNSPECIFIED DEGREES OF FREEDOM WILL BE OMITTED.
!
   mskrst = mask(2)
   IF ( asetx<=0 ) THEN
      mskrst = mask(6)
      imsk = 0
   ENDIF
   DO k = 1 , Luset
      IF ( andf(mskck,Z(k))==0 ) THEN
         imsk = mskrst
         Z(k) = orf(Z(k),mskrst)
      ENDIF
   ENDDO
   IF ( imsk==mask(6) ) asetx = 1
   IF ( imsk==mask(2) ) Omit1 = 1
!
!     CALL SUBROUTINE GP4SP TO EXAMINE GRID POINT SINGULARITIES
!
   CALL gp4sp(Buf2,Buf3,Buf4)
!
!     TURN ON CERTAIN FLAGS IF THERE ARE OMIT OR ASET
!     DEGREES OF FREEDOM
!
   Omit1 = -1
   DO k = 1 , Luset
      IF ( andf(Z(k),Mskuo)/=0 ) THEN
         mcbust(5) = orf(mcbust(5),mask(2))
         Nosets = 1
         Omit1 = 1
         EXIT
      ENDIF
   ENDDO
   DO k = 1 , Luset
      IF ( andf(Z(k),mskua)/=0 ) THEN
         mcbust(5) = orf(mcbust(5),mask(6))
         Nol = 1
         EXIT
      ENDIF
   ENDDO
!
   CALL open(*9100,Scr1,Z(Buf2),Rdrew)
   CALL skprec(Scr1,icount)
!
!     OPEN YS FILE. WRITE SPCSET IN YS HEADER.
!     IF NO USB SET (FROM SPC AND SPC1 CARDS), WRITE NULL COLUMN
!     FOR YS VECTOR. IF USB SET IS PRESENT, BUILD THE YS VECTOR.
!
   file = Scr1
   CALL open(*6800,ys,Z(Buf3),Wrtrew)
   noys = 1
   CALL fname(ys,Buf)
   Buf(3) = Spcset
   CALL write(ys,Buf,3,1)
 6800 Ix = 0
   Ii = 1
   IF ( Single>0 ) THEN
      IF ( noys/=0 ) CALL bldpk(1,1,ys,0,0)
      DO
         CALL read(*9200,*6900,Scr1,Buf,2,0,flag)
         j = Buf(1)
         IF ( Buf(2)/=0 ) THEN
            DO k = Ii , j
               IF ( andf(Z(k),Mskus)/=0 ) Ix = Ix + 1
            ENDDO
            Ii = j + 1
            X(1) = Buf(2)
            IF ( noys/=0 ) THEN
               CALL zblpki
            ELSEIF ( nogoof==0 ) THEN
               Nogo = 1
               nogoof = 1
               CALL mesage(30,132,Buf)
            ENDIF
         ENDIF
      ENDDO
   ELSE
      IF ( Nauto>0 .OR. usgset>0 ) Single = 1
      IF ( noys/=0 ) CALL bldpk(1,1,ys,0,0)
   ENDIF
 6900 IF ( noys/=0 ) CALL bldpkn(ys,0,mcbys)
   IF ( Ii<=Luset ) THEN
      DO k = Ii , Luset
         IF ( andf(Z(k),Mskus)/=0 ) Ix = Ix + 1
      ENDDO
   ENDIF
   mcbys(3) = Ix
   IF ( noys/=0 ) THEN
      CALL wrttrl(mcbys)
      CALL close(ys,Clsrew)
   ENDIF
   CALL close(Scr1,Clsrew)
!
   IF ( l21+l22>0 .OR. Idsub>0 ) CALL gp4prt(Buf1)
   IF ( Nauto/=0 ) THEN
!
!     CHANGE AUTO SPC FLAGS TO BOUNDARY SPC FLAGS
!
      j = 0
      DO k = 1 , Luset
         IF ( andf(Z(k),Mskus)/=0 ) THEN
            IF ( andf(Z(k),mskusg)==0 .AND. andf(Z(k),mskusb)==0 ) THEN
               Z(k) = mask(5)
               j = 1
            ENDIF
         ENDIF
      ENDDO
      IF ( j==1 ) mcbust(5) = orf(mcbust(5),mask(5))
   ENDIF
!
   file = uset
   IF ( dup==0 ) GOTO 7100
   CALL open(*9100,uset,Z(Buf1),Rdrew)
   file = Scr1
   CALL open(*9100,Scr1,Z(Buf2),Wrtrew)
   file = uset
   DO
      CALL read(*7000,*7000,uset,Buf(1),2,0,flag)
      CALL write(Scr1,Buf(1),2,0)
   ENDDO
 7000 CALL write(Scr1,0,0,1)
   CALL close(uset,Clsrew)
 7100 CALL open(*9100,uset,Z(Buf1),Wrtrew)
   CALL fname(uset,Buf)
   Buf(3) = Spcset
   Buf(4) = Mpcset
   CALL write(uset,Buf,4,1)
   CALL write(uset,Z(1),Luset,1)
   IF ( Nol==1 ) mcbust(5) = orf(mcbust(5),mask(6))
!
!     SEPARATE TRAILER WORD 4 INTO TWO PARTS
!
   mcbust(4) = rshift(mcbust(5),Ihalf)
   mcbust(5) = andf(mcbust(5),complf(lshift(mcbust(4),Ihalf)))
   CALL wrttrl(mcbust)
   CALL close(uset,Clsrew)
!
!     PROCESS USET FOR CONSISTENCY OF DISPLACEMENT SET DEFINITIONS.
!     EACH POINT IN USET MAY BELONG TO AT MOST ONE DEPENDENT SUBSET.
!
   flag = 0
   mask(1) = Mskum
   mask(2) = Mskus
   mask(3) = Mskuo
   mask(4) = Mskur
   mskums = orf(Mskum,Mskus)
   mskuor = orf(Mskuo,Mskur)
   Buf(1) = orf(Mskus,mskuor)
   Buf(2) = orf(Mskum,mskuor)
   Buf(3) = orf(Mskur,mskums)
   Buf(4) = orf(Mskuo,mskums)
   mskall = orf(mskums,mskuor)
   mskal = orf(mskall,Mskul)
   DO i = 1 , Luset
      iuset = Z(i)
      idepn = andf(mskal,iuset)
      DO ik = 1 , 4
         IF ( andf(mak(ik),idepn)==mak(ik) ) GOTO 7150
      ENDDO
      idepn = andf(iuset,mskall)
      IF ( idepn/=0 ) THEN
         DO j = 1 , 4
            msk1 = mask(j)
            msk2 = Buf(j)
            IF ( andf(idepn,msk1)/=0 ) THEN
               IF ( andf(idepn,msk2)/=0 ) GOTO 7150
            ENDIF
         ENDDO
      ENDIF
      CYCLE
 7150 IF ( flag==0 .AND. iflag==0 ) THEN
         file = Scr1
         CALL open(*9100,Scr1,Z(Buf1),Wrtrew)
      ENDIF
      Buf(5) = i
      Buf(6) = idepn
      flag = 1
      CALL write(Scr1,Buf(5),2,0)
   ENDDO
 7200 IF ( Mpcf1>0 .OR. Single>0 .OR. Omit1>0 .OR. React>0 ) Nosets = 1
   IF ( Mpcf1==-1 .AND. Single==-1 .AND. Omit1==-1 ) Noa = -1
   IF ( andf(mskua,mcbust(5))==0 .AND. Omit1>=0 ) THEN
      CALL page2(2)
      WRITE (Outtap,99006) Ufm
99006 FORMAT (A23,' 2403, INVALID TO HAVE AN O-SET WITH A NULL A-SET.')
      Nogo = 1
   ENDIF
   IF ( Nogo/=0 ) GOTO 10400
   IF ( iflag/=0 .OR. flag/=0 ) GOTO 8400
!
!     RECOMPUTE YS MATRIX TO ACCOUNT FOR SPCD CARDS
!
!
   IF ( noys==0 .OR. nogeom==0 ) GOTO 8300
!     BRING EQEXIN,SIL,AND USET BACK INTO CORE
!
   ASSIGN 7300 TO ret
   GOTO 200
 7300 CALL gopen(uset,Z(Buf1),0)
   file = uset
   CALL read(*9200,*7400,uset,Z(Knkl1),Buf4-Knkl1,1,Luset)
   icrq = Buf4
   insuff = 9711
   GOTO 9400
 7400 CALL close(uset,1)
!
!     CONVERT USET POINTERS INTO SILA VALUES
!
   m = Knkl1
   n = Knkl1 + Luset - 1
   Ix = 0
   DO i = m , n
      IF ( andf(Z(i),Mskus)/=0 ) THEN
         Ix = Ix + 1
         Z(i) = Ix
      ELSE
         Z(i) = 0
      ENDIF
   ENDDO
!
!     POSITION CASECC
!
   file = casecc
   iload = n + 1
   icrq = n + 2*nskp1 + 1 - Buf4
   insuff = 977
   IF ( icrq>0 ) GOTO 9400
   CALL gopen(casecc,Z(Buf1),0)
   CALL skprec(casecc,Nskip-1)
   DO i = 1 , nskp1
      DO
         CALL fread(casecc,Buf,16,1)
         IF ( Buf(16)==0 ) THEN
            k = iload + 2*(i-1)
            Z(k) = Buf(4)
            Z(k+1) = 0
            EXIT
         ENDIF
      ENDDO
   ENDDO
   CALL close(casecc,Clsrew)
!
!     CONVERT SPCD CARD TO SILA + VALUE AND WRITE ON SCR2
!
   CALL gopen(scr2,Z(Buf2),1)
   file = Geomp
   CALL preloc(*9100,Z(Buf1),Geomp)
   CALL locate(*8100,Z(Buf1),spcd,flag)
   nn = 2*nskp1 + iload - 2
   iold = 0
   irecn = 0
 7500 DO
      CALL read(*9200,*8000,Geomp,Buf,4,0,flag)
      DO i = iload , nn , 2
         IF ( Buf(1)==Z(i) ) GOTO 7600
!
!     GO ON TO NEXT SET
!
      ENDDO
   ENDDO
!
 7600 IF ( Buf(1)/=iold ) THEN
      IF ( iold/=0 ) CALL write(scr2,0,0,1)
      iold = Buf(1)
      irecn = irecn + 1
      DO i = iload , nn , 2
         IF ( iold==Z(i) ) Z(i+1) = irecn
      ENDDO
   ENDIF
   Gpoint = Buf(2)
   ASSIGN 7700 TO ret
   ASSIGN 10300 TO ret1
   ASSIGN 7500 TO ret2
   ASSIGN 7800 TO ret3
   GOTO 8800
!
!     FOUND SIL
!
 7700 index = 13
   icomp = Buf(3)
   GOTO 8900
 7800 IF ( icomp/=0 ) THEN
!
!     BREAK UP COMPONENTS
!
      CALL scalex(Gpoint,Buf(3),Buf(8))
      DO i = 1 , 6
         IF ( Buf(i+7)==0 ) EXIT
         m = Knkl1 + Buf(i+7) - 1
         IF ( Z(m)==0 ) GOTO 7900
         mcb(1) = Z(m)
         mcb(2) = Buf(4)
         CALL write(scr2,mcb,2,0)
      ENDDO
      GOTO 7500
   ELSE
      m = Knkl1 + Gpoint - 1
      IF ( Z(m)/=0 ) THEN
         mcb(1) = Z(m)
         mcb(2) = Buf(4)
         CALL write(scr2,mcb,2,0)
         GOTO 7500
      ENDIF
   ENDIF
 7900 n = 108
   Buf(1) = Buf(2)
   Buf(2) = Buf(i+7) - Gpoint
   GOTO 10100
!
!     END OF SPCD-S
!
 8000 IF ( Nogo/=0 ) GOTO 10400
   CALL write(scr2,0,0,1)
 8100 CALL close(Geomp,1)
   CALL close(scr2,1)
   IF ( Single<0 ) GOTO 8300
!
!     BRING IN OLD YS
!
   n = 2*nskp1
   DO i = 1 , n
      k = iload + i - 1
      Z(i) = Z(k)
   ENDDO
   ioys = n
   inys = ioys + Ix
   icrq = inys + Ix - Buf4
   insuff = 988
   IF ( icrq>0 ) GOTO 9400
   mcb(1) = ys
   CALL rdtrl(mcb)
   mcb(2) = 0
   mcb(6) = 0
   mcb(7) = 0
   CALL gopen(ys,Z(Buf1),0)
   Itb = mcb(5)
   Ita1 = Itb
   Itb1 = Itb
   Incr = 1
   Incr1 = 1
   Ii = 1
   Ii1 = 1
   Jj = mcb(3)
   Jj1 = Jj
   DO i = 1 , Ix
      Rz(ioys+i) = 0.0
   ENDDO
   CALL unpack(*8200,ys,Rz(ioys+1))
 8200 CALL close(ys,Clsrew)
   CALL gopen(ys,Z(Buf1),1)
   CALL gopen(scr2,Z(Buf2),0)
   file = scr2
   DO i = 1 , n , 2
!
!     COPY OLD YS TO NEW YS
!
      DO k = 1 , Ix
         Rz(inys+k) = Rz(ioys+k)
      ENDDO
      IF ( Z(i+1)/=0 ) THEN
!
!     POSITION SCR2
!
         CALL skprec(scr2,Z(i+1)-1)
         DO
            CALL read(*9200,*8250,scr2,Buf,2,0,flag)
            k = Buf(1) + inys
            Rz(k) = Bufr(2)
         ENDDO
      ENDIF
!
!     PUT OUT COLUMN
!
 8250 CALL pack(Rz(inys+1),ys,mcb)
      CALL rewind(scr2)
      CALL fwdrec(*9200,scr2)
   ENDDO
   CALL close(ys,1)
   CALL wrttrl(mcb)
   CALL close(scr2,1)
 8300 IF ( Nogo/=0 ) GOTO 10400
   IF ( flag==0 ) THEN
      IF ( Iogpst==1 ) CALL mesage(17,Iautsp,0)
      RETURN
   ENDIF
!
!     INCONSISTENT DISPLACEMENT SET DEFINITIONS--
!     READ EQEXIN AND SIL INTO CORE. FOR EACH INCONSISTANT DEFINITION,
!     LOOK UP EXTERNAL NUMBER AND QUEUE MESSAGE.
!
 8400 CALL write(Scr1,0,0,1)
   CALL close(Scr1,Clsrew)
   ASSIGN 8500 TO ret
   GOTO 200
 8500 CALL open(*9100,Scr1,Z(Buf1),Rdrew)
   isil = km + 1
   neqx = Kn - 1
   Z(Knkl1) = Luset + 1
 8600 CALL read(*8700,*8700,Scr1,Buf(5),2,0,iflg)
   DO i = isil , Knkl1
      IF ( Z(i+1)>Buf(5) ) EXIT
   ENDDO
   intrnl = i - km
   komp = Buf(5) - Z(i) + 1
   IF ( Z(i+1)-Z(i)==1 ) komp = 0
   DO j = 1 , neqx , 2
      IF ( Z(j+1)==intrnl ) EXIT
   ENDDO
   IF ( dup/=0 ) THEN
      IF ( iflag/=0 ) THEN
         CALL page2(2)
         IF ( Ib6==2 ) THEN
         ELSEIF ( Ib6==3 ) THEN
            IF ( komp==0 ) THEN
               WRITE (Outtap,99012) Ufm , Z(j) , r
               Nogo = 1
            ELSE
               WRITE (Outtap,99011) Ufm , Z(j) , komp , r
               Nogo = 1
            ENDIF
         ELSEIF ( Ib6==4 ) THEN
            IF ( komp==0 ) THEN
               WRITE (Outtap,99012) Ufm , Z(j) , sg
               Nogo = 1
            ELSE
               WRITE (Outtap,99011) Ufm , Z(j) , komp , sg
               Nogo = 1
            ENDIF
         ELSEIF ( komp==0 ) THEN
            WRITE (Outtap,99012) Ufm , Z(j) , mset
            Nogo = 1
         ELSE
            Nogo = 1
            WRITE (Outtap,99011) Ufm , Z(j) , komp , mset
         ENDIF
         GOTO 8600
      ENDIF
   ENDIF
   Buf(7) = Z(j)
   Buf(8) = komp
   IF ( andf(Buf(6),Mskum)/=0 ) Buf(8) = Buf(8) + 10
   IF ( andf(Buf(6),Mskus)/=0 ) Buf(8) = Buf(8) + 100
   IF ( andf(Buf(6),Mskuo)/=0 ) Buf(8) = Buf(8) + 1000
   IF ( andf(Buf(6),Mskur)/=0 ) Buf(8) = Buf(8) + 10000
   IF ( andf(Buf(6),Mskul)/=0 ) Buf(8) = Buf(8) + 100000
   CALL mesage(30,101,Buf(7))
   GOTO 8600
 8700 IF ( dup==0 ) THEN
      CALL close(Scr1,Clsrew)
      GOTO 10400
   ELSEIF ( iflag==0 ) THEN
      CALL close(Scr1,Clsrew)
      GOTO 10400
   ELSE
      iflag = 0
      IF ( flag/=0 ) GOTO 8600
      CALL close(Scr1,Clsrew)
      GOTO 7200
   ENDIF
!
!
!     INTERNAL SUBROUTINE TO PERFORM BINARY SEARCH IN EQEXIN
!     AND CONVERT THE EXTERNAL NUMBER TO A SIL VALUE AND A
!     CORRESPONDING TYPE CODE
!
 8800 klo = 0
   khi = kn2
   lastk = 0
   DO
      k = (klo+khi+1)/2
      IF ( lastk==k ) THEN
         GOTO ret1
      ELSE
         lastk = k
         IF ( Gpoint<Z(2*k-1) ) THEN
            khi = k
         ELSEIF ( Gpoint==Z(2*k-1) ) THEN
            k = 2*k + Kn
            ipoint = Gpoint
            Gpoint = Z(k)/10
            icode = Z(k) - 10*Gpoint
            GOTO ret
         ELSE
            klo = k
         ENDIF
      ENDIF
   ENDDO
!
!     CHECK TO SEE IF GRID AND SCALAR POINTS HAVE BEEN PROPERLY USED
!     ON CONSTRAINT CARDS
!
 8900 IF ( icode==2 ) THEN
!
!     SCALAR POINTS ARE CHECKED HERE
!
      IF ( icomp<=1 ) GOTO 9000
      Nogo = 1
      CALL page2(2)
      WRITE (Outtap,99007) Ufm , ipoint , ctype(index) , ctype(index+1)
99007 FORMAT (A23,' 3146, ILLEGAL COMPONENT SPECIFIED FOR SCALAR POINT',I9,4H ON ,2A4,6HCARDS.)
   ELSE
!
!     GRID POINTS ARE CHECKED HERE
!
      IF ( icomp>0 ) GOTO 9000
      Nogo = 1
      CALL page2(2)
      WRITE (Outtap,99008) Ufm , ipoint , ctype(index) , ctype(index+1)
99008 FORMAT (A23,' 3145, COMPONENT 0 (OR BLANK) SPECIFIED FOR GRID ','POINT',I9,4H ON ,2A4,6HCARDS.)
   ENDIF
   GOTO ret2
 9000 GOTO ret3
!
!
!     FATAL ERROR MESSAGES
!
 9100 j = -1
   GOTO 9500
 9200 j = -2
   GOTO 9500
 9300 j = -3
   GOTO 9500
 9400 j = -8
   WRITE (Outtap,99009) insuff
99009 FORMAT (/33X,'GP4 INSUFFICIENT CORE AT ',I5)
   file = icrq
 9500 CALL mesage(j,file,name)
 9600 Buf(1) = Gpoint
   Buf(2) = Mpcset
   n = 48
   Gpoint = 1
   GOTO 10100
 9700 Buf(1) = Gpoint
   Gpoint = 1
   n = 49
!WKBNE 3/95 NCL94002
   Buf(2) = 0
   GOTO 10100
 9800 Buf(1) = Gpoint
   Gpoint = 1
   n = 50
   Buf(2) = 0
   GOTO 10100
 9900 n = 51
10000 Buf(1) = Gpoint
   Buf(2) = Spcset
   Gpoint = 1
!WKBNB 3/95 NCL94002
   IF ( l51/=0 ) THEN
      WRITE (Outtap,99010) Uwm , 2051 , Buf(1) , Spcset
99010 FORMAT (A25,I5,' UNDEFINED GRID POINT ',I6,' IN SINGLE-POINT',' CONSTRAINT SET ',I8)
      GOTO 10200
   ENDIF
10100 Nogo = 1
   CALL mesage(30,n,Buf)
!WKBI  3/95 NCL94002
10200 GOTO ret2
10300 n = 52
   GOTO 10000
10400 IF ( l21+l22>0 .OR. Idsub>0 ) CALL gp4prt(-Buf4)
   j = -37
   GOTO 9500
99011 FORMAT (A23,' 2152, GRID POINT',I9,' COMPONENT',I3,' DUPLICATELY DEFINED IN THE ',A4,5H SET.)
99012 FORMAT (A23,' 2153, SCALAR POINT',I9,' DUPLICATELY DEFINED IN ','THE ',A4,5H SET.)
END SUBROUTINE gp4
