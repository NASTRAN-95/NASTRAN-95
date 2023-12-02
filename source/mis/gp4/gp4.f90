!*==gp4.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
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
   USE c_bitpos
   USE c_blank
   USE c_gp4fil
   USE c_gp4prm
   USE c_gp4spx
   USE c_machin
   USE c_names
   USE c_packx
   USE c_system
   USE c_two
   USE c_unpakx
   USE c_xmssg
   USE c_zblpkx
   USE c_zzzzzz
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: aset , aset1 , mpc , mpcadd , name , omit , omitx1 , spc , spc1 , spcadd , spcd , suport
   INTEGER :: asetx , dup , file , flag , i , i1 , i2 , i3 , iaxic , iaxif , ib6 , icode , icomp , icount , icrq , id , idepn ,     &
            & ifl , iflag , iflg , igotch , ijk , ik , iload , impc , impcad , imsk , index , insuff , intrnl , inys , iold , ioys ,&
            & ipoint , irecn , isil , ispc , ispcad , iuset , j , jjx , jxx , k , khi , kj , kk , klo , km , kn2 , komp , l21 ,     &
            & l22 , l51 , lastk , m , mask15 , mpcold , msk1 , msk2 , mskal , mskall , mskck , mskfng , mskrst , mskua , mskuf ,    &
            & mskug , mskums , mskun , mskung , mskuor , mskusb , mskusg , multi , n , n23 , n231 , ncol , neqx , nmpc
   REAL , DIMENSION(2) :: bufr
   INTEGER , SAVE :: casecc , eqexin , gpdt , iz138 , iz16 , iz2 , iz3 , iz5 , mpcax1 , mpcax2 , mset , r , scr2 , sg , uset , ys
   INTEGER , DIMENSION(18) , SAVE :: ctype
   INTEGER , DIMENSION(4) :: mak
   INTEGER , DIMENSION(6) :: mask
   INTEGER , DIMENSION(7) :: mcb , mcbust , mcbys
   INTEGER :: nmpcad , nn , nn1 , nnx , nogeom , nogoo , nogoof , nold , noys , nrigid , nskp1 , nspc , nspcad , outtap , ret ,     &
            & ret1 , ret2 , ret3 , rigid , sild , spcold , sysbuf , ugset , usgset
   REAL , DIMENSION(1) :: rz
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!WKBI 3/95 NCL94002
!WKBR 3/95 NCL94002      COMMON /XMSSG / UFM
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Outtap) , (Ksystm(27),Iaxic) , (Ksystm(38),Iaxif) , (Z(1),Rz(1)) , (Buf(1),Bufr(1)) ,&
!>>>>    & (ugset,usgset) , (Ib6,Buf(6))
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
   geomp = 102
   bgpdt = 105
   cstm = 106
   rgt = 201
   scr1 = 301
   nauto = 0
   iogpst = -1
   buf1 = korsz(z) - sysbuf - 2
   buf2 = buf1 - sysbuf
   buf3 = buf2 - sysbuf
   buf4 = buf3 - sysbuf
   icrq = luset - buf4
   insuff = 10
   IF ( luset>=buf4 ) GOTO 9600
   mask16 = jhalf
   mask15 = jhalf/2
   n23 = 2
   mskum = two(um)
   mskuo = two(uo)
   mskur = two(ur)
   mskusg = two(usg)
   mskusb = two(usb)
   mskul = two(ul)
   mskua = two(ua)
   mskuf = two(uf)
   mskus = two(us)
   mskun = two(un)
   mskug = two(ug)
   mskung = orf(mskun,mskug)
   mskfng = orf(mskuf,mskung)
   msksng = orf(mskus,mskung)
   mask(1) = orf(mskum,mskug)
   mask(2) = orf(mskuo,mskfng)
   mask(3) = orf(mskur,orf(mskua,mskfng))
   mask(4) = orf(mskusg,msksng)
   mask(5) = orf(mskusb,msksng)
   mask(6) = orf(mskul,orf(mskua,mskfng))
   mak(1) = orf(mskum,mskul)
   mak(2) = orf(mskus,mskul)
   mak(3) = orf(mskuo,mskul)
   mak(4) = orf(mskur,mskul)
   CALL makmcb(mcbys,ys,0,2,1)
   CALL makmcb(mcbust,uset,luset,0,0)
   multi = -1
   usgset = -1
   single = -1
   omit1 = -1
   nosets = -1
   asetx = -1
   react = -1
   noys = 0
   nogeom = 0
   nol = -1
   noa = +1
   nogo = 0
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
   mcb(1) = geomp
   CALL rdtrl(mcb(1))
   IF ( mcb(1)>=0 ) THEN
!
!     BIT ASSIGNMENTS FOR RIGID ELEMENTS -
!     CRIGD1 - 53       CRROD    - 65       CRBE1 - 68
!     CRIGD2 - 54       CRBAR    - 66       CRBE2 - 69
!     CRIGD3 - 83       CRTRPLT  - 67       CRBE3 - 70
!     CRIGDR - 82       CRSPLINE - 71
!
      IF ( andf(mcb(5),two(21))==two(21) ) rigid = 1
      IF ( andf(mcb(5),two(22))==two(22) ) rigid = 1
      IF ( andf(mcb(7),two(19))==two(19) ) rigid = 1
      IF ( andf(mcb(7),two(18))==two(18) ) rigid = 1
      i = mcb(6)
      DO j = 17 , 23
         IF ( andf(i,two(j))==two(j) ) rigid = 1
      ENDDO
      CALL makmcb(mcb,rgt,0,2,1)
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
   repeat = -1
   mpcf1 = -1
   mpcf2 = -1
   nskp1 = 1
   file = casecc
   CALL gopen(casecc,z(buf1),0)
   IF ( nskip>1 ) CALL skprec(casecc,nskip-1)
   CALL fread(casecc,z,36,1)
   IF ( nskip>0 ) THEN
!
!     SUBSEQUENT SUBCASE - POSITION CASE CONTROL AND INITIALIZE.
!
      mpcold = z(iz2)
      spcold = z(iz3)
      DO
         nskip = nskip + 1
         CALL fread(casecc,z,36,1)
         IF ( z(iz16)==0 ) THEN
            IF ( z(iz2)/=mpcold .OR. z(iz3)/=spcold ) THEN
               mpcset = z(iz2)
               spcset = z(iz3)
               EXIT
            ENDIF
         ENDIF
      ENDDO
   ELSE
!
!     FIRST SUBCASE - INITIALIZE.
!
      mpcset = z(iz2)
      spcset = z(iz3)
      nskip = 1
   ENDIF
   DO
!
!     LOOK AHEAD TO END OF CURRENT SUBCASE AND SET PARAMETERS.
!
      CALL read(*100,*9500,casecc,z,138,1,flag)
!
!     CHECK FOR SYMMETRY
!
      IF ( z(iz16)==0 ) THEN
!
!     CHECK FOR BUCKLING OR DIFFERENTIAL STIFFNESS
!
         IF ( z(iz5)/=0 .OR. z(iz138)/=0 ) EXIT
         IF ( z(iz2)==mpcset .AND. z(iz3)==spcset ) THEN
!
            nskp1 = nskp1 + 1
         ELSE
            repeat = 1
            EXIT
         ENDIF
      ENDIF
   ENDDO
!
!     CHECK TO SEE IF MPC SET IS SELECTED OR IF RIGID ELEMENTS EXIST
!
 100  IF ( mpcset/=0 .OR. rigid/=0 ) THEN
      mpcf1 = 1
      mpcf2 = 1
      IF ( nskip/=1 ) THEN
         IF ( mpcset==mpcold ) mpcf2 = -1
      ENDIF
   ENDIF
   CALL close(casecc,clsrew)
   ASSIGN 400 TO ret
!
!     READ EQEXIN INTO CORE
!
 200  file = eqexin
   CALL gopen(eqexin,z(buf1),0)
   CALL read(*9400,*300,eqexin,z,buf4,1,kn)
   insuff = 80
   icrq = buf4
   GOTO 9600
 300  CALL read(*9400,*9500,eqexin,z(kn+1),kn,1,flag)
   CALL close(eqexin,clsrew)
   km = 2*kn
   kn2 = kn/2
!
!     FORM ARRAY OF SORTED SIL VALUES STARTING AT Z(KM+1)
!
   DO i = 1 , kn2
      j = 2*(i-1) + 2 + kn
      z(km+i) = z(j)/10
   ENDDO
   CALL sort(0,0,1,1,z(km+1),kn2)
   z(km+kn2+1) = luset + 1
   knkl1 = km + kn2 + 2
!
!     SET DIAG-S 21 AND 22 FOR DEGREE-OF-FREEDOM PRINTER LATER.
!
   CALL sswtch(21,l21)
   CALL sswtch(22,l22)
   GOTO ret
!
!     OPEN INPUT DATA FILE
!
 400  file = geomp
   CALL preloc(*500,z(buf1),geomp)
   nogeom = 1
!
!     CHECK TO SEE IF MPC SET IS SELECTED OR IF RIGID ELEMENTS EXIST
!
   IF ( mpcset/=0 .OR. rigid/=0 ) THEN
!
!     OPEN RGT FILE
!
      file = rgt
      CALL gopen(rgt,z(buf3),1)
!
!     IF RIGID ELEMENTS EXIST, GENERATE THEIR COEFFICIENTS
!
      nogoo = nogo
      nogo = 0
      IF ( rigid==1 ) CALL criggp(n23)
      IF ( nogo/=0 ) GOTO 10600
      nogo = nogoo
   ENDIF
!
!     OPEN SCRATCH DATA FILE
!
 500  file = scr1
   CALL open(*9300,scr1,z(buf2),wrtrew)
!
!     CHECK TO SEE IF GEOMP FILE EXISTS
!
   IF ( nogeom==0 ) GOTO 3700
!
!     CHECK TO SEE IF MPC SET IS SELECTED OR IF RIGID ELEMENTS EXIST
!
   IF ( mpcset==0 .AND. rigid==0 ) GOTO 2200
   IF ( mpcset/=0 ) THEN
!
!     IF MPC SET IS SELECTED, DETERMINE IF SET IS ON MPCADD CARD.
!     IF NOT, SIMULATE AN MPCADD SET LIST WITH ONE SET = MPCSET.
!
      impcad = knkl1
      nmpcad = knkl1
      impc = impcad + 2
      i = impcad
      z(i) = mpcset
      z(i+1) = 0
      file = geomp
      CALL locate(*700,z(buf1),mpcadd,flag)
      DO
         CALL read(*9400,*700,geomp,id,1,0,flag)
         IF ( id==mpcset ) THEN
            DO
               CALL read(*9400,*600,geomp,buf,1,0,flag)
               IF ( buf(1)==-1 ) THEN
                  CALL fwdrec(*9400,geomp)
                  GOTO 600
               ELSE
                  z(i) = buf(1)
                  z(i+1) = 0
                  i = i + 2
               ENDIF
            ENDDO
         ELSE
            DO
               CALL fread(geomp,buf,1,0)
               IF ( buf(1)==-1 ) EXIT
            ENDDO
         ENDIF
      ENDDO
   ELSE
!
!     NO MPC SET IS SELECTED
!
      multi = 0
      impc = knkl1
      i = impc
      j = buf3 - 1
      GOTO 1500
   ENDIF
 600  impc = i
   nmpcad = i - 2
!
!     READ MPC CARDS. FOR EACH EQUATION WHOSE SET ID MATCHES A SET ID
!     IN THE MPCADD SET LIST, CONVERT THE GRID POINT AND COMPONENT NO.
!     (OR SCALAR NO.) TO A SIL VALUE. COMPUTE THE ROW AND COLUMN NO.
!     FOR THE POINT AND SAVE THIS ALONG WITH ITS VALUE.
!
 700  CALL locate(*1400,z(buf1),mpc,flag)
   j = buf3 - 1
   i = impc
   multi = 0
   ASSIGN 1200 TO ret
   ASSIGN 9800 TO ret1
   ASSIGN 1100 TO ret2
   ASSIGN 1300 TO ret3
 800  DO
      CALL read(*9400,*1400,geomp,id,1,0,flag)
      DO k = impcad , nmpcad , 2
         IF ( z(k)==id ) GOTO 1000
      ENDDO
      DO
         CALL fread(geomp,buf,3,0)
         IF ( buf(1)==-1 ) GOTO 900
      ENDDO
      EXIT
 900  ENDDO
 1000 multi = multi + 1
   z(k+1) = 1
   ifl = 0
 1100 CALL fread(geomp,buf,3,0)
   IF ( buf(1)==-1 ) THEN
!
!     SAVE A LIST OF DEPENDENT SIL VALUES
!
      z(j) = sild
      j = j - 1
      GOTO 800
   ELSE
      gpoint = buf(1)
      GOTO 9000
   ENDIF
 1200 index = 1
   icomp = buf(2)
   GOTO 9100
 1300 IF ( icomp/=0 ) gpoint = gpoint + icomp - 1
   IF ( ifl==0 ) sild = gpoint
   IF ( n23==3 ) THEN
      z(i) = gpoint
      z(i+1) = sild
      z(i+2) = buf(3)
   ELSEIF ( gpoint>mask15 ) THEN
!
!     GPOINT IS TOO BIG TO BE PACKED INTO HALF A WORD.  ABANDON COL.
!     AND ROW PACKING LOGIC, AND DO IT OVER AGAIN WITHOUT PACKING.
!
      n23 = 3
      CALL rewind(geomp)
      CALL fwdrec(*9400,geomp)
      GOTO 700
   ELSE
      z(i) = orf(lshift(gpoint,ihalf),sild)
      z(i+1) = buf(3)
   ENDIF
   i = i + n23
   insuff = 236
   IF ( i>=j ) GOTO 9600
   ifl = 1
   GOTO 1100
!
!     DETERMINE IF ALL MPC SETS IN MPCADD SET LIST HAVE BEEN INPUT
!
 1400 IF ( nogo/=0 ) GOTO 10600
   nogoo = nogo
   nogo = 0
   igotch = 0
   DO k = impcad , nmpcad , 2
      IF ( z(k+1)/=0 ) THEN
         igotch = 1
      ELSE
         nogo = -1
         IF ( z(k)/=200000000 .OR. iaxif==0 ) THEN
            IF ( iaxic/=0 ) THEN
               IF ( z(k)==mpcax1 .OR. z(k)==mpcax2 ) CYCLE
               IF ( z(k)==200000000 ) CYCLE
            ENDIF
            nogo = +1
            buf(1) = z(k)
            buf(2) = 0
            CALL mesage(30,47,buf)
         ENDIF
      ENDIF
   ENDDO
   IF ( nogo/=0 ) THEN
      IF ( nogo==-1 .AND. igotch==1 ) THEN
         IF ( nogo==-1 .AND. nogoo==0 ) nogo = 0
      ELSE
         mpcset = 0
         multi = -1
         mpcf1 = -1
         mpcf2 = -1
         IF ( nogo==-1 .AND. nogoo==0 ) nogo = 0
         CALL close(rgt,clsrew)
         GOTO 2200
      ENDIF
   ENDIF
!
!     CHECK TO SEE IF RIGID ELEMENTS EXIST
!
 1500 IF ( rigid==0 ) GOTO 2000
!
!     EXPAND THE DEPENDENT SET BY APPENDING RIGID ELEMENT
!     DATA TO MPC DATA
!
   CALL gopen(rgt,z(buf3),0)
   CALL skprec(rgt,1)
   i1 = buf3 - i
   CALL read(*9400,*1600,rgt,z(i),i1,1,nrigid)
   insuff = 3020
   GOTO 9600
 1600 j = j - nrigid
   multi = multi + nrigid
   CALL skprec(rgt,-2)
   CALL read(*9400,*1800,rgt,z(i),i1,1,flag)
   insuff = 3030
   i2 = i1
   DO
      CALL bckrec(rgt)
      CALL read(*9400,*1700,rgt,z(i),-i2,0,flag)
      CALL read(*9400,*1700,rgt,z(i),i1,0,flag)
      i2 = i2 + i1
   ENDDO
 1700 flag = i2 + flag
   GOTO 1900
!
!     RE-CODE COLUMN-ROW PACKED WORD IF NECESSARY FOR DATA JUST BROUGHT
!     IN FROM RIGID ELEMENTS
!     THEN READ THE LAST RECORD FROM RGT
!
 1800 IF ( n23==3 ) THEN
!
      insuff = 3050
   ELSE
      i1 = i - 1
      i2 = i1
      i3 = i1 + flag
      DO
         z(i2+1) = orf(lshift(z(i1+1),ihalf),z(i1+2))
         z(i2+2) = z(i1+3)
         i1 = i1 + 3
         i2 = i2 + 2
         IF ( i1>=i3 ) THEN
            flag = i2 - i + 1
            insuff = 3050
            EXIT
         ENDIF
      ENDDO
   ENDIF
 1900 i3 = i + flag
   IF ( i3<j ) THEN
      i = i3
      CALL read(*9400,*9500,rgt,z(j+1),nrigid,1,flag)
      CALL close(rgt,clsrew)
      CALL gopen(rgt,z(buf3),1)
   ELSE
      WRITE (outtap,99001) i , i3 , j , flag , buf3 , nrigid , n23
99001 FORMAT ('  GP4/3060 I,I3,J,FLAG,BUF3,NRIGID,N23 =',7I7)
      icrq = i - j
      GOTO 9600
   ENDIF
!
!     SORT THE LIST OF DEPENDENT SIL VALUES
!     THUS FORMING THE UM SUBSET
!
 2000 ii = j + 1
   m = buf3 - ii
   nnx = buf3 - 1
   IF ( m/=1 ) THEN
      CALL sort(0,0,1,1,z(ii),m)
!
!     CHECK FOR DEPENDENT COMPONENT ERRORS IN MPC/RIGID ELEMENT DATA
!
      jj = nnx - 1
      nold = 0
      jxx = 0
      DO j = ii , jj
         IF ( z(j)/=nold ) THEN
            IF ( z(j)==z(j+1) ) THEN
               nold = z(j)
               nogo = 1
               jxx = jxx + 1
               IF ( jxx<=50 ) THEN
                  CALL page2(2)
                  WRITE (outtap,99002) ufm , z(j)
99002             FORMAT (A23,' 2423, DEPENDENT COMPONENT SPECIFIED MORE THAN ONCE',' ON MPC CARDS AND/OR IN RIGID ELEMENTS.  SIL ='&
                        & ,I9)
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      IF ( jxx>50 ) WRITE (outtap,99003)
99003 FORMAT (//12X,12H... AND MORE,/)
   ENDIF
   IF ( nogo/=0 ) GOTO 10600
   CALL write(scr1,z(ii),m,1)
!
!     SORT THE LIST OF CODED COL AND ROW NOS (OR UNCODED NOS)
!     THEN BLDPK EACH COL THUS FORMING THE RG MATRIX
!
   n = i - impc
   nmpc = i - n23
   j = impc
   IF ( n23==3 ) CALL sort2k(0,0,3,1,z(j),n)
   IF ( n23==2 ) CALL sort(0,0,2,1,z(j),n)
!
!     CHECK FOR INDEPENDENT COMPONENT ERRORS IN MPC DATA
!
   kj = j + n - 2*n23
   nold = 0
   nogo = 0
   DO kk = j , kj , n23
      IF ( z(kk)/=nold ) THEN
         IF ( z(kk)==z(kk+n23) ) THEN
            IF ( n23/=3 .OR. z(kk+1)==z(kk+n23+1) ) THEN
               nold = z(kk)
               nogo = 1
               jj = nold
               IF ( n23==2 ) jj = rshift(nold,ihalf)
               CALL page2(-2)
               WRITE (outtap,99004) ufm , jj
99004          FORMAT (A23,' 3180, INDEPENDENT COMPONENT SPECIFIED MORE THAN ','ONCE IN AN MPC RELATIONSHIP.   SIL =',I6)
            ENDIF
         ENDIF
      ENDIF
   ENDDO
   IF ( nogo/=0 ) GOTO 10600
   ncol = 1
   m = buf3 - i
   n231 = n23 - 1
   CALL bldpk(1,1,rgt,0,0)
 2100 DO WHILE ( j<=nmpc )
      jj = z(j)
      IF ( n23==2 ) jj = rshift(z(j),ihalf)
      IF ( jj>ncol ) EXIT
      ix = z(j+1)
      IF ( n23==2 ) ix = andf(z(j),mask16)
      x(1) = z(j+n231)
      DO nn1 = ii , nnx
         IF ( ix==z(nn1) ) GOTO 2150
      ENDDO
      GOTO 10600
 2150 ix = nn1 - ii + 1
      CALL zblpki
      j = j + n23
   ENDDO
   CALL bldpkn(rgt,0,mcb)
   ncol = ncol + 1
   IF ( ncol<=luset ) THEN
      CALL bldpk(1,1,rgt,0,0)
      GOTO 2100
   ELSE
      mcb(3) = multi
      CALL wrttrl(mcb)
      CALL close(rgt,clsrew)
   ENDIF
!
!     READ OMIT CARDS (IF PRESENT).
!
 2200 i = knkl1
   CALL locate(*2600,z(buf1),omit,flag)
   ASSIGN 2400 TO ret
   ASSIGN 9900 TO ret1
   ASSIGN 2300 TO ret2
   ASSIGN 2500 TO ret3
   omit1 = 1
 2300 CALL read(*9400,*2600,geomp,buf,2,0,flag)
   gpoint = buf(1)
   GOTO 9000
 2400 index = 3
   icomp = buf(2)
   GOTO 9100
 2500 IF ( icomp/=0 ) gpoint = gpoint + icomp - 1
   z(i) = gpoint
   i = i + 1
   IF ( i<=buf3 ) GOTO 2300
   icrq = i - buf3
   insuff = 345
   GOTO 9600
!
!     READ OMIT1 CARDS (IF PRESENT).
!
 2600 IF ( nogo/=0 ) GOTO 10600
   CALL locate(*3100,z(buf1),omitx1,flag)
   omit1 = 1
   ASSIGN 2900 TO ret
   ASSIGN 9900 TO ret1
   ASSIGN 2800 TO ret2
   ASSIGN 3000 TO ret3
 2700 CALL read(*9400,*3100,geomp,buf,1,0,flag)
   IF ( buf(1)/=0 ) CALL scalex(1,buf(1),buf(8))
 2800 CALL read(*9400,*3100,geomp,buf(2),1,0,flag)
   IF ( buf(2)==-1 ) GOTO 2700
   gpoint = buf(2)
   GOTO 9000
 2900 index = 5
   icomp = buf(1)
   GOTO 9100
 3000 IF ( icomp/=0 ) THEN
      gpoint = gpoint - 1
      DO ijk = 1 , 6
         IF ( buf(ijk+7)==0 ) EXIT
         z(i) = gpoint + buf(ijk+7)
         i = i + 1
      ENDDO
   ELSE
      z(i) = gpoint
      i = i + 1
   ENDIF
   GOTO 2800
 3100 IF ( omit1==1 ) THEN
      IF ( nogo/=0 ) GOTO 10600
!
!     SORT OMIT AND OMIT1 DATA AND WRITE IT ON SCR1.
!
      n = i - knkl1
      i = knkl1
      CALL sort(0,0,1,1,z(i),n)
      CALL write(scr1,z(i),n,1)
   ENDIF
!
!     READ SUPORT CARDS (IF PRESENT)
!
   CALL locate(*3600,z(buf1),suport,flag)
   react = 1
   i = knkl1
   ASSIGN 3300 TO ret
   ASSIGN 10000 TO ret1
   ASSIGN 3200 TO ret2
   ASSIGN 3400 TO ret3
 3200 CALL read(*9400,*3500,geomp,buf,2,0,flag)
   gpoint = buf(1)
   GOTO 9000
 3300 index = 7
   icomp = buf(2)
   GOTO 9100
 3400 IF ( icomp/=0 ) gpoint = gpoint + icomp - 1
   z(i) = gpoint
   i = i + 1
   IF ( i<buf3 ) GOTO 3200
   icrq = i - buf3
   insuff = 445
   GOTO 9600
 3500 IF ( nogo/=0 ) GOTO 10600
   n = i - knkl1
   i = knkl1
   CALL sort(0,0,1,1,z(i),n)
   CALL write(scr1,z(i),n,1)
!
!     READ THE GPDT AND EXTRACT CONSTRAINED POINTS (IF ANY)
!
 3600 CALL close(geomp,clsrew)
 3700 file = gpdt
   ASSIGN 3900 TO ret
   CALL gopen(gpdt,z(buf1),0)
 3800 DO
      CALL read(*9300,*4000,gpdt,buf,7,0,flag)
      IF ( buf(7)/=0 ) THEN
         j = buf(1) + km
         buf(1) = z(j)
         CALL scalex(buf,buf(7),buf(8))
!
!
!     INTERNAL SUBROUTINE TO SORT THE SCALAR COMPONENTS
!
         DO ii = 1 , 6
            IF ( buf(ii+7)==0 ) GOTO 3820
         ENDDO
         ii = 7
 3820    n = ii - 1
         IF ( n==0 ) GOTO ret
         DO ii = 1 , n
            ijk = luset + 1
            DO jj = ii , n
               IF ( buf(jj+7)<ijk ) THEN
                  ijk = buf(jj+7)
                  jjx = jj
               ENDIF
            ENDDO
            buf(jjx+7) = buf(ii+7)
            buf(ii+7) = ijk
         ENDDO
         GOTO ret
      ENDIF
   ENDDO
 3900 CALL write(scr1,buf(8),n,0)
   ugset = 1
   GOTO 3800
 4000 IF ( ugset>0 ) CALL write(scr1,0,0,1)
   CALL close(gpdt,clsrew)
   file = geomp
   IF ( nogeom==0 ) THEN
      IF ( mpcset/=0 ) CALL mesage(30,47,mpcset)
      IF ( spcset/=0 ) CALL mesage(30,53,spcset)
      IF ( mpcset/=0 .OR. spcset/=0 ) nogo = +1
      GOTO 6800
   ELSE
      CALL preloc(*9300,z(buf1),geomp)
   ENDIF
!
!     IF SPC SET IS SELECTED, READ SPCADD CARDS (IF PRESENT).
!     DETERMINE IF SET ID IS ON SPCADD CARD.
!     IF NOT, SIMULATE AN SPCADD SET LIST WITH ONE SET = SPCSET.
!
 4100 IF ( spcset==0 ) GOTO 5800
   ispcad = knkl1
   nspcad = knkl1
   ispc = ispcad + 2
   i = ispcad
   z(i) = spcset
   z(i+1) = 0
   CALL locate(*4300,z(buf1),spcadd,flag)
   DO
      CALL read(*9400,*4300,geomp,id,1,0,flag)
      IF ( id==spcset ) THEN
         DO
            CALL read(*9400,*4200,geomp,buf,1,0,flag)
            IF ( buf(1)==-1 ) THEN
               CALL fwdrec(*9400,geomp)
               GOTO 4200
            ELSE
               z(i) = buf(1)
               z(i+1) = 0
               i = i + 2
            ENDIF
         ENDDO
      ELSE
         DO
            CALL fread(geomp,id,1,0)
            IF ( id==-1 ) EXIT
         ENDDO
      ENDIF
   ENDDO
 4200 ispc = i
   nspcad = i - 2
!
!     READ SPC1 AND SPC CARDS.
!     FOR EACH SET ID WHICH IS IN THE SPCADD SET LIST,
!     CONVERT THE GRID POINT NO. AND COMPONENT VALUE (OR SCALAR NO.)
!     TO AN SIL VALUE. SAVE A LIST IN CORE OF SIL VALUES AND
!     ENFORCED DISPLACEMENT (ON SPC1 CARDS, ENF. DISPL. = 0.)
!
 4300 i = ispc
!
!     PROCESSING OF SPC CARDS EXECUTES FIRST.
!
   CALL locate(*4400,z(buf1),spc,flag)
   ASSIGN 5300 TO ret
   ASSIGN 10500 TO ret1
   ASSIGN 5100 TO ret2
   ASSIGN 5400 TO ret3
   GOTO 5100
!
!     SPC1 PROCESSING EXECUTES AFTER SPC PROCESSING
!
 4400 IF ( nogo/=0 ) GOTO 10600
   CALL locate(*5700,z(buf1),spc1,flag)
   ASSIGN 4900 TO ret
   ASSIGN 10100 TO ret1
   ASSIGN 4800 TO ret2
   ASSIGN 5000 TO ret3
 4500 DO
      CALL read(*9400,*5700,geomp,id,1,0,flag)
      DO k = ispcad , nspcad , 2
         IF ( z(k)==id ) GOTO 4700
      ENDDO
      DO
         CALL fread(geomp,buf,1,0)
         IF ( buf(1)==-1 ) GOTO 4600
      ENDDO
      EXIT
 4600 ENDDO
 4700 z(k+1) = 1
   CALL fread(geomp,buf,1,0)
   single = 1
   IF ( buf(1)/=0 ) CALL scalex(1,buf(1),buf(8))
 4800 CALL read(*9400,*4500,geomp,buf(2),1,0,flag)
   IF ( buf(2)<0 ) GOTO 4500
   gpoint = buf(2)
   GOTO 9000
 4900 index = 9
   icomp = buf(1)
   GOTO 9100
 5000 IF ( icomp/=0 ) THEN
      gpoint = gpoint - 1
      DO ijk = 1 , 6
         IF ( buf(ijk+7)==0 ) EXIT
         z(i) = gpoint + buf(ijk+7)
         z(i+1) = 0
         i = i + 2
      ENDDO
   ELSE
      z(i) = gpoint
      z(i+1) = 0
      i = i + 2
   ENDIF
   GOTO 4800
 5100 DO
      CALL read(*9400,*5500,geomp,buf,4,0,flag)
      DO k = ispcad , nspcad , 2
         IF ( z(k)==buf(1) ) GOTO 5200
      ENDDO
   ENDDO
 5200 single = 1
   z(k+1) = 1
   gpoint = buf(2)
   GOTO 9000
 5300 index = 11
   icomp = buf(3)
   GOTO 9100
 5400 IF ( icomp/=0 ) THEN
      CALL scalex(gpoint,buf(3),buf(8))
      DO ijk = 1 , 6
         IF ( buf(ijk+7)==0 ) EXIT
         z(i) = buf(ijk+7)
         z(i+1) = buf(4)
         i = i + 2
      ENDDO
   ELSE
      z(i) = gpoint
      z(i+1) = buf(4)
      i = i + 2
   ENDIF
   GOTO 5100
 5500 IF ( nogo/=0 ) GOTO 10600
   n = i - ispc
   IF ( n<=2 ) GOTO 4400
!
!     CHECK FOR DUPLICATELY DEFINED ENFORCED DISPLACEMENTS ON SPC CARDS
!
   CALL sort(0,0,2,1,z(ispc),n)
   n = n - 2
   nold = 0
   DO k = 1 , n , 2
      IF ( z(ispc+k-1)/=nold ) THEN
         IF ( z(ispc+k-1)==z(ispc+k+1) ) THEN
            IF ( z(ispc+k)/=0 .OR. z(ispc+k+2)/=0 ) THEN
               nold = z(ispc+k-1)
               nogo = 1
               CALL page2(3)
               WRITE (outtap,99005) ufm , nold
99005          FORMAT (A23,' 3147, ENFORCED DISPLACEMENT ON SPC CARDS SPECIFIED',' MORE THAN ONCE',/5X,                             &
                      &'FOR THE SAME COMPONENT.  SIL VALUE =',I10)
            ENDIF
         ENDIF
      ENDIF
   ENDDO
   IF ( nogo==0 ) GOTO 4400
   GOTO 10600
!
!     FLUID PROBLEM AND NO SPC-S AT ALL.
!
 5600 spcset = 0
   GOTO 4100
 5700 nspc = i - 2
   icrq = nspc - buf3
   insuff = 740
   IF ( icrq>0 ) GOTO 9600
!
!     DETERMINE IF ALL SPC SETS IN SPCADD SET LIST HAVE BEEN DEFINED
!
   IF ( nogo/=0 ) GOTO 10600
   DO k = ispcad , nspcad , 2
      IF ( z(k+1)==0 ) THEN
         IF ( iaxif/=0 .AND. z(k)==200000000 ) GOTO 5600
         nogo = 1
         buf(1) = z(k)
         buf(2) = 0
         CALL mesage(30,53,buf)
      ENDIF
   ENDDO
   IF ( nogo/=0 ) GOTO 10600
!
!     SORT THE SPC LIST AND WRITE IT ON SCR1
!
   n = nspc - ispc + 2
   CALL sort(0,0,2,1,z(ispc),n)
   CALL write(scr1,z(ispc),n,1)
!
!     READ ASET CARDS (IF PRESENT)
!
 5800 i = knkl1
   CALL locate(*6200,z(buf1),aset,flag)
   ASSIGN 6000 TO ret
   ASSIGN 9900 TO ret1
   ASSIGN 5900 TO ret2
   ASSIGN 6100 TO ret3
   asetx = 1
 5900 CALL read(*9400,*6200,geomp,buf,2,0,flag)
   gpoint = buf(1)
   GOTO 9000
 6000 index = 15
   icomp = buf(2)
   GOTO 9100
 6100 IF ( icomp/=0 ) gpoint = gpoint + icomp - 1
   z(i) = gpoint
   i = i + 1
   IF ( i<=buf3 ) GOTO 5900
   icrq = i - buf3
   insuff = 1445
   GOTO 9600
!
!     READ ASET1 CARDS (IF PRESENT)
!
 6200 IF ( nogo/=0 ) GOTO 10600
   CALL locate(*6700,z(buf1),aset1,flag)
   asetx = 1
   ASSIGN 6500 TO ret
   ASSIGN 9900 TO ret1
   ASSIGN 6400 TO ret2
   ASSIGN 6600 TO ret3
 6300 CALL read(*9400,*6700,geomp,buf,1,0,flag)
   IF ( buf(1)/=0 ) CALL scalex(1,buf(1),buf(8))
 6400 CALL read(*9400,*6700,geomp,buf(2),1,0,flag)
   IF ( buf(2)==-1 ) GOTO 6300
   gpoint = buf(2)
   GOTO 9000
 6500 index = 17
   icomp = buf(1)
   GOTO 9100
 6600 IF ( icomp/=0 ) THEN
      gpoint = gpoint - 1
      DO ijk = 1 , 6
         IF ( buf(ijk+7)==0 ) EXIT
         z(i) = gpoint + buf(ijk+7)
         i = i + 1
      ENDDO
   ELSE
      z(i) = gpoint
      i = i + 1
   ENDIF
   GOTO 6400
 6700 IF ( asetx/=1 ) THEN
      CALL close(geomp,clsrew)
   ELSE
      IF ( nogo/=0 ) GOTO 10600
!
!     SORT ASET AND ASET1 DATA AND WRITE IT ON SCR1
!
      n = i - knkl1
      i = knkl1
      CALL sort(0,0,1,1,z(i),n)
      CALL write(scr1,z(i),n,1)
      CALL close(geomp,clsrew)
   ENDIF
 6800 CALL close(scr1,clsrew)
!
!     FORM THE BASIC USET BY READING EACH OF THE SUBSETS AND
!     TURNING ON THE APPROPRIATE BIT IN THE APPROPRIATE WORD
!
   file = scr1
   CALL open(*9300,scr1,z(buf2),rdrew)
   DO k = 1 , luset
      z(k) = 0
   ENDDO
   buf(1) = multi
   buf(2) = omit1
   buf(3) = react
   buf(4) = usgset
   buf(5) = single
   buf(6) = asetx
   icount = 0
   DO k = 1 , 6
      IF ( buf(k)<0 ) CYCLE
      IF ( k<5 ) icount = icount + 1
      IF ( k/=2 .AND. k/=6 ) THEN
         mcbust(5) = orf(mcbust(5),mask(k))
         nosets = 1
         IF ( k==5 ) THEN
            DO
               CALL read(*9400,*6900,scr1,buf(7),2,0,flag)
               j = buf(7)
               z(j) = orf(z(j),mask(k))
            ENDDO
         ENDIF
      ENDIF
 6850 DO
         CALL read(*9400,*6900,scr1,j,1,0,flag)
         IF ( k==2 ) EXIT
         IF ( k/=6 ) THEN
            IF ( andf(z(j),mask(k))==mask(k) ) THEN
               dup = 1
               IF ( iflag==0 ) THEN
                  file = uset
                  CALL open(*9300,uset,z(buf1),wrtrew)
                  iflag = 1
                  file = scr1
               ENDIF
               buf(1) = j
               buf(2) = k
               CALL write(uset,buf(1),2,0)
            ENDIF
            EXIT
         ELSEIF ( andf(z(j),mskua)==0 ) THEN
            EXIT
         ENDIF
      ENDDO
      z(j) = orf(z(j),mask(k))
      GOTO 6850
 6900 ENDDO
   IF ( dup/=0 ) THEN
      CALL write(uset,0,0,1)
      CALL close(uset,clsrew)
   ENDIF
   CALL close(scr1,clsrew)
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
   DO k = 1 , luset
      IF ( andf(mskck,z(k))==0 ) THEN
         imsk = mskrst
         z(k) = orf(z(k),mskrst)
      ENDIF
   ENDDO
   IF ( imsk==mask(6) ) asetx = 1
   IF ( imsk==mask(2) ) omit1 = 1
!
!     CALL SUBROUTINE GP4SP TO EXAMINE GRID POINT SINGULARITIES
!
   CALL gp4sp(buf2,buf3,buf4)
!
!     TURN ON CERTAIN FLAGS IF THERE ARE OMIT OR ASET
!     DEGREES OF FREEDOM
!
   omit1 = -1
   DO k = 1 , luset
      IF ( andf(z(k),mskuo)/=0 ) THEN
         mcbust(5) = orf(mcbust(5),mask(2))
         nosets = 1
         omit1 = 1
         EXIT
      ENDIF
   ENDDO
   DO k = 1 , luset
      IF ( andf(z(k),mskua)/=0 ) THEN
         mcbust(5) = orf(mcbust(5),mask(6))
         nol = 1
         EXIT
      ENDIF
   ENDDO
!
   CALL open(*9300,scr1,z(buf2),rdrew)
   CALL skprec(scr1,icount)
!
!     OPEN YS FILE. WRITE SPCSET IN YS HEADER.
!     IF NO USB SET (FROM SPC AND SPC1 CARDS), WRITE NULL COLUMN
!     FOR YS VECTOR. IF USB SET IS PRESENT, BUILD THE YS VECTOR.
!
   file = scr1
   CALL open(*7000,ys,z(buf3),wrtrew)
   noys = 1
   CALL fname(ys,buf)
   buf(3) = spcset
   CALL write(ys,buf,3,1)
 7000 ix = 0
   ii = 1
   IF ( single>0 ) THEN
      IF ( noys/=0 ) CALL bldpk(1,1,ys,0,0)
      DO
         CALL read(*9400,*7100,scr1,buf,2,0,flag)
         j = buf(1)
         IF ( buf(2)/=0 ) THEN
            DO k = ii , j
               IF ( andf(z(k),mskus)/=0 ) ix = ix + 1
            ENDDO
            ii = j + 1
            x(1) = buf(2)
            IF ( noys/=0 ) THEN
               CALL zblpki
            ELSEIF ( nogoof==0 ) THEN
               nogo = 1
               nogoof = 1
               CALL mesage(30,132,buf)
            ENDIF
         ENDIF
      ENDDO
   ELSE
      IF ( nauto>0 .OR. usgset>0 ) single = 1
      IF ( noys/=0 ) CALL bldpk(1,1,ys,0,0)
   ENDIF
 7100 IF ( noys/=0 ) CALL bldpkn(ys,0,mcbys)
   IF ( ii<=luset ) THEN
      DO k = ii , luset
         IF ( andf(z(k),mskus)/=0 ) ix = ix + 1
      ENDDO
   ENDIF
   mcbys(3) = ix
   IF ( noys/=0 ) THEN
      CALL wrttrl(mcbys)
      CALL close(ys,clsrew)
   ENDIF
   CALL close(scr1,clsrew)
!
   IF ( l21+l22>0 .OR. idsub>0 ) CALL gp4prt(buf1)
   IF ( nauto/=0 ) THEN
!
!     CHANGE AUTO SPC FLAGS TO BOUNDARY SPC FLAGS
!
      j = 0
      DO k = 1 , luset
         IF ( andf(z(k),mskus)/=0 ) THEN
            IF ( andf(z(k),mskusg)==0 .AND. andf(z(k),mskusb)==0 ) THEN
               z(k) = mask(5)
               j = 1
            ENDIF
         ENDIF
      ENDDO
      IF ( j==1 ) mcbust(5) = orf(mcbust(5),mask(5))
   ENDIF
!
   file = uset
   IF ( dup==0 ) GOTO 7300
   CALL open(*9300,uset,z(buf1),rdrew)
   file = scr1
   CALL open(*9300,scr1,z(buf2),wrtrew)
   file = uset
   DO
      CALL read(*7200,*7200,uset,buf(1),2,0,flag)
      CALL write(scr1,buf(1),2,0)
   ENDDO
 7200 CALL write(scr1,0,0,1)
   CALL close(uset,clsrew)
 7300 CALL open(*9300,uset,z(buf1),wrtrew)
   CALL fname(uset,buf)
   buf(3) = spcset
   buf(4) = mpcset
   CALL write(uset,buf,4,1)
   CALL write(uset,z(1),luset,1)
   IF ( nol==1 ) mcbust(5) = orf(mcbust(5),mask(6))
!
!     SEPARATE TRAILER WORD 4 INTO TWO PARTS
!
   mcbust(4) = rshift(mcbust(5),ihalf)
   mcbust(5) = andf(mcbust(5),complf(lshift(mcbust(4),ihalf)))
   CALL wrttrl(mcbust)
   CALL close(uset,clsrew)
!
!     PROCESS USET FOR CONSISTENCY OF DISPLACEMENT SET DEFINITIONS.
!     EACH POINT IN USET MAY BELONG TO AT MOST ONE DEPENDENT SUBSET.
!
   flag = 0
   mask(1) = mskum
   mask(2) = mskus
   mask(3) = mskuo
   mask(4) = mskur
   mskums = orf(mskum,mskus)
   mskuor = orf(mskuo,mskur)
   buf(1) = orf(mskus,mskuor)
   buf(2) = orf(mskum,mskuor)
   buf(3) = orf(mskur,mskums)
   buf(4) = orf(mskuo,mskums)
   mskall = orf(mskums,mskuor)
   mskal = orf(mskall,mskul)
   DO i = 1 , luset
      iuset = z(i)
      idepn = andf(mskal,iuset)
      DO ik = 1 , 4
         IF ( andf(mak(ik),idepn)==mak(ik) ) GOTO 7350
      ENDDO
      idepn = andf(iuset,mskall)
      IF ( idepn/=0 ) THEN
         DO j = 1 , 4
            msk1 = mask(j)
            msk2 = buf(j)
            IF ( andf(idepn,msk1)/=0 ) THEN
               IF ( andf(idepn,msk2)/=0 ) GOTO 7350
            ENDIF
         ENDDO
      ENDIF
      CYCLE
 7350 IF ( flag==0 .AND. iflag==0 ) THEN
         file = scr1
         CALL open(*9300,scr1,z(buf1),wrtrew)
      ENDIF
      buf(5) = i
      buf(6) = idepn
      flag = 1
      CALL write(scr1,buf(5),2,0)
   ENDDO
 7400 IF ( mpcf1>0 .OR. single>0 .OR. omit1>0 .OR. react>0 ) nosets = 1
   IF ( mpcf1==-1 .AND. single==-1 .AND. omit1==-1 ) noa = -1
   IF ( andf(mskua,mcbust(5))==0 .AND. omit1>=0 ) THEN
      CALL page2(2)
      WRITE (outtap,99006) ufm
99006 FORMAT (A23,' 2403, INVALID TO HAVE AN O-SET WITH A NULL A-SET.')
      nogo = 1
   ENDIF
   IF ( nogo/=0 ) GOTO 10600
   IF ( iflag/=0 .OR. flag/=0 ) GOTO 8600
!
!     RECOMPUTE YS MATRIX TO ACCOUNT FOR SPCD CARDS
!
!
   IF ( noys==0 .OR. nogeom==0 ) GOTO 8500
!     BRING EQEXIN,SIL,AND USET BACK INTO CORE
!
   ASSIGN 7500 TO ret
   GOTO 200
 7500 CALL gopen(uset,z(buf1),0)
   file = uset
   CALL read(*9400,*7600,uset,z(knkl1),buf4-knkl1,1,luset)
   icrq = buf4
   insuff = 9711
   GOTO 9600
 7600 CALL close(uset,1)
!
!     CONVERT USET POINTERS INTO SILA VALUES
!
   m = knkl1
   n = knkl1 + luset - 1
   ix = 0
   DO i = m , n
      IF ( andf(z(i),mskus)/=0 ) THEN
         ix = ix + 1
         z(i) = ix
      ELSE
         z(i) = 0
      ENDIF
   ENDDO
!
!     POSITION CASECC
!
   file = casecc
   iload = n + 1
   icrq = n + 2*nskp1 + 1 - buf4
   insuff = 977
   IF ( icrq>0 ) GOTO 9600
   CALL gopen(casecc,z(buf1),0)
   CALL skprec(casecc,nskip-1)
   DO i = 1 , nskp1
      DO
         CALL fread(casecc,buf,16,1)
         IF ( buf(16)==0 ) THEN
            k = iload + 2*(i-1)
            z(k) = buf(4)
            z(k+1) = 0
            EXIT
         ENDIF
      ENDDO
   ENDDO
   CALL close(casecc,clsrew)
!
!     CONVERT SPCD CARD TO SILA + VALUE AND WRITE ON SCR2
!
   CALL gopen(scr2,z(buf2),1)
   file = geomp
   CALL preloc(*9300,z(buf1),geomp)
   CALL locate(*8300,z(buf1),spcd,flag)
   nn = 2*nskp1 + iload - 2
   iold = 0
   irecn = 0
 7700 DO
      CALL read(*9400,*8200,geomp,buf,4,0,flag)
      DO i = iload , nn , 2
         IF ( buf(1)==z(i) ) GOTO 7800
!
!     GO ON TO NEXT SET
!
      ENDDO
   ENDDO
!
 7800 IF ( buf(1)/=iold ) THEN
      IF ( iold/=0 ) CALL write(scr2,0,0,1)
      iold = buf(1)
      irecn = irecn + 1
      DO i = iload , nn , 2
         IF ( iold==z(i) ) z(i+1) = irecn
      ENDDO
   ENDIF
   gpoint = buf(2)
   ASSIGN 7900 TO ret
   ASSIGN 10500 TO ret1
   ASSIGN 7700 TO ret2
   ASSIGN 8000 TO ret3
   GOTO 9000
!
!     FOUND SIL
!
 7900 index = 13
   icomp = buf(3)
   GOTO 9100
 8000 IF ( icomp/=0 ) THEN
!
!     BREAK UP COMPONENTS
!
      CALL scalex(gpoint,buf(3),buf(8))
      DO i = 1 , 6
         IF ( buf(i+7)==0 ) EXIT
         m = knkl1 + buf(i+7) - 1
         IF ( z(m)==0 ) GOTO 8100
         mcb(1) = z(m)
         mcb(2) = buf(4)
         CALL write(scr2,mcb,2,0)
      ENDDO
      GOTO 7700
   ELSE
      m = knkl1 + gpoint - 1
      IF ( z(m)/=0 ) THEN
         mcb(1) = z(m)
         mcb(2) = buf(4)
         CALL write(scr2,mcb,2,0)
         GOTO 7700
      ENDIF
   ENDIF
 8100 n = 108
   buf(1) = buf(2)
   buf(2) = buf(i+7) - gpoint
   GOTO 10300
!
!     END OF SPCD-S
!
 8200 IF ( nogo/=0 ) GOTO 10600
   CALL write(scr2,0,0,1)
 8300 CALL close(geomp,1)
   CALL close(scr2,1)
   IF ( single<0 ) GOTO 8500
!
!     BRING IN OLD YS
!
   n = 2*nskp1
   DO i = 1 , n
      k = iload + i - 1
      z(i) = z(k)
   ENDDO
   ioys = n
   inys = ioys + ix
   icrq = inys + ix - buf4
   insuff = 988
   IF ( icrq>0 ) GOTO 9600
   mcb(1) = ys
   CALL rdtrl(mcb)
   mcb(2) = 0
   mcb(6) = 0
   mcb(7) = 0
   CALL gopen(ys,z(buf1),0)
   itb = mcb(5)
   ita1 = itb
   itb1 = itb
   incr = 1
   incr1 = 1
   ii = 1
   ii1 = 1
   jj = mcb(3)
   jj1 = jj
   DO i = 1 , ix
      rz(ioys+i) = 0.0
   ENDDO
   CALL unpack(*8400,ys,rz(ioys+1))
 8400 CALL close(ys,clsrew)
   CALL gopen(ys,z(buf1),1)
   CALL gopen(scr2,z(buf2),0)
   file = scr2
   DO i = 1 , n , 2
!
!     COPY OLD YS TO NEW YS
!
      DO k = 1 , ix
         rz(inys+k) = rz(ioys+k)
      ENDDO
      IF ( z(i+1)/=0 ) THEN
!
!     POSITION SCR2
!
         CALL skprec(scr2,z(i+1)-1)
         DO
            CALL read(*9400,*8450,scr2,buf,2,0,flag)
            k = buf(1) + inys
            rz(k) = bufr(2)
         ENDDO
      ENDIF
!
!     PUT OUT COLUMN
!
 8450 CALL pack(rz(inys+1),ys,mcb)
      CALL rewind(scr2)
      CALL fwdrec(*9400,scr2)
   ENDDO
   CALL close(ys,1)
   CALL wrttrl(mcb)
   CALL close(scr2,1)
 8500 IF ( nogo/=0 ) GOTO 10600
   IF ( flag==0 ) THEN
      IF ( iogpst==1 ) CALL mesage(17,iautsp,0)
      RETURN
   ENDIF
!
!     INCONSISTENT DISPLACEMENT SET DEFINITIONS--
!     READ EQEXIN AND SIL INTO CORE. FOR EACH INCONSISTANT DEFINITION,
!     LOOK UP EXTERNAL NUMBER AND QUEUE MESSAGE.
!
 8600 CALL write(scr1,0,0,1)
   CALL close(scr1,clsrew)
   ASSIGN 8700 TO ret
   GOTO 200
 8700 CALL open(*9300,scr1,z(buf1),rdrew)
   isil = km + 1
   neqx = kn - 1
   z(knkl1) = luset + 1
 8800 CALL read(*8900,*8900,scr1,buf(5),2,0,iflg)
   DO i = isil , knkl1
      IF ( z(i+1)>buf(5) ) EXIT
   ENDDO
   intrnl = i - km
   komp = buf(5) - z(i) + 1
   IF ( z(i+1)-z(i)==1 ) komp = 0
   DO j = 1 , neqx , 2
      IF ( z(j+1)==intrnl ) EXIT
   ENDDO
   IF ( dup/=0 ) THEN
      IF ( iflag/=0 ) THEN
         CALL page2(2)
         IF ( ib6==2 ) THEN
         ELSEIF ( ib6==3 ) THEN
            IF ( komp==0 ) THEN
               WRITE (outtap,99012) ufm , z(j) , r
               nogo = 1
            ELSE
               WRITE (outtap,99011) ufm , z(j) , komp , r
               nogo = 1
            ENDIF
         ELSEIF ( ib6==4 ) THEN
            IF ( komp==0 ) THEN
               WRITE (outtap,99012) ufm , z(j) , sg
               nogo = 1
            ELSE
               WRITE (outtap,99011) ufm , z(j) , komp , sg
               nogo = 1
            ENDIF
         ELSEIF ( komp==0 ) THEN
            WRITE (outtap,99012) ufm , z(j) , mset
            nogo = 1
         ELSE
            nogo = 1
            WRITE (outtap,99011) ufm , z(j) , komp , mset
         ENDIF
         GOTO 8800
      ENDIF
   ENDIF
   buf(7) = z(j)
   buf(8) = komp
   IF ( andf(buf(6),mskum)/=0 ) buf(8) = buf(8) + 10
   IF ( andf(buf(6),mskus)/=0 ) buf(8) = buf(8) + 100
   IF ( andf(buf(6),mskuo)/=0 ) buf(8) = buf(8) + 1000
   IF ( andf(buf(6),mskur)/=0 ) buf(8) = buf(8) + 10000
   IF ( andf(buf(6),mskul)/=0 ) buf(8) = buf(8) + 100000
   CALL mesage(30,101,buf(7))
   GOTO 8800
 8900 IF ( dup==0 ) THEN
      CALL close(scr1,clsrew)
      GOTO 10600
   ELSEIF ( iflag==0 ) THEN
      CALL close(scr1,clsrew)
      GOTO 10600
   ELSE
      iflag = 0
      IF ( flag/=0 ) GOTO 8800
      CALL close(scr1,clsrew)
      GOTO 7400
   ENDIF
!
!
!     INTERNAL SUBROUTINE TO PERFORM BINARY SEARCH IN EQEXIN
!     AND CONVERT THE EXTERNAL NUMBER TO A SIL VALUE AND A
!     CORRESPONDING TYPE CODE
!
 9000 klo = 0
   khi = kn2
   lastk = 0
   DO
      k = (klo+khi+1)/2
      IF ( lastk==k ) THEN
         GOTO ret1
      ELSE
         lastk = k
         IF ( gpoint<z(2*k-1) ) THEN
            khi = k
         ELSEIF ( gpoint==z(2*k-1) ) THEN
            k = 2*k + kn
            ipoint = gpoint
            gpoint = z(k)/10
            icode = z(k) - 10*gpoint
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
 9100 IF ( icode==2 ) THEN
!
!     SCALAR POINTS ARE CHECKED HERE
!
      IF ( icomp<=1 ) GOTO 9200
      nogo = 1
      CALL page2(2)
      WRITE (outtap,99007) ufm , ipoint , ctype(index) , ctype(index+1)
99007 FORMAT (A23,' 3146, ILLEGAL COMPONENT SPECIFIED FOR SCALAR POINT',I9,4H ON ,2A4,6HCARDS.)
   ELSE
!
!     GRID POINTS ARE CHECKED HERE
!
      IF ( icomp>0 ) GOTO 9200
      nogo = 1
      CALL page2(2)
      WRITE (outtap,99008) ufm , ipoint , ctype(index) , ctype(index+1)
99008 FORMAT (A23,' 3145, COMPONENT 0 (OR BLANK) SPECIFIED FOR GRID ','POINT',I9,4H ON ,2A4,6HCARDS.)
   ENDIF
   GOTO ret2
 9200 GOTO ret3
!
!
!     FATAL ERROR MESSAGES
!
 9300 j = -1
   GOTO 9700
 9400 j = -2
   GOTO 9700
 9500 j = -3
   GOTO 9700
 9600 j = -8
   WRITE (outtap,99009) insuff
99009 FORMAT (/33X,'GP4 INSUFFICIENT CORE AT ',I5)
   file = icrq
 9700 CALL mesage(j,file,name)
 9800 buf(1) = gpoint
   buf(2) = mpcset
   n = 48
   gpoint = 1
   GOTO 10300
 9900 buf(1) = gpoint
   gpoint = 1
   n = 49
!WKBNE 3/95 NCL94002
   buf(2) = 0
   GOTO 10300
10000 buf(1) = gpoint
   gpoint = 1
   n = 50
   buf(2) = 0
   GOTO 10300
10100 n = 51
10200 buf(1) = gpoint
   buf(2) = spcset
   gpoint = 1
!WKBNB 3/95 NCL94002
   IF ( l51/=0 ) THEN
      WRITE (outtap,99010) uwm , 2051 , buf(1) , spcset
99010 FORMAT (A25,I5,' UNDEFINED GRID POINT ',I6,' IN SINGLE-POINT',' CONSTRAINT SET ',I8)
      GOTO 10400
   ENDIF
10300 nogo = 1
   CALL mesage(30,n,buf)
!WKBI  3/95 NCL94002
10400 GOTO ret2
10500 n = 52
   GOTO 10200
10600 IF ( l21+l22>0 .OR. idsub>0 ) CALL gp4prt(-buf4)
   j = -37
   GOTO 9700
99011 FORMAT (A23,' 2152, GRID POINT',I9,' COMPONENT',I3,' DUPLICATELY DEFINED IN THE ',A4,5H SET.)
99012 FORMAT (A23,' 2153, SCALAR POINT',I9,' DUPLICATELY DEFINED IN ','THE ',A4,5H SET.)
END SUBROUTINE gp4
