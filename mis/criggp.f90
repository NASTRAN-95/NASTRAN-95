
SUBROUTINE criggp(N23)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Bgpdt , Buf(20) , Buf1 , Buf2 , Buf3 , Buf4 , Cstm , Geomp , Gpoint , Ihalf , Iprec , Jhalf , Kn , Knkl1 , Ksystm(55) ,  &
         & Mach , Mask16 , Nogo , Nout , Rdrew , Rgt , Scr1 , Z(1)
   REAL Clsrew , Rd , Rz(1) , Wrt , Wrtrew
   DOUBLE PRECISION Dz(1)
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /gp4fil/ Geomp , Bgpdt , Cstm , Rgt , Scr1
   COMMON /gp4prm/ Buf , Buf1 , Buf2 , Buf3 , Buf4 , Knkl1 , Mask16 , Nogo , Gpoint , Kn
   COMMON /machin/ Mach , Ihalf , Jhalf
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER N23
!
! Local variable declarations
!
   REAL a(36) , b(6) , c(18) , coeff , det
   LOGICAL again , debug , genre , l38
   INTEGER crbar(2) , crbe1(2) , crbe2(2) , crbe3(2) , crigd1(2) , crigd2(2) , crigd3(2) , crigdr(2) , crrod(2) , crspli(2) ,       &
         & crtrpt(2) , file , flag , i , ib(6) , iba , ibase , ibb , ibuf1 , ic(1) , icomp , idepgp , idr , ifile , ig , igg , ij , &
         & ilast , imhere , indcmp(6) , index , indx , indxx , indxxx , iopen , irdg , ising , ita , itb , itc , itest , itype , j ,&
         & jrigid , k , kcol , khi , ki4 , kinew , kiold , klo , kn2 , knkl2 , knkl3 , knkl4 , krow , l , lastk , left , m ,        &
         & mask15 , mcode(2) , mdbgp , mdep , mm , more , mset , mu , n , name(2) , nbgpdt , ncstm , nind , ntype , nwds , ret ,    &
         & ret1
!
! End of declarations
!
!
!     ******************************************************************
!                                                                      *
!     THIS SUBROUTINE GENERATES COEFFICIENTS FOR RIGID ELEMENTS        *
!     FOR USE BY SUBROUTINE GP4.  THE DATA SO GENERATED IS             *
!     COMPATIBLE WITH MPC SET DATA.                                    *
!                                                                      *
!     (MODIFIED BY G.CHAN/SPERRY TO REDUCE EXCESSIVE OPENINGS,         *
!     CLOSINGS, AND READINGS OF THE BGPDT FILE (IN 2ND METHOD).        *
!     WITHOUT THIS MODIFICATION, A PROBLEM OF 2000 RIGID ELEMENTS,     *
!     FOR EXAMPLE, WOULD REQUIRE MORE THAN 10,000 OPENS AND 10,000     *
!     CLOSES AND OVER 10 MILLION CALLS TO SUBROUTINE READ   10/86)     *
!                                                                      *
!     (MODIFIED AGAIN BY G.CHAN/UNISYS TO INCLUDE CRROD, CRBAR, CRBE1, *
!     CRBE2, CRBE3, CRTRPLT, AND CRSPLINE RIGID ELEMENTS    11/88)     *
!                                                                      *
!     ******************************************************************
!
!     EXTERNAL          ORF    ,LSHIFT
!     INTEGER           ORF    ,LSHIFT
!WKBR 8/94 SUN  INTEGER           RDREW    ,CLRSEW
   EQUIVALENCE (Z(1),Rz(1),Dz(1))
   EQUIVALENCE (ic(1),c(1))
   EQUIVALENCE (Ksystm(2),Nout)
   EQUIVALENCE (Ksystm(55),Iprec)
   DATA crigd1/5310 , 53/ , crigd2/5410 , 54/ , crigd3/8310 , 83/ , crigdr/8210 , 82/ , crrod/6510 , 65/ , crbar/6610 , 66/ ,       &
      & crtrpt/6710 , 67/ , crbe1/6810 , 68/ , crbe2/6910 , 69/ , crbe3/7010 , 70/ , crspli/7110 , 71/
   DATA name/4HCRIG , 2HGP/
   DATA mset/4HMSET/
   DATA a/36*0./
   DATA debug/.FALSE./
   DATA l38/.FALSE./
!
!
!     ****************************************************************
!      OPEN CORE -
!                                        ALLOCATED BY
!     !<--- ALLOCATED BY GP4 --->!<------- CRIGGP -   --------->!
!     +--------+--------+------+-+----+-----+------   ----+-----+-----+
!     ! EQEXIN ! EQEXIN !SORTED!U!CSTM!BGPDT!   ...       ! DEP ! GINO!
!     !1ST REC !2ND REC ! SIL  !S!    !     !             ! SIL !BFFRS!
!     +--------+--------+------+-+----+-----+------   ----+-----+-----+
!      1      KN       KM        /    /    / \_KNKL1      /     /
!                            KNKL2  KNKL3 KNKL4         MU   BUF4
!
!      OPEN CORE FORMAT, STARTS WITH Z(KNKL2)
!      (KNKL2 = INITIAL VALUE OF KNKL1)
!
!      NUMBER OF WORDS                 CONTENTS
!
!           NCSTM    ***     COORDINATE SYSTEM TRANSFORMATION TABLE
!           NBGPDT   ***     BASIC GRID POINT DEFINITION TABLE
!                    *       (ONLY IF ENOUGH OPEN CORE SPACE AVAILABLE)
!                    ***     SIL 1   ***
!                    *       SIL 2     *
!             6      *       SIL 3     *  INDEPENDENT GRID POINT
!                    *       SIL 4     *
!                    *       SIL 5     *
!                    ***     SIL 6   ***
!                    ***     SIL 1          ***             ***
!                    *       DEGREE OF FREEDOM*               *
!                    *       INTERNAL INDEX   *               *
!                    *       SIL 2            *               *
!                    *       DEGREE OF FREEDOM*  FIRST DEPEND.*  ALL
!                    *       INTERNAL INDEX   *  GRID POINT   *  DEPEND.
!           3*MDEP   *           .            *               *  GRID
!                    *           .            *               *  POINTS
!                    *       SIL 6            *               *
!                    *       DEGREE OF FREEDOM*               *
!                    ***     INTERNAL INDEX ***             ***
!                    ***     INDEPENDENT GRID POINT BGPDT TABLE
!             4      *          WORD 1     COORDINATE SYSTEM ID-INTEGER
!                    ***        WORD 2-4 = X, Y, Z, IN BASIC SYSTEM-REAL
!          4*MDBGP   ***     DEPENDENT GRID POINT BGPDT TABLE
!                    ***
!         36*MDBGP   *       ROW STORED GG MATRIX (SINGLE PRECISION)
!                    ***     36 ELEMENTS * NO. DEPEND. GRID PT.
!          9*IPREC   ***     INDEPEND. GRID PT TRANSFORMATION MAT.-REAL
!          9*IPREC   ***     DEPEND. GRID PT TRANSFORMATION MATRIX-REAL
!         36*IPREC   ***     GG MATRIX  -  REAL  36 ELEMENTS
!                    ***          .        ***
!                    *            .          *
!                    *  AVAILABLE OPEN CORE  *
!                    *            .          *
!                    *            .          *
!                    ***          .        ***
!                    ***
!           MDEP     *       DEPENDENT SILS
!                    ***
!                    ***
!          BUFFERS   *       GINO BUFFERS
!                    ***
!
!     *************************************************************
!     NOTE  IPREC = 1   SINGLE PRECISION
!           IPREC = 2   DOUBLE PRECISION
!           MDEP  =     NUMBER DEPENDENT SILS
!           MDBGP =     NUMBER DEPENDENT GRID POINTS
!     *************************************************************
!
   mask15 = Jhalf/2
   kn2 = Kn/2
   ncstm = 0
   knkl2 = Knkl1
   kiold = 0
   ibuf1 = -99
   again = .FALSE.
   CALL sswtch(20,j)
   IF ( j==1 ) debug = .TRUE.
   CALL sswtch(38,j)
   IF ( j==1 ) l38 = .TRUE.
   CALL page2(-4)
   WRITE (Nout,99001) Uim
99001 FORMAT (A29,' 3113, RIGID ELEMENTS ARE BEING PROCESSED IN GP4',/)
!
!     OPEN CSTM AND READ INTO CORE, FROM Z(KNKL2) THRU Z(KNKL3)
!
   left = Buf4 - Knkl1
   file = Cstm
   CALL open(*200,Cstm,Z(Buf2),Rdrew)
   CALL skprec(Cstm,1)
   CALL read(*3700,*100,Cstm,Z(knkl2),left,1,ncstm)
   GOTO 4000
!
!     IF CORE WAS FILLED WITHOUT HITTING AN EOR, CALL MESAGE
!
 100  IF ( Iprec==1 ) CALL pretrs(Z(Knkl1),ncstm)
   IF ( Iprec==2 ) CALL pretrd(Z(Knkl1),ncstm)
   CALL close(Cstm,Clsrew)
 200  file = Bgpdt
   CALL open(*3600,Bgpdt,Z(Buf2),Rdrew)
   CALL fwdrec(*3800,Bgpdt)
   kiold = 0
!
!     CALCULATE STARTING POINT
!     AND READ BGPDT INTO OPEN CORE
!
   Knkl1 = Knkl1 + ncstm
   IF ( .NOT.(again) ) THEN
      knkl3 = Knkl1
      CALL read(*3700,*300,Bgpdt,Z(knkl3+1),Buf4-knkl3,1,nbgpdt)
      imhere = 305
      IF ( debug ) WRITE (Nout,99010) imhere
      knkl3 = 0
      nbgpdt = Knkl1
      again = .TRUE.
      CALL bckrec(Bgpdt)
   ENDIF
 300  IF ( .NOT.again ) CALL close(Bgpdt,Clsrew)
   knkl4 = knkl3 + nbgpdt
   Knkl1 = knkl4 + 1
   mu = Buf4 - 1
   irdg = 0
   itype = 0
   genre = .FALSE.
!
!     *************************************************************
!
!     CRIGD1, CRIDG2, AND CRBE2 RIGID ELEMENTS ARE PROCESSED HERE
!
!     *************************************************************
!
!     LOCATE CRIGD1 DATA IN THE INPUT FILE
!
   file = Geomp
   CALL locate(*400,Z(Buf1),crigd1,flag)
   irdg = 1
   GOTO 600
!
!     LOCATE CRIGD2 DATA ON INPUT FILE
!
 400  file = Geomp
   CALL locate(*500,Z(Buf1),crigd2,flag)
   irdg = 2
   imhere = 500
   IF ( debug ) WRITE (Nout,99008) imhere
   GOTO 600
!
!     LOCATE CRBE2 DATA ON INPUT FILE
!
 500  file = Geomp
   CALL locate(*1600,Z(Buf1),crbe2,flag)
   irdg = 3
   imhere = 600
   IF ( debug ) WRITE (Nout,99008) imhere
!
 600  IF ( debug ) WRITE (Nout,99007) irdg
!
!     READ ELEMENT ID AND INDEPENDENT GRID POINT NUMBER
!
 700  ifile = Geomp
   nwds = 2
   GOTO 900
 800  ifile = Scr1
   nwds = 9
 900  file = ifile
   CALL read(*3700,*3800,ifile,Buf,nwds,0,flag)
   IF ( (debug .OR. l38) .AND. Buf(1)/=ibuf1 ) WRITE (Nout,99002) Buf(1)
99002 FORMAT (5X,'ELEMENT',I8,' IS BEING PROCESSED')
   IF ( genre ) THEN
      ibuf1 = Buf(1)
!
!     SET UP INDEPENDENT D.O.F. FOR THE GENERAL RIGID ELEMENTS,
!     CRIGID3 AND CRBE1, AND ALSO THE CRBAR AND CRTRPLT ELEMENTS
!     WHICH WERE CONVERTED TO CRIGID3 FORMAT BY IFS3P
!
      DO i = 1 , 6
         indcmp(i) = Buf(i+2)
      ENDDO
      itype = Buf(9)
      IF ( itype==0 ) THEN
         DO i = 1 , 36
            a(i) = 0.0
         ENDDO
         index = 0
         ilast = 0
         DO i = 1 , 6
            IF ( indcmp(i)==i ) THEN
               j = 6*ilast + i
               a(j) = 1.0
               ilast = ilast + 1
            ENDIF
         ENDDO
         nind = ilast
      ENDIF
   ENDIF
!
   ASSIGN 1000 TO ret
   ASSIGN 1100 TO ret1
   idr = Buf(1)
   Gpoint = Buf(2)
   ntype = 1
   GOTO 3500
!
!     STORE SIL FOR INDEPENDENT DEGREES OF FREEDOM
!
 1000 DO i = 1 , 6
      Z(Knkl1+i-1) = Gpoint + i - 1
   ENDDO
 1100 kinew = k - 2*Kn
   ASSIGN 1300 TO ret
   ASSIGN 1200 TO ret1
!
!     READ DEPENDENT GRID POINTS
!
   j = Knkl1 + 3
   mdbgp = 0
   mdep = 0
 1200 CALL read(*3700,*3800,ifile,Buf,7,0,flag)
   IF ( Buf(1)==-1 ) THEN
!
!     HERE WHEN ALL DEPENDENT GRID POINTS FOR AN ELEMENT HAVE BEEN READ
!
      more = 0
      i = Knkl1 + 6 + 3*mdep + 4 + 4*mdbgp + (9+9+36*mdbgp+36)*Iprec
!
!     CHECK FOR OPEN CORE AVAILABILITY
!
      imhere = 176
      IF ( i>=mu ) GOTO 3900
      IF ( Buf(2)==0 ) more = 1
      IF ( Nogo/=0 ) GOTO 1500
!
!     LOCATE DATA IN BGPDT FOR INDEPENDENT GRID POINT
!
      iopen = Knkl1 + 6 + 3*mdep
      IF ( again ) THEN
         file = Bgpdt
         IF ( kinew<=kiold ) THEN
            CALL bckrec(Bgpdt)
            kiold = 0
         ENDIF
         ki4 = (kinew-kiold-1)*4
         IF ( ki4>0 ) CALL read(*3700,*3800,Bgpdt,Buf,-ki4,0,flag)
         CALL read(*3700,*3800,Bgpdt,Buf,4,0,flag)
         Z(iopen) = Buf(1)
         Z(iopen+1) = Buf(2)
         Z(iopen+2) = Buf(3)
         Z(iopen+3) = Buf(4)
         GOTO 1400
      ELSE
         ki4 = knkl3 + kinew*4
         IF ( ki4>knkl4 ) GOTO 4100
         Z(iopen) = Z(ki4-3)
         Z(iopen+1) = Z(ki4-2)
         Z(iopen+2) = Z(ki4-1)
         Z(iopen+3) = Z(ki4)
         GOTO 1400
      ENDIF
   ELSE
      mdbgp = mdbgp + 1
      Gpoint = Buf(1)
      ntype = 2
      GOTO 3500
   ENDIF
 1300 IF ( Nogo==0 ) THEN
!
!     STORE DEPENDENT GRID POINT SIL, DOF, AND INTERNAL INDEX
!
      DO i = 1 , 6
         IF ( Buf(i+1)/=0 ) THEN
            j = j + 3
            l = j
            Z(l) = Gpoint + i - 1
            Z(l+1) = i
            Z(l+2) = k - 2*Kn
            mdep = mdep + 1
         ENDIF
      ENDDO
   ENDIF
   GOTO 1200
 1400 kiold = kinew
!
!     SORT DEPENDENT DEGREE OF FREEDOM LIST ON BGPDT REFERENCE NUMBER
!
   i = mdep*3
   CALL sort(0,0,3,3,Z(Knkl1+6),i)
!
   j = 0
   m = 0
   indx = Knkl1 + 5
   indxx = Knkl1 + 6 + 3*mdep + 4
   DO i = 1 , mdep
      k = indx + 3*i
      kinew = Z(k)
      IF ( kiold/=kinew ) THEN
         j = j + 1
!
!     READ GRID POINT INFORMATION
!
         m = m + 1
         n = indxx + (m-1)*4
         IF ( again ) THEN
            file = Bgpdt
            IF ( kinew<=kiold ) THEN
               CALL bckrec(Bgpdt)
               kiold = 0
            ENDIF
            ki4 = (kinew-kiold-1)*4
            IF ( ki4>0 ) CALL read(*3700,*3800,Bgpdt,Buf,-ki4,0,flag)
            CALL read(*3700,*3800,Bgpdt,Buf,4,0,flag)
            Z(n) = Buf(1)
            Z(n+1) = Buf(2)
            Z(n+2) = Buf(3)
            Z(n+3) = Buf(4)
         ELSE
            ki4 = knkl3 + kinew*4
            IF ( ki4>knkl4 ) GOTO 4100
            Z(n) = Z(ki4-3)
            Z(n+1) = Z(ki4-2)
            Z(n+2) = Z(ki4-1)
            Z(n+3) = Z(ki4)
         ENDIF
         kiold = kinew
      ENDIF
      Z(k) = j
   ENDDO
!
   IF ( Iprec==2 ) THEN
!
!     FORM REFERENCE GRID POINT TRANSFORMATION MATRIX (DOUBLE PREC.)
!
      ibase = (Knkl1+6+3*mdep+4+4*mdbgp+36*mdbgp)/2 + 1
      iba = Knkl1 + 6 + 3*mdep
      ita = ibase
      IF ( Z(iba)/=0 ) CALL transd(Rz(iba),Dz(ita))
!
!     PREPARE POINTERS USED TO FORM THE G MATRIX
!
      itb = ita + 9
      itc = itb - 1
!
!     SET INDEXES FOR TRANSFORMATION MATRIXES AND GG MATRIXES TO
!     FIRST ELEMENT - 1 FOR SUBROUTINE FORMGG
!
      ita = ita - 1
      ig = indxx + 4*mdbgp - 1
      igg = ibase + 9 + 9 - 1
      indx = Knkl1 + 3
      m = -1
!
!     BEGIN LOOP TO FORM THE G MATRIX
!
      DO i = 1 , mdep
         k = indx + i*3
         mm = Z(k+2)
         IF ( mm/=m ) THEN
            ibb = indxx + (mm-1)*4
!
!     FORM DEPENDENT DEGREE OF FREEDOM TRANSFORMATION MATRIX
!
            IF ( Z(ibb)/=0 ) CALL transd(Rz(ibb),Dz(itb))
!
!     FORM THE GG MATRIX
!
            CALL formg2(igg,ita,itc,iba,ibb)
         ENDIF
!
!     SELECT PROPER ROW BASED ON COMPONENT NUMBER AND STORE IN G
!
         m = mm
         mm = Z(k+1)
         DO ij = 1 , 6
            indxxx = igg + (mm-1)*6 + ij
            Rz(ig+ij) = Dz(indxxx)
         ENDDO
         ig = ig + 6
      ENDDO
   ELSE
!
!     FORM REFERENCE GRID POINT TRANSFORMATION MATRIX
!
      iba = Knkl1 + 6 + 3*mdep
      ita = iba + 4 + 4*mdbgp + 36*mdbgp
      IF ( Z(iba)/=0 ) CALL transs(Rz(iba),Rz(ita))
!
!     PREPARE POINTERS USED TO FORM THE G MATRIX
!
      itb = ita + 9
      itc = itb - 1
!
!     SET INDEXES FOR TRANSFORMATION MATRIXES AND GG MATRIXES TO
!     FIRST ELEMENT - 1 FOR SUBROUTINE FORMGG
!
      ita = ita - 1
      ig = indxx + 4*mdbgp - 1
      igg = ig + (36*mdbgp) + 9 + 9
      indx = Knkl1 + 3
      m = -1
!
!     BEGIN LOOP TO FORM THE G MATRIX
!
      DO i = 1 , mdep
         k = indx + i*3
         mm = Z(k+2)
         IF ( mm/=m ) THEN
            ibb = indxx + (mm-1)*4
!
!     FORM DEPENDENT DEGREE OF FREEDOM TRANSFORMATION MATRIX
!
            IF ( Z(ibb)/=0 ) CALL transs(Rz(ibb),Rz(itb))
!
!     FORM THE GG MATRIX
!
            CALL formgg(igg,ita,itc,iba,ibb)
         ENDIF
!
!     SELECT PROPER ROW BASED ON COMPONENT NUMBER AND STORE IN G
!     ACCORDING TO PARTITIONING VECTOR OF REFERENCE GRID POINT.
!
         m = mm
         mm = Z(k+1)
         DO ij = 1 , 6
            indxxx = igg + (mm-1)*6 + ij
            Rz(ig+ij) = Rz(indxxx)
         ENDDO
         ig = ig + 6
      ENDDO
   ENDIF
   ig = indxx + 4*mdbgp - 1
!
!     WRITE THE CODED COLUMN-ROW NUMBERS AND ELEMENTS OF THE GM
!     MATRIX ON RGT FILE SO AS TO MAKE RIGID ELEMENT DATA
!     COMPATIBLE WITH MPC SET DATA
!     (REVISED 7/86, CODED COLUMN-ROW NUMBERS ARE NOT USED HERE.
!     THEY WILL BE RE-CODED IN GP4 IF NEEDED)
!
   k = 0
   IF ( .NOT.(genre .AND. itype==0) ) THEN
      mu = mu - mdep
!
!     TEST FOR OPEN CORE AVAILABILITY
!
      imhere = 3380
      IF ( iopen>=mu ) GOTO 3900
   ENDIF
   indx = Knkl1 + 3
   DO i = 1 , mdep
      IF ( .NOT.(genre .AND. itype==0) ) Z(mu+i) = Z(indx+i*3)
      krow = Z(indx+i*3)
      mcode(2) = krow
      IF ( krow>mask15 ) N23 = 3
      DO j = 1 , 6
         k = k + 1
         kcol = Z(Knkl1+j-1)
         mcode(1) = kcol
         IF ( kcol>mask15 ) N23 = 3
         IF ( genre .AND. itype==0 ) THEN
            IF ( index<nind ) THEN
               IF ( indcmp(j)==j ) THEN
                  index = index + 1
                  ib(index) = kcol
               ENDIF
            ENDIF
            a(6*ilast+j) = Rz(ig+k)
         ELSE
            Rz(ig+k) = -Rz(ig+k)
            IF ( genre .AND. itype==1 ) THEN
               ic(j) = ib(j)
               IF ( ic(j)>mask15 ) N23 = 3
            ELSE
               CALL write(Rgt,mcode,2,0)
               CALL write(Rgt,Rz(ig+k),1,0)
            ENDIF
         ENDIF
      ENDDO
      IF ( genre ) THEN
         IF ( itype/=-1 ) THEN
            IF ( itype==1 ) THEN
               CALL gmmats(Rz(ig+k-5),1,6,0,a,6,6,0,b)
               DO j = 1 , 6
                  CALL write(Rgt,ic(j),1,0)
                  CALL write(Rgt,krow,1,0)
                  CALL write(Rgt,b(j),1,0)
               ENDDO
            ELSE
               index = index + 1
               ib(index) = krow
               ilast = ilast + 1
               CYCLE
            ENDIF
         ENDIF
      ENDIF
      mcode(1) = krow
      coeff = 1.0
      CALL write(Rgt,mcode,2,0)
      CALL write(Rgt,coeff,1,0)
   ENDDO
   IF ( .NOT.(.NOT.genre .OR. itype/=0) ) THEN
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
      ising = -1
      CALL invers(6,a,6,b,0,det,ising,c)
!
!     CHECK TO SEE IF GENERAL RIGID ELEMENTS (CRIGD3, CRBE1, CRBAR, AND
!     CRTRPLT) ARE PROPERLY DEFINED
!
      IF ( ising==2 ) THEN
         WRITE (Nout,99009) Ufm , idr
         Nogo = 1
      ENDIF
   ENDIF
 1500 IF ( more==0 ) THEN
!
      IF ( genre ) CALL close(Scr1,1)
      IF ( irdg<=8 ) THEN
         IF ( irdg==1 ) GOTO 400
         IF ( irdg==2 ) GOTO 500
         IF ( irdg==3 ) GOTO 1600
         IF ( irdg==4 ) GOTO 1700
         IF ( irdg==5 ) GOTO 1800
         IF ( irdg==6 ) GOTO 1900
         IF ( irdg==7 ) GOTO 2200
      ENDIF
      CALL errtrc('CRIGGP  ',3655)
   ELSE
      IF ( genre ) GOTO 800
      GOTO 700
   ENDIF
!
!     ******************************************************************
!
!     CRBAR, CRTRPLT, CRIGD3, AND CRBE1 ELEMENTS ARE PROCESSED HERE.
!     THE CRBAR AND CRTRPLT HAVE THE SAME DATA FORMAT AS THAT OF THE
!     GENERAL RIGID ELEMENT CRIGD3.
!     CRBE1 WAS MADE EXACTLY SAME AS CRIGD3 IN IFS3P ROUTINE.
!
!     ******************************************************************
!
!     LOCATE CRBAR DATA ON INPUT FILE
!
 1600 file = Geomp
   imhere = 4000
   IF ( debug ) WRITE (Nout,99008) imhere
   CALL locate(*1700,Z(Buf1),crbar,flag)
   irdg = 4
   GOTO 2000
!
!     LOCATE CRTRPLT DATA ON INPUT FILE
!
 1700 file = Geomp
   imhere = 4100
   IF ( debug ) WRITE (Nout,99008) imhere
   CALL locate(*1800,Z(Buf1),crtrpt,flag)
   irdg = 5
   GOTO 2000
!
!     LOCATE CRIGD3 DATA ON INPUT FILE
!
 1800 CALL locate(*1900,Z(Buf1),crigd3,flag)
   imhere = 4200
   IF ( debug ) WRITE (Nout,99008) imhere
   irdg = 6
   GOTO 2000
!
!     LOCATE CRBE1 DATA ON INPUT FILE
!
 1900 CALL locate(*2200,Z(Buf1),crbe1,flag)
   imhere = 4300
   IF ( debug ) WRITE (Nout,99008) imhere
   irdg = 7
!
 2000 genre = .TRUE.
   more = 1
   IF ( debug ) WRITE (Nout,99007) irdg
!
!     OPEN SCR1 FILE TO WRITE
!
   CALL open(*3600,Scr1,Z(Buf4),1)
!
!     READ ELEMENT ID
!
 2100 file = Geomp
   CALL read(*3700,*3800,Geomp,Buf,1,0,flag)
   idr = Buf(1)
!
!     READ INDEPENDENT GRID POINTS AND THEIR COMPONENT NUMBERS
!
   n = 0
   j = Knkl1
   DO
      CALL read(*3700,*3800,Geomp,Buf,1,0,flag)
      IF ( Buf(1)==mset ) THEN
         nind = n/7
!
!     CHECK TO SEE IF THE NUMBER OF INDEPENDENT GRID POINTS
!     IS MORE THAN ONE AND SET TYPE FLAG
!
         itype = -1
         IF ( nind/=1 ) THEN
            itype = 0
            j = Knkl1
!
!     WRITE THE INDEPENDENT GRID POINTS AS A PSEUDO CRIGD2 ELEMENT
!
!
!     WRITE THE ELEMENT ID
!
            CALL write(Scr1,idr,1,0)
!
!     WRITE THE FIRST INDEPENDENT GRID POINT AND ITS COMPONENT NUMBERS
!
            CALL write(Scr1,Z(j),7,0)
!
!     WRITE THE TYPE FLAG
!
            CALL write(Scr1,itype,1,0)
!
!     WRITE THE REMAINING INDEPENDENT GRID POINTS AND THEIR
!     COMPONENT NUMBERS
!
            j = j + 7
            n = n - 7
            CALL write(Scr1,Z(j),n,0)
            DO l = 1 , 7
               Buf(l) = -1
            ENDDO
            Buf(2) = 0
            CALL write(Scr1,Buf,7,0)
            itype = 1
         ENDIF
!
!     WRITE THE FIRST INDEPENDENT GRID POINT AND ALL THE
!     DEPENDENT GRID POINTS AS A PSEUDO CRIGD2 ELEMENT
!
         j = Knkl1
!
!     WRITE THE ELEMENT ID
!
         CALL write(Scr1,idr,1,0)
!
!     WRITE THE FIRST INDEPENDENT GRID POINT AND ITS COMPONENT NUMBERS
!
         CALL write(Scr1,Z(j),7,0)
!
!     WRITE THE TYPE FLAG
!
         CALL write(Scr1,itype,1,0)
         DO
!
!     PROCESS THE DEPENDENT GRID POINTS AND THEIR COMPONENT NUMBERS
!
            CALL read(*3700,*3800,Geomp,Buf,7,0,flag)
            IF ( Buf(1)==-1 ) THEN
               IF ( Buf(2)==-1 ) more = 0
               DO l = 1 , 7
                  Buf(l) = -1
               ENDDO
               Buf(2) = 0
               IF ( more==0 ) Buf(2) = -1
               CALL write(Scr1,Buf,7,0)
               IF ( more==1 ) GOTO 2100
               CALL write(Scr1,0,0,1)
!
!     CLOSE SCR1, AND OPEN IT FOR READ
!
               CALL close(Scr1,1)
               CALL open(*3600,Scr1,Z(Buf4),0)
               imhere = 5085
               IF ( debug ) WRITE (Nout,99008) imhere
               GOTO 800
            ELSE
               CALL write(Scr1,Buf,7,0)
            ENDIF
         ENDDO
      ELSE
         n = n + 7
         Z(j) = Buf(1)
         CALL read(*3700,*3800,Geomp,Z(j+1),6,0,flag)
         j = j + 7
      ENDIF
   ENDDO
!
!     *********************************************************
!
!     CRBE3 AND CRSPLINE ELEMENTS ARE PROCESSED HERE
!
!     *********************************************************
!
!     LOCATE CRBE3 DATA ON INPUT FILE
!
 2200 file = Geomp
   irdg = 8
   CALL locate(*2300,Z(Buf1),crbe3,flag)
   imhere = 5200
   IF ( debug ) WRITE (Nout,99008) imhere
   GOTO 2400
!
!     LOCATE CRSPLINE DATA ON INPUT FILE
!
 2300 file = Geomp
   irdg = 9
   CALL locate(*2600,Z(Buf1),crspli,flag)
   imhere = 530
   IF ( debug ) WRITE (Nout,99008) imhere
!
 2400 j = irdg - 7
   IF ( debug ) WRITE (Nout,99007) irdg
   IF ( Iprec==1 ) CALL crspls(*2500,j,mu,knkl3+1,Z(Knkl1),again,N23)
   IF ( Iprec==2 ) CALL crspld(*2500,j,mu,knkl3+1,Z(Knkl1),again,N23)
   IF ( j==1 ) GOTO 2300
   IF ( j==2 ) GOTO 2600
 2500 WRITE (Nout,99003) Ufm
99003 FORMAT (A23,' 8, INSUFFICIENT CORE FOR CRBE3 OR CRSPLINE RIGID ','ELEMENT COMPUTATION')
   Nogo = 1
!
!     *********************************************************
!
!     CRIGDR AND CRROD (RIGID ROD ELEMENTS) ARE PROCESSED HERE
!     (CRROD DATA FORMAT WAS CONVERTED TO CRIGDR FORMAT IN IFS3P)
!
!     *********************************************************
!
!     LOCATE CRIGDR AND CRROD DATA ON INPUT FILE
!
 2600 genre = .FALSE.
   nwds = 4
   file = Geomp
   CALL locate(*2700,Z(Buf1),crigdr,flag)
   irdg = 10
   imhere = 5800
   IF ( debug ) WRITE (Nout,99008) imhere
   GOTO 2800
 2700 file = Geomp
   irdg = 11
   CALL locate(*3400,Z(Buf1),crrod,flag)
   imhere = 5900
   IF ( debug ) WRITE (Nout,99008) imhere
!
!     ***************************************************************
!
!                  OPEN CORE FORMAT FOR RIGID ROD
!
!      NUMBER OF WORDS                 CONTENTS
!
!           NCSTM    ***     COORDINATE SYSTEM TRANSFORMATION TABLE
!           NBGPDT   ***     BASIC GRIP POINT DEFINITION TABLE
!                    ***     SIL 1 ***
!             3      *       SIL 2   * INDEPENDENT GRID POINT
!                    ***     SIL 3 ***
!                    ***     INDEPENDENT GRID POINT BGPDT TABLE
!             4      *          WORD 1     COORDINATE SYSTEM ID-INTEGER
!                    ***        WORD 2-4   X, Y, Z, IN BASIC SYSTEM-REAL
!                    ***     SIL 1 ***
!             3      *       SIL 2   * DEPENDENT GRID POINT
!                    ***     SIL 3 ***
!             4      ***     DEPENDENT GRID POINT BGPDT TABLE
!                    ***                   ***
!                    *                       *
!                    *  AVAILABLE OPEN CORE  *
!                    *                       *
!                    *                       *
!                    ***                   ***
!                    ***
!           MDEP     *       DEPENDENT SILS
!                    ***
!                    ***
!          BUFFERS   *       GINO BUFFERS
!                    ***
!
!     **************************************************************
!
!
!     CHECK AVAILABILITY OF CORE
!
 2800 IF ( debug ) WRITE (Nout,99007) irdg
   itest = Knkl1 + 14 + 27*Iprec + 2
   IF ( itest>=mu ) GOTO 3900
!
!     READ ELEMENT DATA
!
 2900 CALL read(*3700,*3400,Geomp,Buf,nwds,0,flag)
   idr = Buf(1)
   idepgp = Buf(3)
   icomp = Buf(4)
!
!     PROCESS THE INDEPENDENT GRID POINT
!
   file = Bgpdt
   j = Knkl1
   Gpoint = Buf(2)
   ASSIGN 3000 TO ret
   ASSIGN 3100 TO ret1
   GOTO 3500
!
!     STORE SIL VALUES
!
 3000 IF ( Nogo==0 ) THEN
      Z(j) = Gpoint
      Z(j+1) = Gpoint + 1
      Z(j+2) = Gpoint + 2
      kinew = k - 2*Kn
!
!     LOCATE DATA IN BGPDT
!
      IF ( again ) THEN
         IF ( kinew<=kiold ) THEN
            CALL bckrec(Bgpdt)
            kiold = 0
         ENDIF
         ki4 = (kinew-kiold-1)*4
         IF ( ki4>0 ) CALL read(*3700,*3800,Bgpdt,Buf,-ki4,0,flag)
         CALL read(*3700,*3800,Bgpdt,Buf,4,0,flag)
!
!     STORE BASIC GRID POINT DATA
!
         Z(j+3) = Buf(1)
         Z(j+4) = Buf(2)
         Z(j+5) = Buf(3)
         Z(j+6) = Buf(4)
      ELSE
         ki4 = knkl3 + kinew*4
         IF ( ki4>knkl4 ) GOTO 4100
         Z(j+3) = Z(ki4-3)
         Z(j+4) = Z(ki4-2)
         Z(j+5) = Z(ki4-1)
         Z(j+6) = Z(ki4)
      ENDIF
      kiold = kinew
      IF ( j/=Knkl1 ) THEN
         IF ( Iprec==1 ) CALL crdrd(*3200,*3300,mu,icomp,N23)
         IF ( Iprec==2 ) CALL crdrd2(*3200,*3300,mu,icomp,N23)
         GOTO 2900
      ENDIF
   ELSEIF ( j/=Knkl1 ) THEN
      GOTO 2900
   ENDIF
!
!     PROCESS THE DEPENDENT GRID POINT
!
 3100 j = j + 7
   Gpoint = idepgp
   ASSIGN 2900 TO ret1
   GOTO 3500
 3200 WRITE (Nout,99004) Ufm , idr
99004 FORMAT (A23,' 3133, RIGID ELEMENT',I9,' HAS ZERO LENGTH')
   Nogo = 1
   GOTO 2900
 3300 WRITE (Nout,99009) Ufm , idr
   Nogo = 1
   GOTO 2900
!
 3400 IF ( irdg==10 ) GOTO 2700
!
   IF ( again ) CALL close(Bgpdt,Clsrew)
   IF ( Nogo/=0 ) CALL mesage(-61,0,name)
   CALL write(Rgt,0,0,1)
!
!     WRITE A LIST OF DEPENDENT SIL VALUES FOR RIGID ELEMENTS ONTO THE
!     RGT IN SORTED FORM
!
   jrigid = mu + 1
   m = Buf4 - jrigid
   CALL sort(0,0,1,1,Z(jrigid),m)
   CALL write(Rgt,Z(jrigid),m,1)
   j = Buf4 - 1
   IF ( debug ) WRITE (Nout,99005) (Z(i),i=jrigid,j)
99005 FORMAT (/,'  CRIGGP/@7010  DEPEND.SIL LIST:',/,(5X,10I7))
   Knkl1 = knkl2
!
!     CLOSE RGT FILE AND RETURN
!
   CALL close(Rgt,Clsrew)
   RETURN
!
!     **********************************************************
!
!     INTERNAL SUBROUTINE TO PERFORM BINARY SEARCH IN EQEXIN
!     AND CONVERT THE EXTERNAL NUMBER TO A SIL VALUE
!
 3500 klo = 0
   khi = kn2
   lastk = 0
   DO
      k = (klo+khi+1)/2
      IF ( lastk==k ) THEN
         IF ( genre .AND. itype==1 .AND. ntype==1 ) GOTO 1100
         Buf(1) = Gpoint
         Buf(2) = irdg*100000000 + idr
         n = 151
         Nogo = 1
         CALL mesage(30,n,Buf)
         GOTO ret1
      ELSE
         lastk = k
         IF ( Gpoint<Z(2*k-1) ) THEN
            khi = k
         ELSEIF ( Gpoint==Z(2*k-1) ) THEN
            k = Z(2*k) + 2*Kn
            Gpoint = Z(k)
            GOTO ret
         ELSE
            klo = k
         ENDIF
      ENDIF
   ENDDO
!
!     **********************************************************
!
!     FATAL ERROR MESSAGES
!
 3600 j = -1
   CALL mesage(j,file,name)
   GOTO 4000
 3700 j = -2
   CALL mesage(j,file,name)
   GOTO 4000
 3800 j = -3
   CALL mesage(j,file,name)
   GOTO 4000
 3900 IF ( .NOT.(again) ) THEN
      CALL close(Scr1,Clsrew)
      WRITE (Nout,99010) imhere
!
!     IF THERE IS ENOUGH CORE AVAILABLE, OPEN AND READ BGPDT INTO OPEN
!     CORE, FROM Z(KNKL3+1) THRU Z(KNKL4), CLOSE BGPDT FILE, AND RESET
!     VARIOUS POINTERS FOR BUILDING UP RGT DATA. (AGAIN=.FALSE.)
!     THIS METHOD USES ONLY ONE OPEN, ONE CLOSE, AND ONE READ.
!
!     HOWEVER, IF THERE IS NOT ENOUGH CORE FOR BGPDT DATA AND THE NEEDED
!     SPACE FOR BUILDING UP RGT DATA, SET AGAIN TO .TRUE., AND REPEAT
!     DATA PROCESSING BY READING DATA DIRECTLY OFF THE BGPDT FILE EACH
!     TIME WHEN THE BGPDT DATA IS NEEDED.   THIS SECOND METHOD USES ONLY
!     ONE OPEN, ONE CLOSE, AND MULTIPLE READS.
!
!     IN THE SECOND METHOD, TWO POINTERS, KIOLD AND KINEW, ARE USED TO
!     COMPUTE PRECISELY WHERE TO READ DATA OFF THE BGPDT FILE
!
      again = .TRUE.
      CALL write(Rgt,0,0,1)
      CALL bckrec(Rgt)
      knkl3 = 0
      Knkl1 = knkl2
      nbgpdt = Knkl1 + ncstm
      CALL close(Bgpdt,Clsrew)
      GOTO 200
   ENDIF
 4000 DO
      j = -8
      CALL mesage(j,file,name)
   ENDDO
 4100 WRITE (Nout,99006) Knkl1 , knkl3 , knkl4 , ki4
99006 FORMAT (//,' *** SYSTEM FATAL ERROR IN CRIGGP',4I10)
   j = -61
   CALL mesage(j,file,name)
   GOTO 4000
99007 FORMAT ('0 IRDG/CRIGGP =',I6)
99008 FORMAT ('0  I AM HERE/CRIGGP =',I6)
99009 FORMAT (A23,' 3134, RIGID ELEMENT',I9,' IS NOT PROPERLY DEFINED')
99010 FORMAT (///,' *** CRIGGP/GP4 NEEDS MORE OPEN CORE.',/5X,' CRIGGP REVERTED TO USE SLOW METHOD',I9,//)
END SUBROUTINE criggp
