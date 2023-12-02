!*==criggp.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE criggp(N23)
   USE c_gp4fil
   USE c_gp4prm
   USE c_machin
   USE c_names
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: N23
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(36) , SAVE :: a
   LOGICAL :: again , genre
   REAL , DIMENSION(6) :: b
   REAL , DIMENSION(18) :: c
   REAL :: coeff , det
   INTEGER , DIMENSION(2) , SAVE :: crbar , crbe1 , crbe2 , crbe3 , crigd1 , crigd2 , crigd3 , crigdr , crrod , crspli , crtrpt ,   &
                                  & name
   LOGICAL , SAVE :: debug , l38
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER :: file , flag , i , iba , ibase , ibb , ibuf1 , icomp , idepgp , idr , ifile , ig , igg , ij , ilast , imhere , index , &
            & indx , indxx , indxxx , iopen , iprec , irdg , ising , ita , itb , itc , itest , itype , j , jrigid , k , kcol , khi ,&
            & ki4 , kinew , kiold , klo , kn2 , knkl2 , knkl3 , knkl4 , krow , l , lastk , left , m , mask15 , mdbgp , mdep , mm ,  &
            & more , mu , n , nbgpdt , ncstm , nind , nout , ntype , nwds , ret , ret1
   INTEGER , DIMENSION(6) :: ib , indcmp
   INTEGER , DIMENSION(1) :: ic
   INTEGER , DIMENSION(2) :: mcode
   INTEGER , SAVE :: mset
   REAL , DIMENSION(1) :: rz
   EXTERNAL bckrec , close , crdrd , crdrd2 , crspld , crspls , errtrc , formg2 , formgg , fwdrec , gmmats , invers , locate ,      &
          & mesage , open , page2 , pretrd , pretrs , read , skprec , sort , sswtch , transd , transs , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   !>>>>EQUIVALENCE (Z(1),Rz(1),Dz(1))
   !>>>>EQUIVALENCE (ic(1),c(1))
   !>>>>EQUIVALENCE (Ksystm(2),Nout)
   !>>>>EQUIVALENCE (Ksystm(55),Iprec)
   DATA crigd1/5310 , 53/ , crigd2/5410 , 54/ , crigd3/8310 , 83/ , crigdr/8210 , 82/ , crrod/6510 , 65/ , crbar/6610 , 66/ ,       &
      & crtrpt/6710 , 67/ , crbe1/6810 , 68/ , crbe2/6910 , 69/ , crbe3/7010 , 70/ , crspli/7110 , 71/
   DATA name/4HCRIG , 2HGP/
   DATA mset/4HMSET/
   DATA a/36*0./
   DATA debug/.FALSE./
   DATA l38/.FALSE./
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         mask15 = jhalf/2
         kn2 = kn/2
         ncstm = 0
         knkl2 = knkl1
         kiold = 0
         ibuf1 = -99
         again = .FALSE.
         CALL sswtch(20,j)
         IF ( j==1 ) debug = .TRUE.
         CALL sswtch(38,j)
         IF ( j==1 ) l38 = .TRUE.
         CALL page2(-4)
         WRITE (nout,99001) uim
99001    FORMAT (A29,' 3113, RIGID ELEMENTS ARE BEING PROCESSED IN GP4',/)
!
!     OPEN CSTM AND READ INTO CORE, FROM Z(KNKL2) THRU Z(KNKL3)
!
         left = buf4 - knkl1
         file = cstm
         CALL open(*40,cstm,z(buf2),rdrew)
         CALL skprec(cstm,1)
         CALL read(*520,*20,cstm,z(knkl2),left,1,ncstm)
         spag_nextblock_1 = 13
         CYCLE SPAG_DispatchLoop_1
!
!     IF CORE WAS FILLED WITHOUT HITTING AN EOR, CALL MESAGE
!
 20      IF ( iprec==1 ) CALL pretrs(z(knkl1),ncstm)
         IF ( iprec==2 ) CALL pretrd(z(knkl1),ncstm)
         CALL close(cstm,clsrew)
 40      file = bgpdt
         CALL open(*500,bgpdt,z(buf2),rdrew)
         CALL fwdrec(*540,bgpdt)
         kiold = 0
!
!     CALCULATE STARTING POINT
!     AND READ BGPDT INTO OPEN CORE
!
         knkl1 = knkl1 + ncstm
         IF ( .NOT.(again) ) THEN
            knkl3 = knkl1
            CALL read(*520,*60,bgpdt,z(knkl3+1),buf4-knkl3,1,nbgpdt)
            imhere = 305
            IF ( debug ) WRITE (nout,99010) imhere
            knkl3 = 0
            nbgpdt = knkl1
            again = .TRUE.
            CALL bckrec(bgpdt)
         ENDIF
 60      IF ( .NOT.again ) CALL close(bgpdt,clsrew)
         knkl4 = knkl3 + nbgpdt
         knkl1 = knkl4 + 1
         mu = buf4 - 1
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
         file = geomp
         CALL locate(*80,z(buf1),crigd1,flag)
         irdg = 1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     LOCATE CRIGD2 DATA ON INPUT FILE
!
 80      file = geomp
         CALL locate(*100,z(buf1),crigd2,flag)
         irdg = 2
         imhere = 500
         IF ( debug ) WRITE (nout,99008) imhere
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     LOCATE CRBE2 DATA ON INPUT FILE
!
 100     file = geomp
         CALL locate(*200,z(buf1),crbe2,flag)
         irdg = 3
         imhere = 600
         IF ( debug ) WRITE (nout,99008) imhere
         spag_nextblock_1 = 2
      CASE (2)
!
         IF ( debug ) WRITE (nout,99007) irdg
         spag_nextblock_1 = 3
      CASE (3)
!
!     READ ELEMENT ID AND INDEPENDENT GRID POINT NUMBER
!
         ifile = geomp
         nwds = 2
         spag_nextblock_1 = 5
      CASE (4)
         ifile = scr1
         nwds = 9
         spag_nextblock_1 = 5
      CASE (5)
         file = ifile
         CALL read(*520,*540,ifile,buf,nwds,0,flag)
         IF ( (debug .OR. l38) .AND. buf(1)/=ibuf1 ) WRITE (nout,99002) buf(1)
99002    FORMAT (5X,'ELEMENT',I8,' IS BEING PROCESSED')
         IF ( genre ) THEN
            ibuf1 = buf(1)
!
!     SET UP INDEPENDENT D.O.F. FOR THE GENERAL RIGID ELEMENTS,
!     CRIGID3 AND CRBE1, AND ALSO THE CRBAR AND CRTRPLT ELEMENTS
!     WHICH WERE CONVERTED TO CRIGID3 FORMAT BY IFS3P
!
            DO i = 1 , 6
               indcmp(i) = buf(i+2)
            ENDDO
            itype = buf(9)
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
         ASSIGN 120 TO ret
         ASSIGN 140 TO ret1
         idr = buf(1)
         gpoint = buf(2)
         ntype = 1
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
!
!     STORE SIL FOR INDEPENDENT DEGREES OF FREEDOM
!
 120     DO i = 1 , 6
            z(knkl1+i-1) = gpoint + i - 1
         ENDDO
 140     kinew = k - 2*kn
         ASSIGN 180 TO ret
         ASSIGN 160 TO ret1
!
!     READ DEPENDENT GRID POINTS
!
         j = knkl1 + 3
         mdbgp = 0
         mdep = 0
 160     CALL read(*520,*540,ifile,buf,7,0,flag)
         IF ( buf(1)==-1 ) THEN
!
!     HERE WHEN ALL DEPENDENT GRID POINTS FOR AN ELEMENT HAVE BEEN READ
!
            more = 0
            i = knkl1 + 6 + 3*mdep + 4 + 4*mdbgp + (9+9+36*mdbgp+36)*iprec
!
!     CHECK FOR OPEN CORE AVAILABILITY
!
            imhere = 176
            IF ( i>=mu ) THEN
               spag_nextblock_1 = 12
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( buf(2)==0 ) more = 1
            IF ( nogo/=0 ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     LOCATE DATA IN BGPDT FOR INDEPENDENT GRID POINT
!
            iopen = knkl1 + 6 + 3*mdep
            IF ( again ) THEN
               file = bgpdt
               IF ( kinew<=kiold ) THEN
                  CALL bckrec(bgpdt)
                  kiold = 0
               ENDIF
               ki4 = (kinew-kiold-1)*4
               IF ( ki4>0 ) CALL read(*520,*540,bgpdt,buf,-ki4,0,flag)
               CALL read(*520,*540,bgpdt,buf,4,0,flag)
               z(iopen) = buf(1)
               z(iopen+1) = buf(2)
               z(iopen+2) = buf(3)
               z(iopen+3) = buf(4)
               spag_nextblock_1 = 6
            ELSE
               ki4 = knkl3 + kinew*4
               IF ( ki4>knkl4 ) THEN
                  spag_nextblock_1 = 14
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               z(iopen) = z(ki4-3)
               z(iopen+1) = z(ki4-2)
               z(iopen+2) = z(ki4-1)
               z(iopen+3) = z(ki4)
               spag_nextblock_1 = 6
            ENDIF
         ELSE
            mdbgp = mdbgp + 1
            gpoint = buf(1)
            ntype = 2
            spag_nextblock_1 = 11
         ENDIF
         CYCLE
 180     IF ( nogo==0 ) THEN
!
!     STORE DEPENDENT GRID POINT SIL, DOF, AND INTERNAL INDEX
!
            DO i = 1 , 6
               IF ( buf(i+1)/=0 ) THEN
                  j = j + 3
                  l = j
                  z(l) = gpoint + i - 1
                  z(l+1) = i
                  z(l+2) = k - 2*kn
                  mdep = mdep + 1
               ENDIF
            ENDDO
         ENDIF
         GOTO 160
      CASE (6)
         kiold = kinew
!
!     SORT DEPENDENT DEGREE OF FREEDOM LIST ON BGPDT REFERENCE NUMBER
!
         i = mdep*3
         CALL sort(0,0,3,3,z(knkl1+6),i)
!
         j = 0
         m = 0
         indx = knkl1 + 5
         indxx = knkl1 + 6 + 3*mdep + 4
         DO i = 1 , mdep
            k = indx + 3*i
            kinew = z(k)
            IF ( kiold/=kinew ) THEN
               j = j + 1
!
!     READ GRID POINT INFORMATION
!
               m = m + 1
               n = indxx + (m-1)*4
               IF ( again ) THEN
                  file = bgpdt
                  IF ( kinew<=kiold ) THEN
                     CALL bckrec(bgpdt)
                     kiold = 0
                  ENDIF
                  ki4 = (kinew-kiold-1)*4
                  IF ( ki4>0 ) CALL read(*520,*540,bgpdt,buf,-ki4,0,flag)
                  CALL read(*520,*540,bgpdt,buf,4,0,flag)
                  z(n) = buf(1)
                  z(n+1) = buf(2)
                  z(n+2) = buf(3)
                  z(n+3) = buf(4)
               ELSE
                  ki4 = knkl3 + kinew*4
                  IF ( ki4>knkl4 ) THEN
                     spag_nextblock_1 = 14
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  z(n) = z(ki4-3)
                  z(n+1) = z(ki4-2)
                  z(n+2) = z(ki4-1)
                  z(n+3) = z(ki4)
               ENDIF
               kiold = kinew
            ENDIF
            z(k) = j
         ENDDO
!
         IF ( iprec==2 ) THEN
!
!     FORM REFERENCE GRID POINT TRANSFORMATION MATRIX (DOUBLE PREC.)
!
            ibase = (knkl1+6+3*mdep+4+4*mdbgp+36*mdbgp)/2 + 1
            iba = knkl1 + 6 + 3*mdep
            ita = ibase
            IF ( z(iba)/=0 ) CALL transd(rz(iba),dz(ita))
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
            indx = knkl1 + 3
            m = -1
!
!     BEGIN LOOP TO FORM THE G MATRIX
!
            DO i = 1 , mdep
               k = indx + i*3
               mm = z(k+2)
               IF ( mm/=m ) THEN
                  ibb = indxx + (mm-1)*4
!
!     FORM DEPENDENT DEGREE OF FREEDOM TRANSFORMATION MATRIX
!
                  IF ( z(ibb)/=0 ) CALL transd(rz(ibb),dz(itb))
!
!     FORM THE GG MATRIX
!
                  CALL formg2(igg,ita,itc,iba,ibb)
               ENDIF
!
!     SELECT PROPER ROW BASED ON COMPONENT NUMBER AND STORE IN G
!
               m = mm
               mm = z(k+1)
               DO ij = 1 , 6
                  indxxx = igg + (mm-1)*6 + ij
                  rz(ig+ij) = dz(indxxx)
               ENDDO
               ig = ig + 6
            ENDDO
         ELSE
!
!     FORM REFERENCE GRID POINT TRANSFORMATION MATRIX
!
            iba = knkl1 + 6 + 3*mdep
            ita = iba + 4 + 4*mdbgp + 36*mdbgp
            IF ( z(iba)/=0 ) CALL transs(rz(iba),rz(ita))
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
            indx = knkl1 + 3
            m = -1
!
!     BEGIN LOOP TO FORM THE G MATRIX
!
            DO i = 1 , mdep
               k = indx + i*3
               mm = z(k+2)
               IF ( mm/=m ) THEN
                  ibb = indxx + (mm-1)*4
!
!     FORM DEPENDENT DEGREE OF FREEDOM TRANSFORMATION MATRIX
!
                  IF ( z(ibb)/=0 ) CALL transs(rz(ibb),rz(itb))
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
               mm = z(k+1)
               DO ij = 1 , 6
                  indxxx = igg + (mm-1)*6 + ij
                  rz(ig+ij) = rz(indxxx)
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
            IF ( iopen>=mu ) THEN
               spag_nextblock_1 = 12
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         indx = knkl1 + 3
         DO i = 1 , mdep
            IF ( .NOT.(genre .AND. itype==0) ) z(mu+i) = z(indx+i*3)
            krow = z(indx+i*3)
            mcode(2) = krow
            IF ( krow>mask15 ) N23 = 3
            DO j = 1 , 6
               k = k + 1
               kcol = z(knkl1+j-1)
               mcode(1) = kcol
               IF ( kcol>mask15 ) N23 = 3
               IF ( genre .AND. itype==0 ) THEN
                  IF ( index<nind ) THEN
                     IF ( indcmp(j)==j ) THEN
                        index = index + 1
                        ib(index) = kcol
                     ENDIF
                  ENDIF
                  a(6*ilast+j) = rz(ig+k)
               ELSE
                  rz(ig+k) = -rz(ig+k)
                  IF ( genre .AND. itype==1 ) THEN
                     ic(j) = ib(j)
                     IF ( ic(j)>mask15 ) N23 = 3
                  ELSE
                     CALL write(rgt,mcode,2,0)
                     CALL write(rgt,rz(ig+k),1,0)
                  ENDIF
               ENDIF
            ENDDO
            IF ( genre ) THEN
               IF ( itype/=-1 ) THEN
                  IF ( itype==1 ) THEN
                     CALL gmmats(rz(ig+k-5),1,6,0,a,6,6,0,b)
                     DO j = 1 , 6
                        CALL write(rgt,ic(j),1,0)
                        CALL write(rgt,krow,1,0)
                        CALL write(rgt,b(j),1,0)
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
            CALL write(rgt,mcode,2,0)
            CALL write(rgt,coeff,1,0)
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
               WRITE (nout,99009) ufm , idr
               nogo = 1
            ENDIF
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
         IF ( more==0 ) THEN
!
            IF ( genre ) CALL close(scr1,1)
            IF ( irdg<=8 ) THEN
               IF ( irdg==1 ) GOTO 80
               IF ( irdg==2 ) GOTO 100
               IF ( irdg==3 ) GOTO 200
               IF ( irdg==4 ) GOTO 220
               IF ( irdg==5 ) GOTO 240
               IF ( irdg==6 ) GOTO 260
               IF ( irdg==7 ) GOTO 280
            ENDIF
            CALL errtrc('CRIGGP  ',3655)
         ELSE
            IF ( .NOT.(genre) ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
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
 200     file = geomp
         imhere = 4000
         IF ( debug ) WRITE (nout,99008) imhere
         CALL locate(*220,z(buf1),crbar,flag)
         irdg = 4
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
!
!     LOCATE CRTRPLT DATA ON INPUT FILE
!
 220     file = geomp
         imhere = 4100
         IF ( debug ) WRITE (nout,99008) imhere
         CALL locate(*240,z(buf1),crtrpt,flag)
         irdg = 5
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
!
!     LOCATE CRIGD3 DATA ON INPUT FILE
!
 240     CALL locate(*260,z(buf1),crigd3,flag)
         imhere = 4200
         IF ( debug ) WRITE (nout,99008) imhere
         irdg = 6
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
!
!     LOCATE CRBE1 DATA ON INPUT FILE
!
 260     CALL locate(*280,z(buf1),crbe1,flag)
         imhere = 4300
         IF ( debug ) WRITE (nout,99008) imhere
         irdg = 7
         spag_nextblock_1 = 8
      CASE (8)
!
         genre = .TRUE.
         more = 1
         IF ( debug ) WRITE (nout,99007) irdg
!
!     OPEN SCR1 FILE TO WRITE
!
         CALL open(*500,scr1,z(buf4),1)
         SPAG_Loop_1_1: DO
!
!     READ ELEMENT ID
!
            file = geomp
            CALL read(*520,*540,geomp,buf,1,0,flag)
            idr = buf(1)
!
!     READ INDEPENDENT GRID POINTS AND THEIR COMPONENT NUMBERS
!
            n = 0
            j = knkl1
            DO
               CALL read(*520,*540,geomp,buf,1,0,flag)
               IF ( buf(1)==mset ) THEN
                  nind = n/7
!
!     CHECK TO SEE IF THE NUMBER OF INDEPENDENT GRID POINTS
!     IS MORE THAN ONE AND SET TYPE FLAG
!
                  itype = -1
                  IF ( nind/=1 ) THEN
                     itype = 0
                     j = knkl1
!
!     WRITE THE INDEPENDENT GRID POINTS AS A PSEUDO CRIGD2 ELEMENT
!
!
!     WRITE THE ELEMENT ID
!
                     CALL write(scr1,idr,1,0)
!
!     WRITE THE FIRST INDEPENDENT GRID POINT AND ITS COMPONENT NUMBERS
!
                     CALL write(scr1,z(j),7,0)
!
!     WRITE THE TYPE FLAG
!
                     CALL write(scr1,itype,1,0)
!
!     WRITE THE REMAINING INDEPENDENT GRID POINTS AND THEIR
!     COMPONENT NUMBERS
!
                     j = j + 7
                     n = n - 7
                     CALL write(scr1,z(j),n,0)
                     DO l = 1 , 7
                        buf(l) = -1
                     ENDDO
                     buf(2) = 0
                     CALL write(scr1,buf,7,0)
                     itype = 1
                  ENDIF
!
!     WRITE THE FIRST INDEPENDENT GRID POINT AND ALL THE
!     DEPENDENT GRID POINTS AS A PSEUDO CRIGD2 ELEMENT
!
                  j = knkl1
!
!     WRITE THE ELEMENT ID
!
                  CALL write(scr1,idr,1,0)
!
!     WRITE THE FIRST INDEPENDENT GRID POINT AND ITS COMPONENT NUMBERS
!
                  CALL write(scr1,z(j),7,0)
!
!     WRITE THE TYPE FLAG
!
                  CALL write(scr1,itype,1,0)
                  DO
!
!     PROCESS THE DEPENDENT GRID POINTS AND THEIR COMPONENT NUMBERS
!
                     CALL read(*520,*540,geomp,buf,7,0,flag)
                     IF ( buf(1)==-1 ) THEN
                        IF ( buf(2)==-1 ) more = 0
                        DO l = 1 , 7
                           buf(l) = -1
                        ENDDO
                        buf(2) = 0
                        IF ( more==0 ) buf(2) = -1
                        CALL write(scr1,buf,7,0)
                        IF ( more==1 ) CYCLE SPAG_Loop_1_1
                        CALL write(scr1,0,0,1)
!
!     CLOSE SCR1, AND OPEN IT FOR READ
!
                        CALL close(scr1,1)
                        CALL open(*500,scr1,z(buf4),0)
                        imhere = 5085
                        IF ( debug ) WRITE (nout,99008) imhere
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ELSE
                        CALL write(scr1,buf,7,0)
                     ENDIF
                  ENDDO
               ELSE
                  n = n + 7
                  z(j) = buf(1)
                  CALL read(*520,*540,geomp,z(j+1),6,0,flag)
                  j = j + 7
               ENDIF
            ENDDO
            EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
!
!     *********************************************************
!
!     CRBE3 AND CRSPLINE ELEMENTS ARE PROCESSED HERE
!
!     *********************************************************
!
!     LOCATE CRBE3 DATA ON INPUT FILE
!
 280     file = geomp
         irdg = 8
         CALL locate(*300,z(buf1),crbe3,flag)
         imhere = 5200
         IF ( debug ) WRITE (nout,99008) imhere
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
!
!     LOCATE CRSPLINE DATA ON INPUT FILE
!
 300     file = geomp
         irdg = 9
         CALL locate(*340,z(buf1),crspli,flag)
         imhere = 530
         IF ( debug ) WRITE (nout,99008) imhere
         spag_nextblock_1 = 9
      CASE (9)
!
         j = irdg - 7
         IF ( debug ) WRITE (nout,99007) irdg
         IF ( iprec==1 ) CALL crspls(*320,j,mu,knkl3+1,z(knkl1),again,N23)
         IF ( iprec==2 ) CALL crspld(*320,j,mu,knkl3+1,z(knkl1),again,N23)
         IF ( j==1 ) GOTO 300
         IF ( j==2 ) GOTO 340
 320     WRITE (nout,99003) ufm
99003    FORMAT (A23,' 8, INSUFFICIENT CORE FOR CRBE3 OR CRSPLINE RIGID ','ELEMENT COMPUTATION')
         nogo = 1
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
 340     genre = .FALSE.
         nwds = 4
         file = geomp
         CALL locate(*360,z(buf1),crigdr,flag)
         irdg = 10
         imhere = 5800
         IF ( debug ) WRITE (nout,99008) imhere
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 360     file = geomp
         irdg = 11
         CALL locate(*480,z(buf1),crrod,flag)
         imhere = 5900
         IF ( debug ) WRITE (nout,99008) imhere
         spag_nextblock_1 = 10
      CASE (10)
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
         IF ( debug ) WRITE (nout,99007) irdg
         itest = knkl1 + 14 + 27*iprec + 2
         IF ( itest>=mu ) THEN
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     READ ELEMENT DATA
!
 380     CALL read(*520,*480,geomp,buf,nwds,0,flag)
         idr = buf(1)
         idepgp = buf(3)
         icomp = buf(4)
!
!     PROCESS THE INDEPENDENT GRID POINT
!
         file = bgpdt
         j = knkl1
         gpoint = buf(2)
         ASSIGN 400 TO ret
         ASSIGN 420 TO ret1
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
!
!     STORE SIL VALUES
!
 400     IF ( nogo==0 ) THEN
            z(j) = gpoint
            z(j+1) = gpoint + 1
            z(j+2) = gpoint + 2
            kinew = k - 2*kn
!
!     LOCATE DATA IN BGPDT
!
            IF ( again ) THEN
               IF ( kinew<=kiold ) THEN
                  CALL bckrec(bgpdt)
                  kiold = 0
               ENDIF
               ki4 = (kinew-kiold-1)*4
               IF ( ki4>0 ) CALL read(*520,*540,bgpdt,buf,-ki4,0,flag)
               CALL read(*520,*540,bgpdt,buf,4,0,flag)
!
!     STORE BASIC GRID POINT DATA
!
               z(j+3) = buf(1)
               z(j+4) = buf(2)
               z(j+5) = buf(3)
               z(j+6) = buf(4)
            ELSE
               ki4 = knkl3 + kinew*4
               IF ( ki4>knkl4 ) THEN
                  spag_nextblock_1 = 14
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               z(j+3) = z(ki4-3)
               z(j+4) = z(ki4-2)
               z(j+5) = z(ki4-1)
               z(j+6) = z(ki4)
            ENDIF
            kiold = kinew
            IF ( j/=knkl1 ) THEN
               IF ( iprec==1 ) CALL crdrd(*440,*460,mu,icomp,N23)
               IF ( iprec==2 ) CALL crdrd2(*440,*460,mu,icomp,N23)
               GOTO 380
            ENDIF
         ELSEIF ( j/=knkl1 ) THEN
            GOTO 380
         ENDIF
!
!     PROCESS THE DEPENDENT GRID POINT
!
 420     j = j + 7
         gpoint = idepgp
         ASSIGN 380 TO ret1
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
 440     WRITE (nout,99004) ufm , idr
99004    FORMAT (A23,' 3133, RIGID ELEMENT',I9,' HAS ZERO LENGTH')
         nogo = 1
         GOTO 380
 460     WRITE (nout,99009) ufm , idr
         nogo = 1
         GOTO 380
!
 480     IF ( irdg==10 ) GOTO 360
!
         IF ( again ) CALL close(bgpdt,clsrew)
         IF ( nogo/=0 ) CALL mesage(-61,0,name)
         CALL write(rgt,0,0,1)
!
!     WRITE A LIST OF DEPENDENT SIL VALUES FOR RIGID ELEMENTS ONTO THE
!     RGT IN SORTED FORM
!
         jrigid = mu + 1
         m = buf4 - jrigid
         CALL sort(0,0,1,1,z(jrigid),m)
         CALL write(rgt,z(jrigid),m,1)
         j = buf4 - 1
         IF ( debug ) WRITE (nout,99005) (z(i),i=jrigid,j)
99005    FORMAT (/,'  CRIGGP/@7010  DEPEND.SIL LIST:',/,(5X,10I7))
         knkl1 = knkl2
!
!     CLOSE RGT FILE AND RETURN
!
         CALL close(rgt,clsrew)
         RETURN
      CASE (11)
!
!     **********************************************************
!
!     INTERNAL SUBROUTINE TO PERFORM BINARY SEARCH IN EQEXIN
!     AND CONVERT THE EXTERNAL NUMBER TO A SIL VALUE
!
         klo = 0
         khi = kn2
         lastk = 0
         DO
            k = (klo+khi+1)/2
            IF ( lastk==k ) THEN
               IF ( genre .AND. itype==1 .AND. ntype==1 ) GOTO 140
               buf(1) = gpoint
               buf(2) = irdg*100000000 + idr
               n = 151
               nogo = 1
               CALL mesage(30,n,buf)
               GOTO ret1
            ELSE
               lastk = k
               IF ( gpoint<z(2*k-1) ) THEN
                  khi = k
               ELSEIF ( gpoint==z(2*k-1) ) THEN
                  k = z(2*k) + 2*kn
                  gpoint = z(k)
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
 500     j = -1
         CALL mesage(j,file,name)
         spag_nextblock_1 = 13
         CYCLE SPAG_DispatchLoop_1
 520     j = -2
         CALL mesage(j,file,name)
         spag_nextblock_1 = 13
         CYCLE SPAG_DispatchLoop_1
 540     j = -3
         CALL mesage(j,file,name)
         spag_nextblock_1 = 13
      CASE (12)
         IF ( .NOT.(again) ) THEN
            CALL close(scr1,clsrew)
            WRITE (nout,99010) imhere
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
            CALL write(rgt,0,0,1)
            CALL bckrec(rgt)
            knkl3 = 0
            knkl1 = knkl2
            nbgpdt = knkl1 + ncstm
            CALL close(bgpdt,clsrew)
            GOTO 40
         ENDIF
         spag_nextblock_1 = 13
      CASE (13)
         DO
            j = -8
            CALL mesage(j,file,name)
         ENDDO
         spag_nextblock_1 = 14
      CASE (14)
         WRITE (nout,99006) knkl1 , knkl3 , knkl4 , ki4
99006    FORMAT (//,' *** SYSTEM FATAL ERROR IN CRIGGP',4I10)
         j = -61
         CALL mesage(j,file,name)
         spag_nextblock_1 = 13
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99007 FORMAT ('0 IRDG/CRIGGP =',I6)
99008 FORMAT ('0  I AM HERE/CRIGGP =',I6)
99009 FORMAT (A23,' 3134, RIGID ELEMENT',I9,' IS NOT PROPERLY DEFINED')
99010 FORMAT (///,' *** CRIGGP/GP4 NEEDS MORE OPEN CORE.',/5X,' CRIGGP REVERTED TO USE SLOW METHOD',I9,//)
END SUBROUTINE criggp
