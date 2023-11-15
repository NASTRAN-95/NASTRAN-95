
SUBROUTINE premat(Iz,Rz,Bfr,Nimat,N2mat,Mptf,Ditf)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Alph1 , Alph12 , Alph2 , Alpha , Ax3 , Ay3 , Az3 , Bufm6(46) , Clsrew , Costh , Dum26(26) , E , Ex3 , Ey3 , Ez3 , G , G11 , &
      & G113 , G12 , G123 , G13 , G133 , G22 , G223 , G23 , G233 , G33 , G333 , Ge , Ge3 , Gey , Gxy3 , Gyz3 , Gzx3 , J11 , J12 ,   &
      & J22 , Matset , Nu , Nuxy3 , Nuyz3 , Nuzx3 , Plaans , Plaarg , Pzout(51) , Rd , Rdrew , Rho , Rho3 , Rhoy , Sigcy , Sigcy3 , &
      & Sigmac , Sigmas , Sigmat , Sigsy , Sigsy3 , Sigty , Sigty3 , Sinth , Skp(7) , Space(15) , Temp , To , Toy , Tref3 , Wrt ,   &
      & Wrtrew , Y(25) , Zz(1)
   INTEGER Buf(3) , Ib(46) , Icell2 , Indstr , Inflag , Matid , Nout , Sysbuf , Tempid , Z(1)
   LOGICAL Tdep
   COMMON /matin / Matid , Inflag , Temp , Plaarg , Sinth , Costh
   COMMON /matiso/ Bufm6
   COMMON /matout/ E , G , Nu , Rho , Alpha , To , Ge , Sigmat , Sigmac , Sigmas , Space , Tdep , Dum26
   COMMON /matpz / Pzout
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew
   COMMON /system/ Sysbuf , Nout , Skp , Tempid
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Ditf , Elemid , Mptf , N2mat , Nimat
   INTEGER Bfr(1) , Iz(1)
   REAL Rz(1)
!
! Local variable declarations
!
   REAL alphx , bufpz(51) , buftm6(39) , ce11 , ce12 , ce13 , ce33 , ce44 , ce66 , costho , dd , ddn1 , ddn2 , e15 , e31 , e33 ,    &
      & eps11 , eps33 , epso , ex , factor , gex , gx , nux , nuxx , prop , propt , rhox , se1 , se2 , sigcx , sigsx , sigtx ,      &
      & sintho , tox , up , x(27) , xl1 , xl2 , xl3 , xm1 , xm2 , xm3 , xn1 , xn2 , xn3 , xx , xy(108) , yy
   INTEGER back , dit , flag , i , igoto , ii , iii , ijk , ijkl , ilist , imat1 , imat2 , imat3 , imat6 , imat8 , imatf , imhere , &
         & imtpz1 , imtpz2 , inflgo , iret , isub , itabl , itype , j , jj , jjj , jx , k , khi , kkk , klo , kmat1 , kmat2 ,       &
         & kmat3 , kmat6 , kmat8 , kmatf , kmtpz1 , kmtpz2 , kx , kxx , kxx1 , l , l5 , lll , lmat1 , lmat2 , lmat3 , lmat6 ,       &
         & lmat8 , lmatf , lmtpz1 , lmtpz2 , lx , mapck , mat1(2) , mat2(2) , mat3(2) , mat6(2) , mat8(2) , matf(2) , matido ,      &
         & matpz1(2) , matpz2(2) , mats1(2) , matt1(2) , matt2(2) , matt3(2) , matt6(2) , min , mmat , mpt , mttpz1(2) , mttpz2(2) ,&
         & n , n1mat , nam(2) , nlist , nmat1
   INTEGER locfx
   INTEGER nmat2 , nmat3 , nmat6 , nmat8 , nmatf , nmtpz1 , nmtpz2 , nn , ntabl , ntypes , nwds , nwmat1 , nwmat2 , nx , offset ,   &
         & pass , qmat1 , qmat2 , qmat3 , qmat6 , qmat8 , qmatf , qmatx , qmtpz1 , qmtpz2 , ret , ret1 , tablei(16) , tablid
   LOGICAL part1 , pla
!
! End of declarations
!
!
!     REVISED 7/92, BY G.CHAN, NEW REFERENCE TO OPEN CORE ARRAY, SUCH
!     THAT THE SOURCE CODE IS UP TO ANSI FORTRAN 77 STANADARD
!
!
!     COMMON FOR PIEZOELECTRIC MATERIAL
!
!     ISOPARAMETRIC MATERIALS
!     COMMON /MATISO/ G11   ,G12   ,G13   ,G14   ,G15   ,G16   ,G12   ,
!                     G22,..,G56   ,G66   ,RHO   ,
!                     AXX   ,AYY   ,AZZ   ,AXY   ,AYZ   ,AZX   ,TREF  ,
!                     GE    ,IER
!
   EQUIVALENCE (E,Buf(1),Y(1)) , (Y(1),G11,Ex3,Plaans,Indstr) , (Y(2),G12,Ey3,Icell2) , (Y(3),G13,Ez3) , (Y(4),G22,Nuxy3) ,         &
    & (Y(5),G23,Nuyz3) , (Y(6),G33,Nuzx3) , (Y(7),Rhoy,Rho3) , (Y(8),Alph1,Gxy3) , (Y(9),Alph2,Gyz3) , (Y(10),Alph12,Gzx3) ,        &
    & (Y(11),Toy,Ax3) , (Y(12),Gey,Ay3) , (Y(13),Sigty,Az3) , (Y(14),Sigcy,Tref3) , (Y(15),Sigsy,Ge3)
   EQUIVALENCE (Y(16),J11,G113) , (Y(17),J12,G123) , (Y(18),J22,G133) , (Y(19),G223) , (Y(20),G233) , (Y(21),G333) , (Y(22),Sigty3) &
    & , (Y(23),Sigcy3) , (Y(24),Sigsy3) , (Y(25),Matset)
   EQUIVALENCE (x(1),ex) , (x(2),gx) , (x(3),nux) , (x(4),rhox) , (x(5),alphx) , (x(6),tox) , (x(7),gex) , (x(8),sigtx) ,           &
    & (x(9),sigcx) , (x(10),sigsx)
   EQUIVALENCE (Bufm6(1),Ib(1))
   EQUIVALENCE (Zz(1),Z(1))
!
!     DATA DEFINING CARDS TO BE READ
!
   DATA mat1 , kmat1 , lmat1/103 , 1 , 11 , 31/ , mat2 , kmat2 , lmat2/203 , 2 , 16 , 46/ , mat3 , kmat3 , lmat3/1403 , 14 , 16 ,   &
      & 46/ , mat6 , kmat6 , lmat6/2503 , 25 , 40 , 118/ , mat8 , kmat8 , lmat8/603 , 6 , 18 , 52/ , matt1/703 , 7/ , matt2/803 ,   &
      & 8/ , matt3/1503 , 15/ , matt6/2603 , 26/ , mats1/503 , 5/ , matpz1 , kmtpz1 , lmtpz1/1603 , 16 , 15 , 43/ , matpz2 ,        &
      & kmtpz2 , lmtpz2/1703 , 17 , 52 , 154/ , mttpz1/1803 , 18/ , mttpz2/1903 , 19/ , matf , kmatf , lmatf/5110 , 51 , 3 , 3/
   DATA ntypes , tablei/8 , 105 , 1 , 205 , 2 , 305 , 3 , 405 , 4 , 3105 , 31 , 3205 , 32 , 3305 , 33 , 3405 , 34/
   DATA nam/4HMAT  , 4H    /
!
!     MAT1 AND MAT2 HAVE ONE EXTRA WORD AT END
!
   DATA nwmat1/12/ , nwmat2/17/
!
!      - DATA IN /MATOUT/ IN VARIOUS MAT FORMATS, AND INFLAGS -
!
!     FORMAT  MAT1    MAT2                     MAT3      MAT6  MAT8 MATP
!     INFLAG=   1   2,3,12    4      5    6,8     7   11   10    12
!     -WORD- -----  ------  --- ------ ------ ----- ---- ---- ----- ----
!       1       E     G11   RHO INDSTR PLAANS    EX         :    E1    E
!       2       G     G12             ICDLL/8    EY         :  NU12    E
!       3      NU     G13                        EZ              E2
!       4     PHO     G22                      NUXY  RHO        G12
!       5    ALPH     G23                      NUYZ             G2Z
!       6      T0     G33                      NUZX             G1Z
!       7      GE     RHO                       RHO             RHO
!       8    SIGT   ALPH1    (X/N INDICATES     GXY           ALPH1
!       9    SIGC   ALPH2     ITEM X IS FOR     GYZ           ALPH2
!      10    SIGS  ALPH12     INFLAG=N ONLY)    GZX              TO
!      11              TO                        AX              TL
!      12              GE                        AY              CL
!      13            SIGT                        AZ              TT
!      14            SIGC                        TO              CT
!      15            SIGS                        GE              IS
!      16         E/12 J11/3                     G11             GE
!      17         E/12 J12/3                     G12            F12
!      18              J22/3                     G13
!      19                                        G22
!      20                                        G23
!      21                                        G33
!      22                                       SIGT
!      23                                       SIGC
!      24                                       SIGS
!      25  MATSET  MATSET                     MATSET          MATSET
!      26    TDEP
!       :
!
!     PERFORM GENERAL INITIALIZATION
!
   qmat1 = 0
   qmat2 = 0
   qmat3 = 0
   qmat6 = 0
   qmat8 = 0
   qmatf = 0
   qmatx = 0
   qmtpz1 = 0
   qmtpz2 = 0
   pla = .FALSE.
   IF ( Ditf<0 ) pla = .TRUE.
   part1 = .TRUE.
   i = -1
   mpt = Mptf
   dit = iabs(Ditf)
   offset = locfx(Iz(1)) - locfx(Z(1))
   IF ( offset<0 ) CALL errtrc('PREMAT  ',10)
   n1mat = Nimat + offset
!
!     READ MAT1,MAT2 AND MAT3 CARDS.  SPREAD FORMAT SO THAT MATTI AND
!     MATSI TEMPERATURE AND STRESS-STRAIN TABLE NUMBERS CAN BE MERGED
!
   CALL preloc(*2900,Bfr,mpt)
   imat1 = 1 + offset
   i = 1 + offset
   CALL locate(*200,Bfr,mat1,flag)
   qmat1 = 1
   imhere = 30
   DO
      CALL read(*10600,*100,mpt,Buf,nwmat1,0,flag)
      Z(i) = Buf(1)
      i = i + 1
      DO j = 2 , kmat1
         Z(i) = Buf(j)
         Z(i+1) = 0
         Z(i+2) = 0
         IF ( i>n1mat ) GOTO 11000
         i = i + 3
      ENDDO
   ENDDO
 100  nmat1 = i - lmat1
 200  imat2 = i
   CALL locate(*400,Bfr,mat2,flag)
   qmat2 = 1
   imhere = 70
   DO
      CALL read(*10600,*300,mpt,Buf,nwmat2,0,flag)
      Z(i) = Buf(1)
      i = i + 1
      DO j = 2 , kmat2
         Z(i) = Buf(j)
         Z(i+1) = 0
         Z(i+2) = 0
         IF ( i>n1mat ) GOTO 11000
         i = i + 3
      ENDDO
   ENDDO
 300  nmat2 = i - lmat2
 400  imat3 = i
   CALL locate(*600,Bfr,mat3,flag)
   qmat3 = 1
   imhere = 110
   DO
      CALL read(*10600,*500,mpt,Buf,kmat3,0,flag)
      Z(i) = Buf(1)
      i = i + 1
      DO j = 2 , kmat3
         Z(i) = Buf(j)
         Z(i+1) = 0
         Z(i+2) = 0
         IF ( i>n1mat ) GOTO 11000
         i = i + 3
      ENDDO
   ENDDO
 500  nmat3 = i - lmat3
 600  imtpz1 = i
   CALL locate(*800,Bfr,matpz1,flag)
   qmtpz1 = 1
   imhere = 132
   DO
      CALL read(*10600,*700,mpt,Buf,kmtpz1,0,flag)
      Z(i) = Buf(1)
      i = i + 1
      DO j = 2 , kmtpz1
         Z(i) = Buf(j)
         Z(i+1) = 0
         Z(i+2) = 0
         IF ( i>n1mat ) GOTO 11000
         i = i + 3
      ENDDO
   ENDDO
 700  nmtpz1 = i - lmtpz1
 800  imtpz2 = i
   CALL locate(*1000,Bfr,matpz2,flag)
   qmtpz2 = 1
   imhere = 136
   DO
      CALL read(*10600,*900,mpt,Buf,kmtpz2,0,flag)
      Z(i) = Buf(1)
      i = i + 1
      DO j = 2 , kmtpz2
         Z(i) = Buf(j)
         Z(i+1) = 0
         Z(i+2) = 0
         IF ( i>n1mat ) GOTO 11000
         i = i + 3
      ENDDO
   ENDDO
 900  nmtpz2 = i - lmtpz2
 1000 imat6 = i
   CALL locate(*1200,Bfr,mat6,flag)
   qmat6 = 1
   imhere = 141
   DO
      flag = 0
      CALL read(*10600,*1100,mpt,Buf,kmat6,0,flag)
      Z(i) = Buf(1)
      i = i + 1
      DO j = 2 , kmat6
         Z(i) = Buf(j)
         Z(i+1) = 0
         Z(i+2) = 0
         IF ( i>n1mat ) GOTO 11000
         i = i + 3
      ENDDO
   ENDDO
 1100 nmat6 = i - lmat6
   IF ( flag/=0 ) THEN
      n = 219
      GOTO 12700
   ENDIF
 1200 imat8 = i
   CALL locate(*1400,Bfr,mat8,flag)
   qmat8 = 1
   imhere = 1441
   DO
      flag = 0
      CALL read(*10600,*1300,mpt,Buf,kmat8,0,flag)
      Z(i) = Buf(1)
      i = i + 1
      DO j = 2 , kmat8
         Z(i) = Buf(j)
         Z(i+1) = 0
         Z(i+2) = 0
         IF ( i>n1mat ) GOTO 11000
         i = i + 3
      ENDDO
   ENDDO
 1300 nmat8 = i - lmat8
   IF ( flag/=0 ) THEN
      n = 219
      GOTO 12700
   ENDIF
 1400 imatf = i
   CALL locate(*1600,Bfr,matf,flag)
   qmatf = 1
   imhere = 145
   DO
      CALL read(*10600,*1500,mpt,Buf,kmatf,0,flag)
      Z(i) = Buf(1)
      Z(i+1) = Buf(2)
      Z(i+2) = Buf(3)
      i = i + 3
   ENDDO
 1500 nmatf = i - lmatf
 1600 ilist = i
   IF ( i>n1mat ) GOTO 11000
   CALL close(mpt,Clsrew)
!
!     IF TEMPERATURE OR PLA PROBLEM, READ THE MATTI OR MATSI CARDS.
!     MERGE MATSI AND MATTI DATA WITH MATI DATA.
!     SAVE A LIST OF TABLES REFERENCED.
!
   IF ( pla .AND. Tempid/=0 ) THEN
      Buf(1) = Tempid
      GOTO 12300
   ELSE
      IF ( .NOT.pla .AND. Tempid==0 ) GOTO 2900
      CALL preloc(*2900,Bfr,mpt)
      IF ( Tempid==0 ) THEN
         nx = 3
         CALL locate(*1700,Bfr,mats1,flag)
         qmatx = 1
         ASSIGN 1700 TO back
         ASSIGN 11200 TO ret1
         ASSIGN 8000 TO pass
         n = kmat1
!
!     INTERNAL ROUTINE TO READ MATXI CARDS, MERGE DATA IN MATI TABLE
!     AND STORE TABLE IDS IN CORE.
!
         ASSIGN 8700 TO ret
         GOTO 8600
      ENDIF
   ENDIF
 1700 nx = 2
   CALL locate(*1800,Bfr,matt1,flag)
   qmatx = 1
   ASSIGN 1800 TO back
   ASSIGN 11400 TO ret1
   ASSIGN 8000 TO pass
   n = kmat1
   ASSIGN 8700 TO ret
   GOTO 8600
 1800 CALL locate(*1900,Bfr,matt2,flag)
   qmatx = 1
   ASSIGN 1900 TO back
   ASSIGN 11500 TO ret1
   ASSIGN 8100 TO pass
   n = kmat2
   ASSIGN 8700 TO ret
   GOTO 8600
 1900 CALL locate(*2000,Bfr,matt3,flag)
   qmatx = 1
   ASSIGN 2000 TO back
   ASSIGN 12100 TO ret1
   ASSIGN 8200 TO pass
   n = kmat3
   ASSIGN 8700 TO ret
   GOTO 8600
 2000 CALL locate(*2100,Bfr,mttpz1,flag)
   qmatx = 1
   ASSIGN 2100 TO back
   ASSIGN 12400 TO ret1
   ASSIGN 8300 TO pass
   n = kmtpz1
   ASSIGN 8700 TO ret
   GOTO 8600
 2100 CALL locate(*2200,Bfr,mttpz2,flag)
   qmatx = 1
   ASSIGN 2200 TO back
   ASSIGN 12500 TO ret1
   ASSIGN 8400 TO pass
   n = kmtpz2
   ASSIGN 8700 TO ret
   GOTO 8600
 2200 CALL locate(*2300,Bfr,matt6,flag)
   qmatx = 1
   ASSIGN 2300 TO back
   ASSIGN 12800 TO ret1
   ASSIGN 8500 TO pass
   n = 31
   ASSIGN 8700 TO ret
   GOTO 8600
 2300 itabl = i
   imhere = 190
   IF ( i>n1mat ) GOTO 11000
   nlist = itabl - 11
   CALL close(mpt,Clsrew)
!
!     IF ANY MATTI OR MATSI CARDS WERE READ, FORM A SORTED LIST OF TABLE
!     NUMBERS REFERENCED ON THESE CARDS. THEN, DISCARD ANY DUPLICATES IN
!     THE LIST SO THAT THE LIST CONTAINS UNIQUE TABLE NOS. TO BE READ.
!
   IF ( qmatx==0 ) GOTO 2900
   DO ii = ilist , nlist , 11
      min = 999999999
      DO jj = ii , nlist , 11
         IF ( Z(jj)<min ) THEN
            min = Z(jj)
            jx = jj
         ENDIF
      ENDDO
      Z(jx) = Z(ii)
      Z(ii) = min
   ENDDO
   Z(itabl) = 0
   jj = ilist
   DO ii = ilist , nlist , 11
      IF ( Z(ii+11)/=Z(ii) ) THEN
         Z(jj) = Z(ii)
         jj = jj + 11
      ENDIF
   ENDDO
   itabl = jj
   nlist = jj - 11
!
!     READ THE DIT BY TABLE TYPE. FOR EACH TABLE IN THE DIT, LOOK UP IN
!     TABLE NO. LIST TO DETERMINE IF THE TABLE IS REQUIRED FOR PROBLEM
!     SOLUTION. IF NOT, SKIP THE TABLE. IF SO, READ THE TABLE INTO CORE
!     AND STORE POINTERS TO THE FIRST AND LAST ENTRIES IN THE TABLE AND
!     THE TYPE OF TABLE. THIS INFORMATION IS STORED IN THE TABLE NO.
!     LIST
!
   CALL preloc(*10700,Bfr,dit)
   i = itabl
   j = 1
   ASSIGN 2600 TO ret
   ASSIGN 2700 TO ret1
 2400 jj = j + j - 1
   CALL locate(*2800,Bfr,tablei(jj),flag)
 2500 CALL read(*10800,*2800,dit,Buf,8,0,flag)
   nwds = 2
   IF ( j==4 .OR. j==8 ) nwds = 1
   tablid = Buf(1)
   GOTO 8900
 2600 Z(l+1) = j
   IF ( j>4 ) Z(l+1) = j - 4
   Z(l+2) = i
   imhere = 270
   DO
      CALL read(*10800,*10900,dit,Z(i),nwds,0,flag)
      IF ( Z(i)==-1 ) THEN
         Z(l+3) = i - nwds
!
!     STORE THE PARAMETERS ON THE TABLEI CARD IN LOCATIONS
!     Z(L+4),Z(L+5),...,Z(L+10)
!
         DO k = 2 , 8
            lx = l + k
            Z(lx+2) = Buf(k)
         ENDDO
!
!     IF THIS TABLE IS A POLYNOMIAL (TABLE4), EVALUATE THE END POINTS
!     AND STORE AT ZZ(L+8) AND ZZ(L+9)
!
         IF ( j/=4 ) GOTO 2500
         xx = (Zz(l+6)-Zz(l+4))/Zz(l+5)
         ASSIGN 10400 TO igoto
         GOTO 10200
      ELSE
         i = i + nwds
         IF ( i>n1mat ) GOTO 11000
      ENDIF
   ENDDO
 2700 DO
      CALL read(*10800,*10900,dit,Buf,nwds,0,flag)
      IF ( Buf(1)==-1 ) GOTO 2500
   ENDDO
 2800 j = j + 1
   IF ( j<=ntypes ) GOTO 2400
   CALL close(dit,Clsrew)
!
!     TEST TO FOR ALL REFERENCED TABLES IN CORE
!
   flag = 0
   DO l = ilist , nlist , 11
      IF ( Z(l+1)==0 ) THEN
         flag = 1
         Buf(1) = Z(l)
         Buf(2) = 0
         CALL mesage(30,41,Buf)
      ENDIF
   ENDDO
   IF ( flag/=0 ) CALL mesage(-37,0,nam)
!
!     WRAP UP PREMAT
!
 2900 N2mat = i + 1 - offset
   matido = 0
   sintho = 2.0
   costho = 2.0
   inflgo = 0
   part1 = .FALSE.
   mapck = +999
   RETURN
!
!     THE FOLLOWING POINTERS AND FLAGS ARE SET IN PREMAT FOR USE BY MAT-
!     QMAT1 = 0, NO MAT1 TABLE, = 1, MAT1 TABLE PRESENT
!     IMAT1 = POINTER TO FIRST ENTRY IN MAT1 TABLE
!     LMAT1 = LENGTH  OF EACH  ENTRY IN MAT1 TABLE
!     NAMT1 = POINTER TO LAST  ENTRY IN MAT1 TABLE
!     QMAT2,  IMAT2, LMAT2 AND NMAT2 ARE DEFINED AS ABOVE FOR MAT2 TABLE
!     QMATX = 0, NO TEMP OR STRESS TABLES PRESENT, = 1, OTHERWISW
!     ILIST = POINTER TO FIRST ENTRY IN TABLE LIST
!     NLIST = POINTER TO  LAST ENTRY IN TABLE LIST
!
!     THE TABLE LIST HAS 11 WORDS PER ENTRY AS FOLLOWS--
!      1. TABLE NUMBER (I.E. ID NO.)
!      2. TABLE TYPE (I.E. 1 = TABLE1, 2 = TABLE2 ETC.)
!      3. POINTER TO FIRST ENTRY IN TABLE
!      4. POINTER TO  LAST ENTRY IN TABLE
!      5. THRU 11. PARAMETERS ON TABLEI CARD
!     MATIDO = OLD MATERIAL ID (INITIALIZED TO  0 BY PREMAT)
!     SINTHO = OLD SIN THETA   (INITIALIZED TO 2. BY PREMAT)
!     INFLGO = OLD INFLAG      (INITIALIZED TO  0 BY PREMAT)
!
!
!
   ENTRY mat(Elemid)
!     ==================
!
!     IF MAPCK .NE. +999 PREMAT HAS BEEN CORRUPTED.  (OVERLAY ERROR)
!
   IF ( mapck/=+999 ) THEN
      WRITE (Nout,99001) mapck
99001 FORMAT (//,' *** PREMAT OVERLEY ERROR',I12)
      CALL errtrc('PREMAT  ',353)
   ENDIF
!
!
!     INFLAG DESCRIBES PROCESSING REQUESTED BY CALLER
!
   IF ( Inflag==2 ) THEN
!
!     INFLAG = 2 MEANS CALLER WANTS MAT2 FORMAT WHETHER PROPERTIES ARE
!     DEFINED IN MAT1 OR MAT2 TABLE.
!     IF NO TEMPERATURE DEPENDENT PROPERTIES AND MATID = OLD MATID AND
!     SIN THETA = OLD SIN THETA, RETURN
!
      IF ( Tempid==0 .AND. Matid==matido .AND. Sinth==sintho .AND. .NOT.pla .AND. Inflag==inflgo .AND. Costh==costho ) RETURN
      inflgo = Inflag
      matido = Matid
      sintho = Sinth
      costho = Costh
      GOTO 3200
   ELSEIF ( Inflag==3 ) THEN
!
!     INFLAG = 3 IMPLIES THE CALLER WANTS
!             (1) ONLY J11, J12 AND J22, AND
!             (2) THE FIRST 15 LOCATIONS OF /MATOUT/ TO BE UNDISTURBED.
!
      IF ( Matid==matido .AND. Inflag==inflgo .AND. .NOT.pla ) RETURN
      IF ( Matid/=matido ) GOTO 3900
      IF ( inflgo/=2 ) GOTO 3900
      IF ( mmat>=2 ) GOTO 4300
      J11 = gx
      J12 = 0.0
      J22 = gx
      GOTO 4400
   ELSEIF ( Inflag==4 ) THEN
!
!     INFLAG = 4 MEANS CALLER DESIRES ONLY THE DENSITY PROPERTY (RHO)
!     LOOK UP MATID IN MAT1 TABLE.
!
      IF ( Tempid==0 .AND. Matid==matido .AND. Inflag==inflgo .AND. .NOT.pla ) RETURN
      ASSIGN 4600 TO ret
      ASSIGN 4500 TO ret1
      GOTO 8000
   ELSEIF ( Inflag==5 ) THEN
!
!     INFLAG = 5, USED ONLY IN MODULE PLA1, DETERMINES IF THE MAT CARD
!     REFERENCED IS A MAT1 WITH E, YOUNGS MODULUS, DEFINED AS STRESS
!     DEPENDENT.  IF IT IS STRESS DEPENDENT, INDSTR, THE FIRST WORD OF
!     THE /MATOUT/ BLOCK IS SET = 1.  IF NOT STRESS DEPENDENT, INDSTR
!     IS SET = 0 ONLY MAT1 CARDS ARE ADMISSIBLE FOR THIS TEST.
!
      IF ( pla .AND. Matid==matido .AND. Inflag==inflgo ) RETURN
      matido = Matid
      inflgo = Inflag
      ASSIGN 5000 TO ret
      ASSIGN 5100 TO ret1
      Indstr = 0
      GOTO 8000
   ELSEIF ( Inflag==6 ) THEN
!
!     INFLAG = 6, USED ONLY IN SUBROUTINES PLA3 AND PLA4, ACCEPTS
!     EPSILON - STRAIN - IN THE /MATIN/ BLOCK (PLAARG) AND LOOKS-UP
!     SIGMA   - STRESS - AND STORES THIS VALUE IN PLAANS IN /MATOUT/.
!     ONLY MAT1 AND MATS1 CARDS ARE ADMISSIBLE FOR THIS INFLAG.
!
      ASSIGN 5200 TO ret
      ASSIGN 11900 TO ret1
      matido = Matid
      inflgo = Inflag
      GOTO 8000
   ELSEIF ( Inflag==7 ) THEN
!
!     INFLAG = 7, USED CURRENTLY ONLY BY BELL AEROSYSTEMS ELEMENTS,
!     IMPLIES THE USER WANTS HIS DATA IN MAT3 FORMAT.  IF THE MATID IS
!     FOUND IN THE MAT1 SET, THE DATA IS STORED IN MAT3 FORMAT.  IF NOT
!     FOUND IN THE MAT1 SET, THE MAT3 SET IS SEARCHED. IF NOT FOUND IN
!     THE MAT3 SET THE MAT2 SET IS SEARCHED. IF NOT FOUND HERE, A FATAL
!     ERROR EXISTS.
!
      IF ( Tempid==0 .AND. Inflag==inflgo .AND. Matid==matido ) RETURN
      inflgo = Inflag
      matido = Matid
      ASSIGN 5600 TO ret
      GOTO 5500
   ELSEIF ( Inflag==8 ) THEN
!
!     INFLAG = 8 IS USED ONLY BY TWO-DIMENSIONAL ELEMENTS IN PIECEWISE
!     LINEAR ANALYSIS.  HERE WE PERFORM AN INVERSE INTERPOLATION TO
!     OBTAIN STRAIN (EPS) GIVEN STRESS (TAU)
!
      ASSIGN 6200 TO ret
      ASSIGN 11900 TO ret1
      matido = Matid
      inflgo = Inflag
      yy = Plaarg
      GOTO 8000
   ELSEIF ( Inflag==9 ) THEN
!
!     INFLAG = 9 IS USED ONLY BY TRAPAX AND TRIAAX WHEN PIEZOELECTRIC
!     MATERIALS ARE SELECTED.  WANT MATERIALS RETURNED INTO MATPZ2
!     FORMAT.
!
!     MATPZ1 CODE TRANSFORMS 1,2,3 MATERIAL DIRECTIONS INTO Z, THETA,
!     R = 0 DIRECTIONS, RESPECTIVLELY, AND INTERCHANGES 4TH AND 6TH ROWS
!     AND COLUMNS TO ACCOUNT FOR DIFFERENT SHEAR ORDERING.
!     ELEMENT ROUTINE WILL TRANSFORM FOR R-POLARIZATION
!     MATPZ2 CODE ASSUMES USER HAS PERFO-MED ALL TRANSFORMATIONS AS
!     EXPLAINED FOR MATPZ1
!
      IF ( Tempid==0 .AND. Inflag==inflgo .AND. Matid==matido ) RETURN
      inflgo = Inflag
      matido = Matid
!
!     LOOK UP MATID IN MATPZ1 TABLE
!
      ASSIGN 6700 TO ret1
      ASSIGN 6800 TO ret
      GOTO 8300
   ELSEIF ( Inflag==10 ) THEN
!
!     INFLAG = 10, USED CURRENTLY ONLY BY ISOPARAMETRIC SOLIDS IHEX1,2,3
!     IMPLIES CALLER WANTS HIS DATA IN MAT6 FORMAT STORED IN MATISO.
!     MATERIALS COULD BE ON MAT1 OR ON MAT6. IN EITHER CASE,MATERIALS
!     WILL BE COMPUTED FOR MAT6 OUTPUT. IF NOT FOUND ON MAT1 OR MAT6,
!     FATAL.
!
      IF ( Tempid==0 .AND. Matid==matido .AND. Inflag==inflgo ) RETURN
      inflgo = Inflag
      matido = Matid
      Tdep = .FALSE.
!
!     LOOK UP MATID IN MAT1 TABLE
!
      ASSIGN 7200 TO ret1
      ASSIGN 7300 TO ret
      GOTO 8000
   ELSEIF ( Inflag==11 ) THEN
!
!     INFLAG = 11 IS USED ONLY BY A HYDROELASTIC ANALYSIS TO FIND THE
!     DENSITY FOR THREE DIMENSIONAL FLUID ELEMENTS FROM MATF CARDS.
!
      IF ( qmatf/=0 ) THEN
         DO k = imatf , nmatf , lmatf
            IF ( Z(k)==Matid ) GOTO 7700
         ENDDO
      ENDIF
      GOTO 11700
   ELSEIF ( Inflag==12 ) THEN
!
!     INFLAG = 12 IS USED ONLY BY SHELL ELEMENTS QUAD4 AND TRIA3.
!     MAT1 IS FIRST SEARCHED, IF NOT FOUND, MAT2 IS SEARCHED. IF FOUND
!     IN EITHER CASE, /MATOUT/ WILL BE FILLED WITH MAT2 FORMAT DATA.
!     IF NOT FOUND IN MAT1 OR MAT2, MAT8 IS SEARCHED AND MAT8 FORMAT IS
!     USED IN /MATOUT/. FATAL ERROR IF MAT8 IS NOT FOUND.
!
      IF ( Tempid==0 .AND. Matid==matido .AND. Inflag==inflgo .AND. Sinth==sintho .AND. Costh==costho .AND. .NOT.pla ) RETURN
      inflgo = Inflag
      matido = Matid
      sintho = Sinth
      costho = Costh
!
!     GO TO INFLAG = 2 CODE TO PICK UP MAT1 OR MAT2 PROPERTIES
!     SET MATSET TO 1.0 IF PROPERTY DATA COMES FROM MAT1, OR
!     TO 2.0 IF FROM MAT2
!
      GOTO 3200
   ELSE
!
!     INFLAG = 1 MEANS CALLER WANTS ONLY MAT1 PROPERTIES IN MAT1 FORMAT
!     IF NO TEMPERATURE DEPENDENT PROPERTIES AND MATID = OLD MATID,
!     RETURN SINCE DATA IS ALREADY IN MATOUT
!
      IF ( Tempid==0 .AND. Matid==matido .AND. Inflag==inflgo .AND. .NOT.pla ) RETURN
      matido = Matid
      inflgo = Inflag
      Tdep = .FALSE.
!
!     LOOK UP MATID IN MAT1 TABLE
!
      ASSIGN 3000 TO ret
      ASSIGN 11700 TO ret1
      GOTO 8000
   ENDIF
!
!     PICK UP MATERIAL PROPERTIES FROM MAT1 ENTRY.
!
 3000 i = k + 1
   j = 1
   ASSIGN 3100 TO back
   GOTO 9000
 3100 Y(j) = prop
   i = i + 3
   j = j + 1
   IF ( j<kmat1 ) GOTO 9000
   RETURN
!
!     LOOK UP MATID IN MAT1 TABLE
!
 3200 ASSIGN 3300 TO ret1
   ASSIGN 3500 TO ret
   GOTO 8000
!
!     MATID NOT IN MAT1 TABLE, LOOK UP IN MAT2 TABLE
!     - IF NOT PRESENT, FATAL ERROR IF INFLAG = 2
!     - IF NOT PRESENT, SEARCH MAT8 TABLE IF INFLAG = 12
!
 3300 ASSIGN 3700 TO ret
   ASSIGN 3400 TO ret1
   GOTO 8100
 3400 IF ( inflgo==12 ) THEN
!
!     NOT FOUND IN MAT1 AND MAT2.  LOOK FOR MAT8, ERROR IF NOT FOUND
!
      IF ( qmat8/=0 ) THEN
         DO k = imat8 , nmat8 , lmat8
            IF ( Z(k)==Matid ) GOTO 7800
         ENDDO
      ENDIF
   ENDIF
   GOTO 11700
!
!     MATID FOUND IN MAT1 TABLE.
!     COMPUTE G MATRIX FROM MAT1 PROPERTIES.
!     COMPLETE REMAINDER OF OUTPUT BUFFER IN MAT2 FORMAT.
!
 3500 i = k + 1
   j = 1
   ASSIGN 3600 TO back
   mmat = 1
   GOTO 9000
 3600 x(j) = prop
   i = i + 3
   j = j + 1
   IF ( j<kmat1 ) GOTO 9000
   nuxx = 1.0 - nux**2
   G11 = ex/nuxx
   G12 = nux*G11
   G13 = 0.
   G22 = G11
   G23 = 0.
   G33 = gx
   Rhoy = rhox
   Alph1 = alphx
   Alph2 = alphx
   Alph12 = 0.
   Toy = tox
   Gey = gex
   Sigty = sigtx
   Sigcy = sigcx
   Sigsy = sigsx
   IF ( inflgo==12 ) THEN
!
      Matset = 1.0
      Y(16) = ex
      Y(17) = ex
      RETURN
   ELSE
      RETURN
   ENDIF
!
!     MATID FOUND IN MAT2 TABLE.
!     PLACE PROPERTIES IN OUTPUT AREA IN MAT2 FORMAT
!     THEN TEST FOR TRANSFORMATION. IF IDENTITY, RETURN. 3THERWISE,
!     PERFORM  U(T)*G*U .
!
 3700 i = k + 1
   j = 1
   mmat = 2
   ASSIGN 3800 TO back
   GOTO 9000
 3800 Y(j) = prop
   i = i + 3
   j = j + 1
   IF ( j<kmat2 ) GOTO 9000
   IF ( inflgo==12 ) THEN
      Matset = 2.0
      RETURN
   ELSE
      IF ( Sinth/=0.0 ) THEN
         IF ( abs(Sinth**2+Costh**2-1.0)>.0001 ) THEN
            n = 103
            Buf(1) = 0
            Buf(2) = 0
            GOTO 11600
         ELSE
!
!     TRANSFORM G , THE MATERIAL STIFFNESS PROPERTY MATRIX.
!                M                   T
!                            G   =  U * G * U
!                             E          M
!
            x(1) = Costh**2
            x(2) = Sinth**2
            x(3) = Costh*Sinth
            x(4) = x(2)
            x(5) = x(1)
            x(6) = -x(3)
            x(7) = 2.0*x(6)
            x(8) = -x(7)
            x(9) = x(1) - x(2)
            x(10) = G11
            x(11) = G12
            x(12) = G13
            x(13) = G12
            x(14) = G22
            x(15) = G23
            x(16) = G13
            x(17) = G23
            x(18) = G33
            CALL gmmats(x(10),3,3,0,x(1),3,3,0,x(19))
            CALL gmmats(x(1),3,3,1,x(19),3,3,0,x(10))
            G11 = x(10)
            G12 = x(11)
            G13 = x(12)
            G22 = x(14)
            G23 = x(15)
            G33 = x(18)
!
!     COMPUTE THE TRANSFORMED TEMPERATURE EXPANSION VECTOR
!               (ALPHA) = (U)*(ALPHA)
!                                    M
!
            x(3) = -x(3)
            x(6) = -x(6)
            x(7) = -x(7)
            x(8) = -x(8)
            CALL gmmats(x(1),3,3,0,Y(8),3,1,0,x(10))
            Alph1 = x(10)
            Alph2 = x(11)
            Alph12 = x(12)
         ENDIF
      ENDIF
      IF ( Inflag==7 ) THEN
         Sigty3 = Sigty
         Sigcy3 = Sigcy
         Sigsy3 = Sigsy
         Tref3 = Toy
         Ge3 = Gey
         Ax3 = Alph1
         Ay3 = Alph2
         Az3 = Alph12
         G113 = G11
         G123 = G12
         G133 = G13
         G223 = G22
         G233 = G23
         G333 = G33
         Matset = 2.0
         RETURN
      ELSE
         RETURN
      ENDIF
   ENDIF
!
!     SEARCH MAT1 TABLE FOR MATID
!
 3900 ASSIGN 4000 TO ret1
   ASSIGN 4100 TO ret
   GOTO 8000
!
!     MATID NOT IN MAT1 TABLE. LOOK IN MAT2 TABLE - ERROR IF NOT PRESENT
!
 4000 ASSIGN 11700 TO ret1
   ASSIGN 4300 TO ret
   GOTO 8100
 4100 i = k + 4
   ASSIGN 4200 TO back
   GOTO 9000
 4200 J11 = prop
   J12 = 0.0
   J22 = prop
   GOTO 4400
 4300 J11 = 0.0
   J12 = 0.0
   J22 = 0.0
 4400 inflgo = Inflag
   matido = Matid
   RETURN
!
!     MATID NOT IN MAT1 TABLE, LOOK UP IN MAT2 TABLE - ERROR IF NOT
!     PRESENT
!
 4500 ASSIGN 4900 TO ret
   ASSIGN 11700 TO ret1
   GOTO 8100
!
!     MATID FOUND IN MAT1 TABLE. PICK UP RHO
!
 4600 i = k + 10
 4700 ASSIGN 4800 TO back
   GOTO 9000
 4800 Y(1) = prop
   matido = Matid
   inflgo = Inflag
   RETURN
!
!     MATID FOUND IN MAT2 TABLE. PICK UP RHO.
!
 4900 i = k + 19
   GOTO 4700
!
!     TEST TO SEE IF THE MATERIAL PROPERTY E IS DEPENDENT ON A TABLE OF
!     STRAIN VS. STRESS (EPSILON VS. SIGMA)
!
 5000 tablid = Z(k+3)
   IF ( tablid/=0 ) Indstr = 1
 5100 RETURN
 5200 tablid = Z(k+3)
   IF ( tablid<=0 ) GOTO 12000
   xx = Plaarg
   ASSIGN 5300 TO ret
   ASSIGN 11800 TO ret1
   GOTO 8900
 5300 itype = Z(l+1)
   IF ( itype/=1 ) GOTO 12200
   ASSIGN 5400 TO iret
   GOTO 9700
 5400 Plaans = prop
   RETURN
 5500 ASSIGN 5800 TO ret1
   GOTO 8000
 5600 i = k + 1
   j = 1
   ASSIGN 5700 TO back
   GOTO 9000
 5700 IF ( j==2 ) THEN
      Gxy3 = prop
      Gyz3 = prop
      Gzx3 = prop
   ELSEIF ( j==3 ) THEN
      Nuxy3 = prop
      Nuyz3 = prop
      Nuzx3 = prop
   ELSEIF ( j==4 ) THEN
      Rho3 = prop
   ELSEIF ( j==5 ) THEN
      Ax3 = prop
      Ay3 = prop
      Az3 = prop
   ELSEIF ( j==6 ) THEN
      Tref3 = prop
   ELSEIF ( j==7 ) THEN
      Ge3 = prop
   ELSEIF ( j==8 ) THEN
      Sigty3 = prop
   ELSEIF ( j==9 ) THEN
      Sigcy3 = prop
   ELSEIF ( j==10 ) THEN
      Sigsy3 = prop
      Matset = 1.0
      RETURN
   ELSE
      Ex3 = prop
      Ey3 = prop
      Ez3 = prop
   ENDIF
!
   j = j + 1
   i = i + 3
   GOTO 9000
!
!     SEARCH FOR MATID IN THE MAT3 SET
!
 5800 ASSIGN 5900 TO ret
   ASSIGN 6100 TO ret1
   GOTO 8200
!
!     PICK UP MATERIAL PROPERTIES FROM MAT3 ENTRY
!
 5900 i = k + 1
   j = 1
   ASSIGN 6000 TO back
   GOTO 9000
 6000 Y(j) = prop
   i = i + 3
   j = j + 1
   IF ( j<kmat3 ) GOTO 9000
   Matset = 3.0
   RETURN
!
!     SEARCH FOR MATID IN THE MAT2 SET
!
 6100 ASSIGN 3700 TO ret
   ASSIGN 11700 TO ret1
!
!     GO TO INFLAG = 2 CODE TO PICK UP MAT2 PROPERTIES
!
   GOTO 8100
 6200 tablid = Z(k+3)
   IF ( tablid<=0 ) GOTO 12000
   ASSIGN 6300 TO ret
   ASSIGN 11800 TO ret1
   GOTO 8900
 6300 itype = Z(l+1)
   IF ( itype/=1 ) GOTO 12200
!
!     ROUTINE TO PERFORM INVERSE LINEAR INTERPOLATION OR EXTRAPOLATION.
!
   itabl = Z(l+2)
   ntabl = Z(l+3)
   up = 1.0
   IF ( Zz(itabl)>Zz(itabl+2) ) up = -1.0
   kxx1 = itabl
   IF ( (yy-Zz(itabl+1))*up<0.0 ) GOTO 6600
   kxx1 = ntabl - 2
   IF ( (yy-Zz(ntabl+1))*up>0.0 ) THEN
      IF ( Zz(ntabl+1)==Zz(ntabl-1) ) GOTO 6600
   ENDIF
   klo = 1
   khi = (ntabl-itabl)/2 + 1
   DO
      kx = (klo+khi+1)/2
      kxx = (kx-1)*2 + itabl
      IF ( (yy-Zz(kxx+1))*up<0 ) THEN
         khi = kx
      ELSEIF ( (yy-Zz(kxx+1))*up==0 ) THEN
         GOTO 6500
      ELSE
         klo = kx
      ENDIF
      IF ( khi-klo==1 ) THEN
         kxx1 = 2*(klo-1) + itabl
         IF ( kxx==kxx1 ) THEN
            Plaans = (yy-Zz(kxx1+1))*(Zz(kxx1+2)-Zz(kxx1))/(Zz(kxx1+3)-Zz(kxx1+1)) + Zz(kxx1)
            EXIT
         ELSEIF ( yy==Zz(kxx1+3) ) THEN
!
            kxx = kxx1 + 2
            GOTO 6500
         ELSE
            Plaans = (yy-Zz(kxx1+1))*(Zz(kxx1+2)-Zz(kxx1))/(Zz(kxx1+3)-Zz(kxx1+1)) + Zz(kxx1)
            EXIT
         ENDIF
      ENDIF
   ENDDO
 6400 Icell2 = 0
   RETURN
 6500 IF ( yy==Zz(kxx-1) ) THEN
      Plaans = (Zz(kxx)+Zz(kxx-2))/2.0
   ELSEIF ( yy==Zz(kxx+3) ) THEN
      Plaans = (Zz(kxx)+Zz(kxx+2))/2.0
   ELSE
      Plaans = Zz(kxx)
   ENDIF
   GOTO 6400
!
!     YY IS OUT OF THE RANGE OF THE FUNCTION, SET THE SECOND CELL OF
!     /MATOUT/ EQUAL TO ONE.
!
 6600 Plaans = 0.0
   Icell2 = 1
   RETURN
!
!     NOT IN MATPZ1, LOOK AT MATPZ2
!
 6700 ASSIGN 7000 TO ret
   ASSIGN 5500 TO ret1
   GOTO 8400
!
!     FOUND IN MATPZ1 - PUT OUT LIKE MATPZ2
!
 6800 i = k + 1
   j = 1
   ASSIGN 6900 TO back
   GOTO 9000
 6900 bufpz(j) = prop
   i = i + 3
   j = j + 1
   IF ( j<kmtpz1 ) GOTO 9000
   epso = 8.854E-12
   DO ijk = 1 , 8
      bufpz(ijk) = bufpz(ijk)*1.E-12
   ENDDO
   se1 = (bufpz(4)-bufpz(1))*2.*bufpz(5)**2 - bufpz(2)*(bufpz(4)**2-bufpz(1)**2)
   se2 = 2.*bufpz(5)**2 - bufpz(2)*(bufpz(4)+bufpz(1))
   IF ( se1==0. .OR. se2==0. ) THEN
      n = 214
      GOTO 12700
   ELSE
      ce11 = -(bufpz(5)**2-bufpz(1)*bufpz(2))/se1
      ce12 = (bufpz(5)**2-bufpz(2)*bufpz(4))/se1
      ce13 = bufpz(5)/se2
      ce33 = -(bufpz(4)+bufpz(1))/se2
      ce44 = 1./bufpz(3)
      ce66 = 0.5/(bufpz(1)-bufpz(4))
      e15 = bufpz(8)*ce44
      e31 = bufpz(6)*(ce11+ce12) + bufpz(7)*ce13
      e33 = bufpz(6)*ce13*2. + bufpz(7)*ce33
      eps11 = bufpz(9)*epso
      eps33 = bufpz(10)*epso
      DO ijk = 4 , 44
         Pzout(ijk) = 0.
      ENDDO
      Pzout(1) = ce33
      Pzout(2) = ce13
      Pzout(3) = ce13
      Pzout(7) = ce11
      Pzout(8) = ce12
      Pzout(12) = ce11
      Pzout(16) = ce44
      Pzout(19) = ce44
      Pzout(21) = ce66
      Pzout(22) = e33
      Pzout(23) = e31
      Pzout(24) = e31
      Pzout(31) = e15
      Pzout(38) = e15
      Pzout(40) = eps33
      Pzout(43) = eps11
      Pzout(45) = eps11
      Pzout(46) = bufpz(11)
      Pzout(47) = bufpz(12)
      Pzout(48) = bufpz(12)
      Pzout(49) = bufpz(12)
      Pzout(50) = bufpz(13)
      Pzout(51) = bufpz(14)
      Matset = 4.0
      RETURN
   ENDIF
!
!     FOUND IN MATPZ2 FORMAT
!
 7000 i = k + 1
   j = 1
   ASSIGN 7100 TO back
   GOTO 9000
 7100 Pzout(j) = prop
   i = i + 3
   j = j + 1
   IF ( j<kmtpz2 ) GOTO 9000
   Matset = 5.0
   RETURN
!
!     MATID NOT IN MAT1. CHECK MAT6
!
 7200 ASSIGN 7500 TO ret
   ASSIGN 11700 TO ret1
   GOTO 8500
!
!     MATID FOUND IN MAT1 TABLE. COMPUTE G MATRIX,ETC.
!
 7300 Ib(46) = 1
   i = k + 1
   j = 1
   ASSIGN 7400 TO back
   GOTO 9000
 7400 x(j) = prop
   i = i + 3
   j = j + 1
   IF ( j<kmat1 ) GOTO 9000
   dd = (1.+nux)*(1.-2.*nux)
   IF ( dd/=0. ) THEN
!
      dd = ex*(1.-nux)/dd
      ddn1 = nux/(1.-nux)
      ddn2 = 0.5*(1.-2.*nux)/(1.-nux)
      DO ijkl = 1 , 45
         Bufm6(ijkl) = 0.
      ENDDO
      Bufm6(1) = dd
      Bufm6(2) = dd*ddn1
      Bufm6(3) = Bufm6(2)
      Bufm6(7) = Bufm6(2)
      Bufm6(8) = Bufm6(1)
      Bufm6(9) = Bufm6(2)
      Bufm6(13) = Bufm6(2)
      Bufm6(14) = Bufm6(2)
      Bufm6(15) = Bufm6(1)
      Bufm6(22) = dd*ddn2
      Bufm6(29) = Bufm6(22)
      Bufm6(36) = Bufm6(22)
      Bufm6(37) = rhox
      Bufm6(38) = alphx
      Bufm6(39) = alphx
      Bufm6(40) = alphx
      Bufm6(44) = tox
      Bufm6(45) = gex
      RETURN
   ELSE
      Ib(46) = 0
      RETURN
   ENDIF
!
!     MATID FOUND IN MAT6 TABLE. PUT PROPERTIES IN MAT6 FORMAT AND
!     TRANSFORM USING DIRECTION COSINES
!
 7500 Ib(46) = 6
   i = k + 1
   j = 1
   ASSIGN 7600 TO back
   GOTO 9000
 7600 buftm6(j) = prop
   i = i + 3
   j = j + 1
   IF ( j<kmat6 ) GOTO 9000
!
!     PUT SYMMETRIC PORTION OF G INTO A FULL 6 X 6 AND CREATE A 6 X 6
!     DIRECTION COSINE MATRIX BY COOK PP. 212-213. THEN TRANSFORM
!     (U-TRANSPOSE)*G*U
!
   kkk = 0
   lll = 0
   DO iii = 1 , 6
      DO jjj = iii , 6
         kkk = kkk + 1
         lll = lll + 1
         xy(lll) = buftm6(kkk)
         IF ( jjj/=iii ) THEN
            l5 = 5*(jjj-iii)
            isub = lll + l5
            xy(isub) = xy(lll)
         ENDIF
      ENDDO
      lll = lll + iii
   ENDDO
   xl1 = buftm6(31)
   xm1 = buftm6(32)
   xn1 = buftm6(33)
   xl2 = buftm6(34)
   xm2 = buftm6(35)
   xn2 = buftm6(36)
   xl3 = buftm6(37)
   xm3 = buftm6(38)
   xn3 = buftm6(39)
   xy(37) = xl1**2
   xy(38) = xm1**2
   xy(39) = xn1**2
   xy(40) = xl1*xm1
   xy(41) = xm1*xn1
   xy(42) = xn1*xl1
   xy(43) = xl2**2
   xy(44) = xm2**2
   xy(45) = xn2**2
   xy(46) = xl2*xm2
   xy(47) = xm2*xn2
   xy(48) = xn2*xl2
   xy(49) = xl3**2
   xy(50) = xm3**2
   xy(51) = xn3**2
   xy(52) = xl3*xm3
   xy(53) = xm3*xn3
   xy(54) = xn3*xl3
   xy(55) = xl1*xl2*2.
   xy(56) = xm1*xm2*2.
   xy(57) = xn1*xn2*2.
   xy(58) = xl1*xm2 + xl2*xm1
   xy(59) = xm1*xn2 + xm2*xn1
   xy(60) = xn1*xl2 + xn2*xl1
   xy(61) = xl2*xl3*2.
   xy(62) = xm2*xm3*2.
   xy(63) = xn2*xn3*2.
   xy(64) = xl2*xm3 + xl3*xm2
   xy(65) = xm2*xn3 + xm3*xn2
   xy(66) = xn2*xl3 + xn3*xl2
   xy(67) = xl3*xl1*2.
   xy(68) = xm3*xm1*2.
   xy(69) = xn3*xn1*2.
   xy(70) = xl3*xm1 + xl1*xm3
   xy(71) = xm3*xn1 + xm1*xn3
   xy(72) = xn3*xl1 + xn1*xl3
!
   CALL gmmats(xy(1),6,6,0,xy(37),6,6,0,xy(73))
   CALL gmmats(xy(37),6,6,1,xy(73),6,6,0,Bufm6(1))
!
!     MUST ALSO TRANSFORM THERMAL EXPANSION VECOT= (U-INVERSE)*ALPHA
!     BY COOK P.212, THE INVERSE OF U IS THE TRANSPOSE OF THE
!     MATRIX WHICH TRANSFORMS STRESSES
!
   kkk = 72
   DO iii = 1 , 6
      DO jjj = 1 , 36 , 6
         kkk = kkk + 1
         lll = jjj + iii + 35
         xy(kkk) = xy(lll)
      ENDDO
   ENDDO
   DO iii = 75 , 87 , 6
      DO jjj = 1 , 3
         kkk = iii + jjj
         xy(kkk) = xy(kkk)*0.5
      ENDDO
   ENDDO
   DO iii = 90 , 102 , 6
      DO jjj = 1 , 3
         kkk = iii + jjj
         xy(kkk) = xy(kkk)*2.0
      ENDDO
   ENDDO
!
   CALL gmmats(xy(73),6,6,0,buftm6(23),6,1,0,Bufm6(38))
!
   Bufm6(37) = buftm6(22)
   Bufm6(44) = buftm6(29)
   Bufm6(45) = buftm6(30)
   RETURN
 7700 Rho = Zz(k+1)
   RETURN
 7800 i = k + 1
   j = 1
   ASSIGN 7900 TO back
   GOTO 9000
!
!     OUTPUT IN MAT8 FORMAT AND SET MATSET TO 8.0
!
 7900 x(j) = prop
   i = i + 3
   j = j + 1
   IF ( j<kmat8 ) GOTO 9000
   DO k = 1 , 17
      Y(k) = x(k)
   ENDDO
   Y(2) = x(3)
   Y(3) = x(2)
   Y(5) = x(6)
   Y(6) = x(5)
   Matset = 8.0
   RETURN
!
!
!     INTERNAL ROUTINE TO SEARCH FOR MATERIAL IN MAT1 TABLE
!
 8000 IF ( qmat1/=0 ) THEN
      DO k = imat1 , nmat1 , lmat1
         IF ( Z(k)==Matid ) GOTO ret
      ENDDO
   ENDIF
   GOTO ret1
!
!     INTERNAL ROUTINE TO SEARCH FOR MATERIAL IN MAT2 TABLE
!
 8100 IF ( qmat2/=0 ) THEN
      DO k = imat2 , nmat2 , lmat2
         IF ( Z(k)==Matid ) GOTO ret
      ENDDO
   ENDIF
   GOTO ret1
!
!     INTERNAL ROUTINE TO SEARCH FOR MATERIAL IN MAT3 TABLE.
!
 8200 IF ( qmat3/=0 ) THEN
      DO k = imat3 , nmat3 , lmat3
         IF ( Z(k)==Matid ) GOTO ret
      ENDDO
   ENDIF
   GOTO ret1
!
!     PIEZOELECTRIC MATERIALS
!
 8300 IF ( qmtpz1/=0 ) THEN
      DO k = imtpz1 , nmtpz1 , lmtpz1
         IF ( Z(k)==Matid ) GOTO ret
      ENDDO
   ENDIF
   GOTO ret1
 8400 IF ( qmtpz2/=0 ) THEN
      DO k = imtpz2 , nmtpz2 , lmtpz2
         IF ( Z(k)==Matid ) GOTO ret
      ENDDO
   ENDIF
   GOTO ret1
!
!     SEARCH FOR MATERIAL IN MAT6 TABLE(ISOPARAMETRIC SOLIDS)
!
 8500 IF ( qmat6/=0 ) THEN
      DO k = imat6 , nmat6 , lmat6
         IF ( Z(k)==Matid ) GOTO ret
      ENDDO
   ENDIF
   GOTO ret1
 8600 CALL read(*10600,*8800,mpt,Buf,n,0,flag)
   Matid = Buf(1)
   GOTO pass
 8700 DO j = 2 , n
      IF ( Buf(j)/=0 ) THEN
         jx = k + 3*(j-2) + nx
         Z(jx) = Buf(j)
         Z(i) = Buf(j)
         Z(i+1) = 0
         i = i + 11
      ENDIF
   ENDDO
   GOTO 8600
 8800 GOTO back
!
!     INTERNAL ROUTINE TO SEARCH FOR A TABLE IN THE TABLE LIST
!
 8900 DO l = ilist , nlist , 11
      IF ( Z(l)==tablid ) GOTO ret
   ENDDO
   GOTO ret1
!
!     ROUTINE TO TEST FOR DEPENDENCE OF A MATERIAL PROPERTY ON
!     TEMPERATURE OR STRESS. IF DEPENDENT, APPROPRIATE TABLE LOOK UP
!     PROCEDURE IS EMPLOYED. IN EITHER CASE, THE PROPERTY IS RETURNED
!     IN PROP.
!
 9000 IF ( qmatx==0 ) THEN
      prop = Zz(i)
      GOTO 9600
   ELSE
      flag = 0
      tablid = Z(i+1)
      IF ( Elemid<0 ) THEN
!
!     SINCE THIS IS NOT A PIECEWISE LINEAR ANALYSIS PROBLEM, NO STRESS
!     DEPENDENT MATERIAL PROPERTIES ARE ALLOWED.  IF AND WHEN THIS
!     RESTRICTION IS LIFTED THE FOLLOWING CODE CAN BE IMPLEMENTED.
!     CURRENTLY A TRANSFER IS ALWAYS MADE TO STATEMENT 1060, SINCE THE
!     ELEMENT ID. IS ALWAYS POSITIVE.
!
         IF ( pla ) THEN
            Buf(1) = Elemid
            GOTO 12300
         ELSEIF ( Elemid>0 ) THEN
            prop = Zz(i)
            GOTO 9600
         ELSE
            tablid = Z(i+2)
            IF ( tablid==0 ) THEN
               IF ( flag==0 ) prop = Zz(i)
               GOTO 9600
            ELSE
               ASSIGN 9400 TO ret
               ASSIGN 11800 TO ret1
               GOTO 8900
            ENDIF
         ENDIF
      ELSEIF ( tablid==0 ) THEN
         prop = Zz(i)
         GOTO 9600
      ELSE
         xx = Temp
         Tdep = .TRUE.
         flag = 1
         ASSIGN 9100 TO ret
         ASSIGN 11800 TO ret1
         GOTO 8900
      ENDIF
   ENDIF
 9100 ASSIGN 9300 TO ret
 9200 itype = Z(l+1)
   IF ( itype==1 ) THEN
!
!     TABLE TYPE = 1
!     ARGUMENT = XX
!
      ASSIGN 10000 TO iret
      GOTO 9700
   ELSEIF ( itype==2 ) THEN
!
!     TABLE TYPE = 2
!     ARGUMENT = (XX-X1)
!
      xx = xx - Zz(l+4)
      ASSIGN 10100 TO iret
      GOTO 9700
   ELSEIF ( itype==3 ) THEN
!
!     TABLE TYPE = 3
!     ARGUMENT = (XX-X1)/X2
!
      xx = (xx-Zz(l+4))/Zz(l+5)
      ASSIGN 10100 TO iret
      GOTO 9700
   ELSEIF ( itype==4 ) THEN
!
!     TABLE TYPE = 4
!     PERFORM POLYNOMIAL INTERPOLATION
!
!
!     NOTE...
!         ZZ(L+4) = X1
!         ZZ(L+5) = X2
!         ZZ(L+6) = X3
!         ZZ(L+7) = X4
!         ZZ(L+8) = F((X3-X1)/X2)
!         ZZ(L+9) = F((X4-X1)/X2)
!         WHERE X1 AND X2 ARE TRANSLATION AND SCALE FACTORS RESPECTIVELY
!         AND X3 AND X4 (X3 .LT. X4) ARE THE END POINTS OF THE
!         INTERVAL OVER WHICH THE POLYNOMIAL IS DEFINED.
!
      factor = Zz(i)
!
!     DETERMINE THE ARGUMENT XX
!
      xx = (xx-Zz(l+4))/Zz(l+5)
      IF ( xx<=(Zz(l+6)-Zz(l+4))/Zz(l+5) ) THEN
         prop = Zz(l+8)
         GOTO 10300
      ELSE
         IF ( xx<(Zz(l+7)-Zz(l+4))/Zz(l+5) ) GOTO 10200
         prop = Zz(l+9)
         GOTO 10300
      ENDIF
   ENDIF
 9300 propt = prop
   GOTO 9600
 9400 ASSIGN 9500 TO ret
   xx = Plaarg
   GOTO 9200
 9500 IF ( flag/=0 ) prop = prop*propt
 9600 GOTO back
!
!     ROUTINE TO PERFORM LINEAR INTERPOLATION FOR FUNCTION IN TABLE.
!     L POINTS TO THE ENTRY IN THE TABLE LIST WHICH DEFINES THE TABLE.
!     ARGUMENT IS XX. FUNCTION VALUE IS RETURNED IN PROP. EXTRAPOLATION
!     IS MADE IF XX IS OUTSIDE THE LIMITS OF THE TABLE.
!
 9700 itabl = Z(l+2)
   ntabl = Z(l+3)
   up = 1.0
   IF ( Zz(itabl)>Zz(itabl+2) ) up = -1.0
   kxx1 = itabl
   IF ( (xx-Zz(itabl))*up<=0. ) GOTO 9800
   kxx1 = ntabl - 2
   IF ( (xx-Zz(ntabl))*up>=0. ) GOTO 9800
   klo = 1
   khi = (ntabl-itabl)/2 + 1
   DO
      kx = (klo+khi+1)/2
      kxx = (kx-1)*2 + itabl
      IF ( (xx-Zz(kxx))*up<0 ) THEN
         khi = kx
      ELSEIF ( (xx-Zz(kxx))*up==0 ) THEN
         GOTO 9900
      ELSE
         klo = kx
      ENDIF
      IF ( khi-klo==1 ) THEN
         kxx1 = (klo-1)*2 + itabl
         IF ( kxx==kxx1 ) EXIT
         IF ( xx/=Zz(kxx1+2) ) EXIT
         kxx = kxx1 + 2
         GOTO 9900
      ENDIF
   ENDDO
 9800 prop = (xx-Zz(kxx1))*(Zz(kxx1+3)-Zz(kxx1+1))/(Zz(kxx1+2)-Zz(kxx1)) + Zz(kxx1+1)
   GOTO iret
 9900 IF ( xx==Zz(kxx-2) ) THEN
      prop = (Zz(kxx-1)+Zz(kxx+1))/2.0
      GOTO iret
   ELSEIF ( xx==Zz(kxx+2) ) THEN
      prop = (Zz(kxx+1)+Zz(kxx+3))/2.0
      ASSIGN 10000 TO iret
      GOTO 9700
   ELSE
      prop = Zz(kxx+1)
      GOTO iret
   ENDIF
10000 GOTO ret
10100 prop = Zz(i)*prop
   GOTO ret
10200 nn = Z(l+3)
   prop = Zz(nn)
   DO WHILE ( nn>Z(l+2) )
      prop = prop*xx + Zz(nn-1)
      nn = nn - 1
   ENDDO
   IF ( part1 ) GOTO igoto
10300 prop = prop*factor
   GOTO ret
10400 Zz(l+8) = prop
   ASSIGN 10500 TO igoto
   xx = (Zz(l+7)-Zz(l+4))/Zz(l+5)
   GOTO 10200
10500 Zz(l+9) = prop
   GOTO 2500
!
!     FATAL ERROR MESSAGES
!
10600 n = -2
   dit = mpt
   GOTO 11100
10700 n = -1
   GOTO 11100
10800 n = -2
   GOTO 11100
10900 n = -3
   GOTO 11100
11000 IF ( Nimat<=2*Sysbuf+4 ) THEN
      WRITE (Nout,99002) imhere , i , n1mat , offset , Nimat
99002 FORMAT ('0*** NIMAT SPACE TOO SMALL.  ERROR AT',I5,'/PREMAT',/5X,'I,N1MAT,OFFSET,NIMAT =',3I12,I7,/)
      CALL errtrc('MAT     ',1472)
!
      CALL mesage(-30,n,Buf)
      GOTO 11700
   ELSE
      n = -8
      dit = i - n1mat
   ENDIF
11100 CALL mesage(n,dit,nam)
   n = 16
   Buf(1) = 0
   Buf(2) = 0
   GOTO 11600
11200 n = 17
11300 Buf(1) = Matid
   Buf(2) = 0
   GOTO 11600
11400 n = 19
   GOTO 11300
11500 n = 20
   GOTO 11300
!
11600 CALL sswtch(20,j)
   IF ( j==0 ) THEN
      CALL mesage(-30,n,Buf)
   ELSE
      WRITE (Nout,99003) Buf(1) , Buf(2)
99003 FORMAT (' PREMAT/1471 - BUF(1),BUF(2) =',2I10)
      CALL errtrc('MAT     ',1472)
      CALL mesage(-30,n,Buf)
   ENDIF
!
11700 n = 42
   Buf(1) = Elemid
   Buf(2) = Matid
   GOTO 11600
11800 n = 112
   Buf(1) = tablid
   GOTO 11600
11900 n = 113
   GOTO 11300
12000 n = 116
   Buf(1) = Matid
   Buf(2) = tablid
   GOTO 11600
12100 n = 114
   GOTO 11300
12200 n = 115
   Buf(1) = tablid
   Buf(2) = itype
   GOTO 11600
12300 Buf(2) = 0
   n = 117
   GOTO 11600
12400 Buf(2) = 1
   GOTO 12600
12500 Buf(2) = 2
12600 n = 216
12700 Buf(1) = Matid
   GOTO 11600
12800 n = 217
   GOTO 12700
END SUBROUTINE premat
