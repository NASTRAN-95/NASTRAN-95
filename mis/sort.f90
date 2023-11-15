
SUBROUTINE sort(Idum,Jdum,Nr,Keywd,Z,Nwds)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Dm37(37)
   INTEGER Ibuf , Ijhlf(2) , Lqro , Mach , Nbpw , Nout , Two(16)
   COMMON /machin/ Mach , Ijhlf , Lqro
   COMMON /system/ Ibuf , Nout , Dm37 , Nbpw
   COMMON /two   / Two
!
! Dummy argument declarations
!
   INTEGER Idum , Jdum , Keywd , Nr , Nwds
   INTEGER Z(Nr,1)
!
! Local variable declarations
!
   INTEGER i , ii , isort , j , jj , k , key , kk , l , limit , m , n , nc , subr(6) , temp , two31 , zi , zn
   INTEGER khrfn1
   REAL ri , rn , zero
   LOGICAL rvsbcd
!
! End of declarations
!
!
!     THE ORIGINAL NASTRAN SORT ROUTINE FOR IN-CORE SORTING AND FILE
!     SORT IS NOW RENAMED SORTI
!     (ONLY 5 PERCENT OF NASTRAN ROUTINES ACTUALLY CALL SORTI, WITH NON-
!     ZERO IDUM AND JDUM)
!
!     THIS NEW SORT ROUTINE WITH IDUM=JDUM=0, PERFORMS ONLY IN-CORE SORT
!     FOR INTEGERS, FLOATING POINT NUMBERS, AND BCD WORDS, BY THE
!     MODIFIED SHELL METHOD
!     IT USES MUCH LESS CORE SPACE
!
!     ARRAY Z IS NR-ROWS BY (NWDS/NR)-COLUMNS IN SIZE
!     DATA STORED ROW-WISE IN Z, AND TO BE SORTED BY KEYWD-TH ROW
!
!     USE A NEGATIVE KEYWD  IF THE ORIGINAL ORDER OF THE TABLE ENTRIES
!     ARE TO BE PRESERVED AND THE COLUMN OF KEYWORDS CONTAINS DUPLICATES
!     (INTEGER SORT ONLY)    E.G.
!
!     ORIGINAL TABLE     SORTED(KEYWD=+1)       SORTED(KEYWD=-1)
!     ---------------    ----------------       ----------------
!       1      4             1      4               1      4
!       2      2             1     10               1      3
!       1      3             1      3               1     10
!       1     10             2      2               2      2
!
!
!     THIS ROUTINE WOULD SWITCH BACK TO THE OLD SHUTTLE EXCHANGE METHOD
!     NUMBERS OVERFLOW DUE TO THE REQUIREMENT THAT ORIGINAL ORDER MUST B
!     MAINTAINED
!
!     ENTRY POINTS
!
!     SORT   - TABLE SORT BY INTEGER
!     SORTF  - TABLE SORT BY F.P. NUMBER
!     SORTA  - TABLE SORT BY ALPHABETS, 4-BCD CHARACTERS
!     SORTA8 - TABLE SORT BY ALPHABETS, 8-BCD CHAR. (KEYWD AND KEYWD+1)
!     SORTA7 - SAME AS SORTA8, EXCEPT LEADING CHAR. IS IGNORED
!     SORT2K - 2-KEYWORD SORT, SORT BY KEYWD AND KEYWD+1, INTEGER OR
!              REAL NUMBER KEYS. NEGATIVE KEYWD IS IGNORED
!
!     THE TWO SORT CALLS OF THE FOLLOWING FORM CAN BE REPLACED BY ONE CA
!     TO SORT2K, WHICH IS FASTER, NO DANGER OF NUMBER OVERFLOW, AND THE
!     ORIGINAL SEQUENCE WILL NOT CHANGE WHEN THERE ARE DUPLICATES.
!
!         CALL SORT (0,0,N1,-(N2+1),TABLE,N3)
!         CALL SORT (0,0,N1,-N2,    TABLE,N3)
!              CAN BE REPLACED BY
!         CALL SORT2K (0,0,N1,N2,TABLE,N3)
!
!
!     WRITTEN BY G.CHAN/SPERRY, 3/1987
!
   EQUIVALENCE (zi,ri) , (zn,rn)
   DATA subr/2H   , 2HF  , 2HA  , 2HA8 , 2HA7 , 2H2K/
!
!     CHECK ERROR, CHECK DATA TYPE, AND PREPARE FOR SORT
!
   isort = 1
   GOTO 100
!
   ENTRY sortf(Idum,Jdum,Nr,Keywd,Z,Nwds)
!     =======================================
   isort = 2
   GOTO 100
!
   ENTRY sorta(Idum,Jdum,Nr,Keywd,Z,Nwds)
!     =======================================
   isort = 3
   GOTO 100
!
   ENTRY sorta8(Idum,Jdum,Nr,Keywd,Z,Nwds)
!     ========================================
   isort = 4
   GOTO 100
!
   ENTRY sorta7(Idum,Jdum,Nr,Keywd,Z,Nwds)
!     ========================================
   isort = 5
   GOTO 100
!
   ENTRY sort2k(Idum,Jdum,Nr,Keywd,Z,Nwds)
!     ========================================
   isort = 6
!
 100  IF ( Nwds==0 ) GOTO 99999
   IF ( Idum/=0 .OR. Jdum/=0 ) THEN
      WRITE (Nout,99001)
99001 FORMAT ('0*** CALLING ROUTINE SHOULD CALL SORTI')
      GOTO 99999
   ELSE
      rvsbcd = mod(Lqro,10)==1
      key = iabs(Keywd)
      IF ( key>Nr ) GOTO 900
      nc = Nwds/Nr
      IF ( nc*Nr/=Nwds ) GOTO 900
      m = nc
      IF ( isort==1 .AND. Keywd<0 ) THEN
!
!                     - INTEGER SORT ONLY -
!     IF ORIGINAL ORDER IS TO BE MAINTAINED WHERE DUPLICATE KEYWORDS MAY
!     OCCUR, ADD INDICES TO THE KEYWORDS (GOOD FOR BOTH POSITIVE AND
!     NEGATIVE RANGES, AND BE SURE THAT KEYWORDS ARE NOT OVERFLOWED),
!     SORT THE DATA, AND REMOVE THE INDICES LATER
!
!     IF KEYWORD OVERFLOWS, SWITCH TO SHUTTLE EXCHANGE METHOD
!
         IF ( nc>=Two(16) .AND. Nbpw<=32 ) GOTO 800
         j = 30
         IF ( Nbpw>=60 ) j = 62
         two31 = 2**j
         limit = (two31-nc)/nc
         DO i = 1 , nc
            j = Z(key,i)
            IF ( iabs(j)>limit ) GOTO 800
            j = j*nc + i
            k = -1
            IF ( j<0 ) k = -nc
            Z(key,i) = j + k
         ENDDO
      ENDIF
   ENDIF
!
!     SORT BY
!     MODIFIED SHELL METHOD, A SUPER FAST SORTER
!
 200  m = m/2
   IF ( m==0 ) THEN
      IF ( isort==1 .AND. Keywd<0 ) THEN
         DO i = 1 , nc
            Z(key,i) = Z(key,i)/nc
         ENDDO
      ENDIF
      GOTO 99999
   ELSE
      j = 1
      k = nc - m
      i = j
   ENDIF
 300  n = i + m
   zi = Z(key,i)
   zn = Z(key,n)
   IF ( isort==2 ) THEN
      IF ( ri>rn ) GOTO 600
      GOTO 700
   ELSEIF ( isort==3 .OR. isort==4 .OR. isort==5 ) THEN
      kk = 1
      IF ( isort==5 ) GOTO 500
   ELSE
!           INT FP A4 A8 A7 2K
!
      IF ( zi<zn ) GOTO 700
      IF ( zi==zn ) THEN
         IF ( isort==1 ) GOTO 700
         IF ( Z(key+1,i)>Z(key+1,n) ) GOTO 600
         GOTO 700
      ELSE
         GOTO 600
      ENDIF
   ENDIF
!
!     COMPARE 1ST BYTE, THEN COMPARE 2ND, 3RD, AND 4TH BYTES TOGETHER
!     IF MACHINE DOES NOT USE REVERSED BCD ORDER. THOSE MACHINES WITH
!     REVERSED BCD ORDER (VAX, ULTRIX, S/G) MUST COMPARE EACH BYTE
!     SEPERATELY BECAUSE OF THE SIGN BIT
!
 400  IF ( khrfn1(zero,4,zi,1)<khrfn1(zero,4,zn,1) ) GOTO 700
   IF ( khrfn1(zero,4,zi,1)/=khrfn1(zero,4,zn,1) ) GOTO 600
 500  IF ( .NOT.rvsbcd ) THEN
      IF ( khrfn1(zi,1,zero,4)<khrfn1(zn,1,zero,4) ) GOTO 700
      IF ( khrfn1(zi,1,zero,4)/=khrfn1(zn,1,zero,4) ) GOTO 600
   ELSE
      IF ( khrfn1(zero,4,zi,2)<khrfn1(zero,4,zn,2) ) GOTO 700
      IF ( khrfn1(zero,4,zi,2)==khrfn1(zero,4,zn,2) ) THEN
         IF ( khrfn1(zero,4,zi,3)<khrfn1(zero,4,zn,3) ) GOTO 700
         IF ( khrfn1(zero,4,zi,3)==khrfn1(zero,4,zn,3) ) THEN
            IF ( khrfn1(zero,4,zi,4)<khrfn1(zero,4,zn,4) ) GOTO 700
            IF ( khrfn1(zero,4,zi,4)/=khrfn1(zero,4,zn,4) ) GOTO 600
         ELSE
            GOTO 600
         ENDIF
      ELSE
         GOTO 600
      ENDIF
   ENDIF
   IF ( isort<=3 .OR. kk==2 ) GOTO 700
   zi = Z(key+1,i)
   zn = Z(key+1,n)
   kk = 2
   GOTO 400
 600  DO l = 1 , Nr
      temp = Z(l,i)
      Z(l,i) = Z(l,n)
      Z(l,n) = temp
   ENDDO
   i = i - m
   IF ( i>=1 ) GOTO 300
 700  j = j + 1
   IF ( j>k ) GOTO 200
   i = j
   GOTO 300
!
!     SORT BY
!     SHUTTLE EXCHANGE THETHOD, A SLOW SORTER
!     (THIS WAS NASTRAN ORIGINAL SORTER, MODIFIED FOR 2-D ARRAY OPERATIO
!     WITH 20-COLUMN LIMITATION REMOVED)
!
 800  IF ( i>1 ) THEN
      j = i - 1
      DO i = 1 , j
         Z(key,i) = Z(key,i)/nc
      ENDDO
   ENDIF
!
   DO ii = 2 , nc
      zi = Z(key,ii)
      jj = ii - 1
      IF ( zi<Z(key,jj) ) THEN
         DO
            jj = jj - 1
            IF ( jj<=0 ) EXIT
            IF ( zi>=Z(key,jj) ) EXIT
         ENDDO
         jj = jj + 2
         DO i = 1 , Nr
            temp = Z(i,ii)
            m = ii
            DO j = jj , ii
               Z(i,m) = Z(i,m-1)
               m = m - 1
            ENDDO
            Z(i,jj-1) = temp
         ENDDO
      ENDIF
   ENDDO
   GOTO 99999
!
!     ERROR. FORCING A WALK BACK
!
 900  WRITE (Nout,99002) subr(isort) , Nr , key , Nwds , nc
99002 FORMAT ('0*** ERROR IN SORT',A2,4I8)
!WKBR  320 CALL ERRTRC ('SORT    ',320)
99999 END SUBROUTINE sort
