!*==sort.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sort(Idum,Jdum,Nr,Keywd,Z,Nwds)
   USE c_machin
   USE c_system
   USE c_two
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nr
   INTEGER :: Idum
   INTEGER :: Jdum
   INTEGER :: Keywd
   INTEGER , DIMENSION(Nr,1) :: Z
   INTEGER :: Nwds
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ii , isort , j , jj , k , key , kk , l , limit , m , n , nc , temp , two31 , zi , zn
   REAL :: ri , rn , zero
   LOGICAL :: rvsbcd
   INTEGER , DIMENSION(6) , SAVE :: subr
   INTEGER :: spag_nextblock_1
!
! End of declarations rewritten by SPAG
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
   !>>>>EQUIVALENCE (zi,ri) , (zn,rn)
   DATA subr/2H   , 2HF  , 2HA  , 2HA8 , 2HA7 , 2H2K/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     CHECK ERROR, CHECK DATA TYPE, AND PREPARE FOR SORT
!
         isort = 1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
         ENTRY sortf(Idum,Jdum,Nr,Keywd,Z,Nwds)
!     =======================================
         isort = 2
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
         ENTRY sorta(Idum,Jdum,Nr,Keywd,Z,Nwds)
!     =======================================
         isort = 3
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
         ENTRY sorta8(Idum,Jdum,Nr,Keywd,Z,Nwds)
!     ========================================
         isort = 4
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
         ENTRY sorta7(Idum,Jdum,Nr,Keywd,Z,Nwds)
!     ========================================
         isort = 5
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
         ENTRY sort2k(Idum,Jdum,Nr,Keywd,Z,Nwds)
!     ========================================
         isort = 6
         spag_nextblock_1 = 2
      CASE (2)
!
         IF ( Nwds==0 ) RETURN
         IF ( Idum/=0 .OR. Jdum/=0 ) THEN
            WRITE (nout,99001)
99001       FORMAT ('0*** CALLING ROUTINE SHOULD CALL SORTI')
            RETURN
         ELSE
            rvsbcd = mod(lqro,10)==1
            key = iabs(Keywd)
            IF ( key>Nr ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            nc = Nwds/Nr
            IF ( nc*Nr/=Nwds ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ENDIF
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
               IF ( nc>=two(16) .AND. nbpw<=32 ) THEN
                  spag_nextblock_1 = 9
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               j = 30
               IF ( nbpw>=60 ) j = 62
               two31 = 2**j
               limit = (two31-nc)/nc
               DO i = 1 , nc
                  j = Z(key,i)
                  IF ( iabs(j)>limit ) THEN
                     spag_nextblock_1 = 9
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  j = j*nc + i
                  k = -1
                  IF ( j<0 ) k = -nc
                  Z(key,i) = j + k
               ENDDO
            ENDIF
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
!     SORT BY
!     MODIFIED SHELL METHOD, A SUPER FAST SORTER
!
         m = m/2
         IF ( m==0 ) THEN
            IF ( isort==1 .AND. Keywd<0 ) THEN
               DO i = 1 , nc
                  Z(key,i) = Z(key,i)/nc
               ENDDO
            ENDIF
            RETURN
         ELSE
            j = 1
            k = nc - m
            i = j
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
         n = i + m
         zi = Z(key,i)
         zn = Z(key,n)
         IF ( isort==2 ) THEN
            IF ( ri>rn ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( isort==3 .OR. isort==4 .OR. isort==5 ) THEN
            kk = 1
            IF ( isort==5 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
!           INT FP A4 A8 A7 2K
!
            IF ( zi<zn ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( zi/=zn ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( isort==1 ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Z(key+1,i)>Z(key+1,n) ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
!
!     COMPARE 1ST BYTE, THEN COMPARE 2ND, 3RD, AND 4TH BYTES TOGETHER
!     IF MACHINE DOES NOT USE REVERSED BCD ORDER. THOSE MACHINES WITH
!     REVERSED BCD ORDER (VAX, ULTRIX, S/G) MUST COMPARE EACH BYTE
!     SEPERATELY BECAUSE OF THE SIGN BIT
!
         IF ( khrfn1(zero,4,zi,1)<khrfn1(zero,4,zn,1) ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( khrfn1(zero,4,zi,1)/=khrfn1(zero,4,zn,1) ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
         IF ( .NOT.rvsbcd ) THEN
            IF ( khrfn1(zi,1,zero,4)<khrfn1(zn,1,zero,4) ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( khrfn1(zi,1,zero,4)/=khrfn1(zn,1,zero,4) ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
            IF ( khrfn1(zero,4,zi,2)<khrfn1(zero,4,zn,2) ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( khrfn1(zero,4,zi,2)/=khrfn1(zero,4,zn,2) ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( khrfn1(zero,4,zi,3)<khrfn1(zero,4,zn,3) ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( khrfn1(zero,4,zi,3)/=khrfn1(zero,4,zn,3) ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( khrfn1(zero,4,zi,4)<khrfn1(zero,4,zn,4) ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( khrfn1(zero,4,zi,4)/=khrfn1(zero,4,zn,4) ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         IF ( isort<=3 .OR. kk==2 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         zi = Z(key+1,i)
         zn = Z(key+1,n)
         kk = 2
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      CASE (7)
         DO l = 1 , Nr
            temp = Z(l,i)
            Z(l,i) = Z(l,n)
            Z(l,n) = temp
         ENDDO
         i = i - m
         IF ( i>=1 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
         j = j + 1
         IF ( j>k ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         i = j
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (9)
!
!     SORT BY
!     SHUTTLE EXCHANGE THETHOD, A SLOW SORTER
!     (THIS WAS NASTRAN ORIGINAL SORTER, MODIFIED FOR 2-D ARRAY OPERATIO
!     WITH 20-COLUMN LIMITATION REMOVED)
!
         IF ( i>1 ) THEN
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
               SPAG_Loop_2_1: DO
                  jj = jj - 1
                  IF ( jj<=0 ) EXIT SPAG_Loop_2_1
                  IF ( zi>=Z(key,jj) ) EXIT SPAG_Loop_2_1
               ENDDO SPAG_Loop_2_1
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
         RETURN
      CASE (10)
!
!     ERROR. FORCING A WALK BACK
!
         WRITE (nout,99002) subr(isort) , Nr , key , Nwds , nc
99002    FORMAT ('0*** ERROR IN SORT',A2,4I8)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!WKBR  320 CALL ERRTRC ('SORT    ',320)
END SUBROUTINE sort
