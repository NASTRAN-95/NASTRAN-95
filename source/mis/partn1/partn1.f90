!*==partn1.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE partn1
!
!     THIS IS THE DMAP MODULE PARTN WHICH PARTITIONS A MATRIX -A- INTO
!     FOUR PARTITIONS, SOME OR ALL OF WHICH MAY BE PURGED.
!
!
!                             **                  **
!                             *       I            *
!                             *  A11  I    A12     *
!          **   **            *       I            *
!          *     *            * ------+----------- *
!          *  A  *  BECOMES   *       I            *
!          *     *            *       I            *
!          **   **            *  A21  I    A22     *
!                             *       I            *
!                             **                  **
!
!
!     BASED ON ROW PARTITION MATRIX -RP- AND COLUMN PARTITION MATRIX
!     -CP-
!
!     DMAP SEQUENCE.
!
!     PARTN A,CP,RP/A11,A21,A12,A22/V,Y,SYM/V,Y,TYPE/V,Y,F11/V,Y,F21/
!                                   V,Y,F12/V,Y,F22 $
!
   IMPLICIT NONE
   USE C_BLANK
   USE C_MAHCIN
   USE C_NAMES
   USE C_PRTMRG
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZNTPKX
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: a , cp , eor , naform , natype , rp
   INTEGER , DIMENSION(4) , SAVE :: aij
   INTEGER :: bit , bitwd , buff , cols , core , cpzero , file , i , iblock , ifile , il1 , inform , iz , izm1 , j , jj , jrow ,    &
            & jz , k , kblock , kfile , l , m , nform , ntype , nz , ones , rows , rpzero , shift , zero
   INTEGER , DIMENSION(80) :: block
   INTEGER , DIMENSION(5) :: buffs
   INTEGER , DIMENSION(2) :: head
   INTEGER , DIMENSION(7,4) :: mcb
   INTEGER , DIMENSION(7) :: mcba
   INTEGER , DIMENSION(3) , SAVE :: refus
   INTEGER , DIMENSION(2) , SAVE :: subr
   EXTERNAL andf , bldpk , bldpki , bldpkn , close , fname , intpk , korsz , makmcb , mesage , open , partn2 , rdtrl , rshift ,     &
          & skprec , write , wrttrl , zntpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   DATA subr/4HPART , 4HN1  / , a , cp , rp/101 , 102 , 103/ , aij/201 , 202 , 203 , 204/
   DATA naform/4HFORM/ , natype/4HTYPE/ , refus/2*3H    , 3HREF/
   DATA eor/1/
!
   core = korsz(Z)
   buffs(1) = core - Sysbuf - 2
   DO i = 2 , 5
      buffs(i) = buffs(i-1) - Sysbuf - 2
   ENDDO
   core = buffs(5) - 1
   IF ( core<10 ) CALL mesage(-8,0,subr)
!
!     OPEN MATRIX TO BE PARTITIONED.  IF PURGED RETURN IS MADE
!
   buff = buffs(5)
   CALL open(*99999,a,Z(buff),Rdrew)
   CALL skprec(a,1)
   mcba(1) = a
   CALL rdtrl(mcba)
   inform = mcba(4)
!
!     CALL TO PARTN2 WILL PROCESS -CP- AND -RP- INTO BIT STRINGS AND
!     DETERMINE SIZES OF THE PARTITIONS.
!
   buff = buffs(4)
   CALL partn2(cp,rp,core,Z(buff))
!
!     IF RPSIZE OR CPSIZE ARE 0 THEY ARE SET EQUAL TO THE RESPECTIVE
!     SIZE OF A
!
   IF ( Cpsize==0 ) Cpsize = mcba(2)
   IF ( Rpsize==0 ) Rpsize = mcba(3)
!
!     MATRIX COMPATIBILITY CHECKS
!
   IF ( Rpsize/=mcba(3) .OR. Cpsize/=mcba(2) ) THEN
      WRITE (Outpt,99001) Swm , mcba(3) , mcba(2) , Rpsize , Cpsize
99001 FORMAT (A27,' 2166, MATRIX TO BE PARTITIONED IS OF SIZE',I10,' ROWS BY',I10,' COLUMNS.',/5X,'ROW PARTITION SIZE IS',I10,      &
             &' COLUMN PARTITION SIZE IS',I10,' (INCOMPATIBLE).')
   ENDIF
!
!     PREPARE OUTPUT DATA BLOCKS AS REQUIRED.
!
   cpzero = mcba(2) - Cpones
   rpzero = mcba(3) - Rpones
!
!     CHECK OF TYPE PARAMETER
!
   ntype = mcba(5)
   IF ( ntype/=Type ) THEN
      IF ( Type==0 ) THEN
         Type = ntype
      ELSEIF ( Type<0 .OR. Type>4 ) THEN
         WRITE (Outpt,99003) Swm , natype , Type , refus(3) , subr , ntype
         Type = ntype
      ELSE
         WRITE (Outpt,99003) Swm , natype , Type , refus(1) , subr , ntype
         ntype = Type
      ENDIF
   ENDIF
!
   DO i = 1 , 4
      file = aij(i)
      mcb(1,i) = 0
      cols = cpzero
      rows = rpzero
      IF ( i==3 .OR. i==4 ) cols = Cpones
      IF ( i==2 .OR. i==4 ) rows = Rpones
!
!     IF ROWS OR COLS EQUAL ZERO NOTHING IS WRITTEN ON THIS PARTITION
!
      IF ( rows/=0 .AND. cols/=0 ) THEN
         buff = buffs(i)
         CALL open(*100,file,Z(buff),Wrtrew)
         CALL fname(file,head)
         CALL write(file,head,2,eor)
!
!     CHECK OF THE FORM PARAMETER
!
         nform = Form(i)
         IF ( nform<1 .OR. nform>8 ) THEN
!
!     NO FORM SPECIFIED THUS IT IS SQUARE IF ROWS = COLS OR RECTANGULAR
!     OTHERWISE.
!
            nform = 2
            IF ( rows==cols ) nform = 1
            IF ( Sym<0 .AND. inform==6 .AND. nform==1 .AND. (i==1 .OR. i==4) ) nform = 6
            IF ( Form(i)/=0 ) THEN
               jj = 1
               IF ( Form(i)<1 .OR. Form(i)>8 ) jj = 3
               WRITE (Outpt,99003) Swm , naform , Form(i) , refus(jj) , subr , nform
               IF ( jj/=3 ) nform = Form(i)
            ENDIF
            Form(i) = nform
         ELSEIF ( nform/=2 ) THEN
            IF ( nform==3 .OR. nform==7 ) THEN
!
!     DIAGONAL OR ROW MATRIX
!
               IF ( cols==1 ) GOTO 20
!
!     FORM IMPLIES SQUARE
!
            ELSEIF ( rows==cols ) THEN
               GOTO 20
            ENDIF
            WRITE (Outpt,99002) Swm , head , nform , rows , cols
99002       FORMAT (A27,' 2168, THE FORM PARAMETER AS GIVEN TO THE PARTITION','ING MODULE FOR SUB-PARTITION ',2A4,/5X,              &
                   &'IS INCONSISTANT',' WITH ITS SIZE.  FORM =',I9,' SIZE =',I9,' ROWS BY',I9,' COLUMNS.')
         ENDIF
!
!     TRAILER INITIALIZATION.  BLDPKN WILL SET MCB(2) AND MCB(6) LATER.
!
 20      CALL makmcb(mcb(1,i),file,rows,nform,ntype)
      ENDIF
 100  ENDDO
!
!     ROW PARTITIONING BIT STRING IS AT THIS POINT CONVERTED TO A CORE
!     VECTOR ONE WORD PER BIT.  EACH WORD CONTAINS THE ROW NUMBER OF THE
!     PARTITION TO WHICH THE ELEMENT OF -A- IS TO BE MOVED TO.  IF THE
!     NUMBER IS NEGATIVE THE ELEMENT IS MOVED TO THE LOWER PARTITIONS
!     AND IF THE NUMBER IS POSITIVE THE ELEMENT IS MOVED TO THE UPPER
!     PARTITION
!
   iz = Nrp + 1
   nz = iz + Rpsize - 1
   IF ( nz+Nbpw>core ) CALL mesage(-8,0,subr)
   IF ( .NOT.Rpnull .AND. Rpones/=0 ) THEN
      jz = iz - 1
      zero = 0
      ones = 0
!
!     NOTE THIS LOGIC WORKS ON CRAY WITH 48 OF 64 BIT INTEGER WORD
!
      DO i = Irp , Nrp
         DO j = 1 , Nbpw
            shift = Nbpw - j
            bit = rshift(Z(i),shift)
            jz = jz + 1
            IF ( andf(bit,1)/=0 ) THEN
               ones = ones - 1
               Z(jz) = ones
            ELSE
               zero = zero + 1
               Z(jz) = zero
            ENDIF
         ENDDO
      ENDDO
   ELSE
      k = 0
      DO i = iz , nz
         k = k + 1
         Z(i) = k
      ENDDO
   ENDIF
!
!     LOOP ON ALL THE COLUMNS OF -A-.
!
   izm1 = iz - 1
   DO i = 1 , Cpsize
      spag_nextblock_1 = 1
      SPAG_DispatchLoop_1: DO
         SELECT CASE (spag_nextblock_1)
         CASE (1)
            IF ( .NOT.(Cpnull) ) THEN
               il1 = i - 1
               bitwd = il1/Nbpw + Icp
               shift = Nbpw - mod(il1,Nbpw) - 1
               bit = rshift(Z(bitwd),shift)
               IF ( andf(bit,1)/=0 ) THEN
!
!     ONE-S COLUMN (RIGHT PARTITIONS A12 AND A22)
!
                  ifile = 3
                  iblock = 41
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
!
!     ZERO-S COLUMN (LEFT PARTITIONS A11 AND A21)
!
            ifile = 1
            iblock = 1
            spag_nextblock_1 = 2
         CASE (2)
!
!     START COLUMNS OF THE 2 AIJ PARTITIONS.
!
            kfile = ifile
            kblock = iblock
            m = 0
            DO j = 1 , 2
               IF ( mcb(1,kfile)>0 ) THEN
                  CALL bldpk(ntype,mcb(5,kfile),mcb(1,kfile),block(kblock),1)
                  m = 1
               ENDIF
               kfile = kfile + 1
               kblock = kblock + 20
            ENDDO
            IF ( m/=0 ) THEN
!
!     START THE I-TH COLUMN OF THE MATRIX BEING PARTITIONED -A-.
!
               CALL intpk(*110,a,0,ntype,0)
!
!     LOOP ON NON-ZEROS OF THE COLUMN
!
               DO WHILE ( Eol<=0 )
!
!     PICK UP A NON-ZERO ELEMENT
!
                  CALL zntpki
!
!     DETERMINE ROW POSITION AND FILE DESTINATION.
!
                  l = izm1 + Row
                  IF ( Z(l)<0 ) THEN
!
!     ONE-S ROW PARTITION.
!
                     jrow = -Z(l)
                     kfile = ifile + 1
                     kblock = iblock + 20
                  ELSE
!
!     ZERO-S ROW PARTITION.
!
                     jrow = Z(l)
                     kfile = ifile
                     kblock = iblock
                  ENDIF
!
!     OUTPUT THE ELEMENT.
!
                  IF ( mcb(1,kfile)>0 ) CALL bldpki(Elem,jrow,mcb(1,kfile),block(kblock))
               ENDDO
            ELSE
!
!     COLUMN NOT BEING OUTPUT TO ANY PARTITIONS AT ALL THUS SKIP IT.
!
               CALL skprec(a,1)
               CYCLE
            ENDIF
!
!     COMPLETE COLUMNS OF THE 2 AIJ PARTITIONS BEING WORKED ON.
!
 110        kfile = ifile
            kblock = iblock
            DO j = 1 , 2
               IF ( mcb(1,kfile)>0 ) CALL bldpkn(mcb(1,kfile),block(kblock),mcb(1,kfile))
               kfile = kfile + 1
               kblock = kblock + 20
            ENDDO
            EXIT SPAG_DispatchLoop_1
         END SELECT
      ENDDO SPAG_DispatchLoop_1
!
   ENDDO
!
!     WRAP UP.
!
   CALL close(a,Clsrew)
   DO i = 1 , 4
      IF ( mcb(1,i)>0 ) THEN
         CALL wrttrl(mcb(1,i))
         CALL close(mcb(1,i),Clsrew)
      ENDIF
   ENDDO
99003 FORMAT (A27,' 2163, REQUESTED VALUE OF ',A4,I10,2X,A3,'USED BY ',2A4,'. LOGICAL CHOICE IS',I10)
99999 END SUBROUTINE partn1
