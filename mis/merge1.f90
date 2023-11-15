
SUBROUTINE merge1
!
!     THIS IS THE DMAP MODULE MERGE WHICH MERGES 1 TO 4 PARTITIONS
!     A11, A21, A12, A22, INTO A SINGLE MATRIX -A-.
!
!          **                  **           **                  **
!          *       I            *           *                    *
!          *  A11  I    A12     *           *                    *
!          *       I            *           *                    *
!          * ------+----------- *  BECOMES  *          A         *
!          *       I            *           *                    *
!          *       I            *           *                    *
!          *  A21  I    A22     *           *                    *
!          *       I            *           *                    *
!          **                  **           **                  **
!
!     BASED ON THE ZEROS AND NON-ZEROS IN THE ROW PARTITIONING VECTOR
!     -RP- AND THE COLUMN PARTITIONING VECTOR -CP-.
!
!     DMAP CALLING SEQUENCE.
!
!     MERGE  A11,A21,A12,A22,CP,RP/ A /V,Y,SYM  /V,Y,TYPE/V,Y,FORM/
!                                      V,Y,CPCOL/V,Y,RPCOL  $
!
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Cls , Clsrew , Cpcol , Cpones , Cpsize , Dumfor(3) , Elem(4) , Form , Icp , Ireqcl , Irp , Nbpw , Ncp , Nrp , Outpt ,    &
         & Rd , Rdrew , Row , Rpcol , Rpones , Rpsize , Sym , Sysbuf , Type , Wrt , Wrtrew , Xxx(37) , Z(1)
   LOGICAL Cphere , Cpnull , Rphere , Rpnull
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Sym , Type , Form , Cpcol , Rpcol , Dumfor , Ireqcl
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Cls
   COMMON /prtmrg/ Cpsize , Rpsize , Cpones , Rpones , Cpnull , Rpnull , Cphere , Rphere , Icp , Ncp , Irp , Nrp
   COMMON /system/ Sysbuf , Outpt , Xxx , Nbpw
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   COMMON /zblpkx/ Elem , Row
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER a , aij(4) , bit , bitwd , block(80) , buff , cols , core , cp , cpzero , elem1(4) , elem2(4) , eol1 , eol2 , eor ,      &
         & head(2) , i , i1 , i2 , ibloc1 , ibloc2 , iblock , icol , ifile , il1 , ipos1 , ipos2 , ipr , irlcx , irow , irow1 ,     &
         & irow2 , iz , j , jrow , k , kblock , kfile , m , mcb(7,4) , mcba(7) , mpart , naform , nam1 , nam2 , natype , nform ,    &
         & ntype , nz , ones , refus(3) , rows , rp , rpzero , shift , subr(2) , zero
   INTEGER andf , korsz , rshift
   LOGICAL only , pass
   EXTERNAL andf , rshift
!
! End of declarations
!
   DATA subr/4HMERG , 4HE1  / , eor/1/
   DATA aij/101 , 102 , 103 , 104/ , cp , rp/105 , 106/ , a/201/
   DATA naform/4HFORM/ , natype/4HTYPE/ , refus/2*3H    , 3HREF/
!
!     OPEN MATRICES TO BE MERGED.  IF ALL ARE PURGED, RETURN IS MADE.
!
   core = korsz(Z)
   m = 0
   DO i = 1 , 4
      kfile = aij(i)
      mcb(1,i) = kfile
      CALL rdtrl(mcb(1,i))
      IF ( mcb(1,i)>0 ) THEN
         buff = core - Sysbuf - 2
         core = buff - 1
         IF ( core<10 ) CALL mesage(-8,0,subr)
         CALL open(*100,kfile,Z(buff),Rdrew)
         CALL skprec(kfile,1)
         m = 1
      ENDIF
 100  ENDDO
   IF ( m==0 ) RETURN
   buff = core - Sysbuf - 2
   core = buff - 1
   IF ( core<10 ) CALL mesage(-8,0,subr)
   CALL open(*700,a,Z(buff),Wrtrew)
   CALL close(a,Clsrew)
!
!     CALL TO PARTN2 WILL PROCESS -CP- AND -RP- INTO BIT STRINGS AND
!     DETERMINE SIZES OF PARTITIONS REQUIRED.
!
!     STANDARDISE BLANK COMMON FOR PARTN2 CALLS FROM MERGE1-PARTN1
!
   Dumfor(2) = Cpcol
   Dumfor(3) = Rpcol
   CALL partn2(cp,rp,core,Z(buff))
   Cpcol = Dumfor(2)
   Rpcol = Dumfor(3)
!
!     IF CPSIZE OR RPSIZE IS 0 AS A RESULT OF A NULL VECTOR (PURGED
!     VECTOR) THERE SIZE IS ESTIMATED HERE FROM THEIR RESPECTIVE
!     PARTITIONS.
!
   IF ( Cpsize==0 ) THEN
      IF ( mcb(1,1)>0 ) THEN
         Cpsize = mcb(2,1)
!
      ELSEIF ( mcb(1,2)>0 ) THEN
         Cpsize = mcb(2,2)
      ENDIF
   ENDIF
!
   IF ( Rpsize==0 ) THEN
      IF ( mcb(1,1)>0 ) THEN
         Rpsize = mcb(3,1)
!
      ELSEIF ( mcb(1,3)>0 ) THEN
         Rpsize = mcb(3,3)
      ENDIF
   ENDIF
!
!     MATRIX COMPATIBILITY CHECKS.
!
   cpzero = Cpsize - Cpones
   rpzero = Rpsize - Rpones
   ipr = 1
   irlcx = 0
   DO i = 1 , 4
      IF ( mcb(1,i)>0 ) THEN
         cols = mcb(2,i)
         rows = mcb(3,i)
         icol = cpzero
         irow = rpzero
         IF ( mcb(5,i)==2 .OR. mcb(5,i)==4 ) ipr = 2
         IF ( mcb(5,i)==3 .OR. mcb(5,i)==4 ) irlcx = 2
         IF ( i==3 .OR. i==4 ) icol = Cpones
         IF ( i==2 .OR. i==4 ) irow = Rpones
         IF ( icol>0 ) THEN
            IF ( irow>0 ) THEN
!
!     CHECK PARTITION SIZE WITH PARTITIONING VECTOR DEMANDS.
!
               IF ( rows/=irow .OR. cols/=icol ) THEN
                  WRITE (Outpt,99001) Swm , aij(i) , rows , cols , irow , icol
99001             FORMAT (A27,' 2161, PARTITION FILE',I4,' IS OF SIZE',I10,' ROWS BY',I10,' COLUMNS.',/5X,'PARTITIONING VECTORS ',  &
                         &'INDICATE THAT THIS PARTITION SHOULD BE OF SIZE',I10,' ROWS BY',I10,' COLUMNS FOR A SUCCESSFUL MERGE.')
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDDO
!
!     CHECK OF FORM VALUE.
!
   nform = Form
   IF ( nform<1 .OR. nform>8 ) THEN
      nform = 2
      IF ( rows==cols .OR. Cpsize==Rpsize ) THEN
         nform = 1
         IF ( Sym<0 ) nform = 6
      ENDIF
      IF ( Form/=0 .AND. Form/=nform ) WRITE (Outpt,99003) Swm , naform , Form , refus(3) , subr , nform
      Form = nform
   ELSE
      IF ( nform==2 ) GOTO 200
      IF ( nform==3 .OR. nform==7 ) THEN
         IF ( Cpsize==1 ) GOTO 200
!
!     FORM = SQUARE
!
      ELSEIF ( Cpsize==Rpsize ) THEN
         GOTO 200
      ENDIF
      WRITE (Outpt,99002) Swm , nform , Rpsize , Cpsize
99002 FORMAT (A27,' 2162, THE FORM PARAMETER AS GIVEN TO THE MERGE ','MODULE IS INCONSISTANT WITH THE SIZE OF THE',/5X,             &
             &'MERGED MATRIX, HOWEVER IT HAS BEEN USED.  FORM =',I9,' SIZE =',I10,' ROWS BY',I10,' COLUMNS.')
   ENDIF
!
!     CHECK PARAMETER -TYPE-
!
 200  ntype = irlcx + ipr
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
!     THE ROW PARTITIONING BIT STRING IS AT THIS POINT CONVERTED TO A
!     CORE VECTOR ONE WORD PER BIT.  EACH WORD CONATINS THE ACTUAL ROW
!     POSITION THE SUB-PARTITON ELEMENT WILL OCCUPY IN THE MERGED
!     MATRIX.
!
   iz = Nrp + 1
   nz = iz + Rpsize - 1
   IF ( nz>core ) CALL mesage(-8,0,subr)
   IF ( .NOT.Rpnull .AND. Rpones/=0 ) THEN
      k = 0
      zero = iz - 1
      ones = zero + rpzero
      DO i = Irp , Nrp
         DO j = 1 , Nbpw
            shift = Nbpw - j
            bit = rshift(Z(i),shift)
            k = k + 1
            IF ( k>Rpsize ) GOTO 300
            IF ( andf(bit,1)/=0 ) THEN
               ones = ones + 1
               Z(ones) = k
            ELSE
               zero = zero + 1
               Z(zero) = k
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
!     OPEN OUTPUT FILE AND FILL MCB.
!
 300  CALL open(*700,a,Z(buff),Wrtrew)
   CALL fname(a,head)
   CALL write(a,head,2,eor)
   CALL makmcb(mcba,a,Rpsize,nform,ntype)
!
!     MERGE OPERATIONS.  LOOPING ON OUTPUT COLUMNS OF -A-.
!
   i1 = iz - 1
   i2 = i1 + rpzero
   DO i = 1 , Cpsize
!
!     START A COLUMN OUT ON -A-
!
      CALL bldpk(ntype,ntype,a,0,0)
      IF ( .NOT.(Cpnull) ) THEN
         il1 = i - 1
         bitwd = il1/Nbpw + Icp
         shift = Nbpw - mod(il1,Nbpw) - 1
         bit = rshift(Z(bitwd),shift)
         IF ( andf(bit,1)/=0 ) THEN
!
!     ONE-S COLUMN (RIGHT PARTITIONS A12 AND A22 USED THIS PASS)
!
            ifile = 3
            iblock = 41
            GOTO 350
         ENDIF
      ENDIF
!
!     ZERO-S COLUMN (LEFT PARTITONS A11 AND A21 USED THIS PASS)
!
      ifile = 1
      iblock = 1
!
!     START UNPACKING COLUMN OF EACH PARTITION BEING USED THIS PASS.
!
 350  kfile = ifile
      kblock = iblock
      mpart = 0
      DO j = 1 , 2
         IF ( mcb(1,kfile)>0 ) THEN
            CALL intpk(*360,mcb(1,kfile),block(kblock),ntype,1)
            mpart = mpart + j
         ENDIF
 360     kfile = kfile + 1
         kblock = kblock + 20
      ENDDO
      IF ( mpart<=0 ) GOTO 650
!
!     UNPACK NON-ZEROS FROM EACH OF THE TWO PARTITIONS AS NEEDED UNTIL
!     BOTH PARTITIONS HAVE THIS COLUMN EXHAUSED.
!
      eol1 = 1
      eol2 = 1
      nam1 = mcb(1,ifile)
      nam2 = mcb(1,ifile+1)
      ibloc1 = iblock
      ibloc2 = iblock + 20
      IF ( mpart==1 .OR. mpart==3 ) eol1 = 0
      IF ( mpart>1 ) eol2 = 0
      pass = .FALSE.
      only = .FALSE.
      IF ( eol1>0 ) THEN
         only = .TRUE.
         GOTO 450
      ELSEIF ( eol2>0 ) THEN
         only = .TRUE.
      ENDIF
!
!     UNPACK A NON-ZERO FROM THE ZEROS PARTITION
!
 400  CALL intpki(elem1,irow1,nam1,block(ibloc1),eol1)
!
!     SET OUTPUT ROW POSITION
!
      jrow = i1 + irow1
      ipos1 = Z(jrow)
      IF ( only ) GOTO 550
      IF ( pass ) GOTO 500
!
!     UNPACK A NON-ZERO FROM THE ONE-S PARTITION
!
 450  CALL intpki(elem2,irow2,nam2,block(ibloc2),eol2)
!
!     SET OUTPUT ROW POSITION
!
      jrow = i2 + irow2
      ipos2 = Z(jrow)
      IF ( only ) GOTO 600
      pass = .TRUE.
!
!     OK COMING HERE MEANS THERE IS ONE ELEMENT FORM EACH PARTITION
!     AVAILABLE FOR OUTPUT.  THUS OUTPUT THE ONE WITH THE LOWEST
!     OUTPUT ROW NUMBER.
!
 500  IF ( ipos2<ipos1 ) GOTO 600
!
!     OUTPUT ELEMENT FROM ZERO-S PARTITION.
!
 550  Row = ipos1
      Elem(1) = elem1(1)
      Elem(2) = elem1(2)
      Elem(3) = elem1(3)
      Elem(4) = elem1(4)
      CALL zblpki
      IF ( eol1<=0 ) GOTO 400
      IF ( only ) GOTO 650
      only = .TRUE.
!
!     OUTPUT ELEMENT FROM ONES-PARTITION.
!
 600  Row = ipos2
      Elem(1) = elem2(1)
      Elem(2) = elem2(2)
      Elem(3) = elem2(3)
      Elem(4) = elem2(4)
      CALL zblpki
      IF ( eol2<=0 ) GOTO 450
      IF ( .NOT.(only) ) THEN
         only = .TRUE.
         GOTO 550
      ENDIF
!
!     COMPLETE THE COLUMN BEING OUTPUT
!
 650  CALL bldpkn(a,0,mcba)
   ENDDO
!
!     MERGE IS COMPLETE.  WRAP UP.
!
   CALL close(a,Clsrew)
   CALL wrttrl(mcba)
 700  DO i = 1 , 4
      IF ( mcb(1,i)>0 ) CALL close(mcb(1,i),Clsrew)
   ENDDO
99003 FORMAT (A27,' 2163, REQUESTED VALUE OF ',A4,I10,2X,A3,'USED BY ',2A4,'. LOGICAL CHOICE IS',I10)
!
END SUBROUTINE merge1
