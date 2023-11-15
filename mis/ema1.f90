
SUBROUTINE ema1
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Cls , Clsrew , Elem(1) , Ii1 , Ii2 , Incr1 , Incr2 , Incre , Ix , Jj1 , Jj2 , Jlast , Mach , Nelem , Nok4 , Nout , Rd ,  &
         & Rdrew , Sysbuf , System(80) , Typin1 , Typin2 , Typou1 , Wrt , Wrtrew , Zi(1)
   DOUBLE PRECISION D(18) , Xd(2) , Zd(1)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   REAL Wtmass , Xs(4) , Zs(1)
   COMMON /blank / Nok4 , Wtmass
   COMMON /gpta1 / Nelem , Jlast , Incre , Elem
   COMMON /ma1xx / D
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Cls
   COMMON /packx / Typin1 , Typou1 , Ii1 , Jj1 , Incr1
   COMMON /system/ System
   COMMON /unpakx/ Typin2 , Ii2 , Jj2 , Incr2
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zblpkx/ Xd , Ix
   COMMON /zzzzzz/ Zd
!
! Local variable declarations
!
   INTEGER block(20) , buf(10) , buf1 , buf2 , buf3 , dof , dofg , ect , gpect , i , icgvec , icode , idict , igrid , ii , iloc ,   &
         & ilook0 , imat , ipvt , isil0 , j , jj , k , kdict , kelem , kerr , kfact , kgg , kk , large , lcore , ll , lpcb , luset ,&
         & maxdct , maxnbr , maxvec , mcb(7) , mcbkgg(7) , minnbr , n , nbrcon , nbrgrd , nbrsil , ncgvec , ngrid , nhema1 , nmat , &
         & npvt , nsca , ntrmec , nwdcgv , nwdcol , nwddct , nwdect , nwdmat , oldcod , opcls , openr , openw , prec , scalas(32) , &
         & scr1 , scr2 , sil , silnbr , subnam(2) , trlsil(7) , tt(3)
   REAL eps , factor
   INTEGER even
   INTEGER korsz
   LOGICAL last
!
! End of declarations
!
!
!     EMA1 ASSEMBLES A STRUCTURAL MATRIX FOR THE MODEL FROM EACH OF
!     THE INDIVIDUAL ELEMENT STRUCTURAL MATRICES.
!
!     EMA1   GPECT,KDICT,KELEM,SIL,ECT / KGG / C,N,NOK4/ C,N,WTMASS
!
!     NOK4 .NE. -1 MEANS MULTIPLY BY DAMPING FACTOR (GE)
!     ABS(WTMASS-1.0) .GT. 1.E-6 MEANS MULTIPLY BY WTMASS
!
!     EMA1 USES 2 SCRATCH FILES
!
   EQUIVALENCE (System(1),Sysbuf) , (System(2),Nout) , (trlsil(2),nbrsil) , (trlsil(3),luset) , (System(22),Mach) ,                 &
    & (Zd(1),Zs(1),Zi(1)) , (Xd(1),Xs(1))
!
!     DEFINITION OF INPUT DATA BLOCKS
!
   DATA gpect , kdict , kelem , sil , ect/101 , 102 , 103 , 104 , 105/
!
!     DEFINITION OF OUTPUT DATA BLOCKS
!
   DATA kgg/201/
!
!     DEFINITION OF SCRATCH FILES
!
   DATA scr1 , scr2/301 , 302/
!
!     MISCELANEOUS DATA
!
   DATA subnam/4HEMA1 , 4H    / , nhema1/4HEMA1/ , large/2147483647/ , lpcb/8/
!
!     DATA    TERMS /  1, 0, 9, 0, 0, 18 /,
!    1        SCL   /  1, 1, 0           /
!
!     STATEMENT FUNCTION
!
   even(n) = 2*((n+1)/2)
!
!     PERFORM GENERALIZATION
!
   lcore = korsz(Zd)
   trlsil(1) = sil
   CALL rdtrl(trlsil)
   WRITE (Nout,99002) (trlsil(i),i=1,7)
   isil0 = lcore - nbrsil - 1
   lcore = isil0
   buf1 = lcore - Sysbuf
   buf2 = buf1 - Sysbuf
   buf3 = buf2 - Sysbuf
   buf(1) = kelem
   CALL rdtrl(buf)
   WRITE (Nout,99002) (buf(i),i=1,7)
   prec = buf(2)
   CALL makmcb(mcbkgg,kgg,luset,6,prec)
   openw = Wrtrew
   openr = Rdrew
   last = .FALSE.
   silnbr = 0
   opcls = Cls
   maxdct = 0
   maxvec = 0
   oldcod = 0
!
!     SET SWITCH FOR MULTIPLICATION BY DAMPING AND/OR WEIGHT MASS FACTOR
!
   eps = abs(Wtmass-1.0)
   IF ( eps<1.E-6 .AND. Nok4<0 ) ASSIGN 800 TO kfact
   IF ( eps<1.E-6 .AND. Nok4>=0 ) ASSIGN 900 TO kfact
   IF ( eps>=1.E-6 .AND. Nok4<0 ) ASSIGN 1000 TO kfact
   IF ( eps>=1.E-6 .AND. Nok4>=0 ) ASSIGN 1100 TO kfact
!
!     READ THE CONTENTS OF THE SIL DATA BLOCK INTO CORE
!
   CALL gopen(sil,Zi(buf1),Rdrew)
   CALL fread(sil,Zi(isil0+1),nbrsil,1)
   CALL close(sil,Clsrew)
   Zi(isil0+nbrsil+1) = luset + 1
   CALL cdcbug(nhema1,100,Zi(isil0+1),nbrsil+1)
!
!     READ THE KDICT AND ECT DATA BLOCKS. WRITE A MODIFIED KDICT ON SCR2
!     WHICH INCLUDES THE INTERNAL GRID NUMBERS FOR EACH ELEMENT.
!     THE FORMAT FOR EACH RECORD ON SCR2 IS...
!     3-WORD RECORD HEADER
!        1  ELEMENT TYPE
!        2  NBR OF WORDS PER ENTRY( N )
!        3  NBR OF GRID POINTS PER ENTRY
!     N-WORD ELEMENT ENTRY
!        1  ELEMENT ID( INTERNAL NUMBER )
!        2  FORM OF COLUMN PARTITIONS( 1=RECT, 2=DIAG )
!        3  NUMBER OF TERMS PER COLUMN PARTITION
!        4  SCALAR CODE DEFINING DOF PER GRID POINT
!        5  GE
!        6  INTERNAL INDEX OF 1ST GRID POINT
!        7  GINO ADDRESS OF 1ST COLUMN PARTITION
!       ...
!       N-1 INTERNAL INDEX OF LAST GRID POINT
!        N  GINO ADDRESS OF LAST COLUMN PARTITION
!
!     NOTE...
!     GRID POINTS ARE IN SORT BY INTERNAL INDEX. ZERO INDICATES
!     MISSING GRID POINT. ANY ZERO-S ARE LAST IN LIST.
!
   CALL gopen(kdict,Zi(buf1),Rdrew)
   CALL gopen(ect,Zi(buf2),Rdrew)
   CALL gopen(scr2,Zi(buf3),Wrtrew)
 100  CALL read(*300,*100,kdict,buf(4),3,0,j)
   CALL cdcbug(nhema1,111,buf(4),3)
   DO
      CALL ectloc(*1500,ect,buf,i)
      CALL cdcbug(nhema1,112,buf,3)
      IF ( Elem(i+2)==buf(4) ) THEN
         buf(5) = buf(5) + buf(6)
         CALL write(scr2,buf(4),3,0)
         igrid = Elem(i+12)
         nbrgrd = Elem(i+9)
         nwdect = Elem(i+5)
         idict = nwdect + 1
         nwddct = buf(5) - buf(6)
         ngrid = igrid + nbrgrd - 1
         maxdct = max0(maxdct,buf(5))
         IF ( nbrgrd/=buf(6) ) THEN
            kerr = 114
            GOTO 1700
         ELSE
            DO
               CALL read(*200,*200,ect,Zi,nwdect,0,j)
               CALL cdcbug(nhema1,115,Zi,nwdect)
               CALL fread(kdict,Zi(idict),nwddct,0)
               CALL cdcbug(nhema1,116,Zi(idict),nwddct)
               DO j = igrid , ngrid
                  IF ( Zi(j)==0 ) Zi(j) = large
               ENDDO
               CALL sort(0,0,1,1,Zi(igrid),nbrgrd)
               DO j = igrid , ngrid
                  IF ( Zi(j)==large ) Zi(j) = 0
               ENDDO
               CALL cdcbug(nhema1,118,Zi(igrid),nbrgrd)
               CALL write(scr2,Zi(idict),nwddct-nbrgrd,0)
               iloc = idict + nwddct - nbrgrd
               DO j = 1 , nbrgrd
                  CALL write(scr2,Zi(igrid+j-1),1,0)
                  CALL write(scr2,Zi(iloc+j-1),1,0)
               ENDDO
               maxvec = max0(maxvec,Zi(idict+2)*prec)
            ENDDO
         ENDIF
      ELSE
         CALL skprec(ect,1)
      ENDIF
   ENDDO
 200  CALL skprec(kdict,1)
   CALL write(scr2,0,0,1)
   GOTO 100
 300  CALL close(kdict,Clsrew)
   CALL close(ect,Clsrew)
   CALL close(scr2,Clsrew)
   tt(1) = maxdct
   tt(2) = maxvec
   CALL cdcbug(nhema1,125,tt,2)
!
!     READ GPECT AND PREPARE THE SCR1 DATA BLOCK. FOR EACH GRID/SCALAR
!     POINT, TWO RECORDS ARE WRITTEN. THE 1ST CONTAINS 6 WORDS...
!       1  INTERNAL INDEX OF GRID/SCALAR POINT
!       2  DOF OF POINT (1=SCALAR, 6=GRID)
!       3  DOF OF EACH CONNECTED POINT (0 IF NO CONNECTED POINTS)
!       4  NUMBER OF CONNECTED POINTS
!       5  INDEX OF  1ST CONNECTED POINT
!       6  INDEX OF LAST CONNECTED POINT
!
!     THE 2ND RECORD IS A PACKED COLUMN WHICH CONTAINS A NON-ZERO TERM
!     FOR EACH CONNECTED POINT.
!
   Typin1 = 1
   Typou1 = 1
   Incr1 = 1
   Incr2 = 1
   CALL makmcb(mcb,scr1,nbrsil,1,1)
   ilook0 = nbrsil + 1
   IF ( ilook0+luset+1>=buf3 ) CALL mesage(-8,0,subnam)
   DO i = 1 , nbrsil
      j = Zi(isil0+i)
      Zi(ilook0+j) = i
   ENDDO
   CALL cdcbug(nhema1,131,Zi(ilook0+1),luset)
   CALL gopen(gpect,Zi(buf1),Rdrew)
   CALL gopen(scr1,Zi(buf2),Wrtrew)
   DO ii = 1 , nbrsil
      nbrcon = buf(4)
      minnbr = buf(5)
      maxnbr = buf(6)
      IF ( ii==1 ) THEN
         nbrcon = nbrsil
         minnbr = 1
         maxnbr = nbrsil
      ENDIF
      CALL fread(gpect,buf,2,0)
      buf(1) = ii
      buf(3) = 0
      buf(4) = 0
      buf(5) = large
      buf(6) = 0
      IF ( nbrcon/=0 ) THEN
         DO i = minnbr , maxnbr
            Zi(i) = 0
         ENDDO
      ENDIF
      DO
         CALL read(*350,*350,gpect,tt,3,0,i)
         CALL cdcbug(nhema1,134,tt,3)
         nbrgrd = iabs(tt(1)) - 2
         DO i = 1 , nbrgrd
            CALL fread(gpect,silnbr,1,0)
            j = Zi(ilook0+silnbr)
            IF ( Zs(j)==0 ) THEN
               buf(3) = max0(buf(3),Zi(isil0+j+1)-Zi(isil0+j))
               buf(4) = buf(4) + 1
               buf(5) = min0(buf(5),j)
               buf(6) = max0(buf(6),j)
               Zs(j) = 1.0
            ENDIF
         ENDDO
      ENDDO
 350  CALL write(scr1,buf,6,1)
      CALL cdcbug(nhema1,138,buf,6)
      IF ( buf(4)==0 ) THEN
!
!     HERE IF PIVOT HAS NO CONNECTED POINTS
!
         mcb(2) = mcb(2) + 1
      ELSE
!
!     PACK COLUMN FOR POINT WITH CONNECTED POINTS
!
         Ii1 = buf(5)
         Jj1 = buf(6)
         CALL cdcbug(nhema1,139,Zi(Ii1),Jj1-Ii1+1)
         CALL pack(Zs(Ii1),scr1,mcb)
      ENDIF
!
!     CLOSE FILES
!
   ENDDO
   CALL close(gpect,Clsrew)
   CALL close(scr1,Clsrew)
   CALL wrttrl(mcb)
!
!     ALLOCATE STORAGE FOR MAXIMUM COLUMN OF ELEMENT MATRIX
!     AND MAXIMUM ENTRY FROM MODIFIED KDICT( SCR2 )
!
   idict = maxvec + 1
   igrid = idict + 5
   ipvt = idict + maxdct
   lcore = even(buf2) - 1
!
!
!     BEGIN A PASS BY OPENING SCR1 AND SETTING ALLOCATION POINTERS
!
!
 400  CALL gopen(scr1,Zi(buf1),openr)
   ii = ipvt
   jj = lcore
   DO
!
!     BEGIN A PIVOT ALLOCATION BY READING PIVOT CONTROL BLOCK FROM SCR1
!
      tt(1) = ii
      tt(2) = jj
      CALL cdcbug(nhema1,160,tt,2)
      IF ( ii+lpcb>=jj ) EXIT
      CALL fread(scr1,Zi(ii),6,1)
      silnbr = Zi(ii)
      Zi(ii+6) = 0
      Zi(ii+7) = 0
      IF ( Zi(ii+3)/=0 ) THEN
!
!     ATTEMPT TO ALLOCATE SPACE FOR CONNECTED GRID VECTOR
!     AND FOR MATRICES CONNECTED TO THE PIVOT
!
         nwdcgv = Zi(ii+5) - Zi(ii+4) + 1
         nwdmat = prec*Zi(ii+1)*Zi(ii+2)*Zi(ii+3)
         IF ( ii+lpcb>=jj-nwdcgv-nwdmat ) THEN
!
!     HERE IF CURRENT PIVOT CANNOT BE ALLOCATED -- MAKE SURE AT LEAST
!     ONE PIVOT HAS BEEN ALLOCATED.
!
            CALL bckrec(scr1)
            EXIT
         ELSE
            imat = jj - nwdmat
            Zi(ii+6) = imat - even(nwdcgv)
            Zi(ii+7) = imat
            jj = Zi(ii+6)
            nmat = imat + nwdmat - 1
            DO i = imat , nmat
               Zs(i) = 0
            ENDDO
            icgvec = jj
            ncgvec = icgvec + nwdcgv - 1
!
!     UNPACK CONNECTED GRID VECTOR. CONVERT NON-ZERO POSITIONS TO
!     RELATIVE POINTERS (IN PRECISION OF PROBLEM) TO THE CORRESPONDING
!     1ST TERM OF THE ELEMENT MATRIX
!
            Ii2 = Zi(ii+4)
            Jj2 = Zi(ii+5)
            ntrmec = Zi(ii+2)
            kk = 1
            Typin2 = 1
            CALL unpack(*1600,scr1,Zs(icgvec))
            DO i = icgvec , ncgvec
               IF ( Zi(i)/=0 ) THEN
                  Zi(i) = kk
                  kk = kk + ntrmec
               ENDIF
            ENDDO
            CALL cdcbug(nhema1,174,Zi(ii),8)
            CALL cdcbug(nhema1,175,Zi(icgvec),nwdcgv)
            IF ( kk-1/=Zi(ii+2)*Zi(ii+3) ) THEN
               kerr = 174
               GOTO 1700
            ENDIF
         ENDIF
      ENDIF
!
!     TEST FOR LAST PIVOT. IF NOT, TRY TO ALLOCATE ANOTHER PIVOT
!
      IF ( silnbr==nbrsil ) THEN
!
!     HERE WHEN LAST PIVOT HAS BEEN READ AND ALLOCATED
!
         last = .TRUE.
         opcls = Clsrew
         npvt = ii
         GOTO 500
      ELSE
         ii = ii + lpcb
      ENDIF
   ENDDO
   IF ( ii==ipvt ) CALL mesage(-8,0,subnam)
   npvt = ii - lpcb
!
!
!     CLOSE SCR1, OPEN SCR2 AND KELEM. PREPARE TO ASSEMBLE
!     STRUCTURAL MATRIX FOR THOSE PIVOTS CURRENTLY ALLOCATED.
!
!
 500  CALL close(scr1,opcls)
   CALL gopen(scr2,Zi(buf1),Rdrew)
   CALL gopen(kelem,Zi(buf2),Rdrew)
!
!     READ HEADER FOR CURRENT ELEMENT TYPE FROM SCR2
!
 600  CALL read(*1400,*600,scr2,tt,3,0,i)
   CALL cdcbug(nhema1,230,tt,3)
   nwddct = tt(2)
   ngrid = igrid + 2*(tt(3)-1)
 700  DO
!
!     READ AN ELEMENT DEFINITION. IF ANY GRID POINT IS IN CURRENT
!     ALLOCATION, PREPARE TO PROCESS IT.
!
      CALL read(*600,*600,scr2,Zi(idict),nwddct,0,i)
      CALL cdcbug(nhema1,240,Zi(idict),nwddct)
      DO i = igrid , ngrid , 2
         IF ( Zi(i)>=Zi(ipvt) .AND. Zi(i)<=Zi(npvt) ) GOTO kfact
      ENDDO
   ENDDO
 800  factor = 1.0
   GOTO 1200
 900  factor = Zs(idict+4)
   GOTO 1200
 1000 factor = Wtmass
   GOTO 1200
 1100 factor = Wtmass*Zs(idict+4)
!
!     DECODE RELATIVE COLUMN NUMBERS
!
 1200 IF ( oldcod/=Zi(idict+3) ) THEN
      icode = Zi(idict+3)
      CALL decode(icode,scalas,nsca)
      oldcod = Zi(idict+3)
   ENDIF
!
!     READ EACH COLUMN OF THE ELEMENT MATRIX.
!     ADD IT TO THE STRUCTURAL MATRIX.
!
   nwdcol = prec*Zi(idict+2)
   IF ( Zi(idict+1)==2 ) nwdcol = prec
 1300 ii = ipvt + (Zi(i)-Zi(ipvt))*lpcb
   tt(1) = i
   tt(2) = Zi(i)
   tt(3) = nsca
   CALL cdcbug(nhema1,252,tt,3)
   CALL filpos(kelem,Zi(i+1))
   icgvec = Zi(ii+6)
   imat = Zi(ii+7)
   DO j = 1 , nsca
      CALL fread(kelem,Zi,nwdcol,0)
      CALL cdcbug(nhema1,254,Zi,nwdcol)
      IF ( prec==1 ) CALL ema1s(j,nsca,scalas,Zi(ii),Zi(idict),Zi(icgvec),Zi(imat),Zi,factor)
      IF ( prec==2 ) CALL ema1d(j,nsca,scalas,Zi(ii),Zi(idict),Zi(icgvec),Zi(imat),Zi,factor)
   ENDDO
   DO WHILE ( i/=ngrid )
      i = i + 2
      IF ( Zi(i)>=Zi(ipvt) .AND. Zi(i)<=Zi(npvt) ) GOTO 1300
   ENDDO
   GOTO 700
!
!     ALL COLUMNS OF STRUCTURAL MATRIX NOW ALLOCATED ARE COMPLETE.
!     OPEN KGG AND PACK COLUMNS.
!
 1400 CALL close(scr2,Clsrew)
   CALL close(kelem,Clsrew)
   CALL gopen(kgg,Zi(buf1),openw)
   DO ii = ipvt , npvt , lpcb
      dof = Zi(ii+1)
      dofg = Zi(ii+2)
      nbrcon = Zi(ii+3)
      icgvec = Zi(ii+6)
      imat = Zi(ii+7)
      Ii1 = Zi(ii+4)
      Ii2 = Zi(ii+5)
      kk = imat
      CALL cdcbug(nhema1,260,Zi(imat),((Ii2-Ii1+1)*(dof*dofg)))
!
!     PACK COLUMNS WITH BLDPK
!
      DO jj = 1 , dof
         CALL bldpk(prec,prec,kgg,block,1)
         IF ( nbrcon/=0 ) THEN
            i = icgvec
            DO j = Ii1 , Ii2
               IF ( Zi(i)/=0 ) THEN
                  k = Zi(isil0+j)
                  n = k + min0(dofg,Zi(isil0+j+1)-Zi(isil0+j)) - 1
                  ll = kk
                  DO silnbr = k , n
                     CALL bldpki(Zs(ll),silnbr,kgg,block)
                     ll = ll + prec
                  ENDDO
                  kk = kk + dofg*prec
               ENDIF
               i = i + 1
            ENDDO
         ENDIF
         CALL bldpkn(kgg,block,mcbkgg)
      ENDDO
   ENDDO
   CALL close(kgg,opcls)
!
!     TEST FOR COMPLETION OF LAST PASS
!
   IF ( last ) THEN
!
!     KGG NOW COMPLETE -- WRITE ITS TRAILER.
!
      CALL wrttrl(mcbkgg)
      RETURN
   ELSE
      openr = Rd
      openw = Wrt
      GOTO 400
   ENDIF
!
!     FATAL ERRORS
!
 1500 kerr = 112
   GOTO 1700
 1600 kerr = 172
!
!     PROCESS LOGIC ERROR
!
 1700 WRITE (Nout,99001) Sfm , kerr
99001 FORMAT (A25,' 3102, EMA1 LOGIC ERROR',I4)
   IF ( Mach==2 .OR. Mach==5 .OR. Mach==21 ) kerr = -kerr
   CALL gperr(subnam,kerr)
99002 FORMAT (1H ,7I10)
END SUBROUTINE ema1
