!*==smcph1.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE smcph1(Zi,Zr,Zd)
   USE i_smcomx
   USE c_logout
   USE c_names
   USE c_ntime
   USE c_type
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(4) :: Zi
   REAL , DIMENSION(4) :: Zr
   REAL*8 , DIMENSION(4) :: Zd
!
! Local variable declarations rewritten by SPAG
!
   CHARACTER(4) , DIMENSION(2) :: cname
   CHARACTER(14) , DIMENSION(4) , SAVE :: ctype
   REAL :: dcr , dsc , dsr , minds
   LOGICAL :: frstval
   INTEGER :: i , iacrow , icurcol , idbind , ifirstc , inddir , indexr , indexz , indxv , iprec , irflag , irow , irval , itcols , &
            & itest , itwrds , ivd , ivval , izeros , k , kpos , l , l45 , len , lrow , maxrow , maxtes , minmum , mrow , mstr ,    &
            & mterms , mtype , nexcol , nrow , nrvals , nterms , nv , nvvals , nwords
   INTEGER , DIMENSION(4) :: itemp
   INTEGER , DIMENSION(2) :: name
   REAL*8 , DIMENSION(10) :: xnd
   INTEGER :: spag_nextblock_1
!
! End of declarations rewritten by SPAG
!
!
!  KTYPE   = TYPE (1-RS,2-RD,3-CS,4-CD) OF LOWER TRIANGULAR MATRIX
!  KPREC   = PRECISION (1-SINGL, 2-DOUBL) OF LOWER TRIANGULAR MATRIX
!  MAXROW  = HIGHEST ROW NUMBER REFERENCED THUS FAR IN PROCESSING
!            A GIVEN COLUMN - NEEDED TO DETERMINE CREATED TERMS DURING
!            DECOMPOSITION
!  MAXINLOP= MAXIMUM TERMS FOR ANY GIVEN INNER LOOP
!  MAXNCOL = MAXIMUM NUMBER OF COLUMNS REFERENCED BY ANY GIVEN COLUMN
!  LASCOL  = LAST COLUMN NUMBER OF MATRIX TO BE DECOMPOSED
!  NEXCOL  = FIRST NON-ZERO TERM IN CURRENT PIVOT COLUMN BELOW DIAGONAL
!            USED TO DETERMINE THE NEXT PIVOT COLUMN WHERE THE ROW
!            WILL BE NEEDED.
!  ICURCOL = CURRENT COLUMN BEING PROCESSED
!  MXRECL  = MAXIMUM SIZE IN WORDS OF ANY ONE RECORD WRITTEN TO THE
!            SPILL FILE
!  NSPILL  = NUMBER OF COLUMNS WRITTEN TO THE SPILL FILE
!
   !>>>>EQUIVALENCE (Ddr,Dsr) , (Ddc,Dsc)
   !>>>>EQUIVALENCE (Mindd,Minds) , (Xns,Xnd)
   !>>>>EQUIVALENCE (Mblk(6),Mterms) , (Mblk(5),Mstr)
   !>>>>EQUIVALENCE (Mblk(4),Mrow) , (Mblk(2),Mtype)
   !>>>>EQUIVALENCE (cname,name)
   DATA ctype/'REAL SINGLE   ' , 'REAL DOUBLE   ' , 'COMPLEX SINGLE' , 'COMPLEX DOUBLE'/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!   open core is allocated as follows for phase1 of the decomposition
!
!       -------------------------------
!       zi(1) -  Beginning of directory for in-memory column data
!       Directory (4,n) , n=number of columns of matrix
!                  (1,i) = index to active rows and terms within memory
!                  (2,i) = first column data needed for this pivot
!                  (3,i) = last pivot column to use this data
!                  (4,i) = savpos position pointer for data spilled to a
!                          scratch file
!       -------------------------------
!       zi(iacrow) - Beginning of active row vector.
!       Vector for determining active rows for each column, n words
!       Each row value will define the next column where the row value
!       is next needed for calculation of the lll matrix.
!       -------------------------------
!       zi(IRVAL) - Stagging area for storing data
!       Defines the values in the next section of open core, 2*n
!                  (1,i) = row number
!                  (2,i) = number of consecutive terms beginning at row
!       This section and the next section are staging areas for storing
!       of rows and row values of columns to be pointed to by the directory
!       in the first part of open core.
!       -------------------------------
!       zi(IVVAL)
!       Row values of column as defined by previous section, n*iprec words
!       -------------------------------
!       zi(idbase)
!       Memory for rows and terms of columns as pointed to by directory
!       in the first part of open core.  This data is loaded from the
!       bottom up to allow for better management of open core in
!       subroutine smcph2 which is called after this subroutine.
!       The format for the storage of this data is as follows:
!          (index from directory above points to the first word of the
!           data that follows)
!               1.  Column number
!               2.  Length of active row section (m*2), m=number of
!                   repeats of contents of words 5 and 6 below.
!               3.  Total number of words in this block of allocation
!               4.  Length of values section
!               5.  row number
!               6.  number of consecutive values beginning at this row
!                   (words 5 and 6 repeat m times)
!           5+2*m.  row value for first row
!     5+2*m+iprec.  next row value (iprec=1,2,4)
!   5+2*m+iprec*l.  last row value for column (l=total values)
!       -------------------------------
!       zi(ibuf2)
!       Buffer for spill file if all column values can not be kept in memory
!       -------------------------------
!       zi(ibuf1)
!       Buffer for input matrix file to be decomposed
!       -------------------------------
!
!      CALL AUDIT ( 'BEGIN   ', 1 )
!      CALL AUDIT ( 'SMCPH1  ', 1 )
         CALL fname(mcb,name)
         ncol = mcb(2)
         memcoln = 0
         mxrecl = 0
         maxnac = 0
         maxnar = 0
         iprec = prc(mcb(5))
         ktype = mcb(5)
         IF ( isprec==2 .AND. ktype==1 ) ktype = 2
         IF ( isprec==2 .AND. ktype==3 ) ktype = 4
         IF ( isprec==1 .AND. ktype==2 ) ktype = 1
         IF ( isprec==1 .AND. ktype==4 ) ktype = 3
         IF ( ktype==1 .OR. ktype==3 ) kprec = 1
         IF ( ktype==2 .OR. ktype==4 ) kprec = 2
         ivwrds = words(ktype)
         iacrow = 4*ncol + 1
         irval = iacrow + ncol
         ivval = irval + 2*ncol
!
! ENSURE THAT IVVAL IS ON A DOUBLE WORD BOUNDARY
!
         IF ( mod(ivval,2)==0 ) ivval = ivval + 1
         idbase = ivval + ivwrds*ncol
!
! ENSURE THAT IDBASE IS ON A DOUBLE WORD BOUNDARY
!
         IF ( mod(idbase,2)==0 ) idbase = idbase + 1
         IF ( lcore<(idbase+2*isysbf) ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ibuf1 = lcore - isysbf
         ibuf2 = ibuf1 - isysbf
         idbmax = ibuf2 - 1
!
! ENSURE THAT IDBMAX IS ON A DOUBLE WORD BOUNDARY
!
         IF ( mod(idbmax,2)==0 ) idbmax = idbmax - 1
         idbind = idbmax
         CALL open(*40,mcb,Zi(ibuf1),rdrew)
         CALL skprec(mcb,1)
         mblk(1) = mcb(1)
         lll(2) = mcb(2)
         lll(3) = mcb(2)
         lll(4) = 4
         lll(5) = ktype
         lll(6) = 0
         lll(7) = lshift(1,nbpw-2-(nbpw-32))
         icurcol = 1
         opnscr = .FALSE.
         nspill = 0
         maxrow = 0
         maxinlop = 0
         maxncol = 0
         lascol = 0
         power = 0
         IF ( kprec/=2 ) THEN
            dsr = 1.0
            dcr = 0.0
            minds = 1.0E+25
         ELSE
            ddr = 1.0D0
            ddc = 0.0D0
            mindd = 1.0D+25
         ENDIF
         moblk(1) = lll(1)
         moblk(2) = ktype
         moblk(3) = 1
!
! ZERO OUT THE ACTIVE COLUMN VECTOR
!
         DO i = 1 , ncol
            Zi(iacrow+i-1) = 0
         ENDDO
         len = ncol*4
!
! ZERO OUT THE DIRECTORY
!
         DO i = 1 , len
            Zi(i) = 0
         ENDDO
         spag_nextblock_1 = 2
      CASE (2)
         nterms = 0
         frstval = .TRUE.
         indexr = irval
         indexv = ivval
         indexvd = (indexv/2) + 1
         mblk(8) = -1
         nexcol = 0
         inddir = (icurcol-1)*4 + 1
         spag_nextblock_1 = 3
      CASE (3)
         SPAG_Loop_1_1: DO
            CALL getstr(*20,mblk)
            IF ( icurcol<=(mrow+mterms-1) ) THEN
!
! SAVE SOME OR ALL OF THE TERMS IN THIS STRING
! IF THIS IS NOT THE FIRST STRING TO PROCESS, THEN SAVE ALL VALUES
!
               iskip = 0
               IF ( .NOT.frstval ) THEN
!
! CHECK TO SEE IF CURRENT STRING IS AN EXTENSION OF PREVIOUS STRING
!
                  IF ( (Zi(indexr)+Zi(indexr+1))==mrow ) EXIT SPAG_Loop_1_1
!
! NO, MUST CREATE NEW POINTER FOR VALUES
! BUT FIRST, CHECK FOR PROVIDING FOR COMPUTED TERMS OF
! PREVIOUS PIVOT COLUMNS
!
                  irow1 = Zi(indexr) + Zi(indexr+1)
                  irown = mrow - 1
                  irflag = 1
                  spag_nextblock_1 = 9
                  CYCLE SPAG_DispatchLoop_1
               ELSE
!
! CHECK IF THIS IS THE FIRST TERM OF THE PIVOT COLUMN
!
                  IF ( Zi(inddir+1)==0 ) Zi(inddir+1) = mrow
!
! OTHERWISE, CHECK IF ALL TERMS OR ONLY SOME ARE TO BE SAVED
!
                  frstval = .FALSE.
                  IF ( icurcol==mrow ) THEN
                     IF ( mterms>1 ) nexcol = mrow + 1
                     Zi(indexr) = mrow
                     Zi(indexr+1) = mterms
                     nterms = mterms
                     spag_nextblock_1 = 5
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
!
! CHECK FOR ZERO ON THE DIAGONAL
!
                     IF ( icurcol<mrow ) THEN
                        spag_nextblock_1 = 11
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
!
! SKIP ALL TERMS BEFORE THE CURRENT PIVOT COLUMN
!
                     iskip = icurcol - mrow
                     Zi(indexr) = icurcol
                     nterms = mterms - iskip
                     Zi(indexr+1) = nterms
                     IF ( (mterms-iskip)>1 ) nexcol = icurcol + 1
                     spag_nextblock_1 = 5
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
            ELSE
!
! ALL ROW TERMS ARE BEFORE CURRENT PIVOT COLUMN; SKIP THESE TERMS
! AND GET NEXT STRING.
! CHECK TO SEE IF THIS IS THE FIRST TERM OF THE PIVOT COLUMN
!
               IF ( Zi(inddir+1)==0 ) Zi(inddir+1) = mrow
               CALL endget(mblk)
            ENDIF
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 4
      CASE (4)
!
! TERMS ARE AN EXTENSION OF EXISTING DATA, CHANGE THE NUMBER OF TERMS
!
         Zi(indexr+1) = Zi(indexr+1) + mterms
         nterms = nterms + mterms
         IF ( nexcol==0 ) nexcol = mrow
         spag_nextblock_1 = 5
      CASE (5)
         CALL smcrtr(Zr,Zd)
!
! SET ACTIVE COLUMN ROW NUMBERS FOR POSSIBLE EXPANDED TERMS
!
         irow1 = mrow + iskip
         irown = irow1 + mterms - 1 - iskip
         DO k = irow1 , irown
            Zi(iacrow+k-1) = nexcol
         ENDDO
!
! GO AND GET ADDITIONAL STRINGS IF ANY
!
         CALL endget(mblk)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
! END OF READING CURRENT COLUMN, CHECK IF DIAGONAL TERM FOUND
!
!      PRINT *,' SMCPH1,ICURCOL,NEXCOL,MAXROW=',ICURCOL,NEXCOL,MAXROW
 20      IF ( frstval ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
! SEE IF ANY COMPUTED TERMS FROM PREVIOUS PIVOT COLUMNS ARE TO BE
! ADDED ONTO THE END OF THE CURRENT ACTIVE ROWS FOR THIS COLUMN
!
         lrow = Zi(indexr) + Zi(indexr+1) - 1
         IF ( lrow>maxrow ) maxrow = lrow
         irow1 = lrow + 1
         irown = maxrow
         irflag = 2
!      PRINT *,' B1050,ICURCOL,IROWN,IROW1=',ICURCOL,IROWN,IROW1
         IF ( irown>=irow1 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
!
! SET UP DIRECTORY AND SAVE DATA EITHER
! IN MEMORY OR ON SPILL FILE
!
!
! RECOMPUTE LROW IN CASE NEW TERMS WERE ADDED FROM PREVIOUS PIVOT COLUMNS
!
         lrow = Zi(indexr) + Zi(indexr+1) - 1
!
! INDEXR POINTS TO CURRENT DIRECTORY ENTRY BUT INDEXV POINTS TO NEXT
! AVAILABLE POSITION FOR STORING TERMS
!
         nrvals = indexr - irval + 2
         nvvals = indexv - ivval
         nwords = nrvals + nvvals + 4
!
! SAVE DATA IN MEMORY AND SET DIRECTORY ACCORDINGLY
!
         itest = idbind - nwords + 1
!
! MAKE SURE ITEST IS ON DOUBLE WORD BOUNDARY
!
         IF ( mod(itest,2)==0 ) itest = itest - 1
!
! CHECK TO SEE IF THERE IS SUFFICIENT MEMORY
!
         maxnar = max0(nrvals,maxnar)
         IF ( itest<idbase ) THEN
            IF ( .NOT.(opnscr) ) THEN
               opnscr = .TRUE.
               CALL open(*60,iscr1,Zi(ibuf2),wrtrew)
            ENDIF
!
! NO MORE MEMORY, SAVE COLUMN DATA TO SPILL FILE, KEEP RECORD POSITION
!
            itemp(1) = icurcol
            itemp(2) = nrvals
            itemp(3) = 0
            itemp(4) = nterms
            CALL write(iscr1,itemp,4,0)
            CALL savpos(iscr1,kpos)
            CALL write(iscr1,Zi(irval),indexr-irval+2,0)
            CALL write(iscr1,Zi(ivval),indexv-ivval+2,1)
            Zi(inddir) = 0
            Zi(inddir+3) = kpos
            nspill = nspill + 1
            itest = nrvals + nvvals + 4
            IF ( itest>mxrecl ) mxrecl = itest
         ELSE
            idbind = itest
            Zi(inddir) = idbind
            Zi(inddir+3) = 0
            Zi(idbind) = icurcol
            Zi(idbind+1) = nrvals
            Zi(idbind+2) = nwords
            Zi(idbind+3) = nterms
            idbind = idbind + 3
            DO k = 1 , nrvals
               Zi(idbind+k) = Zi(irval+k-1)
            ENDDO
            idbind = idbind + nrvals
            IF ( kprec==2 ) THEN
               indxv = idbind/2
               nv = nvvals/2
               ivd = ivval/2
               DO k = 1 , nv
                  Zd(indxv+k) = Zd(ivd+k)
               ENDDO
            ELSE
               DO k = 1 , nvvals
                  Zi(idbind+k) = Zi(ivval+k-1)
               ENDDO
            ENDIF
            idbind = idbind + nvvals - nwords
            lascol = icurcol
            memcoln = icurcol
            itest = nrvals + nvvals + 4
            IF ( itest>mxrecl ) mxrecl = itest
         ENDIF
         lrow = Zi(indexr) + Zi(indexr+1) - 1
!
! SAVE LAST PIVOT COLUMN FOR WHICH DATA IN THIS COLUMN IS USED
!
         IF ( nterms>maxnac ) maxnac = nterms
         Zi(inddir+2) = lrow
         ifirstc = Zi(inddir+1)
         maxtes = (icurcol-ifirstc+1)
         IF ( maxtes>maxncol ) maxncol = maxtes
         maxtes = nterms*(icurcol-ifirstc)
         IF ( maxtes>maxinlop ) maxinlop = maxtes
!
! CHECK TO DETERMINE IF ALL COLUMNS HAVE BEEN PROCESSED
!
         IF ( icurcol>=ncol ) THEN
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
! CHECK IF ONLY ONE TERM IN THIS COLUMN
!
         IF ( nexcol==0 ) THEN
!
! MUST FIND FIRST NON-ZERO TERM FOLLOWING THE CURRENT PIVOT
!
            DO k = icurcol + 1 , ncol
               IF ( Zi(iacrow+k)==icurcol ) THEN
                  nexcol = k
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            WRITE (nout,99001) icurcol
99001       FORMAT (' SYMMETRIC DECOMPOSITION FOUND NO TERMS BEING ',' CONNECTED TO DIAGONAL ON COLUMN ',I8)
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
!      PRINT *,' AFTER 2005,ICURCOL,NEXCOL=',ICURCOL,NEXCOL
!
! UPDATE ACTIVE ROWS IN COLUMN VECTOR FOR ALL TERMS OF THIS COLUMN
!
         len = irval + nrvals - 1
         DO k = irval , len , 2
            irow = Zi(k)
            nrow = irow + Zi(k+1) - 1
            DO l = irow , nrow
               Zi(iacrow+l-1) = nexcol
            ENDDO
         ENDDO
         DO l = icurcol + 1 , maxrow
            IF ( Zi(iacrow+l-1)==icurcol ) Zi(iacrow+l-1) = nexcol
         ENDDO
         spag_nextblock_1 = 8
      CASE (8)
!
! END OF CURRENT COLUMN, PREPARE FOR NEXT COLUMN
!
!      write ( nout, 901 ) icurcol
!901   format(20x,' Active rows after processing column ',i10)
!      do 2040 l = 1, ncol
!      write ( nout, 902 ) l, zi(iacrow+l-1)
!902   format(' Row, next reference =',2i7)
!2040  continue
!      write ( nout, 903 )
!903   format(20x, ' Directory',/,
!     &' Column  Memory Index   First Used    Last Used    Savpos')
!      do 2050 l = 1, ncol
!      ind = ( l-1 ) * 4 + 1
!      write ( nout, 904 ) l, zi(ind), zi(ind+1), zi(ind+2), zi(ind+3)
!904   format( i7, i14, i13, i13, i9)
!2050  continue
         icurcol = icurcol + 1
         inddir = (icurcol-1)*4 + 1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      CASE (9)
!
! THE FOLLOWING IS AN INTERNAL ROUTINE TO ADD COMPUTED TERMS RESULTING
! FROM THE PROCESSING OF PREVIOUS PIVOT COLUMNS INTO THE CURRENT ACTIVE
! ROWS FOR THE CURRENT COLUMN
!
         DO k = irow1 , irown
            IF ( Zi(iacrow+k-1)>=icurcol ) THEN
               IF ( nexcol==0 ) nexcol = k
!
! NEED TO ADD THIS TERM TO THE ACTIVE ROWS
! CHECK TO SEE IF THIS TERM IS AN EXTENSION OF CURRENT TERMS
!
               IF ( (Zi(indexr)+Zi(indexr+1))==k ) THEN
!
! JUST ADD TO THE NUMBER OF CONSECUTIVE VALUES FOR CURRENT ROW
!
                  Zi(indexr+1) = Zi(indexr+1) + 1
                  nterms = nterms + 1
               ELSE
!
! NO, NEED TO CREATE ANOTHER POINTER
!
                  indexr = indexr + 2
                  Zi(indexr) = k
                  Zi(indexr+1) = 1
                  nterms = nterms + 1
               ENDIF
!
! NOW, ZERO OUT ROW VALUE
!
               IF ( ktype==2 ) THEN
!
! TYPE IS REAL DP
!
                  Zd(indexvd) = 0.D0
                  indexvd = indexvd + 1
                  indexv = indexv + 2
               ELSEIF ( ktype==3 ) THEN
!
! TYPE IS COMPLEX SP
!
                  Zr(indexv) = 0.
                  Zr(indexv+1) = 0.
                  indexv = indexv + 2
               ELSEIF ( ktype==4 ) THEN
!
! TYPE IS COMPLEX DP
!
                  Zd(indexvd) = 0.D0
                  Zd(indexvd+1) = 0.D0
                  indexvd = indexvd + 2
                  indexv = indexv + 4
               ELSE
!
! TYPE IS REAL SP
!
                  Zr(indexv) = 0.
                  indexv = indexv + 1
               ENDIF
            ENDIF
         ENDDO
         IF ( irflag==1 ) THEN
!
! NOW CHECK IF THE ADDED TERMS ARE PART OF SAME STRING AS THAT JUST
! GOTTEN FROM GETSTR CALL
!
            IF ( (Zi(indexr)+Zi(indexr+1))==mrow ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
! NEW STRING TO BE DEFINED FOR THE CURRENT TERMS FROM GETSTR
!
            indexr = indexr + 2
            Zi(indexr) = mrow
            Zi(indexr+1) = mterms
            nterms = nterms + mterms
            IF ( nexcol==0 ) nexcol = mrow
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( irflag==2 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 10
      CASE (10)
!
! INSUFFICIENT MEMORY
!
         minmum = ncol*7 + 2*ncol*ivwrds + 2*isysbf
         WRITE (nout,99002) ufm , mcb(1) , cname , ncol , ktype , lcore , minmum
99002    FORMAT (1X,A23,/,' INSUFFICIENT MEMORY TO DECOMPOSE MATRIX IN ',I4,' FILE NAME=',2A4,/,' NUMBER OF COLUMNS=',I8,' TYPE=',  &
               & I2,' MEMORY AVAILABLE =',I16,/,' MINIMUM REQUIRED IS =',I16)
!      CALL MESAGE ( -8, 0, 0 )
         ierror = 1
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
 40      CALL fname(mcb,name)
         WRITE (nout,99003) ufm , mcb(1) , cname
99003    FORMAT (1X,A23,/,' SMCPH1 UNABLE TO OPEN FILE ',I4,' NAME= ',2A4)
         ierror = 2
         CALL mesage(-61,0,0)
 60      CALL fname(iscr1,name)
         WRITE (nout,99004) ufm , iscr1 , cname
99004    FORMAT (1X,A23,/,' SMCPH1 UNABLE TO OPEN FILE ',I4,' NAME= ',2A4)
         ierror = 2
         CALL mesage(-61,0,0)
         spag_nextblock_1 = 11
      CASE (11)
!
! ZERO ON DIAGONAL, TERMINATE DECOMPOSITION BUT FIRST SCAN REST OF
! MATRIX TO DETERMINE OTHER COLUMNS WITH ZERO DIAGONALS.
!
         ierror = 7
         izeros = 1
         indexz = 0
         IF ( .NOT.(frstval) ) THEN
            CALL endget(mblk)
            CALL skprec(mblk,1)
         ENDIF
 80      SPAG_Loop_1_3: DO
            indexz = indexz + 1
            Zi(indexz) = icurcol
            DO
               icurcol = icurcol + 1
               IF ( icurcol>ncol ) THEN
                  CALL close(mcb,rew)
                  CALL close(iscr1,rew)
                  WRITE (nout,99005) ufm , cname , (Zi(k),k=1,indexz)
99005             FORMAT (A23,' 3097, SYMMETRIC DECOMPOSITION OF DATA BLOCK ',2A4,                                                  &
                         &' ABORTED BECAUSE THE FOLLOWING COLUMNS ARE SINGULAR -',/,(5X,20I6,/))
                  RETURN
               ELSE
                  mblk(8) = -1
                  SPAG_Loop_3_2: DO
                     CALL getstr(*80,mblk)
                     CALL endget(mblk)
                     IF ( icurcol>=mrow .AND. icurcol<=mrow+mterms-1 ) THEN
                        CALL skprec(mblk,1)
                        EXIT SPAG_Loop_3_2
                     ELSEIF ( mrow>icurcol ) THEN
                        CALL skprec(mblk,1)
                        CYCLE SPAG_Loop_1_3
                     ENDIF
                  ENDDO SPAG_Loop_3_2
               ENDIF
            ENDDO
            EXIT SPAG_Loop_1_3
         ENDDO SPAG_Loop_1_3
         spag_nextblock_1 = 12
      CASE (12)
!      CALL SMCHLP
!      CALL SMCDMP ( ZI, ZR, ZD )
         CALL close(mcb,rew)
         itwrds = idbmax - idbind
         itcols = ncol - nspill
         CALL sswtch(45,l45)
         IF ( l45/=0 ) THEN
            WRITE (lout,99006) itcols , nspill , maxnac , maxncol , maxinlop , itwrds
99006       FORMAT (/,14X,' STATISTICS FOR SYMMETRIC DECOMPOSITION OF FILE ',/,/,7X,                                                &
                   &' COLUMNS CONTAINED IN MEMORY                         =',I16,/,7X,                                              &
                   &' COLUMNS WRITTEN TO SPILL FILE                       =',I16,/,7X,                                              &
                   &' MAX. NO. OF ACTIVE ROWS FOR ANY ACTIVE COLUMN       =',I16,/,7X,                                              &
                   &' MAX. NUMBER OF COLUMNS REFERENCED BY A PIVOT COLUMN =',I16,/,7X,                                              &
                   &' MAX. TERMS FOR ANY GIVEN INNER LOOP                 =',I16,/,7X,                                              &
                   &' TOTAL WORDS IN OPEN CORE USED FOR COLUMN DATA       =',I16)
            WRITE (lout,99007) 'INPUT ' , cname , ctype(mcb(5))
            CALL fname(lll,name)
            WRITE (lout,99007) 'OUTPUT' , cname , ctype(ktype)
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99007 FORMAT (8X,A6,' FILE: ',2A4,'      DATA TYPE= ',A14)
!      CALL AUDIT( 'SMCPH1  ', 2 )
END SUBROUTINE smcph1
