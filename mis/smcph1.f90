
SUBROUTINE smcph1(Zi,Zr,Zd)
   IMPLICIT NONE
   INCLUDE 'SMCOMX.COM'
   REAL Cdp , Csp , Diag , Eofnrw , Rdnrw , Rdp , Rdrew , Rect , Rsp , Sqr , Sym , Tmbpak , Tmgstr , Tmio , Tmipak , Tml(4) ,       &
      & Tmpak , Tmpstr , Tmt(4) , Tmupak , Uprtri , Wrt , Wrtrew , Xns(10)
   INTEGER Lout , Lowtri , Nitems , Norew , Prc(2) , Rew , Rlcmpx(4) , Words(4), Mrow, Mstr, Mterms, Mtype
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   DOUBLE PRECISION Xnd(10)
   COMMON /logout/ Lout
   COMMON /names / Rdnrw , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp , Csp , Cdp , Sqr , Rect , Diag , Lowtri ,       &
                 & Uprtri , Sym
   COMMON /ntime / Nitems , Tmio , Tmbpak , Tmipak , Tmpak , Tmupak , Tmgstr , Tmpstr , Tmt , Tml
   COMMON /type  / Prc , Words , Rlcmpx
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ Xns
   DOUBLE PRECISION Zd(4)
   INTEGER Zi(4)
   REAL Zr(4)
   CHARACTER*4 cname(2)
   CHARACTER*14 ctype(4)
   REAL dcr, dsr, dsc, minds
   LOGICAL frstval
   INTEGER i , iacrow , icurcol , idbind , ifirstc , inddir , indexr , indexz , indxv , iprec , irflag , irow , irval , itcols ,    &
         & itemp(4) , itest , itwrds , ivd , ivval , izeros , k , kpos , l , l45 , len , lrow , maxrow , maxtes , minmum , name(2) ,&
         & nexcol , nrow , nrvals , nterms , nv , nvvals , nwords
   INTEGER lshift
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
   EQUIVALENCE (Ddr,Dsr) , (Ddc,Dsc)
   EQUIVALENCE (Mindd,Minds) , (Xns,Xnd)
   EQUIVALENCE (Mblk(6),Mterms) , (Mblk(5),Mstr)
   EQUIVALENCE (Mblk(4),Mrow) , (Mblk(2),Mtype)
   EQUIVALENCE (cname,name)
   DATA ctype/'REAL SINGLE   ' , 'REAL DOUBLE   ' , 'COMPLEX SINGLE' , 'COMPLEX DOUBLE'/
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
   CALL fname(Mcb,name)
   Ncol = Mcb(2)
   Memcoln = 0
   Mxrecl = 0
   Maxnac = 0
   Maxnar = 0
   iprec = Prc(Mcb(5))
   Ktype = Mcb(5)
   IF ( Isprec==2 .AND. Ktype==1 ) Ktype = 2
   IF ( Isprec==2 .AND. Ktype==3 ) Ktype = 4
   IF ( Isprec==1 .AND. Ktype==2 ) Ktype = 1
   IF ( Isprec==1 .AND. Ktype==4 ) Ktype = 3
   IF ( Ktype==1 .OR. Ktype==3 ) Kprec = 1
   IF ( Ktype==2 .OR. Ktype==4 ) Kprec = 2
   Ivwrds = Words(Ktype)
   iacrow = 4*Ncol + 1
   irval = iacrow + Ncol
   ivval = irval + 2*Ncol
!
! ENSURE THAT IVVAL IS ON A DOUBLE WORD BOUNDARY
!
   IF ( mod(ivval,2)==0 ) ivval = ivval + 1
   Idbase = ivval + Ivwrds*Ncol
!
! ENSURE THAT IDBASE IS ON A DOUBLE WORD BOUNDARY
!
   IF ( mod(Idbase,2)==0 ) Idbase = Idbase + 1
   IF ( Lcore<(Idbase+2*Isysbf) ) GOTO 1000
   Ibuf1 = Lcore - Isysbf
   Ibuf2 = Ibuf1 - Isysbf
   Idbmax = Ibuf2 - 1
!
! ENSURE THAT IDBMAX IS ON A DOUBLE WORD BOUNDARY
!
   IF ( mod(Idbmax,2)==0 ) Idbmax = Idbmax - 1
   idbind = Idbmax
   CALL open(*1100,Mcb,Zi(Ibuf1),Rdrew)
   CALL skprec(Mcb,1)
   Mblk(1) = Mcb(1)
   Lll(2) = Mcb(2)
   Lll(3) = Mcb(2)
   Lll(4) = 4
   Lll(5) = Ktype
   Lll(6) = 0
   Lll(7) = lshift(1,Nbpw-2-(Nbpw-32))
   icurcol = 1
   Opnscr = .FALSE.
   Nspill = 0
   maxrow = 0
   Maxinlop = 0
   Maxncol = 0
   Lascol = 0
   Power = 0
   IF ( Kprec/=2 ) THEN
      Dsr = 1.0
      dcr = 0.0
      Minds = 1.0E+25
   ELSE
      Ddr = 1.0D0
      Ddc = 0.0D0
      Mindd = 1.0D+25
   ENDIF
   Moblk(1) = Lll(1)
   Moblk(2) = Ktype
   Moblk(3) = 1
!
! ZERO OUT THE ACTIVE COLUMN VECTOR
!
   DO i = 1 , Ncol
      Zi(iacrow+i-1) = 0
   ENDDO
   len = Ncol*4
!
! ZERO OUT THE DIRECTORY
!
   DO i = 1 , len
      Zi(i) = 0
   ENDDO
 100  nterms = 0
   frstval = .TRUE.
   indexr = irval
   Indexv = ivval
   Indexvd = (Indexv/2) + 1
   Mblk(8) = -1
   nexcol = 0
   inddir = (icurcol-1)*4 + 1
 200  DO
      CALL getstr(*500,Mblk)
      IF ( icurcol<=(Mrow+Mterms-1) ) THEN
!
! SAVE SOME OR ALL OF THE TERMS IN THIS STRING
! IF THIS IS NOT THE FIRST STRING TO PROCESS, THEN SAVE ALL VALUES
!
         Iskip = 0
         IF ( .NOT.frstval ) THEN
!
! CHECK TO SEE IF CURRENT STRING IS AN EXTENSION OF PREVIOUS STRING
!
            IF ( (Zi(indexr)+Zi(indexr+1))==Mrow ) EXIT
!
! NO, MUST CREATE NEW POINTER FOR VALUES
! BUT FIRST, CHECK FOR PROVIDING FOR COMPUTED TERMS OF
! PREVIOUS PIVOT COLUMNS
!
            Irow1 = Zi(indexr) + Zi(indexr+1)
            Irown = Mrow - 1
            irflag = 1
            GOTO 900
         ELSE
!
! CHECK IF THIS IS THE FIRST TERM OF THE PIVOT COLUMN
!
            IF ( Zi(inddir+1)==0 ) Zi(inddir+1) = Mrow
!
! OTHERWISE, CHECK IF ALL TERMS OR ONLY SOME ARE TO BE SAVED
!
            frstval = .FALSE.
            IF ( icurcol==Mrow ) THEN
               IF ( Mterms>1 ) nexcol = Mrow + 1
               Zi(indexr) = Mrow
               Zi(indexr+1) = Mterms
               nterms = Mterms
               GOTO 400
            ELSE
!
! CHECK FOR ZERO ON THE DIAGONAL
!
               IF ( icurcol<Mrow ) GOTO 1300
!
! SKIP ALL TERMS BEFORE THE CURRENT PIVOT COLUMN
!
               Iskip = icurcol - Mrow
               Zi(indexr) = icurcol
               nterms = Mterms - Iskip
               Zi(indexr+1) = nterms
               IF ( (Mterms-Iskip)>1 ) nexcol = icurcol + 1
               GOTO 400
            ENDIF
         ENDIF
      ELSE
!
! ALL ROW TERMS ARE BEFORE CURRENT PIVOT COLUMN; SKIP THESE TERMS
! AND GET NEXT STRING.
! CHECK TO SEE IF THIS IS THE FIRST TERM OF THE PIVOT COLUMN
!
         IF ( Zi(inddir+1)==0 ) Zi(inddir+1) = Mrow
         CALL endget(Mblk)
      ENDIF
   ENDDO
!
! TERMS ARE AN EXTENSION OF EXISTING DATA, CHANGE THE NUMBER OF TERMS
!
 300  Zi(indexr+1) = Zi(indexr+1) + Mterms
   nterms = nterms + Mterms
   IF ( nexcol==0 ) nexcol = Mrow
 400  CALL smcrtr(Zr,Zd)
!
! SET ACTIVE COLUMN ROW NUMBERS FOR POSSIBLE EXPANDED TERMS
!
   Irow1 = Mrow + Iskip
   Irown = Irow1 + Mterms - 1 - Iskip
   DO k = Irow1 , Irown
      Zi(iacrow+k-1) = nexcol
   ENDDO
!
! GO AND GET ADDITIONAL STRINGS IF ANY
!
   CALL endget(Mblk)
   GOTO 200
!
! END OF READING CURRENT COLUMN, CHECK IF DIAGONAL TERM FOUND
!
!      PRINT *,' SMCPH1,ICURCOL,NEXCOL,MAXROW=',ICURCOL,NEXCOL,MAXROW
 500  IF ( frstval ) GOTO 1300
!
! SEE IF ANY COMPUTED TERMS FROM PREVIOUS PIVOT COLUMNS ARE TO BE
! ADDED ONTO THE END OF THE CURRENT ACTIVE ROWS FOR THIS COLUMN
!
   lrow = Zi(indexr) + Zi(indexr+1) - 1
   IF ( lrow>maxrow ) maxrow = lrow
   Irow1 = lrow + 1
   Irown = maxrow
   irflag = 2
!      PRINT *,' B1050,ICURCOL,IROWN,IROW1=',ICURCOL,IROWN,IROW1
   IF ( Irown>=Irow1 ) GOTO 900
!
! SET UP DIRECTORY AND SAVE DATA EITHER
! IN MEMORY OR ON SPILL FILE
!
!
! RECOMPUTE LROW IN CASE NEW TERMS WERE ADDED FROM PREVIOUS PIVOT COLUMNS
!
 600  lrow = Zi(indexr) + Zi(indexr+1) - 1
!
! INDEXR POINTS TO CURRENT DIRECTORY ENTRY BUT INDEXV POINTS TO NEXT
! AVAILABLE POSITION FOR STORING TERMS
!
   nrvals = indexr - irval + 2
   nvvals = Indexv - ivval
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
   Maxnar = max0(nrvals,Maxnar)
   IF ( itest<Idbase ) THEN
      IF ( .NOT.(Opnscr) ) THEN
         Opnscr = .TRUE.
         CALL open(*1200,Iscr1,Zi(Ibuf2),Wrtrew)
      ENDIF
!
! NO MORE MEMORY, SAVE COLUMN DATA TO SPILL FILE, KEEP RECORD POSITION
!
      itemp(1) = icurcol
      itemp(2) = nrvals
      itemp(3) = 0
      itemp(4) = nterms
      CALL write(Iscr1,itemp,4,0)
      CALL savpos(Iscr1,kpos)
      CALL write(Iscr1,Zi(irval),indexr-irval+2,0)
      CALL write(Iscr1,Zi(ivval),Indexv-ivval+2,1)
      Zi(inddir) = 0
      Zi(inddir+3) = kpos
      Nspill = Nspill + 1
      itest = nrvals + nvvals + 4
      IF ( itest>Mxrecl ) Mxrecl = itest
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
      IF ( Kprec==2 ) THEN
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
      Lascol = icurcol
      Memcoln = icurcol
      itest = nrvals + nvvals + 4
      IF ( itest>Mxrecl ) Mxrecl = itest
   ENDIF
   lrow = Zi(indexr) + Zi(indexr+1) - 1
!
! SAVE LAST PIVOT COLUMN FOR WHICH DATA IN THIS COLUMN IS USED
!
   IF ( nterms>Maxnac ) Maxnac = nterms
   Zi(inddir+2) = lrow
   ifirstc = Zi(inddir+1)
   maxtes = (icurcol-ifirstc+1)
   IF ( maxtes>Maxncol ) Maxncol = maxtes
   maxtes = nterms*(icurcol-ifirstc)
   IF ( maxtes>Maxinlop ) Maxinlop = maxtes
!
! CHECK TO DETERMINE IF ALL COLUMNS HAVE BEEN PROCESSED
!
   IF ( icurcol>=Ncol ) GOTO 1500
!
! CHECK IF ONLY ONE TERM IN THIS COLUMN
!
   IF ( nexcol==0 ) THEN
!
! MUST FIND FIRST NON-ZERO TERM FOLLOWING THE CURRENT PIVOT
!
      DO k = icurcol + 1 , Ncol
         IF ( Zi(iacrow+k)==icurcol ) THEN
            nexcol = k
            GOTO 700
         ENDIF
      ENDDO
      WRITE (Nout,99001) icurcol
99001 FORMAT (' SYMMETRIC DECOMPOSITION FOUND NO TERMS BEING ',' CONNECTED TO DIAGONAL ON COLUMN ',I8)
      GOTO 800
   ENDIF
!      PRINT *,' AFTER 2005,ICURCOL,NEXCOL=',ICURCOL,NEXCOL
!
! UPDATE ACTIVE ROWS IN COLUMN VECTOR FOR ALL TERMS OF THIS COLUMN
!
 700  len = irval + nrvals - 1
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
 800  icurcol = icurcol + 1
   inddir = (icurcol-1)*4 + 1
   GOTO 100
!
! THE FOLLOWING IS AN INTERNAL ROUTINE TO ADD COMPUTED TERMS RESULTING
! FROM THE PROCESSING OF PREVIOUS PIVOT COLUMNS INTO THE CURRENT ACTIVE
! ROWS FOR THE CURRENT COLUMN
!
 900  DO k = Irow1 , Irown
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
         IF ( Ktype==2 ) THEN
!
! TYPE IS REAL DP
!
            Zd(Indexvd) = 0.D0
            Indexvd = Indexvd + 1
            Indexv = Indexv + 2
         ELSEIF ( Ktype==3 ) THEN
!
! TYPE IS COMPLEX SP
!
            Zr(Indexv) = 0.
            Zr(Indexv+1) = 0.
            Indexv = Indexv + 2
         ELSEIF ( Ktype==4 ) THEN
!
! TYPE IS COMPLEX DP
!
            Zd(Indexvd) = 0.D0
            Zd(Indexvd+1) = 0.D0
            Indexvd = Indexvd + 2
            Indexv = Indexv + 4
         ELSE
!
! TYPE IS REAL SP
!
            Zr(Indexv) = 0.
            Indexv = Indexv + 1
         ENDIF
      ENDIF
   ENDDO
   IF ( irflag==1 ) THEN
!
! NOW CHECK IF THE ADDED TERMS ARE PART OF SAME STRING AS THAT JUST
! GOTTEN FROM GETSTR CALL
!
      IF ( (Zi(indexr)+Zi(indexr+1))==Mrow ) GOTO 300
!
! NEW STRING TO BE DEFINED FOR THE CURRENT TERMS FROM GETSTR
!
      indexr = indexr + 2
      Zi(indexr) = Mrow
      Zi(indexr+1) = Mterms
      nterms = nterms + Mterms
      IF ( nexcol==0 ) nexcol = Mrow
      GOTO 400
   ELSEIF ( irflag==2 ) THEN
      GOTO 600
   ENDIF
!
! INSUFFICIENT MEMORY
!
 1000 minmum = Ncol*7 + 2*Ncol*Ivwrds + 2*Isysbf
   WRITE (Nout,99002) Ufm , Mcb(1) , cname , Ncol , Ktype , Lcore , minmum
99002 FORMAT (1X,A23,/,' INSUFFICIENT MEMORY TO DECOMPOSE MATRIX IN ',I4,' FILE NAME=',2A4,/,' NUMBER OF COLUMNS=',I8,' TYPE=',I2,  &
             &' MEMORY AVAILABLE =',I16,/,' MINIMUM REQUIRED IS =',I16)
!      CALL MESAGE ( -8, 0, 0 )
   Ierror = 1
   GOTO 1500
 1100 CALL fname(Mcb,name)
   WRITE (Nout,99003) Ufm , Mcb(1) , cname
99003 FORMAT (1X,A23,/,' SMCPH1 UNABLE TO OPEN FILE ',I4,' NAME= ',2A4)
   Ierror = 2
   CALL mesage(-61,0,0)
 1200 CALL fname(Iscr1,name)
   WRITE (Nout,99004) Ufm , Iscr1 , cname
99004 FORMAT (1X,A23,/,' SMCPH1 UNABLE TO OPEN FILE ',I4,' NAME= ',2A4)
   Ierror = 2
   CALL mesage(-61,0,0)
!
! ZERO ON DIAGONAL, TERMINATE DECOMPOSITION BUT FIRST SCAN REST OF
! MATRIX TO DETERMINE OTHER COLUMNS WITH ZERO DIAGONALS.
!
 1300 Ierror = 7
   izeros = 1
   indexz = 0
   IF ( .NOT.(frstval) ) THEN
      CALL endget(Mblk)
      CALL skprec(Mblk,1)
   ENDIF
 1400 indexz = indexz + 1
   Zi(indexz) = icurcol
   DO
      icurcol = icurcol + 1
      IF ( icurcol>Ncol ) THEN
         CALL close(Mcb,Rew)
         CALL close(Iscr1,Rew)
         WRITE (Nout,99005) Ufm , cname , (Zi(k),k=1,indexz)
99005    FORMAT (A23,' 3097, SYMMETRIC DECOMPOSITION OF DATA BLOCK ',2A4,' ABORTED BECAUSE THE FOLLOWING COLUMNS ARE SINGULAR -',/, &
               & (5X,20I6,/))
         RETURN
      ELSE
         Mblk(8) = -1
         DO
            CALL getstr(*1400,Mblk)
            CALL endget(Mblk)
            IF ( icurcol>=Mrow .AND. icurcol<=Mrow+Mterms-1 ) THEN
               CALL skprec(Mblk,1)
               EXIT
            ELSEIF ( Mrow>icurcol ) THEN
               CALL skprec(Mblk,1)
               GOTO 1400
            ENDIF
         ENDDO
      ENDIF
   ENDDO
!      CALL SMCHLP
!      CALL SMCDMP ( ZI, ZR, ZD )
 1500 CALL close(Mcb,Rew)
   itwrds = Idbmax - idbind
   itcols = Ncol - Nspill
   CALL sswtch(45,l45)
   IF ( l45/=0 ) THEN
      WRITE (Lout,99006) itcols , Nspill , Maxnac , Maxncol , Maxinlop , itwrds
99006 FORMAT (/,14X,' STATISTICS FOR SYMMETRIC DECOMPOSITION OF FILE ',/,/,7X,                                                      &
             &' COLUMNS CONTAINED IN MEMORY                         =',I16,/,7X,                                                    &
             &' COLUMNS WRITTEN TO SPILL FILE                       =',I16,/,7X,                                                    &
             &' MAX. NO. OF ACTIVE ROWS FOR ANY ACTIVE COLUMN       =',I16,/,7X,                                                    &
             &' MAX. NUMBER OF COLUMNS REFERENCED BY A PIVOT COLUMN =',I16,/,7X,                                                    &
             &' MAX. TERMS FOR ANY GIVEN INNER LOOP                 =',I16,/,7X,                                                    &
             &' TOTAL WORDS IN OPEN CORE USED FOR COLUMN DATA       =',I16)
      WRITE (Lout,99007) 'INPUT ' , cname , ctype(Mcb(5))
      CALL fname(Lll,name)
      WRITE (Lout,99007) 'OUTPUT' , cname , ctype(Ktype)
   ENDIF
99007 FORMAT (8X,A6,' FILE: ',2A4,'      DATA TYPE= ',A14)
!      CALL AUDIT( 'SMCPH1  ', 2 )
END SUBROUTINE smcph1
