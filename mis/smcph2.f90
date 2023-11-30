
SUBROUTINE smcph2(Zi,Zr,Zd)
   IMPLICIT NONE
   INCLUDE 'SMCOMX.COM'
   REAL Cdp , Csp , Diag , Eofnrw , Rdnrw , Rdp , Rdrew , Rect , Rew , Rsp , Sqr , Sym , Uprtri , Wrt , Wrtrew , Xns(10)
   INTEGER Lout , Lowtri , Norew , Prc(2) , Rlcmpx(4) , Words(4), Mrow, Mterms, Mstr, Mtype
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   DOUBLE PRECISION Xnd(10)
   COMMON /logout/ Lout
   COMMON /names / Rdnrw , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp , Csp , Cdp , Sqr , Rect , Diag , Lowtri ,       &
                 & Uprtri , Sym
   COMMON /type  / Prc , Words , Rlcmpx
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ Xns
   DOUBLE PRECISION Zd(10)
   INTEGER Zi(10)
   REAL Zr(10)
   CHARACTER*4 cname(2)
   INTEGER iadj , ilsrow , inddir , index , irval , itemp(4) , itotal , ivval , iwork , kpos , l45 , left , maxmem , minum , more , &
         & mspill , name(2) , nar , need , nextra , nrvals , nterms
   REAL percnt , xcore , xfact , xmaxmem , xncol , xspill
!
! SMCPH2 PERFORMS THE ACTUAL DECOMPOSITION OF THE MATRIX THAT WAS
! SETUP IN MEMORY AND/OR THE SPILL BY SMCPH1.
! SEE SMCPH1 FOR THE DEFINITION OF SOME OF THE VARIABLES IN /SMCOMX/
!
   EQUIVALENCE (Xns,Xnd)
   EQUIVALENCE (Mblk(6),Mterms) , (Mblk(5),Mstr)
   EQUIVALENCE (Mblk(4),Mrow) , (Mblk(2),Mtype)
   EQUIVALENCE (name,cname)
!
!   open core is allocated as follows for the decomposition
!
!       -------------------------------
!       zi(1)
!       Directory  (4,n) , n=number of columns of matrix
!                  (1,i) = index to active rows and terms within memory
!                  (2,i) = first column data needed for this pivot
!                  (3,i) = last pivot column to use this data
!                          (also, the last active row of this column)
!                  (4,i) = savpos position pointer for data spilled to a
!                          scratch file
!       -------------------------------
!       zi(nar)
!       Area for storage of row numbers used for previous column of
!       decomposition (length=MAXNAR+2)
!       -------------------------------
!       zi(ispill)
!       Area to read data from spill file (length =MXRECL+4)
!       This area is not needed if no columns written to spill file
!       -------------------------------
!       zi(ILSROW)
!       Area for storage of last non-zero row term for a given column
!       (length=MAXNCOL)
!       -------------------------------
!       zi(ioloop)
!       Values for outer loop terms in all row computations in the
!       current pivotal column.
!
!         temp = temp + a(i,j) * a(k,j) / a(j,j)
!                                ===============
!            i = row of column
!            k = pivotal column being processed
!            j = 1, k-1
!          a(i,k) = a(i,k) - temp
!          (Note, length is 2*MAXNCOL)
!          MAXNCOL = maximum number of columns referenced by any
!                    pivotal column
!       -------------------------------
!       zi(iiloop)
!       Values for inner loop terms in each row computation
!         temp = temp + a(i,j) * a(k,j) / a(j,j)
!                       ======
!            i = row of column
!            k = pivotal column being processed
!            j = 1, k-1
!          a(i,k) = a(i,k) - temp
!          (Note, length is  MAXNCOL*MAXNAC)
!          MAXNAC = MAXIMUM NUMBER OF ACTIVE ROWS FOR ANY GIVEN COLUMN
!       -------------------------------
!       zi(iwork)
!       Temporary storage for storing "temp" values for each row (see
!       "temp" in above equation for zi(iiloop) )
!       -------------------------------
!       zi(idbase)
!       Memory for rows and terms of columns as pointed to by directory
!       in the first part of open core.  This data is loaded from the
!       bottom up to allow for better management of open core.
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
!           5+2*m.  value for first row
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
!      CALL AUDIT ('SMCPH2  ',1 )
   nar = Ncol*4 + 1
   ilsrow = nar + Maxnar + 2
   mspill = 0
   Ispill = 0
   IF ( Nspill/=0 ) THEN
      Ispill = nar + Maxnar + 1
      IF ( mod(Ispill,2)==0 ) Ispill = Ispill + 1
      ilsrow = Ispill + Mxrecl + 4
   ENDIF
   Ioloop = ilsrow + Maxncol + 2
   IF ( mod(Ioloop,2)==0 ) Ioloop = Ioloop + 1
   Iiloop = Ioloop + 2*Maxncol*Ivwrds
   iwork = Iiloop + Maxncol*Maxnac*Ivwrds
   itotal = iwork + Maxnac*Ivwrds
   inddir = Lascol*4 - 3
   Idbase = Zi(inddir)
   Memfre = 0
   Memlas = 0
   Memlck = 0
   Memcol1 = 1
   xncol = Ncol
   xspill = Nspill
   xfact = xncol/(xncol-Nspill)
!
!  MORE = ESTIMATED NUMBER OF WORDS NEEDED FOR STORING ALL OF MATRIX
!  IADJ = WORDS OF COLUMN DATA THAT WILL NEED TO BE WRITTEN TO THE SPILL
!         FILE TO ALLOW FOR "ITOTAL" WORDS FOR THE PHASE II ARRAYS.
!
   more = xfact*(Lcore-Idbase)
   iadj = 0
   IF ( itotal>Idbase ) iadj = itotal - Idbase
   maxmem = itotal + more + iadj
   CALL sswtch(45,l45)
   IF ( Nspill/=0 .OR. l45/=0 ) THEN
      WRITE (Lout,99001) maxmem , Lcore
99001 FORMAT (7X,' ESTIMATED OPEN CORE NEEDED TO ELIMINATE USE OF SPILL=',I16,/,7X,                                                 &
             &' OPEN CORE AVAILABLE FOR THIS DECOMPOSITION          =',I16)
!
! TEST TO BE SURE THAT AT LEAST HALF OF THE MEMORY IS AVAILABLE.
! IF NOT, USE OLD METHOD INSTEAD OF THIS ONE.
!
      xmaxmem = maxmem
      xcore = Lcore
      percnt = xcore/xmaxmem
      IF ( percnt<.5 ) GOTO 400
   ENDIF
!
! CHECK TO SEE IF ENOUGH OPEN CORE FOR INNER AND OUTER LOOP VALUES
!
   DO WHILE ( itotal>=Idbase )
!
! NEED MORE OPEN CORE FOR LOOP AREAS.  WRITE COLUMN DATA TO SPILL FILE.
! IF COLUMNS WERE WRITTEN TO SPILL FILE FROM SMCPH1, THEN FILE WILL
! STILL BE OPEN.  IF NOT, MUST ALLOW FOR SPILL AREA IN OPEN CORE AND
! RE-ADJUST THE OPEN CORE POINTERS.
!
      nextra = 0
      IF ( .NOT.(Opnscr) ) THEN
         Opnscr = .TRUE.
         CALL open(*300,Iscr1,Zi(Ibuf2),Wrtrew)
         Ispill = nar + Maxnar + 1
         IF ( mod(Ispill,2)==0 ) Ispill = Ispill + 1
         ilsrow = Ispill + Mxrecl + 4
         Ioloop = ilsrow + Maxncol + 2
         IF ( mod(Ioloop,2)==0 ) Ioloop = Ioloop + 1
         Iiloop = Ioloop + 2*Maxncol*Ivwrds
         iwork = Iiloop + Maxncol*Maxnac*Ivwrds
         itotal = iwork + Maxnac*Ivwrds
      ENDIF
!
! WRITE THE LAST COLUMN OF DATA CURRENTLY IN MEMORY TO THE SPILL FILE
!
      index = Zi(inddir)
      irval = index + 4
      nrvals = Zi(index+1)
      nterms = Zi(index+3)
      ivval = irval + nrvals
      itemp(1) = Zi(index)
      itemp(2) = nrvals
      itemp(3) = 0
      itemp(4) = nterms
!      PRINT *,' SMCPH2 CALLING WRITE FOR ITEMP,NRVALS,NTERMS,IVWRDS'
!      PRINT *,                           ITEMP,NRVALS,NTERMS,IVWRDS
      CALL write(Iscr1,itemp,4,0)
      CALL savpos(Iscr1,kpos)
      CALL write(Iscr1,Zr(irval),nrvals,0)
      CALL write(Iscr1,Zr(ivval),nterms*Ivwrds,1)
      Zi(inddir) = 0
      Zi(inddir+3) = kpos
      DO
         inddir = inddir - 4
         IF ( inddir<=0 ) GOTO 400
         IF ( Zi(inddir)/=0 ) THEN
!
! RESET IDBASE TO INDICATE THE LAST COLUMN OF DATA IN MEMORY
!
            Idbase = Zi(inddir)
            mspill = mspill + 1
            EXIT
         ENDIF
      ENDDO
   ENDDO
!
! OPEN THE OUTPUT FILE
!
   left = Idbase - itotal
!
! DETERMINE HOW MANY MORE COLUMNS OF THE INNER LOOP AREA AND
! EXTRA TERMS OF THE OUTER LOOP AREA ARE AVAILABLE
!   NEXTRA = NUMBER OF EXTRA COLUMNS AVAILABLE IN THE INNER LOOP AREA
!          = NUMBER OF EXTRA COLUMNS AVAILABLE IN THE OUTER LOOP AREA
!            (INNER LOOP AREA SIZE = MAXNAC * ( MAXNCOL + NEXTRA ) )
!            (OUTER LOOP AREA SIZE = 2      * ( MAXNCOL + NEXTRA ) )
!          = NUMBER OF EXTRA ROWS IN THE "ILSROW" ARRAY (MAXNCOL+NEXTRA)
!  (Note: for each column added, we need the following:
!           for array ILSROW:                1
!           to insure double word boundary:  1
!           for outer loop:                  2*IVWRDS
!           for inner loop:                  MAXNAC*IVWRDS
!           ( must allow for temp array size:    MAXNAC*IVWRDS
   need = 2 + 2*Ivwrds + Maxnac*Ivwrds
   nextra = (left-2-(Maxnac*Ivwrds))/need
!      PRINT *,' LEFT,NEED,NEXTRA=',LEFT,NEED,NEXTRA
   IF ( nextra/=0 ) THEN
      Ioloop = ilsrow + (Maxncol+nextra) + 2
      IF ( mod(Ioloop,2)==0 ) Ioloop = Ioloop + 1
      Iiloop = Ioloop + (2*(Maxncol+nextra))*Ivwrds
      iwork = Iiloop + (Maxnac*(Maxncol+nextra))*Ivwrds
      itotal = iwork + (Maxnac)*Ivwrds
   ENDIF
   IF ( Kprec==2 ) Ioloop = Ioloop/2 + 1
   IF ( Kprec==2 ) Iiloop = Iiloop/2 + 1
   IF ( Kprec==2 ) iwork = iwork/2 + 1
   Nvterm = 1
   IF ( Ktype>=3 ) Nvterm = 2
   IF ( mspill/=0 ) WRITE (Lout,99002) mspill
99002 FORMAT (8X,'ADDITIONAL COLUMNS WRITTEN TO SPILL ','FOR PHASE II PROCESSING =',I8)
   IF ( Opnscr ) THEN
      CALL close(Iscr1,1)
      CALL open(*200,Iscr1,Zi(Ibuf2),Rdrew)
   ENDIF
   CALL open(*100,Lll,Zi(Ibuf1),Wrtrew)
   CALL fname(Lll,name)
   CALL write(Lll,name,2,1)
!
! DO THE DECOMPOSITION NOW
!
!      CALL AUDIT ( 'SMC2RD  ', 1 )
!      PRINT *,' IILOOP,IOLOOP,NAR,ILSROW,NEXTRA,IDBASE,IWORK,ISPILL'
!      PRINT *,  IILOOP,IOLOOP,NAR,ILSROW,NEXTRA,IDBASE,IWORK,ISPILL
   IF ( Ktype==2 ) THEN
      CALL smc2rd(Zi,Zd,Zd(Iiloop),Zd(Ioloop),Zi(nar),Zi(ilsrow),Zd(iwork),Maxnac,Maxncol+nextra,Maxnar)
   ELSEIF ( Ktype==3 ) THEN
!      PRINT *,' CALLING SMC2CS'
      CALL smc2cs(Zi,Zr,Zd(Iiloop),Zd(Ioloop),Zi(nar),Zi(ilsrow),Zd(iwork),Maxnac,Maxncol+nextra,Maxnar)
   ELSEIF ( Ktype==4 ) THEN
!      PRINT *,' CALLING SMC2CD'
      CALL smc2cd(Zi,Zd,Zd(Iiloop),Zd(Ioloop),Zi(nar),Zi(ilsrow),Zd(iwork),Maxnac,Maxncol+nextra,Maxnar)
   ELSE
      CALL smc2rs(Zi,Zr,Zr(Iiloop),Zr(Ioloop),Zi(nar),Zi(ilsrow),Zr(iwork),Maxnac,Maxncol+nextra,Maxnar)
   ENDIF
!      CALL AUDIT ( 'SMC2RD  ', 2 )
   CALL close(Lll,1)
   CALL close(Iscr1,1)
   GOTO 99999
 100  CALL fname(Lll,name)
   Ierror = 2
   WRITE (Nout,99004) Ufm , Lll(1) , cname
   CALL mesage(-61,0,0)
   GOTO 99999
 200  CALL fname(Iscr1,name)
   WRITE (Nout,99004) Ufm , Iscr1 , cname
   Ierror = 3
   CALL mesage(-61,0,0)
   GOTO 99999
 300  Ierror = 2
   CALL fname(Iscr1,name)
   WRITE (Nout,99004) Ufm , Iscr1 , cname
   CALL mesage(-61,0,0)
   GOTO 99999
 400  CALL fname(Lll,name)
   minum = (.5*xmaxmem) - Lcore
   WRITE (Lout,99003) Ncol , minum
99003 FORMAT (8X,'INSUFFICIENT OPEN CORE FOR DECOMPOSITION WITH NEW',' METHOD',/,8X,'TOTAL NUMBER OF COLUMNS IN MATRIX =',I16,/,8X, &
             &'SUGGESTED ADDITIONAL OPEN CORE IS =',I16)
   CALL close(Iscr1,1)
!      CALL MESAGE ( -8, 0, 0 )
   Ierror = 1
99004 FORMAT (1X,A23,/,' SMCPH2 UNABLE TO OPEN FILE ',I4,' ;FILE NAME =',2A4)
!      CALL AUDIT ( 'SMCPH2  ',2)
!      CALL AUDIT ( 'END     ',1)
!      IF ( NCOL .NE. 0 ) STOP
99999 RETURN
END SUBROUTINE smcph2
