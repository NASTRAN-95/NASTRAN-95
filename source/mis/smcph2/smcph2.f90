!*==smcph2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE smcph2(Zi,Zr,Zd)
   USE i_smcomx
   USE c_logout
   USE c_names
   USE c_type
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(10) :: Zi
   REAL , DIMENSION(10) :: Zr
   REAL*8 , DIMENSION(10) :: Zd
!
! Local variable declarations rewritten by SPAG
!
   CHARACTER(4) , DIMENSION(2) :: cname
   INTEGER :: iadj , ilsrow , inddir , index , irval , itotal , ivval , iwork , kpos , l45 , left , maxmem , minum , more , mrow ,  &
            & mspill , mstr , mterms , mtype , nar , need , nextra , nrvals , nterms
   INTEGER , DIMENSION(4) :: itemp
   INTEGER , DIMENSION(2) :: name
   REAL :: percnt , xcore , xfact , xmaxmem , xncol , xspill
   REAL*8 , DIMENSION(10) :: xnd
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
! End of declarations rewritten by SPAG
!
!
! SMCPH2 PERFORMS THE ACTUAL DECOMPOSITION OF THE MATRIX THAT WAS
! SETUP IN MEMORY AND/OR THE SPILL BY SMCPH1.
! SEE SMCPH1 FOR THE DEFINITION OF SOME OF THE VARIABLES IN /SMCOMX/
!
   !>>>>EQUIVALENCE (Xns,Xnd)
   !>>>>EQUIVALENCE (Mblk(6),Mterms) , (Mblk(5),Mstr)
   !>>>>EQUIVALENCE (Mblk(4),Mrow) , (Mblk(2),Mtype)
   !>>>>EQUIVALENCE (name,cname)
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
         nar = ncol*4 + 1
         ilsrow = nar + maxnar + 2
         mspill = 0
         ispill = 0
         IF ( nspill/=0 ) THEN
            ispill = nar + maxnar + 1
            IF ( mod(ispill,2)==0 ) ispill = ispill + 1
            ilsrow = ispill + mxrecl + 4
         ENDIF
         ioloop = ilsrow + maxncol + 2
         IF ( mod(ioloop,2)==0 ) ioloop = ioloop + 1
         iiloop = ioloop + 2*maxncol*ivwrds
         iwork = iiloop + maxncol*maxnac*ivwrds
         itotal = iwork + maxnac*ivwrds
         inddir = lascol*4 - 3
         idbase = Zi(inddir)
         memfre = 0
         memlas = 0
         memlck = 0
         memcol1 = 1
         xncol = ncol
         xspill = nspill
         xfact = xncol/(xncol-nspill)
!
!  MORE = ESTIMATED NUMBER OF WORDS NEEDED FOR STORING ALL OF MATRIX
!  IADJ = WORDS OF COLUMN DATA THAT WILL NEED TO BE WRITTEN TO THE SPILL
!         FILE TO ALLOW FOR "ITOTAL" WORDS FOR THE PHASE II ARRAYS.
!
         more = xfact*(lcore-idbase)
         iadj = 0
         IF ( itotal>idbase ) iadj = itotal - idbase
         maxmem = itotal + more + iadj
         CALL sswtch(45,l45)
         IF ( nspill/=0 .OR. l45/=0 ) THEN
            WRITE (lout,99001) maxmem , lcore
99001       FORMAT (7X,' ESTIMATED OPEN CORE NEEDED TO ELIMINATE USE OF SPILL=',I16,/,7X,                                           &
                   &' OPEN CORE AVAILABLE FOR THIS DECOMPOSITION          =',I16)
!
! TEST TO BE SURE THAT AT LEAST HALF OF THE MEMORY IS AVAILABLE.
! IF NOT, USE OLD METHOD INSTEAD OF THIS ONE.
!
            xmaxmem = maxmem
            xcore = lcore
            percnt = xcore/xmaxmem
            IF ( percnt<.5 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
! CHECK TO SEE IF ENOUGH OPEN CORE FOR INNER AND OUTER LOOP VALUES
!
         DO WHILE ( itotal>=idbase )
!
! NEED MORE OPEN CORE FOR LOOP AREAS.  WRITE COLUMN DATA TO SPILL FILE.
! IF COLUMNS WERE WRITTEN TO SPILL FILE FROM SMCPH1, THEN FILE WILL
! STILL BE OPEN.  IF NOT, MUST ALLOW FOR SPILL AREA IN OPEN CORE AND
! RE-ADJUST THE OPEN CORE POINTERS.
!
            nextra = 0
            IF ( .NOT.(opnscr) ) THEN
               opnscr = .TRUE.
               CALL open(*60,iscr1,Zi(ibuf2),wrtrew)
               ispill = nar + maxnar + 1
               IF ( mod(ispill,2)==0 ) ispill = ispill + 1
               ilsrow = ispill + mxrecl + 4
               ioloop = ilsrow + maxncol + 2
               IF ( mod(ioloop,2)==0 ) ioloop = ioloop + 1
               iiloop = ioloop + 2*maxncol*ivwrds
               iwork = iiloop + maxncol*maxnac*ivwrds
               itotal = iwork + maxnac*ivwrds
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
            CALL write(iscr1,itemp,4,0)
            CALL savpos(iscr1,kpos)
            CALL write(iscr1,Zr(irval),nrvals,0)
            CALL write(iscr1,Zr(ivval),nterms*ivwrds,1)
            Zi(inddir) = 0
            Zi(inddir+3) = kpos
            SPAG_Loop_2_1: DO
               inddir = inddir - 4
               IF ( inddir<=0 ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( Zi(inddir)/=0 ) THEN
!
! RESET IDBASE TO INDICATE THE LAST COLUMN OF DATA IN MEMORY
!
                  idbase = Zi(inddir)
                  mspill = mspill + 1
                  EXIT SPAG_Loop_2_1
               ENDIF
            ENDDO SPAG_Loop_2_1
         ENDDO
!
! OPEN THE OUTPUT FILE
!
         left = idbase - itotal
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
         need = 2 + 2*ivwrds + maxnac*ivwrds
         nextra = (left-2-(maxnac*ivwrds))/need
!      PRINT *,' LEFT,NEED,NEXTRA=',LEFT,NEED,NEXTRA
         IF ( nextra/=0 ) THEN
            ioloop = ilsrow + (maxncol+nextra) + 2
            IF ( mod(ioloop,2)==0 ) ioloop = ioloop + 1
            iiloop = ioloop + (2*(maxncol+nextra))*ivwrds
            iwork = iiloop + (maxnac*(maxncol+nextra))*ivwrds
            itotal = iwork + (maxnac)*ivwrds
         ENDIF
         IF ( kprec==2 ) ioloop = ioloop/2 + 1
         IF ( kprec==2 ) iiloop = iiloop/2 + 1
         IF ( kprec==2 ) iwork = iwork/2 + 1
         nvterm = 1
         IF ( ktype>=3 ) nvterm = 2
         IF ( mspill/=0 ) WRITE (lout,99002) mspill
99002    FORMAT (8X,'ADDITIONAL COLUMNS WRITTEN TO SPILL ','FOR PHASE II PROCESSING =',I8)
         IF ( opnscr ) THEN
            CALL close(iscr1,1)
            CALL open(*40,iscr1,Zi(ibuf2),rdrew)
         ENDIF
         CALL open(*20,lll,Zi(ibuf1),wrtrew)
         CALL fname(lll,name)
         CALL write(lll,name,2,1)
!
! DO THE DECOMPOSITION NOW
!
!      CALL AUDIT ( 'SMC2RD  ', 1 )
!      PRINT *,' IILOOP,IOLOOP,NAR,ILSROW,NEXTRA,IDBASE,IWORK,ISPILL'
!      PRINT *,  IILOOP,IOLOOP,NAR,ILSROW,NEXTRA,IDBASE,IWORK,ISPILL
         IF ( ktype==2 ) THEN
            CALL smc2rd(Zi,Zd,Zd(iiloop),Zd(ioloop),Zi(nar),Zi(ilsrow),Zd(iwork),maxnac,maxncol+nextra,maxnar)
         ELSEIF ( ktype==3 ) THEN
!      PRINT *,' CALLING SMC2CS'
            CALL smc2cs(Zi,Zr,Zd(iiloop),Zd(ioloop),Zi(nar),Zi(ilsrow),Zd(iwork),maxnac,maxncol+nextra,maxnar)
         ELSEIF ( ktype==4 ) THEN
!      PRINT *,' CALLING SMC2CD'
            CALL smc2cd(Zi,Zd,Zd(iiloop),Zd(ioloop),Zi(nar),Zi(ilsrow),Zd(iwork),maxnac,maxncol+nextra,maxnar)
         ELSE
            CALL smc2rs(Zi,Zr,Zr(iiloop),Zr(ioloop),Zi(nar),Zi(ilsrow),Zr(iwork),maxnac,maxncol+nextra,maxnar)
         ENDIF
!      CALL AUDIT ( 'SMC2RD  ', 2 )
         CALL close(lll,1)
         CALL close(iscr1,1)
         RETURN
 20      CALL fname(lll,name)
         ierror = 2
         WRITE (nout,99004) ufm , lll(1) , cname
         CALL mesage(-61,0,0)
         RETURN
 40      CALL fname(iscr1,name)
         WRITE (nout,99004) ufm , iscr1 , cname
         ierror = 3
         CALL mesage(-61,0,0)
         RETURN
 60      ierror = 2
         CALL fname(iscr1,name)
         WRITE (nout,99004) ufm , iscr1 , cname
         CALL mesage(-61,0,0)
         RETURN
      CASE (2)
         CALL fname(lll,name)
         minum = (.5*xmaxmem) - lcore
         WRITE (lout,99003) ncol , minum
99003    FORMAT (8X,'INSUFFICIENT OPEN CORE FOR DECOMPOSITION WITH NEW',' METHOD',/,8X,'TOTAL NUMBER OF COLUMNS IN MATRIX =',I16,/, &
               & 8X,'SUGGESTED ADDITIONAL OPEN CORE IS =',I16)
         CALL close(iscr1,1)
!      CALL MESAGE ( -8, 0, 0 )
         ierror = 1
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99004 FORMAT (1X,A23,/,' SMCPH2 UNABLE TO OPEN FILE ',I4,' ;FILE NAME =',2A4)
!      CALL AUDIT ( 'SMCPH2  ',2)
!      CALL AUDIT ( 'END     ',1)
!      IF ( NCOL .NE. 0 ) STOP
END SUBROUTINE smcph2
