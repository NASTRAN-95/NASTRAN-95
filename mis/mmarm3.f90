
SUBROUTINE mmarm3(Zi,Zr,Mempcol)
   IMPLICIT NONE
   INCLUDE 'MMACOM.COM'
   INTEGER Ibfsiz , Iwr
   REAL Rxl(1)
   COMMON /system/ Ibfsiz , Iwr
   COMMON /zzzzzz/ Rxl
   INTEGER Mempcol
   INTEGER Zi(1)
   REAL Zr(1)
   INTEGER i , iblk(15) , icblk , icbp , iclr , icol , ii , index , jrow , mem , mem1 , mindex , module(2) , ntms , ntms2
!
!  MMARM3 - This routine will store matrix columns in memory in compact
!           form and in real complex precision.  The input matrix is
!           assumed to be stored as real single or complex precision.
!           The column is stored in memory according to the following scheme:
!
!  MEMPCOL  = Input, extra memory needed for each column that is stored
!             in memory in compact form.  This is needed for methods 40
!             and 41 where for each column of "B" stored in compact form
!             in memory, there needs to be space available for a column
!             of the "D" matrix.
!
!  1st word = column number (negative)
!  2nd word = index to next column within this array
!  3st word = row position of first element in following string
!  4nd word = number of terms in string (ntms)
!  5rd word           }
!     |               }
!     |               } = actual
!     |               }   matrix
!     |               }   string
!     |               }   data
!     |               }
!     |               }
!  5+(ntms*prec)      } (where prec=1 for s.p.;  =2 for d.p. )
!     n               } Last value of last string for this column
!
!  Words 3 through 5+(ntms*prec) above data repeat for all strings
!  within a column.  Words 1 through n repeat for all columns that are
!  read into memory.
!
!
!  Argument list :
!     ZI  - Memory for storage of data (integer)
!     ZR  - Same location as ZI but real single reference
!
   DATA module/4HMMAR , 4HM3  /
   mem = 1
   DO i = 1 , 15
      iblk(i) = 0
   ENDDO
   iblk(1) = Irfile
!
! IRCOL1, FIRST COLUMN EXPECTED FOR THIS PASS
! IRCOLN, ON INPUT, THIS IS THE LAST COLUMN THAT IS NEEDED
!         ON OUTPUT, THIS IS THE LAST COLUMN READ
! LASMEM, LAST AVAILABLE MEMORY INDEX TO THE "ZI" ARRAY
!
   icol = Ircol1
 100  iblk(8) = -1
   Lasindm = mem - 1
   CALL dscpos(Irfile,icblk,iclr,icbp)
   CALL getstr(*200,iblk)
!      IF ( ICOL .NE. IBLK( 12 ) ) GO TO 7001
   Zi(mem) = -icol
   mem1 = mem + 1
   mem = mem + 2
   DO
      ntms = iblk(6)
      IF ( (mem+2+ntms*2)>Lasmem ) GOTO 400
      Itype = iblk(2)
      jrow = iblk(4)
      index = iblk(5)
      Zi(mem) = jrow
      Zi(mem+1) = ntms
      IF ( Itype==2 ) THEN
!
! THE FOLLOWING LINE SHOULD NEVER BE REFERENCED
!
         WRITE (Iwr,*) ' ERROR IN MMARM3'
         STOP
      ELSEIF ( Itype==3 ) THEN
         mindex = mem + 1
         ntms2 = ntms*2
         DO ii = 1 , ntms2
            Zr(mindex+ii) = Sign*Rxl(index+ii-1)
         ENDDO
      ELSE
         mindex = mem + 2
         DO ii = 1 , ntms
            Zr(mindex) = Sign*Rxl(index+ii-1)
            Zr(mindex+1) = 0.
            mindex = mindex + 2
         ENDDO
      ENDIF
      mem = mem + 2 + ntms*2
      CALL endget(iblk)
      CALL getstr(*300,iblk)
   ENDDO
 200  Zi(mem) = -icol
   mem1 = mem + 1
   mem = mem + 2
!
! CHECK IF SPACE AVAILABLE FOR A FULL COLUMN OF "D" MATRIX, IF NECESSARY
!
 300  IF ( mem<=(Lasmem-Mempcol) ) THEN
      Lasmem = Lasmem - Mempcol
      Zi(mem1) = mem
      icol = icol + 1
      IF ( icol<=Ircoln ) GOTO 100
      Lasindm = mem - 1
      GOTO 99999
   ENDIF
 400  Lasindm = mem1 - 2
!
! SAVE I/O LOCATION OF LAST COLUMN FOR NEXT PASS
!
   Irpos(1) = icblk
   Irpos(2) = iclr
   Irpos(3) = icbp
   Ircoln = icol - 1
   IF ( Ircoln<Ircol1 ) CALL mesage(-8,mem+Mempcol,module)
!      GO TO 7777
!7001  WRITE( IWR, 9001 ) ICOL, IBLK(12), IRFILE
!9001  FORMAT(' ERROR OCCURRED IN MMARM3, EXPECTED COLUMN =',I10
!     &,/,    ' BUT READ COLUMN =',I10,' FROM FILE =',I5 )
!      CALL MESAGE ( -61, 0, 0 )
99999 RETURN
END SUBROUTINE mmarm3
