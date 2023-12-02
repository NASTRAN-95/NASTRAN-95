!*==mmarm2.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE mmarm2(Zi,Zd,Mempcol)
   IMPLICIT NONE
   USE I_MMACOM
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Zi
   REAL*8 , DIMENSION(1) :: Zd
   INTEGER :: Mempcol
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , icblk , icbp , iclr , icol , ii , index , jrow , mem , mem1 , mindex , ntms
   INTEGER , DIMENSION(15) :: iblk
   INTEGER , DIMENSION(2) , SAVE :: module
   REAL , DIMENSION(1) :: rxl
!
! End of declarations rewritten by SPAG
!
!
!  MMARM2 - This routine will store matrix columns in memory in compact
!           form and in real double precision.  The input matrix is
!           assumed to be stored as either real single or double precision.
!           The column is stored in memory according to the following scheme:
!
!  MEMPCOL  = Input, extra memory needed for each column that is stored
!             in memory in compact form.  This is needed for methods 40
!             and 41 where for each column of "B" stored in compact form
!             in memory, there needs to be space available for a column
!             of the "D" matrix.
!
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
!  Argument list :
!     ZI  - Memory for storage of data (integer)
!     ZD  - Same location as ZI but real double reference
!
   !>>>>EQUIVALENCE (Dxl,Rxl)
   DATA module/4HMMAR , 4HM2  /
   mem = 1
   DO i = 1 , 15
      iblk(i) = 0
   ENDDO
   iblk(1) = irfile
!
! IRCOL1, FIRST COLUMN EXPECTED FOR THIS PASS
! IRCOLN, ON INPUT, THIS IS THE LAST COLUMN THAT IS NEEDED
!         ON OUTPUT, THIS IS THE LAST COLUMN READ
! LASMEM, LAST AVAILABLE MEMORY INDEX TO THE "ZI" ARRAY
!
   icol = ircol1
 100  iblk(8) = -1
   lasindm = mem - 1
   CALL dscpos(irfile,icblk,iclr,icbp)
   CALL getstr(*200,iblk)
!      IF ( ICOL .NE. IBLK( 12 ) ) GO TO 7001
   Zi(mem) = -icol
   mem1 = mem + 1
   mem = mem + 2
   DO
      ntms = iblk(6)
      IF ( (mem+2+ntms*2)>lasmem ) GOTO 400
      itype = iblk(2)
      jrow = iblk(4)
      index = iblk(5)
      Zi(mem) = jrow
      Zi(mem+1) = ntms
      IF ( itype==2 ) THEN
         mindex = mem/2 + 1
         DO ii = 1 , ntms
            Zd(mindex+ii) = sign*Dxl(index+ii-1)
         ENDDO
      ELSE
         mindex = mem/2 + 1
         DO ii = 1 , ntms
            Zd(mindex+ii) = sign*rxl(index+ii-1)
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
! CHECK IF SPACE IS AVAILABLE FOR A FULL COLUMN OF "D" MATRIX, IF NECESSARY
!
 300  IF ( mem<=(lasmem-Mempcol) ) THEN
      lasmem = lasmem - Mempcol
      Zi(mem1) = mem
      icol = icol + 1
      IF ( icol<=ircoln ) GOTO 100
      lasindm = mem - 1
      GOTO 99999
   ENDIF
 400  lasindm = mem1 - 2
!
! SAVE I/O LOCATION OF LAST COLUMN FOR NEXT PASS
!
   irpos(1) = icblk
   irpos(2) = iclr
   irpos(3) = icbp
   ircoln = icol - 1
   IF ( ircoln<ircol1 ) CALL mesage(-8,mem+Mempcol,module)
!      GO TO 7777
!7001  WRITE( IWR, 9001 ) ICOL, IBLK(12), IRFILE
!9001  FORMAT(' ERROR OCCURRED IN MMARM2, EXPECTED COLUMN =',I10
!     &,/,    ' BUT READ COLUMN =',I10,' FROM FILE =',I5 )
!      PRINT *,' IBLK=',IBLK
!      CALL DSMSG ( 777 )
!      CALL MESAGE ( -61, 0, 0 )
99999 END SUBROUTINE mmarm2
