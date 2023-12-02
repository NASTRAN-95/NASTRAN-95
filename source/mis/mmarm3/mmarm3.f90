!*==mmarm3.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mmarm3(Zi,Zr,Mempcol)
   USE i_mmacom
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Zi
   REAL , DIMENSION(1) :: Zr
   INTEGER :: Mempcol
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , icblk , icbp , iclr , icol , ii , index , jrow , mem , mem1 , mindex , ntms , ntms2
   INTEGER , DIMENSION(15) :: iblk
   INTEGER , DIMENSION(2) , SAVE :: module
   INTEGER :: spag_nextblock_1
!
! End of declarations rewritten by SPAG
!
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         spag_nextblock_1 = 2
      CASE (2)
         iblk(8) = -1
         lasindm = mem - 1
         CALL dscpos(irfile,icblk,iclr,icbp)
         CALL getstr(*20,iblk)
!      IF ( ICOL .NE. IBLK( 12 ) ) GO TO 7001
         Zi(mem) = -icol
         mem1 = mem + 1
         mem = mem + 2
         DO
            ntms = iblk(6)
            IF ( (mem+2+ntms*2)>lasmem ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            itype = iblk(2)
            jrow = iblk(4)
            index = iblk(5)
            Zi(mem) = jrow
            Zi(mem+1) = ntms
            IF ( itype==2 ) THEN
!
! THE FOLLOWING LINE SHOULD NEVER BE REFERENCED
!
               WRITE (iwr,*) ' ERROR IN MMARM3'
               STOP
            ELSEIF ( itype==3 ) THEN
               mindex = mem + 1
               ntms2 = ntms*2
               DO ii = 1 , ntms2
                  Zr(mindex+ii) = sign*rxl(index+ii-1)
               ENDDO
            ELSE
               mindex = mem + 2
               DO ii = 1 , ntms
                  Zr(mindex) = sign*rxl(index+ii-1)
                  Zr(mindex+1) = 0.
                  mindex = mindex + 2
               ENDDO
            ENDIF
            mem = mem + 2 + ntms*2
            CALL endget(iblk)
            CALL getstr(*40,iblk)
         ENDDO
 20      Zi(mem) = -icol
         mem1 = mem + 1
         mem = mem + 2
!
! CHECK IF SPACE AVAILABLE FOR A FULL COLUMN OF "D" MATRIX, IF NECESSARY
!
 40      IF ( mem<=(lasmem-Mempcol) ) THEN
            lasmem = lasmem - Mempcol
            Zi(mem1) = mem
            icol = icol + 1
            IF ( icol<=ircoln ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            lasindm = mem - 1
            RETURN
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         lasindm = mem1 - 2
!
! SAVE I/O LOCATION OF LAST COLUMN FOR NEXT PASS
!
         irpos(1) = icblk
         irpos(2) = iclr
         irpos(3) = icbp
         ircoln = icol - 1
         IF ( ircoln<ircol1 ) CALL mesage(-8,mem+Mempcol,module)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!      GO TO 7777
!7001  WRITE( IWR, 9001 ) ICOL, IBLK(12), IRFILE
!9001  FORMAT(' ERROR OCCURRED IN MMARM3, EXPECTED COLUMN =',I10
!     &,/,    ' BUT READ COLUMN =',I10,' FROM FILE =',I5 )
!      CALL MESAGE ( -61, 0, 0 )
END SUBROUTINE mmarm3
