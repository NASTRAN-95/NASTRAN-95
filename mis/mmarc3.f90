
SUBROUTINE mmarc3(Zi,Zr)
   IMPLICIT NONE
   INCLUDE 'MMACOM.COM'
!
! COMMON variable declarations
!
   INTEGER Ibfsiz , Iwr
   REAL Rxl(1)
   COMMON /system/ Ibfsiz , Iwr
   COMMON /zzzzzz/ Rxl
!
! Dummy argument declarations
!
   INTEGER Zi(1)
   REAL Zr(1)
!
! Local variable declarations
!
   INTEGER i , iblk(15) , ii , index , jrow , mem , mindex , ntms , ntms2
!
! End of declarations
!
!
!  MARRC3 - This routine will store a matrix column in memory in compact
!           form and in complex single precision.  The input matrix is
!           assumed to be stored as either real or complex single precision.
!           The column is stored in memory according to the following scheme:
!
!  1st word = row position of first element in following string
!  2nd word = number of terms in string (ntms)
!  3rd word           }
!     |               }
!     |               } = actual
!     |               }   matrix
!     |               }   string
!     |               }   data
!     |               }
!     |               }
!  3+(ntms*prec)      } (where prec=1 for s.p.;  =2 for d.p. )
!
!  The above data repeats for all strings within a column
!
!  Argument list :
!     ZI  - Memory for storage of data (integer)
!     ZR  - Same location as ZI but real single reference
!
   mem = 1
   DO i = 1 , 15
      iblk(i) = 0
   ENDDO
   iblk(1) = Irfile
   iblk(8) = -1
   Lasind = mem - 1
   Zi(mem) = 0
 100  CALL getstr(*200,iblk)
   Itype = iblk(2)
   jrow = iblk(4)
   index = iblk(5)
   ntms = iblk(6)
   Zi(mem) = jrow
   Zi(mem+1) = ntms
   IF ( Itype==2 ) THEN
!
! THE FOLLOWING LINE SHOULD NEVER BE REFERENCED
!
      WRITE (Iwr,*) ' ERROR IN MMARC3'
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
   GOTO 100
 200  Lasind = mem - 1
END SUBROUTINE mmarc3
