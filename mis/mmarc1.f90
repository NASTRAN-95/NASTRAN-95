
 
SUBROUTINE mmarc1(Zi,Zr)
   IMPLICIT NONE
   INCLUDE 'MMACOM.COM'
   REAL Rxl(1)
   COMMON /zzzzzz/ Rxl
   INTEGER Zi(1)
   REAL Zr(1)
   INTEGER i , iblk(15) , ii , index , jrow , mem , ntms
!  MMARC1 - This routine will store a matrix column in memory in compact
!           form and in real single precision.  The input matrix is
!           assumed to be stored as real single precision.
!           The column is stored in memory according to the following scheme:
!
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
   DO
      CALL getstr(*100,iblk)
      jrow = iblk(4)
      index = iblk(5)
      ntms = iblk(6)
      Zi(mem) = jrow
      Zi(mem+1) = ntms
      mem = mem + 1
      DO ii = 1 , ntms
         Zr(mem+ii) = Sign*Rxl(index+ii-1)
      ENDDO
      mem = mem + 1 + ntms
      CALL endget(iblk)
   ENDDO
 100  Lasind = mem - 1
END SUBROUTINE mmarc1