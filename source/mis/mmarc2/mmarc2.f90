!*==mmarc2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mmarc2(Zi,Zd)
   USE i_mmacom
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Zi
   REAL*8 , DIMENSION(1) :: Zd
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ii , index , jrow , mem , mindex , ntms
   INTEGER , DIMENSION(15) :: iblk
   REAL , DIMENSION(1) :: rxl
!
! End of declarations rewritten by SPAG
!
!  MMARC2 - This routine will store a matrix column in memory in compact
!           form and in real double precision.  The input matrix is
!           assumed to be stored as either real single or double precision.
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
!     ZD  - Same location as ZI but real double reference
!
   !>>>>EQUIVALENCE (Dxl,Rxl)
   mem = 1
   DO i = 1 , 15
      iblk(i) = 0
   ENDDO
   iblk(1) = irfile
   iblk(8) = -1
   lasind = mem - 1
   Zi(mem) = 0
   DO
      CALL getstr(*100,iblk)
      itype = iblk(2)
      jrow = iblk(4)
      index = iblk(5)
      ntms = iblk(6)
      Zi(mem) = jrow
      Zi(mem+1) = ntms
      IF ( itype==2 ) THEN
         mindex = mem/2 + 1
         DO ii = 1 , ntms
            Zd(mindex+ii) = sign*dxl(index+ii-1)
         ENDDO
      ELSE
         mindex = mem/2 + 1
         DO ii = 1 , ntms
            Zd(mindex+ii) = sign*rxl(index+ii-1)
         ENDDO
      ENDIF
      mem = mem + 2 + ntms*2
      CALL endget(iblk)
   ENDDO
 100  lasind = mem - 1
END SUBROUTINE mmarc2