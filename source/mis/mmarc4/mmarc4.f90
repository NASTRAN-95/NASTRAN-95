!*==mmarc4.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE mmarc4(Zi,Zd)
   IMPLICIT NONE
   USE I_MMACOM
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Zi
   REAL*8 , DIMENSION(1) :: Zd
!
! Local variable declarations rewritten by SPAG
!
   REAL*8 , DIMENSION(1) :: dxl
   INTEGER :: i , ii , index , jrow , mem , mindex , ntms , ntms2
   INTEGER , DIMENSION(15) :: iblk
!
! End of declarations rewritten by SPAG
!
!  MMARC4 - This routine will store a matrix column in memory in compact
!           form and in complex double precision.  The input matrix can
!           be stored in any precision or type.
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
   !>>>>EQUIVALENCE (Rxl,Dxl)
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
            Zd(mindex+1) = sign*dxl(index+ii-1)
            Zd(mindex+2) = 0.D0
            mindex = mindex + 2
         ENDDO
      ELSEIF ( itype==3 ) THEN
         mindex = mem/2 + 1
         ntms2 = ntms*2
         DO ii = 1 , ntms2
            Zd(mindex+ii) = sign*Rxl(index+ii-1)
         ENDDO
      ELSEIF ( itype==4 ) THEN
         mindex = mem/2 + 1
         ntms2 = ntms*2
         DO ii = 1 , ntms2
            Zd(mindex+ii) = sign*dxl(index+ii-1)
         ENDDO
      ELSE
         mindex = mem/2 + 1
         DO ii = 1 , ntms
            Zd(mindex+1) = sign*Rxl(index+ii-1)
            Zd(mindex+2) = 0.D0
            mindex = mindex + 2
         ENDDO
      ENDIF
      mem = mem + 2 + ntms*4
      CALL endget(iblk)
   ENDDO
 100  lasind = mem - 1
END SUBROUTINE mmarc4
