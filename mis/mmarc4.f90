
SUBROUTINE mmarc4(Zi,Zd)
   IMPLICIT NONE
   INCLUDE 'MMACOM.COM'
   DOUBLE PRECISION Dxl(1)
   REAL Rxl(1)
   COMMON /zzzzzz/ Rxl
   DOUBLE PRECISION Zd(1)
   INTEGER Zi(1)
   INTEGER i , iblk(15) , ii , index , jrow , mem , mindex , ntms , ntms2
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
   EQUIVALENCE (Rxl,Dxl)
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
      Itype = iblk(2)
      jrow = iblk(4)
      index = iblk(5)
      ntms = iblk(6)
      Zi(mem) = jrow
      Zi(mem+1) = ntms
      IF ( Itype==2 ) THEN
         mindex = mem/2 + 1
         DO ii = 1 , ntms
            Zd(mindex+1) = Sign*Dxl(index+ii-1)
            Zd(mindex+2) = 0.D0
            mindex = mindex + 2
         ENDDO
      ELSEIF ( Itype==3 ) THEN
         mindex = mem/2 + 1
         ntms2 = ntms*2
         DO ii = 1 , ntms2
            Zd(mindex+ii) = Sign*Rxl(index+ii-1)
         ENDDO
      ELSEIF ( Itype==4 ) THEN
         mindex = mem/2 + 1
         ntms2 = ntms*2
         DO ii = 1 , ntms2
            Zd(mindex+ii) = Sign*Dxl(index+ii-1)
         ENDDO
      ELSE
         mindex = mem/2 + 1
         DO ii = 1 , ntms
            Zd(mindex+1) = Sign*Rxl(index+ii-1)
            Zd(mindex+2) = 0.D0
            mindex = mindex + 2
         ENDDO
      ENDIF
      mem = mem + 2 + ntms*4
      CALL endget(iblk)
   ENDDO
 100  Lasind = mem - 1
END SUBROUTINE mmarc4
