!*==sma1b.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE sma1b(Ke,J,Ii,Ifile,Dampc)
   IMPLICIT NONE
   USE c_blank
   USE c_sem
   USE c_sma1bk
   USE c_sma1cl
   USE c_sma1et
   USE c_sma1io
   USE c_system
   USE c_zzzzzz
!
! Dummy argument declarations rewritten by SPAG
!
   REAL*8 , DIMENSION(36) :: Ke
   INTEGER :: J
   INTEGER :: Ii
   INTEGER :: Ifile
   REAL*8 :: Dampc
!
! Local variable declarations rewritten by SPAG
!
   REAL*8 , DIMENSION(1) :: dz
   INTEGER :: i , i1 , ibase , index , isave , j1 , j2 , j3 , jj , k , k1 , l , l1 , lim , low , nrow
   INTEGER , DIMENSION(1) :: iz
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     SUBROUTINE SMA1B ADDS A N X N DOUBLE PRECISION MATRIX, KE, TO THE
!     SUBMATRIX OF ORDER NROWSC X JMAX, WHICH IS IN CORE.  N IS 1 IF
!     EITHER  NPVT, THE PIVOT POINT, IS A SCALAR POINT, OR J,THE SECOND
!     SUBSCRIPT OF KE CORRESPONDS TO A SCALAR POINT, OR J .NE. TO ANY
!     ENTRY IN THE GPCT.  OTHERWISE N IS 6.
!
!
!     SMA1 I/O PARAMETERS
!
!
!     SMA1 VARIABLE CORE
!
!
!     SMA1 VARIABLE CORE BOOKKEEPING PARAMETERS
!
!
!     SMA1 PROGRAM CONTROL PARAMETERS
!
!
!     ECPT COMMON BLOCK
!
!
   !>>>>EQUIVALENCE (Z(1),Iz(1),Dz(1))
!
!
!     CALL EMG1B AND THEN RETURN IF THIS IS LINK 8.
!     PROCEED NORMALLY FOR OTHER LINKS.
!
   IF ( linkno/=lnknos(8) ) THEN
!
!     DETERMINE WHICH MATRIX IS BEING COMPUTED.
!
      ibase = i6x6k
      IF ( Ifile/=ifkgg ) THEN
         IF ( iopt4<0 ) RETURN
         ibase = i6x64
      ENDIF
!
!     SEARCH THE GPCT AND FIND AN INDEX M SUCH THAT
!     IABS(GPCT(M)) .LE. J .LT. IABS(GPCT(M+1))
!
      low = igpct + 1
      lim = ngpct + low - 2
      IF ( low>lim ) THEN
!
!     IF II .GT. 0, WE ARE DEALING WITH A SCALAR POINT.
!
         isave = low
      ELSE
         DO i = low , lim
            isave = i
            IF ( J<iabs(iz(i+1)) ) THEN
               IF ( J>=iabs(iz(i)) ) GOTO 50
            ENDIF
         ENDDO
         IF ( J>=iabs(iz(isave+1)) ) isave = isave + 1
      ENDIF
 50   IF ( Ii>0 ) THEN
!
!     AT THIS POINT WE ARE DEALING WITH A 1 X 1.
!     FIRST COMPUTE THE ROW NUMBER, NROW
!
         nrow = Ii - npvt + 1
!
!     THE FOLLOWING 2 FORTRAN STATEMENTS ARE MERELY TO CHECK THE PROGRAM
!     LOGIC.  EVENTUALLY THEY CAN BE DELETED.
!
         IF ( nrow<1 .OR. nrow>tnrows ) CALL mesage(-30,22,nrow)
         lrowic = frowic + nrowsc - 1
!
!     IF NROW, THE ROW INTO WHICH THE NUMBER KE(1) IS TO BE ADDED IS NOT
!     IN CORE IT CANNOT BE ADDED AT THIS TIME.
!
         IF ( nrow<frowic .OR. nrow>lrowic ) RETURN
         j2 = isave
         j3 = ipoint + isave - igpct
         index = ibase + (nrow-1)*jmax + iz(j3) - iabs(iz(j2)) + J
         dz(index) = dz(index) + Ke(1)
         GOTO 99999
      ELSE
!
!     AT THIS POINT IT HAS BEEN DETERMINED THAT J IS A SCALAR INDEX
!     NUMBER WHICH CORRESPONDS TO A GRID POINT.  HENCE THE DOUBLE
!     PRECISION 6 X 6 MATRIX, KE, WILL BE ADDED TO THE MATRIX.
!
         l1 = frowic - 1
         jj = ipoint + isave - igpct
         j2 = iz(jj) - 1
         i1 = 0
         lim = nrowsc - 1
      ENDIF
   ELSE
      CALL emg1b(Ke,J,Ii,Ifile,Dampc)
      RETURN
   ENDIF
   DO
      IF ( i1>lim ) RETURN
      k1 = ibase + i1*jmax + j2
      j1 = 0
      l = 6*l1
      k = k1
      DO
         j1 = j1 + 1
         IF ( j1>6 ) THEN
            i1 = i1 + 1
            l1 = l1 + 1
            GOTO 100
         ELSE
            k = k + 1
            l = l + 1
            IF ( Ifile/=ifkgg ) THEN
               dz(k) = dz(k) + Dampc*Ke(l)
            ELSE
               dz(k) = dz(k) + Ke(l)
            ENDIF
         ENDIF
      ENDDO
      EXIT
 100  ENDDO
99999 END SUBROUTINE sma1b
