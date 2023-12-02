!*==sma2b.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE sma2b(Ke,J,Ii,Ifile,Dumdp)
   IMPLICIT NONE
   USE C_BLANK
   USE C_SEM
   USE C_SMA2BK
   USE C_SMA2CL
   USE C_SMA2ET
   USE C_SMA2IO
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   REAL*8 , DIMENSION(36) :: Ke
   INTEGER :: J
   INTEGER :: Ii
   INTEGER :: Ifile
   REAL*8 :: Dumdp
!
! Local variable declarations rewritten by SPAG
!
   REAL*8 , DIMENSION(1) :: dz
   INTEGER :: i , i1 , ibase , index , isave , j1 , j2 , j3 , jj , k , k1 , l , l1 , lim , low , nrow
   INTEGER , DIMENSION(1) :: iz
!
! End of declarations rewritten by SPAG
!
! ******
! SUBROUTINE SMA2B  ADDS A N X N DOUBLE PRECISION MATRIX, KE, TO THE
! SUBMATRIX OF ORDER NROWSC X JMAX, WHICH IS IN CORE.  N IS 1 IF EITHER
! NPVT, THE PIVOT POINT, IS A  SCALAR POINT, OR J, THE SECOND SUBSCRIPT
! OF KE CORRESPONDS TO A SCALAR POINT, OR J .NE. TO ANY ENTRY IN THE
! GPCT.  OTHERWISE N IS 6.
! ******
!
!
!
!
!
!
!
!
!
!
!
!
! SMA2 I/O PARAMETERS
!
!
! SMA2 VARIABLE CORE
!
!
! SMA2 VARIABLE CORE BOOKKEEPING PARAMETERS
!
!
! SMA2 PROGRAM CONTROL PARAMETERS
!
!
! ECPT COMMON BLOCK
!
!
!
!
   !>>>>EQUIVALENCE (Z(1),Iz(1),Dz(1))
!
!
!     CALL EMG1B AND THEN RETURN IF THIS IS LINK 8.
!     PROCEED NORMALLY FOR OTHER LINKS.
!
   IF ( Linkno/=Lnknos(8) ) THEN
!
! DETERMINE WHICH MATRIX IS BEING COMPUTED.
!
      ibase = I6x6m
      IF ( Ifile/=Ifmgg ) THEN
         IF ( Ioptb<0 ) RETURN
         ibase = I6x6b
      ENDIF
!
! SEARCH THE GPCT AND FIND AN INDEX M SUCH THAT
! IABS(GPCT(M)) .LE. J .LT. IABS(GPCT(M+1))
!
      low = Igpct + 1
      lim = Ngpct + low - 2
      IF ( low>lim ) THEN
!
! IF II .GT. 0, WE ARE DEALING WITH A SCALAR POINT.
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
! AT THIS POINT WE ARE DEALING WITH A 1 X 1.
! FIRST COMPUTE THE ROW NUMBER, NROW
!
         nrow = Ii - Npvt + 1
!
! THE FOLLOWING 2 FORTRAN STATEMENTS ARE MERELY TO CHECK THE PROGRAM
! LOGIC.  EVENTUALLY THEY CAN BE DELETED.
!
         IF ( nrow<1 .OR. nrow>Tnrows ) CALL mesage(-30,22,Ecpt(1))
         Lrowic = Frowic + Nrowsc - 1
!
! IF NROW, THE ROW INTO WHICH THE NUMBER KE(1) IS TO BE ADDED IS NOT
! IN CORE IT CANNOT BE ADDED AT THIS TIME.
!
         IF ( nrow<Frowic .OR. nrow>Lrowic ) RETURN
         j2 = isave
         j3 = Ipoint + isave - Igpct
         index = ibase + (nrow-1)*Jmax + iz(j3) + J - iabs(iz(j2))
         dz(index) = dz(index) + Ke(1)
         GOTO 99999
      ELSE
!
! AT THIS POINT IT HAS BEEN DETERMINED THAT J IS A SCALAR INDEX NUMBER
! WHICH CORRESPONDS TO A GRID POINT.  HENCE THE DOUBLE PRECISION 6 X 6
! MATRIX, KE, WILL BE ADDED TO THE MATRIX.
!
         l1 = Frowic - 1
         jj = Ipoint + isave - Igpct
         j2 = iz(jj) - 1
         i1 = 0
         lim = Nrowsc - 1
      ENDIF
   ELSE
      CALL emg1b(Ke,J,Ii,Ifile,Dumdp)
      RETURN
   ENDIF
   DO
      IF ( i1>lim ) RETURN
      k1 = ibase + i1*Jmax + j2
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
            l = l + 1
            k = k + 1
            dz(k) = dz(k) + Ke(l)
         ENDIF
      ENDDO
      EXIT
 100  ENDDO
!
99999 END SUBROUTINE sma2b
