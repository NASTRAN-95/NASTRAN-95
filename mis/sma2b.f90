
SUBROUTINE sma2b(Ke,J,Ii,Ifile,Dumdp)
   IMPLICIT NONE
   REAL Bggind , Z(1)
   INTEGER Clsnrw , Clsrw , Ecpt(100) , Eor , Frowic , I6x6b , I6x6m , Icstm , Idum1 , Idum2 , Idum3 , Idum4 , Idum5 , Ifbgg ,      &
         & Ifcstm , Ifdit , Ifecpt , Ifgpct , Ifmgg , Ifmpt , Igbgg , Igecpt , Iggpct , Igmgg , Igpct , Inrw , Ioptb , Ipoint ,     &
         & Isys(21) , Iz(1) , Jmax , Left , Link(10) , Linkno , Lnknos(15) , Lrowic , Mask(3) , Mcbbgg(7) , Mcbmgg(7) , N6x6b ,     &
         & N6x6m , Ncstm , Neor , Ngpct , Nlinks , Nobgg , Nogo , Npoint , Npvt , Nrowsc , Outrw , Tnrows
   DOUBLE PRECISION Dz(1)
   COMMON /blank / Nobgg
   COMMON /sem   / Mask , Lnknos
   COMMON /sma2bk/ Icstm , Ncstm , Igpct , Ngpct , Ipoint , Npoint , I6x6m , N6x6m , I6x6b , N6x6b
   COMMON /sma2cl/ Ioptb , Bggind , Npvt , Left , Frowic , Lrowic , Nrowsc , Tnrows , Jmax , Nlinks , Link , Nogo
   COMMON /sma2et/ Ecpt
   COMMON /sma2io/ Ifcstm , Ifmpt , Ifdit , Idum1 , Ifecpt , Igecpt , Ifgpct , Iggpct , Idum2 , Idum3 , Ifmgg , Igmgg , Ifbgg ,     &
                 & Igbgg , Idum4 , Idum5 , Inrw , Outrw , Clsnrw , Clsrw , Neor , Eor , Mcbmgg , Mcbbgg
   COMMON /system/ Isys , Linkno
   COMMON /zzzzzz/ Z
   DOUBLE PRECISION Dumdp
   INTEGER Ifile , Ii , J
   DOUBLE PRECISION Ke(36)
   INTEGER i , i1 , ibase , index , isave , j1 , j2 , j3 , jj , k , k1 , l , l1 , lim , low , nrow
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
            IF ( J<iabs(Iz(i+1)) ) THEN
               IF ( J>=iabs(Iz(i)) ) GOTO 50
            ENDIF
         ENDDO
         IF ( J>=iabs(Iz(isave+1)) ) isave = isave + 1
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
         index = ibase + (nrow-1)*Jmax + Iz(j3) + J - iabs(Iz(j2))
         Dz(index) = Dz(index) + Ke(1)
         GOTO 99999
      ELSE
!
! AT THIS POINT IT HAS BEEN DETERMINED THAT J IS A SCALAR INDEX NUMBER
! WHICH CORRESPONDS TO A GRID POINT.  HENCE THE DOUBLE PRECISION 6 X 6
! MATRIX, KE, WILL BE ADDED TO THE MATRIX.
!
         l1 = Frowic - 1
         jj = Ipoint + isave - Igpct
         j2 = Iz(jj) - 1
         i1 = 0
         lim = Nrowsc - 1
      ENDIF
   ELSE
      CALL emg1b(Ke,J,Ii,Ifile,Dumdp)
      RETURN
   ENDIF
 100  IF ( i1>lim ) RETURN
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
         Dz(k) = Dz(k) + Ke(l)
      ENDIF
   ENDDO
!
99999 RETURN
END SUBROUTINE sma2b