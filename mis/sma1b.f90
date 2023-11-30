
SUBROUTINE sma1b(Ke,J,Ii,Ifile,Dampc)
   IMPLICIT NONE
   INTEGER Clsnrw , Clsrw , Eor , Frowic , I6x64 , I6x6k , Icom , Icstm , Idetck , Idum1 , If4gg , Ifcstm , Ifdit , Ifecpt , Ifgei ,&
         & Ifgpct , Ifgpst , Ifkgg , Ifmpt , Ig4gg , Igecpt , Iggei , Iggpct , Iggpst , Igkgg , Igpct , Inrw , Iopt4 , Ipoint ,     &
         & Isys(21) , Iz(1) , Jmax , K4ggsw , Left , Link(10) , Linkno , Lnknos(15) , Lrowic , Mask(3) , Mcb4gg(7) , Mcbkgg(7) ,    &
         & N6x64 , N6x6k , Ncstm , Neor , Ngpct , Nlinks , Nogo , Npoint , Npvt , Nrowsc , Outrw , Tnrows
   REAL Dodet , Ecpt(100) , Z(1)
   DOUBLE PRECISION Dz(1)
   COMMON /blank / Icom
   COMMON /sem   / Mask , Lnknos
   COMMON /sma1bk/ Icstm , Ncstm , Igpct , Ngpct , Ipoint , Npoint , I6x6k , N6x6k , I6x64 , N6x64
   COMMON /sma1cl/ Iopt4 , K4ggsw , Npvt , Left , Frowic , Lrowic , Nrowsc , Tnrows , Jmax , Nlinks , Link , Idetck , Dodet , Nogo
   COMMON /sma1et/ Ecpt
   COMMON /sma1io/ Ifcstm , Ifmpt , Ifdit , Idum1 , Ifecpt , Igecpt , Ifgpct , Iggpct , Ifgei , Iggei , Ifkgg , Igkgg , If4gg ,     &
                 & Ig4gg , Ifgpst , Iggpst , Inrw , Outrw , Clsnrw , Clsrw , Neor , Eor , Mcbkgg , Mcb4gg
   COMMON /system/ Isys , Linkno
   COMMON /zzzzzz/ Z
   DOUBLE PRECISION Dampc
   INTEGER Ifile , Ii , J
   DOUBLE PRECISION Ke(36)
   INTEGER i , i1 , ibase , index , isave , j1 , j2 , j3 , jj , k , k1 , l , l1 , lim , low , nrow
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
   IF ( Linkno/=Lnknos(8) ) THEN
!
!     DETERMINE WHICH MATRIX IS BEING COMPUTED.
!
      ibase = I6x6k
      IF ( Ifile/=Ifkgg ) THEN
         IF ( Iopt4<0 ) RETURN
         ibase = I6x64
      ENDIF
!
!     SEARCH THE GPCT AND FIND AN INDEX M SUCH THAT
!     IABS(GPCT(M)) .LE. J .LT. IABS(GPCT(M+1))
!
      low = Igpct + 1
      lim = Ngpct + low - 2
      IF ( low>lim ) THEN
!
!     IF II .GT. 0, WE ARE DEALING WITH A SCALAR POINT.
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
!     AT THIS POINT WE ARE DEALING WITH A 1 X 1.
!     FIRST COMPUTE THE ROW NUMBER, NROW
!
         nrow = Ii - Npvt + 1
!
!     THE FOLLOWING 2 FORTRAN STATEMENTS ARE MERELY TO CHECK THE PROGRAM
!     LOGIC.  EVENTUALLY THEY CAN BE DELETED.
!
         IF ( nrow<1 .OR. nrow>Tnrows ) CALL mesage(-30,22,nrow)
         Lrowic = Frowic + Nrowsc - 1
!
!     IF NROW, THE ROW INTO WHICH THE NUMBER KE(1) IS TO BE ADDED IS NOT
!     IN CORE IT CANNOT BE ADDED AT THIS TIME.
!
         IF ( nrow<Frowic .OR. nrow>Lrowic ) RETURN
         j2 = isave
         j3 = Ipoint + isave - Igpct
         index = ibase + (nrow-1)*Jmax + Iz(j3) - iabs(Iz(j2)) + J
         Dz(index) = Dz(index) + Ke(1)
         GOTO 99999
      ELSE
!
!     AT THIS POINT IT HAS BEEN DETERMINED THAT J IS A SCALAR INDEX
!     NUMBER WHICH CORRESPONDS TO A GRID POINT.  HENCE THE DOUBLE
!     PRECISION 6 X 6 MATRIX, KE, WILL BE ADDED TO THE MATRIX.
!
         l1 = Frowic - 1
         jj = Ipoint + isave - Igpct
         j2 = Iz(jj) - 1
         i1 = 0
         lim = Nrowsc - 1
      ENDIF
   ELSE
      CALL emg1b(Ke,J,Ii,Ifile,Dampc)
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
         k = k + 1
         l = l + 1
         IF ( Ifile/=Ifkgg ) THEN
            Dz(k) = Dz(k) + Dampc*Ke(l)
         ELSE
            Dz(k) = Dz(k) + Ke(l)
         ENDIF
      ENDIF
   ENDDO
99999 RETURN
END SUBROUTINE sma1b