
SUBROUTINE ds1b(Ke,J)
   IMPLICIT NONE
   INTEGER Clsrw , Cstm , Dit , Ecptds , Eor , Frowic , I6x6k , Icstm , Igpct , Inrw , Ipoint , Iz(1) , Jmax , Kggd , Link(10) ,    &
         & Lrowic , Mpt , N6x6k , Ncstm , Neor , Ngpct , Nlinks , Nogo , Npoint , Npvt , Nrowsc , Outrw
   DOUBLE PRECISION Dz(1)
   REAL Gpct , Z(1)
   COMMON /ds1aaa/ Npvt , Icstm , Ncstm , Igpct , Ngpct , Ipoint , Npoint , I6x6k , N6x6k , Cstm , Mpt , Dit , Ecptds , Gpct ,      &
                 & Kggd , Inrw , Outrw , Eor , Neor , Clsrw , Jmax , Frowic , Lrowic , Nrowsc , Nlinks , Link , Nogo
   COMMON /zzzzzz/ Z
   INTEGER J
   DOUBLE PRECISION Ke(36)
   INTEGER i , i1 , isave , j1 , j2 , jj , k , k1 , l , l1 , lim , low
!
!     THIS ROUTINE ADDS THE 6 X 6 DOUBLE PRECISION MATRIX KE TO THE
!     SUBMATRIX OF ORDER NROWSC X JMAX.
!
   !>>>>EQUIVALENCE (Dz(1),Z(1),Iz(1))
!
!     SEARCH THE GPCT AND FIND AN INDEX M SUCH THAT
!     IABS(GPCT(M)) .LE. J .LT. IABS(GPCT(M+1))
!
   low = Igpct + 1
   lim = Ngpct + low - 2
   IF ( low>lim ) THEN
      isave = low
   ELSE
      DO i = low , lim
         isave = i
         IF ( J<iabs(Iz(i+1)) ) THEN
            IF ( J>=iabs(Iz(i)) ) GOTO 100
         ENDIF
      ENDDO
      IF ( J>=iabs(Iz(isave+1)) ) isave = isave + 1
   ENDIF
!
!     ADD KE TO THE SUBMATRIX
!
 100  l1 = Frowic - 1
   jj = Ipoint + isave - Igpct
   j2 = Iz(jj) - 1
   i1 = 0
   lim = Nrowsc - 1
 200  IF ( i1>lim ) RETURN
   k1 = I6x6k + i1*Jmax + j2
   j1 = 0
   l = 6*l1
   k = k1
   DO
      j1 = j1 + 1
      IF ( j1>6 ) THEN
         i1 = i1 + 1
         l1 = l1 + 1
         GOTO 200
      ELSE
         k = k + 1
         l = l + 1
         Dz(k) = Dz(k) + Ke(l)
      ENDIF
   ENDDO
END SUBROUTINE ds1b