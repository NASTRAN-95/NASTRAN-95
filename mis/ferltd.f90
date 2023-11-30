
SUBROUTINE ferltd(Ifile,Dz,Dy,Zm)
   IMPLICIT NONE
   DOUBLE PRECISION Dcore(1)
   INTEGER Ibflt , Ibforv , Ibfsma , Icore(1) , Incr , Ip , Ityp , Ltpos(7) , Nidlt , Nidorv , Nidsma , Nltli , Np , Nsmali ,       &
         & Smapos(7)
   COMMON /feerim/ Nidsma , Nidlt , Nidorv , Nltli , Nsmali , Ibfsma , Ibflt , Ibforv , Smapos , Ltpos
   COMMON /unpakx/ Ityp , Ip , Np , Incr
   COMMON /zzzzzz/ Icore
   DOUBLE PRECISION Dy(1) , Dz(1) , Zm(1)
   INTEGER Ifile(7)
   DOUBLE PRECISION dsum
   INTEGER i , iccol , icol , ii , ilcol , indx , j , mem , n , ntms
!
!  FERLTD was originally subroutine FRMLTD.  FERLTD allows for
!  reading the input matrix from core and after the core data is
!  exhausted, then reading the remaining data from the file.
!  See subroutine FERRDM for how data is stored within memory for the
!  matrix and for the contents of SMAPOS.
!
!   FEER MATRIX TRANSPOSE MULTIPLY  (DOUBLE PREC)
!
   !>>>>EQUIVALENCE (Dcore(1),Icore(1))
   n = Ifile(2)
   iccol = 1
   IF ( Nidsma==0 ) THEN
      CALL rewind(Ifile)
      CALL skprec(Ifile,1)
      GOTO 200
   ELSE
      mem = Nidsma
      ilcol = Smapos(1)
      DO i = 1 , n
         iccol = i
! CHECK TO SEE IF REMAINING DATA IS ON THE FILE AND NOT IN MEMORY
         IF ( iccol>ilcol ) GOTO 100
         Dy(i) = 0.D0
         dsum = 0.D0
         DO
            icol = Icore(mem)
            IF ( icol/=i ) EXIT
            ntms = Icore(mem+1)
            Ip = Icore(mem+2+2*ntms)
            Np = Ip + ntms - 1
            indx = mem/2 + 1
            ii = 0
            DO j = Ip , Np
               ii = ii + 1
               dsum = dsum + Dcore(indx+ii)*Dz(j)
            ENDDO
            Dy(i) = dsum
            mem = mem + 4 + 2*ntms
         ENDDO
      ENDDO
      GOTO 99999
   ENDIF
 100  CALL dsspos(Ifile,Smapos(2),Smapos(3),Smapos(4))
 200  Incr = 1
   Ityp = Ifile(5)
   DO i = iccol , n
      Dy(i) = 0.D0
      Ip = 0
      CALL unpack(*300,Ifile,Zm(1))
      ii = 0
      dsum = 0.D0
      DO j = Ip , Np
         ii = ii + 1
         dsum = dsum + Zm(ii)*Dz(j)
      ENDDO
      Dy(i) = dsum
 300  ENDDO
99999 RETURN
END SUBROUTINE ferltd