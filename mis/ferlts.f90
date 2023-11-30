
SUBROUTINE ferlts(Ifile,Dz,Dy,Zm)
   IMPLICIT NONE
   REAL Dcore(1)
   INTEGER Ibflt , Ibforv , Ibfsma , Icore(1) , Incr , Ip , Iprc , Ltpos(7) , Nidlt , Nidorv , Nidsma , Nltli , Np , Nsmali ,       &
         & Smapos(7)
   COMMON /feerim/ Nidsma , Nidlt , Nidorv , Nltli , Nsmali , Ibfsma , Ibflt , Ibforv , Smapos , Ltpos
   COMMON /unpakx/ Iprc , Ip , Np , Incr
   COMMON /zzzzzz/ Icore
   REAL Dy(1) , Dz(1) , Zm(1)
   INTEGER Ifile(7)
   REAL dsum
   INTEGER i , iccol , icol , ii , ilcol , indx , j , mem , n , ntms
!
!   FEER MATRIX TRANSPOSE MULTIPLY  (SINGLE PRECISION)
!   SEE SUBROUTINE FERRDM FOR CONTENTS OF SMAPOS AND HOW THE MATRIX
!   DATA IS STORED IN MEMORY.
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
         Dy(i) = 0.
         dsum = 0.
         DO
            icol = Icore(mem)
            IF ( icol/=i ) EXIT
            ntms = Icore(mem+1)
            Ip = Icore(mem+2+ntms)
            Np = Ip + ntms - 1
            indx = mem + 1
            ii = 0
            DO j = Ip , Np
               ii = ii + 1
               dsum = dsum + Dcore(indx+ii)*Dz(j)
            ENDDO
            Dy(i) = dsum
            mem = mem + 4 + ntms
         ENDDO
      ENDDO
      GOTO 99999
   ENDIF
 100  CALL dsspos(Ifile,Smapos(2),Smapos(3),Smapos(4))
 200  Incr = 1
   Iprc = Ifile(5)
   DO i = iccol , n
      Dy(i) = 0.
      Ip = 0
      CALL unpack(*300,Ifile,Zm(1))
      ii = 0
      dsum = 0.0
      DO j = Ip , Np
         ii = ii + 1
         dsum = dsum + Zm(ii)*Dz(j)
      ENDDO
      Dy(i) = dsum
 300  ENDDO
99999 RETURN
END SUBROUTINE ferlts