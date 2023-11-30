
SUBROUTINE rombsk(B,Precis,Itdone,Fintg,K,X)
   IMPLICIT NONE
   REAL B , Fintg , Precis
   INTEGER Itdone , K
   REAL X(6)
   REAL const , den , diff , f , faaaa(20) , faaab(20) , faaac , faaad , faaae , faaaf , faaag , faaah
   INTEGER iaaaa , iaaac , iaaad , iaaaf , iret
!
!     THIS SUBROUTINE IS USED TO INTEGRATE A FUNCTION FROM X=0. TO X=B
!
!     SINGLE PRECISION VERSION
!
!     B      = UPPER LIMIT
!     NOSIG  = NUMBER OF CORRECT SIGNIFICANT DIGITS DESIRED
!              (NOT MORE THAN 7) = 5
!     PRECIS = 0.0  UPON RETURN, PRECIS = ACTUAL NUMBER
!              OF SIGNIFICANT DIGITS ATTAINED
!     NUM    = MAXIMUM NUMBER OF HALVINGS OF B-A TO BE MADE
!              (NOT MORE THAN 99) = 15
!
!     UPON RETURN FROM ROMBSK, THE VALUE OF THE INTEGRAL WILL BE FOUND
!     IN FINTG.
!
!     IT IS CUSTOMARY TO MEASURE THE PRECISION OF LARGE NUMBERS IN
!     TERMS OF NUMBER OF SIGNIFICANT DIGITS AND THE ACCURACY OF SMALL
!     NUMBERS IN TERMS OF NUMBER OF SIGNIFICANT DECIMALS.  TO CONFORM
!     TO THIS PRACTICE, THE SUBROUTINE TERMINATES WHEN EITHER OF THESE
!     CONDITIONS IS MET.
!
!
!
   faaac = .00001
   iaaaa = 1
   faaad = B
   X(1) = 0.
   ASSIGN 100 TO iret
   IF ( K==1 ) GOTO 600
   IF ( K==2 ) GOTO 700
   IF ( K==3 ) GOTO 800
 100  faaae = f
   X(1) = B
   ASSIGN 200 TO iret
   IF ( K==1 ) GOTO 600
   IF ( K==2 ) GOTO 700
   IF ( K==3 ) GOTO 800
 200  faaae = faaae + f
   faaaa(1) = 0.5*faaad*faaae
 300  faaad = 0.5*faaad
   iaaac = 2**(iaaaa-1)
   faaae = 0.0
   iaaad = 0
 400  iaaad = iaaad + 1
   faaaf = iaaad
   X(1) = (2.0*faaaf-1.0)*faaad
   ASSIGN 500 TO iret
   IF ( K==1 ) GOTO 600
   IF ( K==2 ) GOTO 700
   IF ( K==3 ) GOTO 800
 500  faaae = faaae + f
   IF ( iaaad<iaaac ) GOTO 400
   faaab(1) = 0.5*faaaa(1) + faaad*faaae
   iaaaa = iaaaa + 1
   DO iaaad = 2 , iaaaa
      faaag = 4.0**(iaaad-1)
      faaah = faaag - 1.0
      iaaaf = iaaad - 1
      faaab(iaaad) = (faaag*faaab(iaaaf)-faaaa(iaaaf))/faaah
   ENDDO
   iaaac = 2*iaaac + 1
   diff = faaab(iaaaa) - faaaa(iaaaa-1)
   IF ( abs(diff)>=abs(faaac*faaab(iaaaa)) ) THEN
      DO iaaad = 1 , iaaaa
         faaaa(iaaad) = faaab(iaaad)
      ENDDO
      IF ( iaaaa<15 ) GOTO 300
   ENDIF
   Precis = diff
   Itdone = iaaaa - 1
   Fintg = faaab(iaaaa)
   RETURN
!
!     THIS CODE REPLACES D4K
!
 600  IF ( X(1)==0. ) THEN
      f = 0.
   ELSE
      den = X(3) - X(2)*X(5) + X(2)*X(5)*cos(X(1)) + X(2)*X(4)*sin(X(1))
      f = X(1)**(X(6)-1.)*sin(X(1))**2/den
   ENDIF
   GOTO iret
!
!     THIS CODE REPLACES D5K
!
 700  IF ( X(1)==0. ) THEN
      f = 0.
   ELSE
      den = X(3) - X(2)*X(5) + X(2)*X(5)*cos(X(1)) + X(2)*X(4)*sin(X(1))
      f = X(1)**(X(6)-1.)*2.*sin(X(1))*cos(X(1))/den
   ENDIF
   GOTO iret
!
!     THIS CODE REPLACES D6K
!
 800  den = X(3) - X(2)*X(5) + X(2)*X(5)*cos(X(1)) + X(2)*X(4)*sin(X(1))
   IF ( X(6)==1.0 ) const = 1.0
   IF ( X(6)/=1.0 ) const = X(1)**(X(6)-1.0)
   IF ( den==0. ) THEN
      f = 0.
   ELSE
      f = const*cos(X(1))**2/den
   ENDIF
   GOTO iret
END SUBROUTINE rombsk
