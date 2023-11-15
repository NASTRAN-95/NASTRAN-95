
SUBROUTINE summ(Sum,Isum,Term1,Iterm1,Term2,Iterm2,N)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER Isum , Iterm1 , Iterm2 , N
   DOUBLE PRECISION Sum , Term1 , Term2
!
! Local variable declarations
!
   DOUBLE PRECISION factor , temp1 , temp2
   INTEGER isave , mult
!
! End of declarations
!
!
!
   IF ( Term1/=0.0D0 ) THEN
      IF ( Term2==0.0D0 ) GOTO 100
      temp1 = Term1
      temp2 = Term2
      isave = Iterm1
      IF ( Iterm1==Iterm2 ) GOTO 200
      mult = iabs(Iterm1-Iterm2)
!DVAX TEST TO PREVENT FLOATING PT OVFLOW IF EXPONENT DIFF TOO LARGE
      IF ( mult>37 .AND. Iterm1>Iterm2 ) GOTO 100
      IF ( mult<=37 .OR. Iterm2<=Iterm1 ) THEN
         factor = 10.0D0**mult
         IF ( Iterm1>Iterm2 ) THEN
            temp2 = Term2/factor
         ELSE
            temp1 = Term1/factor
            isave = Iterm2
         ENDIF
         GOTO 200
      ENDIF
   ENDIF
   IF ( N/=1 ) THEN
      Sum = -Term2
   ELSE
      Sum = Term2
   ENDIF
   Isum = Iterm2
   IF ( Sum==0.0D0 ) Isum = 0
   GOTO 99999
 100  Sum = Term1
   Isum = Iterm1
   IF ( Sum==0.0D0 ) Isum = 0
   GOTO 99999
 200  IF ( N/=1 ) THEN
      Sum = temp1 - temp2
      Isum = isave
   ELSE
      Sum = temp1 + temp2
      Isum = isave
   ENDIF
   IF ( Sum==0.0D0 ) Isum = 0
99999 END SUBROUTINE summ
