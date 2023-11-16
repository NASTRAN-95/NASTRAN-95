
SUBROUTINE geturn(Namfil)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
!
! COMMON variable declarations
!
   INTEGER Ifatca , Ifatmx , Ifatuf , Ifiat(640) , Ifist(100) , Ifstca , Ifstmx , Ixfiat(19)
   INTEGER*2 Iunit(220)
   COMMON /dsunit/ Iunit
   COMMON /xfiat / Ifatuf , Ifatmx , Ifatca , Ifiat
   COMMON /xfist / Ifstmx , Ifstca , Ifist
   COMMON /xxfiat/ Ixfiat
!
! Dummy argument declarations
!
   INTEGER Namfil
!
! Local variable declarations
!
   INTEGER ifst , lim , mask
!
! End of declarations
!
   DATA mask/'00007FFF'x/
   IF ( Namfil/=Lasnam .OR. Ifilex==0 ) THEN
      Ifilex = 0
      lim = 2*Ifstca - 1
      DO ifst = 1 , lim , 2
         IF ( Namfil==Ifist(ifst) ) THEN
            IF ( Namfil>=101 .AND. Namfil<=320 ) THEN
               Ifilex = iand(Ifiat(Ifist(ifst+1)-2),mask)
               IF ( Ifilex<=MAXPRI ) THEN
                  Iunit(Namfil-100) = Ifilex
                  GOTO 100
               ENDIF
            ELSEIF ( Ifist(ifst+1)>0 ) THEN
               Ifilex = iand(Ifiat(Ifist(ifst+1)-2),mask)
               GOTO 100
            ELSE
               Ifilex = Ixfiat(iabs(Ifist(ifst+1))+1)
               IF ( Ifilex<=MAXPRI ) GOTO 100
            ENDIF
            Ifilex = 0
            EXIT
         ENDIF
      ENDDO
      GOTO 99999
   ENDIF
 100  Iprvop = Fcb(1,Ifilex)
   IF ( Iprvop==2 ) Iprvop = 0
   Nlr = Fcb(3,Ifilex)
   Nblock = Fcb(4,Ifilex)
   Lasnam = Namfil
99999 RETURN
END SUBROUTINE geturn
