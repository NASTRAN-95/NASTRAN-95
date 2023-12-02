!*==geturn.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE geturn(Namfil)
   IMPLICIT NONE
   USE I_DSIOF
   USE C_DSUNIT
   USE C_XFIAT
   USE C_XFIST
   USE C_XXFIAT
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Namfil
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: ifst , lim
   INTEGER , SAVE :: mask
!
! End of declarations rewritten by SPAG
!
   DATA mask/'00007FFF'x/
   IF ( Namfil/=lasnam .OR. ifilex==0 ) THEN
      ifilex = 0
      lim = 2*Ifstca - 1
      DO ifst = 1 , lim , 2
         IF ( Namfil==Ifist(ifst) ) THEN
            IF ( Namfil>=101 .AND. Namfil<=320 ) THEN
               ifilex = iand(Ifiat(Ifist(ifst+1)-2),mask)
               IF ( ifilex<=maxpri ) THEN
                  Iunit(Namfil-100) = ifilex
                  GOTO 100
               ENDIF
            ELSEIF ( Ifist(ifst+1)>0 ) THEN
               ifilex = iand(Ifiat(Ifist(ifst+1)-2),mask)
               GOTO 100
            ELSE
               ifilex = Ixfiat(iabs(Ifist(ifst+1))+1)
               IF ( ifilex<=maxpri ) GOTO 100
            ENDIF
            ifilex = 0
            EXIT
         ENDIF
      ENDDO
      GOTO 99999
   ENDIF
 100  iprvop = fcb(1,ifilex)
   IF ( iprvop==2 ) iprvop = 0
   nlr = fcb(3,ifilex)
   nblock = fcb(4,ifilex)
   lasnam = Namfil
99999 END SUBROUTINE geturn
