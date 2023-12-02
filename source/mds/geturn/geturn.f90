!*==geturn.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE geturn(Namfil)
   USE i_dsiof
   USE c_dsunit
   USE c_xfiat
   USE c_xfist
   USE c_xxfiat
   IMPLICIT NONE
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
      lim = 2*ifstca - 1
      SPAG_Loop_1_1: DO ifst = 1 , lim , 2
         IF ( Namfil==ifist(ifst) ) THEN
            IF ( Namfil>=101 .AND. Namfil<=320 ) THEN
               ifilex = iand(ifiat(ifist(ifst+1)-2),mask)
               IF ( ifilex<=maxpri ) THEN
                  iunit(Namfil-100) = ifilex
                  CALL spag_block_1
                  RETURN
               ENDIF
            ELSEIF ( ifist(ifst+1)>0 ) THEN
               ifilex = iand(ifiat(ifist(ifst+1)-2),mask)
               CALL spag_block_1
               RETURN
            ELSE
               ifilex = ixfiat(iabs(ifist(ifst+1))+1)
               IF ( ifilex<=maxpri ) THEN
                  CALL spag_block_1
                  RETURN
               ENDIF
            ENDIF
            ifilex = 0
            EXIT SPAG_Loop_1_1
         ENDIF
      ENDDO SPAG_Loop_1_1
      RETURN
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      iprvop = fcb(1,ifilex)
      IF ( iprvop==2 ) iprvop = 0
      nlr = fcb(3,ifilex)
      nblock = fcb(4,ifilex)
      lasnam = Namfil
   END SUBROUTINE spag_block_1
END SUBROUTINE geturn
