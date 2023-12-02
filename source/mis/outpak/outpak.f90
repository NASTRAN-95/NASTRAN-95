!*==outpak.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE outpak(Ii,Iout,Isn)
   IMPLICIT NONE
   USE C_SYSTEM
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ii
   INTEGER , DIMENSION(1) :: Iout
   INTEGER :: Isn
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , icode , j , k , kcode , ll , ncpw , nline , nlpp , op
   INTEGER , DIMENSION(4) :: idig
   INTEGER , SAVE :: nblank
   INTEGER , DIMENSION(10) , SAVE :: number
   EXTERNAL klshft , krshft , orf , page
!
! End of declarations rewritten by SPAG
!
!
   !>>>>EQUIVALENCE (Ksys(2),Op) , (Ksys(9),Nlpp) , (Ksys(12),Nline) , (Ksys(41),Ncpw)
   DATA number/1H1 , 1H2 , 1H3 , 1H4 , 1H5 , 1H6 , 1H7 , 1H8 , 1H9 , 1H0/
   DATA nblank/4H    /
!
   kcode = 0
   IF ( Isn<0 ) kcode = 1
   Isn = iabs(Isn)
   icode = 0
   IF ( Ii>32 ) icode = 1
   IF ( icode==1 ) THEN
      nline = nline + 1
      IF ( nline>nlpp ) THEN
         CALL page
         nline = nline + 1
         WRITE (op,99001)
99001    FORMAT (/,1H )
         nline = nline + 1
      ENDIF
      WRITE (op,99002) (Iout(i),i=2,32)
!
99002 FORMAT (5X,31A4)
      Ii = 5
      IF ( kcode==1 ) Ii = 7
      DO ll = 2 , 32
         Iout(ll) = nblank
      ENDDO
   ENDIF
!
!     TRANSLATE ISN TO DIGITS
!
   idig(1) = Isn/1000
   idig(2) = (Isn-idig(1)*1000)/100
   idig(3) = (Isn-idig(1)*1000-idig(2)*100)/10
   idig(4) = Isn - idig(1)*1000 - idig(2)*100 - idig(3)*10
   DO i = 1 , 4
      IF ( idig(i)==0 ) idig(i) = 10
   ENDDO
!
!     FORM WORD AND STORE IN IOUT ARRAY
!
   k = 0
   DO i = 1 , 4
      j = idig(i)
      k = orf(klshft(krshft(number(j),ncpw-1),ncpw-i),k)
   ENDDO
   Iout(Ii) = k
END SUBROUTINE outpak
