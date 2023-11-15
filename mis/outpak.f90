
SUBROUTINE outpak(Ii,Iout,Isn)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ksys(65) , Ncpw , Nline , Nlpp , Op
   COMMON /system/ Ksys
!
! Dummy argument declarations
!
   INTEGER Ii , Isn
   INTEGER Iout(1)
!
! Local variable declarations
!
   INTEGER i , icode , idig(4) , j , k , kcode , ll , nblank , number(10)
   INTEGER klshft , krshft , orf
   EXTERNAL orf
!
! End of declarations
!
!
   EQUIVALENCE (Ksys(2),Op) , (Ksys(9),Nlpp) , (Ksys(12),Nline) , (Ksys(41),Ncpw)
   DATA number/1H1 , 1H2 , 1H3 , 1H4 , 1H5 , 1H6 , 1H7 , 1H8 , 1H9 , 1H0/
   DATA nblank/4H    /
!
   kcode = 0
   IF ( Isn<0 ) kcode = 1
   Isn = iabs(Isn)
   icode = 0
   IF ( Ii>32 ) icode = 1
   IF ( icode==1 ) THEN
      Nline = Nline + 1
      IF ( Nline>Nlpp ) THEN
         CALL page
         Nline = Nline + 1
         WRITE (Op,99001)
99001    FORMAT (/,1H )
         Nline = Nline + 1
      ENDIF
      WRITE (Op,99002) (Iout(i),i=2,32)
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
      k = orf(klshft(krshft(number(j),Ncpw-1),Ncpw-i),k)
   ENDDO
   Iout(Ii) = k
   RETURN
END SUBROUTINE outpak
