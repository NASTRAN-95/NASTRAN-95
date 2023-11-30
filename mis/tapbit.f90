
LOGICAL FUNCTION tapbit(File)
   IMPLICIT NONE
   INTEGER Fiat(1) , Fist(1) , Ib(45) , Itwo(32) , Lfiat , Lfist , Mfiat , Nfiat , Nfist , Npfist , Xfiat(1)
   COMMON /system/ Ib
   COMMON /two   / Itwo
   COMMON /xfiat / Mfiat , Nfiat , Lfiat , Fiat
   COMMON /xfist / Nfist , Lfist , Fist
   COMMON /xpfist/ Npfist
   COMMON /xxfiat/ Xfiat
   INTEGER File
   INTEGER andf
   INTEGER j , nam(2) , npf1
   EXTERNAL andf
!
   DATA nam/4HTAPB , 4HIT  /
!
   tapbit = .TRUE.
   DO j = 1 , Npfist
      IF ( Fist(2*j-1)==File ) GOTO 100
   ENDDO
   npf1 = Npfist + 1
   DO j = npf1 , Lfist
      IF ( Fist(2*j-1)==File ) GOTO 200
   ENDDO
   CALL mesage(-21,File,nam)
!
 100  j = -Fist(2*j)
   IF ( andf(Itwo(32-j),Ib(45))/=0 ) RETURN
   IF ( andf(Xfiat(j+1),32768)==0 ) tapbit = .FALSE.
   RETURN
!
 200  j = Fist(2*j)
   IF ( andf(Fiat(j+1),32768)==0 ) tapbit = .FALSE.
END FUNCTION tapbit