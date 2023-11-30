
SUBROUTINE mpy3nu(Iz)
   IMPLICIT NONE
   REAL Dum1(4) , Dum2(8) , Sysbuf
   INTEGER Iacols , Icore , Id , Ipoint , Itrl , J , Laend , N , Ncb , Nout , Ntbu , Zpntrs(22)
   CHARACTER*23 Ufm
   COMMON /mpy3cp/ Itrl , Icore , N , Ncb , Dum1 , Zpntrs , Laend , Dum2 , J , Id , Ntbu
   COMMON /system/ Sysbuf , Nout
   COMMON /xmssg / Ufm
   INTEGER Iz(1)
   INTEGER l , l1 , l2 , lac , ll , lp , name(2)
!
!     CALCULATES NEXT TIME USED FOR INDIVIDUAL COLUMNS OF B OR FOR ROWS
!     CORRESPONDING TO NON-ZERO TERMS IN COLUMN OF A.
!
   !>>>>EQUIVALENCE (Ipoint,Zpntrs(3)) , (Iacols,Zpntrs(5))
   DATA name/4HMPY3 , 4HNU  /
!
!     CALCULATION BY SEARCH THROUGH ROW OF A IN QUESTION.
!
   lp = Ipoint + Id - 1
   l1 = Iz(lp)
   IF ( l1==0 ) THEN
!
!    ERROR MESSAGE.
!
      WRITE (Nout,99001) Ufm
99001 FORMAT (A23,' 6557, UNEXPECTED NULL COLUMN OF A(T) ENCOUNTERED.')
      CALL mesage(-37,0,name)
   ELSE
      IF ( Id/=Ncb ) THEN
         ll = Id + 1
         DO l = ll , Ncb
            lp = lp + 1
            IF ( Iz(lp)/=0 ) THEN
               l2 = Iz(lp) - 1
               GOTO 50
            ENDIF
         ENDDO
      ENDIF
      l2 = Laend
 50   lac = Iacols + l1 - 2
      DO l = l1 , l2
         lac = lac + 1
         IF ( J<Iz(lac) ) GOTO 100
      ENDDO
      Ntbu = 99999999
   ENDIF
   GOTO 99999
 100  Ntbu = Iz(lac)
!
99999 RETURN
END SUBROUTINE mpy3nu