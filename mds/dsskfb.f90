
SUBROUTINE dsskfb(Nn)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER Nn
   INTEGER id , n
   n = Nn
   DO WHILE ( n/=0 )
      DO
         CALL dsbrc1
         id = iand(Ibase(Indclr),Maskq1)
         IF ( id==Idsef ) THEN
            n = n + 1
            EXIT
         ELSEIF ( Nblock==1 ) THEN
            IF ( (Indclr-Indbas)<=5 ) GOTO 99999
         ENDIF
      ENDDO
   ENDDO
99999 RETURN
END SUBROUTINE dsskfb