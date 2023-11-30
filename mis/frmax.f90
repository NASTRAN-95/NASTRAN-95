
SUBROUTINE frmax(Ifk,Ifm,N,Ipr,Rsn,Rsm)
   IMPLICIT NONE
   INTEGER Incr , Ip , Iprc , Np
   COMMON /unpakx/ Iprc , Ip , Np , Incr
   INTEGER Ifk , Ifm , Ipr , N
   DOUBLE PRECISION Rsm , Rsn
   DOUBLE PRECISION dzk(1) , dzm(1) , ratinv , ratio
   INTEGER i
   REAL zk(1) , zm(1)
   EQUIVALENCE (dzk(1),zk(1)) , (dzm(1),zm(1))
   Iprc = Ipr
   Incr = 1
   Rsn = 0.D0
   Rsm = 0.D0
   DO i = 1 , N
      Ip = i
      Np = i
      CALL unpack(*100,Ifk,dzk(1))
      CALL unpack(*100,Ifm,dzm(1))
      IF ( Ipr==2 ) THEN
         IF ( dzk(1)==0.0D0 .OR. dzm(1)==0.0D0 ) CYCLE
         ratio = dzk(1)/dzm(1)
      ELSE
         IF ( zk(1)==0 .OR. zm(1)==0 ) CYCLE
         ratio = zk(1)/zm(1)
      ENDIF
      ratinv = 1.D0/ratio
      IF ( ratio>Rsm ) Rsm = ratio
      IF ( ratinv>Rsn ) Rsn = ratinv
 100  ENDDO
   Rsn = 1.D0/Rsn
END SUBROUTINE frmax
