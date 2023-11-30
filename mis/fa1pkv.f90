
SUBROUTINE fa1pkv(Az,Amk,Amb,N,E1,Cz,Bref,Pi,Vel,Ibuf)
   IMPLICIT NONE
   INTEGER Lines , Nlpp , Nout
   REAL Space(6) , Sysbuf , X(2)
   COMMON /system/ Sysbuf , Nout , Space , Nlpp , X , Lines
   REAL Bref , Pi , Vel
   INTEGER N
   REAL Amb(1) , Amk(1) , Cz(1) , E1(5)
   COMPLEX Az(1)
   INTEGER Ibuf(1)
   COMPLEX ceig , eigen , eigz
   REAL e(2) , v(6)
   INTEGER i , ipass , iscr , iv(6) , j , k , n2 , na , nb , nc , nd , trl(7)
!
   EQUIVALENCE (v(1),iv(1)) , (eigen,e(1))
   DATA iscr/301/ , ipass/0/
!
   eigz = (0.0,0.0)
   IF ( N>=2 ) THEN
      e(1) = E1(1)
      e(2) = E1(2)
      IF ( ipass/=0 ) THEN
         CALL open(*100,iscr,Ibuf,3)
      ELSE
         CALL open(*100,iscr,Ibuf,1)
      ENDIF
      ipass = ipass + 1
!
!     BUILD A = IP2 + M-1B P + M-1K
!
      ceig = eigen*eigen
      k = 0
      DO i = 1 , N
         DO j = 1 , N
            k = k + 1
            Az(k) = -Amb(k)*eigen - Amk(k)
            IF ( i==j ) Az(k) = Az(k) + ceig
         ENDDO
      ENDDO
!
!     CORE FOR EGNVCT
!
      n2 = N*2
      na = 1 + n2*N
      nb = na + n2
      nc = nb + n2
      nd = nc + n2
      CALL egnvct(Az,Cz(na),eigz,Cz(nb),Cz(nc),Cz(nd),N)
!
!     BUILD ON SCR1 DATA FOR VECTOR OUTPUT
!
      iv(1) = ipass
      iv(2) = ipass
      v(3) = E1(1)
      v(4) = E1(2)
      IF ( E1(2)==0.0 ) THEN
         v(5) = 0.0
         v(6) = (Bref/(.34657*Vel))*E1(1)
      ELSE
         v(5) = E1(3)
         v(6) = E1(5)
      ENDIF
      CALL write(iscr,iv,6,1)
      CALL write(iscr,Cz(nb),n2,1)
!
!     VECTOR IS IN CZ(NB)
!
      Lines = Nlpp
      k = 0
      DO i = 1 , N
         IF ( Lines>=Nlpp ) THEN
            CALL page1
            WRITE (Nout,99001) eigen
99001       FORMAT (1H0,47X,30HEIGENVECTOR FROM THE PK METHOD,/3X,13HEIGENVALUE = ,1P,E15.5,1P,E15.5,//3X,11HEIGENVECTOR)
            Lines = Lines + 5
         ENDIF
         Lines = Lines + 1
         WRITE (Nout,99002) Cz(nb+k) , Cz(nb+k+1)
99002    FORMAT (16X,1P,E15.5,1P,E15.5)
         k = k + 2
      ENDDO
      trl(1) = iscr
      trl(2) = 1
      CALL wrttrl(trl)
   ENDIF
 100  CALL close(iscr,3)
END SUBROUTINE fa1pkv
