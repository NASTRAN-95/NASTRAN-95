
COMPLEX FUNCTION sumphi(Ixr,Iyr,Nd1,Ndn,Capphi,Dss,N,M,Asym)
   IMPLICIT NONE
   LOGICAL Asym
   INTEGER Ixr , Iyr , M , N
   COMPLEX Capphi(1) , Dss(N,M)
   INTEGER Nd1(1) , Ndn(1)
   INTEGER i , ijphi , ip , iphi , ixs , iys , j , l , ltot
   REAL s
!
!     FUNCTION TO COMPUTE SUM OF CAPPHI-DELTA SOURCE STENGTH PRODUCT
!
!
   sumphi = (0.0,0.0)
   IF ( Ixr==0 ) RETURN
   DO i = 1 , Ixr
      ixs = i - 1
      ip = Ixr - ixs
      ltot = 2*ip + 1
      iphi = (ip*(ip+1))/2
      iys = Iyr - Ixr + ixs
      DO l = 1 , ltot
         IF ( .NOT.(Asym .AND. iys==0) ) THEN
            j = iabs(iys) + 1
            IF ( i>=(Nd1(j)) .AND. i<=Ndn(j) ) THEN
               s = 1.0
               IF ( Asym .AND. iys<0 ) s = -s
               ijphi = iphi + 1 + iabs(Iyr-iys)
               sumphi = sumphi + s*Capphi(ijphi)*Dss(i,j)
            ENDIF
         ENDIF
         iys = iys + 1
      ENDDO
   ENDDO
END FUNCTION sumphi
