!*==sumphi.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION sumphi(Ixr,Iyr,Nd1,Ndn,Capphi,Dss,N,M,Asym)
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   INTEGER :: N
   INTEGER :: M
   COMPLEX :: sumphi
   INTEGER :: Ixr
   INTEGER :: Iyr
   INTEGER , DIMENSION(1) :: Nd1
   INTEGER , DIMENSION(1) :: Ndn
   COMPLEX , DIMENSION(1) :: Capphi
   COMPLEX , DIMENSION(N,M) :: Dss
   LOGICAL :: Asym
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ijphi , ip , iphi , ixs , iys , j , l , ltot
   REAL :: s
!
! End of declarations rewritten by SPAG
!
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
