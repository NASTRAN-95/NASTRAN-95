!*==fa1pkv.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fa1pkv(Az,Amk,Amb,N,E1,Cz,Bref,Pi,Vel,Ibuf)
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   COMPLEX , DIMENSION(1) :: Az
   REAL , DIMENSION(1) :: Amk
   REAL , DIMENSION(1) :: Amb
   INTEGER :: N
   REAL , DIMENSION(5) :: E1
   REAL , DIMENSION(1) :: Cz
   REAL :: Bref
   REAL :: Pi
   REAL :: Vel
   INTEGER , DIMENSION(1) :: Ibuf
!
! Local variable declarations rewritten by SPAG
!
   COMPLEX :: ceig , eigen , eigz
   REAL , DIMENSION(2) :: e
   INTEGER :: i , j , k , n2 , na , nb , nc , nd
   INTEGER , SAVE :: ipass , iscr
   INTEGER , DIMENSION(6) :: iv
   INTEGER , DIMENSION(7) :: trl
   REAL , DIMENSION(6) :: v
   EXTERNAL close , egnvct , open , page1 , write , wrttrl
!
! End of declarations rewritten by SPAG
!
!
   !>>>>EQUIVALENCE (v(1),iv(1)) , (eigen,e(1))
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
      lines = nlpp
      k = 0
      DO i = 1 , N
         IF ( lines>=nlpp ) THEN
            CALL page1
            WRITE (nout,99001) eigen
99001       FORMAT (1H0,47X,30HEIGENVECTOR FROM THE PK METHOD,/3X,13HEIGENVALUE = ,1P,E15.5,1P,E15.5,//3X,11HEIGENVECTOR)
            lines = lines + 5
         ENDIF
         lines = lines + 1
         WRITE (nout,99002) Cz(nb+k) , Cz(nb+k+1)
99002    FORMAT (16X,1P,E15.5,1P,E15.5)
         k = k + 2
      ENDDO
      trl(1) = iscr
      trl(2) = 1
      CALL wrttrl(trl)
   ENDIF
 100  CALL close(iscr,3)
END SUBROUTINE fa1pkv
