!*==mbmode.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mbmode(Input,Out,Icor,Ncor,Z,Ni,Nd,Xd,Yd,Is,Cr)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Input
   REAL :: Out
   INTEGER :: Icor
   INTEGER :: Ncor
   REAL , DIMENSION(1) :: Z
   INTEGER :: Ni
   INTEGER :: Nd
   REAL , DIMENSION(1) :: Xd
   REAL , DIMENSION(1) :: Yd
   INTEGER :: Is
   REAL :: Cr
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , icc , idp , j , l , m , ncore , nnd , nni
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL fread , mesage , ssplin , write
!
! End of declarations rewritten by SPAG
!
!
!     MBMODE BUILDS THE MODE LIKE DATA ON OUT FROM SURFACE SPLINE INTER
!
   DATA name/4HMBMO , 4HDE  /
   nni = Ni*2
   nnd = Nd*2
   IF ( Icor+nni+nnd>Ncor ) CALL mesage(-8,0,name)
   CALL fread(Input,Z(Icor),nni,0)
   idp = Icor + nni
   l = 0
   DO i = 1 , Nd
      Z(idp+l) = Xd(i)
      Z(idp+l+1) = Yd(i)
      l = l + 2
   ENDDO
   icc = idp + l
   ncore = Ncor - icc
!
!     CALL SSPLIN TO INTERPOLATE
!
   CALL ssplin(Ni,Z(Icor),Nd,Z(idp),0,0,1,0,0.0,Z(icc),ncore,Is)
   IF ( Is/=2 ) THEN
!
!     REORDER INTO MACH BOX ORDER
!
      m = idp + Nd
      icc = icc - 1
      DO i = 1 , Ni
         l = 0
         DO j = 1 , nnd , 2
            Z(idp+l) = Z(icc+j)
            Z(m+l) = Z(icc+j+1)*Cr
            l = l + 1
         ENDDO
         CALL write(Out,Z(idp),nnd,0)
         icc = icc + nnd
      ENDDO
   ENDIF
END SUBROUTINE mbmode
