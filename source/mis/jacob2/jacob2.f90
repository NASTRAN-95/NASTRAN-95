!*==jacob2.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE jacob2(Elid,Shp,Dshp,Gpth,Bgpdt,Gpnorm,Jacob)
USE C_Q4DT
USE C_SYSTEM
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Elid
   REAL(REAL64) , DIMENSION(1) :: Shp
   REAL(REAL64) , DIMENSION(1) :: Dshp
   REAL(REAL64) , DIMENSION(1) :: Gpth
   REAL , DIMENSION(4,1) :: Bgpdt
   REAL , DIMENSION(4,1) :: Gpnorm
   REAL(REAL64) , DIMENSION(3,3) :: Jacob
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(3) :: dum , enk , sk , tk , v1 , v2 , v3
   REAL(REAL64) , SAVE :: eps
   INTEGER :: i , ipoint , ising , j , jtemp , k
   INTEGER , DIMENSION(3,3) :: index
   REAL(REAL64) , DIMENSION(3,8) :: tgrid
   REAL(REAL64) :: thick , val
   EXTERNAL daxb , inverd
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE WAS CALLED JACOBD BEFORE, AND WAS THE ONLY ROUTINE
!     THAT ENDED WITH 'DB' AND WAS NOT A BLOCK DATA SUBROUTINE.
!
!     THIS SUBROUTINE CALCULATES JACOBIAN AT EACH GIVEN INTEGRATION
!     POINT FOR QUAD4 POTVIN TYPE ELEMENTS.
!
!     DOUBLE PRECISION VERSION
!
!
   !>>>>EQUIVALENCE (Psitrn(1),V1(1))
   !>>>>EQUIVALENCE (Psitrn(4),V2(1))
   !>>>>EQUIVALENCE (Psitrn(7),V3(1))
!
   DATA eps/1.0D-15/
!
!     INITIALIZE BADJ LOGICAL
!
   Badj = .FALSE.
!
!     COMPUTE THE JACOBIAN AT THIS GAUSS POINT,
!     ITS INVERSE AND ITS DETERMINANT.
!
   DO i = 1 , Nnode
      thick = Gpth(i)
      tgrid(1,i) = Bgpdt(2,i) + Hzta*thick*Gpnorm(2,i)
      tgrid(2,i) = Bgpdt(3,i) + Hzta*thick*Gpnorm(3,i)
      tgrid(3,i) = Bgpdt(4,i) + Hzta*thick*Gpnorm(4,i)
   ENDDO
   DO i = 1 , 2
      ipoint = N1*(i-1)
      DO j = 1 , 3
         Jacob(i,j) = 0.0D0
         DO k = 1 , Nnode
            Jacob(i,j) = Jacob(i,j) + Dshp(k+ipoint)*tgrid(j,k)
         ENDDO
      ENDDO
   ENDDO
   DO j = 1 , 3
      Jacob(3,j) = 0.0D0
      DO k = 1 , Nnode
         jtemp = j + 1
         Jacob(3,j) = Jacob(3,j) + 0.5D0*Gpth(k)*Gpnorm(jtemp,k)*Shp(k)
      ENDDO
   ENDDO
!
!     SAVE THE S, T, AND N VECTORS FOR CALCULATING PSI LATER.
!
   DO i = 1 , 3
      IF ( dabs(Jacob(1,i))<=eps ) Jacob(1,i) = 0.0D0
      sk(i) = Jacob(1,i)
      IF ( dabs(Jacob(2,i))<=eps ) Jacob(2,i) = 0.0D0
      tk(i) = Jacob(2,i)
      IF ( dabs(Jacob(3,i))<=eps ) Jacob(3,i) = 0.0D0
      enk(i) = Jacob(3,i)
   ENDDO
!
!     THE INVERSE OF THE JACOBIAN WILL BE STORED IN
!     JACOB AFTER THE SUBROUTINE INVERD HAS EXECUTED.
!
   CALL inverd(3,Jacob,3,dum,0,Detj,ising,index)
   IF ( ising==1 .AND. Detj>0.0D0 ) THEN
      CALL daxb(sk,tk,v3)
      val = dsqrt(v3(1)*v3(1)+v3(2)*v3(2)+v3(3)*v3(3))
      v3(1) = v3(1)/val
      v3(2) = v3(2)/val
      v3(3) = v3(3)/val
!
!     CROSS ELEMENT Y DIRECTION WITH UNIT VECTOR V3 IN ORDER
!     TO BE CONSISTENT WITH THE ELEMENT COORDINATE SYSTEM.
!
!     NOTE - THIS IS IMPORTANT FOR THE DIRECTIONAL REDUCED
!            INTEGRATION CASES.
!
!
!
      v2(1) = 0.0D0
      v2(2) = 1.0D0
      v2(3) = 0.0D0
!
      CALL daxb(v2,v3,v1)
      val = dsqrt(v1(1)*v1(1)+v1(2)*v1(2)+v1(3)*v1(3))
      v1(1) = v1(1)/val
      v1(2) = v1(2)/val
      v1(3) = v1(3)/val
      CALL daxb(v3,v1,v2)
!
!     REMEMBER THAT V1(1) IS EQUIVALENCED TO PSITRN(1), AND SO ON.
!
!     ELIMINATE SMALL NUMBERS
!
      DO i = 1 , 3
         IF ( dabs(v1(i))<=eps ) v1(i) = 0.0D0
         IF ( dabs(v2(i))<=eps ) v2(i) = 0.0D0
         IF ( dabs(v3(i))<=eps ) v3(i) = 0.0D0
      ENDDO
   ELSE
      WRITE (Nout,99001) Elid
!
99001 FORMAT ('0*** USER FATAL ERROR, ELEMENT ID =',I10,'  HAS BAD OR REVERSE GEOMETRY')
      Nogo = 1
      Badj = .TRUE.
   ENDIF
!
END SUBROUTINE jacob2
