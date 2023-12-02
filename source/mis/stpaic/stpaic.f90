!*==stpaic.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE stpaic(Bloc,Dy,Nsize,Gap,Bm,Gm,Pm,Ns,Cla,Ajjl)
   IMPLICIT NONE
   USE C_AMGMN
   USE C_PACKX
   USE C_STRIPC
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ns
   REAL , DIMENSION(1) :: Bloc
   REAL , DIMENSION(1) :: Dy
   INTEGER , DIMENSION(1) :: Nsize
   REAL , DIMENSION(1) :: Gap
   REAL , DIMENSION(4,4,Ns) :: Bm
   REAL , DIMENSION(4,3,Ns) :: Gm
   REAL , DIMENSION(37,Ns) :: Pm
   REAL , DIMENSION(1) :: Cla
   REAL :: Ajjl
!
! Local variable declarations rewritten by SPAG
!
   REAL :: bob , ci , const , cr , ekl , tsr
   COMPLEX , DIMENSION(4,4) :: cdum
   COMPLEX , DIMENSION(3,3) :: ch
   INTEGER :: i , im , j , j1 , jm , k , m , n , nopen , nsted
   EXTERNAL pack , stpk
!
! End of declarations rewritten by SPAG
!
   k = 1
   Ii = Nrow + 1
   Nn = Nrow
   IF ( Ekr(1)<=.00001 ) Ekr(1) = 0.0
   nsted = 0
   IF ( Ekr(k)==0.0 ) nsted = 1
   DO n = 1 , Ns
      bob = Bloc(n)/Bref
      ekl = Ekr(k)*bob
      const = Cla(n)*Dy(n)*Clam
      cr = Fm
      IF ( Ncirc/=0 ) cr = Bb(1)
      ci = 0.
      nopen = 0
      IF ( Nsize(n)==3 .AND. Gap(n)==0.0 ) nopen = 1
      tsr = 0.5*Gap(n)/Bloc(n)
      im = Nsize(n)
      IF ( im<3 ) THEN
         jm = 2
         j1 = 2
      ELSE
         jm = 4
         j1 = 3
      ENDIF
      CALL stpk(ekl,n,Nsize(n),nopen,nsted,tsr,Pm(1,n),cr,ci,im,j1)
      DO i = 1 , im
         DO j = 1 , jm
            cdum(i,j) = cmplx(0.0,0.0)
            DO m = 1 , jm
               cdum(i,j) = cdum(i,j) + Bm(i,m,n)*Ekm(m,j)
            ENDDO
         ENDDO
      ENDDO
      DO i = 1 , im
         DO j = 1 , j1
            ch(i,j) = cmplx(0.0,0.0)
            DO m = 1 , jm
               ch(i,j) = ch(i,j) + cdum(i,m)*Gm(m,j,n)
            ENDDO
            ch(i,j) = const*ch(i,j)
         ENDDO
      ENDDO
      Nn = Nn + im
      DO i = 1 , im
         CALL pack(ch(1,i),Ajjl,Mcb)
      ENDDO
      Ii = Ii + im
   ENDDO
END SUBROUTINE stpaic
