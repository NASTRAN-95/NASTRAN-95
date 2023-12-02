!*==stpaic.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE stpaic(Bloc,Dy,Nsize,Gap,Bm,Gm,Pm,Ns,Cla,Ajjl)
   USE c_amgmn
   USE c_packx
   USE c_stripc
   IMPLICIT NONE
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
   ii = nrow + 1
   nn = nrow
   IF ( ekr(1)<=.00001 ) ekr(1) = 0.0
   nsted = 0
   IF ( ekr(k)==0.0 ) nsted = 1
   DO n = 1 , Ns
      bob = Bloc(n)/bref
      ekl = ekr(k)*bob
      const = Cla(n)*Dy(n)*clam
      cr = fm
      IF ( ncirc/=0 ) cr = bb(1)
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
               cdum(i,j) = cdum(i,j) + Bm(i,m,n)*ekm(m,j)
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
      nn = nn + im
      DO i = 1 , im
         CALL pack(ch(1,i),Ajjl,mcb)
      ENDDO
      ii = ii + im
   ENDDO
END SUBROUTINE stpaic
