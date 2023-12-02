!*==gend.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gend(Ncaray,Nbaray,Ys,Zs,Sg,Cg,Dt,Work,Matout)
   USE c_amgmn
   USE c_dlcom
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Ncaray
   INTEGER , DIMENSION(1) :: Nbaray
   REAL , DIMENSION(1) :: Ys
   REAL , DIMENSION(1) :: Zs
   REAL , DIMENSION(1) :: Sg
   REAL , DIMENSION(1) :: Cg
   COMPLEX , DIMENSION(1) :: Dt
   REAL , DIMENSION(1) :: Work
   INTEGER :: Matout
!
! Local variable declarations rewritten by SPAG
!
   REAL :: cgr , sgr
   INTEGER :: i , i1 , i2 , idtpt , j1 , j2 , k , ks , nbxr
   EXTERNAL dpps , pack
!
! End of declarations rewritten by SPAG
!
!  GENERATE THE INFLUENCE COEFFICIENT MATRIX ADPP
   i1 = 1
   i2 = ntp
   j1 = 1
   j2 = ntp
!
!     POSITION IN DT TO START OF THIS PART OF MATRIX
!
   idtpt = i1 + nrow
   DO i = i1 , njj
      Dt(i) = (0.0,0.0)
   ENDDO
!     DPP LOOP
   k = 1
!     K IS THE PANEL NUMBER
   ks = 1
!     KS IS THE STRIP NUMBER
   nbxr = Ncaray(k)
   DO i = i1 , i2
      sgr = Sg(ks)
      cgr = Cg(ks)
      CALL dpps(ks,i,j1,j2,sgr,cgr,Ys,Zs,Nbaray,Ncaray,Dt(idtpt),Work)
      CALL pack(Dt,Matout,mcb)
      IF ( i/=i2 ) THEN
         IF ( i==Nbaray(k) ) k = k + 1
         IF ( i==nbxr ) THEN
            ks = ks + 1
            nbxr = nbxr + Ncaray(k)
         ENDIF
      ENDIF
   ENDDO
END SUBROUTINE gend
