!*==dzymat.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dzymat(D,Nfb,Nlb,Ntzys,Idzdy,Ntape,Xp,Beta,Iprnt,Ns,Nc,Yp,Zp,Sg,Cg,Yb,Zb,Nbea)
   USE c_amgmn
   USE c_dlbdy
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ntzys
   REAL , DIMENSION(2,Ntzys) :: D
   INTEGER :: Nfb
   INTEGER :: Nlb
   INTEGER :: Idzdy
   INTEGER :: Ntape
   REAL , DIMENSION(1) :: Xp
   REAL :: Beta
   INTEGER :: Iprnt
   INTEGER , DIMENSION(1) :: Ns
   INTEGER , DIMENSION(1) :: Nc
   REAL , DIMENSION(1) :: Yp
   REAL , DIMENSION(1) :: Zp
   REAL , DIMENSION(1) :: Sg
   REAL , DIMENSION(1) :: Cg
   REAL , DIMENSION(1) :: Yb
   REAL , DIMENSION(1) :: Zb
   INTEGER , DIMENSION(1) :: Nbea
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: by , bz , c , c1 , i , isn , ixp , nbey , nbez , ncp , nfyb , nfybm1 , nsp , p , s , s1 , yt , zt
   REAL :: cgr , dx , dy , dz , sgr
   EXTERNAL rowdyz
!
! End of declarations rewritten by SPAG
!
!
!     CALCULATION OF DZ AND DY MATRICES SLENDER BODY CALCULATIONS
!
!     D         WORKING ARRAY USED TO STORE A ROW OF DZ OR DY
!     NFB       NUMBER OF THE FIRST BODY WITH THE ORIENTATION REQUESTED
!     NLB       NUMBER OF THE LAST BODY WITH THE ORIENTATION
!               REQUESTED
!     NTZYS     NUMBER OF Z OR Y ORIENTED SLENDER BODY ELE.
!     NTAPE     I/O UNIT NUMBER WHICH THE OUTPUT MATRIX IS TO
!               BE WRITTEN ON
!     XP        X-CONTROL POINT COORDINATE OF LIFTING SURFACE
!               BOXES
!     BETA      SQRT(1.0 - M**2)
!
!
   c1 = 0
   s1 = 0
   nfyb = nb - nby + 1
   IF ( np/=0 ) THEN
!
!     THIS LOOP IS FOR EACH LIFTING SURF. PANEL
!
      isn = 0
      DO p = 1 , np
         nsp = Ns(p)
         ncp = Nc(p)
         nsp = (nsp-isn)/ncp
         isn = Ns(p)
!
!     LOOP FOR EACH STRIP IN PANEL -P-
!
         DO s = 1 , nsp
            s1 = s1 + 1
!
!     Y AND Z COORDINATE OF STRIP
!
            dy = Yp(s1)
            dz = Zp(s1)
            sgr = Sg(s1)
            cgr = Cg(s1)
!
!     LOOP FOR EACH CHORDWISE ELEMENT IN STRIP
!
            DO c = 1 , ncp
               c1 = c1 + 1
               dx = Xp(c1)
!
!     - ROWDYC -  CALCULATES ROW -C1- OF DZ OR DY
!
               CALL rowdyz(Nfb,Nlb,c1,Ntzys,D,dx,dy,dz,Beta,Idzdy,Ntape,sgr,cgr,Iprnt,Yb,Zb,z(iarb),z(insbea),z(ixis1),z(ixis2),    &
                         & z(ia0))
!
            ENDDO
         ENDDO
      ENDDO
   ENDIF
!
!     WE HAVE NOW CALCULATED -C1- ROWS WHICH ARE THE LIFTING SURFACES.
!     NOW, LOOP FOR THE -Z- ORIENTED BODIES
!
   IF ( nbz>0 .AND. ntz>0 ) THEN
      sgr = 0.0
      cgr = 1.0
      DO bz = 1 , nbz
         dy = Yb(bz)
         dz = Zb(bz)
         nbez = Nbea(bz)
!
!     LOOP FOR EACH ELEMENT OF BODY -BZ-
!
         DO zt = 1 , nbez
            c1 = c1 + 1
            dx = Xp(c1)
!
            CALL rowdyz(Nfb,Nlb,c1,Ntzys,D,dx,dy,dz,Beta,Idzdy,Ntape,sgr,cgr,Iprnt,Yb,Zb,z(iarb),z(insbea),z(ixis1),z(ixis2),z(ia0))
         ENDDO
      ENDDO
   ENDIF
!
!     NOW, LOOP FOR THE -Y- ORIENTED BODIES
!
   IF ( nb>=nfyb .AND. nty>0 ) THEN
      ixp = ntp
      IF ( nfyb>1 ) THEN
         nfybm1 = nfyb - 1
         DO i = 1 , nfybm1
            ixp = ixp + Nbea(i)
         ENDDO
      ENDIF
      sgr = -1.0
      cgr = 0.0
      DO by = nfyb , nb
         dy = Yb(by)
         dz = Zb(by)
         nbey = Nbea(by)
!
!     LOOP FOR EACH ELEMENT OF BODY -BY-
!
         DO yt = 1 , nbey
            c1 = c1 + 1
            ixp = ixp + 1
            dx = Xp(ixp)
!
            CALL rowdyz(Nfb,Nlb,c1,Ntzys,D,dx,dy,dz,Beta,Idzdy,Ntape,sgr,cgr,Iprnt,Yb,Zb,z(iarb),z(insbea),z(ixis1),z(ixis2),z(ia0))
!
         ENDDO
      ENDDO
   ENDIF
END SUBROUTINE dzymat
