!*==ssplin.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ssplin(Ni,Xyi,Nd,Xyd,Kx,Ky,Kd,Kt,Dz,G,Ncore,Isng)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ni
   REAL , DIMENSION(1) :: Xyi
   INTEGER :: Nd
   REAL , DIMENSION(1) :: Xyd
   INTEGER :: Kx
   INTEGER :: Ky
   INTEGER :: Kd
   INTEGER :: Kt
   REAL :: Dz
   REAL , DIMENSION(1) :: G
   INTEGER :: Ncore
   INTEGER :: Isng
!
! Local variable declarations rewritten by SPAG
!
   REAL :: alt1 , alt2 , alt3 , alt4 , det , ex , ey , sum , t1 , t2 , t3 , t4 , xm , xp , ym , yp
   INTEGER :: i , ia , ib , ic , ig , ii , ik , inr , is , j , jj , k , kk , mp , n , nb , nc , needed , nr , nt , spag_nextblock_1
   LOGICAL :: ikd , ikt , lone , lx , ly
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL gmmats , invers , mesage
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
   DATA name/4HSSPL , 4HIN  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
         lone = .TRUE.
         lx = .TRUE.
         ly = .TRUE.
         ikt = .FALSE.
         ikd = .FALSE.
         IF ( Ky<0 .OR. Kx<0 ) lone = .FALSE.
         IF ( Ky<0 .OR. Kx>0 ) lx = .FALSE.
         IF ( Ky>0 .OR. Kx<0 ) ly = .FALSE.
         n = Ni
         IF ( lone ) n = n + 1
         IF ( lx ) n = n + 1
         IF ( ly ) n = n + 1
         ex = float(Kx)
         ey = float(Ky)
         IF ( Kt==1 ) ikt = .TRUE.
         IF ( Kd==1 ) ikd = .TRUE.
         nb = Nd*(1+Kd)
!
!     CORE NEEDED
!
!                A          G         INVERS
         needed = nb*Ni + 3*n
!                               B         C
         IF ( ikt ) needed = needed + nb*n + Ni*n
!                                      C          A OR B
         IF ( .NOT.ikt ) needed = needed + Ni*n + max0(n*n,nb*n)
         IF ( needed>Ncore ) CALL mesage(-8,0,name)
         is = Ncore - 3*n - 1
         ig = 1
!
!     IF  KT = 1 COMPUTE B THEN A THEN C IN A SPACE
!
!     IF KT = 0 COMPUTE C THEN A THEN B IN A SPACE
!
         nt = 2*Ni
         IF ( .NOT.ikt ) THEN
!
!     C MATRIX COLUMN STORED
!
            ic = nb*Ni
            mp = ic + 1
            spag_nextblock_1 = 3
         ELSE
!
!     B MATRIX COLUMN STORED
!
            ib = nb*Ni
            mp = ib + 1
            spag_nextblock_1 = 4
         ENDIF
      CASE (2)
!
!     COMPUTE TO A MATRIX
!
         k = ia
!
!     ZERO A
!
         ii = k + 1
         ik = ii + n*n
         DO i = ii , ik
            G(i) = 0.0
         ENDDO
         ii = 1
         ik = 0
         DO i = 1 , nt , 2
            k = k + ik
            jj = i/2
            DO j = i , nt , 2
               k = k + 1
               jj = jj + 1
               sum = 0.0
               xm = (Xyi(i)-Xyi(j))**2
               xp = (Xyi(i)+Xyi(j))**2
               ym = (Xyi(i+1)-Xyi(j+1))**2
               yp = (Xyi(i+1)+Xyi(j+1))**2
               t1 = xm + ym
               t2 = xp + ym
               t3 = xm + yp
               t4 = xp + yp
               IF ( t1/=0.0 ) sum = t1*alog(t1)
               IF ( t2/=0.0 .AND. Kx/=0 ) sum = sum + (t2*alog(t2)*ex)
               IF ( t3/=0.0 .AND. Ky/=0 ) sum = sum + (t3*alog(t3)*ey)
               IF ( t4/=0.0 .AND. Ky/=0 .AND. Kx/=0 ) sum = sum + (t4*alog(t4)*ex*ey)
               IF ( j==i ) THEN
                  G(k) = sum + Dz
                  kk = k
               ELSE
                  G(k) = sum
!
!     SYMETRY TERM
!
                  kk = k + (n-1)*(jj-ii)
                  G(kk) = sum
               ENDIF
            ENDDO
            inr = 0
            IF ( lone ) THEN
               inr = inr + 1
               G(k+inr) = 1.0
               G(kk+inr*n) = 1.0
            ENDIF
            IF ( lx ) THEN
               inr = inr + 1
               G(k+inr) = Xyi(i)
               G(kk+inr*n) = Xyi(i)
            ENDIF
            IF ( ly ) THEN
               inr = inr + 1
               G(k+inr) = Xyi(i+1)
               G(kk+inr*n) = Xyi(i+1)
            ENDIF
            ik = ii + inr
            ii = ii + 1
         ENDDO
!
!     CALL INVERS FOR A-1 C  OR A-1 B
!
!     REPLACE CALLS TO INVAER WITH CALLS TO INVERS
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
         Isng = -1
         CALL invers(n,G(ia+1),n,G(mp),nc,det,Isng,G(is))
         IF ( Isng==2 ) RETURN
         IF ( .NOT.ikt ) THEN
            ib = ia
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSE
            ic = ia
            k = ic + 1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         DO i = 1 , Ni
            DO j = 1 , n
               ic = ic + 1
               G(ic) = 0.0
               IF ( i==j ) G(ic) = 1.0
            ENDDO
         ENDDO
         IF ( ikt ) THEN
!
!     GMMATS WANTS ROW STORED SO INVERT ROWS AND COLUMNS AND INVERT
!     MULTIPLICATION ORDER
!
            CALL gmmats(G(mp),nb,n,0,G(k),Ni,n,1,G(ig))
            RETURN
         ELSE
            nc = Ni
            ia = ic
            spag_nextblock_1 = 2
         ENDIF
      CASE (4)
         nr = 2*Nd
         k = ib + 1
         DO j = 1 , nr , 2
            DO i = 1 , nt , 2
               ib = ib + 1
               alt1 = 0.0
               alt2 = 0.0
               alt3 = 0.0
               alt4 = 0.0
               xm = Xyd(j) - Xyi(i)
               xp = Xyi(i) + Xyd(j)
               ym = Xyd(j+1) - Xyi(i+1)
               yp = Xyi(i+1) + Xyd(j+1)
               t1 = xm*xm + ym*ym
               t2 = xp*xp + ym*ym
               t3 = xm*xm + yp*yp
               t4 = xp*xp + yp*yp
               IF ( t1/=0.0 ) alt1 = alog(t1)
               IF ( t2/=0.0 .AND. Kx/=0 ) alt2 = alog(t2)
               IF ( t3/=0.0 .AND. Ky/=0 ) alt3 = alog(t3)
               IF ( t4/=0.0 .AND. Kx/=0 .AND. Ky/=0 ) alt4 = alog(t4)
               G(ib) = t1*alt1 + t2*alt2*ex + t3*alt3*ey + t4*alt4*ex*ey
               IF ( ikd ) THEN
                  ik = ib + n
                  G(ik) = 2.0*(xm*(1.0+alt1)+xp*(1.0+alt2)*ex+xm*(1.0+alt3)*ey+xp*(1.0+alt4)*ex*ey)
               ENDIF
            ENDDO
            inr = 0
            IF ( lone ) THEN
               inr = inr + 1
               G(ib+inr) = 1.0
               IF ( ikd ) G(ib+inr+n) = 0.0
            ENDIF
            IF ( lx ) THEN
               inr = inr + 1
               G(ib+inr) = Xyd(j)
               IF ( ikd ) G(ib+inr+n) = 1.0
            ENDIF
            IF ( ly ) THEN
               inr = inr + 1
               G(ib+inr) = Xyd(j+1)
               IF ( ikd ) G(ib+inr+n) = 0.0
            ENDIF
            ib = ib + inr + n*Kd
         ENDDO
         IF ( .NOT.ikt ) THEN
            CALL gmmats(G(mp),Ni,n,0,G(k),nb,n,1,G(ig))
         ELSE
            ia = ib
            nc = nb
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ssplin
