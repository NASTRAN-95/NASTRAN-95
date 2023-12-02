!*==hdsket.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE hdsket(X,Y,Z,Np,Nc)
   USE c_hdptrs
   USE c_hdsc
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: X
   REAL , DIMENSION(1) :: Y
   REAL , DIMENSION(1) :: Z
   INTEGER :: Np
   INTEGER :: Nc
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a , b , c , rx , t , u , v
   INTEGER :: i , ik , is , ix , ix1skt , j , l , li , lx , m , m1 , mx , npx
   INTEGER , DIMENSION(1) :: iz
   EXTERNAL hdlin
!
! End of declarations rewritten by SPAG
!
!
!     THIS SUBROUTINE SETS UP PEN MOTION INDICATORS.
!
   !>>>>EQUIVALENCE (Iz(1),Rz(1))
!
   l = Np
   li = Np
   IF ( l>2 ) THEN
      lx = 1
      npx = Np
      SPAG_Loop_1_2: DO
         npx = npx - 1
         i = lx
         SPAG_Loop_2_1: DO m = i , npx
            rx = 0
            a = X(m+1) - X(m)
            b = Y(m+1) - Y(m)
            c = Z(m+1) - Z(m)
            IF ( a==0. ) THEN
               IF ( b==0. ) THEN
                  IF ( c==0. ) THEN
                     ix = m
                     ix1skt = npx
                     DO mx = ix , ix1skt
                        X(mx) = X(mx+1)
                        Y(mx) = Y(mx+1)
                        Z(mx) = Z(mx+1)
                     ENDDO
                     rx = 1
                     lx = m
                     IF ( lx==npx ) EXIT SPAG_Loop_2_1
                     CYCLE SPAG_Loop_1_2
                  ENDIF
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_2_1
         EXIT SPAG_Loop_1_2
      ENDDO SPAG_Loop_1_2
      IF ( rx==1. ) npx = npx - 1
      Np = npx + 1
      li = Np
      IF ( Np>2 ) THEN
         ix = 0
         m1 = 0
         m = 1
         is = Np - 1
         SPAG_Loop_1_3: DO
            m = m + ix
            m1 = m1 + ix + 1
            IF ( m-1==li ) THEN
               CALL spag_block_1
               RETURN
            ENDIF
!
!     SEARCH FOR MATCHING COORDINATES.
!
            DO j = m , is
               t = X(j+1) - X(m)
               u = Z(j+1) - Z(m)
               v = Y(j+1) - Y(m)
               IF ( t==0. ) THEN
                  IF ( v==0. ) THEN
                     IF ( u==0. ) THEN
                        Np = Np + 1
!
!     MATCH FOUND.....STORE COORDINATES AND SET SWITCH TO LIFT PEN
!     AND/OR END SET.
!
                        ix = j + 2 - m
                        ix1skt = j - is + 1
                        DO ik = 1 , ix
                           rz(x1skt+m1-2+ik) = X(m-1+ik)
                           rz(y1skt+m1-2+ik) = Y(m-1+ik)
                           rz(z1skt+m1-2+ik) = Z(m-1+ik)
                        ENDDO
                        rz(z1skt-1+m1+ix) = -isign(1,ix1skt)*9999.
                        CYCLE SPAG_Loop_1_3
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
            EXIT SPAG_Loop_1_3
         ENDDO SPAG_Loop_1_3
      ENDIF
   ENDIF
   DO j = 1 , li
      rz(x1skt-1+j) = X(j)
      rz(y1skt-1+j) = Y(j)
      rz(z1skt-1+j) = Z(j)
   ENDDO
   Np = Np + 1
   rz(z1skt-1+Np) = -9999.
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      CALL hdlin(rz(X1skt),rz(Y1skt),rz(Z1skt),Np,Nc,rz(xcc),Iz(icount),Iz(irct),rz(x21),rz(y21),rz(z21),Iz(iia),rz(xe),rz(ye),     &
               & rz(xu),rz(yu),rz(xi),rz(yi),rz(zi),rz(di),Iz(ibeg),Iz(iend),Iz(ict),Iz(icct),Iz(w),Iz(w),rz(w),rz(w),Iz(w),Iz(w),  &
               & Iz(w),rz(w),rz(w),rz(w),rz(w),rz(w),rz(w),rz(w),Iz(w),Iz(w),rz(w),rz(w),rz(w),rz(w),Iz(w),Iz(w))
      Np = L
!
!     RESET VALUE FOR MAXIMUM NUMBER OF EDGES IF ARGUMENT IS COMPLETED.
!
      IF ( vp>0. ) lz = lz/5
   END SUBROUTINE spag_block_1
END SUBROUTINE hdsket
