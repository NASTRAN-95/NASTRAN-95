!*==hdsket.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE hdsket(X,Y,Z,Np,Nc)
   IMPLICIT NONE
   USE C_HDPTRS
   USE C_HDSC
   USE C_ZZZZZZ
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
                           Rz(X1skt+m1-2+ik) = X(m-1+ik)
                           Rz(Y1skt+m1-2+ik) = Y(m-1+ik)
                           Rz(Z1skt+m1-2+ik) = Z(m-1+ik)
                        ENDDO
                        Rz(Z1skt-1+m1+ix) = -isign(1,ix1skt)*9999.
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
      Rz(X1skt-1+j) = X(j)
      Rz(Y1skt-1+j) = Y(j)
      Rz(Z1skt-1+j) = Z(j)
   ENDDO
   Np = Np + 1
   Rz(Z1skt-1+Np) = -9999.
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      CALL hdlin(Rz(X1skt),Rz(Y1skt),Rz(Z1skt),Np,Nc,Rz(Xcc),iz(Icount),iz(Irct),Rz(X21),Rz(Y21),Rz(Z21),iz(Iia),Rz(Xe),Rz(Ye),     &
               & Rz(Xu),Rz(Yu),Rz(Xi),Rz(Yi),Rz(Zi),Rz(Di),iz(Ibeg),iz(Iend),iz(Ict),iz(Icct),iz(W),iz(W),Rz(W),Rz(W),iz(W),iz(W),  &
               & iz(W),Rz(W),Rz(W),Rz(W),Rz(W),Rz(W),Rz(W),Rz(W),iz(W),iz(W),Rz(W),Rz(W),Rz(W),Rz(W),iz(W),iz(W))
      Np = l
!
!     RESET VALUE FOR MAXIMUM NUMBER OF EDGES IF ARGUMENT IS COMPLETED.
!
      IF ( Vp>0. ) Lz = Lz/5
   END SUBROUTINE spag_block_1
END SUBROUTINE hdsket
