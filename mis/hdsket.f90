
SUBROUTINE hdsket(X,Y,Z,Np,Nc)
   IMPLICIT NONE
   INTEGER Di , Ibeg , Icct , Icore , Icount , Ict , Iend , Iia , Irct , Iz(1) , Jjj , Lz , W , X1skt , X21 , Xcc , Xe , Xi , Xu ,  &
         & Y1skt , Y21 , Ye , Yi , Yu , Z1skt , Z21 , Zi
   REAL Pit , Rol , Rz(1) , Scx , Vp , Xasolv , Xdum , Yasolv , Yaw , Zasolv , Zcoef , Zcoef1
   COMMON /hdptrs/ Xdum , Xcc , Xasolv , Yasolv , Zasolv , X1skt , Y1skt , Z1skt , Zcoef1 , Zcoef , Icount , Irct , X21 , Y21 ,     &
                 & Z21 , Iia , Xe , Ye , Xu , Yu , Xi , Yi , Zi , Di , Ibeg , Iend , Ict , Icct , W
   COMMON /hdsc  / Scx , Yaw , Rol , Pit , Lz , Vp , Jjj , Icore
   COMMON /zzzzzz/ Rz
   INTEGER Nc , Np
   REAL X(1) , Y(1) , Z(1)
   REAL a , b , c , rx , t , u , v
   INTEGER i , ik , is , ix , ix1skt , j , l , li , lx , m , m1 , mx , npx
!
!     THIS SUBROUTINE SETS UP PEN MOTION INDICATORS.
!
   EQUIVALENCE (Iz(1),Rz(1))
!
   l = Np
   li = Np
   IF ( l>2 ) THEN
      lx = 1
      npx = Np
      DO
         npx = npx - 1
         i = lx
         DO m = i , npx
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
                     IF ( lx/=npx ) GOTO 50
                     EXIT
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
         EXIT
 50   ENDDO
      IF ( rx==1. ) npx = npx - 1
      Np = npx + 1
      li = Np
      IF ( Np>2 ) THEN
         ix = 0
         m1 = 0
         m = 1
         is = Np - 1
         DO
            m = m + ix
            m1 = m1 + ix + 1
            IF ( m-1==li ) GOTO 100
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
                        GOTO 60
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
            EXIT
 60      ENDDO
      ENDIF
   ENDIF
   DO j = 1 , li
      Rz(X1skt-1+j) = X(j)
      Rz(Y1skt-1+j) = Y(j)
      Rz(Z1skt-1+j) = Z(j)
   ENDDO
   Np = Np + 1
   Rz(Z1skt-1+Np) = -9999.
 100  CALL hdlin(Rz(X1skt),Rz(Y1skt),Rz(Z1skt),Np,Nc,Rz(Xcc),Iz(Icount),Iz(Irct),Rz(X21),Rz(Y21),Rz(Z21),Iz(Iia),Rz(Xe),Rz(Ye),     &
               & Rz(Xu),Rz(Yu),Rz(Xi),Rz(Yi),Rz(Zi),Rz(Di),Iz(Ibeg),Iz(Iend),Iz(Ict),Iz(Icct),Iz(W),Iz(W),Rz(W),Rz(W),Iz(W),Iz(W),  &
               & Iz(W),Rz(W),Rz(W),Rz(W),Rz(W),Rz(W),Rz(W),Rz(W),Iz(W),Iz(W),Rz(W),Rz(W),Rz(W),Rz(W),Iz(W),Iz(W))
   Np = l
!
!     RESET VALUE FOR MAXIMUM NUMBER OF EDGES IF ARGUMENT IS COMPLETED.
!
   IF ( Vp>0. ) Lz = Lz/5
END SUBROUTINE hdsket
