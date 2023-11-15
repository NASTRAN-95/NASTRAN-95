
SUBROUTINE hdstat(Mt,Nit,Ixr,X21,Y21,Z21,Iia,Iv,A,B,C,Ik,Xa,Ya,Za,Ccc,Xxx,Lz)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER L0 , L00 , L01 , L1 , L10 , L11 , L12 , L13 , L2 , L3 , L4 , L5 , L6 , L7 , L8 , L9 , Xcc
   REAL Rz(1) , Xdum
   COMMON /go3   / L0 , L1 , L00 , L01 , L2 , L3 , L4 , L5 , L6 , L7 , L8 , L9 , L10 , L11 , L12 , L13
   COMMON /hdptrs/ Xdum , Xcc
   COMMON /zzzzzz/ Rz
!
! Dummy argument declarations
!
   REAL A , B , C
   INTEGER Ik , Ixr , Lz , Mt , Nit
   REAL Ccc(1) , X21(1) , Xa(1) , Xxx(1) , Y21(1) , Ya(1) , Z21(1) , Za(1)
   INTEGER Iia(1) , Iv(1)
!
! Local variable declarations
!
   REAL amax , amaxx , amaxy , amin , aminx , aminy , d , dx , ei , exx , r , s , s1 , t , tx , ty , ve , xi , xii , ye , yi , yii ,&
      & ze , zi , zii
   INTEGER i , ib , ie , ig , is , j , jc , ji , jk , jo , jx , jxc , k , m , nk , nsub , nx
!
! End of declarations
!
!
!
!     THIS SUBROUTINE TAKES THE PTS OF INTERSECTION DETERMINED BY
!     SUBROUTINE SOLVE AND PICKS THE COORDINATES WITH THE MAX AND
!     MIN X COORDINATES PROVIDED THEY LIE ON THE INTERIOR/BOUNDARY
!     OF BOTH ELEMENTS.
!
!
!
   exx = .015
   nx = 0
   IF ( Mt/=0 ) THEN
      DO jx = 1 , Mt
         ei = 0
         DO
            ei = ei + .1
            IF ( ei>=1. ) GOTO 99999
            d = ei*Xa(jx) - Ya(jx)
            DO jo = 1 , 2
               m = Iv(jo)
               jc = L13 + (m-1)*Lz
               jxc = L12 + (m-1)*5
               nk = Xxx(5+jxc)
               i = 0
               ib = nk*5
!
!
!     DETERMINE IF THE PROJECTION OF THE POINT OF INTERSECTION
!     BELONGS TO THE INTERIOR OF BOTH PLANES.
!
!
               DO j = 1 , ib , 5
                  exx = .015
                  nsub = j + 1 + jc
                  IF ( abs(Ccc(nsub))>=100. ) exx = alog10(abs(Ccc(nsub)))
                  ve = Xa(jx)
                  IF ( Ccc(j+jc)==0. ) ve = Ya(jx)
                  s = ve - Ccc(j+3+jc)
                  s1 = ve - Ccc(j+4+jc)
                  t = Ccc(j+jc)*Ya(jx) + Ccc(j+1+jc)*Xa(jx) + Ccc(j+2+jc)
                  IF ( (abs(t)<exx) .AND. (s*s1<=0.) ) GOTO 10
                  t = -Ccc(j+2+jc) + Ccc(j+jc)*d
                  r = ei*Ccc(j+jc) + Ccc(j+1+jc)
                  IF ( r/=0. ) THEN
                     t = t/r
                     IF ( t>=Xa(jx) ) THEN
                        IF ( Ccc(j+jc)==0. ) t = ei*t - d
                        IF ( (t==Ccc(j+3+jc)) .OR. (t==Ccc(j+4+jc)) ) GOTO 20
                        s = t - Ccc(j+3+jc)
                        s1 = t - Ccc(j+4+jc)
                        IF ( s*s1<=0. ) i = i + 1
                     ENDIF
                  ENDIF
               ENDDO
               IF ( mod(i,2)==0 ) GOTO 50
 10         ENDDO
            nx = nx + 1
            Xa(nx) = Xa(jx)
            Ya(nx) = Ya(jx)
            Za(nx) = Za(jx)
            EXIT
 20      ENDDO
 50   ENDDO
      IF ( nx/=0 ) THEN
!
!
!
!     THIS CODE FINDS THE MAX/MIN X-COORDINATES(Y-COORDINATES) AND
!     STORES THEM. FUTHERMORE BOTH THE EQUATION OF LINE AND POINTS(2)
!     ARE TREATED LIKE ADDITIONAL EDGES. IN THIS WAY, THE ALGORITHM NEED
!     NOT BE DISTURBED. ESSENTIALLY,THEN,THIS TRICK IS TRANSPARENT TO
!     THE REST OF THE PROGRAM.
!
!
         amaxx = -(10**6)
         aminx = -amaxx
         amaxy = amaxx
         aminy = aminx
         is = 5 + (Ik-1)*5 + L12
         is = Xxx(is)
         DO ji = 1 , nx
            IF ( A==0. ) THEN
               IF ( Ya(ji)<aminy ) THEN
                  aminy = Ya(ji)
                  xi = Xa(ji)
                  zi = Za(ji)
               ENDIF
               IF ( Ya(ji)>amaxy ) THEN
                  xii = Xa(ji)
                  amaxy = Ya(ji)
                  zii = Za(ji)
               ENDIF
            ELSE
               IF ( Xa(ji)<aminx ) THEN
                  aminx = Xa(ji)
                  yi = Ya(ji)
                  zi = Za(ji)
               ENDIF
               IF ( Xa(ji)>amaxx ) THEN
                  amaxx = Xa(ji)
                  yii = Ya(ji)
                  zii = Za(ji)
               ENDIF
            ENDIF
         ENDDO
         Nit = Nit + 1
         k = 5*(Nit-1+is) + 1
         Rz(Xcc+k-1) = A
         Rz(Xcc+k) = B
         Rz(Xcc+k+1) = C
         IF ( A==0. ) THEN
            Rz(Xcc+k+2) = aminy
            Rz(Xcc+k+3) = amaxy
            amin = xi
            amax = xii
            yi = aminy
            ye = amaxy
            ze = zii
         ELSE
            Rz(Xcc+k+2) = aminx
            Rz(Xcc+k+3) = amaxx
            amin = aminx
            amax = amaxx
            ye = yii
            ze = zii
         ENDIF
         ig = Ixr + Nit*3
         X21(ig-2) = amin
         Y21(ig-2) = yi
         Z21(ig-2) = zi
         DO jk = 1 , 2
            ie = ig - jk + 1
            X21(ie) = amax
            Y21(ie) = ye
            Z21(ie) = ze
         ENDDO
         DO jk = 1 , 2
            Iia(ig-jk) = 0
         ENDDO
         Iia(ig) = 1
         tx = (amax-amin)**2
         ty = (ye-yi)**2
         dx = (tx+ty)**.5
         IF ( dx<.001 ) Nit = Nit - 1
      ENDIF
   ENDIF
99999 END SUBROUTINE hdstat
