!*==alg07.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE alg07
   USE c_ud300c
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: il , iw , j , l1 , l2
   REAL :: x1 , x2 , x3 , x4 , xn
   EXTERNAL alg2 , alg3 , alg4
!
! End of declarations rewritten by SPAG
!
!
!
!
   l1 = i + nl1(i)
   l2 = i + nl2(i)
   iw = nwork(i)
   il = nloss(i)
   xn = speed(i)*spdfac(icase)*pi/(30.0*sclfac)
   IF ( iw==2 ) THEN
      DO j = 1 , nstrms
         h(j,i) = work(j)
         vw(j,i) = (xn*rim1(j)*vw(j,i-1)+(h(j,i)-h(j,i-1))*g*ej)/(xn*r(j,i))
      ENDDO
      CALL spag_block_2
      RETURN
   ELSEIF ( iw==3 ) THEN
      DO j = 1 , nstrms
         vw(j,i) = work(j)/r(j,i)
      ENDDO
      CALL spag_block_1
      RETURN
   ELSEIF ( iw==4 ) THEN
      DO j = 1 , nstrms
         vw(j,i) = work(j)
      ENDDO
      CALL spag_block_1
      RETURN
   ELSEIF ( iw==5 .OR. iw==6 .OR. iw==7 ) THEN
      DO j = 1 , nstrms
         xi(j) = h(j,i-1) - xn*rim1(j)*vw(j,i-1)/(g*ej)
      ENDDO
      IF ( il==2 ) THEN
         IF ( ipass==1 .AND. iter==0 ) THEN
            DO j = 1 , nstrms
               s(j,i) = s(j,l1)
            ENDDO
         ELSE
            DO j = 1 , nstrms
               IF ( iter==0 ) vv(j) = vm(j,i)
               x1 = h(j,i-1) + xn*(vv(j)*(tbeta(j,i)+xn*r(j,i)/vv(j))*r(j,i)-rim1(j)*vw(j,i-1))/(g*ej)
               IF ( x1<hmin ) x1 = hmin
               x2 = alg4(h(j,l1)+(x1-h(j,l1))*loss(j),s(j,l1))
               s(j,i) = alg3(x2,x1)
            ENDDO
         ENDIF
      ELSEIF ( il==3 ) THEN
         DO j = 1 , nstrms
            s(j,i) = s(j,l1) + loss(j)
         ENDDO
      ELSEIF ( l2/=i ) THEN
         DO j = 1 , nstrms
            x4 = xi(j) + (xn*r(j,i))**2/(2.0*g*ej)
            IF ( x4<hmin ) x4 = hmin
            x1 = alg4(x4,s(j,l1))
            IF ( ipass/=1 .OR. l2<=i ) THEN
               x2 = xi(j) + (xn*r(j,l2))**2/(2.0*g*ej)
               x3 = h(j,l2) - (vm(j,l2)**2+vw(j,l2)**2)/(2.0*g*ej)
               IF ( x2<hmin ) x2 = hmin
               IF ( x3<hmin ) x3 = hmin
               x1 = x1 - loss(j)*(alg4(x2,s(j,l2))-alg4(x3,s(j,l2)))
            ENDIF
            s(j,i) = alg3(x1,x4)
         ENDDO
      ELSE
         DO j = 1 , nstrms
            x2 = xi(j) + (xn*r(j,i))**2/(2.0*g*ej)
            IF ( ipass==1 .AND. iter==0 ) THEN
               x3 = 1.0
            ELSE
               IF ( iter==0 ) vv(j) = vm(j,i)
               x1 = x2 - vv(j)**2*(1.0+tbeta(j,i)**2)/(2.0*g*ej)
               IF ( x1<hmin ) x1 = hmin
               IF ( x2<hmin ) x2 = hmin
               x3 = 1.0/(1.0+loss(j)*(1.0-alg4(x1,s(j,i))/alg4(x2,s(j,i))))
            ENDIF
            s(j,i) = alg3(x3*alg4(x2,s(j,l1)),x2)
         ENDDO
      ENDIF
   ELSE
      IF ( il==2 ) THEN
         DO j = 1 , nstrms
            h(j,i) = h(j,l1) + (alg2(s(j,l1),work(j))-h(j,l1))/loss(j)
            s(j,i) = alg3(work(j),h(j,i))
         ENDDO
      ELSEIF ( il==3 ) THEN
         DO j = 1 , nstrms
            s(j,i) = s(j,l1) + loss(j)
            h(j,i) = alg2(s(j,i),work(j))
         ENDDO
      ELSEIF ( l2/=i ) THEN
         DO j = 1 , nstrms
            IF ( ipass==1 .AND. l2>i ) THEN
               x4 = 1.0
            ELSE
               x1 = h(j,l1) - (vw(j,l1)**2-(vw(j,l1)-xn*r(j,l1))**2)/(2.0*g*ej) + xn**2*(r(j,i)**2-r(j,l1)**2)/(2.0*g*ej)
               IF ( x1<hmin ) x1 = hmin
               x2 = h(j,l2) - (vm(j,l2)**2+vw(j,l2)**2)/(2.0*g*ej)
               x3 = h(j,l2) - (vw(j,l2)**2-(vw(j,l2)-xn*r(j,l2))**2)/(2.0*g*ej)
               IF ( x2<hmin ) x2 = hmin
               IF ( x3<hmin ) x3 = hmin
               x4 = 1.0 - loss(j)/alg4(x1,s(j,l1))*(alg4(x3,s(j,l2))-alg4(x2,s(j,l2)))
            ENDIF
            h(j,i) = alg2(s(j,l1),work(j)/x4)
            s(j,i) = alg3(work(j),h(j,i))
         ENDDO
      ELSE
         DO j = 1 , nstrms
            IF ( ipass==1 .AND. iter==0 ) THEN
               x3 = 1.0
            ELSE
               IF ( iter==0 ) vv(j) = vm(j,i)
               x1 = h(j,i) - (vv(j)**2+vw(j,i)**2)/(2.0*g*ej)
               x2 = h(j,i) - (vw(j,i)**2-(vw(j,i)-xn*r(j,i))**2)/(2.0*g*ej)
               IF ( x1<hmin ) x1 = hmin
               IF ( x2<hmin ) x2 = hmin
               x3 = 1.0/(1.0+loss(j)*(1.0-alg4(x1,s(j,i))/alg4(x2,s(j,i))))
            ENDIF
            h(j,i) = alg2(s(j,l1),work(j)/x3)
            s(j,i) = alg3(work(j),h(j,i))
         ENDDO
      ENDIF
      DO j = 1 , nstrms
         vw(j,i) = (xn*rim1(j)*vw(j,i-1)+(h(j,i)-h(j,i-1))*g*ej)/(xn*r(j,i))
      ENDDO
   ENDIF
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
      DO J = 1 , Nstrms
         h(J,I) = h(J,I-1) + Xn*(r(J,I)*vw(J,I)-rim1(J)*vw(J,I-1))/(G*Ej)
      ENDDO
      CALL spag_block_2
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
      IF ( Il==2 ) THEN
         DO J = 1 , Nstrms
            s(J,I) = alg3(alg4(h(J,L1)+loss(J)*(h(J,I)-h(J,L1)),s(J,L1)),h(J,I))
         ENDDO
      ELSEIF ( Il==3 ) THEN
         DO J = 1 , Nstrms
            s(J,I) = s(J,L1) + loss(J)
         ENDDO
      ELSEIF ( L2/=I ) THEN
         DO J = 1 , Nstrms
            IF ( Ipass==1 .AND. L2>I ) THEN
               X4 = 1.0
            ELSE
               X1 = h(J,L1) - (vw(J,L1)**2-(vw(J,L1)-Xn*r(J,L1))**2)/(2.0*G*Ej) + Xn**2*(r(J,I)**2-r(J,L1)**2)/(2.0*G*Ej)
               IF ( X1<Hmin ) X1 = Hmin
               X2 = h(J,L2) - (vm(J,L2)**2+vw(J,L2)**2)/(2.0*G*Ej)
               X3 = h(J,L2) - (vw(J,L2)**2-(vw(J,L2)-Xn*r(J,L2))**2)/(2.0*G*Ej)
               IF ( X2<Hmin ) X2 = Hmin
               IF ( X3<Hmin ) X3 = Hmin
               X4 = 1.0 - loss(J)/alg4(X1,s(J,L1))*(alg4(X3,s(J,L2))-alg4(X2,s(J,L2)))
            ENDIF
            s(J,I) = alg3(X4*alg4(h(J,I),s(J,L1)),h(J,I))
         ENDDO
      ELSE
         DO J = 1 , Nstrms
            IF ( Ipass==1 .AND. Iter==0 ) THEN
               X3 = 1.0
            ELSE
               IF ( Iter==0 ) vv(J) = vm(J,I)
               X1 = h(J,I) - (vv(J)**2+vw(J,I)**2)/(2.0*G*Ej)
               X2 = h(J,I) - (vw(J,I)**2-(vw(J,I)-Xn*r(J,I))**2)/(2.0*G*Ej)
               IF ( X1<Hmin ) X1 = Hmin
               IF ( X2<Hmin ) X2 = Hmin
               X3 = 1.0/(1.0+loss(J)*(1.0-alg4(X1,s(J,I))/alg4(X2,s(J,I))))
            ENDIF
            s(J,I) = alg3(X3*alg4(h(J,I),s(J,L1)),h(J,I))
         ENDDO
      ENDIF
   END SUBROUTINE spag_block_2
END SUBROUTINE alg07
