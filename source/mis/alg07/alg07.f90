!*==alg07.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE alg07
   IMPLICIT NONE
   USE C_UD300C
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
   l1 = I + Nl1(I)
   l2 = I + Nl2(I)
   iw = Nwork(I)
   il = Nloss(I)
   xn = Speed(I)*Spdfac(Icase)*Pi/(30.0*Sclfac)
   IF ( iw==2 ) THEN
      DO j = 1 , Nstrms
         H(j,I) = Work(j)
         Vw(j,I) = (xn*Rim1(j)*Vw(j,I-1)+(H(j,I)-H(j,I-1))*G*Ej)/(xn*R(j,I))
      ENDDO
      CALL spag_block_2
      RETURN
   ELSEIF ( iw==3 ) THEN
      DO j = 1 , Nstrms
         Vw(j,I) = Work(j)/R(j,I)
      ENDDO
      CALL spag_block_1
      RETURN
   ELSEIF ( iw==4 ) THEN
      DO j = 1 , Nstrms
         Vw(j,I) = Work(j)
      ENDDO
      CALL spag_block_1
      RETURN
   ELSEIF ( iw==5 .OR. iw==6 .OR. iw==7 ) THEN
      DO j = 1 , Nstrms
         Xi(j) = H(j,I-1) - xn*Rim1(j)*Vw(j,I-1)/(G*Ej)
      ENDDO
      IF ( il==2 ) THEN
         IF ( Ipass==1 .AND. Iter==0 ) THEN
            DO j = 1 , Nstrms
               S(j,I) = S(j,l1)
            ENDDO
         ELSE
            DO j = 1 , Nstrms
               IF ( Iter==0 ) Vv(j) = Vm(j,I)
               x1 = H(j,I-1) + xn*(Vv(j)*(Tbeta(j,I)+xn*R(j,I)/Vv(j))*R(j,I)-Rim1(j)*Vw(j,I-1))/(G*Ej)
               IF ( x1<Hmin ) x1 = Hmin
               x2 = alg4(H(j,l1)+(x1-H(j,l1))*Loss(j),S(j,l1))
               S(j,I) = alg3(x2,x1)
            ENDDO
         ENDIF
      ELSEIF ( il==3 ) THEN
         DO j = 1 , Nstrms
            S(j,I) = S(j,l1) + Loss(j)
         ENDDO
      ELSEIF ( l2/=I ) THEN
         DO j = 1 , Nstrms
            x4 = Xi(j) + (xn*R(j,I))**2/(2.0*G*Ej)
            IF ( x4<Hmin ) x4 = Hmin
            x1 = alg4(x4,S(j,l1))
            IF ( Ipass/=1 .OR. l2<=I ) THEN
               x2 = Xi(j) + (xn*R(j,l2))**2/(2.0*G*Ej)
               x3 = H(j,l2) - (Vm(j,l2)**2+Vw(j,l2)**2)/(2.0*G*Ej)
               IF ( x2<Hmin ) x2 = Hmin
               IF ( x3<Hmin ) x3 = Hmin
               x1 = x1 - Loss(j)*(alg4(x2,S(j,l2))-alg4(x3,S(j,l2)))
            ENDIF
            S(j,I) = alg3(x1,x4)
         ENDDO
      ELSE
         DO j = 1 , Nstrms
            x2 = Xi(j) + (xn*R(j,I))**2/(2.0*G*Ej)
            IF ( Ipass==1 .AND. Iter==0 ) THEN
               x3 = 1.0
            ELSE
               IF ( Iter==0 ) Vv(j) = Vm(j,I)
               x1 = x2 - Vv(j)**2*(1.0+Tbeta(j,I)**2)/(2.0*G*Ej)
               IF ( x1<Hmin ) x1 = Hmin
               IF ( x2<Hmin ) x2 = Hmin
               x3 = 1.0/(1.0+Loss(j)*(1.0-alg4(x1,S(j,I))/alg4(x2,S(j,I))))
            ENDIF
            S(j,I) = alg3(x3*alg4(x2,S(j,l1)),x2)
         ENDDO
      ENDIF
   ELSE
      IF ( il==2 ) THEN
         DO j = 1 , Nstrms
            H(j,I) = H(j,l1) + (alg2(S(j,l1),Work(j))-H(j,l1))/Loss(j)
            S(j,I) = alg3(Work(j),H(j,I))
         ENDDO
      ELSEIF ( il==3 ) THEN
         DO j = 1 , Nstrms
            S(j,I) = S(j,l1) + Loss(j)
            H(j,I) = alg2(S(j,I),Work(j))
         ENDDO
      ELSEIF ( l2/=I ) THEN
         DO j = 1 , Nstrms
            IF ( Ipass==1 .AND. l2>I ) THEN
               x4 = 1.0
            ELSE
               x1 = H(j,l1) - (Vw(j,l1)**2-(Vw(j,l1)-xn*R(j,l1))**2)/(2.0*G*Ej) + xn**2*(R(j,I)**2-R(j,l1)**2)/(2.0*G*Ej)
               IF ( x1<Hmin ) x1 = Hmin
               x2 = H(j,l2) - (Vm(j,l2)**2+Vw(j,l2)**2)/(2.0*G*Ej)
               x3 = H(j,l2) - (Vw(j,l2)**2-(Vw(j,l2)-xn*R(j,l2))**2)/(2.0*G*Ej)
               IF ( x2<Hmin ) x2 = Hmin
               IF ( x3<Hmin ) x3 = Hmin
               x4 = 1.0 - Loss(j)/alg4(x1,S(j,l1))*(alg4(x3,S(j,l2))-alg4(x2,S(j,l2)))
            ENDIF
            H(j,I) = alg2(S(j,l1),Work(j)/x4)
            S(j,I) = alg3(Work(j),H(j,I))
         ENDDO
      ELSE
         DO j = 1 , Nstrms
            IF ( Ipass==1 .AND. Iter==0 ) THEN
               x3 = 1.0
            ELSE
               IF ( Iter==0 ) Vv(j) = Vm(j,I)
               x1 = H(j,I) - (Vv(j)**2+Vw(j,I)**2)/(2.0*G*Ej)
               x2 = H(j,I) - (Vw(j,I)**2-(Vw(j,I)-xn*R(j,I))**2)/(2.0*G*Ej)
               IF ( x1<Hmin ) x1 = Hmin
               IF ( x2<Hmin ) x2 = Hmin
               x3 = 1.0/(1.0+Loss(j)*(1.0-alg4(x1,S(j,I))/alg4(x2,S(j,I))))
            ENDIF
            H(j,I) = alg2(S(j,l1),Work(j)/x3)
            S(j,I) = alg3(Work(j),H(j,I))
         ENDDO
      ENDIF
      DO j = 1 , Nstrms
         Vw(j,I) = (xn*Rim1(j)*Vw(j,I-1)+(H(j,I)-H(j,I-1))*G*Ej)/(xn*R(j,I))
      ENDDO
   ENDIF
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
      DO j = 1 , Nstrms
         H(j,I) = H(j,I-1) + xn*(R(j,I)*Vw(j,I)-Rim1(j)*Vw(j,I-1))/(G*Ej)
      ENDDO
      CALL spag_block_2
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
      IF ( il==2 ) THEN
         DO j = 1 , Nstrms
            S(j,I) = alg3(alg4(H(j,l1)+Loss(j)*(H(j,I)-H(j,l1)),S(j,l1)),H(j,I))
         ENDDO
      ELSEIF ( il==3 ) THEN
         DO j = 1 , Nstrms
            S(j,I) = S(j,l1) + Loss(j)
         ENDDO
      ELSEIF ( l2/=I ) THEN
         DO j = 1 , Nstrms
            IF ( Ipass==1 .AND. l2>I ) THEN
               x4 = 1.0
            ELSE
               x1 = H(j,l1) - (Vw(j,l1)**2-(Vw(j,l1)-xn*R(j,l1))**2)/(2.0*G*Ej) + xn**2*(R(j,I)**2-R(j,l1)**2)/(2.0*G*Ej)
               IF ( x1<Hmin ) x1 = Hmin
               x2 = H(j,l2) - (Vm(j,l2)**2+Vw(j,l2)**2)/(2.0*G*Ej)
               x3 = H(j,l2) - (Vw(j,l2)**2-(Vw(j,l2)-xn*R(j,l2))**2)/(2.0*G*Ej)
               IF ( x2<Hmin ) x2 = Hmin
               IF ( x3<Hmin ) x3 = Hmin
               x4 = 1.0 - Loss(j)/alg4(x1,S(j,l1))*(alg4(x3,S(j,l2))-alg4(x2,S(j,l2)))
            ENDIF
            S(j,I) = alg3(x4*alg4(H(j,I),S(j,l1)),H(j,I))
         ENDDO
      ELSE
         DO j = 1 , Nstrms
            IF ( Ipass==1 .AND. Iter==0 ) THEN
               x3 = 1.0
            ELSE
               IF ( Iter==0 ) Vv(j) = Vm(j,I)
               x1 = H(j,I) - (Vv(j)**2+Vw(j,I)**2)/(2.0*G*Ej)
               x2 = H(j,I) - (Vw(j,I)**2-(Vw(j,I)-xn*R(j,I))**2)/(2.0*G*Ej)
               IF ( x1<Hmin ) x1 = Hmin
               IF ( x2<Hmin ) x2 = Hmin
               x3 = 1.0/(1.0+Loss(j)*(1.0-alg4(x1,S(j,I))/alg4(x2,S(j,I))))
            ENDIF
            S(j,I) = alg3(x3*alg4(H(j,I),S(j,l1)),H(j,I))
         ENDDO
      ENDIF
   END SUBROUTINE spag_block_2
END SUBROUTINE alg07
