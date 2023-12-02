!*==alg05.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE alg05
   IMPLICIT NONE
   USE C_UD300C
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: ii , iii , ik , il , iw , j , k , l1 , l2 , l3 , l4
   REAL :: x1 , x2 , xn
   REAL , DIMENSION(21) :: xx1 , xx2 , xx3 , xx4 , xx5
   EXTERNAL alg01
!
! End of declarations rewritten by SPAG
!
!
!
!
!
   l1 = Ndimen(I) + 1
   IF ( l1==2 ) THEN
      DO j = 1 , Nstrms
         xx5(j) = R(j,I)/R(Nstrms,I)
      ENDDO
   ELSEIF ( l1==3 ) THEN
      DO j = 1 , Nstrms
         xx5(j) = Xl(j,I)
      ENDDO
   ELSEIF ( l1==4 ) THEN
      DO j = 1 , Nstrms
         xx5(j) = Xl(j,I)/Xl(Nstrms,I)
      ENDDO
   ELSE
      DO j = 1 , Nstrms
         xx5(j) = R(j,I)
      ENDDO
   ENDIF
   l2 = Is2(I)
   l3 = Ndata(I)
   l4 = Nterp(I)
   CALL alg01(Datac(l2),Data1(l2),l3,xx5,Work,x1,Nstrms,l4,0)
   CALL alg01(Datac(l2),Data3(l2),l3,xx5,Taneps,x1,Nstrms,l4,0)
   DO j = 1 , Nstrms
      Taneps(j) = tan(Taneps(j)/C1)
   ENDDO
   iw = Nwork(I)
   il = Nloss(I)
   IF ( iw==7 .OR. il<=3 ) CALL alg01(Datac(l2),Data2(l2),l3,xx5,Loss,x1,Nstrms,l4,0)
   IF ( iw>=5 ) CALL alg01(Datac(l2),Data6(l2),l3,xx5,xx1,x1,Nstrms,l4,0)
   IF ( il==4 ) THEN
      SPAG_Loop_1_1: DO ii = I , Nstns
         IF ( Nloss(ii)==1 ) EXIT SPAG_Loop_1_1
      ENDDO SPAG_Loop_1_1
      l2 = Is2(ii)
      l3 = Ndata(ii)
      l4 = Nterp(ii)
      l1 = Ndimen(ii) + 1
      IF ( l1==2 ) THEN
         DO j = 1 , Nstrms
            xx5(j) = R(j,ii)/R(Nstrms,ii)
         ENDDO
      ELSEIF ( l1==3 ) THEN
         DO j = 1 , Nstrms
            xx5(j) = Xl(j,ii)
         ENDDO
      ELSEIF ( l1==4 ) THEN
         DO j = 1 , Nstrms
            xx5(j) = Xl(j,ii)/Xl(Nstrms,ii)
         ENDDO
      ELSE
         DO j = 1 , Nstrms
            xx5(j) = R(j,ii)
         ENDDO
      ENDIF
      CALL alg01(Datac(l2),Data2(l2),l3,xx5,Loss,x1,Nstrms,l4,0)
      iii = I + Nl1(I) + 1
      DO j = 1 , Nstrms
         xx2(j) = 0.0
         DO ik = iii , ii
            xx2(j) = xx2(j) + sqrt((X(j,ik)-X(j,ik-1))**2+(R(j,ik)-R(j,ik-1))**2)
            IF ( ik==I ) xx3(j) = xx2(j)
         ENDDO
         xx3(j) = xx3(j)/xx2(j)
      ENDDO
      l1 = Ncurve(I)
      l2 = Nm(l1)
      l3 = Nrad(l1)
      DO j = 1 , Nstrms
         DO k = 1 , l3
            CALL alg01(Dm(1,k,l1),Wfrac(1,k,l1),l2,xx3(j),xx2(k),x1,1,0,0)
         ENDDO
         x2 = (R(j,ii)-R(1,ii))/(R(Nstrms,ii)-R(1,ii))
         CALL alg01(Terad(1,l1),xx2,l3,x2,x1,x1,1,0,0)
         Loss(j) = Loss(j)*x1
      ENDDO
   ENDIF
   IF ( iw>=5 ) THEN
      IF ( iw==5 ) THEN
         DO j = 1 , Nstrms
            Tbeta(j,I) = tan((Work(j)+xx1(j))/C1)
         ENDDO
      ELSEIF ( iw==7 ) THEN
         xn = Speed(I)*Spdfac(Icase)*Pi/(30.0*Sclfac)
         CALL alg01(Datac(l2),Data7(l2),l3,xx5,xx2,x1,Nstrms,l4,0)
         CALL alg01(Datac(l2),Data8(l2),l3,xx5,xx3,x1,Nstrms,l4,0)
         CALL alg01(Datac(l2),Data9(l2),l3,xx5,xx4,x1,Nstrms,l4,0)
         ii = I + Nl1(I)
         DO j = 1 , Nstrms
            x1 = C1*atan((Vw(j,ii)-xn*R(j,ii))/Vm(j,ii))
            x2 = xx3(j)
            IF ( x1<xx1(j) ) x2 = xx4(j)
            Loss(j) = Loss(j)*(1.0+((x1-xx1(j))/(x2-xx1(j)))**2)
            IF ( Loss(j)>0.5 ) Loss(j) = 0.5
            Tbeta(j,I) = tan((Work(j)+(x1-xx1(j))*xx2(j))/C1)
         ENDDO
      ELSE
         DO j = 1 , Nstrms
            xx2(j) = tan((atan((R(j,I+1)-R(j,I))/(X(j,I+1)-X(j,I)))+atan((R(j,I)-R(j,I-1))/(X(j,I)-X(j,I-1))))/2.0)
         ENDDO
         l1 = Is1(I)
         CALL alg01(Rstn(l1),Xstn(l1),Nspec(I),R(1,I),x1,xx3,Nstrms,0,1)
         DO j = 1 , Nstrms
            Tbeta(j,I) = tan(atan((tan(Work(j)/C1)*(1.0-xx3(j)*xx2(j))-xx2(j)*Taneps(j)*sqrt(1.0+xx3(j)**2))/sqrt(1.0+xx2(j)**2))   &
                       & +xx1(j)/C1)
         ENDDO
      ENDIF
   ENDIF
END SUBROUTINE alg05
