!*==alg05.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE alg05
   USE c_ud300c
   IMPLICIT NONE
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
   l1 = ndimen(i) + 1
   IF ( l1==2 ) THEN
      DO j = 1 , nstrms
         xx5(j) = r(j,i)/r(nstrms,i)
      ENDDO
   ELSEIF ( l1==3 ) THEN
      DO j = 1 , nstrms
         xx5(j) = xl(j,i)
      ENDDO
   ELSEIF ( l1==4 ) THEN
      DO j = 1 , nstrms
         xx5(j) = xl(j,i)/xl(nstrms,i)
      ENDDO
   ELSE
      DO j = 1 , nstrms
         xx5(j) = r(j,i)
      ENDDO
   ENDIF
   l2 = is2(i)
   l3 = ndata(i)
   l4 = nterp(i)
   CALL alg01(datac(l2),data1(l2),l3,xx5,work,x1,nstrms,l4,0)
   CALL alg01(datac(l2),data3(l2),l3,xx5,taneps,x1,nstrms,l4,0)
   DO j = 1 , nstrms
      taneps(j) = tan(taneps(j)/c1)
   ENDDO
   iw = nwork(i)
   il = nloss(i)
   IF ( iw==7 .OR. il<=3 ) CALL alg01(datac(l2),data2(l2),l3,xx5,loss,x1,nstrms,l4,0)
   IF ( iw>=5 ) CALL alg01(datac(l2),data6(l2),l3,xx5,xx1,x1,nstrms,l4,0)
   IF ( il==4 ) THEN
      SPAG_Loop_1_1: DO ii = i , nstns
         IF ( nloss(ii)==1 ) EXIT SPAG_Loop_1_1
      ENDDO SPAG_Loop_1_1
      l2 = is2(ii)
      l3 = ndata(ii)
      l4 = nterp(ii)
      l1 = ndimen(ii) + 1
      IF ( l1==2 ) THEN
         DO j = 1 , nstrms
            xx5(j) = r(j,ii)/r(nstrms,ii)
         ENDDO
      ELSEIF ( l1==3 ) THEN
         DO j = 1 , nstrms
            xx5(j) = xl(j,ii)
         ENDDO
      ELSEIF ( l1==4 ) THEN
         DO j = 1 , nstrms
            xx5(j) = xl(j,ii)/xl(nstrms,ii)
         ENDDO
      ELSE
         DO j = 1 , nstrms
            xx5(j) = r(j,ii)
         ENDDO
      ENDIF
      CALL alg01(datac(l2),data2(l2),l3,xx5,loss,x1,nstrms,l4,0)
      iii = i + nl1(i) + 1
      DO j = 1 , nstrms
         xx2(j) = 0.0
         DO ik = iii , ii
            xx2(j) = xx2(j) + sqrt((x(j,ik)-x(j,ik-1))**2+(r(j,ik)-r(j,ik-1))**2)
            IF ( ik==i ) xx3(j) = xx2(j)
         ENDDO
         xx3(j) = xx3(j)/xx2(j)
      ENDDO
      l1 = ncurve(i)
      l2 = nm(l1)
      l3 = nrad(l1)
      DO j = 1 , nstrms
         DO k = 1 , l3
            CALL alg01(dm(1,k,l1),wfrac(1,k,l1),l2,xx3(j),xx2(k),x1,1,0,0)
         ENDDO
         x2 = (r(j,ii)-r(1,ii))/(r(nstrms,ii)-r(1,ii))
         CALL alg01(terad(1,l1),xx2,l3,x2,x1,x1,1,0,0)
         loss(j) = loss(j)*x1
      ENDDO
   ENDIF
   IF ( iw>=5 ) THEN
      IF ( iw==5 ) THEN
         DO j = 1 , nstrms
            tbeta(j,i) = tan((work(j)+xx1(j))/c1)
         ENDDO
      ELSEIF ( iw==7 ) THEN
         xn = speed(i)*spdfac(icase)*pi/(30.0*sclfac)
         CALL alg01(datac(l2),data7(l2),l3,xx5,xx2,x1,nstrms,l4,0)
         CALL alg01(datac(l2),data8(l2),l3,xx5,xx3,x1,nstrms,l4,0)
         CALL alg01(datac(l2),data9(l2),l3,xx5,xx4,x1,nstrms,l4,0)
         ii = i + nl1(i)
         DO j = 1 , nstrms
            x1 = c1*atan((vw(j,ii)-xn*r(j,ii))/vm(j,ii))
            x2 = xx3(j)
            IF ( x1<xx1(j) ) x2 = xx4(j)
            loss(j) = loss(j)*(1.0+((x1-xx1(j))/(x2-xx1(j)))**2)
            IF ( loss(j)>0.5 ) loss(j) = 0.5
            tbeta(j,i) = tan((work(j)+(x1-xx1(j))*xx2(j))/c1)
         ENDDO
      ELSE
         DO j = 1 , nstrms
            xx2(j) = tan((atan((r(j,i+1)-r(j,i))/(x(j,i+1)-x(j,i)))+atan((r(j,i)-r(j,i-1))/(x(j,i)-x(j,i-1))))/2.0)
         ENDDO
         l1 = is1(i)
         CALL alg01(rstn(l1),xstn(l1),nspec(i),r(1,i),x1,xx3,nstrms,0,1)
         DO j = 1 , nstrms
            tbeta(j,i) = tan(atan((tan(work(j)/c1)*(1.0-xx3(j)*xx2(j))-xx2(j)*taneps(j)*sqrt(1.0+xx3(j)**2))/sqrt(1.0+xx2(j)**2))   &
                       & +xx1(j)/c1)
         ENDDO
      ENDIF
   ENDIF
END SUBROUTINE alg05
