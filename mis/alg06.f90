
SUBROUTINE alg06(R1,R2,X1,X2,H,S,Vm,Tb1,Tb2,W,Xk,Sclfac,Speed,Spdfac,G,Ej,Hmin,Nstrms,Pi)
   IMPLICIT NONE
   REAL Ej , G , Hmin , Pi , Sclfac , Spdfac , Speed , Xk
   INTEGER Nstrms
   REAL H(1) , R1(1) , R2(1) , S(1) , Tb1(1) , Tb2(1) , Vm(1) , W(1) , X1(1) , X2(1)
   REAL a , b(150) , dr , q1 , q2 , r(150) , w2d(150) , w3d(150) , xx1(150) , xx2(150) , xx3(150) , xx5(9,9)
   REAL alg4 , alg5
   INTEGER j , jj , jjj , l , ll , nkeep , ntub
!
!
   EQUIVALENCE (xx2(1),xx5(1,1))
!
   ntub = Nstrms - 1
   DO j = 1 , Nstrms
      q1 = H(j) - Vm(j)**2*(1.0+(Tb2(j)+R2(j)*Speed*Spdfac*Pi/(Sclfac*30.0*Vm(j)))**2)/(2.0*G*Ej)
      IF ( q1<Hmin ) q1 = Hmin
      xx1(j) = alg4(q1,S(j))
      xx2(j) = alg5(q1,S(j))
   ENDDO
   CALL alg01(R2,xx1,Nstrms,R2,q1,xx3,Nstrms,0,1)
   DO j = 1 , Nstrms
      xx1(j) = xx3(j)*G/xx2(j)
   ENDDO
   q1 = (R2(Nstrms)-R2(1))/149.0
   r(1) = R2(1)
   DO j = 2 , 150
      r(j) = r(j-1) + q1
   ENDDO
   CALL alg01(R2,xx1,Nstrms,r,xx2,q1,150,0,0)
   DO j = 1 , Nstrms
      xx3(j) = ((R2(j)-R1(j))**2+(X2(j)-X1(j))**2)*(1.0+((Tb1(j)+Tb2(j))*0.5)**2)
   ENDDO
   CALL alg01(R2,xx3,Nstrms,r,xx1,q1,150,0,0)
   DO j = 1 , Nstrms
      w2d(j) = Vm(j)**2*(1.0+Tb2(j)**2)
   ENDDO
   CALL alg01(R2,w2d,Nstrms,r,xx3,q1,150,0,0)
   CALL alg01(R2,W,Nstrms,r,w2d,q1,150,0,0)
   nkeep = Nstrms
   Nstrms = 150
   ntub = 149
   q2 = (Speed*Spdfac*Pi/(30.0*Sclfac))**2
   DO j = 1 , Nstrms
      w3d(j) = 0.0
   ENDDO
   b(1) = (r(2)-r(1))/2.0
   b(Nstrms) = (r(Nstrms)-r(ntub))/2.0
   DO j = 2 , ntub
      b(j) = (r(j+1)-r(j-1))/2.0
   ENDDO
   DO j = 1 , Nstrms
      dr = Xk*xx1(j)/xx3(j)*(q2*r(j)-xx2(j))
      IF ( dr<0 ) THEN
         IF ( j/=1 ) THEN
            IF ( r(j)+dr<=r(1) ) THEN
               a = b(j)*w2d(j)/(r(Nstrms)-r(1))
               IF ( j/=Nstrms ) a = b(j)*w2d(j)/((r(j+1)+r(j))*0.5-r(1))
               DO jj = 1 , j
                  w3d(jj) = w3d(jj) + a
               ENDDO
            ELSE
               DO jj = 2 , j
                  jjj = j - jj + 1
                  IF ( r(j)+dr>=r(jjj) ) EXIT
               ENDDO
               jjj = jjj + 1
               q1 = w2d(j)*b(j)/(b(j)-dr)
               DO jj = jjj , j
                  w3d(jj) = w3d(jj) + q1
               ENDDO
            ENDIF
            CYCLE
         ENDIF
      ELSEIF ( dr/=0 ) THEN
         IF ( j/=Nstrms ) THEN
            IF ( r(j)+dr>=r(Nstrms) ) THEN
               a = b(j)*w2d(j)/(r(Nstrms)-r(1))
               IF ( j/=1 ) a = b(j)*w2d(j)/(r(Nstrms)-(r(j)+r(j-1))*0.5)
               DO jj = j , Nstrms
                  w3d(jj) = w3d(jj) + a
               ENDDO
            ELSE
               DO jj = j , Nstrms
                  IF ( r(j)+dr<r(jj) ) EXIT
               ENDDO
               jj = jj - 1
               q1 = w2d(j)*b(j)/(b(j)+dr)
               DO jjj = j , jj
                  w3d(jjj) = w3d(jjj) + q1
               ENDDO
            ENDIF
            CYCLE
         ENDIF
      ENDIF
      w3d(j) = w3d(j) + w2d(j)
   ENDDO
   Nstrms = nkeep
   xx1(1) = 0.0
   DO ll = 1 , 150
      xx1(1) = xx1(1) + w3d(ll)
   ENDDO
   DO l = 2 , 9
      xx1(l) = 0.0
      DO ll = 1 , 150
         xx1(l) = xx1(l) + r(ll)**(l-1)*w3d(ll)
      ENDDO
   ENDDO
   DO l = 1 , 9
      DO j = l , 9
         IF ( j==1 ) THEN
            xx5(1,1) = 150
         ELSE
            xx5(l,j) = 0.0
            DO ll = 1 , 150
               xx5(l,j) = xx5(l,j) + r(ll)**(l+j-2)
            ENDDO
         ENDIF
         xx5(j,l) = xx5(l,j)
      ENDDO
   ENDDO
   CALL alg30(xx5,xx1)
   DO j = 1 , Nstrms
      W(j) = (((((((xx1(9)*R2(j)+xx1(8))*R2(j)+xx1(7))*R2(j)+xx1(6))*R2(j)+xx1(5))*R2(j)+xx1(4))*R2(j)+xx1(3))*R2(j)+xx1(2))*R2(j)  &
           & + xx1(1)
   ENDDO
END SUBROUTINE alg06
