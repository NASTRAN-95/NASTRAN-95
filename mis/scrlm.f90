
SUBROUTINE scrlm(Scurl,Xxi,E,H,Cont,Rp,Alf1,R1,Lam1,Hf)
   IMPLICIT NONE
   REAL Alf1 , Cont , H , Hf , Lam1 , R1 , Rp
   REAL E(2,2) , Scurl(15,10) , Xxi(3)
   REAL eel , el , ell , lam2 , lam3 , lam4 , sc1 , sc2 , xx1 , xx2 , xx3 , xx4 , xx5
   INTEGER i , j , jj , k , kk , ll
!
! THIS SUBROUTINE COMPUTES THE STRESS MATRIX IN FIELD COORDINATES
! FOR THE TOROIDAL RING ELEMENT
!
!
! NOTE THE DOUBLE SUBSCRIPTING USED IN THE SCRLM SUBROUTINE IS
! COMPATIBLE WITH THE CALLING PROGRAM. THE SEL ARRAY WILL RETURN WITH
! THE STRESS MATRIX TRANSPOSED (10X15, STORED ROWWISE) BUT IN THE SCRLM
! SUBROUTINE THE STRESS MATRIX IS COMPUTED AS A DOUBLY SUBSCRIPTED
! 15X10 ARRAY (STORED COLUMNWISE).
!
!     ------------------------------------------------------------------
!
   sc1 = H
   sc2 = Hf**3/12.0
   jj = 1
   kk = 3
   ll = 5
!
   DO i = 1 , 3
      xx1 = Xxi(i)
      xx2 = xx1*xx1
      xx3 = xx2*xx1
      xx4 = xx3*xx1
      xx5 = xx4*xx1
      CALL solve1(Alf1,R1,Rp,xx1,lam2,lam3,lam4,Cont)
      DO j = 1 , 2
         Scurl(jj,1) = lam2*E(j,2)
         Scurl(jj,2) = Scurl(jj,1)*xx1 + E(1,j)
         Scurl(jj,3) = Scurl(jj,1)*xx2 + E(1,j)*2.0*xx1
         Scurl(jj,4) = Scurl(jj,1)*xx3 + E(1,j)*3.0*xx2
         Scurl(jj,5) = Lam1*E(1,j) + lam3*E(j,2)
         Scurl(jj,6) = Scurl(jj,5)*xx1
         Scurl(jj,7) = Scurl(jj,5)*xx2
         Scurl(jj,8) = Scurl(jj,5)*xx3
         Scurl(jj,9) = Scurl(jj,5)*xx4
         Scurl(jj,10) = Scurl(jj,5)*xx5
         jj = jj + 1
      ENDDO
      jj = jj + 3
      DO k = 1 , 2
         Scurl(kk,1) = 0.0
         Scurl(kk,2) = 0.0
         Scurl(kk,3) = 0.0
         Scurl(kk,4) = 0.0
         Scurl(kk,5) = 0.0
         Scurl(kk,6) = -lam2*E(k,2)
         Scurl(kk,7) = Scurl(kk,6)*2.0*xx1 - E(1,k)*2.0
         Scurl(kk,8) = Scurl(kk,6)*3.0*xx2 - E(1,k)*6.0*xx1
         Scurl(kk,9) = Scurl(kk,6)*4.0*xx3 - E(1,k)*12.0*xx2
         Scurl(kk,10) = Scurl(kk,6)*5.0*xx4 - E(1,k)*20.0*xx3
         kk = kk + 1
      ENDDO
      kk = kk + 3
      el = E(1,1)*lam2
      ell = el*Lam1
      eel = E(1,1)*Lam1
      Scurl(ll,1) = 0.0
      Scurl(ll,2) = 0.0
      Scurl(ll,3) = 0.0
      Scurl(ll,4) = 0.0
      Scurl(ll,5) = 0.0
      Scurl(ll,6) = lam2**2*E(2,2) - lam4*E(1,2)
      Scurl(ll,7) = Scurl(ll,6)*2.0*xx1 - 2.0*el
      Scurl(ll,8) = Scurl(ll,6)*3.0*xx2 - 6.0*(el*xx1+E(1,1))
      Scurl(ll,9) = Scurl(ll,6)*4.0*xx3 - 12.0*el*xx2 - 24.0*E(1,1)*xx1
      Scurl(ll,10) = Scurl(ll,6)*5.0*xx4 - 20.0*el*xx3 - 60.0*E(1,1)*xx2
      ll = ll + 5
   ENDDO
!
!     ADJUSTMENT FOR SHELL CAP CASE
   IF ( Alf1==0.0 ) THEN
      Scurl(1,2) = E(1,2) + E(1,1)
      Scurl(2,2) = E(2,2) + E(1,2)
      Scurl(3,7) = -2.*(E(1,2)+E(1,1))
      Scurl(4,7) = 2.*(E(2,2)+E(1,2))
      Scurl(5,8) = 3.*(E(2,2)-4.*E(1,1))
   ENDIF
   DO j = 1 , 15 , 5
      DO i = 1 , 10
         Scurl(j,i) = Scurl(j,i)*sc1
         Scurl(j+1,i) = Scurl(j+1,i)*sc1
         Scurl(j+2,i) = Scurl(j+2,i)*sc2
         Scurl(j+3,i) = Scurl(j+3,i)*sc2
         Scurl(j+4,i) = Scurl(j+4,i)*sc2
      ENDDO
   ENDDO
END SUBROUTINE scrlm