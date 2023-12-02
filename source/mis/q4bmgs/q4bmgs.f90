!*==q4bmgs.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE q4bmgs(Dshp,Gpth,Bgpdt,Gpnorm,Phi,Bmatrx)
   USE c_q4coms
   USE c_q4dt
   USE c_terms
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: Dshp
   REAL , DIMENSION(1) :: Gpth
   REAL , DIMENSION(4,1) :: Bgpdt
   REAL , DIMENSION(4,1) :: Gpnorm
   REAL , DIMENSION(9) :: Phi
   REAL , DIMENSION(1) :: Bmatrx
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(6) :: atrans , bsbar1
   REAL :: bb1 , bb2 , bb3 , deriv , term , thick
   REAL , DIMENSION(120) :: bbar
   REAL , DIMENSION(48) :: bsbar
   INTEGER :: i , ib , ibar , iji , ipoint , itot , j , k , ka , kbar , kk , kkk , kp1 , kp2 , kpoint , m , n , nd2 , nd3 , nd4 ,   &
            & nd5 , nd6 , ndof , ndof3 , nn
   REAL , DIMENSION(9) :: tee
   EXTERNAL gmmats
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE ASSEMBLES PORTIONS OF B-MATRIX FOR QUAD4
!
!*****
!     INITIALIZE
!*****
   ndof = nnode*6
   ndof3 = nnode*3
   nd2 = ndof*2
   nd3 = ndof*3
   nd4 = ndof*4
   nd5 = ndof*5
   nd6 = ndof*6
!
!     SET THE SIZE OF B-MATRIX BASED ON THE ROW FLAG.
!     ROWFLG = 1      OUT OF PLANE SHEAR (LAST 2 ROWS)   ND2
!     ROWFLG = 2      IN-PLANE SHEAR (THIRD  ROW)        NDOF
!     ROWFLG = 3      THE FIRST SIX (THREE) ROWS         ND6 (ND3)
!
   nn = nd6
   IF ( norpth ) nn = nd3
   IF ( rowflg==1 ) nn = nd2
   IF ( rowflg==2 ) nn = nd2
!*****
!     SET UP TERMS TO BE FILLED IN B-MATRIX
!*****
   DO k = 1 , nnode
      kpoint = 6*(k-1)
      thick = Gpth(k)
!
!     COMPUTE THE TERMS WHICH GO IN THE FIRST 6(3) ROWS.
!
      IF ( rowflg==1 ) THEN
!
!     COMPUTE THE TERMS WHICH GO IN THE LAST 2 ROWS.
!
         IF ( .NOT.bendng ) RETURN
         tee(1) = 0.0
         tee(2) = -Gpnorm(4,k)
         tee(3) = Gpnorm(3,k)
         tee(4) = -tee(2)
         tee(5) = 0.0
         tee(6) = -Gpnorm(2,k)
         tee(7) = -tee(3)
         tee(8) = -tee(6)
         tee(9) = 0.0
!
         kp1 = kpoint*2
         kp2 = kp1 + 7
         j = iorder(k)
         i = j - 1
         IF ( i==0 ) i = 4
!
         ib = 0
         SPAG_Loop_2_1: DO
            ib = ib + 1
            bb1 = -unv(ib,j)*edgshr(1,j)/edgel(j) + unv(ib,i)*edgshr(1,i)/edgel(i)
            bb2 = -unv(ib,j)*edgshr(2,j)/edgel(j) + unv(ib,i)*edgshr(2,i)/edgel(i)
            bb3 = -unv(ib,j)*edgshr(3,j)/edgel(j) + unv(ib,i)*edgshr(3,i)/edgel(i)
            bsbar(kp1+ib) = psitrn(1)*bb1 + psitrn(2)*bb2 + psitrn(3)*bb3
            bsbar(kp1+ib+3) = psitrn(4)*bb1 + psitrn(5)*bb2 + psitrn(6)*bb3
            IF ( ib>=3 ) THEN
!
               ib = 0
               DO
                  ib = ib + 1
                  bb1 = -(uev(ib,j)*edgshr(1,j)+uev(ib,i)*edgshr(1,i))*0.5
                  bb2 = -(uev(ib,j)*edgshr(2,j)+uev(ib,i)*edgshr(2,i))*0.5
                  bb3 = -(uev(ib,j)*edgshr(3,j)+uev(ib,i)*edgshr(3,i))*0.5
                  bsbar1(ib) = psitrn(1)*bb1 + psitrn(2)*bb2 + psitrn(3)*bb3
                  bsbar1(ib+3) = psitrn(4)*bb1 + psitrn(5)*bb2 + psitrn(6)*bb3
                  IF ( ib>=3 ) THEN
                     CALL gmmats(bsbar1,2,3,0,tee,3,3,0,bsbar(kp2))
                     EXIT SPAG_Loop_2_1
                  ENDIF
               ENDDO
            ENDIF
         ENDDO SPAG_Loop_2_1
      ELSE
         atrans(1) = -psitrn(2)*Gpnorm(4,k) + psitrn(3)*Gpnorm(3,k)
         atrans(2) = psitrn(1)*Gpnorm(4,k) - psitrn(3)*Gpnorm(2,k)
         atrans(3) = -psitrn(1)*Gpnorm(3,k) + psitrn(2)*Gpnorm(2,k)
         atrans(4) = -psitrn(5)*Gpnorm(4,k) + psitrn(6)*Gpnorm(3,k)
         atrans(5) = psitrn(4)*Gpnorm(4,k) - psitrn(6)*Gpnorm(2,k)
         atrans(6) = -psitrn(4)*Gpnorm(3,k) + psitrn(5)*Gpnorm(2,k)
!
         DO i = 1 , 2
            ipoint = nd3*(i-1)
            itot = ipoint + kpoint
            deriv = Dshp(n1*(i-1)+k)
            bbar(1+itot) = deriv*psitrn(1)
            bbar(2+itot) = deriv*psitrn(2)
            bbar(3+itot) = deriv*psitrn(3)
            bbar(ndof+1+itot) = deriv*psitrn(4)
            bbar(ndof+2+itot) = deriv*psitrn(5)
            bbar(ndof+3+itot) = deriv*psitrn(6)
            term = hzta*thick*deriv
            bbar(4+itot) = term*atrans(1)
            bbar(5+itot) = term*atrans(2)
            bbar(6+itot) = term*atrans(3)
            bbar(ndof+4+itot) = term*atrans(4)
            bbar(ndof+5+itot) = term*atrans(5)
            bbar(ndof+6+itot) = term*atrans(6)
         ENDDO
      ENDIF
   ENDDO
!
!*****
!     FILL IN B-MATRIX FOR THE NORMAL PATH
!*****
!
   IF ( .NOT.norpth ) THEN
!
!*****
!     FILL IN B-MATRIX FOR THE MIDI PATH
!*****
!
      DO iji = 1 , nn
         Bmatrx(iji) = 0.0E0
      ENDDO
      IF ( rowflg==1 ) THEN
!
!     ROWFLG = 1       OUT-OF-PLANE SHEAR (LAST 2 ROWS)
!
         DO ka = 1 , nnode
            kk = (ka-1)*6
            DO m = 1 , 3
               n = 3 + m
               kkk = kk*2
               Bmatrx(m+kk+ndof) = bsbar(m+kkk)
               Bmatrx(n+kk+ndof) = bsbar(m+kkk+6)
               Bmatrx(m+kk) = bsbar(n+kkk)
               Bmatrx(n+kk) = bsbar(n+kkk+6)
            ENDDO
         ENDDO
      ELSEIF ( rowflg==2 ) THEN
!
!     ROWFLG = 2       IN-PLANE SHEAR (3RD AND 6TH ROWS)
!
         DO ka = 1 , nnode
            kk = (ka-1)*6
            DO m = 1 , 3
               n = 3 + m
               Bmatrx(m+kk) = Phi(4)*bbar(m+kk) + Phi(1)*bbar(m+kk+ndof) + Phi(5)*bbar(m+kk+nd3) + Phi(2)*bbar(m+kk+nd4)
               Bmatrx(n+kk+ndof) = Phi(4)*bbar(n+kk) + Phi(1)*bbar(n+kk+ndof) + Phi(5)*bbar(n+kk+nd3) + Phi(2)*bbar(n+kk+nd4)
            ENDDO
         ENDDO
      ELSE
!
!     ROWFLG = 3       FIRST SIX ROWS
!
         IF ( membrn ) THEN
            DO ka = 1 , nnode
               kk = (ka-1)*6
               DO m = 1 , 3
                  Bmatrx(m+kk) = Phi(1)*bbar(m+kk) + Phi(2)*bbar(m+kk+nd3)
                  Bmatrx(m+kk+ndof) = Phi(4)*bbar(m+kk+ndof) + Phi(5)*bbar(m+kk+nd4)
               ENDDO
            ENDDO
         ENDIF
!
         IF ( bendng ) THEN
            DO ka = 1 , nnode
               kk = (ka-1)*6
               DO n = 4 , 6
                  Bmatrx(n+kk+nd3) = Phi(1)*bbar(n+kk) + Phi(2)*bbar(n+kk+nd3)
                  Bmatrx(n+kk+nd4) = Phi(4)*bbar(n+kk+ndof) + Phi(5)*bbar(n+kk+nd4)
               ENDDO
            ENDDO
         ENDIF
      ENDIF
   ELSEIF ( rowflg==1 ) THEN
!
!     ROWFLG = 1       OUT-OF-PLANE SHEAR (LAST 2 ROWS)
!
      DO kbar = 1 , ndof
         ibar = ((kbar-1)/3)*3 + kbar
         Bmatrx(kbar+ndof) = bsbar(ibar)
         Bmatrx(kbar) = bsbar(ibar+3)
      ENDDO
   ELSEIF ( rowflg==2 ) THEN
!
!     ROWFLG = 2       IN-PLANE SHEAR (3RD ROW)
!
      DO kbar = 1 , ndof
         Bmatrx(kbar) = Phi(4)*bbar(kbar) + Phi(1)*bbar(kbar+ndof) + Phi(5)*bbar(kbar+nd3) + Phi(2)*bbar(kbar+nd4)
      ENDDO
   ELSE
!
!     ROWFLG = 3       FIRST THREE ROWS
!
      DO kbar = 1 , ndof
         Bmatrx(kbar) = Phi(1)*bbar(kbar) + Phi(2)*bbar(kbar+nd3)
         Bmatrx(kbar+ndof) = Phi(4)*bbar(kbar+ndof) + Phi(5)*bbar(kbar+nd4)
         Bmatrx(kbar+nd2) = Phi(4)*bbar(kbar) + Phi(1)*bbar(kbar+ndof) + Phi(5)*bbar(kbar+nd3) + Phi(2)*bbar(kbar+nd4)
      ENDDO
   ENDIF
!
END SUBROUTINE q4bmgs
