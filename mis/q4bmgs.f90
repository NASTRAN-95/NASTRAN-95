
SUBROUTINE q4bmgs(Dshp,Gpth,Bgpdt,Gpnorm,Phi,Bmatrx)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Anglei(4) , Detj , Edgel(4) , Edgshr(3,4) , Hzta , Psitrn(9) , Uev(3,4) , Unv(3,4)
   LOGICAL Badj , Bendng , Mbcoup , Membrn , Norpth , Shrflx
   INTEGER Iorder(4) , N1 , Nnode , Rowflg
   COMMON /q4coms/ Anglei , Edgshr , Edgel , Unv , Uev , Rowflg , Iorder
   COMMON /q4dt  / Detj , Hzta , Psitrn , Nnode , Badj , N1
   COMMON /terms / Membrn , Bendng , Shrflx , Mbcoup , Norpth
!
! Dummy argument declarations
!
   REAL Bgpdt(4,1) , Bmatrx(1) , Dshp(1) , Gpnorm(4,1) , Gpth(1) , Phi(9)
!
! Local variable declarations
!
   REAL atrans(6) , bb1 , bb2 , bb3 , bbar(120) , bsbar(48) , bsbar1(6) , deriv , tee(9) , term , thick
   INTEGER i , ib , ibar , iji , ipoint , itot , j , k , ka , kbar , kk , kkk , kp1 , kp2 , kpoint , m , n , nd2 , nd3 , nd4 , nd5 ,&
         & nd6 , ndof , ndof3 , nn
!
! End of declarations
!
!
!     THIS ROUTINE ASSEMBLES PORTIONS OF B-MATRIX FOR QUAD4
!
!*****
!     INITIALIZE
!*****
   ndof = Nnode*6
   ndof3 = Nnode*3
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
   IF ( Norpth ) nn = nd3
   IF ( Rowflg==1 ) nn = nd2
   IF ( Rowflg==2 ) nn = nd2
!*****
!     SET UP TERMS TO BE FILLED IN B-MATRIX
!*****
   DO k = 1 , Nnode
      kpoint = 6*(k-1)
      thick = Gpth(k)
!
!     COMPUTE THE TERMS WHICH GO IN THE FIRST 6(3) ROWS.
!
      IF ( Rowflg==1 ) THEN
!
!     COMPUTE THE TERMS WHICH GO IN THE LAST 2 ROWS.
!
         IF ( .NOT.Bendng ) RETURN
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
         j = Iorder(k)
         i = j - 1
         IF ( i==0 ) i = 4
!
         ib = 0
         DO
            ib = ib + 1
            bb1 = -Unv(ib,j)*Edgshr(1,j)/Edgel(j) + Unv(ib,i)*Edgshr(1,i)/Edgel(i)
            bb2 = -Unv(ib,j)*Edgshr(2,j)/Edgel(j) + Unv(ib,i)*Edgshr(2,i)/Edgel(i)
            bb3 = -Unv(ib,j)*Edgshr(3,j)/Edgel(j) + Unv(ib,i)*Edgshr(3,i)/Edgel(i)
            bsbar(kp1+ib) = Psitrn(1)*bb1 + Psitrn(2)*bb2 + Psitrn(3)*bb3
            bsbar(kp1+ib+3) = Psitrn(4)*bb1 + Psitrn(5)*bb2 + Psitrn(6)*bb3
            IF ( ib>=3 ) THEN
!
               ib = 0
               DO
                  ib = ib + 1
                  bb1 = -(Uev(ib,j)*Edgshr(1,j)+Uev(ib,i)*Edgshr(1,i))*0.5
                  bb2 = -(Uev(ib,j)*Edgshr(2,j)+Uev(ib,i)*Edgshr(2,i))*0.5
                  bb3 = -(Uev(ib,j)*Edgshr(3,j)+Uev(ib,i)*Edgshr(3,i))*0.5
                  bsbar1(ib) = Psitrn(1)*bb1 + Psitrn(2)*bb2 + Psitrn(3)*bb3
                  bsbar1(ib+3) = Psitrn(4)*bb1 + Psitrn(5)*bb2 + Psitrn(6)*bb3
                  IF ( ib>=3 ) THEN
                     CALL gmmats(bsbar1,2,3,0,tee,3,3,0,bsbar(kp2))
                     GOTO 100
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
      ELSE
         atrans(1) = -Psitrn(2)*Gpnorm(4,k) + Psitrn(3)*Gpnorm(3,k)
         atrans(2) = Psitrn(1)*Gpnorm(4,k) - Psitrn(3)*Gpnorm(2,k)
         atrans(3) = -Psitrn(1)*Gpnorm(3,k) + Psitrn(2)*Gpnorm(2,k)
         atrans(4) = -Psitrn(5)*Gpnorm(4,k) + Psitrn(6)*Gpnorm(3,k)
         atrans(5) = Psitrn(4)*Gpnorm(4,k) - Psitrn(6)*Gpnorm(2,k)
         atrans(6) = -Psitrn(4)*Gpnorm(3,k) + Psitrn(5)*Gpnorm(2,k)
!
         DO i = 1 , 2
            ipoint = nd3*(i-1)
            itot = ipoint + kpoint
            deriv = Dshp(N1*(i-1)+k)
            bbar(1+itot) = deriv*Psitrn(1)
            bbar(2+itot) = deriv*Psitrn(2)
            bbar(3+itot) = deriv*Psitrn(3)
            bbar(ndof+1+itot) = deriv*Psitrn(4)
            bbar(ndof+2+itot) = deriv*Psitrn(5)
            bbar(ndof+3+itot) = deriv*Psitrn(6)
            term = Hzta*thick*deriv
            bbar(4+itot) = term*atrans(1)
            bbar(5+itot) = term*atrans(2)
            bbar(6+itot) = term*atrans(3)
            bbar(ndof+4+itot) = term*atrans(4)
            bbar(ndof+5+itot) = term*atrans(5)
            bbar(ndof+6+itot) = term*atrans(6)
         ENDDO
      ENDIF
 100  ENDDO
!
!*****
!     FILL IN B-MATRIX FOR THE NORMAL PATH
!*****
!
   IF ( .NOT.Norpth ) THEN
!
!*****
!     FILL IN B-MATRIX FOR THE MIDI PATH
!*****
!
      DO iji = 1 , nn
         Bmatrx(iji) = 0.0E0
      ENDDO
      IF ( Rowflg==1 ) THEN
!
!     ROWFLG = 1       OUT-OF-PLANE SHEAR (LAST 2 ROWS)
!
         DO ka = 1 , Nnode
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
      ELSEIF ( Rowflg==2 ) THEN
!
!     ROWFLG = 2       IN-PLANE SHEAR (3RD AND 6TH ROWS)
!
         DO ka = 1 , Nnode
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
         IF ( Membrn ) THEN
            DO ka = 1 , Nnode
               kk = (ka-1)*6
               DO m = 1 , 3
                  Bmatrx(m+kk) = Phi(1)*bbar(m+kk) + Phi(2)*bbar(m+kk+nd3)
                  Bmatrx(m+kk+ndof) = Phi(4)*bbar(m+kk+ndof) + Phi(5)*bbar(m+kk+nd4)
               ENDDO
            ENDDO
         ENDIF
!
         IF ( Bendng ) THEN
            DO ka = 1 , Nnode
               kk = (ka-1)*6
               DO n = 4 , 6
                  Bmatrx(n+kk+nd3) = Phi(1)*bbar(n+kk) + Phi(2)*bbar(n+kk+nd3)
                  Bmatrx(n+kk+nd4) = Phi(4)*bbar(n+kk+ndof) + Phi(5)*bbar(n+kk+nd4)
               ENDDO
            ENDDO
         ENDIF
      ENDIF
   ELSEIF ( Rowflg==1 ) THEN
!
!     ROWFLG = 1       OUT-OF-PLANE SHEAR (LAST 2 ROWS)
!
      DO kbar = 1 , ndof
         ibar = ((kbar-1)/3)*3 + kbar
         Bmatrx(kbar+ndof) = bsbar(ibar)
         Bmatrx(kbar) = bsbar(ibar+3)
      ENDDO
   ELSEIF ( Rowflg==2 ) THEN
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
