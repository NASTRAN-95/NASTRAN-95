!*==alg04.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE alg04(H,S,Vw,R1,R2,X1,X2,Vm,Eps,Sclfac,G,Ej,Hmin,Vmin,Psmid,Nstrms,Log2,Lnct,Ifail)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: H
   REAL , DIMENSION(1) :: S
   REAL , DIMENSION(1) :: Vw
   REAL , DIMENSION(1) :: R1
   REAL , DIMENSION(1) :: R2
   REAL , DIMENSION(1) :: X1
   REAL , DIMENSION(1) :: X2
   REAL , DIMENSION(1) :: Vm
   REAL :: Eps
   REAL :: Sclfac
   REAL :: G
   REAL :: Ej
   REAL :: Hmin
   REAL :: Vmin
   REAL :: Psmid
   INTEGER :: Nstrms
   INTEGER :: Log2
   INTEGER :: Lnct
   INTEGER :: Ifail
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(20) :: delr , rmid
   REAL :: delz , flow , q1 , q2 , vm2 , x3 , x4
   REAL , DIMENSION(21) :: hdn , hsdn , hsup , hup , psdn , psup , r , sdn , sup , vwdn , vwfun , vwup , vzdn , vzfun , vzup , xx1 ,&
                         & xx2
   INTEGER :: imid , istep , itub , j , j1 , j2 , jj , jstep , k , kk , kstep
   EXTERNAL alg03 , alg2 , alg29 , alg3 , alg5 , alg9
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
         DO j = 1 , Nstrms
            r(j) = (R1(j)+R2(j))*0.5
         ENDDO
         q1 = r(Nstrms) - r(1)
         q2 = Vm(1)
         DO j = 2 , Nstrms
            IF ( r(j)-r(j-1)<q1 ) q1 = r(j) - r(j-1)
            IF ( Vm(j)<q2 ) q2 = Vm(j)
         ENDDO
         delz = q2*q1**2/(Eps*Sclfac)*0.25
         q1 = (X2(1)+X2(Nstrms)-X1(1)-X1(Nstrms))*0.5
         istep = q1/delz + 1.0
         delz = q1/float(istep)
         vm2 = Vmin**2
         itub = Nstrms - 1
         imid = Nstrms/2 + 1
         DO j = 1 , Nstrms
            psup(j) = Psmid
            hup(j) = H(j)
            vwup(j) = Vw(j)
            sup(j) = S(j)
         ENDDO
         DO j = 1 , itub
            rmid(j) = (r(j)+r(j+1))*0.5
            delr(j) = r(j+1) - r(j)
         ENDDO
         Ifail = 0
         kstep = 1
         spag_nextblock_1 = 2
      CASE (2)
         CALL alg29(vwup,r,xx2,Nstrms)
         DO j = 1 , Nstrms
            vwfun(j) = Eps/r(j)*(xx2(j)-vwup(j)/r(j))*Sclfac
         ENDDO
         IF ( kstep<=1 ) THEN
            jstep = 1
            j1 = imid
            SPAG_Loop_1_2: DO
               j2 = j1 + jstep
               jj = j1
               IF ( jstep==-1 ) jj = j2
               q1 = ((vwup(j1)+vwup(j2))*0.5)**2/rmid(jj)
               q1 = delr(jj)*q1*float(jstep)
               x3 = (sup(j1)+sup(j2))*0.5
               k = 1
               SPAG_Loop_2_1: DO
                  q2 = alg2(x3,(psup(j1)+psup(j2))*0.5)
                  IF ( q2>=Hmin ) THEN
                     q2 = alg5(q2,x3)/G
                     x4 = psup(j2)
                     psup(j2) = psup(j1) + q1*q2
                     IF ( abs(x4/psup(j2)-1.0)>1.0E-5 ) THEN
                        k = k + 1
                        IF ( k>10 ) THEN
                           Ifail = 2
                           spag_nextblock_1 = 4
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                     ELSEIF ( j2==1 ) THEN
                        DO j = 1 , Nstrms
                           hsup(j) = alg2(sup(j),psup(j))
                           IF ( hsup(j)>=Hmin ) THEN
                              q1 = 2.0*G*Ej*(hup(j)-hsup(j)) - vwup(j)**2
                              IF ( q1>=vm2 ) THEN
                                 vzup(j) = sqrt(q1)
                              ELSE
                                 Ifail = 4
                                 spag_nextblock_1 = 4
                                 CYCLE SPAG_DispatchLoop_1
                              ENDIF
                           ELSE
                              Ifail = 3
                              spag_nextblock_1 = 4
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                        ENDDO
                        flow = 0.0
                        DO j = 1 , itub
                           flow = flow + (r(j+1)**2-r(j)**2)*(vzup(j)+vzup(j+1))*alg5((hsup(j)+hsup(j+1))*0.5,(sup(j)+sup(j+1))*0.5)
                        ENDDO
                        EXIT SPAG_Loop_2_1
                     ELSE
                        IF ( j2==Nstrms ) THEN
                           jstep = -1
                           j1 = imid
                        ELSE
                           j1 = j2
                        ENDIF
                        CYCLE SPAG_Loop_1_2
                     ENDIF
                  ELSE
                     Ifail = 1
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO SPAG_Loop_2_1
               EXIT SPAG_Loop_1_2
            ENDDO SPAG_Loop_1_2
         ENDIF
         CALL alg29(hsup,r,xx2,Nstrms)
         DO j = 1 , Nstrms
            hdn(j) = hup(j) + delz/vzup(j)*Eps/r(j)*xx2(j)*Sclfac
         ENDDO
         DO j = 1 , Nstrms
            vwdn(j) = vwup(j) + delz/vzup(j)*vwfun(j)
         ENDDO
         CALL alg29(vzup,r,vzfun,Nstrms)
         DO j = 1 , Nstrms
            vzfun(j) = delz*Eps*Sclfac*vzfun(j)/r(j)
            sdn(j) = sup(j)
            psdn(j) = psup(j)
         ENDDO
         kk = 1
         spag_nextblock_1 = 3
      CASE (3)
         j1 = imid
         jstep = 1
         SPAG_Loop_1_4: DO
            j2 = j1 + jstep
            jj = j1
            IF ( jstep==-1 ) jj = j2
            q1 = ((vwdn(j1)+vwdn(j2))*0.5)**2/rmid(jj)
            q1 = delr(jj)*q1*float(jstep)
            x3 = (sdn(j1)+sdn(j2))*0.5
            k = 1
            SPAG_Loop_2_3: DO
               q2 = alg2(x3,(psdn(j1)+psdn(j2))*0.5)
               IF ( q2>=Hmin ) THEN
                  q2 = alg5(q2,x3)/G
                  x4 = psdn(j2)
                  psdn(j2) = psdn(j1) + q1*q2
                  IF ( abs(x4/psdn(j2)-1.0)>1.0E-5 ) THEN
                     k = k + 1
                     IF ( k>10 ) THEN
                        Ifail = 6
                        EXIT SPAG_Loop_2_3
                     ENDIF
                  ELSEIF ( j2==1 ) THEN
                     DO j = 1 , Nstrms
                        vzdn(j) = vzup(j) + (vzfun(j)-(psdn(j)-psup(j))/alg5(hsup(j),sup(j))*G)/vzup(j)
                        hsdn(j) = hdn(j) - (vzdn(j)**2+vwdn(j)**2)/(2.0*G*Ej)
                        IF ( hsdn(j)>=Hmin ) THEN
                           sdn(j) = alg3(psdn(j),hsdn(j))
                        ELSE
                           Ifail = 7
                           EXIT SPAG_Loop_2_3
                        ENDIF
                     ENDDO
                     xx1(1) = 0.0
                     DO j = 1 , itub
                        xx1(j+1) = xx1(j) + (r(j+1)**2-r(j)**2)*(vzdn(j+1)+vzdn(j))*alg5((hsdn(j)+hsdn(j+1))*0.5,(sdn(j)+sdn(j+1))  &
                                 & *0.5)
                     ENDDO
                     q1 = xx1(Nstrms)
                     IF ( abs(q1/flow-1.0)<=1.0E-5 .AND. kk>1 ) THEN
                        IF ( kstep==istep ) THEN
                           DO j = 1 , Nstrms
                              H(j) = hdn(j)
                              S(j) = sdn(j)
                              Vw(j) = vwdn(j)
                           ENDDO
                           RETURN
                        ELSE
                           DO j = 1 , Nstrms
                              psup(j) = psdn(j)
                              hsup(j) = hsdn(j)
                              vzup(j) = vzdn(j)
                              vwup(j) = vwdn(j)
                              hup(j) = hdn(j)
                              sup(j) = sdn(j)
                           ENDDO
                           kstep = kstep + 1
                           spag_nextblock_1 = 2
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                     ELSEIF ( kk<=15 ) THEN
                        q2 = alg9(hsdn(imid),sdn(imid),vzdn(imid)**2)
                        q1 = (q1-flow)*psdn(imid)*q2/(flow*(1.0-q2))
                        DO j = 1 , Nstrms
                           psdn(j) = psdn(j) + q1
                        ENDDO
                        kk = kk + 1
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ELSE
                        Ifail = 8
                        EXIT SPAG_Loop_2_3
                     ENDIF
                  ELSE
                     IF ( j2==Nstrms ) THEN
                        j1 = imid
                        jstep = -1
                     ELSE
                        j1 = j2
                     ENDIF
                     CYCLE SPAG_Loop_1_4
                  ENDIF
               ELSE
                  Ifail = 5
                  EXIT SPAG_Loop_2_3
               ENDIF
            ENDDO SPAG_Loop_2_3
            EXIT SPAG_Loop_1_4
         ENDDO SPAG_Loop_1_4
         spag_nextblock_1 = 4
      CASE (4)
         CALL alg03(Lnct,1)
         WRITE (Log2,99001) Ifail
99001    FORMAT (5X,30HMIXING CALCULATION FAILURE NO.,I2)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE alg04
