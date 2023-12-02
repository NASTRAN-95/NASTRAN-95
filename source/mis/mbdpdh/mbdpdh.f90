!*==mbdpdh.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mbdpdh(Ajjl,F,Df,F1,Df1,F2,Df2,Xwte,Ywte,Parea,Capphi,Dphite,Dss,Q,Q1,Q2,Ndn,Nd1,Nw1,Nwn,Kte,Kte1,Kte2,Nte,Nncb,Nnsbd,   &
                & In17,Ibuf,A)
   IMPLICIT NONE
   USE C_MBOXA
   USE C_MBOXC
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nncb
   INTEGER :: Nnsbd
   REAL :: Ajjl
   REAL , DIMENSION(1) :: F
   REAL , DIMENSION(1) :: Df
   REAL , DIMENSION(1) :: F1
   REAL , DIMENSION(1) :: Df1
   REAL , DIMENSION(1) :: F2
   REAL , DIMENSION(1) :: Df2
   REAL , DIMENSION(1) :: Xwte
   REAL , DIMENSION(1) :: Ywte
   REAL , DIMENSION(50,50,3) :: Parea
   COMPLEX , DIMENSION(1) :: Capphi
   COMPLEX , DIMENSION(3,Nnsbd) :: Dphite
   COMPLEX , DIMENSION(Nncb,Nnsbd) :: Dss
   COMPLEX , DIMENSION(1) :: Q
   COMPLEX , DIMENSION(1) :: Q1
   COMPLEX , DIMENSION(1) :: Q2
   INTEGER , DIMENSION(1) :: Ndn
   INTEGER , DIMENSION(1) :: Nd1
   INTEGER , DIMENSION(1) :: Nw1
   INTEGER , DIMENSION(1) :: Nwn
   INTEGER , DIMENSION(1) :: Kte
   INTEGER , DIMENSION(1) :: Kte1
   INTEGER , DIMENSION(1) :: Kte2
   INTEGER , DIMENSION(1) :: Nte
   INTEGER :: In17
   INTEGER , DIMENSION(1) :: Ibuf
   COMPLEX , DIMENSION(1) :: A
!
! Local variable declarations rewritten by SPAG
!
   COMPLEX :: dphi , tdh , temphi , wf1 , wf2 , wphi , ws
   REAL :: ex , pad , paf1 , paf2 , paw , pawf , xb , xbb , xt , yb , z
   INTEGER :: i , ixr , iyr , j , jj , k , mood , n1 , nskp
   LOGICAL :: lphi , surf , tebox
   INTEGER , SAVE :: nhcont , nhdss
   EXTERNAL bckrec , bug , close , fread , gopen , mbgae , mbgate , mbgaw , sumphi , traile
!
! End of declarations rewritten by SPAG
!
!
   DATA nhcont , nhdss/4HCONT , 4HDSS /
!
   nskp = 0
   n1 = Npts0 + Npts1
   CALL gopen(In17,Ibuf,0)
   DO mood = 1 , Njj
      DO i = 1 , Nsbd
         Nte(i) = 0
         Dphite(1,i) = (0.0,0.0)
         Dphite(2,i) = (0.0,0.0)
         Dphite(3,i) = (0.0,0.0)
      ENDDO
      DO i = 1 , Ncb
         DO j = 1 , Nsbd
            Dss(i,j) = (0.0,0.0)
         ENDDO
      ENDDO
      DO j = 1 , Kct
         Q(j) = (0.0,0.0)
         F(j) = 0.0
         Df(j) = 0.0
      ENDDO
      IF ( Cntrl1 ) THEN
         DO j = 1 , Kc1t
            Q1(j) = (0.0,0.0)
            F1(j) = 0.0
            Df1(j) = 0.0
         ENDDO
      ENDIF
      IF ( Cntrl2 ) THEN
         DO j = 1 , Kc2t
            Q2(j) = (0.0,0.0)
            F2(j) = 0.0
            Df2(j) = 0.0
         ENDDO
      ENDIF
      CALL fread(In17,z,-nskp,0)
      jj = mood
      IF ( jj<=Npts0 ) THEN
         CALL fread(In17,F,Kct,0)
         CALL fread(In17,Df,Kct,0)
         nskp = nskp + 2*Kct
      ELSEIF ( jj>n1 ) THEN
         CALL fread(In17,F2,Kc2t,0)
         CALL fread(In17,Df2,Kc2t,0)
         nskp = nskp + Kc2t*2
      ELSE
         CALL fread(In17,F1,Kc1t,0)
         CALL fread(In17,Df1,Kc1t,0)
         nskp = nskp + Kc1t*2
      ENDIF
      CALL bckrec(In17)
!
!     START LOOP FOR ROWS ON PLANFORM
!
      Kc = 0
      Kc1 = 0
      Kc2 = 0
      DO i = 1 , Ncb
         ixr = i - 1
         xb = Boxl*(float(ixr)+0.5)
         xbb = xb + Boxl/2.0
!
!     BOXES ON PLANE OF MAIN
!
         DO j = 1 , Nsbd
            IF ( i>=Nd1(j) .AND. i<=Ndn(j) ) THEN
               dphi = (0.0,0.0)
               wphi = dphi
               tdh = (0.0,0.0)
               lphi = .FALSE.
               surf = .FALSE.
               tebox = .FALSE.
               IF ( i>=(Nw1(j)+Nwn(j))/2 ) tebox = .TRUE.
               iyr = j - 1
               yb = Boxw*float(iyr)
               k = 1
               IF ( yb>Y(2) ) k = 2
               paw = Parea(i,j,1)
               paf1 = Parea(i,j,2)
               paf2 = Parea(i,j,3)
               pawf = paw + paf1 + paf2
               IF ( .NOT.tebox .AND. Beta>Tang(k) ) pawf = 1.0
               pad = 1.0 - pawf
               ws = (0.0,0.0)
               wf1 = (0.0,0.0)
               wf2 = (0.0,0.0)
               IF ( .NOT.(j==1 .AND. Asym) ) THEN
                  IF ( j<=Nsb ) THEN
                     IF ( pad<0.995 ) THEN
                        IF ( paw>=0.005 ) THEN
!
                           Kc = Kc + 1
                           ws = 2.0*paw*cmplx(Df(Kc),Ek*F(Kc))
                        ENDIF
!
                        IF ( paf1>=0.005 ) THEN
!
                           Kc1 = Kc1 + 1
                           wf1 = 2.0*paf1*cmplx(Df1(Kc1),Ek*F1(Kc1))
                        ENDIF
!
                        IF ( paf2>=0.005 ) THEN
!
                           Kc2 = Kc2 + 1
                           wf2 = 2.0*paf2*cmplx(Df2(Kc2),Ek*F2(Kc2))
                        ENDIF
!
                        tdh = (ws+wf1+wf2)/(pawf*Cr)
                        lphi = .TRUE.
                        temphi = sumphi(ixr,iyr,Nd1,Ndn,Capphi,Dss,Nncb,Nnsbd,Asym)
                        dphi = tdh*Capphi(1) + temphi
                        IF ( pawf>=.005 ) surf = .TRUE.
                        IF ( .NOT.(.NOT.surf .OR. .NOT.tebox) ) THEN
                           Nte(j) = i
                           Dphite(3,j) = Dphite(2,j)
                           Dphite(2,j) = Dphite(1,j)
                           Dphite(1,j) = dphi
                        ENDIF
!
                        IF ( pawf>0.995 ) GOTO 5
                     ENDIF
                     IF ( tebox ) THEN
                        xt = Xwte(j)
                        IF ( xt<xbb ) THEN
                           Dphite(1,j) = traile(xt,j,Nte,Dphite,Nnsbd,Boxl)
                           IF ( xt<=xb ) THEN
                              ex = Ek*(xb-xt)/Boxl
                              wphi = Dphite(1,j)*cmplx(cos(ex),-sin(ex))
                              GOTO 5
                           ENDIF
                        ENDIF
                        IF ( xt<xbb+Boxl ) THEN
                           wphi = dphi
                           GOTO 5
                        ENDIF
                     ENDIF
                  ENDIF
                  dphi = pawf*dphi
                  wphi = (0.0,0.0)
                  IF ( .NOT.lphi ) temphi = sumphi(ixr,iyr,Nd1,Ndn,Capphi,Dss,Nncb,Nnsbd,Asym)
                  tdh = pad*(wphi-temphi)/Capphi(1) + pawf*tdh
                  IF ( .NOT.surf ) dphi = wphi
               ENDIF
 5             IF ( surf ) CALL mbgaw(Boxl,dphi,ws,paw,paf1,paf2,Q,Q1,Q2,j,Kc,Kc1,Kc2)
!
               Dss(i,j) = tdh
            ENDIF
         ENDDO
      ENDDO
      CALL mbgate(Ntote,Dphite,Nnsbd,Ywte,Q,Q1,Q2,Kte,Kte1,Kte2)
      CALL mbgae(Ajjl,In17,A,F,Df,F1,Df1,F2,Df2,Q,Q1,Q2,mood)
      CALL bug(nhcont,3000,Njj,30)
      CALL bug(nhdss,3000,Dss,4)
   ENDDO
   CALL close(In17,1)
END SUBROUTINE mbdpdh
