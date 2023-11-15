
SUBROUTINE sqdm22
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Cfrvec(20) , Cshars(4) , Cvc(4) , Deform , Dum8(8) , Dummy(35) , F(4,3) , F1 , F2 , F3 , F4 , F5 , F6 , F7 , F8 , Ff(4,3) , &
      & Fk1 , Fk2 , Fk3 , Fk4 , Fnchk , Force(17) , Frtmei(2) , Kij(9,16) , Pt(3,4) , Q1 , Q2 , Q3 , Q4 , Rg(4) , Sg(36) , Shears(4)&
      & , Sigxyz(3) , St(3) , Stress(8) , Temp , Tloads , Tsub0 , Twotop , Vec(4) , Z(1)
   INTEGER Ibfsz , Id(217) , Idm(9) , Iforce(1) , Ild , Isils(4) , Istr(1) , Isub , Ivec , Ivecn , Ldtemp , Line , Nchk , Nout
   COMMON /sdr2x4/ Dummy , Ivec , Ivecn , Ldtemp , Deform , Dum8 , Tloads
   COMMON /sdr2x7/ Id
   COMMON /sdr2x8/ Vec , Sigxyz , F , Shears , Cvc , Ff , Cfrvec , Cshars
   COMMON /sdr2x9/ Nchk , Isub , Ild , Frtmei , Twotop , Fnchk
   COMMON /system/ Ibfsz , Nout , Idm , Line
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   REAL delta , frlast(2) , tbar
   INTEGER eject
   INTEGER i , ip1 , ipart , ished(7) , istyp(2) , j , k , lld , lsub
!
! End of declarations
!
!
!     PHASE-II STRESS-DATA-RECOVERY ROUTINE FOR THE -QDMEM2- ELEMENT.
!
!     THIS ROUTINE USES DATA PREPARED BY -SQDM21-, THE PHASE-I ROUTINE,
!     TOGETHER WITH THE DISPLACEMENT VECTOR AND TEMPERATURE DATA
!     TO ARRIVE AT STRESS AND FORCE OUTPUTS.
!
   EQUIVALENCE (Temp,Ldtemp)
   EQUIVALENCE (Id(2),Isils(1)) , (Id(7),Tsub0) , (Id(8),Kij(1,1)) , (Id(101),Stress(1),Istr(1)) , (Id(152),Sg(1)) ,                &
    & (Id(188),Pt(1,1)) , (Id(200),St(1)) , (Id(201),Force(1),Iforce(1)) , (Id(203),Rg(1))
   EQUIVALENCE (F2,Force(2)) , (F1,Force(3)) , (F3,Force(4)) , (F4,Force(5)) , (F6,Force(6)) , (F5,Force(7)) , (F7,Force(8)) ,      &
    & (F8,Force(9)) , (Fk1,Force(10)) , (Q1,Force(11)) , (Fk2,Force(12)) , (Q2,Force(13)) , (Fk3,Force(14)) , (Q3,Force(15)) ,      &
    & (Fk4,Force(16)) , (Q4,Force(17))
   EQUIVALENCE (ished(1),lsub) , (ished(2),lld) , (ished(6),frlast(1))
   DATA istyp/4HQDME , 2HM2/
   DATA lsub , lld , frlast/2* - 1 , -1.0E30 , -1.0E30/
!
!     SIG , SIG , TAU   = SUMMATION((S )(U )) - (S )(TEMP-T )
!        X     Y     XY               I   I       T        0
!
   Sigxyz(1) = 0.0
   Sigxyz(2) = 0.0
   Sigxyz(3) = 0.0
   Cfrvec(2) = 0.0
   Cfrvec(3) = 0.0
   Cfrvec(4) = 0.0
!
   DO i = 1 , 4
      j = Ivec + Isils(i)
      CALL smmats(Sg(9*i-8),3,3,0,Z(j-1),3,1,0,Vec,Cvc)
      DO j = 1 , 3
         Sigxyz(j) = Sigxyz(j) + Vec(j)
         Cfrvec(j+1) = Cfrvec(j+1) + Cvc(j)
      ENDDO
   ENDDO
!
   IF ( Ldtemp/=-1 ) THEN
      tbar = Temp - Tsub0
      DO j = 1 , 3
         Sigxyz(j) = Sigxyz(j) - St(j)*tbar
      ENDDO
   ENDIF
!
!     FORCES
!          I                             T
!        (F ) = SUMMATION((K  )(U )) - (P )(TEMP-T )
!                           IJ   I       I        0
!
   ipart = 0
   DO i = 1 , 4
      F(i,1) = 0.0
      F(i,2) = 0.0
      F(i,3) = 0.0
      Ff(i,1) = 0.0
      Ff(i,2) = 0.0
      Ff(i,3) = 0.0
      DO j = 1 , 4
         k = Ivec + Isils(j)
         ipart = ipart + 1
         CALL smmats(Kij(1,ipart),3,3,0,Z(k-1),3,1,0,Vec,Cvc)
         F(i,1) = F(i,1) + Vec(1)
         F(i,2) = F(i,2) + Vec(2)
         F(i,3) = F(i,3) + Vec(3)
         Ff(i,1) = Ff(i,1) + Cvc(1)
         Ff(i,2) = Ff(i,2) + Cvc(2)
         Ff(i,3) = Ff(i,3) + Cvc(3)
      ENDDO
      IF ( Ldtemp/=-1 ) THEN
         tbar = Temp - Tsub0
         F(i,1) = F(i,1) - Pt(1,i)*tbar
         F(i,2) = F(i,2) - Pt(2,i)*tbar
         F(i,3) = F(i,3) - Pt(3,i)*tbar
      ENDIF
   ENDDO
!
!     SHEARS = SUMMATION (R )(U )
!                          I   I
   DO i = 1 , 4
      ip1 = i + 1
      IF ( ip1==5 ) ip1 = 1
      Shears(i) = (F(ip1,2)-F(i,1))/Rg(i)
      Cshars(i) = (Ff(ip1,2)-Ff(i,1))/abs(Rg(i))
   ENDDO
!
!     ALL COMPUTATIONS COMPLETE.
!
   Q1 = -Shears(1)
   Q2 = Shears(2)
   Q3 = -Shears(3)
   Q4 = Shears(4)
   Cfrvec(14) = -Cshars(1)
   Cfrvec(16) = +Cshars(2)
   Cfrvec(18) = -Cshars(3)
   Cfrvec(20) = +Cshars(4)
!
   Istr(1) = Id(1)
   Cfrvec(1) = Stress(1)
   Stress(2) = Sigxyz(1)
   Stress(3) = Sigxyz(2)
   Stress(4) = Sigxyz(3)
!
   Iforce(1) = Id(1)
   F1 = F(1,1)
   F2 = F(1,2)
   F3 = F(2,2)
   F4 = F(2,1)
   F5 = F(3,1)
   F6 = F(3,2)
   F7 = F(4,2)
   F8 = F(4,1)
   Cfrvec(6) = Ff(1,1)
   Cfrvec(5) = Ff(1,2)
   Cfrvec(7) = Ff(2,2)
   Cfrvec(8) = Ff(2,1)
   Cfrvec(10) = Ff(3,1)
   Cfrvec(9) = Ff(3,2)
   Cfrvec(11) = Ff(4,2)
   Cfrvec(12) = Ff(4,1)
!
   Fk1 = F(1,3)
   Fk2 = F(2,3)
   Fk3 = F(3,3)
   Fk4 = F(4,3)
   Cfrvec(13) = Ff(1,3)
   Cfrvec(15) = Ff(2,3)
   Cfrvec(17) = Ff(3,3)
   Cfrvec(19) = Ff(4,3)
!
   Temp = Stress(2) - Stress(3)
!
!     COMPUTE TAU
!
   Stress(8) = sqrt((Temp/2.0)**2+Stress(4)**2)
   delta = (Stress(2)+Stress(3))/2.0
!
!     COMPUTE SIGMA 1 AND SIGMA 2
!
   Stress(6) = delta + Stress(8)
   Stress(7) = delta - Stress(8)
   delta = 2.0*Stress(4)
!
!     COMPUTE PHI 1 DEPENDING ON WHETHER OR NOT SIGMA XY AND/OR
!               (SIGMA 1 - SIGMA 2) ARE ZERO
!
   IF ( abs(Temp)>=1.0E-15 ) THEN
      Stress(5) = atan2(delta,Temp)*28.64788980
   ELSEIF ( abs(delta)<1.0E-15 ) THEN
      Stress(5) = 45.0
   ELSE
      Stress(5) = 0.0
   ENDIF
   IF ( Nchk>0 ) THEN
!
!     STRESS/FORCE PRECISION CHECK
!
      k = 0
!
!     STRESSES
!
      CALL sdrchk(Stress(2),Cfrvec(2),3,k)
!
!     FORCES
!
      CALL sdrchk(Force(2),Cfrvec(5),16,k)
      IF ( k==0 ) GOTO 99999
!
!     LIMITS EXCEEDED
!
      j = 0
      IF ( lsub/=Isub .OR. frlast(1)/=Frtmei(1) .OR. lld/=Ild .OR. frlast(2)/=Frtmei(2) ) THEN
!
         lsub = Isub
         lld = Ild
         frlast(1) = Frtmei(1)
         frlast(2) = Frtmei(2)
         j = 1
         CALL page1
      ELSEIF ( eject(2)==0 ) THEN
         GOTO 50
      ENDIF
      CALL sd2rhd(ished,j)
      Line = Line + 1
      WRITE (Nout,99001)
99001 FORMAT (3X,4HTYPE,5X,3HEID,4X,2HSX,4X,2HSY,3X,3HSXY,11H  F1-4  F1-,                                                           &
             &60H2  F2-1  F2-3  F3-2  F3-4  F4-3  F4-1   K-1  SH12   K-2  SH2,25H3   K-3  SH34   K-4  SH41)
!
 50   WRITE (Nout,99002) istyp , Cfrvec
99002 FORMAT (2H0 ,A4,A2,I7,19F6.1)
   ENDIF
!
99999 END SUBROUTINE sqdm22
