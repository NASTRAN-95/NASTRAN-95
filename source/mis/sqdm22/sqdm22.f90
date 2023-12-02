!*==sqdm22.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sqdm22
   USE c_sdr2x4
   USE c_sdr2x7
   USE c_sdr2x8
   USE c_sdr2x9
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: delta , f1 , f2 , f3 , f4 , f5 , f6 , f7 , f8 , fk1 , fk2 , fk3 , fk4 , q1 , q2 , q3 , q4 , tbar , temp , tsub0
   REAL , DIMENSION(17) :: force
   REAL , DIMENSION(2) , SAVE :: frlast
   INTEGER :: i , ip1 , ipart , j , k
   INTEGER , DIMENSION(1) :: iforce , istr
   INTEGER , DIMENSION(7) :: ished
   INTEGER , DIMENSION(4) :: isils
   INTEGER , DIMENSION(2) , SAVE :: istyp
   REAL , DIMENSION(9,16) :: kij
   INTEGER , SAVE :: lld , lsub
   REAL , DIMENSION(3,4) :: pt
   REAL , DIMENSION(4) :: rg
   REAL , DIMENSION(36) :: sg
   REAL , DIMENSION(3) :: st
   REAL , DIMENSION(8) :: stress
   EXTERNAL eject , page1 , sd2rhd , sdrchk , smmats
!
! End of declarations rewritten by SPAG
!
!
!     PHASE-II STRESS-DATA-RECOVERY ROUTINE FOR THE -QDMEM2- ELEMENT.
!
!     THIS ROUTINE USES DATA PREPARED BY -SQDM21-, THE PHASE-I ROUTINE,
!     TOGETHER WITH THE DISPLACEMENT VECTOR AND TEMPERATURE DATA
!     TO ARRIVE AT STRESS AND FORCE OUTPUTS.
!
   !>>>>EQUIVALENCE (Temp,Ldtemp)
   !>>>>EQUIVALENCE (Id(2),Isils(1)) , (Id(7),Tsub0) , (Id(8),Kij(1,1)) , (Id(101),Stress(1),Istr(1)) , (Id(152),Sg(1)) ,                &
!>>>>    & (Id(188),Pt(1,1)) , (Id(200),St(1)) , (Id(201),Force(1),Iforce(1)) , (Id(203),Rg(1))
   !>>>>EQUIVALENCE (F2,Force(2)) , (F1,Force(3)) , (F3,Force(4)) , (F4,Force(5)) , (F6,Force(6)) , (F5,Force(7)) , (F7,Force(8)) ,      &
!>>>>    & (F8,Force(9)) , (Fk1,Force(10)) , (Q1,Force(11)) , (Fk2,Force(12)) , (Q2,Force(13)) , (Fk3,Force(14)) , (Q3,Force(15)) ,      &
!>>>>    & (Fk4,Force(16)) , (Q4,Force(17))
   !>>>>EQUIVALENCE (ished(1),lsub) , (ished(2),lld) , (ished(6),frlast(1))
   DATA istyp/4HQDME , 2HM2/
   DATA lsub , lld , frlast/2* - 1 , -1.0E30 , -1.0E30/
!
!     SIG , SIG , TAU   = SUMMATION((S )(U )) - (S )(TEMP-T )
!        X     Y     XY               I   I       T        0
!
   sigxyz(1) = 0.0
   sigxyz(2) = 0.0
   sigxyz(3) = 0.0
   cfrvec(2) = 0.0
   cfrvec(3) = 0.0
   cfrvec(4) = 0.0
!
   DO i = 1 , 4
      j = ivec + isils(i)
      CALL smmats(sg(9*i-8),3,3,0,z(j-1),3,1,0,vec,cvc)
      DO j = 1 , 3
         sigxyz(j) = sigxyz(j) + vec(j)
         cfrvec(j+1) = cfrvec(j+1) + cvc(j)
      ENDDO
   ENDDO
!
   IF ( ldtemp/=-1 ) THEN
      tbar = temp - tsub0
      DO j = 1 , 3
         sigxyz(j) = sigxyz(j) - st(j)*tbar
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
      f(i,1) = 0.0
      f(i,2) = 0.0
      f(i,3) = 0.0
      ff(i,1) = 0.0
      ff(i,2) = 0.0
      ff(i,3) = 0.0
      DO j = 1 , 4
         k = ivec + isils(j)
         ipart = ipart + 1
         CALL smmats(kij(1,ipart),3,3,0,z(k-1),3,1,0,vec,cvc)
         f(i,1) = f(i,1) + vec(1)
         f(i,2) = f(i,2) + vec(2)
         f(i,3) = f(i,3) + vec(3)
         ff(i,1) = ff(i,1) + cvc(1)
         ff(i,2) = ff(i,2) + cvc(2)
         ff(i,3) = ff(i,3) + cvc(3)
      ENDDO
      IF ( ldtemp/=-1 ) THEN
         tbar = temp - tsub0
         f(i,1) = f(i,1) - pt(1,i)*tbar
         f(i,2) = f(i,2) - pt(2,i)*tbar
         f(i,3) = f(i,3) - pt(3,i)*tbar
      ENDIF
   ENDDO
!
!     SHEARS = SUMMATION (R )(U )
!                          I   I
   DO i = 1 , 4
      ip1 = i + 1
      IF ( ip1==5 ) ip1 = 1
      shears(i) = (f(ip1,2)-f(i,1))/rg(i)
      cshars(i) = (ff(ip1,2)-ff(i,1))/abs(rg(i))
   ENDDO
!
!     ALL COMPUTATIONS COMPLETE.
!
   q1 = -shears(1)
   q2 = shears(2)
   q3 = -shears(3)
   q4 = shears(4)
   cfrvec(14) = -cshars(1)
   cfrvec(16) = +cshars(2)
   cfrvec(18) = -cshars(3)
   cfrvec(20) = +cshars(4)
!
   istr(1) = id(1)
   cfrvec(1) = stress(1)
   stress(2) = sigxyz(1)
   stress(3) = sigxyz(2)
   stress(4) = sigxyz(3)
!
   iforce(1) = id(1)
   f1 = f(1,1)
   f2 = f(1,2)
   f3 = f(2,2)
   f4 = f(2,1)
   f5 = f(3,1)
   f6 = f(3,2)
   f7 = f(4,2)
   f8 = f(4,1)
   cfrvec(6) = ff(1,1)
   cfrvec(5) = ff(1,2)
   cfrvec(7) = ff(2,2)
   cfrvec(8) = ff(2,1)
   cfrvec(10) = ff(3,1)
   cfrvec(9) = ff(3,2)
   cfrvec(11) = ff(4,2)
   cfrvec(12) = ff(4,1)
!
   fk1 = f(1,3)
   fk2 = f(2,3)
   fk3 = f(3,3)
   fk4 = f(4,3)
   cfrvec(13) = ff(1,3)
   cfrvec(15) = ff(2,3)
   cfrvec(17) = ff(3,3)
   cfrvec(19) = ff(4,3)
!
   temp = stress(2) - stress(3)
!
!     COMPUTE TAU
!
   stress(8) = sqrt((temp/2.0)**2+stress(4)**2)
   delta = (stress(2)+stress(3))/2.0
!
!     COMPUTE SIGMA 1 AND SIGMA 2
!
   stress(6) = delta + stress(8)
   stress(7) = delta - stress(8)
   delta = 2.0*stress(4)
!
!     COMPUTE PHI 1 DEPENDING ON WHETHER OR NOT SIGMA XY AND/OR
!               (SIGMA 1 - SIGMA 2) ARE ZERO
!
   IF ( abs(temp)>=1.0E-15 ) THEN
      stress(5) = atan2(delta,temp)*28.64788980
   ELSEIF ( abs(delta)<1.0E-15 ) THEN
      stress(5) = 45.0
   ELSE
      stress(5) = 0.0
   ENDIF
   IF ( nchk>0 ) THEN
!
!     STRESS/FORCE PRECISION CHECK
!
      k = 0
!
!     STRESSES
!
      CALL sdrchk(stress(2),cfrvec(2),3,k)
!
!     FORCES
!
      CALL sdrchk(force(2),cfrvec(5),16,k)
      IF ( k==0 ) RETURN
!
!     LIMITS EXCEEDED
!
      j = 0
      IF ( lsub/=isub .OR. frlast(1)/=frtmei(1) .OR. lld/=ild .OR. frlast(2)/=frtmei(2) ) THEN
!
         lsub = isub
         lld = ild
         frlast(1) = frtmei(1)
         frlast(2) = frtmei(2)
         j = 1
         CALL page1
      ELSEIF ( eject(2)==0 ) THEN
         GOTO 50
      ENDIF
      CALL sd2rhd(ished,j)
      line = line + 1
      WRITE (nout,99001)
99001 FORMAT (3X,4HTYPE,5X,3HEID,4X,2HSX,4X,2HSY,3X,3HSXY,11H  F1-4  F1-,                                                           &
             &60H2  F2-1  F2-3  F3-2  F3-4  F4-3  F4-1   K-1  SH12   K-2  SH2,25H3   K-3  SH34   K-4  SH41)
!
 50   WRITE (nout,99002) istyp , cfrvec
99002 FORMAT (2H0 ,A4,A2,I7,19F6.1)
   ENDIF
!
END SUBROUTINE sqdm22
