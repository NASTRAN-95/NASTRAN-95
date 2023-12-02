!*==spanl2.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE spanl2(Iarg)
   USE c_sdr2x4
   USE c_sdr2x7
   USE c_sdr2x8
   USE c_sdr2x9
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iarg
!
! Local variable declarations rewritten by SPAG
!
   REAL :: cs1br , ctrm , f , f1 , f2 , f3 , f4 , f5 , f6 , f7 , f8 , p13 , p24 , q1 , q2 , q3 , q4 , rk1 , rk2 , rk3 , rk4 ,       &
         & safmar , tauavg , taumax
   REAL , DIMENSION(2) , SAVE :: frlast
   INTEGER :: i , ifrvec , j , k , marsaf
   INTEGER , DIMENSION(7) :: ishd
   INTEGER , DIMENSION(2) :: istyp
   INTEGER , SAVE :: larg , lld , lsub
   INTEGER , DIMENSION(4) , SAVE :: typ
   EXTERNAL eject , page1 , sd2rhd , sdrchk , smmats
!
! End of declarations rewritten by SPAG
!
!*****
! THIS ROUTINE IS PHASE II OF STRESS DATA RECOVERY FOR THE SHEAR AND
! TWIST PANEL ELEMENTS.
!*****
!
!
!
!
! SDR2 VARIABLE CORE
!
!
! BLOCK FOR POINTERS, LOADING TEMPERATURE AND ELEMENT DEFORMATION.
!
!
! SDR2 PHASE II INPUT AND OUTPUT BLOCK.
!
!
! SDR2 SCRATCH BLOCK
!
!
! OUTPUT PRECISION CHECK BLOCK
!
!
!
   !>>>>EQUIVALENCE (Stres(1),Taumax)
   !>>>>EQUIVALENCE (Stres(2),Tauavg)
   !>>>>EQUIVALENCE (Stres(3),Marsaf,Safmar)
   !>>>>EQUIVALENCE (Forces(1),Ifor(1),P13)
   !>>>>EQUIVALENCE (Forces(2),P24)
!////////  FOLLOWING 8 FORCES MAY NOT BE EQUIVALENCED CORRECTLY YET/////
   !>>>>EQUIVALENCE (Forces(1),F1)
   !>>>>EQUIVALENCE (Forces(2),F2)
   !>>>>EQUIVALENCE (Forces(3),F3)
   !>>>>EQUIVALENCE (Forces(4),F4)
   !>>>>EQUIVALENCE (Forces(5),F5)
   !>>>>EQUIVALENCE (Forces(6),F6)
   !>>>>EQUIVALENCE (Forces(7),F7)
   !>>>>EQUIVALENCE (Forces(8),F8)
   !>>>>EQUIVALENCE (Forces(9),Rk1)
   !>>>>EQUIVALENCE (Forces(10),Q1)
   !>>>>EQUIVALENCE (Forces(11),Rk2)
   !>>>>EQUIVALENCE (Forces(12),Q2)
   !>>>>EQUIVALENCE (Forces(13),Rk3)
   !>>>>EQUIVALENCE (Forces(14),Q3)
   !>>>>EQUIVALENCE (Forces(15),Rk4)
   !>>>>EQUIVALENCE (Forces(16),Q4)
   !>>>>EQUIVALENCE (ishd(1),lsub)
   !>>>>EQUIVALENCE (ishd(2),lld)
   !>>>>EQUIVALENCE (ishd(6),frlast(1))
   !>>>>EQUIVALENCE (Cfrvec(1),Ifrvec)
!
   DATA lsub , lld , frlast/2* - 1 , -1.0E30 , -1.0E30/
   DATA typ/4HSHEA , 1HR , 4HTWIS , 1HT/
   DATA larg/0/
!
   idisp = ivec - 1
!
! COMPUTE AVERAGE STRESS ALONG SIDE 1 IF WE ARE DEALING WITH A SHEAR
! PANEL OR MEAN FIBRE SHEAR STRESS IF WE HAVE A TWIST PANEL.
!
   cs1br = 0.0
   s1bar = 0.0
   DO i = 1 , 4
      iu = idisp + isilno(i)
      IF ( Iarg==5 ) iu = iu + 3
      CALL smmats(s(1,i),3,1,1,zz(iu),3,1,0,term,ctrm)
      cs1br = cs1br + ctrm
      s1bar = s1bar + term
   ENDDO
!
! COMPUTE STRESSES AT THE CORNERS
!
   tau(1) = ratio(1)*s1bar
   tau(2) = s1bar/ratio(1)
   tau(3) = ratio(2)*s1bar
   tau(4) = ratio(3)*s1bar
   ctu(1) = abs(ratio(1))*cs1br
   ctu(2) = cs1br/abs(ratio(1))
   ctu(3) = abs(ratio(2))*cs1br
   ctu(4) = abs(ratio(3))*cs1br
!
! COMPUTE AVERAGE STRESS
!
   tauavg = 0.25*(tau(1)+tau(2)+tau(3)+tau(4))
   cfrvec(3) = 0.25E0*(ctu(1)+ctu(2)+ctu(3)+ctu(4))
!
! COMPUTE MAXIMUM STRESS
!
   taumax = abs(tau(1))
   cfrvec(2) = taumax
   DO i = 2 , 4
      IF ( abs(tau(i))>taumax ) taumax = abs(tau(i))
      IF ( ctu(i)>cfrvec(2) ) cfrvec(2) = ctu(i)
   ENDDO
!
! COMPUTE MARGIN OF SAFETY
!
   IF ( sigs<=0.0 ) THEN
      marsaf = 1
   ELSEIF ( taumax==0.0 ) THEN
      marsaf = 1
   ELSE
      safmar = sigs/taumax - 1.0
   ENDIF
!
! FOR A SHEAR PANEL COMPUTE LOADS, FOR A TWIST PANEL COMPUTE STRESSES.
!
   IF ( Iarg/=4 ) THEN
!
!     TWIST STRESSES
!
      p13 = a(1)*s1bar*t
      p24 = a(2)*s1bar*t
      term = t/6.0
      cfrvec(4) = a(1)*cs1br*t
      cfrvec(5) = a(2)*cs1br*t
      p13 = p13*term
      p24 = p24*term
      cfrvec(4) = abs(cfrvec(4)*term)
      cfrvec(5) = abs(cfrvec(5)*term)
   ELSE
!
!     SHEAR PANEL FORCES
!
      q1 = s1bar*t/sqrt(1.0+(rq(4)/rk(1))**2)
      q2 = s1bar*rq(1)/sqrt(1.0+(rq(4)/rk(2))**2)
      q3 = s1bar*rq(2)/sqrt(1.0+(rq(4)/rk(3))**2)
      q4 = s1bar*rq(3)/sqrt(1.0+(rq(4)/rk(4))**2)
      cfrvec(13) = cs1br*abs(t)/sqrt(1.0E0+(rq(4)/rk(1))**2)
      DO i = 1 , 3
         f = sqrt(1.0E0+(rq(4)/rk(i+1))**2)
         forces(2*i+10) = s1bar*rq(i)/f
         cfrvec(2*i+13) = cs1br*abs(rq(i))/f
      ENDDO
!
      f = abs(rq(4))
      rk1 = -(q1+q4)*rq(4)
      rk2 = -(q1+q2)*rq(4)
      rk3 = -(q2+q3)*rq(4)
      rk4 = -(q3+q4)*rq(4)
      cfrvec(12) = (cfrvec(13)+cfrvec(19))*f
      cfrvec(14) = (cfrvec(13)+cfrvec(15))*f
      cfrvec(16) = (cfrvec(15)+cfrvec(17))*f
      cfrvec(18) = (cfrvec(17)+cfrvec(19))*f
      f1 = q4*rk(4)
      f2 = q1*rk(1)
      f5 = q2*rk(2)
      f6 = q3*rk(3)
      cfrvec(4) = cfrvec(19)*abs(rk(4))
      cfrvec(5) = cfrvec(13)*abs(rk(1))
      cfrvec(8) = cfrvec(15)*abs(rk(2))
      cfrvec(9) = cfrvec(17)*abs(rk(3))
      f3 = -f2
      f4 = -f5
      f7 = -f6
      f8 = -f1
      cfrvec(6) = cfrvec(5)
      cfrvec(7) = cfrvec(8)
      cfrvec(10) = cfrvec(9)
      cfrvec(11) = cfrvec(4)
   ENDIF
!
! STORE ELEMENT ID IN OUTPUT SLOTS.
!
   jselid = ielid
   jfelid = ielid
   IF ( nchk>0 ) THEN
!
!  . CHECK PRECISION...
!
      k = 0
!
!  . STRESSES...
      CALL sdrchk(stres(1),cfrvec(2),2,k)
!
!  . FORCES...
      i = 16
      IF ( Iarg/=4 ) i = 2
      CALL sdrchk(forces(1),cfrvec(4),i,k)
      IF ( k==0 ) RETURN
!
!  . LIMITS EXCEEDED...
      ifrvec = ielid
      i = 1
      IF ( Iarg/=4 ) i = 3
      istyp(1) = typ(i)
      istyp(2) = typ(i+1)
      j = 0
!
      IF ( lsub/=isub .OR. frlast(1)/=frtmei(1) .OR. larg/=Iarg .OR. lld/=ild .OR. frlast(2)/=frtmei(2) ) THEN
         lsub = isub
         larg = Iarg
         lld = ild
         frlast(1) = frtmei(1)
         frlast(2) = frtmei(2)
         j = 2
         CALL page1
      ELSEIF ( eject(2)==0 ) THEN
         GOTO 50
      ENDIF
      CALL sd2rhd(ishd,j)
      line = line + 1
      IF ( Iarg==4 ) WRITE (nout,99001)
99001 FORMAT (7X,4HTYPE,5X,42HEID  SMAX  SAVE  F1-4  F1-2  F2-1  F2-3  F,                                                           &
             &60H3-2  F3-4  F4-3  F4-1   K-1  SH12   K-2  SH23   K-3  SH34   ,9HK-4  SH41)
      IF ( Iarg/=4 ) WRITE (nout,99002)
99002 FORMAT (7X,4HTYPE,5X,27HEID  SMAX  SAVE  M1-3  M2-4)
 50   i = 19
      IF ( Iarg/=4 ) i = 5
      WRITE (nout,99003) istyp , (cfrvec(j),j=1,i)
99003 FORMAT (1H0,6X,A4,A1,I7,18F6.1)
   ENDIF
!
END SUBROUTINE spanl2
