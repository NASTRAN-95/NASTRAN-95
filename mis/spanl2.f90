
SUBROUTINE spanl2(Iarg)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL A(2) , Cfrvec(19) , Ctu(4) , Dummy(33) , Eldefm , F1 , F2 , F3 , F4 , F5 , F6 , F7 , F8 , Fnchk , Forces(16) , Frtmei(2) ,  &
      & P13 , P24 , Q1 , Q2 , Q3 , Q4 , Ratio(3) , Rk(4) , Rk1 , Rk2 , Rk3 , Rk4 , Rq(4) , S(3,4) , S1bar , Safmar , Sigs , Stres(3)&
      & , T , Tau(4) , Tauavg , Taumax , Templd , Term , Twotop , Xxxxxx(68) , Yyyyyy(96) , Zz(1) , Zzzzzz(8)
   INTEGER Ibfsz , Icstm , Idisp , Idm(9) , Ielid , Ifor(1) , Ifrvec , Ild , Isilno(4) , Isub , Iu , Ivec , Ivecn , Jfelid ,        &
         & Jselid , Line , Marsaf , Nchk , Ncstm , Nout
   COMMON /sdr2x4/ Dummy , Icstm , Ncstm , Ivec , Ivecn , Templd , Eldefm
   COMMON /sdr2x7/ Ielid , Isilno , S , A , T , Ratio , Sigs , Rq , Rk , Xxxxxx , Jselid , Stres , Yyyyyy , Jfelid , Forces , Zzzzzz
   COMMON /sdr2x8/ S1bar , Term , Tau , Idisp , Iu , Ctu , Cfrvec
   COMMON /sdr2x9/ Nchk , Isub , Ild , Frtmei , Twotop , Fnchk
   COMMON /system/ Ibfsz , Nout , Idm , Line
   COMMON /zzzzzz/ Zz
!
! Dummy argument declarations
!
   INTEGER Iarg
!
! Local variable declarations
!
   REAL cs1br , ctrm , f , frlast(2)
   INTEGER eject
   INTEGER i , ishd(7) , istyp(2) , j , k , larg , lld , lsub , typ(4)
!
! End of declarations
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
   EQUIVALENCE (Stres(1),Taumax)
   EQUIVALENCE (Stres(2),Tauavg)
   EQUIVALENCE (Stres(3),Marsaf,Safmar)
   EQUIVALENCE (Forces(1),Ifor(1),P13)
   EQUIVALENCE (Forces(2),P24)
!////////  FOLLOWING 8 FORCES MAY NOT BE EQUIVALENCED CORRECTLY YET/////
   EQUIVALENCE (Forces(1),F1)
   EQUIVALENCE (Forces(2),F2)
   EQUIVALENCE (Forces(3),F3)
   EQUIVALENCE (Forces(4),F4)
   EQUIVALENCE (Forces(5),F5)
   EQUIVALENCE (Forces(6),F6)
   EQUIVALENCE (Forces(7),F7)
   EQUIVALENCE (Forces(8),F8)
   EQUIVALENCE (Forces(9),Rk1)
   EQUIVALENCE (Forces(10),Q1)
   EQUIVALENCE (Forces(11),Rk2)
   EQUIVALENCE (Forces(12),Q2)
   EQUIVALENCE (Forces(13),Rk3)
   EQUIVALENCE (Forces(14),Q3)
   EQUIVALENCE (Forces(15),Rk4)
   EQUIVALENCE (Forces(16),Q4)
   EQUIVALENCE (ishd(1),lsub)
   EQUIVALENCE (ishd(2),lld)
   EQUIVALENCE (ishd(6),frlast(1))
   EQUIVALENCE (Cfrvec(1),Ifrvec)
!
   DATA lsub , lld , frlast/2* - 1 , -1.0E30 , -1.0E30/
   DATA typ/4HSHEA , 1HR , 4HTWIS , 1HT/
   DATA larg/0/
!
   Idisp = Ivec - 1
!
! COMPUTE AVERAGE STRESS ALONG SIDE 1 IF WE ARE DEALING WITH A SHEAR
! PANEL OR MEAN FIBRE SHEAR STRESS IF WE HAVE A TWIST PANEL.
!
   cs1br = 0.0
   S1bar = 0.0
   DO i = 1 , 4
      Iu = Idisp + Isilno(i)
      IF ( Iarg==5 ) Iu = Iu + 3
      CALL smmats(S(1,i),3,1,1,Zz(Iu),3,1,0,Term,ctrm)
      cs1br = cs1br + ctrm
      S1bar = S1bar + Term
   ENDDO
!
! COMPUTE STRESSES AT THE CORNERS
!
   Tau(1) = Ratio(1)*S1bar
   Tau(2) = S1bar/Ratio(1)
   Tau(3) = Ratio(2)*S1bar
   Tau(4) = Ratio(3)*S1bar
   Ctu(1) = abs(Ratio(1))*cs1br
   Ctu(2) = cs1br/abs(Ratio(1))
   Ctu(3) = abs(Ratio(2))*cs1br
   Ctu(4) = abs(Ratio(3))*cs1br
!
! COMPUTE AVERAGE STRESS
!
   Tauavg = 0.25*(Tau(1)+Tau(2)+Tau(3)+Tau(4))
   Cfrvec(3) = 0.25E0*(Ctu(1)+Ctu(2)+Ctu(3)+Ctu(4))
!
! COMPUTE MAXIMUM STRESS
!
   Taumax = abs(Tau(1))
   Cfrvec(2) = Taumax
   DO i = 2 , 4
      IF ( abs(Tau(i))>Taumax ) Taumax = abs(Tau(i))
      IF ( Ctu(i)>Cfrvec(2) ) Cfrvec(2) = Ctu(i)
   ENDDO
!
! COMPUTE MARGIN OF SAFETY
!
   IF ( Sigs<=0.0 ) THEN
      Marsaf = 1
   ELSEIF ( Taumax==0.0 ) THEN
      Marsaf = 1
   ELSE
      Safmar = Sigs/Taumax - 1.0
   ENDIF
!
! FOR A SHEAR PANEL COMPUTE LOADS, FOR A TWIST PANEL COMPUTE STRESSES.
!
   IF ( Iarg/=4 ) THEN
!
!     TWIST STRESSES
!
      P13 = A(1)*S1bar*T
      P24 = A(2)*S1bar*T
      Term = T/6.0
      Cfrvec(4) = A(1)*cs1br*T
      Cfrvec(5) = A(2)*cs1br*T
      P13 = P13*Term
      P24 = P24*Term
      Cfrvec(4) = abs(Cfrvec(4)*Term)
      Cfrvec(5) = abs(Cfrvec(5)*Term)
   ELSE
!
!     SHEAR PANEL FORCES
!
      Q1 = S1bar*T/sqrt(1.0+(Rq(4)/Rk(1))**2)
      Q2 = S1bar*Rq(1)/sqrt(1.0+(Rq(4)/Rk(2))**2)
      Q3 = S1bar*Rq(2)/sqrt(1.0+(Rq(4)/Rk(3))**2)
      Q4 = S1bar*Rq(3)/sqrt(1.0+(Rq(4)/Rk(4))**2)
      Cfrvec(13) = cs1br*abs(T)/sqrt(1.0E0+(Rq(4)/Rk(1))**2)
      DO i = 1 , 3
         f = sqrt(1.0E0+(Rq(4)/Rk(i+1))**2)
         Forces(2*i+10) = S1bar*Rq(i)/f
         Cfrvec(2*i+13) = cs1br*abs(Rq(i))/f
      ENDDO
!
      f = abs(Rq(4))
      Rk1 = -(Q1+Q4)*Rq(4)
      Rk2 = -(Q1+Q2)*Rq(4)
      Rk3 = -(Q2+Q3)*Rq(4)
      Rk4 = -(Q3+Q4)*Rq(4)
      Cfrvec(12) = (Cfrvec(13)+Cfrvec(19))*f
      Cfrvec(14) = (Cfrvec(13)+Cfrvec(15))*f
      Cfrvec(16) = (Cfrvec(15)+Cfrvec(17))*f
      Cfrvec(18) = (Cfrvec(17)+Cfrvec(19))*f
      F1 = Q4*Rk(4)
      F2 = Q1*Rk(1)
      F5 = Q2*Rk(2)
      F6 = Q3*Rk(3)
      Cfrvec(4) = Cfrvec(19)*abs(Rk(4))
      Cfrvec(5) = Cfrvec(13)*abs(Rk(1))
      Cfrvec(8) = Cfrvec(15)*abs(Rk(2))
      Cfrvec(9) = Cfrvec(17)*abs(Rk(3))
      F3 = -F2
      F4 = -F5
      F7 = -F6
      F8 = -F1
      Cfrvec(6) = Cfrvec(5)
      Cfrvec(7) = Cfrvec(8)
      Cfrvec(10) = Cfrvec(9)
      Cfrvec(11) = Cfrvec(4)
   ENDIF
!
! STORE ELEMENT ID IN OUTPUT SLOTS.
!
   Jselid = Ielid
   Jfelid = Ielid
   IF ( Nchk>0 ) THEN
!
!  . CHECK PRECISION...
!
      k = 0
!
!  . STRESSES...
      CALL sdrchk(Stres(1),Cfrvec(2),2,k)
!
!  . FORCES...
      i = 16
      IF ( Iarg/=4 ) i = 2
      CALL sdrchk(Forces(1),Cfrvec(4),i,k)
      IF ( k==0 ) GOTO 99999
!
!  . LIMITS EXCEEDED...
      Ifrvec = Ielid
      i = 1
      IF ( Iarg/=4 ) i = 3
      istyp(1) = typ(i)
      istyp(2) = typ(i+1)
      j = 0
!
      IF ( lsub/=Isub .OR. frlast(1)/=Frtmei(1) .OR. larg/=Iarg .OR. lld/=Ild .OR. frlast(2)/=Frtmei(2) ) THEN
         lsub = Isub
         larg = Iarg
         lld = Ild
         frlast(1) = Frtmei(1)
         frlast(2) = Frtmei(2)
         j = 2
         CALL page1
      ELSEIF ( eject(2)==0 ) THEN
         GOTO 50
      ENDIF
      CALL sd2rhd(ishd,j)
      Line = Line + 1
      IF ( Iarg==4 ) WRITE (Nout,99001)
99001 FORMAT (7X,4HTYPE,5X,42HEID  SMAX  SAVE  F1-4  F1-2  F2-1  F2-3  F,                                                           &
             &60H3-2  F3-4  F4-3  F4-1   K-1  SH12   K-2  SH23   K-3  SH34   ,9HK-4  SH41)
      IF ( Iarg/=4 ) WRITE (Nout,99002)
99002 FORMAT (7X,4HTYPE,5X,27HEID  SMAX  SAVE  M1-3  M2-4)
 50   i = 19
      IF ( Iarg/=4 ) i = 5
      WRITE (Nout,99003) istyp , (Cfrvec(j),j=1,i)
99003 FORMAT (1H0,6X,A4,A1,I7,18F6.1)
   ENDIF
!
99999 RETURN
END SUBROUTINE spanl2
