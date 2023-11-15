
SUBROUTINE srod2
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Area , Cfrvec(4) , Chkvec(4) , Dummy(33) , Dummy2(77) , Dummy3(95) , Dummy4(22) , Eldefm , Fjovrc , Fnchk , Frtmei(2) , P , &
      & Rota , Rotb , Sar(3) , Sat(3) , Sbr(3) , Sbt(3) , Sdelta , Sigma , Sigmac , Sigmas , Sigmat , Skp2de(8) , Smsig , Smtau ,   &
      & St , Tau , Templd , Torque , Trana , Tranb , Tsubc0 , Twotop , Zz(1)
   INTEGER Ibfsz , Icstm , Idm(9) , Ielid , Ieltyp , Ifrvec(7) , Ild , Isilno(2) , Isub , Iura , Iurb , Iuta , Iutb , Ivec , Ivecn ,&
         & Jfelid , Jselid , Ldtemp , Line , Mssig , Mstau , Nchk , Ncstm , Nout
   COMMON /sdr2de/ Skp2de , Ieltyp
   COMMON /sdr2x4/ Dummy , Icstm , Ncstm , Ivec , Ivecn , Templd , Eldefm
   COMMON /sdr2x7/ Ielid , Isilno , Sat , Sbt , Sar , Sbr , St , Sdelta , Area , Fjovrc , Tsubc0 , Sigmat , Sigmac , Sigmas ,       &
                 & Dummy2 , Jselid , Sigma , Smsig , Tau , Smtau , Dummy3 , Jfelid , P , Torque , Dummy4
   COMMON /sdr2x8/ Trana , Tranb , Rota , Rotb , Iuta , Iutb , Iura , Iurb , Ifrvec , Chkvec
   COMMON /sdr2x9/ Nchk , Isub , Ild , Frtmei , Twotop , Fnchk
   COMMON /system/ Ibfsz , Nout , Idm , Line
   COMMON /zzzzzz/ Zz
!
! Local variable declarations
!
   REAL cp , crta , crtb , csiga , ctau , ctrna , ctrnb , ctrque , frlast(2)
   INTEGER eject
   INTEGER idisp , ishd(7) , j , k , lld , lsub , typ(4)
!
! End of declarations
!
!*****
! THIS ROUTINE IS PHASE II OF STRESS DATA RECOVERY FOR THE ROD.
!*****
!
!
! SDR2 VARIABLE CORE
!
!
! BLOCK FOR POINTERS, LOADING TEMPERATURE AND ELEMENT DEFORMATION.
!
!
! SDR2 INPUT AND OUTPUT BLOCK
!
!
! SCRATCH BLOCK
!
!
!
   EQUIVALENCE (Templd,Ldtemp) , (Smsig,Mssig) , (Smtau,Mstau) , (Cfrvec(1),csiga) , (Cfrvec(2),ctau) , (Cfrvec(3),cp) ,            &
    & (Cfrvec(4),ctrque) , (Ifrvec(4),Cfrvec(1)) , (ishd(1),lsub) , (ishd(2),lld) , (ishd(6),frlast(1))
!
   DATA lld , lsub , frlast/2* - 1 , -1.0E30 , -1.0E30/
   DATA typ/4H CON , 4HROD  , 4HTUBE , 1H /
!
   idisp = Ivec - 1
   Iuta = idisp + Isilno(1)
   CALL smmats(Sat(1),3,1,1,Zz(Iuta),3,1,0,Trana,ctrna)
   Iutb = idisp + Isilno(2)
   CALL smmats(Sbt(1),3,1,1,Zz(Iutb),3,1,0,Tranb,ctrnb)
   Sigma = Trana + Tranb + Sdelta*Eldefm
   csiga = ctrna + ctrnb
   IF ( Ldtemp/=(-1) ) Sigma = Sigma + St*(Templd-Tsubc0)
   Iura = Iuta + 3
   Chkvec(1) = Sigma
   CALL smmats(Sar(1),3,1,1,Zz(Iura),3,1,0,Rota,crta)
   Iurb = Iutb + 3
   CALL smmats(Sbr(1),3,1,1,Zz(Iurb),3,1,0,Rotb,crtb)
   Torque = Rota + Rotb
   cp = Area*csiga
   Chkvec(3) = P
   ctau = abs(Fjovrc)*ctrque
   Chkvec(2) = Tau
   ctrque = crta + crtb
   Chkvec(4) = Torque
!
! COMPUTE AXIAL FORCE, P, AND TORQUE
!
   P = Area*Sigma
   Tau = Fjovrc*Torque
!
! COMPUTE MARGIN OF SAFETY IN EXTENSION
!
   IF ( Sigma<=0.0 ) THEN
      IF ( Sigma==0.0 ) THEN
         Mssig = 1
      ELSEIF ( Sigmac<=0.0 ) THEN
         Mssig = 1
      ELSE
         Smsig = -Sigmac/Sigma - 1.0
      ENDIF
   ELSEIF ( Sigmat<=0.0 ) THEN
      Mssig = 1
   ELSE
      Smsig = Sigmat/Sigma - 1.0
   ENDIF
!
!     COMPUTE MARGIN OF SAFETY IN TORSION
!
   IF ( Sigmas<=0.0 ) THEN
      Mstau = 1
   ELSEIF ( Tau==0.0 ) THEN
      Mstau = 1
   ELSE
      Smtau = Sigmas/abs(Tau) - 1.0
   ENDIF
   Jselid = Ielid
   Jfelid = Ielid
   IF ( Nchk>0 ) THEN
!
!  . CHECK PRECISION...
!
      Ifrvec(3) = Ielid
      k = 0
      CALL sdrchk(Chkvec,Cfrvec,4,k)
!
      IF ( k==0 ) GOTO 99999
!
!  . LIMITS EXCEEDED...
!
      j = 0
      Ifrvec(1) = typ(4)
      IF ( Ieltyp==10 ) Ifrvec(1) = typ(1)
      Ifrvec(2) = typ(2)
      IF ( Ieltyp==3 ) Ifrvec(2) = typ(3)
!
      IF ( lsub/=Isub .OR. frlast(1)/=Frtmei(1) .OR. lld/=Ild .OR. frlast(2)/=Frtmei(2) ) THEN
!
         lsub = Isub
         lld = Ild
         frlast(1) = Frtmei(1)
         frlast(2) = Frtmei(2)
         j = 1
         CALL page1
!
      ELSEIF ( eject(2)==0 ) THEN
         GOTO 50
      ENDIF
!
      CALL sd2rhd(ishd,j)
      Line = Line + 1
      WRITE (Nout,99001)
99001 FORMAT (7X,4HTYPE,5X,3HEID,5X,2HSA,5X,2HST,5X,9HAF TORQUE)
 50   WRITE (Nout,99002) Ifrvec
99002 FORMAT (1H0,3X,2A4,I7,4F7.1)
   ENDIF
99999 END SUBROUTINE srod2
