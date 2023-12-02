!*==srod2.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE srod2
   IMPLICIT NONE
   USE C_SDR2DE
   USE C_SDR2X4
   USE C_SDR2X7
   USE C_SDR2X8
   USE C_SDR2X9
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(4) :: cfrvec
   REAL :: cp , crta , crtb , csiga , ctau , ctrna , ctrnb , ctrque
   REAL , DIMENSION(2) , SAVE :: frlast
   INTEGER :: idisp , j , k , ldtemp , mssig , mstau
   INTEGER , DIMENSION(7) :: ishd
   INTEGER , SAVE :: lld , lsub
   INTEGER , DIMENSION(4) , SAVE :: typ
   EXTERNAL eject , page1 , sd2rhd , sdrchk , smmats
!
! End of declarations rewritten by SPAG
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
   !>>>>EQUIVALENCE (Templd,Ldtemp) , (Smsig,Mssig) , (Smtau,Mstau) , (Cfrvec(1),csiga) , (Cfrvec(2),ctau) , (Cfrvec(3),cp) ,            &
!>>>>    & (Cfrvec(4),ctrque) , (Ifrvec(4),Cfrvec(1)) , (ishd(1),lsub) , (ishd(2),lld) , (ishd(6),frlast(1))
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
   IF ( ldtemp/=(-1) ) Sigma = Sigma + St*(Templd-Tsubc0)
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
         mssig = 1
      ELSEIF ( Sigmac<=0.0 ) THEN
         mssig = 1
      ELSE
         Smsig = -Sigmac/Sigma - 1.0
      ENDIF
   ELSEIF ( Sigmat<=0.0 ) THEN
      mssig = 1
   ELSE
      Smsig = Sigmat/Sigma - 1.0
   ENDIF
!
!     COMPUTE MARGIN OF SAFETY IN TORSION
!
   IF ( Sigmas<=0.0 ) THEN
      mstau = 1
   ELSEIF ( Tau==0.0 ) THEN
      mstau = 1
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
      CALL sdrchk(Chkvec,cfrvec,4,k)
!
      IF ( k==0 ) RETURN
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
END SUBROUTINE srod2
