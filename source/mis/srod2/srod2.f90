!*==srod2.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE srod2
   USE c_sdr2de
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
   idisp = ivec - 1
   iuta = idisp + isilno(1)
   CALL smmats(sat(1),3,1,1,zz(iuta),3,1,0,trana,ctrna)
   iutb = idisp + isilno(2)
   CALL smmats(sbt(1),3,1,1,zz(iutb),3,1,0,tranb,ctrnb)
   sigma = trana + tranb + sdelta*eldefm
   csiga = ctrna + ctrnb
   IF ( ldtemp/=(-1) ) sigma = sigma + st*(templd-tsubc0)
   iura = iuta + 3
   chkvec(1) = sigma
   CALL smmats(sar(1),3,1,1,zz(iura),3,1,0,rota,crta)
   iurb = iutb + 3
   CALL smmats(sbr(1),3,1,1,zz(iurb),3,1,0,rotb,crtb)
   torque = rota + rotb
   cp = area*csiga
   chkvec(3) = p
   ctau = abs(fjovrc)*ctrque
   chkvec(2) = tau
   ctrque = crta + crtb
   chkvec(4) = torque
!
! COMPUTE AXIAL FORCE, P, AND TORQUE
!
   p = area*sigma
   tau = fjovrc*torque
!
! COMPUTE MARGIN OF SAFETY IN EXTENSION
!
   IF ( sigma<=0.0 ) THEN
      IF ( sigma==0.0 ) THEN
         mssig = 1
      ELSEIF ( sigmac<=0.0 ) THEN
         mssig = 1
      ELSE
         smsig = -sigmac/sigma - 1.0
      ENDIF
   ELSEIF ( sigmat<=0.0 ) THEN
      mssig = 1
   ELSE
      smsig = sigmat/sigma - 1.0
   ENDIF
!
!     COMPUTE MARGIN OF SAFETY IN TORSION
!
   IF ( sigmas<=0.0 ) THEN
      mstau = 1
   ELSEIF ( tau==0.0 ) THEN
      mstau = 1
   ELSE
      smtau = sigmas/abs(tau) - 1.0
   ENDIF
   jselid = ielid
   jfelid = ielid
   IF ( nchk>0 ) THEN
!
!  . CHECK PRECISION...
!
      ifrvec(3) = ielid
      k = 0
      CALL sdrchk(chkvec,cfrvec,4,k)
!
      IF ( k==0 ) RETURN
!
!  . LIMITS EXCEEDED...
!
      j = 0
      ifrvec(1) = typ(4)
      IF ( ieltyp==10 ) ifrvec(1) = typ(1)
      ifrvec(2) = typ(2)
      IF ( ieltyp==3 ) ifrvec(2) = typ(3)
!
      IF ( lsub/=isub .OR. frlast(1)/=frtmei(1) .OR. lld/=ild .OR. frlast(2)/=frtmei(2) ) THEN
!
         lsub = isub
         lld = ild
         frlast(1) = frtmei(1)
         frlast(2) = frtmei(2)
         j = 1
         CALL page1
!
      ELSEIF ( eject(2)==0 ) THEN
         GOTO 50
      ENDIF
!
      CALL sd2rhd(ishd,j)
      line = line + 1
      WRITE (nout,99001)
99001 FORMAT (7X,4HTYPE,5X,3HEID,5X,2HSA,5X,2HST,5X,9HAF TORQUE)
 50   WRITE (nout,99002) ifrvec
99002 FORMAT (1H0,3X,2A4,I7,4F7.1)
   ENDIF
END SUBROUTINE srod2
