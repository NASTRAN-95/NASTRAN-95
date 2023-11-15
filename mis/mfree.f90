
SUBROUTINE mfree
   IMPLICIT NONE
!
! COMMON variable declarations
!
   DOUBLE PRECISION Ct , Dr , Em , Rn , Rp
   REAL Dum(2) , Ecpt(100)
   INTEGER Io(36) , Necpt(100) , Npvt
   COMMON /sma2cl/ Dum , Npvt
   COMMON /sma2dp/ Rp , Rn , Dr , Ct , Em
   COMMON /sma2et/ Ecpt
   COMMON /sma2io/ Io
!
! Local variable declarations
!
   INTEGER ifile , in , ip
!
! End of declarations
!
!     THIS ROUTINE GENERATES MASS TERMS FOR THE INTERNALLY CREATED
!     ELEMENT WHICH DESCRIBES FREE SURFACE EFFECTS
!*****
!     THE ECPT DATA IS
!         NO.       DESCRIPTION
!         1         EL ID
!         2         SIL 1
!         3         SIL 2
!         4         GAMMA
!         5         N
!         6         0
!         7         R1
!         8         Z1
!         9         -
!         10        0
!         11        R2
!         12        Z2
!         13        -
!
!
!
!
   EQUIVALENCE (Necpt(1),Ecpt(1))
   ifile = Io(11)
   IF ( Ecpt(4)==0.0 ) GOTO 99999
   IF ( Necpt(2)/=Necpt(3) ) THEN
      Dr = Ecpt(11) - Ecpt(7)
      IF ( Npvt==Necpt(2) ) THEN
!
         Rp = Ecpt(7)
         Rn = Ecpt(11)
         ip = Necpt(2)
         in = Necpt(3)
      ELSE
         IF ( Npvt/=Necpt(3) ) GOTO 100
!
         Rp = Ecpt(11)
         Rn = Ecpt(7)
         ip = Necpt(3)
!
         in = Necpt(2)
      ENDIF
      Ct = (0.2617994D0/Ecpt(4))*Dr
      IF ( Necpt(5)==0 ) Ct = 2.0D0*Ct
      Em = Ct*(3.0D0*Rp+Rn)
      CALL sma2b(Em,ip,ip,ifile,0.0D0)
      Em = Ct*(Rp+Rn)
      CALL sma2b(Em,in,ip,ifile,0.0D0)
      GOTO 99999
!
!      CASE OF CENTER ELEMENT CONNECTED TO ONE POINT
!
   ELSEIF ( Necpt(2)==Npvt ) THEN
      Ct = 1.5707963D0/dble(Ecpt(4))
      Rp = Ecpt(7)
      IF ( Necpt(5)>0 ) THEN
         Rn = Necpt(5)
         Ct = Ct/(2.0D0*Rn+2.0D0)
      ENDIF
      Em = Ct*Rp**2
      ip = Npvt
      CALL sma2b(Em,ip,ip,ifile,0.0D0)
   ENDIF
 100  RETURN
99999 END SUBROUTINE mfree
