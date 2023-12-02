!*==mfree.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mfree
USE C_SMA2CL
USE C_SMA2DP
USE C_SMA2ET
USE C_SMA2IO
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: ifile , in , ip
   INTEGER , DIMENSION(100) :: necpt
   EXTERNAL sma2b
!
! End of declarations rewritten by SPAG
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
   !>>>>EQUIVALENCE (Necpt(1),Ecpt(1))
   ifile = Io(11)
   IF ( Ecpt(4)==0.0 ) RETURN
   IF ( necpt(2)/=necpt(3) ) THEN
      Dr = Ecpt(11) - Ecpt(7)
      IF ( Npvt==necpt(2) ) THEN
!
         Rp = Ecpt(7)
         Rn = Ecpt(11)
         ip = necpt(2)
         in = necpt(3)
      ELSE
         IF ( Npvt/=necpt(3) ) THEN
            CALL spag_block_1
            RETURN
         ENDIF
!
         Rp = Ecpt(11)
         Rn = Ecpt(7)
         ip = necpt(3)
!
         in = necpt(2)
      ENDIF
      Ct = (0.2617994D0/Ecpt(4))*Dr
      IF ( necpt(5)==0 ) Ct = 2.0D0*Ct
      Em = Ct*(3.0D0*Rp+Rn)
      CALL sma2b(Em,ip,ip,ifile,0.0D0)
      Em = Ct*(Rp+Rn)
      CALL sma2b(Em,in,ip,ifile,0.0D0)
      RETURN
!
!      CASE OF CENTER ELEMENT CONNECTED TO ONE POINT
!
   ELSEIF ( necpt(2)==Npvt ) THEN
      Ct = 1.5707963D0/dble(Ecpt(4))
      Rp = Ecpt(7)
      IF ( necpt(5)>0 ) THEN
         Rn = necpt(5)
         Ct = Ct/(2.0D0*Rn+2.0D0)
      ENDIF
      Em = Ct*Rp**2
      ip = Npvt
      CALL sma2b(Em,ip,ip,ifile,0.0D0)
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      RETURN
   END SUBROUTINE spag_block_1
END SUBROUTINE mfree
