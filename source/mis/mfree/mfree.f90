!*==mfree.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mfree
   USE c_sma2cl
   USE c_sma2dp
   USE c_sma2et
   USE c_sma2io
   USE iso_fortran_env
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
   ifile = io(11)
   IF ( ecpt(4)==0.0 ) RETURN
   IF ( necpt(2)/=necpt(3) ) THEN
      dr = ecpt(11) - ecpt(7)
      IF ( npvt==necpt(2) ) THEN
!
         rp = ecpt(7)
         rn = ecpt(11)
         ip = necpt(2)
         in = necpt(3)
      ELSE
         IF ( npvt/=necpt(3) ) THEN
            CALL spag_block_1
            RETURN
         ENDIF
!
         rp = ecpt(11)
         rn = ecpt(7)
         ip = necpt(3)
!
         in = necpt(2)
      ENDIF
      ct = (0.2617994D0/ecpt(4))*dr
      IF ( necpt(5)==0 ) ct = 2.0D0*ct
      em = ct*(3.0D0*rp+rn)
      CALL sma2b(em,ip,ip,ifile,0.0D0)
      em = ct*(rp+rn)
      CALL sma2b(em,in,ip,ifile,0.0D0)
      RETURN
!
!      CASE OF CENTER ELEMENT CONNECTED TO ONE POINT
!
   ELSEIF ( necpt(2)==npvt ) THEN
      ct = 1.5707963D0/dble(ecpt(4))
      rp = ecpt(7)
      IF ( necpt(5)>0 ) THEN
         rn = necpt(5)
         ct = ct/(2.0D0*rn+2.0D0)
      ENDIF
      em = ct*rp**2
      ip = npvt
      CALL sma2b(em,ip,ip,ifile,0.0D0)
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      USE ISO_FORTRAN_ENV                 
   END SUBROUTINE spag_block_1
END SUBROUTINE mfree
