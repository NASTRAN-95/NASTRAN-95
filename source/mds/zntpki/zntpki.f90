!*==zntpki.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE zntpki
   IMPLICIT NONE
   USE I_DSIOF
   USE I_PAKBLK
   USE I_XNSTRN
   USE C_ZNTPKX
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: index , itypot , kk , num
!
! End of declarations rewritten by SPAG
!
   iretrn = 0
   I = iblkb(4)
   index = (iblkb(5)-1)*iblkb(14) + 1 + iblkb(7)*iblkb(11)
   itypot = iblkb(13)
!DIR$ NOVECTOR
   IF ( itypot/=iblkb(2) ) THEN
      CALL dsupkc(iblkb(2),itypot,ibase(index),A)
   ELSE
      num = nwrdel(itypot)
      DO kk = 1 , num
         A(kk) = ibase(index+kk-1)
!DIR$ VECTOR
      ENDDO
   ENDIF
   iblkb(4) = iblkb(4) + 1
   iblkb(7) = iblkb(7) + 1
   iblkb(10) = iblkb(4)
   IF ( iblkb(7)<iblkb(6) ) GOTO 200
   CALL endget(iblkb)
   CALL getstr(*100,iblkb)
 100  iblkb(7) = 0
 200  IF ( iretrn/=0 ) THEN
      Ieol = 1
      Iendrc = 1
   ELSE
      Ieol = 0
      Iendrc = 0
   ENDIF
END SUBROUTINE zntpki
