!*==zntpki.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE zntpki
   USE i_dsiof
   USE i_pakblk
   USE i_xnstrn
   USE c_zntpkx
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: index , itypot , kk , num
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
! End of declarations rewritten by SPAG
!
         iretrn = 0
         i = iblkb(4)
         index = (iblkb(5)-1)*iblkb(14) + 1 + iblkb(7)*iblkb(11)
         itypot = iblkb(13)
!DIR$ NOVECTOR
         IF ( itypot/=iblkb(2) ) THEN
            CALL dsupkc(iblkb(2),itypot,ibase(index),a)
         ELSE
            num = nwrdel(itypot)
            DO kk = 1 , num
               a(kk) = ibase(index+kk-1)
!DIR$ VECTOR
            ENDDO
         ENDIF
         iblkb(4) = iblkb(4) + 1
         iblkb(7) = iblkb(7) + 1
         iblkb(10) = iblkb(4)
         IF ( iblkb(7)<iblkb(6) ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL endget(iblkb)
         CALL getstr(*20,iblkb)
 20      iblkb(7) = 0
         spag_nextblock_1 = 2
      CASE (2)
         IF ( iretrn/=0 ) THEN
            ieol = 1
            iendrc = 1
         ELSE
            ieol = 0
            iendrc = 0
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE zntpki
