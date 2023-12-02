!*==zblpki.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE zblpki
   USE i_dsiof
   USE i_pakblk
   USE i_xnstrn
   USE c_zblpkx
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: icrow , inccnt , index , itypin , k , kk , nexrow
!
! End of declarations rewritten by SPAG
!
   iblka(15) = i
   itypin = iblka(13)
   nwords = nwrdel(itypin)
   IF ( iblka(2)>=3 ) THEN
      inccnt = 2
   ELSE
      inccnt = 1
   ENDIF
   DO k = 1 , nwords
      IF ( a(k)/=0 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
   ENDDO
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
      IF ( iblka(4)/=0 ) THEN
         nexrow = iblka(4) + iblka(7)
         icrow = iblka(15)
         IF ( icrow<nexrow ) THEN
            CALL dsmsg1(iblka)
            CALL dsmsg(119)
         ENDIF
         IF ( icrow==nexrow ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         CALL endput(iblka)
         CALL putstr(iblka)
         iblka(7) = 0
      ENDIF
      icrow = iblka(15)
      iblka(4) = icrow
      CALL spag_block_2
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
      index = (iblka(5)-1)*iblka(14) + 1
      IF ( itypin/=iblka(2) ) THEN
         CALL dsupkc(itypin,iblka(2),a,ibase(index))
      ELSE
!DIR$ NOVECTOR
         DO kk = 1 , nwords
            ibase(index+kk-1) = a(kk)
!DIR$ VECTOR
         ENDDO
      ENDIF
      iblka(5) = iblka(5) + inccnt
      iblka(7) = iblka(7) + 1
      iblka(10) = iblka(10) + iblka(11)
      IF ( iblka(6)<=iblka(7) ) THEN
         CALL endput(iblka)
         CALL putstr(iblka)
         iblka(4) = 0
         iblka(7) = 0
      ENDIF
   END SUBROUTINE spag_block_2
END SUBROUTINE zblpki
