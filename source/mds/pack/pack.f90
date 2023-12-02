!*==pack.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pack(A,File,Mcb)
   USE i_dsiof
   USE i_pakblk
   USE i_xnstrn
   USE c_ddiosv
   USE c_packx
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(4) :: A
   INTEGER :: File
   INTEGER , DIMENSION(7) :: Mcb
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: incrr , indea1 , indexa , indexb , irow , k , kk , klast , klim , lasind , ncnt , nwdin
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
! End of declarations rewritten by SPAG
!
         name = File
         iblkc(1) = name
         iblkc(2) = itypot
         iblkc(3) = 0
         iblkc(4) = 0
         iblkc(7) = 0
         iblkc(8) = -1
         iblkc(9) = itypin
         iblkc(10) = 0
         IF ( itypin>0 .AND. itypin<=4 ) THEN
            IF ( itypot>0 .AND. itypot<=4 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         CALL dsmsg1(iblkc)
         CALL dsmsg(118)
         spag_nextblock_1 = 2
      CASE (2)
         nwdin = nwrdel(itypin)
         iblkc(12) = Mcb(2) + 1
         CALL dsgefl
         iflpos(1,ifilex) = fcb(3,ifilex)
         iflpos(2,ifilex) = fcb(4,ifilex)
         CALL putstr(iblkc)
         ieor = 0
         indexa = 0
         irow = irobgn
         indexb = (iblkc(5)-1)*iblkc(14) + 1
         spag_nextblock_1 = 3
      CASE (3)
!DIR$ NOVECTOR
         DO k = 1 , nwdin
            IF ( A(indexa+k)/=0 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
!DIR$ VECTOR
         lasind = (lasrow-irow+1)*incr*nwdin
         klim = lasind + incr
         klast = klim
         incrr = incr*nwdin
         DO kk = 1 , nwdin
            indea1 = indexa - 1 + kk
            SPAG_Loop_2_1: DO k = 1 , lasind , incrr
               IF ( A(indea1+k)/=0 ) THEN
                  IF ( k<klast ) klast = k
                  EXIT SPAG_Loop_2_1
               ENDIF
            ENDDO SPAG_Loop_2_1
         ENDDO
         ncnt = ((klast-1)/incrr) - 1
         IF ( klast==klim ) ncnt = lasrow - irow
         irow = irow + ncnt
         indexa = indexa + ncnt*(nwdin*incr)
         ieor = 1
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
         IF ( iblkc(7)==0 ) THEN
            iblkc(4) = irow
         ELSEIF ( ieor/=0 ) THEN
            CALL endput(iblkc)
            CALL putstr(iblkc)
            iblkc(7) = 0
            indexb = (iblkc(5)-1)*iblkc(14) + 1
            iblkc(4) = irow
         ENDIF
         IF ( itypin/=itypot ) THEN
            CALL dsupkc(itypin,itypot,A(indexa+1),ibase(indexb))
         ELSE
!DIR$ NOVECTOR
            DO k = 1 , nwdin
               ibase(indexb+k-1) = A(indexa+k)
!DIR$ VECTOR
            ENDDO
         ENDIF
         ieor = 0
         indexb = indexb + iblkc(11)
         iblkc(7) = iblkc(7) + 1
         iblkc(10) = iblkc(10) + iblkc(11)
         IF ( iblkc(7)>=iblkc(6) ) THEN
            CALL endput(iblkc)
            CALL putstr(iblkc)
            iblkc(7) = 0
            indexb = (iblkc(5)-1)*iblkc(14) + 1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         indexa = indexa + (incr*nwdin)
         irow = irow + 1
         IF ( irow<=lasrow ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL dsbpnk(iblkc,Mcb)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE pack
