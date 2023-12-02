!*==dsgnwr.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dsgnwr
   USE i_xnstrn
   USE i_ginox
   USE i_dsiof
   USE c_system
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   CHARACTER(4) , DIMENSION(3) :: cbuff
   INTEGER :: i , ialloc , iavail , iccer , idevic , idsnr , ifirst , inext , iop , isave , istrb , itest , kk , lasblk , maxpr1
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
! End of declarations rewritten by SPAG
!
   !>>>>EQUIVALENCE (Cbuff,Ibase)
         idsn = mdsfcb(2,ifilex)
         idsnr = idsn
         spag_nextblock_1 = 2
      CASE (2)
         istrb = fcb(5,idsnr)
         IF ( nblock>=istrb ) THEN
            ialloc = fcb(7,idsnr)
            IF ( nblock<=(ialloc+istrb-1) ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( idsn==8 ) CALL dsmsg(9)
            inext = iand(mdsfcb(3,idsnr),maskh2)
            IF ( inext==0 ) THEN
               maxpr1 = maxpri + 1
               SPAG_Loop_1_1: DO i = maxpr1 , maxfcb
                  iavail = mdsfcb(3,i)
                  IF ( iavail==0 ) THEN
                     ifirst = ialloc + istrb
                     ialloc = 20000000
                     fcb(5,i) = ifirst
                     fcb(6,i) = ifirst - 1
                     mdsfcb(3,i) = idsnr*mulq2
                     inext = i
                     mdsfcb(3,idsnr) = ior(mdsfcb(3,idsnr),i)
                     EXIT SPAG_Loop_1_1
                  ENDIF
               ENDDO SPAG_Loop_1_1
            ENDIF
         ELSE
            inext = mdsfcb(3,idsnr)/mulq2
         ENDIF
         idsnr = inext
         IF ( idsnr>=1 .AND. idsnr<=maxdsn ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL dsmsg(122)
         spag_nextblock_1 = 3
      CASE (3)
         IF ( idsn/=idsnr ) THEN
            CALL dsclos(idsn)
            mdsfcb(1,idsn) = iand(mdsfcb(1,idsn),maskh1)
            idsn = idsnr
            mdsfcb(1,idsn) = ior(mdsfcb(1,idsn),maskh2)
            mdsfcb(2,ifilex) = idsn
            CALL dsmsg(8)
            idevic = 0
            DO kk = 1 , numdev
               mdsnam(idsn)(1:2) = dev(kk)
               isave = iop
               iop = 0
               CALL dsopen(mdsnam(idsn),idsn,iop)
               iop = isave
               cbuff(indbas) = mdsnam(idsn)
               CALL dswrit(idsn,ibase(indbas+3),nbuff,ioblk,iccer)
               IF ( iccer==0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               CALL dsclos(idsn)
            ENDDO
            WRITE (iwr,99001)
99001       FORMAT (///,' NO MORE DISK SPACE AVAILABLE, JOB ABORTED.')
            CALL dsmsg(122)
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
         ioblk = nblock - istrb + 1
         CALL dswrit(idsn,ibase(indbas+3),nbuff,ioblk,iccer)
         IF ( iccer/=0 ) THEN
            IF ( iccer/=28 ) CALL dsmsg(101)
            IF ( idsn>21 .OR. idsn==8 .OR. idsn==9 ) THEN
! ALLOW XPDT TO EXTEND (IDSN=9)---NOTE IDSN=8 IS THE NPTP
               itest = index(mdsnam(8),'ZAP')
               IF ( idsn/=8 .OR. itest/=0 ) THEN
                  fcb(7,ifilex) = fcb(6,ifilex)
                  idsnr = idsn
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
            WRITE (iwr,99002)
99002       FORMAT (///,' NO MORE DISK SPACE AVAILABLE IN DEFAULT DIRECTORY',' FOR PERMANENT FILES',/,' JOB ABORTED')
            CALL dsmsg(122)
         ELSE
            lasblk = fcb(6,idsn)
            IF ( lasblk<nblock ) fcb(6,idsn) = fcb(6,idsn) + 1
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE dsgnwr
