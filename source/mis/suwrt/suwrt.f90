!*==suwrt.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE suwrt(Ia,Nwords,Itest)
   USE c_machin
   USE c_sof
   USE c_sys
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Ia
   INTEGER :: Nwords
   INTEGER :: Itest
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: icount , imdi , j
   INTEGER , SAVE :: idle , ieog , ieoi , iwrt
   INTEGER , DIMENSION(2) , SAVE :: nmsbr
   EXTERNAL andf , chkopn , fmdi , getblk , lshift , mesage , orf , retblk , sofcls , sofio
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     COPIES DATA FROM THE ARRAY IA ON THE SOF.  NWORD IS AN INPUT
!     PARAMETER INDICATING THE NUMBER OF WORDS TO BE COPIED.  ITEST IS
!     AN INPUT PARAMETER WHERE ITEST=1 MEANS MORE TO COME, ITEST=2 MEANS
!     WRITE END OF GROUP, AND ITEST=3 MEANS WRITE END OF ITEM.
!
   DATA idle , iwrt/0 , 2/
   DATA ieog , ieoi/4H$EOG , 4H$EOI/ , nmsbr/4HSUWR , 4HT   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         CALL chkopn(nmsbr(1))
         icount = 0
         IF ( iomode/=iwrt ) THEN
            Itest = 4
            RETURN
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     KEEP COPYING DATA FROM THE ARRAY IA INTO THE INPUT/OUTPUT BUFFER
!     UNTIL THE BUFFER IS FULL, OR UNTIL THE REQUESTED NUMBER OF WORDS
!     HAS BEEN COPIED.
!
         IF ( ioptr>blksiz+io ) THEN
!
!     THE BUFFER IS FULL.  OUTPUT IT ON THE SOF.
!
            CALL sofio(iwrt,iopbn,buf(io-2))
            CALL getblk(iopbn,j)
            IF ( j==-1 ) THEN
!
!     THERE ARE NO MORE FREE BLOCKS ON THE SOF.  RETURN THE BLOCKS THAT
!     HAVE BEEN USED SO FAR BY THE ITEM BEING WRITTEN, AND CLOSE THE SOF
!     THEN ISSUE A FATAL ERROR MESSAGE.
!
               CALL retblk(ioblk)
               CALL sofcls
!
!     ERROR MESSAGES.
!
               WRITE (nout,99001) ufm
99001          FORMAT (A23,' 6223, THERE ARE NO MORE FREE BLOCKS AVAILABLE ON',' THE SOF FILE.')
               CALL sofcls
               CALL mesage(-61,0,0)
               RETURN
            ELSE
               iopbn = j
               iolbn = iolbn + 1
               ioptr = io + 1
            ENDIF
         ENDIF
         IF ( icount==Nwords ) THEN
            IF ( Itest/=1 ) THEN
               IF ( Itest==2 ) THEN
!
!     WRITE END OF GROUP.
!
                  buf(ioptr) = ieog
               ELSEIF ( Itest==3 ) THEN
!
!     WRITE END OF ITEM, OUTPUT THE INPUT/OUTPUT BUFFER ON THE SOF, AND
!     UPDATE THE MDI.
!
                  buf(ioptr) = ieoi
                  CALL sofio(iwrt,iopbn,buf(io-2))
                  CALL fmdi(iosind,imdi)
                  buf(imdi+ioitcd) = ioblk
                  buf(imdi+ioitcd) = orf(andf(buf(imdi+ioitcd),jhalf),lshift(iolbn,ihalf))
                  mdiup = .TRUE.
                  iomode = idle
               ELSE
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               ioptr = ioptr + 1
            ENDIF
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         icount = icount + 1
         buf(ioptr) = Ia(icount)
         ioptr = ioptr + 1
         spag_nextblock_1 = 2
      CASE (4)
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE suwrt
