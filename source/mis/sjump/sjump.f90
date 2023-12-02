!*==sjump.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sjump(N)
   IMPLICIT NONE
   USE C_MACHIN
   USE C_SOF
   USE C_SYS
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: N
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: eog , eoi , indsbr , ird
   INTEGER :: icount , inxt , next
   INTEGER , DIMENSION(2) , SAVE :: nmsbr
   EXTERNAL andf , chkopn , errmkn , fnxt , rshift , sofio
!
! End of declarations rewritten by SPAG
!
!
!     JUMP OVER N GROUPS WITHIN AN ITEM WHEN IN READ MODE.  N WILL BE
!     RETURNED AS -1 IF THE END OF ITEM IS REACHED BEFORE JUMPING OVER
!     N GROUPS.
!
   DATA ird/1/
   DATA eog , eoi/4H$EOG , 4H$EOI/
   DATA indsbr/17/ , nmsbr/4HSJUM , 4HP   /
!
   CALL chkopn(nmsbr(1))
   IF ( N<=0 ) RETURN
   icount = 0
   IF ( Iomode/=ird ) THEN
      N = -2
      RETURN
   ENDIF
   SPAG_Loop_1_1: DO
      IF ( Ioptr>Blksiz+Io ) THEN
!
!     REACHED END OF BLOCK.  REPLACE THE BLOCK CURRENTLY IN CORE BY ITS
!     LINK BLOCK.
!
         CALL fnxt(Iopbn,inxt)
         IF ( mod(Iopbn,2)==1 ) THEN
            next = andf(Buf(inxt),Jhalf)
         ELSE
            next = andf(rshift(Buf(inxt),Ihalf),Jhalf)
         ENDIF
         IF ( next==0 ) THEN
            CALL errmkn(indsbr,9)
            EXIT SPAG_Loop_1_1
         ELSE
            Iopbn = next
            Iolbn = Iolbn + 1
            CALL sofio(ird,Iopbn,Buf(Io-2))
            Ioptr = Io + 1
         ENDIF
      ENDIF
      IF ( Buf(Ioptr)==eoi ) THEN
         N = -1
         RETURN
!
      ELSEIF ( Buf(Ioptr)/=eog ) THEN
!
!     SEARCH THROUGH SOF FOR END OF ITEM AND END OF GROUP.
!
         Ioptr = Ioptr + 1
      ELSE
         icount = icount + 1
         IF ( icount/=N ) THEN
            Ioptr = Ioptr + 1
         ELSE
            Ioptr = Ioptr + 1
            RETURN
         ENDIF
      ENDIF
   ENDDO SPAG_Loop_1_1
END SUBROUTINE sjump
