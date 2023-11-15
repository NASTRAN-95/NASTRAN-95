
SUBROUTINE sjump(N)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Blksiz , Buf(1) , Dirsiz , Ihalf , Io , Ioblk , Ioitcd , Iolbn , Iomode , Iopbn , Ioptr , Iosind , Jhalf , Mach
   REAL Ditdum(6)
   COMMON /machin/ Mach , Ihalf , Jhalf
   COMMON /sof   / Ditdum , Io , Iopbn , Iolbn , Iomode , Ioptr , Iosind , Ioitcd , Ioblk
   COMMON /sys   / Blksiz , Dirsiz
   COMMON /zzzzzz/ Buf
!
! Dummy argument declarations
!
   INTEGER N
!
! Local variable declarations
!
   INTEGER andf , rshift
   INTEGER eog , eoi , icount , indsbr , inxt , ird , next , nmsbr(2)
   EXTERNAL andf , rshift
!
! End of declarations
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
 100  IF ( Ioptr>Blksiz+Io ) THEN
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
         GOTO 99999
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
      GOTO 100
   ELSE
      icount = icount + 1
      IF ( icount/=N ) THEN
         Ioptr = Ioptr + 1
         GOTO 100
      ELSE
         Ioptr = Ioptr + 1
         RETURN
      ENDIF
   ENDIF
99999 END SUBROUTINE sjump
