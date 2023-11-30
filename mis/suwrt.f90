
SUBROUTINE suwrt(Ia,Nwords,Itest)
   IMPLICIT NONE
   INTEGER Blksiz , Buf(1) , Dirsiz , Ihalf , Io , Ioblk , Ioitcd , Iolbn , Iomode , Iopbn , Ioptr , Iosind , Jhalf , Mach , Mdi ,  &
         & Mdibl , Mdilbn , Mdipbn , Nbuff , Nout , Nxtdum(15)
   REAL Ditdum(6) , Ditup
   LOGICAL Mdiup
   CHARACTER*23 Ufm
   COMMON /machin/ Mach , Ihalf , Jhalf
   COMMON /sof   / Ditdum , Io , Iopbn , Iolbn , Iomode , Ioptr , Iosind , Ioitcd , Ioblk , Mdi , Mdipbn , Mdilbn , Mdibl , Nxtdum ,&
                 & Ditup , Mdiup
   COMMON /sys   / Blksiz , Dirsiz
   COMMON /system/ Nbuff , Nout
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Buf
   INTEGER Itest , Nwords
   INTEGER Ia(1)
   INTEGER andf , lshift , orf
   INTEGER icount , idle , ieog , ieoi , imdi , iwrt , j , nmsbr(2)
   EXTERNAL andf , lshift , orf
!
!     COPIES DATA FROM THE ARRAY IA ON THE SOF.  NWORD IS AN INPUT
!     PARAMETER INDICATING THE NUMBER OF WORDS TO BE COPIED.  ITEST IS
!     AN INPUT PARAMETER WHERE ITEST=1 MEANS MORE TO COME, ITEST=2 MEANS
!     WRITE END OF GROUP, AND ITEST=3 MEANS WRITE END OF ITEM.
!
   DATA idle , iwrt/0 , 2/
   DATA ieog , ieoi/4H$EOG , 4H$EOI/ , nmsbr/4HSUWR , 4HT   /
!
   CALL chkopn(nmsbr(1))
   icount = 0
   IF ( Iomode/=iwrt ) THEN
      Itest = 4
      RETURN
   ENDIF
!
!     KEEP COPYING DATA FROM THE ARRAY IA INTO THE INPUT/OUTPUT BUFFER
!     UNTIL THE BUFFER IS FULL, OR UNTIL THE REQUESTED NUMBER OF WORDS
!     HAS BEEN COPIED.
!
 100  IF ( Ioptr>Blksiz+Io ) THEN
!
!     THE BUFFER IS FULL.  OUTPUT IT ON THE SOF.
!
      CALL sofio(iwrt,Iopbn,Buf(Io-2))
      CALL getblk(Iopbn,j)
      IF ( j==-1 ) THEN
!
!     THERE ARE NO MORE FREE BLOCKS ON THE SOF.  RETURN THE BLOCKS THAT
!     HAVE BEEN USED SO FAR BY THE ITEM BEING WRITTEN, AND CLOSE THE SOF
!     THEN ISSUE A FATAL ERROR MESSAGE.
!
         CALL retblk(Ioblk)
         CALL sofcls
!
!     ERROR MESSAGES.
!
         WRITE (Nout,99001) Ufm
99001    FORMAT (A23,' 6223, THERE ARE NO MORE FREE BLOCKS AVAILABLE ON',' THE SOF FILE.')
         CALL sofcls
         CALL mesage(-61,0,0)
         GOTO 99999
      ELSE
         Iopbn = j
         Iolbn = Iolbn + 1
         Ioptr = Io + 1
      ENDIF
   ENDIF
   IF ( icount==Nwords ) THEN
      IF ( Itest==1 ) GOTO 300
      IF ( Itest==2 ) THEN
!
!     WRITE END OF GROUP.
!
         Buf(Ioptr) = ieog
      ELSEIF ( Itest==3 ) THEN
!
!     WRITE END OF ITEM, OUTPUT THE INPUT/OUTPUT BUFFER ON THE SOF, AND
!     UPDATE THE MDI.
!
         Buf(Ioptr) = ieoi
         CALL sofio(iwrt,Iopbn,Buf(Io-2))
         CALL fmdi(Iosind,imdi)
         Buf(imdi+Ioitcd) = Ioblk
         Buf(imdi+Ioitcd) = orf(andf(Buf(imdi+Ioitcd),Jhalf),lshift(Iolbn,Ihalf))
         Mdiup = .TRUE.
         Iomode = idle
      ELSE
         GOTO 200
      ENDIF
      Ioptr = Ioptr + 1
      GOTO 300
   ENDIF
 200  icount = icount + 1
   Buf(Ioptr) = Ia(icount)
   Ioptr = Ioptr + 1
   GOTO 100
 300  RETURN
99999 RETURN
END SUBROUTINE suwrt
