
SUBROUTINE getblk(Iold,Inew)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Avblks , Blksiz , Buf(1) , Dirsiz , Dit , Ditbl , Ditlbn , Ditnsb , Ditpbn , Ditsiz , Filsiz(10) , Ihalf , Iodum(8) ,    &
         & Jhalf , Mach , Mdidum(4) , Mdiup , Nbuff , Nfiles , Nxt , Nxtcur , Nxtfsz(10) , Nxtlbn , Nxtpbn , Nxttsz , Supsiz
   LOGICAL Ditup , Nxtrst , Nxtup
   REAL Filnam(10)
   COMMON /machin/ Mach , Ihalf , Jhalf
   COMMON /sof   / Dit , Ditpbn , Ditlbn , Ditsiz , Ditnsb , Ditbl , Iodum , Mdidum , Nxt , Nxtpbn , Nxtlbn , Nxttsz , Nxtfsz ,     &
                 & Nxtcur , Ditup , Mdiup , Nxtup , Nxtrst
   COMMON /sofcom/ Nfiles , Filnam , Filsiz
   COMMON /sys   / Blksiz , Dirsiz , Supsiz , Avblks
   COMMON /system/ Nbuff
   COMMON /zzzzzz/ Buf
!
! Dummy argument declarations
!
   INTEGER Inew , Iold
!
! Local variable declarations
!
   INTEGER andf , lshift , orf , rshift
   INTEGER btfree , filind , filnum , filsup , i , ind , index , indsbr , ird , iwrt , last , left , lmask , lstsiz , max , nmsbr(2)&
         & , nxtblk , tpfree
   EXTERNAL andf , lshift , orf , rshift
!
! End of declarations
!
!
!     FINDS A FREE BLOCK INEW.  IF IOLD IS NOT ZERO IOLD POINTER WILL
!     BE SET TO INEW.
!
   DATA ird , iwrt/1 , 2/
   DATA indsbr/11/ , nmsbr/4HGETB , 4HLK  /
!
!     CHECK IF THE SUPERBLOCK NXTCUR HAS A FREE BLOCK.
!
   CALL chkopn(nmsbr(1))
   lmask = lshift(Jhalf,Ihalf)
 100  IF ( Nxtcur==Nxtlbn ) GOTO 300
!
!     THE SUPERBLOCK NXTCUR IS NOT IN CORE.
!
   IF ( Nxtlbn==0 ) THEN
      IF ( Ditpbn/=0 ) THEN
!
!     THE IN CORE BUFFER SHARED BY THE DIT AND THE ARRAY NXT IS NOW
!     OCCUPIED BY A BLOCK OF DIT.
!
         IF ( Ditup ) THEN
!
!     THE DIT BLOCK WHICH IS NOW IN CORE HAS BEEN UPDATED, MUST
!     THEREFORE WRITE IT OUT BEFORE READING IN A NEW BLOCK.
!
            CALL sofio(iwrt,Ditpbn,Buf(Dit-2))
            Ditup = .FALSE.
         ENDIF
         Ditpbn = 0
         Ditlbn = 0
      ENDIF
!
!     THE IN CORE BUFFER SHARED BY THE DIT AND THE ARRAY NXT IS NOW
!     OCCUPIED BY A BLOCK OF NXT.
!
   ELSEIF ( Nxtup ) THEN
!
!     THE BLOCK OF THE ARRAY NXT WHICH IS NOW IN CORE HAS BEEN UPDATED,
!     MUST THEREFORE WRITE IT OUT BEFORE READING IN A NEW BLOCK.
!
      CALL sofio(iwrt,Nxtpbn,Buf(Nxt-2))
      Nxtup = .FALSE.
   ENDIF
!
!     READ INTO CORE THE DESIRED BLOCK OF THE ARRAY NXT.
!
   Nxtlbn = Nxtcur
   Nxtpbn = 0
   left = Nxtlbn
   DO i = 1 , Nfiles
      IF ( left>Nxtfsz(i) ) THEN
         Nxtpbn = Nxtpbn + Filsiz(i)
         left = left - Nxtfsz(i)
      ELSE
         filnum = i
         GOTO 200
      ENDIF
   ENDDO
   CALL errmkn(indsbr,4)
   GOTO 99999
 200  Nxtpbn = Nxtpbn + (left-1)*Supsiz + 2
   CALL sofio(ird,Nxtpbn,Buf(Nxt-2))
!
!     CHECK THE FREE LIST OF SUPERBLOCK NXTCUR.
!
 300  tpfree = rshift(Buf(Nxt+1),Ihalf)
   IF ( tpfree>0 ) THEN
!
!     SUPERBLOCK NXTCUR DOES HAVE A FREE BLOCK.
!
      Inew = tpfree
      Avblks = Avblks - 1
!
!     COMPUTE THE INDEX OF TPFREE ENTRY IN THE BLOCK OF ARRAY NXT
!     BELONGING TO SUPERBLOCK NXTCUR.
!
      filind = tpfree
      DO i = 1 , Nfiles
         IF ( filind<=Filsiz(i) ) EXIT
         filind = filind - Filsiz(i)
      ENDDO
      filsup = (filind-1)/Supsiz
      IF ( filind-1/=filsup*Supsiz ) filsup = filsup + 1
      index = (filind-(filsup-1)*Supsiz)/2 + 1
      IF ( mod(tpfree,2)==1 ) THEN
!
!     TPFREE IS AN ODD INTEGER.  THE ENTRY FOR TPFREE IS THEREFORE
!     IN BITS 0 TO IHALF OF THE WORD.  SAVE TPFREE ENTRY IN NXTBLK
!     AND THEN SET IT TO ZERO.
!
         nxtblk = andf(Buf(Nxt+index),Jhalf)
         Buf(Nxt+index) = andf(Buf(Nxt+index),lmask)
      ELSE
!
!     TPFREE IS AN EVEN INTEGER.  THE ENTRY FOR TPFREE IS THEREFORE
!     IN BITS (IHALF+1) TO (2*IHALF-1) OF THE WORD.  SAVE TPFREE ENTRY
!     IN NXTBLK AND THEN SET IT TO ZERO.
!
         nxtblk = rshift(Buf(Nxt+index),Ihalf)
         Buf(Nxt+index) = andf(Buf(Nxt+index),Jhalf)
      ENDIF
      btfree = andf(Buf(Nxt+1),Jhalf)
      IF ( tpfree==btfree ) THEN
!
!     SET TPFREE AND BTFREE TO ZERO.
!
         Buf(Nxt+1) = 0
      ELSE
!
!     SET TPFREE TO NXTBLK.
!
         Buf(Nxt+1) = orf(andf(Buf(Nxt+1),Jhalf),lshift(nxtblk,Ihalf))
      ENDIF
      GOTO 700
!
!     THE SUPERBLOCK NXTCUR DOES NOT HAVE ANY FREE BLOCKS.
!
   ELSEIF ( Nxtcur==Nxttsz ) THEN
!
!     NXTCUR IS THE LAST SUPERBLOCK.
!
      IF ( Nxtrst ) THEN
!
!     MUST START A BRAND NEW SUPERBLOCK.
!
         Nxtrst = .FALSE.
         IF ( Nxtup ) CALL sofio(iwrt,Nxtpbn,Buf(Nxt-2))
         Nxtup = .FALSE.
      ELSE
         Nxtcur = 1
         Nxtrst = .TRUE.
         GOTO 100
      ENDIF
   ELSE
      Nxtcur = Nxtcur + 1
      GOTO 100
   ENDIF
 400  Nxtcur = Nxtcur + 1
   left = Nxtcur
   DO i = 1 , Nfiles
      IF ( left>Nxtfsz(i) ) THEN
         left = left - Nxtfsz(i)
      ELSE
         filnum = i
         GOTO 500
      ENDIF
   ENDDO
   Nxtcur = Nxtcur - 1
   GOTO 800
 500  last = Nbuff - 4
   DO i = 1 , last
      Buf(Nxt+i) = 0
   ENDDO
   IF ( left==1 ) THEN
!
!     NXTCUR IS THE FIRST SUPERBLOCK ON FILE FILNUM.
!
      lstsiz = mod(Filsiz(filnum-1)-2,Supsiz) + 1
      Nxtpbn = Nxtpbn + lstsiz + 1
      Avblks = Avblks - 1
      IF ( Filsiz(filnum)>=Supsiz+1 ) THEN
         btfree = Nxtpbn + Supsiz - 1
      ELSE
         btfree = Nxtpbn + Filsiz(filnum) - 2
      ENDIF
   ELSE
!
!     NXTCUR IS NOT THE FIRST SUPERBLOCK ON FILE FILNUM.
!
      Nxtpbn = Nxtpbn + Supsiz
      IF ( left/=Nxtfsz(filnum) ) THEN
         btfree = Nxtpbn + Supsiz - 1
      ELSE
!
!     NXTCUR IS THE LAST BLOCK ON FILE FILNUM.
!
         lstsiz = mod(Filsiz(filnum)-2,Supsiz) + 1
         IF ( lstsiz<=1 ) THEN
!
!     THE SIZE OF THE LAST BLOCK ON FILE FILNUM IS EQUAL TO 1.
!     THERE ARE THEREFORE NO FREE BLOCKS AVAILABLE ON SUPERBLOCK NXTCUR.
!     SET TPFREE AND BTFREE OF NXTCUR EQUAL TO ZERO.
!
            Buf(Nxt+1) = 0
            Avblks = Avblks - 1
            CALL sofio(iwrt,Nxtpbn,Buf(Nxt-2))
            GOTO 400
!
!     THE SIZE OF SUPERBLOCK NXTCUR IS LARGER THAN 1.
!
         ELSEIF ( lstsiz>2 ) THEN
!
!     THE SIZE OF SUPERBLOCK NXTCUR IS LARGER THAN 2.
!
            btfree = Nxtpbn + lstsiz - 1
         ELSE
!
!     THE SIZE OF SUPERBLOCK NXTCUR IS EQUAL TO 2.  THERE IS THEREFORE
!     ONLY ONE FREE BLOCK IN NXTCUR.  SET TPFREE AND BTFREE TO ZERO.
!
            Buf(Nxt+1) = 0
            GOTO 600
         ENDIF
      ENDIF
   ENDIF
!
!     INITIALIZE THE NEW SUPERBLOCK.
!
   tpfree = Nxtpbn + 2
!
!     PUT THE VALUES OF BTFREE AND TPFREE IN THE FIRST WORD OF THE ARRAY
!     NXT BELONGING TO SUPERBLOCK NXTCUR.
!
   Buf(Nxt+1) = btfree
   Buf(Nxt+1) = orf(Buf(Nxt+1),lshift(tpfree,Ihalf))
   IF ( mod(btfree,2)==1 ) THEN
!
!     BTFREE IS AN ODD INTEGER.
!
      max = (btfree-Nxtpbn+1)/2
      Buf(Nxt+max+1) = lshift(btfree,Ihalf)
   ELSE
!
!     BTFREE IS AN EVEN INTEGER.
!
      max = (btfree-Nxtpbn+2)/2
      Buf(Nxt+max+1) = 0
   ENDIF
!
!     SET UP THE THREAD THROUGH THE BLOCKS OF SUPERBLOCK NXTCUR.
!
   IF ( max>=3 ) THEN
      DO i = 3 , max
         Buf(Nxt+i) = 2*i + Nxtpbn - 2
         Buf(Nxt+i) = orf(Buf(Nxt+i),lshift(2*i+Nxtpbn-3,Ihalf))
      ENDDO
   ENDIF
!
!     SETUP VARIABLES RELATED TO THE SUPERBLOCK NXTCUR.
!
 600  Buf(Nxt+2) = 0
   Inew = Nxtpbn + 1
   Avblks = Avblks - 2
   Nxtlbn = Nxtcur
   Nxttsz = Nxtcur
 700  IF ( Iold/=0 ) THEN
!
!     WANT TO SET IOLD POINTER TO INEW.
!
      Nxtup = .TRUE.
      CALL fnxt(Iold,ind)
      IF ( mod(Iold,2)==1 ) THEN
!
!     IOLD IS AN ODD INTEGER
!
         Buf(ind) = orf(andf(Buf(ind),lmask),Inew)
      ELSE
!
!     IOLD IS AN EVEN INTEGER
!
         Buf(ind) = orf(andf(Buf(ind),Jhalf),lshift(Inew,Ihalf))
      ENDIF
   ENDIF
   Nxtup = .TRUE.
   RETURN
!
!     ERROR MESSAGES.
!
 800  Inew = -1
   RETURN
99999 END SUBROUTINE getblk
