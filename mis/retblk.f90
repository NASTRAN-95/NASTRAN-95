
SUBROUTINE retblk(Ibl)
!
!     RETURNS BLOCK I AND ALL BLOCKS LINKED TO IT TO THE LIST OF FREE
!     BLOCKS IN THE SUPERBLOCK TO WHICH BLOCK I BELONGS   IF SOME OF
!     THE BLOCKS THAT ARE LINKED TO BLOCK I DO NOT BELONG TO THE SAME
!     SUPERBLOCK, THEY ARE RETURNED TO THE FREE LIST OF THEIR OWN
!     RESPECTIVE SUPERBLOCKS.
!
   IMPLICIT NONE
   INTEGER Avblks , Blksiz , Buf(1) , Dirsiz , Dit , Ditbl , Ditlbn , Ditnsb , Ditpbn , Ditsiz , Filnam(10) , Filsiz(10) , Ihalf ,  &
         & Iodum(8) , Jhalf , Mach , Mdidum(4) , Mdiup , Nfiles , Nxt , Nxtcur , Nxtfsz(10) , Nxtlbn , Nxtpbn , Nxttsz , Supsiz
   LOGICAL Ditup , Nxtup
   COMMON /machin/ Mach , Ihalf , Jhalf
   COMMON /sof   / Dit , Ditpbn , Ditlbn , Ditsiz , Ditnsb , Ditbl , Iodum , Mdidum , Nxt , Nxtpbn , Nxtlbn , Nxttsz , Nxtfsz ,     &
                 & Nxtcur , Ditup , Mdiup , Nxtup
   COMMON /sofcom/ Nfiles , Filnam , Filsiz
   COMMON /sys   / Blksiz , Dirsiz , Supsiz , Avblks
   COMMON /zzzzzz/ Buf
   INTEGER Ibl
   INTEGER andf , lshift , orf , rshift
   INTEGER btfree , filind , filnum , filsup , i , ilbn , ind , indsbr , ird , isv , iwrt , j , l , left , lmask , lstblk , max ,   &
         & nmsbr(2) , tpfree
   LOGICAL repeat
   EXTERNAL andf , lshift , orf , rshift
   DATA ird , iwrt/1 , 2/
   DATA indsbr/13/ , nmsbr/4HRETB , 4HLK  /
!
   CALL chkopn(nmsbr(1))
   i = Ibl
   IF ( i<=0 ) THEN
!
!     ERROR MESSAGE.
!
      CALL errmkn(indsbr,2)
      GOTO 99999
   ELSE
      lmask = lshift(Jhalf,Ihalf)
   ENDIF
!
!     COMPUTE THE NUMBER OF THE FILE TO WHICH BLOCK I BELONGS,
!     THE INDEX OF BLOCK I WITHIN THAT FILE, THE NUMBER WITHIN THE
!     FILE OF THE SUPERBLOCK TO WHICH BLOCK I BELONGS, AND THE LOGICAL
!     BLOCK NUMBER OVER THE SYSTEM OF THAT SUPERBLOCK.
!
 100  left = i
   DO l = 1 , Nfiles
      IF ( left>Filsiz(l) ) THEN
         left = left - Filsiz(l)
      ELSE
         filnum = l
         GOTO 200
      ENDIF
   ENDDO
   CALL errmkn(indsbr,2)
   GOTO 99999
 200  filind = left
   filsup = (filind-1)/Supsiz
   IF ( filind-1/=filsup*Supsiz ) filsup = filsup + 1
   ilbn = 0
   max = filnum - 1
   IF ( max>=1 ) THEN
      DO l = 1 , max
         ilbn = ilbn + Nxtfsz(l)
      ENDDO
   ENDIF
   ilbn = ilbn + filsup
   IF ( ilbn/=Nxtlbn ) THEN
!
!     THE DESIRED BLOCK OF THE ARRAY NXT IS NOT IN CORE.
!
      IF ( Nxtlbn==0 ) THEN
!
!     THE IN CORE BUFFER SHARED BY THE DIT AND THE ARRAY NXT IS NOW
!     OCCUPIED BY A BLOCK OF THE DIT.  IF THAT BLOCK HAS BEEN UPDATED,
!     MUST WRITE IT OUT BEFORE READING IN THE NEW BLOCK.
!
         IF ( Ditup ) THEN
            CALL sofio(iwrt,Ditpbn,Buf(Dit-2))
            Ditup = .FALSE.
         ENDIF
         Ditpbn = 0
         Ditlbn = 0
!
!     THE IN CORE BUFFER SHARED BY THE DIT AND THE ARRAY NXT IS NOW
!     OCCUPIED BY A BLOCK OF NXT.  IF THAT BLOCK HAS BEEN UPDATED,
!     MUST WRITE IT OUT BEFORE READING IN THE NEW BLOCK.
!
      ELSEIF ( Nxtup ) THEN
         CALL sofio(iwrt,Nxtpbn,Buf(Nxt-2))
         Nxtup = .FALSE.
      ENDIF
!
!     READ IN THE DESIRED BLOCK OF NXT.
!
      Nxtlbn = ilbn
      Nxtpbn = 0
      max = filnum - 1
      IF ( max>=1 ) THEN
         DO l = 1 , max
            Nxtpbn = Nxtpbn + Filsiz(l)
         ENDDO
      ENDIF
      Nxtpbn = Nxtpbn + (filsup-1)*Supsiz + 2
      CALL sofio(ird,Nxtpbn,Buf(Nxt-2))
   ENDIF
!
!     THE DESIRED BLOCK OF NXT IS IN CORE.
!
   btfree = andf(Buf(Nxt+1),Jhalf)
   tpfree = rshift(Buf(Nxt+1),Ihalf)
   IF ( btfree/=0 ) THEN
!
!     CHECK IF BLOCK I IS ALREADY IN THE LIST OF FREE BLOCKS.
!
      j = tpfree
      DO WHILE ( j/=i )
         IF ( j==0 ) GOTO 300
         ind = (j-Nxtpbn+2)/2 + 1
         IF ( mod(j,2)==1 ) THEN
            j = andf(Buf(Nxt+ind),Jhalf)
         ELSE
            j = rshift(Buf(Nxt+ind),Ihalf)
         ENDIF
      ENDDO
      GOTO 400
   ENDIF
!
!     BLOCK I IS NOT IN THE LIST OF FREE BLOCKS.
!     SET TPFREE TO I
!
 300  Buf(Nxt+1) = lshift(i,Ihalf)
!
!     EXAMINE THE BLOCKS THAT ARE LINKED TO BLOCK I.
!
   repeat = .FALSE.
   IF ( filsup/=Nxtfsz(filnum) ) THEN
      lstblk = Nxtpbn + Supsiz - 1
   ELSE
      lstblk = Nxtpbn + Filsiz(filnum) - (filsup-1)*Supsiz - 2
   ENDIF
   DO
      Avblks = Avblks + 1
      ind = (i-Nxtpbn+2)/2 + 1
      IF ( mod(i,2)==1 ) THEN
         isv = andf(Buf(Nxt+ind),Jhalf)
      ELSE
         isv = rshift(Buf(Nxt+ind),Ihalf)
      ENDIF
      IF ( isv==0 ) EXIT
      IF ( isv<Nxtpbn .OR. isv>lstblk ) THEN
         repeat = .TRUE.
         EXIT
      ELSE
         i = isv
      ENDIF
   ENDDO
!
!     ALL THE BLOCKS IN THIS SUPERBLOCK HAVE BEEN FOUND.
!     SET POINTER OF I TO VALUE OF OLD TPFREE.
!
   IF ( mod(i,2)==1 ) THEN
      Buf(Nxt+ind) = orf(andf(Buf(Nxt+ind),lmask),tpfree)
   ELSE
      Buf(Nxt+ind) = orf(andf(Buf(Nxt+ind),Jhalf),lshift(tpfree,Ihalf))
   ENDIF
   IF ( btfree==0 ) btfree = i
!
!     SET BTFREE TO LAST BLOCK IN CHAIN.
!
   Buf(Nxt+1) = orf(andf(Buf(Nxt+1),lmask),btfree)
   Nxtup = .TRUE.
   IF ( repeat ) THEN
!
!     ISV BELONGS TO A DIFFERENT SUPERBLOCK, REPEAT
!     SUBROUTINE FOR BLOCK ISV.
!
      i = isv
      GOTO 100
   ENDIF
!
!     NO MORE BLOCKS LINKED TO BLOCK I, RETURN.
!
 400  RETURN
99999 RETURN
END SUBROUTINE retblk