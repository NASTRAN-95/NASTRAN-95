!*==getblk.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE getblk(Iold,Inew)
   USE c_machin
   USE c_sof
   USE c_sofcom
   USE c_sys
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iold
   INTEGER :: Inew
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: btfree , filind , filnum , filsup , i , ind , index , last , left , lmask , lstsiz , max , nxtblk , tpfree
   INTEGER , SAVE :: indsbr , ird , iwrt
   INTEGER , DIMENSION(2) , SAVE :: nmsbr
   EXTERNAL andf , chkopn , errmkn , fnxt , lshift , orf , rshift , sofio
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     FINDS A FREE BLOCK INEW.  IF IOLD IS NOT ZERO IOLD POINTER WILL
!     BE SET TO INEW.
!
   DATA ird , iwrt/1 , 2/
   DATA indsbr/11/ , nmsbr/4HGETB , 4HLK  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     CHECK IF THE SUPERBLOCK NXTCUR HAS A FREE BLOCK.
!
         CALL chkopn(nmsbr(1))
         lmask = lshift(jhalf,ihalf)
         spag_nextblock_1 = 2
      CASE (2)
         IF ( nxtcur==nxtlbn ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     THE SUPERBLOCK NXTCUR IS NOT IN CORE.
!
         IF ( nxtlbn==0 ) THEN
            IF ( ditpbn/=0 ) THEN
!
!     THE IN CORE BUFFER SHARED BY THE DIT AND THE ARRAY NXT IS NOW
!     OCCUPIED BY A BLOCK OF DIT.
!
               IF ( ditup ) THEN
!
!     THE DIT BLOCK WHICH IS NOW IN CORE HAS BEEN UPDATED, MUST
!     THEREFORE WRITE IT OUT BEFORE READING IN A NEW BLOCK.
!
                  CALL sofio(iwrt,ditpbn,buf(dit-2))
                  ditup = .FALSE.
               ENDIF
               ditpbn = 0
               ditlbn = 0
            ENDIF
!
!     THE IN CORE BUFFER SHARED BY THE DIT AND THE ARRAY NXT IS NOW
!     OCCUPIED BY A BLOCK OF NXT.
!
         ELSEIF ( nxtup ) THEN
!
!     THE BLOCK OF THE ARRAY NXT WHICH IS NOW IN CORE HAS BEEN UPDATED,
!     MUST THEREFORE WRITE IT OUT BEFORE READING IN A NEW BLOCK.
!
            CALL sofio(iwrt,nxtpbn,buf(nxt-2))
            nxtup = .FALSE.
         ENDIF
!
!     READ INTO CORE THE DESIRED BLOCK OF THE ARRAY NXT.
!
         nxtlbn = nxtcur
         nxtpbn = 0
         left = nxtlbn
         DO i = 1 , nfiles
            IF ( left>nxtfsz(i) ) THEN
               nxtpbn = nxtpbn + filsiz(i)
               left = left - nxtfsz(i)
            ELSE
               filnum = i
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         CALL errmkn(indsbr,4)
         RETURN
      CASE (3)
         nxtpbn = nxtpbn + (left-1)*supsiz + 2
         CALL sofio(ird,nxtpbn,buf(nxt-2))
         spag_nextblock_1 = 4
      CASE (4)
!
!     CHECK THE FREE LIST OF SUPERBLOCK NXTCUR.
!
         tpfree = rshift(buf(nxt+1),ihalf)
         IF ( tpfree>0 ) THEN
!
!     SUPERBLOCK NXTCUR DOES HAVE A FREE BLOCK.
!
            Inew = tpfree
            avblks = avblks - 1
!
!     COMPUTE THE INDEX OF TPFREE ENTRY IN THE BLOCK OF ARRAY NXT
!     BELONGING TO SUPERBLOCK NXTCUR.
!
            filind = tpfree
            SPAG_Loop_1_1: DO i = 1 , nfiles
               IF ( filind<=filsiz(i) ) EXIT SPAG_Loop_1_1
               filind = filind - filsiz(i)
            ENDDO SPAG_Loop_1_1
            filsup = (filind-1)/supsiz
            IF ( filind-1/=filsup*supsiz ) filsup = filsup + 1
            index = (filind-(filsup-1)*supsiz)/2 + 1
            IF ( mod(tpfree,2)==1 ) THEN
!
!     TPFREE IS AN ODD INTEGER.  THE ENTRY FOR TPFREE IS THEREFORE
!     IN BITS 0 TO IHALF OF THE WORD.  SAVE TPFREE ENTRY IN NXTBLK
!     AND THEN SET IT TO ZERO.
!
               nxtblk = andf(buf(nxt+index),jhalf)
               buf(nxt+index) = andf(buf(nxt+index),lmask)
            ELSE
!
!     TPFREE IS AN EVEN INTEGER.  THE ENTRY FOR TPFREE IS THEREFORE
!     IN BITS (IHALF+1) TO (2*IHALF-1) OF THE WORD.  SAVE TPFREE ENTRY
!     IN NXTBLK AND THEN SET IT TO ZERO.
!
               nxtblk = rshift(buf(nxt+index),ihalf)
               buf(nxt+index) = andf(buf(nxt+index),jhalf)
            ENDIF
            btfree = andf(buf(nxt+1),jhalf)
            IF ( tpfree==btfree ) THEN
!
!     SET TPFREE AND BTFREE TO ZERO.
!
               buf(nxt+1) = 0
            ELSE
!
!     SET TPFREE TO NXTBLK.
!
               buf(nxt+1) = orf(andf(buf(nxt+1),jhalf),lshift(nxtblk,ihalf))
            ENDIF
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
!
!     THE SUPERBLOCK NXTCUR DOES NOT HAVE ANY FREE BLOCKS.
!
         ELSEIF ( nxtcur==nxttsz ) THEN
!
!     NXTCUR IS THE LAST SUPERBLOCK.
!
            IF ( nxtrst ) THEN
!
!     MUST START A BRAND NEW SUPERBLOCK.
!
               nxtrst = .FALSE.
               IF ( nxtup ) CALL sofio(iwrt,nxtpbn,buf(nxt-2))
               nxtup = .FALSE.
            ELSE
               nxtcur = 1
               nxtrst = .TRUE.
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
            nxtcur = nxtcur + 1
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         nxtcur = nxtcur + 1
         left = nxtcur
         DO i = 1 , nfiles
            IF ( left>nxtfsz(i) ) THEN
               left = left - nxtfsz(i)
            ELSE
               filnum = i
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         nxtcur = nxtcur - 1
         spag_nextblock_1 = 9
      CASE (6)
         last = nbuff - 4
         DO i = 1 , last
            buf(nxt+i) = 0
         ENDDO
         IF ( left==1 ) THEN
!
!     NXTCUR IS THE FIRST SUPERBLOCK ON FILE FILNUM.
!
            lstsiz = mod(filsiz(filnum-1)-2,supsiz) + 1
            nxtpbn = nxtpbn + lstsiz + 1
            avblks = avblks - 1
            IF ( filsiz(filnum)>=supsiz+1 ) THEN
               btfree = nxtpbn + supsiz - 1
            ELSE
               btfree = nxtpbn + filsiz(filnum) - 2
            ENDIF
         ELSE
!
!     NXTCUR IS NOT THE FIRST SUPERBLOCK ON FILE FILNUM.
!
            nxtpbn = nxtpbn + supsiz
            IF ( left/=nxtfsz(filnum) ) THEN
               btfree = nxtpbn + supsiz - 1
            ELSE
!
!     NXTCUR IS THE LAST BLOCK ON FILE FILNUM.
!
               lstsiz = mod(filsiz(filnum)-2,supsiz) + 1
               IF ( lstsiz<=1 ) THEN
!
!     THE SIZE OF THE LAST BLOCK ON FILE FILNUM IS EQUAL TO 1.
!     THERE ARE THEREFORE NO FREE BLOCKS AVAILABLE ON SUPERBLOCK NXTCUR.
!     SET TPFREE AND BTFREE OF NXTCUR EQUAL TO ZERO.
!
                  buf(nxt+1) = 0
                  avblks = avblks - 1
                  CALL sofio(iwrt,nxtpbn,buf(nxt-2))
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
!
!     THE SIZE OF SUPERBLOCK NXTCUR IS LARGER THAN 1.
!
               ELSEIF ( lstsiz>2 ) THEN
!
!     THE SIZE OF SUPERBLOCK NXTCUR IS LARGER THAN 2.
!
                  btfree = nxtpbn + lstsiz - 1
               ELSE
!
!     THE SIZE OF SUPERBLOCK NXTCUR IS EQUAL TO 2.  THERE IS THEREFORE
!     ONLY ONE FREE BLOCK IN NXTCUR.  SET TPFREE AND BTFREE TO ZERO.
!
                  buf(nxt+1) = 0
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDIF
!
!     INITIALIZE THE NEW SUPERBLOCK.
!
         tpfree = nxtpbn + 2
!
!     PUT THE VALUES OF BTFREE AND TPFREE IN THE FIRST WORD OF THE ARRAY
!     NXT BELONGING TO SUPERBLOCK NXTCUR.
!
         buf(nxt+1) = btfree
         buf(nxt+1) = orf(buf(nxt+1),lshift(tpfree,ihalf))
         IF ( mod(btfree,2)==1 ) THEN
!
!     BTFREE IS AN ODD INTEGER.
!
            max = (btfree-nxtpbn+1)/2
            buf(nxt+max+1) = lshift(btfree,ihalf)
         ELSE
!
!     BTFREE IS AN EVEN INTEGER.
!
            max = (btfree-nxtpbn+2)/2
            buf(nxt+max+1) = 0
         ENDIF
!
!     SET UP THE THREAD THROUGH THE BLOCKS OF SUPERBLOCK NXTCUR.
!
         IF ( max>=3 ) THEN
            DO i = 3 , max
               buf(nxt+i) = 2*i + nxtpbn - 2
               buf(nxt+i) = orf(buf(nxt+i),lshift(2*i+nxtpbn-3,ihalf))
            ENDDO
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
!
!     SETUP VARIABLES RELATED TO THE SUPERBLOCK NXTCUR.
!
         buf(nxt+2) = 0
         Inew = nxtpbn + 1
         avblks = avblks - 2
         nxtlbn = nxtcur
         nxttsz = nxtcur
         spag_nextblock_1 = 8
      CASE (8)
         IF ( Iold/=0 ) THEN
!
!     WANT TO SET IOLD POINTER TO INEW.
!
            nxtup = .TRUE.
            CALL fnxt(Iold,ind)
            IF ( mod(Iold,2)==1 ) THEN
!
!     IOLD IS AN ODD INTEGER
!
               buf(ind) = orf(andf(buf(ind),lmask),Inew)
            ELSE
!
!     IOLD IS AN EVEN INTEGER
!
               buf(ind) = orf(andf(buf(ind),jhalf),lshift(Inew,ihalf))
            ENDIF
         ENDIF
         nxtup = .TRUE.
         RETURN
      CASE (9)
!
!     ERROR MESSAGES.
!
         Inew = -1
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE getblk
