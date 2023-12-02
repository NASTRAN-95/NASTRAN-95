!*==retblk.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE retblk(Ibl)
!
!     RETURNS BLOCK I AND ALL BLOCKS LINKED TO IT TO THE LIST OF FREE
!     BLOCKS IN THE SUPERBLOCK TO WHICH BLOCK I BELONGS   IF SOME OF
!     THE BLOCKS THAT ARE LINKED TO BLOCK I DO NOT BELONG TO THE SAME
!     SUPERBLOCK, THEY ARE RETURNED TO THE FREE LIST OF THEIR OWN
!     RESPECTIVE SUPERBLOCKS.
!
   USE c_machin
   USE c_sof
   USE c_sofcom
   USE c_sys
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ibl
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: btfree , filind , filnum , filsup , i , ilbn , ind , isv , j , l , left , lmask , lstblk , max , tpfree
   INTEGER , SAVE :: indsbr , ird , iwrt
   INTEGER , DIMENSION(2) , SAVE :: nmsbr
   LOGICAL :: repeat
   EXTERNAL andf , chkopn , errmkn , lshift , orf , rshift , sofio
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   DATA ird , iwrt/1 , 2/
   DATA indsbr/13/ , nmsbr/4HRETB , 4HLK  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         CALL chkopn(nmsbr(1))
         i = Ibl
         IF ( i<=0 ) THEN
!
!     ERROR MESSAGE.
!
            CALL errmkn(indsbr,2)
            RETURN
         ELSE
            lmask = lshift(jhalf,ihalf)
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     COMPUTE THE NUMBER OF THE FILE TO WHICH BLOCK I BELONGS,
!     THE INDEX OF BLOCK I WITHIN THAT FILE, THE NUMBER WITHIN THE
!     FILE OF THE SUPERBLOCK TO WHICH BLOCK I BELONGS, AND THE LOGICAL
!     BLOCK NUMBER OVER THE SYSTEM OF THAT SUPERBLOCK.
!
         left = i
         DO l = 1 , nfiles
            IF ( left>filsiz(l) ) THEN
               left = left - filsiz(l)
            ELSE
               filnum = l
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         CALL errmkn(indsbr,2)
         RETURN
      CASE (3)
         filind = left
         filsup = (filind-1)/supsiz
         IF ( filind-1/=filsup*supsiz ) filsup = filsup + 1
         ilbn = 0
         max = filnum - 1
         IF ( max>=1 ) THEN
            DO l = 1 , max
               ilbn = ilbn + nxtfsz(l)
            ENDDO
         ENDIF
         ilbn = ilbn + filsup
         IF ( ilbn/=nxtlbn ) THEN
!
!     THE DESIRED BLOCK OF THE ARRAY NXT IS NOT IN CORE.
!
            IF ( nxtlbn==0 ) THEN
!
!     THE IN CORE BUFFER SHARED BY THE DIT AND THE ARRAY NXT IS NOW
!     OCCUPIED BY A BLOCK OF THE DIT.  IF THAT BLOCK HAS BEEN UPDATED,
!     MUST WRITE IT OUT BEFORE READING IN THE NEW BLOCK.
!
               IF ( ditup ) THEN
                  CALL sofio(iwrt,ditpbn,buf(dit-2))
                  ditup = .FALSE.
               ENDIF
               ditpbn = 0
               ditlbn = 0
!
!     THE IN CORE BUFFER SHARED BY THE DIT AND THE ARRAY NXT IS NOW
!     OCCUPIED BY A BLOCK OF NXT.  IF THAT BLOCK HAS BEEN UPDATED,
!     MUST WRITE IT OUT BEFORE READING IN THE NEW BLOCK.
!
            ELSEIF ( nxtup ) THEN
               CALL sofio(iwrt,nxtpbn,buf(nxt-2))
               nxtup = .FALSE.
            ENDIF
!
!     READ IN THE DESIRED BLOCK OF NXT.
!
            nxtlbn = ilbn
            nxtpbn = 0
            max = filnum - 1
            IF ( max>=1 ) THEN
               DO l = 1 , max
                  nxtpbn = nxtpbn + filsiz(l)
               ENDDO
            ENDIF
            nxtpbn = nxtpbn + (filsup-1)*supsiz + 2
            CALL sofio(ird,nxtpbn,buf(nxt-2))
         ENDIF
!
!     THE DESIRED BLOCK OF NXT IS IN CORE.
!
         btfree = andf(buf(nxt+1),jhalf)
         tpfree = rshift(buf(nxt+1),ihalf)
         IF ( btfree/=0 ) THEN
!
!     CHECK IF BLOCK I IS ALREADY IN THE LIST OF FREE BLOCKS.
!
            j = tpfree
            DO WHILE ( j/=i )
               IF ( j==0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               ind = (j-nxtpbn+2)/2 + 1
               IF ( mod(j,2)==1 ) THEN
                  j = andf(buf(nxt+ind),jhalf)
               ELSE
                  j = rshift(buf(nxt+ind),ihalf)
               ENDIF
            ENDDO
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
!
!     BLOCK I IS NOT IN THE LIST OF FREE BLOCKS.
!     SET TPFREE TO I
!
         buf(nxt+1) = lshift(i,ihalf)
!
!     EXAMINE THE BLOCKS THAT ARE LINKED TO BLOCK I.
!
         repeat = .FALSE.
         IF ( filsup/=nxtfsz(filnum) ) THEN
            lstblk = nxtpbn + supsiz - 1
         ELSE
            lstblk = nxtpbn + filsiz(filnum) - (filsup-1)*supsiz - 2
         ENDIF
         SPAG_Loop_1_1: DO
            avblks = avblks + 1
            ind = (i-nxtpbn+2)/2 + 1
            IF ( mod(i,2)==1 ) THEN
               isv = andf(buf(nxt+ind),jhalf)
            ELSE
               isv = rshift(buf(nxt+ind),ihalf)
            ENDIF
            IF ( isv==0 ) EXIT SPAG_Loop_1_1
            IF ( isv<nxtpbn .OR. isv>lstblk ) THEN
               repeat = .TRUE.
               EXIT SPAG_Loop_1_1
            ELSE
               i = isv
            ENDIF
         ENDDO SPAG_Loop_1_1
!
!     ALL THE BLOCKS IN THIS SUPERBLOCK HAVE BEEN FOUND.
!     SET POINTER OF I TO VALUE OF OLD TPFREE.
!
         IF ( mod(i,2)==1 ) THEN
            buf(nxt+ind) = orf(andf(buf(nxt+ind),lmask),tpfree)
         ELSE
            buf(nxt+ind) = orf(andf(buf(nxt+ind),jhalf),lshift(tpfree,ihalf))
         ENDIF
         IF ( btfree==0 ) btfree = i
!
!     SET BTFREE TO LAST BLOCK IN CHAIN.
!
         buf(nxt+1) = orf(andf(buf(nxt+1),lmask),btfree)
         nxtup = .TRUE.
         IF ( repeat ) THEN
!
!     ISV BELONGS TO A DIFFERENT SUPERBLOCK, REPEAT
!     SUBROUTINE FOR BLOCK ISV.
!
            i = isv
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
!
!     NO MORE BLOCKS LINKED TO BLOCK I, RETURN.
!
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE retblk
