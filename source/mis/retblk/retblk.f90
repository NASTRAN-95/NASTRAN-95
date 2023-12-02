!*==retblk.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE retblk(Ibl)
!
!     RETURNS BLOCK I AND ALL BLOCKS LINKED TO IT TO THE LIST OF FREE
!     BLOCKS IN THE SUPERBLOCK TO WHICH BLOCK I BELONGS   IF SOME OF
!     THE BLOCKS THAT ARE LINKED TO BLOCK I DO NOT BELONG TO THE SAME
!     SUPERBLOCK, THEY ARE RETURNED TO THE FREE LIST OF THEIR OWN
!     RESPECTIVE SUPERBLOCKS.
!
   IMPLICIT NONE
   USE C_MACHIN
   USE C_SOF
   USE C_SOFCOM
   USE C_SYS
   USE C_ZZZZZZ
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
            lmask = lshift(Jhalf,Ihalf)
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
         DO l = 1 , Nfiles
            IF ( left>Filsiz(l) ) THEN
               left = left - Filsiz(l)
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
               IF ( j==0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               ind = (j-Nxtpbn+2)/2 + 1
               IF ( mod(j,2)==1 ) THEN
                  j = andf(Buf(Nxt+ind),Jhalf)
               ELSE
                  j = rshift(Buf(Nxt+ind),Ihalf)
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
         Buf(Nxt+1) = lshift(i,Ihalf)
!
!     EXAMINE THE BLOCKS THAT ARE LINKED TO BLOCK I.
!
         repeat = .FALSE.
         IF ( filsup/=Nxtfsz(filnum) ) THEN
            lstblk = Nxtpbn + Supsiz - 1
         ELSE
            lstblk = Nxtpbn + Filsiz(filnum) - (filsup-1)*Supsiz - 2
         ENDIF
         SPAG_Loop_1_1: DO
            Avblks = Avblks + 1
            ind = (i-Nxtpbn+2)/2 + 1
            IF ( mod(i,2)==1 ) THEN
               isv = andf(Buf(Nxt+ind),Jhalf)
            ELSE
               isv = rshift(Buf(Nxt+ind),Ihalf)
            ENDIF
            IF ( isv==0 ) EXIT SPAG_Loop_1_1
            IF ( isv<Nxtpbn .OR. isv>lstblk ) THEN
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
