!*==getblk.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE getblk(Iold,Inew)
   IMPLICIT NONE
   USE C_MACHIN
   USE C_SOF
   USE C_SOFCOM
   USE C_SYS
   USE C_SYSTEM
   USE C_ZZZZZZ
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
         lmask = lshift(Jhalf,Ihalf)
         spag_nextblock_1 = 2
      CASE (2)
         IF ( Nxtcur==Nxtlbn ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
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
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         CALL errmkn(indsbr,4)
         RETURN
      CASE (3)
         Nxtpbn = Nxtpbn + (left-1)*Supsiz + 2
         CALL sofio(ird,Nxtpbn,Buf(Nxt-2))
         spag_nextblock_1 = 4
      CASE (4)
!
!     CHECK THE FREE LIST OF SUPERBLOCK NXTCUR.
!
         tpfree = rshift(Buf(Nxt+1),Ihalf)
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
            SPAG_Loop_1_1: DO i = 1 , Nfiles
               IF ( filind<=Filsiz(i) ) EXIT SPAG_Loop_1_1
               filind = filind - Filsiz(i)
            ENDDO SPAG_Loop_1_1
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
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
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
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
            Nxtcur = Nxtcur + 1
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         Nxtcur = Nxtcur + 1
         left = Nxtcur
         DO i = 1 , Nfiles
            IF ( left>Nxtfsz(i) ) THEN
               left = left - Nxtfsz(i)
            ELSE
               filnum = i
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         Nxtcur = Nxtcur - 1
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
      CASE (6)
         last = Nbuff - 4
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
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
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
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
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
         spag_nextblock_1 = 7
      CASE (7)
!
!     SETUP VARIABLES RELATED TO THE SUPERBLOCK NXTCUR.
!
         Buf(Nxt+2) = 0
         Inew = Nxtpbn + 1
         Avblks = Avblks - 2
         Nxtlbn = Nxtcur
         Nxttsz = Nxtcur
         spag_nextblock_1 = 8
      CASE (8)
         IF ( Iold/=0 ) THEN
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
      CASE (9)
!
!     ERROR MESSAGES.
!
         Inew = -1
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE getblk
