!*==mxcid.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mxcid(Z,Mset,Msze,Nwds,Uset,Gpl,Sil,Buf1) !HIDESTARS (*,Z,Mset,Msze,Nwds,Uset,Gpl,Sil,Buf1)
!
!     THIS SUBROUTINE CREATES AN ARRAY AT Z(1) OF LENGTH MSZE*NWDS
!     WHICH CONTAINS THE EXTERNAL ID*10 + COMPONENT AT Z(1,M) FOR
!     EACH DEGREE OF FREEDOM BELONGING TO SET -MSET-.
!
!     OPEN CORE IS Z(1) TO Z(BUF1-1).   TWO  BUFFERS NEEDED.
!
!     NONSTANDARD RETURN IF TASK NOT COMPLETED.
!
!     IF THIS IS A SUBSTRUCTURING PROBLEM, MXCIDS SHOULD BE CALLED
!     INSTEAD
!
   USE c_bitpos
   USE c_names
   USE c_system
   USE c_two
   USE c_xmssg
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Z
   INTEGER :: Mset
   INTEGER :: Msze
   INTEGER :: Nwds
   INTEGER :: Uset
   INTEGER :: Gpl
   INTEGER :: Sil
   INTEGER :: Buf1
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: buf2 , i , iset , j , k , l , lgp , ll , lll , lsil , luset , mcount , ndf , ndof , ngp , psil , puset , sil1 , sil2
   INTEGER , DIMENSION(2) :: fnam
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , SAVE :: none , nset
   INTEGER , DIMENSION(7) :: x
   EXTERNAL andf , close , fname , fread , gopen , lshift , open , orf , page2 , rdtrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   DATA nset/20/
   DATA name , none/4HMXCI , 4HD    , 4H (NO/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     ALLOCATE CORE - CHECK DATA FILE AVAILABILITY
!
         buf2 = Buf1 + nbufsz
         IF ( Nwds<=0 ) Nwds = 1
         lgp = Msze*Nwds + 1
         x(1) = Sil
         CALL fname(Sil,fnam)
         IF ( fnam(1)/=none ) THEN
            CALL rdtrl(x)
            ngp = x(2)
            lsil = lgp + ngp
!
!     SEVEN WORDS NEEDED IF SIL AND USET OUT OF CORE
!
            IF ( lsil>Buf1-7 ) THEN
!
!     INSUFFICIENT CORE
!
               CALL page2(2)
               WRITE (koutp,99001) swm , name
99001          FORMAT (A27,' 3008, INSUFFICIENT CORE AVAILABLE FOR SUBROUTINE ',2A4,1H.)
               spag_nextblock_1 = 3
            ELSE
!
!     DETERMINE IF SIL (AND USET) FIT IN CORE
!
               luset = lsil + ngp
               x(1) = Uset
               CALL fname(Uset,fnam)
               IF ( fnam(1)==none ) GOTO 20
               CALL rdtrl(x)
               ndof = x(3)
               l = orf(lshift(x(4),16),x(5))
!
               IF ( luset+ndof>Buf1 ) luset = 0
               IF ( luset>Buf1 ) lsil = 0
!
!     CHECK SET REQUEST
!
               DO iset = 1 , nset
                  IF ( hset(iset)==Mset ) GOTO 5
               ENDDO
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
 5             iset = mask2(iset)
               iset = itwo(iset)
               IF ( andf(l,iset)==0 ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!
!     LOAD GPL INTO CORE
!
               x(1) = Gpl
               CALL open(*20,Gpl,Z(buf2),krdrw)
               CALL fread(Gpl,0,0,1)
               CALL fread(Gpl,Z(lgp),ngp,0)
               CALL close(Gpl,kcl)
               x(1) = Sil
               CALL gopen(Sil,Z(Buf1),krdrw)
               CALL gopen(Uset,Z(buf2),krdrw)
!
!     LOAD SIL AND USET IF POSSIBLE
!
               IF ( lsil==0 ) THEN
                  CALL fread(Sil,sil1,1,0)
                  i = 1
                  psil = lgp + ngp
               ELSE
                  CALL fread(Sil,Z(lsil),ngp,0)
                  CALL close(Sil,kcl)
                  sil1 = Z(lsil)
                  psil = lsil + 1
                  i = ngp - 1
               ENDIF
               IF ( luset/=0 ) THEN
                  CALL fread(Uset,Z(luset),ndof,0)
                  CALL close(Uset,kcl)
                  puset = luset
               ENDIF
               IF ( luset==0 ) puset = psil + i
!
!     PSIL POINTS SECOND SIL ENTRY IF SIL IN CORE, ELSE LOCATION TO USE
!     PUSET POINTS TO FIRST WORD USET, ELSE LOCATION IN Z TO USE
!     LSIL, LUSET ARE ZERO IF FILES NOT IN CORE.
!     LOOP ON NUMBER GRID POINTS - EXIT WHEN MSIZE ACHIEVED.
!
               mcount = 1
!
               DO lll = 1 , ngp
                  spag_nextblock_2 = 1
                  SPAG_DispatchLoop_2: DO
                     SELECT CASE (spag_nextblock_2)
                     CASE (1)
                        IF ( lll==ngp ) THEN
                           sil2 = ndof + 1
                        ELSE
                           IF ( lsil==0 ) CALL fread(Sil,Z(psil),1,0)
                           sil2 = Z(psil)
                           IF ( lsil/=0 ) psil = psil + 1
                        ENDIF
                        ndf = sil2 - sil1
                        IF ( ndf<1 .OR. ndf>6 ) THEN
                           spag_nextblock_1 = 2
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
!
!     GET NDF WORDS FROM USET
!
                        IF ( luset==0 ) CALL fread(Uset,Z(puset),ndf,0)
!
!     DETERMINE IF IN THE SET
!
                        j = puset
                        k = j + ndf - 1
                        spag_nextblock_2 = 2
                     CASE (2)
                        DO i = j , k
                           IF ( andf(Z(i),iset)/=0 ) THEN
                              spag_nextblock_2 = 3
                              CYCLE SPAG_DispatchLoop_2
                           ENDIF
                        ENDDO
                        spag_nextblock_2 = 4
                     CASE (3)
!
!     LOCATED A POINT IN THE SET
!
                        ll = i - puset + 1
                        l = lgp + lll - 1
                        IF ( ndf==1 ) ll = 0
                        Z(mcount) = Z(l)*10 + ll
                        mcount = mcount + Nwds
                        IF ( mcount>=lgp ) THEN
                           spag_nextblock_1 = 4
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        IF ( i/=k ) THEN
                           j = i + 1
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                        spag_nextblock_2 = 4
                     CASE (4)
                        IF ( luset/=0 ) puset = puset + ndf
                        sil1 = sil2
                        EXIT SPAG_DispatchLoop_2
                     END SELECT
                  ENDDO SPAG_DispatchLoop_2
               ENDDO
!
!     END OF ALL GRIDS AND MATRIX NOT FILLED - NEED IMMEDIATE MESSAGE.
!
               CALL page2(2)
               WRITE (koutp,99002) swm , name
99002          FORMAT (A27,' 3016, MATRIX IS NOT IN PROPER FORM IN SUBROUTINE ',2A4)
               spag_nextblock_1 = 3
            ENDIF
            CYCLE
         ENDIF
!
!     PURGED FILES
!
 20      CALL page2(2)
         WRITE (koutp,99003) swm , x(1) , name
99003    FORMAT (A27,' 3001, ATTEMPT TO OPEN DATA SET',I4,' IN SUBROUTINE',1X,2A4,' WHICH WAS NOT DEFINED IN FIST')
         spag_nextblock_1 = 3
      CASE (2)
!
!     ILLEGAL INPUT
!
         CALL page2(2)
         WRITE (koutp,99004) swm , name
99004    FORMAT (A27,' 3007, ILLEGAL INPUT TO SUBROUTINE ',2A4)
         spag_nextblock_1 = 3
      CASE (3)
!
         CALL close(Sil,kcl)
         CALL close(Uset,kcl)
         RETURN 1
      CASE (4)
         CALL close(Sil,kcl)
         CALL close(Uset,kcl)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE mxcid
