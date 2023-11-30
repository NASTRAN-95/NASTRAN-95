
SUBROUTINE mxcid(*,Z,Mset,Msze,Nwds,Uset,Gpl,Sil,Buf1)
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
   IMPLICIT NONE
   INTEGER Hset(32) , Itwo(32) , Kcl , Kclrw , Koutp , Krd , Krdrw , Kweof , Kwr , Kwrrw , Mask2(32) , Nbufsz
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /bitpos/ Mask2 , Hset
   COMMON /names / Krd , Krdrw , Kwr , Kwrrw , Kclrw , Kcl , Kweof
   COMMON /system/ Nbufsz , Koutp
   COMMON /two   / Itwo
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   INTEGER Buf1 , Gpl , Mset , Msze , Nwds , Sil , Uset
   INTEGER Z(1)
   INTEGER andf , lshift , orf
   INTEGER buf2 , fnam(2) , i , iset , j , k , l , lgp , ll , lll , lsil , luset , mcount , name(2) , ndf , ndof , ngp , none ,     &
         & nset , psil , puset , sil1 , sil2 , x(7)
   EXTERNAL andf , lshift , orf
   DATA nset/20/
   DATA name , none/4HMXCI , 4HD    , 4H (NO/
!
!     ALLOCATE CORE - CHECK DATA FILE AVAILABILITY
!
   buf2 = Buf1 + Nbufsz
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
         WRITE (Koutp,99001) Swm , name
99001    FORMAT (A27,' 3008, INSUFFICIENT CORE AVAILABLE FOR SUBROUTINE ',2A4,1H.)
         GOTO 300
      ELSE
!
!     DETERMINE IF SIL (AND USET) FIT IN CORE
!
         luset = lsil + ngp
         x(1) = Uset
         CALL fname(Uset,fnam)
         IF ( fnam(1)==none ) GOTO 100
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
            IF ( Hset(iset)==Mset ) GOTO 20
         ENDDO
         GOTO 200
 20      iset = Mask2(iset)
         iset = Itwo(iset)
         IF ( andf(l,iset)==0 ) GOTO 200
!
!     LOAD GPL INTO CORE
!
         x(1) = Gpl
         CALL open(*100,Gpl,Z(buf2),Krdrw)
         CALL fread(Gpl,0,0,1)
         CALL fread(Gpl,Z(lgp),ngp,0)
         CALL close(Gpl,Kcl)
         x(1) = Sil
         CALL gopen(Sil,Z(Buf1),Krdrw)
         CALL gopen(Uset,Z(buf2),Krdrw)
!
!     LOAD SIL AND USET IF POSSIBLE
!
         IF ( lsil==0 ) THEN
            CALL fread(Sil,sil1,1,0)
            i = 1
            psil = lgp + ngp
         ELSE
            CALL fread(Sil,Z(lsil),ngp,0)
            CALL close(Sil,Kcl)
            sil1 = Z(lsil)
            psil = lsil + 1
            i = ngp - 1
         ENDIF
         IF ( luset/=0 ) THEN
            CALL fread(Uset,Z(luset),ndof,0)
            CALL close(Uset,Kcl)
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
            IF ( lll==ngp ) THEN
               sil2 = ndof + 1
            ELSE
               IF ( lsil==0 ) CALL fread(Sil,Z(psil),1,0)
               sil2 = Z(psil)
               IF ( lsil/=0 ) psil = psil + 1
            ENDIF
            ndf = sil2 - sil1
            IF ( ndf<1 .OR. ndf>6 ) GOTO 200
!
!     GET NDF WORDS FROM USET
!
            IF ( luset==0 ) CALL fread(Uset,Z(puset),ndf,0)
!
!     DETERMINE IF IN THE SET
!
            j = puset
            k = j + ndf - 1
 30         DO i = j , k
               IF ( andf(Z(i),iset)/=0 ) GOTO 40
            ENDDO
            GOTO 50
!
!     LOCATED A POINT IN THE SET
!
 40         ll = i - puset + 1
            l = lgp + lll - 1
            IF ( ndf==1 ) ll = 0
            Z(mcount) = Z(l)*10 + ll
            mcount = mcount + Nwds
            IF ( mcount>=lgp ) GOTO 400
            IF ( i/=k ) THEN
               j = i + 1
               GOTO 30
            ENDIF
 50         IF ( luset/=0 ) puset = puset + ndf
            sil1 = sil2
         ENDDO
!
!     END OF ALL GRIDS AND MATRIX NOT FILLED - NEED IMMEDIATE MESSAGE.
!
         CALL page2(2)
         WRITE (Koutp,99002) Swm , name
99002    FORMAT (A27,' 3016, MATRIX IS NOT IN PROPER FORM IN SUBROUTINE ',2A4)
         GOTO 300
      ENDIF
   ENDIF
!
!     PURGED FILES
!
 100  CALL page2(2)
   WRITE (Koutp,99003) Swm , x(1) , name
99003 FORMAT (A27,' 3001, ATTEMPT TO OPEN DATA SET',I4,' IN SUBROUTINE',1X,2A4,' WHICH WAS NOT DEFINED IN FIST')
   GOTO 300
!
!     ILLEGAL INPUT
!
 200  CALL page2(2)
   WRITE (Koutp,99004) Swm , name
99004 FORMAT (A27,' 3007, ILLEGAL INPUT TO SUBROUTINE ',2A4)
!
 300  CALL close(Sil,Kcl)
   CALL close(Uset,Kcl)
   RETURN 1
 400  CALL close(Sil,Kcl)
   CALL close(Uset,Kcl)
END SUBROUTINE mxcid
