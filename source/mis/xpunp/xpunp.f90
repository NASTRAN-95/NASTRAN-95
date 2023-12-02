!*==xpunp.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xpunp
!
!     THIS SUBROUTINE POOLS AND UNPOOLS FILES AS PRESCRIBED BY XFIAT
!
   IMPLICIT NONE
   USE C_SYSTEM
   USE C_XDPL
   USE C_XFIAT
   USE C_XFIST
   USE C_XMSSG
   USE C_XPFIST
   USE C_XSFA1
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: almsk , apndmk , cursno , dculg , dfnusv , dmxlg , dnaf , entn1 , entn1x , entn2 , entn3 , entn4 , fculg , flag ,     &
            & fmxlg , fn , fnx , fstidx , funlg , hold , i , ii , isw1 , isw2 , j , k , kk , l , lmsk , lmt3 , lmt4 , lxmsk ,       &
            & macsft , mlgn , ncnt , nn , nx , rmsk , rxmsk , s , scornt , slgn , tapmsk , thcrmk , zap
   INTEGER , DIMENSION(1000) :: block
   INTEGER , DIMENSION(1) :: ddbn , dfnu , fcum , fcus , fdbn , fequ , file , fknd , fmat , fntu , fon , ford , fpun , minp , mlsn ,&
                           & mout , mscr , sal , sdbn , sntu , sord
   INTEGER , SAVE :: entn5 , n , pool
   INTEGER , DIMENSION(2) :: head
   INTEGER , DIMENSION(8) :: header
   INTEGER , DIMENSION(2) , SAVE :: npunp
   EXTERNAL andf , close , cpyfil , eof , lshift , mesage , open , orf , page2 , read , sswtch , write , xfilps , xpolck
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   !>>>>EQUIVALENCE (Dpd(1),Dnaf) , (Dpd(2),Dmxlg) , (Dpd(3),Dculg) , (Dpd(4),Ddbn(1)) , (Dpd(6),Dfnu(1)) , (Fiat(1),Funlg) ,            &
!>>>>    & (Fiat(2),Fmxlg) , (Fiat(3),Fculg) , (Fiat(4),Fequ(1)) , (Fiat(4),File(1)) , (Fiat(4),Ford(1)) , (Fiat(5),Fdbn(1)) ,           &
!>>>>    & (Fiat(7),Fmat(1)) , (Md(1),Mlgn) , (Md(2),Mlsn(1)) , (Md(3),Minp(1)) , (Md(4),Mout(1)) , (Md(5),Mscr(1)) , (Sos(1),Slgn) ,    &
!>>>>    & (Sos(2),Sdbn(1)) , (Sos(4),Sal(1)) , (Sos(4),Sntu(1)) , (Sos(4),Sord(1)) , (Xf1at(1),Fntu(1)) , (Xf1at(1),Fon(1)) ,           &
!>>>>    & (Xf1at(2),Fpun(1)) , (Xf1at(3),Fcum(1)) , (Xf1at(4),Fcus(1)) , (Xf1at(5),Fknd(1))
   !>>>>EQUIVALENCE (Comm(1),Almsk) , (Comm(2),Apndmk) , (Comm(3),Cursno) , (Comm(4),Entn1) , (Comm(5),Entn2) , (Comm(6),Entn3) ,        &
!>>>>    & (Comm(7),Entn4) , (Comm(8),Flag) , (Comm(9),Fnx) , (Comm(10),Lmsk) , (Comm(11),Lxmsk) , (Comm(12),Macsft) , (Comm(13),Rmsk) , &
!>>>>    & (Comm(14),Rxmsk) , (Comm(15),S) , (Comm(16),Scornt) , (Comm(17),Tapmsk) , (Comm(18),Thcrmk) , (Comm(19),Zap)
   DATA n/1000/ , pool/4HPOOL/ , entn5/2/ , npunp/4HXPUN , 4HP   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     ENTRY SIZE NUMBERS,  1=FIAT, 4=DPD
!
         isw1 = 0
         isw2 = 0
         entn1x = entn1 - 1
         Fist(2) = 1 + Pfist
!
!     COMPUTE INDEX FOR DUMMY ENTRY IN FIST
!
         fstidx = Fist(2)*2 + 1
         lmt3 = fculg*entn1
!
!     CHECK FOR ANY FILES TO POOL
!
         Fist(fstidx) = 101
         SPAG_Loop_1_1: DO i = 1 , lmt3 , entn1
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  IF ( fpun(i)>=0 ) CYCLE
                  nn = andf(almsk,fpun(i))
                  fpun(i) = 0
                  IF ( fmat(i)==0 .AND. fmat(i+1)==0 .AND. fmat(i+2)==0 ) THEN
                     IF ( .NOT.(entn1==11 .AND. (fmat(i+5)/=0 .OR. fmat(i+6)/=0 .OR. fmat(i+7)/=0)) ) THEN
                        nn = 1
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                  ENDIF
                  CALL xpolck(fdbn(i),fdbn(i+1),fn,nx)
                  IF ( fn==0 ) THEN
                     IF ( isw1==0 ) THEN
                        isw1 = 1
                        CALL open(*20,pool,Buf1,2)
                        CALL xfilps(dnaf)
                        CALL close(pool,2)
                        CALL open(*20,pool,Buf1,3)
                        fnx = dnaf
                     ENDIF
                     Fist(fstidx+1) = i + entn5
                     CALL open(*20,101,Buf1(Ibufsz+1),0)
                     ncnt = 0
!
!     WRITE SPECIAL FILE HEADER RECORD -- XPOOL DICT NAME    ( 2 WORDS )
!                                       + DATA BLOCK TRAILER ( 3 WORDS
!                                                         OR   6 WORDS )
!
                     CALL write(pool,fdbn(i),2,0)
                     IF ( entn1==11 ) THEN
                        CALL write(pool,fmat(i),3,0)
                        CALL write(pool,fmat(i+5),3,1)
                     ELSE
                        CALL write(pool,fmat(i),3,1)
                     ENDIF
!
!     READ AND WRITE 1ST 2 WORDS OF DATA BLOCK HEADER.
!     THEN CALL CPYFIL TO COPY REMAINDER OF FILE.
!
                     CALL read(*40,*60,101,head,2,0,flag)
                     CALL write(pool,head,2,0)
                     CALL cpyfil(101,pool,block,n,flag)
                     ncnt = andf(lxmsk,lshift(flag/1000+1,16))
                     CALL eof(pool)
                     CALL close(101,1)
!
!     ADD FILE NAME OF FILE JUST POOLED TO DPD
!
                     j = dculg*entn4 + 1
                     dculg = dculg + 1
                     IF ( dculg>dmxlg ) THEN
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     dfnu(j) = orf(dnaf,ncnt)
                     ddbn(j) = fdbn(i)
                     ddbn(j+1) = fdbn(i+1)
                     CALL sswtch(3,l)
                     IF ( l==1 ) THEN
                        CALL page2(-2)
                        WRITE (Outtap,99001) ddbn(j) , ddbn(j+1) , head(1) , head(2)
99001                   FORMAT (16H0POOL FILE NAME ,2A4,17H DATA BLOCK NAME ,2A4)
                     ENDIF
                     dnaf = dnaf + 1
                     fnx = fnx + 1
                  ELSE
                     j = nx
                  ENDIF
                  spag_nextblock_2 = 2
               CASE (2)
                  hold = andf(rxmsk,file(i))
                  lmt4 = i + entn1x
                  DO kk = i , lmt4
                     file(kk) = 0
                  ENDDO
                  file(i) = hold
                  fdbn(i) = almsk
!
!     CHECK FOR EQUIV FILES
!
                  IF ( nn/=1 ) THEN
!
!     THERE ARE EQUIV FILES
!
                     dfnu(j) = orf(s,dfnu(j))
                     dfnusv = dfnu(j)
                     DO k = 1 , lmt3 , entn1
                        IF ( fequ(k)<0 .AND. i/=k ) THEN
                           IF ( andf(rmsk,file(i))/=andf(rmsk,file(k)) ) CYCLE
!
!     THIS IS AN EQUIV FILE
!
                           CALL xpolck(fdbn(k),fdbn(k+1),fn,nx)
                           IF ( fn/=0 ) THEN
                              IF ( dfnu(nx)==dfnusv ) GOTO 2
                              ddbn(nx) = 0
                              ddbn(nx+1) = 0
                           ENDIF
                           j = j + entn4
                           dculg = dculg + 1
                           IF ( dculg>dmxlg ) THEN
                              spag_nextblock_1 = 2
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           dfnu(j) = dfnusv
                           ddbn(j) = fdbn(k)
                           ddbn(j+1) = fdbn(k+1)
                           lmt4 = k + entn1x
                           DO kk = k , lmt4
                              file(kk) = 0
                           ENDDO
 2                         nn = nn - 1
                           IF ( nn==1 ) CYCLE SPAG_Loop_1_1
                        ENDIF
                     ENDDO
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO SPAG_Loop_1_1
         IF ( isw1/=0 ) THEN
            CALL close(pool,1)
            fnx = 1
         ENDIF
!
!     CHECK FOR ANY FILES TO UNPOOL
!
         Fist(fstidx) = 201
         DO
            fn = dnaf
            DO i = 1 , lmt3 , entn1
               IF ( fpun(i)>0 .AND. fpun(i)<fn ) THEN
                  fn = fpun(i)
                  ii = i
               ENDIF
            ENDDO
            IF ( fn==dnaf ) THEN
               IF ( isw2/=0 ) THEN
                  CALL close(pool,1)
                  fnx = 1
               ENDIF
               RETURN
            ELSE
               fpun(ii) = 0
               IF ( isw2==0 ) THEN
                  isw2 = 1
                  CALL open(*20,pool,Buf1,0)
                  fnx = 1
               ENDIF
               CALL xfilps(fn)
               fnx = fn
               Fist(fstidx+1) = ii + entn5
               CALL open(*20,201,Buf1(Ibufsz+1),1)
!
!     READ SPECIAL FILE HEADER RECORD AND, IF DIAG 3 IS ON, PRINT MSG
!
               CALL read(*40,*60,pool,header,entn1-3,1,flag)
               CALL sswtch(3,l)
               IF ( l==1 ) THEN
                  CALL page2(-2)
                  WRITE (Outtap,99002) fdbn(ii) , fdbn(ii+1) , header(1) , header(2)
99002             FORMAT (17H0XUNPL-DICT NAME ,2A4,16H POOL FILE NAME ,2A4)
               ENDIF
!
!     COPY FILE USING CPYFIL
!
               CALL cpyfil(pool,201,block,n,flag)
               CALL close(201,1)
               fnx = fnx + 1
               fmat(ii) = header(3)
               fmat(ii+1) = header(4)
               fmat(ii+2) = header(5)
               IF ( entn1==11 ) THEN
                  fmat(ii+5) = header(6)
                  fmat(ii+6) = header(7)
                  fmat(ii+7) = header(8)
               ENDIF
!
!     IS FILE EQUIVALENCED
!
               IF ( fequ(ii)<0 ) THEN
!
!     YES, COPY SAME TRAILER INTO ALL EQUIV FILES
!
                  hold = andf(rmsk,file(ii))
                  DO j = 1 , lmt3 , entn1
                     IF ( fequ(j)<0 .AND. ii/=j ) THEN
                        IF ( hold==andf(rmsk,file(j)) ) THEN
                           fmat(j) = header(3)
                           fmat(j+1) = header(4)
                           fmat(j+2) = header(5)
                           IF ( entn1==11 ) THEN
                              fmat(j+5) = header(6)
                              fmat(j+6) = header(7)
                              fmat(j+7) = header(8)
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDDO
               ENDIF
            ENDIF
         ENDDO
         spag_nextblock_1 = 2
      CASE (2)
!
!
         WRITE (Outtap,99003)
99003    FORMAT (1H0,23X,19H 1031, DPL OVERFLOW)
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 20      WRITE (Outtap,99004)
99004    FORMAT (1H0,23X,62H 1032, POOL OR FILE BEING POOLED/UN-POOLED COULD NOT BE OPENED)
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 40      WRITE (Outtap,99005)
99005    FORMAT (1H0,23X,39H 1033, ILLEGAL EOF ON FILE BEING POOLED)
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 60      WRITE (Outtap,99006)
99006    FORMAT (1H0,23X,39H 1034, ILLEGAL EOR ON FILE BEING POOLED)
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (3)
         WRITE (Outtap,99007)
99007    FORMAT (1H0,23X,33H 1035, EQUIV INDICATED,NONE FOUND)
         spag_nextblock_1 = 4
      CASE (4)
         CALL page2(-4)
         WRITE (Outtap,99008) Sfm
99008    FORMAT (A25,1H.)
         CALL mesage(-37,0,npunp)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE xpunp
