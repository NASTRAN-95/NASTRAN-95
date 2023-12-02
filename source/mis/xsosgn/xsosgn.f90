!*==xsosgn.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xsosgn
!
!     THIS SUBROUTINE SCANS THE OSCAR TAPE AND GENERATES THE SOS + MD
!
!     LAST REVISED BY G.CHAN/UNISYS TO REMOVE THE VAX AND NOT-VAX
!     LOGICS, AND TO SYNCHRONIZE THE SCRATH FILE NAMES AS SET FORTH BY
!     THE XSEMX ROUTINES.   2/1990
!
   IMPLICIT NONE
   USE C_ISOSGN
   USE C_SYSTEM
   USE C_XDPL
   USE C_XFIAT
   USE C_XFIST
   USE C_XMSSG
   USE C_XSFA1
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: almsk , apndmk , blkcnt , cursno , dnaf , entn1 , entn2 , entn3 , entn4 , flag , fnx , funlg , i , iflag , isw , l ,  &
            & ll , lll , lmsk , lxmsk , mlgn , n1 , nwds , rmsk , rxmsk , s , scornt , scrn3 , slgn , tapmsk , zap
   INTEGER , DIMENSION(100) :: block
   INTEGER , DIMENSION(93) :: block1
   INTEGER , SAVE :: cond , jump , oscar , rept , scrn1 , scrn2
   INTEGER , DIMENSION(1) :: fequ , fntu , fon , ford , minp , mlsn , mout , mscr , sal , sdbn , sntu , sord
   INTEGER , DIMENSION(2) , SAVE :: nsosgn
   INTEGER , DIMENSION(10) , SAVE :: numbr
   EXTERNAL andf , bckrec , close , fwdrec , khrfn1 , mesage , open , orf , read , rshift , skpfil
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!     LOGICAL         DEC
   !>>>>EQUIVALENCE (Dpd(1),Dnaf) , (Fiat(1),Funlg) , (File(1),Fequ(1)) , (File(1),Ford(1)) , (block(8),block1(1))
   !>>>>EQUIVALENCE (Md(1),Mlgn) , (Md(2),Mlsn(1)) , (Md(3),Minp(1)) , (Md(4),Mout(1)) , (Md(5),Mscr(1)) , (Sos(1),Slgn) ,               &
!>>>>    & (Sos(2),Sdbn(1)) , (Sos(4),Sal(1),Sntu(1),Sord(1)) , (Comm(1),Almsk) , (Comm(2),Apndmk) , (Comm(3),Cursno) , (Comm(4),Entn1) ,&
!>>>>    & (Comm(5),Entn2) , (Comm(6),Entn3) , (Comm(7),Entn4) , (Comm(8),Flag) , (Comm(9),Fnx) , (Comm(10),Lmsk) , (Comm(11),Lxmsk) ,   &
!>>>>    & (Comm(13),Rmsk) , (Comm(14),Rxmsk) , (Comm(15),S) , (Comm(16),Scornt) , (Comm(17),Tapmsk) , (Comm(19),Zap) ,                  &
!>>>>    & (Xf1at(1),Fntu(1),Fon(1))
   DATA jump/4HJUMP/ , rept/4HREPT/ , cond/4HCOND/
   DATA oscar/4HPOOL/ , scrn1 , scrn2/4HSCRA , 4HTCH0/
   DATA nsosgn/4HXSOS , 2HGN/
   DATA numbr/1H1 , 1H2 , 1H3 , 1H4 , 1H5 , 1H6 , 1H7 , 1H8 , 1H9 , 1H0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         iflag = 0
         CALL open(*80,oscar,Buf1,2)
         CALL bckrec(oscar)
         CALL read(*60,*100,oscar,block,7,0,flag)
         IF ( block(2)==cursno ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         WRITE (Outtap,99001) Sfm , block(2) , cursno
99001    FORMAT (A25,' 1014, POOL FILE MIS-POSITIONED ',2I7)
         CALL mesage(-37,0,nsosgn)
         RETURN
      CASE (2)
!
!     READ OSCAR FORMAT HEADER + 1
!
         IF ( J>1400 .OR. K>390 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL read(*60,*100,oscar,block,7,0,flag)
         spag_nextblock_1 = 3
      CASE (3)
         block(3) = andf(rmsk,block(3))
         IF ( block(6)>=0 ) THEN
            CALL fwdrec(*60,oscar)
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( block(3)<=2 ) THEN
!
!     INPUT FILES
!
            minp(K) = block(7)
            IF ( block(7)==0 ) THEN
!
!     ZERO INPUT FILES
!
               CALL read(*60,*100,oscar,block(7),1,0,flag)
               IF ( block(3)==2 ) THEN
!
!     TYPE O FORMAT - NO OUTPUTS
!
                  mout(K) = 0
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  mout(K) = block(7)
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSE
               nwds = block(7)*Entn5
               ASSIGN 20 TO isw
            ENDIF
         ELSE
            IF ( block(3)==3 ) THEN
               l = rshift(andf(lxmsk,block(7)),16) - block(2)
               IF ( block(4)/=jump ) THEN
                  IF ( block(4)/=rept .AND. block(4)/=cond ) THEN
                     CALL fwdrec(*60,oscar)
                  ELSE
                     IF ( l<0 ) iflag = -1
                     CALL fwdrec(*60,oscar)
                  ENDIF
               ELSEIF ( l<=1 ) THEN
                  IF ( l<0 ) iflag = -1
                  CALL fwdrec(*60,oscar)
               ELSE
                  DO i = 1 , l
                     CALL fwdrec(*60,oscar)
                  ENDDO
               ENDIF
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL fwdrec(*60,oscar)
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
!
!     FILES READER
!
         CALL read(*60,*100,oscar,block1,nwds+1,0,flag)
         blkcnt = 0
         DO i = 1 , nwds , Entn5
            IF ( block1(i)==0 ) THEN
               blkcnt = blkcnt + 1
            ELSE
               Sos(J) = block1(i)
               Sos(J+1) = block1(i+1)
               Sos(J+2) = block1(i+2)
               J = J + 3
               IF ( J>1500 ) THEN
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDDO
         GOTO isw
!
 20      minp(K) = minp(K) - blkcnt
         IF ( block(3)==2 ) THEN
            mout(K) = 0
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     OUTPUT FILES
!
            mout(K) = block1(nwds+1)
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         IF ( mout(K)==0 ) THEN
!
!     ZERO OUTPUT FILES
!
            CALL read(*60,*100,oscar,block1(nwds+1),1,0,flag)
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ELSE
            nwds = mout(K)*Entn6
            ASSIGN 40 TO isw
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
 40      mout(K) = mout(K) - blkcnt
         spag_nextblock_1 = 6
      CASE (6)
         CALL fwdrec(*60,oscar)
!
!     SCRATCH FILES
!
         mscr(K) = block1(nwds+1)
         IF ( mscr(K)/=0 ) THEN
            l = mscr(K)
            scrn3 = scrn2
            lll = 1
            ll = 0
            DO i = 1 , l
               ll = ll + 1
               IF ( ll==10 ) scrn3 = khrfn1(scrn3,3,numbr(lll),1)
               Sos(J) = scrn1
               Sos(J+1) = khrfn1(scrn3,4,numbr(ll),1)
               IF ( ll==10 ) THEN
                  ll = 0
                  lll = lll + 1
               ENDIF
               IF ( Str(i)/=0 ) THEN
                  n1 = Str(i)
                  Sos(n1) = orf(lmsk,block(2))
               ENDIF
               Str(i) = J + 2
               Sos(J+2) = scornt + i
               J = J + 3
               IF ( J>1500 ) THEN
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
!
         mlsn(K) = block(2)
         IF ( iflag/=0 ) mlsn(K) = orf(s,mlsn(K))
         IF ( minp(K)+mout(K)+mscr(K)==0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         K = K + entn3
         IF ( K>400 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
 60      CALL skpfil(oscar,-1)
         spag_nextblock_1 = 7
      CASE (7)
         CALL close(oscar,2)
         slgn = (J-1)/entn2
         mlgn = (K-1)/entn3
         RETURN
      CASE (8)
!
!     SYSTEM FATAL MESSAGES
!
         WRITE (Outtap,99002) Sfm
99002    FORMAT (A25,' 1011, MD OR SOS TABLE OVERFLOW')
         CALL mesage(-37,0,nsosgn)
         RETURN
 80      WRITE (Outtap,99003) Sfm
99003    FORMAT (A25,' 1012, POOL COULD NOT BE OPENED')
         CALL mesage(-37,0,nsosgn)
         RETURN
 100     WRITE (Outtap,99004) Sfm
99004    FORMAT (A25,' 1013, ILLEGAL EOR ON POOL')
         CALL mesage(-37,0,nsosgn)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE xsosgn
