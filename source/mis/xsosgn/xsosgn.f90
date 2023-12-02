!*==xsosgn.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xsosgn
!
!     THIS SUBROUTINE SCANS THE OSCAR TAPE AND GENERATES THE SOS + MD
!
!     LAST REVISED BY G.CHAN/UNISYS TO REMOVE THE VAX AND NOT-VAX
!     LOGICS, AND TO SYNCHRONIZE THE SCRATH FILE NAMES AS SET FORTH BY
!     THE XSEMX ROUTINES.   2/1990
!
   USE c_isosgn
   USE c_system
   USE c_xdpl
   USE c_xfiat
   USE c_xfist
   USE c_xmssg
   USE c_xsfa1
   USE c_zzzzzz
   IMPLICIT NONE
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
         CALL open(*80,oscar,buf1,2)
         CALL bckrec(oscar)
         CALL read(*60,*100,oscar,block,7,0,flag)
         IF ( block(2)==cursno ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         WRITE (outtap,99001) sfm , block(2) , cursno
99001    FORMAT (A25,' 1014, POOL FILE MIS-POSITIONED ',2I7)
         CALL mesage(-37,0,nsosgn)
         RETURN
      CASE (2)
!
!     READ OSCAR FORMAT HEADER + 1
!
         IF ( j>1400 .OR. k>390 ) THEN
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
            minp(k) = block(7)
            IF ( block(7)==0 ) THEN
!
!     ZERO INPUT FILES
!
               CALL read(*60,*100,oscar,block(7),1,0,flag)
               IF ( block(3)==2 ) THEN
!
!     TYPE O FORMAT - NO OUTPUTS
!
                  mout(k) = 0
                  spag_nextblock_1 = 6
               ELSE
                  mout(k) = block(7)
                  spag_nextblock_1 = 5
               ENDIF
               CYCLE
            ELSE
               nwds = block(7)*entn5
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
         DO i = 1 , nwds , entn5
            IF ( block1(i)==0 ) THEN
               blkcnt = blkcnt + 1
            ELSE
               sos(j) = block1(i)
               sos(j+1) = block1(i+1)
               sos(j+2) = block1(i+2)
               j = j + 3
               IF ( j>1500 ) THEN
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDDO
         GOTO isw
!
 20      minp(k) = minp(k) - blkcnt
         IF ( block(3)==2 ) THEN
            mout(k) = 0
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     OUTPUT FILES
!
            mout(k) = block1(nwds+1)
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         IF ( mout(k)==0 ) THEN
!
!     ZERO OUTPUT FILES
!
            CALL read(*60,*100,oscar,block1(nwds+1),1,0,flag)
            spag_nextblock_1 = 6
         ELSE
            nwds = mout(k)*entn6
            ASSIGN 40 TO isw
            spag_nextblock_1 = 4
         ENDIF
         CYCLE
!
 40      mout(k) = mout(k) - blkcnt
         spag_nextblock_1 = 6
      CASE (6)
         CALL fwdrec(*60,oscar)
!
!     SCRATCH FILES
!
         mscr(k) = block1(nwds+1)
         IF ( mscr(k)/=0 ) THEN
            l = mscr(k)
            scrn3 = scrn2
            lll = 1
            ll = 0
            DO i = 1 , l
               ll = ll + 1
               IF ( ll==10 ) scrn3 = khrfn1(scrn3,3,numbr(lll),1)
               sos(j) = scrn1
               sos(j+1) = khrfn1(scrn3,4,numbr(ll),1)
               IF ( ll==10 ) THEN
                  ll = 0
                  lll = lll + 1
               ENDIF
               IF ( str(i)/=0 ) THEN
                  n1 = str(i)
                  sos(n1) = orf(lmsk,block(2))
               ENDIF
               str(i) = j + 2
               sos(j+2) = scornt + i
               j = j + 3
               IF ( j>1500 ) THEN
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
!
         mlsn(k) = block(2)
         IF ( iflag/=0 ) mlsn(k) = orf(s,mlsn(k))
         IF ( minp(k)+mout(k)+mscr(k)==0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         k = k + entn3
         IF ( k>400 ) THEN
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
         slgn = (j-1)/entn2
         mlgn = (k-1)/entn3
         RETURN
      CASE (8)
!
!     SYSTEM FATAL MESSAGES
!
         WRITE (outtap,99002) sfm
99002    FORMAT (A25,' 1011, MD OR SOS TABLE OVERFLOW')
         CALL mesage(-37,0,nsosgn)
         RETURN
 80      WRITE (outtap,99003) sfm
99003    FORMAT (A25,' 1012, POOL COULD NOT BE OPENED')
         CALL mesage(-37,0,nsosgn)
         RETURN
 100     WRITE (outtap,99004) sfm
99004    FORMAT (A25,' 1013, ILLEGAL EOR ON POOL')
         CALL mesage(-37,0,nsosgn)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE xsosgn
