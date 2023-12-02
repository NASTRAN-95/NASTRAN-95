!*==oscxrf.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE oscxrf(Iop,Avail)
!
   IMPLICIT NONE
   USE C_LNKLST
   USE C_MODDMP
   USE C_OUTPUT
   USE C_SYSTEM
   USE C_XVPS
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iop
   INTEGER :: Avail
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(6) :: block
   INTEGER , DIMENSION(3) :: dbent
   INTEGER :: iauto , ii , il , irlh , isn , ist , itemp , j , k , kdh , len , link , ll , mask1 , mask2 , mi , ndb , nline , nlpp ,&
            & nosgn , nparam , nparm , ntype , nwds , op , pseq , q
   INTEGER , DIMENSION(32) , SAVE :: ihd1 , ihd2 , ihd3 , ihd4 , ihd5 , iout
   INTEGER , DIMENSION(6) , SAVE :: lab
   INTEGER , SAVE :: nastk , nblank , notapp , pool
   EXTERNAL andf , complf , fwdrec , linkup , lshift , orf , outpak , page , read , rewind , rshift , skpfil , sorta8 , xgpidg
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   !>>>>EQUIVALENCE (Ksys(2),Op) , (Ksys(9),Nlpp) , (Ksys(12),Nline)
   DATA ihd1/7*4H     , 4HCOSM , 4HIC / , 4H NAS , 4HTRAN , 4H DMA , 4HP CO , 4HMPIL , 4HER - , 4H DMA , 4HP CR , 4HOSS  , 4HREFE , &
       &4HRENC , 4HE LI , 4HSTIN , 4HG    , 9*4H    /
   DATA ihd2/32*4H    /
   DATA ihd3/4HMODU , 4HLE N , 4HAME  , 4HDMAP , 4H STA , 4HTEME , 4HNT N , 4HUMBE , 4HRS   , 23*4H    /
   DATA ihd4/4HDATA , 4H BLO , 4HCK   , 4HDMAP , 4H STA , 4HTEME , 4HNT N , 4HUMBE , 4HRS   , 23*4H    /
   DATA ihd5/4HPARA , 4HMETE , 4HR    , 4HTYPE , 4H     , 4HDMAP , 4H STA , 4HTEME , 4HNT N , 4HUMBE , 4HRS   , 21*4H    /
   DATA lab/4HI    , 4HR    , 4HBCD  , 4HRDP  , 4HCSP  , 4HCDP /
   DATA pool/4HPOOL/
   DATA nblank/4H    / , iout/32*4H    /
   DATA nastk/4H*   / , notapp/4HN.A./
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     RESTRICT OPEN CORE DUE TO LIMITED FIELD SIZE FOR POINTERS
!
         Nvail = Avail
         IF ( Nvail>16350 ) Nvail = 16350
!
!     PROCESS VARAIABLE PARAMETER LIST
!
         mask2 = lshift(1,16) - 1
         mask1 = andf(lshift(1,20)-1,complf(mask2))
         Mask3 = lshift(1,14) - 1
         Mask4 = lshift(Mask3,14)
         Mask5 = complf(orf(Mask3,Mask4))
         nosgn = complf(lshift(1,Ksys(40)-1))
!
         DO I = 1 , 1600
            Z(I) = 0
         ENDDO
         k = 3
         I = 1
         Kind = -5
         nparam = 1
         SPAG_Loop_1_1: DO
            Itype = andf(Vps(k+2),mask1)
            Itype = rshift(Itype,16)
            len = andf(Vps(k+2),mask2)
            CALL linkup(*80,Vps(k))
            k = k + len + 3
            IF ( k>Vps(2) ) THEN
!
!     PROCESS NAMES OF MODULES AND DATA BLOCKS
!
               pseq = 0
               EXIT SPAG_Loop_1_1
            ELSE
               nparam = nparam + 1
            ENDIF
         ENDDO SPAG_Loop_1_1
 20      SPAG_Loop_1_2: DO
            CALL read(*40,*60,pool,block,6,0,q)
            iauto = 0
            mi = rshift(block(3),16)
            Itype = andf(mask2,block(3))
            Iseqn = andf(nosgn,block(6))
            Kind = 1
            IF ( pseq==Iseqn .AND. (mi==3 .OR. mi==8) ) iauto = 1
            IF ( iauto==1 ) Kind = 2
            pseq = Iseqn
            CALL linkup(*80,block(4))
            Kind = 3
            IF ( Itype==3 ) THEN
               IF ( mi==7 ) THEN
                  Kind = 5
                  CALL read(*60,*60,pool,il,1,0,q)
                  il = andf(mask2,il)
                  CALL linkup(*80,Vps(il-3))
               ENDIF
               CALL fwdrec(*60,pool)
            ELSEIF ( Itype==4 ) THEN
               mi = mi - 7
               IF ( mi<0 ) mi = 4
               IF ( mi==2 .OR. mi==4 ) EXIT SPAG_Loop_1_2
               IF ( mi==3 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               CALL read(*60,*60,pool,ndb,1,0,q)
               Kind = 5
               IF ( iauto==1 ) Kind = 6
               DO j = 1 , ndb
                  CALL read(*60,*60,pool,dbent,2,0,q)
                  il = dbent(1)
                  CALL linkup(*80,Vps(il-3))
               ENDDO
               CALL fwdrec(*60,pool)
            ELSE
!
!     PROCESS FUNCTIONAL MODULE IO SECTIONS
!
               irlh = 0
               SPAG_Loop_2_3: DO
                  irlh = irlh + 1
                  CALL read(*60,*60,pool,ndb,1,0,q)
                  DO j = 1 , ndb
                     CALL read(*60,*60,pool,dbent,3,0,q)
                     IF ( dbent(1)/=0 ) CALL linkup(*80,dbent)
                  ENDDO
                  Kind = 4
                  IF ( Itype/=1 .OR. irlh/=1 ) THEN
                     Kind = 5
                     CALL read(*60,*60,pool,ndb,-1,0,q)
                     CALL read(*60,*60,pool,nparm,1,0,q)
                     IF ( nparm/=0 ) THEN
                        DO j = 1 , nparm
                           CALL read(*60,*60,pool,il,1,0,q)
                           IF ( il<0 ) THEN
                              il = andf(nosgn,il)
                              CALL linkup(*80,Vps(il-3))
                           ELSE
                              CALL read(*60,*60,pool,dbent,-il,0,q)
                           ENDIF
                        ENDDO
                     ENDIF
                     CALL fwdrec(*60,pool)
                     EXIT SPAG_Loop_2_3
                  ENDIF
               ENDDO SPAG_Loop_2_3
            ENDIF
         ENDDO SPAG_Loop_1_2
         spag_nextblock_1 = 2
      CASE (2)
         CALL read(*60,*20,pool,ndb,1,0,q)
         Kind = 3
         spag_nextblock_1 = 3
      CASE (3)
         DO j = 1 , ndb
            CALL read(*60,*60,pool,dbent,2,0,q)
            IF ( dbent(1)/=0 ) CALL linkup(*80,dbent)
         ENDDO
         IF ( mi==4 ) THEN
            CALL fwdrec(*60,pool)
            GOTO 20
         ELSE
            CALL read(*60,*60,pool,il,1,0,q)
            IF ( il>0 ) THEN
               Kind = 5
               CALL linkup(*80,Vps(il-3))
            ENDIF
            IF ( mi==2 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
         CALL read(*60,*20,pool,ndb,1,0,q)
         Kind = 3
         CALL read(*60,*60,pool,dbent,3,0,q)
         IF ( dbent(1)/=0 ) CALL linkup(*80,dbent)
         ndb = ndb - 1
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     SORT PARAMETER AND MODULE NAMES, 8-BCD WORD SORT
!
 40      nwds = 4*nparam
         CALL sorta8(0,0,4,1,Z(1),nwds)
         ist = nwds + 1
         j = I - 1 - nwds
         CALL sorta8(0,0,4,1,Z(ist),j)
         nwds = I - 1
!
!     TRAVERSE LINKED LISTS AND GENERATE OUTPUT
!
         k = 1
         kdh = 0
         DO j = 1 , 32
            Ihead(j) = ihd1(j)
            Ihead(j+32) = ihd2(j)
            Ihead(j+64) = ihd5(j)
         ENDDO
         CALL page
         WRITE (op,99005)
         nline = nline + 1
         spag_nextblock_1 = 5
      CASE (5)
!
!     PROCESS PARAMETER NAMES
!
         iout(2) = Z(k)
         iout(3) = Z(k+1)
         ntype = rshift(Z(k+2),28)
         iout(4) = nblank
         iout(5) = lab(ntype)
         IF ( ntype==0 .OR. ntype>6 ) iout(5) = notapp
         iout(6) = nblank
!
!     TRACE THROUGH LINKED LIST
!
         ii = 7
         spag_nextblock_1 = 6
      CASE (6)
         link = andf(Mask3,Z(k+2))
         SPAG_Loop_1_4: DO
            isn = andf(Mask3,Z(link))
            IF ( kdh==0 ) isn = -isn
            CALL outpak(ii,iout,isn)
            itemp = rshift(Z(link),28)
            IF ( itemp==2 .OR. itemp==4 .OR. itemp==6 ) iout(ii+1) = nastk
            link = rshift(andf(Z(link),Mask4),14)
            IF ( link==0 ) THEN
!
!     PRINT OUTPUT
!
               nline = nline + 1
               IF ( nline>nlpp ) THEN
                  CALL page
                  nline = nline + 1
                  WRITE (op,99005)
                  nline = nline + 1
               ENDIF
               WRITE (op,99001) (iout(ll),ll=2,32)
99001          FORMAT (5X,31A4)
               DO ll = 2 , 32
                  iout(ll) = nblank
               ENDDO
               EXIT SPAG_Loop_1_4
            ELSE
               ii = ii + 2
            ENDIF
         ENDDO SPAG_Loop_1_4
         spag_nextblock_1 = 7
      CASE (7)
         SPAG_Loop_1_5: DO
            k = k + 4
            IF ( k>=ist ) THEN
!
!     PROCESS MODULE NAMES
!
               IF ( kdh<=0 ) THEN
                  kdh = 1
                  DO j = 1 , 32
                     Ihead(j+64) = ihd3(j)
                  ENDDO
                  WRITE (op,99002)
99002             FORMAT (//6X,'* DENOTES APPEARANCE OF PARAMETER IN AUTOMATICALLY',' GENERATED SAVE INSTRUCTION')
                  CALL page
                  nline = nline + 1
                  WRITE (op,99005)
                  k = ist
                  ist = nwds
!
!     PROCESS DATA BLOCKS
!
               ELSEIF ( kdh>1 ) THEN
                  WRITE (op,99003)
99003             FORMAT (//6X,'* DENOTES STATEMENTS IN WHICH THE DATA BLOCK ','APPEARSRS AS OUTPUT.')
                  CALL rewind(pool)
                  CALL skpfil(pool,Iop)
                  CALL fwdrec(*60,pool)
                  spag_nextblock_1 = 10
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  kdh = 2
                  DO j = 1 , 32
                     Ihead(j+64) = ihd4(j)
                  ENDDO
                  WRITE (op,99004)
99004             FORMAT (//6X,'* DENOTES AUTOMATICALLY GENERATED INSTRUCTIONS',/8X,                                                &
                         &'STATEMENT NUMBER REFERS TO DMAP SEQUENCE NUMBER OF ','PREVIOUS INSTRUCTION')
                  CALL page
                  nline = nline + 1
                  WRITE (op,99005)
                  k = 4*nparam + 1
                  ist = nwds
                  spag_nextblock_1 = 9
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( kdh/=1 ) THEN
               IF ( kdh==2 ) THEN
                  spag_nextblock_1 = 9
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( rshift(Z(k+3),28)<3 ) EXIT SPAG_Loop_1_5
         ENDDO SPAG_Loop_1_5
         spag_nextblock_1 = 8
      CASE (8)
         iout(2) = Z(k)
         iout(3) = Z(k+1)
         iout(4) = nblank
         ii = 5
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
      CASE (9)
         IF ( rshift(Z(k+3),28)>=3 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 60      CALL xgpidg(59,0,0,0)
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 80      CALL xgpidg(60,0,0,0)
         spag_nextblock_1 = 10
      CASE (10)
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
99005 FORMAT (1H )
END SUBROUTINE oscxrf
