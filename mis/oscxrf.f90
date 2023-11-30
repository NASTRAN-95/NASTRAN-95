
SUBROUTINE oscxrf(Iop,Avail)
!
   IMPLICIT NONE
   INTEGER I , Iflg(5) , Ihead(96) , Iseqn , Ititl(96) , Itype , Kind , Ksys(65) , Mask3 , Mask4 , Mask5 , Nline , Nlpp , Nvail ,   &
         & Op , Vps(3) , Z(1)
   COMMON /lnklst/ I , Nvail , Iseqn , Kind , Itype , Mask3 , Mask4 , Mask5
   COMMON /moddmp/ Iflg
   COMMON /output/ Ititl , Ihead
   COMMON /system/ Ksys
   COMMON /xvps  / Vps
   COMMON /zzzzzz/ Z
   INTEGER Avail , Iop
   INTEGER andf , complf , lshift , orf , rshift
   INTEGER block(6) , dbent(3) , iauto , ihd1(32) , ihd2(32) , ihd3(32) , ihd4(32) , ihd5(32) , ii , il , iout(32) , irlh , isn ,   &
         & ist , itemp , j , k , kdh , lab(6) , len , link , ll , mask1 , mask2 , mi , nastk , nblank , ndb , nosgn , notapp ,      &
         & nparam , nparm , ntype , nwds , pool , pseq , q
   EXTERNAL andf , complf , lshift , orf , rshift
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
   DO
      Itype = andf(Vps(k+2),mask1)
      Itype = rshift(Itype,16)
      len = andf(Vps(k+2),mask2)
      CALL linkup(*1200,Vps(k))
      k = k + len + 3
      IF ( k>Vps(2) ) THEN
!
!     PROCESS NAMES OF MODULES AND DATA BLOCKS
!
         pseq = 0
         EXIT
      ELSE
         nparam = nparam + 1
      ENDIF
   ENDDO
 100  DO
      CALL read(*500,*1100,pool,block,6,0,q)
      iauto = 0
      mi = rshift(block(3),16)
      Itype = andf(mask2,block(3))
      Iseqn = andf(nosgn,block(6))
      Kind = 1
      IF ( pseq==Iseqn .AND. (mi==3 .OR. mi==8) ) iauto = 1
      IF ( iauto==1 ) Kind = 2
      pseq = Iseqn
      CALL linkup(*1200,block(4))
      Kind = 3
      IF ( Itype==3 ) THEN
         IF ( mi==7 ) THEN
            Kind = 5
            CALL read(*1100,*1100,pool,il,1,0,q)
            il = andf(mask2,il)
            CALL linkup(*1200,Vps(il-3))
         ENDIF
         CALL fwdrec(*1100,pool)
      ELSEIF ( Itype==4 ) THEN
         mi = mi - 7
         IF ( mi<0 ) mi = 4
         IF ( mi==2 .OR. mi==4 ) EXIT
         IF ( mi==3 ) GOTO 400
         CALL read(*1100,*1100,pool,ndb,1,0,q)
         Kind = 5
         IF ( iauto==1 ) Kind = 6
         DO j = 1 , ndb
            CALL read(*1100,*1100,pool,dbent,2,0,q)
            il = dbent(1)
            CALL linkup(*1200,Vps(il-3))
         ENDDO
         CALL fwdrec(*1100,pool)
      ELSE
!
!     PROCESS FUNCTIONAL MODULE IO SECTIONS
!
         irlh = 0
         DO
            irlh = irlh + 1
            CALL read(*1100,*1100,pool,ndb,1,0,q)
            DO j = 1 , ndb
               CALL read(*1100,*1100,pool,dbent,3,0,q)
               IF ( dbent(1)/=0 ) CALL linkup(*1200,dbent)
            ENDDO
            Kind = 4
            IF ( Itype/=1 .OR. irlh/=1 ) THEN
               Kind = 5
               CALL read(*1100,*1100,pool,ndb,-1,0,q)
               CALL read(*1100,*1100,pool,nparm,1,0,q)
               IF ( nparm/=0 ) THEN
                  DO j = 1 , nparm
                     CALL read(*1100,*1100,pool,il,1,0,q)
                     IF ( il<0 ) THEN
                        il = andf(nosgn,il)
                        CALL linkup(*1200,Vps(il-3))
                     ELSE
                        CALL read(*1100,*1100,pool,dbent,-il,0,q)
                     ENDIF
                  ENDDO
               ENDIF
               CALL fwdrec(*1100,pool)
               EXIT
            ENDIF
         ENDDO
      ENDIF
   ENDDO
 200  CALL read(*1100,*100,pool,ndb,1,0,q)
   Kind = 3
 300  DO j = 1 , ndb
      CALL read(*1100,*1100,pool,dbent,2,0,q)
      IF ( dbent(1)/=0 ) CALL linkup(*1200,dbent)
   ENDDO
   IF ( mi==4 ) THEN
      CALL fwdrec(*1100,pool)
      GOTO 100
   ELSE
      CALL read(*1100,*1100,pool,il,1,0,q)
      IF ( il>0 ) THEN
         Kind = 5
         CALL linkup(*1200,Vps(il-3))
      ENDIF
      IF ( mi==2 ) GOTO 200
   ENDIF
 400  CALL read(*1100,*100,pool,ndb,1,0,q)
   Kind = 3
   CALL read(*1100,*1100,pool,dbent,3,0,q)
   IF ( dbent(1)/=0 ) CALL linkup(*1200,dbent)
   ndb = ndb - 1
   GOTO 300
!
!     SORT PARAMETER AND MODULE NAMES, 8-BCD WORD SORT
!
 500  nwds = 4*nparam
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
   WRITE (Op,99005)
   Nline = Nline + 1
!
!     PROCESS PARAMETER NAMES
!
 600  iout(2) = Z(k)
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
 700  link = andf(Mask3,Z(k+2))
   DO
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
         Nline = Nline + 1
         IF ( Nline>Nlpp ) THEN
            CALL page
            Nline = Nline + 1
            WRITE (Op,99005)
            Nline = Nline + 1
         ENDIF
         WRITE (Op,99001) (iout(ll),ll=2,32)
99001    FORMAT (5X,31A4)
         DO ll = 2 , 32
            iout(ll) = nblank
         ENDDO
         EXIT
      ELSE
         ii = ii + 2
      ENDIF
   ENDDO
 800  k = k + 4
   IF ( k>=ist ) THEN
!
!     PROCESS MODULE NAMES
!
      IF ( kdh<=0 ) THEN
         kdh = 1
         DO j = 1 , 32
            Ihead(j+64) = ihd3(j)
         ENDDO
         WRITE (Op,99002)
99002    FORMAT (//6X,'* DENOTES APPEARANCE OF PARAMETER IN AUTOMATICALLY',' GENERATED SAVE INSTRUCTION')
         CALL page
         Nline = Nline + 1
         WRITE (Op,99005)
         k = ist
         ist = nwds
!
!     PROCESS DATA BLOCKS
!
      ELSEIF ( kdh>1 ) THEN
         WRITE (Op,99003)
99003    FORMAT (//6X,'* DENOTES STATEMENTS IN WHICH THE DATA BLOCK ','APPEARSRS AS OUTPUT.')
         CALL rewind(pool)
         CALL skpfil(pool,Iop)
         CALL fwdrec(*1100,pool)
         GOTO 1300
      ELSE
         kdh = 2
         DO j = 1 , 32
            Ihead(j+64) = ihd4(j)
         ENDDO
         WRITE (Op,99004)
99004    FORMAT (//6X,'* DENOTES AUTOMATICALLY GENERATED INSTRUCTIONS',/8X,'STATEMENT NUMBER REFERS TO DMAP SEQUENCE NUMBER OF ',   &
                &'PREVIOUS INSTRUCTION')
         CALL page
         Nline = Nline + 1
         WRITE (Op,99005)
         k = 4*nparam + 1
         ist = nwds
         GOTO 1000
      ENDIF
   ELSEIF ( kdh/=1 ) THEN
      IF ( kdh/=2 ) GOTO 600
      GOTO 1000
   ENDIF
   IF ( rshift(Z(k+3),28)>=3 ) GOTO 800
 900  iout(2) = Z(k)
   iout(3) = Z(k+1)
   iout(4) = nblank
   ii = 5
   GOTO 700
 1000 IF ( rshift(Z(k+3),28)<3 ) GOTO 800
   GOTO 900
 1100 CALL xgpidg(59,0,0,0)
   GOTO 1300
 1200 CALL xgpidg(60,0,0,0)
 1300 RETURN
!
99005 FORMAT (1H )
END SUBROUTINE oscxrf