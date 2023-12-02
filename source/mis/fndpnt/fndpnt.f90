!*==fndpnt.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fndpnt(Iary,Id)
   USE C_FPT
   USE C_LOADX
   USE C_SYSTEM
   USE C_ZZZZZZ
   IMPLICIT NONE
   INTEGER Bgpdt , Edt , I1(2) , I2 , Ibuf , icore(1) , Ied , Igptt , Impt , Isil , Lcore , Mpt , Nout , Nrow1 , Old , Sil
   REAL Core(1) , Cstm , Dum(3) , Gptt
   COMMON /fpt   / Dum , Nrow1 , Lcore
   COMMON /loadx / I1 , Bgpdt , Old , Cstm , Sil , Isil , I2 , Mpt , Gptt , Edt , Impt , Igptt , Ied
   COMMON /system/ Ibuf , Nout
   COMMON /zzzzzz/ Core
   REAL Delta
   INTEGER Id , Idef , Ied1 , Ip
   INTEGER Iary(4)
   REAL arry(3) , flag
   INTEGER i , icp , iedt(2) , if , ifed(2) , ifound , ipm , iry(3) , isave(4) , k , name(2) , ns
   INTEGER :: spag_nextblock_1
!
   !>>>>EQUIVALENCE (iry(1),arry(1)) , (Core(1),Icore(1))
   DATA name/4HFNDP , 4HNT  /
   DATA iedt/4HEDT  , 4HFEDT/ , ifed/4HFEDT , 4HST  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     FIND POINT ON BGPDT
!
         IF ( Id<0 ) GOTO 20
         IF ( Id>=268435455 .OR. Old<0 ) THEN
!               268435455 = 2**28 - 1
            WRITE (Nout,99001) Id , Old
99001       FORMAT (//,' BAD DATA PASSED TO FNDPNT, ID,OLD =',2I14)
            CALL mesage(-37,0,name)
         ENDIF
         SPAG_Loop_1_1: DO
            ns = 4*(Id-Old)
            IF ( ns<4 ) THEN
!
               IF ( ns==0 ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               CALL bckrec(Bgpdt)
               Old = 0
            ELSEIF ( ns==4 ) THEN
               EXIT SPAG_Loop_1_1
            ELSE
               CALL read(*20,*20,Bgpdt,isave(1),-ns+4,0,flag)
               EXIT SPAG_Loop_1_1
            ENDIF
         ENDDO SPAG_Loop_1_1
         CALL read(*20,*20,Bgpdt,isave(1),4,0,flag)
         Old = Id
         spag_nextblock_1 = 2
      CASE (2)
         DO i = 1 , 4
            Iary(i) = isave(i)
         ENDDO
         spag_nextblock_1 = 3
      CASE (3)
         RETURN
!
 20      ipm = Bgpdt
         spag_nextblock_1 = 4
      CASE (4)
         CALL mesage(-2,ipm,name)
 40      ipm = Sil
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 60      ipm = Edt
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
!
!
         ENTRY fndsil(Ip)
         SPAG_Loop_1_2: DO
!     =================
!
!     FIND SIL VALUE
!
            ns = Ip - Isil
            IF ( ns<1 ) THEN
               IF ( ns==0 ) THEN
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               CALL bckrec(Sil)
               Isil = 0
            ELSEIF ( ns==1 ) THEN
               EXIT SPAG_Loop_1_2
            ELSE
               CALL read(*40,*40,Sil,i,-ns+1,0,flag)
               EXIT SPAG_Loop_1_2
            ENDIF
         ENDDO SPAG_Loop_1_2
         CALL read(*40,*40,Sil,if,1,0,flag)
         Isil = Ip
         spag_nextblock_1 = 5
      CASE (5)
         Ip = if
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!
         ENTRY fedtst(Idef)
!     ===================
!
!     FIND ENFORCED DISPLACEMENT
!
!     PUT DEFORM EID S AND VALUES INTO CORE FOR THIS SET
!
         icp = Nrow1 + 1
         k = 0
         CALL read(*60,*60,Edt,arry(1),-3,0,flag)
         SPAG_Loop_1_3: DO
            CALL read(*60,*80,Edt,arry(1),3,0,flag)
            IF ( Idef==iry(1) .OR. k/=0 ) THEN
               IF ( Idef/=iry(1) ) EXIT SPAG_Loop_1_3
               k = k + 2
               Core(icp+k) = arry(3)
               icore(icp+k-1) = iry(2)
               IF ( Lcore-Nrow1+k<=0 ) CALL mesage(-8,ipm,ifed)
            ENDIF
         ENDDO SPAG_Loop_1_3
 80      IF ( k==0 ) CALL mesage(-32,Idef,iedt)
         CALL bckrec(Edt)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!
         ENTRY fedt(Ied1,Delta,Idef)
!     ============================
!
!     FIND VALUE FOR EID IF IT EXISTS
!
         DO i = 1 , k , 2
            IF ( Ied1==icore(icp+i) ) THEN
               icore(icp+i) = -icore(icp+i)
               Delta = Core(icp+i+1)
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         Delta = 0.0
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!
         ENTRY fedted(Idef)
!     ===================
!
!     CHECK TO SEE IF ALL ELEMENTS IN THE SET WERE USED
!
         ifound = 0
         DO i = 1 , k , 2
            IF ( icore(icp+i)>=0 ) THEN
               iedt(1) = icore(icp+i)
               iedt(2) = Idef
               CALL mesage(30,139,iedt)
               ifound = 1
            ENDIF
         ENDDO
         IF ( ifound==1 ) CALL mesage(-61,0,0)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE fndpnt
