!*==xscndm.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xscndm
!
!     THE PURPOSE OF THIS ROUTINE IS TO RETURN TO THE CALLING PROGRAM
!     THE NEXT BCD OR BINARY ENTRY IN DMAP ARRAY.
!
!     IBUFF  = BUFFER AREA WHERE CARD IMAGE IS STORED FOR XRCARD INPUT.
!     IDLMTR = TABLE OF DELIMITER CHARACTERS
!     ITYPE  = TABLE FOR CONVERTING NUMBER TYPE TO WORD LENGTH.
!
!     LAST REVISED BY G.CHAN/UNISYS, 2/90
!     REMOVING LVAX AND .NOT.LVAX AND STANDARDIZED ALL BYTE OPERATIONS
!
   IMPLICIT NONE
   USE C_PASSER
   USE C_SYSTEM
   USE C_XGPI4
   USE C_XGPI5
   USE C_XGPI6
   USE C_XGPIC
   USE C_XGPIE
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: dmpcrd , gnobuf , oscar
   INTEGER :: i , ibufct , ibwrd , icall , icom , kblank , kcomma , kdh , kfl1 , kh , l , loscar , lx , nchar , nogo , osbot ,      &
            & ospnt , osprc
   INTEGER , DIMENSION(8) , SAVE :: idlmtr
   INTEGER , DIMENSION(6) , SAVE :: itype
   INTEGER , SAVE :: izero , ncpw , noscr1 , noscr2 , npt , nwpc
   INTEGER , DIMENSION(5) :: os
   EXTERNAL khrfn1 , orf , read , rshift , xgpidg , xgpimw , xrcard
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   !>>>>EQUIVALENCE (Ksystm(3),Nogo) , (Core(1),Os(1),Loscar) , (Os(2),Osprc) , (Os(3),Osbot) , (Os(4),Ospnt) ,                          &
!>>>>    & (Os(5),Oscar(1),Dmpcrd(1),Gnobuf(1))
   DATA itype/1 , 1 , 2 , 2 , 2 , 4/ , idlmtr/4H$    , 4H/    , 4H=    , 4H,    , 4H(    , 4H)    , 4H     , 4H*   /
   DATA noscr1/4HOSCA/ , noscr2/4HR   /
   DATA npt/4HNPTP/ , izero/0/
   DATA nwpc/18/ , ncpw/4/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
! *** WARNING - NWPC AFFECTS CODE IN XOSGEN SO BEWARE IF YOU CHANGE IT.
!
         kcomma = khrfn1(izero,1,idlmtr(4),1)
         kblank = khrfn1(izero,1,idlmtr(7),1)
         Kkcomm = 0
!
!     CHECK FOR OSCAR TABLE OVERFLOW
!
         IF ( oscar(osbot)+osbot>Icrdtp ) THEN
!
!     OSCAR TABLE OVERFLOW
!
            CALL xgpidg(14,noscr1,noscr2,Dmpcnt)
            CALL xgpidg(-38,2000,0,0)
            GOTO 80
         ELSE
!
!     CHECK FOR CARD READ ERROR
!
            IF ( nogo==2 ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     CHECK FOR NEW CARD NEEDED.
!
            IF ( Newcrd/=0 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Bcdcnt<0 ) THEN
!
!     CANNOT INTERPRET DMAP CARD
!
               CALL xgpidg(34,0,Dmpcnt,0)
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Bcdcnt/=0 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     BCDCNT = 0, TEST MODE
!
         IF ( Modnam/=0 ) THEN
            kfl1 = 0
            icom = 0
            SPAG_Loop_1_1: DO kh = 1 , nwpc
               DO kdh = 1 , ncpw
                  nchar = khrfn1(izero,1,Ibuff(kh),kdh)
                  IF ( nchar/=kblank ) THEN
                     IF ( nchar/=kcomma ) THEN
                        IF ( icom==1 .OR. kfl1==2 ) EXIT SPAG_Loop_1_1
                        kfl1 = 1
                     ELSE
                        kfl1 = 2
                        icom = icom + 1
                        IF ( icom==2 ) THEN
                           Kkcomm = 1
                           EXIT SPAG_Loop_1_1
                        ENDIF
                     ENDIF
                  ELSEIF ( kfl1/=0 ) THEN
                     kfl1 = 2
                  ENDIF
               ENDDO
            ENDDO SPAG_Loop_1_1
         ENDIF
         IF ( Dmap(Idmpnt)==rshift(Iallon,1) ) THEN
!
!     END OF DMAP INSTRUCTION
!
            Irturn = 4
            RETURN
         ELSEIF ( Dmap(Idmpnt)<0 ) THEN
         ELSEIF ( Dmap(Idmpnt)==0 ) THEN
!
!     CONTINUE MODE - GET NEXT CARD
!
            Newcrd = 1
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     MODE IS BCD, INITIALIZE BCDCNT, DMPPNT, AND CHECK FOR OVERFLOW
!
            Bcdcnt = Dmap(Idmpnt)
            Idmpnt = Idmpnt + 1
            IF ( 2*Bcdcnt+Idmpnt<=Ldmap ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL xgpidg(34,0,Dmpcnt,0)
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
!     BINARY VALUE - TRANSLATE TYPE INTO LENGTH
!
         i = iabs(Dmap(Idmpnt))
         IF ( i>6 ) THEN
            CALL xgpidg(34,0,Dmpcnt,0)
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     A MISUNDERSTANDING MAKES THE FOLLOWING STATEMENT NECESSARY.
!
            Dmap(Idmpnt) = orf(Isgnon,i)
            Length = itype(i)
            Dmppnt = Idmpnt
            Idmpnt = Length + 1 + Idmpnt
            Irturn = 3
            RETURN
         ENDIF
      CASE (4)
!
!     TEST FOR OPERATOR ENTRY.
!
         Irturn = 2
         IF ( Dmap(Idmpnt)==Iallon ) THEN
!
!     DELIMITER FOUND - CHECK FOR COMPLEX NUMBER
!
            Irturn = 1
            IF ( khrfn1(izero,1,Dmap(Idmpnt+1),1)==khrfn1(izero,1,idlmtr(5),1) ) THEN
!
!     LEFT PAREN FOUND - SEE IF TWO NUMBERS FOLLOW
!
               IF ( Dmap(Idmpnt+2)==-2 .AND. Dmap(Idmpnt+4)==-2 ) THEN
!
!     SINGLE PRECISION COMPLEX NUMBER FOUND - FORM NUMBER CORRECTLY
!
                  Dmap(Idmpnt+4) = Dmap(Idmpnt+3)
                  Dmap(Idmpnt+3) = -5
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( Dmap(Idmpnt+2)==-4 .AND. Dmap(Idmpnt+5)==-4 ) THEN
!
!     DOUBLE PRECISION COMPLEX NUMBER FOUND - FORM NUMBER CORRECTLY AND
!     SET TYPE CODE.
!
                  Dmap(Idmpnt+5) = Dmap(Idmpnt+4)
                  Dmap(Idmpnt+4) = Dmap(Idmpnt+3)
                  Dmap(Idmpnt+3) = -6
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDIF
         Dmppnt = Idmpnt
         Idmpnt = Idmpnt + 2
         Bcdcnt = Bcdcnt - 1
         RETURN
      CASE (5)
         Bcdcnt = 0
         Idmpnt = Idmpnt + 3
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (6)
!
!     GET NEXT CARD IMAGE AND TRANSLATE INTO DMAP ARRAY.
!
         ibufct = 1
         ibwrd = 1
         icall = 0
!
!     CHECK FOR INSERT TO BE MADE
!
         IF ( Insert>0 .OR. Insert==-1 ) THEN
!
!     GET NEXT CARD IMAGE FROM ALTER FILE
!
            CALL read(*40,*20,npt,Ibuff,18,1,l)
         ELSE
!
!     FILL IBUFF WITH CARD IMAGE
!
            CALL read(*80,*60,Nscr,Ibuff,nwpc,0,lx)
         ENDIF
         GOTO 60
!
!     NO MORE INSTRUCTIONS TO INSERT FOR THIS ALTER
!     MOVE NEXT ALTER CONTROL TO ALTER CELLS
!
 20      Alter(1) = Ibuff(1)
         Alter(2) = Ibuff(2)
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
!
!     END OF ALTER FILE - SET ALTER CELL INFINITE
!
 40      Alter(1) = 10000
         spag_nextblock_1 = 7
      CASE (7)
!
!     DIAGNOSTIC MESSAGES -
!
!     ERROR IN ALTER DECK - CANNOT FIND LOGICAL END OF CARD
!
         IF ( Newcrd>0 ) CALL xgpidg(40,0,0,0)
         Irturn = 4
         RETURN
!
!     CHECK INSERT FOR NO PRINT
!
 60      IF ( Insert>=0 ) THEN
!
!     PRINTOUT DMAP INSTRUCTION
!
            IF ( Ifirst/=0 ) THEN
               IF ( .NOT.(Diag17==0 .AND. (Diag14==0 .OR. Diag14>=10)) ) THEN
                  i = 5
                  IF ( Newcrd>0 ) i = 6
                  CALL xgpimw(i,nwpc,Dmpcnt,Ibuff)
               ENDIF
            ENDIF
         ENDIF
!
!     CHECK FOR COMMENT CARD
!
         IF ( khrfn1(izero,1,idlmtr(1),1)==khrfn1(izero,1,Ibuff(1),1) ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     CONVERT CARD IMAGE
!
         CALL xrcard(Dmap,Ldmap,Ibuff)
!
!     CHECK FOR BAD CARD FORMAT
!
         IF ( Dmap(1)==0 ) THEN
            Irturn = 4
            RETURN
         ELSE
!
!     TRANSLATE CARD IMAGE INTO DMAP ARRAY
!
            Idmpnt = 1
            Bcdcnt = 0
            Newcrd = 0
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     THIS DMAP INSTRUCTION NOT FOLLOWED BY END CARD.
!
 80      CALL xgpidg(44,ospnt,0,0)
         spag_nextblock_1 = 8
      CASE (8)
!
!     ABORT - CANNOT CONTINUE COMPILATION
!
         nogo = 2
         Irturn = 5
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE xscndm
