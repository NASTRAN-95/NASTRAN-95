
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
!
! COMMON variable declarations
!
   INTEGER Alter(2) , Bcdcnt , Core(1) , Diag14 , Diag17 , Diag25 , Diag4 , Dmap(1) , Dmpcnt , Dmpcrd(1) , Dmppnt , Gnobuf(1) ,     &
         & Iallon , Iapp , Ibuff(20) , Ichar , Icold , Icrdtp , Idmpnt , Iequl , Ifirst , Insert , Irturn , Isavdw , Iseqn ,        &
         & Isgnon , Islsh , Istopf , Kkcomm , Krud(6) , Ksystm(100) , Ldmap , Length , Loscar , Maskhi , Masklo , Modidx , Modnam , &
         & Nacpw , Nawpc , Nbegin , Nblank , Nbpc , Nchkpt , Ncond , Ndiag , Ndmap , Nend , Nequiv , Nestm1 , Nestm2 , Newcrd ,     &
         & Nexit , Njump , Nogo , Nosgn , Noutpt , Npurge , Nrept , Nsave , Nscr , Nsol , Ntime , Nxequi , Os(5) , Osbot , Oscar(1) &
         & , Ospnt , Osprc , Start
   COMMON /passer/ Istopf , Modnam , Kkcomm
   COMMON /system/ Ksystm
   COMMON /xgpi4 / Irturn , Insert , Iseqn , Dmpcnt , Idmpnt , Dmppnt , Bcdcnt , Length , Icrdtp , Ichar , Newcrd , Modidx , Ldmap ,&
                 & Isavdw , Dmap
   COMMON /xgpi5 / Iapp , Start , Alter
   COMMON /xgpi6 / Krud , Diag14 , Diag17 , Diag4 , Diag25 , Ifirst , Ibuff
   COMMON /xgpic / Icold , Islsh , Iequl , Nblank , Nxequi , Ndiag , Nsol , Ndmap , Nestm1 , Nestm2 , Nexit , Nbegin , Nend ,       &
                 & Njump , Ncond , Nrept , Ntime , Nsave , Noutpt , Nchkpt , Npurge , Nequiv , Nacpw , Nbpc , Nawpc , Maskhi ,      &
                 & Masklo , Isgnon , Nosgn , Iallon
   COMMON /xgpie / Nscr
   COMMON /zzzzzz/ Core
!
! Local variable declarations
!
   INTEGER i , ibufct , ibwrd , icall , icom , idlmtr(8) , itype(6) , izero , kblank , kcomma , kdh , kfl1 , kh , l , lx , nchar ,  &
         & ncpw , noscr1 , noscr2 , npt , nwpc
   INTEGER khrfn1 , lshift , orf , rshift
   EXTERNAL lshift , orf , rshift
!
! End of declarations
!
   EQUIVALENCE (Ksystm(3),Nogo) , (Core(1),Os(1),Loscar) , (Os(2),Osprc) , (Os(3),Osbot) , (Os(4),Ospnt) ,                          &
    & (Os(5),Oscar(1),Dmpcrd(1),Gnobuf(1))
   DATA itype/1 , 1 , 2 , 2 , 2 , 4/ , idlmtr/4H$    , 4H/    , 4H=    , 4H,    , 4H(    , 4H)    , 4H     , 4H*   /
   DATA noscr1/4HOSCA/ , noscr2/4HR   /
   DATA npt/4HNPTP/ , izero/0/
   DATA nwpc/18/ , ncpw/4/
!
! *** WARNING - NWPC AFFECTS CODE IN XOSGEN SO BEWARE IF YOU CHANGE IT.
!
   kcomma = khrfn1(izero,1,idlmtr(4),1)
   kblank = khrfn1(izero,1,idlmtr(7),1)
   Kkcomm = 0
!
!     CHECK FOR OSCAR TABLE OVERFLOW
!
   IF ( Oscar(Osbot)+Osbot>Icrdtp ) THEN
!
!     OSCAR TABLE OVERFLOW
!
      CALL xgpidg(14,noscr1,noscr2,Dmpcnt)
      CALL xgpidg(-38,2000,0,0)
      GOTO 1100
   ELSE
!
!     CHECK FOR CARD READ ERROR
!
      IF ( Nogo==2 ) GOTO 1200
!
!     CHECK FOR NEW CARD NEEDED.
!
      IF ( Newcrd/=0 ) GOTO 600
      IF ( Bcdcnt<0 ) THEN
!
!     CANNOT INTERPRET DMAP CARD
!
         CALL xgpidg(34,0,Dmpcnt,0)
         GOTO 1200
      ELSEIF ( Bcdcnt/=0 ) THEN
         GOTO 400
      ENDIF
   ENDIF
!
!     BCDCNT = 0, TEST MODE
!
 100  IF ( Modnam/=0 ) THEN
      kfl1 = 0
      icom = 0
      DO kh = 1 , nwpc
         DO kdh = 1 , ncpw
            nchar = khrfn1(izero,1,Ibuff(kh),kdh)
            IF ( nchar/=kblank ) THEN
               IF ( nchar/=kcomma ) THEN
                  IF ( icom==1 .OR. kfl1==2 ) GOTO 200
                  kfl1 = 1
               ELSE
                  kfl1 = 2
                  icom = icom + 1
                  IF ( icom==2 ) THEN
                     Kkcomm = 1
                     GOTO 200
                  ENDIF
               ENDIF
            ELSEIF ( kfl1/=0 ) THEN
               kfl1 = 2
            ENDIF
         ENDDO
      ENDDO
   ENDIF
 200  IF ( Dmap(Idmpnt)==rshift(Iallon,1) ) THEN
!
!     END OF DMAP INSTRUCTION
!
      Irturn = 4
      GOTO 99999
   ELSEIF ( Dmap(Idmpnt)<0 ) THEN
   ELSEIF ( Dmap(Idmpnt)==0 ) THEN
!
!     CONTINUE MODE - GET NEXT CARD
!
      Newcrd = 1
      GOTO 600
   ELSE
!
!     MODE IS BCD, INITIALIZE BCDCNT, DMPPNT, AND CHECK FOR OVERFLOW
!
      Bcdcnt = Dmap(Idmpnt)
      Idmpnt = Idmpnt + 1
      IF ( 2*Bcdcnt+Idmpnt<=Ldmap ) GOTO 400
      CALL xgpidg(34,0,Dmpcnt,0)
      GOTO 1200
   ENDIF
!
!     BINARY VALUE - TRANSLATE TYPE INTO LENGTH
!
 300  i = iabs(Dmap(Idmpnt))
   IF ( i>6 ) THEN
      CALL xgpidg(34,0,Dmpcnt,0)
      GOTO 1200
   ELSE
!
!     A MISUNDERSTANDING MAKES THE FOLLOWING STATEMENT NECESSARY.
!
      Dmap(Idmpnt) = orf(Isgnon,i)
      Length = itype(i)
      Dmppnt = Idmpnt
      Idmpnt = Length + 1 + Idmpnt
      Irturn = 3
      GOTO 99999
   ENDIF
!
!     TEST FOR OPERATOR ENTRY.
!
 400  Irturn = 2
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
            GOTO 500
         ELSEIF ( Dmap(Idmpnt+2)==-4 .AND. Dmap(Idmpnt+5)==-4 ) THEN
!
!     DOUBLE PRECISION COMPLEX NUMBER FOUND - FORM NUMBER CORRECTLY AND
!     SET TYPE CODE.
!
            Dmap(Idmpnt+5) = Dmap(Idmpnt+4)
            Dmap(Idmpnt+4) = Dmap(Idmpnt+3)
            Dmap(Idmpnt+3) = -6
            GOTO 500
         ENDIF
      ENDIF
   ENDIF
   Dmppnt = Idmpnt
   Idmpnt = Idmpnt + 2
   Bcdcnt = Bcdcnt - 1
   GOTO 99999
 500  Bcdcnt = 0
   Idmpnt = Idmpnt + 3
   GOTO 300
!
!     GET NEXT CARD IMAGE AND TRANSLATE INTO DMAP ARRAY.
!
 600  ibufct = 1
   ibwrd = 1
   icall = 0
!
!     CHECK FOR INSERT TO BE MADE
!
   IF ( Insert>0 .OR. Insert==-1 ) THEN
!
!     GET NEXT CARD IMAGE FROM ALTER FILE
!
      CALL read(*800,*700,npt,Ibuff,18,1,l)
   ELSE
!
!     FILL IBUFF WITH CARD IMAGE
!
      CALL read(*1100,*1000,Nscr,Ibuff,nwpc,0,lx)
   ENDIF
   GOTO 1000
!
!     NO MORE INSTRUCTIONS TO INSERT FOR THIS ALTER
!     MOVE NEXT ALTER CONTROL TO ALTER CELLS
!
 700  Alter(1) = Ibuff(1)
   Alter(2) = Ibuff(2)
   GOTO 900
!
!     END OF ALTER FILE - SET ALTER CELL INFINITE
!
 800  Alter(1) = 10000
!
!     DIAGNOSTIC MESSAGES -
!
!     ERROR IN ALTER DECK - CANNOT FIND LOGICAL END OF CARD
!
 900  IF ( Newcrd>0 ) CALL xgpidg(40,0,0,0)
   Irturn = 4
   GOTO 99999
!
!     CHECK INSERT FOR NO PRINT
!
 1000 IF ( Insert>=0 ) THEN
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
   IF ( khrfn1(izero,1,idlmtr(1),1)==khrfn1(izero,1,Ibuff(1),1) ) GOTO 600
!
!     CONVERT CARD IMAGE
!
   CALL xrcard(Dmap,Ldmap,Ibuff)
!
!     CHECK FOR BAD CARD FORMAT
!
   IF ( Dmap(1)==0 ) THEN
      Irturn = 4
      GOTO 99999
   ELSE
!
!     TRANSLATE CARD IMAGE INTO DMAP ARRAY
!
      Idmpnt = 1
      Bcdcnt = 0
      Newcrd = 0
      GOTO 100
   ENDIF
!
!     THIS DMAP INSTRUCTION NOT FOLLOWED BY END CARD.
!
 1100 CALL xgpidg(44,Ospnt,0,0)
!
!     ABORT - CANNOT CONTINUE COMPILATION
!
 1200 Nogo = 2
   Irturn = 5
99999 RETURN
END SUBROUTINE xscndm
