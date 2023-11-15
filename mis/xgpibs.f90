
SUBROUTINE xgpibs
!
!     PURPOSE OF THIS ROUTINE IS TO INITIALIZE MACHINE DEPENDENT
!     CONSTANTS FOR XGPI AND ASSOCIATED ROUTINES AND TO INITIALIZE
!     THE MODULE LINK TABLE.
!
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER A(22) , B(7) , Core(1) , Cpntry(7) , D(5) , Eotflg , Iallon , Iappnd , Ieqflg , Ijhalf(4) , Intap , Intgr , Iplus ,      &
         & Isgnon , Itape , Ixx(1) , Jmp(7) , Lhpw(2) , Link(1) , Llink , Lmpl , Lopncr , Losgn , Lpch , Lxlink , Maskhi , Masklo , &
         & Masks(1) , Maxlnk , Mchnam , Mlink(1) , Mpl(1) , Mplpnt , Nbpc , Ncpw , Nlines , Nlpp , Noflgs , Nosgn , Nwpc , Nwpic ,  &
         & Optap , Os(2) , Pghdg(113) , Seteor , Xsys(90)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /lhpwx / Lhpw , Nwpic
   COMMON /machin/ Ijhalf , Mchnam
   COMMON /output/ Pghdg
   COMMON /system/ Xsys , Lpch
   COMMON /xgpi2 / Lmpl , Mplpnt , Mpl
   COMMON /xgpi2x/ Ixx
   COMMON /xgpi6 / D , Iplus
   COMMON /xgpic / A , Ncpw , Nbpc , Nwpc , Maskhi , Masklo , Isgnon , Nosgn , Iallon , Masks
   COMMON /xgpid / B , Itape , Iappnd , Intgr , Losgn , Noflgs , Seteor , Eotflg , Ieqflg , Cpntry , Jmp
   COMMON /xlink / Lxlink , Maxlnk , Mlink
   COMMON /xlkspc/ Llink , Link
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   COMMON /zzzzzz/ Core
!
! Local variable declarations
!
   INTEGER andf , complf , khrfn1 , korsz , lshift , orf , rshift
   INTEGER delete , dolsgn , enddta(2) , frstin , hdg1(32) , hdg2(32) , i , i1 , i10 , inbuff(20) , irtn , j , j1 , j10 , j2 , j3 , &
         & j4 , k , k1 , l , last , lastin , lk , ll(15) , lnkbot , lnkedt(15) , lnkspc(1) , lnktop , lstgrp , m , m1 , m10 ,       &
         & mhibyt , modidx , modnam(2) , n , nblank , none(2) , nwptyp(6) , nxtgrp , nxtlin , opbtop , opbuff(1) , opncor(1) ,      &
         & utilty(1) , utlbot , utltop , xnone
   EXTERNAL andf , complf , lshift , orf , rshift
!
! End of declarations
!
   EQUIVALENCE (Xsys(2),Optap) , (Xsys(9),Nlpp) , (Xsys(12),Nlines) , (Xsys(4),Intap) ,                                             &
    & (opncor(1),lnkspc(1),opbuff(1),Os(2),utilty(1)) , (Core(1),Os(1),Lopncr)
   DATA modnam/4HCHKP , 4HNT  /
   DATA delete/4HDELE/ , xnone/4HNONE/
   DATA enddta/4HENDD , 4HATA / , dolsgn/4H$   /
!
!             NWPTYP = NUMBER OF WORDS PER PARAMETER TYPE CODE
!                        INT,   REAL,   BCD,   D.P.,   CMPLX,  D.P.CMPLX
   DATA nwptyp/1 , 1 , 2 , 2 , 2 , 4/
   DATA nblank/4H    / , none/4H(NON , 4HE)  /
   DATA lnkedt/4H  1  , 4H  2  , 4H  3  , 4H  4  , 4H  5  , 4H  6  , 4H  7  , 4H  8  , 4H  9  , 4H 10  , 4H 11  , 4H 12  , 4H 13  , &
       &4H 14  , 4H 15 /
   DATA hdg1/4HMODU , 4HLE - , 4H DMA , 4HP NA , 4HME - , 4H MOD , 4HULE  , 4HENTR , 4HY -  , 4HLINK , 4HS MO , 4HDULE , 4H RES ,   &
       &4HIDES , 4H IN  , 4HON   , 16*4H    /
   DATA hdg2/4HINDE , 4HX    , 4H OF  , 4HMODU , 4HLE   , 4H POI , 4HNT N , 4HAME  , 24*4H    /
!
!     INITIALIZE MACHINE DEPENDENT CONSTANTS FOR XGPI
!     SEE SUBROUTINE XGPIDD FOR DESCRIPTION OF CONSTANTS.
!
!     INITIALIZE  /XGPIC/
!
!     NCPW   = NUMBER OF CHARACTERS PER WORD
!     NBPC   = NUMBER OF BITS PER CHARACTER
!     NWPC   = NUMBER OF WORDS PER CARD = NWPIC
!                 7094         360        1108             6600
!    MASKLO = 017777600000, 7FFF0000, 017777600000, 00000000017777600000
!    ISGNON = 400000000000, 80000000, 400000000000, 40000000000000000000
!    NOSGN  = 377777777777, 7FFFFFFF, 377777777777, 37777777777777777777
!    IALLON = 777777777777, FFFFFFFF, 777777777777, 77777777777777777777
!
!    MASKHI = MASK FOR LOW ORDER 16 BITS AND SIGN BIT = 32767,
!             INITIALIZED IN XGPIDD
!
   Ncpw = Xsys(41)
   Nbpc = Xsys(39)
   Nwpc = Nwpic
   Masklo = lshift(Maskhi,16)
   Isgnon = lshift(1,Xsys(40)-1)
   Nosgn = complf(Isgnon)
   Iallon = complf(0)
!
!     GENERATE MASKS ARRAY
!     MASK IS IN 4 PARTS - MASK DESCRIPTION WILL BE GIVEN IN TERMS OF
!                          IBM 360
!     PART 1 - FFOOOOOO,OOFFOOOO,OOOOFFOO,OOOOOOFF
!     PART 2 - COMPLEMENT OF PART 1
!     PART 3 - FFFFFFFF,OOFFFFFF,OOOOFFFF,OOOOOOFF
!     PART 4 - COMPLEMENT OF PART 3
!
   mhibyt = lshift(Iallon,Nbpc*(Ncpw-1))
   DO j = 1 , Ncpw
      Masks(j) = rshift(mhibyt,Nbpc*(j-1))
      j2 = j + Ncpw
      Masks(j2) = complf(Masks(j))
      j3 = 2*Ncpw + j
      Masks(j3) = rshift(Iallon,Nbpc*(j-1))
      j4 = 3*Ncpw + j
      Masks(j4) = complf(Masks(j3))
   ENDDO
!
!     INITIALIZE  /XGPID/
!
!                 7094         360        1108             6600
!    ITAPE  = 000000100000, 00008000, 000000100000, 00000000000000100000
!    IAPPND = 010000000000, 40000000, 010000000000, 00000000010000000000
!    INTGR  = 400000000001, 80000001, 400000000001, 40000000000000000001
!    LOSGN  = 000000100000, 00008000, 000000100000, 00000000000000100000
!    NOFLGS = 000377777777, 03FFFFFF, 000377777777, 00000000000377777777
!    SETEOR = 004000000000, 20000000, 004000000000, 00000000004000000000
!    EOTFLG = 010000000000, 40000000, 010000000000, 00000000010000000000
!    IEQFLG = 400000000000, 80000000, 400000000000, 40000000000000000000
!    CPNTRY(3) = CHKPNT MODULE INDEX/TYPE CODE
!    NTRY(6)= 400000000001, 80000001, 400000000001, 40000000000000000001
!    JMP(3) = JUMP MODULE INDEX/TYPE CODE
!
   Itape = lshift(1,15)
   Iappnd = lshift(1,30)
   Intgr = orf(Isgnon,1)
   Losgn = lshift(1,15)
   Noflgs = rshift(Iallon,Xsys(40)-26)
   Seteor = lshift(1,29)
   Eotflg = lshift(1,30)
   Ieqflg = Isgnon
!
!     PRINT MPL CONTENTS IF DIAG 31 IS ON
!
   ASSIGN 200 TO irtn
   CALL sswtch(31,l)
   IF ( l/=0 ) CALL mplprt
!
!     GET CHKPNT MODULE INDEX
!
 100  modidx = 1
   Mplpnt = 1
   DO
      IF ( Mpl(Mplpnt+1)==modnam(1) .AND. Mpl(Mplpnt+2)==modnam(2) ) GOTO irtn
      modidx = modidx + 1
      Mplpnt = Mplpnt + Mpl(Mplpnt)
      IF ( Mplpnt>Lmpl .OR. Mpl(Mplpnt)<1 ) THEN
!
!     FATAL ERROR IN MPL TABLE
!
         CALL xgpidg(49,Mplpnt,Mpl(Mplpnt+1),Mpl(Mplpnt+2))
!
!     FATAL ERROR EXIT
!
         Xsys(3) = 3
         GOTO 99999
      ENDIF
   ENDDO
 200  Cpntry(3) = lshift(modidx,16) + 4
!
!     GET JUMP MODULE INDEX
!
   ASSIGN 300 TO irtn
   modnam(1) = Jmp(4)
   modnam(2) = Jmp(5)
   GOTO 100
 300  Jmp(3) = lshift(modidx,16) + 3
   Cpntry(6) = orf(Isgnon,1)
   Jmp(6) = Cpntry(6)
!
!     COMPUTE LENGTH OF OPENCORE (SUBTRACT OFF SOME FOR UTILITY BUFFERS)
!
   Lopncr = korsz(opncor) - Xsys(1) - 1
   utltop = Lopncr + 1
   utlbot = utltop + Xsys(1) - 1
!
!     INITIALIZE  /XGPI2/ (I.E. MPL TABLE)
!
!     LOAD FLOATING POINT NUMBERS INTO MPL FROM ARRAY IN /XGPI2X/
!
   Mplpnt = 1
 400  IF ( Mpl(Mplpnt)>=4 ) THEN
      IF ( Mpl(Mplpnt+3)>=1 .AND. Mpl(Mplpnt+3)<=2 ) THEN
!
!     MPL ENTRY HAS MODULE TYPE CODE 1 OR 2 - PROCESS PARAMETER SECTION.
!
         i = Mplpnt + 7
!
!     CHECK FOR END OF MPL ENTRY
!
         DO WHILE ( i<Mplpnt+Mpl(Mplpnt) )
!
!     CHECK VALIDITY OF PARAMETER TYPE CODE
!
            j = iabs(Mpl(i))
            IF ( j<1 .OR. j>6 ) THEN
!
!     ERROR IN PARAMETER SECTION OF MPL TABLE
!
               CALL xgpidg(49,Mplpnt,Mpl(Mplpnt+1),Mpl(Mplpnt+2))
               EXIT
            ELSE
               l = 1
!
!     SEE IF PARAMETER VALUE FOLLOWS TYPE CODE.
!
               IF ( Mpl(i)>=0 ) THEN
!
!     GET LENGTH OF PARAMETER VALUE TO BE LOADED.
!
                  l = nwptyp(j)
!
!     A VALUE FOLLOWS IF TYPE CODE IS INTEGER OR BCD - OTHERWISE AN
!     INDEX INTO A TABLE CONTAINING THE VALUE FOLLOWS THE TYPE CODE.
!
                  IF ( j/=1 .AND. j/=3 ) THEN
!
!     GET INDEX INTO VALUE TABLE - NOTE INDEX MUST BE CONVERTED FROM
!     DOUBLE PRECISION INDEX TO ONE DIMENSIONAL INDEX.
!
                     m = Mpl(i+1)*2 - 1
                     DO k = 1 , l
                        n = k + m - 1
                        k1 = i + k
                        Mpl(k1) = Ixx(n)
                     ENDDO
                  ENDIF
                  i = i + 1
               ENDIF
!
!     INCREMENT TO NEXT PARAMETER TYPE CODE.
!
               i = i + l
            ENDIF
         ENDDO
      ENDIF
   ENDIF
!
!     GET NEXT MPL ENTRY
!
   IF ( Mpl(Mplpnt)+Mplpnt>Lmpl ) THEN
!
!     INITIALIZE /XLINK/
!
!     MAXLNK = MAXIMUM NUMBER OF LINKS THAT CAN BE HANDLED. IF MAXLNK IS
!              INCREASED THEN LNKEDT TABLE MUST BE INCREASED.
!              (MAXLNK WAS SET IN SEMDBD ROUTINE)
!
!     MOVE LINK TABLE INTO OPEN CORE
!
      lnktop = 1
      lnkbot = Llink + lnktop - 5
      DO j = 1 , Llink
         lnkspc(j) = Link(j)
      ENDDO
!
!     UPDATE LNKSPC TABLE IF SENSE SWITCH 29 IS ON
!
      CALL sswtch(29,l)
      IF ( l==0 ) GOTO 1300
      ASSIGN 800 TO irtn
   ELSE
      Mplpnt = Mplpnt + Mpl(Mplpnt)
      IF ( Mpl(Mplpnt)>=1 ) GOTO 400
      CALL xgpidg(49,Mplpnt,Mpl(Mplpnt+1),Mpl(Mplpnt+2))
      Xsys(3) = 3
      GOTO 99999
   ENDIF
!
!     PROCESS INPUT CARD (NOTE-DO NOT USE VARIABLES I,J OR M)
!
 500  CALL page1
   Nlines = Nlines + 2
   WRITE (Optap,99001)
99001 FORMAT (42H0LINK SPECIFICATION TABLE UPDATE DECK ECHO)
 600  DO
      Nlines = Nlines + 1
      IF ( Nlines>=Nlpp ) GOTO 500
      CALL xread(*700,inbuff)
      WRITE (Optap,99002) inbuff
99002 FORMAT (5X,20A4)
!
!     CHECK FOR COMMENT CARD
!
      IF ( khrfn1(0,1,inbuff(1),1)/=khrfn1(0,1,dolsgn,1) ) THEN
!
!     CONVERT CARD IMAGE
!
         CALL xrcard(utilty(utltop),utlbot-utltop+1,inbuff)
         IF ( utilty(utltop)/=0 ) THEN
!
!     CHECK FOR ENDDATA CARD
!
            IF ( utilty(utltop+1)==enddta(1) .AND. utilty(utltop+2)==enddta(2) ) THEN
!
!     PUNCH OUT LNKSPC TABLE IF SENSE SWITCH 28 IS ON.
!
               CALL sswtch(28,l)
               IF ( l/=0 ) GOTO 1100
               GOTO 1300
            ELSE
               GOTO irtn
            ENDIF
         ENDIF
      ENDIF
   ENDDO
 700  CALL page2(2)
   WRITE (Optap,99003) Ufm
99003 FORMAT (A23,' 220, MISSING ENDDATA CARD.')
   Xsys(3) = 3
   GOTO 99999
!
!     CHECK FORMAT OF CARD
!
 800  IF ( utilty(utltop)<2 ) GOTO 1900
!
!     SEE IF MODULE NAME IS IN LNKSPC TABLE
!
   DO i = lnktop , lnkbot , 5
      IF ( lnkspc(i)==utilty(utltop+1) .AND. lnkspc(i+1)==utilty(utltop+2) ) GOTO 900
   ENDDO
!
!     MODULE IS NOT IN LNKSPC - MAKE NEW ENTRY
!
   lnkbot = lnkbot + 5
   IF ( lnkbot>Lopncr ) THEN
!
!     ERROR MESSAGES -
!
!     NOT ENOUGH OPEN CORE
!
      CALL xgpidg(51,lnkbot-Lopncr,0,0)
      Xsys(3) = 3
      GOTO 99999
   ELSE
      i = lnkbot
   ENDIF
!
!     TRANSFER MODULE NAME AND ENTRY POINT TO LNKSPC
!
 900  lnkspc(i) = utilty(utltop+1)
   lnkspc(i+1) = utilty(utltop+2)
   lnkspc(i+2) = utilty(utltop+3)
   lnkspc(i+3) = utilty(utltop+4)
!
!     CHECK FOR DELETE OR NONE
!
   IF ( utilty(utltop)==2 ) THEN
!
!     GENERATE A LINK FLAG WORD
!
      m = 0
      j = 5
   ELSEIF ( utilty(utltop+5)==delete ) THEN
!
!     MODULE IS TO BE DELETED
!
      lnkspc(i) = 0
!
!     PROCESS NEXT INPUT CARD
!
      ASSIGN 800 TO irtn
      GOTO 600
   ELSE
      IF ( utilty(utltop+5)/=xnone ) GOTO 1900
!
!     MODULE HAS NO ENTRY POINT
!
      lnkspc(i+2) = none(1)
      lnkspc(i+3) = none(2)
      m = 0
      j = 7
      IF ( utilty(utltop+7)/=-1 ) j = 9
   ENDIF
 1000 DO
!
!     CHECK MODE WORD
!
      k = utltop + j
      IF ( utilty(k)<0 ) THEN
!
!     INTEGER FOUND
!
         IF ( utilty(k)/=-1 ) GOTO 1900
         m = orf(m,lshift(1,utilty(k+1)-1))
         j = j + 2
      ELSEIF ( utilty(k)==0 ) THEN
!
!     CONTINUE MODE FOUND
!
         j = 1
         ASSIGN 1000 TO irtn
         GOTO 600
      ELSE
!
!     END OF INSTRUCTION FOUND
!
!     TRANSFER GENERATED LINK WORD TO LNKSPC ENTRY
!
         IF ( utilty(k)/=Nosgn ) GOTO 1900
         j = i + 4
         lnkspc(j) = m
         ASSIGN 800 TO irtn
         GOTO 600
      ENDIF
   ENDDO
!
!     ELIMINATE DELETED LNKSPC ENTRIES
!
 1100 DO i = lnktop , lnkbot , 5
      IF ( lnkspc(i)==0 ) GOTO 1200
   ENDDO
   CALL page2(2)
   WRITE (Optap,99004)
99004 FORMAT (98H0***USER REQUESTS LINK SPECIFICATION TABLE BE PUNCHED OUT FOR USE IN RECOMPILING SUBROUTINE XLNKDD)
   WRITE (Lpch,99005)
99005 FORMAT (70(1H*),/38HLINK SPEC. TABLE FOR SUBROUTINE XLNKDD)
   j = lnkbot - lnktop + 5
   n = j/90
   WRITE (Lpch,99006) j
99006 FORMAT (6X,16HDIMENSION LINK (,I4,1H))
   k = 90
   IF ( n/=0 ) THEN
      DO i = 1 , n
         i10 = i/10
         i1 = i - 10*i10
         WRITE (Lpch,99026) i10 , i1 , k
      ENDDO
   ENDIF
   k = mod(j,90)
   i = n + 1
   i10 = i/10
   i1 = i - 10*i10
   IF ( k>0 ) WRITE (Lpch,99026) i10 , i1 , k
   WRITE (Lpch,99007) j
99007 FORMAT (6X,28HCOMMON/XLKSPC/ LLINK, KLINK(,I4,1H),/,6X,34HEQUIVALENCE (LINK(   1),LINK01(1)))
   IF ( k>0 ) n = n + 1
   IF ( n>=2 ) THEN
      DO i = 2 , n
         i10 = i/10
         i1 = i - 10*i10
         k = 90*(i-1) + 1
         WRITE (Lpch,99008) k , i10 , i1
99008    FORMAT (5X,2H1,,11X,6H(LINK(,I4,6H),LINK,2I1,4H(1)))
      ENDDO
   ENDIF
   j = lnktop - 1
   m = 0
   DO
      j = j + 1
      m = m + 1
      m10 = m/10
      m1 = m - 10*m10
      k = min0(j+89,lnkbot+4)
      WRITE (Lpch,99009) m10 , m1 , (lnkspc(i),i=j,k)
99009 FORMAT (6X,9HDATA LINK,2I1,1H/,/,5X,4H1 4H,A4,3H,4H,A4,4H, 4H,A4,3H,4H,A4,1H,,I6,/,(5X,4H1,4H,A4,3H,4H,A4,4H, 4H,A4,3H,4H,A4, &
             &1H,,I6))
      WRITE (Lpch,99010)
99010 FORMAT (5X,2H1/)
      j = k
      IF ( j>=lnkbot+4 ) THEN
         j = lnkbot - lnktop + 5
         WRITE (Lpch,99011) j
99011    FORMAT (6X,8HLLINK = ,I4)
         GOTO 1300
      ENDIF
   ENDDO
 1200 k = i + 4
   n = lnkbot - 1
   DO m = i , k
      n = n + 1
      lnkspc(m) = lnkspc(n)
   ENDDO
   lnkbot = lnkbot - 5
   GOTO 1100
!
!     INITIALIZE PAGE HEADING
!
 1300 DO i = 1 , 32
      Pghdg(i+96) = hdg1(i)
      Pghdg(i+128) = hdg2(i)
      Pghdg(i+160) = nblank
   ENDDO
   Pghdg(113) = Mchnam
   Nlines = Nlpp
!
!     INITIALIZE O/P BUFFER PARAMETERS - O/P BUFFERS ARE IN OPEN CORE
!
   opbtop = lnkbot + 5
   nxtlin = opbtop - 20
!
!     GET FIRST/NEXT MPL ENTRY
!
   Mplpnt = 1
   modidx = 1
!
!     CHECK FOR DECLARATIVE OR NULL ENTRY
!
 1400 IF ( Mpl(Mplpnt+3)<=4 .AND. Mpl(Mplpnt+3)>=1 ) THEN
!
!     PREPARE TO GENERATE NEXT LINE OF OUTPUT
!
      nxtlin = nxtlin + 20
      i = nxtlin + 19
      IF ( i>Lopncr ) THEN
         CALL xgpidg(49,Mplpnt,Mpl(Mplpnt+1),Mpl(Mplpnt+2))
         Xsys(3) = 3
         GOTO 99999
      ELSE
         DO j = nxtlin , i
            opbuff(j) = nblank
         ENDDO
!
!     MODULE INDEX INTO WORD 1 OF O/P ENTRY
!
         opbuff(nxtlin) = modidx
!
!     DMAP NAME TO WORDS 2,3 OF O/P ENTRY
!
         opbuff(nxtlin+1) = Mpl(Mplpnt+1)
         opbuff(nxtlin+2) = Mpl(Mplpnt+2)
!
!     GET ENTRY POINT NAME AND ENTER IN WORDS 4,5 OF O/P ENTRY
!
         opbuff(nxtlin+3) = none(1)
         opbuff(nxtlin+4) = none(2)
         DO i = lnktop , lnkbot , 5
            IF ( lnkspc(i)==Mpl(Mplpnt+1) .AND. lnkspc(i+1)==Mpl(Mplpnt+2) ) GOTO 1600
         ENDDO
      ENDIF
   ENDIF
 1500 IF ( Mpl(Mplpnt)<1 ) GOTO 1700
   Mplpnt = Mplpnt + Mpl(Mplpnt)
   modidx = modidx + 1
   IF ( Mplpnt>=Lmpl ) GOTO 1700
   GOTO 1400
 1600 opbuff(nxtlin+3) = lnkspc(i+2)
   opbuff(nxtlin+4) = lnkspc(i+3)
!
!     EXAMINE LINK FLAG
!
   l = lnkspc(i+4)
   DO j = 1 , Maxlnk
      IF ( andf(l,lshift(1,j-1))/=0 ) THEN
!
!     MODULE IS IN LINK J - SET BIT J IN MAIN LINK TABLE AND O/P BUFFER
!     MAKE SURE LINK TABLE IS LONG ENOUGH.
!
         IF ( Lxlink<modidx ) GOTO 1800
         Mlink(modidx) = orf(Mlink(modidx),lshift(1,j-1))
         k = nxtlin + j + 4
         opbuff(k) = lnkedt(j)
      ENDIF
   ENDDO
   GOTO 1500
!
!     SEE IF O/P BUFFER IS TO BE PRINTED (I.E. SENSE SWITCH 31 IS ON)
!
 1700 CALL sswtch(31,l)
   IF ( l==0 ) THEN
!
!     PRINT O/P BUFFER IF LINK DRIVER PUNCHED O/P  REQUESTED(I.E. SENSE
!     SWITCH 30 IS ON)
!
      CALL sswtch(30,l)
      IF ( l==0 ) RETURN
   ENDIF
   DO i = opbtop , nxtlin , 20
      Nlines = Nlines + 1
      IF ( Nlines>=Nlpp ) CALL page
      j = i + 19
      WRITE (Optap,99012) (opbuff(k),k=i,j)
99012 FORMAT (5X,I6,3X,2A4,4X,2A4,7X,15A4)
   ENDDO
!
!     SEE IF ANY DRIVERS SHOULD BE PUNCHED  (I.E. SENSE SWITCH 30 ON)
!
   CALL sswtch(30,l)
   IF ( l==0 ) RETURN
   CALL page1
   Nlines = Nlines + 2
   WRITE (Optap,99013)
99013 FORMAT ('0USER REQUESTS PUNCHED OUTPUT FOR THE FOLLOWING LINK ','DRIVER SUBROUTINES')
   WRITE (Lpch,99014)
99014 FORMAT (70(1H*),/,' INSERT FOLLOWING FORTRAN CODE IN RESPECTIVE',' LINK DRIVER ROUTINES')
   DO j = 1 , Maxlnk
      CALL sswtch(j,l)
      IF ( l/=0 ) THEN
         j10 = j/10
         j1 = j - 10*j10
         WRITE (Lpch,99015) j10 , j1 , j
99015    FORMAT (70(1H*),/6X,15HSUBROUTINE XSEM,2I1,/6X,12HDATA THISLK,/,I2,1H/)
         WRITE (Lpch,99016) j10 , j1
99016    FORMAT (6X,21HDATA SUBNAM/4HXSEM,4H,2I1,3H  /)
         Nlines = Nlines + 2
         IF ( Nlines>=Nlpp ) CALL page
         WRITE (Optap,99017) j10 , j1
99017    FORMAT (9H0    XSEM,2I1)
!
!     USER REQUESTS PUNCHED O/P FOR LINK J
!     SEARCH LINK TABLE FOR MODULES RESIDING IN LINK J
!
         frstin = 0
         l = 0
         lastin = 0
         nxtgrp = 1000
         DO i = 1 , Lxlink
            ll(l+1) = 940
            IF ( andf(Mlink(i),lshift(1,j-1))/=0 ) ll(l+1) = 2000 + i
            IF ( i<lastin ) THEN
            ELSEIF ( i==lastin ) THEN
               GOTO 1710
            ELSE
               IF ( frstin<=0 .AND. ll(l+1)==940 ) CYCLE
               frstin = i
               lastin = min0(i+180,Lxlink)
               lstgrp = nxtgrp
               nxtgrp = nxtgrp - 5
            ENDIF
            l = l + 1
            IF ( ll(l)/=940 ) THEN
               last = i
               GOTO 1720
            ELSE
!
!     ONLY TWO CONSECUTIVE BRANCHES TO 940 IN COMPUTED  -GO TO -
!
               IF ( last+2>=i ) GOTO 1720
               lastin = last
               l = max0(0,l-1+lastin-i)
            ENDIF
 1710       ll(15) = ll(l+1)
            IF ( l>0 ) GOTO 1730
            GOTO 1740
 1720       IF ( l<13 ) CYCLE
 1730       IF ( frstin==ll(1)-2000 ) WRITE (Lpch,99018) nxtgrp
99018       FORMAT (I5,8H GO TO ()
            lk = min0(l,10)
            WRITE (Lpch,99019) (ll(k),k=1,lk)
99019       FORMAT (5X,1H1,10(I5,1H,))
            l = l - lk
            DO k = 1 , l
               ll(k) = ll(k+10)
            ENDDO
            IF ( i<lastin ) CYCLE
 1740       last = nxtgrp + 15
            IF ( i==Lxlink ) last = 970
            IF ( frstin==lastin ) THEN
               WRITE (Lpch,99020) lstgrp , frstin , ll(15) , last
99020          FORMAT (I5,12H IF (MODX - ,I3,7H ) 940,,I5,1H,,I5)
            ELSE
               frstin = frstin - 1
               WRITE (Lpch,99021) ll(15) , lstgrp , lastin , last , frstin , nxtgrp
99021          FORMAT (5X,1H1,I5,4H ),I,/,I5,14H IF (MODX .GT.,I3,8H) GO TO ,I5,/,6X,10HI = MODX -,I3,/,6X,17HIF (I ) 940, 940,,I5)
            ENDIF
            nxtgrp = last
            frstin = -1
         ENDDO
!
!     PUNCH OUT GO TO AND IF STATEMENTS FOR LAST GROUP OF MODULES IN
!     LINK J.
!
         IF ( frstin/=0 ) THEN
            IF ( last/=970 ) WRITE (Lpch,99022) nxtgrp
99022       FORMAT (I5,34H IF (MODX - LXLINK ) 940, 940, 970)
!
!     SEARCH O/P BUFFER FOR MODULES RESIDING IN LINK J
!
            DO i = opbtop , nxtlin , 20
               k = i + 4 + j
               IF ( opbuff(k)/=nblank ) THEN
!
!     THIS MODULE IS IN LINK J - PUNCH OUT CALL AND GO TO STATEMENT
!
                  n = 2000 + opbuff(i)
                  WRITE (Lpch,99023) n , opbuff(i+3) , opbuff(i+4)
99023             FORMAT (I5,1X,5HCALL ,2A4,/6X,8HGO TO 10)
               ENDIF
            ENDDO
         ELSE
!
!     CANNOT FIND ANY MODULES IN THIS LINK
!
            Nlines = Nlines + 2
            WRITE (Optap,99024) j
99024       FORMAT (1H0,10X,29HTHERE ARE NO MODULES IN LINK ,I3)
         ENDIF
      ENDIF
   ENDDO
   j = Llink/8
   IF ( j>Lxlink ) CALL page2(-3)
   IF ( j>Lxlink ) WRITE (Optap,99025) Swm , j , Lxlink
99025 FORMAT (A27,' 54, THE NUMBER OF MODULES SPECIFIED IN THE LINK ','SPECIFICATION TABLE,',I5,/20X,'EXCEEDS THE ALLOWABLE ',      &
             &'NUMBER SPECIFIED BY SEMDBD,',I5,1H.)
   CALL pexit
   CALL xgpidg(51,lnkbot-Lopncr,0,0)
   Xsys(3) = 3
   GOTO 99999
!
!     NAMED COMMON /XLINK/ IS TOO SMALL
!
 1800 CALL xgpidg(52,0,0,0)
   RETURN
!
!     INCORRECT FORMAT IN ABOVE CARD.
!
 1900 CALL xgpidg(53,0,0,0)
   ASSIGN 800 TO irtn
   GOTO 600
99026 FORMAT (5X,2H1,,9X,4HLINK,2I1,1H(,I4,1H))
99999 END SUBROUTINE xgpibs
