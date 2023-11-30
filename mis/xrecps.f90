
SUBROUTINE xrecps(Inew,Iold)
!
!     ******************************************************************
!     * ATTENTION CDC 6600 SET-UPS ** THESE ENTRY POINTS MAY BE        *
!     * SEPARATED EACH ENTRY MAY BE MADE A SUBROUTINE (EXCEPT /CRDFLG/ *
!     * AND /INTEXT/ WHICH USE COMMON CODE)  DUPE THE SPECIFICATION    *
!     * STMTS FOR EACH SUB                                             *
!     ******************************************************************
!
   IMPLICIT NONE
   INTEGER B , Bimsk1(6) , Bimsk2(5) , Bimsk3(4) , Bimsk4(4) , Bimsk5(2) , Bimsk6 , Bkmsk1(8) , Bkmsk2 , Blank , D1(6) , D2(2) ,    &
         & D3(26) , Dollar , Ibits(1) , Icards(2) , Icon1 , Icon2 , Is , Itwo(32) , Lbd , Lcc , Lcnt , Mach , Mask , Mbit4 , Mk(4) ,&
         & Mka , Nbpc , Nbpw , Ncpw , Nlpp , Num , Outtap , Plus , Sft(4) , Sft1 , Sftm , Shifts(4) , Slash , Star , Starl
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /ifpx0 / Lbd , Lcc , Ibits
   COMMON /ifpx1 / Num , Icards
   COMMON /machin/ Mach
   COMMON /system/ B , Outtap , D1 , Nlpp , D2 , Lcnt , D3 , Nbpc , Nbpw , Ncpw
   COMMON /two   / Itwo
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /xsrtcm/ Bimsk1 , Bimsk2 , Bimsk3 , Bimsk4 , Bimsk5 , Bimsk6 , Bkmsk1 , Bkmsk2 , Shifts , Icon1 , Icon2 , Star , Plus ,   &
                 & Dollar , Starl , Slash , Sftm , Mask , Blank , Mka , Is , Mbit4
   INTEGER Inew , Iold , Kk , Sd
   INTEGER Ba(2) , Bf(1) , Bff(2) , Card(4) , Extwrd(1) , Intwrd(2)
   INTEGER andf , complf , khrfn1 , khrfn3 , lshift , orf , rshift
   INTEGER bfi , c10c(7) , con(38) , extab(37) , exwrdi , i , iba , ichar , icycl , idif , idollr , ii , inwrdi , ipls , ipos ,     &
         & iret , islsh , istr , istrl , itape4 , itemp , itst , ivar , ivax , izero , j , ji , jtemp , k , kard1 , kard2 ,         &
         & kbmsk1(8) , kbrn , kpret1 , kpret2 , lmt , mbits , nrecps(2) , par1 , par2 , sftji , test
   LOGICAL dec
   EXTERNAL andf , complf , lshift , orf , rshift
!
!     ENTRY XFADJ (BF,SD,KK)
!     * XFADJ ADJUSTS 4 CHARACTER FIELDS, LEFT OR RIGHT, 2 OR 4 FIELDS
!       AT A TIME - IF FIELDS CONTAIN ONLY INTEGERS 0 THRU 9, SHIFT IS
!       RIGHT, OTHERWISE SHIFT IS LEFT  / BF= ADDR OF LEFT MOST FIELD /
!       SD= 0 SINGLE (2 FIELDS), 1 DOUBLE (4 FIELDS).  THIS ROUTINE
!       DETERMINES ONLY TYPE OF SHIFT NEEDED, SHIFTING IS DONE BY XFADJ1
!       KK IS RETURNED EQUAL TO 0 FOR INTEGER, 1 FOR NON-INTEGER
!
!
!     ENTRY XBCDBI (BA)
!     * XBCDBI CONVERTS 2, 4 CHARACTER BCD INTEGER FIELDS (RIGHT
!       ADJUSTED IN THE LEFT MOST 4 CHAR) INTO A SINGLE FORTRAN BINARY
!       INTEGER (RIGHT ADJUSTED IN THE WORD IN THE RIGHT FIELD)
!       BA= ADDR OF LEFT FIELD
!
!
!     ENTRY XPRETY (BFF)
!     * ROUTINE PRETTIES UP SORT OUTPUT BY LEFT ADJUSTING ALL FIELDS
!
!
!     ENTRY CRDFLG (CARD)
!     * ROUTINE SETS CARD TYPE FLAGS IN RESTART TABLES
!       CONVERTS TO EXTERNAL CODE FIRST
!       IF CARD TYPE IS PARAM, SET FLAG FOR PARAM NAME (FIELD 2)
!
!
!     ENTRY EXTINT (EXTWRD)
!     * ROUTINE CONVERTS FROM EXTERNAL MACHINE DEPENDENT CHARACTER CODES
!       TO AN INTERNAL MACHINE INDEPENDENT INTEGER
!
!
!     ENTRY INTEXT (INTWRD)
!     * ROUTINE CONVERTS FROM INTERNAL MACHINE INDEPENDENT INTEGERS TO
!       AN EXTERNAL MACHINE DEPENDENT CHARACTER CODE
!
!
   EQUIVALENCE (Sft(1),Shifts(1)) , (Mk(1),Bimsk3(1)) , (Sft1,Shifts(2)) , (extab(1),con(1))
   DATA itape4/304/ , nrecps/4HXREC , 4HPS  /
   DATA con/4H     , 4H   0 , 4H   1 , 4H   2 , 4H   3 , 4H   4 , 4H   5 , 4H   6 , 4H   7 , 4H   8 , 4H   9 , 4H   A , 4H   B ,    &
       &4H   C , 4H   D , 4H   E , 4H   F , 4H   G , 4H   H , 4H   I , 4H   J , 4H   K , 4H   L , 4H   M , 4H   N , 4H   O ,        &
      & 4H   P , 4H   Q , 4H   R , 4H   S , 4H   T , 4H   U , 4H   V , 4H   W , 4H   X , 4H   Y , 4H   Z , 4H    /
   DATA c10c/10 , 100 , 1000 , 10000 , 100000 , 1000000 , 10000000/
   DATA par1 , par2/4HPARA , 4HM   /
   DATA kpret1 , kpret2/4H.    , 4H0.0 /
!
   DATA kbmsk1/4H0000 , 4H000$ , 4H00$$ , 4H0$$$ , 4H$$$  , 4H$$   , 4H$    , 4H    /
   DATA istr , istrl , ipls , idollr , islsh , izero/4H   * , 4H*    , 4H+    , 4H$    , 4H/    , 4H0   /
!
!
!     THE ARRAYS IN /XSRTCM/ WILL BE SET BY INITO AS FOLLOWS
!
!                           VAX
!                    CDC    IBM   UNIVAC
!        SHIFTS(1) =  0      0      0
!        SHIFTS(2) =  6      8      9
!        SHIFTS(3) = 12     16     18
!        SHIFTS(4) = 18     24     27
!        SFTM      = 36      0      0
!
!                      ----------- BYTE --------------
!                      1ST   2ND   3RD   4TH   5TH,...
!        BIMSK1(1) = / 777 / 777 / 777 / 000 / 00..        CDC USES /77/
!        BIMSK1(2) = / 777 / 777 / 000 / 000 / 00..        INSTEAD OF
!        BIMSK1(3) = / 777 / 000 / 000 / 000 / 00..        /777/ IN A
!        BIMSK1(4) = / 000 / 000 / 000 / 777 / 00..        BYTE
!        BIMSK1(5) = / 000 / 000 / 777 / 777 / 00..
!        BIMSK1(6) = / 000 / 777 / 777 / 777 / 00..
!
!        BIMSK2(1) = / 777 / 777 / 777 / 777 / 77.. (FOR CDC ONLY)
!                  = / 377 / 777 / 777 / 777 / 00.. (FOR IBM,VAX,UNIVAC)
!        BIMSK2(2) = / 777 / 777 / 777 / 000 / 77..
!        BIMSK2(3) = / 777 / 777 / 000 / 000 / 77..
!        BIMSK2(4) = / 777 / 000 / 000 / 000 / 77..
!        BIMSK2(5) = / 000 / 000 / 000 / 000 / 77..
!
!        BIMSK3(1) = / 777 / 000 / 000 / 000 / 00..
!        BIMSK3(2) = / 000 / 777 / 000 / 000 / 00..
!        BIMSK3(3) = / 000 / 000 / 777 / 000 / 00..
!        BIMSK3(4) = / 000 / 000 / 000 / 777 / 00..
!
!        BIMSK4(1) = / 000 / 777 / 777 / 777 / 77..
!        BIMSK4(2) = / 777 / 000 / 777 / 777 / 77..
!        BIMSK4(3) = / 777 / 777 / 000 / 777 / 77..
!        BIMSK4(4) = / 777 / 777 / 777 / 000 / 77..
!
!        BIMSK5(1) = / 377 / 777 / 777 / 777 / 00..
!        BIMSK5(2) = / 377 / 777 / 777 / 000 / 00..
!        BIMSK6    = / 000 / 000 / 000 / 000 / 77..
!
!        IS        = / 400 / 000 / 000 / 000 / 77..
!        MKA       = / 000 / 000 / 000 / 777 / 77..
!        MASK      = 4TH OR 10TH BYTE IS /777/, ZERO FILLED
!        BLANK     = 4TH OR 10TH BYTE IS BLANK, ZERO FILLED
!
!     ARRAY BKMSK1 IS SAME AS KBMSK1 EXCEPT THAT THE DOLLARS ARE
!     REPLACED BY BINARY ZEROS
!     SIMILARY, THE BLANKS IN ISTR,ISTRL,IPLS,IDOLLR,ISLSH, AND ARRAY
!     CON ARE ALSO REPLACED BY BINARY ZEROS.
!     ICON1 AND ICON2 ARE LEFT ADJUSTED CON(1) AND CON(2), ZERO FILLED.
!
!     THIS ROUTINE POSITIONS ITAPE4 TO THE PROPER CONTINUATION RECORD
!
   dec = Mach==5 .OR. Mach==6 .OR. Mach==21
   IF ( Inew/=1 ) THEN
!
      idif = Inew - Iold
      IF ( idif<0 ) THEN
         idif = iabs(idif)
         DO i = 1 , idif
            CALL bckrec(itape4)
         ENDDO
      ELSEIF ( idif/=0 ) THEN
!
         DO i = 1 , idif
            CALL fwdrec(*100,itape4)
         ENDDO
      ENDIF
      Iold = Inew + 1
      RETURN
   ELSE
      CALL rewind(itape4)
      Iold = 2
      RETURN
   ENDIF
 100  WRITE (Outtap,99001) Sfm
99001 FORMAT (A25,' 217, ILLEGAL EOF ON ITAPE4.')
   CALL mesage(-37,0,nrecps)
   RETURN
!
!     INITIALIZES BCD CONSTANTS FOR USE WITHIN SORT
!
   ENTRY initco
!     ============
!
!     INITIALIZE (CREATE) BINARY CHARACTER MASKS
!
   dec = Mach==5 .OR. Mach==6 .OR. Mach==21
   Shifts(1) = 0
   Shifts(2) = Nbpc
   Shifts(3) = Nbpc*2
   Shifts(4) = Nbpc*3
   mbits = complf(0)
   Sftm = (Ncpw-4)*Nbpc
   Mbit4 = lshift(mbits,Sftm)
   Bimsk1(1) = lshift(Mbit4,Nbpc)
   Bimsk1(2) = lshift(Bimsk1(1),Nbpc)
   Bimsk1(3) = lshift(Bimsk1(2),Nbpc)
   Bimsk1(4) = rshift(Bimsk1(3),Nbpc*3)
   Bimsk1(5) = rshift(Bimsk1(2),Nbpc*2)
   Bimsk1(6) = rshift(Bimsk1(1),Nbpc)
   Bimsk2(1) = mbits
   Bimsk2(2) = complf(Bimsk1(4))
   Bimsk2(3) = complf(Bimsk1(5))
   Bimsk2(4) = complf(Bimsk1(6))
   Bimsk2(5) = rshift(mbits,Nbpc*4)
   Bimsk3(4) = Bimsk1(4)
   Bimsk3(3) = lshift(Bimsk3(4),Nbpc)
   Bimsk3(2) = lshift(Bimsk3(3),Nbpc)
   Bimsk3(1) = Bimsk1(3)
   Bimsk4(1) = complf(Bimsk3(1))
   Bimsk4(2) = complf(Bimsk3(2))
   Bimsk4(3) = complf(Bimsk3(3))
   Bimsk4(4) = complf(Bimsk3(4))
   Bimsk5(1) = rshift(Bimsk2(1),1)
   Bimsk5(2) = rshift(lshift(Bimsk2(2),1),1)
   Bimsk6 = Bimsk2(5)
   IF ( Mach==2 .OR. dec ) Bimsk2(1) = Bimsk5(1)
!
!     NEXT CARD FOR UNIVAC ASCII VERSION ONLY (NOT FORTRAN 5)
!
   IF ( Mach==3 ) Bimsk2(1) = Bimsk5(1)
   Mask = rshift(Bimsk3(4),Sftm)
   Blank = rshift(kbmsk1(8),(3*Nbpc+Sftm))
   Is = complf(Bimsk5(1))
   Mka = orf(Bimsk3(4),Bimsk6)
!
!     INITIALIZE THE BCD BLANK DATA
!
   IF ( dec ) THEN
!
!     VAX
!
      Bkmsk2 = 0
      Bkmsk1(1) = kbmsk1(1)
      Bkmsk1(2) = khrfn3(Bkmsk2,kbmsk1(2),-1,1)
      Bkmsk1(3) = khrfn3(Bkmsk2,kbmsk1(3),-2,1)
      Bkmsk1(4) = khrfn3(Bkmsk2,kbmsk1(4),-3,1)
      Bkmsk1(5) = khrfn3(Bkmsk2,kbmsk1(5),-3,0)
      Bkmsk1(6) = khrfn3(Bkmsk2,kbmsk1(6),-2,0)
      Bkmsk1(7) = khrfn3(Bkmsk2,kbmsk1(7),-1,0)
      Bkmsk1(8) = kbmsk1(8)
      Star = khrfn1(Bkmsk2,4,istr,4)
      Plus = khrfn1(Bkmsk2,1,ipls,1)
      Dollar = khrfn1(Bkmsk2,1,idollr,1)
      Starl = khrfn1(Bkmsk2,1,istrl,1)
      Slash = khrfn1(Bkmsk2,1,islsh,1)
      DO i = 1 , 38
         con(i) = khrfn1(Bkmsk2,4,con(i),4)
      ENDDO
      Icon1 = rshift(khrfn1(Bkmsk2,1,con(1),4),1)
      Icon2 = rshift(khrfn1(Bkmsk2,1,con(2),4),1)
      RETURN
   ELSE
!
!     IBM, CDC, UNIVAC
!
      Bkmsk1(1) = kbmsk1(1)
      Bkmsk1(2) = andf(kbmsk1(2),Bimsk2(2))
      Bkmsk1(3) = andf(kbmsk1(3),Bimsk2(3))
      Bkmsk1(4) = andf(kbmsk1(4),Bimsk2(4))
      Bkmsk1(5) = andf(kbmsk1(5),orf(Bimsk1(4),Bimsk6))
      Bkmsk1(6) = andf(kbmsk1(6),orf(Bimsk1(5),Bimsk6))
      Bkmsk1(7) = andf(kbmsk1(7),orf(Bimsk1(6),Bimsk6))
      Bkmsk1(8) = kbmsk1(8)
      Bkmsk2 = andf(Bkmsk1(1),Bimsk6)
      Star = andf(istr,orf(Bimsk1(4),Bimsk6))
      Plus = andf(ipls,Bimsk2(4))
      Dollar = andf(idollr,Bimsk2(4))
      Starl = andf(istrl,Bimsk2(4))
      Slash = andf(islsh,Bimsk2(4))
      DO i = 1 , 38
         con(i) = andf(con(i),Bimsk3(4))
      ENDDO
      Icon1 = lshift(con(1),Sft(4)-1)
      Icon2 = lshift(con(2),Sft(4)-1)
      RETURN
   ENDIF
!
!
   ENTRY xfadj(Bf,Sd,Kk)
!     ======================
!
!     DATA SFT /0,6,12,18/
!     DATA MK  /O770000000000,O007700000000,O000077000000,O000000770000/
!
   dec = Mach==5 .OR. Mach==6 .OR. Mach==21
   ii = 2
   IF ( Sd==1 ) ii = 4
   DO i = 1 , ii
      bfi = Bf(i)
      DO j = 1 , 4
         ji = 5 - j
         IF ( .NOT.dec ) test = rshift(andf(bfi,Mk(j)),Sft(ji))
         IF ( dec ) test = khrfn1(Bkmsk2,4,bfi,j)
         DO k = 1 , 11
            IF ( test==con(k) ) GOTO 120
         ENDDO
!
!     CHARACTER NON-INTEGER
!
         CALL xfadj1(Bf,lshift,Sd)
         Kk = 1
         RETURN
!
 120     IF ( k/=1 ) THEN
!
!     CHARACTER INTEGER
!
            CALL xfadj1(Bf,rshift,Sd)
            Kk = 0
            RETURN
         ENDIF
!
      ENDDO
   ENDDO
!
!     ALL FIELDS BLANK
!
   Kk = 0
   RETURN
!
!
   ENTRY xbcdbi(Ba)
!     =================
!
!     DATA SFT1/6/,SFTM/12/,MASK/O77/,BLANK/O60/
!
!     IF MACHINE IS VAX-11/780, ORDER OF CHARACTERS IN A WORD IS REVERSE
!     OF THAT ON OTHER MACHINES.  THE CHARACTER ORDER MUST THEREFORE BE
!     REVERSED BEFORE DECODING TO AN INTEGER VALUE.
!
   dec = Mach==5 .OR. Mach==6 .OR. Mach==21
   IF ( dec ) THEN
      DO iba = 1 , 2
         itemp = 0
         DO ivax = 1 , 4
            jtemp = rshift(Ba(iba),8*(ivax-1))
            jtemp = andf(Mask,jtemp)
            jtemp = lshift(jtemp,8*(4-ivax))
            itemp = orf(itemp,jtemp)
         ENDDO
         Ba(iba) = itemp
      ENDDO
   ENDIF
!
   Ba(1) = rshift(Ba(1),Sftm)
   Ba(2) = rshift(Ba(2),Sftm)
   ivar = andf(Ba(2),Mask)
   IF ( ivar/=Blank ) THEN
!
      IF ( Mach==4 ) ivar = ivar - 27
      ivar = andf(ivar,15)
      DO i = 1 , 3
         Ba(2) = rshift(Ba(2),Sft1)
         ichar = andf(Ba(2),Mask)
         IF ( Mach==4 ) ichar = ichar - 27
         ivar = ivar + c10c(i)*andf(15,ichar)
      ENDDO
      ichar = andf(Ba(1),Mask)
      IF ( Mach==4 ) ichar = ichar - 27
      ivar = ivar + c10c(4)*andf(15,ichar)
      DO i = 5 , 7
         Ba(1) = rshift(Ba(1),Sft1)
         ichar = andf(Ba(1),Mask)
         IF ( Mach==4 ) ichar = ichar - 27
         ivar = ivar + c10c(i)*andf(15,ichar)
      ENDDO
      Ba(2) = ivar
      RETURN
   ELSE
      Ba(2) = 0
      RETURN
   ENDIF
!
!
   ENTRY xprety(Bff)
!     ==================
!
!     DATA  MKA/O000000777777/, STAR/4H000*/
!
   dec = Mach==5 .OR. Mach==6 .OR. Mach==21
   IF ( .NOT.dec ) itst = andf(Mka,Bff(2))
   IF ( dec ) itst = khrfn1(Bkmsk2,4,Bff(2),4)
   IF ( itst==Star ) THEN
!
      DO i = 3 , 15 , 4
         IF ( Bff(i)/=Bkmsk1(8) .OR. Bff(i+1)/=Bkmsk1(8) .OR. Bff(i+2)/=Bkmsk1(8) .OR. Bff(i+3)/=Bkmsk1(8) ) THEN
            CALL xfadj1(Bff(i),lshift,1)
            IF ( Bff(i)==kpret1 ) Bff(i) = kpret2
            IF ( Bff(i)==Bkmsk1(8) ) THEN
               IF ( .NOT.dec ) Bff(i) = orf(rshift(Bff(i),Sft(2)),Bkmsk1(4))
               IF ( dec ) Bff(i) = khrfn3(izero,Bff(i),1,0)
            ENDIF
         ENDIF
      ENDDO
      RETURN
   ELSE
      DO i = 3 , 17 , 2
         IF ( Bff(i)/=Bkmsk1(8) .OR. Bff(i+1)/=Bkmsk1(8) ) THEN
            CALL xfadj1(Bff(i),lshift,0)
            IF ( Bff(i)==kpret1 ) Bff(i) = kpret2
            IF ( Bff(i)==Bkmsk1(8) ) THEN
               IF ( .NOT.dec ) Bff(i) = orf(rshift(Bff(i),Sft(2)),Bkmsk1(4))
               IF ( dec ) Bff(i) = khrfn3(izero,Bff(i),1,0)
            ENDIF
         ENDIF
      ENDDO
      RETURN
   ENDIF
!
!
   ENTRY crdflg(Card)
!     ===================
!
   dec = Mach==5 .OR. Mach==6 .OR. Mach==21
   inwrdi = Card(1)
   kard2 = Card(2)
   kbrn = -1
   ASSIGN 200 TO iret
   GOTO 400
 200  IF ( .NOT.dec ) kard2 = orf(andf(Bimsk1(1),kard2),Bkmsk1(5))
   IF ( dec ) kard2 = khrfn1(kard2,4,Bkmsk1(8),4)
   IF ( kard1==par1 .AND. kard2==par2 ) THEN
      kard1 = Card(3)
      kard2 = Card(4)
   ENDIF
   lmt = Num*2
   DO i = 1 , lmt , 2
      IF ( kard1==Icards(i) .AND. kard2==Icards(i+1) ) GOTO 300
   ENDDO
   RETURN
!
 300  j = i/2
   icycl = (j/31) + 1
   ipos = mod(j,31) + 2
   Ibits(icycl) = orf(Ibits(icycl),Itwo(ipos))
   RETURN
!
!
   ENTRY extint(Extwrd)
!     =====================
!
   dec = Mach==5 .OR. Mach==6 .OR. Mach==21
   DO i = 1 , 2
      exwrdi = Extwrd(i)
      DO j = 1 , 4
         ji = 5 - j
         sftji = Sft(ji)
         IF ( .NOT.dec ) test = rshift(andf(exwrdi,Mk(j)),sftji)
         IF ( dec ) test = khrfn1(Bkmsk2,4,exwrdi,j)
         DO k = 1 , 37
            IF ( test==extab(k) ) GOTO 320
         ENDDO
         k = 1
         EXIT
 320     IF ( .NOT.dec ) exwrdi = orf(andf(exwrdi,Bimsk4(j)),lshift(k,sftji+Sftm))
         IF ( dec ) exwrdi = khrfn1(exwrdi,j,k,-1)
         IF ( k==1 ) EXIT
      ENDDO
      Extwrd(i) = exwrdi
      IF ( k==1 ) RETURN
   ENDDO
   RETURN
!
!
   ENTRY intext(Intwrd)
!     =====================
!
   dec = Mach==5 .OR. Mach==6 .OR. Mach==21
   ASSIGN 600 TO iret
   inwrdi = Intwrd(1)
   kbrn = 0
 400  DO j = 1 , 4
      ji = 5 - j
      sftji = Sft(ji)
      IF ( .NOT.dec ) test = rshift(andf(inwrdi,Mk(j)),sftji+Sftm)
      IF ( dec ) test = khrfn1(Bkmsk2,-1,inwrdi,j)
      IF ( test>37 ) EXIT
      IF ( .NOT.dec ) inwrdi = orf(andf(inwrdi,Bimsk4(j)),lshift(extab(test),sftji))
      IF ( dec ) inwrdi = khrfn1(inwrdi,j,extab(test),4)
      IF ( test==1 ) EXIT
   ENDDO
   IF ( kbrn<0 ) THEN
      kard1 = inwrdi
      inwrdi = Card(2)
      kbrn = +2
   ELSEIF ( kbrn==0 ) THEN
      Intwrd(1) = inwrdi
      inwrdi = Intwrd(2)
      kbrn = +1
   ELSE
      IF ( kbrn==1 ) THEN
         Intwrd(2) = inwrdi
      ELSE
         kard2 = inwrdi
      ENDIF
      GOTO 500
   ENDIF
!
   IF ( test/=1 .AND. test<=37 ) GOTO 400
 500  GOTO iret
 600  RETURN
!
END SUBROUTINE xrecps
