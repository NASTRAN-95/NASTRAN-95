
SUBROUTINE ifp1xy(Card,Xycard)
!
!     THIS ROUTINE PROCESSES THE XRCARD IMAGES OF THE XY-PLOT CONTROL
!     CARDS AND CREATES THE -XYCDB- FILE WHICH IS OPENED AND CLOSED BY
!     THE CALLING ROUTINE.
!
!     THE ARGUMENT -CARD- IS = 1 ON THE FIRST CALL TO THIS ROUTINE
!                            = 0 ON OTHER CALLS WHEN AN IMAGE IS SENT
!                            =-1 ON LAST CALL AND NO IMAGE IS SENT
!
!     TWO RECORDS WILL BE FORMED BY THIS ROUTINE.
!     THE FIRST RECORD HAS XY-PLOT, XY-PRINT, AND XY-PUNCH DATA AND IS
!     USED BY THE XYTRAN MODULE.
!     THE SECOND RECORD IS A SORTED NX6 MATRIX STORED BY ROWS. EACH ROW
!     CONTAINS THE FOLLOWING.
!
!          1 - SUBCASE ID OR 0 INDICATING ALL.
!          2 - VECTOR CODE NUMBER E.G. DISP,STRESS,SPCF, ETC.
!          3 - POINT OR ELEMENT ID NUMBER.
!          4 - COMPONENT NUMBER.
!          5 - TYPE OF PLOT (1=RESP,2=AUTO,3=PSDF)
!          6 - DESTINATION CODE 1-7 (BIT1=PRINT,BIT2=PLOT,BIT3=PUNCH).
!              CODE 8 ADDED - BIT 4 PAPERPLOT
!
   IMPLICIT NONE
   INTEGER A377 , Bits(1) , Blank , Buff(150) , Corex(1) , Corey(771) , Dum3(3) , Dummy(11) , Icse(400) , Ihalf , Ilink , Incard(20)&
         & , Intr , Iresrt , Ksystm(21) , L , Lbd , Lcc , Line , Mach , Nlpp , Nogo , Nwpc , Skp63(63) , Subcas(200) , Sysbuf , Z(1)
   LOGICAL Bit64
   CHARACTER*23 Ufm
   COMMON /ifp1a / Dum3 , Nwpc , Dummy , A377
   COMMON /ifpx0 / Lbd , Lcc , Bits
   COMMON /machin/ Mach , Ihalf
   COMMON /system/ Ksystm , Ilink , Skp63 , Intr
   COMMON /xifp1 / Blank , Bit64
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Corex
   INTEGER Card
   INTEGER Xycard(1)
   INTEGER a777 , acce , auto , bcd , binplt , bitwrd , buf(10) , bword(16) , clea , compon , cont , destin , disp , elfo , elst ,  &
         & eor , f , file , forc , fram , g , i , iat , icont , icore , icrq , idcom , iden , iequal , ilnk , imodel , inte ,       &
         & iword(26) , iwrd , j , kword(3) , load , model , modid(2) , n , n1 , n2 , nbword , ncurve , niword , nmod , no , noeor , &
         & nonl , nrword , nsubs , oloa , oparen , paplot , plot , ploter , plots , plt1 , plt2 , print , psdf , punch , r1 , r1ip ,&
         & r1rm , r2 , r2ip , r2rm , r3 , r3ip , r3rm , real , resp , rword(14) , sacc , sdis , sdrbit , slas , spcf , stre
   INTEGER andf , complf , korsz , lshift , orf , rshift
   LOGICAL contin , ofbcd , pairs , slash , xycm
   INTEGER subc , svel , t1 , t1ip , t1rm , t2 , t2ip , t2rm , t3 , t3ip , t3rm , tcur , thru , type , vdrbit , vdum , vect ,       &
         & vector , velo , vg , xtit , xtype , xvect , xy , xypa , xype , xypl , xypr , xypu , ybti , yes , ytit , ytti
   LOGICAL tapbit
   EXTERNAL andf , complf , lshift , orf , rshift
!     COMMON /ZZIFP1/ ICSE(400),INCARD(20),BUFF(150),SUBCAS(200),Z(1)
   EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),L) , (Ksystm(3),Nogo) , (Ksystm(9),Nlpp) , (Ksystm(12),Line) , (Ksystm(21),Iresrt) , &
    & (Icse(1),Corex(1),Corey(1)) , (Incard(1),Corey(401)) , (Buff(1),Corey(421)) , (Subcas(1),Corey(571)) , (Z(1),Corey(771))
   DATA nrword/14/ , niword/26/ , nbword/16/
   DATA ilnk/4HNS01/
   DATA kword/4HFILM , 4HPAPE , 4HBOTH/
   DATA rword/4HXMIN , 4HXMAX , 4HYMIN , 4HYMAX , 4HYTMI , 4HYTMA , 4HYBMI , 4HYBMA , 4HYINT , 4HXINT , 4HYTIN , 4HYBIN , 4HXPAP ,  &
       &4HYPAP/
   DATA iword/4HXDIV , 4HYDIV , 4HYTDI , 4HYBDI , 4HXVAL , 4HYVAL , 4HYTVA , 4HYBVA , 4HUPPE , 4HLOWE , 4HLEFT , 4HRIGH , 4HTLEF ,  &
       &4HTRIG , 4HBLEF , 4HBRIG , 4HALLE , 4HTALL , 4HBALL , 4HCURV , 4HDENS , 4HCAME , 4HPENS , 4HSKIP , 4HCSCA , 4HCOLO/
   DATA bword/4HXAXI , 4HYAXI , 4HXTAX , 4HXBAX , 4HXLOG , 4HYLOG , 4HYTLO , 4HYBLO , 4HXGRI , 4HYGRI , 4HXTGR , 4HXBGR , 4HPLOT ,  &
       &4HYTGR , 4HYBGR , 4HLONG/
   DATA clea/4HCLEA/ , yes/4HYES / , no/4HNO  / , t1/4HT1  / , r1/4HR1  / , t1rm/4HT1RM/ , t2/4HT2  / , r2/4HR2  / , t2rm/4HT2RM/ , &
      & t3/4HT3  / , r3/4HR3  / , t3rm/4HT3RM/ , t1ip/4HT1IP/ , r1rm/4HR1RM/ , r1ip/4HR1IP/ , t2ip/4HT2IP/ , r2rm/4HR2RM/ ,         &
       &r2ip/4HR2IP/ , t3ip/4HT3IP/ , r3rm/4HR3RM/ , r3ip/4HR3IP/ , xypl/4HXYPL/ , xypu/4HXYPU/ , xypr/4HXYPR/ , slas/4H/   / ,     &
      & thru/4HTHRU/ , fram/4HFRAM/ , xy/4HXY  / , auto/4HAUTO/ , resp/4HRESP/
   DATA psdf/4HPSDF/ , vdum/4HVDUM/ , disp/4HDISP/ , velo/4HVELO/ , svel/4HSVEL/ , elst/4HELST/ , acce/4HACCE/ , spcf/4HSPCF/ ,     &
      & sacc/4HSACC/ , oloa/4HOLOA/ , load/4HLOAD/ , stre/4HSTRE/ , nonl/4HNONL/ , subc/4HSUBC/ , forc/4HFORC/ , sdis/4HSDIS/ ,     &
      & elfo/4HELFO/ , xtit/4HXTIT/ , ytit/4HYTIT/ , ytti/4HYTTI/ , tcur/4HTCUR/ , ybti/4HYBTI/ , xype/4HXYPE/ , vect/4HVECT/ ,     &
      & plt1/4HPLT1/ , plt2/4HPLT2/ , eor/1/ , xypa/4HXYPA/ , xycm/.FALSE./ , noeor/0/
   DATA iden/4HDENS/ , iequal/4H=   / , oparen/4H(   / , file/4HXYCD/ , vg/2HVG/ , imodel/4HMODE/ , real/ - 2/ , inte/ - 1/ ,       &
      & cont/0/ , g/1HG/ , f/1HF/
!
   bitwrd = Lbd + 1
   n = 1
   IF ( Intr>1 .OR. Ilink/=ilnk ) THEN
      Incard(1) = Xycard(1)
      CALL xrcard(Buff,149,Xycard)
      Buff(150) = rshift(complf(0),1)
      A377 = Buff(150)
      file = 301
   ENDIF
   IF ( Card<0 ) THEN
!
!     NO MORE CARDS AVAILABLE. RAP IT UP IF NO ERROR. WRITE XY-SET
!     RECORD
!
      IF ( contin ) THEN
         j = 689
         WRITE (L,99001) Ufm , j
99001    FORMAT (A23,I4,', LAST CARD ENDED WITH A DELIMITER BUT NO ','CONTINUATION CARD WAS PRESENT.')
         GOTO 3200
      ELSE
         CALL write(file,Z(1),0,eor)
         IF ( iat/=0 ) THEN
            j = 7
            DO i = 1 , 6
               j = j - 1
               CALL sort(0,0,6,-j,Z(1),iat)
            ENDDO
         ENDIF
         CALL write(file,Z(1),iat,eor)
!
!     SET CARD = 0 IF NO PLOTS
!     SET CARD = 1 IF PLOTS
!
         Card = plots
!
!     SET RESTART BITS FOR VDR AND SDR
!
         IF ( Iresrt<0 ) Bits(bitwrd) = orf(Bits(bitwrd),vdrbit+sdrbit)
!
!     CHECK FOR COMMAND OP CARD
!
         IF ( .NOT.xycm ) THEN
            j = 697
            WRITE (L,99002) Ufm , j
99002       FORMAT (A23,I4,', XYPLOT, XYPRINT, XYPUNCH, XYPEAK, OR XYPAPLOT',/5X,                                                   &
                   &' COMMAND CARD NOT FOUND IN XY PLOTTER OUT PUT PACKAGE.')
            GOTO 3200
         ELSE
!
!     CHECK PLOT TAPE BITS
!
            IF ( plots==0 ) RETURN
!
!     CHECK FOR TAPE SETUPS
!
            IF ( binplt/=0 .AND. .NOT.tapbit(plt1) .AND. .NOT.tapbit(plt2) ) CALL ifp1d(-618)
            RETURN
         ENDIF
      ENDIF
   ELSEIF ( Card/=0 ) THEN
!
!     FIRST CALL AND FIRST CARD IMAGE.
!
      iat = 0
      Card = 0
      plots = 0
      ploter = 0
      sdrbit = 0
      vdrbit = 0
      binplt = 0
      contin = .FALSE.
      a777 = complf(0)
      icore = korsz(Z) - 2*Sysbuf - Nwpc - 1
   ENDIF
!
!     RETURNING WITH ANOTHER CARD IMAGE
!
   IF ( Buff(n)==A377 ) RETURN
!
   IF ( .NOT.contin ) THEN
!
!     BEGIN PROCESSING NON-CONTINUATION CARD (MUST BEGIN WITH BCD FIELD)
!
      iwrd = Incard(1)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,iwrd,0)
      IF ( iwrd==xtit .OR. iwrd==ytit .OR. iwrd==ytti .OR. iwrd==ybti .OR. iwrd==tcur ) THEN
!
!     TITLE CARD
!
         CALL write(file,Incard(1),1,noeor)
         CALL write(file,Buff(1),32,noeor)
         RETURN
      ELSE
         IF ( Buff(n)==0 ) RETURN
!
         IF ( Buff(n)<0 ) THEN
!
!     FATAL ERROR CONDITIONS
!
            j = 675
            WRITE (L,99003) Ufm , j
99003       FORMAT (A23,I4,', ABOVE CARD DOES NOT BEGIN WITH A NON-NUMERIC ','WORD.')
            GOTO 3200
         ELSE
            bcd = Buff(n+1)
            IF ( Bit64 ) CALL mvbits(Blank,0,32,bcd,0)
            DO i = 1 , nrword
               IF ( bcd==rword(i) ) GOTO 300
            ENDDO
!
            DO i = 1 , niword
               IF ( bcd==iword(i) ) GOTO 10
            ENDDO
!
            DO i = 1 , nbword
               IF ( bcd==bword(i) ) GOTO 400
            ENDDO
!
            IF ( bcd==clea .OR. bcd==vdum ) THEN
!
               CALL write(file,bcd,1,noeor)
               RETURN
            ELSE
               IF ( bcd/=xype .AND. bcd/=xypl .AND. bcd/=xypr .AND. bcd/=xypu .AND. bcd/=xypa ) GOTO 2200
!
!     PRINT, PLOT, OR PUNCH COMMAND CARD
!
               xtype = 0
               type = 0
               xvect = 0
               vector = 0
               print = 0
               plot = 0
               punch = 0
               paplot = 0
               slash = .FALSE.
               n1 = 2
               n2 = 2*Buff(n) + n
!
!     PROCESS ALL WORDS
!
               DO i = n1 , n2 , 2
                  bcd = Buff(i)
                  IF ( bcd/=a777 ) THEN
                     IF ( Bit64 ) CALL mvbits(Blank,0,32,bcd,0)
                     IF ( bcd==xypl ) THEN
                        plot = 2
                        plots = 1
                        IF ( ploter==0 ) THEN
                           ploter = 1
                           model = -1
                           buf(1) = bword(13)
                           buf(2) = orf(lshift(ploter,Ihalf),model+100)
                           binplt = binplt + 1
                           CALL write(file,buf(1),2,noeor)
                        ENDIF
                     ELSEIF ( bcd==xypr ) THEN
                        print = 1
                     ELSEIF ( bcd==xypu ) THEN
                        punch = 4
                     ELSEIF ( bcd/=xype ) THEN
                        IF ( bcd==xypa ) THEN
                           paplot = 1
                           GOTO 4
                        ELSE
                           IF ( bcd==resp ) THEN
                              type = 1
                           ELSEIF ( bcd==auto ) THEN
                              type = 3
                           ELSEIF ( bcd==psdf ) THEN
                              type = 2
                           ELSE
                              IF ( bcd==subc ) CYCLE
                              IF ( bcd==disp ) THEN
                                 vector = 1
                                 sdrbit = 16
                                 GOTO 2
                              ELSEIF ( bcd==vect ) THEN
                                 vector = 1
                                 sdrbit = 16
                                 GOTO 2
                              ELSEIF ( bcd==velo ) THEN
                                 vector = 2
                                 sdrbit = 16
                                 GOTO 2
                              ELSEIF ( bcd==acce ) THEN
                                 vector = 3
                                 sdrbit = 16
                                 GOTO 2
                              ELSEIF ( bcd==spcf ) THEN
                                 vector = 4
                                 sdrbit = 16
                                 GOTO 2
                              ELSEIF ( bcd==load ) THEN
                                 vector = 5
                                 sdrbit = 16
                                 GOTO 2
                              ELSEIF ( bcd==stre ) THEN
                                 vector = 6
                                 sdrbit = 16
                                 GOTO 2
                              ELSEIF ( bcd==forc ) THEN
                                 vector = 7
                                 sdrbit = 16
                                 GOTO 2
                              ELSEIF ( bcd==sdis ) THEN
                                 vector = 8
                                 vdrbit = 2
                                 GOTO 2
                              ELSEIF ( bcd==svel ) THEN
                                 vector = 9
                                 vdrbit = 2
                                 GOTO 2
                              ELSEIF ( bcd==sacc ) THEN
                                 vector = 10
                                 vdrbit = 2
                                 GOTO 2
                              ELSEIF ( bcd==nonl ) THEN
                                 vector = 11
                                 vdrbit = 2
                                 GOTO 2
                              ELSEIF ( bcd==elfo ) THEN
                                 vector = 7
                                 sdrbit = 16
                                 GOTO 2
                              ELSEIF ( bcd==elst ) THEN
                                 vector = 6
                                 sdrbit = 16
                                 GOTO 2
                              ELSEIF ( bcd==oloa ) THEN
                                 vector = 5
                                 sdrbit = 16
                                 GOTO 2
                              ELSEIF ( bcd==vg ) THEN
                                 vector = 5
                                 sdrbit = 16
                                 GOTO 2
                              ELSE
                                 n = i - 1
                                 GOTO 2200
                              ENDIF
                           ENDIF
                           IF ( xtype/=0 ) GOTO 2400
                           xtype = 1
                           CYCLE
                        ENDIF
 2                      IF ( xvect/=0 ) GOTO 2400
                        xvect = 1
                        CYCLE
                     ENDIF
 4                   xycm = .TRUE.
!
!     DELIMETER HIT OF SOME KIND. IGNORE IF NOT LAST WORD OF BCD GROUP.
!
                  ELSEIF ( i==n2-1 ) THEN
                     iwrd = Buff(i+1)
                     IF ( Bit64 ) CALL mvbits(Blank,0,32,iwrd,0)
                     IF ( iwrd==slas ) slash = .TRUE.
                     IF ( .NOT.slash ) GOTO 2500
                  ENDIF
               ENDDO
!
!     WRITE PLOT CONTROL INFORMATION
!
               buf(1) = xy
               buf(2) = print
               buf(3) = plot
               buf(4) = punch
               IF ( paplot==1 ) plot = 2
               destin = print + plot + punch
               IF ( type==0 ) type = 1
               buf(5) = type
               IF ( vector==0 ) THEN
                  j = 690
                  WRITE (L,99004) Ufm , j
99004             FORMAT (A23,I4,', TYPE OF CURVE WAS NOT SPECIFIED. (E.G. ','DISPLACEMENT, STRESS, ETC.).')
                  GOTO 3200
               ELSE
                  buf(6) = vector
                  buf(7) = paplot
                  CALL write(file,buf(1),7,noeor)
!
!     ALL WORDS PROCESSED. IF SLASH HAS NOT BEEN HIT, START READING
!     SUBCASE NUMBERS.
!
                  nsubs = 0
                  n = n2 + 1
                  IF ( .NOT.(slash) ) GOTO 500
                  GOTO 1000
               ENDIF
            ENDIF
!
!     VERB FOLLOWED BY AN INTEGER VALUE
!     ON CAMERA CARD BCD ALSO ACCEPTED
!
 10         n = n + 2*Buff(n) + 1
            IF ( i==22 .AND. Buff(n)/=inte ) THEN
!
               iwrd = Buff(n-2)
               IF ( Bit64 ) CALL mvbits(Blank,0,32,iwrd,0)
               DO i = 1 , 3
                  IF ( iwrd==kword(i) ) THEN
                     Buff(n+1) = i
                     GOTO 100
                  ENDIF
               ENDDO
               GOTO 2300
            ELSE
               IF ( Buff(n)/=inte ) GOTO 2300
               IF ( i==26 ) THEN
!
                  buf(1) = bcd
                  buf(2) = Buff(n+1)
                  buf(3) = Buff(n+3)
                  CALL write(file,buf(1),3,noeor)
                  RETURN
               ELSEIF ( Buff(n+1)<0 .OR. i>8 ) THEN
                  IF ( i<=8 ) GOTO 2300
                  IF ( Buff(n+1)/=0 .AND. i<=19 ) Buff(n+1) = Buff(n+1)/iabs(Buff(n+1))
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ELSE
      contin = .FALSE.
      GOTO icont
   ENDIF
 100  buf(1) = bcd
   buf(2) = Buff(n+1)
 200  CALL write(file,buf(1),2,noeor)
   RETURN
!
!     VERB FOLLOWED BY A REAL VALUE
!
 300  n = n + 2*Buff(n) + 1
   IF ( Buff(n)==real ) GOTO 100
   GOTO 2300
!
!     VERB FOLLOWED BY BCD YES OR NO, UNLESS BCD = PLOT...
!
 400  IF ( i==13 ) THEN
!
!     PLOTTER SPECIFICATION CARD LOGIC
!
      IF ( Buff(n+3)==a777 ) n = n + 2
      n = n + 2
      nmod = n + 3
      iwrd = Buff(nmod)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,iwrd,0)
      IF ( iwrd==imodel ) nmod = nmod + 2
      IF ( iwrd/=iden ) THEN
         modid(1) = 0
         modid(2) = 0
         IF ( Buff(nmod)/=A377 ) THEN
            IF ( Buff(nmod)==-1 ) modid(1) = Buff(nmod+1)
            IF ( Buff(nmod)/=-1 ) modid(1) = Buff(nmod)
            nmod = nmod + 2
            IF ( Buff(nmod)==1 ) nmod = nmod + 1
            iwrd = Buff(nmod)
            IF ( Bit64 ) CALL mvbits(Blank,0,32,iwrd,0)
            IF ( iwrd/=iden ) THEN
               IF ( Buff(nmod)/=A377 ) THEN
                  IF ( Buff(nmod)==-1 ) modid(2) = Buff(nmod+1)
                  IF ( Buff(nmod)/=-1 ) modid(2) = Buff(nmod)
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      CALL fndplt(ploter,model,modid(1))
      buf(1) = bcd
      buf(2) = orf(lshift(ploter,Ihalf),model+100)
      binplt = binplt + 1
      GOTO 200
   ELSE
      n = n + 2*Buff(n) - 2
      j = n
      DO
!
!     SEARCH FOR EQUAL SIGN
!
         iwrd = Buff(n)
         IF ( Bit64 ) CALL mvbits(Blank,0,32,iwrd,0)
         IF ( iwrd==iequal ) THEN
            i = -1
            EXIT
         ELSE
            n = n - 2
            IF ( n<=0 ) THEN
               n = j
               i = -1
               EXIT
            ENDIF
         ENDIF
      ENDDO
      DO
         iwrd = Buff(n+1)
         IF ( Bit64 ) CALL mvbits(Blank,0,32,iwrd,0)
         IF ( iwrd==yes ) i = 1
         IF ( iwrd==no ) i = 0
         IF ( i>=0 ) THEN
            buf(1) = bcd
            buf(2) = i
            GOTO 200
         ELSEIF ( i<-3 ) THEN
            n = j
            GOTO 2200
         ELSE
            i = i - 1
            n = n + 1
         ENDIF
      ENDDO
   ENDIF
!
!     FORM LIST OF SUBCASES, MAXIMUM OF 200 FOR THIS COMMAND CARD.
!
 500  IF ( Buff(n)/=cont ) THEN
!
      Subcas(1) = 0
      IF ( Buff(n)/=inte ) GOTO 2600
!
!     SUBCASES ARE NOT APPLICABLE IN AUTO AND PSDF
!
      IF ( type/=1 ) GOTO 2700
   ELSE
      ASSIGN 500 TO icont
      GOTO 2100
   ENDIF
!
 600  nsubs = nsubs + 1
   IF ( nsubs>200 ) GOTO 2800
   IF ( Buff(n+1)<=0 ) GOTO 2900
   Subcas(nsubs) = Buff(n+1)
   n = n + 2
 700  DO WHILE ( Buff(n)/=A377 )
      IF ( Buff(n)/=cont ) THEN
!
         IF ( Buff(n)/=inte ) GOTO 800
         IF ( Subcas(nsubs)<Buff(n+1) ) GOTO 600
         IF ( Subcas(nsubs)==Buff(n+1) ) THEN
            n = n + 2
         ELSE
            GOTO 2900
         ENDIF
      ELSE
         ASSIGN 700 TO icont
         GOTO 2100
      ENDIF
   ENDDO
   j = 692
   WRITE (L,99005) Ufm , j
99005 FORMAT (A23,I4,', XY-OUTPUT COMMAND IS INCOMPLETE.')
   GOTO 3200
 800  IF ( Buff(n)/=cont ) THEN
!
      IF ( Buff(n)<0 ) GOTO 2600
      iwrd = Buff(n+2)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,iwrd,0)
      IF ( iwrd/=slas ) THEN
!
         IF ( iwrd/=thru ) GOTO 2600
         n = n + 3
      ELSE
         slash = .TRUE.
         n = n + 3
         GOTO 1000
      ENDIF
   ELSE
      ASSIGN 800 TO icont
      GOTO 2100
   ENDIF
 900  IF ( Buff(n)/=cont ) THEN
!
      IF ( Buff(n)/=inte ) GOTO 2600
      IF ( Buff(n+1)<Subcas(nsubs) ) GOTO 2900
      IF ( Buff(n+1)==Subcas(nsubs) ) THEN
         n = n + 2
         GOTO 700
      ELSE
         DO
            nsubs = nsubs + 1
            IF ( nsubs>200 ) GOTO 2800
            Subcas(nsubs) = Subcas(nsubs-1) + 1
            IF ( Subcas(nsubs)>=Buff(n+1) ) THEN
               n = n + 2
               GOTO 700
            ENDIF
         ENDDO
      ENDIF
   ELSE
      ASSIGN 900 TO icont
      GOTO 2100
   ENDIF
!
!     SLASH HIT. BEGIN PROCESSING FRAME DATA. FIRST WRITE SUBCASE
!     NUMBERS.
!
 1000 CALL write(file,nsubs,1,noeor)
   IF ( nsubs/=0 ) CALL write(file,Subcas(1),nsubs,noeor)
   IF ( nsubs==0 ) Subcas(1) = 0
   IF ( nsubs==0 ) nsubs = 1
 1100 slash = .FALSE.
   CALL write(file,fram,1,noeor)
   pairs = .FALSE.
   ncurve = 0
 1200 IF ( Buff(n)/=cont ) THEN
!
      IF ( Buff(n)/=inte ) GOTO 2600
      buf(1) = Buff(n+1)
      buf(2) = 0
      buf(3) = 0
      idcom = 0
      ncurve = ncurve + 1
      IF ( buf(1)<=0 ) THEN
         j = 685
         WRITE (L,99006) Ufm , j , buf(1)
99006    FORMAT (A23,I4,1H,,I12,' = POINT OR ELEMENT ID IS ILLEGAL (LESS ','THAN 1).')
         GOTO 3200
      ELSE
!
!     GET COMPONENT. POSITIVE INTEGER.
!     MAY BE T1,T2,T3,R1,R2,R3 ETC. IF THE VECTOR IS NOT STRESS OR FORCE
!
         n = n + 2
      ENDIF
   ELSE
      ASSIGN 1200 TO icont
      GOTO 2100
   ENDIF
 1300 IF ( Buff(n)/=cont ) THEN
!
      iwrd = Buff(n+2)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,iwrd,0)
      IF ( Buff(n)<=0 .OR. iwrd/=oparen ) GOTO 2600
!
      ofbcd = .FALSE.
      IF ( Buff(n)>1 ) THEN
!
!     FALL HERE AND A BCD COMPONENT IS EXPECTED. T1,T2,T3,R1,R2,R3
!
         n1 = n + 3
         n = n + 2*Buff(n) + 1
         GOTO 1600
      ELSE
!
!     FALL HERE AND A POSITIVE INTEGER COMPONENT IS EXPECTED.
!
         n = n + 3
      ENDIF
   ELSE
      ASSIGN 1300 TO icont
      GOTO 2100
   ENDIF
 1400 IF ( Buff(n)==cont ) THEN
      ASSIGN 1400 TO icont
      GOTO 2100
   ENDIF
!
 1500 IF ( Buff(n)/=inte ) GOTO 2600
   IF ( Buff(n+1)<=0 ) THEN
      j = 686
      WRITE (L,99007) Ufm , j
99007 FORMAT (A23,I4,', NEGATIVE OR ZERO COMPONENTS ARE ILLEGAL.')
      GOTO 3200
   ELSE
      ofbcd = .FALSE.
      compon = Buff(n+1)
      n = n + 2
      GOTO 1700
   ENDIF
 1600 bcd = Buff(n1)
   IF ( Bit64 ) CALL mvbits(Blank,0,32,bcd,0)
   IF ( bcd/=Blank ) THEN
      IF ( vector==6 .OR. vector==7 ) THEN
         j = 687
         WRITE (L,99008) Ufm , j
99008    FORMAT (A23,I4,', ALPHA-COMPONENTS ARE NOT PERMITTED FOR STRESS ','OR FORCE XY-OUTPUT REQUESTS.')
         GOTO 3200
      ENDIF
   ENDIF
   ofbcd = .TRUE.
   compon = 3
   IF ( bcd/=t1 .AND. bcd/=t1rm ) THEN
      IF ( bcd/=g ) THEN
         compon = 4
         IF ( bcd/=t2 .AND. bcd/=t2rm ) THEN
            IF ( bcd/=f ) THEN
               compon = 5
               IF ( bcd/=t3 .AND. bcd/=t3rm ) THEN
                  compon = 6
                  IF ( bcd/=r1 .AND. bcd/=r1rm ) THEN
                     compon = 7
                     IF ( bcd/=r2 .AND. bcd/=r2rm ) THEN
                        compon = 8
                        IF ( bcd/=r3 .AND. bcd/=r3rm ) THEN
                           compon = 9
                           IF ( bcd/=t1ip ) THEN
                              compon = 10
                              IF ( bcd/=t2ip ) THEN
                                 compon = 11
                                 IF ( bcd/=t3ip ) THEN
                                    compon = 12
                                    IF ( bcd/=r1ip ) THEN
                                       compon = 13
                                       IF ( bcd/=r2ip ) THEN
                                         compon = 14
                                         IF ( bcd/=r3ip ) THEN
                                         compon = 1000
                                         IF ( bcd/=Blank ) THEN
                                         j = 688
                                         WRITE (L,99009) Ufm , j , bcd
99009                                    FORMAT (A23,I4,1H,,A4,' COMPONENT NAME NOT RECOGNIZED.')
                                         GOTO 3200
                                         ENDIF
                                         ENDIF
                                       ENDIF
                                    ENDIF
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF
!
 1700 idcom = idcom + 1
   buf(idcom+1) = compon
!
!     CHECK RANGE OF COMPONENT
!
   IF ( compon/=1000 ) THEN
      IF ( (type==2 .OR. type==3) .AND. (compon<3 .OR. compon>8) .AND. (vector/=6 .AND. vector/=7) ) THEN
         j = 695
         WRITE (L,99010) Ufm , j , compon
99010    FORMAT (A23,I4,', COMPONENT VALUE =',I8,', IS ILLEGAL FOR AUTO ','OR PSDF VECTOR REQUESTS.')
         GOTO 3200
      ELSEIF ( (compon<3 .OR. compon>14) .AND. (vector/=6 .AND. vector/=7) ) THEN
         j = 696
         WRITE (L,99011) Ufm , j , compon
99011    FORMAT (A23,I4,', COMPONENT VALUE =',I8,', IS ILLEGAL FOR VECTOR',' TYPE SPECIFIED.')
         GOTO 3200
      ELSEIF ( Nogo==0 ) THEN
!
!     ADD THIS COMPONENT-ID TO XY-MASTER SET IN OPEN CORE.
!
         DO i = 1 , nsubs
            IF ( iat+6>icore ) GOTO 3100
            Z(iat+1) = Subcas(i)
            Z(iat+2) = vector
            Z(iat+3) = buf(1)
            Z(iat+4) = compon
            Z(iat+5) = type
            Z(iat+6) = destin
            iat = iat + 6
         ENDDO
      ENDIF
   ENDIF
!
!     PROCEED TO NEXT COMPONENT OR ID OF THIS FRAME
!
   IF ( ncurve==1 .AND. idcom==2 ) pairs = .TRUE.
   IF ( pairs .AND. (type==2 .OR. type==3) ) THEN
      j = 694
      WRITE (L,99012) Ufm , j
99012 FORMAT (A23,I4,', AUTO OR PSDF REQUESTS MAY NOT USE SPLIT FRAME',', THUS ONLY ONE COMPONENT PER ID IS PERMITTED.')
      GOTO 3200
   ELSE
      IF ( .NOT.pairs .AND. idcom==2 ) GOTO 3000
      IF ( idcom>2 ) GOTO 3000
      IF ( ofbcd ) THEN
         IF ( n1<n-2 ) THEN
            n1 = n1 + 2
            iwrd = Buff(n1+1)
            IF ( Bit64 ) CALL mvbits(Blank,0,32,iwrd,0)
            IF ( iwrd/=slas ) GOTO 1600
            slash = .TRUE.
            GOTO 1900
         ENDIF
      ENDIF
   ENDIF
!
!     IS NEXT FIELD AN INTEGER FOLLOWED BY AN OPAREN
!
 1800 IF ( Buff(n)==cont ) THEN
      ASSIGN 1800 TO icont
      GOTO 2100
!
   ELSEIF ( Buff(n)==inte ) THEN
      IF ( Buff(n+2)==A377 ) GOTO 1500
      iwrd = Buff(n+4)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,iwrd,0)
      IF ( iwrd/=oparen ) GOTO 1500
   ELSEIF ( Buff(n)/=A377 ) THEN
      iwrd = Buff(n+2)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,iwrd,0)
      IF ( Buff(n)>0 .AND. iwrd/=slas ) THEN
         n1 = n + 1
         n = n + 2*Buff(n) + 1
         GOTO 1600
      ENDIF
   ENDIF
 1900 IF ( pairs .AND. idcom==1 ) GOTO 3000
   CALL write(file,buf(1),3,noeor)
   IF ( .NOT.slash .AND. Buff(n)==inte ) GOTO 1200
   buf(1) = -1
   buf(2) = -1
   buf(3) = -1
   CALL write(file,buf(1),3,noeor)
   IF ( Buff(n)==A377 ) RETURN
!
 2000 IF ( Buff(n)/=cont ) THEN
      IF ( slash ) GOTO 1100
      iwrd = Buff(n+2)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,iwrd,0)
      IF ( iwrd/=slas ) GOTO 2600
      n = n + 2*Buff(n) + 1
      GOTO 1100
   ELSE
      ASSIGN 2000 TO icont
   ENDIF
!
!     RETURN FOR A CONTINUATION CARD
!
 2100 contin = .TRUE.
   RETURN
 2200 j = 676
   WRITE (L,99013) Ufm , j , Buff(n+1) , Buff(n+2)
99013 FORMAT (A23,I4,1H,,2A4,' IS NOT RECOGNIZED AS AN XYPLOT COMMAND ','CARD OR PARAMETER.')
   GOTO 3200
 2300 j = 677
   WRITE (L,99014) Ufm , j
99014 FORMAT (A23,I4,', ILLEGAL VALUE SPECIFIED.')
   GOTO 3200
 2400 j = 678
   WRITE (L,99015) Ufm , j , Buff(i) , Buff(i+1)
99015 FORMAT (A23,I4,1H,,2A4,' CONTRADICTS PREVIOUS DEFINITION.')
   GOTO 3200
 2500 j = 679
   WRITE (L,99016) Ufm , j , Buff(i+1)
99016 FORMAT (A23,I4,1H,,A4,' DELIMITER ILLEGALLY USED.')
   GOTO 3200
 2600 IF ( Buff(n)/=real ) THEN
      IF ( Buff(n)==inte ) THEN
         j = 682
         WRITE (L,99017) Ufm , j , Buff(n+1)
99017    FORMAT (A23,I4,1H,,I10,' IS ILLEGAL IN STATEMENT.')
      ELSE
         j = 680
         WRITE (L,99018) Ufm , j , Buff(n+1) , Buff(n+2)
99018    FORMAT (A23,I4,1H,,2A4,' IS ILLEGAL IN STATEMENT.')
      ENDIF
      GOTO 3200
   ENDIF
 2700 j = 681
   WRITE (L,99019) Ufm , j , Buff(n+1)
99019 FORMAT (A23,I4,1H,,E16.8,' IS ILLEGAL IN STATEMENT.')
   GOTO 3200
 2800 j = 683
   WRITE (L,99020) Ufm , j
99020 FORMAT (A23,I4,', TOO MANY SUBCASES. MAXIMUM = 200 ON ANY ONE XY','-OUTPUT COMMAND CARD.')
   GOTO 3200
 2900 j = 684
   WRITE (L,99021) Ufm , j
99021 FORMAT (A23,I4,', SUBCASE-ID IS LESS THAN 1 OR IS NOT IN ','ASCENDING ORDER.')
   GOTO 3200
 3000 j = 691
   WRITE (L,99022) Ufm , j
99022 FORMAT (A23,I4,', MORE THAN 2 OR UNEQUAL NUMBER OF COMPONENTS ','FOR ID-S WITHIN A SINGLE FRAME.')
   GOTO 3200
 3100 j = 693
   WRITE (L,99023) Ufm , j
99023 FORMAT (A23,I4,', INSUFFICIENT CORE FOR SET TABLE.')
   icrq = (nsubs-i+1)*6
   WRITE (L,99024) icrq
99024 FORMAT (5X,8HAT LEAST,I8,19H MORE WORDS NEEDED.)
 3200 Nogo = 1
   Line = Line + 2
   IF ( Line>=Nlpp ) CALL page
END SUBROUTINE ifp1xy
