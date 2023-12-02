!*==xclean.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xclean
!
   IMPLICIT NONE
   USE C_SYSTEM
   USE C_XDPL
   USE C_XFIAT
   USE C_XFIST
   USE C_XMSSG
   USE C_XSFA1
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: almsk , apndmk , cursno , dculg , dmxlg , dnaf , entn1 , entn1x , entn1y , entn2 , entn3 , entn4 , fculg , flag ,     &
            & fmxlg , fnx , funlg , hold , i , icursn , ifail , ifordi , ihop , ik , isw , j , k , l , lmsk , lmt1 , lmt2 , lmt3 ,  &
            & lmt4 , lmt5 , lmt6 , lmt7 , lmti , lxmsk , macsft , mlgn , nfculg , rmsk , rxmsk , s , scornt , slgn , tapmsk ,       &
            & thcrmk , trial , zap
   INTEGER , DIMENSION(1) :: ddbn , dfnu , fcum , fcus , fdbn , fequ , file , fknd , fmat , fntu , fon , ford , fpun , minp , mlsn ,&
                           & mout , mscr , sal , sdbn , sntu , sord
   INTEGER , DIMENSION(2) , SAVE :: nclean
   EXTERNAL andf , lshift , mesage , orf , rshift , xpolck
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   !>>>>EQUIVALENCE (Dpd(1),Dnaf) , (Dpd(2),Dmxlg) , (Dpd(3),Dculg) , (Dpd(4),Ddbn(1)) , (Dpd(6),Dfnu(1)) , (Fiat(1),Funlg) ,            &
!>>>>    & (Fiat(2),Fmxlg) , (Fiat(3),Fculg) , (Fiat(4),Fequ(1)) , (Fiat(4),File(1)) , (Fiat(4),Ford(1)) , (Fiat(5),Fdbn(1)) ,           &
!>>>>    & (Fiat(7),Fmat(1)) , (Md(1),Mlgn) , (Md(2),Mlsn(1)) , (Md(3),Minp(1)) , (Md(4),Mout(1)) , (Md(5),Mscr(1)) , (Sos(1),Slgn) ,    &
!>>>>    & (Sos(2),Sdbn(1)) , (Sos(4),Sal(1)) , (Sos(4),Sntu(1)) , (Sos(4),Sord(1)) , (Xf1at(1),Fntu(1)) , (Xf1at(1),Fon(1)) ,           &
!>>>>    & (Xf1at(2),Fpun(1)) , (Xf1at(3),Fcum(1)) , (Xf1at(4),Fcus(1)) , (Xf1at(5),Fknd(1))
   !>>>>EQUIVALENCE (Comm(1),Almsk) , (Comm(2),Apndmk) , (Comm(3),Cursno) , (Comm(4),Entn1) , (Comm(5),Entn2) , (Comm(6),Entn3) ,        &
!>>>>    & (Comm(7),Entn4) , (Comm(8),Flag) , (Comm(9),Fnx) , (Comm(10),Lmsk) , (Comm(11),Lxmsk) , (Comm(12),Macsft) , (Comm(13),Rmsk) , &
!>>>>    & (Comm(14),Rxmsk) , (Comm(15),S) , (Comm(16),Scornt) , (Comm(17),Tapmsk) , (Comm(18),Thcrmk) , (Comm(19),Zap)
   DATA nclean/4HXCLE , 4HAN  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     ENTRY SIZE NUMBERS,  1=FIAT, 2=SOS, 3=MD
!
         ifail = 0
         entn1x = entn1 - 1
         entn1y = entn1*2
         lmt1 = funlg*entn1
         lmt2 = lmt1 + 1
         lmt3 = fculg*entn1
         flag = 0
         icursn = lshift(cursno,16)
!
!     MERGE FIAT BY REPLACING ANY UNIQUE FILES WITH MATCHED CURRENT
!     FILES ONLY CURRENT TAIL  AND  EXCEPT EQU FILES
!
         IF ( funlg==fculg ) GOTO 20
         ASSIGN 20 TO isw
         spag_nextblock_1 = 2
      CASE (2)
         DO i = lmt2 , lmt3 , entn1
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  trial = andf(rmsk,file(i))
                  IF ( trial==zap ) CYCLE
!
!     ERASE SCRATCH AND LTU EXPIRED FILES FROM CURRENT TAIL
!
                  k = andf(lmsk,ford(i))
                  IF ( k==lmsk .OR. (icursn>k .AND. k/=0 .AND. fequ(i)>0) ) THEN
                     lmt4 = i + entn1x
                     DO k = i , lmt4
                        file(k) = 0
                     ENDDO
                     j = i
                     spag_nextblock_2 = 3
                     CYCLE SPAG_DispatchLoop_2
                  ELSE
                     SPAG_Loop_2_1: DO j = 1 , lmt1 , entn1
                        IF ( trial==andf(rmsk,file(j)) ) THEN
                           IF ( fequ(j)<0 ) EXIT SPAG_Loop_2_1
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDDO SPAG_Loop_2_1
                     CYCLE
                  ENDIF
               CASE (2)
                  k = andf(lmsk,ford(j))
                  IF ( k/=lmsk .AND. icursn<=k ) CYCLE
                  lmt4 = i + entn1x
                  DO k = i , lmt4
                     file(j) = file(k)
                     file(k) = 0
                     fntu(j) = fntu(k)
                     j = j + 1
                  ENDDO
                  j = j - entn1
                  spag_nextblock_2 = 3
               CASE (3)
                  CALL xpolck(fdbn(j),fdbn(j+1),ik,l)
                  IF ( fequ(j)<0 ) flag = -1
                  IF ( ik/=0 ) THEN
                     ddbn(l) = 0
                     ddbn(l+1) = 0
                  ENDIF
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         GOTO isw
!
!     REGENERATE ALL NTU VALUES (AND LTU IF EMPTY) IN FIAT BY SCANNING
!     SOS DELETE FIAT ENTRY IF NOT FOUND, OR A SCRATCH
!
 20      lmt4 = mlgn*entn3
         lmt2 = lmt1 + 1
!
!     FIAT LOOP
!
         SPAG_Loop_1_2: DO i = 1 , lmt3 , entn1
            IF ( andf(lmsk,ford(i))/=lmsk ) THEN
               trial = fdbn(i)
               IF ( trial==0 ) CYCLE
!
!     SOS LOOP - BY MODULE
!
               lmt6 = 0
               DO j = 1 , lmt4 , entn3
                  lmt5 = lmt6 + 1
                  lmti = lmt6 + minp(j)*entn2
                  lmt6 = lmt6 + (minp(j)+mout(j)+mscr(j))*entn2
!
!     SOS LOOP - BY FILE WITHIN MODULE
!
                  DO k = lmt5 , lmt6 , entn2
                     IF ( trial==sdbn(k) .AND. fdbn(i+1)==sdbn(k+1) ) THEN
                        IF ( andf(rmsk,file(i))/=zap ) THEN
                           IF ( k<=lmti .AND. fmat(i)==0 .AND. fmat(i+1)==0 .AND. fmat(i+2)==0 ) THEN
                              IF ( .NOT.(entn1==11 .AND. (fmat(i+5)/=0 .OR. fmat(i+6)/=0 .OR. fmat(i+7)/=0)) ) THEN
!
!     IF FIAT ENTRY IS INPUT WITH ZERO TRAILERS - PURGE IT
!
                                 IF ( i>lmt1 ) THEN
                                    lmti = 0
                                    file(i) = orf(file(i),zap)
                                    GOTO 25
!
!     PURGE FILE --PUT ENTRY AT END OF FIAT
!
                                 ELSEIF ( fculg==fmxlg ) THEN
!
!     TRY TO PACK FIAT FOR MORE SPACE
!
                                    IF ( ifail==1 ) THEN
                                       spag_nextblock_1 = 7
                                       CYCLE SPAG_DispatchLoop_1
                                    ENDIF
                                    ifail = 1
                                    ASSIGN 20 TO ihop
                                    GOTO 40
                                 ELSE
                                    nfculg = fculg*entn1 + 1
                                    ifail = 0
                                    fculg = fculg + 1
                                    file(nfculg) = orf(file(i),zap)
                                    fdbn(nfculg) = fdbn(i)
                                    fdbn(nfculg+1) = fdbn(i+1)
                                    GOTO 25
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDIF
                        fntu(i) = andf(rmsk,mlsn(j))
                        IF ( andf(lmsk,ford(i))==0 ) ford(i) = orf(ford(i),andf(lmsk,sord(k)))
                        CYCLE SPAG_Loop_1_2
                     ENDIF
                  ENDDO
               ENDDO
!
!     DELETE FIAT ENTRY (UNLESS LTU YET TO COME)
!
!     HOLD FILES UNTIL LARGEST LTU OF EQUIVALENCED GROUP EXPIRES
!
               IF ( fequ(i)<0 .OR. icursn<=andf(lmsk,ford(i)) ) THEN
                  fntu(i) = rshift(andf(lmsk,ford(i)),16)
                  CYCLE
               ENDIF
 25            CALL xpolck(fdbn(i),fdbn(i+1),ik,l)
               IF ( ik/=0 ) THEN
                  ddbn(l) = 0
                  ddbn(l+1) = 0
               ENDIF
               IF ( lmti==0 ) CYCLE
            ENDIF
            hold = andf(rxmsk,file(i))
            IF ( fequ(i)<0 ) flag = -1
            lmt6 = i + entn1x
            DO k = i , lmt6
               file(k) = 0
            ENDDO
            IF ( i<=lmt1 ) THEN
               file(i) = hold
               flag = -1
            ENDIF
         ENDDO SPAG_Loop_1_2
         lmt3 = fculg*entn1
!
!     CHECK EQU FILES FOR BREAKING OF EQU
!
         IF ( funlg==fculg ) RETURN
         SPAG_Loop_1_3: DO i = 1 , lmt3 , entn1
            IF ( fequ(i)<0 .AND. andf(lmsk,ford(i))<icursn ) THEN
               DO j = 1 , lmt3 , entn1
                  IF ( fequ(j)<0 ) THEN
                     IF ( i/=j ) THEN
                        IF ( andf(rmsk,file(i))==andf(rmsk,file(j)) .AND. icursn<=andf(lmsk,ford(j)) ) CYCLE SPAG_Loop_1_3
                     ENDIF
                  ENDIF
!
               ENDDO
               fequ(i) = andf(almsk,fequ(i))
               flag = -1
            ENDIF
         ENDDO SPAG_Loop_1_3
!
!     IF BREAK HAS OCCURED, REPEAT FIAT MERGE
!
         ASSIGN 60 TO ihop
         IF ( flag==-1 ) THEN
            ASSIGN 40 TO isw
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     CLOSE UP FILES(IF ANY) BELOW UNIQUE LENGTH - RESET FCULG
!
 40      lmt7 = lmt3 - entn1x
         lmt3 = lmt7 - 1
         spag_nextblock_1 = 3
      CASE (3)
         IF ( lmt7<lmt2 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( fdbn(lmt7)/=0 ) THEN
            DO i = lmt2 , lmt3 , entn1
               IF ( fdbn(i)==0 ) THEN
                  lmt4 = i + entn1x
                  DO k = i , lmt4
                     file(k) = file(lmt7)
                     file(lmt7) = 0
                     fntu(k) = fntu(lmt7)
                     lmt7 = lmt7 + 1
                  ENDDO
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ELSE
            lmt7 = lmt7 - entn1
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
      CASE (4)
         lmt7 = lmt7 - entn1y
         lmt2 = i + entn1
         spag_nextblock_1 = 5
      CASE (5)
         lmt3 = lmt3 - entn1
         fculg = fculg - 1
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (6)
!
!     RESET ANY NECESSARY OFF SWITCHES
!
         GOTO ihop
 60      IF ( funlg==fculg ) RETURN
         lmt2 = lmt1 + 1
         lmt3 = fculg*entn1
         DO i = lmt2 , lmt3 , entn1
            IF ( fequ(i)>=0 ) THEN
               trial = andf(rmsk,file(i))
               IF ( trial/=rmsk ) THEN
                  ifordi = andf(lmsk,ford(i))
                  DO j = 1 , lmt3 , entn1
                     IF ( trial==andf(rmsk,file(j)) ) THEN
                        IF ( i/=j ) THEN
                           IF ( andf(lmsk,ford(j))<ifordi ) THEN
                              fon(j) = orf(s,fon(j))
                           ELSEIF ( andf(lmsk,ford(j))/=ifordi ) THEN
                              fon(i) = orf(s,fon(i))
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDDO
               ENDIF
            ENDIF
         ENDDO
         RETURN
      CASE (7)
!
         WRITE (Outtap,99001) Sfm
99001    FORMAT (A25,' 1021, FIAT OVERFLOW.')
         CALL mesage(-37,0,nclean)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE xclean
