
SUBROUTINE xclean
!
   IMPLICIT NONE
   INTEGER Almsk , Apndmk , Comm(20) , Cursno , Dculg , Ddbn(1) , Dfnu(1) , Dmxlg , Dnaf , Dpd(6) , Entn1 , Entn2 , Entn3 , Entn4 , &
         & Fculg , Fcum(1) , Fcus(1) , Fdbn(1) , Fequ(1) , Fiat(7) , File(1) , Fist , Fknd(1) , Flag , Fmat(1) , Fmxlg , Fntu(1) ,  &
         & Fnx , Fon(1) , Ford(1) , Fpun(1) , Funlg , Ibufsz , Lmsk , Lxmsk , Macsft , Md(401) , Minp(1) , Mlgn , Mlsn(1) , Mout(1) &
         & , Mscr(1) , Outtap , Rmsk , Rxmsk , S , Sal(1) , Scornt , Sdbn(1) , Slgn , Sntu(1) , Sord(1) , Sos(1501) , Tapmsk ,      &
         & Thcrmk , Xf1at(5) , Zap
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /system/ Ibufsz , Outtap
   COMMON /xdpl  / Dpd
   COMMON /xfiat / Fiat
   COMMON /xfist / Fist
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /xsfa1 / Md , Sos , Comm , Xf1at
   INTEGER andf , lshift , orf , rshift
   INTEGER entn1x , entn1y , hold , i , icursn , ifail , ifordi , ihop , ik , isw , j , k , l , lmt1 , lmt2 , lmt3 , lmt4 , lmt5 ,  &
         & lmt6 , lmt7 , lmti , nclean(2) , nfculg , trial
   EXTERNAL andf , lshift , orf , rshift
   EQUIVALENCE (Dpd(1),Dnaf) , (Dpd(2),Dmxlg) , (Dpd(3),Dculg) , (Dpd(4),Ddbn(1)) , (Dpd(6),Dfnu(1)) , (Fiat(1),Funlg) ,            &
    & (Fiat(2),Fmxlg) , (Fiat(3),Fculg) , (Fiat(4),Fequ(1)) , (Fiat(4),File(1)) , (Fiat(4),Ford(1)) , (Fiat(5),Fdbn(1)) ,           &
    & (Fiat(7),Fmat(1)) , (Md(1),Mlgn) , (Md(2),Mlsn(1)) , (Md(3),Minp(1)) , (Md(4),Mout(1)) , (Md(5),Mscr(1)) , (Sos(1),Slgn) ,    &
    & (Sos(2),Sdbn(1)) , (Sos(4),Sal(1)) , (Sos(4),Sntu(1)) , (Sos(4),Sord(1)) , (Xf1at(1),Fntu(1)) , (Xf1at(1),Fon(1)) ,           &
    & (Xf1at(2),Fpun(1)) , (Xf1at(3),Fcum(1)) , (Xf1at(4),Fcus(1)) , (Xf1at(5),Fknd(1))
   EQUIVALENCE (Comm(1),Almsk) , (Comm(2),Apndmk) , (Comm(3),Cursno) , (Comm(4),Entn1) , (Comm(5),Entn2) , (Comm(6),Entn3) ,        &
    & (Comm(7),Entn4) , (Comm(8),Flag) , (Comm(9),Fnx) , (Comm(10),Lmsk) , (Comm(11),Lxmsk) , (Comm(12),Macsft) , (Comm(13),Rmsk) , &
    & (Comm(14),Rxmsk) , (Comm(15),S) , (Comm(16),Scornt) , (Comm(17),Tapmsk) , (Comm(18),Thcrmk) , (Comm(19),Zap)
   DATA nclean/4HXCLE , 4HAN  /
!
!     ENTRY SIZE NUMBERS,  1=FIAT, 2=SOS, 3=MD
!
   ifail = 0
   entn1x = Entn1 - 1
   entn1y = Entn1*2
   lmt1 = Funlg*Entn1
   lmt2 = lmt1 + 1
   lmt3 = Fculg*Entn1
   Flag = 0
   icursn = lshift(Cursno,16)
!
!     MERGE FIAT BY REPLACING ANY UNIQUE FILES WITH MATCHED CURRENT
!     FILES ONLY CURRENT TAIL  AND  EXCEPT EQU FILES
!
   IF ( Funlg==Fculg ) GOTO 300
   ASSIGN 300 TO isw
 100  DO i = lmt2 , lmt3 , Entn1
      trial = andf(Rmsk,File(i))
      IF ( trial==Zap ) CYCLE
!
!     ERASE SCRATCH AND LTU EXPIRED FILES FROM CURRENT TAIL
!
      k = andf(Lmsk,Ford(i))
      IF ( k==Lmsk .OR. (icursn>k .AND. k/=0 .AND. Fequ(i)>0) ) THEN
         lmt4 = i + entn1x
         DO k = i , lmt4
            File(k) = 0
         ENDDO
         j = i
         GOTO 200
      ELSE
         DO j = 1 , lmt1 , Entn1
            IF ( trial==andf(Rmsk,File(j)) ) THEN
               IF ( Fequ(j)>=0 ) GOTO 150
               EXIT
            ENDIF
         ENDDO
         CYCLE
      ENDIF
 150  k = andf(Lmsk,Ford(j))
      IF ( k/=Lmsk .AND. icursn<=k ) CYCLE
      lmt4 = i + entn1x
      DO k = i , lmt4
         File(j) = File(k)
         File(k) = 0
         Fntu(j) = Fntu(k)
         j = j + 1
      ENDDO
      j = j - Entn1
 200  CALL xpolck(Fdbn(j),Fdbn(j+1),ik,l)
      IF ( Fequ(j)<0 ) Flag = -1
      IF ( ik/=0 ) THEN
         Ddbn(l) = 0
         Ddbn(l+1) = 0
      ENDIF
   ENDDO
   GOTO isw
!
!     REGENERATE ALL NTU VALUES (AND LTU IF EMPTY) IN FIAT BY SCANNING
!     SOS DELETE FIAT ENTRY IF NOT FOUND, OR A SCRATCH
!
 300  lmt4 = Mlgn*Entn3
   lmt2 = lmt1 + 1
!
!     FIAT LOOP
!
   DO i = 1 , lmt3 , Entn1
      IF ( andf(Lmsk,Ford(i))/=Lmsk ) THEN
         trial = Fdbn(i)
         IF ( trial==0 ) CYCLE
!
!     SOS LOOP - BY MODULE
!
         lmt6 = 0
         DO j = 1 , lmt4 , Entn3
            lmt5 = lmt6 + 1
            lmti = lmt6 + Minp(j)*Entn2
            lmt6 = lmt6 + (Minp(j)+Mout(j)+Mscr(j))*Entn2
!
!     SOS LOOP - BY FILE WITHIN MODULE
!
            DO k = lmt5 , lmt6 , Entn2
               IF ( trial==Sdbn(k) .AND. Fdbn(i+1)==Sdbn(k+1) ) THEN
                  IF ( andf(Rmsk,File(i))/=Zap ) THEN
                     IF ( k<=lmti .AND. Fmat(i)==0 .AND. Fmat(i+1)==0 .AND. Fmat(i+2)==0 ) THEN
                        IF ( .NOT.(Entn1==11 .AND. (Fmat(i+5)/=0 .OR. Fmat(i+6)/=0 .OR. Fmat(i+7)/=0)) ) THEN
!
!     IF FIAT ENTRY IS INPUT WITH ZERO TRAILERS - PURGE IT
!
                           IF ( i>lmt1 ) THEN
                              lmti = 0
                              File(i) = orf(File(i),Zap)
                              GOTO 320
!
!     PURGE FILE --PUT ENTRY AT END OF FIAT
!
                           ELSEIF ( Fculg==Fmxlg ) THEN
!
!     TRY TO PACK FIAT FOR MORE SPACE
!
                              IF ( ifail==1 ) GOTO 1200
                              ifail = 1
                              ASSIGN 300 TO ihop
                              GOTO 600
                           ELSE
                              nfculg = Fculg*Entn1 + 1
                              ifail = 0
                              Fculg = Fculg + 1
                              File(nfculg) = orf(File(i),Zap)
                              Fdbn(nfculg) = Fdbn(i)
                              Fdbn(nfculg+1) = Fdbn(i+1)
                              GOTO 320
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
                  Fntu(i) = andf(Rmsk,Mlsn(j))
                  IF ( andf(Lmsk,Ford(i))==0 ) Ford(i) = orf(Ford(i),andf(Lmsk,Sord(k)))
                  GOTO 400
               ENDIF
            ENDDO
         ENDDO
!
!     DELETE FIAT ENTRY (UNLESS LTU YET TO COME)
!
!     HOLD FILES UNTIL LARGEST LTU OF EQUIVALENCED GROUP EXPIRES
!
         IF ( Fequ(i)<0 .OR. icursn<=andf(Lmsk,Ford(i)) ) THEN
            Fntu(i) = rshift(andf(Lmsk,Ford(i)),16)
            CYCLE
         ENDIF
 320     CALL xpolck(Fdbn(i),Fdbn(i+1),ik,l)
         IF ( ik/=0 ) THEN
            Ddbn(l) = 0
            Ddbn(l+1) = 0
         ENDIF
         IF ( lmti==0 ) CYCLE
      ENDIF
      hold = andf(Rxmsk,File(i))
      IF ( Fequ(i)<0 ) Flag = -1
      lmt6 = i + entn1x
      DO k = i , lmt6
         File(k) = 0
      ENDDO
      IF ( i<=lmt1 ) THEN
         File(i) = hold
         Flag = -1
      ENDIF
 400  ENDDO
   lmt3 = Fculg*Entn1
!
!     CHECK EQU FILES FOR BREAKING OF EQU
!
   IF ( Funlg==Fculg ) RETURN
   DO i = 1 , lmt3 , Entn1
      IF ( Fequ(i)<0 .AND. andf(Lmsk,Ford(i))<icursn ) THEN
         DO j = 1 , lmt3 , Entn1
            IF ( Fequ(j)<0 ) THEN
               IF ( i/=j ) THEN
                  IF ( andf(Rmsk,File(i))==andf(Rmsk,File(j)) .AND. icursn<=andf(Lmsk,Ford(j)) ) GOTO 500
               ENDIF
            ENDIF
!
         ENDDO
         Fequ(i) = andf(Almsk,Fequ(i))
         Flag = -1
      ENDIF
 500  ENDDO
!
!     IF BREAK HAS OCCURED, REPEAT FIAT MERGE
!
   ASSIGN 1100 TO ihop
   IF ( Flag==-1 ) THEN
      ASSIGN 600 TO isw
      GOTO 100
   ENDIF
!
!     CLOSE UP FILES(IF ANY) BELOW UNIQUE LENGTH - RESET FCULG
!
 600  lmt7 = lmt3 - entn1x
   lmt3 = lmt7 - 1
 700  IF ( lmt7<lmt2 ) GOTO 1000
   IF ( Fdbn(lmt7)/=0 ) THEN
      DO i = lmt2 , lmt3 , Entn1
         IF ( Fdbn(i)==0 ) THEN
            lmt4 = i + entn1x
            DO k = i , lmt4
               File(k) = File(lmt7)
               File(lmt7) = 0
               Fntu(k) = Fntu(lmt7)
               lmt7 = lmt7 + 1
            ENDDO
            GOTO 800
         ENDIF
      ENDDO
      GOTO 1000
   ELSE
      lmt7 = lmt7 - Entn1
      GOTO 900
   ENDIF
 800  lmt7 = lmt7 - entn1y
   lmt2 = i + Entn1
 900  lmt3 = lmt3 - Entn1
   Fculg = Fculg - 1
   GOTO 700
!
!     RESET ANY NECESSARY OFF SWITCHES
!
 1000 GOTO ihop
 1100 IF ( Funlg==Fculg ) RETURN
   lmt2 = lmt1 + 1
   lmt3 = Fculg*Entn1
   DO i = lmt2 , lmt3 , Entn1
      IF ( Fequ(i)>=0 ) THEN
         trial = andf(Rmsk,File(i))
         IF ( trial/=Rmsk ) THEN
            ifordi = andf(Lmsk,Ford(i))
            DO j = 1 , lmt3 , Entn1
               IF ( trial==andf(Rmsk,File(j)) ) THEN
                  IF ( i/=j ) THEN
                     IF ( andf(Lmsk,Ford(j))<ifordi ) THEN
                        Fon(j) = orf(S,Fon(j))
                     ELSEIF ( andf(Lmsk,Ford(j))/=ifordi ) THEN
                        Fon(i) = orf(S,Fon(i))
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
      ENDIF
   ENDDO
   RETURN
!
 1200 WRITE (Outtap,99001) Sfm
99001 FORMAT (A25,' 1021, FIAT OVERFLOW.')
   CALL mesage(-37,0,nclean)
END SUBROUTINE xclean
