!*==xdph.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xdph
!
!     DATA POOL HOUSEKEEPER (XDPH)
!
!     THIS SUBROUTINE SCANS THE DATA POOL DICT AND TO DETERMINE THE
!     NUMBER AND SIZE OF ANY FILES NO LONGER NEEDED.  IF A SUFFICIENT
!     QUANTITY IS NOT NEEDED, THE FILE IS RECOPIED WITH THE DEAD FILES
!     DELETED.
!
   USE c_system
   USE c_xdpl
   USE c_xfiat
   USE c_xfist
   USE c_xmssg
   USE c_xpfist
   USE c_xsfa1
   USE c_xxfiat
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: almsk , apndmk , cursno , dnaf , entn1 , entn2 , entn3 , entn4 , flag , fn , fnx , funlg , i , ii , iprt1 , iprt2 ,   &
            & iprt3 , isav , istart , iwkbuf , ix , j , k , kk , lmsk , lmt , lmt2 , lmt3 , lxmsk , m , ncnt , nculg , nfile ,      &
            & ngcnt , rmsk , rxmsk , s , scornt , slgn , tapmsk , thcrmk , trial , zap
   INTEGER , DIMENSION(1) :: fequ , fntu , fon , ford , minp , mlsn , mout , mscr , ndpd , sal , sdbn , sntu , sord
   INTEGER , SAVE :: nconst , npol , pool , scrn1 , scrn2
   INTEGER , DIMENSION(2) , SAVE :: ndph
   EXTERNAL andf , close , cpyfil , eof , korsz , mesage , open , orf , page1 , rshift , sswtch , write , xfilps
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   !>>>>EQUIVALENCE (Dpd(1),Dnaf) , (Fiat(1),Funlg) , (File(1),Fequ(1)) , (File(1),Ford(1)) , (Endsfa(1),Ndpd(1))
   !>>>>EQUIVALENCE (Md(2),Mlsn(1)) , (Md(3),Minp(1)) , (Md(4),Mout(1)) , (Md(5),Mscr(1)) , (Sos(1),Slgn) , (Sos(2),Sdbn(1)) ,           &
!>>>>    & (Sos(4),Sal(1)) , (Sos(4),Sntu(1)) , (Sos(4),Sord(1)) , (Comm(1),Almsk) , (Comm(2),Apndmk) , (Comm(3),Cursno) ,               &
!>>>>    & (Comm(4),Entn1) , (Comm(5),Entn2) , (Comm(6),Entn3) , (Comm(7),Entn4) , (Comm(8),Flag) , (Comm(9),Fnx) , (Comm(10),Lmsk) ,    &
!>>>>    & (Comm(11),Lxmsk) , (Comm(13),Rmsk) , (Comm(14),Rxmsk) , (Comm(15),S) , (Comm(16),Scornt) , (Comm(17),Tapmsk) ,                &
!>>>>    & (Comm(18),Thcrmk) , (Comm(19),Zap) , (Xf1at(1),Fntu(1)) , (Xf1at(1),Fon(1))
!
   DATA nconst/100/
   DATA scrn1/4HSCRA/ , scrn2/4HTCH*/
   DATA pool , npol/4HPOOL , 4HNPOL/ , ndph/4HXDPH , 4H    /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
         flag = 0
         SPAG_Loop_1_1: DO
            lmt3 = dculg*entn4
            lmt = (dculg-1)*entn4 + 1
            ncnt = 0
            ngcnt = 0
            trial = dnaf - 1
!
!     COUNT DEAD FILE SIZE, PUT SIZE IN NCNT
!
            DO i = 1 , lmt3 , entn4
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     IF ( ddbn(i)/=0 .OR. ddbn(i+1)/=0 ) THEN
!
!     COUNT GOOD STUFF ALSO
!
                        ngcnt = ngcnt + rshift(andf(lmsk,dfnu(i)),16)
                        CYCLE
                     ELSE
                        IF ( dfnu(i)<0 ) THEN
!
!     DEAD FILE IS EQUIV
!
                           flag = -1
                           kk = andf(rmsk,dfnu(i))
                           DO j = 1 , lmt3 , entn4
                              IF ( dfnu(j)<0 .AND. i/=j ) THEN
                                 IF ( kk==andf(rmsk,dfnu(j)) ) THEN
                                    IF ( ddbn(j)/=0 .OR. ddbn(j+1)/=0 ) GOTO 2
                                    dfnu(j) = 0
                                 ENDIF
                              ENDIF
                           ENDDO
                        ENDIF
                        IF ( kk==trial ) THEN
                           dnaf = trial
                        ELSE
                           IF ( dfnu(i)/=0 ) ncnt = ncnt + rshift(andf(lmsk,dfnu(i)),16)
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
 2                      dfnu(i) = 0
                     ENDIF
                     spag_nextblock_2 = 2
                  CASE (2)
                     IF ( i==lmt ) THEN
                        dculg = dculg - 1
                        flag = -1
                        CYCLE SPAG_Loop_1_1
                     ENDIF
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
            ENDDO
!
!     CHECK FOR BREAKING OF EQUIV
!
            IF ( flag/=0 ) THEN
               SPAG_Loop_2_2: DO i = 1 , lmt3 , entn4
                  IF ( dfnu(i)<0 ) THEN
                     kk = andf(rmsk,dfnu(i))
                     DO j = 1 , lmt3 , entn4
                        IF ( dfnu(j)<0 .AND. i/=j ) THEN
                           IF ( kk==andf(rmsk,dfnu(j)) ) CYCLE SPAG_Loop_2_2
                        ENDIF
                     ENDDO
                     dfnu(i) = andf(almsk,dfnu(i))
                  ENDIF
               ENDDO SPAG_Loop_2_2
            ENDIF
!
!     IS NCNT OF SUFFICIENT SIZE TO WARRANT RECOPYING POOL
!
            CALL sswtch(3,ix)
            IF ( ix==1 ) THEN
               CALL page1
               WRITE (outtap,99001) ncnt
99001          FORMAT (21H0DPH DEAD FILE COUNT=,I6)
               WRITE (outtap,99002) (dpd(ix),ix=1,3)
99002          FORMAT (16H0DPD BEFORE DPH ,3I4)
               ii = dculg*3 + 3
               DO ix = 4 , ii , 3
                  iprt1 = rshift(dpd(ix+2),nbpw-1)
                  iprt2 = rshift(andf(lxmsk,dpd(ix+2)),16)
                  iprt3 = andf(rxmsk,dpd(ix+2))
                  WRITE (outtap,99005) dpd(ix) , dpd(ix+1) , iprt1 , iprt2 , iprt3
               ENDDO
            ENDIF
!
!     RECOPY POOL IF THERE ARE MORE THAN 500,000 WORD DEAD AND
!     THE GOOD STUFF IS TWICE AS BIG AS THE DEAD STUFF
!
            IF ( ncnt>nconst .AND. ncnt>2*ngcnt ) EXIT SPAG_Loop_1_1
            IF ( ncnt>0 .AND. dculg+5>=dmxlg ) EXIT SPAG_Loop_1_1
            RETURN
         ENDDO SPAG_Loop_1_1
!
!     RECOPY POOL, SWITCH POOL FILE POINTERS
!
         lmt2 = funlg*entn1
         kk = andf(thcrmk,scrn2)
         DO i = 1 , lmt2 , entn1
            IF ( fdbn(i)==0 .AND. fdbn(i+1)==0 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( fdbn(i)==scrn1 .AND. andf(thcrmk,fdbn(i+1))==kk ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
!
!     NO FILE AVAILABLE TO COPY ONTO, FORGET IT
!
         RETURN
      CASE (2)
!
!     SET-UP FOR A RECOPY
!
         isav = i
         CALL open(*20,pool,endsfa,0)
         fnx = 1
         fist(2*npfist+4) = isav + 2
         fist(2) = npfist + 1
         fist(2*npfist+3) = npol
         CALL open(*20,npol,endsfa(ibufsz+1),1)
         m = 2*ibufsz
         i = m + 1
         istart = i
         m = m + dculg*3 + 3
         iwkbuf = korsz(endsfa) - m
         IF ( iwkbuf<100 ) CALL mesage(-8,0,ndph)
         m = m + 1
         nfile = 1
         nculg = 0
         DO j = 1 , lmt3 , entn4
            IF ( ddbn(j)/=0 .OR. ddbn(j+1)/=0 ) THEN
               IF ( ddbn(j)/=63 .OR. ddbn(j+1)/=63 ) THEN
!
!     RECOPY DICTIONARY
!
                  ndpd(i) = ddbn(j)
                  ndpd(i+1) = ddbn(j+1)
                  ndpd(i+2) = orf(andf(lxmsk,dfnu(j)),nfile)
                  IF ( dfnu(j)<0 ) THEN
                     ndpd(i+2) = orf(s,ndpd(i+2))
                     kk = andf(rmsk,dfnu(j))
                     DO k = 1 , lmt3 , entn4
                        IF ( dfnu(k)<0 .AND. j/=k ) THEN
                           IF ( kk==andf(rmsk,dfnu(k)) ) THEN
                              i = i + 3
                              nculg = nculg + 1
                              ndpd(i) = ddbn(k)
                              ddbn(k) = 63
                              ndpd(i+1) = ddbn(k+1)
                              ddbn(k+1) = 63
                              ndpd(i+2) = ndpd(i-1)
                           ENDIF
                        ENDIF
                     ENDDO
                  ENDIF
                  i = i + 3
                  nculg = nculg + 1
!
!     RECOPY NECESSARY FILE
!
                  fn = andf(rmsk,dfnu(j))
                  CALL xfilps(fn)
                  CALL cpyfil(pool,npol,endsfa(m),iwkbuf,flag)
                  CALL eof(npol)
                  nfile = nfile + 1
                  fnx = fn + 1
               ENDIF
            ENDIF
         ENDDO
!
!     COPY TEMPORARY DPD INTO ACTUAL DPD
!
         i = i - 1
         ix = 0
         DO j = istart , i
            ix = ix + 1
            ddbn(ix) = ndpd(j)
         ENDDO
         dnaf = nfile
         dculg = nculg
         CALL close(pool,1)
         CALL close(npol,1)
         fnx = 1
!
!     COPY POOL BACK TO POOL UNIT
!
         CALL open(*20,npol,endsfa,0)
         CALL open(*20,pool,endsfa(ibufsz+1),1)
         nfile = nfile - 1
         DO ix = 1 , nfile
            CALL cpyfil(npol,pool,endsfa(m),iwkbuf,flag)
            CALL eof(pool)
         ENDDO
         CALL close(pool,1)
         CALL close(npol,1)
!
!     THE FOLLOWING 3 LINES OF CODE WILL FREE DISK AREA ON SOME CONFIG.
!
         CALL open(*20,npol,endsfa,1)
         CALL write(npol,ndph,2,1)
         CALL close(npol,1)
         CALL sswtch(3,ix)
         IF ( ix/=1 ) RETURN
!
         WRITE (outtap,99003) (dpd(ix),ix=1,3)
99003    FORMAT (15H0DPD AFTER DPH ,3I4)
         ii = dculg*3 + 3
         DO ix = 4 , ii , 3
            iprt1 = rshift(dpd(ix+2),nbpw-1)
            iprt2 = rshift(andf(lxmsk,dpd(ix+2)),16)
            iprt3 = andf(rxmsk,dpd(ix+2))
            WRITE (outtap,99005) dpd(ix) , dpd(ix+1) , iprt1 , iprt2 , iprt3
         ENDDO
         RETURN
!
 20      WRITE (outtap,99004) sfm
99004    FORMAT (A25,' 1041, OLD/NEW POOL COULD NOT BE OPENED.')
         CALL mesage(-37,0,ndph)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99005 FORMAT (1H ,2A4,3I6)
END SUBROUTINE xdph
