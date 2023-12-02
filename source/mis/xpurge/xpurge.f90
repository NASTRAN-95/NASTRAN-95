!*==xpurge.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xpurge
!
!     THIS SUBROUTINE PURGES AND EQUATES FILES WITHIN FIAT AND DPD
!
   USE c_ipurge
   USE c_oscent
   USE c_system
   USE c_xdpl
   USE c_xfiat
   USE c_xfist
   USE c_xmssg
   USE c_xsfa1
   USE c_xvps
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: almsk , apndmk , cursno , dculg , dmxlg , dnaf , entn1 , entn2 , entn3 , entn4 , fculg , flag , fmxlg , fn , fnsav ,  &
            & fnx , funlg , i , iback , iexec , ij , incr , ivps , jpt , jx , l , lmsk , lmt1 , lmt2 , lmt3 , lmt4 , lsav , lxmsk , &
            & m , m1 , macsft , mlgn , n , ndbs , nfculg , nn , nwds , rmsk , rxmsk , s , scornt , secchn , slgn , snsav , ssav ,   &
            & tapmsk , thcrmk , xj1 , xj2 , zap
   INTEGER , DIMENSION(1) :: ddbn , dfnu , fcum , fcus , fdbn , fequ , file , fknd , fmat , fntu , fon , ford , fpun , minp , mlsn ,&
                           & mout , mscr , sal , sdbn , sntu , sord
   INTEGER , DIMENSION(2) , SAVE :: purge1
   INTEGER :: spag_nextblock_1
!
! End of declarations rewritten by SPAG
!
   !>>>>EQUIVALENCE (Dpd(1),Dnaf) , (Dpd(2),Dmxlg) , (Dpd(3),Dculg) , (Dpd(4),Ddbn(1)) , (Dpd(6),Dfnu(1)) , (Fiat(1),Funlg) ,            &
!>>>>    & (Fiat(2),Fmxlg) , (Fiat(3),Fculg) , (Fiat(4),Fequ(1)) , (Fiat(4),File(1)) , (Fiat(4),Ford(1)) , (Fiat(5),Fdbn(1)) ,           &
!>>>>    & (Fiat(7),Fmat(1)) , (Md(1),Mlgn) , (Md(2),Mlsn(1)) , (Md(3),Minp(1)) , (Md(4),Mout(1)) , (Md(5),Mscr(1)) , (Sos(1),Slgn) ,    &
!>>>>    & (Sos(2),Sdbn(1)) , (Sos(4),Sal(1)) , (Sos(4),Sntu(1)) , (Sos(4),Sord(1)) , (Xf1at(1),Fntu(1)) , (Xf1at(1),Fon(1)) ,           &
!>>>>    & (Xf1at(2),Fpun(1)) , (Xf1at(3),Fcum(1)) , (Xf1at(4),Fcus(1)) , (Xf1at(5),Fknd(1))
   !>>>>EQUIVALENCE (Comm(1),Almsk) , (Comm(2),Apndmk) , (Comm(3),Cursno) , (Comm(4),Entn1) , (Comm(5),Entn2) , (Comm(6),Entn3) ,        &
!>>>>    & (Comm(7),Entn4) , (Comm(8),Flag) , (Comm(9),Fnx) , (Comm(10),Lmsk) , (Comm(11),Lxmsk) , (Comm(12),Macsft) , (Comm(13),Rmsk) , &
!>>>>    & (Comm(14),Rxmsk) , (Comm(15),S) , (Comm(16),Scornt) , (Comm(17),Tapmsk) , (Comm(18),Thcrmk) , (Comm(19),Zap)
   DATA purge1/4HXPUR , 4HGE  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         CALL xsfadd
         k = -1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!
         ENTRY xequiv
!     ============
!
         CALL xsfadd
         k = +1
         prisav = 0
         secchn = 0
         spag_nextblock_1 = 2
      CASE (2)
         entn1 = icfiat
         lmt1 = funlg*entn1
         lmt2 = lmt1 + 1
         lmt3 = fculg*entn1
         IF ( fculg>=fmxlg ) THEN
!
!     POTENTIAL FIAT OVERFLOW- LOOK FOR OTHER SLOTS IN FIAT TAIL
!
            ASSIGN 20 TO iback
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ELSE
            nfculg = lmt3 + 1
            incr = 1
         ENDIF
!
!     S = O 400000000000     Z 80000000
!
 20      s = lshift(1,nbpw-1)
!
!     INITIALIZE FOR FIRST SET OF DATA BLOCKS
!
         nwds = x(1)
         i = 7
         spag_nextblock_1 = 3
      CASE (3)
         ndbs = x(i)
!
!     FIND POSITION OF VPS POINTER WORD
!
         jpt = i + 2*ndbs + 1
         IF ( k==1 ) jpt = jpt + 1
         ivps = x(jpt)
         iexec = -1
!
!     TEST CONDITIONAL INDICATOR (NOT HERE, BELOW TO PERMIT UNPURGE-
!     UNEQUIV
!
         IF ( ivps>0 ) iexec = vps(ivps)
!
!     TEST FOR PURGE OR EQUIV
!
         j = i + 1
         IF ( k<=0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
!
!     INCREMENT AND LOOK AT NEXT SET OF DATA BLOCKS
!
         i = jpt + 1
         IF ( i<nwds ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         RETURN
      CASE (5)
!
!     PURGE LOGIC FOLLOWS
!
         xj1 = x(j)
         xj2 = x(j+1)
         SPAG_Loop_1_1: DO n = 1 , lmt3 , entn1
            IF ( xj1==fdbn(n) .AND. xj2==fdbn(n+1) ) THEN
               IF ( n<=lmt1 ) THEN
                  IF ( iexec>=0 ) THEN
                     spag_nextblock_1 = 6
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  hold = andf(rxmsk,file(n))
                  lmt4 = n + entn1 - 1
                  DO m = n , lmt4
                     file(m) = 0
                  ENDDO
                  file(n) = hold
!
                  EXIT SPAG_Loop_1_1
               ELSEIF ( iexec>=0 ) THEN
!
                  IF ( andf(rmsk,file(n))==zap ) THEN
!
!     UNPURGE (CLEAR THE ENTRY)
!
                     lmt4 = n + entn1 - 1
                     DO m = n , lmt4
                        file(m) = 0
                     ENDDO
                  ENDIF
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  file(n) = zap
                  GOTO 40
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_1_1
         IF ( iexec>=0 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         file(nfculg) = zap
         fdbn(nfculg) = xj1
         fdbn(nfculg+1) = xj2
         fculg = fculg + incr
         IF ( fculg>=fmxlg ) THEN
            ASSIGN 40 TO iback
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ELSE
            nfculg = nfculg + entn1
         ENDIF
!
 40      CALL xpolck(xj1,xj2,fn,l)
         IF ( fn/=0 ) THEN
            ddbn(l) = 0
            ddbn(l+1) = 0
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
!
         j = j + 2
         IF ( j-2==i+1 .AND. k>0 ) j = j + 1
         IF ( j>=jpt ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      CASE (7)
!
!     EQUIV LOGIC FOLLOWS
!
         xj1 = x(j)
         xj2 = x(j+1)
         DO n = 1 , lmt3 , entn1
            IF ( xj1==fdbn(n) .AND. xj2==fdbn(n+1) ) THEN
               IF ( j==i+1 ) THEN
!
!     PRIMARY
!
                  prisav = andf(rxmsk,file(n))
                  IF ( iexec>=0 ) THEN
                     spag_nextblock_1 = 9
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
!
!     IF PRIMARY FILE IS PURGED OR HAS ZERO TRAILERS, PURGE SECONDARYS
!
                  IF ( andf(rmsk,prisav)==zap ) THEN
                     spag_nextblock_1 = 6
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( fmat(n)==0 .AND. fmat(n+1)==0 .AND. fmat(n+2)==0 ) THEN
                     IF ( .NOT.(entn1==11 .AND. (fmat(n+5)/=0 .OR. fmat(n+6)/=0 .OR. fmat(n+7)/=0)) ) THEN
                        spag_nextblock_1 = 6
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDIF
                  fequ(n) = orf(s,fequ(n))
                  nsav = n
                  CALL xpolck(xj1,xj2,fnsav,lsav)
                  IF ( fnsav/=0 ) dfnu(lsav) = orf(s,dfnu(lsav))
!
!     IF PRIMARY FILE CONTAINS OTHER UNEQUIV D.B.- CLEAR THEM
!
                  DO jx = 1 , lmt3 , entn1
                     IF ( fequ(jx)>=0 .AND. jx/=n ) THEN
                        IF ( prisav==andf(rxmsk,file(jx)) ) THEN
                           lmt4 = jx + entn1 - 1
                           DO m = jx , lmt4
                              file(m) = 0
                           ENDDO
                           IF ( jx<=lmt1 ) file(jx) = prisav
                        ENDIF
                     ENDIF
                  ENDDO
                  spag_nextblock_1 = 9
                  CYCLE SPAG_DispatchLoop_1
!
!     SECONDARY
!
               ELSEIF ( iexec>=0 ) THEN
                  IF ( fequ(n)<0 .AND. prisav==andf(rxmsk,file(n)) ) THEN
!
!     UNEQUIV (CLEAR SEC EQUIV ENTRY)
!
                     lmt4 = n + entn1 - 1
                     DO m = n , lmt4
                        file(m) = 0
                     ENDDO
                     IF ( n<=lmt1 ) file(n) = prisav
                     CALL xpolck(xj1,xj2,fn,l)
                     IF ( fn/=0 ) THEN
                        ddbn(l) = 0
                        ddbn(l+1) = 0
                     ENDIF
                  ENDIF
                  spag_nextblock_1 = 9
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  IF ( prisav/=0 ) THEN
                     IF ( file(n)<0 .AND. andf(rxmsk,file(n))/=prisav ) secchn = andf(rmsk,file(n))
                     IF ( n<=lmt1 ) THEN
                        file(nfculg) = orf(andf(lxmsk,file(n)),prisav)
                     ELSE
                        file(n) = orf(andf(lxmsk,file(n)),prisav)
                        fequ(n) = orf(s,fequ(n))
                        fmat(n) = fmat(nsav)
                        fmat(n+1) = fmat(nsav+1)
                        fmat(n+2) = fmat(nsav+2)
                        IF ( entn1==11 ) THEN
                           fmat(n+5) = fmat(nsav+5)
                           fmat(n+6) = fmat(nsav+6)
                           fmat(n+7) = fmat(nsav+7)
                        ENDIF
                        GOTO 60
                     ENDIF
                  ENDIF
                  hold = andf(rxmsk,file(n))
                  lmt4 = n + entn1 - 1
                  DO m = n , lmt4
                     file(m) = 0
                  ENDDO
                  IF ( n<=lmt1 ) file(n) = hold
                  IF ( prisav/=0 ) THEN
                     spag_nextblock_1 = 8
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  GOTO 60
               ENDIF
            ENDIF
!
!     FILE IS NOT IN FIAT -- CHECK PARM FOR TYPE OF EQUIV
!
         ENDDO
         IF ( iexec>=0 ) THEN
!
!     ELIMINATE EQUIV FILES -- CHECK FOR PRIMARY FILE
!
            IF ( j/=i+1 ) THEN
!
!     SECONDARY FILE --  BREAK EQUIV
!
               CALL xpolck(xj1,xj2,snsav,ssav)
!
!     CHECK IF FILE EXISTS AND IS EQUIVED TO PRIMARY FILE
!
               IF ( snsav/=0 .AND. fnsav==snsav ) THEN
                  ddbn(ssav) = 0
                  ddbn(ssav+1) = 0
               ENDIF
            ELSE
!
!     PRIMARY FILE
!
!
!     LEAVE EQUIV FLAG FOR XDPH
!
               CALL xpolck(xj1,xj2,fnsav,lsav)
            ENDIF
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
!
!     CHECK FOR PRIMARY FILE
!
         ELSEIF ( j/=i+1 ) THEN
!
!     -IF SECONDARY, WAS PRIMARY IN FIAT
!
            IF ( prisav==0 ) GOTO 60
!
!     -PRIMARY WAS IN FIAT, SET UP SECONDARY IN FIAT
!
            file(nfculg) = prisav
         ELSE
!
!     -IF PRIMARY, IT MUST BE ON POOL
!
            CALL xpolck(xj1,xj2,fnsav,lsav)
            IF ( fnsav==0 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            dfnu(lsav) = orf(s,dfnu(lsav))
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
         fequ(nfculg) = orf(s,fequ(nfculg))
         fdbn(nfculg) = xj1
         fdbn(nfculg+1) = xj2
         fmat(nfculg) = fmat(nsav)
         fmat(nfculg+1) = fmat(nsav+1)
         fmat(nfculg+2) = fmat(nsav+2)
         IF ( entn1==11 ) THEN
            fmat(nfculg+5) = fmat(nsav+5)
            fmat(nfculg+6) = fmat(nsav+6)
            fmat(nfculg+7) = fmat(nsav+7)
         ENDIF
         fculg = fculg + incr
         IF ( fculg>=fmxlg ) THEN
            ASSIGN 60 TO iback
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ELSE
            nfculg = nfculg + entn1
         ENDIF
!
!     WAS SECONDARY FILE IN FIAT ALREADY EQUIV TO OTHERS
!
 60      IF ( secchn/=0 ) THEN
!
!     SEC. FILE WAS EQUIV - DRAG ALONG ALL EQUIVS
!
            DO ij = 1 , lmt3 , entn1
               IF ( file(ij)<0 ) THEN
                  IF ( ij/=n ) THEN
                     IF ( andf(rmsk,file(ij))==secchn ) THEN
!
!     CREATE AN ENTRY IN OSCENT TO EXPLICITLY EQUIV THIS DB
!
                        m1 = nwds + 1
                        nwds = nwds + 6
                        x(m1) = 2
                        x(m1+1) = xj1
                        x(m1+2) = xj2
                        IF ( k==1 ) THEN
                           x(m1+3) = 0
                           nwds = nwds + 1
                           m1 = m1 + 1
                        ENDIF
                        x(m1+3) = fdbn(ij)
                        x(m1+4) = fdbn(ij+1)
                        x(m1+5) = ivps
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
!
!     IS SECONDARY FILE ON POOL
!
         CALL xpolck(xj1,xj2,fn,l)
         IF ( fn/=0 ) THEN
!
!     WAS SEC. FILE ON POOL ALREADY EQUIV TO OTHERS
!
            IF ( dfnu(l)<0 .AND. fnsav/=fn ) THEN
!
!     SEC. FILE ON POOL WAS EQUIV - DRAG ALONG ALL EQUIVS
!
               lmt4 = dculg*entn4
               m = lmt4 + 1
               DO ij = 1 , lmt4 , entn4
                  IF ( dfnu(ij)<0 .AND. ij/=l ) THEN
                     IF ( andf(rmsk,dfnu(ij))==fn ) THEN
                        IF ( fnsav==0 ) THEN
!
!     CREATE AN ENTRY IN OSCENT TO EXPLICITLY EQUIV THIS DB
!
                           m1 = nwds + 1
                           nwds = nwds + 6
                           x(m1) = 2
                           x(m1+1) = xj1
                           x(m1+2) = xj2
                           IF ( k==1 ) THEN
                              x(m1+3) = 0
                              nwds = nwds + 1
                              m1 = m1 + 1
                           ENDIF
                           x(m1+3) = ddbn(ij)
                           x(m1+4) = ddbn(ij+1)
                           x(m1+5) = ivps
                        ELSE
                           ddbn(m) = ddbn(ij)
                           ddbn(m+1) = ddbn(ij+1)
                           dfnu(m) = dfnu(lsav)
                           dculg = dculg + 1
                           IF ( dculg>dmxlg ) THEN
                              spag_nextblock_1 = 11
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           m = m + entn4
                        ENDIF
                        ddbn(ij) = 0
                        ddbn(ij+1) = 0
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
!
!     IF SECONDARY FILE IS ON POOL AND PRIMARY IS NOT - DELETE SEC REF
!
            IF ( fnsav/=0 ) THEN
!
!     BOTH PRIMARY AND SECONDARY ON POOL - IF NOT SAME FILE,
!     DELETE OLD SEC REF AND ADD NEW SEC REF
!
               IF ( fnsav==fn ) THEN
                  spag_nextblock_1 = 9
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               ddbn(l) = 0
               ddbn(l+1) = 0
            ELSE
               ddbn(l) = 0
               ddbn(l+1) = 0
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     IF SECONDARY FILE IS NOT ON POOL AND PRIMARY IS - ADD SEC REF
!
         ELSEIF ( fnsav==0 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         m = dculg*entn4 + 1
         ddbn(m) = xj1
         ddbn(m+1) = xj2
         dfnu(m) = dfnu(lsav)
         dculg = dculg + 1
         IF ( dculg>dmxlg ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 9
      CASE (9)
!
         j = j + 2
         IF ( j-2==i+1 ) j = j + 1
         secchn = 0
         IF ( j<jpt ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         prisav = 0
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (10)
         IF ( fculg>fmxlg ) THEN
!
!     ERROR MESSAGES
!
            WRITE (outtap,99001) sfm
99001       FORMAT (A25,' 1201, FIAT OVERFLOW.')
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ELSE
            DO nn = lmt2 , lmt3 , entn1
               IF ( file(nn)>=0 .AND. andf(zap,file(nn))/=zap ) THEN
                  IF ( fmat(nn)==0 .AND. fmat(nn+1)==0 .AND. fmat(nn+2)==0 ) THEN
                     IF ( .NOT.(entn1==11 .AND. (fmat(nn+5)/=0 .OR. fmat(nn+6)/=0 .OR. fmat(nn+7)/=0)) ) THEN
                        nfculg = nn
                        incr = 0
                        GOTO 70
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
            nfculg = nfculg + entn1
            incr = 1
 70         GOTO iback
         ENDIF
      CASE (11)
         WRITE (outtap,99002) sfm
99002    FORMAT (A25,' 1202, DPL OVERFLOW.')
         spag_nextblock_1 = 12
      CASE (12)
         CALL mesage(-37,0,purge1)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE xpurge
