!*==xpolck.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xpolck(Dbn1,Dbn2,Fn,L)
!
   USE c_system
   USE c_xdpl
   USE c_xfiat
   USE c_xfist
   USE c_xmssg
   USE c_xsfa1
   USE C_SYSTEM
   USE C_XDPL
   USE C_XFIAT
   USE C_XFIST
   USE C_XMSSG
   USE C_XSFA1
   IMPLICIT NONE
   INTEGER almsk , apndmk , Comm(20) , cursno , dculg , ddbn(1) , dfnu(1) , dmxlg , dnaf , Dpd(6) , entn1 , entn2 , entn3 , entn4 , &
         & fculg , fcum(1) , fcus(1) , fdbn(1) , fequ(1) , Fiat(7) , file(1) , Fist , fknd(1) , flag , fmat(1) , fmxlg , fntu(1) ,  &
         & fnx , fon(1) , ford(1) , fpun(1) , funlg , Ksystm(65) , lmsk , lxmsk , macsft , Md(401) , minp(1) , mlgn , mlsn(1) ,     &
         & mout(1) , mscr(1) , outtap , rmsk , rxmsk , s , sal(1) , scornt , sdbn(1) , slgn , sntu(1) , sord(1) , Sos(1501) ,       &
         & tapmsk , thcrmk , Xf1at(5) , zap
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /system/ Ksystm
   COMMON /xdpl  / Dpd
   COMMON /xfiat / Fiat
   COMMON /xfist / Fist
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /xsfa1 / Md , Sos , Comm , Xf1at
   INTEGER Dbn1 , Dbn2 , Fn , L , New , Nx , Ny
   INTEGER andf , lshift , orf
   INTEGER fdif , i , j , kfil , lmt1 , lmt2 , lmt3 , nfculg , npolck(2) , pool
   EXTERNAL andf , lshift , orf
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   !>>>>EQUIVALENCE (Ksystm(2),Outtap)
   !>>>>EQUIVALENCE (Dpd(1),Dnaf) , (Dpd(2),Dmxlg) , (Dpd(3),Dculg) , (Dpd(4),Ddbn(1)) , (Dpd(6),Dfnu(1)) , (Fiat(1),Funlg) ,            &
!>>>>    & (Fiat(2),Fmxlg) , (Fiat(3),Fculg) , (Fiat(4),Fequ(1)) , (Fiat(4),File(1)) , (Fiat(4),Ford(1)) , (Fiat(5),Fdbn(1)) ,           &
!>>>>    & (Fiat(7),Fmat(1)) , (Md(1),Mlgn) , (Md(2),Mlsn(1)) , (Md(3),Minp(1)) , (Md(4),Mout(1)) , (Md(5),Mscr(1)) , (Sos(1),Slgn) ,    &
!>>>>    & (Sos(2),Sdbn(1)) , (Sos(4),Sal(1)) , (Sos(4),Sntu(1)) , (Sos(4),Sord(1)) , (Xf1at(1),Fntu(1)) , (Xf1at(1),Fon(1)) ,           &
!>>>>    & (Xf1at(2),Fpun(1)) , (Xf1at(3),Fcum(1)) , (Xf1at(4),Fcus(1)) , (Xf1at(5),Fknd(1))
   !>>>>EQUIVALENCE (Comm(1),Almsk) , (Comm(2),Apndmk) , (Comm(3),Cursno) , (Comm(4),Entn1) , (Comm(5),Entn2) , (Comm(6),Entn3) ,        &
!>>>>    & (Comm(7),Entn4) , (Comm(8),Flag) , (Comm(9),Fnx) , (Comm(10),Lmsk) , (Comm(11),Lxmsk) , (Comm(12),Macsft) , (Comm(13),Rmsk) , &
!>>>>    & (Comm(14),Rxmsk) , (Comm(15),S) , (Comm(16),Scornt) , (Comm(17),Tapmsk) , (Comm(18),Thcrmk) , (Comm(19),Zap)
   DATA pool/4HPOOL/
   DATA npolck/4HXPOL , 4HCK  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
!     XPOLCK CHECKS THE DATA POOL DICT FOR A DATA BLOCK NAME
!
         lmt1 = dculg*entn4
         DO i = 1 , lmt1 , entn4
            IF ( Dbn1==ddbn(i) .AND. Dbn2==ddbn(i+1) ) THEN
               Fn = andf(rmsk,dfnu(i))
               L = i
               RETURN
            ENDIF
!
         ENDDO
         Fn = 0
         RETURN
!
!
         ENTRY xfilps(New)
!     ==================
!
!     XFILPS POSITIONS THE POOL TAPE FORWARD OR BACKWARD
!
!     NEW IS DESIRED POSITION
!     FNX IS CURRENT POSITION
!
         fdif = New - fnx
         IF ( fdif<0 ) THEN
            fdif = fdif - 1
         ELSEIF ( fdif==0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL skpfil(pool,fdif)
         IF ( fdif<0 .AND. New/=1 ) CALL skpfil(pool,+1)
         spag_nextblock_1 = 2
      CASE (2)
         RETURN
!
!
         ENTRY xpleqk(Nx,Ny)
!     ====================
!
!     XPLEQK MOVES SECONDARY EQUIVALENCED DATA BLOCK NAMES FROM THE
!     POOL DICT. TO FIAT.   NTU-LTU DATA ARE ALSO STORED IN FIAT FOR THE
!     EQUIV. D.B.   NTU-LTU DATA IS EXTRACTED FROM SOS IF FOUND, IF NOT,
!     IT IS COPIED FROM THE CALLING PRIMARY D.B.
!
!     NX IS THE POOL DICT. INDEX
!     NY IS THE FIAT INDEX FOR PRIMARY D.B.
!
         fequ(Ny) = orf(s,fequ(Ny))
         kfil = andf(rmsk,dfnu(Nx))
         lmt1 = dculg*entn4
         lmt2 = slgn*entn2
         lmt3 = fculg*entn1
         nfculg = lmt3 + 1
!
!     SEARCH FOR EQUIV FILES IN DICT
!
         DO i = 1 , lmt1 , entn4
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  IF ( ddbn(i)==0 .AND. ddbn(i+1)==0 ) CYCLE
                  IF ( kfil==andf(rmsk,dfnu(i)) ) THEN
                     IF ( i==Nx ) CYCLE
!
!     SEE IF NAME IS IN FIAT
!
                     DO j = 1 , lmt3 , entn1
                        IF ( ddbn(i)==fdbn(j) .AND. ddbn(i+1)==fdbn(j+1) ) GOTO 2
                     ENDDO
                     fdbn(nfculg) = ddbn(i)
                     fdbn(nfculg+1) = ddbn(i+1)
                     file(nfculg) = file(Ny)
                     fntu(nfculg) = fntu(Ny)
                     ford(nfculg) = orf(lshift(1000,16),andf(rmsk,file(nfculg)))
                     fequ(nfculg) = orf(s,fequ(nfculg))
                     DO j = 1 , lmt2 , entn2
                        IF ( ddbn(i)==sdbn(j) .AND. ddbn(i+1)==sdbn(j+1) ) THEN
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDDO
                     spag_nextblock_2 = 3
                     CYCLE SPAG_DispatchLoop_2
!
!     FILE ALREADY ALLOCATED  BE SURE EQUIVED
!
 2                   file(j) = orf(andf(rmsk,file(Ny)),andf(lmsk,file(j)))
                     fequ(j) = orf(s,fequ(j))
                  ENDIF
               CASE (2)
                  ford(nfculg) = orf(andf(lmsk,sord(j)),andf(rmsk,file(nfculg)))
                  fequ(nfculg) = orf(s,fequ(nfculg))
                  fntu(nfculg) = sntu(j)
                  spag_nextblock_2 = 3
               CASE (3)
                  nfculg = nfculg + entn1
                  fculg = fculg + 1
!
!     FLAG INDICATES D.B. S HAVE BEEN ADDED TO FIAT
!
                  flag = -1
                  IF ( fculg>fmxlg ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         RETURN
      CASE (3)
!
         WRITE (outtap,99001) Sfm
99001    FORMAT (A25,' 1051, FIAT OVERFLOW')
         CALL mesage(-37,0,npolck)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE xpolck
