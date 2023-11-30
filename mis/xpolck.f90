
SUBROUTINE xpolck(Dbn1,Dbn2,Fn,L)
!
   IMPLICIT NONE
   INTEGER Almsk , Apndmk , Comm(20) , Cursno , Dculg , Ddbn(1) , Dfnu(1) , Dmxlg , Dnaf , Dpd(6) , Entn1 , Entn2 , Entn3 , Entn4 , &
         & Fculg , Fcum(1) , Fcus(1) , Fdbn(1) , Fequ(1) , Fiat(7) , File(1) , Fist , Fknd(1) , Flag , Fmat(1) , Fmxlg , Fntu(1) ,  &
         & Fnx , Fon(1) , Ford(1) , Fpun(1) , Funlg , Ksystm(65) , Lmsk , Lxmsk , Macsft , Md(401) , Minp(1) , Mlgn , Mlsn(1) ,     &
         & Mout(1) , Mscr(1) , Outtap , Rmsk , Rxmsk , S , Sal(1) , Scornt , Sdbn(1) , Slgn , Sntu(1) , Sord(1) , Sos(1501) ,       &
         & Tapmsk , Thcrmk , Xf1at(5) , Zap
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
!
!
!     XPOLCK CHECKS THE DATA POOL DICT FOR A DATA BLOCK NAME
!
   lmt1 = Dculg*Entn4
   DO i = 1 , lmt1 , Entn4
      IF ( Dbn1==Ddbn(i) .AND. Dbn2==Ddbn(i+1) ) THEN
         Fn = andf(Rmsk,Dfnu(i))
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
   fdif = New - Fnx
   IF ( fdif<0 ) THEN
      fdif = fdif - 1
   ELSEIF ( fdif==0 ) THEN
      GOTO 100
   ENDIF
   CALL skpfil(pool,fdif)
   IF ( fdif<0 .AND. New/=1 ) CALL skpfil(pool,+1)
 100  RETURN
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
   Fequ(Ny) = orf(S,Fequ(Ny))
   kfil = andf(Rmsk,Dfnu(Nx))
   lmt1 = Dculg*Entn4
   lmt2 = Slgn*Entn2
   lmt3 = Fculg*Entn1
   nfculg = lmt3 + 1
!
!     SEARCH FOR EQUIV FILES IN DICT
!
   DO i = 1 , lmt1 , Entn4
      IF ( Ddbn(i)==0 .AND. Ddbn(i+1)==0 ) CYCLE
      IF ( kfil==andf(Rmsk,Dfnu(i)) ) THEN
         IF ( i==Nx ) CYCLE
!
!     SEE IF NAME IS IN FIAT
!
         DO j = 1 , lmt3 , Entn1
            IF ( Ddbn(i)==Fdbn(j) .AND. Ddbn(i+1)==Fdbn(j+1) ) GOTO 120
         ENDDO
         Fdbn(nfculg) = Ddbn(i)
         Fdbn(nfculg+1) = Ddbn(i+1)
         File(nfculg) = File(Ny)
         Fntu(nfculg) = Fntu(Ny)
         Ford(nfculg) = orf(lshift(1000,16),andf(Rmsk,File(nfculg)))
         Fequ(nfculg) = orf(S,Fequ(nfculg))
         DO j = 1 , lmt2 , Entn2
            IF ( Ddbn(i)==Sdbn(j) .AND. Ddbn(i+1)==Sdbn(j+1) ) GOTO 150
         ENDDO
         GOTO 200
!
!     FILE ALREADY ALLOCATED  BE SURE EQUIVED
!
 120     File(j) = orf(andf(Rmsk,File(Ny)),andf(Lmsk,File(j)))
         Fequ(j) = orf(S,Fequ(j))
      ENDIF
      CYCLE
 150  Ford(nfculg) = orf(andf(Lmsk,Sord(j)),andf(Rmsk,File(nfculg)))
      Fequ(nfculg) = orf(S,Fequ(nfculg))
      Fntu(nfculg) = Sntu(j)
 200  nfculg = nfculg + Entn1
      Fculg = Fculg + 1
!
!     FLAG INDICATES D.B. S HAVE BEEN ADDED TO FIAT
!
      Flag = -1
      IF ( Fculg>Fmxlg ) GOTO 300
   ENDDO
   RETURN
!
 300  WRITE (Outtap,99001) Sfm
99001 FORMAT (A25,' 1051, FIAT OVERFLOW')
   CALL mesage(-37,0,npolck)
END SUBROUTINE xpolck