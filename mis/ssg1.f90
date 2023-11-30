
SUBROUTINE ssg1
   IMPLICIT NONE
   REAL Ary(1) , Dum53(53) , Dum6(6) , Old
   INTEGER Bgpdt , Core(166) , Cstm , Ecpt , Edt , Gptt , Iary(1) , Icore(1) , Idit , Iotpe , Iptr , Isil , Itherm , Lc , Loadnn ,  &
         & Lodc , Mass , Mpt , N(3) , Nload , Nobld , Nrowsp , Sil , Slt , Sysbuf
   CHARACTER*23 Ufm
   COMMON /blank / Nrowsp , Loadnn
   COMMON /loads / Nload , Iptr
   COMMON /loadx / Lc , Slt , Bgpdt , Old , Cstm , Sil , Isil , Ecpt , Mpt , Gptt , Edt , N , Lodc , Mass , Nobld , Idit , Dum6
   COMMON /system/ Sysbuf , Iotpe , Dum53 , Itherm
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Icore
   REAL a , defml(2) , flag , gvect(1080)
   INTEGER casecc , i , icr2 , icr3 , idefml(2) , iflag , ifrst , iharm , ilist(360) , inull , ip1 , ip2 , ipont , ipont1 , islt ,  &
         & ispcn , iword(4) , j , k , lc1 , mcb(7) , mpcn , n1 , n1a , ncent , nedt , newslt , nex , ngrav , nllst , ntemp , nwds , &
         & pg(7) , subnam(2)
   INTEGER korsz
!
   EQUIVALENCE (Core(1),Icore(1),Iary(1),Ary(1)) , (defml(1),idefml(1))
   DATA iword/4 , 6 , 7 , 162/
   DATA subnam/4HSSG1 , 4H    /
!
!     MODIFY OPEN CORE POINTER IPTR FOR MAGNETICS PROBLEM
!
   Iptr = max0(Nrowsp,166)
   mcb(1) = 105
   CALL rdtrl(mcb(1))
   IF ( mcb(1)>0 ) Iptr = max0(3*Nrowsp,3*mcb(2),166)
!
!     INITIALIZE.
!
   Lc = korsz(Icore(1))
   nllst = Lc - 2*Sysbuf
   Slt = 101
   Bgpdt = 102
   Cstm = 103
   Sil = 104
   Ecpt = 105
   Mpt = 106
   Gptt = 107
   Edt = 108
   Mass = 109
   casecc = 110
   Idit = 111
   Lodc = 201
!             205 = NEWSLT (THERMAL)
   pg(1) = 301
   icr2 = 302
   icr3 = 303
   DO i = 2 , 7
      pg(i) = 0
   ENDDO
   pg(3) = Nrowsp
   pg(4) = 2
   pg(5) = 1
!
!     AVOID CALCULATING UNUSED LOADS
!
!     NEDT  = NUMBER OF ELEMENT  DEFORMATIONS
!     NTEMP = NUMBER OF THERMAL LOADS
!     NCENT = NUMBER OF CENTRIFUGAL LOADS
!
   CALL ssg1a(n1,ilist(1),nedt,ntemp,ncent,casecc,iharm)
   n1a = n1 + 1
   Lc = Lc - Sysbuf
   CALL open(*600,pg(1),Icore(Lc+1),1)
   CALL write(pg(1),pg(1),2,1)
   ngrav = 0
   nex = n1 + ntemp + nedt + ncent
   IF ( n1/=0 ) THEN
!
!     MODIFY SLT -QVOL-, -QBDY1-, -QBDY2-, AND -QVECT- CARDS.
!
      newslt = icr3
      IF ( Itherm/=0 ) newslt = 205
      islt = Slt
      CALL ssgslt(Slt,newslt,Ecpt)
      Slt = newslt
      CALL extern(nex,ngrav,gvect(1),ilist(1),pg(1),n1,iharm)
!
!     RESET -SLT- TO ORIGINAL SLT DATA BLOCK
!
      Slt = islt
      n1 = n1 - ngrav
   ENDIF
   IF ( ntemp/=0 ) THEN
      CALL templ(ntemp,ilist(n1+1),pg(1))
      n1 = n1 + ntemp
   ENDIF
   IF ( nedt/=0 ) THEN
      CALL edtl(nedt,ilist(n1+1),pg(1))
      n1 = n1 + nedt
   ENDIF
   CALL close(pg,1)
   CALL wrttrl(pg(1))
   IF ( ngrav/=0 ) THEN
!
!     CHECK TO SEE IF THE MASS MATRIX IS PURGED
!
      mcb(1) = Mass
      CALL rdtrl(mcb(1))
      IF ( mcb(1)<=0 ) CALL mesage(-56,0,iword)
      CALL gravl1(ngrav,gvect(1),icr2,iharm)
!
!     USE LOAD FILE AS SCRATCH NOTHING ON IT NOW
!
      CALL ssg2b(Mass,icr2,0,icr3,0,1,1,Lodc)
      CALL gravl2(ngrav,icr3,pg(1))
      n1 = n1 + ngrav
   ENDIF
   ipont1 = Iptr + 2
   ipont = Iptr + 1
   Nload = 0
   DO i = 1 , nllst
      Iary(i) = 0
   ENDDO
   CALL open(*700,casecc,Icore(Lc+1),0)
   lc1 = Lc - Sysbuf
   islt = 0
   CALL open(*100,Slt,Icore(lc1+1),0)
   islt = 1
   DO i = 1 , n1a
      CALL fwdrec(*300,Slt)
   ENDDO
 100  DO i = 1 , Loadnn
      CALL fwdrec(*700,casecc)
   ENDDO
   ifrst = 0
   DO
      CALL read(*200,*200,casecc,Core(1),166,1,flag)
      IF ( ifrst==0 ) THEN
         ifrst = 1
         ispcn = Core(3)
         mpcn = Core(2)
      ENDIF
!
!     TEST FOR SYMMETRY, BUCKLING OR DIFFERENTIAL STIFFNESS.
!
      IF ( Core(16)==0 .AND. Core(5)==0 .AND. Core(138)==0 ) THEN
         IF ( Core(3)/=ispcn .OR. Core(2)/=mpcn ) EXIT
         inull = 0
         DO k = 1 , 4
            i = iword(k)
            IF ( Itherm==0 .OR. i/=7 ) THEN
               IF ( Core(i)==0 ) CYCLE
               DO j = 1 , n1
                  IF ( Core(i)==ilist(j) ) GOTO 105
               ENDDO
!
!     COMBINATION CARD
!
               inull = 1
               DO
                  CALL read(*300,*800,Slt,idefml(1),2,0,iflag)
                  IF ( Core(i)==idefml(1) ) THEN
                     a = defml(2)
                     DO
                        CALL read(*300,*800,Slt,idefml(1),2,0,iflag)
                        IF ( idefml(2)==-1 ) THEN
                           CALL bckrec(Slt)
                           GOTO 120
                        ELSE
                           IF ( ipont+1>nllst ) GOTO 900
                           Iary(ipont) = Iary(ipont) + 1
                           Iary(ipont1) = idefml(2)
                           Ary(ipont1+1) = a*defml(1)
                           ipont1 = ipont1 + 2
                        ENDIF
                     ENDDO
                  ELSE
                     DO
                        CALL read(*300,*800,Slt,idefml(1),2,0,iflag)
                        IF ( idefml(2)==-1 ) EXIT
                     ENDDO
                  ENDIF
               ENDDO
 105           Iary(ipont) = Iary(ipont) + 1
               IF ( ipont+1>nllst ) GOTO 900
               Iary(ipont1) = Core(i)
               Ary(ipont1+1) = 1.0
               ipont1 = ipont1 + 2
               inull = 1
            ENDIF
 120     ENDDO
         IF ( inull==0 ) THEN
!
            Iary(ipont) = 1
            IF ( ipont+1>nllst ) GOTO 900
            Iary(ipont1) = -1
            Ary(ipont1+1) = 1.0
            ipont1 = ipont1 + 2
         ENDIF
         ipont = ipont + Iary(ipont)*2 + 1
         Nload = Nload + 1
         ipont1 = ipont1 + 1
      ENDIF
   ENDDO
 200  CALL close(casecc,1)
   IF ( islt==1 ) CALL close(Slt,1)
   CALL combin(pg(1),ilist(1),n1)
   RETURN
!
 300  ip1 = Slt
 400  ip2 = -1
 500  CALL mesage(ip2,ip1,subnam)
   ip1 = casecc
   GOTO 400
 600  ip1 = pg(1)
   GOTO 400
 700  ip1 = casecc
   GOTO 400
 800  ip2 = -2
   ip1 = Slt
   GOTO 500
!
 900  i = Icore(i)
   nwds = 0
   DO
      CALL read(*800,*1000,Slt,Core(1),Lc,0,iflag)
      nwds = nwds + Lc
   ENDDO
 1000 nwds = nwds + iflag
   WRITE (Iotpe,99001) Ufm , i , nllst , nwds
99001 FORMAT (A23,' 3176, INSUFFICIENT OPEN CORE AVAILABLE TO PROCESS ','ALL LOAD CARD COMBINATIONS IN MODULE SSG1.',/32X,          &
             &'CURRENT LOAD ID BEING PROCESSED IS',I9,1H.,/32X,'OPEN CORE AVAILABLE IS',I9,' WORDS.',/32X,                          &
             &'ADDITIONAL OPEN CORE REQUIRED IS',I9,' WORDS.')
   ip1 = 0
   ip2 = -61
   GOTO 500
END SUBROUTINE ssg1
