
SUBROUTINE xcei
!
   IMPLICIT NONE
   INTEGER Bfsz , Buf(7) , Ceitbl(42) , Databf(1) , Dum21(21) , Dum57(57) , Icfiat , Icpflg , Idic(1) , Idpl(3) , Ifiat(3) ,        &
         & Isysou , Vps(1)
   COMMON /oscent/ Buf
   COMMON /system/ Bfsz , Isysou , Dum21 , Icfiat , Dum57 , Icpflg
   COMMON /xceitb/ Ceitbl
   COMMON /xdpl  / Idpl
   COMMON /xfiat / Ifiat
   COMMON /xvps  / Vps
   COMMON /zzzzzz/ Databf
   INTEGER andf , korsz , lshift , orf , rshift
   INTEGER ceitbx , contrl(4) , dcparm(2) , i , ibot , ihold , ii , itop , ivpspt , j , jj , kk , ldic , loop , lpflg , mask ,      &
         & mask1 , mxloop , nbegn , nblank , nend , newsq , noflgs , noscar , nrecsz , nxcei(2) , nxptdc(2) , pool
   EXTERNAL andf , lshift , orf , rshift
!WKBR COMMON /XCEITB/ CEITBL(2)
   EQUIVALENCE (Databf(1),Idic(1))
   DATA nxptdc/4HXPTD , 4HIC  /
   DATA nxcei/4HXCEI , 4H    /
   DATA noscar/4HXOSC/
   DATA pool/4HPOOL/
   DATA contrl/4HJUMP , 4HREPT , 4HCOND , 4HEXIT/
   DATA nblank/4H    /
   DATA mask1/65535/ , noflgs/536870911/
!
!     MASK1  = 000000177777 =     65536 = 2**16-1
!     NOFLGS = 003777777777 = 536870911 = 2**29-1
!     MASK   = 017777600000
!     LPFLG  = 010000000000
!
   mask = lshift(mask1,16)
   lpflg = lshift(1,30)
   CALL open(*1700,pool,Databf,2)
!
!     DETERMINE WHICH TYPE OF CONTROL REQUEST
!
   DO j = 1 , 4
      IF ( Buf(4)==contrl(j) ) THEN
         IF ( j==1 ) GOTO 700
         IF ( j==2 ) GOTO 300
         IF ( j==3 ) GOTO 1200
         IF ( j==4 ) GOTO 1400
      ENDIF
   ENDDO
   CALL mesage(-61,0,0)
!
!     PROCESS  JUMP CONTROL REQUEST
!
 100  IF ( newsq>Buf(2) ) THEN
!
!     MUST FORWARD REC WITHIN OSCAR FILE
!
      newsq = newsq - Buf(2) - 1
      IF ( newsq==0 ) GOTO 1300
   ELSE
!
!     MUST BACKSPACE WITHIN OSCAR FILE
!     DUE TO GINO TECHNIQUES IT IS USUALLY FASTER TO REWIND AND FORWARD
!     REC RATHER THAN BACKREC
!
      CALL rewind(pool)
!
!     POSITION POOL TAPE AT BEGINNING OF OSCAR FILE
!
      jj = Idpl(3)*3 + 1
      DO j = 4 , jj , 3
         IF ( Idpl(j)==noscar ) GOTO 150
      ENDDO
      CALL mesage(-61,0,0)
 150  CALL skpfil(pool,andf(Idpl(j+2),mask1)-1)
      newsq = newsq - 1
   ENDIF
   DO i = 1 , newsq
      CALL fwdrec(*1500,pool)
   ENDDO
!
!     CHECK FOR REPEAT INSTRUCTION
!
   IF ( Buf(4)==contrl(2) ) GOTO 1300
!
!     JUMP REQUEST - CHECK FOR JUMP OUT OF LOOPS
!
   newsq = rshift(andf(Buf(7),mask),16)
   kk = 3
   ceitbx = 0
 200  DO
      ceitbx = 4 + ceitbx
      IF ( ceitbx>Ceitbl(2) ) GOTO 1300
      IF ( andf(Ceitbl(ceitbx-1),lpflg)/=0 .AND. Ceitbl(ceitbx+1)/=0 ) THEN
         nbegn = rshift(andf(Ceitbl(ceitbx-1),noflgs),16)
         nend = andf(mask1,Ceitbl(ceitbx-1))
         IF ( newsq<nbegn .OR. newsq>nend ) GOTO 500
      ENDIF
   ENDDO
!
!     PROCESS  REPEAT CONTROL REQUEST
!
 300  kk = 1
 400  ceitbx = andf(Buf(7),mask1)
   IF ( Ceitbl(ceitbx)<0 ) THEN
!
!      NEGATIVE ENTRY IMPLIES VARIABLE REPT INSTRUCTION
!      FIND VALUE IN VPS AND UPDATE CEITBL
!
      ivpspt = rshift(andf(Ceitbl(ceitbx),noflgs),16)
      loop = andf(Ceitbl(ceitbx),mask1)
      ivpspt = Vps(ivpspt+3)
      Ceitbl(ceitbx) = orf(lshift(ivpspt,16),loop)
   ENDIF
!
!     CHECK FOR END OF LOOP
!
   mxloop = rshift(andf(Ceitbl(ceitbx),noflgs),16)
   loop = andf(Ceitbl(ceitbx),mask1)
   IF ( mxloop>loop ) GOTO 600
!
!     REPEATS FINISHED - ZERO LOOP COUNT AND TURN OFF LOOP FLAG
!
 500  Ceitbl(ceitbx) = andf(Ceitbl(ceitbx),mask)
   Ceitbl(ceitbx-1) = andf(Ceitbl(ceitbx-1),noflgs)
   IF ( kk==1 ) GOTO 1300
   IF ( kk==2 ) THEN
      CALL pexit
      GOTO 1500
   ELSEIF ( kk==3 ) THEN
      GOTO 200
   ENDIF
!
!     ANOTHER  TIME THRU - INCREMENT COUNTER BY 1
!
 600  Ceitbl(ceitbx) = Ceitbl(ceitbx) + 1
!
!     SET LOOP FLAG IN WORD 1 OF CEITBL ENTRY
!
   Ceitbl(ceitbx-1) = orf(Ceitbl(ceitbx-1),lpflg)
   IF ( kk==2 ) GOTO 1300
 700  newsq = rshift(andf(Buf(7),mask),16)
!
!     MAKE SURE WE ARE LOOPING
!
   IF ( newsq>=Buf(2) ) GOTO 100
!
!     IF CHECKPOINTING - BACKUP PROBLEM TAPE DICTIONARY TO BEGINNING OF
!     LOOP
!
   IF ( Icpflg==0 ) GOTO 1100
!
!     READ IN CHECKPOINT DICTIONARY
!
   itop = 2*Bfsz + 1
   ldic = korsz(Idic(itop))
   CALL open(*1700,nxptdc,Databf(Bfsz+1),0)
   CALL read(*1600,*800,nxptdc,dcparm,2,1,nrecsz)
 800  IF ( nxptdc(1)/=dcparm(1) ) CALL mesage(-61,0,0)
   CALL read(*1600,*900,nxptdc,dcparm,2,1,nrecsz)
 900  CALL read(*1600,*1000,nxptdc,Idic(itop),ldic,1,nrecsz)
   GOTO 1700
 1000 ibot = nrecsz + itop - 3
   CALL close(nxptdc,1)
   j = ibot
   DO i = itop , ibot , 3
      IF ( Idic(i)==nblank ) THEN
         IF ( andf(Idic(i+2),mask1)>=newsq ) THEN
            j = i - 3
            EXIT
         ENDIF
      ENDIF
   ENDDO
   ibot = j
!
!     WRITE IDIC ON NEW PROBLEM TAPE
!
   CALL open(*1700,nxptdc,Databf(Bfsz+1),1)
   CALL write(nxptdc,nxptdc,2,1)
   CALL write(nxptdc,dcparm,2,1)
   CALL write(nxptdc,Idic(itop),ibot+3-itop,1)
   CALL close(nxptdc,1)
!
!     SCAN FIAT FOR FILES REGENERATED NEXT TIME THRU LOOP.
!
 1100 j = Ifiat(3)*Icfiat - 2
   jj = Idpl(3)*3 + 1
   DO i = 4 , j , Icfiat
      IF ( rshift(andf(Ifiat(i),noflgs),16)<Buf(2) ) THEN
         IF ( rshift(andf(Ifiat(i),noflgs),16)/=0 ) THEN
            IF ( andf(rshift(Ifiat(i),30),1)==0 ) THEN
!
!     LTU IS LESS THAN LOOP END - CLEAR FIAT TRAILER
!
               Ifiat(i+3) = 0
               Ifiat(i+4) = 0
               Ifiat(i+5) = 0
               IF ( Icfiat/=8 ) THEN
                  Ifiat(i+8) = 0
                  Ifiat(i+9) = 0
                  Ifiat(i+10) = 0
               ENDIF
!
!     IF EQUIV, REMOVE ENTIRE ENTRY FROM FIAT
!     REMOVE ENTIRE ENTRY FROM FIAT TO FORCE REALLOCATION
!
               ihold = andf(mask1,Ifiat(i))
               Ifiat(i) = 0
               Ifiat(i+1) = 0
               Ifiat(i+2) = 0
               IF ( i<Ifiat(1)*Icfiat ) Ifiat(i) = ihold
!
!     ZERO FILE NAME IF IN DPL
!
               DO ii = 4 , jj , 3
                  IF ( Idpl(ii)==Ifiat(i+1) .AND. Idpl(ii+1)==Ifiat(i+2) ) GOTO 1110
               ENDDO
            ENDIF
            CYCLE
 1110       Idpl(ii) = 0
            Idpl(ii+1) = 0
         ENDIF
      ENDIF
   ENDDO
   GOTO 100
!
!     PROCESS  CONDITIONAL CONTROL REQUEST
!
 1200 ceitbx = andf(Buf(7),mask1)
   IF ( Vps(ceitbx)<0 ) GOTO 700
 1300 CALL close(pool,2)
   RETURN
!
!     PROCESS EXIT  CONTROL REQUESTS
!
 1400 kk = 2
   IF ( Buf(7)/=contrl(4) ) GOTO 400
   CALL pexit
 1500 CALL mesage(-2,pool,nxcei)
 1600 CALL mesage(-2,nxptdc,nxcei)
 1700 CALL mesage(-61,0,0)
END SUBROUTINE xcei
