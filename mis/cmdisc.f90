
SUBROUTINE cmdisc
   IMPLICIT NONE
   REAL Buf1 , Buf4 , Buf5 , Casecc , Conset , Geom4 , Scbdat , Scmcon , Sctoc , Tdat(6) , Toler
   INTEGER Buf2 , Buf3 , Combo(7,5) , Iauto , Inpt , Lcore , Nipnew , Npsub , Outt , Scconn , Score , Scr1 , Scr2 , Scsfil , Z(1)
   COMMON /cmb001/ Scr1 , Scr2 , Scbdat , Scsfil , Scconn , Scmcon , Sctoc , Geom4 , Casecc
   COMMON /cmb002/ Buf1 , Buf2 , Buf3 , Buf4 , Buf5 , Score , Lcore , Inpt , Outt
   COMMON /cmb003/ Combo , Conset , Iauto , Toler , Npsub
   COMMON /cmb004/ Tdat , Nipnew
   COMMON /zzzzzz/ Z
   INTEGER aaa(2) , ce(9) , de(9) , i , id , ifile , ilen , imsg , ip , iptr(7) , istrt , isvcor , itot , j , kdh , kk , len , loc ,&
         & ncsub , nn , nnn , nwd , scdisc
   INTEGER orf
   EXTERNAL orf
!
!     THIS SUBROUTINE DETERMINES THE DISCONNECTED DEGREES OF FREEDOM
!     AND GENERATES  DISCONNECTION ENTRIES  WHICH ARE MERGED WITH THE
!     CONNECTION ENTRIES
!
   DATA aaa/4HCMDI , 4HSC  /
!
!
   nwd = Npsub + 2
   isvcor = Score
   itot = 0
   ilen = Lcore
   nn = 0
   kk = Score
   CALL open(*800,Scsfil,Z(Buf3),0)
!
!     LOOP ON THE NUMBER OF PSEUDO STRUCTURES READING THE SIL,C TABLE
!     INTO CORE FOR EACH.  THE ARRAY IPTR(I) POINTS TO THE START OF
!     THE I-TH TABLE IN CORE
!
   DO i = 1 , Npsub
      ncsub = Combo(i,5)
!
!     FIND SIL, C TABLE
!
      DO j = 1 , ncsub
         CALL fwdrec(*900,Scsfil)
      ENDDO
      kk = kk + nn
      iptr(i) = kk
      CALL read(*900,*50,Scsfil,Z(kk),Lcore,1,nn)
      GOTO 1000
!
!     ZERO OUT SIL VALUES, LOCATION WILL STORE CNEW
!
 50   DO j = 1 , nn , 2
         Z(kk+j-1) = 0
      ENDDO
      Lcore = Lcore - nn
      itot = itot + nn
      CALL skpfil(Scsfil,1)
   ENDDO
   CALL close(Scsfil,1)
!
!     ALL EQSS HAVE BEEN PROCESSED, NOW SCAN THE CONNECTION ENTRIES
!     AND GET CNEW VALUES.
!
   CALL open(*800,Scconn,Z(Buf3),0)
!
!     READ AND PROCESS CONNECTION ENTRIES ONE AT A TIME
!
 100  CALL read(*300,*200,Scconn,ce,10,1,nn)
 200  DO i = 1 , Npsub
      IF ( ce(2+i)/=0 ) THEN
!
!     TRANSLATE CODED IP TO ACTUAL IP, COMPUTE LOCATION IN OPEN CORE
!     AND UPDATE CNEW
!
         ip = ce(2+i) - 1000000*(ce(2+i)/1000000)
         loc = iptr(i) + 2*ip - 2
         Z(loc) = orf(Z(loc),ce(1))
      ENDIF
   ENDDO
   GOTO 100
!
!     ALL CONNECTIONS HAVE BEEN ACCOUNTED FOR,NOW DETERMINE DISCONN.
!
 300  scdisc = Scr1
   IF ( Scr1==Scconn ) scdisc = Scr2
   CALL open(*800,scdisc,Z(Buf2),1)
   DO i = 1 , Npsub
      IF ( i<Npsub ) len = iptr(i+1) - iptr(i)
      IF ( i==Npsub ) len = itot - iptr(i)
      istrt = iptr(i)
      DO j = 1 , len , 2
         DO kdh = 1 , 9
            de(kdh) = 0
         ENDDO
         ip = j/2 + 1
         loc = istrt + j - 1
!
!     POINT IS TOTALLY DISCONNECTED
!
         IF ( Z(loc)/=Z(loc+1) ) THEN
            IF ( Z(loc)/=0 ) THEN
!
!     POINT IS PARTIALLY DISCONNECTED
!
               de(1) = Z(loc+1) - Z(loc)
               de(2) = 2**i
               de(2+i) = ip
            ELSE
!
!     POINT IS TOTALLY CONNECTED
!
               de(1) = Z(loc+1)
               de(2) = 2**i
               de(2+i) = ip
            ENDIF
            CALL write(scdisc,de,nwd,1)
         ENDIF
      ENDDO
   ENDDO
   CALL eof(scdisc)
   CALL close(scdisc,1)
   kk = Score
   Lcore = ilen
   CALL open(*800,scdisc,Z(Buf2),0)
   CALL rewind(Scconn)
   id = 1
 400  CALL read(*500,*600,scdisc,Z(kk),Lcore,1,nnn)
   GOTO 1000
 500  id = 2
   CALL read(*700,*600,Scconn,Z(kk),Lcore,1,nnn)
   GOTO 1000
 600  kk = kk + nwd
   Lcore = Lcore - nwd
   IF ( Lcore<nwd ) GOTO 1000
   IF ( id/=1 ) GOTO 500
   GOTO 400
 700  CALL close(Scconn,1)
   CALL close(scdisc,1)
   CALL open(*800,Scconn,Z(Buf3),1)
   len = kk - Score
   Nipnew = len/nwd
   DO i = 1 , len , nwd
      Z(Score+i) = iabs(Z(Score+i))
   ENDDO
   CALL sort(0,0,nwd,2,Z(Score),len)
   DO i = 1 , len , nwd
      CALL write(Scconn,Z(Score+i-1),nwd,1)
   ENDDO
   CALL eof(Scconn)
   CALL close(Scconn,1)
   CALL close(scdisc,1)
   Score = isvcor
   Lcore = ilen
   RETURN
!
 800  imsg = -1
   GOTO 1100
 900  imsg = -2
   GOTO 1100
 1000 imsg = -8
 1100 CALL mesage(imsg,ifile,aaa)
END SUBROUTINE cmdisc