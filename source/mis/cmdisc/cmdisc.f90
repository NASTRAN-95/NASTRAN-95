!*==cmdisc.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmdisc
   IMPLICIT NONE
   USE C_CMB001
   USE C_CMB002
   USE C_CMB003
   USE C_CMB004
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: aaa
   INTEGER , DIMENSION(9) :: ce , de
   INTEGER :: i , id , ifile , ilen , imsg , ip , istrt , isvcor , itot , j , kdh , kk , len , loc , ncsub , nn , nnn , nwd , scdisc
   INTEGER , DIMENSION(7) :: iptr
   EXTERNAL close , eof , fwdrec , mesage , open , orf , read , rewind , skpfil , sort , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS SUBROUTINE DETERMINES THE DISCONNECTED DEGREES OF FREEDOM
!     AND GENERATES  DISCONNECTION ENTRIES  WHICH ARE MERGED WITH THE
!     CONNECTION ENTRIES
!
   DATA aaa/4HCMDI , 4HSC  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
         nwd = Npsub + 2
         isvcor = Score
         itot = 0
         ilen = Lcore
         nn = 0
         kk = Score
         CALL open(*120,Scsfil,Z(Buf3),0)
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
               CALL fwdrec(*140,Scsfil)
            ENDDO
            kk = kk + nn
            iptr(i) = kk
            CALL read(*140,*10,Scsfil,Z(kk),Lcore,1,nn)
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
!
!     ZERO OUT SIL VALUES, LOCATION WILL STORE CNEW
!
 10         DO j = 1 , nn , 2
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
         CALL open(*120,Scconn,Z(Buf3),0)
         spag_nextblock_1 = 2
      CASE (2)
!
!     READ AND PROCESS CONNECTION ENTRIES ONE AT A TIME
!
         CALL read(*40,*20,Scconn,ce,10,1,nn)
 20      DO i = 1 , Npsub
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
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     ALL CONNECTIONS HAVE BEEN ACCOUNTED FOR,NOW DETERMINE DISCONN.
!
 40      scdisc = Scr1
         IF ( Scr1==Scconn ) scdisc = Scr2
         CALL open(*120,scdisc,Z(Buf2),1)
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
         CALL open(*120,scdisc,Z(Buf2),0)
         CALL rewind(Scconn)
         id = 1
         spag_nextblock_1 = 3
      CASE (3)
         CALL read(*60,*80,scdisc,Z(kk),Lcore,1,nnn)
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 60      id = 2
         CALL read(*100,*80,Scconn,Z(kk),Lcore,1,nnn)
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 80      kk = kk + nwd
         Lcore = Lcore - nwd
         IF ( Lcore<nwd ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( id==1 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         GOTO 60
 100     CALL close(Scconn,1)
         CALL close(scdisc,1)
         CALL open(*120,Scconn,Z(Buf3),1)
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
 120     imsg = -1
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 140     imsg = -2
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
         imsg = -8
         spag_nextblock_1 = 5
      CASE (5)
         CALL mesage(imsg,ifile,aaa)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE cmdisc
