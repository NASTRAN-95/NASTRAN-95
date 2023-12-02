!*==cmdisc.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmdisc
   USE c_cmb001
   USE c_cmb002
   USE c_cmb003
   USE c_cmb004
   USE c_zzzzzz
   IMPLICIT NONE
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
         nwd = npsub + 2
         isvcor = score
         itot = 0
         ilen = lcore
         nn = 0
         kk = score
         CALL open(*120,scsfil,z(buf3),0)
!
!     LOOP ON THE NUMBER OF PSEUDO STRUCTURES READING THE SIL,C TABLE
!     INTO CORE FOR EACH.  THE ARRAY IPTR(I) POINTS TO THE START OF
!     THE I-TH TABLE IN CORE
!
         DO i = 1 , npsub
            ncsub = combo(i,5)
!
!     FIND SIL, C TABLE
!
            DO j = 1 , ncsub
               CALL fwdrec(*140,scsfil)
            ENDDO
            kk = kk + nn
            iptr(i) = kk
            CALL read(*140,*10,scsfil,z(kk),lcore,1,nn)
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
!
!     ZERO OUT SIL VALUES, LOCATION WILL STORE CNEW
!
 10         DO j = 1 , nn , 2
               z(kk+j-1) = 0
            ENDDO
            lcore = lcore - nn
            itot = itot + nn
            CALL skpfil(scsfil,1)
         ENDDO
         CALL close(scsfil,1)
!
!     ALL EQSS HAVE BEEN PROCESSED, NOW SCAN THE CONNECTION ENTRIES
!     AND GET CNEW VALUES.
!
         CALL open(*120,scconn,z(buf3),0)
         spag_nextblock_1 = 2
      CASE (2)
!
!     READ AND PROCESS CONNECTION ENTRIES ONE AT A TIME
!
         CALL read(*40,*20,scconn,ce,10,1,nn)
 20      DO i = 1 , npsub
            IF ( ce(2+i)/=0 ) THEN
!
!     TRANSLATE CODED IP TO ACTUAL IP, COMPUTE LOCATION IN OPEN CORE
!     AND UPDATE CNEW
!
               ip = ce(2+i) - 1000000*(ce(2+i)/1000000)
               loc = iptr(i) + 2*ip - 2
               z(loc) = orf(z(loc),ce(1))
            ENDIF
         ENDDO
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     ALL CONNECTIONS HAVE BEEN ACCOUNTED FOR,NOW DETERMINE DISCONN.
!
 40      scdisc = scr1
         IF ( scr1==scconn ) scdisc = scr2
         CALL open(*120,scdisc,z(buf2),1)
         DO i = 1 , npsub
            IF ( i<npsub ) len = iptr(i+1) - iptr(i)
            IF ( i==npsub ) len = itot - iptr(i)
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
               IF ( z(loc)/=z(loc+1) ) THEN
                  IF ( z(loc)/=0 ) THEN
!
!     POINT IS PARTIALLY DISCONNECTED
!
                     de(1) = z(loc+1) - z(loc)
                     de(2) = 2**i
                     de(2+i) = ip
                  ELSE
!
!     POINT IS TOTALLY CONNECTED
!
                     de(1) = z(loc+1)
                     de(2) = 2**i
                     de(2+i) = ip
                  ENDIF
                  CALL write(scdisc,de,nwd,1)
               ENDIF
            ENDDO
         ENDDO
         CALL eof(scdisc)
         CALL close(scdisc,1)
         kk = score
         lcore = ilen
         CALL open(*120,scdisc,z(buf2),0)
         CALL rewind(scconn)
         id = 1
         spag_nextblock_1 = 3
      CASE (3)
         CALL read(*60,*80,scdisc,z(kk),lcore,1,nnn)
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 60      id = 2
         CALL read(*100,*80,scconn,z(kk),lcore,1,nnn)
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 80      kk = kk + nwd
         lcore = lcore - nwd
         IF ( lcore<nwd ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( id==1 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         GOTO 60
 100     CALL close(scconn,1)
         CALL close(scdisc,1)
         CALL open(*120,scconn,z(buf3),1)
         len = kk - score
         nipnew = len/nwd
         DO i = 1 , len , nwd
            z(score+i) = iabs(z(score+i))
         ENDDO
         CALL sort(0,0,nwd,2,z(score),len)
         DO i = 1 , len , nwd
            CALL write(scconn,z(score+i-1),nwd,1)
         ENDDO
         CALL eof(scconn)
         CALL close(scconn,1)
         CALL close(scdisc,1)
         score = isvcor
         lcore = ilen
         RETURN
!
 120     imsg = -1
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 140     imsg = -2
         spag_nextblock_1 = 5
      CASE (4)
         imsg = -8
         spag_nextblock_1 = 5
      CASE (5)
         CALL mesage(imsg,ifile,aaa)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE cmdisc
