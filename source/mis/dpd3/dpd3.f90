!*==dpd3.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dpd3
   USE c_blank
   USE c_condas
   USE c_dpdcom
   USE c_names
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(20) :: bufr
   REAL :: delf , delt , delta , f , f0 , fe , fi , fn , t0 , ti , twopi
   INTEGER :: file , flag , i , ifrq , ifrq1 , ifrq2 , ilist , irps , irt1 , irt2 , ix , j , j1 , jn , jx , k , n , nfrq1 , nfrq2 , &
            & ngrid , nlist , nofrq , nofrq1 , nofrq2 , nort , nort1 , nort2 , nrps , nrt1
   INTEGER , DIMENSION(2) , SAVE :: freq2 , randps , randt1 , randt2
   REAL , DIMENSION(1) :: zz
   EXTERNAL close , fname , locate , mesage , open , preloc , read , sort , sortf , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     DPD3 ASSEMBLES THE FREQUENCY RESPONSE LIST (FRL)
!     AND THE POWER SPECTRAL DENSITY LIST (PSDL).
!
   !>>>>EQUIVALENCE (Consts(2),Twopi) , (Z(1),Zz(1)) , (Buf(1),Bufr(1)) , (Msg(2),Ngrid)
   DATA freq2 , randps , randt1 , randt2/1107 , 11 , 2107 , 21 , 2207 , 22 , 2307 , 23/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     OPEN DYNAMICS POOL. SET POINTERS.
!
         file = dpool
         CALL preloc(*240,z(buf1),dpool)
         nofrq1 = 0
         nofrq2 = 0
         nofrq = 0
         ifrq1 = 1
         ifrq2 = ifrq1
         ifrq = ifrq1
         i = ifrq1
         j = i
!
!     READ FREQ1 CARDS. CONVERT F1 AND DELTA F TO RADIANS.
!
         CALL locate(*40,z(buf1),freq1,flag)
         nofrq1 = 1
         DO
            CALL read(*260,*20,dpool,z(i),4,0,flag)
            zz(i+1) = twopi*zz(i+1)
            zz(i+2) = twopi*zz(i+2)
            i = i + 4
         ENDDO
 20      nfrq1 = i - 4
         ifrq2 = i
         ifrq = i
         j = i
!
!     READ FREQ2 CARDS. CONVERT FREQUENCIES TO RADIANS.
!
 40      CALL locate(*80,z(buf1),freq2,flag)
         nofrq2 = 1
         DO
            CALL read(*260,*60,dpool,z(i),4,0,flag)
            zz(i+1) = twopi*zz(i+1)
            zz(i+2) = twopi*zz(i+2)
            i = i + 4
         ENDDO
 60      nfrq2 = i - 4
         ifrq = i
         j = i
!
!     READ FREQ CARDS. CONVERT FREQUENCIES TO RADIANS.
!
 80      CALL locate(*100,z(buf1),freq,flag)
         nofrq = 1
         SPAG_Loop_1_1: DO
            CALL read(*260,*100,dpool,z(j+1),1,0,flag)
            j = j + 2
            DO
               CALL read(*260,*280,dpool,z(j),1,0,flag)
               IF ( z(j)==-1 ) THEN
                  z(i) = j - (i+1)
                  i = j
                  CYCLE SPAG_Loop_1_1
               ELSE
                  zz(j) = twopi*zz(j)
                  j = j + 1
               ENDIF
            ENDDO
            EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
!
!     TEST FOR ANY FREQ TYPE CARDS.
!
 100     nofrl = nofrq1 + nofrq2 + nofrq
         IF ( nofrl==0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     COLLECT LIST OF FREQUENCY SET IDS AND POINTERS TO CARDS.
!     SORT THIS LIST ON SET ID.
!
         ilist = j + 1
         i = ilist
         IF ( nofrq1/=0 ) THEN
!
!     FOR FREQ1 SET STORE SET ID, POINTER TO SET, 0.
!
            DO k = ifrq1 , nfrq1 , 4
               z(i) = z(k)
               z(i+1) = k
               z(i+2) = 0
               i = i + 3
            ENDDO
            nlist = i - 3
         ENDIF
         IF ( nofrq2/=0 ) THEN
!
!     FOR FREQ2 SET STORE SET ID, POINTER TO SET, -1.
!
            DO k = ifrq2 , nfrq2 , 4
               z(i) = z(k)
               z(i+1) = k
               z(i+2) = -1
               i = i + 3
            ENDDO
            nlist = i - 3
         ENDIF
         IF ( nofrq==0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     FOR FREQ SET STORE SET ID, POINTER TO SET, NO. OF WORDS IN SET.
!
         j = ifrq
         DO
            n = z(j)
            IF ( n==-1 ) THEN
               nlist = i - 3
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ELSE
               j = j + 1
               z(i) = z(j)
               z(i+1) = j
               z(i+2) = n
               i = i + 3
               j = j + n
            ENDIF
         ENDDO
 120     ineq = 0
         spag_nextblock_1 = 2
      CASE (2)
         nofrl = -1
         spag_nextblock_1 = 4
      CASE (3)
         n = i - ilist
         CALL sort(0,0,3,1,z(ilist),n)
!
!     OPEN THE FRL. WRITE NAME + SET IDS IN HEADER.
!
         file = frl
         CALL open(*120,frl,z(buf2),wrtrew)
         CALL fname(frl,buf)
         CALL write(frl,buf,2,0)
         DO i = ilist , nlist , 3
            buf(1) = z(i)
            CALL write(frl,buf,1,0)
         ENDDO
         CALL write(frl,0,0,1)
!
!     WRITE THE FRL ONE RECORD PER FREQUENCY SET.
!     CONVERT FREQ1 SETS TO LOOK LIKE FREQ SETS.
!     CONVERT FREQ2 SETS TO LOOK LIKE FREQ SETS.
!
         DO i = ilist , nlist , 3
            j = z(i+1)
            n = z(i+2)
            IF ( n<0 ) THEN
!
!     FREQ2 SET-- FORM F = F0*10.0**((I-1)*DELTA)
!     WHERE DELTA = (LOG10(FE/F0))/N AND I = 1 THRU N+1.
!
               f0 = zz(j+1)
               fe = zz(j+2)
               n = z(j+3)
               fn = n
               delta = (alog10(fe/f0))/fn
               fi = 0.
               n = n + 1
               DO k = 1 , n
                  f = f0*10.0**(fi*delta)
                  CALL write(frl,f,1,0)
                  fi = fi + 1.0
               ENDDO
               CALL write(frl,0,0,1)
            ELSEIF ( n==0 ) THEN
!
!     FREQ1 SET-- FORM F = F0 + (I-1)*DELTA F, WHERE I = 1 THRU N+1.
!
               f0 = zz(j+1)
               delf = zz(j+2)
               n = z(j+3) + 1
               fi = 0.
               DO k = 1 , n
                  f = f0 + fi*delf
                  CALL write(frl,f,1,0)
                  fi = fi + 1.0
               ENDDO
               CALL write(frl,0,0,1)
            ELSE
!
!     FREQ SET ---  SORT FREQUENCY LIST AND DISCARD ANY DUPLICATES.
!     THEN WRITE FREQUENCIES ON THE FRL
!
               n = n - 1
               IF ( n/=1 ) THEN
                  CALL sortf(0,0,1,1,z(j+1),n)
                  j1 = j + 2
                  jn = j + n
                  ix = j + 1
                  DO jx = j1 , jn
                     IF ( z(jx)/=z(ix) ) THEN
                        ix = ix + 1
                        z(ix) = z(jx)
                     ENDIF
                  ENDDO
                  n = ix - j
               ENDIF
               CALL write(frl,z(j+1),n,1)
            ENDIF
         ENDDO
!
!     CLOSE FRL AND WRITE TRAILER.
!
         mcb(1) = frl
         mcb(2) = (nlist-ilist)/3 + 1
         CALL wrttrl(mcb)
         CALL close(frl,clsrew)
         ineq = 0
         spag_nextblock_1 = 4
      CASE (4)
!
!     OPEN PSDL. IF PURGED, BYPASS PSDL PROCESSING.
!     OTHERWISE, LOCATE RANDPS CARDS. IF ABSENT, BYPASS PSDL PROCESSING.
!
         file = psdl
         CALL open(*220,psdl,z(buf2),wrtrew)
         CALL locate(*220,z(buf1),randps,flag)
!
!     READ RANDPS CARDS INTO CORE.
!
         irps = 1
         file = dpool
         CALL read(*260,*140,dpool,z(irps),buf2-irps,1,nrps)
         n = -8
         CALL mesage(n,file,nam)
         RETURN
 140     irt1 = irps + nrps
         irt2 = irt1
         i = irt1
         j = i
         nort1 = 0
         nort2 = 0
!
!     READ RANDT1 CARDS.
!
         CALL locate(*180,z(buf1),randt1,flag)
         CALL read(*260,*160,dpool,z(irt1),buf2-irt1,1,nort1)
         n = -8
         CALL mesage(n,file,nam)
         RETURN
 160     irt2 = irt1 + nort1
         nrt1 = irt2 - 4
         i = irt2
         j = i
!
!     READ RANDT2 CARDS.
!
 180     CALL locate(*200,z(buf1),randt2,flag)
         nort2 = 1
         SPAG_Loop_1_2: DO
            CALL read(*260,*200,dpool,z(j+1),1,0,flag)
            j = j + 2
            DO
               CALL read(*260,*280,dpool,z(j),1,0,flag)
               IF ( z(j)==-1 ) THEN
                  z(i) = j - (i+1)
                  i = j
                  CYCLE SPAG_Loop_1_2
               ELSE
                  j = j + 1
                  IF ( j>=buf2 ) THEN
                     n = -8
                     CALL mesage(n,file,nam)
                     RETURN
                  ENDIF
               ENDIF
            ENDDO
            EXIT SPAG_Loop_1_2
         ENDDO SPAG_Loop_1_2
!
!     COLLECT LIST OF RANDT1 AND RANDT2 SET IDS AND POINTERS TO DATA.
!
 200     nort = nort1 + nort2
         IF ( nort/=0 ) THEN
            ilist = j + 1
            i = ilist
            IF ( nort1/=0 ) THEN
!
!     FOR RANDT1 SETS STORE SET ID, POINTER TO SET, 0.
!
               DO k = irt1 , nrt1 , 4
                  z(i) = z(k)
                  z(i+1) = k
                  z(i+2) = 0
                  i = i + 3
               ENDDO
               nlist = i - 3
               IF ( i>buf2 ) THEN
                  n = -8
                  CALL mesage(n,file,nam)
                  RETURN
               ENDIF
            ENDIF
            IF ( nort2/=0 ) THEN
!
!     FOR RANDT2 SETS STORE SET ID, POINTER TO SET, NO. OF WORDS IN SET.
!
               j = irt2
               SPAG_Loop_1_3: DO
                  n = z(j)
                  IF ( n==-1 ) THEN
                     nlist = i - 3
                     EXIT SPAG_Loop_1_3
                  ELSE
                     z(i) = z(j)
                     z(i+1) = j
                     z(i+2) = n
                     i = i + 3
                     j = j + n
                     IF ( i>=buf2 ) THEN
                        n = -8
                        CALL mesage(n,file,nam)
                        RETURN
                     ENDIF
                  ENDIF
               ENDDO SPAG_Loop_1_3
            ENDIF
!
!     SORT LIST ON SET ID.
!
            n = i - ilist
            CALL sort(0,0,3,1,z(ilist),n)
         ENDIF
!
!     WRITE SET IDS FOR RANDT1 AND RANDT2 CARDS IN HEADER RECORD OF
!     PSDL. THEN WRITE RANDPS DATA AS FIRST RECORD OF PSDL.
!
         CALL fname(psdl,buf)
         CALL write(psdl,buf,2,0)
         IF ( nort/=0 ) THEN
            DO i = ilist , nlist , 3
               CALL write(psdl,z(i),1,0)
            ENDDO
         ENDIF
         CALL write(psdl,0,0,1)
         CALL write(psdl,z(irps),nrps,1)
         IF ( nort/=0 ) THEN
!
!     WRITE ONE RECORD ON PSDL FOR EACH RANDT1 OR RANDT2 SET.
!
            DO i = ilist , nlist , 3
               j = z(i+1)
               n = z(i+2)
               IF ( n==0 ) THEN
!
!     RANDT1 SET-- WRITE TI = T0 + (I-1)*DELTA T, WHERE I = 1 THRU N+1.
!
                  n = z(j+1)
                  fn = n
                  delt = (zz(j+3)-zz(j+2))/fn
                  t0 = zz(j+2)
                  fi = 0.
                  n = n + 1
                  DO k = 1 , n
                     ti = t0 + fi*delt
                     CALL write(psdl,ti,1,0)
                     fi = fi + 1.0
                  ENDDO
                  CALL write(psdl,0,0,1)
               ELSE
!
!     RANDT2 SET--  SORT DATA AND DISCARD ANY DUPLICATES. THEN WRITE SET
!
                  n = n - 1
                  IF ( n/=1 ) THEN
                     CALL sortf(0,0,1,1,z(j+1),n)
                     j1 = j + 2
                     jn = j + n
                     ix = j + 1
                     DO jx = j1 , jn
                        IF ( z(jx)/=z(ix) ) THEN
                           ix = ix + 1
                           z(ix) = z(jx)
                        ENDIF
                     ENDDO
                     n = ix - j
                  ENDIF
                  CALL write(psdl,z(j+1),n,1)
               ENDIF
            ENDDO
         ENDIF
!
!     CLOSE FILES, WRITE TRAILER AND EXIT.
!
         mcb(1) = psdl
         mcb(2) = (nlist-ilist)/3 + 1
!      2147483647  = 2**31 - 1
         IF ( nort==0 ) mcb(2) = 2147483647
         CALL wrttrl(mcb)
         ineq = 0
         nopsdl = 1
 220     CALL close(dpool,clsrew)
         CALL close(psdl,clsrew)
         RETURN
!
!     FATAL FILE ERRORS
!
 240     n = -1
         CALL mesage(n,file,nam)
         RETURN
 260     n = -2
         CALL mesage(n,file,nam)
         RETURN
 280     n = -3
         CALL mesage(n,file,nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE dpd3
