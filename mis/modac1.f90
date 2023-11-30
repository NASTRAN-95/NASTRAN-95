
SUBROUTINE modac1(Casecc,Tol,Tol1,Casezz,Caseyy)
   IMPLICIT NONE
   INTEGER Id , Iz(1) , Nfn , Nfo , Nz , Sysbuf
   REAL Z(1)
   COMMON /modac3/ Nfo , Nfn , Nz , Id
   COMMON /system/ Sysbuf
   COMMON /zzzzzz/ Iz
   INTEGER Casecc , Caseyy , Casezz , Tol , Tol1
   REAL diff , diff1 , r , real
   INTEGER file , flag , i , ibuf(6) , ibuf1 , ibuf2 , ibuf3 , icc , ifrout , ifset , ihd(2) , ilist , ilsym , ip1 , isetf ,        &
         & isetnf , ivec , ix , j , k , lw , m , mcb(7) , n , name(2) , nlist , nsetf , nzx
!
!     MODAC1 REDUCES THE NUMBER OF ENTRIES ON TOL TO THE TIMES
!         SPECIFIED BY THE OFREQ SET IN CASECC
!
!     CORE IS        OUT AS FOLLOWS ON RETURN
!
!         CONTENTS            LENGTH  TYPE   POINTER
!         --------            ------  ----   -------
!         NEW TIMES           NFN      R     IFN
!         KEEP REMOVE         NFO      I     IKR
!
!
!
!
!
!
!
!
!
!
!
!
!
   EQUIVALENCE (Z(1),Iz(1))
   DATA name/4HMODA , 4HC1  /
!
!     BRING  IN  CASECC
!
   lw = 6
   IF ( Id==4 ) lw = 7
   ibuf1 = Nz - Sysbuf + 1
   ibuf2 = ibuf1 - Sysbuf
   ibuf3 = ibuf2 - Sysbuf
   CALL gopen(Casecc,Iz(ibuf1),0)
   file = Casecc
   CALL read(*800,*100,Casecc,Iz,ibuf2-1,0,ivec)
   CALL mesage(-8,0,name)
 100  icc = 0
   CALL close(Casecc,1)
   ifrout = 145
   ilsym = 200
   ivec = ivec + 1
   ilist = ivec
   IF ( Id==5 ) THEN
!
!     STATIC ANALYSIS
!
      r = 1.0
      Nfo = Nfo + ilist
      nlist = Nfo - 2
      DO i = ilist , Nfo , 2
         Z(i) = r
         Iz(i+1) = 0
         r = r + 1.
      ENDDO
!
!     COPY EDT
!
      CALL open(*300,Tol,Iz(ibuf1),0)
      CALL open(*300,Tol1,Iz(ibuf2),1)
      file = Tol
      CALL fname(Tol1,ihd)
      CALL write(Tol1,ihd,2,0)
      DO
         CALL read(*700,*1100,Tol,Iz(Nfo+2),Nz,0,flag)
         CALL write(Tol1,Iz(Nfo+2),flag,1)
      ENDDO
   ELSE
!
!     BRING IN OLD TIME/FREQ  LIST
!
      file = Tol
      CALL open(*800,Tol,Iz(ibuf1),0)
      i = ilist
      m = 3
      ix = 2
      Nfo = Nfo + i
      IF ( Id==2 .OR. Id==4 ) THEN
         CALL fwdrec(*1000,Tol)
         CALL fwdrec(*1000,Tol)
         DO
            CALL read(*1000,*200,Tol,ibuf,lw,0,flag)
            Iz(i) = ibuf(4)
!     REIG SHOULD BE ON CYCLES
            IF ( Id==4 ) Iz(i) = ibuf(5)
            Iz(i+1) = 0
            i = i + 2
            IF ( i==Nfo ) EXIT
         ENDDO
      ELSE
         DO
            CALL read(*1000,*200,Tol,ibuf,m,0,flag)
            Iz(i) = ibuf(m)
            Iz(i+1) = 0
            i = i + ix
            m = 1
         ENDDO
      ENDIF
   ENDIF
 200  CALL close(Tol,1)
   nlist = i - ix
!
!     MATCH LIST OF  SELECTED VALUES WITH TIME LIST IN CORE
!
 300  ix = icc + ifrout
   ifset = Iz(ix)
   IF ( ifset>0 ) THEN
      ix = icc + ilsym
      isetnf = ix + Iz(ix) + 1
      DO
         isetf = isetnf + 2
         nsetf = Iz(isetnf+1) + isetf - 1
         IF ( Iz(isetnf)==ifset ) THEN
            DO i = isetf , nsetf
               k = 0
               diff = 1.E25
               real = Z(i)
               DO j = ilist , nlist , 2
                  IF ( Iz(j+1)==0 ) THEN
                     diff1 = abs(Z(j)-real)
                     IF ( diff1<diff ) THEN
                        diff = diff1
                        k = j
                     ENDIF
                  ENDIF
               ENDDO
               IF ( k/=0 ) Iz(k+1) = 1
            ENDDO
            GOTO 400
         ELSE
            isetnf = nsetf + 1
            IF ( isetnf>=ivec ) THEN
               ifset = -1
               EXIT
            ENDIF
         ENDIF
      ENDDO
   ENDIF
   DO j = ilist , nlist , 2
      Iz(j+1) = 1
   ENDDO
!
!     SELECTED FREQUENCIES MARKED FOR OUTPUT
!
 400  Nfo = (nlist-ilist+2)/2
!
!     MOVE NEW FREQ  TO UPPER
!
   k = 1
   DO i = ilist , nlist , 2
      IF ( Iz(i+1)/=0 ) THEN
         Z(k) = Z(i)
         k = k + 1
      ENDIF
   ENDDO
   Nfn = k - 1
   DO i = ilist , nlist , 2
      Iz(k) = Iz(i+1)
      k = k + 1
   ENDDO
   IF ( Id==5 ) RETURN
   file = Tol1
   CALL open(*500,Tol1,Iz(ibuf1),1)
   CALL fname(Tol1,ihd)
   CALL write(Tol1,ihd,2,0)
   IF ( Id==2 .OR. Id==4 ) THEN
!
!     COPY OVER CLAMA STUFF
!
      CALL write(Tol1,0,0,1)
      k = Nfn + Nfo + 1
      nzx = ibuf3 - k
      file = Tol
      CALL gopen(Tol,Iz(ibuf2),0)
      CALL read(*1000,*1100,Tol,Iz(k),146,1,flag)
      CALL write(Tol1,Iz(k),146,1)
      m = Nfn + 1
      n = m + Nfo - 1
      DO i = m , n
         CALL read(*1000,*1100,Tol,Iz(k),lw,0,flag)
         IF ( Iz(i)/=0 ) CALL write(Tol1,Iz(k),lw,0)
      ENDDO
      CALL close(Tol,1)
      CALL write(Tol1,0,0,1)
   ELSE
      CALL write(Tol1,Z,Nfn,1)
   ENDIF
   CALL close(Tol1,1)
   mcb(1) = Tol1
   mcb(2) = Nfn
   CALL wrttrl(mcb)
   IF ( Id==2 ) THEN
!
!      COPY OVER CASECC
!
      CALL gopen(Casecc,Iz(ibuf1),0)
      CALL gopen(Casezz,Iz(ibuf2),1)
      m = Nfn + 1
      n = m + Nfo - 1
      DO i = m , n
         CALL read(*600,*420,Casecc,Iz(k),nzx,0,flag)
 420     IF ( Iz(i)/=0 ) CALL write(Casezz,Iz(k),flag,1)
      ENDDO
      GOTO 600
   ENDIF
 500  RETURN
 600  CALL close(Casecc,1)
   CALL close(Casezz,1)
   mcb(1) = Casecc
   CALL rdtrl(mcb)
   mcb(1) = Casezz
   CALL wrttrl(mcb)
   RETURN
 700  CALL close(Tol,1)
   CALL close(Tol1,1)
   mcb(1) = Tol
   CALL rdtrl(mcb)
   mcb(1) = Tol1
   CALL wrttrl(mcb)
   GOTO 300
!
!     ERROR MESSAGES
!
 800  ip1 = -1
 900  CALL mesage(ip1,file,name)
 1000 ip1 = -2
   GOTO 900
 1100 ip1 = -3
   GOTO 900
END SUBROUTINE modac1
