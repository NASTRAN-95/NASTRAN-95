!*==ampa.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ampa(Aero,Qjh,Qhh,Ajjl,Qhhlo,Qjhlo,Index,Imax,Iany)
   USE c_ampcom
   USE c_blank
   USE c_packx
   USE c_system
   USE c_unpakx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Aero
   INTEGER :: Qjh
   INTEGER :: Qhh
   INTEGER :: Ajjl
   INTEGER :: Qhhlo
   INTEGER :: Qjhlo
   INTEGER :: Index
   INTEGER :: Imax
   INTEGER :: Iany
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: file , i , iaero , iajjl , ibuf1 , ibuf2 , iflag , ip1 , iqhh , izx , j , k , nclqhh , nclqjh , noajjh , nqhh , nz
   INTEGER , DIMENSION(7) :: mcbajj
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL :: xka , xma
   REAL , DIMENSION(1) :: z
   EXTERNAL close , cyct2b , fname , fread , fwdrec , gopen , korsz , makmcb , mesage , open , rdtrl , read , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THE PURPOSE OF THIS ROUTINE IS TO
!         1. INITIALIZE QHJ AND QHH
!         2. COPY USEFUL DATA FROM QJH AND QHH TO QHHLO AND  QJHLO
!         3. SET UP INDEX, IMAX,IANY, AND AMPCOM
!
!     OPEN CORE IS LAID OUT AS FOLLOWS
!
!     CONTENTS                POINTER                  LENGTH
!     --------                -------                  ------
!     AJJL HEADER
!     NCOL
!     NSUB
!     M-K PAIRS               IAJJL                    2*NSUB +2
!      .
!      .
!      .
!     AERO RECORD 2           IAERO                    2* IMAX
!     M- K  PAIRS
!      .
!      .
!      .
!     QHH HEADER RECORD(RST)
!     NOH (OLD)
!     M- K PAIRS              IQHH                     2*NQHH
!       .
!       .
!       .
!     BUFFER2                 IBUF2
!     BUFFER1                 IBUF1
!
!
!     SPECIAL CODE EXISTS IN CASE AJJK HEADER HAS ONLY 2 WORDS
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
   DATA name/4HAMPA , 4H..../
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     INITIALIZE
!
         mcbajj(1) = Ajjl
         CALL rdtrl(mcbajj)
         mcbqhh(1) = Qhh
         CALL rdtrl(mcbqhh)
         mcbqjh(1) = Qjh
         CALL rdtrl(mcbqjh)
         Iany = 1
         ibuf1 = korsz(iz) - sysbuf + 1
         ibuf2 = ibuf1 - sysbuf
!
!     EXTRACT DATA FROM AJJL HEADER
!
         file = Ajjl
         CALL open(*80,Ajjl,iz(ibuf1),0)
         CALL read(*100,*120,Ajjl,iz,-2,0,iflag)
         CALL read(*100,*20,Ajjl,iz,ibuf2-1,0,iflag)
         ip1 = -8
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     PROCESS AJJL  DATA
!
 20      CALL close(Ajjl,1)
         noajjh = 0
         iajjl = 4
         IF ( iflag==0 ) THEN
!
!     NO AJJ HEADER DATA
!
            noajjh = 1
            iaero = 3
         ELSE
            ncol = iz(1)
            izx = 3
            nsub = min0(iz(izx),mcbajj(2)/ncol)
            ngp = iz(2*nsub+4)
            k = 2*nsub + 5
            iaero = k - 1
            DO i = 1 , ngp
               ngpd(1,i) = iz(k)
               ngpd(2,i) = iz(k+1)
               k = k + 3
            ENDDO
         ENDIF
!
!     BRING IN AERO DATA
!
         CALL gopen(Aero,iz(ibuf1),0)
         file = Aero
         CALL fwdrec(*100,Aero)
         nz = ibuf2 - iaero
         CALL read(*100,*40,Aero,iz(iaero),nz,0,iflag)
         ip1 = -8
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     AERO DATA IN CORE
!
 40      CALL close(Aero,1)
         Imax = iflag/2
         IF ( noajjh/=0 ) THEN
!
!     FIX UP FOR AJJ MISSING HEADER
!
            ncol = mcbajj(2)/Imax
            nsub = Imax
            ngp = 1
            ngpd(1,1) = 1
            ngpd(2,1) = ncol
            iaero = iflag + 3
            k = iaero
            DO i = 1 , iflag
               iz(k) = iz(i+2)
               k = k + 1
            ENDDO
         ENDIF
!
!     PUT HEADERS FROM OLD QHH IN CORE
!
         IF ( xqhhl==1 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         file = Qhh
         CALL open(*80,Qhh,iz(ibuf1),0)
         CALL fread(Qhh,iz,-2,0)
         iqhh = iaero + 2*Imax + 2
         nz = nz - 2*Imax
         CALL read(*100,*60,Qhh,iz(iqhh),nz,0,iflag)
         ip1 = -8
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 60      CALL close(Qhh,1)
         iqhh = iqhh + 2
         nqhh = min0((iflag-2)/2,mcbqhh(2)/noh)
         spag_nextblock_1 = 2
      CASE (2)
!
!     BUILD INDEX FILE
!
         i = 0
         CALL gopen(Index,iz(ibuf1),1)
         SPAG_Loop_1_2: DO
            xm = z(iaero+i)
            xk = z(iaero+i+1)
!
!     SEARCH FOR COLUMN NUMBER IN AJJL
!
            j = 0
            DO
               xma = z(iajjl+j)
               xka = z(iajjl+j+1)
               IF ( xma==xm .AND. xka==xk ) THEN
!
!     FOUND IN AJJL
!
                  ajjcol = (j/2)*ncol + 1
!
!     SEARCH FOR COLUMN NUMBER IN QHH
!
                  qhhcol = 0
                  IF ( xqhhl/=1 ) THEN
                     j = 0
                     SPAG_Loop_3_1: DO
                        xma = z(iqhh+j)
                        xka = z(iqhh+j+1)
                        IF ( xma==xm .AND. xka==xk ) THEN
!
!     FOUND IN QHH
!
                           qhhcol = (j/2)*noh + 1
                           EXIT SPAG_Loop_3_1
                        ELSE
                           j = j + 2
                           IF ( j>=2*nqhh ) EXIT SPAG_Loop_3_1
                        ENDIF
                     ENDDO SPAG_Loop_3_1
                  ENDIF
!
!     WRITE ON INDEX
!
                  CALL write(Index,xm,4,1)
                  IF ( qhhcol==0 ) Iany = 0
                  i = i + 2
                  IF ( i<2*Imax ) CYCLE SPAG_Loop_1_2
!
!     DONE WITH INDEX
!
                  CALL close(Index,1)
!
!     COPY OLD  QHH  ONTO QHHLO
!
                  IF ( xqhhl/=1 ) THEN
                     it1 = mcbqhh(5)
                     it2 = it1
                     it3 = it1
                     incr = 1
                     incr1 = 1
                     IF ( mcbqhh(1)>0 ) THEN
                        CALL gopen(Qhh,iz(ibuf1),0)
                        CALL gopen(Qhhlo,iz(ibuf2),1)
                        nclqhh = mcbqhh(2)
                        mcbqhh(2) = 0
                        mcbqhh(6) = 0
                        mcbqhh(7) = 0
                        mcbqhh(1) = Qhhlo
                        CALL cyct2b(Qhh,Qhhlo,nclqhh,iz,mcbqhh)
                        CALL close(Qhh,1)
                        CALL close(Qhhlo,1)
                        CALL wrttrl(mcbqhh)
                     ENDIF
!
!     COPY OLD QJH ONTO QJHLO
!
!
!     COPY QJH ONTO QJHLO
!
                     IF ( mcbqjh(1)>0 ) THEN
                        CALL gopen(Qjh,iz(ibuf1),0)
                        CALL gopen(Qjhlo,iz(ibuf2),1)
                        nclqjh = mcbqjh(2)
                        mcbqjh(1) = Qjhlo
                        mcbqjh(2) = 0
                        mcbqjh(6) = 0
                        mcbqjh(7) = 0
                        CALL cyct2b(Qjh,Qjhlo,nclqjh,iz,mcbqjh)
                        CALL close(Qjh,1)
                        CALL close(Qjhlo,1)
                        CALL wrttrl(mcbqjh)
                     ENDIF
                  ENDIF
!
!     PUT HEADERS ON NEW OUTPUT FILES
!
                  IF ( mcbqhh(1)>0 ) THEN
                     file = Qhh
                     CALL open(*80,Qhh,iz(ibuf1),1)
                     CALL fname(Qhh,mcbqhh)
                     CALL write(Qhh,mcbqhh,2,0)
                     CALL write(Qhh,noh,1,0)
                     CALL write(Qhh,Imax,1,0)
                     CALL write(Qhh,iz(iaero),2*Imax,1)
                     CALL close(Qhh,3)
                     mcbqhh(1) = Qhh
                     mcbqhh(2) = 0
                     mcbqhh(3) = noh
                     mcbqhh(4) = 2
                     mcbqhh(5) = 2 + iprec
                     mcbqhh(6) = 0
                     mcbqhh(7) = 0
                  ENDIF
                  IF ( mcbqjh(1)>0 ) THEN
                     file = Qjh
                     CALL open(*80,Qjh,iz(ibuf1),1)
                     CALL fname(Qjh,mcbqjh)
                     CALL write(Qjh,mcbqjh,2,0)
                     CALL write(Qjh,noh,1,0)
                     CALL write(Qjh,Imax,1,0)
                     CALL write(Qjh,iz(iaero),2*Imax,1)
                     CALL close(Qjh,3)
                     mcbqjh(1) = Qjh
                     mcbqjh(2) = 0
                     mcbqjh(3) = ncol
                     mcbqjh(4) = 2
                     mcbqjh(5) = 2 + iprec
                     mcbqjh(6) = 0
                     mcbqjh(7) = 0
                  ENDIF
                  Iany = 0
!
!     PUT HEADER ON QHJL
!
                  IF ( igust<=0 ) RETURN
                  file = mcbrjh(1)
                  CALL open(*80,file,iz(ibuf1),1)
                  CALL fname(file,mcbrjh(2))
                  CALL write(file,mcbrjh(2),2,0)
                  CALL write(file,noh,1,0)
                  CALL write(file,Imax,1,0)
                  CALL write(file,iz(iaero),2*Imax,1)
                  CALL close(file,3)
                  CALL makmcb(mcbrjh,file,ncol,2,2+iprec)
                  CALL wrttrl(mcbrjh)
                  RETURN
               ELSE
                  j = j + 2
                  IF ( j>=2*nsub ) CALL mesage(-7,0,name)
               ENDIF
            ENDDO
            EXIT SPAG_Loop_1_2
         ENDDO SPAG_Loop_1_2
!
!     ERROR MESSAGES
!
 80      ip1 = -1
         spag_nextblock_1 = 3
      CASE (3)
         CALL mesage(ip1,file,name)
 100     ip1 = -2
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 120     ip1 = -3
         spag_nextblock_1 = 3
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ampa
