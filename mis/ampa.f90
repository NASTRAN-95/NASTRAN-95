
SUBROUTINE ampa(Aero,Qjh,Qhh,Ajjl,Qhhlo,Qjhlo,Index,Imax,Iany)
   IMPLICIT NONE
   INTEGER Ajjcol , Idjh , Igust , Ii , Ii1 , Incr , Incr1 , Iprec , It1 , It2 , It3 , Iz(1) , Jj , Jj1 , Mcbqhh(7) , Mcbqjh(7) ,   &
         & Mcbrjh(7) , Ncol , Ngp , Ngpd(2,30) , Noh , Noue , Nout , Nsub , Qhhcol , Sysbuf , Xqhhl
   REAL Skp(52) , Xk , Xm , Z(1)
   COMMON /ampcom/ Ncol , Nsub , Xm , Xk , Ajjcol , Qhhcol , Ngp , Ngpd , Mcbqhh , Mcbqjh , Noh , Idjh , Mcbrjh
   COMMON /blank / Noue , Xqhhl , Igust
   COMMON /packx / It2 , It3 , Ii1 , Jj1 , Incr1
   COMMON /system/ Sysbuf , Nout , Skp , Iprec
   COMMON /unpakx/ It1 , Ii , Jj , Incr
   COMMON /zzzzzz/ Iz
   INTEGER Aero , Ajjl , Iany , Imax , Index , Qhh , Qhhlo , Qjh , Qjhlo
   INTEGER file , i , iaero , iajjl , ibuf1 , ibuf2 , iflag , ip1 , iqhh , izx , j , k , mcbajj(7) , name(2) , nclqhh , nclqjh ,    &
         & noajjh , nqhh , nz
   INTEGER korsz
   REAL xka , xma
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
!
!     INITIALIZE
!
   mcbajj(1) = Ajjl
   CALL rdtrl(mcbajj)
   Mcbqhh(1) = Qhh
   CALL rdtrl(Mcbqhh)
   Mcbqjh(1) = Qjh
   CALL rdtrl(Mcbqjh)
   Iany = 1
   ibuf1 = korsz(Iz) - Sysbuf + 1
   ibuf2 = ibuf1 - Sysbuf
!
!     EXTRACT DATA FROM AJJL HEADER
!
   file = Ajjl
   CALL open(*600,Ajjl,Iz(ibuf1),0)
   CALL read(*800,*900,Ajjl,Iz,-2,0,iflag)
   CALL read(*800,*100,Ajjl,Iz,ibuf2-1,0,iflag)
   ip1 = -8
   GOTO 700
!
!     PROCESS AJJL  DATA
!
 100  CALL close(Ajjl,1)
   noajjh = 0
   iajjl = 4
   IF ( iflag==0 ) THEN
!
!     NO AJJ HEADER DATA
!
      noajjh = 1
      iaero = 3
   ELSE
      Ncol = Iz(1)
      izx = 3
      Nsub = min0(Iz(izx),mcbajj(2)/Ncol)
      Ngp = Iz(2*Nsub+4)
      k = 2*Nsub + 5
      iaero = k - 1
      DO i = 1 , Ngp
         Ngpd(1,i) = Iz(k)
         Ngpd(2,i) = Iz(k+1)
         k = k + 3
      ENDDO
   ENDIF
!
!     BRING IN AERO DATA
!
   CALL gopen(Aero,Iz(ibuf1),0)
   file = Aero
   CALL fwdrec(*800,Aero)
   nz = ibuf2 - iaero
   CALL read(*800,*200,Aero,Iz(iaero),nz,0,iflag)
   ip1 = -8
   GOTO 700
!
!     AERO DATA IN CORE
!
 200  CALL close(Aero,1)
   Imax = iflag/2
   IF ( noajjh/=0 ) THEN
!
!     FIX UP FOR AJJ MISSING HEADER
!
      Ncol = mcbajj(2)/Imax
      Nsub = Imax
      Ngp = 1
      Ngpd(1,1) = 1
      Ngpd(2,1) = Ncol
      iaero = iflag + 3
      k = iaero
      DO i = 1 , iflag
         Iz(k) = Iz(i+2)
         k = k + 1
      ENDDO
   ENDIF
!
!     PUT HEADERS FROM OLD QHH IN CORE
!
   IF ( Xqhhl==1 ) GOTO 400
   file = Qhh
   CALL open(*600,Qhh,Iz(ibuf1),0)
   CALL fread(Qhh,Iz,-2,0)
   iqhh = iaero + 2*Imax + 2
   nz = nz - 2*Imax
   CALL read(*800,*300,Qhh,Iz(iqhh),nz,0,iflag)
   ip1 = -8
   GOTO 700
 300  CALL close(Qhh,1)
   iqhh = iqhh + 2
   nqhh = min0((iflag-2)/2,Mcbqhh(2)/Noh)
!
!     BUILD INDEX FILE
!
 400  i = 0
   CALL gopen(Index,Iz(ibuf1),1)
 500  Xm = Z(iaero+i)
   Xk = Z(iaero+i+1)
!
!     SEARCH FOR COLUMN NUMBER IN AJJL
!
   j = 0
   DO
      xma = Z(iajjl+j)
      xka = Z(iajjl+j+1)
      IF ( xma==Xm .AND. xka==Xk ) THEN
!
!     FOUND IN AJJL
!
         Ajjcol = (j/2)*Ncol + 1
!
!     SEARCH FOR COLUMN NUMBER IN QHH
!
         Qhhcol = 0
         IF ( Xqhhl/=1 ) THEN
            j = 0
            DO
               xma = Z(iqhh+j)
               xka = Z(iqhh+j+1)
               IF ( xma==Xm .AND. xka==Xk ) THEN
!
!     FOUND IN QHH
!
                  Qhhcol = (j/2)*Noh + 1
                  EXIT
               ELSE
                  j = j + 2
                  IF ( j>=2*nqhh ) EXIT
               ENDIF
            ENDDO
         ENDIF
!
!     WRITE ON INDEX
!
         CALL write(Index,Xm,4,1)
         IF ( Qhhcol==0 ) Iany = 0
         i = i + 2
         IF ( i<2*Imax ) GOTO 500
!
!     DONE WITH INDEX
!
         CALL close(Index,1)
!
!     COPY OLD  QHH  ONTO QHHLO
!
         IF ( Xqhhl/=1 ) THEN
            It1 = Mcbqhh(5)
            It2 = It1
            It3 = It1
            Incr = 1
            Incr1 = 1
            IF ( Mcbqhh(1)>0 ) THEN
               CALL gopen(Qhh,Iz(ibuf1),0)
               CALL gopen(Qhhlo,Iz(ibuf2),1)
               nclqhh = Mcbqhh(2)
               Mcbqhh(2) = 0
               Mcbqhh(6) = 0
               Mcbqhh(7) = 0
               Mcbqhh(1) = Qhhlo
               CALL cyct2b(Qhh,Qhhlo,nclqhh,Iz,Mcbqhh)
               CALL close(Qhh,1)
               CALL close(Qhhlo,1)
               CALL wrttrl(Mcbqhh)
            ENDIF
!
!     COPY OLD QJH ONTO QJHLO
!
!
!     COPY QJH ONTO QJHLO
!
            IF ( Mcbqjh(1)>0 ) THEN
               CALL gopen(Qjh,Iz(ibuf1),0)
               CALL gopen(Qjhlo,Iz(ibuf2),1)
               nclqjh = Mcbqjh(2)
               Mcbqjh(1) = Qjhlo
               Mcbqjh(2) = 0
               Mcbqjh(6) = 0
               Mcbqjh(7) = 0
               CALL cyct2b(Qjh,Qjhlo,nclqjh,Iz,Mcbqjh)
               CALL close(Qjh,1)
               CALL close(Qjhlo,1)
               CALL wrttrl(Mcbqjh)
            ENDIF
         ENDIF
!
!     PUT HEADERS ON NEW OUTPUT FILES
!
         IF ( Mcbqhh(1)>0 ) THEN
            file = Qhh
            CALL open(*600,Qhh,Iz(ibuf1),1)
            CALL fname(Qhh,Mcbqhh)
            CALL write(Qhh,Mcbqhh,2,0)
            CALL write(Qhh,Noh,1,0)
            CALL write(Qhh,Imax,1,0)
            CALL write(Qhh,Iz(iaero),2*Imax,1)
            CALL close(Qhh,3)
            Mcbqhh(1) = Qhh
            Mcbqhh(2) = 0
            Mcbqhh(3) = Noh
            Mcbqhh(4) = 2
            Mcbqhh(5) = 2 + Iprec
            Mcbqhh(6) = 0
            Mcbqhh(7) = 0
         ENDIF
         IF ( Mcbqjh(1)>0 ) THEN
            file = Qjh
            CALL open(*600,Qjh,Iz(ibuf1),1)
            CALL fname(Qjh,Mcbqjh)
            CALL write(Qjh,Mcbqjh,2,0)
            CALL write(Qjh,Noh,1,0)
            CALL write(Qjh,Imax,1,0)
            CALL write(Qjh,Iz(iaero),2*Imax,1)
            CALL close(Qjh,3)
            Mcbqjh(1) = Qjh
            Mcbqjh(2) = 0
            Mcbqjh(3) = Ncol
            Mcbqjh(4) = 2
            Mcbqjh(5) = 2 + Iprec
            Mcbqjh(6) = 0
            Mcbqjh(7) = 0
         ENDIF
         Iany = 0
!
!     PUT HEADER ON QHJL
!
         IF ( Igust<=0 ) RETURN
         file = Mcbrjh(1)
         CALL open(*600,file,Iz(ibuf1),1)
         CALL fname(file,Mcbrjh(2))
         CALL write(file,Mcbrjh(2),2,0)
         CALL write(file,Noh,1,0)
         CALL write(file,Imax,1,0)
         CALL write(file,Iz(iaero),2*Imax,1)
         CALL close(file,3)
         CALL makmcb(Mcbrjh,file,Ncol,2,2+Iprec)
         CALL wrttrl(Mcbrjh)
         RETURN
      ELSE
         j = j + 2
         IF ( j>=2*Nsub ) CALL mesage(-7,0,name)
      ENDIF
   ENDDO
!
!     ERROR MESSAGES
!
 600  ip1 = -1
 700  CALL mesage(ip1,file,name)
 800  ip1 = -2
   GOTO 700
 900  ip1 = -3
   GOTO 700
END SUBROUTINE ampa