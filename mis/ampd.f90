
SUBROUTINE ampd(Qjhua,Qhho,Skj,Gki,Qhh,Scr1,Scr2,Scr3,Scr4)
   IMPLICIT NONE
   INTEGER Ajjcol , Ii , Incr , Iprec , Itc , Iz(1) , Jj , Mcbqhh(7) , Mcbqjh(7) , Ncol , Ngp , Ngpd(2,30) , Noh , Noue , Nout ,    &
         & Nsub , Qhhcol , Sysbuf
   REAL Skp(52) , Xk , Xm
   COMMON /ampcom/ Ncol , Nsub , Xm , Xk , Ajjcol , Qhhcol , Ngp , Ngpd , Mcbqhh , Mcbqjh , Noh
   COMMON /blank / Noue
   COMMON /system/ Sysbuf , Nout , Skp , Iprec
   COMMON /unpakx/ Itc , Ii , Jj , Incr
   COMMON /zzzzzz/ Iz
   INTEGER Gki , Qhh , Qhho , Qjhua , Scr1 , Scr2 , Scr3 , Scr4 , Skj
   INTEGER file , i , ibuf1 , ibuf2 , ip1 , k , mcb(7) , name(2) , ncolj , qkh
   INTEGER korsz
!
!     THE PURPOSE OF THIS ROUTINE IS TO COMPUTE(OR RETRIEVE) QHH
!
!     QHH EITHER EXISTS ON QHHO (AS COLUMN QCOL) OR MUST BE COMPUTED
!     AS FOLLOWS
!
!     1. QKH = SKJ*QJH
!     2. QIH = GKI(T)*QKH
!     3. QHH = 1 QIH 1
!              1-----1
!              1 0   1
!              1     1
!
   DATA name/4HAMPD , 4H    /
!
   ibuf1 = korsz(Iz) - Sysbuf + 1
   ibuf2 = ibuf1 - Sysbuf
   Incr = 1
   Itc = Mcbqhh(5)
!
!     DETERMINE IF QHH EXISTS ON QHHO
!
   IF ( Qhhcol==0 ) THEN
!
!     QHH MUST BE COMPUTED
!
!
!     COPY SKJ TO SCR4 FOR PROPER M-K PAIR
!
      CALL gopen(Skj,Iz(ibuf1),0)
      CALL gopen(Scr4,Iz(ibuf2),1)
      k = Ajjcol - 1
      CALL skprec(Skj,k)
      mcb(1) = Qjhua
      CALL rdtrl(mcb)
      ncolj = mcb(3)
      mcb(1) = Skj
      CALL rdtrl(mcb)
      Mcbqjh(3) = mcb(3)
      mcb(1) = Scr4
      mcb(2) = 0
      mcb(6) = 0
      mcb(7) = 0
      Itc = mcb(5)
      CALL cyct2b(Skj,Scr4,ncolj,Iz,mcb)
      CALL close(Skj,1)
      CALL close(Scr4,1)
      CALL wrttrl(mcb)
      CALL ssg2b(Scr4,Qjhua,0,Scr1,0,Iprec,1,Scr2)
!
!     COPY SCR1(QKH) TO OUTPUT
!
      qkh = Mcbqjh(1)
      IF ( qkh>0 ) THEN
         Itc = Mcbqjh(5)
         Incr = 1
         CALL gopen(Scr1,Iz(ibuf1),0)
         CALL gopen(qkh,Iz(ibuf2),3)
         CALL cyct2b(Scr1,qkh,Noh,Iz,Mcbqjh)
         CALL close(qkh,3)
         CALL close(Scr1,1)
      ENDIF
      CALL ssg2b(Gki,Scr1,0,Scr3,1,Iprec,1,Scr2)
!
!     COPY TO QHH
!
      CALL gopen(Qhh,Iz(ibuf1),3)
      CALL gopen(Scr3,Iz(ibuf2),0)
      Itc = Mcbqhh(5)
      Incr = 1
      CALL cyct2b(Scr3,Qhh,Noh,Iz,Mcbqhh)
      CALL close(Scr3,1)
      CALL close(Qhh,3)
      RETURN
   ELSE
!
!     COPY FROM QHHO TO QHH
!
      CALL gopen(Qhh,Iz(ibuf1),3)
      CALL gopen(Qhho,Iz(ibuf2),0)
      k = Qhhcol - 1
      IF ( k/=0 ) THEN
         file = Qhho
         DO i = 1 , k
            CALL fwdrec(*100,Qhho)
         ENDDO
      ENDIF
      CALL cyct2b(Qhho,Qhh,Noh,Iz,Mcbqhh)
      CALL close(Qhho,1)
      CALL close(Qhh,3)
      RETURN
   ENDIF
 100  DO
!
!     ERRORS
!
      ip1 = -2
      CALL mesage(ip1,file,name)
   ENDDO
END SUBROUTINE ampd