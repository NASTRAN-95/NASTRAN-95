!*==ampd.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ampd(Qjhua,Qhho,Skj,Gki,Qhh,Scr1,Scr2,Scr3,Scr4)
   USE c_ampcom
   USE c_blank
   USE c_system
   USE c_unpakx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Qjhua
   INTEGER :: Qhho
   INTEGER :: Skj
   INTEGER :: Gki
   INTEGER :: Qhh
   INTEGER :: Scr1
   INTEGER :: Scr2
   INTEGER :: Scr3
   INTEGER :: Scr4
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: file , i , ibuf1 , ibuf2 , ip1 , k , ncolj , qkh
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL close , cyct2b , fwdrec , gopen , korsz , mesage , rdtrl , skprec , ssg2b , wrttrl
!
! End of declarations rewritten by SPAG
!
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
   ibuf1 = korsz(iz) - sysbuf + 1
   ibuf2 = ibuf1 - sysbuf
   incr = 1
   itc = mcbqhh(5)
!
!     DETERMINE IF QHH EXISTS ON QHHO
!
   IF ( qhhcol==0 ) THEN
!
!     QHH MUST BE COMPUTED
!
!
!     COPY SKJ TO SCR4 FOR PROPER M-K PAIR
!
      CALL gopen(Skj,iz(ibuf1),0)
      CALL gopen(Scr4,iz(ibuf2),1)
      k = ajjcol - 1
      CALL skprec(Skj,k)
      mcb(1) = Qjhua
      CALL rdtrl(mcb)
      ncolj = mcb(3)
      mcb(1) = Skj
      CALL rdtrl(mcb)
      mcbqjh(3) = mcb(3)
      mcb(1) = Scr4
      mcb(2) = 0
      mcb(6) = 0
      mcb(7) = 0
      itc = mcb(5)
      CALL cyct2b(Skj,Scr4,ncolj,iz,mcb)
      CALL close(Skj,1)
      CALL close(Scr4,1)
      CALL wrttrl(mcb)
      CALL ssg2b(Scr4,Qjhua,0,Scr1,0,iprec,1,Scr2)
!
!     COPY SCR1(QKH) TO OUTPUT
!
      qkh = mcbqjh(1)
      IF ( qkh>0 ) THEN
         itc = mcbqjh(5)
         incr = 1
         CALL gopen(Scr1,iz(ibuf1),0)
         CALL gopen(qkh,iz(ibuf2),3)
         CALL cyct2b(Scr1,qkh,noh,iz,mcbqjh)
         CALL close(qkh,3)
         CALL close(Scr1,1)
      ENDIF
      CALL ssg2b(Gki,Scr1,0,Scr3,1,iprec,1,Scr2)
!
!     COPY TO QHH
!
      CALL gopen(Qhh,iz(ibuf1),3)
      CALL gopen(Scr3,iz(ibuf2),0)
      itc = mcbqhh(5)
      incr = 1
      CALL cyct2b(Scr3,Qhh,noh,iz,mcbqhh)
      CALL close(Scr3,1)
      CALL close(Qhh,3)
      RETURN
   ELSE
!
!     COPY FROM QHHO TO QHH
!
      CALL gopen(Qhh,iz(ibuf1),3)
      CALL gopen(Qhho,iz(ibuf2),0)
      k = qhhcol - 1
      IF ( k/=0 ) THEN
         file = Qhho
         DO i = 1 , k
            CALL fwdrec(*100,Qhho)
         ENDDO
      ENDIF
      CALL cyct2b(Qhho,Qhh,noh,iz,mcbqhh)
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
