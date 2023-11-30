
SUBROUTINE plbar1(Ido,Lcore)
   IMPLICIT NONE
   REAL D1(5) , D2(11) , Pv(1) , Slt , Temp
   INTEGER Est , Ibuff , Ilid , Inflag , Lc , Matid , Nout
   COMMON /loadx / Lc , Slt , D1 , Est , D2 , Ilid
   COMMON /matin / Matid , Inflag , Temp
   COMMON /ssga1x/ Pv
   COMMON /system/ Ibuff , Nout
   INTEGER Ido , Lcore
   INTEGER bar , i , ibg , iect , iept , ipg , islt(7) , iz(1) , n , nam(2) , nwds , oldid
   REAL flag , pa(6) , pb(6) , pg(42) , ta(9) , tb(9)
!
!     THIS SUBROUTINE SETS UP THE DATA NEEDED TO CALL PLOAD1
!     TO GET THE APPLIED CONCENTRATED, UNIFORMLY OR LINEARLY DISTRIBUTED
!     LOADS, ON A BAR ELEMENT FROM A PLOAD1 CARD
!     AND INSERTS THE VECTOR INO PV
!
   EQUIVALENCE (pg(1),iz(1))
   DATA nam/4HPLBA , 4HR1  / , n , oldid , islt/9*0/
   DATA iect , iept , ibg , nwds , bar/1 , 16 , 34 , 42 , 34/
!
!     INITIALIZE AND OPEN EST
!
   IF ( n/=0 ) GOTO 200
   CALL gopen(Est,iz(Lcore),0)
   CALL read(*400,*100,Est,i,1,0,flag)
 100  DO WHILE ( i/=bar )
      CALL fwdrec(*400,Est)
      CALL read(*400,*100,Est,i,1,0,flag)
   ENDDO
!
!     READ SLT THEN FIND BAR ELEMENT
!
 200  CALL read(*300,*300,Slt,islt,7,0,flag)
   IF ( islt(1)/=oldid ) THEN
      DO
         CALL read(*400,*400,Est,iz(iect),nwds,0,flag)
         oldid = iz(iect)
         IF ( iz(iect)<islt(1) ) THEN
         ELSEIF ( iz(iect)==islt(1) ) THEN
!
!     CONVERT COORD. SYSTEMS
!
            IF ( iz(iect+6)/=0 ) CALL glbbas(pg(iect+3),pg(iect+3),pg(ibg+1),iz(iect+6))
            IF ( iz(ibg)/=0 ) CALL glbbas(pg(iect+9),pg(iect+9),pg(ibg+1),iz(ibg))
            IF ( iz(ibg+4)/=0 ) CALL glbbas(pg(iect+12),pg(iect+12),pg(ibg+5),iz(ibg+4))
            CALL gbtran(iz(ibg),iz(ibg+1),ta)
            CALL gbtran(iz(ibg+4),iz(ibg+5),tb)
!
!     DATA READY
!
            Inflag = 1
            Temp = pg(ibg+8)
            Matid = iz(iept)
            CALL mat(oldid)
            EXIT
         ELSE
            GOTO 400
         ENDIF
      ENDDO
   ENDIF
   CALL pload1(1,islt,pg(iect+3),pg(iect+9),pg(iect+12),pg(ibg+1),pg(ibg+5),pa,pb,ta,tb,islt,iz(iect))
!
!     INSERT INTO PV
!
   ipg = iz(iect+1) - 1
   DO i = 1 , 6
      Pv(ipg+i) = Pv(ipg+i) + pa(i)
   ENDDO
   ipg = iz(iect+2) - 1
   DO i = 1 , 6
      Pv(ipg+i) = Pv(ipg+i) + pb(i)
   ENDDO
   n = n + 1
   IF ( n==Ido ) THEN
      n = 0
      oldid = 0
      CALL close(Est,1)
   ENDIF
   GOTO 99999
!
!     ERROR
!
 300  CALL mesage(-1,Slt,nam)
 400  WRITE (Nout,99001) islt(1) , Ilid
99001 FORMAT ('0*** USER FATAL MESSAGE 2286, CBAR ELEMENT',I9,' REFERENCED ON PLOAD1',I9,' NOT FOUND')
   CALL mesage(-61,0,nam)
!
99999 RETURN
END SUBROUTINE plbar1
