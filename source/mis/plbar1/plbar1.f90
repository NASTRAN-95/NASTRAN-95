!*==plbar1.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE plbar1(Ido,Lcore)
   USE c_loadx
   USE c_matin
   USE c_ssga1x
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ido
   INTEGER :: Lcore
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: bar , ibg , iect , iept , n , nwds , oldid
   REAL :: flag
   INTEGER :: i , ipg
   INTEGER , DIMENSION(7) , SAVE :: islt
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) , SAVE :: nam
   REAL , DIMENSION(6) :: pa , pb
   REAL , DIMENSION(42) :: pg
   REAL , DIMENSION(9) :: ta , tb
   EXTERNAL close , fwdrec , gbtran , glbbas , gopen , mat , mesage , pload1 , read
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS SUBROUTINE SETS UP THE DATA NEEDED TO CALL PLOAD1
!     TO GET THE APPLIED CONCENTRATED, UNIFORMLY OR LINEARLY DISTRIBUTED
!     LOADS, ON A BAR ELEMENT FROM A PLOAD1 CARD
!     AND INSERTS THE VECTOR INO PV
!
   !>>>>EQUIVALENCE (pg(1),iz(1))
   DATA nam/4HPLBA , 4HR1  / , n , oldid , islt/9*0/
   DATA iect , iept , ibg , nwds , bar/1 , 16 , 34 , 42 , 34/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     INITIALIZE AND OPEN EST
!
         IF ( n/=0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL gopen(est,iz(Lcore),0)
         CALL read(*60,*20,est,i,1,0,flag)
 20      DO WHILE ( i/=bar )
            CALL fwdrec(*60,est)
            CALL read(*60,*20,est,i,1,0,flag)
         ENDDO
         spag_nextblock_1 = 2
      CASE (2)
!
!     READ SLT THEN FIND BAR ELEMENT
!
         CALL read(*40,*40,slt,islt,7,0,flag)
         IF ( islt(1)/=oldid ) THEN
            SPAG_Loop_1_1: DO
               CALL read(*60,*60,est,iz(iect),nwds,0,flag)
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
                  inflag = 1
                  temp = pg(ibg+8)
                  matid = iz(iept)
                  CALL mat(oldid)
                  EXIT SPAG_Loop_1_1
               ELSE
                  GOTO 60
               ENDIF
            ENDDO SPAG_Loop_1_1
         ENDIF
         CALL pload1(1,islt,pg(iect+3),pg(iect+9),pg(iect+12),pg(ibg+1),pg(ibg+5),pa,pb,ta,tb,islt,iz(iect))
!
!     INSERT INTO PV
!
         ipg = iz(iect+1) - 1
         DO i = 1 , 6
            pv(ipg+i) = pv(ipg+i) + pa(i)
         ENDDO
         ipg = iz(iect+2) - 1
         DO i = 1 , 6
            pv(ipg+i) = pv(ipg+i) + pb(i)
         ENDDO
         n = n + 1
         IF ( n==Ido ) THEN
            n = 0
            oldid = 0
            CALL close(est,1)
         ENDIF
         RETURN
!
!     ERROR
!
 40      CALL mesage(-1,slt,nam)
 60      WRITE (nout,99001) islt(1) , ilid
99001    FORMAT ('0*** USER FATAL MESSAGE 2286, CBAR ELEMENT',I9,' REFERENCED ON PLOAD1',I9,' NOT FOUND')
         CALL mesage(-61,0,nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE plbar1
