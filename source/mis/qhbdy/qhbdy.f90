!*==qhbdy.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE qhbdy
   IMPLICIT NONE
   USE C_CONDAS
   USE C_LOADX
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   REAL :: af , fact , factx , flag , length , pi , q0
   REAL , DIMENSION(4,4) :: bgpdt
   INTEGER , DIMENSION(7) :: card
   REAL , DIMENSION(4) :: data4 , p , x , y , z
   INTEGER , DIMENSION(4) :: grids , order , sils
   INTEGER :: i , i1 , i2 , i3 , iflag , imap , isil , l , ngrids , nmap
   INTEGER , DIMENSION(5) , SAVE :: igrids
   INTEGER , DIMENSION(15) , SAVE :: map
   REAL , DIMENSION(3) :: r12 , r13
   INTEGER , DIMENSION(2) , SAVE :: subr
   EXTERNAL fndpnt , fndsil , mesage , permut , read , saxb
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!*****
!
!  THIS ROUTINE APPLIES THE LOADS DUE TO A SELECTED HEAT FLUX
!  LOADING CONDITION.
!
!  DATA CARD IS...
!
!  QHBDY  SETID  FLAG  Q0  AF  G1  G2  G3  G4
!                ============================
!                ABOVE FIELDS AVAILABLE TO THIS ROUTINE ONLY.
!                GRIDS ARE IN INTERNAL NOTATION AT THIS POINT.
!*****
!
!
!
   !>>>>EQUIVALENCE (Consts(1),Pi)
   !>>>>EQUIVALENCE (x(1),bgpdt(1,2)) , (y(1),bgpdt(1,3)) , (z(1),bgpdt(1,4))
   !>>>>EQUIVALENCE (iflag,card(1)) , (q0,card(2)) , (af,card(3))
   !>>>>EQUIVALENCE (grids(1),card(4),sils(1))
!
   DATA map/1 , 2 , 3 , 1 , 2 , 4 , 2 , 3 , 1 , 3 , 4 , 2 , 4 , 1 , 3/
   DATA igrids/1 , 2 , 2 , 3 , 4/
   DATA subr/4HQHBD , 4HY   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!*****
!  READ AND PROCESS ONE QHBDY IMAGE PER CALL TO THIS ROUTINE.
!*****
         CALL read(*20,*40,Slt,card(1),7,0,flag)
         ngrids = igrids(iflag)
!*****
!  OBTAIN A GRID (INTERNAL) POINT SORT VECTOR SO AS TO CALL FOR BGPDT
!  DATA EFFICIENTLY.
!*****
         IF ( ngrids<=1 ) THEN
            order(1) = 1
         ELSE
            CALL permut(grids(1),order(1),ngrids,Old)
         ENDIF
!*****
!  PICK UP BGPDT FOR THE 1 TO 4 POINTS AND OBTAIN THE SILS.
!*****
         DO i = 1 , ngrids
            l = order(i)
            CALL fndpnt(data4(1),grids(l))
            bgpdt(l,1) = data4(1)
            bgpdt(l,2) = data4(2)
            bgpdt(l,3) = data4(3)
            bgpdt(l,4) = data4(4)
            CALL fndsil(grids(l))
         ENDDO
!*****
!  ALL DATA IS AT HAND FOR LOAD CALCULATIONS
!*****
         af = af*q0
         IF ( iflag==2 ) THEN
!*****
!  IFLAG=2  A LINE...
!*****
            length = sqrt((x(2)-x(1))**2+(y(2)-y(1))**2+(z(2)-z(1))**2)
            p(1) = af*length*0.50E0
            p(2) = p(1)
         ELSEIF ( iflag==3 ) THEN
!*****
!  IFLAG=3  A LINE OF REVOLUTION...
!*****
            fact = pi*q0*sqrt((x(2)-x(1))**2+(z(2)-z(1))**2)/3.0E0
            p(1) = fact*(2.0E0*x(1)+x(2))
            p(2) = fact*(x(1)+2.0E0*x(2))
         ELSEIF ( iflag==4 ) THEN
!*****
!  IFLAG=4  A TRIANGLE...
!*****
            fact = q0/6.0E0
            imap = 1
            nmap = 3
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( iflag==5 ) THEN
!*****
!  IFLAG=5  A QUADRILATERAL...
!*****
            fact = q0/12.0E0
            imap = 4
            nmap = 15
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSE
!*****
!  IFLAG=1   A POINT...
!*****
            p(1) = af
         ENDIF
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (2)
!*****
!  MAP 1 OR 4 TRIANGLES INTO 3 OR 4 POINTS.
!*****
         p(1) = 0.0E0
         p(2) = 0.0E0
         p(3) = 0.0E0
         p(4) = 0.0E0
         DO i = imap , nmap , 3
            i1 = map(i)
            i2 = map(i+1)
            i3 = map(i+2)
            r12(1) = x(i2) - x(i1)
            r12(2) = y(i2) - y(i1)
            r12(3) = z(i2) - z(i1)
            r13(1) = x(i3) - x(i1)
            r13(2) = y(i3) - y(i1)
            r13(3) = z(i3) - z(i1)
            CALL saxb(r12(1),r13(1),r12(1))
            factx = fact*sqrt(r12(1)**2+r12(2)**2+r12(3)**2)
            p(i1) = p(i1) + factx
            p(i2) = p(i2) + factx
            p(i3) = p(i3) + factx
         ENDDO
         spag_nextblock_1 = 3
      CASE (3)
!*****
!  LOAD VALUES COMPLETE.
!*****
         DO i = 1 , ngrids
            isil = sils(i)
            Core(isil) = Core(isil) + p(i)
         ENDDO
         RETURN
!*****
!  END OF FILE OR END OF RECORD HIT ERROR.
!*****
 20      CALL mesage(-2,Slt,subr)
 40      CALL mesage(-3,Slt,subr)
         GOTO 20
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE qhbdy
