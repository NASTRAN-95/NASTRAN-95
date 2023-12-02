!*==ruler.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ruler(Rule,Icp,Zrct,Onct,List,N,Buff,Iopt)
   IMPLICIT NONE
   USE C_TWO
   USE C_ZNTPKX
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Rule
   INTEGER , DIMENSION(1) :: Icp
   INTEGER :: Zrct
   INTEGER :: Onct
   INTEGER , DIMENSION(1) :: List
   INTEGER :: N
   REAL , DIMENSION(1) :: Buff
   INTEGER :: Iopt
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a , r
   INTEGER :: i , is , j , j1 , k , m , n1 , namcp , oct , zct
   EXTERNAL close , gopen , intpk , orf , zntpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     DETERMINES STRING OF ZEROS AND ONES IN LIST BY APPLYING RULE TO
!     CP.
!
!
!     PICK UP PARAMETERS
!
         Eol = 0
         r = Rule
         namcp = Icp(1)
         zct = 0
         oct = 0
         ASSIGN 24 TO is
         IF ( r>=0.0 ) ASSIGN 22 TO is
         r = abs(r)
         L = 0
         j1 = 0
         m = 0
         n1 = N
         IF ( namcp/=0 ) THEN
            CALL gopen(namcp,Buff,0)
            CALL intpk(*20,namcp,0,1,0)
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 20      m = n1
         Eol = 1
         spag_nextblock_1 = 2
      CASE (2)
         DO i = 1 , n1
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  j = (i+31)/32
                  IF ( m<i ) THEN
                     IF ( Eol==0 ) THEN
                        CALL zntpki
                     ELSE
                        L = n1
                        A1(1) = 0.0
                     ENDIF
                  ENDIF
                  IF ( L==i ) THEN
                     a = A1(1)
                  ELSE
                     m = L
                     a = 0.0
                  ENDIF
                  IF ( Iopt/=1 .AND. j>j1 ) THEN
                     j1 = j
                     List(j) = 0
                  ENDIF
                  GOTO is
 22               IF ( a/=r ) THEN
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  spag_nextblock_2 = 3
                  CYCLE SPAG_DispatchLoop_2
 24               IF ( a<r ) THEN
                  ELSEIF ( a==r ) THEN
                     spag_nextblock_2 = 3
                     CYCLE SPAG_DispatchLoop_2
                  ELSE
                     CYCLE
                  ENDIF
                  spag_nextblock_2 = 2
               CASE (2)
                  oct = oct + 1
                  IF ( Iopt==1 ) THEN
                     List(i) = oct
                  ELSE
                     k = i - ((i-1)/32)*32
                     List(j) = orf(List(j),Two1(k))
                  ENDIF
                  CYCLE
               CASE (3)
                  zct = zct + 1
                  IF ( Iopt/=0 ) List(i) = -zct
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         Zrct = zct
         Onct = oct
         IF ( namcp/=0 ) CALL close(namcp,1)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ruler
