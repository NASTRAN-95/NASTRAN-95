!*==fndset.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fndset(Gpid,X,Ibuf,N)
   USE c_blank
   USE c_names
   USE c_xxparm
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Gpid
   REAL , DIMENSION(3,1) :: X
   INTEGER :: Ibuf
   INTEGER :: N
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , insub , j
   REAL , SAVE :: twopi
   REAL , DIMENSION(3) :: u
   EXTERNAL bckrec , close , fread , fwdrec , gopen , read
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     GPID = GRID POINT TABLE FOR THIS SET
!
!     N  = 0 INPUT
!     FNDSET READS THE COORDINATES OF THE GRID POINTS IN THIS SET.
!     IF THE GRID POINT TABLE VALUE IS ZERO THE CORRESPONDING GRID
!     POINT IS NOT USED IN THIS SET AND ITS VALUES SKIPPED, OTHERWISE
!     THE XYZ COORDINDATE VALUES ARE READ FROM BGPDT AND PACKED INTO
!     X SPACE. TOTALLY THERE ARE NGPSET GRID DATA SAVED IN X.
!     CORE NEEDED FOR X = 3*NGPSET (PROVIDED BY CALLING ROUTINE)
!
!     N  = 1 INPUT/OUTPUT
!     FNDSET POSITIONS THE STRESS FILE TO THE SUBCASE/VALUE LAST
!     PROCESSED
!
   !>>>>EQUIVALENCE (u(1),insub)
   DATA twopi/0.0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         IF ( N/=0 ) THEN
!
!     POSITION OES1
!
            IF ( twopi<6.2 ) twopi = 8.0*atan(1.0)
            CALL gopen(oes1,Gpid(Ibuf),inprew)
         ELSE
            CALL gopen(bgpdt,Gpid(Ibuf),inprew)
            j = 1
            DO i = 1 , ngp
               IF ( Gpid(i)/=0 ) THEN
                  CALL fread(bgpdt,0,-1,0)
                  CALL fread(bgpdt,X(1,j),3,0)
                  j = j + 1
               ELSE
                  CALL fread(bgpdt,0,-4,0)
               ENDIF
            ENDDO
            CALL close(bgpdt,rew)
            RETURN
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         CALL read(*20,*20,oes1,j,1,0,i)
         CALL fread(oes1,0,-2,0)
         CALL fread(oes1,u,3,0)
         IF ( subc==insub ) THEN
            IF ( flag<1.0 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( flag/=1.0 ) THEN
               j = j/10
!
!     REAL EIGENVALUE ANALYSIS - CONVERT TO FREQUENCY
!
               IF ( j==2 ) u(3) = sqrt(abs(u(3)))/twopi
               IF ( data-u(3)<=1.0E-6 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( data==u(2) ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!     WRONG CASE
!
         CALL fwdrec(*20,oes1)
         CALL fwdrec(*20,oes1)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 20      N = N + 1
         spag_nextblock_1 = 3
      CASE (3)
         CALL bckrec(oes1)
         CALL close(oes1,norew)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE fndset
