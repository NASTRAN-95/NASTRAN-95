!*==finder.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE finder(Nam,Subno,Comno)
   USE c_cmb001
   USE c_cmb002
   USE c_cmb003
   USE c_cmbfnd
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: Nam
   INTEGER :: Subno
   INTEGER :: Comno
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: cnam
   INTEGER :: i , ieor , j , ncom , nnn
   INTEGER , DIMENSION(3) :: id
   EXTERNAL close , open , read , rewind
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
!     THIS SUBROUTINE READS THE TABLE OF CONTENTS OF SUBSTRUCTURES
!     BEING COMBINED ( SCRATCH FILE SCTOC ) AND FOR ANY GIVEN
!     BASIC SUBSTRUCTURE NAME ( NAM ) RETURNS THE ID NUMBER OF THE
!     PSEUDO-STRUCTURE CONTAINING IT ( SUBNO ) AND ITS POSITION IN
!     THE COMPONENT LIST FOR THAT STRUCTURE ( COMNO ).  IF A NAME
!     DOES NOT APPEAR IN THE SCTOC AN ERROR MESSAGE IS ISSUED.
!
!
!     OPEN SCTOC FILE
!
         ierr = 0
         IF ( .NOT.tocopn ) CALL open(*99999,sctoc,z(buf4),0)
         CALL rewind(sctoc)
!
         DO i = 1 , npsub
            CALL read(*99999,*99999,sctoc,id,3,0,nnn)
            ncom = id(3)
            DO j = 1 , ncom
               ieor = 0
               IF ( j==ncom ) ieor = 1
               CALL read(*99999,*99999,sctoc,cnam,2,ieor,nnn)
               IF ( Nam(1)==cnam(1) .AND. Nam(2)==cnam(2) ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDDO
!
!     IERR = 1 MEANS THAT THE SUBSTRUCTURE NAME IS NOT IN THE TOC
!
         ierr = 1
         RETURN
      CASE (2)
         Subno = i
         inam(1) = id(1)
         inam(2) = id(2)
         Comno = j
         IF ( .NOT.tocopn ) CALL close(sctoc,1)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99999 END SUBROUTINE finder
