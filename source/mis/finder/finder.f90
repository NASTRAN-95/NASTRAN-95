!*==finder.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE finder(Nam,Subno,Comno)
   IMPLICIT NONE
   USE C_CMB001
   USE C_CMB002
   USE C_CMB003
   USE C_CMBFND
   USE C_ZZZZZZ
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
         Ierr = 0
         IF ( .NOT.Tocopn ) CALL open(*99999,Sctoc,Z(Buf4),0)
         CALL rewind(Sctoc)
!
         DO i = 1 , Npsub
            CALL read(*99999,*99999,Sctoc,id,3,0,nnn)
            ncom = id(3)
            DO j = 1 , ncom
               ieor = 0
               IF ( j==ncom ) ieor = 1
               CALL read(*99999,*99999,Sctoc,cnam,2,ieor,nnn)
               IF ( Nam(1)==cnam(1) .AND. Nam(2)==cnam(2) ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDDO
!
!     IERR = 1 MEANS THAT THE SUBSTRUCTURE NAME IS NOT IN THE TOC
!
         Ierr = 1
         RETURN
      CASE (2)
         Subno = i
         Inam(1) = id(1)
         Inam(2) = id(2)
         Comno = j
         IF ( .NOT.Tocopn ) CALL close(Sctoc,1)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99999 END SUBROUTINE finder
