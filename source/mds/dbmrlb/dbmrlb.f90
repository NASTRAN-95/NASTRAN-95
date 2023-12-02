!*==dbmrlb.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dbmrlb(Index)
   USE i_dsiof
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Index
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: indexl , isave , lindex
!
! End of declarations rewritten by SPAG
!
!********************************************************************
!  DBMRLB  -   RELEASES AN IN-MEMORY BLOCK THAT IS CURRENTLY
!              ALLOCATED AS THE LAST BLOCK OF AN IN-MEMORY FILE.
!              THIS IS USED TO RELEASE THE NEXT ALLOCATED BLOCK FOR A
!              FILE OPENED FOR WRITE BUT WAS NEVER USED BECAUSE THE
!              FILE WAS CLOSED--I.E., THE LAST BLOCK ALLOCATED FOR A
!              FILE OPENED FOR WRITE IS NEVER USED BUT IT MUST HAVE
!              BEEN ALLOCATED JUST IN CASE THE FILE IS NOT TO BE CLOSED.
!********************************************************************
   indexl = Index
! CHECK IF OTHER BLOCKS ARE CHAINED TO THE END OF THIS BLOCK
   IF ( mem(Index+1)/=0 ) THEN
! MORE THAN ONE BLOCK IN THIS CHAIN TO RELEASE BACK TO FREE CHAIN
      DO WHILE ( mem(indexl+1)/=0 )
!WKBR SPR94012 10/94      INDEXL = MEM( INDEX+1 )
         indexl = mem(indexl+1)
      ENDDO
   ENDIF
! SET "NEXT" OF PREVIOUS BLOCK TO ZERO, IF IT EXISTS
   lindex = mem(Index)
   IF ( lindex/=0 ) mem(lindex+1) = 0
   IF ( idbfre/=0 ) THEN
! SET BLOCKS TO BE FREED AT FIRST OF FREE CHAIN AND
! THEN CONNECT FREE CHAIN TO THIS BLOCK
      isave = idbfre
      idbfre = Index
      mem(isave) = indexl
      mem(Index) = 0
      mem(indexl+1) = isave
   ELSE
! FREE CHAIN IS EMPTY, THIS BLOCK BECOMES FREE CHAIN
      idbfre = Index
! SET "NEXT" AND "PREVIOUS" OF THIS CHAIN TO ZERO
      mem(Index) = 0
      mem(indexl+1) = 0
   ENDIF
END SUBROUTINE dbmrlb
