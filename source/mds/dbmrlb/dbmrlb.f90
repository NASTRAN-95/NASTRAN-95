!*==dbmrlb.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE dbmrlb(Index)
   IMPLICIT NONE
   USE I_DSIOF
   USE C_SYSTEM
   USE C_ZZZZZZ
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
   IF ( Mem(Index+1)/=0 ) THEN
! MORE THAN ONE BLOCK IN THIS CHAIN TO RELEASE BACK TO FREE CHAIN
      DO WHILE ( Mem(indexl+1)/=0 )
!WKBR SPR94012 10/94      INDEXL = MEM( INDEX+1 )
         indexl = Mem(indexl+1)
      ENDDO
   ENDIF
! SET "NEXT" OF PREVIOUS BLOCK TO ZERO, IF IT EXISTS
   lindex = Mem(Index)
   IF ( lindex/=0 ) Mem(lindex+1) = 0
   IF ( idbfre/=0 ) THEN
! SET BLOCKS TO BE FREED AT FIRST OF FREE CHAIN AND
! THEN CONNECT FREE CHAIN TO THIS BLOCK
      isave = idbfre
      idbfre = Index
      Mem(isave) = indexl
      Mem(Index) = 0
      Mem(indexl+1) = isave
   ELSE
! FREE CHAIN IS EMPTY, THIS BLOCK BECOMES FREE CHAIN
      idbfre = Index
! SET "NEXT" AND "PREVIOUS" OF THIS CHAIN TO ZERO
      Mem(Index) = 0
      Mem(indexl+1) = 0
   ENDIF
END SUBROUTINE dbmrlb
