!*==magbdy.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE magbdy
   USE c_blank
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: buf1 , file , i , idx , ieqex , jloc , lcore , n , neq , ngrids , npts
   INTEGER , SAVE :: eqexin , geom1 , permbd
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: nam , permby
   EXTERNAL bisloc , close , gopen , korsz , locate , mesage , preloc , read , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE PICKS UP THE GRIDS ON THE AIR/IRON INTERFACES
!     FROM A PERMBDY CARD,CONVERTS EXTERNAL TO INTERNAL SILS, AND
!     STORES RESULTS ON PERMBD WHICH IS READ IN SSG1. SSG1 WILL NEED TO
!     COMPUTE MAGNETIC LOADS ONLY AT THESE POINTS.
!
!     MAGBDY   GEOM1,HEQEXIN/PERMBD/V,N,IPG $
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
   DATA nam/4HMAGB , 4HDY  /
   DATA geom1 , eqexin , permbd/101 , 102 , 201/
   DATA permby/4201 , 42/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         lcore = korsz(z)
         buf1 = lcore - sysbuf
         lcore = buf1 - 1
         IF ( lcore>0 ) THEN
!
!     SEE IF A PERMBDY CARD EXISTS
!
            ipg = -1
            file = geom1
            CALL preloc(*100,z(buf1),geom1)
            CALL locate(*20,z(buf1),permby,idx)
            ipg = 1
!
!     READ PERMBDY INTO CORE
!
            CALL read(*120,*40,geom1,z,lcore,0,npts)
         ENDIF
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     NO PERMBDY CARD - RETURN
!
 20      CALL close(geom1,1)
         RETURN
 40      CALL close(geom1,1)
!
!     READ IN 1ST RECORD OF EQEXIN
!
         lcore = lcore - npts
         ieqex = npts
         CALL gopen(eqexin,z(buf1),0)
         file = eqexin
         CALL read(*120,*60,eqexin,z(ieqex+1),lcore,0,neq)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 60      CALL close(eqexin,1)
         ngrids = neq/2
         lcore = lcore - neq
!
!     GET THE INTERNAL NUMBER (=SIL NUMBER FOR HEAT TRAMSFER)FOR EACH
!     POINT ON PERMBDY AND STORE IT BACK ONTO EXTERNAL NUMBER,SINCE THE
!     EXTERNAL IS NO LONGER NEEDED
!
         DO i = 1 , npts
            CALL bisloc(*80,iz(i),iz(ieqex+1),2,ngrids,jloc)
            iz(i) = iz(ieqex+jloc+1)
         ENDDO
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
 80      WRITE (iout,99001) ufm , iz(i)
99001    FORMAT (A23,', GRID',I9,' ON PERMBDY CARD DOES NOT EXIST')
         CALL mesage(-61,0,0)
         spag_nextblock_1 = 2
      CASE (2)
!
!     WRITE THESE INTERNAL ID-S ONTO PERMBD
!
         CALL gopen(permbd,z(buf1),1)
         CALL write(permbd,iz(1),npts,1)
         CALL close(permbd,1)
         mcb(1) = permbd
         mcb(2) = npts
         DO i = 3 , 7
            mcb(i) = 0
         ENDDO
         CALL wrttrl(mcb)
!
         RETURN
!
 100     n = -1
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 120     n = -2
         spag_nextblock_1 = 4
      CASE (3)
         n = -8
         file = 0
         spag_nextblock_1 = 4
      CASE (4)
         CALL mesage(n,file,nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE magbdy
