!*==casege.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE casege
   USE c_blank
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: buf1 , buf2 , i , itot , iwords , lcore
   INTEGER , SAVE :: casecc , casedd
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: nam
   EXTERNAL close , gopen , korsz , mesage , rdtrl , read , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
! GENERATES IDENTICAL SUBCASES LMODES*NDIR TIMES FOR DDAM
!
!     CASEGEN  CASECC/CASEDD/C,Y,LMODES/V,N,NDIR/V,N,NMODES $
!    EQUIV CASEDD,CASECC  $
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
   DATA casecc , casedd/101 , 201/
   DATA nam/4HCASE , 4HGE  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         lcore = korsz(z)
         buf1 = lcore - sysbuf + 1
         buf2 = buf1 - sysbuf
         lcore = buf2 - 1
         IF ( lcore>0 ) THEN
!
            CALL gopen(casecc,z(buf1),0)
            CALL gopen(casedd,z(buf2),1)
            CALL read(*40,*20,casecc,z,lcore,0,iwords)
         ENDIF
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 20      IF ( lmodes>nmodes ) lmodes = nmodes
         itot = lmodes*ndir
         DO i = 1 , itot
            iz(1) = i
            CALL write(casedd,z,iwords,1)
         ENDDO
         CALL close(casecc,1)
         CALL close(casedd,1)
         mcb(1) = casecc
         CALL rdtrl(mcb)
         mcb(1) = casedd
         mcb(2) = itot
         CALL wrttrl(mcb)
         RETURN
!
 40      CALL mesage(-2,casecc,nam)
         spag_nextblock_1 = 2
      CASE (2)
         CALL mesage(-8,0,nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE casege
