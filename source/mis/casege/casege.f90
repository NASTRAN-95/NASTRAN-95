!*==casege.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE casege
   IMPLICIT NONE
   USE C_BLANK
   USE C_SYSTEM
   USE C_ZZZZZZ
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
         lcore = korsz(Z)
         buf1 = lcore - Sysbuf + 1
         buf2 = buf1 - Sysbuf
         lcore = buf2 - 1
         IF ( lcore>0 ) THEN
!
            CALL gopen(casecc,Z(buf1),0)
            CALL gopen(casedd,Z(buf2),1)
            CALL read(*40,*20,casecc,Z,lcore,0,iwords)
         ENDIF
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 20      IF ( Lmodes>Nmodes ) Lmodes = Nmodes
         itot = Lmodes*Ndir
         DO i = 1 , itot
            iz(1) = i
            CALL write(casedd,Z,iwords,1)
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
