!*==ampe.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ampe(Phidh,Gtka,Gkh,Scr1,Scr2,Useta)
   IMPLICIT NONE
   USE C_BITPOS
   USE C_BLANK
   USE C_PATX
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Phidh
   INTEGER :: Gtka
   INTEGER :: Gkh
   INTEGER :: Scr1
   INTEGER :: Scr2
   INTEGER :: Useta
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: phiah
   EXTERNAL calcv , korsz , ssg2a , ssg2b
!
! End of declarations rewritten by SPAG
!
!
!     THE PURPOSE OF THIS ROUTINE IS TO COMPUTE GKH
!
!
   phiah = Phidh
!
!     DETERMINE IF PHIDH MUST BE MODIFIED
!
   IF ( Noue/=-1 ) THEN
!
!     BUILD PARTITIONING VECTORS
!
      Iuset = Useta
      Lc = korsz(Z)
      CALL calcv(Scr1,Ud,Ua,Ue,Z)
!
!     PERFORM PARTITION
!
      Nrow1 = Ns0
      Nrow2 = Ns1
      phiah = Scr2
      CALL ssg2a(Phidh,phiah,0,Scr1)
   ENDIF
!
!     COMPUTE GKH
!
   CALL ssg2b(Gtka,phiah,0,Gkh,1,Iprec,1,Scr1)
END SUBROUTINE ampe
