!*==ampe.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ampe(Phidh,Gtka,Gkh,Scr1,Scr2,Useta)
   USE c_bitpos
   USE c_blank
   USE c_patx
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
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
   IF ( noue/=-1 ) THEN
!
!     BUILD PARTITIONING VECTORS
!
      iuset = Useta
      lc = korsz(z)
      CALL calcv(Scr1,ud,ua,ue,z)
!
!     PERFORM PARTITION
!
      nrow1 = ns0
      nrow2 = ns1
      phiah = Scr2
      CALL ssg2a(Phidh,phiah,0,Scr1)
   ENDIF
!
!     COMPUTE GKH
!
   CALL ssg2b(Gtka,phiah,0,Gkh,1,iprec,1,Scr1)
END SUBROUTINE ampe
