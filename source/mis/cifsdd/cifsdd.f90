!*==cifsdd.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE cifsdd
   IMPLICIT NONE
   USE c_cifs1p
   USE c_cifs2p
   USE c_cifs3p
   USE c_cifs4p
   USE c_cifs5p
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i
   INTEGER , SAVE :: icc , ipp
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     THIS SUBROUTINE INITIALIZES THE CIFS1P, CIFS2P, CIFS3P,
!     CIFS4P, AND CIFS5P COMMON BLOCKS.
!
!
!
!
   DATA icc/1HC/ , ipp/1HP/
!
   b1 = 1
   bardf2 = 0
   bardf5 = 0
   bardf6 = 0
   bardf7 = 0
   bardf8 = 0
   km1 = 0
   slot = .FALSE.
   idrdl = 0
!
   fphys = .TRUE.
   fphys1 = .TRUE.
   km2 = 0
   dmiflg = .FALSE.
   ibcds = 0
   fthru = .FALSE.
   fphys2 = .TRUE.
!
   grdmsg = .FALSE.
   la1 = 0
   l7 = 0
   km3 = 0
   l0 = 1
   g1 = 1
   lh = .TRUE.
   igdst2 = 0
   igdst6 = 0
   igdst7 = 0
   igdst8 = 0
   iddsf = 0
   idfreq = .TRUE.
   idrad = 0
   nvar = 0
   ids = 0
   jms = 0
   kms = 0
   lplf = 0
!
   DO i = 3 , 20
      j(i) = 0
   ENDDO
   j(1) = 20
   j(2) = 2
   km4 = 0
   lflsym = .FALSE.
   ffphys = .TRUE.
!
   km5 = 0
   ic = icc
   ip = ipp
   icont = 0
   iaero = 0
   ipopt = 0
!
END SUBROUTINE cifsdd
