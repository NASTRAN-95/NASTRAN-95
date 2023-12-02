!*==cifsdd.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE cifsdd
   IMPLICIT NONE
   USE C_CIFS1P
   USE C_CIFS2P
   USE C_CIFS3P
   USE C_CIFS4P
   USE C_CIFS5P
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i
   INTEGER , SAVE :: icc , ipp
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
   B1 = 1
   Bardf2 = 0
   Bardf5 = 0
   Bardf6 = 0
   Bardf7 = 0
   Bardf8 = 0
   Km1 = 0
   Slot = .FALSE.
   Idrdl = 0
!
   Fphys = .TRUE.
   Fphys1 = .TRUE.
   Km2 = 0
   Dmiflg = .FALSE.
   Ibcds = 0
   Fthru = .FALSE.
   Fphys2 = .TRUE.
!
   Grdmsg = .FALSE.
   La1 = 0
   L7 = 0
   Km3 = 0
   L0 = 1
   G1 = 1
   Lh = .TRUE.
   Igdst2 = 0
   Igdst6 = 0
   Igdst7 = 0
   Igdst8 = 0
   Iddsf = 0
   Idfreq = .TRUE.
   Idrad = 0
   Nvar = 0
   Ids = 0
   Jms = 0
   Kms = 0
   Lplf = 0
!
   DO i = 3 , 20
      J(i) = 0
   ENDDO
   J(1) = 20
   J(2) = 2
   Km4 = 0
   Lflsym = .FALSE.
   Ffphys = .TRUE.
!
   Km5 = 0
   Ic = icc
   Ip = ipp
   Icont = 0
   Iaero = 0
   Ipopt = 0
!
END SUBROUTINE cifsdd
