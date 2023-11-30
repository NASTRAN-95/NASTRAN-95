
SUBROUTINE cifsdd
   IMPLICIT NONE
   INTEGER B1 , Bardf2 , Bardf5 , Bardf6 , Bardf7 , Bardf8 , G1 , Iaero , Ibcds , Ic , Icont , Iddsf , Idrad , Idrdl , Ids ,        &
         & Igdst2 , Igdst6 , Igdst7 , Igdst8 , Ip , Ipopt , J(20) , Jms , Km1 , Km2 , Km3 , Km4 , Km5 , Kms , L0 , L7 , La1 , Lplf ,&
         & Nvar
   LOGICAL Dmiflg , Ffphys , Fphys , Fphys1 , Fphys2 , Fthru , Grdmsg , Idfreq , Lflsym , Lh , Slot
   COMMON /cifs1p/ B1 , Bardf2 , Bardf5 , Bardf6 , Bardf7 , Bardf8 , Km1 , Slot , Idrdl
   COMMON /cifs2p/ Fphys , Fphys1 , Km2 , Dmiflg , Ibcds , Fthru , Fphys2
   COMMON /cifs3p/ Grdmsg , La1 , L7 , Km3 , L0 , G1 , Lh , Igdst2 , Igdst6 , Igdst7 , Igdst8 , Iddsf , Idfreq , Idrad , Nvar ,     &
                 & Ids , Jms , Kms , Lplf
   COMMON /cifs4p/ J , Km4 , Lflsym , Ffphys
   COMMON /cifs5p/ Km5 , Ic , Ip , Icont , Iaero , Ipopt
   INTEGER i , icc , ipp
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