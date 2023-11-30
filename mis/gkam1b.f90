
SUBROUTINE gkam1b(Usetd,Scr1,Scr2,Phidh,Phidh1,Modes,Core,Lhset,Noue,Scr3)
   IMPLICIT NONE
   REAL A(4) , Ua , Ud , Ue , Uf , Ufe , Ug , Ul , Um , Un , Une , Uo , Up , Ur , Us , Usb , Usg
   INTEGER Ia(7) , Ia11(7) , Ia12(7) , Ia21(7) , Ia22(7) , Ii , Irule , Lc , Lcore , N1 , N2 , N3 , Sysbuf , Uset
   COMMON /bitpos/ Um , Uo , Ur , Usg , Usb , Ul , Ua , Uf , Us , Un , Ug , Ue , Up , Une , Ufe , Ud
   COMMON /parmeg/ Ia , Ia11 , Ia12 , Ia21 , Ia22 , Lcore , Irule
   COMMON /patx  / Lc , N1 , N2 , N3 , Uset
   COMMON /system/ Sysbuf
   COMMON /zblpkx/ A , Ii
   INTEGER Lhset , Modes , Noue , Phidh , Phidh1 , Scr1 , Scr2 , Scr3 , Usetd
   INTEGER Core(1)
   INTEGER i , mcb(7) , nz
   INTEGER korsz
!
!
!
! ----------------------------------------------------------------------
!
   Lhset = Lhset + Noue
!
!     BUILD  MERGE  VECTOR
!
   Uset = Usetd
   Lc = korsz(Core(Modes))
   Lcore = Lc
   CALL calcv(Scr1,Ud,Ua,Ue,Core(Modes))
!
!     BUILD  EXE  IDENTY   MATRIX
!
   nz = Lc - Sysbuf
   CALL gopen(Scr2,Core(nz+1),1)
   CALL makmcb(mcb,Scr2,Noue,6,1)
   A(1) = 1.0
   DO i = 1 , Noue
      CALL bldpk(1,1,Scr2,0,0)
      Ii = i
      CALL zblpki
      CALL bldpkn(Scr2,0,mcb)
   ENDDO
   CALL close(Scr2,1)
   CALL wrttrl(mcb)
!
!     SET  UP  FOR  MERGE
!
   Irule = 0
   Ia22(1) = Scr2
   CALL rdtrl(Ia22)
   Ia(1) = Phidh
   Ia(2) = Lhset
   Ia(3) = N1 + N2 + N3
   Ia(4) = 2
   Ia(5) = 1
   Ia21(1) = 0
   Ia12(1) = 0
   Ia11(1) = Phidh1
   CALL makmcb(Core(Modes),Scr3,Lhset,2,1)
   CALL rdtrl(Ia11)
!
!     BUILD  VECTOR IN CORE
!
   CALL gopen(Scr3,Core(nz+1),1)
   CALL bldpk(1,1,Scr3,0,0)
   Ii = Modes - 1
   DO i = 1 , Noue
      Ii = Ii + 1
      CALL zblpki
   ENDDO
   CALL bldpkn(Scr3,0,Core(Modes))
   CALL close(Scr3,1)
   CALL wrttrl(Core(Modes))
   CALL merge(Scr3,Scr1,Core(Modes))
   CALL wrttrl(Ia)
END SUBROUTINE gkam1b
