
SUBROUTINE ampe(Phidh,Gtka,Gkh,Scr1,Scr2,Useta)
   IMPLICIT NONE
   INTEGER Iprec , Iuset , Lc , Noue , Nout , Nrow1 , Nrow2 , Ns0 , Ns1 , Ns2
   REAL Skip(52) , Sysbuf , Ua , Ud , Ue , Uf , Ufe , Ug , Uk , Ul , Um , Un , Une , Uo , Up , Upa , Ups , Ur , Us , Usa , Usb ,    &
      & Usg , Xxx , Z(1)
   COMMON /bitpos/ Um , Uo , Ur , Usg , Usb , Ul , Ua , Uf , Us , Un , Ug , Ue , Up , Une , Ufe , Ud , Ups , Usa , Uk , Upa
   COMMON /blank / Noue
   COMMON /patx  / Lc , Ns0 , Ns1 , Ns2 , Iuset , Xxx , Nrow1 , Nrow2
   COMMON /system/ Sysbuf , Nout , Skip , Iprec
   COMMON /zzzzzz/ Z
   INTEGER Gkh , Gtka , Phidh , Scr1 , Scr2 , Useta
   INTEGER korsz
   INTEGER phiah
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
