
SUBROUTINE ampb(Phidh,Gtka,D1jk,D2jk,D1je,D2je,Useta,Djh1,Djh2,Gki,Scr1,Scr2,Scr3)
   IMPLICIT NONE
   INTEGER Iprec , Iuset , Lc , Ncolj , Noue , Nout , Ns0 , Ns1 , Ns2 , Ua , Ud , Ue
   REAL Skip(52) , Sysbuf , Uf , Ufe , Ug , Uk , Ul , Um , Un , Une , Uo , Up , Upa , Ups , Ur , Us , Usa , Usb , Usg , Z(1)
   COMMON /ampcom/ Ncolj
   COMMON /bitpos/ Um , Uo , Ur , Usg , Usb , Ul , Ua , Uf , Us , Un , Ug , Ue , Up , Une , Ufe , Ud , Ups , Usa , Uk , Upa
   COMMON /blank / Noue
   COMMON /patx  / Lc , Ns0 , Ns1 , Ns2 , Iuset
   COMMON /system/ Sysbuf , Nout , Skip , Iprec
   COMMON /zzzzzz/ Z
   INTEGER D1je , D1jk , D2je , D2jk , Djh1 , Djh2 , Gki , Gtka , Phidh , Scr1 , Scr2 , Scr3 , Useta
   INTEGER dji1 , dji2 , mcb(7) , noh , phia
   INTEGER korsz
!
!     THE PURPOSE OF THIS SUBROUTINE IS TO SOLVE FOR THE DJH MATRICES.
!      IT ALSO COMPUTES GKI FOR LATER USE.
!      THE STEPS ARE,
!
!     1. PHIDH GOES TO   1       1      1
!                        1 PHIA  1      1
!                        1 ----- 1 ---- 1
!                        1       1      1
!                        1       1      1
!
!     2. GKI =GTKA$PHIA
!
!     3. DJI1=D1JK*GKI
!     4. DJI2=D2JK*GKI
!     5.
!     6. DJH1= 1 DJI1 1 D1JE 1
!              1      1      1
!     7. DJH2= 1 DJI2 1 D2JE 1
!
!
!
!
!
!-----------------------------------------------------------------------
!
   mcb(1) = Phidh
   CALL rdtrl(mcb)
   noh = mcb(2)
!
!     DETERMINE IF PHIDH MUST BE MODIFIED
!
   IF ( Noue==-1 ) THEN
!
!     NO MOD REQUIRED
!
      phia = Phidh
   ELSE
!
!     BUILD PARTITIONING VECTORS
!
      Iuset = Useta
      Lc = korsz(Z)
      CALL calcv(Scr1,Ud,Ua,Ue,Z)
      CALL ampb1(Scr2,noh-Noue,Noue)
!
!     PERFORM PARTITION
!                       RP   CP
      CALL ampb2(Phidh,Scr3,0,0,0,Scr2,Scr1,0,0)
      phia = Scr3
   ENDIF
!
!     COMPUTE GKI
!
   CALL ssg2b(Gtka,phia,0,Gki,1,Iprec,1,Scr1)
!
!     START COMPUTATION OF DJH MATRICES
!
   dji1 = Scr3
   dji2 = Scr3
   IF ( Noue<=0 ) THEN
      dji1 = Djh1
      dji2 = Djh2
   ENDIF
   CALL ssg2b(D1jk,Gki,0,dji1,1,Iprec,1,Scr1)
   IF ( Noue/=-1 ) CALL merged(dji1,D1je,0,0,Djh1,Scr2,0,0,Ncolj)
   CALL ssg2b(D2jk,Gki,0,dji2,1,Iprec,1,Scr1)
   IF ( Noue/=-1 ) CALL merged(dji2,D2je,0,0,Djh2,Scr2,0,0,Ncolj)
END SUBROUTINE ampb