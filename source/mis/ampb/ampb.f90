!*==ampb.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ampb(Phidh,Gtka,D1jk,D2jk,D1je,D2je,Useta,Djh1,Djh2,Gki,Scr1,Scr2,Scr3)
   USE c_ampcom
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
   INTEGER :: D1jk
   INTEGER :: D2jk
   INTEGER :: D1je
   INTEGER :: D2je
   INTEGER :: Useta
   INTEGER :: Djh1
   INTEGER :: Djh2
   INTEGER :: Gki
   INTEGER :: Scr1
   INTEGER :: Scr2
   INTEGER :: Scr3
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: dji1 , dji2 , noh , phia
   INTEGER , DIMENSION(7) :: mcb
   EXTERNAL ampb1 , ampb2 , calcv , korsz , merged , rdtrl , ssg2b
!
! End of declarations rewritten by SPAG
!
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
   IF ( noue==-1 ) THEN
!
!     NO MOD REQUIRED
!
      phia = Phidh
   ELSE
!
!     BUILD PARTITIONING VECTORS
!
      iuset = Useta
      lc = korsz(z)
      CALL calcv(Scr1,ud,ua,ue,z)
      CALL ampb1(Scr2,noh-noue,noue)
!
!     PERFORM PARTITION
!                       RP   CP
      CALL ampb2(Phidh,Scr3,0,0,0,Scr2,Scr1,0,0)
      phia = Scr3
   ENDIF
!
!     COMPUTE GKI
!
   CALL ssg2b(Gtka,phia,0,Gki,1,iprec,1,Scr1)
!
!     START COMPUTATION OF DJH MATRICES
!
   dji1 = Scr3
   dji2 = Scr3
   IF ( noue<=0 ) THEN
      dji1 = Djh1
      dji2 = Djh2
   ENDIF
   CALL ssg2b(D1jk,Gki,0,dji1,1,iprec,1,Scr1)
   IF ( noue/=-1 ) CALL merged(dji1,D1je,0,0,Djh1,Scr2,0,0,ncolj)
   CALL ssg2b(D2jk,Gki,0,dji2,1,iprec,1,Scr1)
   IF ( noue/=-1 ) CALL merged(dji2,D2je,0,0,Djh2,Scr2,0,0,ncolj)
END SUBROUTINE ampb
