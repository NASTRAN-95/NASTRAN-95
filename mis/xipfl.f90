
SUBROUTINE xipfl
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Bcdcnt , Dmap(1) , Dmpcnt , Dmppnt , Iallon , Ichar , Icold , Icomon , Icrdtp , Idmpnt , Iequl , Insert , Irturn ,       &
         & Isavdw , Iseqn , Isgnon , Islsh , Istopf , Ldmap , Length , Lmpl , Loscar , Maskhi , Masklo , Masks(1) , Modidx ,        &
         & Modnam , Mpl(1) , Mplpnt , Nbegin , Nblank , Nbpc , Nchkpt , Ncond , Ncpw , Ndiag , Ndmap , Nend , Nequiv , Nestm1 ,     &
         & Nestm2 , Newcrd , Nexit , Njump , Nosgn , Noutpt , Npurge , Nrept , Nsave , Nsol , Ntime , Nwpc , Nxequi , Os(5) ,       &
         & Osbot , Oscar(1) , Ospnt , Osprc
   REAL Core(1)
   COMMON /passer/ Istopf , Modnam , Icomon
   COMMON /xgpi2 / Lmpl , Mplpnt , Mpl
   COMMON /xgpi4 / Irturn , Insert , Iseqn , Dmpcnt , Idmpnt , Dmppnt , Bcdcnt , Length , Icrdtp , Ichar , Newcrd , Modidx , Ldmap ,&
                 & Isavdw , Dmap
   COMMON /xgpic / Icold , Islsh , Iequl , Nblank , Nxequi , Ndiag , Nsol , Ndmap , Nestm1 , Nestm2 , Nexit , Nbegin , Nend ,       &
                 & Njump , Ncond , Nrept , Ntime , Nsave , Noutpt , Nchkpt , Npurge , Nequiv , Ncpw , Nbpc , Nwpc , Maskhi ,        &
                 & Masklo , Isgnon , Nosgn , Iallon , Masks
   COMMON /zzzzzz/ Core
!
! Local variable declarations
!
   INTEGER andf , orf
   INTEGER i , iofl , ityp , j , k , k1 , k2 , k3 , l
   EXTERNAL andf , orf
!
! End of declarations
!
!
!     THE PURPOSE OF XIOFL IS TO GENERATE THE INPUT AND OUTPUT FILE
!     SECTIONS FOR AN OSCAR ENTRY.
!
!                  ** CONTROL CARD NAMES **
!                  ** DMAP CARD NAMES **
   EQUIVALENCE (Core(1),Os(1),Loscar) , (Os(2),Osprc) , (Os(3),Osbot) , (Os(4),Ospnt) , (Os(5),Oscar(1))
!
!
!     SET INPUT FILE FLAG
!
   iofl = 1
   k3 = 0
   Istopf = 0
   j = Mpl(Mplpnt)
   Mplpnt = Mplpnt + 1
   IF ( j/=0 ) GOTO 100
!
!     NO INPUT FILES - MAKE ONE NULL ENTRY IN OSCAR
!
   Oscar(Ospnt+6) = 1
   Oscar(Ospnt+7) = 0
   Oscar(Ospnt+8) = 0
   Oscar(Ospnt+9) = 0
   Oscar(Ospnt) = Oscar(Ospnt) + 4
   GOTO 200
!
!
   ENTRY xopfl
!     ===========
!
!     SET O/P FLAG
!
   iofl = 0
   k3 = 0
   Istopf = 0
   j = Mpl(Mplpnt)
   Mplpnt = Mplpnt + 1
   IF ( j==0 ) THEN
!
!     THERE ARE NO O/P FILES - CHANGE OSCAR ENTRY TYPE CODE TO O FORMAT
!
      Oscar(Ospnt+2) = orf(2,andf(Masklo,Oscar(Ospnt+2)))
      GOTO 200
   ENDIF
!
!
!     SCAN INPUT OR OUTPUT SECTION
!
 100  i = Ospnt + Oscar(Ospnt)
   Istopf = i
   Oscar(i) = j
   Oscar(Ospnt) = 1 + Oscar(Ospnt)
   i = i + 1
   j = i + 3*(j-1)
   Oscar(Ospnt) = j + 3 - Ospnt
!
!     ZERO I/O SECTION
!
   l = j + 2
   DO k = i , l
      Oscar(k) = 0
   ENDDO
!
!     ENTER FILE NAME IN OSCAR FROM DMAP
!
   DO k = i , j , 3
      CALL xscndm
      IF ( Irturn==1 .OR. Irturn==4 ) GOTO 500
      IF ( Irturn==3 .OR. Irturn==5 ) GOTO 300
!
!     OK IF NAME RETURNED FROM XSCNDM
!
      IF ( Dmap(Dmppnt)/=Nblank ) THEN
!
!     ENTER NAME IN OSCAR AND INITIALIZE ORDNAL
!
         Oscar(k) = Dmap(Dmppnt)
         Oscar(k+1) = Dmap(Dmppnt+1)
         Oscar(k+2) = 0
      ENDIF
   ENDDO
 200  CALL xscndm
   IF ( Irturn==2 .OR. Irturn==3 .OR. Irturn==5 ) THEN
   ELSEIF ( Irturn==4 ) THEN
      Irturn = 1
      GOTO 400
   ELSEIF ( Dmap(Dmppnt+1)==Islsh ) THEN
      Irturn = 1
      GOTO 400
   ENDIF
!
!     NORMAL EXIT IF DMAP OPERATOR IS /
!
!     ERROR EXIT
!     BLANK ITEM IN O/P SECTION OF TYPE O FORMAT IS OKAY
!
 300  IF ( j==0 .AND. iofl==0 .AND. Dmap(Dmppnt)==Nblank ) GOTO 200
   k1 = 1 + (k-i)/3
   k2 = 1 + (j-i)/3
   IF ( k1<=k2 ) THEN
      Irturn = 2
   ELSE
      IF ( k3/=1 ) THEN
         IF ( iofl==1 ) CALL xgpidg(62,Ospnt,0,0)
         IF ( iofl==0 ) CALL xgpidg(63,Ospnt,0,0)
         k3 = 1
      ENDIF
      GOTO 200
   ENDIF
 400  RETURN
!
!
!     DELIMITER OR END OF INSTRUCTION ENCOUNTERED BEFORE ANTICIPATED -
!     CHECK FOR ILLEGAL INPUT FORMAT
!
 500  IF ( iofl/=1 .OR. Dmap(Dmppnt+1)/=Islsh ) GOTO 300
   IF ( Icomon==0 ) THEN
      Irturn = 2
   ELSE
      ityp = andf(Oscar(Ospnt+2),7)
      IF ( ityp/=2 ) THEN
!
!     FIRST INPUT FILE WAS NULL - SHIFT I/P SECTION BY ONE ENTRY AND
!     ZERO FIRST ENTRY
!     ISSUE WARNING MESSAGE
!
         CALL xgpidg(-1,Ospnt,0,0)
         IF ( i/=j ) THEN
            i = i + 3
            j = j + 2
            DO k = i , j
               l = j - k + i
               Oscar(l) = Oscar(l-3)
            ENDDO
            Oscar(i-3) = 0
            Oscar(i-2) = 0
            Oscar(i-1) = 0
         ENDIF
      ENDIF
      Irturn = 1
   ENDIF
   GOTO 400
END SUBROUTINE xipfl
