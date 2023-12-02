!*==xipfl.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE xipfl
   IMPLICIT NONE
   USE C_PASSER
   USE C_XGPI2
   USE C_XGPI4
   USE C_XGPIC
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , iofl , ityp , j , k , k1 , k2 , k3 , l , ospnt
   INTEGER , DIMENSION(1) :: oscar
!
! End of declarations rewritten by SPAG
!
!
!     THE PURPOSE OF XIOFL IS TO GENERATE THE INPUT AND OUTPUT FILE
!     SECTIONS FOR AN OSCAR ENTRY.
!
!                  ** CONTROL CARD NAMES **
!                  ** DMAP CARD NAMES **
   !>>>>EQUIVALENCE (Core(1),Os(1),Loscar) , (Os(2),Osprc) , (Os(3),Osbot) , (Os(4),Ospnt) , (Os(5),Oscar(1))
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
   oscar(ospnt+6) = 1
   oscar(ospnt+7) = 0
   oscar(ospnt+8) = 0
   oscar(ospnt+9) = 0
   oscar(ospnt) = oscar(ospnt) + 4
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
      oscar(ospnt+2) = orf(2,andf(Masklo,oscar(ospnt+2)))
      GOTO 200
   ENDIF
!
!
!     SCAN INPUT OR OUTPUT SECTION
!
 100  i = ospnt + oscar(ospnt)
   Istopf = i
   oscar(i) = j
   oscar(ospnt) = 1 + oscar(ospnt)
   i = i + 1
   j = i + 3*(j-1)
   oscar(ospnt) = j + 3 - ospnt
!
!     ZERO I/O SECTION
!
   l = j + 2
   DO k = i , l
      oscar(k) = 0
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
         oscar(k) = Dmap(Dmppnt)
         oscar(k+1) = Dmap(Dmppnt+1)
         oscar(k+2) = 0
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
         IF ( iofl==1 ) CALL xgpidg(62,ospnt,0,0)
         IF ( iofl==0 ) CALL xgpidg(63,ospnt,0,0)
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
      ityp = andf(oscar(ospnt+2),7)
      IF ( ityp/=2 ) THEN
!
!     FIRST INPUT FILE WAS NULL - SHIFT I/P SECTION BY ONE ENTRY AND
!     ZERO FIRST ENTRY
!     ISSUE WARNING MESSAGE
!
         CALL xgpidg(-1,ospnt,0,0)
         IF ( i/=j ) THEN
            i = i + 3
            j = j + 2
            DO k = i , j
               l = j - k + i
               oscar(l) = oscar(l-3)
            ENDDO
            oscar(i-3) = 0
            oscar(i-2) = 0
            oscar(i-1) = 0
         ENDIF
      ENDIF
      Irturn = 1
   ENDIF
   GOTO 400
END SUBROUTINE xipfl
