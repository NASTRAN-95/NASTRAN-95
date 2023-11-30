
SUBROUTINE ktriqd(Ntype)
   IMPLICIT NONE
   REAL Dum(6) , Dummy(600) , Ecpt(100) , Rho , Save(32) , Skip(16) , Surfac , Volume
   LOGICAL Heat
   INTEGER Iecpt(4)
   COMMON /blank / Skip , Volume , Surfac
   COMMON /matout/ Dum , Rho
   COMMON /sma1dp/ Dummy
   COMMON /sma1et/ Ecpt
   COMMON /sma1ht/ Heat
   INTEGER Ntype
   INTEGER bcd(2,4) , bgpdt(4) , i , k , kount , ngpt , scr4
   REAL old
!
!
!     8/18/67         E C P T     L I S T I N G
!
! ECPT  TRMEM   QDMEM   TRPLT   QDPLT   TRIA1   QUAD1   TRIA2   QUAD2
! ***** ******* ******* ******* ******* ******* ******* ******* ********
!   1   EL.ID   EL.ID   EL.ID   EL.ID   EL.ID   EL.ID   EL.ID   EL.ID
!   2   GRID A  GRID A  GRID A  GRID A  GRID A  GRID A  GRID A  GRID A
!   3   GRID B  GRID B  GRID B  GRID B  GRID B  GRID B  GRID B  GRID B
!   4   GRID C  GRID C  GRID C  GRID C  GRID C  GRID C  GRID C  GRID C
!   5   THETA   GRID D  THETA   GRID D  THETA   GRID D  THETA   GRID D
!   6   MATID   THETA   MATID1  THETA   MATID1  THETA   MAT ID  THETA
!   7   T       MAT ID  I       MATID1  T1      MATID1  T       MAT ID
!   8   NS MASS T       MATID2  I       MATID2  T1      NS MASS T
!   9   CSID 1  NS MASS T2      MATID2  I       MATID2  CSID 1  NS MASS
!  10   X1      CSID 1  NS MASS T2      MATID3  I       X1      CSID 1
!  11   Y1      X1      Z1      NS MASS T2      MATID3  Y1      X1
!  12   Z1      Y1      Z2      Z1      NS MASS T2      Z1      Y1
!  13   CSID 2  Z1      CSID 1  Z2      Z1      NS MASS CSID 2  Z1
!  14   X2      CSID 2  X1      CSID 1  Z2      Z1      X2      CSID 2
!  15   Y2      X2      Y1      X1      CSID 1  Z2      Y2      X2
!  16   Z2      Y2      Z1      Y1      X1      CSID 1  Z2      Y2
!  17   CSID 3  Z2      CSID 2  Z1      Y1      X1      CSID 3  Z2
!  18   X3      CSID 3  X2      CSID 2  Z1      Y1      X3      CSID 3
!  19   Y3      X3      Y2      X2      CSID 2  Z1      Y3      X3
!  20   Z3      Y3      Z2      Y2      X2      CSID 2  Z3      Y3
!  21   TEMP    Z3      CSID 3  Z2      Y2      X2      TEMP    Z3
!  22           CSID 4  X3      CSID 3  Z2      Y2              CSID 4
!  23           X4      Y3      X3      CSID 3  Z2              X4
!  24           Y4      Z3      Y3      X3      CSID 3          Y4
!  25           Z4      TEMP    Z3      Y3      X3              Z4
!  26           TEMP            CSID 4  Z3      Y3              TEMP
!  27                           X4      TEMP    Z3
!  28                           Y4              CSID 4
!  29                           Z4              X4
!  30                           TEMP            Y4
!  31                                           Z4
!  32                                           TEMP
!
!
   !>>>>EQUIVALENCE (Save(1),Ecpt(50)) , (Ecpt(1),Iecpt(1))
   DATA bcd/4HCTRI , 2HA1 , 4HCTRI , 2HA2 , 4HCQUA , 2HD1 , 4HCQUA , 2HD2/
   DATA old , kount , ngpt/0.0 , 2*0/
   DATA scr4 , bgpdt/304 , 15 , 9 , 16 , 10/
!
!     THIS SUBROUTINE INCORPORATES TRIA1, QUAD1, TRIA2, QUAD2
!
!             NTYPE = 1  IMPLIES KTRIA1
!             NTYPE = 2  IMPLIES KTRIA2
!             NTYPE = 3  IMPLIES KQUAD1
!             NTYPE = 4  IMPLIES KQUAD2
!
!     CALLS FROM THIS ROUTINE CAN BE MADE TO
!
!            KTRMEM - TRIANGULAR MEMBRANE ROUTINE
!            KQDMEM - QUADRILATERAL MEMBRANE ROUTINE
!            KTQPLT - TRIANGULAR OR QUADRILATERAL PLATE ROUTINE
!            QDMM1X - HIGHER LEVEL QUADRIALATER MEMBRANE ROUTINE
!
!     ALL INSERTIONS OF 6X6 ELEMENT STIFFNESS MATRICES ARE HANDLED BY
!     THE ABOVE ROUTINES.
!
!
!     THE SAVED ECPT IS EQUIVALENCED TO ECPT(50)
!
!     SAVE THE INCOMING ECPT
!
   DO i = 1 , 32
      Save(i) = Ecpt(i)
   ENDDO
!
!     TRANSFER TO OPERATIONS DESIRED
!
!           KTRIA1 KTRIA2 KQUAD1 KQUAD2
   IF ( Ntype==2 ) THEN
!
!     *** KTRIA2 ***
!
      IF ( Save(7)/=0.0 ) THEN
!
!     SET UP ECPT FOR CALL TO KTRMEM (0)
!
!     ECPT IS OK AS DELIVERED TO THIS ROUTINE
!
         CALL ktrmem(0)
!
!     SET UP ECPT FOR CALL TO KTQPLT (3)
!
         DO i = 1 , 6
            Ecpt(i) = Save(i)
         ENDDO
         Ecpt(7) = Save(7)**3/12.0
         Ecpt(8) = Save(6)
         Ecpt(9) = Save(7)
         Ecpt(10) = Save(8)
         DO i = 13 , 25
            Ecpt(i) = Save(i-4)
         ENDDO
!
         IF ( .NOT.Heat ) CALL ktrplt
      ENDIF
   ELSEIF ( Ntype==3 ) THEN
!
!     *** KQUAD1 ***
!
      IF ( Save(8)/=0.0 ) THEN
!
!     SET UP ECPT FOR CALL TO KQDMEM
!
         Ecpt(9) = Save(13)
         DO i = 10 , 26
            Ecpt(i) = Save(i+6)
         ENDDO
!
         CALL kqdmem
      ENDIF
!
      IF ( Save(10)/=0.0 ) THEN
!
!     SET UP ECPT FOR CALL TO KTQPLT (4)
!
         DO i = 1 , 6
            Ecpt(i) = Save(i)
         ENDDO
         DO i = 7 , 30
            Ecpt(i) = Save(i+2)
         ENDDO
!
         IF ( .NOT.Heat ) CALL kqdplt
      ENDIF
   ELSEIF ( Ntype==4 ) THEN
!
!     *** KQUAD2 ***
!
      IF ( Save(8)/=0.0 ) THEN
!
!     SET UP ECPT FOR CALL TO KQDMEM
!     (WHICH HAS WEAK STIFFNESS MATRIX FORMULATION)
!     OR
!     SET UP ECPT FOR CALL TO QDMM1D/S (BETTER MEMBRANE FORMALATION)
!     THE PROBLEM HERE IS THAT KTRIQD AND KQDMEM ARE EMGOLD ELEMENTS
!     WHILE QDMM1D/S ARE EMGPRO NEW ELEMENTS.
!     TO SOLVE THIS PROPLEM, WE NEED A S.P./D.P. QDMM1X ELEMENT ROUTINE
!     THAT USES QDMM1D/S FORMULATION WITH EMGOLD/SMA1B TECHNIQUE.
!
!     ECPT IS OK AS DELIVERED TO THIS ROUTINE
!
!     CALL QDMM1X
!     (QDMM1X IS INCOMPLETE AS OF 3/92. GO BACK TO KQDMEM)
!
         CALL kqdmem
!
!     SET UP ECPT FOR CALL TO KTQPLT (4)
!
         DO i = 1 , 7
            Ecpt(i) = Save(i)
         ENDDO
         Ecpt(8) = Save(8)**3/12.0
         Ecpt(9) = Save(7)
         Ecpt(10) = Save(8)
         Ecpt(11) = Save(9)
         DO i = 14 , 30
            Ecpt(i) = Save(i-4)
         ENDDO
!
         IF ( .NOT.Heat ) CALL kqdplt
      ENDIF
   ELSE
!
!     *** KTRIA1 ***
!
!     SET UP ECPT FOR CALL TO KTRMEM (0), FIRST CHECK T1 FOR ZERO.
!
      IF ( Save(7)/=0.0 ) THEN
         DO i = 9 , 21
            Ecpt(i) = Save(i+6)
         ENDDO
!
         CALL ktrmem(0)
      ENDIF
!
!     SET UP ECPT FOR CALL TO TQPLT(3), FIRST CHECK I AND T2 EQUAL ZERO.
!
      IF ( Save(9)/=0.0 ) THEN
         DO i = 1 , 5
            Ecpt(i) = Save(i)
         ENDDO
         DO i = 6 , 25
            Ecpt(i) = Save(i+2)
         ENDDO
!
         IF ( .NOT.Heat ) CALL ktrplt
      ENDIF
   ENDIF
!
!
!     SAVE ELEMENT NAME, ID, THICKNESS, DENSITY, NO. OF GRID POINTS,
!     AND GRID PT DATA IF USER REQUESTED VOLUME AND AREA COMPUTATION
!
   IF ( .NOT.(Heat .OR. (Volume<=0.0 .AND. Surfac<=0.0)) ) THEN
      IF ( Save(1)/=old ) THEN
         old = Save(1)
         ngpt = 3
         IF ( Ntype>=3 ) ngpt = 4
         kount = 0
      ENDIF
      kount = kount + 1
      IF ( kount>=ngpt ) THEN
         Ecpt(2) = Save(7)
         Ecpt(3) = Rho
         Iecpt(4) = ngpt
         i = bgpdt(Ntype)
         k = ngpt*4
         IF ( Ntype>=3 ) Ecpt(2) = Save(8)
         CALL write(scr4,bcd(1,Ntype),2,0)
         CALL write(scr4,Ecpt(1),4,0)
         CALL write(scr4,Save(2),ngpt,0)
         CALL write(scr4,Save(i),k,1)
      ENDIF
   ENDIF
END SUBROUTINE ktriqd