
SUBROUTINE triqd(Ntype,T)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Core(1) , Ecpt(100) , Save(32)
   COMMON /trimex/ Ecpt
   COMMON /zzzzzz/ Core
!
! Dummy argument declarations
!
   INTEGER Ntype
   REAL T(1)
!
! Local variable declarations
!
   INTEGER i
!
! End of declarations
!
!*****
!  ELEMENT THERMAL AND DEFORMATION LOADING ROUTINE FOR FOUR ELEMENTS
!*****
!
!                     E C P T     L I S T I N G
!                    ***************************
! ECPT  TRMEM   QDMEM   TRPLT   QDPLT   TRIA1   QUAD1   TRIA2   QUAD2
! **********************************************************************
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
! **********************************************************************
!
   EQUIVALENCE (Save(1),Ecpt(50))
!
!     THIS SUBROUTINE INCORPORATES TRIA1, QUAD1, TRIA2, QUAD2
!
!              NTYPE = 1  IMPLIES  TRIA1
!              NTYPE = 2  IMPLIES  TRIA2
!              NTYPE = 3  IMPLIES  QUAD1
!              NTYPE = 4  IMPLIES  QUAD2
!
!     CALLS FROM THIS ROUTINE ARE MADE TO THE FOLLOWING ELEMENT
!     THERMAL AND DEFORMATION LOADING ROUTINES.
!
!               TRMEM - TRIANGULAR MEMBRANE ROUTINE
!               QDMEM - QUADRILATERAL MEMBRANE ROUTINE
!               TRPLT - TRIANGULAR PLATE ROUTINE.
!               QDPLT - QUADRILATERAL PLATE ROUTINE.
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
!     TRANSFER TO ELEMENT TYPE DESIRED
!
   IF ( Ntype==2 ) THEN
!*****
!     ***  TRIA2 ***
!*****
      IF ( Save(7)==0.0E0 ) RETURN
!
!     SET UP ECPT FOR CALL TO  TRMEM(0)
!
!     ECPT IS OK AS DELIVERED TO THIS ROUTINE
!
      CALL trimem(0,T(1),Core(1))
!
!     SET UP ECPT FOR CALL TO TRPLT
!
      DO i = 1 , 6
         Ecpt(i) = Save(i)
      ENDDO
      Ecpt(7) = Save(7)**3/12.0E0
      Ecpt(8) = Save(6)
      Ecpt(9) = Save(7)
      Ecpt(10) = Save(8)
      DO i = 13 , 25
         Ecpt(i) = Save(i-4)
      ENDDO
!
      CALL trplt(T(1))
      RETURN
   ELSEIF ( Ntype==3 ) THEN
!*****
!     ***  QUAD1 ***
!*****
      IF ( Save(8)/=0.0E0 ) THEN
!
!     SET UP ECPT FOR CALL TO  QDMEM
!
         Ecpt(9) = Save(13)
         DO i = 10 , 26
            Ecpt(i) = Save(i+6)
         ENDDO
!
         CALL qdmem(T(1),Core(1))
      ENDIF
   ELSEIF ( Ntype==4 ) THEN
!*****
!     ***  QUAD2 ***
!*****
      IF ( Save(8)==0.0E0 ) RETURN
!
!     SET UP ECPT FOR CALL TO  QDMEM
!
!     ECPT IS OK AS DELIVERED TO THIS ROUTINE
!
      CALL qdmem(T(1),Core(1))
!
!     SET UP ECPT FOR CALL TO QDPLT
!
      DO i = 1 , 7
         Ecpt(i) = Save(i)
      ENDDO
      Ecpt(8) = Save(8)**3/12.0E0
      Ecpt(9) = Save(7)
      Ecpt(10) = Save(8)
      Ecpt(11) = Save(9)
      DO i = 14 , 30
         Ecpt(i) = Save(i-4)
      ENDDO
!
      CALL qdplt(T(1))
      GOTO 99999
   ELSE
!*****
!     ***  TRIA1 ***
!*****
!     SET UP ECPT FOR CALL TO  TRMEM(0), FIRST CHECK T1 FOR ZERO.
!
      IF ( Save(7)/=0.0E0 ) THEN
         DO i = 9 , 21
            Ecpt(i) = Save(i+6)
         ENDDO
!
         CALL trimem(0,T(1),Core(1))
      ENDIF
!
!     SET UP ECPT FOR CALL TO TRPLT, FIRST CHECK I AND T2 EQUAL ZERO.
!
      IF ( Save(9)==0.0E0 ) RETURN
      DO i = 1 , 5
         Ecpt(i) = Save(i)
      ENDDO
      DO i = 6 , 25
         Ecpt(i) = Save(i+2)
      ENDDO
!
      CALL trplt(T(1))
      RETURN
   ENDIF
!
   IF ( Save(10)==0.0E0 ) RETURN
!
!     SET UP ECPT FOR CALL TO QDPLT
!
   DO i = 1 , 6
      Ecpt(i) = Save(i)
   ENDDO
   DO i = 7 , 30
      Ecpt(i) = Save(i+2)
   ENDDO
!
   CALL qdplt(T(1))
   RETURN
!
99999 END SUBROUTINE triqd
