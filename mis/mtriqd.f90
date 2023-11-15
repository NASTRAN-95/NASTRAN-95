
SUBROUTINE mtriqd(Ntype)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Costh , Ecpt(100) , Eltemp , Rho , Save(32) , Sinth , Stress
   LOGICAL Heat
   INTEGER Inflag , Isave(32) , Matid
   COMMON /matin / Matid , Inflag , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ Rho
   COMMON /sma2et/ Ecpt
   COMMON /sma2ht/ Heat
!
! Dummy argument declarations
!
   INTEGER Ntype
!
! Local variable declarations
!
   INTEGER i , narg
!
! End of declarations
!
!
!
!     8/18/67           E C P T     L I S T I N G
!
!     ECPT    TRPLT     TRIA1     TRIA2     QDPLT     QUAD1     QUAD2
!     *****************************************************************
!
!      1     ELEM ID   ELEM ID   ELEM ID   ELEM ID   ELEM ID   ELEM ID
!      2     GRID A    GRID A    GRID A    GRID A    GRID A    GRID A
!      3     GRID B    GRID B    GRID B    GRID B    GRID B    GRID B
!      4     GRID C    GRID C    GRID C    GRID C    GRID C    GRID C
!      5     THETA     THETA     THETA     GRID D    GRID D    GRID D
!      6     MATID1    MATID1    MAT ID    THETA     THETA     THETA
!      7     I         T1        T         MATID1    MATID1    MAT ID
!      8     MATID2    MATID2    NS MASS   I         T1        T
!      9     T2        I         CSID 1    MATID2    MATID2    NS MASS
!     10     NS MASS   MATID3    X1        T2        I         CSID 1
!     11     Z1        T2        Y1        NS MASS   MATID3    X1
!     12     Z2        NS MASS   Z1        Z1        T2        Y1
!     13     CSID 1    Z1        CSID 3    Z2        NS MASS   Z1
!     14     X1        Z2        X2        CSID 1    Z1        CSID 2
!     15     Y1        CSID 1    Y2        X1        Z2        X2
!     16     Z1        X1        Z2        Y1        CSID 1    Y2
!     17     CSID 2    Y1        CSID 3    Z1        X1        Z2
!     18     X2        Z1        X3        CSID 2    Y1        CSID 3
!     19     Y2        CSID 2    Y3        X2        Z1        X3
!     20     Z2        X2        Z3        Y2        CSID 2    Y3
!     21     CSID 3    Y2        TEMP      Z2        X2        Z3
!     22     X3        Z2                  CSID 3    Y2        CSID 4
!     23     Y3        CSID 3              X3        Z2        X4
!     24     Z3        X3                  Y3        CSID 3    Y4
!     25     TEMP      Y3                  Z3        X3        Z4
!     26               Z3                  CSID 4    Y3        TEMP
!     27               TEMP                X4        Z3
!     28                                   Y4        CSID 4
!     29                                   Z4        X4
!     30                                   TEMP      Y4
!     31                                             Z4
!     32                                             TEMP
!
!
   EQUIVALENCE (Save(1),Isave(1),Ecpt(50))
!
!     THIS SUBROUTINE INCORPORATES TRIA1, QUAD1, TRIA2, QUAD2
!
!             NTYPE = 1  IMPLIES MTRIA1
!             NTYPE = 2  IMPLIES MTRIA2
!             NTYPE = 3  IMPLIES MQUAD1
!             NTYPE = 4  IMPLIES MQUAD2
!
!     IF (I . EQ. 0) THEN COMPUTE UNCOUPLED MASS
!
!     CALL MASSTQ (NARG)
!          WHERE   NARG = 5 FOR TRIA1
!                  NARG = 4 FOR TRIA2
!                  NARG = 2 FOR QUAD1
!                  NARG = 1 FOR QUAD2
!
!     CALLS FROM THIS ROUTINE CAN BE MADE TO
!
!            MTRPLT - TRIANGULAR PLATE ROUTINE
!            MQDPLT - QUADRILATERAL PLATE ROUTINE
!            MASSTQ - UNCOUPLED MASS COMBINATION ELEMENT ROUTINE
!
!     ALL INSERTIONS OF 6X6 ELEMENT MASS MATRICES ARE HANDLED BY
!     THE ABOVE ROUTINES.
!
!     THE SAVED ECPT IS EQUIVALENCED TO ECPT(50)
!
!
!     SAVE THE INCOMING ECPT
!
   Inflag = 4
   DO i = 1 , 32
      Save(i) = Ecpt(i)
   ENDDO
!
!     TRANSFER TO OPERATIONS DESIRED
!
!            MTRIA1 MTRIA2 MQUAD1 MQUAD2
   IF ( Ntype==2 ) THEN
!
!     *** MTRIA2 ***
!
!     SET UP ECPT FOR CALL TO MTRPLT
!
      IF ( Save(7)/=0.0 ) THEN
!
         DO i = 1 , 6
            Ecpt(i) = Save(i)
         ENDDO
         Ecpt(7) = Save(7)**3/12.0
         Ecpt(8) = Save(6)
         Ecpt(9) = Save(7)
         Matid = Isave(6)
         CALL mat(Ecpt(1))
         Ecpt(10) = Save(8) + Rho*Save(7)
         DO i = 13 , 25
            Ecpt(i) = Save(i-4)
         ENDDO
!
         IF ( .NOT.Heat ) CALL mtrplt
      ELSE
         narg = 4
         CALL masstq(narg)
      ENDIF
   ELSEIF ( Ntype==3 ) THEN
!
!     *** MQUAD1 ***
!
!      SET UP ECPT FOR CALL TO MQDPLT.  FIRST CHECK I EQUAL ZERO
!
      IF ( Save(10)/=0.0 ) THEN
!
         DO i = 1 , 6
            Ecpt(i) = Save(i)
         ENDDO
         DO i = 7 , 30
            Ecpt(i) = Save(i+2)
         ENDDO
         Matid = Isave(7)
         IF ( Save(8)==0.0 ) THEN
            Ecpt(11) = Save(13)
         ELSE
            CALL mat(Ecpt(1))
!
            Ecpt(11) = Save(13) + Rho*Save(8)
         ENDIF
         IF ( .NOT.Heat ) CALL mqdplt
      ELSE
         narg = 2
         CALL masstq(narg)
      ENDIF
   ELSEIF ( Ntype==4 ) THEN
!
!     *** MQUAD2 ***
!
!     SET UP ECPT FOR CALL TO MQDPLT
!
      IF ( Save(8)/=0.0 ) THEN
!
         DO i = 1 , 7
            Ecpt(i) = Save(i)
         ENDDO
         Ecpt(8) = Save(8)**3/12.0
         Ecpt(9) = Save(7)
         Ecpt(10) = Save(8)
         Matid = Isave(7)
         CALL mat(Ecpt(1))
         Ecpt(11) = Save(9) + Rho*Save(8)
         DO i = 14 , 30
            Ecpt(i) = Save(i-4)
         ENDDO
!
         IF ( .NOT.Heat ) CALL mqdplt
      ELSE
         narg = 1
         CALL masstq(narg)
      ENDIF
!
!     *** MTRIA1 ***
!
!     SET UP ECPT FOR CALL TO MTRPLT.  FIRST CHECK I EQUAL ZERO
!
   ELSEIF ( Save(9)/=0.0 ) THEN
!
      DO i = 1 , 5
         Ecpt(i) = Save(i)
      ENDDO
      DO i = 6 , 25
         Ecpt(i) = Save(i+2)
      ENDDO
      Matid = Isave(6)
      IF ( Save(7)==0.0 ) THEN
         Ecpt(10) = Save(12)
      ELSE
         CALL mat(Ecpt(1))
!
         Ecpt(10) = Save(12) + Rho*Save(7)
      ENDIF
      IF ( .NOT.Heat ) CALL mtrplt
   ELSE
      narg = 5
      CALL masstq(narg)
   ENDIF
END SUBROUTINE mtriqd
