!*==mtriqd.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mtriqd(Ntype)
   USE c_matin
   USE c_matout
   USE c_sma2et
   USE c_sma2ht
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ntype
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , narg
   INTEGER , DIMENSION(32) :: isave
   REAL , DIMENSION(32) :: save
   EXTERNAL masstq , mat , mqdplt , mtrplt
!
! End of declarations rewritten by SPAG
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
   !>>>>EQUIVALENCE (Save(1),Isave(1),Ecpt(50))
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
   inflag = 4
   DO i = 1 , 32
      save(i) = ecpt(i)
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
      IF ( save(7)/=0.0 ) THEN
!
         DO i = 1 , 6
            ecpt(i) = save(i)
         ENDDO
         ecpt(7) = save(7)**3/12.0
         ecpt(8) = save(6)
         ecpt(9) = save(7)
         matid = isave(6)
         CALL mat(ecpt(1))
         ecpt(10) = save(8) + rho*save(7)
         DO i = 13 , 25
            ecpt(i) = save(i-4)
         ENDDO
!
         IF ( .NOT.heat ) CALL mtrplt
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
      IF ( save(10)/=0.0 ) THEN
!
         DO i = 1 , 6
            ecpt(i) = save(i)
         ENDDO
         DO i = 7 , 30
            ecpt(i) = save(i+2)
         ENDDO
         matid = isave(7)
         IF ( save(8)==0.0 ) THEN
            ecpt(11) = save(13)
         ELSE
            CALL mat(ecpt(1))
!
            ecpt(11) = save(13) + rho*save(8)
         ENDIF
         IF ( .NOT.heat ) CALL mqdplt
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
      IF ( save(8)/=0.0 ) THEN
!
         DO i = 1 , 7
            ecpt(i) = save(i)
         ENDDO
         ecpt(8) = save(8)**3/12.0
         ecpt(9) = save(7)
         ecpt(10) = save(8)
         matid = isave(7)
         CALL mat(ecpt(1))
         ecpt(11) = save(9) + rho*save(8)
         DO i = 14 , 30
            ecpt(i) = save(i-4)
         ENDDO
!
         IF ( .NOT.heat ) CALL mqdplt
      ELSE
         narg = 1
         CALL masstq(narg)
      ENDIF
!
!     *** MTRIA1 ***
!
!     SET UP ECPT FOR CALL TO MTRPLT.  FIRST CHECK I EQUAL ZERO
!
   ELSEIF ( save(9)/=0.0 ) THEN
!
      DO i = 1 , 5
         ecpt(i) = save(i)
      ENDDO
      DO i = 6 , 25
         ecpt(i) = save(i+2)
      ENDDO
      matid = isave(6)
      IF ( save(7)==0.0 ) THEN
         ecpt(10) = save(12)
      ELSE
         CALL mat(ecpt(1))
!
         ecpt(10) = save(12) + rho*save(7)
      ENDIF
      IF ( .NOT.heat ) CALL mtrplt
   ELSE
      narg = 5
      CALL masstq(narg)
   ENDIF
END SUBROUTINE mtriqd
