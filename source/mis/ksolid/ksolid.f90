!*==ksolid.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ksolid(Itype)
   USE c_sma1cl
   USE c_sma1dp
   USE c_sma1et
   USE c_system
   USE c_xmssg
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Itype
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , igflag , iopt , itemp , itet , j , jpoint , jtype , kpoint , ngrids , ntet
   INTEGER , SAVE :: idelem
   INTEGER , DIMENSION(22,4) , SAVE :: m
   INTEGER , DIMENSION(52) :: necpt
   EXTERNAL kpltst , ktetra , sadotb , saxb
!
! End of declarations rewritten by SPAG
!
!
!     IOPT = 1 IMPLIES WEDGE - 3 TETRAHEDRONS
!     IOPT = 2 IMPLIES HEXA(6-SIDED-SOLID) 5  TETRAHEDRONS
!     IOPT = 3 IMPLIES HEXA(6-SIDED-SOLID) 10 TETRAHEDRONS
!
!     ECPT        TETRA          WEDGE          HEXA
!     ------------------------------------------------
!     ECPT( 1) =  EL ID          EL ID          EL ID
!     ECPT( 2) =  MAT-ID         MAT-ID         MAT-ID
!     ECPT( 3) =  GRID-1         GRID-1         GRID-1
!     ECPT( 4) =  GRID-2         GRID-2         GRID-2
!     ECPT( 5) =  GRID-3         GRID-3         GRID-3
!     ECPT( 6) =  GRID-4         GRID-4         GRID-4
!     ECPT( 7) =  CSID-1         GRID-5         GRID-5
!     ECPT( 8) =  X1             GRID-6         GRID-6
!     ECPT( 9) =  Y1             CSID-1         GRID-7
!     ECPT(10) =  Z1             X1             GRID-8
!     ECPT(11) =  CSID-2         Y1             CSID-1
!     ECPT(12) =  X2             Z1             X1
!     ECPT(13) =  Y2             CSID-2         Y1
!     ECPT(14) =  Z2             X2             Z1
!     ECPT(15) =  CSID-3         Y2             CSID-2
!     ECPT(16) =  X3             Z2             X2
!     ECPT(17) =  Y3             CSID-3         Y2
!     ECPT(18) =  Z3             X3             Z2
!     ECPT(19) =  CSID-4         Y3             CSID-3
!     ECPT(20) =  X4             Z3             X3
!     ECPT(21) =  Y4             CSID-4         Y3
!     ECPT(22) =  Z4             X4             Z3
!     ECPT(23) =  EL-TEM         Y4             CSID-4
!     ECPT(24)                   Z4             X4
!     ECPT(25)                   CSID-5         Y4
!     ECPT(26)                   X5             Z4
!     ECPT(27)                   Y5             CSID-5
!     ECPT(28)                   Z5             X5
!     ECPT(29)                   CSID-6         Y5
!     ECPT(30)                   X6             Z5
!     ECPT(31)                   Y6             CSID-6
!     ECPT(32)                   Z6             X6
!     ECPT(33)                   ELTEMP         Y6
!     ECPT(34)                                  Z6
!     ECPT(35)                                  CSID-7
!     ECPT(36)                                  X7
!     ECPT(37)                                  Y7
!     ECPT(38)
!     ECPT(39)                                  CSID-8
!     ECPT(40)                                  X8
!     ECPT(41)                                  Y8
!     ECPT(42)                                  Z8
!     ECPT(43)                                  EL-TEMP
!
!     MAP FOR WEDGE  M(I,J)  I = TETRAHEDRON, J = GRID POINT
!
   !>>>>EQUIVALENCE (Necpt(1),Ecpt(1))
   DATA m(1,1) , m(1,2) , m(1,3) , m(1,4)/1 , 2 , 3 , 4/
   DATA m(2,1) , m(2,2) , m(2,3) , m(2,4)/1 , 2 , 3 , 5/
   DATA m(3,1) , m(3,2) , m(3,3) , m(3,4)/1 , 2 , 3 , 6/
   DATA m(4,1) , m(4,2) , m(4,3) , m(4,4)/1 , 4 , 5 , 6/
   DATA m(5,1) , m(5,2) , m(5,3) , m(5,4)/2 , 4 , 5 , 6/
   DATA m(6,1) , m(6,2) , m(6,3) , m(6,4)/3 , 4 , 5 , 6/
   DATA m(7,1) , m(7,2) , m(7,3) , m(7,4)/2 , 1 , 4 , 6/
   DATA m(8,1) , m(8,2) , m(8,3) , m(8,4)/2 , 3 , 4 , 6/
   DATA m(9,1) , m(9,2) , m(9,3) , m(9,4)/1 , 3 , 4 , 5/
   DATA m(10,1) , m(10,2) , m(10,3) , m(10,4)/2 , 3 , 4 , 5/
   DATA m(11,1) , m(11,2) , m(11,3) , m(11,4)/3 , 1 , 5 , 6/
   DATA m(12,1) , m(12,2) , m(12,3) , m(12,4)/2 , 1 , 5 , 6/
!
!     MAP FOR HEXA-SOLID (5 OR 10 TETRAHEDRONS)
!
   DATA m(13,1) , m(13,2) , m(13,3) , m(13,4)/1 , 2 , 3 , 6/
   DATA m(14,1) , m(14,2) , m(14,3) , m(14,4)/1 , 3 , 4 , 8/
   DATA m(15,1) , m(15,2) , m(15,3) , m(15,4)/1 , 3 , 8 , 6/
   DATA m(16,1) , m(16,2) , m(16,3) , m(16,4)/1 , 5 , 6 , 8/
   DATA m(17,1) , m(17,2) , m(17,3) , m(17,4)/3 , 6 , 7 , 8/
   DATA m(18,1) , m(18,2) , m(18,3) , m(18,4)/2 , 3 , 4 , 7/
   DATA m(19,1) , m(19,2) , m(19,3) , m(19,4)/1 , 2 , 4 , 5/
   DATA m(20,1) , m(20,2) , m(20,3) , m(20,4)/2 , 4 , 5 , 7/
   DATA m(21,1) , m(21,2) , m(21,3) , m(21,4)/2 , 5 , 6 , 7/
   DATA m(22,1) , m(22,2) , m(22,3) , m(22,4)/4 , 5 , 7 , 8/
   DATA idelem/0/
!
!     BRANCH ON ELEMENT TYPE
!
   igflag = 0
   IF ( Itype==2 ) THEN
!
!     COME HERE FOR 5-TETRAHEDRON 6-SIDED SOLID
!
      itet = 13
      ntet = 17
      itemp = 43
      ngrids = 8
      iopt = 0
      CALL spag_block_3
      RETURN
   ELSEIF ( Itype==3 ) THEN
!
!     COME HERE FOR 10-TETRAHEDRON 6-SIDED SOLID
!
      itet = 13
      ntet = 22
      itemp = 43
      ngrids = 8
      iopt = 1
      CALL spag_block_3
      RETURN
   ELSE
!
!     COME HERE FOR WEDGE COMPUTATIONS.
!     KTETRA IS CALLED 3 TIMES BASED ON WEDGE MAPPING MATRIX.
!
      itet = 1
      ntet = 12
      itemp = 33
      ngrids = 6
      iopt = 0
!
!     BASE CROSS PRODUCT
!
      IF ( necpt(1)==idelem ) THEN
         CALL spag_block_2
         RETURN
      ENDIF
      idelem = necpt(1)
      igflag = 1
      r12(1) = ecpt(14) - ecpt(10)
      r12(2) = ecpt(15) - ecpt(11)
      r12(3) = ecpt(16) - ecpt(12)
      r13(1) = ecpt(18) - ecpt(10)
      r13(2) = ecpt(19) - ecpt(11)
      r13(3) = ecpt(20) - ecpt(12)
      CALL saxb(r12,r13,rxr)
!
!     IN THE ABOVE, THE WEDGE IS NUMBERED 1,2,3 COUNTERCLOCKWISE AT THE
!     BASE AND 4,5,6 COUNTER CLOCKWISE AT THE TOP. (LOOKING DOWN ON WED)
!
      r12(1) = ecpt(26) - ecpt(22)
      r12(2) = ecpt(27) - ecpt(23)
      r12(3) = ecpt(28) - ecpt(24)
      r13(1) = ecpt(30) - ecpt(22)
      r13(2) = ecpt(31) - ecpt(23)
      r13(3) = ecpt(32) - ecpt(24)
      CALL saxb(r12,r13,r)
!
      IF ( sadotb(r,rxr)>0 ) THEN
!
!     PLANER CHECKS FOR WEDGE
!
         CALL kpltst(ecpt(10),ecpt(14),ecpt(26),ecpt(22))
         CALL kpltst(ecpt(10),ecpt(22),ecpt(30),ecpt(18))
         CALL kpltst(ecpt(14),ecpt(18),ecpt(30),ecpt(26))
         CALL spag_block_2
         RETURN
      ENDIF
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
!
!     ERROR CONDITION - BAD GEOMETRY
!
      WRITE (out,99001) ufm , Necpt(1)
99001 FORMAT (A23,' 4001, ELEMENT',I10,' HAS BAD GEOMETRY.')
      nogoo = 1
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
      IF ( nogoo==1 ) RETURN
      CALL spag_block_4
   END SUBROUTINE spag_block_2
   SUBROUTINE spag_block_3
!
!     CHECK GEOMETRY OF 6-SIDED SOLID AT THIS POINT
!
      IF ( Necpt(1)/=Idelem ) THEN
         Idelem = Necpt(1)
         Igflag = 1
         r13(1) = ecpt(20) - ecpt(12)
         r13(2) = ecpt(21) - ecpt(13)
         r13(3) = ecpt(22) - ecpt(14)
         r24(1) = ecpt(24) - ecpt(16)
         r24(2) = ecpt(25) - ecpt(17)
         r24(3) = ecpt(26) - ecpt(18)
         CALL saxb(r13,r24,Rxr)
!
         r12(1) = ecpt(36) - ecpt(28)
         r12(2) = ecpt(37) - ecpt(29)
         r12(3) = ecpt(38) - ecpt(30)
         r13(1) = ecpt(40) - ecpt(32)
         r13(2) = ecpt(41) - ecpt(33)
         r13(3) = ecpt(42) - ecpt(34)
         CALL saxb(r12,r13,R)
!
         IF ( sadotb(Rxr,R)<=0 ) THEN
            CALL spag_block_1
            RETURN
         ENDIF
!
!     PLANER CHECKS FOR HEXA-5 OR HEXA-10
!
         CALL kpltst(ecpt(12),ecpt(16),ecpt(20),ecpt(24))
         CALL kpltst(ecpt(12),ecpt(16),ecpt(32),ecpt(28))
         CALL kpltst(ecpt(16),ecpt(20),ecpt(36),ecpt(32))
         CALL kpltst(ecpt(20),ecpt(24),ecpt(40),ecpt(36))
         CALL kpltst(ecpt(24),ecpt(12),ecpt(28),ecpt(40))
         CALL kpltst(ecpt(28),ecpt(32),ecpt(36),ecpt(40))
      ENDIF
      IF ( nogoo==1 ) RETURN
      CALL spag_block_4
   END SUBROUTINE spag_block_3
   SUBROUTINE spag_block_4
!
!     AT THIS POINT ALL CHECKS HAVE BEEN MADE. NOW FORM THE ECPT FOR
!     EACH TETRAHEDRON AND CALL KTETRA(IOPT). IOPT = 1 IMPLIES TO COMPUT
!     HALF STIFFNESS. IOPT = 0 IMPLIES COMPUTE FULL STIFFNESS.
!
      DO J = 1 , 50
         ecpt(J+50) = ecpt(J)
      ENDDO
!
!     FILL MAT ID AND EL TEMP
!
      Necpt(2) = Necpt(52)
      Necpt(23) = Necpt(Itemp+50)
      Jtype = Itype
      DO I = Itet , Ntet
         IF ( I==Ntet ) Jtype = -Itype
         IF ( Itype==1 ) Iopt = I + 10
!
!     FILL IN GRID SIL-S AND COORDINATE SETS
!
         DO J = 1 , 4
            Kpoint = M(I,J)
            Necpt(J+2) = Necpt(Kpoint+52)
            Kpoint = 4*Kpoint + Ngrids - 3
            Jpoint = 4*J + 2
            Necpt(Jpoint+1) = Necpt(Kpoint+52)
            Necpt(Jpoint+2) = Necpt(Kpoint+53)
            Necpt(Jpoint+3) = Necpt(Kpoint+54)
            Necpt(Jpoint+4) = Necpt(Kpoint+55)
         ENDDO
!
!     BUMP IOPT IF GEOMETRY TESTS ARE TO BE MADE
!
         IF ( Igflag==1 ) Iopt = Iopt + 100
         CALL ktetra(Iopt,Jtype)
      ENDDO
   END SUBROUTINE spag_block_4
!
!     ALL THROUGH
!
END SUBROUTINE ksolid
