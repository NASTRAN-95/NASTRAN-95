!*==ktetra.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ktetra(Iopt,Jtype)
USE C_BLANK
USE C_HMTOUT
USE C_HYDROE
USE C_MATIN
USE C_MATOUT
USE C_SMA1CL
USE C_SMA1DP
USE C_SMA1ET
USE C_SMA1HT
USE C_SMA1IO
USE C_SYSTEM
USE C_XMSSG
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iopt
   INTEGER :: Jtype
!
! Local variable declarations rewritten by SPAG
!
   REAL :: dum
   INTEGER , DIMENSION(2,4) , SAVE :: el
   INTEGER :: i , idlh , ihcol , ising , j , ka , npoint
   INTEGER , SAVE :: idflag , scr4
   INTEGER , DIMENSION(4) :: necpt
   EXTERNAL gmmatd , hmat , inverd , mat , mesage , sma1b , transd , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     ELEMENT STIFFNESS MATRIX GENERATOR FOR THE TETRAHEDRON SOLID
!     ELEMENT
!
!     LOOKING DOWN ON THIS ELEMENT, GRIDS 1,2,3 ARE THE BASE AND MUST BE
!     LABELED COUNTERCLOCKWISE. GRID 4 MUST BE ABOVE THE PLANE FORMED BY
!     GRIDS 1,2,3 AND CLOSEST TO THIS OBSERVER.
!
!     ECPT FOR THE TETRAHEDRON SOLID ELEMENT
!     --------------------------------------
!     ECPT( 1) = ELEMENT  ID
!     ECPT( 2) = MATERIAL ID (MAT1 MATERIAL TYPE)
!     ECPT( 3) = SIL GRID POINT 1
!     ECPT( 4) = SIL GRID POINT 2
!     ECPT( 5) = SIL GRID POINT 3
!     ECPT( 6) = SIL GRID POINT 4
!     ECPT( 7) = COORD SYS ID GRID PT 1
!     ECPT( 8) = X1
!     ECPT( 9) = Y1
!     ECPT(10) = Z1
!     ECPT(11) = COORD SYS ID GRID PT 2
!     ECPT(12) = X2
!     ECPT(13) = Y2
!     ECPT(14) = Z2
!     ECPT(15) = COORD SYS ID GRID PT 3
!     ECPT(16) = X3
!     ECPT(17) = Y3
!     ECPT(18) = Z3
!     ECPT(19) = COORD SYS ID GRID PT 4
!     ECPT(20) = X4
!     ECPT(21) = Y4
!     ECPT(22) = Z4
!     ECPT(23) = ELEMENT TEMPERATURE
!
!     JTYPE = 1 FOR WEDGE, = 2 FOR HEXA1, = 3 FOR HEXA2, AND = 0 TETRA
!     IF JTYPE IS NEGATIVE, THIS IS LAST CALL FROM KSOLID
!
!
   !>>>>EQUIVALENCE (Necpt(1),Ecpt(1))
   DATA idflag/0/ , scr4/304/
   DATA el/4HCWED , 4HGE   , 4HCHEX , 4HA1   , 4HCHEX , 4HA2   , 4HCTET , 4HRA  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     FILL THE 4 X 4 H MATRIX.
!
         IF ( necpt(1)/=idflag ) THEN
            idflag = necpt(1)
            Direc = 0
            Kount = 0
            Tvol = 0.0
            Ngpt = 99
            IF ( Volume>0.0 .OR. Surfac>0.0 ) THEN
               Ngpt = 8
               IF ( iabs(Jtype)==1 ) Ngpt = 6
               IF ( Jtype==0 ) Ngpt = 4
            ENDIF
         ENDIF
         IF ( Jtype<=0 ) Kount = Kount + 1
!
!     RETURN IF SUB-TETRA DOES NOT CONTRIBUTE TO PIVOT STIFFNESS AND NO
!     GEOMETRY TESTS ARE BEING MADE ON IT.
!
         IF ( Iopt<100 ) THEN
            DO i = 3 , 6
               IF ( Npvt==necpt(i) ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            IF ( Kount==Ngpt .AND. Jtype/=0 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            RETURN
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
         H(1) = 1.0D0
         H(2) = Ecpt(8)
         H(3) = Ecpt(9)
         H(4) = Ecpt(10)
         H(5) = 1.0D0
         H(6) = Ecpt(12)
         H(7) = Ecpt(13)
         H(8) = Ecpt(14)
         H(9) = 1.0D0
         H(10) = Ecpt(16)
         H(11) = Ecpt(17)
         H(12) = Ecpt(18)
         H(13) = 1.0D0
         H(14) = Ecpt(20)
         H(15) = Ecpt(21)
         H(16) = Ecpt(22)
!
!     INVERT H AND GET THE DETERMINANT
!
         ising = 0
         CALL inverd(4,H(1),4,dum,0,Hdeter,ising,Temp(1))
!
!     IF THE DETERMINANT IS .LE. 0 THE TETRAHEDRON HAS BAD OR REVERSE
!     GEOMETRY WHICH IS AN ERROR CONDITION.
!
         IF ( ising/=2 ) THEN
            IF ( Iopt<100 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            Iopt = Iopt - 100
            IF ( Direc/=0 ) THEN
               IF ( Direc==1 .AND. Hdeter>0.0D0 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( Direc==-1 .AND. Hdeter<0.0D0 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSE
               Direc = 1
               IF ( Hdeter<0.0D0 ) Direc = -1
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         WRITE (Out,99001) Ufm , necpt(1)
99001    FORMAT (A23,' 4004, MODULE SMA1 DETECTS BAD OR REVERSE GEOMETRY ','FOR ELEMENT ID',I10)
         Nogoo = 1
         RETURN
      CASE (3)
!
!     SKIP SUB-TETRAHEDRON IF IT DOES NOT CONTRIBUTE TO PIVOT STIFFNESS
!
         DO i = 3 , 6
            IF ( Npvt==necpt(i) ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         IF ( Kount==Ngpt .AND. Jtype/=0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         RETURN
      CASE (4)
!
!     AT THIS POINT BRANCH ON HEAT OR STRUCTURE PROBLEM.
!
         Hdeter = dabs(Hdeter)
         IF ( Heat ) THEN
!
!     HEAT PROBLEM LOGIC FOR 1 PIVOT ROW OF 1 TETRAHEDRON.
!
!     OBTAIN G  MATERIAL MATRIX FROM HMAT ROUTINE
!             E
!
            Matid = necpt(2)
            Inflag = 3
            Eltemp = Ecpt(23)
            CALL hmat(necpt)
            G(1) = 0.0D0
            G(2) = 0.0D0
            G(3) = 0.0D0
            G(4) = 0.0D0
            G(5) = 0.0D0
            G(6) = Matbuf(1)
            G(7) = Matbuf(2)
            G(8) = Matbuf(3)
            G(9) = 0.0D0
            G(10) = Matbuf(2)
            G(11) = Matbuf(4)
            G(12) = Matbuf(5)
            G(13) = 0.0D0
            G(14) = Matbuf(3)
            G(15) = Matbuf(5)
            G(16) = Matbuf(6)
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( Hydro ) THEN
!
!     HYDROELASTIC PROBLEM, OBTAIN DENSITY AND RETURN
!
            Matid = necpt(2)
            Inflag = 11
            CALL mat(necpt(1))
            DO idlh = 1 , 16
               G(idlh) = 0.0D0
            ENDDO
            G(6) = 1.0D0/dble(Rho)
            G(11) = G(6)
            G(16) = G(6)
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     GET THE MATERIAL DATA AND FILL THE 6X6 G MATERIAL STRESS-STRAIN
!     MATRIX.
!
            Inflag = 1
            Matid = necpt(2)
            Eltemp = Ecpt(23)
            CALL mat(necpt(1))
            DO i = 1 , 36
               G(i) = 0.0D0
            ENDDO
            Temp1 = (1.0+Nu)*(1.0-2.0*Nu)
            IF ( dabs(Temp1)>1.0D-6 ) THEN
!
               G(1) = E*(1.0-Nu)/Temp1
               G(8) = G(1)
               G(15) = G(1)
               G(2) = E*Nu/Temp1
               G(3) = G(2)
               G(7) = G(2)
               G(9) = G(2)
               G(13) = G(2)
               G(14) = G(2)
               G(22) = Gg
               G(29) = Gg
               G(36) = Gg
!
!     FILL 4 C-MATRICES. (6X3) EACH.
!
               DO i = 1 , 72
                  C(i) = 0.0D0
               ENDDO
               DO i = 1 , 4
                  j = 18*i - 18
                  C(j+1) = H(i+4)
                  C(j+5) = H(i+8)
                  C(j+9) = H(i+12)
                  C(j+11) = H(i+12)
                  C(j+12) = H(i+8)
                  C(j+13) = H(i+12)
                  C(j+15) = H(i+4)
                  C(j+16) = H(i+8)
                  C(j+17) = H(i+4)
               ENDDO
!
!     DIVIDE DETERMINANT BY 6.0, AND BY AN ADDITIONAL 2.0 IF A SUB-TETRA
!     FOR THE HEXA-10 ELEMENT.
!     FOR WEDGES, 1ST 6 CONFIGURATUONS ARE MULTIPLIED BY 2.
!     ALL CONFIGURATIONS ARE DIVIDED BY 6.
!
               IF ( Iopt==0 ) THEN
                  Hdeter = Hdeter/6.0D0
               ELSEIF ( Iopt>=11 .AND. Iopt<=22 ) THEN
!
!     WEDGES
!
                  Hdeter = Hdeter/36.0D0
                  IF ( Iopt<=16 ) Hdeter = Hdeter*2.0D0
               ELSE
                  Hdeter = Hdeter/12.0D0
               ENDIF
               DO i = 1 , 36
                  Kij(i) = 0.0D0
               ENDDO
!
!     DETERMINE THE PIVOT POINT
!
               DO i = 2 , 5
                  ka = 4*i - 1
                  npoint = 18*i - 35
                  IF ( necpt(i+1)==Npvt ) GOTO 5
               ENDDO
               CALL mesage(-30,34,Ecpt(1))
!
!     PICK UP PIVOT TRANSFORMATION IF CSID IS NON-ZERO.
!
 5             IF ( necpt(ka)/=0 ) THEN
                  CALL transd(necpt(ka),T)
                  CALL gmmatd(T(1),3,3,1,C(npoint),6,3,1,Ct(1))
                  CALL gmmatd(Ct(1),3,6,0,G(1),6,6,0,Gct(1))
               ELSE
!
!                     T  T
!     AT THIS POINT  T  C  IS STORED AS A 3X6 IN THE CT ARRAY.
!                     I  I
!
!                                                T T
!     NOW MULTIPLY ON THE RIGHT BY G   TO FORM  T C G   (3X6)
!                                   E            I I E
!
                  CALL gmmatd(C(npoint),6,3,1,G(1),6,6,0,Gct(1))
               ENDIF
               DO i = 1 , 18
                  Gct(i) = Gct(i)*Hdeter
               ENDDO
!
!     LOOP THROUGH THE 4 POINTS INSERTING THE STIFFNESS MATRIX FOR
!     EACH WITH RESPECT TO THE PIVOT POINT.
!
               DO i = 1 , 4
                  IF ( necpt(4*i+3)/=0 ) THEN
                     CALL transd(necpt(4*i+3),T)
                     CALL gmmatd(C(18*i-17),6,3,0,T(1),3,3,0,Ct(1))
                     CALL gmmatd(Gct(1),3,6,0,Ct(1),6,3,0,T(1))
                  ELSE
!
!     NO TRANSFORMATION
!
                     CALL gmmatd(Gct(1),3,6,0,C(18*i-17),6,3,0,T(1))
                  ENDIF
!
!     INSERT 3X3 KIJ INTO 6X6 KIJ AND CALL SMA1B FOR INSERTION.
!
                  Kij(1) = T(1)
                  Kij(2) = T(2)
                  Kij(3) = T(3)
                  Kij(7) = T(4)
                  Kij(8) = T(5)
                  Kij(9) = T(6)
                  Kij(13) = T(7)
                  Kij(14) = T(8)
                  Kij(15) = T(9)
!
                  CALL sma1b(Kij(1),necpt(i+2),-1,Ifkgg,0.0D0)
                  Temp1 = Gsube
                  IF ( Iopt4/=0 ) THEN
                     IF ( Gsube/=0 ) CALL sma1b(Kij(1),necpt(i+2),-1,If4gg,Temp1)
                  ENDIF
!
               ENDDO
!
!     IF USER REQUESTED VOLUME AND SURFACE CALCULATIONS, WE NEED TO SAVE
!     IN SCR4 THE FOLLOWING
!     WORDS 1,2 = ELEM. BCD NAME
!             3 = ELEM. ID
!             4 = VOLUME
!             5 = MASS
!             6 = NO. OF GRID POINTS, NGPT
!           7 THRU 6+NGPT = GRID POINTS
!           7+NGPT THRU 7+5*NGPT = BGPDT DATA
!
               Tvol = Tvol + Hdeter/4.0D+0
               IF ( Kount<Ngpt ) THEN
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSE
               WRITE (Out,99002) Ufm , Matid , Ecpt(1)
99002          FORMAT (A23,' 4005, AN ILLEGAL VALUE OF -NU- HAS BEEN SPECIFIED ','UNDER MATERIAL ID',I10,' FOR ELEMENT ID',I10)
               Nogoo = 1
               RETURN
            ENDIF
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         IF ( Jtype<=0 ) THEN
            Ecpt(2) = Tvol*Volume
            IF ( Jtype==0 .AND. Surfac>0.0 ) THEN
               CALL write(scr4,el(1,4),2,0)
               CALL write(scr4,Ecpt(1),2,0)
               IF ( Rho>0.0 ) Tvol = Tvol*Rho
               Ecpt(1) = Tvol
               necpt(2) = Ngpt
               CALL write(scr4,Ecpt(1),22,1)
            ELSE
               j = iabs(Jtype)
               Ecpt(3) = Tvol
               IF ( Rho>0.0 ) Ecpt(3) = Tvol*Rho
               necpt(4) = Ngpt
               CALL write(scr4,el(1,j),2,0)
               CALL write(scr4,Ecpt(1),4,0)
               IF ( Surfac>0.0 ) THEN
                  j = Ngpt*5
                  CALL write(scr4,Ecpt(53),j,1)
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
!
         RETURN
      CASE (7)
!
!     OBTAIN THE FOUR CONDUCTIVITY VALUES NEEDED FOR PIVOT ROW BEING
!     INSERTED.
!
         CALL gmmatd(G(1),4,4,0,H(1),4,4,0,C(5))
         ihcol = i - 2
         Temp(1) = H(ihcol)
         Temp(2) = H(ihcol+4)
         Temp(3) = H(ihcol+8)
         Temp(4) = H(ihcol+12)
         CALL gmmatd(Temp(1),1,4,0,C(5),4,4,0,C(1))
!
!     DIVIDE CONDUCTIVITY BY 2.0 IF THIS IS A SUB-TETRA OF A HEXA2
!     ELEMENT.
!
         IF ( Iopt==0 ) THEN
            Hdeter = Hdeter/6.0D0
         ELSEIF ( Iopt>=11 .AND. Iopt<=22 ) THEN
!
!     WEDGES
!
            Hdeter = Hdeter/36.0D0
            IF ( Iopt<=16 ) Hdeter = Hdeter*2.0D0
         ELSE
            Hdeter = Hdeter/12.0D0
         ENDIF
         DO i = 1 , 4
            C(i) = C(i)*Hdeter
         ENDDO
!
!     INSERT THE PIVOT ROW.
!
         DO i = 1 , 4
            CALL sma1b(C(i),necpt(i+2),Npvt,Ifkgg,0.0D0)
         ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ktetra
