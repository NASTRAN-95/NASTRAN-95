!*==ktetra.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ktetra(Iopt,Jtype)
   USE c_blank
   USE c_hmtout
   USE c_hydroe
   USE c_matin
   USE c_matout
   USE c_sma1cl
   USE c_sma1dp
   USE c_sma1et
   USE c_sma1ht
   USE c_sma1io
   USE c_system
   USE c_xmssg
   USE iso_fortran_env
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
            direc = 0
            kount = 0
            tvol = 0.0
            ngpt = 99
            IF ( volume>0.0 .OR. surfac>0.0 ) THEN
               ngpt = 8
               IF ( iabs(Jtype)==1 ) ngpt = 6
               IF ( Jtype==0 ) ngpt = 4
            ENDIF
         ENDIF
         IF ( Jtype<=0 ) kount = kount + 1
!
!     RETURN IF SUB-TETRA DOES NOT CONTRIBUTE TO PIVOT STIFFNESS AND NO
!     GEOMETRY TESTS ARE BEING MADE ON IT.
!
         IF ( Iopt<100 ) THEN
            DO i = 3 , 6
               IF ( npvt==necpt(i) ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            IF ( kount==ngpt .AND. Jtype/=0 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            RETURN
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
         h(1) = 1.0D0
         h(2) = ecpt(8)
         h(3) = ecpt(9)
         h(4) = ecpt(10)
         h(5) = 1.0D0
         h(6) = ecpt(12)
         h(7) = ecpt(13)
         h(8) = ecpt(14)
         h(9) = 1.0D0
         h(10) = ecpt(16)
         h(11) = ecpt(17)
         h(12) = ecpt(18)
         h(13) = 1.0D0
         h(14) = ecpt(20)
         h(15) = ecpt(21)
         h(16) = ecpt(22)
!
!     INVERT H AND GET THE DETERMINANT
!
         ising = 0
         CALL inverd(4,h(1),4,dum,0,hdeter,ising,temp(1))
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
            IF ( direc/=0 ) THEN
               IF ( direc==1 .AND. hdeter>0.0D0 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( direc==-1 .AND. hdeter<0.0D0 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSE
               direc = 1
               IF ( hdeter<0.0D0 ) direc = -1
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         WRITE (out,99001) ufm , necpt(1)
99001    FORMAT (A23,' 4004, MODULE SMA1 DETECTS BAD OR REVERSE GEOMETRY ','FOR ELEMENT ID',I10)
         nogoo = 1
         RETURN
      CASE (3)
!
!     SKIP SUB-TETRAHEDRON IF IT DOES NOT CONTRIBUTE TO PIVOT STIFFNESS
!
         DO i = 3 , 6
            IF ( npvt==necpt(i) ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         IF ( kount==ngpt .AND. Jtype/=0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         RETURN
      CASE (4)
!
!     AT THIS POINT BRANCH ON HEAT OR STRUCTURE PROBLEM.
!
         hdeter = dabs(hdeter)
         IF ( heat ) THEN
!
!     HEAT PROBLEM LOGIC FOR 1 PIVOT ROW OF 1 TETRAHEDRON.
!
!     OBTAIN G  MATERIAL MATRIX FROM HMAT ROUTINE
!             E
!
            matid = necpt(2)
            inflag = 3
            eltemp = ecpt(23)
            CALL hmat(necpt)
            g(1) = 0.0D0
            g(2) = 0.0D0
            g(3) = 0.0D0
            g(4) = 0.0D0
            g(5) = 0.0D0
            g(6) = matbuf(1)
            g(7) = matbuf(2)
            g(8) = matbuf(3)
            g(9) = 0.0D0
            g(10) = matbuf(2)
            g(11) = matbuf(4)
            g(12) = matbuf(5)
            g(13) = 0.0D0
            g(14) = matbuf(3)
            g(15) = matbuf(5)
            g(16) = matbuf(6)
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( hydro ) THEN
!
!     HYDROELASTIC PROBLEM, OBTAIN DENSITY AND RETURN
!
            matid = necpt(2)
            inflag = 11
            CALL mat(necpt(1))
            DO idlh = 1 , 16
               g(idlh) = 0.0D0
            ENDDO
            g(6) = 1.0D0/dble(rho)
            g(11) = g(6)
            g(16) = g(6)
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     GET THE MATERIAL DATA AND FILL THE 6X6 G MATERIAL STRESS-STRAIN
!     MATRIX.
!
            inflag = 1
            matid = necpt(2)
            eltemp = ecpt(23)
            CALL mat(necpt(1))
            DO i = 1 , 36
               g(i) = 0.0D0
            ENDDO
            temp1 = (1.0+nu)*(1.0-2.0*nu)
            IF ( dabs(temp1)>1.0D-6 ) THEN
!
               g(1) = e*(1.0-nu)/temp1
               g(8) = g(1)
               g(15) = g(1)
               g(2) = e*nu/temp1
               g(3) = g(2)
               g(7) = g(2)
               g(9) = g(2)
               g(13) = g(2)
               g(14) = g(2)
               g(22) = gg
               g(29) = gg
               g(36) = gg
!
!     FILL 4 C-MATRICES. (6X3) EACH.
!
               DO i = 1 , 72
                  c(i) = 0.0D0
               ENDDO
               DO i = 1 , 4
                  j = 18*i - 18
                  c(j+1) = h(i+4)
                  c(j+5) = h(i+8)
                  c(j+9) = h(i+12)
                  c(j+11) = h(i+12)
                  c(j+12) = h(i+8)
                  c(j+13) = h(i+12)
                  c(j+15) = h(i+4)
                  c(j+16) = h(i+8)
                  c(j+17) = h(i+4)
               ENDDO
!
!     DIVIDE DETERMINANT BY 6.0, AND BY AN ADDITIONAL 2.0 IF A SUB-TETRA
!     FOR THE HEXA-10 ELEMENT.
!     FOR WEDGES, 1ST 6 CONFIGURATUONS ARE MULTIPLIED BY 2.
!     ALL CONFIGURATIONS ARE DIVIDED BY 6.
!
               IF ( Iopt==0 ) THEN
                  hdeter = hdeter/6.0D0
               ELSEIF ( Iopt>=11 .AND. Iopt<=22 ) THEN
!
!     WEDGES
!
                  hdeter = hdeter/36.0D0
                  IF ( Iopt<=16 ) hdeter = hdeter*2.0D0
               ELSE
                  hdeter = hdeter/12.0D0
               ENDIF
               DO i = 1 , 36
                  kij(i) = 0.0D0
               ENDDO
!
!     DETERMINE THE PIVOT POINT
!
               DO i = 2 , 5
                  ka = 4*i - 1
                  npoint = 18*i - 35
                  IF ( necpt(i+1)==npvt ) GOTO 5
               ENDDO
               CALL mesage(-30,34,ecpt(1))
!
!     PICK UP PIVOT TRANSFORMATION IF CSID IS NON-ZERO.
!
 5             IF ( necpt(ka)/=0 ) THEN
                  CALL transd(necpt(ka),t)
                  CALL gmmatd(t(1),3,3,1,c(npoint),6,3,1,ct(1))
                  CALL gmmatd(ct(1),3,6,0,g(1),6,6,0,gct(1))
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
                  CALL gmmatd(c(npoint),6,3,1,g(1),6,6,0,gct(1))
               ENDIF
               DO i = 1 , 18
                  gct(i) = gct(i)*hdeter
               ENDDO
!
!     LOOP THROUGH THE 4 POINTS INSERTING THE STIFFNESS MATRIX FOR
!     EACH WITH RESPECT TO THE PIVOT POINT.
!
               DO i = 1 , 4
                  IF ( necpt(4*i+3)/=0 ) THEN
                     CALL transd(necpt(4*i+3),t)
                     CALL gmmatd(c(18*i-17),6,3,0,t(1),3,3,0,ct(1))
                     CALL gmmatd(gct(1),3,6,0,ct(1),6,3,0,t(1))
                  ELSE
!
!     NO TRANSFORMATION
!
                     CALL gmmatd(gct(1),3,6,0,c(18*i-17),6,3,0,t(1))
                  ENDIF
!
!     INSERT 3X3 KIJ INTO 6X6 KIJ AND CALL SMA1B FOR INSERTION.
!
                  kij(1) = t(1)
                  kij(2) = t(2)
                  kij(3) = t(3)
                  kij(7) = t(4)
                  kij(8) = t(5)
                  kij(9) = t(6)
                  kij(13) = t(7)
                  kij(14) = t(8)
                  kij(15) = t(9)
!
                  CALL sma1b(kij(1),necpt(i+2),-1,ifkgg,0.0D0)
                  temp1 = gsube
                  IF ( iopt4/=0 ) THEN
                     IF ( gsube/=0 ) CALL sma1b(kij(1),necpt(i+2),-1,if4gg,temp1)
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
               tvol = tvol + hdeter/4.0D+0
               IF ( kount<ngpt ) THEN
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSE
               WRITE (out,99002) ufm , matid , ecpt(1)
99002          FORMAT (A23,' 4005, AN ILLEGAL VALUE OF -NU- HAS BEEN SPECIFIED ','UNDER MATERIAL ID',I10,' FOR ELEMENT ID',I10)
               nogoo = 1
               RETURN
            ENDIF
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         IF ( Jtype<=0 ) THEN
            ecpt(2) = tvol*volume
            IF ( Jtype==0 .AND. surfac>0.0 ) THEN
               CALL write(scr4,el(1,4),2,0)
               CALL write(scr4,ecpt(1),2,0)
               IF ( rho>0.0 ) tvol = tvol*rho
               ecpt(1) = tvol
               necpt(2) = ngpt
               CALL write(scr4,ecpt(1),22,1)
            ELSE
               j = iabs(Jtype)
               ecpt(3) = tvol
               IF ( rho>0.0 ) ecpt(3) = tvol*rho
               necpt(4) = ngpt
               CALL write(scr4,el(1,j),2,0)
               CALL write(scr4,ecpt(1),4,0)
               IF ( surfac>0.0 ) THEN
                  j = ngpt*5
                  CALL write(scr4,ecpt(53),j,1)
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
         CALL gmmatd(g(1),4,4,0,h(1),4,4,0,c(5))
         ihcol = i - 2
         temp(1) = h(ihcol)
         temp(2) = h(ihcol+4)
         temp(3) = h(ihcol+8)
         temp(4) = h(ihcol+12)
         CALL gmmatd(temp(1),1,4,0,c(5),4,4,0,c(1))
!
!     DIVIDE CONDUCTIVITY BY 2.0 IF THIS IS A SUB-TETRA OF A HEXA2
!     ELEMENT.
!
         IF ( Iopt==0 ) THEN
            hdeter = hdeter/6.0D0
         ELSEIF ( Iopt>=11 .AND. Iopt<=22 ) THEN
!
!     WEDGES
!
            hdeter = hdeter/36.0D0
            IF ( Iopt<=16 ) hdeter = hdeter*2.0D0
         ELSE
            hdeter = hdeter/12.0D0
         ENDIF
         DO i = 1 , 4
            c(i) = c(i)*hdeter
         ENDDO
!
!     INSERT THE PIVOT ROW.
!
         DO i = 1 , 4
            CALL sma1b(c(i),necpt(i+2),npvt,ifkgg,0.0D0)
         ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ktetra
