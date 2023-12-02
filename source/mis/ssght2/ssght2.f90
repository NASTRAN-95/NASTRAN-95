!*==ssght2.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ssght2(File,Delta,Uni)
   USE c_condad
   USE c_estout
   USE c_hmtout
   USE c_matin
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: File
   REAL , DIMENSION(1) :: Delta
   REAL , DIMENSION(1) :: Uni
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: area , determ , el , fact , pi , rbar
   REAL(REAL64) , DIMENSION(12) :: c
   REAL(REAL64) , DIMENSION(3,4) :: dr , drtemp
   REAL(REAL64) , DIMENSION(4,4) :: drt
   INTEGER :: flag , i , i1 , i2 , i3 , i4 , iel , ig , ipg , ising , itype , j , l , lrow , ltemp , m , nel , np , nq
   INTEGER , DIMENSION(4) :: ip , sindx
   REAL(REAL64) , DIMENSION(9) :: k , kq
   INTEGER , DIMENSION(15) , SAVE :: nels , npts
   INTEGER , DIMENSION(52) , SAVE :: smap
   INTEGER , DIMENSION(2) , SAVE :: subr
   REAL(REAL64) , DIMENSION(8) :: t1
   REAL :: xpts
   EXTERNAL dadotb , daxb , gmmatd , hmat , inverd , mesage , read
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE USES THE TEMPERATURE VECTOR DATA TO CALCULATE
!     LOAD VECTER TERMS WITH THE EQUATION-
!
!     DELTAP = (K(TI) - K(TO))*T1
!          WHERE       TO IS THE INITIAL TEMPERATURE
!                      TI IS THE  NEW  TEMPERATURE VECTOR
!                      K  IS THE TEMPERATURE DEPENDENT CONDUCTIVITY
!                          MATRIX
!                      DELTAP  IS THE NONLINEAR LOAD
!
   !>>>>EQUIVALENCE (Constd(1),Pi)
   DATA npts/2 , 3 , 4 , 3 , 4 , 4 , 6 , 8 , 8 , 1 , 2 , 2 , 3 , 4 , 2/
   DATA nels/1 , 1 , 4 , 1 , 4 , 1 , 3 , 5 , 10 , 1 , 1 , 1 , 1 , 4 , 1/
   DATA subr/4HSSGH , 4HT2  /
   DATA smap/1 , 2 , 3 , 6 , 1 , 2 , 6 , 5 , 1 , 4 , 5 , 6 , 1 , 2 , 3 , 6 , 1 , 3 , 4 , 8 , 1 , 3 , 8 , 6 , 1 , 5 , 6 , 8 , 3 , 6 ,&
      & 7 , 8 , 2 , 3 , 4 , 7 , 1 , 2 , 4 , 5 , 2 , 4 , 5 , 7 , 2 , 5 , 6 , 7 , 4 , 5 , 7 , 8/
   DO
!
!     READ DATA, 45 WORDS PER ELEMENT.
!
      CALL read(*200,*100,File,elid,45,0,flag)
!
!     CALCULATE AVERAGE ELEMENT TEMPERATURE
!
      np = npts(sub)
      xpts = float(np)
      IF ( sub>9 ) xpts = xpts*2.0
!
      temp = 0.0
      DO i = 1 , np
         ltemp = sil(i)
         temp = temp + Uni(ltemp)
         IF ( sub>9 ) THEN
            IF ( sil(i+4)/=0 ) THEN
               ltemp = sil(i+4)
               temp = temp + Uni(ltemp)
            ENDIF
         ENDIF
      ENDDO
      temp = temp/xpts
!
!     SET UP CALL TO MATERIAL SUBROUTINE
!
      inflag = 1
      IF ( sub>=2 .AND. sub<=5 ) inflag = 2
      IF ( sub>=6 .AND. sub<=9 ) inflag = 3
      sinth = 0.0
      costh = 1.0
      IF ( theta/=0.0 .AND. inflag==2 ) THEN
         sinth = sin(theta)
         costh = cos(theta)
      ENDIF
      matid = imat
      CALL hmat(elid)
!
!     SUBTRACT  CONDUCTIVITY AT INITIAL TEMPERATURE AND PLACE IN  MATRIX
!
      IF ( inflag==2 ) THEN
         k(1) = matout(1) - mato(1)
         k(2) = matout(2) - mato(2)
         k(3) = k(2)
         k(4) = matout(3) - mato(3)
      ELSEIF ( inflag==3 ) THEN
         k(1) = matout(1) - mato(1)
         k(2) = matout(2) - mato(2)
         k(3) = matout(3) - mato(3)
         k(4) = k(2)
         k(5) = matout(4) - mato(4)
         k(6) = matout(5) - mato(5)
         k(7) = k(3)
         k(8) = k(6)
         k(9) = matout(6) - mato(6)
      ELSE
         k(1) = matout(1) - mato(1)
      ENDIF
      ip(1) = 1
      ip(2) = 2
      ip(3) = 3
      IF ( sub==3 .OR. sub==5 ) THEN
!
!     MOVE  QUADS TO ELEMENT COORDINATES
!
         DO j = 1 , 2
            l = 1
            m = 2
            i1 = 1
            i2 = 2
            i3 = 3
            i4 = 4
            IF ( j/=1 ) THEN
               l = 3
               m = 4
               i1 = 3
               i2 = 4
               i3 = 1
               i4 = 2
            ENDIF
            DO i = 1 , 3
               dr(i,1) = r(i,i2) - r(i,i1)
               dr(i,3) = r(i,i3) - r(i,i1)
               dr(i,2) = r(i,i4) - r(i,i2)
            ENDDO
            CALL daxb(dr(1,3),dr(1,2),dr(1,4))
!
            area = dsqrt(dr(1,4)**2+dr(2,4)**2+dr(3,4)**2)
!
            DO i = 1 , 3
               dr(i,4) = dr(i,4)/area
            ENDDO
            el = dr(1,4)*dr(1,1) + dr(2,4)*dr(2,1) + dr(3,4)*dr(3,1)
            DO i = 1 , 3
               dr(i,1) = dr(i,1) - el*dr(i,4)
            ENDDO
            el = dsqrt(dr(1,1)**2+dr(2,1)**2+dr(3,1)**2)
            DO i = 1 , 3
               dr(i,1) = dr(i,1)/el
            ENDDO
!
            CALL daxb(dr(1,4),dr(1,1),dr(1,2))
            DO i = 1 , 3
               dr(i,4) = r(i,i4) - r(i,i1)
            ENDDO
            CALL gmmatd(dr(1,1),2,3,0,dr(1,3),2,3,1,kq)
            drt(l,3) = kq(1)
            drt(l,4) = kq(2)
            drt(m,3) = kq(3)
            drt(m,4) = kq(4)
            drt(l,1) = 0.0D0
            drt(l,2) = el
            drt(m,1) = 0.0D0
            drt(m,2) = 0.0D0
         ENDDO
      ELSEIF ( sub==2 .OR. sub==4 ) THEN
!
!     MOVE  TRIANGLES TO ELEMENT COORDINATES
!
         DO i = 1 , 3
            dr(i,1) = r(i,2) - r(i,1)
            dr(i,2) = r(i,3) - r(i,1)
         ENDDO
!
         el = dr(1,1)**2 + dr(2,1)**2 + dr(3,1)**2
         el = dsqrt(el)
         area = dadotb(dr(1,1),dr(1,2))/el
         CALL daxb(dr(1,1),dr(1,2),dr(1,3))
         dr(2,3) = dsqrt(dr(1,3)**2+dr(2,3)**2+dr(3,3)**2)/el
         dr(1,3) = area
         dr(1,1) = 0.0D0
         dr(1,2) = el
         dr(2,1) = 0.0D0
         dr(2,2) = 0.0D0
      ENDIF
!
!     LOOP  ON  SUBELEMENTS  (ONE FOR MOST)
!
      nel = nels(sub)
      SPAG_Loop_2_1: DO iel = 1 , nel
         spag_nextblock_1 = 1
         SPAG_DispatchLoop_1: DO
            SELECT CASE (spag_nextblock_1)
            CASE (1)
!
               IF ( sub==2 .OR. sub==3 ) THEN
               ELSEIF ( sub==4 .OR. sub==5 ) THEN
!
!     RING ELEMENTS, TRIANGLES AND QUADRILATERALS
!
                  rbar = 0.0
                  DO i = 1 , 3
                     ig = i + iel - 1
                     IF ( ig>4 ) ig = ig - 4
                     rbar = rbar + r(1,ig)
                     ip(i) = ig
                  ENDDO
                  af = rbar/3.0*pi
                  IF ( sub/=5 ) THEN
                     i1 = ip(1)
                     i2 = ip(2)
                     i3 = ip(3)
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ELSEIF ( sub==6 ) THEN
!
!     SOLID ELEMENTS
!
                  DO i = 1 , 4
                     ip(i) = i
                  ENDDO
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( sub==7 ) THEN
!
!     WEDGE
!
                  lrow = 4*iel - 4
                  DO i = 1 , 4
                     i1 = lrow + i
                     ip(i) = smap(i1)
                  ENDDO
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( sub==8 .OR. sub==9 ) THEN
!
!     HEXA1 AND HEXA2 ELEMENTS
!
                  lrow = 4*iel + 8
                  DO i = 1 , 4
                     i1 = lrow + i
                     ip(i) = smap(i1)
                  ENDDO
                  i1 = ip(1)
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( sub==10 .OR. sub==11 .OR. sub==12 .OR. sub==13 .OR. sub==14 .OR. sub==15 ) THEN
!
!     BOUNDARY HEAT CONVECTION ELEMENTS
!
                  itype = sub - 9
                  IF ( itype>7 ) EXIT SPAG_Loop_2_1
                  IF ( itype==2 .OR. itype==6 .OR. itype==7 ) THEN
                     np = 2
                     el = (r(1,2)-r(1,1))**2 + (r(2,2)-r(2,1))**2 + (r(3,2)-r(3,1))**2
                     kq(1) = af*k(1)*dsqrt(el)/3.0D0
                     kq(2) = kq(1)/2.0D0
                     kq(3) = kq(2)
                     kq(4) = kq(1)
                  ELSEIF ( itype==3 ) THEN
!
!     RING SURFACE
!
                     el = ((r(1,2)-r(1,1))**2+(r(3,2)-r(3,1))**2)
                     c(1) = pi*k(1)*dsqrt(el)/6.0D0
                     kq(1) = c(1)*(3.0D0*r(1,1)+r(1,2))
                     kq(2) = c(1)*(r(1,1)+r(1,2))
                     kq(3) = kq(2)
                     kq(4) = c(1)*(r(1,1)+3.0D0*r(1,2))
                     np = 2
                  ELSEIF ( itype==4 .OR. itype==5 ) THEN
!
!     TRIANGLES  (ALSO FOR SUBELEMENT OF QUAD)
!
                     DO i = 1 , 3
                        ig = i + iel - 1
                        IF ( ig>4 ) ig = ig - 4
                        ip(i) = ig
                     ENDDO
                     i1 = ip(1)
                     i2 = ip(2)
                     i3 = ip(3)
                     DO i = 1 , 3
                        dr(i,1) = r(i,i2) - r(i,i1)
                        dr(i,2) = r(i,i3) - r(i,i1)
                     ENDDO
                     CALL daxb(dr(i,1),dr(i,2),dr(i,3))
                     area = dsqrt(dr(1,3)**2+dr(2,3)**2+dr(3,3)**2)/12.0D0
                     IF ( itype==5 ) area = area/2.0D0
                     kq(1) = area*k(1)
                     kq(2) = kq(1)/2.0D0
                     kq(3) = kq(2)
                     kq(4) = kq(2)
                     kq(5) = kq(1)
                     kq(6) = kq(2)
                     kq(7) = kq(2)
                     kq(8) = kq(2)
                     kq(9) = kq(1)
                     np = 3
                  ELSE
                     np = 1
                     kq(1) = af*k(1)
                  ENDIF
!
!     PERFORM MATRIX MULTIPLY, FIRST GET TEMPERATURE VECTOR
!
                  DO i = 1 , np
                     ig = ip(i)
                     ltemp = sil(ig)
                     t1(i) = Uni(ltemp)
                     IF ( sil(ig+4)/=0 ) THEN
                        ltemp = sil(ig+4)
                        t1(i+4) = Uni(ltemp)
                     ELSE
                        t1(i+4) = 0.0D0
                     ENDIF
                  ENDDO
                  CALL gmmatd(kq(1),np,np,0,t1(1),np,1,0,c)
                  CALL gmmatd(kq(1),np,np,0,t1(5),np,1,0,c(5))
                  DO i = 1 , np
                     ig = ip(i)
                     ipg = sil(ig)
                     Delta(ipg) = Delta(ipg) + c(i) - c(i+4)
                     ig = ig + 4
                     IF ( sil(ig)>0 ) THEN
                        ipg = sil(ig)
                        Delta(ipg) = Delta(ipg) + c(i+4) - c(i)
                     ENDIF
                  ENDDO
                  CYCLE
               ELSE
!
!     RODS,BARS, ETC.
!
                  c(1) = 1.0D0
                  c(2) = -1.0D0
                  el = (r(1,2)-r(1,1))**2 + (r(2,2)-r(2,1))**2 + (r(3,2)-r(3,1))**2
                  el = dsqrt(el)
                  kq(1) = af*k(1)/el
                  ip(1) = 1
                  ip(2) = 2
                  np = 2
                  nq = 1
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               j = 1
               i1 = 1
               i2 = 2
               i3 = 3
               IF ( iel==2 .OR. iel==4 ) i3 = 4
               ip(1) = 1
               ip(2) = 2
               ip(3) = 3
               IF ( iel/=1 ) THEN
                  ip(3) = 4
                  IF ( iel/=2 ) THEN
                     j = 3
                     ip(1) = 3
                     ip(2) = 4
                     ip(3) = 1
                     IF ( iel/=3 ) ip(3) = 2
                  ENDIF
               ENDIF
               DO i = 1 , 4
                  dr(1,i) = drt(j,i)
                  dr(2,i) = drt(j+1,i)
               ENDDO
               spag_nextblock_1 = 2
            CASE (2)
               area = dr(1,i1)*(dr(2,i2)-dr(2,i3)) + dr(1,i2)*(dr(2,i3)-dr(2,i1)) + dr(1,i3)*(dr(2,i1)-dr(2,i2))
!
               c(1) = (dr(2,i2)-dr(2,i3))/area
               c(2) = (dr(2,i3)-dr(2,i1))/area
               c(3) = (dr(2,i1)-dr(2,i2))/area
!
               c(4) = (dr(1,i3)-dr(1,i2))/area
               c(5) = (dr(1,i1)-dr(1,i3))/area
               c(6) = (dr(1,i2)-dr(1,i1))/area
!
               IF ( sub==3 .OR. sub==5 ) area = area/2.0D0
               DO i = 1 , 4
                  kq(i) = k(i)*area*af/2.0D0
               ENDDO
!
               np = 3
               nq = 2
               spag_nextblock_1 = 4
            CASE (3)
               DO i = 1 , 3
                  ig = ip(i+1)
                  DO j = 1 , 3
                     dr(j,i) = r(j,ig) - r(j,i1)
                  ENDDO
               ENDDO
!
!     COMPUTE INVERSE AND BRING ALONG THE DETERMINANT FROM INVERD.
!
               ising = 0
               CALL inverd(3,dr(1,1),3,c(1),0,determ,ising,c(5))
               DO i = 1 , 3
                  ig = 4*i - 4
                  c(ig+1) = 0.0D0
                  DO j = 2 , 4
                     i1 = ig + j
                     c(i1) = dr(j-1,i)
                     c(ig+1) = c(ig+1) - c(i1)
                  ENDDO
               ENDDO
               fact = determ/6.0D0
               IF ( sub==9 ) fact = fact/2.0D0
               DO i = 1 , 9
                  kq(i) = k(i)*fact
               ENDDO
               np = 4
               nq = 3
               spag_nextblock_1 = 4
            CASE (4)
!
!     PERFORM  MATRIX MULTPLIES FOR EACH SUBELEMENT
!                               T
!                       DP  =  C  K  C * T1
!
               DO i = 1 , np
                  ig = ip(i)
                  ltemp = sil(ig)
                  t1(i) = Uni(ltemp)
                  sindx(i) = sil(ig)
               ENDDO
               CALL gmmatd(c,nq,np,0,t1,np,1,0,drtemp)
               CALL gmmatd(kq,nq,nq,0,drtemp,nq,1,0,drtemp(1,3))
               CALL gmmatd(c,nq,np,1,drtemp(1,3),nq,1,0,kq(1))
               DO i = 1 , np
                  ig = sindx(i)
                  Delta(ig) = Delta(ig) + kq(i)
               ENDDO
               EXIT SPAG_DispatchLoop_1
            END SELECT
         ENDDO SPAG_DispatchLoop_1
!
      ENDDO SPAG_Loop_2_1
   ENDDO
 100  RETURN
!
 200  j = -2
   CALL mesage(File,j,subr)
END SUBROUTINE ssght2
