
SUBROUTINE ssght2(File,Delta,Uni)
   IMPLICIT NONE
   REAL Af , Costh , Dum , Mato(6) , Matout(6) , R(3,8) , Sinth , Temp , Theta
   DOUBLE PRECISION Constd(5) , Pi
   INTEGER Elid , Imat , Inflag , Matid , Name(2) , Sil(8) , Sub
   COMMON /condad/ Constd
   COMMON /estout/ Elid , Sub , Name , Sil , Imat , Af , Theta , R , Mato
   COMMON /hmtout/ Matout
   COMMON /matin / Matid , Inflag , Temp , Dum , Sinth , Costh
   INTEGER File
   REAL Delta(1) , Uni(1)
   DOUBLE PRECISION area , c(12) , determ , dr(3,4) , drt(4,4) , drtemp(3,4) , el , fact , k(9) , kq(9) , rbar , t1(8)
   DOUBLE PRECISION dadotb
   INTEGER flag , i , i1 , i2 , i3 , i4 , iel , ig , ip(4) , ipg , ising , itype , j , l , lrow , ltemp , m , nel , nels(15) , np , &
         & npts(15) , nq , sindx(4) , smap(52) , subr(2)
   REAL xpts
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
      CALL read(*200,*100,File,Elid,45,0,flag)
!
!     CALCULATE AVERAGE ELEMENT TEMPERATURE
!
      np = npts(Sub)
      xpts = float(np)
      IF ( Sub>9 ) xpts = xpts*2.0
!
      Temp = 0.0
      DO i = 1 , np
         ltemp = Sil(i)
         Temp = Temp + Uni(ltemp)
         IF ( Sub>9 ) THEN
            IF ( Sil(i+4)/=0 ) THEN
               ltemp = Sil(i+4)
               Temp = Temp + Uni(ltemp)
            ENDIF
         ENDIF
      ENDDO
      Temp = Temp/xpts
!
!     SET UP CALL TO MATERIAL SUBROUTINE
!
      Inflag = 1
      IF ( Sub>=2 .AND. Sub<=5 ) Inflag = 2
      IF ( Sub>=6 .AND. Sub<=9 ) Inflag = 3
      Sinth = 0.0
      Costh = 1.0
      IF ( Theta/=0.0 .AND. Inflag==2 ) THEN
         Sinth = sin(Theta)
         Costh = cos(Theta)
      ENDIF
      Matid = Imat
      CALL hmat(Elid)
!
!     SUBTRACT  CONDUCTIVITY AT INITIAL TEMPERATURE AND PLACE IN  MATRIX
!
      IF ( Inflag==2 ) THEN
         k(1) = Matout(1) - Mato(1)
         k(2) = Matout(2) - Mato(2)
         k(3) = k(2)
         k(4) = Matout(3) - Mato(3)
      ELSEIF ( Inflag==3 ) THEN
         k(1) = Matout(1) - Mato(1)
         k(2) = Matout(2) - Mato(2)
         k(3) = Matout(3) - Mato(3)
         k(4) = k(2)
         k(5) = Matout(4) - Mato(4)
         k(6) = Matout(5) - Mato(5)
         k(7) = k(3)
         k(8) = k(6)
         k(9) = Matout(6) - Mato(6)
      ELSE
         k(1) = Matout(1) - Mato(1)
      ENDIF
      ip(1) = 1
      ip(2) = 2
      ip(3) = 3
      IF ( Sub==3 .OR. Sub==5 ) THEN
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
               dr(i,1) = R(i,i2) - R(i,i1)
               dr(i,3) = R(i,i3) - R(i,i1)
               dr(i,2) = R(i,i4) - R(i,i2)
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
               dr(i,4) = R(i,i4) - R(i,i1)
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
      ELSEIF ( Sub==2 .OR. Sub==4 ) THEN
!
!     MOVE  TRIANGLES TO ELEMENT COORDINATES
!
         DO i = 1 , 3
            dr(i,1) = R(i,2) - R(i,1)
            dr(i,2) = R(i,3) - R(i,1)
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
      nel = nels(Sub)
      DO iel = 1 , nel
!
         IF ( Sub==2 .OR. Sub==3 ) THEN
         ELSEIF ( Sub==4 .OR. Sub==5 ) THEN
!
!     RING ELEMENTS, TRIANGLES AND QUADRILATERALS
!
            rbar = 0.0
            DO i = 1 , 3
               ig = i + iel - 1
               IF ( ig>4 ) ig = ig - 4
               rbar = rbar + R(1,ig)
               ip(i) = ig
            ENDDO
            Af = rbar/3.0*Pi
            IF ( Sub/=5 ) THEN
               i1 = ip(1)
               i2 = ip(2)
               i3 = ip(3)
               GOTO 20
            ENDIF
         ELSEIF ( Sub==6 ) THEN
!
!     SOLID ELEMENTS
!
            DO i = 1 , 4
               ip(i) = i
            ENDDO
            GOTO 40
         ELSEIF ( Sub==7 ) THEN
!
!     WEDGE
!
            lrow = 4*iel - 4
            DO i = 1 , 4
               i1 = lrow + i
               ip(i) = smap(i1)
            ENDDO
            GOTO 40
         ELSEIF ( Sub==8 .OR. Sub==9 ) THEN
!
!     HEXA1 AND HEXA2 ELEMENTS
!
            lrow = 4*iel + 8
            DO i = 1 , 4
               i1 = lrow + i
               ip(i) = smap(i1)
            ENDDO
            i1 = ip(1)
            GOTO 40
         ELSEIF ( Sub==10 .OR. Sub==11 .OR. Sub==12 .OR. Sub==13 .OR. Sub==14 .OR. Sub==15 ) THEN
!
!     BOUNDARY HEAT CONVECTION ELEMENTS
!
            itype = Sub - 9
            IF ( itype>7 ) EXIT
            IF ( itype==2 .OR. itype==6 .OR. itype==7 ) THEN
               np = 2
               el = (R(1,2)-R(1,1))**2 + (R(2,2)-R(2,1))**2 + (R(3,2)-R(3,1))**2
               kq(1) = Af*k(1)*dsqrt(el)/3.0D0
               kq(2) = kq(1)/2.0D0
               kq(3) = kq(2)
               kq(4) = kq(1)
            ELSEIF ( itype==3 ) THEN
!
!     RING SURFACE
!
               el = ((R(1,2)-R(1,1))**2+(R(3,2)-R(3,1))**2)
               c(1) = Pi*k(1)*dsqrt(el)/6.0D0
               kq(1) = c(1)*(3.0D0*R(1,1)+R(1,2))
               kq(2) = c(1)*(R(1,1)+R(1,2))
               kq(3) = kq(2)
               kq(4) = c(1)*(R(1,1)+3.0D0*R(1,2))
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
                  dr(i,1) = R(i,i2) - R(i,i1)
                  dr(i,2) = R(i,i3) - R(i,i1)
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
               kq(1) = Af*k(1)
            ENDIF
!
!     PERFORM MATRIX MULTIPLY, FIRST GET TEMPERATURE VECTOR
!
            DO i = 1 , np
               ig = ip(i)
               ltemp = Sil(ig)
               t1(i) = Uni(ltemp)
               IF ( Sil(ig+4)/=0 ) THEN
                  ltemp = Sil(ig+4)
                  t1(i+4) = Uni(ltemp)
               ELSE
                  t1(i+4) = 0.0D0
               ENDIF
            ENDDO
            CALL gmmatd(kq(1),np,np,0,t1(1),np,1,0,c)
            CALL gmmatd(kq(1),np,np,0,t1(5),np,1,0,c(5))
            DO i = 1 , np
               ig = ip(i)
               ipg = Sil(ig)
               Delta(ipg) = Delta(ipg) + c(i) - c(i+4)
               ig = ig + 4
               IF ( Sil(ig)>0 ) THEN
                  ipg = Sil(ig)
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
            el = (R(1,2)-R(1,1))**2 + (R(2,2)-R(2,1))**2 + (R(3,2)-R(3,1))**2
            el = dsqrt(el)
            kq(1) = Af*k(1)/el
            ip(1) = 1
            ip(2) = 2
            np = 2
            nq = 1
            GOTO 60
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
 20      area = dr(1,i1)*(dr(2,i2)-dr(2,i3)) + dr(1,i2)*(dr(2,i3)-dr(2,i1)) + dr(1,i3)*(dr(2,i1)-dr(2,i2))
!
         c(1) = (dr(2,i2)-dr(2,i3))/area
         c(2) = (dr(2,i3)-dr(2,i1))/area
         c(3) = (dr(2,i1)-dr(2,i2))/area
!
         c(4) = (dr(1,i3)-dr(1,i2))/area
         c(5) = (dr(1,i1)-dr(1,i3))/area
         c(6) = (dr(1,i2)-dr(1,i1))/area
!
         IF ( Sub==3 .OR. Sub==5 ) area = area/2.0D0
         DO i = 1 , 4
            kq(i) = k(i)*area*Af/2.0D0
         ENDDO
!
         np = 3
         nq = 2
         GOTO 60
 40      DO i = 1 , 3
            ig = ip(i+1)
            DO j = 1 , 3
               dr(j,i) = R(j,ig) - R(j,i1)
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
         IF ( Sub==9 ) fact = fact/2.0D0
         DO i = 1 , 9
            kq(i) = k(i)*fact
         ENDDO
         np = 4
         nq = 3
!
!     PERFORM  MATRIX MULTPLIES FOR EACH SUBELEMENT
!                               T
!                       DP  =  C  K  C * T1
!
 60      DO i = 1 , np
            ig = ip(i)
            ltemp = Sil(ig)
            t1(i) = Uni(ltemp)
            sindx(i) = Sil(ig)
         ENDDO
         CALL gmmatd(c,nq,np,0,t1,np,1,0,drtemp)
         CALL gmmatd(kq,nq,nq,0,drtemp,nq,1,0,drtemp(1,3))
         CALL gmmatd(c,nq,np,1,drtemp(1,3),nq,1,0,kq(1))
         DO i = 1 , np
            ig = sindx(i)
            Delta(ig) = Delta(ig) + kq(i)
         ENDDO
!
      ENDDO
   ENDDO
 100  RETURN
!
 200  j = -2
   CALL mesage(File,j,subr)
END SUBROUTINE ssght2