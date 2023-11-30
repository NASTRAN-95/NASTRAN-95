
SUBROUTINE em2d(Itype,Istart,Jtype,Ncount,Ido,Iwords,Nbdys,All,Nelout)
   IMPLICIT NONE
   REAL Costh , Ecpt(200) , Eltemp , Sinth , Stress , Xmat(6) , Z(1)
   INTEGER Ibuf , Idum(78) , Inflag , Iz(1) , Matid , Necpt(10) , Nrowsp , Otpe
   CHARACTER*23 Ufm
   COMMON /blank / Nrowsp
   COMMON /emecpt/ Ecpt
   COMMON /hmtout/ Xmat
   COMMON /matin / Matid , Inflag , Eltemp , Stress , Sinth , Costh
   COMMON /system/ Ibuf , Otpe , Idum
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
   INTEGER All , Ido , Istart , Itype , Iwords , Jtype , Nbdys , Ncount , Nelout
   REAL ahcx , ahcy , ahcz , angle , area , buf(50) , cs , csq , d , d12(3) , d13(3) , ddnl(24) , ddnlb(24) , determ , dn(8) ,      &
      & dnc(16) , dneta(1) , dnl(16) , dnx(1) , dnxi(1) , dny(1) , dumarg , dxx(3) , et(9) , eta(8) , f(8) , g(9) , g1 , g2 , g3 ,  &
      & gh(3) , h(3) , h1 , h2 , h3 , hc(3) , hc1 , hc2 , hc3 , hci(24) , hcx(3) , hcxyz(3) , hcy(3) , hcz(3) , hl , l(3,4) ,       &
      & pt(3) , r(3,8) , sc(5) , sfact , ssq , temp , twopi3 , vec(3) , vol , vvec(3) , w(4) , x2 , x3 , xi(8) , xjb(4) , xlacc(3) ,&
      & xload(3) , xmul , xn(18) , xng(9) , xx , xxc , xxjb(2,2) , xz(16) , y3 , yy , yyc , z14(3) , zi(3) , zj(3) , zjlen , zk(3) ,&
      & zklen , zlen
   INTEGER i , i1 , i2 , i3 , ia , iel , iii , ijk , inip , ip(3) , ipi , ipt , isc(5) , isc2 , isil , ising , isub , isubi , isys ,&
         & itemp , ith , iws(2,3) , ixx , j , jbuf(50) , jel , jjj , k , ktype , m , mid , n , nam(2) , nel , ng , ngrids , nopts , &
         & noptsp , npts , nsil , pointr(9,9) , scr6 , typold
   LOGICAL onlyc
   REAL zz , zzc
!
!     COMPUTES ADDITIONAL E AND M LOADS FOR TWO DIMENSIONAL ELEMENTS
!
!     THIS ROUTINE HANDLES THE FOLLOWING 2-D ELEMENTS
!
!     TRIA1 -6-   TRMEM -9-   QDMEM-16-  TRIA2-17-  QUAD2-18-  QUAD1-19-
!     TRIARG-36-  TRAPRG-37   IS2D8-80-
!
   EQUIVALENCE (buf(1),jbuf(1)) , (sc(1),isc(1)) , (Z(1),Iz(1)) , (Ecpt(1),Necpt(1)) , (i1,ip(1)) , (i2,ip(2)) , (i3,ip(3)) ,       &
    & (dnc(1),dnxi(1)) , (dnc(9),dneta(1)) , (dnl(1),dnx(1)) , (dnl(9),dny(1))
   DATA xi/ - 1. , 1. , 1. , -1. , 0. , 1. , 0. , -1./
   DATA eta/ - 1. , -1. , 1. , 1. , -1. , 0. , 1. , 0./
   DATA twopi3/2.094395103/
   DATA nam/4HEM2D , 4H    /
   DATA typold/0/ , scr6/306/
!
!     EST STARTING POINTERS
!
!     ISIL   = 1ST SIL NUMBER
!     ITH    = MATERIAL ANGLE
!     MID    = MATERIAL ID
!     IA     = AREA FACTOR (TO COMPUTE VOLUME)
!     ISYS   = 1ST OUTPUT CORRDINATE SYSYTEM NUMBER
!     NGRIDS = NUMBER OF GRID POINTS
!     ITEMP  = ELEMENT TEMPERATURE
!     NEL    = NUMBER OF TRIANGLES USED TO FORM ELEMENT
!
!              ITYPE ISIL ITH MID IA ISYS NGRIDS ITEMP NEL
!
   DATA pointr/6 , 2 , 5 , 6 , 7 , 15 , 3 , 27 , 1 , 9 , 2 , 5 , 6 , 7 , 9 , 3 , 21 , 1 , 16 , 2 , 6 , 7 , 8 , 10 , 4 , 26 , 4 ,    &
      & 17 , 2 , 5 , 6 , 7 , 9 , 3 , 21 , 1 , 18 , 2 , 6 , 7 , 8 , 10 , 4 , 26 , 4 , 19 , 2 , 6 , 7 , 8 , 16 , 4 , 32 , 4 , 36 , 2 ,&
      & 5 , 6 , 0 , 7 , 3 , 19 , 1 , 37 , 2 , 6 , 7 , 0 , 8 , 4 , 24 , 4 , 80 , 2 , 11 , 12 , 13 , 14 , 8 , 46 , 1/
!
   onlyc = .FALSE.
   IF ( Itype/=80 ) THEN
      l(1,1) = 1./3.
      l(2,1) = l(1,1)
      l(3,1) = l(1,1)
      l(1,2) = .6
      l(2,2) = .2
      l(3,2) = .2
      l(1,3) = .2
      l(2,3) = .6
      l(3,3) = .2
      l(1,4) = .2
      l(2,4) = .2
      l(3,4) = .6
      w(1) = -27./48.
      w(2) = 25./48.
      w(3) = w(2)
      w(4) = w(2)
      nopts = 4
   ENDIF
   isc(1) = Necpt(1)
   isc(2) = 1
   IF ( Itype==80 ) isc(2) = 9
!
!     FIND ELEMENT TYPE TO PICK UP POINTERS
!
   IF ( Itype==typold ) GOTO 200
   typold = Itype
   DO i = 1 , 9
      jel = i
      IF ( Itype<pointr(1,i) ) EXIT
      IF ( Itype==pointr(1,i) ) GOTO 100
   ENDDO
!
   WRITE (Otpe,99001) Ufm , nam , Itype
99001 FORMAT (A23,', IN SUBROUTINE',2A4,' ELEMENT TYPE',I8,' IS NOT ','LEGAL')
   CALL mesage(-61,0,0)
   GOTO 99999
!
 100  isil = pointr(2,jel)
   ith = pointr(3,jel)
   mid = pointr(4,jel)
   ia = pointr(5,jel)
   isys = pointr(6,jel)
   ngrids = pointr(7,jel)
   itemp = pointr(8,jel)
   nel = pointr(9,jel)
!
!     CHECK TO SEE IF THIS ELEMENT CONTAINS A GRID POINT ON A PERMBDY
!     CARD. IF SO, OR IF NO PERMBDY CARD EXISTS, COMPUTE LOADS FOR THE
!     ELEMENT. IF NOT, COMPUTE HC CENTROIDAL VALUE ONLY. (ONLYC=.TRUE.)
!     THE PERMBDY SILS START AT Z(ISTART-NBDYS-1)
!
 200  IF ( Nbdys/=0 ) THEN
!
      DO i = 1 , ngrids
         ng = Necpt(isil+i-1)
         DO j = 1 , Nbdys
            IF ( ng==Iz(Istart-Nbdys-Nelout+j-1) ) GOTO 300
         ENDDO
      ENDDO
!
!     ELEMENT HAS NO GRIDS ON PERMBDY
!
      onlyc = .TRUE.
      nopts = 0
   ENDIF
 300  IF ( onlyc .AND. Jtype==24 ) RETURN
!
!     IF ONLYC=TRUE, CHECK TO SEE IF THE ELEMENT HAD AN ELFORCE REQUEST.
!     IF SO, CONTINUE. IF NOT, JUST WRITE ZEROS TO HCCEN,SCR6) AND
!     RETURN.
!
   IF ( onlyc ) THEN
      IF ( All/=1 ) THEN
         IF ( Nelout/=0 ) THEN
!
            DO i = 1 , Nelout
               IF ( Necpt(1)==Iz(Istart-Nelout+i-1) ) GOTO 400
            ENDDO
         ENDIF
         GOTO 500
      ENDIF
   ENDIF
!
!     CHECK FOR ZERO LOAD
!
 400  IF ( Jtype/=20 .AND. Jtype/=24 ) GOTO 600
   h1 = 0.
   h2 = 0.
   h3 = 0.
   g1 = 0.
   g2 = 0.
   g3 = 0.
   DO i = 1 , ngrids
      isub = Istart + 3*Necpt(isil+i-1) - 3
      IF ( Jtype==24 ) isub = Istart + 3*Ncount - 3
      h1 = h1 + abs(Z(isub))
      h2 = h2 + abs(Z(isub+1))
      h3 = h3 + abs(Z(isub+2))
      g1 = g1 + Z(isub)
      g2 = g2 + Z(isub+1)
      g3 = g3 + Z(isub+2)
      IF ( Jtype==24 ) EXIT
   ENDDO
   hl = h1 + h2 + h3
   IF ( hl/=0. ) THEN
!
      IF ( Jtype/=24 ) THEN
!
!     AVERAGE SPCFLD
!
         ahcx = g1/float(ngrids)
         ahcy = g2/float(ngrids)
         ahcz = g3/float(ngrids)
      ENDIF
      GOTO 600
   ELSEIF ( Jtype==24 ) THEN
      RETURN
   ENDIF
!
!     ALL ZEROS - WRITE TO SCR6
!
 500  sc(3) = 0.
   sc(4) = 0.
   sc(5) = 0.
   CALL write(scr6,sc,2,0)
   isc2 = isc(2)
   DO i = 1 , isc2
      CALL write(scr6,sc(3),3,0)
   ENDDO
   RETURN
!
 600  IF ( onlyc ) GOTO 900
!
!     PICK UP MATERIAL INFO
!     INFLAG = 3 MEANS A 3 X 3 MATERIAL MATRIX WILL BE RETURNED. THE
!     REASON FOR DOING THIS FOR A 2-D ELEMENT IS THAT HC CAN HAVE A
!     COMPONENT NORMAL TO THE PLANE OF THE ELEMENT. PARTIAL DERIVATIVE
!     W.R.T Z IS 0.  BUT IF THE MATERIAL IS ANISOTROPIC, THEN A
!     CONTRIBUTION TO THE SCALAR LOAD IS POSSIBLE IF MATERIAL CONTAINS
!     A NON-ZERO X-Z TERM. FOR ISOTROPIC MATERIALS, THE NORMAL COMPONENT
!     OF HC WILL BE IGNORED W.R.T ITS CONTRIBUTION TO THE LOAD. IF ALL
!     TERMS OF MATERIAL MATRIX W.R.T.Z ARE 0, AND IF ANISOTROPIC ANGLE
!     IS NOT 0, THEN WE MUST TRANSFORM MATERIALS TO ELEMENT SYSTEM HERE.
!
   Inflag = 3
   IF ( Jtype==24 ) GOTO 800
   Matid = Necpt(mid)
   Eltemp = Ecpt(itemp)
   angle = Ecpt(ith)*0.017453293
   Sinth = sin(angle)
   Costh = cos(angle)
   CALL hmat(Necpt(1))
!
!     CHECK FOR 3-D ANISOTROPY
!
   IF ( Xmat(3)==0. .AND. Xmat(5)==0. ) THEN
!
!     CHECK FOR 2-D ANISOTROPY
!
      IF ( abs(angle)>.0001 ) THEN
!
!     2-D ANISOTROPY
!
         csq = Costh*Costh
         ssq = Sinth*Sinth
         cs = Costh*Sinth
         g(1) = csq*Xmat(1) - 2.*cs*Xmat(2) + ssq*Xmat(4)
         g(2) = cs*(Xmat(1)-Xmat(4)) + (csq-ssq)*Xmat(2)
         g(3) = 0.
         g(5) = ssq*Xmat(1) + 2.*cs*Xmat(2) + csq*Xmat(4)
         g(6) = 0.
         g(9) = Xmat(6)
         GOTO 700
      ENDIF
   ENDIF
!
   g(1) = Xmat(1)
   g(2) = Xmat(2)
   g(3) = Xmat(3)
   g(5) = Xmat(4)
   g(6) = Xmat(5)
   g(9) = Xmat(6)
!
 700  IF ( Itype==36 .OR. Itype==37 ) THEN
!
!     SWITCH Y-Z MATERIALS FOR TRAPRG AND TRIARG
!
      temp = g(5)
      g(5) = g(9)
      g(9) = temp
      temp = g(2)
      g(2) = g(3)
      g(3) = temp
   ENDIF
!
!     FILL IN SYMMETRIC PART
!
   g(4) = g(2)
   g(7) = g(3)
   g(8) = g(6)
!
!     SINCE QUADRILATERALS ARE COVERED BY 4 OVERLAPPING TRIANGLES,
!     MUST DIVIDE QUAD RESULTS BY 2
!
 800  xmul = 1.
   IF ( ngrids==4 ) xmul = .5
!
!     PICK UP COORDINATES OF GRID POINTS
!
   DO i = 1 , ngrids
      isubi = isys + 4*i - 4
      DO j = 1 , 3
         isub = isubi + j
         r(j,i) = Ecpt(isub)
      ENDDO
   ENDDO
 900  IF ( Itype==80 ) THEN
!
!     IS2D8
!
!     SET UP QUADRATURE POINTS AND WEIGHTS
!
      IF ( .NOT.(onlyc) ) THEN
         pt(1) = -0.57735027
         pt(2) = -pt(1)
         h(1) = 1.
         h(2) = 1.
         IF ( Necpt(10)/=2 ) THEN
            pt(1) = -0.77459667
            pt(2) = 0.
            pt(3) = -pt(1)
            h(1) = 5./9.
            h(2) = 8./9.
            h(3) = h(1)
         ENDIF
!
!     COMPUTE I,J,K VECTORS- I IS 1 TO 2
!
         DO i = 1 , 3
            zi(i) = r(i,2) - r(i,1)
            z14(i) = r(i,4) - r(i,1)
         ENDDO
         zlen = sqrt(zi(1)**2+zi(2)**2+zi(3)**2)
         DO i = 1 , 3
            zi(i) = zi(i)/zlen
         ENDDO
!
!     GET K BY CROSSING I INTO VECTOR FROM 1 TO 4
!
         zk(1) = zi(2)*z14(3) - zi(3)*z14(2)
         zk(2) = zi(3)*z14(1) - zi(1)*z14(3)
         zk(3) = zi(1)*z14(2) - zi(2)*z14(1)
         zklen = sqrt(zk(1)**2+zk(2)**2+zk(3)**2)
         DO i = 1 , 3
            zk(i) = zk(i)/zklen
         ENDDO
!
!     GET J BY CROSSING K INTO I AND STORE INTO TRANSFORMATION MATRIX
!
         zj(1) = zk(2)*zi(3) - zk(3)*zi(2)
         zj(2) = zk(3)*zi(1) - zk(1)*zi(3)
         zj(3) = zk(1)*zi(2) - zk(2)*zi(1)
         zjlen = sqrt(zj(1)**2+zj(2)**2+zj(3)**2)
         DO i = 1 , 3
            zj(i) = zj(i)/zjlen
         ENDDO
!
         DO i = 1 , 3
            et(i) = zi(i)
            et(i+3) = zj(i)
            et(i+6) = zk(i)
         ENDDO
!
!     COMPUTE ELMENT COORDS FOR 1 AND 2
!
         xz(1) = 0.
         xz(2) = 0.
         xz(3) = zlen
         xz(4) = 0.
!
!     FOR 3-8, X IS DOT PRODUCT OF VECTOR FROM 1 TO GRID WITH I.
!     Y IS THE LENFTH OF THE VECTOR RESULTING FROM CROSSING I INTO
!     VECTOR FROM 1 TO GRID
!
         DO i = 3 , 8
            ixx = 2*i - 1
            DO j = 1 , 3
               vec(j) = r(j,i) - r(j,1)
            ENDDO
            xz(ixx) = vec(1)*zi(1) + vec(2)*zi(2) + vec(3)*zi(3)
            vvec(1) = zi(2)*vec(3) - zi(3)*vec(2)
            vvec(2) = zi(3)*vec(1) - zi(1)*vec(3)
            vvec(3) = zi(1)*vec(2) - zi(2)*vec(1)
            xz(ixx+1) = sqrt(vvec(1)**2+vvec(2)**2+vvec(3)**2)
         ENDDO
!
         DO i = 1 , 8
            f(i) = 0.
         ENDDO
!
!     GET HC AT EACH GRID
!
         IF ( Jtype==24 ) THEN
!
!     REMFLUX
!
            isub = Istart + 3*Ncount - 3
            gh(1) = Z(isub)
            gh(2) = Z(isub+1)
            gh(3) = Z(isub+2)
            GOTO 1000
         ENDIF
      ENDIF
!
!     IF SPCFLD, PICK UP GRID VALUES HERE. IF NOT, PICK UP INTEGRATION
!     POINT VALUES LATER
!
      IF ( Jtype==20 ) THEN
         DO i = 1 , ngrids
            isil = 3*Necpt(i+1)
            hci(3*i-2) = Z(Istart+isil-3)
            hci(3*i-1) = Z(Istart+isil-2)
            hci(3*i) = Z(Istart+isil-1)
         ENDDO
      ENDIF
   ELSE
!
!     COMPUTE COORDINATES OF CENTROID (OR, AT LEAST, AVERAGE ELEMENT
!     COORDS)
!
      xxc = 0.
      yyc = 0.
      zzc = 0.
      DO i = 1 , ngrids
         xxc = xxc + r(1,i)
         yyc = yyc + r(2,i)
         zzc = zzc + r(3,i)
      ENDDO
      xxc = xxc/float(ngrids)
      yyc = yyc/float(ngrids)
      zzc = zzc/float(ngrids)
!
!     NOW COMPUTE PROPER LOADS FOR EACH TRIANGLE
!
      DO iel = 1 , nel
         IF ( .NOT.(onlyc) ) THEN
!
!     1ST SET UP AN ARRAY TO PICK UP GRID POINTS IN A PARTICULAR ORDER.
!     FOR TRIANGLES, IT IS 1,2,3. FOR QUADRILATERALS, FORM 4 TRIANGLES
!     BY TAKING GRIDS 1,2,3, 2,3,4, 3,4,1, AND 4,1,2
!
            DO i = 1 , 3
               ip(i) = i + iel - 1
               IF ( ip(i)>4 ) ip(i) = ip(i) - 4
            ENDDO
!
!     COMPUTE VECTORS FROM 1ST GRID TO 2ND AND FROM 1ST TO 3RD
!
            DO i = 1 , 3
               d12(i) = r(i,i2) - r(i,i1)
               d13(i) = r(i,i3) - r(i,i1)
            ENDDO
!
!     SET UP GRADIENTS FOR AXISYMMETRIC ELEMENTS SEPARATELY
!
            IF ( Itype/=36 .AND. Itype/=37 ) THEN
!
!     FIRST, CONVERT COORDINATES TO ELEMNT COORDINATE SYSTEM
!
               zlen = sqrt(d12(1)**2+d12(2)**2+d12(3)**2)
               DO i = 1 , 3
                  zi(i) = d12(i)/zlen
               ENDDO
!
               CALL saxb(zi(1),d13(1),dxx(1))
!
               x2 = zlen
               x3 = d13(1)*zi(1) + d13(2)*zi(2) + d13(3)*zi(3)
               y3 = sqrt(dxx(1)**2+dxx(2)**2+dxx(3)**2)
!
               area = .5*x2*y3
               vol = area*Ecpt(ia)
!
!     GET J AND K VECTORS FOR LATER USE
!
               DO i = 1 , 3
                  zk(i) = dxx(i)/y3
               ENDDO
!
               CALL saxb(zk(1),zi(1),zj(1))
               zlen = sqrt(zj(1)**2+zj(2)**2+zj(3)**2)
               DO i = 1 , 3
                  zj(i) = zj(i)/zlen
               ENDDO
               DO i = 1 , 3
                  et(i) = zi(i)
                  et(i+3) = zj(i)
                  et(i+6) = zk(i)
               ENDDO
!
!     SHAPE FUNCTION GRADIENTS
!
               xn(1) = -1./x2
               xn(2) = (x3-x2)/(x2*y3)
               xn(3) = 0.
               xn(4) = -xn(1)
               xn(5) = -x3/(x2*y3)
               xn(6) = 0.
               xn(7) = 0.
               xn(8) = 1./y3
               xn(9) = 0.
!
!     TRANSFORM SHAPE FN GRADIENTS FROM LOCAL TO BASIC
!
               CALL gmmats(et,3,3,1,xn(1),3,3,1,xng(1))
!
!     FOR ALL EXCEPT REMFLUX, MULT. GRADIENTS OF SHAPE FNS INTO
!     MATERIALS
!
               IF ( Jtype==24 ) THEN
                  xn(1) = xng(1)
                  xn(2) = xng(4)
                  xn(3) = xng(7)
                  xn(4) = xng(2)
                  xn(5) = xng(5)
                  xn(6) = xng(8)
                  xn(7) = xng(3)
                  xn(8) = xng(6)
                  xn(9) = xng(9)
               ELSE
                  CALL gmmats(xng(1),3,3,1,g,3,3,0,xn(10))
               ENDIF
            ELSE
!
!     THE LENGTH OF THE CROSS PRODUCT VECTOR IS TWICE THE AREA OF THE
!     TRIANG
!
               CALL saxb(d12(1),d13(1),d12(1))
               area = .5*sqrt(d12(1)**2+d12(2)**2+d12(3)**2)
               vol = area*twopi3*(r(1,i1)+r(1,i2)+r(1,i3))
!
!     NOW SET UP GRADIENT OF THE SHAPE FUNCTION AT EACH GRID POINT.
!     SET UP A 3 X3 MATRIX ROW-STORED FOR GMMATS
!
               d = (r(1,i2)-r(1,i1))*r(3,i3) + (r(1,i1)-r(1,i3))*r(3,i2) + (r(1,i3)-r(1,i2))*r(3,i1)
               xn(1) = r(3,i2) - r(3,i3)
               xn(2) = 0.
               xn(3) = r(1,i3) - r(1,i2)
               xn(4) = r(3,i3) - r(3,i1)
               xn(5) = 0.
               xn(6) = r(1,i1) - r(1,i3)
               xn(7) = r(3,i1) - r(3,i2)
               xn(8) = 0.
               xn(9) = r(1,i2) - r(1,i1)
!
               DO i = 1 , 9
                  xn(i) = xn(i)/d
               ENDDO
!
!     FOR ALL EXCEPT REMFLUX, MULT. GRADIENTS INTO MATERIALS
!
               IF ( Jtype/=24 ) CALL gmmats(xn(1),3,3,0,g,3,3,0,xn(10))
            ENDIF
            IF ( Jtype==24 ) THEN
!
!     REMFLUX
!
               ipt = Istart + 3*Ncount - 3
               hc(1) = Z(ipt)
               hc(2) = Z(ipt+1)
               hc(3) = Z(ipt+2)
               GOTO 940
            ENDIF
         ENDIF
!
!     START INTEGRATION PROCEDURE- 4 POINTS FOR CUBIC PLUS ONE AT
!     CENTROID
!
         ktype = Jtype - 19
         xlacc(1) = 0.
         xlacc(2) = 0.
         xlacc(3) = 0.
         noptsp = nopts + 1
         DO npts = 1 , noptsp
!
!     DO CENTROID FOR ONLY 1ST TRIANGLE
!
            IF ( npts==noptsp .AND. iel>1 ) CYCLE
!
!     COMPUTE BASIC COORDS OF INTEGRATION POINT
!
            IF ( npts/=noptsp ) THEN
               xx = l(1,npts)*r(1,i1) + l(2,npts)*r(1,i2) + l(3,npts)*r(1,i3)
               yy = l(1,npts)*r(2,i1) + l(2,npts)*r(2,i2) + l(3,npts)*r(2,i3)
               zz = l(1,npts)*r(3,i1) + l(2,npts)*r(3,i2) + l(3,npts)*r(3,i3)
            ELSE
!
!     CENTROID
!
               xx = xxc
               yy = yyc
               zz = zzc
               IF ( Jtype==20 ) THEN
!
!     AVERAGE SPCFLD
!
                  hc(1) = ahcx
                  hc(2) = ahcy
                  hc(3) = ahcz
                  GOTO 920
               ENDIF
            ENDIF
            hc(1) = 0.
            hc(2) = 0.
            hc(3) = 0.
!
!     COMPUTE HC AT THIS POINT FOR ALL LOADS OF THIS TYPE
!
            DO ijk = 1 , Ido
               IF ( Jtype/=20 ) THEN
                  isub = Istart + (ijk-1)*Iwords - 1
                  DO i = 1 , Iwords
                     buf(i) = Z(isub+i)
                  ENDDO
                  IF ( ktype==2 ) THEN
!
!     CEMLOOP, GEMLOOP, MDIPOLE
!
                     CALL axloop(buf,jbuf,xx,yy,zz,hc1,hc2,hc3)
                  ELSEIF ( ktype==3 ) THEN
                     CALL geloop(buf,jbuf,xx,yy,zz,hc1,hc2,hc3)
                  ELSEIF ( ktype==4 ) THEN
                     CALL dipole(buf,jbuf,xx,yy,zz,hc1,hc2,hc3)
                  ELSE
                     GOTO 905
                  ENDIF
                  GOTO 910
               ENDIF
 905           DO i = 1 , 3
                  ipi = ip(i)
                  nsil = Necpt(isil+ipi-1)
                  ipt = Istart + 3*nsil - 3
                  hcx(i) = Z(ipt)
                  hcy(i) = Z(ipt+1)
                  hcz(i) = Z(ipt+2)
               ENDDO
               hc1 = l(1,npts)*hcx(1) + l(2,npts)*hcx(2) + l(3,npts)*hcx(3)
               hc2 = l(1,npts)*hcy(1) + l(2,npts)*hcy(2) + l(3,npts)*hcy(3)
               hc3 = l(1,npts)*hcz(1) + l(2,npts)*hcz(2) + l(3,npts)*hcz(3)
 910           hc(1) = hc(1) + hc1
               hc(2) = hc(2) + hc2
               hc(3) = hc(3) + hc3
            ENDDO
!
 920        IF ( npts/=noptsp ) THEN
!
!     WE HAVE HC AT THIS INTEG. PT. MULT. BY WEIGHT AND ACCUMULATE
!
               DO i = 1 , 3
                  xlacc(i) = xlacc(i) + hc(i)*w(npts)
               ENDDO
            ELSE
               sc(3) = hc(1)
               sc(4) = hc(2)
               sc(5) = hc(3)
               CALL write(scr6,sc,5,0)
            ENDIF
!
!     GET ANOTHER INTEGRATION POINT
!
         ENDDO
!
         IF ( onlyc ) RETURN
         DO i = 1 , 3
            hc(i) = xlacc(i)
         ENDDO
!
!    TAKE XMUL MULTIPLIER INTO ACCOUNT
!
 940     DO i = 1 , 3
            hc(i) = hc(i)*xmul
         ENDDO
!
!     MAKE FINAL COMPUTATION. MULTIPLY PRODUCT OF SHAPE FUNCTION
!     GRADIENTS AND MATERIAL MATRIX INTO HC AND MULTIPLY BY VOLUME
!
         isub = 10
         IF ( Jtype==24 ) isub = 1
         CALL gmmats(xn(isub),3,3,0,hc,3,1,0,xload(1))
!
!     ADD THIS ELEMENT LOAD VECTOR IN OVERALL VECTOR. USE NSIL AND IP TO
!     POI
!
         DO j = 1 , 3
            ipi = ip(j)
            nsil = Necpt(isil+ipi-1)
!
!     IF PERMBDY EXISTS AND IF GRID IS NOT ON IT, IGNORE ITS LOAD
!
            IF ( Nbdys/=0 ) THEN
               DO i = 1 , Nbdys
                  IF ( nsil==Iz(Istart-Nbdys-Nelout+i-1) ) GOTO 950
               ENDDO
               CYCLE
            ENDIF
 950        Z(nsil) = Z(nsil) - xload(j)*vol
         ENDDO
!
!     DONE FOR THIS TRIANGLE. GO BACK FOR ANOTHER
!
      ENDDO
      RETURN
   ENDIF
 1000 inip = Necpt(10)
   ktype = Jtype - 20
   IF ( .NOT.(onlyc) ) THEN
!
!     START INTEGRATION
!
      DO iii = 1 , inip
         DO jjj = 1 , inip
!
!     COMPUTE DERIVATIVES WITH RESPECT TO XI AND ETA
!     EACH GRID POINT
!
            DO n = 1 , 4
               dn(n) = .25*(1.+pt(iii)*xi(n))*(1.+pt(jjj)*eta(n))*(pt(iii)*xi(n)+pt(jjj)*eta(n)-1.)
               dnxi(n) = .25*xi(n)*(1.+pt(jjj)*eta(n))*(2.*pt(iii)*xi(n)+pt(jjj)*eta(n))
               dneta(n) = .25*eta(n)*(1.+pt(iii)*xi(n))*(pt(iii)*xi(n)+2.*pt(jjj)*eta(n))
            ENDDO
            DO n = 5 , 7 , 2
!
               dn(n) = .5*(1.-pt(iii)*pt(iii))*(1.+pt(jjj)*eta(n))
               dnxi(n) = -pt(iii)*(1.+pt(jjj)*eta(n))
               dneta(n) = .5*(1.-pt(iii)*pt(iii))*eta(n)
            ENDDO
!
            DO n = 6 , 8 , 2
               dn(n) = .5*(1.+pt(iii)*xi(n))*(1.-pt(jjj)*pt(jjj))
               dnxi(n) = .5*xi(n)*(1.-pt(jjj)*pt(jjj))
               dneta(n) = -pt(jjj)*(1.+pt(iii)*xi(n))
            ENDDO
!
!     COMPUTE JACOBEAN
!
!           N1XI   N2XI   N3XI   N4XI   N5XI   N6XI   N7XI   N8XI
!     DNC = N1ETA  N2ETA  N3ETA  N4ETA  N5ETA  N6ETA  N7ETA  N8ETA
!
!          X1  Y1
!          X2  Y2
!          X3  Y3
!     XX = X4  Y4
!          X5  Y5
!          X6  Y6
!          X7  Y7
!          X8  Y8
!
            CALL gmmats(dnc,2,8,0,xz,8,2,0,xjb)
!
!     XJB IS ROW-STORED-IT MUST BE COLUMN-STORED AND DOUBLY DIMENSIONED
!     FOR INVERSION
!
            k = 0
            DO i = 1 , 2
               DO j = 1 , 2
                  k = k + 1
                  xxjb(i,j) = xjb(k)
               ENDDO
            ENDDO
!
!     COMPUTE INVERSE AND DETERMINANT OF JACOBEAN
!
            CALL invers(2,xxjb,2,dumarg,0,determ,ising,iws)
!
!     COMPUTE DERIVATIVES WITH RESPECT TO X AND Y
!
            k = 0
            DO i = 1 , 2
               DO j = 1 , 2
                  k = k + 1
                  xjb(k) = xxjb(i,j)
               ENDDO
            ENDDO
            CALL gmmats(xjb,2,2,0,dnc,2,8,0,dnl)
!
!           N1X N2X N3X N4X N5X N6X N7X N8X
!     DNL = N1Y N2Y N3Y N4Y N5Y N6Y N7Y N8Y
!
            IF ( Jtype/=24 ) THEN
!
!     INITIALIZE HC AT PRESENT UNTEGRATION POINT
!
               DO i = 1 , 3
                  hcxyz(i) = 0.
               ENDDO
               IF ( Jtype==20 ) THEN
!
!     SPCFLD
!
                  DO m = 1 , ngrids
                     hcxyz(1) = hcxyz(1) + dn(m)*hci(3*m-2)
                     hcxyz(2) = hcxyz(2) + dn(m)*hci(3*m-1)
                     hcxyz(3) = hcxyz(3) + dn(m)*hci(3*m)
                  ENDDO
               ELSE
!
!     FOR LOOPS AND DIPOLES, COMPITE BASIC COORDS. FOR THIS INTEGRATION
!     PT
!
                  xx = 0.
                  yy = 0.
                  zz = 0.
                  DO m = 1 , ngrids
                     xx = xx + dn(m)*r(1,m)
                     yy = yy + dn(m)*r(2,m)
                     zz = zz + dn(m)*r(3,m)
                  ENDDO
!
                  DO ijk = 1 , Ido
                     isub = Istart + (ijk-1)*Iwords - 1
                     DO m = 1 , Iwords
                        buf(m) = Z(isub+m)
                     ENDDO
                     IF ( ktype==2 ) THEN
                        CALL geloop(buf,jbuf,xx,yy,zz,hc1,hc2,hc3)
                     ELSEIF ( ktype==3 ) THEN
                        CALL dipole(buf,jbuf,xx,yy,zz,hc1,hc2,hc3)
                     ELSE
                        CALL axloop(buf,jbuf,xx,yy,zz,hc1,hc2,hc3)
                     ENDIF
                     hcxyz(1) = hcxyz(1) + hc1
                     hcxyz(2) = hcxyz(2) + hc2
                     hcxyz(3) = hcxyz(3) + hc3
                  ENDDO
               ENDIF
!
!     MULTIPLY MATERIAL INTO HC AT THIS INTEGRATION POINT
!
               CALL gmmats(g,3,3,0,hcxyz,3,1,0,gh)
            ENDIF
            sfact = h(iii)*h(jjj)*determ
!
!     TRANSFORM DNL FROM LOCAL TO BASIC
!     1 ST EXPAND TO ADD IN ZEROS CORRESPONDING TO Z DIRECTION
!
            DO i = 1 , 16
               ddnl(i) = dnl(i)
            ENDDO
            DO i = 17 , 24
               ddnl(i) = 0.
            ENDDO
!
            CALL gmmats(et,3,3,1,ddnl,3,8,0,ddnlb)
!
            DO m = 1 , ngrids
               f(m) = f(m) + (ddnlb(m)*gh(1)+ddnlb(m+8)*gh(2)+ddnlb(m+16)*gh(3))*sfact
            ENDDO
!
!     GET ANOTHER INTEGRATION POINT
!
         ENDDO
      ENDDO
!
!     ADD LOAD INTO LOAD ARRAY
!
      DO m = 1 , ngrids
         isil = Necpt(m+1)
!
!     IF PERMBDY EXISTS AND IF GRID IS NOT ON IT, IGNORE ITS LOAD
!
         IF ( Nbdys/=0 ) THEN
            DO i = 1 , Nbdys
               IF ( isil==Iz(Istart-Nbdys-Nelout+i-1) ) GOTO 1020
            ENDDO
            CYCLE
         ENDIF
 1020    Z(isil) = Z(isil) - f(m)*Ecpt(ia)
      ENDDO
   ENDIF
!
!     BEFORE LEAVING COMPUTE HC AT GRIDS AND CENTROID AND WRITE TO SCR6
!
   IF ( Jtype==24 ) RETURN
   CALL write(scr6,isc,2,0)
!
!     SET UP SHAPE FUNCTIONS AT CENTROID
!
   DO i = 1 , 4
      dn(i) = -.25
   ENDDO
   DO i = 5 , 8
      dn(i) = .5
   ENDDO
!
   IF ( Jtype==20 ) THEN
!
!     FOR SPCFLD HC VALUES AT GRIDS ARE IN CORE
!
      CALL write(scr6,hci,24,0)
!
      DO i = 1 , 3
         hcxyz(i) = 0.
      ENDDO
      DO m = 1 , ngrids
         hcxyz(1) = hcxyz(1) + dn(m)*hci(3*m-2)
         hcxyz(2) = hcxyz(2) + dn(m)*hci(3*m-1)
         hcxyz(3) = hcxyz(3) + dn(m)*hci(3*m)
      ENDDO
!
      CALL write(scr6,hcxyz,3,0)
      RETURN
   ENDIF
!
!     NOT SPCFLD
!
   DO j = 1 , 9
      IF ( j/=9 ) THEN
         xx = r(1,j)
         yy = r(2,j)
         zz = r(3,j)
      ELSE
!
!     CENTROID
!
         xx = 0.
         yy = 0.
         zz = 0.
         DO m = 1 , 8
            xx = xx + dn(m)*r(1,m)
            yy = yy + dn(m)*r(2,m)
            zz = zz + dn(m)*r(3,m)
         ENDDO
      ENDIF
      hc(1) = 0.
      hc(2) = 0.
      hc(3) = 0.
      DO ijk = 1 , Ido
         isub = Istart + (ijk-1)*Iwords - 1
         DO i = 1 , Iwords
            buf(i) = Z(isub+i)
         ENDDO
         IF ( ktype==2 ) THEN
            CALL geloop(buf,jbuf,xx,yy,zz,hc1,hc2,hc3)
         ELSEIF ( ktype==3 ) THEN
            CALL dipole(buf,jbuf,xx,yy,zz,hc1,hc2,hc3)
         ELSE
            CALL axloop(buf,jbuf,xx,yy,zz,hc1,hc2,hc3)
         ENDIF
         hc(1) = hc(1) + hc1
         hc(2) = hc(2) + hc2
         hc(3) = hc(3) + hc3
      ENDDO
!
      CALL write(scr6,hc,3,0)
   ENDDO
!
   RETURN
99999 RETURN
END SUBROUTINE em2d
