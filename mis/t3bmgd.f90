
SUBROUTINE t3bmgd(Ierr,Sheart,Ipt,Iorder,Egpdt,Dgpth,Aic,Th,Detjac,Shp,Bterms,Bmatrx)
   IMPLICIT NONE
   LOGICAL Bendng , Mbcoup , Membrn , Norpth , Shrflx
   COMMON /terms / Membrn , Bendng , Shrflx , Mbcoup , Norpth
   DOUBLE PRECISION Detjac , Th
   INTEGER Ierr , Ipt
   LOGICAL Sheart
   DOUBLE PRECISION Aic(1) , Bmatrx(1) , Bterms(1) , Dgpth(1) , Egpdt(4,1) , Shp(3)
   INTEGER Iorder(3)
   DOUBLE PRECISION dnx , dny , dshpe(3) , dshpx(3) , eps , eta , etai , jacob(4) , psi , psii , ptint(2,7) , shpf , tdshpe(3) ,    &
                  & tdshpx(3) , trc(2,3) , tshp(3) , vi(2) , vj(2) , xsi , xsii
   INTEGER i , i71 , i72 , i81 , i82 , i91 , i92 , ii , ip , ipt1 , ish , j , kk , nd1 , nd2 , nd3 , nd4 , nd5 , nd6 , nd7 , nd8 ,  &
         & nd9 , nnode
!
!     B-MATRIX GENERATOR ROUTINE FOR TRIA3 ELEMENTS
!
!     DOUBLE PRECISION ROUTINE TO GENERATE A 9XNDOF B-MATRIX AT A
!     GIVEN INTEGRATION POINT, USING THE DERIVATIVES OF THE 2-D SHAPE
!     FUNCTIONS.
!     OPTIONALLY, AN 8XNDOF B-MATRIX IS CONSTRUCTED AND/OR SHEAR TERMS
!     MAY BE DROPPED ALTOGETHER, YIELDING 6XNDOF MATRIX.
!     FOR STRESS RECOVERY, THE EVALUATION POINTS ARE AT THE ELEMENT
!     INTERIOR POINTS RATHER THAN ON THE EDGES.
!     THE CONTENTS OF /TERMS/ ARE USED TO CONSTRUCT THE B-MATRIX
!     ACCORDING TO THE BEHAVIORAL REQUIREMENTS OF THE ELEMENT.
!
!
!     INPUT :
!           IPT    - POINTER TO THE CURVILNEAR COORDINATES
!           SHEART - LOGICAL INDICATING THE REQUIREMENT FOR OUT-OF-PLANE
!                    SHEAR TERMS
!           IORDER - ARRAY OF INTERNAL SEQUENCE OF NODES
!           EGPDT  - GRID POINT DATA IN THE ELEMENT COORD. SYSTEM
!           DGPTH  - NODAL THICKNESSES
!           AIC    - TRANSFORMATION TO RELIEVE GEOMETRY BIAS
!     OUTPUT:
!           IERR   - ERROR FLAG
!           TH     - THICKNESS AT THE INTEG. PT.
!           DETJAC - DETERMINANT OF JACOBIAN AT THE INTEG. PT.
!           SHP    - ARRAY OF REORDERED SHAPE FUNCTIONS
!           BTERMS - DERIVATIVES WRT THE PHYSICAL COORDINATES
!           BMATRX - STRAIN-DISPLACEMENT RELATIONSHIP
!
!
   DATA eps/1.0D-13/
   DATA ptint/0.5D0 , 0.0D0 , 0.5D0 , 0.5D0 , 0.0D0 , 0.5D0 , 0.333333333333333D0 , 0.333333333333333D0 , 0.166666666666667D0 ,     &
      & 0.166666666666667D0 , 0.166666666666667D0 , 0.666666666666667D0 , 0.666666666666667D0 , 0.166666666666667D0/
   DATA trc/0.0D0 , 0.0D0 , 1.0D0 , 0.0D0 , 0.0D0 , 1.0D0/
!
!     INITIALIZE
!
   Ierr = 0
   nnode = 3
   nd1 = nnode*6
   nd2 = nd1*2
   nd3 = nd1*3
   nd4 = nd1*4
   nd5 = nd1*5
   nd6 = nd1*6
   nd7 = nd1*7
   nd8 = nd1*8
   nd9 = nd1*9
!
   DO i = 1 , 6
      Bterms(i) = 0.0D0
   ENDDO
   DO i = 1 , nd9
      Bmatrx(i) = 0.0D0
   ENDDO
!
!     CALCULATE THE SHAPE FUNCTIONS AND THEIR DERIVATIVES, THEN SORT
!     THEM.
!
   xsi = ptint(1,Ipt)
   eta = ptint(2,Ipt)
   psi = 1.0D0 - xsi - eta
!
   DO i = 1 , 3
      xsii = trc(1,i)
      etai = trc(2,i)
      psii = 1.0D0 - xsii - etai
!
      Shp(i) = xsi*xsii + eta*etai + psi*psii
      dshpx(i) = xsii - psii
      dshpe(i) = etai - psii
   ENDDO
!
   DO i = 1 , nnode
      tshp(i) = Shp(i)
      tdshpx(i) = dshpx(i)
      tdshpe(i) = dshpe(i)
   ENDDO
!
   DO i = 1 , nnode
      kk = Iorder(i)
      Shp(i) = tshp(kk)
      dshpx(i) = tdshpx(kk)
      dshpe(i) = tdshpe(kk)
   ENDDO
!
!     COMPUTE THE ELEMENT THICKNESS
!
   Th = 0.0D0
   DO ish = 1 , nnode
      Th = Th + Shp(ish)*Dgpth(ish)
   ENDDO
!
!     SET UP THE JACOBIAN
!
   DO i = 1 , 2
      vi(i) = 0.0D0
      vj(i) = 0.0D0
      ii = i + 1
      DO j = 1 , nnode
         vi(i) = vi(i) + Egpdt(ii,j)*dshpx(j)
         vj(i) = vj(i) + Egpdt(ii,j)*dshpe(j)
      ENDDO
   ENDDO
!
!     INVERT THE JACOBIAN
!
   Detjac = vi(1)*vj(2) - vi(2)*vj(1)
   IF ( Detjac>=eps ) THEN
!
      jacob(1) = vj(2)/Detjac
      jacob(2) = -vi(2)/Detjac
      jacob(3) = -vj(1)/Detjac
      jacob(4) = vi(1)/Detjac
!
      DO i = 1 , 4
         IF ( dabs(jacob(i))<eps ) jacob(i) = 0.0D0
      ENDDO
!
      ipt1 = Ipt*2 - 1
      i71 = ipt1
      i72 = ipt1 + 1
      i81 = ipt1 + 6
      i82 = ipt1 + 7
      i91 = ipt1 + 12
      i92 = ipt1 + 13
!
!     LOOP OVER NODES AND BUILD PARTITIONS OF B-MATRIX
!
      ip = 0
      DO i = 1 , nnode
!
!     CALCULATE DERIVATIVES WRT THE PHYSICAL COORDINATES.
!
         dnx = jacob(1)*dshpx(i) + jacob(2)*dshpe(i)
         dny = jacob(3)*dshpx(i) + jacob(4)*dshpe(i)
         shpf = Shp(i)
!
         Bterms(i) = dnx
         Bterms(i+nnode) = dny
!
         IF ( Membrn ) THEN
!
!     ROW 1
!
            Bmatrx(ip+1) = dnx
!
!     ROW 2
!
            Bmatrx(ip+2+nd1) = dny
!
!     ROW 3
!
            Bmatrx(ip+1+nd2) = dny
            Bmatrx(ip+2+nd2) = dnx
         ENDIF
!
         IF ( Bendng ) THEN
!
!     ROW 4
!
            Bmatrx(ip+5+nd3) = -dnx
!
!     ROW 5
!
            Bmatrx(ip+4+nd4) = dny
!
!     ROW 6
!
            Bmatrx(ip+5+nd5) = -dny
            Bmatrx(ip+4+nd5) = dnx
!
            IF ( Sheart ) THEN
               IF ( Ipt<4 ) THEN
!
!     9-ROW MATRIX
!
!     ROW 7
!
                  Bmatrx(ip+3+nd6) = Aic(i71)*dny + Aic(i72)*dnx
                  Bmatrx(ip+4+nd6) = -Aic(i71)*shpf
                  Bmatrx(ip+5+nd6) = Aic(i72)*shpf
!
!     ROW 8
!
                  Bmatrx(ip+3+nd7) = Aic(i81)*dny + Aic(i82)*dnx
                  Bmatrx(ip+4+nd7) = -Aic(i81)*shpf
                  Bmatrx(ip+5+nd7) = Aic(i82)*shpf
!
!     ROW 9
!
                  Bmatrx(ip+3+nd8) = Aic(i91)*dny + Aic(i92)*dnx
                  Bmatrx(ip+4+nd8) = -Aic(i91)*shpf
                  Bmatrx(ip+5+nd8) = Aic(i92)*shpf
               ELSE
!
!     8-ROW MATRIX
!
!     ROW 7
!
                  Bmatrx(ip+3+nd6) = dny
                  Bmatrx(ip+4+nd6) = -shpf
!
!     ROW 8
!
                  Bmatrx(ip+3+nd7) = dnx
                  Bmatrx(ip+5+nd7) = shpf
               ENDIF
            ENDIF
         ENDIF
!
         ip = ip + 6
      ENDDO
      GOTO 99999
   ENDIF
   Ierr = 1
   RETURN
!
99999 RETURN
END SUBROUTINE t3bmgd