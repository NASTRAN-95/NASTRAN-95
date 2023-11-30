
SUBROUTINE termsd(Nnode,Gpth,Epnorm,Egpdt,Iorder,Mmn,Bterms)
   IMPLICIT NONE
   LOGICAL Badjac
   DOUBLE PRECISION Cjac(19) , Detj , Eta , Th , Tie(9) , Vn(3) , Xi , Zeta
   INTEGER Ltypfl
   COMMON /cjacob/ Cjac
   COMMON /comjac/ Xi , Eta , Zeta , Detj , Badjac , Ltypfl
   INTEGER Nnode
   DOUBLE PRECISION Bterms(1) , Gpth(1)
   REAL Egpdt(4,1) , Epnorm(4,1)
   INTEGER Iorder(1) , Mmn(1)
   DOUBLE PRECISION dshp(16) , dshpe(8) , dshpx(8) , dum , eps , gridc(3,8) , jacob(3,3) , shp(8) , tdshp(16) , temp , tj(3,3) ,    &
                  & tshp(8)
   INTEGER i , ii , ij , ik , index(3,3) , io , ising , j , j1 , k , ngp , node3
!
!     DOUBLE PRECISION ROUTINE TO CALCULATE B-MATRIX TERMS
!     FOR ELEMENTS  QUAD4, QUAD8 AND TRIA6.
!
!     THE INPUT FLAG LETS THE SUBROUTINE SWITCH BETWEEN QUAD4,
!     QUAD8 AND TRIA6 VERSIONS
!
!     ELEMENT TYPE FLAG (LTYPFL) = 1  FOR QUAD4,
!                                = 2  FOR TRIA6 (NOT AVAILABLE),
!                                = 3  FOR QUAD8 (NOT AVAILABLE).
!
!     THE OUTPUT CONSISTS OF THE DETERMINANT OF THE JACOBIAN
!     (DETJ), SHAPE FUNCTIONS AND THEIR DERIVATIVES. THE OUTPUT
!     PARAMETER, BADJAC, IS AN INTERNAL LOGICAL FLAG TO THE CALLING
!     ROUTINE INDICATING THAT THE JACOBIAN IS NOT CORRECT.
!     PART OF THE INPUT IS PASSED TO THIS SUBROUTINE THROUGH THE
!     INTERNAL COMMON BLOCK  /COMJAC/.
!
   EQUIVALENCE (dshpx(1),dshp(1)) , (dshpe(1),dshp(9))
   EQUIVALENCE (Vn(1),Cjac(8)) , (Tie(1),Cjac(11))
   EQUIVALENCE (Th,Cjac(1))
!
   eps = 1.0D-15
   Badjac = .FALSE.
!
   IF ( Ltypfl==2 ) THEN
!
!     TRIA6 VERSION
!
      ngp = 6
   ELSEIF ( Ltypfl==3 ) THEN
!
!     QUAD8 VERSION
!
      ngp = 8
   ELSE
!
!     QUAD4 VERSION
!
      ngp = 4
      CALL q4shpd(Xi,Eta,shp,dshp)
   ENDIF
!
   DO i = 1 , ngp
      tshp(i) = shp(i)
      tdshp(i) = dshp(i)
      tdshp(i+8) = dshp(i+ngp)
   ENDDO
   DO i = 1 , ngp
      io = Iorder(i)
      shp(i) = tshp(io)
      dshp(i) = tdshp(io)
      dshp(i+8) = tdshp(io+8)
   ENDDO
!
   Th = 0.0D0
   DO i = 1 , Nnode
      Th = Th + Gpth(i)*shp(i)
      DO j = 1 , 3
         j1 = j + 1
         gridc(j,i) = Egpdt(j1,i) + Zeta*Gpth(i)*Epnorm(j1,i)*0.5D0
      ENDDO
   ENDDO
!
   DO i = 1 , 2
      ii = (i-1)*8
      DO j = 1 , 3
         tj(i,j) = 0.0D0
         DO k = 1 , Nnode
            tj(i,j) = tj(i,j) + dshp(k+ii)*gridc(j,k)
         ENDDO
      ENDDO
   ENDDO
!
   DO i = 1 , 3
      tj(3,i) = 0.0D0
      DO j = 1 , Nnode
         tj(3,i) = tj(3,i) + 0.5D0*Gpth(j)*shp(j)*Epnorm(i+1,j)
      ENDDO
   ENDDO
!
   DO i = 1 , 3
      DO j = 1 , 3
         IF ( dabs(tj(i,j))<eps ) tj(i,j) = 0.0D0
      ENDDO
   ENDDO
!
!     SET UP THE TRANSFORMATION FROM THIS INTEGRATION POINT C.S.
!     TO THE ELEMENT C.S.  TIE
!
   Vn(1) = tj(1,2)*tj(2,3) - tj(2,2)*tj(1,3)
   Vn(2) = tj(2,1)*tj(1,3) - tj(1,1)*tj(2,3)
   Vn(3) = tj(1,1)*tj(2,2) - tj(2,1)*tj(1,2)
!
   temp = dsqrt(Vn(1)*Vn(1)+Vn(2)*Vn(2)+Vn(3)*Vn(3))
!
   Tie(7) = Vn(1)/temp
   Tie(8) = Vn(2)/temp
   Tie(9) = Vn(3)/temp
!
   temp = dsqrt(Tie(8)*Tie(8)+Tie(9)*Tie(9))
!
   Tie(1) = Tie(9)/temp
   Tie(2) = 0.0D0
   Tie(3) = -Tie(7)/temp
!
   Tie(4) = Tie(8)*Tie(3)
   Tie(5) = temp
   Tie(6) = -Tie(1)*Tie(8)
!
   CALL inverd(3,tj,3,dum,0,Detj,ising,index)
!
!
!     NOTE - THE INVERSE OF JACOBIAN HAS BEEN STORED IN TJ
!            UPON RETURN FROM INVERD.
!
   IF ( ising==1 .AND. Detj>0.0D0 ) THEN
!
!
      DO i = 1 , 3
         ii = (i-1)*3
         DO j = 1 , 3
            jacob(i,j) = 0.0D0
            DO k = 1 , 3
               ik = ii + k
               jacob(i,j) = jacob(i,j) + Tie(ik)*tj(k,j)
            ENDDO
         ENDDO
      ENDDO
!
!     MULTIPLY THE INVERSE OF THE JACOBIAN BY THE TRANSPOSE
!     OF THE ARRAY CONTAINING DERIVATIVES OF THE SHAPE FUNCTIONS
!     TO GET THE TERMS USED IN THE ASSEMBLY OF THE B MATRIX.
!     NOTE THAT THE LAST ROW CONTAINS THE SHAPE FUNCTION VALUES.
!
      node3 = Nnode*3
      DO i = 1 , Nnode
         Bterms(node3+i) = shp(i)*jacob(3,3)
      ENDDO
!
      DO i = 1 , 3
         ii = (i-1)*Nnode
         DO j = 1 , Nnode
            ij = ii + j
            Bterms(ij) = 0.0D0
            DO k = 1 , 2
               ik = (k-1)*8
               Bterms(ij) = Bterms(ij) + jacob(i,k)*dshp(ik+j)
            ENDDO
         ENDDO
      ENDDO
   ELSE
      Badjac = .TRUE.
   ENDIF
END SUBROUTINE termsd
