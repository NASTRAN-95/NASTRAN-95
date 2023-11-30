
SUBROUTINE tranem(Mcsid,Ngrid,R,Icomp,U,Rc)
   IMPLICIT NONE
   INTEGER Icomp , Mcsid , Ngrid
   REAL R(9) , Rc(3) , U(9)
   REAL c , grds , rcent(4) , s , sum , temp , ve1 , ve2 , ve3 , vm1 , vm2 , vm3 , vn1 , vn2 , vn3 , vndotm
   INTEGER ecpt(4) , i , ic , ig , k , subnam(2)
!*****
!     COMPUTES A STRESS TRANSFORMATION MATRIX U FOR TRIANGLES AND QUADS.
!     INPUTS
!        MCSID  ID OF COORDINATE SYSTEM REFERENCED ON MAT1,2 DATA CARD.
!        NGRID  3 FOR TRIANGLES, 4 FOR QUADS.
!        R      ARRAY OF BASIC LOCATIONS OF ELEMENT GRID PTS (3,NGRID).
!     OUTPUTS
!        ICOMP  1 (IF MAT X-AXIS IS USED) OR 2 (IF Y-AXIS IS USED).
!        U      ARRAY (3X3) FOR TRANSFORMATION, STORED BY ROW.
!        RC     BASIC LOCATION COORDINATES OF ELEMENT CENTER.
!     REQUIREMENTS
!        SUBROUTINE PRETRS MUST SET UP FOR TRANSS. SEE P.M. PAGE 3.4-66
!*****
!
!
   !>>>>EQUIVALENCE (rcent(1),ecpt(1))
!
   DATA subnam/4HTRAN , 2HEM/
!
!-----------------------------------------------------------------------
!
   IF ( Ngrid/=3 .AND. Ngrid/=4 ) CALL mesage(-61,0,subnam)
!*****
!     FIND THE UNIT NORMAL OF THE ELEMENT
!*****
   i = 3*(Ngrid-3)
   vn1 = (R(8)-R(2))*(R(i+9)-R(6)) - (R(9)-R(3))*(R(i+8)-R(5))
   vn2 = (R(9)-R(3))*(R(i+7)-R(4)) - (R(7)-R(1))*(R(i+9)-R(6))
   vn3 = (R(7)-R(1))*(R(i+8)-R(5)) - (R(8)-R(2))*(R(i+7)-R(4))
   temp = sqrt(vn1**2+vn2**2+vn3**2)
   IF ( temp<=0.0 ) CALL mesage(-61,0,subnam)
   vn1 = vn1/temp
   vn2 = vn2/temp
   vn3 = vn3/temp
!*****
!     GET THE UNIT VECTORS OF MCSID AT ELEM CENTER. PUT IN U TEMPORARILY
!*****
   grds = Ngrid
   DO ic = 1 , 3
      sum = 0.0
      DO ig = 1 , Ngrid
         k = 3*ig + ic - 3
         sum = sum + R(k)
      ENDDO
      rcent(ic+1) = sum/grds
      Rc(ic) = rcent(ic+1)
   ENDDO
   ecpt(1) = Mcsid
   CALL transs(ecpt,U)
!*****
!     SELECT FIRST OR SECOND VECTOR TO PROJECT FOR ELEM-MAT X-AXIS
!*****
   vndotm = vn1*U(1) + vn2*U(4) + vn3*U(7)
   IF ( vndotm**2>0.4 ) THEN
      Icomp = 2
      vm1 = U(2)
      vm2 = U(5)
      vm3 = U(8)
      vndotm = vn1*vm1 + vn2*vm2 + vn3*vm3
   ELSE
      Icomp = 1
      vm1 = U(1)
      vm2 = U(4)
      vm3 = U(7)
   ENDIF
!*****
!     FIND COSINE AND SINE OF ANGLE
!*****
   ve1 = R(4) - R(1)
   ve2 = R(5) - R(2)
   ve3 = R(6) - R(3)
   c = ve1*(vm1-vndotm*vn1) + ve2*(vm2-vndotm*vn2) + ve3*(vm3-vndotm*vn3)
   s = ve1*(vm2*vn3-vm3*vn2) + ve2*(vm3*vn1-vm1*vn3) + ve3*(vm1*vn2-vm2*vn1)
   temp = sqrt(c*c+s*s)
   IF ( temp<=0.0 ) CALL mesage(-61,0,subnam)
   c = c/temp
   s = s/temp
!*****
!     FILL IN THE U MATRIX, ROW STORED.
!*****
   U(1) = c*c
   U(4) = s*s
   U(7) = -c*s
   U(2) = U(4)
   U(5) = U(1)
   U(8) = -U(7)
   U(3) = 2.0*U(8)
   U(6) = -U(3)
   U(9) = U(1) - U(4)
!
!
END SUBROUTINE tranem