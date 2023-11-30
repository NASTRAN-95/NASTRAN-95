
SUBROUTINE geloop(Rbuf,Buf,Xx,Yy,Zz,Hc1,Hc2,Hc3)
   IMPLICIT NONE
   REAL Hc1 , Hc2 , Hc3 , Xx , Yy , Zz
   INTEGER Buf(50)
   REAL Rbuf(50)
   REAL factor , fpi , xi , zdot , zi(3) , zj(3) , zjl , zjxi(3) , zk(3) , zkl , zlen2
   INTEGER i , icid , ii , npts , nptsm1 , ti1 , ti2
!
! GELOOP COMPUTES MAGNETIC FIELD COMPONENTS HC1,HC2,HC3(IN BASIC
! COORDS. AT XX,YY,ZZ DUE TO GEMLOOP CARD. DATA FIELDS(EXCEPT SET ID)
! OF GEMLOOP ARE IN RBUF=REAL AND BUF=INTEGER
!
   DATA fpi/12.566371/
!
   Hc1 = 0.
   Hc2 = 0.
   Hc3 = 0.
!
   xi = Rbuf(1)
!
! ICID IS 0 FOR NOW AND UNUSED
!
   icid = Buf(2)
   npts = Buf(3)
   nptsm1 = npts - 1
   DO i = 1 , nptsm1
!
! 2 CONSECUTIVE POINTS DEFINE A SEGMENT OF A COIL. LET ZI BE THE VECTOR
! FROM 1ST POINT OF SEGMENT TO 2ND. LET ZJ BE VECTOR FROM FILED POINT
! XX,YY,ZZ TO 1ST POINT OF SEGMENT. ZK IS VECTOR FROM FILED POINT
! TO 2ND POINT. IF THE FILED POINT LIES ON A SEGMENT, IGNORE THE
! COMPUTATION FOR THAT SEGMENT FOR THAT POINT
!
      ti1 = 3*i + 3
      ti2 = 3*(i+1) + 3
      zi(1) = Rbuf(ti2-2) - Rbuf(ti1-2)
      zi(2) = Rbuf(ti2-1) - Rbuf(ti1-1)
      zi(3) = Rbuf(ti2) - Rbuf(ti1)
      zj(1) = Rbuf(ti1-2) - Xx
      zj(2) = Rbuf(ti1-1) - Yy
      zj(3) = Rbuf(ti1) - Zz
      zk(1) = Rbuf(ti2-2) - Xx
      zk(2) = Rbuf(ti2-1) - Yy
      zk(3) = Rbuf(ti2) - Zz
!
      zkl = sqrt(zk(1)**2+zk(2)**2+zk(3)**2)
      IF ( zkl>=1.E-8 ) THEN
         zjl = sqrt(zj(1)**2+zj(2)**2+zj(3)**2)
         IF ( zjl>=1.E-8 ) THEN
            zdot = 0.
            DO ii = 1 , 3
               zdot = zdot + zi(ii)*(zk(ii)/zkl-zj(ii)/zjl)
            ENDDO
            zjxi(1) = zj(2)*zi(3) - zj(3)*zi(2)
            zjxi(2) = zj(3)*zi(1) - zj(1)*zi(3)
            zjxi(3) = zj(1)*zi(2) - zj(2)*zi(1)
            zlen2 = zjxi(1)**2 + zjxi(2)**2 + zjxi(3)**2
            IF ( zlen2>=1.E-8 ) THEN
               factor = xi*zdot/fpi/zlen2
               Hc1 = Hc1 + zjxi(1)*factor
               Hc2 = Hc2 + zjxi(2)*factor
               Hc3 = Hc3 + zjxi(3)*factor
            ENDIF
         ENDIF
      ENDIF
   ENDDO
END SUBROUTINE geloop