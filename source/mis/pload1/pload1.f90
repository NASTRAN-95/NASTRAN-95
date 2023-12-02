!*==pload1.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pload1(Opt,Islt,V,Sa,Sb,Ba,Bb,Pa,Pb,Ta,Tb,Slt,Ept)
   USE c_matout
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Opt
   INTEGER , DIMENSION(7) :: Islt
   REAL , DIMENSION(3) :: V
   REAL , DIMENSION(3) :: Sa
   REAL , DIMENSION(3) :: Sb
   REAL , DIMENSION(3) :: Ba
   REAL , DIMENSION(3) :: Bb
   REAL , DIMENSION(6) :: Pa
   REAL , DIMENSION(6) :: Pb
   REAL , DIMENSION(9) :: Ta
   REAL , DIMENSION(9) :: Tb
   REAL , DIMENSION(7) :: Slt
   REAL , DIMENSION(32) :: Ept
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(3) :: a , b , c , tp
   REAL :: aly , alz , f1 , f2 , fscale , fx1 , fx2 , fy1 , fy2 , fz1 , fz2 , len , omaly , omalz , temp , tmp , x1 , x2
   REAL(REAL64) :: ax , ay , az , bx , by , bz , dfx1 , dfx2 , dfy1 , dfy2 , dfz1 , dfz2 , dl , dt , dx1 , dx2 , i01 , i02 , i11 ,  &
                 & i12 , i21 , i22 , i31 , i32 , i41 , i42 , s1 , s2 , s3 , s4 , s5
   REAL , DIMENSION(9) , SAVE :: d
   REAL , DIMENSION(9) :: e
   INTEGER :: i , j , scale , type
   INTEGER , SAVE :: oldid
   EXTERNAL gmmats , mesage , ploapf , sadotb , saxb
!
! End of declarations rewritten by SPAG
!
!
!     PLOAD1 CALCULATES THE END LOADS ON A BAR ELEMENT FOR PLOAD1 LOADS
!     IT IS CALLED ONLY BY PLBAR1
!
!     OPT   = 1, CALLED FROM PLBAR1/EXTERN,  2, CALLED FROM SDRX
!     SLT   = PLOAD1 CARD
!     V     = REFERENCE VECTOR IN BASIC
!     SA    = OFFSET VECTOR IN BASIC POINT A
!     SB    = OFFSET VECTOR IN BASIC POINT B
!     BA    = BASIC  COORD  FOR POINT A
!     BB    = BASIC  COORD  FOR POINT B
!     PA    = LOAD   VECOTR FOR POINT A
!     PB    = LOAD   VECOTR FOR POINT B
!     TA,TB = TRANSFORMATION MATRICES FOR A AND B ONLY USED WITH OPT 1
!     EPT   = POINTER TO EST
!
   !>>>>EQUIVALENCE (a(1),e(1)) , (b(1),e(7)) , (c(1),e(4))
   DATA oldid , d/10*0/
!
   IF ( oldid/=Islt(1) ) THEN
      oldid = Islt(1)
!
!     CALCULATE AXIS AND LENGTH, AND THE E MATRIX
!
      a(1) = Bb(1) - Ba(1) + Sb(1) - Sa(1)
      a(2) = Bb(2) - Ba(2) + Sb(2) - Sa(2)
      a(3) = Bb(3) - Ba(3) + Sb(3) - Sa(3)
      len = sqrt(sadotb(a,a))
      IF ( len==0.0 ) THEN
!
!     ERROR
!
         CALL mesage(-30,31,oldid)
         RETURN
      ELSE
         a(1) = a(1)/len
         a(2) = a(2)/len
         a(3) = a(3)/len
         CALL saxb(a,V,b)
!
         temp = sqrt(sadotb(b,b))
         IF ( temp==0.0 ) THEN
            CALL mesage(-30,31,oldid)
            RETURN
         ELSE
            b(1) = b(1)/temp
            b(2) = b(2)/temp
            b(3) = b(3)/temp
            CALL saxb(b,a,c)
!
!     TRANSVERSE SHEAR
!
            temp = Ept(31)*Ept(17)*g*len**2
            tmp = 12.0*f*Ept(18)
            aly = 0.0
            IF ( abs(temp+tmp)>1.0E-14 ) aly = tmp/(tmp+temp)
            omaly = 1.0 - aly
!
            temp = (temp/Ept(31))*Ept(32)
            tmp = 12.0*f*Ept(19)
            alz = 0.0
            IF ( abs(temp+tmp)>1.0E-14 ) alz = tmp/(tmp+temp)
            omalz = 1.0 - alz
         ENDIF
      ENDIF
   ENDIF
!
!     START BUILDING THE FORCES AND MOMENTS
!
   type = Islt(2)
   scale = Islt(3)
   x1 = Slt(4)
   f1 = Slt(5)
   x2 = Slt(6)
   f2 = Slt(7)
   i = (type-1)/6 + 1
   j = mod(type,6)
   IF ( j/=0 ) THEN
      IF ( j==4 ) THEN
         fx1 = f1
         fy1 = 0.0
         fz1 = 0.0
         fx2 = f2
         fy2 = 0.0
         fz2 = 0.0
      ELSEIF ( j==5 ) THEN
         fx1 = 0.0
         fy1 = f1
         fz1 = 0.0
         fx2 = 0.0
         fy2 = f2
         fz2 = 0.0
         CALL spag_block_2
         RETURN
      ELSEIF ( j==6 ) THEN
         CALL spag_block_1
         RETURN
      ELSE
         fx1 = a(j)*f1
         fy1 = c(j)*f1
         fz1 = b(j)*f1
         fx2 = a(j)*f2
         fy2 = c(j)*f2
         fz2 = b(j)*f2
      ENDIF
      CALL spag_block_3
      RETURN
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      Fx1 = 0.0
      Fy1 = 0.0
      Fz1 = F1
      Fx2 = 0.0
      Fy2 = 0.0
      Fz2 = F2
      CALL spag_block_2
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
      J = 4
      CALL spag_block_3
   END SUBROUTINE spag_block_2
   SUBROUTINE spag_block_3
!
!     SCALED
!
      IF ( Scale/=2 .AND. Scale/=4 ) THEN
         X1 = X1/Len
         X2 = X2/Len
      ENDIF
!
!     DISTRIBUTED SCALED LOADS
!
      IF ( X1==X2 ) THEN
!
!     CONCENTRATED LOADS
!
         Tmp = 1.0 - X1
         IF ( I==2 ) THEN
!
!     MOMENTS
!
            Temp = -(6.0/Len*X1)*Tmp
            Pa(1) = 0.0
            Pa(2) = Temp*Fz1*Omaly
            Pa(3) = -Temp*Fy1*Omalz
            Pa(4) = Tmp*Fx1
            Temp = 1.0 - 4.0*X1 + 3.0*X1**2
            Pa(5) = Temp*Fy1*Omalz + Fy1*Tmp*Alz
            Pa(6) = Temp*Fz1*Omaly + Fz1*Tmp*Aly
            Pb(1) = 0.0
            Pb(2) = -Pa(2)
            Pb(3) = -Pa(3)
            Pb(4) = X1*Fx1
            Temp = 3.0*X1**2 - 2.0*X1
            Pb(5) = Temp*Fy1*Omalz + Fy1*X1*Alz
            Pb(6) = Temp*Fz1*Omaly + Fz1*X1*Aly
         ELSE
!
!     FORCES
!
            Temp = 1.0 - 3.0*X1**2 + 2.0*X1**3
            Pa(1) = Tmp*Fx1
            Pa(2) = Temp*Fy1*Omaly + Fy1*Tmp*Aly
            Pa(3) = Temp*Fz1*Omalz + Fz1*Tmp*Alz
            Pa(4) = 0.0
            Temp = -Len*X1*Tmp**2
            Tmp = Tmp*Len*X1*.50
            Pa(5) = Temp*Fz1*Omalz - Fz1*Tmp*Alz
            Pa(6) = -Temp*Fy1*Omaly + Fy1*Tmp*Aly
            Temp = 3.0*X1**2 - 2.0*X1**3
            Pb(1) = X1*Fx1
            Pb(2) = Temp*Fy1*Omaly + Fy1*X1*Aly
            Pb(3) = Temp*Fz1*Omalz + Fz1*X1*Alz
            Pb(4) = 0.0
            Temp = (1.0-X1)*Len*X1**2
            Pb(5) = Temp*Fz1*Omalz + Fz1*Tmp*Alz
            Pb(6) = -Temp*Fy1*Omaly - Fy1*Tmp*Aly
         ENDIF
         CALL spag_block_5
         RETURN
      ELSE
         IF ( Scale>2 .AND. J/=4 ) THEN
            Fscale = sqrt(1.0-A(J)**2)
            Fx1 = Fscale*Fx1
            Fy1 = Fscale*Fy1
            Fz1 = Fscale*Fz1
            Fx2 = Fscale*Fx2
            Fy2 = Fscale*Fy2
            Fz2 = Fscale*Fz2
         ENDIF
!
!     DISTRIBUTED LOADS
!
         Dx1 = X1
         Dx2 = X2
         Dl = Len
         Dfx1 = Fx1
         Dfy1 = Fy1
         Dfz1 = Fz1
         Dfx2 = Fx2
         Dfy2 = Fy2
         Dfz2 = Fz2
         S1 = Dx2 - Dx1
         S2 = .5000000D0*(Dx2**2-Dx1**2)
         S3 = .3333333D0*(Dx2**3-Dx1**3)
         S4 = .2500000D0*(Dx2**4-Dx1**4)
         S5 = .2000000D0*(Dx2**5-Dx1**5)
         IF ( I==2 ) THEN
!
!     MOMENTS
!
            I01 = Dl*(S1-S2)
            I11 = -6.0D0*(S2-S3)
            I21 = -I11
            I31 = S1 - 4.0D0*S2 + 3.0D0*S3
            I41 = -2.0D0*S2 + 3.0D0*S3
            IF ( F1==F2 ) THEN
               Ax = Dfx1
               Ay = Dfz1
               Az = -Dfy1
            ELSE
               I02 = (S2-S3)*Dl**2
               I12 = -6.0D0*Dl*(S3-S4)
               I22 = -I12
               I32 = Dl*(S2-4.0D0*S3+3.0D0*S4)
               I42 = -Dl*(2.0D0*S3-3.0D0*S4)
               Dt = (Dx2-Dx1)*Dl
               Bx = (Dfx2-Dfx1)/Dt
               By = (Dfz2-Dfz1)/Dt
               Bz = -(Dfy2-Dfy1)/Dt
               Ax = Dfx1 + Dx1*Bx*Dl
               Ay = Dfz1 + Dx1*By*Dl
               Az = -Dfy1 + Dx1*Bz*Dl
               CALL spag_block_4
               RETURN
            ENDIF
         ELSE
!
!     FORCES
!
            I01 = Dl*(S1-S2)
            I11 = Dl*(S1-3.0D0*S3+2.0D0*S4)
            I21 = Dl*(3.0D0*S3-2.0D0*S4)
            I31 = Dl*(S2-2.0D0*S3+S4)
            I41 = Dl*(S4-S3)
            Dt = Dl*Dl
            I02 = Dt*(S2-S3)
            IF ( F1==F2 ) THEN
               Ax = Dfx1
               Ay = Dfy1
               Az = Dfz1
            ELSE
               I12 = Dt*(S2-3.0D0*S4+2.0D0*S5)
               I22 = Dt*(3.0D0*S4-2.0D0*S5)
               I32 = Dt*(S3-2.0D0*S4+S5)
               I42 = Dt*(S5-S4)
               Dt = Dl*(Dx2-Dx1)
               Bx = (Dfx2-Dfx1)/Dt
               By = (Dfy2-Dfy1)/Dt
               Bz = (Dfz2-Dfz1)/Dt
               Ax = Dfx1 - Dx1*Bx*Dl
               Ay = Dfy1 - Dx1*By*Dl
               Az = Dfz1 - Dx1*Bz*Dl
               CALL spag_block_4
               RETURN
            ENDIF
         ENDIF
         Bx = 0.0D0
         By = 0.0D0
         Bz = 0.0D0
         I12 = 0.0D0
         I22 = 0.0D0
         I32 = 0.0D0
         I42 = 0.0D0
      ENDIF
      CALL spag_block_4
   END SUBROUTINE spag_block_3
   SUBROUTINE spag_block_4
!
!     LOADS
!
      Pa(1) = I01*Ax + I02*Bx
      Pa(2) = I11*Ay + I12*By
      Pa(3) = I11*Az + I22*Bz
      Pa(4) = 0.0
      Pa(5) = -Dl*(I31*Az+I32*Bz)
      Pa(6) = Dl*(I31*Ay+I32*By)
      Dt = Dl*Dl
      Pb(1) = Dl*S2*Ax + Dt*S3*Bx
      Pb(2) = I21*Ay + I22*By
      Pb(3) = I21*Az + I22*Bz
      Pb(4) = 0.0
      Pb(5) = -Dl*(I41*Az+I42*Bz)
      Pb(6) = Dl*(I41*Ay+I42*By)
      IF ( I==2 ) THEN
         Temp = Pa(1)
         Pa(1) = Pa(4)
         Pa(4) = Temp
         Temp = Pb(1)
         Pb(1) = Pb(4)
         Pb(4) = Temp
         IF ( Aly/=0.0 ) THEN
            Pa(2) = Omaly*Pa(2)
            Pa(6) = Omaly*Pa(6) + Aly*(I01*Ay+I02*By)
            Pb(2) = Omaly*Pb(2)
            Pb(6) = Omaly*Pb(6) + Aly*(Dl*S2*Ay+S3*By*Dt)
         ENDIF
         IF ( Alz/=0.0 ) THEN
            Pa(3) = Omalz*Pa(3)
            Pa(5) = Omalz*Pa(5) + Alz*(I01*Az+I02*Bz)
            Pb(3) = Omalz*Pb(3)
            Pb(5) = Omalz*Pb(5) + Alz*(Dl*S2*Az+S3*Bz*Dt)
         ENDIF
      ELSE
         IF ( Aly/=0.0 ) THEN
            Pa(2) = Omaly*Pa(2) + Aly*(I01*Ay+I02*By)
            Pa(6) = Omaly*Pa(6) + Aly*(I02*Ay-I41*By*Dt)*.50
            Pb(2) = Omaly*Pb(2) + Aly*(Dl*S2*Ay+S3*By*Dt)
            Pb(6) = Omaly*Pb(6) - Aly*(I02*Ay-I41*By*Dt)*.50
         ENDIF
         IF ( Alz/=0.0 ) THEN
            Pa(3) = Omalz*Pa(3) + Alz*(I01*Az+I02*Bz)
            Pa(5) = Omalz*Pa(5) - Alz*(I02*Az-I41*Bz*Dt)*.50
            Pb(3) = Omalz*Pb(3) + Alz*(Dl*S2*Az+S3*Bz*Dt)
            Pb(5) = Omalz*Pb(5) + Alz*(I02*Az-I41*Bz*Dt)*.50
         ENDIF
      ENDIF
      CALL spag_block_5
   END SUBROUTINE spag_block_4
   SUBROUTINE spag_block_5
!
!     PIN FLAGS
!
      CALL ploapf(Ept,Ept,Len,Pa,Pb)
!
!     LOAD VECTORS DONE FOR SDRX
!
      IF ( Opt==2 ) RETURN
!
!     TRANSFORM LOAD VECTOR TO GLOBAL
!
      CALL gmmats(E,3,3,1,Pa(1),3,1,0,Tp)
      CALL gmmats(Ta,3,3,1,Tp,3,1,0,Pa(1))
      CALL gmmats(E,3,3,1,Pb(1),3,1,0,Tp)
      CALL gmmats(Tb,3,3,1,Tp,3,1,0,Pb(1))
      CALL gmmats(E,3,3,1,Pa(4),3,1,0,Tp)
      CALL gmmats(Ta,3,3,1,Tp,3,1,0,Pa(4))
      CALL gmmats(E,3,3,1,Pb(4),3,1,0,Tp)
      CALL gmmats(Tb,3,3,1,Tp,3,1,0,Pb(4))
!
      DO I = 1 , 3
         IF ( Sa(I)/=0.0 ) THEN
            CALL spag_block_6
            RETURN
         ENDIF
      ENDDO
      CALL spag_block_7
   END SUBROUTINE spag_block_5
   SUBROUTINE spag_block_6
      D(2) = -Sa(3)
      D(3) = Sa(2)
      D(4) = Sa(3)
      D(6) = -Sa(1)
      D(7) = -Sa(2)
      D(8) = Sa(1)
      CALL gmmats(D,3,3,0,Pa(1),3,1,0,Tp)
      Pa(4) = Pa(4) + Tp(1)
      Pa(5) = Pa(5) + Tp(2)
      Pa(6) = Pa(6) + Tp(3)
      CALL spag_block_7
   END SUBROUTINE spag_block_6
   SUBROUTINE spag_block_7
!
      DO I = 1 , 3
         IF ( Sb(I)/=0.0 ) THEN
            CALL spag_block_8
            RETURN
         ENDIF
      ENDDO
   END SUBROUTINE spag_block_7
   SUBROUTINE spag_block_8
      D(2) = -Sb(3)
      D(3) = Sb(2)
      D(4) = Sb(3)
      D(6) = -Sb(1)
      D(7) = -Sb(2)
      D(8) = Sb(1)
      CALL gmmats(D,3,3,0,Pb(1),3,1,0,Tp)
      Pb(4) = Pb(4) + Tp(1)
      Pb(5) = Pb(5) + Tp(2)
      Pb(6) = Pb(6) + Tp(3)
   END SUBROUTINE spag_block_8
!
END SUBROUTINE pload1
