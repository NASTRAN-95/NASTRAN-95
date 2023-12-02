!*==strp11.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE strp11
   IMPLICIT NONE
   USE C_MATIN
   USE C_MATOUT
   USE C_SDR2X5
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a , ai , b , c , d11 , d12 , d13 , d22 , d23 , d33 , f , j11 , j12 , j22 , nsm , theta1 , thetam , thk , tmem1 , tmem3 , &
         & tmem5 , tshr1 , tshr3 , tshr5 , x2 , x2y , x3 , xy , xy2 , y2 , y3
   REAL , SAVE :: blank , degra
   REAL , DIMENSION(9) :: d , dph1 , emod , ph1ben
   REAL*8 :: determ
   REAL , DIMENSION(4) :: g
   REAL , DIMENSION(6) :: gph1 , ph1shr , xc , yc , zc
   INTEGER :: i , i1 , i2 , i3 , idele , ii , ij , ising , j , j1 , j2 , jj , jj1 , kz , l , m , matid1 , matid2 , mz
   INTEGER , DIMENSION(6) :: ics , nl
   INTEGER , DIMENSION(42) :: iest
   INTEGER , DIMENSION(6,3) :: ind
   INTEGER , DIMENSION(20,3) :: index
   REAL , DIMENSION(3) :: ivect , jvect , kvect , v1 , v2 , v3
   INTEGER , DIMENSION(2) , SAVE :: name
   LOGICAL :: nots
   INTEGER , DIMENSION(990) :: nph1ou
   REAL , DIMENSION(20,20) :: qqq
!
! End of declarations rewritten by SPAG
!
!
!     PHASE 1 STRESS DATA RECOVERY FOR CTRPLT1 - HIGHER ORDER PLATE
!     ELEMENT
!
!     OUTPUTS FROM THIS PHASE FOR USE IN PHASE II ARE THE FOLLOWING
!
!     1) ELEMENT ID              WORDS    1     STORAGE IN PH1OUT  1
!     2) SIX SILS                WORDS    6                      2-7
!     3) BENDING THICKNESSES     WORDS    3                      8-10
!     4) STRESS POINTS           WORDS    8                     11-18
!     5) 4 NOS. 6 5X6 S MATRICES WORDS    720                   19-738
!     6) 3X1 S SUB T MATRIX      WORDS    3                    739-741
!
!     ECPT ENTRIES
!     AS IN STIFFNESS ROUTINE KTRPL1
!
!
!     INPUT AND OUTPUT BLOCKS FOR SUBROUTINE MAT
!
!
!
!     EQUIVALENCE IECPT WITH ECPT IN COMMON BLOCK /SMA1ET/ SINCE ECPT IS
!     A MIXED INTEGER AND REAL ARRAY
!
   !>>>>EQUIVALENCE (A,Dista) , (B,Distb) , (C,Distc) , (V1(1),Est(19)) , (V2(1),Est(23)) , (V3(1),Est(27)) , (Iest(1),Est(1)) ,         &
!>>>>    & (D11,Em(1)) , (D12,Em(2)) , (D13,Em(3)) , (D22,Em(4)) , (D23,Em(5)) , (D33,Em(6))
   !>>>>EQUIVALENCE (Nph1ou(1),Ph1out(1))
   !>>>>EQUIVALENCE (Ph1out(401),Index(1,1),Ind(1,1))
   !>>>>EQUIVALENCE (Ph1out(1),Qqq(1,1))
   DATA degra/0.0174532925/
   DATA blank , name/4H     , 4HCTRP , 4HLT1 /
!
   nots = .FALSE.
   idele = iest(1)
   DO i = 1 , 6
      nl(i) = iest(i+1)
   ENDDO
   thetam = Est(8)
   matid1 = iest(9)
   tmem1 = (Est(10)*12.0)**0.333333333333
   tmem3 = (Est(11)*12.0)**0.333333333333
   tmem5 = (Est(12)*12.0)**0.333333333333
   matid2 = iest(13)
   tshr1 = Est(14)
   tshr3 = Est(15)
   tshr5 = Est(16)
   nsm = Est(17)
   j = 0
   DO i = 24 , 44 , 4
      j = j + 1
      ics(j) = iest(i)
      xc(j) = Est(i+1)
      yc(j) = Est(i+2)
      zc(j) = Est(i+3)
   ENDDO
!
!     IF TMEM3 OR TMEM5 IS ZERO OR BLANK, THEY WILL BE SET EQUAL TO
!     TMEM1
!     SO ALSO FOR TEMP3 OR TEMP5
!
   IF ( tmem3==0.0 .OR. tmem3==blank ) tmem3 = tmem1
   IF ( tmem5==0.0 .OR. tmem5==blank ) tmem5 = tmem1
   IF ( tshr3==0.0 .OR. tshr3==blank ) tshr3 = tshr1
   IF ( tshr5==0.0 .OR. tshr5==blank ) tshr5 = tshr1
   IF ( tshr1==0.0 ) nots = .TRUE.
   Eltemp = Est(48)
   theta1 = thetam*degra
   Sinth = sin(theta1)
   Costh = cos(theta1)
   IF ( abs(Sinth)<=1.0E-06 ) Sinth = 0.0
!
!     EVALUATE MATERIAL PROPERTIES
!
   Matflg = 2
   Matid = matid1
   CALL mat(idele)
!
   emod(1) = d11
   emod(2) = d12
   emod(3) = d13
   emod(4) = d12
   emod(5) = d22
   emod(6) = d23
   emod(7) = d13
   emod(8) = d23
   emod(9) = d33
   Matid = matid2
   Matflg = 3
   j11 = 0.0
   j12 = 0.0
   j22 = 0.0
   IF ( .NOT.(nots) ) CALL mat(idele)
!
!     CALCULATIONS FOR THE TRIANGLE
!
   CALL trif(xc,yc,zc,ivect,jvect,kvect,a,b,c,iest(1),name)
   CALL af(f,1,a,b,c,A1,A2,A3,tmem1,tmem3,tmem5,1)
   CALL af(f,1,a,b,c,B1,B2,B3,tshr1,tshr3,tshr5,1)
!
!     FILL E-MATRIX
!
   DO i = 1 , 18
      E(i) = 0.0
   ENDDO
   E(1) = kvect(1)
   E(4) = kvect(2)
   E(7) = kvect(3)
   E(11) = ivect(1)
   E(14) = ivect(2)
   E(17) = ivect(3)
   E(12) = jvect(1)
   E(15) = jvect(2)
   E(18) = jvect(3)
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
!
   ising = -1
!
!     CALCULATIONS FOR QMATRIX (QQQ) AND ITS INVERSE
!
   DO i = 1 , 20
      DO j = 1 , 20
         qqq(i,j) = 0.0
      ENDDO
   ENDDO
   DO i = 1 , 6
      i1 = (i-1)*3 + 1
      i2 = (i-1)*3 + 2
      i3 = (i-1)*3 + 3
      qqq(i1,1) = 1.0
      qqq(i1,2) = xc(i)
      qqq(i1,3) = yc(i)
      qqq(i1,4) = xc(i)*xc(i)
      qqq(i1,5) = xc(i)*yc(i)
      qqq(i1,6) = yc(i)*yc(i)
      qqq(i1,7) = qqq(i1,4)*xc(i)
      qqq(i1,8) = qqq(i1,4)*yc(i)
      qqq(i1,9) = qqq(i1,5)*yc(i)
      qqq(i1,10) = qqq(i1,6)*yc(i)
      qqq(i1,11) = qqq(i1,7)*xc(i)
      qqq(i1,12) = qqq(i1,7)*yc(i)
      qqq(i1,13) = qqq(i1,8)*yc(i)
      qqq(i1,14) = qqq(i1,9)*yc(i)
      qqq(i1,15) = qqq(i1,10)*yc(i)
      qqq(i1,16) = qqq(i1,11)*xc(i)
      qqq(i1,17) = qqq(i1,12)*yc(i)
      qqq(i1,18) = qqq(i1,13)*yc(i)
      qqq(i1,19) = qqq(i1,14)*yc(i)
      qqq(i1,20) = qqq(i1,15)*yc(i)
      qqq(i2,3) = 1.0
      qqq(i2,5) = xc(i)
      qqq(i2,6) = yc(i)*2.0
      qqq(i2,8) = qqq(i1,4)
      qqq(i2,9) = qqq(i1,5)*2.0
      qqq(i2,10) = qqq(i1,6)*3.0
      qqq(i2,12) = qqq(i1,7)
      qqq(i2,13) = qqq(i1,8)*2.0
      qqq(i2,14) = qqq(i1,9)*3.0
      qqq(i2,15) = qqq(i1,10)*4.0
      qqq(i2,17) = qqq(i1,12)*2.0
      qqq(i2,18) = qqq(i1,13)*3.0
      qqq(i2,19) = qqq(i1,14)*4.0
      qqq(i2,20) = qqq(i1,15)*5.0
      qqq(i3,2) = -1.0
      qqq(i3,4) = -2.0*xc(i)
      qqq(i3,5) = -yc(i)
      qqq(i3,7) = -qqq(i1,4)*3.0
      qqq(i3,8) = -qqq(i1,5)*2.0
      qqq(i3,9) = -qqq(i1,6)
      qqq(i3,11) = -qqq(i1,7)*4.0
      qqq(i3,12) = -qqq(i1,8)*3.0
      qqq(i3,13) = -qqq(i1,9)*2.0
      qqq(i3,14) = -qqq(i1,10)
      qqq(i3,16) = -qqq(i1,11)*5.0
      qqq(i3,17) = -qqq(i1,13)*3.0
      qqq(i3,18) = -qqq(i1,14)*2.0
      qqq(i3,19) = -qqq(i1,15)
!
!     IF NO TRANSVERSE SHEAR GO TO 113
!
      IF ( .NOT.(nots) ) THEN
         X = xc(i)
         Y = yc(i)
         CALL strpts(Ts6,nots)
         DO jj = 1 , 20
            qqq(i2,jj) = qqq(i2,jj) - Ts6(20+jj)
            qqq(i3,jj) = qqq(i3,jj) + Ts6(jj)
         ENDDO
      ENDIF
   ENDDO
   qqq(19,16) = 5.0*a**4*c
   qqq(19,17) = 3.0*a**2*c**3 - 2.0*a**4*c
   qqq(19,18) = -2.0*a*c**4 + 3.0*a**3*c**2
   qqq(19,19) = c**5 - 4.0*a**2*c**3
   qqq(19,20) = 5.0*a*c**4
   qqq(20,16) = 5.0*b**4*c
   qqq(20,17) = 3.0*b**2*c**3 - 2.0*b**4*c
   qqq(20,18) = 2.0*b*c**4 - 3.0*b**3*c**2
   qqq(20,19) = c**5 - 4.0*b**2*c**3
   qqq(20,20) = -5.0*b*c**4
!
!     FOURTH ARGUMENT IS A DUMMY LOCATION FOR INVERSE AND HENCE TS1(1)
!     IS U
!
   CALL invers(20,qqq,20,Ts6(1),0,determ,ising,index)
!
!     ISING EQUAL TO 2 IMPLIES THAT QQQ IS SINGULAR
!
!     FIRST 18 COLUMNS OF QQQ INVERSE IS THE QQQINV FOR USE IN STIFFNESS
!     MATRIX CALCULATIONS
!
   DO i = 1 , 20
      DO j = 1 , 18
         ij = (i-1)*18 + j
         Qqqinv(ij) = qqq(i,j)
      ENDDO
   ENDDO
   DO i = 1 , 36
      Balotr(i) = 0.0
   ENDDO
!
   DO i = 1 , 7
      Ph1out(i) = Est(i)
   ENDDO
   Ph1out(8) = tmem1
   Ph1out(9) = tmem3
   Ph1out(10) = tmem5
   Ph1out(11) = Est(18)
   Ph1out(12) = Est(19)
   Ph1out(13) = Est(20)
   Ph1out(14) = Est(21)
   Ph1out(15) = Est(22)
   Ph1out(16) = Est(23)
   DO jj = 1 , 4
      jj1 = jj*2 - 1
      IF ( jj/=4 ) X = xc(jj1)
      IF ( jj/=4 ) Y = yc(jj1)
      IF ( jj==4 ) X = (xc(1)+xc(3)+xc(5))/3.0
      IF ( jj==4 ) Y = (yc(1)+yc(3)+yc(5))/3.0
      IF ( jj==4 ) Ph1out(17) = (A1+A2*X+A3*Y)/2.0
      IF ( jj==4 ) Ph1out(18) = -Ph1out(17)
      DO i = 1 , 60
         Ts7(i) = 0.0
      ENDDO
      ai = Ph1out(7+jj)**3/12.0
      IF ( jj==4 ) ai = Ph1out(17)**3/1.5
      DO i = 1 , 9
         d(i) = emod(i)*ai
      ENDDO
      x2 = X*X
      xy = X*Y
      y2 = Y*Y
      x3 = x2*X
      x2y = x2*Y
      xy2 = X*y2
      y3 = y2*Y
      Ts7(4) = 2.0
      Ts7(7) = 6.0*X
      Ts7(8) = 2.0*Y
      Ts7(11) = 12.0*x2
      Ts7(12) = 6.0*xy
      Ts7(13) = 2.0*y2
      Ts7(16) = 20.0*x3
      Ts7(17) = 6.0*xy2
      Ts7(18) = 2.0*y3
      Ts7(26) = 2.0
      Ts7(29) = 2.0*X
      Ts7(30) = 6.0*Y
      Ts7(33) = 2.0*x2
      Ts7(34) = Ts7(12)
      Ts7(35) = 12.0*y2
      Ts7(37) = 2.0*x3
      Ts7(38) = 6.0*x2y
      Ts7(39) = 12.0*xy2
      Ts7(40) = 20.0*y3
      Ts7(45) = 2.0
      Ts7(48) = 4.0*X
      Ts7(49) = 4.0*Y
      Ts7(52) = 6.0*x2
      Ts7(53) = 8.0*xy
      Ts7(54) = 6.0*y2
      Ts7(57) = 12.0*x2y
      Ts7(58) = Ts7(39)
      Ts7(59) = 8.0*y3
      CALL gmmats(Ts7,3,20,0,Qqqinv,20,18,0,Ph4(1))
      CALL strpts(Ts6,nots)
      CALL gmmats(Ts6,2,20,0,Qqqinv,20,18,0,Ph4(55))
      DO ii = 1 , 6
         IF ( ics(ii)==0 ) THEN
            DO i = 1 , 3
               DO j = 1 , 6
                  i1 = (i-1)*6 + j
                  j1 = (j-1)*3 + i
                  E1(i1) = E(j1)
               ENDDO
            ENDDO
         ELSE
            j = 4*ii + 20
            CALL transs(iest(j),Trans)
            DO j = 1 , 3
               l = 6*(j-1) + 1
               m = 3*(j-1) + 1
               Balotr(l) = Trans(m)
               Balotr(l+1) = Trans(m+1)
               Balotr(l+2) = Trans(m+2)
               Balotr(l+21) = Trans(m)
               Balotr(l+22) = Trans(m+1)
               Balotr(l+23) = Trans(m+2)
            ENDDO
            CALL gmmats(E,6,3,+1,Balotr,6,6,0,E1)
         ENDIF
         kz = (ii-1)*3 + 1
         ph1ben(1) = Ph4(kz)
         ph1ben(2) = Ph4(kz+1)
         ph1ben(3) = Ph4(kz+2)
         ph1ben(4) = Ph4(kz+18)
         ph1ben(5) = Ph4(kz+19)
         ph1ben(6) = Ph4(kz+20)
         ph1ben(7) = Ph4(kz+36)
         ph1ben(8) = Ph4(kz+37)
         ph1ben(9) = Ph4(kz+38)
         CALL gmmats(d,3,3,0,ph1ben,3,3,0,dph1)
         CALL gmmats(dph1,3,3,0,E1,3,6,0,Ph2)
         mz = (ii-1)*3 + 55
         ph1shr(1) = Ph4(mz)
         ph1shr(2) = Ph4(mz+1)
         ph1shr(3) = Ph4(mz+2)
         ph1shr(4) = Ph4(mz+18)
         ph1shr(5) = Ph4(mz+19)
         ph1shr(6) = Ph4(mz+20)
         IF ( nots ) THEN
            gph1(1) = ph1shr(1)
            gph1(2) = ph1shr(2)
            gph1(3) = ph1shr(3)
            gph1(4) = ph1shr(4)
            gph1(5) = ph1shr(5)
            gph1(6) = ph1shr(6)
         ELSE
            thk = B1 + B2*X + B3*Y
            g(1) = Em(6)*thk
            g(2) = 0.0
            g(3) = 0.0
            g(4) = g(1)
            CALL gmmats(g,2,2,0,ph1shr,2,3,0,gph1)
         ENDIF
         CALL gmmats(gph1,2,3,0,E1,3,6,0,Ph3)
         DO i = 1 , 3
            DO j = 1 , 6
               i1 = (i-1)*6 + j
               i2 = i1 + 18
               j1 = (ii-1)*30 + (jj-1)*180 + i1 + 18
               j2 = j1 + 18
               Ph1out(j1) = Ph2(i1)
               IF ( i/=3 ) Ph1out(j2) = Ph3(i1)
            ENDDO
         ENDDO
      ENDDO
      jj1 = (jj-1)*3 + 1
      CALL gmmats(d,3,3,0,Alf,3,1,0,Ph1out(738+jj1))
   ENDDO
END SUBROUTINE strp11
