
SUBROUTINE pload3
   IMPLICIT NONE
   REAL Bgpdt , Core(1) , Old , Slt
   INTEGER Lcore
   COMMON /loadx / Lcore , Slt , Bgpdt , Old
   COMMON /zzzzzz/ Core
   DOUBLE PRECISION absisa , detj , dshp(3,32) , f(3,32) , jinv(3,3) , pfact , s(3,2) , shp(32)
   REAL bgpd(4) , bxyz(3,32) , p(6) , rf(3,32) , sgn
   INTEGER cid(32) , col , face , gp(32) , i , ibgpd(4) , j , k , l , m , n(3) , ngp , ni , nj , nk , seq(32) , sgncol(6) , type
!
!     COMPUTES THE CONTRIBUTION TO THE LOAD VECTOR DUE TO PRESSURES
!     APPLIED TO THE FACES OF ISOPARAMETRIC SOLID ELEMENTS
!
!
!
!
!
   !>>>>EQUIVALENCE (bgpd(1),ibgpd(1),dshp(1,1))
   !>>>>EQUIVALENCE (seq(1),shp(1))
   !>>>>EQUIVALENCE (n(1),ni) , (n(2),nj) , (n(3),nk)
   !>>>>EQUIVALENCE (f(1,1),rf(1,1))
!
   DATA absisa/0.577350269189626D0/
   DATA sgncol/ - 3 , -2 , 1 , 2 , -1 , 3/
!
!     READ PRESSURES AND GRID POINT ID S FROM THE SLT, DETERMINE
!     ELEMENT TYPE AND NUMBER OF GRID POINTS AND GET BASIC COORDINATES.
!
   CALL read(*100,*100,Slt,p,6,0,i)
   CALL read(*100,*100,Slt,gp,32,0,i)
   type = 1
   ngp = 8
   IF ( gp(9)/=0 ) THEN
      type = 2
      ngp = 20
      IF ( gp(21)/=0 ) THEN
         type = 3
         ngp = 32
      ENDIF
   ENDIF
   CALL permut(gp,seq,ngp,Old)
   DO i = 1 , ngp
      j = seq(i)
      CALL fndpnt(bgpd,gp(j))
      cid(j) = ibgpd(1)
      DO k = 1 , 3
         f(k,i) = 0.0
         bxyz(k,j) = bgpd(k+1)
      ENDDO
   ENDDO
!
!     LOOP OVER SIX ELEMENT FACES
!
   DO face = 1 , 6
      IF ( p(face)/=0.0 ) THEN
         j = 1
         i = isign(j,sgncol(face))
         sgn = float(i)
         col = iabs(sgncol(face))
         DO i = 1 , 3
            IF ( i/=col ) THEN
               s(i,1) = -absisa
               s(i,2) = absisa
               n(i) = 2
            ELSE
               s(i,1) = sgn
               n(i) = 1
            ENDIF
         ENDDO
!
!     INTEGRATION LOOPS
!
         DO i = 1 , ni
            DO j = 1 , nj
               DO k = 1 , nk
!
!     GENERATE SHAPE FUNCTIONS AND JACOBIAN MATRIX INVERSE.
!
                  CALL ihexsd(type,shp,dshp,jinv,detj,0,s(1,i),s(2,j),s(3,k),bxyz)
                  IF ( detj==0.0 ) CALL mesage(-61,0,0)
                  pfact = detj*dble(sgn*p(face))
!
!     LOOP OVER GRID POINTS
!
                  DO l = 1 , ngp
                     IF ( shp(l)/=0.0 ) THEN
                        DO m = 1 , 3
                           f(m,l) = pfact*jinv(m,col)*shp(l) + f(m,l)
                        ENDDO
                     ENDIF
                  ENDDO
               ENDDO
            ENDDO
         ENDDO
      ENDIF
   ENDDO
   j = 3*ngp
   DO i = 1 , j
      rf(i,1) = f(i,1)
   ENDDO
!
!     TRANSFORM VECTOR TO GLOBAL AND ADD TO GLOBAL LOAD VECTOR.
!
   DO i = 1 , ngp
      IF ( cid(i)/=0 ) CALL basglb(rf(1,i),rf(1,i),bxyz(1,i),cid(i))
      CALL fndsil(gp(i))
      DO j = 1 , 3
         k = gp(i) + j - 1
         Core(k) = Core(k) + rf(j,i)
      ENDDO
   ENDDO
   RETURN
 100  CALL mesage(-61,0,0)
END SUBROUTINE pload3