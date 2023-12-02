!*==pload3.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pload3
   USE c_loadx
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , SAVE :: absisa
   REAL , DIMENSION(4) :: bgpd
   REAL , DIMENSION(3,32) :: bxyz , rf
   INTEGER , DIMENSION(32) :: cid , gp , seq
   INTEGER :: col , face , i , j , k , l , m , ngp , ni , nj , nk , type
   REAL(REAL64) :: detj , pfact
   REAL(REAL64) , DIMENSION(3,32) :: dshp , f
   INTEGER , DIMENSION(4) :: ibgpd
   REAL(REAL64) , DIMENSION(3,3) :: jinv
   INTEGER , DIMENSION(3) :: n
   REAL , DIMENSION(6) :: p
   REAL(REAL64) , DIMENSION(3,2) :: s
   REAL :: sgn
   INTEGER , DIMENSION(6) , SAVE :: sgncol
   REAL(REAL64) , DIMENSION(32) :: shp
   EXTERNAL basglb , fndpnt , fndsil , ihexsd , mesage , permut , read
!
! End of declarations rewritten by SPAG
!
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
   CALL read(*100,*100,slt,p,6,0,i)
   CALL read(*100,*100,slt,gp,32,0,i)
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
   CALL permut(gp,seq,ngp,old)
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
         core(k) = core(k) + rf(j,i)
      ENDDO
   ENDDO
   RETURN
 100  CALL mesage(-61,0,0)
END SUBROUTINE pload3
