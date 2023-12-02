!*==normal.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE normal
   USE c_blank
   USE c_packx
   USE c_system
   USE c_type
   USE c_unpakx
   USE c_xmssg
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: dx , x , xmax , xx
   REAL(REAL64) :: dxmax
   REAL(REAL64) , SAVE :: dzero
   INTEGER :: i , ibuf1 , ibuf2 , icore , icrreq , iopt1 , iprec , itype , ivec , ivec1 , ivec2 , j , k , kwords , mwords , nrow2 , &
            & nrowp , nwords
   INTEGER , SAVE :: iblnk , isrss , matin , matout , max
   INTEGER , DIMENSION(2) , SAVE :: isubnm
   INTEGER , DIMENSION(7) :: mcb
   REAL , DIMENSION(1) :: z
   REAL(REAL64) , DIMENSION(1) :: zd
   EXTERNAL close , gopen , korsz , mesage , pack , rdtrl , unpack , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS IS THE DRIVER FOR THE NORM MODULE.
!
!     NORM        INMAT/OUTMAT/S,N,NCOL/S,N,NROW/S,N,XNORM/V,Y,IOPT $
!
!     DEPENDING ON THE VALUE OF IOPT, THIS MODULE PERFORMS THE
!     FOLLOWING FUNCTIONS --
!
!     IOPT = 'MAX'
!                 NORM GENERATES A MATRIX.  EACH COLUMN OF THIS OUTPUT
!                 MATRIX REPRESENTS A COLUMN OF THE INPUT MATRIX
!                 NORMALIZED BY ITS LARGEST ROW ELEMENT. (DEFAULT)
!
!     IOPT = 'SRSS'
!                 NORM GENERATES A COLUMN VECTOR.  EACH ELEMENT OF THIS
!                 VECTOR REPRESENTS THE SQUARE ROOT OF THE SUM OF THE
!                 SQUARES (SRSS) OF THE CORRESPONDING ROW OF THE INPUT
!                 MATRIX.
!
!
!
!     INPUT DATA BLOCK --
!
!     INMAT     - ANY MATRIX
!
!     OUTPUT DATA BLOCK --
!
!     OUTMAT    - OUTPUT MATRIX GENERATED AS DESCRIBED BELOW
!
!     PARAMETERS --
!
!     NCOL      - NO. OF COLUMNS OF THE INPUT MATRIX (OUTPUT/INTEGER)
!
!     NROW      - NO. OF ROWS OF THE INPUT MATRIX (OUTPUT/INTEGER)
!
!     XNORM     - MAX. NORMALIZING OR SRSS VALUE, DEPENDING UPON THE
!                 IOPT VALUE SPECIFIED (OUTPUT/REAL)
!     IOPT      - OPTION INDICATING WHETHER EACH COLUMN OF THE INPUT
!                 MATRIX IS TO BE NORMALIZED BY THE MAXIMUM ROW ELEMENT
!                 IN THAT COLUMN OR WHETHER THE SRSS VALUE FOR EACH ROW
!                 OF THE INPUT MATRIX IS TO BE COMPUTED (INPUT/BCD)
!
!     THIS MODULE DEVELOPED BY P. R. PAMIDI OF RPK CORPORATION,
!     MARCH 1988
!
   !>>>>EQUIVALENCE (Iz(1),Z(1),Zd(1)) , (Iopt1,Iopt(1))
   DATA matin , matout/101 , 201/
   DATA isubnm , max , isrss , iblnk , dzero/4HNORM , 4HAL   , 4HMAX  , 4HSRSS , 4H     , 0.0D+0/
!
   IF ( .NOT.(iopt(2)==iblnk .AND. (iopt1==max .OR. iopt1==isrss)) ) THEN
      WRITE (nout,99001) ufm , iopt
99001 FORMAT (A23,', ILLEGAL BCD VALUE (',2A4,') FOR THE 4TH PARAMATER',' IN MODULE NORM')
      CALL mesage(-61,0,0)
   ENDIF
   incru = 1
   incrp = 1
   icore = korsz(iz)
   ibuf1 = icore - isysbf + 1
   ibuf2 = ibuf1 - isysbf
   icore = ibuf2 - 1
   CALL gopen(matin,iz(ibuf1),0)
   CALL gopen(matout,iz(ibuf2),1)
   mcb(1) = matin
   CALL rdtrl(mcb)
   ncol = mcb(2)
   nrow = mcb(3)
   nrow2 = 2*nrow
   itype = mcb(5)
   iprec = itype
   IF ( iprec>2 ) iprec = iprec - 2
   iunout = itype
   ipkot1 = itype
   ipkot2 = itype
   nrowp = iprec*nrow
   nwords = nwds(itype)
   mwords = nrow*nwords
   kwords = mwords
   IF ( iopt1/=max ) kwords = kwords + nrowp
   icrreq = kwords - icore
   IF ( icrreq>0 ) CALL mesage(-8,icrreq,isubnm)
   ivec = mwords
   ivec1 = ivec + 1
   ivec2 = ivec + nrowp
   IF ( iopt1/=max ) THEN
      mcb(5) = iprec
      ipkot1 = iprec
      ipkot2 = iprec
      DO i = ivec1 , ivec2
         z(i) = 0.0
      ENDDO
   ENDIF
   mcb(1) = matout
   mcb(2) = 0
   mcb(6) = 0
   mcb(7) = 0
   iu1 = 1
   iu2 = nrow
!
   xxmax = 0.0
   DO i = 1 , ncol
      spag_nextblock_1 = 1
      SPAG_DispatchLoop_1: DO
         SELECT CASE (spag_nextblock_1)
         CASE (1)
            xx = 0.0
            CALL unpack(*10,matin,z)
            ip1 = iu1
            ip2 = iu2
            xmax = -1.0
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
 10         ip1 = 1
            ip2 = 1
            xmax = 0.0
            DO j = 1 , nwords
               z(j) = 0.0
            ENDDO
            spag_nextblock_1 = 2
         CASE (2)
!
            IF ( iopt1/=isrss ) THEN
               IF ( xmax/=0.0 ) THEN
!
!     OPTION IS MAX
!
                  IF ( itype==2 ) THEN
!
                     dxmax = dzero
                     DO j = 1 , nrow
                        dx = dabs(zd(j))
                        IF ( dx>dxmax ) dxmax = dx
                     ENDDO
                     IF ( dxmax/=dzero ) THEN
                        xx = dxmax
                        DO j = 1 , nrow
                           zd(j) = zd(j)/dxmax
                        ENDDO
!
                        IF ( xx>xxmax ) xxmax = xx
                     ENDIF
                  ELSEIF ( itype==3 ) THEN
!
                     xmax = 0.0
                     DO j = 1 , nrow2 , 2
                        x = sqrt(z(j)*z(j)+z(j+1)**2)
                        IF ( x>xmax ) xmax = x
                     ENDDO
                     IF ( xmax/=0.0 ) THEN
                        xx = xmax
                        DO j = 1 , nrow2 , 2
                           z(j) = z(j)/xmax
                           z(j+1) = z(j+1)/xmax
                        ENDDO
                        IF ( xx>xxmax ) xxmax = xx
                     ENDIF
                  ELSEIF ( itype==4 ) THEN
!
                     dxmax = dzero
                     DO j = 1 , nrow2 , 2
                        dx = dsqrt(zd(j)*zd(j)+zd(j+1)**2)
                        IF ( dx>dxmax ) dxmax = dx
                     ENDDO
                     IF ( dxmax/=dzero ) THEN
                        xx = dxmax
                        DO j = 1 , nrow2 , 2
                           zd(j) = zd(j)/dxmax
                           zd(j+1) = zd(j+1)/dxmax
                        ENDDO
                        IF ( xx>xxmax ) xxmax = xx
                     ENDIF
                  ELSE
!
                     xmax = 0.0
                     DO j = 1 , nrow
                        x = abs(z(j))
                        IF ( x>xmax ) xmax = x
                     ENDDO
                     IF ( xmax/=0.0 ) THEN
                        xx = xmax
                        DO j = 1 , nrow
                           z(j) = z(j)/xmax
                        ENDDO
                        IF ( xx>xxmax ) xxmax = xx
                     ENDIF
                  ENDIF
               ENDIF
               CALL pack(z,matout,mcb)
!
!     OPTION IS SRSS
!
            ELSEIF ( xmax/=0.0 ) THEN
               IF ( itype==2 ) THEN
!
                  DO j = 1 , nrow
                     k = ivec + j
                     zd(k) = zd(k) + zd(j)*zd(j)
                  ENDDO
               ELSEIF ( itype==3 ) THEN
!
                  k = ivec
                  DO j = 1 , nrow2 , 2
                     k = k + 1
                     z(k) = z(k) + z(j)*z(j) + z(j+1)**2
                  ENDDO
               ELSEIF ( itype==4 ) THEN
!
                  k = ivec
                  DO j = 1 , nrow2 , 2
                     k = k + 1
                     zd(k) = zd(k) + zd(j)*zd(j) + zd(j+1)**2
                  ENDDO
               ELSE
!
                  DO j = 1 , nrow
                     k = ivec + j
                     z(k) = z(k) + z(j)*z(j)
                  ENDDO
               ENDIF
            ENDIF
            EXIT SPAG_DispatchLoop_1
         END SELECT
      ENDDO SPAG_DispatchLoop_1
!
   ENDDO
   CALL close(matin,1)
   IF ( iopt1/=max ) THEN
!
      ip1 = iu1
      ip2 = iu2
      IF ( iprec==2 ) THEN
!
         DO i = ivec1 , ivec2
            zd(i) = dsqrt(zd(i))
            IF ( zd(i)>xxmax ) xxmax = zd(i)
         ENDDO
      ELSE
!
         DO i = ivec1 , ivec2
            z(i) = sqrt(z(i))
            IF ( z(i)>xxmax ) xxmax = z(i)
         ENDDO
      ENDIF
!
      CALL pack(z(ivec1),matout,mcb)
   ENDIF
!
   CALL close(matout,1)
   CALL wrttrl(mcb)
END SUBROUTINE normal
