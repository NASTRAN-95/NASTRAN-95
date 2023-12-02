!*==ihex.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ihex(Temps,Pg,Type)
   USE c_matin
   USE c_matiso
   USE c_matout
   USE c_ssgwrk
   USE c_system
   USE c_trimex
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: Temps
   REAL , DIMENSION(1) :: Pg
   INTEGER :: Type
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(6) :: a , dalpha
   REAL(REAL64) :: alpvec , detj , e1 , e2 , e3 , sfact , temp
   LOGICAL :: anis , rect , tdep
   INTEGER :: bcord , bgpdt , cid , eid , gpt , i , ibgp , ii , ijk , j , jj , k , l , ngp , nip , nogo , ntlp , sil
   REAL(REAL64) , DIMENSION(3,32) :: cn
   REAL(REAL64) , DIMENSION(8) , SAVE :: gauss
   REAL(REAL64) , DIMENSION(36) :: gmat
   INTEGER , DIMENSION(46) :: ib
   INTEGER , DIMENSION(1) :: iest
   INTEGER , DIMENSION(32) :: jz
   REAL(REAL64) , DIMENSION(96) :: parg
   REAL , DIMENSION(96) :: psgl
   INTEGER , DIMENSION(6) , SAVE :: ufm
   EXTERNAL basglb , gmmatd , ihexsd , mat , mesage , page2
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     ELEMENT THERMAL LOAD GENERATOR FOR ISOPARAMETRIC SOLID ELEMENTS
!
!     TYPE = 1     CIHEX1
!     TYPE = 2     CIHEX2
!     TYPE = 3     CIHEX3
!
!***********************************************************************
!           THE EST ENTRIES ARE
!
!     NAME  ---------INDEX---------   DESCRIPTION
!            IHEX1   IHEX2   IHEX3
!
!     EID        1       1       1    ELEMENT ID NO.
!     SIL      2-9    2-21    2-33    SCALAR INDEX LIST
!     MID       10      22      34    MATERIAL ID NO.
!     CID       11      23      35    MATERIAL COORD. SYSTEM ID NO.
!     NIP       12      24      36    NO. INTEGRATION POINTS PER EDGE
!     MAXAR     13      25      37    MAX ASPECT RATIO
!     ALFA      14      26      38    MAX ANGLE FOR NORMALS
!     BETA      15      27      39    MAX ANGLE FOR MIDSIDE POINTS
!     BGPDT  16-47  28-107  40-167    BASIC GRID POINT DATA
!     GPT    48-55 108-127 168-199    GRID POINT TEMPERATURES
!***********************************************************************
!
!
!
!
!
!
!
   !>>>>EQUIVALENCE (Eid,Est(1),Iest(1)) , (Jz(1),Shp(1))
   !>>>>EQUIVALENCE (psgl(1),parg(1))
   !>>>>EQUIVALENCE (Ib(1),Bufm6(1))
!
   DATA gauss/0.577350269189626D0 , 0.555555555555556D0 , 0.774596669241483D0 , 0.888888888888889D0 , 0.347854845137454D0 ,         &
      & 0.861136311594053D0 , 0.652145154862546D0 , 0.339981043584856D0/
   DATA ufm/4H0*** , 4H USE , 4HR FA , 4HTAL  , 4HMESS , 4HAGE /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!*****
!     COMPUTE EST POINTERS
!*****
         ngp = 12*Type - 4
         mid = 10 + 12*(Type-1)
         cid = iest(mid+1)
         nip = iest(mid+2)
         IF ( nip<2 .OR. nip>4 ) nip = Type/2 + 2
         bgpdt = mid + 6
         gpt = bgpdt + 4*ngp
         DO i = 1 , ngp
            jz(i) = iest(bgpdt+4*i-4)
         ENDDO
         bcord = gpt - 3
         DO i = 2 , ngp
            DO j = 1 , 3
               k = bgpdt + 4*(ngp-i) + 4 - j
               bcord = bcord - 1
               est(bcord) = est(k)
            ENDDO
         ENDDO
         DO i = 2 , ngp
            iest(bgpdt+i-1) = jz(i)
         ENDDO
         mid = iest(ngp+2)
!
!     ABSCISSAE AND WEIGHT COEFFICIENTS FOR GAUSSIAN QUADRATURE
!
         i = nip - 1
         IF ( i==2 ) THEN
            h(1) = gauss(2)
            s(1) = gauss(3)
            h(2) = gauss(4)
            s(2) = 0.0
            h(3) = h(1)
            s(3) = -s(1)
         ELSEIF ( i==3 ) THEN
            h(1) = gauss(5)
            s(1) = gauss(6)
            h(2) = gauss(7)
            s(2) = gauss(8)
            h(3) = h(2)
            s(3) = -s(2)
            h(4) = h(1)
            s(4) = -s(1)
         ELSE
            h(1) = 1.0
            s(1) = gauss(1)
            h(2) = h(1)
            s(2) = -s(1)
         ENDIF
!
!=======================================================================
!     THIS SECTION OF CODE MUST BE UPDATED WHEN GENERAL ANISOTROPIC
!     MATERIAL IS ADDED
!
!     TEST FOR ANISOTROPIC MATERIAL
!
         anis = .FALSE.
         inflag = 10
!
!     TEST FOR RECTANGULAR COORDINATE SYSTEM IN WHICH THE ANISOTROPIC
!     MATERIAL IS DEFINED
!
         rect = .TRUE.
!=======================================================================
!
!     FETCH MATERIAL AND SET TEMPERATURE DEPENDENCE FLAG
!
         tdep = .TRUE.
         DO i = 2 , ngp
            IF ( est(gpt)/=est(gpt+i-1) ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         tdep = .FALSE.
         spag_nextblock_1 = 2
      CASE (2)
         eltemp = est(gpt)
         CALL mat(eid)
         IF ( .NOT.mtdep ) tdep = .FALSE.
         IF ( ib(46)==6 ) anis = .TRUE.
         tref = bufm6(44)
!*****
!     IF ISOTROPIC TEMPERATURE INDEPENDENT MATERIAL, COMPUTE CONSTANTS
!*****
         IF ( .NOT.(tdep) ) THEN
            IF ( anis ) THEN
!
!=======================================================================
!     CODE TO TRANSFORM GENERAL ANISOTROPIC MATERIAL PROPERTIES TO
!     BASIC COORDINATE SYSTEM MUST BE ADDED HERE
!=======================================================================
!
               DO ijk = 1 , 36
                  gmat(ijk) = bufm6(ijk)
               ENDDO
            ELSEIF ( ib(46)/=0 ) THEN
               e1 = bufm6(1)
               e2 = bufm6(2)
               e3 = bufm6(22)
               talpha = bufm6(38)
            ELSE
               CALL page2(2)
               WRITE (otpt,99001) ufm , mid , eid
               nogo = 1
               RETURN
            ENDIF
         ENDIF
         ntlp = 3*ngp
         DO i = 1 , ntlp
            parg(i) = 0.0
         ENDDO
!*****
!     BEGIN INTEGRATION LOOP NOW
!*****
         DO i = 1 , nip
            DO j = 1 , nip
               DO k = 1 , nip
                  spag_nextblock_2 = 1
                  SPAG_DispatchLoop_2: DO
                     SELECT CASE (spag_nextblock_2)
                     CASE (1)
!*****
!     GENERATE SHAPE FUNCTIONS AND JACOBIAN MATRIX INVERSE
!*****
                        CALL ihexsd(Type,shp,dshp,jacob,detj,eid,s(i),s(j),s(k),est(bcord))
!
!     JACOBIAN MATRIX WAS SINGULAR
!
                        IF ( detj==0.0D0 ) CALL mesage(-61,0,0)
!*****
!     COMPUTE PARTIAL DERIVATIVE OF SHAPE FUNCTIONS WITH RESPECT
!     TO BASIC COORDINATES
!*****
                        CALL gmmatd(dshp,ngp,3,0,jacob,3,3,0,cn)
!*****
!     COMPUTE LOADING TEMPERATURE AT THIS INTEGRATION POINT
!*****
                        temp = 0.0D0
                        DO l = 1 , ngp
                           temp = temp + shp(l)*dble(Temps(l))
                        ENDDO
                        temp = temp - dble(tref)
!*****
!     IF MATERIAL IS TEMPERATURE DEPENDENT, COMPUTE TEMPERATURE AT THIS
!     INTEGRATION POINT AND FETCH MATERIAL PROPERTIES
!*****
                        IF ( .NOT.tdep ) THEN
!*****
!     IF MATERIAL IS ANISOTROPIC AND NOT DEFINED IN RECTANGULAR COOR-
!     DINATE SYSTEM, MUST TRANSFORM TO BASIC COORDINATE SYSTEM AT THIS
!     INTEGRATION POINT
!*****
                           IF ( .NOT.anis ) THEN
                              spag_nextblock_2 = 2
                              CYCLE SPAG_DispatchLoop_2
                           ENDIF
                           IF ( rect ) THEN
                              spag_nextblock_2 = 3
                              CYCLE SPAG_DispatchLoop_2
                           ENDIF
                        ELSE
                           eltemp = 0.0D0
                           DO l = 1 , ngp
                              eltemp = eltemp + shp(l)*dble(est(gpt+l-1))
                           ENDDO
                           CALL mat(eid)
                           IF ( .NOT.(anis) ) THEN
                              IF ( ib(46)/=0 ) THEN
                                 e1 = bufm6(1)
                                 e2 = bufm6(2)
                                 e3 = bufm6(22)
                                 talpha = bufm6(38)
                                 spag_nextblock_2 = 2
                                 CYCLE SPAG_DispatchLoop_2
                              ELSE
                                 CALL page2(2)
                                 WRITE (otpt,99001) ufm , mid , eid
                                 nogo = 1
                                 RETURN
                              ENDIF
                           ENDIF
                        ENDIF
                        DO ijk = 1 , 36
                           gmat(ijk) = bufm6(ijk)
!
!=======================================================================
!     INSERT GLOBAL TO BASIC TRANSFORMATION OPERATIONS HERE FOR
!     ANISOTROPIC MATERIAL MATRIX
                        ENDDO
                        spag_nextblock_2 = 3
                     CASE (2)
!=======================================================================
!*****
!     COMPUTE CONTRIBUTION TO THERMAL LOAD VECTOR FOR ISOTROPIC MATERIAL
!*****
                        alpvec = dble(talpha)*(e1+2.0*e2)
                        sfact = h(i)*h(j)*h(k)*detj*alpvec*temp
                        l = 0
                        DO ii = 1 , ngp
                           DO jj = 1 , 3
                              l = l + 1
                              parg(l) = sfact*cn(jj,ii) + parg(l)
                           ENDDO
                        ENDDO
                     CASE (3)
!=======================================================================
!     ADD LOAD COMPUTATIONS FOR ANISOTROPIC MATERIAL HERE
!=======================================================================
!
                        sfact = h(i)*h(j)*h(k)*detj*temp
                        DO ijk = 1 , 6
                           dalpha(ijk) = bufm6(ijk+37)
                        ENDDO
!
                        CALL gmmatd(gmat,6,6,0,dalpha,6,1,0,a(1))
                        l = 0
                        DO ii = 1 , ngp
                           l = l + 1
                           parg(l) = parg(l) + sfact*(cn(1,ii)*a(1)+cn(2,ii)*a(4)+cn(3,ii)*a(6))
                           l = l + 1
                           parg(l) = parg(l) + sfact*(cn(2,ii)*a(2)+cn(1,ii)*a(4)+cn(3,ii)*a(5))
                           l = l + 1
                           parg(l) = parg(l) + sfact*(cn(3,ii)*a(3)+cn(2,ii)*a(5)+cn(1,ii)*a(6))
                        ENDDO
                        EXIT SPAG_DispatchLoop_2
                     END SELECT
                  ENDDO SPAG_DispatchLoop_2
               ENDDO
            ENDDO
         ENDDO
         DO i = 1 , ntlp
            psgl(i) = parg(i)
         ENDDO
!*****
!     INSERT THERMAL LOAD INTO GLOBAL LOAD VECTOR  (PG ARRAY)
!*****
!
         DO i = 1 , ngp
            sil = iest(i+1)
            ibgp = bgpdt + i - 1
            IF ( iest(ibgp)/=0 ) CALL basglb(psgl(3*i-2),psgl(3*i-2),est(bcord+3*i-3),iest(ibgp))
            DO j = 1 , 3
               Pg(sil+j-1) = Pg(sil+j-1) + psgl(3*i-3+j)
            ENDDO
         ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
!
99001 FORMAT (6A4,69H4005. AN ILLEGAL VALUE OF -NU- HAS BEEN SPECIFIED UNDER MATERIAL ID =,I10,17H FOR ELEMENT ID =,I10)
END SUBROUTINE ihex
