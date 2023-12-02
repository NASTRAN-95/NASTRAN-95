!*==cyct1.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cyct1
USE C_BLANK
USE C_PACKX
USE C_SYSTEM
USE C_XMSSG
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: buf , icol , icos , iprec , isin , j , jcol , jcos , jsin , k , kcol , kcol2 , kcos , kmax , kmin , ksin , ktrig , n ,&
            & ncol , ncos , nloads , nloadt , nsin , ntrig , numrow , nxcol , outpt , precis , sysbuf
   REAL(REAL64) :: dc , dc1 , dfac , dfak , ds , ds1
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER , SAVE :: gcyc , hback , hdrl , hdsa , hrot , scrt , vin , vout
   LOGICAL :: lback , lcos , ldrl , ldsa , lnmult , lvin , lvout
   INTEGER , DIMENSION(7) :: mcb
   REAL :: rn
   REAL , DIMENSION(1) :: rz
   INTEGER , DIMENSION(2) , SAVE :: subr
   EXTERNAL close , gopen , korsz , makmcb , mesage , pack , rdtrl , ssg2b , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     GENERATE CYCLIC TRANSFORMATION MATRIX, TRANSFORM VECTORS
!
!     DMAP CALLING SEQUENCE
!
!     CYCT1   VIN/VOUT,GCYC/V,Y,CTYPE/V,Y,CDIR/V,Y,N/V,Y,KMAX/
!             V,Y,NLOAD/V,N,NOGO $
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Outpt) , (Ksystm(55),Iprec) , (Iz(1),Rz(1),Dz(1))
   DATA subr/4HCYCT , 4H1   / , hback/4HBACK/
   DATA vin/101/ , vout , gcyc/201 , 202/ , scrt/301/
   DATA hrot , hdrl , hdsa/4HROT  , 4HDRL  , 4HDSA /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
!     FIND NECESSARY PARAMETERS
!
         Nogo = 1
         precis = 2
         IF ( iprec/=2 ) precis = 1
         ldrl = Ctype(1)==hdrl
         ldsa = Ctype(1)==hdsa
         IF ( .NOT.((Ctype(1)==hrot) .OR. ldrl .OR. ldsa) ) THEN
!
!     FATAL MESSAGES
!
            Nogo = -1
            WRITE (outpt,99001) Ufm , Ctype(1)
99001       FORMAT (A23,' 4063, ILLEGAL VALUE (',A4,') FOR PARAMETER CTYPE.')
         ENDIF
         lback = Cdir(1)==hback
!
!     CURRENT DOCUMENTED USAGE DOES NOT USE NEGATIVE VALUES OF KMAXI
!     OTHER THAN THE DEFAULT OF -1     10/02/73
!     LOGIC IS INCLUDED IN THE ROUTINE TO USE NEGATIVE KMAXI BUT IS NOT
!     FULLY CHECKED OUT.  THE FOLLOWING STATEMENT NEGATES ALL THIS LOGIC
!
         IF ( Kmaxi<0 ) Kmaxi = Nn/2
         kmax = Kmaxi
         kmin = 0
         IF ( kmax<0 ) THEN
            kmax = -kmax
            kmin = kmax
         ENDIF
         IF ( 2*kmax>Nn .OR. Nn<=0 ) THEN
            Nogo = -1
            WRITE (outpt,99002) Ufm , Nn , Kmaxi
99002       FORMAT (A23,' 4064, ILLEGAL VALUES (',I8,1H,,I8,') FOR PARAMETERS (NSEGS,KMAX).')
         ENDIF
         IF ( Nload<=0 ) THEN
            Nogo = -1
            WRITE (outpt,99003) Ufm , Nload
99003       FORMAT (A23,' 4065, ILLEGAL VALUE (',I8,') FOR PARAMETER NLOAD.')
         ENDIF
         nloads = Nload
         IF ( ldsa ) nloads = 2*Nload
         nloadt = Nload
         IF ( ldrl .OR. ldsa ) nloadt = 2*Nload
         numrow = Nn
         IF ( lback ) THEN
            numrow = 2*(kmax-kmin+1)
            IF ( kmin==0 ) numrow = numrow - 1
            IF ( 2*kmax==Nn ) numrow = numrow - 1
         ENDIF
         numrow = nloadt*numrow
!
!     DEFINE OPEN CORE POINTERS AND GINO BUFFER
!                POINTERS                BEGIN    END
!          TABLE OF COS (2.0*PI*N/NN)     ICOS    NCOS
!          TABLE OF SIN (2.0*PI*N/NN)     ISIN    NSIN
!          AREA TO ASSEMBLE COLUMNS       ICOL    NCOL
!          (NOTE  N = LITTLE N, NN = CAPITAL N)   N = 0,(NN-1)
!          (ALLOW FOR (NLOADS-1) ZEROS BEFORE FIRST ENTRY IN COL)
!
         buf = korsz(Iz) - sysbuf + 1
         icos = 1
         ncos = icos + Nn - 1
         isin = ncos + 1
         nsin = isin + Nn - 1
         icol = nsin + 1
         jcol = icol + nloads - 1
         ncol = jcol + numrow - 1
         IF ( 2*ncol>=buf ) CALL mesage(-8,0,subr)
!
!     CHECK DATA BLOCK TRAILERS
!
         mcb(1) = gcyc
         CALL rdtrl(mcb(1))
         IF ( mcb(1)<=0 ) THEN
            Nogo = -1
            WRITE (outpt,99004) Ufm
99004       FORMAT (A23,' 4066, SECOND OUTPUT DATA BLOCK MUST NOT BE PURGED.')
         ENDIF
         mcb(1) = vout
         CALL rdtrl(mcb(1))
         lvout = mcb(1)>0
         mcb(1) = vin
         CALL rdtrl(mcb(1))
         lvin = mcb(1)>0
         IF ( .NOT.lvin ) mcb(2) = 0
         lnmult = mcb(2)/=numrow
         IF ( lvin .AND. lvout .AND. lnmult ) THEN
            Nogo = -1
            WRITE (outpt,99005) Ufm , mcb(2) , numrow
99005       FORMAT (A23,' 4067, VIN HAS',I9,' COLS, GCYC HAS',I9,6H ROWS.)
            CALL mesage(-61,0,subr)
            RETURN
         ELSEIF ( Nogo<=0 ) THEN
            CALL mesage(-61,0,subr)
            RETURN
         ELSE
!
!     THE PARAMETERS ARE OK
!     PREPARE TRIGONOMETRIC TABLES,  DC1=COS(2*PI/NN), PI = 4*ATAN(1)
!     MOVABLE POINTERS  JXXX=N , KXXX= NN-N
!
            rn = float(Nn)
            dfac = (8.0D0*datan(1.0D0))/dble(rn)
            dc1 = dcos(dfac)
            ds1 = dsin(dfac)
            jcos = icos
            kcos = ncos + 1
            jsin = isin
            ksin = nsin + 1
            dz(jcos) = 1.0D0
            dz(jsin) = 0.0D0
         ENDIF
         SPAG_Loop_1_1: DO
            IF ( kcos-jcos<2 ) THEN
!
!     ZERO THE AREA FOR FORMING THE COLUMN
!
               DO j = icol , ncol
                  dz(j) = 0.0D0
               ENDDO
!
!     OPEN GCYC MATRIX,  GET READY TO USE PACK
!
               CALL gopen(gcyc,Iz(buf),1)
               CALL makmcb(mcb,gcyc,numrow,2,precis)
               Pkin = 2
               Pkout = precis
               Pkirow = 1
               Pknrow = numrow
               Pkincr = 1
               IF ( lback ) THEN
!
!     START LOOPING ON COLUMNS OF MATRIX OF TYPE BACK
!         N = 1,NN
!
                  n = 1
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ELSE
!
!     START LOOPING ON COLUMNS OF MATRIX OF TYPE FORE.
!     FORM A COLUMN AND PACK IT OUT
!          K = KMIN,KMAX  ALTERNATE COSINE AND SINE COLUMNS
!
                  dfac = 2.0D0/dble(rn)
                  IF ( ldrl ) dfac = 0.5D0*dfac
                  k = kmin
                  EXIT SPAG_Loop_1_1
               ENDIF
            ELSEIF ( kcos-jcos==2 ) THEN
               dc = -1.0D0
               ds = 0.0D0
            ELSE
               dc = dc1*dz(jcos) - ds1*dz(jsin)
               ds = ds1*dz(jcos) + dc1*dz(jsin)
            ENDIF
            jcos = jcos + 1
            jsin = jsin + 1
            kcos = kcos - 1
            ksin = ksin - 1
            dz(jcos) = dc
            dz(jsin) = ds
            dz(kcos) = dc
            dz(ksin) = -ds
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 2
      CASE (2)
         dfak = dfac
         IF ( k==0 .OR. 2*k==Nn ) dfak = 0.5D0*dfak
         lcos = .TRUE.
         ktrig = icos
         ntrig = ncos
         spag_nextblock_1 = 3
      CASE (3)
         DO kcol = jcol , ncol , nloadt
            dz(kcol) = dfak*dz(ktrig)
            ktrig = ktrig + k
            IF ( ktrig>ntrig ) ktrig = ktrig - Nn
         ENDDO
         spag_nextblock_1 = 4
      CASE (4)
!
!     PACK OUT NLOADT COLUMNS  (FOR EITHER FORE OR BACK)
!      IF  ROT OR DSA   WE ARE READY
!      IF     DRL       PRODUCE INTERMEDIATE TERMS FIRST (EXPAND)
!
         nxcol = 1
         IF ( ldrl ) THEN
            DO kcol = jcol , ncol , nloadt
               kcol2 = kcol + nloads
               dz(kcol2) = dz(kcol)
            ENDDO
         ENDIF
         kcol = jcol
         SPAG_Loop_1_2: DO
            CALL pack(dz(kcol),gcyc,mcb)
            kcol = kcol - 1
            IF ( kcol<icol ) THEN
               IF ( ldrl .AND. nxcol==1 ) THEN
                  nxcol = 2
                  DO kcol = jcol , ncol , nloadt
                     kcol2 = kcol + nloads
                     dz(kcol2) = -dz(kcol)
                  ENDDO
                  kcol = jcol
               ELSEIF ( lback ) THEN
!
!     BOTTOM OF LOOP FOR TYPE BACK
!
                  n = n + 1
                  IF ( n<=Nn ) EXIT SPAG_Loop_1_2
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
!
!     BOTTOM OF LOOP FOR TYPE FORE
!
               ELSEIF ( k/=0 .AND. 2*k/=Nn .AND. lcos ) THEN
                  lcos = .FALSE.
                  ktrig = isin
                  ntrig = nsin
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  k = k + 1
                  IF ( k<=kmax ) THEN
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_1_2
         spag_nextblock_1 = 5
      CASE (5)
         k = 0
         kcos = icos
         kcol = jcol
         DO
            IF ( k>=kmin ) THEN
               dz(kcol) = dz(kcos)
               kcol = kcol + nloadt
               IF ( k/=0 .AND. 2*k/=Nn ) THEN
                  dz(kcol) = dz(kcos+Nn)
                  kcol = kcol + nloadt
               ENDIF
            ENDIF
            kcos = kcos + n - 1
            IF ( kcos>ncos ) kcos = kcos - Nn
            k = k + 1
            IF ( k>kmax ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 6
      CASE (6)
!
!     THE GCYC MATRIX IS NOW COMPLETE
!
         CALL close(gcyc,1)
         CALL wrttrl(mcb(1))
!
!     IF WE HAVE TO FORM VOUT, USE SSG2B.  (VOUT = VIN*GCYC)
!
         IF ( .NOT.(lnmult) ) CALL ssg2b(vin,gcyc,0,vout,0,precis,1,scrt)
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE cyct1
