!*==fvrst1.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fvrst1
USE C_BLANK
USE C_CONDAD
USE C_PACKX
USE C_SYSTEM
USE C_UNPAKX
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(3,3) :: a , b , c , ta
   REAL(REAL64) :: avgm
   INTEGER , SAVE :: b1gg , basexg , bgpdt , casecc , cstm , dit , frl , frlx , m1gg , m2gg , mgg , pdzero
   INTEGER , DIMENSION(14) :: case
   REAL , DIMENSION(4) :: coord
   REAL :: dummy
   INTEGER :: frqset , i , ibuf1 , ibuf2 , ibuf3 , ibuf4 , ibuf5 , icstm , ifrl , ifrlx , ii , iitid , iltab , index , ip1 , ipdz , &
            & itab , j , jj , k , kk , kkk , l , lcstm , ll , loc , ltab , m , mm , n1 , n2 , n3 , ncolc , ncrd , ncsym , ndof ,    &
            & next , nfile , nform , nfs , nfsets , nfsx , ngrid , nrow , nrowc , nt , ntabl , nterm , ntype , nwds , nwrds , nz ,  &
            & nztab
   INTEGER , DIMENSION(6) :: itid
   INTEGER , DIMENSION(13) , SAVE :: itlist
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(7) :: mcb , mcb1 , mcb2 , mcbb1 , mcbm1 , mcbm2
   LOGICAL :: modfrl
   INTEGER , DIMENSION(3) , SAVE :: modnam
   REAL(REAL64) , DIMENSION(3) :: row
   REAL , DIMENSION(3) :: row2
   REAL , DIMENSION(1) :: zs
   EXTERNAL close , fvrs1a , fvrs1e , fwdrec , gmmatd , gopen , korsz , mesage , open , pack , pretab , pretrd , rdtrl , read ,     &
          & transd , unpack , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!
!    1. ENTRY POINT - FVRST1
!
!    2. PURPOSE -  THIS MODULE IS USED FOR FORCED VIBRATION RESPONSE
!                  ANALYSIS OF ROTATING CYCLIC STRUCTURES.
!                  FVRSTR1 GENERATES DATA BLOCKS FRLX, B1GG, M1GG,
!                  M2GG, BASEXG AND PDZERO. IT ALSO COMPUTES PARAMETERS
!                  FKMAX AND NOBASEX.
!
!    3. DMAP CALLING SEQUENCE -
!
!         FVRSTR1  CASECC,BGPDT,CSTM,DIT,FRL,MGG,, / FRLX,B1GG,M1GG,
!                  M2GG,BASEXG,PDZERO,, /V,N,NOMGG/V,Y,CYCIO/V,Y,NSEGS/
!                  V,Y,KMAX/V,N,FKMAX/V,Y,BXTID=-1/V,Y,BXPTID=-1/
!                  V,Y,BYTID=-1/V,Y,BYPTID=-1/V,Y,BZTID=-1/
!                  V,Y,BZPTID=-1/V,N,NOBASEX/V,N,NOFREQ/V,N,OMEGA  $
!
!    4. INPUT DATA BLOCKS -
!
!         CASECC - CASE CONTROL
!         BGPDT  - BASIC GRID POINT DEFINITION TABLE.
!         CSTM   - COORDINATE SYSTEM TRANSFORMATION MATRICES.
!         DIT    - DIRECT INPUT TABLES.
!         FRL    - FREQUENCY RESPONSE LIST. (FREQUENCIES IN RADIANS)
!         MGG    - GLOBAL MASS MATRIX (G-SET).
!
!         NOTE   - (1) ALL INPUT DATA BLOCKS CAN BE PURGED IF ONLY
!                      PARAMETERS FKMAX AND NOBASEX ARE TO BE COMPUTED.
!                  (2) CASECC, DIT AND FRL CAN BE PURGED IF FRLX AND
!                      BASEXG ARE PURGED.
!
!    5. OUTPUT DATA BLOCKS -
!
!         FRLX    - FREQUENCY RESPONSE LIST (MODIFIED).
!         B1GG    - CORIOLIS ACCELERATION COEFFICIENT MATRIX (G-SET).
!         M1GG    - CENTRIPETAL ACCELERATION COEFFICIENT MATRIX (G-SET).
!         M2GG    - BASE ACCELERATION COEFFICIENT MATRIX (G-SET).
!         BASEXG  - BASE ACCELERATION MATRIX (G-SET).
!         PDZERO  - LOAD MODIFICATION MATRIX IN BASE ACCELERATION
!                   PROBLEMS.
!
!         NOTE    - (1) ALL OUTPUT DATA BLOCKS CAN BE PURGED IF
!                       PARAMETER NOMGG =-1.
!                   (2) B1GG AND M1GG CAN BE PURGED IF NOMGG =-1 OR
!                       IF OMEGA = 0.0.
!                   (3) FRLX AND PDZERO CAN BE PURGED IF OMEGA = 0.0.
!                   (4) FRLX, PDZERO, M2GG AND BASEXG CAN BE PURGED
!                       IF NOMGG =-1 OR NOFREQ =-1 OR CYCIO =+1 OR IF
!                       ALL PARAMETERS BXTID = BXPTID = BYTID =-1.
!
!    6. PARAMETERS -
!
!        (A) NOMGG   - INPUT-INTEGER-NO DEFAULT.  MASS MATRIX WAS NOT
!                      GENERATED IF NOMGG =-1.
!        (B) CYCIO   - INPUT-INTEGER-NO DEFAULT.  THE INTEGER VALUE
!                      OF THIS PARAMETER SPECIFIES THE FORM OF THE INPUT
!                      AND OUTPUT DATA FOR CYCLIC STRUCTURES. A VALUE
!                      OF +1 IS USED TO SPECIFY PHYSICAL SEGMENT REPRE-
!                      SENTATION AND A VALUE OF -1 FOR CYCLIC TRANSFOR-
!                      MATION REPRESENTATION.
!        (C) NSEGS   - INPUT-INTEGER-NO DEFAULT.  THE NUMBER OF
!                      IDENTICAL SEGMENTS IN THE STRUCTURAL MODEL.
!        (D) KMAX    - INPUT-INTEGER-NO DEFAULT.  THE INTEGER VALUE
!                      OF THIS PARAMETER SPECIFIES THE MAXIMUM VALUE
!                      OF THE HARMONIC INDEX.THE MAXIMUM VALUE OF
!                      KMAX IS NSEGS/2.
!        (E) FKMAX   - OUTPUT-INTEGER-NO DEFAULT.  FUNCTION OF KMAX.
!        (F) BXTID   - INPUT -INTEGER-DEFAULTS.  THE VALUES OF THESE
!        (G) BYTID     PARAMETERS DEFINE THE SET IDENTIFICATION NUMBERS
!        (H) BZTID     OF THE TABLEDI BULK DATA CARDS WHICH DEFINE THE
!        (I) BXPTID    COMPONENTS OF THE BASE ACCELERATION VECTOR. THE
!        (J) BYPTID    TABLES REFERED TO BY BXTID, BYTID AND BZTID
!        (K) BZPTID    DEFINE MAGNITUDE(LT-2) AND THE TABLES REFERED TO
!                      BY BXPTID, BYPTID AND BZPTID DEFINE PHASE(DEGREE)
!                      THE DEFAULT VALUES ARE -1 WHICH MEANS THAT THE
!                      RESPECTIVE TERMS ARE IGNORED.
!        (L) NOBASEX - OUTPUT-INTEGER-NO DEFAULT.  NOBASEX =-1 IF DATA
!                      BLOCK BASEXG IS NOT GENERATED.
!        (M) NOFREQ  - INPUT-INTEGER-NO DEFAULT. NOFREQ =-1 IF FREQUENCY
!                      WAS NOT SELECTED IN THE CASE CONTROL DECK.
!        (N) OMEGA   - INPUT-REAL-NO DEFAULT.  ROTATIONAL SPEED OF THE
!                      STRUCTURE IN RADIANS. OMEGA = 2*PI*RPS.
!
!    7. METHOD -  SEE FUNCTIONAL MODULE DESCRIPTION.
!
!    8. SUBROUTINES - FVRST1 CALLS ROUTINES FVRS1A, FVRS1B, FVRS1C,
!                     FVRS1D, FVRS1E, GMMATD, PRETRD, TRANSD, PRETAB,
!                     TAB AND OTHER STANDARD NASTRAN UTILITY ROUTINES.
!                     GINO ROUTINES.
!
!    9. DESIGN REQUIREMENTS -
!
!         (1) OPEN CORE IS DEFINED AT /ZZFVR1/.
!         (2) NO SCRATCH FILES ARE USED.
!         (3) FVRST1 RESIDES IN LINKNS07
!         (4) OPEN CORE FOR 5 BUFFERS PLUS 14*NCSTM  PLUS NTYPE*NROW OF
!             MGG IS REQUIRED.
!
!          NOTE - (1) NTYPE = 1 IF MGG IS REAL SP
!                     NTYPE = 2 IF MGG IS REAL DP
!
!   10. DIAGNOSTIC MESSAGES -
!
!         THE FOLLOWING MESSAGES MAY BE ISSUED - 3001,3002,3003,3008
!                                                AND 3031.
!
!
   !>>>>EQUIVALENCE (coord(1),ncrd) , (Z(1),Zs(1)) , (Z(1),Iz(1)) , (mcb(1),mcb1(1)) , (mcbm1(1),mcb2(1)) , (Itid(1),Bxtid)
   DATA casecc , bgpdt , cstm , dit , frl , mgg/101 , 102 , 103 , 104 , 105 , 106/
   DATA frlx , b1gg , m1gg , m2gg , basexg , pdzero/201 , 202 , 203 , 204 , 205 , 206/
   DATA modnam/4HFRL  , 4HFVRS , 4HTR1 /
   DATA itlist/4 , 1105 , 11 , 1 , 1205 , 12 , 2 , 1305 , 13 , 3 , 1405 , 14 , 4/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!     LOCATE  CODES FOR -  TABLED1    TABLED2    TABLED3    TABLED4
!
!     CALCULATE PARAMETERS
!
!     TEST TO SEE IF BASEXG IS TO BE GENERATED.
!
         Nobasx = -1
         IF ( Nomgg/=-1 .AND. Cycio==-1 .AND. Nofreq/=-1 ) THEN
            IF ( Bxtid/=-1 .OR. Bytid/=-1 .OR. Bztid/=-1 ) Nobasx = 1
         ENDIF
!
         IF ( Cycio==-1 ) THEN
!
!     DETERMINE FKMAX
!
            IF ( mod(Nsegs,2)/=0 ) THEN
               Fkmax = 2*Kmax + 1
            ELSEIF ( Kmax==Nsegs/2 ) THEN
               Fkmax = Nsegs
            ELSE
               Fkmax = 2*Kmax + 1
            ENDIF
         ENDIF
!
!     TEST TO SEE IF ANY DATA BLOCKS ARE TO BE GENERATED.
!
         IF ( Nomgg==-1 ) RETURN
         IF ( Omega==0.0 .AND. (Cycio/=-1 .OR. Nofreq==-1) .AND. (Bxtid==-1 .AND. Bytid==-1 .AND. Bztid==-1) ) RETURN
!
!     TEST TRAILER OF MGG TO SEE IF PURGED
!
         mcb(1) = mgg
         CALL rdtrl(mcb)
         nfile = mgg
         IF ( mcb(1)<=0 ) GOTO 80
!
!     COLUMN COUNT FOR MGG READ CHECK
!
         ncolc = mcb(2)
         nrowc = mcb(3)
         nform = mcb(4)
         ntype = mcb(5)
!
         nz = korsz(Z)
!
!     ALLOCATE BUFFERS
!
!     MGG,CSTM (IBUF1 IS NBUF+1 LONG)
!
         ibuf1 = nz - Nbuf
!
!     BGPDT
!
         ibuf2 = ibuf1 - Nbuf
!
!     B1GG
!
         ibuf3 = ibuf2 - Nbuf
!
!     M1GG
!
         ibuf4 = ibuf3 - Nbuf
!
!     M2GG
!
         ibuf5 = ibuf4 - Nbuf
         IF ( Omega==0.0 ) ibuf5 = ibuf3
!
!     CALCULATE LENGTH OF OPEN CORE
!
         nz = ibuf5 - 1
!
!     PROCESS CSTM DATA BLOCK
!
         nfile = cstm
         mcb(1) = cstm
         CALL rdtrl(mcb)
         IF ( mcb(1)>0 ) THEN
!
!     NO. OF COORDINATE SYSTEMS
!
            ncsym = mcb(3)
            lcstm = 14*ncsym
!
!     CSTM TABLE
!
            icstm = ibuf5 - lcstm
            nz = icstm - 1
!
!     CORE FOR ENOUGH CORE FOR CSTM
!
            IF ( nz<0 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     CORE CHECK FULL COLUMN OF MGG READ ASSUMED
!
            IF ( nz<ntype*nrowc ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL gopen(cstm,zs(ibuf1),0)
            CALL read(*100,*120,cstm,zs(icstm),lcstm,1,nwds)
            CALL pretrd(zs(icstm),lcstm)
            CALL close(cstm,1)
!
!     CORE CHECK NO CSTM
!
         ELSEIF ( nz<ntype*nrowc ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     BGPDT TABLE
!
         mcb(1) = bgpdt
         CALL rdtrl(mcb)
         nfile = bgpdt
         IF ( mcb(1)<=0 ) GOTO 80
!
!     NO. OF GRID POINTS AND SCALAR POINTS READ CHECK FOR BGPDT
!
         ngrid = mcb(2)
         CALL gopen(bgpdt,zs(ibuf2),0)
!
!     OPEN MGG AND OUTPUT MATRICES
!
         CALL gopen(mgg,zs(ibuf1),0)
         IF ( Omega/=0.0 ) THEN
            CALL gopen(b1gg,zs(ibuf3),1)
            mcbb1(1) = b1gg
            mcbb1(2) = 0
            mcbb1(3) = nrowc
            mcbb1(4) = 1
            mcbb1(5) = ntype
            mcbb1(6) = 0
            mcbb1(7) = 0
            CALL gopen(m1gg,zs(ibuf4),1)
            mcbm1(1) = m1gg
            mcbm1(2) = 0
            mcbm1(3) = nrowc
            mcbm1(4) = nform
            mcbm1(5) = ntype
            mcbm1(6) = 0
            mcbm1(7) = 0
         ENDIF
         IF ( Nobasx/=-1 ) THEN
            CALL gopen(m2gg,zs(ibuf5),1)
            mcbm2(1) = m2gg
            mcbm2(2) = 0
            mcbm2(3) = nrowc
            mcbm2(4) = 1
            mcbm2(5) = ntype
            mcbm2(6) = 0
            mcbm2(7) = 0
         ENDIF
!
!     SET UP PACK AND UNPACK TERMS
!
         In1 = ntype
         In = 2
         Iout = ntype
         Incr = 1
         Incr1 = 1
!
!     READ INTERNAL SORT BGPDT PICK UP CID,X,Y,Z
!
         ndof = 0
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_1: DO
            CALL read(*100,*40,bgpdt,coord,4,0,m)
            ndof = ndof + 1
            IF ( ncrd/=-1 ) THEN
!
!     UNPACK 3 COL OF MGG AND SAVE DIAGONAL TERMS
!
               DO i = 1 , 3
                  DO j = 1 , 3
                     a(i,j) = 0.0
                  ENDDO
               ENDDO
               DO i = 1 , 3
                  Nf1 = 0
                  CALL unpack(*2,mgg,Z)
!
!     LOCATE DIAGONAL ELEMENT IN COL-NROW
!
                  nrow = ndof - Nf1 + i
                  nterm = Nl1 - Nf1 + 1
                  IF ( nrow>=1 .AND. nrow<=nterm ) THEN
                     IF ( ntype==1 ) a(i,i) = zs(nrow)
                     IF ( ntype==2 ) a(i,i) = Z(nrow)
                     CYCLE
                  ENDIF
!
!     OUT OF RANGE OF NON-ZERO ELEMENT BAND
!
 2                a(i,i) = 0.0
               ENDDO
!
!     NOW TRANSFORM FROM LOCAL(GLOBAL) TO BASIC
!
               IF ( ncrd/=0 ) THEN
!
!     SELECT TRANSFORMATION MATRIX-TA
!
                  CALL transd(coord,ta)
                  CALL gmmatd(ta,3,3,0,a,3,3,0,b)
                  CALL gmmatd(b,3,3,0,ta,3,3,1,c)
!
!     C-IS NOW IN BASIC COORDINATES-ROW,WISE
!
                  avgm = (c(1,1)+c(2,2)+c(3,3))/3.0
               ELSE
!
!     ALREADY IN BASIC COORDINATES
!
                  avgm = (a(1,1)+a(2,2)+a(3,3))/3.0
               ENDIF
!
               IF ( Omega/=0.0 ) THEN
!
!     PROCESS M1GG
!
                  DO i = 1 , 3
                     DO j = 1 , 3
                        a(i,j) = 0.0
                     ENDDO
                  ENDDO
                  a(2,2) = avgm
                  a(3,3) = avgm
                  IF ( ncrd/=0 ) THEN
!
!     TRANSFORM TO GLOBAL(LOCAL) FROM BASIC
!
                     CALL gmmatd(ta,3,3,1,a,3,3,0,b)
                     CALL gmmatd(b,3,3,0,ta,3,3,0,c)
                  ELSE
                     DO i = 1 , 3
                        DO j = 1 , 3
                           c(i,j) = a(i,j)
                        ENDDO
                     ENDDO
                  ENDIF
!
!     C- IS NOW M1-11 ROW WISE
!
                  DO i = 1 , 3
                     DO k = 1 , 3
                        row(k) = c(i,k)
                     ENDDO
                     Nf = ndof
                     Nl = ndof + 2
                     CALL pack(row,m1gg,mcbm1)
                  ENDDO
!
!     WRITE OUT 3 NULL COLUMNS
!
                  row(1) = 0.0
                  DO k = 1 , 3
                     Nf = 1
                     Nl = 1
                     CALL pack(row,m1gg,mcbm1)
                  ENDDO
!
!     NOW TAKE CARE OF B1GG
!
                  IF ( ncrd/=0 ) THEN
                     DO i = 1 , 3
                        DO j = 1 , 3
                           a(i,j) = 0.0
                        ENDDO
                     ENDDO
                     a(3,2) = -avgm
                     a(2,3) = avgm
!
!     TRANSFORM TO GLOBAL(LOCAL) FROM BASIC
!
                     CALL gmmatd(ta,3,3,1,a,3,3,0,b)
                     CALL gmmatd(b,3,3,0,ta,3,3,0,c)
                  ELSE
                     DO i = 1 , 3
                        DO j = 1 , 3
                           c(i,j) = 0.0
                        ENDDO
                     ENDDO
                     c(3,2) = -avgm
                     c(2,3) = avgm
                  ENDIF
!
!     C-IS NOW B1-11 ROW WISE
!
                  DO i = 1 , 3
                     DO k = 1 , 3
                        row(k) = c(i,k)
                     ENDDO
                     Nf = ndof
                     Nl = ndof + 2
                     CALL pack(row,b1gg,mcbb1)
                  ENDDO
!
!     WRITE OUT 3 NULL COLUMNS
!
                  row(1) = 0.0
                  DO i = 1 , 3
                     Nf = 1
                     Nl = 1
                     CALL pack(row,b1gg,mcbb1)
                  ENDDO
               ENDIF
               IF ( Nobasx/=-1 ) THEN
!
!     NOW PROCESS M2GG
!
                  IF ( ncrd/=0 ) THEN
                     DO i = 1 , 3
                        DO j = 1 , 3
                           a(i,j) = 0.0
                        ENDDO
                     ENDDO
                     a(1,1) = avgm
                     a(2,2) = avgm
                     a(3,3) = avgm
                     a(3,2) = avgm
                     a(2,3) = -avgm
!
!     TRANSFORM TO GLOBAL(LOCAL) FROM BASIC
!
                     CALL gmmatd(ta,3,3,1,a,3,3,0,c)
                  ELSE
                     DO i = 1 , 3
                        DO j = 1 , 3
                           c(i,j) = 0.0
                        ENDDO
                     ENDDO
                     c(1,1) = avgm
                     c(2,2) = avgm
                     c(3,3) = avgm
                     c(3,2) = avgm
                     c(2,3) = -avgm
                  ENDIF
!
!     C-IS NOW M2-11 ROW WISE
!
                  DO i = 1 , 3
                     DO k = 1 , 3
                        row(k) = c(i,k)
                     ENDDO
                     Nf = ndof
                     Nl = ndof + 2
                     CALL pack(row,m2gg,mcbm2)
                  ENDDO
!
!     WRITE OUT 3 NULL COLUMNS
!
                  row(1) = 0.0
                  DO i = 1 , 3
                     Nf = 1
                     Nl = 1
                     CALL pack(row,m2gg,mcbm2)
                  ENDDO
               ENDIF
!
!     SPACE DOWN 3 COL IN MGG
!
               ndof = ndof + 5
               nfile = mgg
               CALL fwdrec(*100,mgg)
               CALL fwdrec(*100,mgg)
               CALL fwdrec(*100,mgg)
               nfile = bgpdt
            ELSE
!
!     SCALAR POINT-UNPACK ONE COL OF MGG
!     SAVE DIAGONAL TERM
!
               Nf1 = 0
               CALL unpack(*20,mgg,Z)
               nrow = ndof - Nf1 + 1
               nterm = Nl1 - Nf1 + 1
               IF ( nrow<1 .OR. nrow>nterm ) EXIT SPAG_Loop_1_1
               IF ( ntype==1 ) row(1) = zs(nrow)
               IF ( ntype==2 ) row(1) = Z(nrow)
               Nf = ndof
               Nl = ndof
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO SPAG_Loop_1_1
!
!     OUT OF RANGE OF NON-ZERO BAND
!
 20      row(1) = 0.0
         Nf = 1
         Nl = 1
         spag_nextblock_1 = 3
      CASE (3)
!
!     NOW PUT DIAGONAL ELEMENT INTO OUTPUT MATRICES
!
         IF ( Omega/=0.0 ) THEN
            CALL pack(row,m1gg,mcbm1)
            CALL pack(row,b1gg,mcbb1)
         ENDIF
         IF ( Nobasx/=-1 ) CALL pack(row,m2gg,mcbm2)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     FINISH PROCESSING
!
 40      CALL close(mgg,1)
         IF ( Nobasx/=-1 ) THEN
            CALL close(m2gg,1)
            CALL wrttrl(mcbm2)
         ENDIF
         IF ( Omega/=0.0 ) THEN
            CALL close(b1gg,1)
            CALL close(m1gg,1)
            CALL wrttrl(mcbb1)
            CALL wrttrl(mcbm1)
         ENDIF
         CALL close(bgpdt,1)
!
!     BEGIN PROCESSING OF FRLX, PDZERO AND BASEXG DATA BLOCKS.
!
!
!     TEST TO SEE IF BASEXG IS TO BE GENERATED.
!
         IF ( Nobasx==-1 ) RETURN
!
!     RE-ESTABLISH LENGTH OF OPEN CORE FOR PHASE II PROCESSING
!
         nz = ibuf3 - 1
!
!     PROCESS FRL, FRLX AND PDZERO
!
         modfrl = .TRUE.
         IF ( Omega==0.0 .OR. Bytid==-1 .AND. Bztid==-1 ) modfrl = .FALSE.
!
         nfile = frl
         mcb1(1) = frl
         CALL rdtrl(mcb1)
         nfsets = mcb1(2)
         ifrl = 1
         CALL open(*80,frl,zs(ibuf1),0)
!
!     READ HEADER RECORD
!
         CALL read(*100,*60,frl,iz(ifrl),nz,1,nwrds)
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
!
!     OPEN CASECC
!
 60      nfile = casecc
         CALL gopen(casecc,zs(ibuf2),0)
!
!     READ RECORD 1, WORD 14 (FREQUENCY SET ID)
!
         CALL read(*100,*120,casecc,case,14,0,dummy)
         frqset = case(14)
         CALL close(casecc,1)
!
!     CHECK WHAT LOGICAL RECORD FRQSET IS IN FRL.
!
         mm = 0
         ii = ifrl + 2
         DO i = ii , nwrds
            mm = mm + 1
            IF ( iz(i)==frqset ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     FREQUENCY SET NOT FOUND.
!
         ENDDO
!
!     FREQUENCY SET NOT FOUND IN FRL (ERROR 3031)
!
         CALL mesage(-31,frqset,modnam)
         RETURN
      CASE (4)
!
!     MM IS LOGICAL RECORD NO. IN FRL FOR FRQSET.
!
         IF ( modfrl ) THEN
            CALL open(*80,frlx,zs(ibuf2),1)
            CALL write(frlx,iz(ifrl),nwrds,1)
            CALL gopen(pdzero,zs(ibuf3),1)
            mcb2(1) = pdzero
            mcb2(2) = 0
            mcb2(3) = 0
            mcb2(4) = 1
            mcb2(5) = 1
            mcb2(6) = 0
            mcb2(7) = 0
            In = 1
            Iout = 1
            Incr1 = 1
            row2(1) = 0.0
            row2(2) = 1.0
            row2(3) = 0.0
         ENDIF
         ifrl = 1
         nfs = 0
         nfsx = 0
         nfile = frl
         DO i = 1 , nfsets
            CALL read(*100,*70,frl,zs(ifrl),nz,1,m)
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
 70         IF ( i==mm ) nfs = m
            IF ( .NOT.modfrl .AND. i==mm ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( modfrl ) THEN
               IF ( i/=mm ) THEN
                  CALL write(frlx,zs(ifrl),m,1)
               ELSE
!
!     SET POINTERS FOR SORT INDEX ,   FRLX AND PDZERO ARRAYS.
!
                  index = ifrl + nfs
                  ifrlx = index + 3*nfs
                  ipdz = ifrlx + 3*nfs
!
!     RESET IFRL POINTER TO CONTINUE READING FRL RECORDS.
!
                  ifrl = ifrlx
!
!     CHECK CORE REQUIRED FOR EXPANDED FREQUENCY LIST AND SORT INDEX
!
                  nz = nz - (ipdz+3*nfs) + 1
                  IF ( nz<0 ) THEN
                     spag_nextblock_1 = 6
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
!
                  ll = ifrlx - 1
                  kkk = ipdz - 1
                  DO ii = 1 , nfs
                     IF ( zs(ii)==0.0 ) THEN
                        zs(ll+1) = 0.0
                        zs(ll+2) = abs(Omega)
                        kkk = kkk + 1
                        zs(kkk) = row2(2)
                        kkk = kkk + 1
                        zs(kkk) = row2(1)
                        ll = ll + 2
                     ELSE
                        DO kk = 1 , 3
                           kkk = kkk + 1
                           zs(kkk) = row2(kk)
                        ENDDO
                        zs(ll+1) = abs(zs(ii)-Omega)
                        zs(ll+2) = zs(ii)
                        zs(ll+3) = abs(zs(ii)+Omega)
                        ll = ll + 3
                     ENDIF
                  ENDDO
!
!     COMPUTE THE EXPANDED NUMBER OF FREQUIENCES, NFSX.
!
                  nfsx = ll - ifrlx + 1
!
!     SORT EXPANDED W'S AND GET INDEX FOR SORTING BASE TABLE.
!
                  CALL fvrs1e(zs(ifrlx),iz(index),nfsx)
                  CALL write(frlx,zs(ifrlx),nfsx,1)
               ENDIF
            ENDIF
         ENDDO
         IF ( modfrl ) THEN
!
!      FRLX IS A COPY OF FRL WITH THE SELECTED FREQUENCY SET, FRQSET,
!      EXPANDED.
!
            CALL close(frlx,1)
            mcb1(1) = frlx
            CALL wrttrl(mcb1)
!
!     SORT PDZERO BY INDEX JUST AS WAS DONE FOR FRLX
!     USE WORK   AT ZS(IFRLX)
!         INDEX  AT ZS(INDEX)   ALL NFSX LONG
!         PDZERO AT ZS(IPDZ)
!
            DO kk = 1 , nfsx
               loc = iz(index+kk-1)
               zs(ifrlx+kk-1) = zs(ipdz+loc-1)
            ENDDO
!
!     NOW OUTPUT NFSX * FKMAX COLUMNS FOR PDZERO
!
            kkk = 0
            DO kk = 1 , Fkmax
               DO jj = 1 , nfsx
                  kkk = kkk + 1
                  Nf = kkk
                  Nl = kkk
                  CALL pack(zs(ifrlx+jj-1),pdzero,mcb2)
               ENDDO
            ENDDO
            CALL close(pdzero,1)
            mcb2(3) = mcb2(2)
            CALL wrttrl(mcb2)
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         CALL close(frl,1)
!
!     RE-ESTABLISH OPEN CORE FOR PHASE III AND
!     RESET POINTER TO ORIGINAL FREQUIENCIES.
!
         ifrl = 1
         nz = ibuf1 - (nfs+nfsx) - 1
!
!     NFS  = THE ORIGINAL NUMBER OF FREQUIENCES
!     NFSX = THE EXPANDED NUMBER OF FREQUIENCES.
!
!     GENERATE BASE ACCELERATION MATRIX BASEXG.
!
!
!     BUILD A LIST OF UNIQUE TABLE IDS FOR PRETAB.
!     INITIALIZE THE TABLE WITH A ZERO ENTRY.
!
         itab = nfs + nfsx + 1
         ntabl = 1
         k = itab + ntabl
         iz(k) = 0
!
!     WE HAVE A LIST OF TABLE ID'S TO CONSIDER
!     WE WANT ONLY A UNIQUE LIST OF TABLE ID'S GIVEN TO PRETAB
!
         SPAG_Loop_1_2: DO i = 1 , 6
            iitid = itid(i)
!
!     SEARCH EXISTING LIST OF TABLE ID'S TO SEE IF IITID IS ALREADY IN
!     LIST
!
            IF ( iitid>0 .AND. iitid<=9999999 ) THEN
               DO l = 1 , ntabl
                  ll = itab + l
                  IF ( iz(ll)==iitid ) CYCLE SPAG_Loop_1_2
               ENDDO
!
!     IITID WAS NOT AMONG EXISTING TABLE ID'S IN LIST,
!     IT'S A NEW TABLE ID,ADD IT TO LIST AND UPDATE LENGHT OF LIST
!
               ntabl = ntabl + 1
               k = itab + ntabl
               iz(k) = iitid
            ENDIF
         ENDDO SPAG_Loop_1_2
!
!     ALL TABLE ID'S HAVE BEEN PROCESSED,NOW PRETAB CAN BE CALLED
!     NTABL IS THE NUMBER OF TID'S IN THE LIST.
!
         iz(itab) = ntabl
!
!     ILTAB IS THE NEXT AVAILABLE LOCATION OF OPEN CORE FOR PRETAB.
!
         iltab = itab + ntabl + 1
!
!     COMPUTE LENGTH OF OPEN CORE AVAILABLE TO PRETAB.
!
         nztab = nz - ntabl - 1
         ltab = 0
         CALL pretab(dit,zs(iltab),iz(iltab),zs(ibuf1),nztab,ltab,iz(itab),itlist)
!
!     COMPUTE LENGTH OF OPEN CORE AFTER PRETAB AND NEXT AVAILABLE LOC.
!
         nz = nz - ltab
         next = iltab + ltab
!
!     ALLOCATE COMPLEX ARRAYS FOR BASEXG. START ON DOUBLE WORD BOUNDARY.
!
         IF ( mod(next,2)==0 ) next = next + 1
!
!     DEFINE NFSX IF MODFRL IS FALSE.
!
         IF ( .NOT.modfrl ) nfsx = nfs
!
         n1 = next
         n2 = n1 + (3*nfsx)*2
         n3 = n2 + (3*nfsx)*2
         nt = n3 + nrowc*2 - 1
         IF ( nz>=nt ) THEN
            CALL fvrs1a(zs(n1),zs(n2),zs(n3),zs(ifrl),zs(ibuf1),zs(index),modfrl,basexg,nrowc,nfs,nfsx,Fkmax,Omega)
            RETURN
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
!
!     ERROR PROCESSING
!
!     NOT ENOUGH CORE (ERROR 3008)
!
         ip1 = -8
         CALL mesage(ip1,nfile,modnam(2))
         RETURN
!
!     DATA SET NOT IN FIST (ERROR 3001)
!
 80      ip1 = -1
         CALL mesage(ip1,nfile,modnam(2))
         RETURN
!
!     EOF ENCOUNTERED (ERROR 3002)
!
 100     ip1 = -2
         CALL mesage(ip1,nfile,modnam(2))
         RETURN
!
!     EOL ENCOUNTERED (ERROR 3003)
!
 120     ip1 = -3
         CALL mesage(ip1,nfile,modnam(2))
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE fvrst1
