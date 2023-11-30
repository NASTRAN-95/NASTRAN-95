
SUBROUTINE xgpidd
   IMPLICIT NONE
   REAL Alter(2) , Cnmtp , Fnmtp
   INTEGER Bcdcnt , Cpntry(7) , Diag14 , Diag17 , Diag25 , Diag4 , Dmap(200) , Dmpcnt , Dmppnt , Eotflg , Iallon , Iapp , Iappnd ,  &
         & Ibuff(20) , Icfpnt , Icftop , Ichar , Icold , Icpbot , Icptop , Icrdtp , Icst , Ictlfl(1) , Idmapp , Idmpnt , Idsapp ,   &
         & Ieqflg , Iestim , Ifile(130) , Ifirst , Iflag , Iflg(6) , Ifpnt , Ihapp , Iholc(21) , Imst , Insert , Intgr , Iplus ,    &
         & Irturn , Isavdw , Isave , Iseqn , Isgnon , Itape , Iunst , Jmp(7) , Lcpdpl , Lctlfl , Ldmap , Length , Lfile , Lmed ,    &
         & Losgn , Maskhi , Masklo , Masks(40) , Medpnt , Medtp , Modidx , Nbpc , Ncpw , Newcrd , Nmpt(26) , Noflgs , Nosgn , Nwpc ,&
         & Pvt(200) , Seteor , Sol , Start , Subset
   COMMON /moddmp/ Iflg , Nmpt
   COMMON /xgpi3 / Pvt
   COMMON /xgpi4 / Irturn , Insert , Iseqn , Dmpcnt , Idmpnt , Dmppnt , Bcdcnt , Length , Icrdtp , Ichar , Newcrd , Modidx , Ldmap ,&
                 & Isavdw , Dmap
   COMMON /xgpi5 / Iapp , Start , Alter , Sol , Subset , Iflag , Iestim , Icftop , Icfpnt , Lctlfl , Ictlfl
   COMMON /xgpi6 / Medtp , Fnmtp , Cnmtp , Medpnt , Lmed , Iplus , Diag14 , Diag17 , Diag4 , Diag25 , Ifirst , Ibuff
   COMMON /xgpi7 / Ifpnt , Lfile , Ifile
   COMMON /xgpi8 / Icptop , Icpbot , Lcpdpl
   COMMON /xgpic / Icold , Iholc , Ncpw , Nbpc , Nwpc , Maskhi , Masklo , Isgnon , Nosgn , Iallon , Masks
   COMMON /xgpid / Icst , Iunst , Imst , Ihapp , Idsapp , Idmapp , Isave , Itape , Iappnd , Intgr , Losgn , Noflgs , Seteor ,       &
                 & Eotflg , Ieqflg , Cpntry , Jmp
   INTEGER i , ihol(22) , ipls , namopt(26)
!
!     THIS SUBROUTINE DEFINES ALL NAMED COMMON FOR SUBROUTINES
!     XGPI,XOSGEN,XLNKHD,XIOFL,XPARAM,XSCNDM,XFLORD,XFLDEF AND XGPIDG.
!     **NOTE - THIS PROGRAM MUST BE LOADED BEFORE ANY OF THE ABOVE.
!
!
!     NAMED COMMON AREAS /XGPIC/ AND /XGPID/ CONTAIN
!     MACHINE DEPENDENT DATA
!
!     COMMON /XGPIC / ICOLD,ISLSH,IEQUL,NBLANK,NXEQUI,
!                        ** CONTROL CARD NAMES **
!    1                NMED,NSOL,NDMAP,NESTM1,NESTM2,NEXIT,
!                        ** DMAP CARD NAMES **
!    2                NBEGIN,NEND,NJUMP,NCOND,NREPT,NTIME,NSAVE,NOUTPT,
!    3                NCHKPT,NPURGE,NEQUIV,
!      ** THE FOLLOWING CONSTANTS ARE INITIALIZED BY XGPIBS ROUTINE **
!    4                NCPW,NBPC,NWPC,
!    5                MASKLO,ISGNON,NOSGN,IALLON,MASKS(40)
!
!
!     ****** /XGPI1 / *******
!
!     COMMON /XGPI1 / LOSCAR,OSPRC,OSBOT,OSPNT,OSCAR(1)
!
!     NOTE - /XGPI1 / MUST BE LOADED AT THE END OF LONGEST LINK IN XGPI
!                     BECAUSE IT DEFINES THE START OF OPEN CORE.
!
!     OSCAR  = OPERATION SEQUENCE CONTROL ARRAY
!     LOSCAR = LENGTH OF OSCAR ARRAY
!     OSPRC  = POINTER TO PRECEDING OSCAR ENTRY
!     OSBOT  = POINTER TO LAST OSCAR ENTRY
!     OSPNT  = POINTER TO PRESENT OSCAR ENTRY BEING PROCESSED
!
!     ***ORDER OF TABLES IN OPEN CORE DURING PHASE 1 OF COMPILATION.
!        EQUIVALENCE (OSCAR,DMPCRD,LBLTBL,MED,IBUFR)
!
!     ***ORDER OF TABLES IN OPEN CORE DURING PHASE 2 OF COMPILATION.
!        EQUIVALENCE (OSCAR,PTDIC,ICPDPL,MED,IBUFR)
!     IBUFR  = GINO BUFFER AREA LOCATED AT HIGH ADDRESS END OF OPEN
!              CORE.
!     DMPCRD = DMAP SEQUENCE CARD IMAGE BUFFER
!     PTDIC  = PROBLEM TAPE CHECKPOINT DICTIONARY
!     MED    = MODULE EXECUTION DECISION TABLE FOR RESTARTS IN RIGID
!              FORMATS
!     ICPDPL = LIST OF CHECKPOINT FILES TO BE WRITTEN ON DATA POOL FROM
!              OLD  PROBLEM TAPE IN ORDER TO RESTART PROBLEM.
!     LBLTBL = TABLE OF LABEL NAMES AND PARAMETER NAMES REFERENCED BY
!              LABEL,COND,PURGE AND EQUIV DMAP INSTRUCTIONS.
!
   DATA namopt/4HGO   , 4H     , 4HNOGO , 4H     , 4HERR  , 4H     , 4HLIST , 4H     , 4HNOLI , 4HST   , 4HDECK , 4H     , 4HNODE , &
       &4HCK   , 4HREF  , 4H     , 4HNORE , 4HF    , 4HOSCA , 4HR    , 4HNOOS , 4HCAR  , 4HALL  , 4H     , 4HEXCE , 4HPT  /
   DATA ihol/4H/    , 4H=    , 4H     , 4HXEQU , 4HMED  , 4HSOL  , 4HDMAP , 4HESTI , 4HM    , 4HEXIT , 4HBEGI , 4HEND  , 4HJUMP ,   &
       &4HCOND , 4HREPT , 4HTIME , 4HSAVE , 4HOUTP , 4HCHKP , 4HPURG , 4HEQUI , 4HXCHK/
   DATA ipls/1H+/
!
!     ****** /XGPIC / *******
!
!     NCPW   = NUMBER OF CHARACTERS PER WORD.
!     NBPC   = NUMBER OF BITS PER CHARACTER.
!     NWPC   = NUMBER OF WORDS PER INPUT CARD (72 CHARACTERS).
!     MASKHI = MASK OUT ALL BITS EXCEPT LOW ORDER 15 BITS
!            = 2**15 - 1
!     MASKLO = MASK FOR HI ORDER 16 BITS, SIGN BIT NOT INCLUDED
!            = LSHIFT(MASKHI,16)
!     ISGNON = MASK OUT ALL BUT SIGN BIT
!            = LSHIFT(1,NBPW-1)
!     NOSGN  = MASK OUT ONLY SIGN BIT
!            = COMPLF(ISGNON)
!     IALLON = ALL BITS ON
!            = COMPLF(0)
!     MASKS  = TABLE OF MASKS FOR MASKING OUT VARIOUS CHARACTERS OF A
!              WORD. TABLE LENGTH = 4*NCPW (40 MAX.)
!
!     DATA     ICOLD /1     /, ISLSH /4H/   /, IEQUL /4H=   /,
!    1         NXEQUI/4HXEQU/, NMED  /4HMED /, NSOL  /4HSOL /,
!    2         NDMAP /4HDMAP/, NESTM1/4HESTI/, NESTM2/4HM   /,
!    3         NEXIT /4HEXIT/, NBEGIN/4HBEGI/, NEND  /4HEND /,
!    3         NJUMP /4HJUMP/, NCOND /4HCOND/, NBLANK/4H    /,
!    4         NREPT /4HREPT/, NTIME /4HTIME/, NSAVE /4HSAVE/,
!    5         NCHKPT/4HCHKP/, NPURGE/4HPURG/, NEQUIV/4HEQUI/,
!    6         MASKHI/32767 /, NOUTPT/4HOUTP/
!
   Icold = 1
   Maskhi = 32767
!              2**15 - 1
   DO i = 1 , 21
      Iholc(i) = ihol(i)
   ENDDO
!
!     ****** /XGPID / *******
!
!     ICST,IUNST,IMST = COLD,UNMODIFIED,MODIFIED START CODES
!     IHAPP,IDSAPP,IDMAPP = HEAT,DISPLACEMENT,DMAP APPROACH CODES
!     ** THE FOLLOWING CONSTANTS ARE INITIALIZED IN XGPIBS ROUTINE **
!     INTGR  = INTEGER TYPE CODE RETURNED BY XSCNDM
!     ISAVE,ITAPE,IAPPND = FLAGS USED IN /XGPI7/
!     MODFLG = PARAM MODIFY FLAG IN VPS
!     LOSGN  = SIGN BIT OF LOW ORDER 16 BITS
!     NOFLGS = MASK OUT FLAGS USED IN PTDIC TABLE AND ICPDPL
!     SETEOR = END OF RECORD FLAG IN PTDIC,ICPDPL TABLES
!     EOTFLG = END OF TAPE FLAG   IN PTDIC,ICPDPL TABLES
!     IEQFLG = EQUIVALENCE FLAG   IN PTDIC,ICPDPL,DPL,FIAT TABLES
!     CPNTRY = TABLE CONTAINING HEADER SECTION OF CHECKPOINT OSCAR ENTRY
!     JMP    = TABLE CONTAINING JUMP OSCAR ENTRY TO BE INSERTED
!
!     DATA  ICST  /1/, IUNST/2/, IMST/3/, IDMAPP/1/, ISAVE/1/,
!    1      CPNTRY/6,2,0,4HXCHK, 4H    ,  0,0/,
!    2      JMP   /7,3,0,4HJUMP, 4H    ,  0,0/
!
   Icst = 1
   Iunst = 2
   Imst = 3
   Idmapp = 1
   Isave = 1
   Jmp(1) = 7
   Jmp(2) = 3
   Jmp(3) = 0
   Jmp(4) = ihol(13)
   Jmp(5) = ihol(3)
   Jmp(6) = 0
   Jmp(7) = 0
   Cpntry(1) = 6
   Cpntry(2) = 2
   Cpntry(3) = 0
   Cpntry(4) = ihol(22)
   Cpntry(5) = ihol(3)
   Cpntry(6) = 0
   Cpntry(7) = 0
!
!     ****** /XGPI3 / *******
!
!     PVT = PARAMETER VALUE TABLE
!     DATA  PVT/200,2,0,0,1,195*0/
!
   Pvt(1) = 200
   Pvt(2) = 2
   DO i = 3 , 200
      Pvt(i) = 0
   ENDDO
   Pvt(5) = 1
!
!     ****** /XGPI4 / *******
!
!     IRTURN = RETURN CODE USED FOR ALTERNATE RETURNS
!     INSERT = -1  INDICATES DMAP INSTRUCTION IS TO BE DELETED.
!            =  0  PROCESS DMAP INSTRUCTION FROM MAIN STREAM.
!            =  1  INSERT DMAP INSTRUCTION FROM ALTER FILE.
!     ISEQN  = NEXT OSCAR SEQUENCE NUMBER TO BE ASSIGNED.
!     DMPCNT = DMAP INSTRUCTION COUNTER
!     IDMPNT = POINTER TO NEXT ITEM TO BE SCANNED IN DMAP ARRAY
!     DMPPNT = POINTER TO ITEM IN DMAP ARRAY RETURNED BY XSCNDM ROUTINE
!     BCDCNT = NUMBER OF BCD ENTRIES REMAINING IN DMAP ARRAY BEFORE MODE
!              CHANGES.
!     LENGTH = LENGTH (IN WORDS) OF BINARY VALUE RETURNED BY XSCNDM
!              ROUTINE.
!     ICRDTP = POINTER TO NEXT WORD TO BE PROCESSED IN DMPCRD ARRAY.
!     ICHAR  = POINTER TO NEXT CHARACTER TO BE PROCESSED IN DMPCRD ARRAY
!     NEWCRD = FLAG TO INDICATE WHETHER OR NOT TO PREPARE NEXT CARD
!              IMAGE FOR TRANSLATION BY XRCARD ROUTINE.
!     MODIDX = MODULE INDEX STORED IN OSCAR ENTRY FOR USE BY XSEM
!              ROUTINE.
!     LDMAP  = LENGTH OF DMAP ARRAY.
!     ISAVDW = POINTER TO LAST DELIMITER ENCOUNTERED IN DMPCRD ARRAY,
!              USED BY XSCNDM WHEN UNPACKING RIGID FORMAT DMAP SEQUENCE.
!     DMAP   = ARRAY CONTAINING OUTPUT FROM XRCARD ROUTINE.
!
!     DESCRIPTION OF VARIABLES EQUIVALENCED TO /XGPI4/ ENTRIES
!     EQUIVALENCE (DMAP,ICF)
!     ICF    = TEMPORARY STORAGE FOR CONTROL FILE DICTIONARY.
!
!     DATA     ISEQN/1/, DMPCNT/0/, ICHAR/1/, LDMAP/200/, BCDCNT/0/
!
   Iseqn = 1
   Dmpcnt = 0
   Ichar = 1
   Ldmap = 200
   Bcdcnt = 0
!
!     ****** /XGPI5/ *******
!
!     IAPP   = APPROACH CODE.
!     START  = TYPE OF START CODE.
!     ALTER  = DMAP NOS. OF INSTRUCTIONS TO BE ALTERED
!     SOL    = SOLUTION CODE.
!     SUBSET = SOLUTION SUBSET CODE.
!     IFLAG  = FLAG FOR USE IN SUBROUTINE XLNKHD.
!     IESTIM = POINTER TO ESTIM ENTRIES IN ICTLFL OR ZERO.
!     ICFTOP = POINTER TO FIRST WORD IN ICTLFL ARRAY
!     ICFPNT = POINTER TO NEXT AVAILABLE WORD IN ICTLFL ARRAY
!     LCTLFL = LENGTH OF ICTLFL ARRAY.
!     ICTLFL = ARRAY CONTAINING INFORMATION FROM ESTIM CONTROL CARD.
!
!     DATA     IESTIM/0/, ICFTOP/1/, LCTLFL/1/, IFLAG/0/
!
   Iestim = 0
   Icftop = 1
   Lctlfl = 1
   Iflag = 0
!
!     ****** /XGPI6/ *******
!
!     MED    = (SEE DESCRIPTION IN /XGPI1/)
!     MEDTP  = POINTER TO FIRST WORD IN MED ARRAY.
!     LMED   = LENGTH OF MED ARRAY.
!     MEDPNT = POINTER TO AN ENTRY IN MED
!     FNMTP  = POINTER TO FIRST WORD OF FILE NAME PORTION OF MED TABLE
!     CNMTP  = POINTER TO FIRST WORD OF CARD NAME PORTION OF MED TABLE
!     IPLUS  = PLUS CHARACTER FOR PRINTER SPACE SUPRESS
!     DIAG14 = SKIP DMAP PRINT UNLESS RESTART (SET BY XGPI)
!     DIAG17 = DMAP PUNCH OPTION FLAG (SET BY XGPI)
!
!     DATA     MEDTP/1/,  LMED/0/,  IPLUS/1H+/
!
   Medtp = 1
   Lmed = 0
   Iplus = ipls
!
!     ****** /XGPI7/ *******
!
!     IFPNT  = POINTER TO LAST ENTRY IN FILE TABLE
!     LFILE  = LENGTH OF FILE TABLE (IN WORDS)
!     IFILE  = TABLE CONTAINING INFO FROM FILE DMAP INSTRUCTION
!
!     DATA     IFPNT/-2/,  LFILE/130/,  IFILE/130*0/
!
   Ifpnt = -2
   Lfile = 130
   DO i = 1 , Lfile
      Ifile(i) = 0
   ENDDO
!
!     ****** /XGPI8 / *******
!
!     ICPDPL = (SEE /XGPI1/ FOR DESCRIPTION)
!     ICPTOP = POINTER TO FIRST ENTRY IN ICPDPL ARRAY.
!     ICPBOT = POINTER TO LAST  ENTRY IN ICPDPL ARRAY.
!     LCPDPL = LENGTH OF ICPDPL ARRAY)
!
!     DATA     ICPTOP/0/,  ICPBOT/0/,  LCPDPL/0/
!
   Icptop = 0
   Icpbot = 0
   Lcpdpl = 0
!
!     ****** /MODDMP/ ********
!
   Iflg(1) = 1
   Iflg(2) = 2
   Iflg(3) = 0
   Iflg(4) = 0
   Iflg(5) = 0
   Iflg(6) = 0
   DO i = 1 , 26
      Nmpt(i) = namopt(i)
   ENDDO
!
END SUBROUTINE xgpidd
