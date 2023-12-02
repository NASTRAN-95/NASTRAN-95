!*==mma.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mma(Zi,Zr,Zd)
   USE i_mmacom
   USE c_logout
   USE c_mpyadx
   USE c_names
   USE c_packx
   USE c_system
   USE c_unpakx
   USE c_xmssg
   USE c_zblpkx
   USE c_zntpkx
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: Zi
   REAL , DIMENSION(2) :: Zr
   REAL*8 , DIMENSION(2) :: Zd
!
! Local variable declarations rewritten by SPAG
!
   REAL*8 , DIMENSION(2) :: ad , dd
   INTEGER , DIMENSION(15) :: blk1 , blk2
   CHARACTER(2) :: ct
   REAL :: denstya , denstyb , denstyc , percnta , percntb , percntc , x , y
   INTEGER :: i , ibuf1 , ibuf2 , ierr , iprec , itest1 , itest2 , ksys58 , l19 , memavl , mpass10 , mpass11 , mpass20 , mpass21 ,  &
            & mpass30 , mpass31 , mpass32 , mpass40 , mpass41 , nac , nadens , naform , nanzwd , napack , nar , nastrgs , naterms , &
            & natotal , natype , nbc , nbdens , nbform , nbnzwd , nbpack , nbpw , nbr , nbstrgs , nbterms , nbtotal , nbtype , ncc ,&
            & ncdens , ncform , ncnzwd , ncols , ncpack , ncr , ncstor , ncstrgs , ncterms , nctotal , nctype , ndc , nddens ,      &
            & ndform , ndnzwd , ndr , ndtotal , ndtype , nout , numc , nwda , nwdc , sysbuf
   INTEGER , DIMENSION(9) :: isave
   INTEGER , SAVE :: jbegn , jend
   INTEGER , DIMENSION(3) , SAVE :: module
   INTEGER , DIMENSION(2) :: namea , nameb , namec , named
   INTEGER , DIMENSION(4) , SAVE :: prntyp
   REAL , SAVE :: testpct
   CHARACTER(6) , DIMENSION(2) , SAVE :: upmeth
   INTEGER :: spag_nextblock_1
!
! End of declarations rewritten by SPAG
!
!
!     MMA PERFORMS THE MATRIX OPERATION
!       (+/-)A    * B (+/-)C = D   OR
!       (+/-)A(T) * B (+/-)C = D
!
!     USING METHODS 10, 11, 20, 21, 30, 31, 32, 40, 41
!
!
!  IN REGARDS TO THE METHODS BELOW, WHEN MULTIPLE COLUMNS OF A MATRIX
!  ARE STORED AND READ BY GETSTR, THEN THE MATRIX IS STORED IN MEMORY IN
!  COMPACT FORM.  SEE SUBROUTINES 'MMARM1,2,3,4' FOR A DESCRIPTION OF
!  THIS COMPACT FORM.  WHEN ONLY A SINGLE COLUMN OF A MATRIX IS STORED
!  AND IT IS BEING READ BY GETSTR, IT IS STORED IN COMPACT FORM IN MEMORY.
!  SEE SUBROUTINES 'MMARC1,2,3,4' FOR A DESCRIPTION OF THIS FORM.
!
!   ------------------------------------------------------------------------
!   METHOD     METHOD OF READING MATRIX    MULTIPLE COLUMNS OF MATRIX STORED
!                 A        B       C           A         B        D
!   ------------------------------------------------------------------------
!     10        UNPACK  UNPACK   UNPACK       YES        NO       NO
!     11        UNPACK  GETSTR   UNPACK       YES        NO       NO
!     20        UNPACK  UNPACK   UNPACK       NO         YES      YES
!     21        GETSTR  UNPACK   UNPACK       NO         YES      YES
!     30        GETSTR  UNPACK   UNPACK       YES        NO       NO
!     31        GETSTR  GETSTR   UNPACK       YES        NO       NO
!     32        GETSTR  GETSTR   GETSTR       YES        NO       NO
!     40        UNPACK  GETSTR   UNPACK       NO         YES      YES
!     41        GETSTR  GETSTR   UNPACK       NO         YES      YES
!   ------------------------------------------------------------------------
!
!   TO DETERMINE WHICH METHOD TO USE, THE FOLLOWING RATIONAL IS USED.
!
!   1.  DETERMINE THE METHOD FOR READING MATRICES "A" AND "B".  THIS IS
!       DETERMINED BY EXAMINING THE FOLLOWING PERCENTAGE:
!
!            (MEMORY TO CONTAIN ENTIRE MATRIX)
!            ------------------------------------  = PERCENTAGE
!            (MEMORY TO CONTAIN COMPACTED MATRIX)
!
!       IF THE PERCENTAGE IS .GE. THE VARIABLE "TESTPCT", THEN UNPACK IS
!       USED.  OTHERWISE, GETSTR IS USED.
!
!            NiSTOR (i=A or B) = 1, CALL UNPACK TO READ MATRIX
!                              = 2, CALL GETSTR TO READ MATRIX
!
!    2. THE RESULTS OF THE FIRST TEST WILL NARROW THE OPTIONS TO TWO
!       DIFFERENT METHODS AS FOLLOWS:
!
!                                  CANDIDATE METHOD
!                10     11     20     21     30     31     32     40     41
!       NASTOR =  1      1      1      2      2      2      2      1      2
!       NBSTOR =  1      2      1      1      1      2      2      2      2
!
!          FOR NASTOR = 1 AND NBSTOR = 1, METHODS 10 AND 20 ARE CONSIDERED
!          FOR NASTOR = 1 AND NBSTOR = 2, METHODS 11 AND 40 ARE CONSIDERED
!          FOR NASTOR = 2 AND NBSTOR = 1, METHODS 21 AND 30 ARE CONSIDERED
!          FOR NASOTR = 2 AND NBSTOR = 2, METHODS 31,32 AND 41 ARE CONSIDERED
!            (NOTE, METHOD 32 IS ONLY AVAILABLE WITH "A" TRANSPOSED)
!
!    3. LASTLY, DETERMINE THE ESTIMATED NUMBER OF PASSES FOR EACH OF THE
!       TWO CANDIDATE METHODS.  THE METHOD WITH THE FEWER NUMBER OF PASSES
!       IS CHOSEN.
!
!       MPASSii (ii=10,11,20,21,30,31,32,40,41) = ESTIMATED NUMBER OF PASSES
!                                                 FOR METHOD ii.
!
!       NiTOTAL (i=A,B,C) = MEMORY WORDS TO CONTAIN ENTIRE FULL MATRIX
!       NiPACK  (i=A,B,C) = MEMORY WORDS TO CONTAIN ENTIRE MATRIX IN COMPACT
!                           FORM.
!       NWDD              = NUMBER OF WORDS FOR EACH ELEMENT OF THE "D" MATRIX
!
!
!     THE FOLLOWING SUBROUTINES ARE CALLED FOR THE DIFFERENT METHODS AND
!     MATRIX "D" TYPES (RS,RD,CS,CD).
!
!          METHODS  MAIN      OTHER SUBROUTINES DEPENDING ON TYPE
!                 SUBROUTINE    RS     RD     CS     CD
!            10     MMA1      MMA101 MMA102 MMA103 MMA104
!            11     MMA1      MMA111 MMA112 MMA113 MMA114
!            20     MMA2      MMA201 MMA202 MMA203 MMA204
!            21     MMA2      MMA211 MMA212 MMA213 MMA214
!            30     MMA3      MMA301 MMA302 MMA303 MMA304
!            31     MMA3      MMA311 MMA312 MMA313 MMA314
!            32     MMA3      MMA321 MMA322 MMA323 MMA324 (TRANSPOSE ONLY)
!            40     MMA4      MMA401 MMA402 MMA403 MMA404
!            41     MMA4      MMA411 MMA412 MMA413 MMA414
! ---------------------------------------------------------------------------
   !>>>>EQUIVALENCE (Ad(1),A(1)) , (Dd(1),D(1))
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Nout) , (Ksystm(58),Ksys58) , (Ksystm(40),Nbpw) , (Ksystm(55),Iprec)
   !>>>>EQUIVALENCE (Filea(2),Nac) , (Filea(3),Nar) , (Filea(4),Naform) , (Filea(5),Natype) , (Filea(6),Nanzwd) , (Filea(7),Nadens)
   !>>>>EQUIVALENCE (Fileb(2),Nbc) , (Fileb(3),Nbr) , (Fileb(4),Nbform) , (Fileb(5),Nbtype) , (Fileb(6),Nbnzwd) , (Fileb(7),Nbdens)
   !>>>>EQUIVALENCE (Filec(2),Ncc) , (Filec(3),Ncr) , (Filec(4),Ncform) , (Filec(5),Nctype) , (Filec(6),Ncnzwd) , (Filec(7),Ncdens)
   !>>>>EQUIVALENCE (Filed(2),Ndc) , (Filed(3),Ndr) , (Filed(4),Ndform) , (Filed(5),Ndtype) , (Filed(6),Ndnzwd) , (Filed(7),Nddens)
!
   DATA module/4HMMA  , 2*4H    /
   DATA jbegn/4HBEGN/ , jend/3HEND/
   DATA upmeth/'UNPACK' , 'STRING'/
   DATA prntyp/2HRS , 2HRD , 2HCS , 2HCD/
   DATA testpct/.8/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
         isave(1) = typei
         isave(2) = typep
         isave(3) = irow1p
         isave(4) = irownp
         isave(5) = incrp
         isave(6) = typeu
         isave(7) = irowu
         isave(8) = irownu
         isave(9) = incru
         CALL sswtch(19,l19)
         module(3) = jbegn
         CALL conmsg(module,3,0)
         ndr = nar
         ndc = nbc
         IF ( t/=0 ) ndr = nac
         IF ( ndform==0 ) THEN
            ndform = 2
            IF ( ndr==ndc ) ndform = 1
         ENDIF
         IF ( filea(6)/=0 .AND. fileb(6)/=0 ) THEN
            IF ( signab/=0 ) THEN
               IF ( t/=0 ) THEN
                  IF ( nar/=nbr ) THEN
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( filec(1)/=0 ) THEN
                     IF ( nac/=ncr ) THEN
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( nbc/=ncc ) THEN
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDIF
               ELSE
                  IF ( nac/=nbr ) THEN
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( filec(1)/=0 ) THEN
                     IF ( nar/=ncr ) THEN
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( nbc/=ncc ) THEN
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDIF
               ENDIF
               nwdc = 0
               CALL dssize(filea,ncols,naterms,nastrgs,nwda)
               CALL dssize(fileb,ncols,nbterms,nbstrgs,nwdb)
               IF ( filec(1)/=0 ) CALL dssize(filec,ncols,ncterms,ncstrgs,nwdc)
               nwdd = max0(nwda,nwdb,nwdc)
               ndtype = 2
               IF ( nwdd==4 ) ndtype = 4
               IF ( nwdd==1 ) ndtype = 1
               IF ( ndtype/=1 .AND. ndtype/=4 ) THEN
                  itest1 = min0(natype,nbtype,nctype)
                  itest2 = max0(natype,nbtype,nctype)
                  ndtype = 3
                  IF ( itest2==3 .AND. (natype==2 .OR. nbtype==2 .OR. nctype==2) ) ndtype = 4
                  IF ( itest2<=2 ) ndtype = 2
               ENDIF
               natotal = nac*nar*nwdd
               nbtotal = nbc*nbr*nwdd
               IF ( filec(1)/=0 ) nctotal = ncc*ncr*nwdd
               ndtotal = ndc*ndr*nwdd
               napack = 2*nac + 2*nastrgs + naterms*nwdd
               nbpack = 2*nbc + 2*nbstrgs + nbterms*nwdd
               IF ( filec(1)/=0 ) ncpack = 2*ndc + 2*ncstrgs + ncterms*nwdd
               denstya = (nadens*1.)/10000.
               denstyb = (nbdens*1.)/10000.
               IF ( filec(1)/=0 ) denstyc = (ncdens*1.)/10000.
               nastor = 2
               nbstor = 2
               ncstor = 2
               x = natotal
               y = napack
               percnta = y/x
               x = nbtotal
               y = nbpack
               percntb = y/x
               IF ( filec(1)/=0 ) THEN
                  x = nctotal
                  y = ncpack
                  percntc = y/x
               ENDIF
               IF ( percnta>=testpct ) nastor = 1
               IF ( percntb>=testpct ) nbstor = 1
               IF ( filec(1)/=0 .AND. percntc>=testpct ) ncstor = 1
               memavl = nz - 4*sysbuf
               mpass10 = (natotal/(memavl-(nbr+ndr)*nwdd)) + 1
               mpass11 = (natotal/(memavl-ndr*nwdd-(nbpack/nbc))) + 1
               mpass20 = ((nbtotal+ndtotal)/(memavl-nar*nwdd)) + 1
               mpass21 = ((nbtotal+ndtotal)/(memavl-(napack/nac))) + 1
               mpass30 = (napack/(memavl-(nbr+ndr)*nwdd)) + 1
               mpass31 = (napack/(memavl-ndr*nwdd-(nbpack/nbc))) + 1
               mpass32 = (napack/(memavl-(ncpack/ndc)-(nbpack/nbc))) + 1
               mpass40 = ((nbpack+ndtotal)/(memavl-nar*nwdd)) + 1
               mpass41 = ((nbpack+ndtotal)/(memavl-(napack/nac))) + 1
               IF ( nastor/=1 .OR. nbstor/=1 ) THEN
                  IF ( nastor==2 .AND. nbstor==1 ) THEN
!---------USE GETSTR FOR MATRIX "A"; UNPACK FOR MATRIX "B"
!         (CHOOSE METHOD 21 OR 30)
                     method = 21
                     IF ( mpass21/=1 ) THEN
                        IF ( mpass21>mpass30 ) method = 30
                     ENDIF
                     GOTO 5
                  ELSEIF ( nastor==1 .AND. nbstor==2 ) THEN
!---------USE UNPACK FOR MATRIX "A"; GETSTR FOR MATRIX "B"
!         (CHOOSE METHOD 11 OR 40)
                     method = 11
                     IF ( mpass11/=1 ) THEN
                        IF ( mpass11>mpass40 ) method = 40
                     ENDIF
                     GOTO 5
                  ELSEIF ( nastor==2 .AND. nbstor==2 ) THEN
!---------USE GETSTR FOR MATRICES "A" AND "B" (CHOOSE METHOD 31, 32 OR 41)
                     method = 31
                     IF ( mpass31==1 ) THEN
                        IF ( ncstor==2 .AND. t/=0 ) method = 32
                     ELSEIF ( mpass31<=mpass41 ) THEN
                        IF ( ncstor==2 .AND. t/=0 ) method = 32
                     ELSE
                        method = 41
                     ENDIF
                     GOTO 5
                  ENDIF
               ENDIF
!---------USE UNPACK FOR MATRICES "A" AND "B"  (CHOOSE METHOD 10 OR 20)
               method = 10
               IF ( mpass10/=1 ) THEN
                  IF ( mpass10>mpass20 ) method = 20
               ENDIF
 5             IF ( l19/=0 ) THEN
                  CALL fname(filea,namea)
                  CALL fname(fileb,nameb)
                  CALL fname(filec,namec)
                  CALL fname(filed,named)
                  WRITE (lout,99010,IOSTAT=ierr) namea , nar , nac , naterms , denstya , prntyp(natype) , nameb , nbr , nbc ,       &
                       & nbterms , denstyb , prntyp(nbtype)
                  IF ( filec(1)/=0 ) WRITE (lout,99011,IOSTAT=ierr) namec , ncr , ncc , ncterms , denstyc , prntyp(nctype)
                  WRITE (lout,99001) named , ndr , ndc , prntyp(ndtype)
99001             FORMAT ('     D- ',2A4,I8,I7,10X,7X,5X,A2)
                  WRITE (lout,99002) signab , signc , nz , ksys58
99002             FORMAT ('     SIGNAB =',I2,'  SIGNC =',I2,'  MEMORY =',I10,'  SYSTEM(58)=',I3)
                  WRITE (lout,99003) upmeth(nastor) , natotal , napack , upmeth(nbstor) , nbtotal , nbpack
99003             FORMAT ('  /-----------------------------------------------------------/',/,                                      &
                         &'  /    READ METHOD   MEMORY (FULL MATRIX)    MEMORY (STRINGS) /',/,                                      &
                         &'  /-----------------------------------------------------------/',/,'     A-  ',A6,I21,I21,/,'     B-  ', &
                        & A6,I21,I21)
                  IF ( filec(1)/=0 ) WRITE (lout,99004) upmeth(ncstor) , nctotal , ncpack
99004             FORMAT ('     C-  ',A6,I21,I21)
                  WRITE (lout,99005) t , method , prntyp(ndtype)
99005             FORMAT ('     T =',I2,'    SUGGESTED METHOD =',I2,'    "D" MATRIX TYPE:',1X,A2)
                  WRITE (lout,99006) mpass10 , mpass11 , mpass20 , mpass21 , mpass30 , mpass31 , mpass32 , mpass40 , mpass41
99006             FORMAT ('  /-----------------------------------------------------------/',                                        &
                         &/'  /       ESTIMATED NUMBER OF PASSES REQUIRED PER METHOD      /',/,                                     &
                         &'  /         10   11   20   21   30   31   32   40   41        /',/,                                      &
                         &'  /-----------------------------------------------------------/',/,'         ',9I5,/,                    &
                         &'  /-----------------------------------------------------------/')
               ENDIF
               IF ( filed(1)>=0 ) THEN
                  IF ( ksys58/=0 .AND. (ksys58>=10 .AND. ksys58<=11) .OR. (ksys58>=20 .AND. ksys58<=21) .OR.                        &
                     & (ksys58>=30 .AND. ksys58<=31) .OR. (ksys58>=40 .AND. ksys58<=41) ) method = ksys58
                  IF ( ksys58==32 .AND. t/=0 ) method = ksys58
                  IF ( method==10 ) nbstor = 1
                  IF ( method==11 ) nbstor = 2
                  IF ( method==20 ) nastor = 1
                  IF ( method==21 ) nastor = 2
                  IF ( method==30 ) nbstor = 1
                  IF ( method==31 ) nbstor = 2
                  IF ( method==32 ) nbstor = 2
                  IF ( method==40 ) nastor = 1
                  IF ( method==41 ) nastor = 2
                  IF ( method==10 .OR. method==11 ) CALL mma1(Zi,Zr,Zd,Zr,Zd)
                  IF ( method==20 .OR. method==21 ) CALL mma2(Zi,Zr,Zd,Zr,Zd)
                  IF ( method>=30 .AND. method<=32 ) CALL mma3(Zi,Zr,Zd,Zr,Zd)
                  IF ( method==40 .OR. method==41 ) CALL mma4(Zi,Zr,Zd,Zr,Zd)
                  ct = 'NT'
                  IF ( t/=0 ) ct = 'T '
                  WRITE (lout,99007) method , ct , ipass
99007             FORMAT ('   METHOD USED = ',I2,A2,'  ACTUAL NUMBER OF PASSES =',I4)
               ENDIF
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
! "A" AND "B" MATRICES ARE NULL, MOVE "C" TO "D" IF "C" EXISTS
!
         IF ( filed(1)>=0 ) THEN
            ndtype = nctype
            WRITE (lout,99008)
99008       FORMAT ('       MMA - NULL MATRIX PRODUCT')
            ibuf1 = nz - sysbuf
            ibuf2 = ibuf1 - sysbuf
            IF ( filec(1)/=0 ) THEN
               IF ( signc/=0 ) THEN
                  IF ( signc<0 ) THEN
!
! USE INTPK/BLDPK TO COPY C TO D BECAUSE SIGNS CONFLICT
!
                     filed(2) = 0
                     filed(6) = 0
                     filed(7) = 0
                     CALL gopen(filec,Zr(ibuf1),rdrew)
                     CALL gopen(filed,Zr(ibuf2),wrtrew)
                     DO i = 1 , ncc
                        CALL bldpk(ndtype,ndtype,filed,blk1,1)
                        CALL intpk(*6,filec,0,ndtype*signc,0)
                        SPAG_Loop_2_1: DO
                           CALL zntpki
                           CALL bldpki(a,irowin,filed,blk1)
                           IF ( eol/=0 ) EXIT SPAG_Loop_2_1
                        ENDDO SPAG_Loop_2_1
 6                      CALL bldpkn(filed,blk1,filed)
                     ENDDO
                     filed(3) = filec(3)
                     filed(4) = filec(4)
                     filed(5) = filec(5)
                     CALL close(filec,clsrew)
                     CALL close(filed,clsrew)
                  ELSE
!
! USE CPYSTR TO COPY "C" TO "D"
!
                     blk1(1) = filec(1)
                     blk2(1) = filed(1)
                     CALL gopen(filec,Zr(ibuf1),rdrew)
                     CALL gopen(filed,Zr(ibuf2),wrtrew)
                     DO i = 1 , ncc
                        CALL cpystr(blk1,blk2,0,0)
                     ENDDO
                     CALL close(filed,clsrew)
                     CALL close(filec,clsrew)
                     filed(2) = filec(2)
                     filed(3) = filec(3)
                     filed(4) = filec(4)
                     filed(5) = filec(5)
                     filed(6) = filec(6)
                     filed(7) = filec(7)
                  ENDIF
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
!
! CREATE NULL MATRIX BECAUSE "C" MATRIX IS NULL
!
            ndr = 0
            ndc = 0
            CALL gopen(filed,Zr(ibuf1),wrtrew)
            ndc = nbc
            ndr = nar
            IF ( nar==nbc ) ndr = nac
            dd(1) = 0.0D0
            incrp = 1
            irow1p = 1
            irownp = 1
            typei = prec1
            IF ( typei==0 ) typei = 1
            typep = typei
            numc = ndc
            filed(2) = 0
            filed(3) = ndr
            filed(5) = iprec
            filed(6) = 0
            filed(7) = 0
            DO i = 1 , numc
               CALL pack(dd,filed,filed)
            ENDDO
            CALL close(filed,clsrew)
         ENDIF
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (2)
! MATRICES ARE INCOMPATIBLE FOR MULTIPLICATION
         WRITE (nout,99009) ufm
99009    FORMAT (A23,' MATRICES FOR MULTIPLICATION HAVE INCOMPATIBLE SIZES',/)
         CALL fname(filea,namea)
         CALL fname(fileb,nameb)
         CALL fname(filec,namec)
         CALL fname(filed,named)
         WRITE (nout,99010,IOSTAT=ierr) namea , nar , nac , naterms , denstya , prntyp(natype) , nameb , nbr , nbc , nbterms ,      &
                                      & denstyb , prntyp(nbtype)
         IF ( filec(1)/=0 ) WRITE (nout,99011,IOSTAT=ierr) namec , ncr , ncc , ncterms , denstyc , prntyp(nctype)
         CALL mesage(-61,0,0)
         spag_nextblock_1 = 3
      CASE (3)
         module(3) = jend
         CALL conmsg(module,3,0)
         typei = isave(1)
         typep = isave(2)
         irow1p = isave(3)
         irownp = isave(4)
         incrp = isave(5)
         typeu = isave(6)
         irowu = isave(7)
         irownu = isave(8)
         incru = isave(9)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99010 FORMAT ('  /-----------------------------------------------------------/',/,                                                  &
             &'  /     MATRIX      ROWS   COLS     TERMS   DENS    TYPE      /',/,                                                  &
             &'  /-----------------------------------------------------------/',/,'     A- ',2A4,I8,I7,I10,F7.4,5X,A2,/,'     B- ', &
            & 2A4,I8,I7,I10,F7.4,5X,A2)
99011 FORMAT ('     C- ',2A4,I8,I7,I10,F7.4,5X,A2)
END SUBROUTINE mma
