
SUBROUTINE mred2e
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Dry , Fuset , Gbuf1 , Gbuf2 , Gbuf3 , Gib , Him , Himprt , Ia(7) , Ia11(7) , Ia12(7) , Ia21(7) , Ia22(7) , Idum1 ,       &
         & Idum2 , Idum3(9) , Idum4(2) , Idum5(5) , Idum6 , Idum7(10) , Idum8 , Incrp , Incru , Infile(12) , Iprntr , Irowp ,       &
         & Irowu , Iscr(10) , Itrlra(7) , Itrlrb(7) , Itrlrc(7) , Itrlrd(7) , Korbgn , Korlen , Lamamr , Lcore , Lcr , Lstzwd ,     &
         & Modlen , Moduse , Nfound , Nmax , Nrowp , Nrowu , Nsub(3) , Nz , Oldnam(2) , Otfile(6) , Phibm , Phiss , Pprtn , Prec ,  &
         & Rule , Sbuf1 , Sbuf2 , Sbuf3 , Scr , Signab , Signc , T , Typep , Typeu , Typin , Ub , Ui , Un , Usetmr , Z(1)
   DOUBLE PRECISION Dz(1)
   LOGICAL Frebdy
   REAL Range(2) , Rz(1)
   CHARACTER*23 Ufm
   COMMON /bitpos/ Idum3 , Un , Idum7 , Ub , Ui
   COMMON /blank / Idum1 , Dry , Idum6 , Gbuf1 , Gbuf2 , Gbuf3 , Sbuf1 , Sbuf2 , Sbuf3 , Infile , Otfile , Iscr , Korlen , Korbgn , &
                 & Oldnam , Idum4 , Frebdy , Range , Nmax , Idum5 , Moduse , Nfound , Modlen , Idum2 , Lstzwd
   COMMON /mpyadx/ Itrlra , Itrlrb , Itrlrc , Itrlrd , Nz , T , Signab , Signc , Prec , Scr
   COMMON /packx / Typin , Typep , Irowp , Nrowp , Incrp
   COMMON /parmeg/ Ia , Ia11 , Ia21 , Ia12 , Ia22 , Lcr , Rule
   COMMON /patx  / Lcore , Nsub , Fuset
   COMMON /system/ Idum8 , Iprntr
   COMMON /unpakx/ Typeu , Irowu , Nrowu , Incru
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER dblkor , dicore , fbmods , himscr , himtyp , i , icore , ifile , iform , ihim , ii , imsg , ipartn , iprc , iscr4 ,      &
         & item , iter , itest , itphis , itrlr(7) , ityp , itype , j , jhim , k , kore , l , lamlen , modal , modext , modnam(2) , &
         & ncore , nnmax , nrows , nwds , phiam , phiim , sglkor
   DOUBLE PRECISION dhimag , dhimsm
   REAL epslon , himmag , himsum , phimsm , pmsm
!
! End of declarations
!
!
!     THIS SUBROUTINE CALCULATES THE MODAL TRANSFORMATION MATRIX FOR THE
!     MRED2 MODULE.
!
!     INPUT DATA
!     GINO   - LAMAMR - EIGENVALUE TABLE FOR SUBSTRUCTURE BEING REDUCED
!              PHISS  - EIGENVECTOR MATRIX FOR SUBSTRUCTURE BEING REDUCE
!     SOF    - GIMS   - G TRANSFORMATION MATRIX FOR ORIGINAL SUBSTRUCTUR
!
!     OUTPUT DATA
!     GINO   - HIM    - HIM MATRIX PARTITION
!
!     PARAMETERS
!     INPUT  - GBUF   - GINO BUFFERS
!              INFILE - INPUT FILE NUMBERS
!              OTFILE - OUTPUT FILE NUMBERS
!              ISCR   - SCRATCH FILE NUMBERS
!              KORLEN - LENGTH OF OPEN CORE
!              KORBGN - BEGINNING ADDRESS OF OPEN CORE
!              OLDNAM - NAME OF SUBSTRUCTURE BEING REDUCED
!              NMAX   - MAXIMUM NUMBER OF FREQUENCIES TO BE USED
!     OUTPUT - MODUSE - BEGINNING ADDRESS OF MODE USE DESCRIPTION ARRAY
!              MODLEN - LENGTH OF MODE USE ARRAY
!              NFOUND - NUMBER OF MODAL POINTS FOUND
!     OTHERS - HIMPRT - HIM PARTITION VECTOR
!              PPRTN  - PHISS MATRIX PARTITION VECTOR
!              PHIAM  - PHIAM MATRIX PARTITION
!              PHIBM  - PHIBM MATRIX PARTITION
!              PHIIM  - PHIIM MATRIX PARTITION
!              IPARTN - BEGINNING ADDRESS OF PHISS PARTITION VECTOR
!              LAMAMR - LAMAMR INPUT FILE NUMBER
!              PHISS  - PHISS INPUT FILE NUMBER
!              PPRTN  - PARTITION VECTOR FILE NUMBER
!              HIMPRT - HIM PARTITION VECTOR FILE NUMBER
!              GIB    - GIB INPUT FILE NUMBER
!              PHIAM  - PHIAM PARTITION MATRIX FILE NUMBER
!              PHIBM  - PHIBM PARTITION MATRIX FILE NUMBER
!              PHIIM  - PHIIM PARTITION MATRIX FILE NUMBER
!              HIM    - HIM INPUT FILE NUMBER
!              HIMSCR - HIM SCRATCH INPUT FILE NUMBER
!
   EQUIVALENCE (Lamamr,Infile(2)) , (Phiss,Infile(3)) , (Usetmr,Infile(5))
   EQUIVALENCE (Gib,Iscr(8)) , (Pprtn,Iscr(5)) , (Him,Iscr(8)) , (Himprt,Iscr(9)) , (Phibm,Iscr(9))
   EQUIVALENCE (Rz(1),Z(1)) , (Dz(1),Z(1))
   DATA modnam/4HMRED , 4H2E  /
   DATA epslon , iscr4 , fbmods/1.0E-03 , 304 , 6/
   DATA item/4HGIMS/
!
!     READ LAMAMR FILE
!
   IF ( Dry==-2 ) GOTO 99999
   kore = Korbgn
   ifile = Lamamr
   CALL gopen(Lamamr,Z(Gbuf1),0)
   CALL fwdrec(*500,Lamamr)
   iter = 0
   DO
      CALL read(*400,*100,Lamamr,Z(Korbgn),7,0,nwds)
!
!     REJECT MODES WITH NO ASSOCIATED VECTORS
!
      IF ( Rz(Korbgn+5)>0.0 ) THEN
         Korbgn = Korbgn + 7
         IF ( Korbgn>=Korlen ) GOTO 600
         iter = iter + 1
      ENDIF
   ENDDO
 100  CALL close(Lamamr,1)
!
!     ZERO OUT PARTITIONING VECTOR AND SET UP MODE USE DESCRIPTION
!     RECORD
!
   modext = Korbgn
   itrlr(1) = Phiss
   CALL rdtrl(itrlr)
   itphis = itrlr(2)
   nrows = itrlr(3)
   IF ( (3*itphis)+modext>=Korlen ) GOTO 600
   lamlen = 7*itphis
   nnmax = min0(Nmax,itphis)
   Moduse = modext + itphis
   ipartn = modext + 2*itphis
   Modlen = itphis
   DO i = 1 , itphis
      Z(modext+i-1) = 0
      Z(Moduse+i-1) = 3
      Rz(ipartn+i-1) = 0.0
   ENDDO
!
!     SELECT DESIRED MODES
!
   Korbgn = modext + 3*itphis
   IF ( Korbgn>=Korlen ) GOTO 600
   Nfound = 0
   DO i = 1 , itphis
      j = 4 + 7*(i-1)
      IF ( Rz(kore+j)>Range(1) .AND. Rz(kore+j)<Range(2) ) THEN
!
!     REMOVE MODES WITH NEGATIVE EIGENVALUES
!
         IF ( Rz(kore+j-2)>=0.0 ) THEN
            Z(modext+Nfound) = i
            Nfound = Nfound + 1
            Z(Moduse+i-1) = 1
            Rz(ipartn+i-1) = 1.0
         ENDIF
      ENDIF
   ENDDO
!
!     PACK OUT PARTITIONING VECTOR
!
   Typin = 1
   Typep = 1
   Irowp = 1
   Nrowp = itrlr(2)
   Incrp = 1
   iform = 2
   CALL makmcb(itrlr,Pprtn,Nrowp,iform,Typin)
   CALL gopen(Pprtn,Z(Gbuf1),1)
   CALL pack(Rz(ipartn),Pprtn,itrlr)
   CALL close(Pprtn,1)
   CALL wrttrl(itrlr)
!
!     PARTITION PHISS MATRIX
!
!        **     **   **         **
!        *       *   *   .       *
!        * PHISS * = * 0 . PHIAM *
!        *       *   *   .       *
!        **     **   **         **
!
   Nsub(1) = itphis - Nfound
   Nsub(2) = Nfound
   Nsub(3) = 0
   Lcore = Korlen - Korbgn
   icore = Lcore
!
!     TEST FOR ALL MODES
!
   IF ( Nsub(1)==0 ) THEN
      phiam = Phiss
   ELSE
      phiam = Iscr(8)
!
!     PARTITION PHIAM MATRIX
!
!                    **     **
!                    *       *
!        **     **   * PHIBM *
!        *       *   *       *
!        * PHIAM * = *.......*
!        *       *   *       *
!        **     **   * PHIIM *
!                    *       *
!                    **     **
!
      CALL gmprtn(Phiss,0,0,phiam,0,Pprtn,0,Nsub(1),Nsub(2),Z(Korbgn),icore)
   ENDIF
!
!     CALCULATE THE VECTOR MAGNITUDE
!
   IF ( Korbgn+nrows>=Korlen ) GOTO 600
   CALL gopen(phiam,Z(Gbuf1),0)
   Typeu = 1
   Irowu = 1
   Nrowu = nrows
   Incru = 1
   DO i = 1 , Nfound
      l = ipartn + i - 1
      Rz(l) = 0.0
      CALL unpack(*200,phiam,Rz(Korbgn))
      DO j = 1 , nrows
         k = Korbgn + j - 1
         Rz(l) = Rz(l) + Rz(k)**2
      ENDDO
 200  ENDDO
   CALL close(phiam,1)
   Fuset = Usetmr
   CALL calcv(Pprtn,Un,Ui,Ub,Z(Korbgn))
!
!     TEST FOR NULL B SET
!
   itrlr(1) = Pprtn
   CALL rdtrl(itrlr)
   IF ( itrlr(6)>0 ) THEN
      phiim = Iscr(7)
      CALL gmprtn(phiam,phiim,Phibm,0,0,0,Pprtn,Nsub(1),Nsub(2),Z(Korbgn),icore)
      jhim = 0
!
!     COMPUTE MODAL TRANSFORMATION MATRIX
!
!        **   **   **     **   **   ** **     **
!        *     *   *       *   *     * *       *
!        * HIM * = * PHIIM * - * GIB * * PHIBM *
!        *     *   *       *   *     * *       *
!        **   **   **     **   **   ** **     **
!
      CALL mtrxi(Gib,Oldnam,item,0,itest)
      IF ( itest/=1 ) GOTO 800
      CALL softrl(Oldnam,item,itrlr)
      itest = itrlr(1)
      IF ( itest/=1 ) GOTO 800
      DO i = 1 , 7
         Itrlra(i) = itrlr(i)
         Itrlrb(i) = Ia21(i)
         Itrlrc(i) = Ia11(i)
      ENDDO
      Itrlra(1) = Gib
      himscr = Iscr(4)
      iform = 2
      iprc = 1
      ityp = 0
      IF ( Itrlra(5)==2 .OR. Itrlra(5)==4 ) iprc = 2
      IF ( Itrlrb(5)==2 .OR. Itrlrb(5)==4 ) iprc = 2
      IF ( Itrlrc(5)==2 .OR. Itrlrc(5)==4 ) iprc = 2
      IF ( Itrlra(5)>=3 ) ityp = 2
      IF ( Itrlrb(5)>=3 ) ityp = 2
      IF ( Itrlrc(5)>=3 ) ityp = 2
      itype = iprc + ityp
      CALL makmcb(Itrlrd,himscr,itrlr(3),iform,itype)
      CALL sofcls
      T = 0
      Signab = -1
      Signc = 1
      Prec = 0
      Scr = Iscr(6)
      dblkor = Korbgn/2 + 1
      Nz = Lstzwd - ((2*dblkor)-1)
      CALL mpyad(Dz(dblkor),Dz(dblkor),Dz(dblkor))
      CALL wrttrl(Itrlrd)
      CALL sofopn(Z(Sbuf1),Z(Sbuf2),Z(Sbuf3))
      i = Itrlrd(2)
      ii = Itrlrd(3)
      iform = Itrlrd(4)
      himtyp = Itrlrd(5)
   ELSE
      phiim = phiam
      Ia11(1) = phiam
      CALL rdtrl(Ia11)
      DO i = 1 , 7
         Ia21(i) = 0
      ENDDO
!
!     PHIBM IS NULL, HIM = PHIIM
!
      himscr = phiim
      i = Ia11(2)
      ii = Ia11(3)
      iform = Ia11(4)
      himtyp = Ia11(5)
      jhim = 1
   ENDIF
!
!     TEST SELECTED MODES
!
   ncore = i
   IF ( Korbgn+ncore>=Korlen ) GOTO 600
   Typin = himtyp
   Typep = himtyp
   Irowp = 1
   Nrowp = ii
   Incrp = 1
   Irowu = 1
   CALL gopen(himscr,Z(Gbuf1),0)
   CALL makmcb(itrlr,Him,ii,iform,himtyp)
   CALL gopen(Him,Z(Gbuf3),1)
   Nfound = 0
   iter = i
   dblkor = Korbgn/2 + 1
   sglkor = 2*dblkor - 1
   IF ( himtyp==1 ) dicore = (sglkor+ii)/2 + 1
   IF ( himtyp==2 ) dicore = dblkor + ii
   icore = 2*dicore - 1
!
!     UNPACK HIM COLUMN
!
   DO i = 1 , iter
!
!     LIMIT VECTORS TO NMAX
!
      IF ( Nfound<nnmax ) THEN
         Typeu = himtyp
         Incru = 1
         Nrowu = ii
         ihim = Nrowu
         CALL unpack(*250,himscr,Dz(dblkor))
!
!     SAVE LARGEST HIM COLUMN VALUE AND CALCULATE MAGNITUDE OF HIM,
!     COLUMN
!
         IF ( himtyp==2 ) THEN
            itype = 2
            dhimsm = 0.0D0
            dhimag = 0.0D0
            DO j = 1 , ihim
               IF ( dabs(Dz(dblkor+j-1))>=dabs(dhimag) ) dhimag = Dz(dblkor+j-1)
               dhimsm = dhimsm + Dz(dblkor+j-1)**2
            ENDDO
            himsum = dhimsm
         ELSE
            itype = 0
            himsum = 0.0
            himmag = 0.0
            DO j = 1 , ihim
               IF ( abs(Rz(sglkor+j-1))>=abs(himmag) ) himmag = Rz(sglkor+j-1)
               himsum = himsum + (Rz(sglkor+j-1)**2)
            ENDDO
         ENDIF
         IF ( jhim==1 ) GOTO 300
         phimsm = Rz(ipartn+i-1)
         IF ( phimsm>0.0 ) THEN
            pmsm = phimsm*epslon*epslon
            IF ( himsum>=pmsm ) GOTO 300
         ENDIF
      ELSE
         j = Z(modext+i-1) + Moduse - 1
         Z(j) = 3
         CYCLE
      ENDIF
!
!     REJECT MODE
!
 250  j = Z(modext+i-1)
      Z(Moduse+j-1) = 2
      CYCLE
!
!     USE MODE
!
 300  Nfound = Nfound + 1
!
!     SCALE HIM COLUMN
!
      IF ( himtyp==2 ) THEN
         DO j = 1 , ihim
            Dz(dblkor+j-1) = Dz(dblkor+j-1)/dhimag
         ENDDO
      ELSE
         DO j = 1 , ihim
            Rz(sglkor+j-1) = Rz(sglkor+j-1)/himmag
         ENDDO
      ENDIF
!
!     PACK HIM COLUMN
!
      Nrowp = Nrowu
      CALL pack(Dz(dblkor),Him,itrlr)
   ENDDO
   CALL close(Him,1)
   IF ( jhim==0 ) CALL close(phiim,1)
   CALL close(himscr,1)
   CALL wrttrl(itrlr)
   Korbgn = kore
   IF ( jhim==1 ) himscr = iscr4
!
!     TEST NUMBER OF MODAL POINTS
!
   modal = itrlr(2)
   IF ( Frebdy ) modal = modal + fbmods
   IF ( modal>itrlr(3) ) THEN
      WRITE (Iprntr,99001) Ufm , Oldnam , modal , itrlr(3)
99001 FORMAT (A23,' 6633, FOR SUBSTRUCTURE ',2A4,' THE TOTAL NUMBER OF',' MODAL COORDINATES (',I8,1H),/30X,                         &
             &'IS LARGER THAN THE NUMBER OF INTERNAL DOF (',I8,2H).)
      Dry = -2
   ENDIF
   GOTO 99999
!
!     PROCESS SYSTEM FATAL ERRORS
!
 400  imsg = -2
   GOTO 700
 500  imsg = -3
   GOTO 700
 600  imsg = -8
   ifile = 0
 700  CALL sofcls
   CALL mesage(imsg,ifile,modnam)
   GOTO 99999
!
!     PROCESS MODULE FATAL ERRORS
!
 800  IF ( itest==3 ) THEN
      imsg = -1
   ELSEIF ( itest==4 ) THEN
      imsg = -2
   ELSEIF ( itest==5 ) THEN
      imsg = -3
   ELSEIF ( itest==6 ) THEN
      imsg = -10
      GOTO 900
   ELSE
      imsg = -11
      GOTO 900
   ENDIF
   CALL smsg(imsg,item,Oldnam)
   GOTO 99999
 900  CALL smsg1(imsg,item,Oldnam,modnam)
   Dry = -2
99999 RETURN
END SUBROUTINE mred2e
