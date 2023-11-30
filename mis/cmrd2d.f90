
SUBROUTINE cmrd2d(Iter)
   IMPLICIT NONE
   INTEGER Dry , Fuset , Gbuf1 , Gbuf2 , Gbuf3 , Gib , Gibbar , Him , Himbar , Himprt , Himscr , Ia(7) , Ia11(7) , Ia12(7) , Ia21(7)&
         & , Ia22(7) , Idum1 , Idum2 , Idum3(9) , Idum4(3) , Idum5 , Idum6 , Idum7(10) , Idum8 , Idum9 , Incrp , Incru , Infile(11) &
         & , Iprntr , Irowp , Irowu , Iscr(11) , Itrlra(7) , Itrlrb(7) , Itrlrc(7) , Itrlrd(7) , Korbgn , Korlen , Lamamr , Lcore , &
         & Lcr , Lstzwd , Modlen , Moduse , Nfound , Nmax , Nrowp , Nrowu , Nsub(3) , Nz , Oldnam(2) , Otfile(6) , Phiam , Phibm ,  &
         & Phiim , Phissl , Phissr , Pprtn , Prec , Rule , Sbuf1 , Sbuf2 , Sbuf3 , Scr , Signab , Signc , T , Typep , Typeu ,       &
         & Typin , Ub , Ui , Un , Usetmr , Z(1)
   DOUBLE PRECISION Dz(1)
   LOGICAL Modes
   REAL Range(2) , Rz(1)
   CHARACTER*23 Ufm
   COMMON /bitpos/ Idum3 , Un , Idum7 , Ub , Ui
   COMMON /blank / Idum1 , Dry , Idum6 , Gbuf1 , Gbuf2 , Gbuf3 , Sbuf1 , Sbuf2 , Sbuf3 , Infile , Otfile , Iscr , Korlen , Korbgn , &
                 & Oldnam , Idum4 , Range , Nmax , Idum5 , Modes , Idum8 , Moduse , Nfound , Modlen , Idum9 , Lstzwd
   COMMON /mpyadx/ Itrlra , Itrlrb , Itrlrc , Itrlrd , Nz , T , Signab , Signc , Prec , Scr
   COMMON /packx / Typin , Typep , Irowp , Nrowp , Incrp
   COMMON /parmeg/ Ia , Ia11 , Ia21 , Ia12 , Ia22 , Lcr , Rule
   COMMON /patx  / Lcore , Nsub , Fuset
   COMMON /system/ Idum2 , Iprntr
   COMMON /unpakx/ Typeu , Irowu , Nrowu , Incru
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
   INTEGER Iter
   INTEGER dblkor , dicore , himtyp , i , icore , ifile , iform , ihim , ii , imsg , ipartn , iphim , iprc , iscr7 , it , item ,    &
         & itest , itphis , itrlr(7) , ityp , itype , j , jhim , k , khim , kore , lamlen , lamwds , modext , modnam(2) , ncore ,   &
         & nnmax , nwds , phiss , sglkor
   DOUBLE PRECISION dhimag , dhimg , dhimsm , dphim
   REAL epslon , himag , himmag , himsum , phimsm
!
!     THIS SUBROUTINE CALCULATES THE MODAL TRANSFORMATION MATRIX FOR THE
!     CMRED2 MODULE.
!
!     INPUT  DATA
!     GINO - LAMAMR - EIGENVALUE TABLE FOR SUBSTRUCTURE BEING REDUCED
!            PHISSR - RIGHT EIGENVECTOR MATRIX FOR SUBSTRUCTURE BEING
!                     REDUCED
!            PHISSL - LEFT EIGENVECTOR MATRIX FOR SUBSTRUCTURE BEING
!                     REDUCED
!     SOF  - GIMS   - G TRANSFORMATION MATRIX FOR ORIGINAL SUBSTRUCTURE
!
!     OUTPUT DATA
!     GINO - HIM    - MODAL TRANSFORMATION MATRIX
!
!     PARAMETERS
!     INPUT- GBUF   - GINO BUFFERS
!            INFILE - INPUT FILE NUMBERS
!            OTFILE - OUTPUT FILE NUMBERS
!            ISCR   - SCRATCH FILE NUMBERS
!            KORLEN - LENGTH OF OPEN CORE
!            KORBGN - BEGINNING ADDRESS OF OPEN CORE
!            OLDNAM - NAME OF SUBSTRUCTURE BEING REDUCED
!            NMAX   - MAXIMUM NUMBER OF FREQUENCIES TO BE USED
!     OUTPUT-MODUSE - BEGINNING ADDRESS OF MODE USE DESCRIPTION ARRAY
!            NFOUND - NUMBER OF MODAL POINTS FOUND
!            MODLEN - LENGTH OF MODE USE ARRAY
!     OTHERS-HIMPRT - HIM PARTITION VECTOR
!            PPRTN  - PHISS MATRIX PARTITION VECTOR
!            PHIAM  - PHIAM MATRIX PARTITION
!            PHIBM  - PHIBM MATRIX PARTITION
!            PHIIM  - PHIIM MATRIX PARTITION
!            IPARTN - BEGINNING ADDRESS OF PHISS PARTITION VECTOR
!            LAMAMR - LAMAMR INPUT FILE NUMBER
!            PHISS  - PHISS INPUT FILE NUMBER
!            PPRTN  - PARTITION VECTOR FILE NUMBER
!            HIMPRT - HIM PARTITION VECTOR FILE NUMBER
!            GIB    - GIB INPUT FILE NUMBER
!            PHIAM  - PHIAM PARTITION MATRIX FILE NUMBER
!            PHIBM  - PHIBM PARTITION MATRIX FILE NUMBER
!            PHIIM  - PHIIM PARTITION MATRIX FILE NUMBER
!            HIM    - HIM INPUT FILE NUMBER
!            HIMSCR - HIM SCRATCH INPUT FILE NUMBER
!
   EQUIVALENCE (Lamamr,Infile(2)) , (Phissr,Infile(3)) , (Phissl,Infile(4)) , (Usetmr,Infile(6)) , (Phiam,Iscr(8)) ,                &
    & (Himscr,Iscr(7)) , (Phibm,Iscr(9)) , (Gib,Iscr(8)) , (Gibbar,Iscr(11)) , (Phiim,Iscr(6)) , (Himprt,Iscr(7)) , (Himbar,Iscr(8))&
    & , (Pprtn,Iscr(7)) , (Him,Iscr(10)) , (Rz(1),Z(1)) , (Dz(1),Z(1))
   DATA modnam/4HCMRD , 4H2D  /
   DATA epslon/1.0E-03/
   DATA item/4HGIMS/
   DATA iscr7/307/
!
!     READ LAMA FILE
!
   IF ( Dry==-2 ) RETURN
   kore = Korbgn
   ifile = Lamamr
   CALL gopen(Lamamr,Z(Gbuf1),0)
   CALL fwdrec(*500,Lamamr)
   lamwds = 6
   IF ( Modes ) lamwds = 7
   it = 0
   DO
      CALL read(*400,*100,Lamamr,Z(Korbgn),lamwds,0,nwds)
      Korbgn = Korbgn + 6
      IF ( Korbgn>=Korlen ) GOTO 600
      it = it + 1
   ENDDO
 100  CALL close(Lamamr,1)
!
!     ZERO OUT PARTITIONING VECTOR AND SET UP MODE USE DESCRIPTION
!     RECORD
!
   modext = Korbgn
   itrlr(1) = Phissr
   IF ( Iter==2 ) itrlr(1) = Phissl
   CALL rdtrl(itrlr)
   itphis = itrlr(2)
   IF ( 3*itphis+modext>=Korlen ) GOTO 600
   lamlen = lamwds*itphis
   nnmax = min0(Nmax,itphis)
   Moduse = modext + itphis
   ipartn = modext + 2*itphis
   Modlen = itphis
   DO i = 1 , itphis
      Z(Moduse+i-1) = 3
      Z(modext+i-1) = 0
      Rz(ipartn+i-1) = 0.0
   ENDDO
!
!     SELECT DESIRED MODES
!
   Korbgn = modext + 3*itphis
   Nfound = 0
   DO i = 1 , itphis
      IF ( Nfound==nnmax ) EXIT
      j = 3 + lamwds*(i-1)
      IF ( Rz(kore+j)>Range(1) .AND. Rz(kore+j)<Range(2) ) THEN
         Z(modext+Nfound) = i
         Nfound = Nfound + 1
         Z(Moduse+i-1) = 1
         Rz(ipartn+i-1) = 1.0
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
   Korbgn = Korbgn - itphis
!
!     PARTITION PHISS(R,L) MATRICES
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
   phiss = Phissr
   IF ( Iter==2 ) phiss = Phissl
   CALL gmprtn(phiss,0,0,Phiam,0,Pprtn,0,Nsub(1),Nsub(2),Z(Korbgn),icore)
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
   Fuset = Usetmr
   CALL calcv(Pprtn,Un,Ui,Ub,Z(Korbgn))
   CALL gmprtn(Phiam,Phiim,Phibm,0,0,0,Pprtn,Nsub(1),Nsub(2),Z(Korbgn),icore)
   khim = 0
   IF ( Ia21(6)==0 ) THEN
!
!     PHIBM IS NULL, HIM = PHIIM
!
      Himscr = Phiim
      i = Ia11(2)
      ii = Ia11(3)
      iform = Ia11(4)
      himtyp = Ia11(5)
      khim = 1
      dblkor = Korbgn/2 + 1
   ELSE
!
!     COMPUTE MODAL TRANSFORMATION MATRIX
!
!        **   **   **     **   **   ** **     **
!        *     *   *       *   *     * *       *
!        * HIM * = * PHIIM * - * GIB * * PHIBM *
!        *     *   *       *   *     * *       *
!        **   **   **     **   **   ** **     **
!
      IF ( Iter==2 ) THEN
         itrlr(1) = Gibbar
         CALL rdtrl(itrlr)
      ELSE
         CALL softrl(Oldnam,item,itrlr)
         itest = itrlr(1)
         IF ( itest/=1 ) GOTO 800
         CALL mtrxi(Gib,Oldnam,item,0,itest)
         IF ( itest/=1 ) GOTO 800
         itrlr(1) = Gib
      ENDIF
      DO i = 1 , 7
         Itrlra(i) = itrlr(i)
         Itrlrb(i) = Ia21(i)
         Itrlrc(i) = Ia11(i)
      ENDDO
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
      CALL makmcb(Itrlrd,Himscr,itrlr(3),iform,itype)
      CALL sofcls
      T = 0
      Signab = -1
      Signc = 1
      Prec = 0
      Scr = Iscr(7)
      dblkor = Korbgn/2 + 1
      Nz = Lstzwd - 2*dblkor - 1
      CALL mpyad(Dz(dblkor),Dz(dblkor),Dz(dblkor))
      CALL wrttrl(Itrlrd)
      CALL sofopn(Z(Sbuf1),Z(Sbuf2),Z(Sbuf3))
      i = Itrlrd(2)
      ii = Itrlrd(3)
      iform = Itrlrd(4)
      himtyp = Itrlrd(5)
   ENDIF
!
!     TEST SELECTED MODES
!
   ncore = 4*ii
   IF ( khim==0 ) ncore = ncore + 4*Ia11(3)
   IF ( Korbgn+ncore>=Korlen ) GOTO 600
   Typin = himtyp
   Typep = himtyp
   Irowp = 1
   Nrowp = ii
   Incrp = 1
   Irowu = 1
   jhim = Him
   IF ( Iter==2 ) jhim = Himbar
   CALL gopen(Himscr,Z(Gbuf1),0)
   IF ( khim==0 ) CALL gopen(Phiim,Z(Gbuf2),0)
   CALL makmcb(itrlr,jhim,ii,iform,himtyp)
   CALL gopen(jhim,Z(Gbuf3),1)
   Nfound = 0
   it = i
   dblkor = Korbgn/2 + 1
   sglkor = 2*dblkor - 1
   IF ( himtyp==3 ) dicore = ((sglkor+2*ii)/2) + 1
   IF ( himtyp==4 ) dicore = dblkor + 2*ii
   icore = 2*dicore - 1
!
!     UNPACK HIM AND PHIIM COLUMNS
!
   DO i = 1 , it
      Typeu = himtyp
      Incru = 1
      Nrowu = ii
      ihim = Nrowu
      CALL unpack(*250,Himscr,Dz(dblkor))
      IF ( khim/=1 ) THEN
         Typeu = Ia11(5)
         Incru = 1
         Nrowu = Ia11(3)
         iphim = Nrowu
         CALL unpack(*150,Phiim,Dz(dicore))
      ENDIF
!
!     SAVE LARGEST HIM COLUMN VALUE AND CALCULATE MAGNITUDE OF HIM,
!     PHIIM COLUMNS
!
      IF ( himtyp==4 ) THEN
         itype = 1
         dhimsm = 0.0D0
         dhimag = 0.0D0
         DO j = 1 , ihim
            k = 1 + 2*(j-1)
            dhimg = dsqrt((Dz(dblkor+k-1)**2)+(Dz(dblkor+k)**2))
            IF ( dhimg>=dhimag ) dhimag = dhimg
            dhimsm = dhimsm + (Dz(dblkor+k-1)**2) + (Dz(dblkor+k)**2)
         ENDDO
      ELSE
         itype = 0
         himsum = 0.0
         himmag = 0.0
         DO j = 1 , ihim
            k = 1 + 2*(j-1)
            himag = sqrt((Rz(sglkor+k-1)**2)+(Rz(sglkor+k)**2))
            IF ( himag>=himmag ) himmag = himag
            himsum = himsum + (Rz(sglkor+k-1)**2) + (Rz(sglkor+k)**2)
         ENDDO
      ENDIF
      IF ( khim==1 ) GOTO 200
      IF ( Ia11(5)==4 ) THEN
         itype = itype + 2
         dphim = 0.0D0
         DO j = 1 , iphim
            k = 1 + 2*(j-1)
            dphim = dphim + (Dz(dicore+k-1)**2) + (Dz(dicore+k)**2)
         ENDDO
      ELSE
         itype = itype + 1
         phimsm = 0.0
         DO j = 1 , iphim
            k = 1 + 2*(j-1)
            phimsm = phimsm + (Rz(icore+k-1)**2) + (Rz(icore+k)**2)
         ENDDO
      ENDIF
!
!     TEST FOR INCLUSION
!
      IF ( itype==2 ) THEN
         IF ( dphim/=0.0 ) THEN
            IF ( sqrt(himsum)/dsqrt(dphim)>=epslon ) GOTO 200
         ENDIF
      ELSEIF ( itype==3 ) THEN
         IF ( phimsm/=0.0 ) THEN
            IF ( dsqrt(dhimsm)/sqrt(phimsm)>=epslon ) GOTO 200
         ENDIF
      ELSEIF ( itype==4 ) THEN
         IF ( dphim/=0.0D0 ) THEN
            IF ( dsqrt(dhimsm)/dsqrt(dphim)>=epslon ) GOTO 200
         ENDIF
      ELSEIF ( phimsm/=0.0 ) THEN
         IF ( sqrt(himsum)/sqrt(phimsm)>=epslon ) GOTO 200
      ENDIF
!
!     REJECT MODE
!
 150  j = Z(modext+i-1)
      Z(Moduse+j-1) = 2
      CYCLE
!
!     USE MODE
!
 200  Nfound = Nfound + 1
!
!     SCALE HIM COLUMN
!
      ihim = 2*ihim
      IF ( himtyp==4 ) THEN
         DO j = 1 , ihim
            Dz(dblkor+j-1) = Dz(dblkor+j-1)/dhimag
         ENDDO
      ELSE
         DO j = 1 , ihim
            Rz(sglkor+j-1) = Rz(sglkor+j-1)/himmag
         ENDDO
      ENDIF
      GOTO 300
!
!     NULL COLUMN
!
 250  ihim = 2*ihim
      IF ( himtyp==4 ) THEN
         DO j = 1 , ihim
            Dz(dblkor+j-1) = 0.0D0
         ENDDO
      ELSE
         DO j = 1 , ihim
            Rz(sglkor+j-1) = 0.0
         ENDDO
      ENDIF
!
!     PACK HIM COLUMN
!
 300  Nrowp = Nrowu
      CALL pack(Dz(dblkor),jhim,itrlr)
   ENDDO
   CALL close(jhim,1)
   IF ( khim==0 ) CALL close(Phiim,1)
   CALL close(Himscr,1)
   CALL wrttrl(itrlr)
   Korbgn = kore
   IF ( khim==1 ) Himscr = iscr7
   RETURN
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
   RETURN
!
!     PROCESS MODULE FATAL ERRORS
!
 800  IF ( itest==3 ) THEN
!
      imsg = -1
   ELSEIF ( itest==4 ) THEN
      imsg = -2
   ELSEIF ( itest==5 ) THEN
      imsg = -3
   ELSEIF ( itest==6 ) THEN
!
      WRITE (Iprntr,99001) Ufm , modnam , item , Oldnam
99001 FORMAT (A23,' 6632, MODULE ',2A4,' - NASTRAN MATRIX FILE FOR I/O',' OF SOF ITEM ',A4,', SUBSTRUCRURE ',2A4,', IS PURGED.')
      Dry = -2
      RETURN
   ELSE
      WRITE (Iprntr,99002) Ufm , modnam , item , Oldnam
!
99002 FORMAT (A23,' 6215, MODULE ',2A4,' - ITEM ',A4,' OF SUBSTRUCTURE ',2A4,' PSEUDO-EXISTS ONLY.')
      Dry = -2
      RETURN
   ENDIF
   CALL smsg(imsg,item,Oldnam)
   RETURN
!
END SUBROUTINE cmrd2d
