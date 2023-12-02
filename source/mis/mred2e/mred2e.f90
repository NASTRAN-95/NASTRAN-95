!*==mred2e.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mred2e
USE C_BITPOS
USE C_BLANK
USE C_MPYADX
USE C_PACKX
USE C_PARMEG
USE C_PATX
USE C_SYSTEM
USE C_UNPAKX
USE C_XMSSG
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: dblkor , dicore , gib , him , himprt , himscr , himtyp , i , icore , ifile , iform , ihim , ii , imsg , ipartn ,      &
            & iprc , iter , itest , itphis , ityp , itype , j , jhim , k , kore , l , lamamr , lamlen , modal , modext , ncore ,    &
            & nnmax , nrows , nwds , phiam , phibm , phiim , phiss , pprtn , sglkor , usetmr
   REAL(REAL64) :: dhimag , dhimsm
   REAL(REAL64) , DIMENSION(1) :: dz
   REAL , SAVE :: epslon
   INTEGER , SAVE :: fbmods , iscr4 , item
   REAL :: himmag , himsum , phimsm , pmsm
   INTEGER , DIMENSION(7) :: itrlr
   INTEGER , DIMENSION(2) , SAVE :: modnam
   REAL , DIMENSION(1) :: rz
   EXTERNAL calcv , close , fwdrec , gmprtn , gopen , makmcb , mesage , mpyad , mtrxi , pack , rdtrl , read , smsg , smsg1 ,        &
          & sofcls , sofopn , softrl , unpack , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
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
   !>>>>EQUIVALENCE (Lamamr,Infile(2)) , (Phiss,Infile(3)) , (Usetmr,Infile(5))
   !>>>>EQUIVALENCE (Gib,Iscr(8)) , (Pprtn,Iscr(5)) , (Him,Iscr(8)) , (Himprt,Iscr(9)) , (Phibm,Iscr(9))
   !>>>>EQUIVALENCE (Rz(1),Z(1)) , (Dz(1),Z(1))
   DATA modnam/4HMRED , 4H2E  /
   DATA epslon , iscr4 , fbmods/1.0E-03 , 304 , 6/
   DATA item/4HGIMS/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     READ LAMAMR FILE
!
         IF ( Dry==-2 ) RETURN
         kore = Korbgn
         ifile = lamamr
         CALL gopen(lamamr,Z(Gbuf1),0)
         CALL fwdrec(*80,lamamr)
         iter = 0
         DO
            CALL read(*60,*20,lamamr,Z(Korbgn),7,0,nwds)
!
!     REJECT MODES WITH NO ASSOCIATED VECTORS
!
            IF ( rz(Korbgn+5)>0.0 ) THEN
               Korbgn = Korbgn + 7
               IF ( Korbgn>=Korlen ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               iter = iter + 1
            ENDIF
         ENDDO
 20      CALL close(lamamr,1)
!
!     ZERO OUT PARTITIONING VECTOR AND SET UP MODE USE DESCRIPTION
!     RECORD
!
         modext = Korbgn
         itrlr(1) = phiss
         CALL rdtrl(itrlr)
         itphis = itrlr(2)
         nrows = itrlr(3)
         IF ( (3*itphis)+modext>=Korlen ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         lamlen = 7*itphis
         nnmax = min0(Nmax,itphis)
         Moduse = modext + itphis
         ipartn = modext + 2*itphis
         Modlen = itphis
         DO i = 1 , itphis
            Z(modext+i-1) = 0
            Z(Moduse+i-1) = 3
            rz(ipartn+i-1) = 0.0
         ENDDO
!
!     SELECT DESIRED MODES
!
         Korbgn = modext + 3*itphis
         IF ( Korbgn>=Korlen ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Nfound = 0
         DO i = 1 , itphis
            j = 4 + 7*(i-1)
            IF ( rz(kore+j)>Range(1) .AND. rz(kore+j)<Range(2) ) THEN
!
!     REMOVE MODES WITH NEGATIVE EIGENVALUES
!
               IF ( rz(kore+j-2)>=0.0 ) THEN
                  Z(modext+Nfound) = i
                  Nfound = Nfound + 1
                  Z(Moduse+i-1) = 1
                  rz(ipartn+i-1) = 1.0
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
         CALL makmcb(itrlr,pprtn,Nrowp,iform,Typin)
         CALL gopen(pprtn,Z(Gbuf1),1)
         CALL pack(rz(ipartn),pprtn,itrlr)
         CALL close(pprtn,1)
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
            phiam = phiss
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
            CALL gmprtn(phiss,0,0,phiam,0,pprtn,0,Nsub(1),Nsub(2),Z(Korbgn),icore)
         ENDIF
!
!     CALCULATE THE VECTOR MAGNITUDE
!
         IF ( Korbgn+nrows>=Korlen ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL gopen(phiam,Z(Gbuf1),0)
         Typeu = 1
         Irowu = 1
         Nrowu = nrows
         Incru = 1
         DO i = 1 , Nfound
            l = ipartn + i - 1
            rz(l) = 0.0
            CALL unpack(*40,phiam,rz(Korbgn))
            DO j = 1 , nrows
               k = Korbgn + j - 1
               rz(l) = rz(l) + rz(k)**2
            ENDDO
 40      ENDDO
         CALL close(phiam,1)
         Fuset = usetmr
         CALL calcv(pprtn,Un,Ui,Ub,Z(Korbgn))
!
!     TEST FOR NULL B SET
!
         itrlr(1) = pprtn
         CALL rdtrl(itrlr)
         IF ( itrlr(6)>0 ) THEN
            phiim = Iscr(7)
            CALL gmprtn(phiam,phiim,phibm,0,0,0,pprtn,Nsub(1),Nsub(2),Z(Korbgn),icore)
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
            CALL mtrxi(gib,Oldnam,item,0,itest)
            IF ( itest/=1 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL softrl(Oldnam,item,itrlr)
            itest = itrlr(1)
            IF ( itest/=1 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            DO i = 1 , 7
               Itrlra(i) = itrlr(i)
               Itrlrb(i) = Ia21(i)
               Itrlrc(i) = Ia11(i)
            ENDDO
            Itrlra(1) = gib
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
            CALL mpyad(dz(dblkor),dz(dblkor),dz(dblkor))
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
         IF ( Korbgn+ncore>=Korlen ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Typin = himtyp
         Typep = himtyp
         Irowp = 1
         Nrowp = ii
         Incrp = 1
         Irowu = 1
         CALL gopen(himscr,Z(Gbuf1),0)
         CALL makmcb(itrlr,him,ii,iform,himtyp)
         CALL gopen(him,Z(Gbuf3),1)
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
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
!
!     LIMIT VECTORS TO NMAX
!
                  IF ( Nfound<nnmax ) THEN
                     Typeu = himtyp
                     Incru = 1
                     Nrowu = ii
                     ihim = Nrowu
                     CALL unpack(*42,himscr,dz(dblkor))
!
!     SAVE LARGEST HIM COLUMN VALUE AND CALCULATE MAGNITUDE OF HIM,
!     COLUMN
!
                     IF ( himtyp==2 ) THEN
                        itype = 2
                        dhimsm = 0.0D0
                        dhimag = 0.0D0
                        DO j = 1 , ihim
                           IF ( dabs(dz(dblkor+j-1))>=dabs(dhimag) ) dhimag = dz(dblkor+j-1)
                           dhimsm = dhimsm + dz(dblkor+j-1)**2
                        ENDDO
                        himsum = dhimsm
                     ELSE
                        itype = 0
                        himsum = 0.0
                        himmag = 0.0
                        DO j = 1 , ihim
                           IF ( abs(rz(sglkor+j-1))>=abs(himmag) ) himmag = rz(sglkor+j-1)
                           himsum = himsum + (rz(sglkor+j-1)**2)
                        ENDDO
                     ENDIF
                     IF ( jhim==1 ) THEN
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                     phimsm = rz(ipartn+i-1)
                     IF ( phimsm>0.0 ) THEN
                        pmsm = phimsm*epslon*epslon
                        IF ( himsum>=pmsm ) THEN
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDIF
                  ELSE
                     j = Z(modext+i-1) + Moduse - 1
                     Z(j) = 3
                     CYCLE
                  ENDIF
!
!     REJECT MODE
!
 42               j = Z(modext+i-1)
                  Z(Moduse+j-1) = 2
                  CYCLE
               CASE (2)
!
!     USE MODE
!
                  Nfound = Nfound + 1
!
!     SCALE HIM COLUMN
!
                  IF ( himtyp==2 ) THEN
                     DO j = 1 , ihim
                        dz(dblkor+j-1) = dz(dblkor+j-1)/dhimag
                     ENDDO
                  ELSE
                     DO j = 1 , ihim
                        rz(sglkor+j-1) = rz(sglkor+j-1)/himmag
                     ENDDO
                  ENDIF
!
!     PACK HIM COLUMN
!
                  Nrowp = Nrowu
                  CALL pack(dz(dblkor),him,itrlr)
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         CALL close(him,1)
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
99001       FORMAT (A23,' 6633, FOR SUBSTRUCTURE ',2A4,' THE TOTAL NUMBER OF',' MODAL COORDINATES (',I8,1H),/30X,                   &
                   &'IS LARGER THAN THE NUMBER OF INTERNAL DOF (',I8,2H).)
            Dry = -2
         ENDIF
         RETURN
!
!     PROCESS SYSTEM FATAL ERRORS
!
 60      imsg = -2
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 80      imsg = -3
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (2)
         imsg = -8
         ifile = 0
         spag_nextblock_1 = 3
      CASE (3)
         CALL sofcls
         CALL mesage(imsg,ifile,modnam)
         RETURN
      CASE (4)
!
!     PROCESS MODULE FATAL ERRORS
!
         IF ( itest==3 ) THEN
            imsg = -1
         ELSEIF ( itest==4 ) THEN
            imsg = -2
         ELSEIF ( itest==5 ) THEN
            imsg = -3
         ELSEIF ( itest==6 ) THEN
            imsg = -10
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ELSE
            imsg = -11
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL smsg(imsg,item,Oldnam)
         RETURN
      CASE (5)
         CALL smsg1(imsg,item,Oldnam,modnam)
         Dry = -2
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE mred2e
