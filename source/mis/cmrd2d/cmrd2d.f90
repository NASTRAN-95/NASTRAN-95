!*==cmrd2d.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmrd2d(Iter)
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
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iter
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: dblkor , dicore , gib , gibbar , him , himbar , himprt , himscr , himtyp , i , icore , ifile , iform , ihim , ii ,    &
            & imsg , ipartn , iphim , iprc , it , itest , itphis , ityp , itype , j , jhim , k , khim , kore , lamamr , lamlen ,    &
            & lamwds , modext , ncore , nnmax , nwds , phiam , phibm , phiim , phiss , phissl , phissr , pprtn , sglkor , usetmr
   REAL(REAL64) :: dhimag , dhimg , dhimsm , dphim
   REAL(REAL64) , DIMENSION(1) :: dz
   REAL , SAVE :: epslon
   REAL :: himag , himmag , himsum , phimsm
   INTEGER , SAVE :: iscr7 , item
   INTEGER , DIMENSION(7) :: itrlr
   INTEGER , DIMENSION(2) , SAVE :: modnam
   REAL , DIMENSION(1) :: rz
   EXTERNAL calcv , close , fwdrec , gmprtn , gopen , makmcb , mesage , mpyad , mtrxi , pack , rdtrl , read , smsg , sofcls ,       &
          & sofopn , softrl , unpack , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
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
   !>>>>EQUIVALENCE (Lamamr,Infile(2)) , (Phissr,Infile(3)) , (Phissl,Infile(4)) , (Usetmr,Infile(6)) , (Phiam,Iscr(8)) ,                &
!>>>>    & (Himscr,Iscr(7)) , (Phibm,Iscr(9)) , (Gib,Iscr(8)) , (Gibbar,Iscr(11)) , (Phiim,Iscr(6)) , (Himprt,Iscr(7)) , (Himbar,Iscr(8))&
!>>>>    & , (Pprtn,Iscr(7)) , (Him,Iscr(10)) , (Rz(1),Z(1)) , (Dz(1),Z(1))
   DATA modnam/4HCMRD , 4H2D  /
   DATA epslon/1.0E-03/
   DATA item/4HGIMS/
   DATA iscr7/307/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     READ LAMA FILE
!
         IF ( Dry==-2 ) RETURN
         kore = Korbgn
         ifile = lamamr
         CALL gopen(lamamr,Z(Gbuf1),0)
         CALL fwdrec(*60,lamamr)
         lamwds = 6
         IF ( Modes ) lamwds = 7
         it = 0
         DO
            CALL read(*40,*20,lamamr,Z(Korbgn),lamwds,0,nwds)
            Korbgn = Korbgn + 6
            IF ( Korbgn>=Korlen ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            it = it + 1
         ENDDO
 20      CALL close(lamamr,1)
!
!     ZERO OUT PARTITIONING VECTOR AND SET UP MODE USE DESCRIPTION
!     RECORD
!
         modext = Korbgn
         itrlr(1) = phissr
         IF ( Iter==2 ) itrlr(1) = phissl
         CALL rdtrl(itrlr)
         itphis = itrlr(2)
         IF ( 3*itphis+modext>=Korlen ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         lamlen = lamwds*itphis
         nnmax = min0(Nmax,itphis)
         Moduse = modext + itphis
         ipartn = modext + 2*itphis
         Modlen = itphis
         DO i = 1 , itphis
            Z(Moduse+i-1) = 3
            Z(modext+i-1) = 0
            rz(ipartn+i-1) = 0.0
         ENDDO
!
!     SELECT DESIRED MODES
!
         Korbgn = modext + 3*itphis
         Nfound = 0
         SPAG_Loop_1_1: DO i = 1 , itphis
            IF ( Nfound==nnmax ) EXIT SPAG_Loop_1_1
            j = 3 + lamwds*(i-1)
            IF ( rz(kore+j)>Range(1) .AND. rz(kore+j)<Range(2) ) THEN
               Z(modext+Nfound) = i
               Nfound = Nfound + 1
               Z(Moduse+i-1) = 1
               rz(ipartn+i-1) = 1.0
            ENDIF
         ENDDO SPAG_Loop_1_1
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
         phiss = phissr
         IF ( Iter==2 ) phiss = phissl
         CALL gmprtn(phiss,0,0,phiam,0,pprtn,0,Nsub(1),Nsub(2),Z(Korbgn),icore)
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
         Fuset = usetmr
         CALL calcv(pprtn,Un,Ui,Ub,Z(Korbgn))
         CALL gmprtn(phiam,phiim,phibm,0,0,0,pprtn,Nsub(1),Nsub(2),Z(Korbgn),icore)
         khim = 0
         IF ( Ia21(6)==0 ) THEN
!
!     PHIBM IS NULL, HIM = PHIIM
!
            himscr = phiim
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
               itrlr(1) = gibbar
               CALL rdtrl(itrlr)
            ELSE
               CALL softrl(Oldnam,item,itrlr)
               itest = itrlr(1)
               IF ( itest/=1 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               CALL mtrxi(gib,Oldnam,item,0,itest)
               IF ( itest/=1 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               itrlr(1) = gib
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
            CALL makmcb(Itrlrd,himscr,itrlr(3),iform,itype)
            CALL sofcls
            T = 0
            Signab = -1
            Signc = 1
            Prec = 0
            Scr = Iscr(7)
            dblkor = Korbgn/2 + 1
            Nz = Lstzwd - 2*dblkor - 1
            CALL mpyad(dz(dblkor),dz(dblkor),dz(dblkor))
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
         jhim = him
         IF ( Iter==2 ) jhim = himbar
         CALL gopen(himscr,Z(Gbuf1),0)
         IF ( khim==0 ) CALL gopen(phiim,Z(Gbuf2),0)
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
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  Typeu = himtyp
                  Incru = 1
                  Nrowu = ii
                  ihim = Nrowu
                  CALL unpack(*24,himscr,dz(dblkor))
                  IF ( khim/=1 ) THEN
                     Typeu = Ia11(5)
                     Incru = 1
                     Nrowu = Ia11(3)
                     iphim = Nrowu
                     CALL unpack(*22,phiim,dz(dicore))
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
                        dhimg = dsqrt((dz(dblkor+k-1)**2)+(dz(dblkor+k)**2))
                        IF ( dhimg>=dhimag ) dhimag = dhimg
                        dhimsm = dhimsm + (dz(dblkor+k-1)**2) + (dz(dblkor+k)**2)
                     ENDDO
                  ELSE
                     itype = 0
                     himsum = 0.0
                     himmag = 0.0
                     DO j = 1 , ihim
                        k = 1 + 2*(j-1)
                        himag = sqrt((rz(sglkor+k-1)**2)+(rz(sglkor+k)**2))
                        IF ( himag>=himmag ) himmag = himag
                        himsum = himsum + (rz(sglkor+k-1)**2) + (rz(sglkor+k)**2)
                     ENDDO
                  ENDIF
                  IF ( khim==1 ) THEN
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  IF ( Ia11(5)==4 ) THEN
                     itype = itype + 2
                     dphim = 0.0D0
                     DO j = 1 , iphim
                        k = 1 + 2*(j-1)
                        dphim = dphim + (dz(dicore+k-1)**2) + (dz(dicore+k)**2)
                     ENDDO
                  ELSE
                     itype = itype + 1
                     phimsm = 0.0
                     DO j = 1 , iphim
                        k = 1 + 2*(j-1)
                        phimsm = phimsm + (rz(icore+k-1)**2) + (rz(icore+k)**2)
                     ENDDO
                  ENDIF
!
!     TEST FOR INCLUSION
!
                  IF ( itype==2 ) THEN
                     IF ( dphim/=0.0 ) THEN
                        IF ( sqrt(himsum)/dsqrt(dphim)>=epslon ) THEN
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDIF
                  ELSEIF ( itype==3 ) THEN
                     IF ( phimsm/=0.0 ) THEN
                        IF ( dsqrt(dhimsm)/sqrt(phimsm)>=epslon ) THEN
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDIF
                  ELSEIF ( itype==4 ) THEN
                     IF ( dphim/=0.0D0 ) THEN
                        IF ( dsqrt(dhimsm)/dsqrt(dphim)>=epslon ) THEN
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDIF
                  ELSEIF ( phimsm/=0.0 ) THEN
                     IF ( sqrt(himsum)/sqrt(phimsm)>=epslon ) THEN
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                  ENDIF
!
!     REJECT MODE
!
 22               j = Z(modext+i-1)
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
                  ihim = 2*ihim
                  IF ( himtyp==4 ) THEN
                     DO j = 1 , ihim
                        dz(dblkor+j-1) = dz(dblkor+j-1)/dhimag
                     ENDDO
                  ELSE
                     DO j = 1 , ihim
                        rz(sglkor+j-1) = rz(sglkor+j-1)/himmag
                     ENDDO
                  ENDIF
                  spag_nextblock_2 = 3
                  CYCLE SPAG_DispatchLoop_2
!
!     NULL COLUMN
!
 24               ihim = 2*ihim
                  IF ( himtyp==4 ) THEN
                     DO j = 1 , ihim
                        dz(dblkor+j-1) = 0.0D0
                     ENDDO
                  ELSE
                     DO j = 1 , ihim
                        rz(sglkor+j-1) = 0.0
                     ENDDO
                  ENDIF
                  spag_nextblock_2 = 3
               CASE (3)
!
!     PACK HIM COLUMN
!
                  Nrowp = Nrowu
                  CALL pack(dz(dblkor),jhim,itrlr)
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         CALL close(jhim,1)
         IF ( khim==0 ) CALL close(phiim,1)
         CALL close(himscr,1)
         CALL wrttrl(itrlr)
         Korbgn = kore
         IF ( khim==1 ) himscr = iscr7
         RETURN
!
!     PROCESS SYSTEM FATAL ERRORS
!
 40      imsg = -2
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 60      imsg = -3
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
!
            imsg = -1
         ELSEIF ( itest==4 ) THEN
            imsg = -2
         ELSEIF ( itest==5 ) THEN
            imsg = -3
         ELSEIF ( itest==6 ) THEN
!
            WRITE (Iprntr,99001) Ufm , modnam , item , Oldnam
99001       FORMAT (A23,' 6632, MODULE ',2A4,' - NASTRAN MATRIX FILE FOR I/O',' OF SOF ITEM ',A4,', SUBSTRUCRURE ',2A4,             &
                   &', IS PURGED.')
            Dry = -2
            RETURN
         ELSE
            WRITE (Iprntr,99002) Ufm , modnam , item , Oldnam
!
99002       FORMAT (A23,' 6215, MODULE ',2A4,' - ITEM ',A4,' OF SUBSTRUCTURE ',2A4,' PSEUDO-EXISTS ONLY.')
            Dry = -2
            RETURN
         ENDIF
         CALL smsg(imsg,item,Oldnam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE cmrd2d
