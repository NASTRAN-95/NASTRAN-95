!*==reig.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE reig
USE C_BLANK
USE C_CONDAS
USE C_FEERCX
USE C_GIVN
USE C_INVPWX
USE C_NTIME
USE C_PACKX
USE C_REGEAN
USE C_REIGKR
USE C_STURMX
USE C_SYSTEM
USE C_UNPAKX
USE C_XMSSG
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: casecc , dm , eed , feerx , givi , i0 , icr1 , icr2 , inv , kaa , lama , maa , mgiv , mi , mode , mr , phia ,  &
                   & pout , sdet , sinv , udet , uinv , uset
   REAL :: core4 , core5 , epsii , xxx
   INTEGER , DIMENSION(4) , SAVE :: eigr
   INTEGER , DIMENSION(3) , SAVE :: error
   INTEGER :: file , i , icore6 , icore7 , icore8 , iflag , ip1 , isil , l26 , llcore , method , nnv , norm
   INTEGER , DIMENSION(7) :: givn , ix
   INTEGER , DIMENSION(12) :: icore
   INTEGER , DIMENSION(2) :: subnam
   EXTERNAL close , detm , feer , fread , givens , gopen , invpwr , korsz , locate , mesage , open , preloc , rdtrl , read , read1 ,&
          & read2 , read3 , read4 , read5 , read7 , skprec , sswtch
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     READ   KAA,MAA,MR,DM,EED,USET,CASECC/LAMA,PHIA,MI,OEIGS/C,N,IPROB
!            /V,N,NUMMOD/C,N,ICASE/C,N,XLAMDA $
!
   !>>>>EQUIVALENCE (Givn(1),Core(1))
   !>>>>EQUIVALENCE (Tcons(4),Apc) , (Tcons(5),Apu) , (Tcons(8),Mb(1)) , (error(2),subnam(1)) , (Dcore(1),Core(1),Icore(1))
   DATA eigr , casecc/307 , 3 , 107 , 1 , 107/ , sdet , udet , inv , sinv , i0/4HSDET , 4HUDET , 4HINV  , 4HSINV , 0/ , uinv ,      &
      & givi , kaa , maa , mr/4HUINV , 4HGIV  , 101 , 102 , 103/ , dm , eed , uset , lama , phia/104 , 105 , 106 , 201 , 202/ , mi ,&
      & pout , icr1 , icr2 , mode/203 , 204 , 301 , 302 , 4HMODE/ , error , feerx , mgiv/4HEED  , 4HREIG , 4H     , 4HFEER , 4HMGIV/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
         Ibuck = 1
         Lcore = korsz(Core) - Sysbuf - 3
         llcore = Lcore - Sysbuf
         CALL gopen(Lamda,Core(Lcore+1),1)
         CALL close(Lamda,2)
         IF ( Iprob(1)/=mode ) Ibuck = 3
         Sturm = -1
         Keep = 0
         Shftpt = 0.0
         Ptshft = 0.0
         Nr = 0
         Shftzo = 0.0
         CALL open(*20,casecc,Core(Lcore+1),0)
         CALL skprec(casecc,Icase)
         CALL fread(casecc,icore,166,1)
         CALL close(casecc,1)
         method = icore(5)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 20      method = -1
         spag_nextblock_1 = 2
      CASE (2)
         file = eed
         CALL preloc(*60,Core(Lcore+1),eed)
         CALL locate(*40,Core(Lcore+1),eigr(Ibuck),iflag)
         DO
            CALL read(*40,*40,eed,Core(1),18,0,iflag)
            IF ( method==icore(1) .OR. method==-1 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
!
!     NO SET NUMBER FOUND
!
 40      CALL mesage(-32,method,error)
         spag_nextblock_1 = 3
      CASE (3)
!
!     FOUND DATA CARD
!
         norm = icore(10)
         CALL close(eed,1)
!
!     TEST THE SIZE OF THE K AND M MATRICES VIA THEIR TRAILERS
!
         CALL rdtrl(Ik(1))
         CALL rdtrl(Im(1))
         IF ( Im(2)/=Ik(2) .OR. Im(3)/=Ik(3) ) THEN
!
!     K AND M MATRICES ARE NOT OF THE SAME SIZE
!
            WRITE (Nout,99001) Ufm
99001       FORMAT (A23,' 3131, INPUT STIFFNESS AND MASS MATRICES ARE NOT ','COMPATIBLE.')
            CALL mesage(-37,0,error(2))
         ENDIF
!
!     K AND M MATRICES ARE COMPATIBLE
!
!
!     CHECK TO SEE IF THE INPUT STIFFNESS AND/OR MASS MATRIX IS NULL
!
         IF ( Ik(6)==0 .OR. Im(6)==0 ) CALL mesage(-60,0,0)
!
!     SET FLAG FOR THE METHOD OF ANALYSIS AND THE PROPER
!     TYPE OF DECOMPOSITION
!
         Option = icore(2)
         Optn2 = icore(3)
         IF ( Option/=givi .AND. Option/=udet .AND. Option/=uinv ) THEN
            IF ( Option/=feerx .AND. Option/=mgiv ) THEN
               IF ( Option/=sdet .AND. Option/=sinv ) THEN
                  Option = udet
                  IF ( icore(2)==inv ) Option = uinv
                  IF ( Im(4)==6 .AND. Ik(4)==6 ) THEN
                     Option = sdet
                     IF ( icore(2)==inv ) Option = sinv
                  ENDIF
               ELSEIF ( Im(4)/=6 .OR. Ik(4)/=6 ) THEN
                  WRITE (Nout,99002) Uwm
99002             FORMAT (A25,' 2368, SYMMETRIC DECOMPOSITION IS SPECIFIED ON THE ','EIGR BULK DATA CARD, BUT',/5X,                 &
                         &'UNSYMMETRIC DECOMPOSITION WILL BE USED AS THIS IS THE ','PROPER TYPE OF DECOMPOSITION FOR THIS PROBLEM.')
                  Option = udet
                  IF ( icore(2)==sinv ) Option = uinv
               ENDIF
            ENDIF
         ENDIF
         isil = icore(12)
         i = 9
         epsii = Core(i)
         IF ( Ibuck/=3 ) THEN
!
!     CONVERT FREQUENCY TO LAMDA
!
            IF ( .NOT.((icore(2)==givi .OR. icore(2)==mgiv) .AND. icore(7)>0) ) THEN
               IF ( Core(i0+4)<0.0 ) THEN
                  WRITE (Nout,99003) Uwm
!
!     ERROR MESSAGES
!
99003             FORMAT (A25,' 2367, FREQUENCY F1 (FIELD 4) ON THE EIGR BULK DATA',' CARD IS NEGATIVE',/5X,                        &
                         &'IT IS ASSUMED TO BE ZERO FOR CALCULATION PURPOSES.',/)
                  Core(i0+4) = 0.0
               ENDIF
            ENDIF
            Core(i0+4) = Fps*Core(i0+4)*Core(i0+4)
            IF ( icore(2)/=feerx ) Core(i0+5) = Fps*Core(i0+5)*Core(i0+5)
         ENDIF
         core4 = Core(i0+4)
         core5 = Core(i0+5)
         icore6 = icore(6)
         icore7 = icore(7)
         icore8 = icore(8)
         IF ( icore(2)/=givi .AND. icore(2)/=mgiv ) THEN
            IF ( icore(2)/=feerx ) THEN
               IF ( icore(7)==0 ) icore(7) = 3*icore(6)
               icore7 = icore(7)
            ENDIF
!
!     FEER, INVERSE POWER AND DETERMINANT METHODS
!
!     CHECK IF IT IS A NORMAL MODES PROBLEM OR A BUCKLING PROBLEM
!
            IF ( Ibuck==3 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     NORMAL MODES PROBLEM
!
!     CHECK FOR APPEND
!
            IF ( Nummod>0 ) THEN
               ix(1) = phia
               CALL rdtrl(ix)
               IF ( ix(1)>0 .AND. ix(2)>0 ) THEN
!
!     NEW EIGENVALUES AND EIGENVECTORS WILL BE APPENDED TO THOSE
!     PREVIOUSLY CHECKPOINTED
!
                  Nr = ix(2)
                  IF ( Nummod<Nr ) Nr = Nummod
                  WRITE (Nout,99004) Uim , Nr
99004             FORMAT (A29,' 3143, THE EIGENVALUES AND EIGENVECTORS FOUND IN ','THIS ANALYSIS WILL BE APPENDED',/5X,'TO THE',I8, &
                         &' EIGENVALUES AND EIGENVECTORS COMPUTED EARLIER.')
!
!     RETRIEVE EIGENVALUES AND EIGENVECTORS PREVIOUSLY CHECKPOINTED.
!
!     COPY OLD EIGENVALUES FROM LAMA FILE TO ICR1 FILE.
!
!     COPY OLD EIGENVECTORS FROM PHIA FILE TO ICR2 FILE.
!
                  CALL read7(Nr,lama,phia,icr1,icr2)
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDIF
!
!     NO APPEND
!
!     CHECK IF RIGID BODY MODES ARE TO BE COMPUTED SEPARATELY
!
         ix(1) = mr
         CALL rdtrl(ix)
!
!     COMPUTE RIGID BODY MODES
!
         IF ( ix(1)>=0 ) CALL read1(dm,mr,Scr4,Scr5,Scr3,icr2,uset,Nr,icr1,Scr6)
!
!     RIGID BODY EIGENVALUES ARE ON ICR1
!
!     RIGID BODY EIGENVECTORS ARE ON ICR2
!
         IF ( Option==givi .OR. Option==mgiv ) THEN
!
!
!     GIVENS METHOD
!     *************
!
            Lfreq = core4
            Hfreq = core5
            method = 3
            Nfr = Nr
            Nprt = icore6
            Nv = icore7
            givn(1) = kaa
            givn(i0+2) = maa
            givn(i0+3) = phia
            DO i = 1 , 4
               givn(i+3) = eigr(i)
            ENDDO
            CALL givens
            nnv = givn(1)
            Nummod = N
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
         IF ( Option==feerx ) THEN
!
!
!     FEER METHOD
!     ***********
!
            Iflrva = icr1
            Iflrvc = icr2
            Xlmbda = core4
            Neig = icore7
            Iepx = icore8
            IF ( Ibuck==3 ) Neig = icore6
            Northo = Nr
            Critf = core5
            ix(1) = kaa
            CALL rdtrl(ix)
            N = ix(2)
            IF ( Critf==0. ) Critf = .001/N
            CALL feer
            method = 2
            Nummod = Mord + Nr
            CALL sswtch(26,l26)
            IF ( Nummod>Neig .AND. l26/=0 ) Nummod = Neig
            Ifilk(2) = Nord
         ELSE
            IF ( Option==sdet ) THEN
!
!
!     DETERMINANT METHOD
!     ******************
!
               Nsym = 1
            ELSEIF ( Option/=udet ) THEN
!
!
!     INVERSE POWER METHOD
!     ********************
!
               Lmin = core4
               Lmax = core5
               Noest = icore6
               Ndplus = icore7
               Ndmnus = 0
               IF ( Ibuck==3 ) Ndmnus = icore8
               Eps = epsii
               IF ( Eps<=0. ) Eps = .0001
               IF ( Eps<.000001 ) Eps = .000001
               CALL rdtrl(Ifilk(1))
               CALL rdtrl(Ifilm(1))
               Novect = Nr
               CALL invpwr
               method = 2
               Nummod = Novect
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            method = 4
            Rmin = core4
            Rmax = core5
            IF ( Rmin==0.0 ) Rmin = Rmax*1.0E-4
            Rminr = -.01*Rmin
            Nev = icore6
            IF ( Ibuck==3 .AND. epsii/=0.0 ) Epsi = epsii
            Nevm = icore7
            CALL rdtrl(Im(1))
            Iev(3) = Ik(3)
            IF ( Nevm>Ik(3) ) Nevm = Ik(3)
            Mz = Nr
!
!     PICK UP UNREMOVED FREE BODY MODES
!
            IF ( icore8>Nr ) Mz = -icore8
            Iev(2) = Nr
            CALL detm
            Nummod = Nfound + Nr
            Ifilk(2) = Iev(3)
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
!
!     SORT EIGENVECTORS AND VALUES
!
         IF ( Nummod==0 ) THEN
            Nummod = -1
            CALL read5(pout)
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ELSE
            CALL read3(Nummod,Ifilk(2),Lamda,Iev,phia,lama)
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
         IF ( method/=2 .AND. Nummod/=1 ) THEN
!
!     CHECK ORTHOGONALITY
!
            Ifilvc(1) = phia
            CALL rdtrl(Ifilvc(1))
            CALL read4(lama,Ifilvc(1),Scr1,epsii,maa)
         ENDIF
!
!     SET FLAG FOR GIVENS METHOD FOR USE IN READ2 ROUTINE
!
         Dum(1) = 0.0
         IF ( method==3 ) Dum(1) = 1.0
         Nv = nnv
!
!     FORM MODAL MASS, NORMALIZE AND FORM SUMMARY FILE.
!
         CALL read2(maa,phia,Scr1,norm,isil,xxx,mi,lama,pout,icr2,epsii,Scr6)
         spag_nextblock_1 = 7
      CASE (7)
         IF ( Nogo==14 ) WRITE (Nout,99005)
99005    FORMAT ('0*** THIS NASTRAN JOB WILL BE TERMINATED')
         RETURN
!
 60      ip1 = -1
         DO
            CALL mesage(ip1,file,subnam)
         ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE reig
