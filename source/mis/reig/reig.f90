!*==reig.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE reig
   USE c_blank
   USE c_condas
   USE c_feercx
   USE c_givn
   USE c_invpwx
   USE c_ntime
   USE c_packx
   USE c_regean
   USE c_reigkr
   USE c_sturmx
   USE c_system
   USE c_unpakx
   USE c_xmssg
   USE c_zzzzzz
   USE iso_fortran_env
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
         ibuck = 1
         lcore = korsz(core) - sysbuf - 3
         llcore = lcore - sysbuf
         CALL gopen(lamda,core(lcore+1),1)
         CALL close(lamda,2)
         IF ( iprob(1)/=mode ) ibuck = 3
         sturm = -1
         keep = 0
         shftpt = 0.0
         ptshft = 0.0
         nr = 0
         shftzo = 0.0
         CALL open(*20,casecc,core(lcore+1),0)
         CALL skprec(casecc,icase)
         CALL fread(casecc,icore,166,1)
         CALL close(casecc,1)
         method = icore(5)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 20      method = -1
         spag_nextblock_1 = 2
      CASE (2)
         file = eed
         CALL preloc(*60,core(lcore+1),eed)
         CALL locate(*40,core(lcore+1),eigr(ibuck),iflag)
         DO
            CALL read(*40,*40,eed,core(1),18,0,iflag)
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
         CALL rdtrl(ik(1))
         CALL rdtrl(im(1))
         IF ( im(2)/=ik(2) .OR. im(3)/=ik(3) ) THEN
!
!     K AND M MATRICES ARE NOT OF THE SAME SIZE
!
            WRITE (nout,99001) ufm
99001       FORMAT (A23,' 3131, INPUT STIFFNESS AND MASS MATRICES ARE NOT ','COMPATIBLE.')
            CALL mesage(-37,0,error(2))
         ENDIF
!
!     K AND M MATRICES ARE COMPATIBLE
!
!
!     CHECK TO SEE IF THE INPUT STIFFNESS AND/OR MASS MATRIX IS NULL
!
         IF ( ik(6)==0 .OR. im(6)==0 ) CALL mesage(-60,0,0)
!
!     SET FLAG FOR THE METHOD OF ANALYSIS AND THE PROPER
!     TYPE OF DECOMPOSITION
!
         option = icore(2)
         optn2 = icore(3)
         IF ( option/=givi .AND. option/=udet .AND. option/=uinv ) THEN
            IF ( option/=feerx .AND. option/=mgiv ) THEN
               IF ( option/=sdet .AND. option/=sinv ) THEN
                  option = udet
                  IF ( icore(2)==inv ) option = uinv
                  IF ( im(4)==6 .AND. ik(4)==6 ) THEN
                     option = sdet
                     IF ( icore(2)==inv ) option = sinv
                  ENDIF
               ELSEIF ( im(4)/=6 .OR. ik(4)/=6 ) THEN
                  WRITE (nout,99002) uwm
99002             FORMAT (A25,' 2368, SYMMETRIC DECOMPOSITION IS SPECIFIED ON THE ','EIGR BULK DATA CARD, BUT',/5X,                 &
                         &'UNSYMMETRIC DECOMPOSITION WILL BE USED AS THIS IS THE ','PROPER TYPE OF DECOMPOSITION FOR THIS PROBLEM.')
                  option = udet
                  IF ( icore(2)==sinv ) option = uinv
               ENDIF
            ENDIF
         ENDIF
         isil = icore(12)
         i = 9
         epsii = core(i)
         IF ( ibuck/=3 ) THEN
!
!     CONVERT FREQUENCY TO LAMDA
!
            IF ( .NOT.((icore(2)==givi .OR. icore(2)==mgiv) .AND. icore(7)>0) ) THEN
               IF ( core(i0+4)<0.0 ) THEN
                  WRITE (nout,99003) uwm
!
!     ERROR MESSAGES
!
99003             FORMAT (A25,' 2367, FREQUENCY F1 (FIELD 4) ON THE EIGR BULK DATA',' CARD IS NEGATIVE',/5X,                        &
                         &'IT IS ASSUMED TO BE ZERO FOR CALCULATION PURPOSES.',/)
                  core(i0+4) = 0.0
               ENDIF
            ENDIF
            core(i0+4) = fps*core(i0+4)*core(i0+4)
            IF ( icore(2)/=feerx ) core(i0+5) = fps*core(i0+5)*core(i0+5)
         ENDIF
         core4 = core(i0+4)
         core5 = core(i0+5)
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
            IF ( ibuck==3 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     NORMAL MODES PROBLEM
!
!     CHECK FOR APPEND
!
            IF ( nummod>0 ) THEN
               ix(1) = phia
               CALL rdtrl(ix)
               IF ( ix(1)>0 .AND. ix(2)>0 ) THEN
!
!     NEW EIGENVALUES AND EIGENVECTORS WILL BE APPENDED TO THOSE
!     PREVIOUSLY CHECKPOINTED
!
                  nr = ix(2)
                  IF ( nummod<nr ) nr = nummod
                  WRITE (nout,99004) uim , nr
99004             FORMAT (A29,' 3143, THE EIGENVALUES AND EIGENVECTORS FOUND IN ','THIS ANALYSIS WILL BE APPENDED',/5X,'TO THE',I8, &
                         &' EIGENVALUES AND EIGENVECTORS COMPUTED EARLIER.')
!
!     RETRIEVE EIGENVALUES AND EIGENVECTORS PREVIOUSLY CHECKPOINTED.
!
!     COPY OLD EIGENVALUES FROM LAMA FILE TO ICR1 FILE.
!
!     COPY OLD EIGENVECTORS FROM PHIA FILE TO ICR2 FILE.
!
                  CALL read7(nr,lama,phia,icr1,icr2)
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
         IF ( ix(1)>=0 ) CALL read1(dm,mr,scr4,scr5,scr3,icr2,uset,nr,icr1,scr6)
!
!     RIGID BODY EIGENVALUES ARE ON ICR1
!
!     RIGID BODY EIGENVECTORS ARE ON ICR2
!
         IF ( option==givi .OR. option==mgiv ) THEN
!
!
!     GIVENS METHOD
!     *************
!
            lfreq = core4
            hfreq = core5
            method = 3
            nfr = nr
            nprt = icore6
            nv = icore7
            givn(1) = kaa
            givn(i0+2) = maa
            givn(i0+3) = phia
            DO i = 1 , 4
               givn(i+3) = eigr(i)
            ENDDO
            CALL givens
            nnv = givn(1)
            nummod = n
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
         IF ( option==feerx ) THEN
!
!
!     FEER METHOD
!     ***********
!
            iflrva = icr1
            iflrvc = icr2
            xlmbda = core4
            neig = icore7
            iepx = icore8
            IF ( ibuck==3 ) neig = icore6
            northo = nr
            critf = core5
            ix(1) = kaa
            CALL rdtrl(ix)
            n = ix(2)
            IF ( critf==0. ) critf = .001/n
            CALL feer
            method = 2
            nummod = mord + nr
            CALL sswtch(26,l26)
            IF ( nummod>neig .AND. l26/=0 ) nummod = neig
            ifilk(2) = nord
         ELSE
            IF ( option==sdet ) THEN
!
!
!     DETERMINANT METHOD
!     ******************
!
               nsym = 1
            ELSEIF ( option/=udet ) THEN
!
!
!     INVERSE POWER METHOD
!     ********************
!
               lmin = core4
               lmax = core5
               noest = icore6
               ndplus = icore7
               ndmnus = 0
               IF ( ibuck==3 ) ndmnus = icore8
               eps = epsii
               IF ( eps<=0. ) eps = .0001
               IF ( eps<.000001 ) eps = .000001
               CALL rdtrl(ifilk(1))
               CALL rdtrl(ifilm(1))
               novect = nr
               CALL invpwr
               method = 2
               nummod = novect
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            method = 4
            rmin = core4
            rmax = core5
            IF ( rmin==0.0 ) rmin = rmax*1.0E-4
            rminr = -.01*rmin
            nev = icore6
            IF ( ibuck==3 .AND. epsii/=0.0 ) epsi = epsii
            nevm = icore7
            CALL rdtrl(im(1))
            iev(3) = ik(3)
            IF ( nevm>ik(3) ) nevm = ik(3)
            mz = nr
!
!     PICK UP UNREMOVED FREE BODY MODES
!
            IF ( icore8>nr ) mz = -icore8
            iev(2) = nr
            CALL detm
            nummod = nfound + nr
            ifilk(2) = iev(3)
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
!
!     SORT EIGENVECTORS AND VALUES
!
         IF ( nummod==0 ) THEN
            nummod = -1
            CALL read5(pout)
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ELSE
            CALL read3(nummod,ifilk(2),lamda,iev,phia,lama)
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
         IF ( method/=2 .AND. nummod/=1 ) THEN
!
!     CHECK ORTHOGONALITY
!
            ifilvc(1) = phia
            CALL rdtrl(ifilvc(1))
            CALL read4(lama,ifilvc(1),scr1,epsii,maa)
         ENDIF
!
!     SET FLAG FOR GIVENS METHOD FOR USE IN READ2 ROUTINE
!
         dum(1) = 0.0
         IF ( method==3 ) dum(1) = 1.0
         nv = nnv
!
!     FORM MODAL MASS, NORMALIZE AND FORM SUMMARY FILE.
!
         CALL read2(maa,phia,scr1,norm,isil,xxx,mi,lama,pout,icr2,epsii,scr6)
         spag_nextblock_1 = 7
      CASE (7)
         IF ( nogo==14 ) WRITE (nout,99005)
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
