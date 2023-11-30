
SUBROUTINE reig
   IMPLICIT NONE
   REAL Apc , Apu , Core(1) , Critf , D1 , D2 , D4 , Degra , Dmpfle , Dum(100) , Eps , Epsi , Fps , Hfreq , Lfreq , Lmax , Lmin ,   &
      & Mb(1) , Order , Pi , Ptshft , Radeg , Rmax , Rmin , Rminr , Shftpt , Shftzo , Sr1fle , Sr2fle , Sr3fle , Sr4fle , Sr5fle ,  &
      & Sr6fle , Sr7fle , Sr8fle , Tcons(15) , Twopi , Xlamda , Xlmbda
   DOUBLE PRECISION Dcore(1)
   INTEGER Givn(7) , Ibk , Ibuck , Icase , Icore(12) , Idump , Iepx , Iev(7) , Ifilk(7) , Ifillm(7) , Ifilm(7) , Ifilvc(7) ,        &
         & Ifkaa(7) , Iflelm(7) , Iflrva , Iflrvc , Iflvec(7) , Ifmaa(7) , Iip , Iiu , Ik(7) , Im(7) , Incrp , Incru , Iprob(2) ,   &
         & Iscr1 , Iscr2 , Iscr3 , Iscr4 , Iscr5 , Iscr6 , Iscr7 , Iscr8 , Itp1 , Itp2 , Itu , Jjp , Jju , Jprec , Keep , Ksys(51) ,&
         & Lamda , Lcore , Lntime , Mord , Mz , N , Ndmnus , Ndplus , Ne , Neig , Nev , Nevm , Nfound , Nfr , Nit , Noest , Nogo ,  &
         & Nord , Northo , Nout , Novect , Nprt , Nr , Nsym , Nummod , Nv , Option , Optn2 , Scr1 , Scr2 , Scr3 , Scr4 , Scr5 ,     &
         & Scr6 , Scr7 , Sturm , Sysbuf
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /blank / Iprob , Nummod , Icase , Xlamda
   COMMON /condas/ Pi , Twopi , Radeg , Degra , Fps
   COMMON /feercx/ Ifkaa , Ifmaa , Iflelm , Iflvec , Sr1fle , Sr2fle , Sr3fle , Sr4fle , Sr5fle , Sr6fle , Sr7fle , Sr8fle ,        &
                 & Dmpfle , Nord , Xlmbda , Neig , Mord , Ibk , Critf , Northo , Iflrva , Iflrvc , Iepx
   COMMON /givn  / Dum , N , Lfreq , Order , D1 , Hfreq , D2 , Nv , Nprt , D4 , Nfr
   COMMON /invpwx/ Ifilk , Ifilm , Ifillm , Ifilvc , Iscr1 , Iscr2 , Iscr3 , Iscr4 , Iscr5 , Iscr6 , Iscr7 , Iscr8 , Idump , Lmin , &
                 & Lmax , Noest , Ndplus , Ndmnus , Eps , Novect
   COMMON /ntime / Lntime , Tcons
   COMMON /packx / Itp1 , Itp2 , Iip , Jjp , Incrp
   COMMON /regean/ Im , Ik , Iev , Scr1 , Scr2 , Scr3 , Scr4 , Scr5 , Lcore , Rmax , Rmin , Mz , Nev , Epsi , Rminr , Ne , Nit ,    &
                 & Nevm , Scr6 , Scr7 , Nfound , Lamda , Ibuck , Nsym
   COMMON /reigkr/ Option , Optn2
   COMMON /sturmx/ Sturm , Shftpt , Keep , Ptshft , Nr , Shftzo
   COMMON /system/ Sysbuf , Nout , Nogo , Ksys , Jprec
   COMMON /unpakx/ Itu , Iiu , Jju , Incru
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zzzzzz/ Core
   INTEGER casecc , dm , eed , eigr(4) , error(3) , feerx , file , givi , i , i0 , icore6 , icore7 , icore8 , icr1 , icr2 , iflag , &
         & inv , ip1 , isil , ix(7) , kaa , l26 , lama , llcore , maa , method , mgiv , mi , mode , mr , nnv , norm , phia , pout , &
         & sdet , sinv , subnam(2) , udet , uinv , uset
   REAL core4 , core5 , epsii , xxx
   INTEGER korsz
!
!     READ   KAA,MAA,MR,DM,EED,USET,CASECC/LAMA,PHIA,MI,OEIGS/C,N,IPROB
!            /V,N,NUMMOD/C,N,ICASE/C,N,XLAMDA $
!
   !>>>>EQUIVALENCE (Givn(1),Core(1))
   !>>>>EQUIVALENCE (Tcons(4),Apc) , (Tcons(5),Apu) , (Tcons(8),Mb(1)) , (error(2),subnam(1)) , (Dcore(1),Core(1),Icore(1))
   DATA eigr , casecc/307 , 3 , 107 , 1 , 107/ , sdet , udet , inv , sinv , i0/4HSDET , 4HUDET , 4HINV  , 4HSINV , 0/ , uinv ,      &
      & givi , kaa , maa , mr/4HUINV , 4HGIV  , 101 , 102 , 103/ , dm , eed , uset , lama , phia/104 , 105 , 106 , 201 , 202/ , mi ,&
      & pout , icr1 , icr2 , mode/203 , 204 , 301 , 302 , 4HMODE/ , error , feerx , mgiv/4HEED  , 4HREIG , 4H     , 4HFEER , 4HMGIV/
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
   CALL open(*100,casecc,Core(Lcore+1),0)
   CALL skprec(casecc,Icase)
   CALL fread(casecc,Icore,166,1)
   CALL close(casecc,1)
   method = Icore(5)
   GOTO 200
 100  method = -1
 200  file = eed
   CALL preloc(*900,Core(Lcore+1),eed)
   CALL locate(*300,Core(Lcore+1),eigr(Ibuck),iflag)
   DO
      CALL read(*300,*300,eed,Core(1),18,0,iflag)
      IF ( method==Icore(1) .OR. method==-1 ) GOTO 400
   ENDDO
!
!     NO SET NUMBER FOUND
!
 300  CALL mesage(-32,method,error)
!
!     FOUND DATA CARD
!
 400  norm = Icore(10)
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
99001 FORMAT (A23,' 3131, INPUT STIFFNESS AND MASS MATRICES ARE NOT ','COMPATIBLE.')
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
   Option = Icore(2)
   Optn2 = Icore(3)
   IF ( Option/=givi .AND. Option/=udet .AND. Option/=uinv ) THEN
      IF ( Option/=feerx .AND. Option/=mgiv ) THEN
         IF ( Option/=sdet .AND. Option/=sinv ) THEN
            Option = udet
            IF ( Icore(2)==inv ) Option = uinv
            IF ( Im(4)==6 .AND. Ik(4)==6 ) THEN
               Option = sdet
               IF ( Icore(2)==inv ) Option = sinv
            ENDIF
         ELSEIF ( Im(4)/=6 .OR. Ik(4)/=6 ) THEN
            WRITE (Nout,99002) Uwm
99002       FORMAT (A25,' 2368, SYMMETRIC DECOMPOSITION IS SPECIFIED ON THE ','EIGR BULK DATA CARD, BUT',/5X,                       &
                   &'UNSYMMETRIC DECOMPOSITION WILL BE USED AS THIS IS THE ','PROPER TYPE OF DECOMPOSITION FOR THIS PROBLEM.')
            Option = udet
            IF ( Icore(2)==sinv ) Option = uinv
         ENDIF
      ENDIF
   ENDIF
   isil = Icore(12)
   i = 9
   epsii = Core(i)
   IF ( Ibuck/=3 ) THEN
!
!     CONVERT FREQUENCY TO LAMDA
!
      IF ( .NOT.((Icore(2)==givi .OR. Icore(2)==mgiv) .AND. Icore(7)>0) ) THEN
         IF ( Core(i0+4)<0.0 ) THEN
            WRITE (Nout,99003) Uwm
!
!     ERROR MESSAGES
!
99003       FORMAT (A25,' 2367, FREQUENCY F1 (FIELD 4) ON THE EIGR BULK DATA',' CARD IS NEGATIVE',/5X,                              &
                   &'IT IS ASSUMED TO BE ZERO FOR CALCULATION PURPOSES.',/)
            Core(i0+4) = 0.0
         ENDIF
      ENDIF
      Core(i0+4) = Fps*Core(i0+4)*Core(i0+4)
      IF ( Icore(2)/=feerx ) Core(i0+5) = Fps*Core(i0+5)*Core(i0+5)
   ENDIF
   core4 = Core(i0+4)
   core5 = Core(i0+5)
   icore6 = Icore(6)
   icore7 = Icore(7)
   icore8 = Icore(8)
   IF ( Icore(2)/=givi .AND. Icore(2)/=mgiv ) THEN
      IF ( Icore(2)/=feerx ) THEN
         IF ( Icore(7)==0 ) Icore(7) = 3*Icore(6)
         icore7 = Icore(7)
      ENDIF
!
!     FEER, INVERSE POWER AND DETERMINANT METHODS
!
!     CHECK IF IT IS A NORMAL MODES PROBLEM OR A BUCKLING PROBLEM
!
      IF ( Ibuck==3 ) GOTO 500
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
99004       FORMAT (A29,' 3143, THE EIGENVALUES AND EIGENVECTORS FOUND IN ','THIS ANALYSIS WILL BE APPENDED',/5X,'TO THE',I8,       &
                   &' EIGENVALUES AND EIGENVECTORS COMPUTED EARLIER.')
!
!     RETRIEVE EIGENVALUES AND EIGENVECTORS PREVIOUSLY CHECKPOINTED.
!
!     COPY OLD EIGENVALUES FROM LAMA FILE TO ICR1 FILE.
!
!     COPY OLD EIGENVECTORS FROM PHIA FILE TO ICR2 FILE.
!
            CALL read7(Nr,lama,phia,icr1,icr2)
            GOTO 500
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
      Givn(1) = kaa
      Givn(i0+2) = maa
      Givn(i0+3) = phia
      DO i = 1 , 4
         Givn(i+3) = eigr(i)
      ENDDO
      CALL givens
      nnv = Givn(1)
      Nummod = N
      GOTO 700
   ENDIF
 500  IF ( Option==feerx ) THEN
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
         GOTO 600
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
!
!     SORT EIGENVECTORS AND VALUES
!
 600  IF ( Nummod==0 ) THEN
      Nummod = -1
      CALL read5(pout)
      GOTO 800
   ELSE
      CALL read3(Nummod,Ifilk(2),Lamda,Iev,phia,lama)
   ENDIF
 700  IF ( method/=2 .AND. Nummod/=1 ) THEN
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
 800  IF ( Nogo==14 ) WRITE (Nout,99005)
99005 FORMAT ('0*** THIS NASTRAN JOB WILL BE TERMINATED')
   RETURN
!
 900  ip1 = -1
   DO
      CALL mesage(ip1,file,subnam)
   ENDDO
!
END SUBROUTINE reig