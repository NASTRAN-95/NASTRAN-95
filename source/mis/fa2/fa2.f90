!*==fa2.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fa2
   IMPLICIT NONE
   USE C_BLANK
   USE C_SYSTEM
   USE C_UNPAKX
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   REAL :: bref , f , g , iml , kfreq , oneok , phib , rel , rho , value , value1 , valuei , valuer , vout , xmach
   INTEGER , DIMENSION(146) , SAVE :: buf
   INTEGER , SAVE :: caseyy , clama , clamal , fsave , iblnk , no , ovg , phih , phihl , yes , yesb
   INTEGER :: file , floop , fmeth , i , ibuf1 , ibuf2 , ibuf3 , ibuf4 , iflag , imr , imr1 , index , ip1 , ir , irho , iri , irr , &
            & itlft , j , k , ki , kr , l , l1 , li , lr , m , meth , ncopy , nloop , now , nrho , nv , nvalu1 , nvalue , nz
   INTEGER , DIMENSION(2) :: ialph
   INTEGER , DIMENSION(22) , SAVE :: iary
   REAL , DIMENSION(6) :: lbuf
   INTEGER , DIMENSION(7) :: mcb , mcbphl
   INTEGER , DIMENSION(7) , SAVE :: mcbcc , mcbcl , mcbovg
   INTEGER , DIMENSION(3) , SAVE :: me
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL , SAVE :: twophi
   REAL , DIMENSION(1) :: z
   EXTERNAL bckrec , close , cyct2b , eject , fread , fwdrec , gopen , int2a8 , klock , korsz , mesage , open , page1 , rdtrl ,     &
          & re2al , read , skpfil , skprec , tmtogo , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS IS THE DMAP MODULE FA2
!
!     DMAP CALLING SEQUENCE
!
!     FA2  PHIH,CLAMA,FSAVE/PHIHL,CLAMAL,CASEYY,OVG/V,N,TSTART/C,Y,VREF/
!    1     C,Y,PRINT=YES $
!
!     ALL OUTPUTS ARE APPEND
!
!     THE PURPOSE OF THIS MODULE IS TO COPY PARTS OF PHIH, CLAMA, AND
!    1    FSAVE ONTO PHIHL, CLAMAL, CASEYY, AND OVG RESPECTIVELY
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
   DATA phih , clama , fsave , phihl , clamal , caseyy , ovg/101 , 102 , 103 , 201 , 202 , 203 , 204/
   DATA name , no , mcbcl , mcbcc , mcbovg , iblnk/4HFA2  , 1H  , 2HNO , 21*0 , 4H    /
   DATA buf/146*1H /
   DATA iary/4H POI , 4HNT = , 1H  , 1H  , 4H MAC , 4HH =  , 1H  , 1H  , 4H KFR , 4HEQ=  , 1H  , 1H  , 4H RHO , 4H =   , 1H  , 1H  ,&
      & 6*1H /
   DATA twophi/6.28318531/
   DATA me/1HK , 2HKE , 2HPK/
   DATA yes , yesb/3HYES , 4HYESB/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     INITIALIZE
!
         nz = korsz(z)
         ibuf1 = nz - Sysbuf + 1
         ibuf2 = ibuf1 - Sysbuf
         ibuf3 = ibuf2 - Sysbuf
         ibuf4 = ibuf3 - Sysbuf
         nz = ibuf4 - 1
         Itc = 3
         Incr = 1
         mcbcl(1) = clamal
         mcbcc(1) = caseyy
         mcbovg(1) = ovg
         IF ( Vref==0.0 ) Vref = 1.0
!
!     FIND PROPER METHOD
!
         file = fsave
         CALL open(*100,fsave,Iz(ibuf1),0)
         CALL read(*120,*140,fsave,Iz(1),8,1,iflag)
         j = 3
         fmeth = Iz(j)
         meth = me(fmeth)
         oneok = 1.E+25
         mcb(1) = fsave
         CALL rdtrl(mcb)
         floop = mcb(2)
         nloop = mcb(3)
         nvalue = mcb(7)
         j = 6
         bref = z(j)
         phib = twophi*bref
         IF ( fmeth==2 ) THEN
         ELSEIF ( fmeth/=3 ) THEN
!
!     K  METHOD
!
!
!     PICK UP CONSTANTS
!
            nvalue = 8
            nvalue = Iz(nvalue)
!
!     COPY ONTO PHIHL
!
            IF ( floop==1 ) THEN
!
!     FIRST TIME
!
               CALL gopen(phihl,Iz(ibuf2),1)
               CALL close(phihl,1)
               mcbphl(1) = phih
               CALL rdtrl(mcbphl)
               mcbphl(2) = 0
               mcbphl(6) = 0
               mcbphl(7) = 0
               mcbphl(1) = phihl
               CALL wrttrl(mcbphl)
               CALL gopen(clamal,Iz(ibuf2),1)
               CALL gopen(clama,Iz(ibuf3),0)
               CALL fread(clama,buf,146,1)
               CALL close(clama,1)
               CALL write(clamal,buf,146,1)
               CALL write(clamal,0,0,1)
               CALL close(clamal,1)
               CALL gopen(caseyy,Iz(ibuf2),1)
               CALL close(caseyy,1)
               CALL gopen(ovg,Iz(ibuf2),1)
               CALL close(ovg,1)
            ENDIF
!
!     COPY NVALUE VECTORS TO PHIHL
!
            mcb(1) = phih
            CALL rdtrl(mcb)
            ncopy = min0(nvalue,mcb(2))
            CALL gopen(phih,Iz(ibuf2),0)
            CALL gopen(phihl,Iz(ibuf3),0)
            CALL skpfil(phihl,1)
            CALL skpfil(phihl,-1)
            CALL close(phihl,2)
            CALL gopen(phihl,Iz(ibuf3),3)
            mcbphl(1) = phihl
            CALL rdtrl(mcbphl)
            mcbphl(7) = (2*mcbphl(7)*mcbphl(2)*mcbphl(3))/10000
            CALL cyct2b(phih,phihl,ncopy,Iz,mcbphl)
            CALL close(phih,1)
            CALL close(phihl,1)
            CALL wrttrl(mcbphl)
!
!     PICK UP M,K,RHO FOR THIS LOOP
!
            CALL fread(fsave,Iz,-3*(floop-1),0)
            CALL fread(fsave,z,3,1)
            j = 0
            xmach = z(1)
            kfreq = z(j+2)
            rho = z(j+3)
            CALL fread(fsave,z,1,1)
!
!     PUT CASEYY INTO CORE
!
            CALL read(*120,*20,fsave,Iz,nz,0,iflag)
            CALL mesage(-8,0,name)
            GOTO 20
         ENDIF
!
!     K-E METHOD
!
!
!     P - K METHOD
!
!
!     READY OVG
!
         CALL gopen(ovg,Iz(ibuf2),1)
         mcbovg(2) = 1
         CALL wrttrl(mcbovg)
!
!     PUT RECORD 2 OF FSAVE INTO CORE
!
         CALL read(*120,*60,fsave,Iz(1),nz,1,iflag)
         CALL mesage(-8,0,name)
         GOTO 60
 20      CALL close(fsave,1)
         k = 39
         DO i = 51 , 146
            buf(i) = Iz(k)
            k = k + 1
         ENDDO
!
!     READY CLAMA
!
         CALL gopen(clama,Iz(ibuf1),0)
         CALL fwdrec(*120,clama)
!
!     READY CLAMAL
!
         CALL gopen(clamal,Iz(ibuf2),0)
         CALL skpfil(clamal,1)
         CALL skpfil(clamal,-1)
         CALL bckrec(clamal)
         CALL read(*120,*40,clamal,Iz(iflag+1),nz,0,i)
         CALL mesage(-8,0,name)
 40      CALL bckrec(clamal)
         CALL close(clamal,2)
         CALL gopen(clamal,Iz(ibuf2),3)
         CALL write(clamal,Iz(iflag+1),i,0)
         CALL rdtrl(mcbcl)
!
!     READY CASEYY
!
         CALL gopen(caseyy,Iz(ibuf3),0)
         CALL skpfil(caseyy,1)
         CALL skpfil(caseyy,-1)
         CALL close(caseyy,2)
         CALL gopen(caseyy,Iz(ibuf3),3)
         CALL rdtrl(mcbcc)
!
!     READY OVG
!
         CALL gopen(ovg,Iz(ibuf4),0)
         CALL skpfil(ovg,1)
         CALL skpfil(ovg,-1)
         CALL close(ovg,2)
         CALL gopen(ovg,Iz(ibuf4),3)
         CALL rdtrl(mcbovg)
         mcbovg(2) = mcbovg(2) + 1
         mcbcc(4) = iflag
         CALL wrttrl(mcbovg)
         mcbcc(2) = mcbcc(2) + ncopy
         CALL wrttrl(mcbcc)
         mcbcl(2) = mcbcl(2) + ncopy
         CALL wrttrl(mcbcl)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 60      CALL skprec(fsave,1)
         CALL fread(fsave,0,-51,0)
         CALL fread(fsave,buf,96,1)
         imr = 1
         floop = 1
!
!     COUNT RHO S
!
         nrho = 1
         IF ( fmeth/=3 ) THEN
            irho = 1
            rho = z(imr+2)
            imr1 = imr + 3
            SPAG_Loop_1_1: DO WHILE ( imr1<=iflag )
               IF ( rho==z(imr1+2) ) EXIT SPAG_Loop_1_1
               nrho = nrho + 1
               imr1 = imr1 + 3
            ENDDO SPAG_Loop_1_1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         nv = 1
!
!     DETERMINE THE NUMBER OF M-RHO PAIRS FOR THIS GO
!
         xmach = z(imr)
         rho = z(imr+2)
         ncopy = 1
         imr1 = imr + 3*nrho
         SPAG_Loop_1_2: DO WHILE ( imr1<=iflag )
            IF ( xmach/=z(imr1) .OR. rho/=z(imr1+2) ) EXIT SPAG_Loop_1_2
            ncopy = ncopy + 1
            imr1 = imr1 + 3*nrho
         ENDDO SPAG_Loop_1_2
         spag_nextblock_1 = 3
      CASE (3)
!
         IF ( Print(1)/=no ) THEN
!     SET UP PAGE FORMATS
!
            CALL page1
            Nlines = Nlines + 7
            IF ( Print(1)==yesb ) WRITE (Nout,99002) floop , xmach , rho , meth
            IF ( Print(1)==yes ) WRITE (Nout,99003) floop , xmach , rho , meth
         ENDIF
!
!     SET UP FOR OVG
!
         buf(1) = 60
         buf(2) = 2002
         buf(4) = 1
         buf(5) = 10*floop
         buf(9) = 1
         buf(10) = 4
         CALL write(ovg,buf,146,1)
         IF ( fmeth/=1 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO i = 115 , 146
            buf(i) = iblnk
         ENDDO
         CALL int2a8(*80,floop,ialph)
 80      iary(3) = ialph(1)
         iary(4) = ialph(2)
         CALL re2al(xmach,ialph)
         iary(7) = ialph(1)
         iary(8) = ialph(2)
         CALL re2al(kfreq,ialph)
         iary(11) = ialph(1)
         iary(12) = ialph(2)
         CALL re2al(rho,ialph)
         iary(15) = ialph(1)
         iary(16) = ialph(2)
         k = 115
         DO i = 1 , 16
            buf(k) = iary(i)
            k = k + 1
         ENDDO
         k = 103
         DO i = 115 , 146
            Iz(k) = buf(i)
            k = k + 1
         ENDDO
         spag_nextblock_1 = 4
      CASE (4)
         DO i = 1 , ncopy
            IF ( fmeth==1 ) THEN
!
!     K METHOD
!
               CALL fread(clama,lbuf,6,0)
               CALL write(clamal,lbuf,6,0)
               rel = lbuf(3)
               iml = lbuf(4)
               vout = abs(iml)/Vref
               g = 0.0
               IF ( iml/=0.0 ) g = 2.0*rel/iml
               f = kfreq*iml/(phib)
!
!     PUT OUT CASEYY
!
               CALL write(caseyy,Iz,iflag,1)
            ELSEIF ( fmeth==3 ) THEN
!
!     PK METHOD
!
               CALL fread(fsave,lbuf,-(nv-1)*5,0)
               CALL fread(fsave,lbuf,5,1)
               rel = lbuf(1)
               iml = lbuf(2)
               kfreq = lbuf(3)
               f = lbuf(4)
               g = lbuf(5)
               vout = abs(z(imr+3*i-2))/Vref
            ELSE
!
!     KE METHOD
!
               IF ( i==1 .AND. nv==1 ) THEN
                  ir = iflag + 1
                  j = nvalue*2
                  DO m = 1 , ncopy
!
!     READ A RECORD OF COMPLEX EIGENVALUES INTO CORE
!
                     CALL fread(fsave,Iz(ir),j,1)
                     CALL skprec(fsave,nrho-1)
!
!     REARRANGE THE COMPLEX EIGENVALUES IN THE RECORD IN ASCENDING
!     ORDER OF THE ABSOLUTE VALUES OF THE IMAGINARY PARTS
!
                     nvalu1 = nvalue - 1
                     DO l = 1 , nvalu1
                        lr = ir + 2*(l-1)
                        li = lr + 1
                        valuer = z(lr)
                        valuei = z(li)
                        value = abs(valuei)
                        index = l
                        l1 = l + 1
                        DO k = l1 , nvalue
                           kr = ir + 2*(k-1)
                           ki = kr + 1
                           value1 = abs(z(ki))
                           IF ( value1<value ) THEN
                              valuer = z(kr)
                              valuei = z(ki)
                              value = value1
                              index = k
                           ENDIF
                        ENDDO
                        IF ( index/=l ) THEN
                           irr = ir + 2*(index-1)
                           iri = irr + 1
                           z(irr) = z(lr)
                           z(iri) = z(li)
                           z(lr) = valuer
                           z(li) = valuei
                        ENDIF
                     ENDDO
                     ir = ir + j
                  ENDDO
               ENDIF
!
!     SELECT EACH FOR OUTPUT
!
               j = iflag + 1 + (i-1)*nvalue*2 + (nv-1)*2
               rel = z(j)
               iml = z(j+1)
               vout = abs(iml)/Vref
               g = 0.0
               IF ( iml/=0.0 ) g = 2.*rel/iml
               kfreq = z(imr+3*i-2)
               f = kfreq*iml/phib
            ENDIF
            IF ( Print(1)/=no ) THEN
!
!     PRINT OUTPUT
!
               k = eject(1)
               IF ( k/=0 ) THEN
                  IF ( Print(1)==yesb ) WRITE (Nout,99002) floop , xmach , rho , meth
                  IF ( Print(1)==yes ) WRITE (Nout,99003) floop , xmach , rho , meth
                  Nlines = Nlines + 7
               ENDIF
               IF ( kfreq/=0.0 ) oneok = 1.0/kfreq
               WRITE (Nout,99001) kfreq , oneok , vout , g , f , rel , iml
99001          FORMAT (1H ,5X,F8.4,5X,6(1X,1P,E14.7,3X))
            ENDIF
!
!     PUT OUT OVG PARTS
!
            lbuf(1) = vout
            lbuf(2) = 0.0
            lbuf(3) = g
            lbuf(4) = f
            CALL write(ovg,lbuf,4,0)
         ENDDO
         floop = floop + 1
         CALL write(ovg,0,0,1)
         IF ( fmeth==1 ) THEN
!
!     FINISH UP
!
            CALL write(clamal,0,0,1)
            CALL close(ovg,1)
            CALL close(clamal,1)
            CALL close(clama,1)
            CALL close(caseyy,1)
!
!     CHECK TIMES
!
            CALL klock(now)
            CALL tmtogo(itlft)
            IF ( now-Tstart>=itlft .AND. floop/=nloop ) THEN
!
!     INSUFFICIENT TIME
!
               CALL mesage(45,nloop-floop,name)
               Tstart = -1
               RETURN
            ELSE
               RETURN
            ENDIF
         ELSEIF ( fmeth==3 ) THEN
!
!     P-K AT POINT END
!
            nv = nv + 1
            IF ( nv<=nvalue ) THEN
               CALL skprec(fsave,-ncopy)
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
!
!     ALL MODES DONE--CONSIDER MORE M-RHO VALUES
!
            ELSEIF ( imr1<=iflag ) THEN
               imr = imr1
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
!
!     FINISH UP FOR KE METHOD
!
            nv = nv + 1
            IF ( nv<=nvalue ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     ALL MODES DONE
!
            IF ( irho<nrho ) THEN
!
!     DO ANOTHER RHO
!
               irho = irho + 1
               imr = imr + 3
               rho = z(imr+2)
               CALL skprec(fsave,ncopy*(nrho-1))
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( imr1<=iflag ) THEN
               imr = imr1
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!     DONE
!
         CALL close(ovg,1)
         CALL close(fsave,1)
         RETURN
!
!     ERROR MESSAGES
!
 100     ip1 = -1
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 120     ip1 = -2
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 140     ip1 = -3
         spag_nextblock_1 = 5
      CASE (5)
         CALL mesage(ip1,file,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99002 FORMAT (1H0,55X,16HFLUTTER  SUMMARY,//7X,9HPOINT =  ,I3,5X,14HSIGMA VALUE = ,F8.3,4X,16HDENSITY RATIO = ,1P,E11.4,5X,         &
             &9HMETHOD = ,A4,///7X,5HKFREQ,12X,8H1./KFREQ,9X,8HVELOCITY,12X,7HDAMPING,9X,9HFREQUENCY,12X,20HCOMPLEX   EIGENVALUE)
99003 FORMAT (1H0,55X,16HFLUTTER  SUMMARY,//7X,9HPOINT =  ,I3,5X,14HMACH NUMBER = ,F7.4,5X,16HDENSITY RATIO = ,1P,E11.4,5X,         &
             &9HMETHOD = ,A4,///7X,5HKFREQ,12X,8H1./KFREQ,9X,8HVELOCITY,12X,7HDAMPING,9X,9HFREQUENCY,12X,20HCOMPLEX   EIGENVALUE)
END SUBROUTINE fa2
