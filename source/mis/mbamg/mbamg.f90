!*==mbamg.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mbamg(Input,Ajjl,Skj)
   USE c_amgmn
   USE c_mboxa
   USE c_mboxc
   USE c_packx
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Input
   INTEGER :: Ajjl
   INTEGER :: Skj
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: buf1 , i , icore , icorr , icrq , ireg , is , l , na , nc1 , nc21 , nc2n , ncap , ncaph , ncn , nd1 , ndn , ndss ,    &
            & nhxect , nkte , nkte1 , nkte2 , nparea , nphit , nq , nq1 , nq2 , nw1 , nwn , nxk , nxk1 , nxk2 , nxwte , nyk , nyk1 ,&
            & nyk2 , nywte
   REAL :: cmax , rm
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , SAVE :: nhcapf , nhcont , nhcore , scr2
   EXTERNAL bug , close , dmpfil , fread , gopen , korsz , mbcap , mbdpdh , mbgeod , mbmode , mbplot , mbreg , mesage , pack
!
! End of declarations rewritten by SPAG
!
!
!     DRIVER FOR MACH BOX THEORY
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
   DATA name/4HMBAM , 4HG   /
   DATA nhcore , nhcapf , nhcont/4HCORE , 4HCAPF , 4HCONT/
   DATA scr2/302/
!
!     SCR2 CONTAINS THE INTERPOLATED POINTS
!
!     2 * KCT FOR NPTS0 POINTS
!     2 * KC1T FOR NPTS1 POINTS
!     2 * KC2T FOR NPTS2 POINTS
!
!
!     OPEN CORE POINTERS FIXED DIMENSIONS
!
   nw1 = 1
   nwn = 51
   nc21 = 101
   nc2n = 151
   nc1 = 201
   ncn = 251
   nd1 = 301
   ndn = 351
   nxk = 401
   nyk = 601
   nxk1 = 801
   nyk1 = 926
   nxk2 = 1051
   nyk2 = 1176
   nxwte = 1301
   nywte = 1351
   nkte = 1401
   nkte1 = 1451
   nkte2 = 1501
   nparea = 1551
   icorr = 9051
!
!     INITITALIZE  PUT HEADER DATA IN MBOXC
!
   icore = korsz(iz) - 4*sysbuf
   buf1 = icore - sysbuf
   CALL fread(Input,njj,9,0)
   asym = .FALSE.
   IF ( nd==-1 ) asym = .TRUE.
   mach = fmach
   beta = sqrt((mach*mach)-1.0)
   CALL fread(Input,z,24,0)
!
!     MOVE X AND Y TO MBOXA
!
   l = 0
   DO i = 1 , 23 , 2
      l = l + 1
      x(l) = z(i)
      y(l) = z(i+1)
   ENDDO
   CALL mbgeod
   ek = (2.0*cr/refc)*rfk
   cmax = amax1(x(4),x(5),x(6))
   boxl = cmax/(float(nbox)+0.50)
   boxw = boxl/beta
   nsb = y(3)/boxw + 0.5
   nsb = min0(nsb,50)
   boxw = y(3)/(float(nsb)-0.50)
   boxl = boxw*beta
   ncb = cmax/boxl + 0.999
!
!     CALL MBREG TO GENERATE BOXES
!
   icrq = icorr - buf1
   IF ( icorr>buf1 ) THEN
      CALL mesage(-8,icrq,name)
   ELSE
      SPAG_Loop_1_1: DO
         CALL mbreg(ireg,z(nw1),z(nwn),z(nc21),z(nc2n),z(nc1),z(ncn),z(nd1),z(ndn),z(nxk),z(nyk),z(nxk1),z(nyk1),z(nxk2),z(nyk2),   &
                  & z(nxwte),z(nywte),z(nkte),z(nkte1),z(nkte2),z(nparea))
         IF ( ireg/=2 ) THEN
            CALL mbplot(z(nw1),z(nd1),z(nwn),z(nc21),z(nc2n),z(nc1),z(ncn),z(ndn))
!
!     CALL MBMODE TO GENERATE MODE LIKE DATA
!
            CALL gopen(scr2,z(buf1),1)
            CALL mbmode(Input,scr2,icorr,buf1,z,npts0,kct,z(nxk),z(nyk),is,cr)
            IF ( is==2 ) THEN
               CALL spag_block_2
               RETURN
            ENDIF
            IF ( cntrl1 ) CALL mbmode(Input,scr2,icorr,buf1,z,npts1,kc1t,z(nxk1),z(nyk1),is,cr)
            IF ( is==2 ) THEN
               CALL spag_block_2
               RETURN
            ENDIF
            IF ( cntrl2 ) CALL mbmode(Input,scr2,icorr,buf1,z,npts2,kc2t,z(nxk2),z(nyk2),is,cr)
            IF ( is==2 ) THEN
               CALL spag_block_2
               RETURN
            ENDIF
            CALL close(scr2,1)
            ekbar = (ek*boxl*mach*mach)/(beta*beta)
            ekm = ekbar/mach
            CALL fread(Input,0,0,1)
            CALL bug(nhcore,80,z,nyk1-1)
            CALL bug(nhcore,80,z(nyk1),nparea-nyk1)
            CALL dmpfil(scr2,z(icorr),buf1-icorr)
!
!     MORE DIMENSIONS
!
            IF ( mod(icorr,2)==0 ) icorr = icorr + 1
            ncap = icorr
            ncaph = ncb*(ncb+1)/2
!
!     COMPLEX PHIS
!
            icorr = ncap + ncaph*2
            icrq = icorr - buf1
            IF ( icorr>buf1 ) THEN
               CALL mesage(-8,icrq,name)
            ELSE
               CALL mbcap(ncaph,z(ncap))
               icorr = ncap + ncaph*2
               CALL bug(nhcapf,80,z(ncap),ncaph*2)
!
!     PUT OUT SKJ
!
               iti = 1
               it0 = 3
               ii = isk
               nsk = nsk + 1
               nn = nsk
               rm = 1.0
               DO i = 1 , njj
                  CALL pack(rm,Skj,tskj)
                  ii = ii + 1
                  IF ( i/=njj ) nn = nn + 1
               ENDDO
               isk = ii
               nsk = nn
!
!     SET UP FOR COLUMN OF AJJL
!
               iti = 3
               it0 = 3
               ii = nrow + 1
               nn = nrow + njj
!
!     GET AJJL MATRIX TERMS
!     MORE DIMENSIONS
!
               nphit = icorr
               ndss = nphit + (3*nsbd)*2
               nq = ndss + (ncb*nsbd)*2
               nq1 = nq + kct*2
               nq2 = nq1 + kc1t*2
               na = nq2 + kc2t*2
               icorr = na + njj*2
               CALL bug(nhxect,100,x,54)
               CALL bug(nhcont,100,njj,30)
               icrq = icorr - buf1
               IF ( icorr>buf1 ) THEN
                  CALL mesage(-8,icrq,name)
               ELSE
                  CALL mbdpdh(Ajjl,z(nxk),z(nyk),z(nxk1),z(nyk1),z(nxk2),z(nyk2),z(nxwte),z(nywte),z(nparea),z(ncap),z(nphit),      &
                            & z(ndss),z(nq),z(nq1),z(nq2),z(ndn),z(nd1),z(nw1),z(nwn),z(nkte),z(nkte1),z(nkte2),z(nc1),ncb,nsbd,    &
                            & scr2,z(buf1),z(na))
                  nrow = nrow + njj
               ENDIF
            ENDIF
            EXIT SPAG_Loop_1_1
         ELSEIF ( nbox<2 ) THEN
            WRITE (nout,99001) ufm
99001       FORMAT (A23,' 2425, MACH BOX GENERATION OF BOXES FAILED')
            CALL mesage(-37,0,name)
            CALL mesage(-8,icrq,name)
            EXIT SPAG_Loop_1_1
         ELSE
            nbox = nbox - 1
         ENDIF
      ENDDO SPAG_Loop_1_1
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
!
!     ERROR MESSAGES
!
      WRITE (Nout,99001) Ufm
99001 FORMAT (A23,' 2424, MACH BOX CONTROL POINTS IMPROPER SINGULAR ','MATRIX RESULTED')
      CALL mesage(-37,0,Name)
      CALL mesage(-8,Icrq,Name)
      CALL spag_block_1
   END SUBROUTINE spag_block_2
END SUBROUTINE mbamg
