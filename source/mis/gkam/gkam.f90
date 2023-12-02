!*==gkam.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gkam
   USE c_blank
   USE c_condas
   USE c_packx
   USE c_system
   USE c_unpakx
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: b2dd , bhh , casecc , k2dd , khh , lama , m2dd , mhh , mi , phia , phidh , phidh1 , scr1 , scr2 , scr3 , sdt , &
                   & usetd
   REAL , DIMENSION(11) , SAVE :: block
   REAL :: f , file
   INTEGER :: i , i149 , icrq , iflag , ip1 , ip2 , isw , lc1 , lhset , modes , nosdt , nvect , nz
   INTEGER , DIMENSION(11) , SAVE :: iblock
   INTEGER , DIMENSION(2) :: icore
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL close , fread , gkam1a , gkam1b , gopen , korsz , mesage , open , pack , rdtrl , read , skprec , ssg2c , unpack ,       &
          & write , wrttrl
!
! End of declarations rewritten by SPAG
!
!
!     ROUTINE TO ASSEMBLE MODAL MATRICES
!
!     INPUTS = 9
!
!     USETD,PHIA,MI,LAMA,SDT,M2DD,B2DD,K2DD,CASECC
!
!     OUTPUTS = 4
!
!     MHH,BHH,KHH,PHIDH
!
!     SCRATCHES = 4
!
!
   !>>>>EQUIVALENCE (Core(1),Icore(1)) , (iblock(1),block(1))
!
   DATA name/4HGKAM , 4H    /
   DATA iblock(1) , iblock(7) , block(2) , block(8)/1 , 1 , 1.0 , 1.0/
   DATA usetd , phia , mi , lama , sdt , m2dd , b2dd , k2dd/101 , 102 , 103 , 104 , 105 , 106 , 107 , 108/
   DATA mhh , bhh , khh , phidh/201 , 202 , 203 , 204/
   DATA scr1 , scr2 , scr3 , phidh1 , casecc/301 , 302 , 303 , 304 , 109/
!
!
!     PICK UP AND STORE SELECTED MODES, SAVING EIGENVECTORS
!
   lc1 = korsz(core)
   nz = lc1 - sysbuf
   icrq = 2*sysbuf - nz
   IF ( icrq>0 ) THEN
      ip1 = -8
      file = icrq
      CALL mesage(ip1,ip2,name)
      GOTO 200
   ELSE
!
!     FIND SELECTED SDT INTO CASECC
!
      CALL gopen(casecc,core(nz+1),0)
      CALL fread(casecc,icore,166,1)
      CALL close(casecc,1)
      i149 = 149
      nosdt = icore(i149)
!
!     OPEN  LAMA, PHIA, AND PHI0H
!
      CALL gopen(lama,core(nz+1),0)
      CALL skprec(lama,1)
      nz = nz - sysbuf
      CALL gopen(phia,core(nz+1),0)
      icore(1) = phia
      CALL rdtrl(icore)
      nvect = icore(2)
      nz = nz - sysbuf
      IF ( noue<0 ) phidh1 = phidh
      CALL gopen(phidh1,core(nz+1),1)
      mcb(1) = phia
      CALL rdtrl(mcb)
      mcb(1) = phidh1
      it1 = mcb(5)
      it2 = it1
      it3 = it1
      incr = 1
      incr1 = 1
      ii = 1
      ii1 = 1
      jj = mcb(3)
      jj1 = jj
      mcb(2) = 0
      mcb(6) = 0
      mcb(7) = 0
      isw = 1
      modes = 1
      SPAG_Loop_1_1: DO i = 1 , nvect
         CALL read(*200,*100,lama,core(nz-6),7,0,iflag)
!
!     PICK UP FREQUENCY
!
         f = core(nz-2)
         IF ( nlmode==0 ) THEN
!
!     FREQUENCY RANGE SPECIFICATION
!
            IF ( f>hfreq ) EXIT SPAG_Loop_1_1
            IF ( f<lfreq ) THEN
               CALL skprec(phia,1)
               isw = isw + 1
               CYCLE
            ENDIF
         ENDIF
!
!     ACCEPT LAMA
!
         core(modes) = f*twophi
         modes = modes + 1
         CALL unpack(*300,phia,core(modes))
         CALL pack(core(modes),phidh1,mcb)
         IF ( nlmode/=0 ) THEN
            IF ( modes>nlmode ) EXIT SPAG_Loop_1_1
         ENDIF
      ENDDO SPAG_Loop_1_1
   ENDIF
!
!     DONE
!
 100  CALL close(lama,1)
   CALL close(phia,1)
   CALL close(phidh1,1)
   CALL wrttrl(mcb)
!
!     BUILD PHIDH
!
   lhset = modes - 1
   nmode = isw
   IF ( lhset<=0 ) THEN
!
!     NO MODES SELECTED
!
      ip1 = -47
      CALL mesage(ip1,ip2,name)
   ELSE
      IF ( noue>=0 ) CALL gkam1b(usetd,scr1,scr2,phidh,phidh1,modes,core,lhset,noue,scr3)
!
!     FORM H MATRICES
!
      modes = modes - 1
!
!     SAVE MODES ON SCRATCH3 IN CASE DMI WIPES THEM OUT
!
      nz = lc1 - sysbuf
      CALL open(*400,scr3,core(nz+1),1)
      CALL write(scr3,core(1),modes,1)
      CALL close(scr3,1)
      noncup = 1
!
!     FORM  MHH
!
      CALL gkam1a(mi,phidh,sdt,scr1,scr2,1,mhh,nom2dd,core(1),modes,nosdt,lhset,m2dd,isw,scr3)
      IF ( nom2dd>=0 ) CALL ssg2c(scr1,scr2,mhh,1,iblock(1))
!
!     FORM  BHH
!
      IF ( nosdt/=0 .OR. nob2dd>=0 ) THEN
         CALL gkam1a(mi,phidh,sdt,scr1,scr2,2,bhh,nob2dd,core(1),modes,nosdt,lhset,b2dd,isw,scr3)
         IF ( nob2dd>=0 ) CALL ssg2c(scr1,scr2,bhh,1,iblock(1))
      ENDIF
!
!     FORM  KHH
!
      CALL gkam1a(mi,phidh,sdt,scr1,scr2,3,khh,nok2dd,core(1),modes,nosdt,lhset,k2dd,isw,scr3)
      IF ( nok2dd>=0 ) CALL ssg2c(scr1,scr2,khh,1,iblock(1))
      IF ( nob2dd<0 .AND. nom2dd<0 .AND. nok2dd<0 ) noncup = -1
      RETURN
   ENDIF
 200  DO
      ip2 = lama
      ip1 = -3
      CALL mesage(ip1,ip2,name)
   ENDDO
 300  WRITE (nout,99001) sfm
99001 FORMAT (A25,' 2204, UNPACK FOUND NULL COLUMN IN PHIA FILE IN ','GKAM MODULE.')
   ip1 = -37
   CALL mesage(ip1,ip2,name)
   GOTO 200
 400  ip2 = scr3
!
!     ERROR MESAGES
!
   ip1 = -1
   CALL mesage(ip1,ip2,name)
   GOTO 200
END SUBROUTINE gkam
