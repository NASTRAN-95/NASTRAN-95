!*==sma1.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE sma1
   IMPLICIT NONE
   USE c_blank
   USE c_gpta1
   USE c_hmatdd
   USE c_sma1bk
   USE c_sma1cl
   USE c_sma1dp
   USE c_sma1et
   USE c_sma1ht
   USE c_sma1io
   USE c_system
   USE c_zzzzzz
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ifile , imat1 , imat11 , iparm , izmax , matcr , nmat1 , nmat2 , nmat3 , nmat4
   INTEGER , DIMENSION(7) :: ibuf
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) , SAVE :: nmsma1
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!*****
! THIS ROUTINE IS A DRIVER AND INITIALIZATION PROGRAM FOR MODULE
! 2.4.1 OF THE NASTRAN SYSTEM.  IT GENERATES THE STIFFNESS MATRIX, KGG,
! THE STRUCTURAL DAMPING MATRIX, K4GG, AND THE GRID POINT SINGULARITY
! TABLE, GPST.
!*****
!
!
!
!
!
! SMA1 I/O PARAMETERS
!
!
! SMA1 VARIABLE CORE
!
!
! SMA1 VARIABLE CORE BOOKKEEPING PARAMETERS
!
!
! SMA1 PROGRAM CONTROL PARAMETERS
!
!
! ELEMENT DATA
!
!
! ECPT COMMON BLOCK
!
!
! SCRATCH COMMON BLOCK USED BY ELEMENT ROUTINES.
!
!
! COMMON INTERFACE FOR HMAT -HEAT- MATERIAL ROUTINE.
!
!
!
   !>>>>EQUIVALENCE (Z(1),Iz(1),Dz)
!
   DATA nmsma1(1)/4HSMA1/ , nmsma1(2)/4H    /
!*****
!  SET THE LOGICAL HEAT FLAG IF THIS IS A -HEAT- FORMULATION
!*****
   CALL delset
   linear = .TRUE.
   option(1) = -1
   heat = .FALSE.
   IF ( itherm/=0 ) heat = .TRUE.
!
   izmax = korsz(z)
!
! IF NOGENL .GT. 0, GENERAL ELEMENTS EXIST AND HENCE THE GPST IS NOT
! CREATED AND SO DETCK WILL NOT BE CALLED.
!
   dodet = .TRUE.
   IF ( nogenl>0 ) dodet = .FALSE.
   ibuf(1) = ifecpt
   CALL rdtrl(ibuf(1))
   IF ( ibuf(3)==1 ) dodet = .FALSE.
!
! SET K4GG PURGE FLAGS
!
   nok4gg = -1
   k4ggsw = -1
!
! ATTEMPT TO OPEN THE OUTPUT FILE FOR THE KGG  MATRIX.  IF IT IS NOT
! IN THE OSCAR, EXECUTION WILL BE TERMINATED SINCE WE DO NOT ALLOW
! THE USER TO GENERATE ONLY A K4GG.
!
   igkgg = izmax - isys
   CALL open(*400,ifkgg,z(igkgg),outrw)
!
! WRITE A TWO WORD BCD HEADER AND CLOSE THE KGG FILE WITHOUT REWIND.
!
   CALL fname(ifkgg,z(1))
   CALL write(ifkgg,z(1),2,eor)
   CALL close(ifkgg,clsnrw)
!
! ATTEMPT TO OPEN THE K4GG FILE.
!
   ig4gg = igkgg
   iopt4 = 0
   CALL open(*100,if4gg,z(ig4gg),outrw)
   iopt4 = 1
   ig4gg = ig4gg - isys
   CALL fname(if4gg,z(1))
   CALL write(if4gg,z(1),2,eor)
   CALL close(if4gg,clsnrw)
!
! SET UP POINTERS TO GINO BUFFERS AND SET UP MATRIX CONTROL BLOCKS.
!
 100  igecpt = ig4gg - isys
   iggpct = igecpt - isys
   iggpst = iggpct - isys
   IF ( .NOT.dodet ) iggpst = iggpst + isys
   mcbkgg(1) = ifkgg
   mcbkgg(2) = 0
   mcbkgg(3) = 0
   mcbkgg(4) = 6
   mcbkgg(5) = iprec
   mcbkgg(6) = 0
   mcbkgg(7) = 0
   IF ( iopt4/=0 ) THEN
      mcb4gg(1) = if4gg
      DO i = 2 , 7
         mcb4gg(i) = mcbkgg(i)
      ENDDO
   ENDIF
!
! ATTEMPT TO READ THE CSTM INTO CORE.
!
   ncstm = 0
   icstm = 0
   left = iggpst - 1
   CALL open(*300,ifcstm,z(igkgg),inrw)
   CALL fwdrec(*500,ifcstm)
   CALL read(*600,*200,ifcstm,z(1),left,eor,ncstm)
!
! IF CORE WAS FILLED WITHOUT HITTING AN EOR CALL MESAGE
!
   CALL mesage(-8,ifcstm,ifcstm)
 200  left = left - ncstm
!
! PRETRD SETS UP FUTURE CALLS TO TRANSD.
!
   CALL pretrd(z(icstm+1),ncstm)
   CALL pretrs(z(icstm+1),ncstm)
   CALL close(ifcstm,clsrw)
 300  imat1 = ncstm
   nmat1 = 0
   nmat2 = 0
   nmat3 = 0
   nmat4 = 0
!
! CALL PREMAT TO READ MPT AND THE DIT INTO CORE
!
   imat11 = imat1 + 1
!*****
!  IF THIS IS A -HEAT- PROBLEM THE HMAT ROUTINE IS USED TO READ MAT4 AND
!  MAT5 CARDS INTO CORE.
!*****
   IF ( .NOT.heat ) THEN
!*****
!  NORMAL PREMAT PROCESSING.
!*****
      CALL premat(iz(imat11),z(imat11),z(igkgg),left,matcr,ifmpt,ifdit)
      left = left - matcr
      igpct = ncstm + matcr
   ELSE
      ihmat = imat11 + 1
      nhmat = imat11 + left - 2
      mptmpt = ifmpt
      idit = ifdit
      CALL hmat(0)
      left = left - nhmat + ihmat
      igpct = nhmat + 1
   ENDIF
!
! OPEN THE ECPT AND GPCT INPUT FILES AND THE GPST OUTPUT FILE.
!
   CALL open(*700,ifecpt,z(igecpt),inrw)
   CALL fwdrec(*800,ifecpt)
   CALL open(*900,ifgpct,z(iggpct),inrw)
   CALL fwdrec(*1000,ifgpct)
   IF ( dodet ) THEN
      CALL open(*1100,ifgpst,z(iggpst),outrw)
      CALL fname(ifgpst,ecpt(1))
      CALL write(ifgpst,ecpt(1),2,eor)
   ENDIF
!
! REOPEN THE KGG OUTPUT FILE WITHOUT REWIND, AND THE K4GG, IF CALLED FOR
!
   CALL open(*1200,ifkgg,z(igkgg),3)
   IF ( iopt4/=0 ) CALL open(*1300,if4gg,z(ig4gg),3)
!
! CALL SUBROUTINE SMA1A WHICH WILL PERFORM ALL THE COMPUTATIONS.
!
   CALL sma1a
   IF ( .NOT.linear ) option(1) = 1
!
! CLOSE FILES AND WRITE TRAILERS.
!
   CALL close(ifecpt,clsrw)
   CALL close(ifgpct,clsrw)
   IF ( dodet ) THEN
      CALL close(ifgpst,clsrw)
      CALL wrttrl(ifgpst)
   ENDIF
   CALL close(ifkgg,clsrw)
   mcbkgg(3) = mcbkgg(2)
   CALL wrttrl(mcbkgg(1))
   IF ( iopt4/=0 ) THEN
      CALL close(if4gg,clsrw)
      IF ( mcb4gg(6)==0 ) THEN
         DO i = 2 , 7
            mcb4gg(i) = 0
         ENDDO
         nok4gg = -1
      ELSE
         mcb4gg(3) = mcb4gg(2)
         CALL wrttrl(mcb4gg(1))
         nok4gg = 1
      ENDIF
   ENDIF
 400  RETURN
!
! SUBROUTINE SMA1 ERROR EXITS.
!
 500  ifile = ifcstm
   iparm = -2
   CALL mesage(iparm,ifile,nmsma1(1))
   GOTO 99999
 600  ifile = -ifcstm
   iparm = -2
   CALL mesage(iparm,ifile,nmsma1(1))
   GOTO 99999
 700  ifile = ifecpt
   GOTO 1400
 800  ifile = ifecpt
   iparm = -2
   CALL mesage(iparm,ifile,nmsma1(1))
   GOTO 99999
 900  ifile = ifgpct
   GOTO 1400
 1000 ifile = ifgpct
   iparm = -2
   CALL mesage(iparm,ifile,nmsma1(1))
   GOTO 99999
 1100 ifile = ifgpst
   GOTO 1400
 1200 ifile = ifkgg
   GOTO 1400
 1300 ifile = if4gg
 1400 iparm = -1
   CALL mesage(iparm,ifile,nmsma1(1))
99999 END SUBROUTINE sma1
