!*==sma2.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE sma2
   IMPLICIT NONE
   USE c_blank
   USE c_gpta1
   USE c_hmatdd
   USE c_sma2bk
   USE c_sma2cl
   USE c_sma2dp
   USE c_sma2et
   USE c_sma2ht
   USE c_sma2io
   USE c_system
   USE c_zzzzzz
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ifile , imat1 , imat11 , iparm , izmax , matcr , nmat1 , nmat2 , nmat3 , nmat4
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) , SAVE :: nmsma2
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
! ******
! THIS ROUTINE IS A DRIVER AND INITIALIZATION PROGRAM FOR MODULE
! 2.4.2 OF THE NASTRAN SYSTEM.  IT GENERATES THE MASS MATRIX, MGG, AND
! THE DAMPING MATRIX, BGG.
! ******
!
!
!
!
!
!
!
!
!
!
!
!
!
!
!
!
! SMA2 I/O PARAMETERS
!
!
! SMA2 VARIABLE CORE
!
!
! SMA2 VARIABLE CORE BOOKKEEPING PARAMETERS.
!
!
! SMA2 PROGRAM CONTROL PARAMETERS
!
!
! ELEMENT DATA
!
!
! ECPT COMMON BLOCK
!
!
! SCRATCH BLOCK FOR ELEMENT ROUTINES
!
!
!
!
!
   !>>>>EQUIVALENCE (Z(1),Iz(1),Dz)
!
!
!
   DATA nmsma2(1)/4HSMA2/ , nmsma2(2)/4H    /
!
!*****
!  SET HEAT FLAG
!*****
   heat = .FALSE.
   IF ( itherm/=0 ) heat = .TRUE.
!
!
   CALL delset
   izmax = korsz(z)
!
! SET PURGE FLAGS FOR BGG AND NO PURGE FLAG FOR MGG.
!
   bggind = -1
   nobgg = -1
   nomgg = 1
!
! ATTEMPT TO OPEN THE OUTPUT FILE FOR THE MASS MATRIX.  IF IT IS NOT
! IN THE OSCAR, EXECUTION WILL BE TERMINATED SINCE WE DO NOT ALLOW
! THE USER TO GENERATE ONLY A BGG. (EXCEPT IN A HEAT TRANSER PROBLEM)
!
   igmgg = izmax - isys
   IF ( .NOT.(heat) ) THEN
      CALL open(*400,ifmgg,z(igmgg),outrw)
!
! WRITE A TWO WORD BCD HEADER AND CLOSE THE MGG FILE WITHOUT REWIND.
!
      CALL fname(ifmgg,z(1))
      CALL write(ifmgg,z(1),2,eor)
      CALL close(ifmgg,clsnrw)
   ENDIF
!
! ATTEMPT TO OPEN THE BGG FILE.
!
   igbgg = igmgg
   ioptb = 0
   CALL open(*100,ifbgg,z(igbgg),outrw)
   ioptb = 1
   igbgg = igbgg - isys
   CALL fname(ifbgg,z(1))
   CALL write(ifbgg,z(1),2,eor)
   CALL close(ifbgg,clsnrw)
!
! SET UP POINTERS TO GINO BUFFERS AND SET UP MATRIX CONTROL BLOCKS.
!
 100  igecpt = igbgg - isys
   iggpct = igecpt - isys
   mcbmgg(1) = ifmgg
   mcbmgg(2) = 0
   mcbmgg(3) = 0
   mcbmgg(4) = 6
   mcbmgg(5) = iprec
   mcbmgg(6) = 0
   mcbmgg(7) = 0
   IF ( ioptb/=0 ) THEN
      mcbbgg(1) = ifbgg
      DO i = 2 , 7
         mcbbgg(i) = mcbmgg(i)
      ENDDO
   ENDIF
!
! ATTEMPT TO READ THE CSTM INTO CORE.
!
   ncstm = 0
   icstm = 0
   left = iggpct - 1
   CALL open(*300,ifcstm,z(igmgg),inrw)
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
   imat11 = imat1 + 1
!*****
!  IF -HEAT- PROBLEM THEN HMAT IS USED FOR MAT4 AND MAT5 CARDS.
!*****
   IF ( .NOT.heat ) THEN
      CALL premat(iz(imat11),z(imat11),z(igmgg),left,matcr,ifmpt,ifdit)
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
! OPEN THE ECPT INPUT FILE AND THE GPCT INPUT FILE.
!
   CALL open(*700,ifecpt,z(igecpt),inrw)
   CALL fwdrec(*800,ifecpt)
   CALL open(*900,ifgpct,z(iggpct),inrw)
   CALL fwdrec(*1000,ifgpct)
!
! REOPEN THE MGG OUTPUT FILE WITHOUT REWIND, AND THE BGG, IF CALLED FOR.
!
   IF ( .NOT.heat ) CALL open(*1100,ifmgg,z(igmgg),3)
   IF ( ioptb/=0 ) CALL open(*1200,ifbgg,z(igbgg),3)
!
! CALL SUBROUTINE SMA2A WHICH WILL PERFORM ALL THE COMPUTATIONS.
!
   CALL sma2a
!
! CLOSE FILES AND WRITE TRAILERS.
!
   CALL close(ifgpct,clsrw)
   CALL close(ifecpt,clsrw)
   CALL close(ifmgg,clsrw)
   mcbmgg(3) = mcbmgg(2)
   IF ( mcbmgg(6)==0 ) THEN
      DO i = 2 , 7
         mcbmgg(i) = 0
      ENDDO
      nomgg = -1
   ENDIF
   IF ( .NOT.heat ) CALL wrttrl(mcbmgg)
   IF ( ioptb/=0 ) THEN
      CALL close(ifbgg,clsrw)
      IF ( mcbbgg(6)==0 ) THEN
         DO i = 2 , 7
            mcbbgg(i) = 0
         ENDDO
         nobgg = -1
      ELSE
         mcbbgg(3) = mcbbgg(2)
         CALL wrttrl(mcbbgg(1))
         nobgg = 1
      ENDIF
   ENDIF
 400  RETURN
!
! SUBROUTINE SMA2 ERROR EXITS.
!
 500  ifile = ifcstm
   iparm = -2
   CALL mesage(iparm,ifile,nmsma2(1))
   GOTO 99999
 600  ifile = -ifcstm
   iparm = -2
   CALL mesage(iparm,ifile,nmsma2(1))
   GOTO 99999
 700  ifile = ifecpt
   iparm = -1
   CALL mesage(iparm,ifile,nmsma2(1))
   GOTO 99999
 800  ifile = ifecpt
   iparm = -2
   CALL mesage(iparm,ifile,nmsma2(1))
   GOTO 99999
 900  ifile = ifgpct
   iparm = -1
   CALL mesage(iparm,ifile,nmsma2(1))
   GOTO 99999
 1000 ifile = ifgpct
   iparm = -2
   CALL mesage(iparm,ifile,nmsma2(1))
   GOTO 99999
 1100 ifile = ifmgg
   iparm = -2
   CALL mesage(iparm,ifile,nmsma2(1))
   GOTO 99999
 1200 ifile = ifbgg
   iparm = -2
   CALL mesage(iparm,ifile,nmsma2(1))
99999 END SUBROUTINE sma2
