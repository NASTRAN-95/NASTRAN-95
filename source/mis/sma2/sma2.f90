!*==sma2.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE sma2
   IMPLICIT NONE
   USE C_BLANK
   USE C_GPTA1
   USE C_HMATDD
   USE C_SMA2BK
   USE C_SMA2CL
   USE C_SMA2DP
   USE C_SMA2ET
   USE C_SMA2HT
   USE C_SMA2IO
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ifile , imat1 , imat11 , iparm , izmax , matcr , nmat1 , nmat2 , nmat3 , nmat4
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) , SAVE :: nmsma2
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
   Heat = .FALSE.
   IF ( Itherm/=0 ) Heat = .TRUE.
!
!
   CALL delset
   izmax = korsz(Z)
!
! SET PURGE FLAGS FOR BGG AND NO PURGE FLAG FOR MGG.
!
   Bggind = -1
   Nobgg = -1
   Nomgg = 1
!
! ATTEMPT TO OPEN THE OUTPUT FILE FOR THE MASS MATRIX.  IF IT IS NOT
! IN THE OSCAR, EXECUTION WILL BE TERMINATED SINCE WE DO NOT ALLOW
! THE USER TO GENERATE ONLY A BGG. (EXCEPT IN A HEAT TRANSER PROBLEM)
!
   Igmgg = izmax - Isys
   IF ( .NOT.(Heat) ) THEN
      CALL open(*400,Ifmgg,Z(Igmgg),Outrw)
!
! WRITE A TWO WORD BCD HEADER AND CLOSE THE MGG FILE WITHOUT REWIND.
!
      CALL fname(Ifmgg,Z(1))
      CALL write(Ifmgg,Z(1),2,Eor)
      CALL close(Ifmgg,Clsnrw)
   ENDIF
!
! ATTEMPT TO OPEN THE BGG FILE.
!
   Igbgg = Igmgg
   Ioptb = 0
   CALL open(*100,Ifbgg,Z(Igbgg),Outrw)
   Ioptb = 1
   Igbgg = Igbgg - Isys
   CALL fname(Ifbgg,Z(1))
   CALL write(Ifbgg,Z(1),2,Eor)
   CALL close(Ifbgg,Clsnrw)
!
! SET UP POINTERS TO GINO BUFFERS AND SET UP MATRIX CONTROL BLOCKS.
!
 100  Igecpt = Igbgg - Isys
   Iggpct = Igecpt - Isys
   Mcbmgg(1) = Ifmgg
   Mcbmgg(2) = 0
   Mcbmgg(3) = 0
   Mcbmgg(4) = 6
   Mcbmgg(5) = Iprec
   Mcbmgg(6) = 0
   Mcbmgg(7) = 0
   IF ( Ioptb/=0 ) THEN
      Mcbbgg(1) = Ifbgg
      DO i = 2 , 7
         Mcbbgg(i) = Mcbmgg(i)
      ENDDO
   ENDIF
!
! ATTEMPT TO READ THE CSTM INTO CORE.
!
   Ncstm = 0
   Icstm = 0
   Left = Iggpct - 1
   CALL open(*300,Ifcstm,Z(Igmgg),Inrw)
   CALL fwdrec(*500,Ifcstm)
   CALL read(*600,*200,Ifcstm,Z(1),Left,Eor,Ncstm)
!
! IF CORE WAS FILLED WITHOUT HITTING AN EOR CALL MESAGE
!
   CALL mesage(-8,Ifcstm,Ifcstm)
 200  Left = Left - Ncstm
!
! PRETRD SETS UP FUTURE CALLS TO TRANSD.
!
   CALL pretrd(Z(Icstm+1),Ncstm)
   CALL pretrs(Z(Icstm+1),Ncstm)
   CALL close(Ifcstm,Clsrw)
 300  imat1 = Ncstm
   nmat1 = 0
   nmat2 = 0
   nmat3 = 0
   nmat4 = 0
   imat11 = imat1 + 1
!*****
!  IF -HEAT- PROBLEM THEN HMAT IS USED FOR MAT4 AND MAT5 CARDS.
!*****
   IF ( .NOT.Heat ) THEN
      CALL premat(iz(imat11),Z(imat11),Z(Igmgg),Left,matcr,Ifmpt,Ifdit)
      Left = Left - matcr
      Igpct = Ncstm + matcr
   ELSE
      Ihmat = imat11 + 1
      Nhmat = imat11 + Left - 2
      Mptmpt = Ifmpt
      Idit = Ifdit
      CALL hmat(0)
      Left = Left - Nhmat + Ihmat
      Igpct = Nhmat + 1
   ENDIF
!
! OPEN THE ECPT INPUT FILE AND THE GPCT INPUT FILE.
!
   CALL open(*700,Ifecpt,Z(Igecpt),Inrw)
   CALL fwdrec(*800,Ifecpt)
   CALL open(*900,Ifgpct,Z(Iggpct),Inrw)
   CALL fwdrec(*1000,Ifgpct)
!
! REOPEN THE MGG OUTPUT FILE WITHOUT REWIND, AND THE BGG, IF CALLED FOR.
!
   IF ( .NOT.Heat ) CALL open(*1100,Ifmgg,Z(Igmgg),3)
   IF ( Ioptb/=0 ) CALL open(*1200,Ifbgg,Z(Igbgg),3)
!
! CALL SUBROUTINE SMA2A WHICH WILL PERFORM ALL THE COMPUTATIONS.
!
   CALL sma2a
!
! CLOSE FILES AND WRITE TRAILERS.
!
   CALL close(Ifgpct,Clsrw)
   CALL close(Ifecpt,Clsrw)
   CALL close(Ifmgg,Clsrw)
   Mcbmgg(3) = Mcbmgg(2)
   IF ( Mcbmgg(6)==0 ) THEN
      DO i = 2 , 7
         Mcbmgg(i) = 0
      ENDDO
      Nomgg = -1
   ENDIF
   IF ( .NOT.Heat ) CALL wrttrl(Mcbmgg)
   IF ( Ioptb/=0 ) THEN
      CALL close(Ifbgg,Clsrw)
      IF ( Mcbbgg(6)==0 ) THEN
         DO i = 2 , 7
            Mcbbgg(i) = 0
         ENDDO
         Nobgg = -1
      ELSE
         Mcbbgg(3) = Mcbbgg(2)
         CALL wrttrl(Mcbbgg(1))
         Nobgg = 1
      ENDIF
   ENDIF
 400  RETURN
!
! SUBROUTINE SMA2 ERROR EXITS.
!
 500  ifile = Ifcstm
   iparm = -2
   CALL mesage(iparm,ifile,nmsma2(1))
   GOTO 99999
 600  ifile = -Ifcstm
   iparm = -2
   CALL mesage(iparm,ifile,nmsma2(1))
   GOTO 99999
 700  ifile = Ifecpt
   iparm = -1
   CALL mesage(iparm,ifile,nmsma2(1))
   GOTO 99999
 800  ifile = Ifecpt
   iparm = -2
   CALL mesage(iparm,ifile,nmsma2(1))
   GOTO 99999
 900  ifile = Ifgpct
   iparm = -1
   CALL mesage(iparm,ifile,nmsma2(1))
   GOTO 99999
 1000 ifile = Ifgpct
   iparm = -2
   CALL mesage(iparm,ifile,nmsma2(1))
   GOTO 99999
 1100 ifile = Ifmgg
   iparm = -2
   CALL mesage(iparm,ifile,nmsma2(1))
   GOTO 99999
 1200 ifile = Ifbgg
   iparm = -2
   CALL mesage(iparm,ifile,nmsma2(1))
99999 END SUBROUTINE sma2
