
SUBROUTINE sma1
   IMPLICIT NONE
   LOGICAL Anytab , Dodet , Heat , Linear
   INTEGER Clsnrw , Clsrw , Eor , Frowic , I6x64 , I6x6k , Icstm , Idetck , Idit , Idum1 , If4gg , Ifcstm , Ifdit , Ifecpt , Ifgei ,&
         & Ifgpct , Ifgpst , Ifkgg , Ifmpt , Ig4gg , Igecpt , Iggei , Iggpct , Iggpst , Igkgg , Igpct , Ihmat , Incr , Inrw ,       &
         & Iopt4 , Ipoint , Iprec , Isys , Itherm , Iz(1) , Jmax , K4ggsw , Last , Left , Link(10) , Lrowic , Mcb4gg(7) , Mcbkgg(7) &
         & , Mptmpt , N6x64 , N6x6k , Ncstm , Ne(1) , Nelems , Neor , Ngpct , Nhmat , Nlinks , Nogenl , Nogo , Nok4gg , Npoint ,    &
         & Npvt , Nrowsc , Option(2) , Outrw , Tnrows
   DOUBLE PRECISION Dpdum(300) , Dz
   REAL Ecpt(100) , Skip(53) , Z(1)
   COMMON /blank / Nogenl , Nok4gg , Option
   COMMON /gpta1 / Nelems , Last , Incr , Ne
   COMMON /hmatdd/ Ihmat , Nhmat , Mptmpt , Idit , Linear , Anytab
   COMMON /sma1bk/ Icstm , Ncstm , Igpct , Ngpct , Ipoint , Npoint , I6x6k , N6x6k , I6x64 , N6x64
   COMMON /sma1cl/ Iopt4 , K4ggsw , Npvt , Left , Frowic , Lrowic , Nrowsc , Tnrows , Jmax , Nlinks , Link , Idetck , Dodet , Nogo
   COMMON /sma1dp/ Dpdum
   COMMON /sma1et/ Ecpt
   COMMON /sma1ht/ Heat
   COMMON /sma1io/ Ifcstm , Ifmpt , Ifdit , Idum1 , Ifecpt , Igecpt , Ifgpct , Iggpct , Ifgei , Iggei , Ifkgg , Igkgg , If4gg ,     &
                 & Ig4gg , Ifgpst , Iggpst , Inrw , Outrw , Clsnrw , Clsrw , Neor , Eor , Mcbkgg , Mcb4gg
   COMMON /system/ Isys , Skip , Iprec , Itherm
   COMMON /zzzzzz/ Z
   INTEGER i , ibuf(7) , ifile , imat1 , imat11 , iparm , izmax , matcr , nmat1 , nmat2 , nmat3 , nmat4 , nmsma1(2)
   INTEGER korsz
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
   EQUIVALENCE (Z(1),Iz(1),Dz)
!
   DATA nmsma1(1)/4HSMA1/ , nmsma1(2)/4H    /
!*****
!  SET THE LOGICAL HEAT FLAG IF THIS IS A -HEAT- FORMULATION
!*****
   CALL delset
   Linear = .TRUE.
   Option(1) = -1
   Heat = .FALSE.
   IF ( Itherm/=0 ) Heat = .TRUE.
!
   izmax = korsz(Z)
!
! IF NOGENL .GT. 0, GENERAL ELEMENTS EXIST AND HENCE THE GPST IS NOT
! CREATED AND SO DETCK WILL NOT BE CALLED.
!
   Dodet = .TRUE.
   IF ( Nogenl>0 ) Dodet = .FALSE.
   ibuf(1) = Ifecpt
   CALL rdtrl(ibuf(1))
   IF ( ibuf(3)==1 ) Dodet = .FALSE.
!
! SET K4GG PURGE FLAGS
!
   Nok4gg = -1
   K4ggsw = -1
!
! ATTEMPT TO OPEN THE OUTPUT FILE FOR THE KGG  MATRIX.  IF IT IS NOT
! IN THE OSCAR, EXECUTION WILL BE TERMINATED SINCE WE DO NOT ALLOW
! THE USER TO GENERATE ONLY A K4GG.
!
   Igkgg = izmax - Isys
   CALL open(*400,Ifkgg,Z(Igkgg),Outrw)
!
! WRITE A TWO WORD BCD HEADER AND CLOSE THE KGG FILE WITHOUT REWIND.
!
   CALL fname(Ifkgg,Z(1))
   CALL write(Ifkgg,Z(1),2,Eor)
   CALL close(Ifkgg,Clsnrw)
!
! ATTEMPT TO OPEN THE K4GG FILE.
!
   Ig4gg = Igkgg
   Iopt4 = 0
   CALL open(*100,If4gg,Z(Ig4gg),Outrw)
   Iopt4 = 1
   Ig4gg = Ig4gg - Isys
   CALL fname(If4gg,Z(1))
   CALL write(If4gg,Z(1),2,Eor)
   CALL close(If4gg,Clsnrw)
!
! SET UP POINTERS TO GINO BUFFERS AND SET UP MATRIX CONTROL BLOCKS.
!
 100  Igecpt = Ig4gg - Isys
   Iggpct = Igecpt - Isys
   Iggpst = Iggpct - Isys
   IF ( .NOT.Dodet ) Iggpst = Iggpst + Isys
   Mcbkgg(1) = Ifkgg
   Mcbkgg(2) = 0
   Mcbkgg(3) = 0
   Mcbkgg(4) = 6
   Mcbkgg(5) = Iprec
   Mcbkgg(6) = 0
   Mcbkgg(7) = 0
   IF ( Iopt4/=0 ) THEN
      Mcb4gg(1) = If4gg
      DO i = 2 , 7
         Mcb4gg(i) = Mcbkgg(i)
      ENDDO
   ENDIF
!
! ATTEMPT TO READ THE CSTM INTO CORE.
!
   Ncstm = 0
   Icstm = 0
   Left = Iggpst - 1
   CALL open(*300,Ifcstm,Z(Igkgg),Inrw)
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
!
! CALL PREMAT TO READ MPT AND THE DIT INTO CORE
!
   imat11 = imat1 + 1
!*****
!  IF THIS IS A -HEAT- PROBLEM THE HMAT ROUTINE IS USED TO READ MAT4 AND
!  MAT5 CARDS INTO CORE.
!*****
   IF ( .NOT.Heat ) THEN
!*****
!  NORMAL PREMAT PROCESSING.
!*****
      CALL premat(Iz(imat11),Z(imat11),Z(Igkgg),Left,matcr,Ifmpt,Ifdit)
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
! OPEN THE ECPT AND GPCT INPUT FILES AND THE GPST OUTPUT FILE.
!
   CALL open(*700,Ifecpt,Z(Igecpt),Inrw)
   CALL fwdrec(*800,Ifecpt)
   CALL open(*900,Ifgpct,Z(Iggpct),Inrw)
   CALL fwdrec(*1000,Ifgpct)
   IF ( Dodet ) THEN
      CALL open(*1100,Ifgpst,Z(Iggpst),Outrw)
      CALL fname(Ifgpst,Ecpt(1))
      CALL write(Ifgpst,Ecpt(1),2,Eor)
   ENDIF
!
! REOPEN THE KGG OUTPUT FILE WITHOUT REWIND, AND THE K4GG, IF CALLED FOR
!
   CALL open(*1200,Ifkgg,Z(Igkgg),3)
   IF ( Iopt4/=0 ) CALL open(*1300,If4gg,Z(Ig4gg),3)
!
! CALL SUBROUTINE SMA1A WHICH WILL PERFORM ALL THE COMPUTATIONS.
!
   CALL sma1a
   IF ( .NOT.Linear ) Option(1) = 1
!
! CLOSE FILES AND WRITE TRAILERS.
!
   CALL close(Ifecpt,Clsrw)
   CALL close(Ifgpct,Clsrw)
   IF ( Dodet ) THEN
      CALL close(Ifgpst,Clsrw)
      CALL wrttrl(Ifgpst)
   ENDIF
   CALL close(Ifkgg,Clsrw)
   Mcbkgg(3) = Mcbkgg(2)
   CALL wrttrl(Mcbkgg(1))
   IF ( Iopt4/=0 ) THEN
      CALL close(If4gg,Clsrw)
      IF ( Mcb4gg(6)==0 ) THEN
         DO i = 2 , 7
            Mcb4gg(i) = 0
         ENDDO
         Nok4gg = -1
      ELSE
         Mcb4gg(3) = Mcb4gg(2)
         CALL wrttrl(Mcb4gg(1))
         Nok4gg = 1
      ENDIF
   ENDIF
 400  RETURN
!
! SUBROUTINE SMA1 ERROR EXITS.
!
 500  ifile = Ifcstm
   iparm = -2
   CALL mesage(iparm,ifile,nmsma1(1))
   GOTO 99999
 600  ifile = -Ifcstm
   iparm = -2
   CALL mesage(iparm,ifile,nmsma1(1))
   GOTO 99999
 700  ifile = Ifecpt
   GOTO 1400
 800  ifile = Ifecpt
   iparm = -2
   CALL mesage(iparm,ifile,nmsma1(1))
   GOTO 99999
 900  ifile = Ifgpct
   GOTO 1400
 1000 ifile = Ifgpct
   iparm = -2
   CALL mesage(iparm,ifile,nmsma1(1))
   GOTO 99999
 1100 ifile = Ifgpst
   GOTO 1400
 1200 ifile = Ifkgg
   GOTO 1400
 1300 ifile = If4gg
 1400 iparm = -1
   CALL mesage(iparm,ifile,nmsma1(1))
99999 RETURN
END SUBROUTINE sma1
