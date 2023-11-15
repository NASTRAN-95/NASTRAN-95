
SUBROUTINE xlnkhd
!
!     THE PURPOSE OF XLNKHD IS TO GENERATE THE LINK HEADER SECTION FOR
!     AN OSCAR ENTRY
!
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Alter(2) , Bcdcnt , Cnmtp , Core(1) , Cpflg , Dmap(1) , Dmpcnt , Dmppnt , Dummy(5) , Fnmtp , Iallon , Iapp , Icfpnt ,    &
         & Icftop , Ichar , Icold , Icrdtp , Icst , Ictlfl(1) , Idmapp , Idmpnt , Idsapp , Iequl , Iestim , Ifirst , Iflag , Ihapp ,&
         & Ihead , Imst , Insert , Irturn , Isavdw , Iseqn , Isgnon , Islsh , Isys(81) , Iunst , Lctlfl , Ldmap , Length , Lmed ,   &
         & Lmpl , Loscar , Maskhi , Masklo , Masks(1) , Med(1) , Medmsk(7) , Medpnt , Medtp , Modidx , Mpl(1) , Mplpnt , Nbegin ,   &
         & Nblank , Nbpc , Nchkpt , Ncond , Ncpw , Ndiag , Ndmap , Nend , Nequiv , Nestm1 , Nestm2 , Newcrd , Nexit , Njump ,       &
         & Nmskcd , Nmskfl , Nmskrf , Nosgn , Noutpt , Npurge , Nrept , Nsave , Nsol , Ntime , Nwpc , Nxequi
   INTEGER Os(5) , Osbot , Oscar(1) , Ospnt , Osprc , Seqno , Sol , Start , Subset , Xx(4)
   COMMON /autohd/ Ihead
   COMMON /system/ Isys , Cpflg
   COMMON /xgpi2 / Lmpl , Mplpnt , Mpl
   COMMON /xgpi4 / Irturn , Insert , Iseqn , Dmpcnt , Idmpnt , Dmppnt , Bcdcnt , Length , Icrdtp , Ichar , Newcrd , Modidx , Ldmap ,&
                 & Isavdw , Dmap
   COMMON /xgpi5 / Iapp , Start , Alter , Sol , Subset , Iflag , Iestim , Icftop , Icfpnt , Lctlfl , Ictlfl
   COMMON /xgpi6 / Medtp , Fnmtp , Cnmtp , Medpnt , Lmed , Dummy , Ifirst
   COMMON /xgpic / Icold , Islsh , Iequl , Nblank , Nxequi , Ndiag , Nsol , Ndmap , Nestm1 , Nestm2 , Nexit , Nbegin , Nend ,       &
                 & Njump , Ncond , Nrept , Ntime , Nsave , Noutpt , Nchkpt , Npurge , Nequiv , Ncpw , Nbpc , Nwpc , Maskhi ,        &
                 & Masklo , Isgnon , Nosgn , Iallon , Masks
   COMMON /xgpid / Icst , Iunst , Imst , Ihapp , Idsapp , Idmapp
   COMMON /xmdmsk/ Nmskcd , Nmskfl , Nmskrf , Medmsk
   COMMON /xoldpt/ Xx , Seqno
   COMMON /zzzzzz/ Core
!
! Local variable declarations
!
   INTEGER and , or
   INTEGER andf , lshift , orf , rshift
   INTEGER i , j , k , mpler , xchk
   EXTERNAL andf , lshift , orf , rshift
!
! End of declarations
!
   EQUIVALENCE (Core(1),Os(1),Loscar) , (Os(2),Osprc) , (Os(3),Osbot) , (Os(4),Ospnt) , (Oscar(1),Med(1),Os(5))
   DATA xchk/4HXCHK/
!
   or(i,j) = orf(i,j)
   and(i,j) = andf(i,j)
   mpler = Mpl(Mplpnt+3)
   IF ( Ihead==1 ) mpler = 4
!
!     CHECK FOR DECLARATIVE INSTRUCTION
!
   IF ( Ihead/=1 ) THEN
      IF ( mpler/=5 ) THEN
!
!     UPDATE OSCAR PARAMETERS
!
         Osprc = Osbot
         Osbot = Oscar(Osbot) + Osbot
         Ospnt = Osbot
         Iseqn = Oscar(Osprc+1) + 1
!
!     LOAD LINK HEADER INFORMATION
!
         Oscar(Ospnt) = 6
         Oscar(Ospnt+1) = Iseqn
         Oscar(Ospnt+2) = mpler + lshift(Modidx,16)
         Oscar(Ospnt+3) = Dmap(Dmppnt)
         Oscar(Ospnt+4) = Dmap(Dmppnt+1)
         Oscar(Ospnt+5) = Dmpcnt
!
         Mplpnt = Mplpnt + 4
      ELSE
         Ospnt = Oscar(Osbot) + Osbot
      ENDIF
   ENDIF
   Oscar(Ospnt+5) = or(Isgnon,Oscar(Ospnt+5))
!
!     ALWAYS RAISE EXECUTE FLAG FOR COLD START RUNS
!
   IF ( Start/=Icst ) THEN
!
!     COMPARE SEQ NO. WITH REENTRY SEQ NO.
!
      IF ( Dmpcnt>=rshift(Seqno,16) ) THEN
!
!     WE ARE BEYOND REENTRY POINT - EXECUTE ALL MODULES HERE ON OUT.
!
         IF ( andf(Maskhi,Seqno)==0 .AND. mpler/=5 ) Seqno = or(Iseqn,and(Masklo,Seqno))
!
!     WE ARE BEFORE REENTRY POINT - CHECK APPROACH AND TYPE OF RESTART
!     ALWAYS RAISE EXECUTE FLAG FOR INSERT FOR MODIFIED RESTARTS.
!
      ELSEIF ( Insert==0 .OR. Start/=Imst ) THEN
         IF ( Start==Imst ) THEN
!
!     FOR RIGID FORMAT - CHECK DECISION TABLE FOR MODIFIED RESTART
!
            i = Med(Medtp+1)
            DO j = 1 , i
               k = Medpnt + j - 1
               IF ( and(Med(k),Medmsk(j))/=0 ) GOTO 100
            ENDDO
            Oscar(Ospnt+5) = and(Nosgn,Oscar(Ospnt+5))
         ELSE
!
!     LOWER EXECUTE FLAG FOR UNMODIFIED RESTART RUNS.
!
            Oscar(Ospnt+5) = and(Nosgn,Oscar(Ospnt+5))
            IF ( mpler==5 ) GOTO 200
            RETURN
         ENDIF
      ENDIF
   ENDIF
 100  IF ( Oscar(Ospnt+3)==xchk .AND. Cpflg==0 ) Oscar(Ospnt+5) = and(Nosgn,Oscar(Ospnt+5))
   IF ( Oscar(Ospnt+5)>=0 .AND. mpler/=5 ) RETURN
!
!     PRINT COMPILE/EXECUTE FLAG FOR RESTART
!
 200  IF ( Start==Icst .OR. Ifirst==0 ) RETURN
   IF ( Dmpcnt==Iflag .AND. Insert==0 ) RETURN
   Iflag = Dmpcnt
   i = 7
   IF ( mpler==5 ) i = 10
   CALL xgpimw(i,0,0,0)
END SUBROUTINE xlnkhd
