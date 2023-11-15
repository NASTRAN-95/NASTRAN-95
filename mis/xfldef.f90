
SUBROUTINE xfldef(Name1,Name2,Nofind)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Bcdcnt , Bs , Cnmtp , Core(1) , Dmap(1) , Dmpcnt , Dmppnt , Dum(78) , Osprc , Seqno , Subset , Xgpid1(5)
   INTEGER Fmdmsk(7) , Fmed(1) , Fmedtp , Fnm(1) , Fnmtp , Iallon , Iapp , Icfpnt , Icftop , Ichar , Icold , Icpflg , Icrdtp ,      &
         & Icst , Ictlfl(1) , Idmapp , Idmpnt , Idsapp , Iequl , Iestim , Iexit(2) , Iflag , Ihapp , Imst , Insert , Iospnt ,       &
         & Irturn , Isavdw , Iseqn , Isgnon , Islsh , Iunst , Lctlfl , Ldmap , Length , Lmed , Loscar , Lptdic , Maskhi , Masklo ,  &
         & Masks(1) , Medpnt , Medtp , Modidx , Nbegin , Nblank , Nbpc , Nchkpt , Ncond , Ncpw , Ndiag , Ndmap , Nend , Nequiv ,    &
         & Nestm1 , Nestm2 , Newcrd , Nexit , Njump , Nmskcd , Nmskfl , Nmskrf , Noflgs , Nogo , Nosgn , Noutpt , Npurge , Nrept ,  &
         & Nrlfl , Nsave , Nsol , Ntime , Nwpc , Nxequi , Op , Os(5) , Osbot , Oscar(1) , Ptdbot , Ptdic(1)
   INTEGER Ptdtp , Reuse , Sol , Start , Two(4)
   COMMON /system/ Bs , Op , Nogo , Dum , Icpflg
   COMMON /two   / Two
   COMMON /xgpi4 / Irturn , Insert , Iseqn , Dmpcnt , Idmpnt , Dmppnt , Bcdcnt , Length , Icrdtp , Ichar , Newcrd , Modidx , Ldmap ,&
                 & Isavdw , Dmap
   COMMON /xgpi5 / Iapp , Start , Iexit , Sol , Subset , Iflag , Iestim , Icftop , Icfpnt , Lctlfl , Ictlfl
   COMMON /xgpi6 / Medtp , Fnmtp , Cnmtp , Medpnt , Lmed
   COMMON /xgpic / Icold , Islsh , Iequl , Nblank , Nxequi , Ndiag , Nsol , Ndmap , Nestm1 , Nestm2 , Nexit , Nbegin , Nend ,       &
                 & Njump , Ncond , Nrept , Ntime , Nsave , Noutpt , Nchkpt , Npurge , Nequiv , Ncpw , Nbpc , Nwpc , Maskhi ,        &
                 & Masklo , Isgnon , Nosgn , Iallon , Masks
   COMMON /xgpid / Icst , Iunst , Imst , Ihapp , Idsapp , Idmapp , Xgpid1 , Noflgs
   COMMON /xmdmsk/ Nmskcd , Nmskfl , Nmskrf , Fmdmsk
   COMMON /xoldpt/ Ptdtp , Ptdbot , Lptdic , Nrlfl , Seqno
   COMMON /zzzzzz/ Core
!
! Dummy argument declarations
!
   INTEGER Nofind
   INTEGER Name1(1) , Name2(1)
!
! Local variable declarations
!
   INTEGER and , or
   INTEGER andf , complf , orf
   INTEGER i , ifirst , ii , index , j , j1 , j2 , jj , k , k1 , l , n , nam1 , nam2 , nxchkp , ospnt , regen
   EXTERNAL andf , complf , orf
!
! End of declarations
!
!
!     THE PURPOSE OF THIS ROUTINE IS TO TURN ON ALL OSCAR ENTRY EXECUTE
!     FLAGS NECESSARY TO DEFINE FILE .
!
!                 DESCRIPTION OF ARGUMENTS
!     NAM1,NAM2 = NAME OF FILE TO BE DEFINED.
!     NOFIND    = INDICATES TO CALLING PROGRAM WHETHER OR NOT FILE WAS
!                 FOUND.
!
!                  ** CONTROL CARD NAMES **
!                  ** DMAP CARD NAMES **
   EQUIVALENCE (Core(1),Os(1),Loscar) , (Osprc,Os(2)) , (Osbot,Os(3)) , (Iospnt,Os(4)) , (Os(5),Oscar(1),Fnm(1),Fmed(1),Ptdic(1)) , &
    & (Medtp,Fmedtp) , (Two(4),Reuse)
   DATA nxchkp/4HXCHK/ , ifirst/0/
!
   and(i,j) = andf(i,j)
   or(i,j) = orf(i,j)
!
   nam1 = Name1(1)
   nam2 = Name2(1)
!
!     SCAN OPTDIC FOR FILE NAME
!
   regen = Nofind
   Nofind = 1
   IF ( Ptdbot>=Ptdtp ) THEN
      DO ii = Ptdtp , Ptdbot , 3
         i = Ptdbot + Ptdtp - ii
         IF ( Ptdic(i)==nam1 .AND. Ptdic(i+1)==nam2 ) GOTO 100
      ENDDO
   ENDIF
!
!     FILE NOT IN PTDIC - CHECK FNM TABLE IF RESTART IS MODIFIED AND
!     APPROACH IS NOT DMAP
!
   IF ( Start/=Icst .AND. Iapp/=Idmapp ) THEN
      IF ( regen>=0 ) THEN
         j = Fnmtp + 1
         k = Fnmtp + Fnm(Fnmtp)*3 - 2
         DO i = j , k , 3
            IF ( nam1==Fnm(i) .AND. nam2==Fnm(i+1) ) GOTO 50
         ENDDO
      ENDIF
      GOTO 99999
!
!     FILE IS IN FNM TABLE - CHECK FOR TABLE ERROR
!
 50   IF ( Fnm(i+2)>0 ) THEN
!
!     CLEAR ALL THE MASK WORDS
!
         k = Fmed(Fmedtp+1)
         DO l = 1 , k
            Fmdmsk(l) = 0
         ENDDO
!
!     SET BIT IN FMDMSK FOR FILE REGENERATION
!
         l = ((Fnm(i+2)-1)/31) + 1
         k = Fnm(i+2) - 31*(l-1) + 1
         Fmdmsk(l) = or(Fmdmsk(l),Two(k))
!
!     USE FMDMSK AND FMED TABLE TO TURN ON OSCAR EXECUTE FLAGS
!
         k = Fmed(Fmedtp+1)
         j1 = Fmedtp + 2
         j2 = j1 + Fmed(Fmedtp)*Fmed(Fmedtp+1) - k
         index = 0
         ospnt = 1
         DO j = j1 , j2 , k
            DO k1 = 1 , k
               jj = j + k1 - 1
               IF ( and(Fmed(jj),Fmdmsk(k1))/=0 ) GOTO 60
            ENDDO
            CYCLE
!
!     NON-ZERO ENTRY FOUND - COMPUTE DMAP SEQUENCE NUMBER FOR FMED ENTRY
!
 60         n = ((j-j1)/k) + 1
            IF ( and(Oscar(Iospnt+5),Nosgn)<n ) GOTO 99999
!
!     SET EXECUTINON FLAG FOR ALL OSCAR ENTRIES WITH SAME DMAP SEQ
!     NUMBER
!
 70         IF ( and(Oscar(ospnt+5),Nosgn)<n ) THEN
            ELSEIF ( and(Oscar(ospnt+5),Nosgn)==n ) THEN
               IF ( .NOT.(Oscar(ospnt+5)<0 .OR. (Oscar(ospnt+3)==nxchkp .AND. Icpflg==0)) ) THEN
                  IF ( ifirst/=1 ) THEN
                     ifirst = 1
                     CALL page1
                     CALL xgpimw(12,0,0,0)
                  ENDIF
                  IF ( index/=1 ) THEN
                     index = 1
                     CALL xgpimw(3,nam1,nam2,0)
                  ENDIF
                  CALL xgpimw(4,0,0,Oscar(ospnt))
                  Nofind = -1
                  Oscar(ospnt+5) = orf(Oscar(ospnt+5),Isgnon)
               ENDIF
            ELSE
               CYCLE
            ENDIF
            IF ( ospnt<Osbot ) THEN
               ospnt = ospnt + Oscar(ospnt)
               GOTO 70
            ENDIF
         ENDDO
!
!     MAKE SURE SOME MODULES WERE TURNED ON
!
         IF ( Nofind==-1 ) THEN
!
!     NEGATE FNM TABLE ENTRY FOR THIS FILE
!
            Fnm(i+2) = -Fnm(i+2)
!
!     TURN OFF REUSE FLAGS IN PTDIC
!
            IF ( Ptdbot>Ptdtp .AND. Iflag==0 ) THEN
               j = complf(Reuse)
               DO i = Ptdtp , Ptdbot , 3
                  Ptdic(i+2) = andf(j,Ptdic(i+2))
               ENDDO
            ENDIF
            GOTO 99999
         ENDIF
      ENDIF
!
!     D I A G N O S T I C    M E S S A G E S
!
!     MED OR FILE TABLE INCORRECT FOR REGENERATING FILE
!
      CALL xgpidg(41,nam1,nam2,Fnm(i+2))
      Nofind = -1
      Nogo = 2
   ENDIF
   GOTO 99999
!
!     FILE IS IN PTDIC - SET REUSE FLAG FOR ALL EQUIVALENCED FILES
!
 100  IF ( Ptdic(i+2)<0 ) THEN
      DO j = Ptdtp , Ptdbot , 3
         IF ( and(Ptdic(j+2),Noflgs)==and(Ptdic(i+2),Noflgs) ) Ptdic(j+2) = or(Ptdic(j+2),Reuse)
      ENDDO
   ENDIF
   Ptdic(i+2) = or(Ptdic(i+2),Reuse)
   Nofind = 0
99999 END SUBROUTINE xfldef
