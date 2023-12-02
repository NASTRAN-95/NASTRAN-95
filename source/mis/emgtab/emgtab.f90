!*==emgtab.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE emgtab
   IMPLICIT NONE
   USE C_EMGFIL
   USE C_EMGPRM
   USE C_HMATDD
   USE C_NAMES
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: buf1 , file , icrq , sysbuf
   INTEGER , SAVE :: eor
   INTEGER , DIMENSION(2) , SAVE :: subr
   EXTERNAL close , fwdrec , mesage , open , prehma , premat , pretrd , pretrs , read
!
! End of declarations rewritten by SPAG
!
!*****
!     THIS ROUTINE OF THE -EMG- MODULE PREPARES OPEN CORE WITH SOME
!     VARIOUS TABLES.  CSTM, MAT, ETC.
!
!     UTILITY ROUTINES ARE USED FOR THE MOST PART.
!*****
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf)
   DATA subr/4HEMGT , 4HAB  / , eor/1/
!*****
!     READ -CSTM- INTO CORE.
!*****
   buf1 = Ncore - sysbuf - 2
   icrq = Jcore - buf1
   IF ( buf1>Jcore ) THEN
      Icstm = Jcore
      Ncstm = Jcore - 1
      file = Cstm
      CALL open(*200,Cstm,Z(buf1),Rdrew)
      CALL fwdrec(*200,Cstm)
      CALL read(*300,*100,Cstm,Z(Icstm),buf1-Jcore,eor,Lcstm)
      icrq = buf1 - Jcore
   ENDIF
   CALL mesage(-8,icrq,subr)
 100  CALL close(Cstm,Clsrew)
   Ncstm = Icstm + Lcstm - 1
   CALL pretrs(Z(Icstm),Lcstm)
   CALL pretrd(Z(Icstm),Lcstm)
!*****
!     HAMT AND PREMAT
!*****
 200  IF ( .NOT.Heat ) THEN
!
!     NON-HEAT PROBLEM THUS USE -MAT-
!
      Imat = Ncstm + 1
      CALL premat(Z(Imat),Z(Imat),Z(buf1),buf1-Imat,Lmat,Mpt,Dit)
      Nmat = Imat + Lmat - 1
      Ihmat = Nmat + 1
      Nhmat = Nmat
      Jcore = Nhmat + 1
   ELSE
!
!     HEAT PROBLEM THUS USE -HMAT-
!
      Imat = Ncstm + 1
      Nmat = Ncstm
      Iihmat = Nmat
      Nnhmat = Ncore
      Mptfil = Mpt
      Ditfil = Dit
      CALL prehma(Z)
      Ihmat = Iihmat
      Nhmat = Nnhmat
      Lhmat = Nhmat - Ihmat
      Jcore = Nhmat + 1
   ENDIF
!
   RETURN
!
 300  CALL mesage(-2,file,subr)
END SUBROUTINE emgtab
