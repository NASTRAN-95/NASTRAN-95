!*==emgtab.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE emgtab
   USE c_emgfil
   USE c_emgprm
   USE c_hmatdd
   USE c_names
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
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
   buf1 = ncore - sysbuf - 2
   icrq = jcore - buf1
   IF ( buf1>jcore ) THEN
      icstm = jcore
      ncstm = jcore - 1
      file = cstm
      CALL open(*200,cstm,z(buf1),rdrew)
      CALL fwdrec(*200,cstm)
      CALL read(*300,*100,cstm,z(icstm),buf1-jcore,eor,lcstm)
      icrq = buf1 - jcore
   ENDIF
   CALL mesage(-8,icrq,subr)
 100  CALL close(cstm,clsrew)
   ncstm = icstm + lcstm - 1
   CALL pretrs(z(icstm),lcstm)
   CALL pretrd(z(icstm),lcstm)
!*****
!     HAMT AND PREMAT
!*****
 200  IF ( .NOT.heat ) THEN
!
!     NON-HEAT PROBLEM THUS USE -MAT-
!
      imat = ncstm + 1
      CALL premat(z(imat),z(imat),z(buf1),buf1-imat,lmat,mpt,dit)
      nmat = imat + lmat - 1
      ihmat = nmat + 1
      nhmat = nmat
      jcore = nhmat + 1
   ELSE
!
!     HEAT PROBLEM THUS USE -HMAT-
!
      imat = ncstm + 1
      nmat = ncstm
      iihmat = nmat
      nnhmat = ncore
      mptfil = mpt
      ditfil = dit
      CALL prehma(z)
      ihmat = iihmat
      nhmat = nnhmat
      lhmat = nhmat - ihmat
      jcore = nhmat + 1
   ENDIF
!
   RETURN
!
 300  CALL mesage(-2,file,subr)
END SUBROUTINE emgtab
