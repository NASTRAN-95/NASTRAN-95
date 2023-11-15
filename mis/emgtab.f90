
SUBROUTINE emgtab
   IMPLICIT NONE
!
! COMMON variable declarations
!
   LOGICAL Anycon , Error , Heat
   INTEGER Cls , Clsrew , Cstm , Dit , Ditfil , Est , Flags(3) , Geom2 , Icmbar , Icong , Icore , Icstm , Idit , Ihmat , Iihmat ,   &
         & Imat , Jcore , Ksystm(65) , Lcong , Lcstm , Lhmat , Lmat , Mpt , Mptfil , Ncong , Ncore , Ncstm , Ndit , Nhmat , Nmat ,  &
         & Nnhmat , Precis , Rd , Rdrew , Sysbuf , Wrt , Wrtrew , Z(1)
   COMMON /emgfil/ Est , Cstm , Mpt , Dit , Geom2
   COMMON /emgprm/ Icore , Jcore , Ncore , Icstm , Ncstm , Imat , Nmat , Ihmat , Nhmat , Idit , Ndit , Icong , Ncong , Lcong ,      &
                 & Anycon , Flags , Precis , Error , Heat , Icmbar , Lcstm , Lmat , Lhmat
   COMMON /hmatdd/ Iihmat , Nnhmat , Mptfil , Ditfil
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Cls
   COMMON /system/ Ksystm
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER buf1 , eor , file , icrq , subr(2)
!
! End of declarations
!
!*****
!     THIS ROUTINE OF THE -EMG- MODULE PREPARES OPEN CORE WITH SOME
!     VARIOUS TABLES.  CSTM, MAT, ETC.
!
!     UTILITY ROUTINES ARE USED FOR THE MOST PART.
!*****
   EQUIVALENCE (Ksystm(1),Sysbuf)
   DATA subr/4HEMGT , 4HAB  / , eor/1/
!*****
!     READ -CSTM- INTO CORE.
!*****
   buf1 = Ncore - Sysbuf - 2
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
