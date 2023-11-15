
SUBROUTINE xrgdcf(Irestb)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Dum(98)
   INTEGER Ichar(80) , Icol , Icount , Idmap , Ierror , Ignore , Ind , Iphase , Irestr , Iscr , Istate , Isysbf , Itype , Limit(2) ,&
         & Member(2) , Name(2) , Nout , Nsubst , Num(2) , Number , Nument , Record(20)
   COMMON /system/ Isysbf , Nout , Dum
   COMMON /xrgdxx/ Irestr , Nsubst , Iphase , Icol , Number , Itype , Istate , Ierror , Num , Ind , Nument , Record , Ichar ,       &
                 & Limit , Icount , Idmap , Iscr , Name , Member , Ignore
!
! Dummy argument declarations
!
   INTEGER Irestb(7)
!
! Local variable declarations
!
   INTEGER ibit , iend , istr , iword , k
   INTEGER orf
   EXTERNAL orf
!
! End of declarations
!
!
!     PURPOSE - XRGDCF PROCESSES THE '****CARD', '****FILE' AND
!               '****RFMT' CONTROL CARDS WITHIN THE RIGID DMAP
!               DATA BASE.
!
!     AUTHOR  - RPK CORPORATION; DECEMBER, 1983
!
!     INPUT
!       /SYSTEM/
!         NOUT         UNIT NUMBER FOR THE OUTPUT PRINT FILE
!       /XRGDXX/
!         NUM          VALUE OF THE NUMBER OR RANGE OF NUMBERS
!                      IN THE CURRENT FIELD BEING PROCESSED
!
!     OUTPUT
!       ARGUMENTS
!         IRESTB       THE MODULE EXECUTION DECISION TABLE
!       OTHER
!         /XRGDXX/
!           ICOL       CURRENT COLUMN NUMBER BEING PROCESSED IN
!                      THE CARD
!           IERROR     ERROR FLAG - NON-ZERO IF AN ERROR OCCURRS
!
!     LOCAL VARIABLES
!       IBIT           BIT NUMBER FOR FLAG IN THE MODULE EXEC. DEC.
!                      TABLE
!       IEND           LAST NUMBER OF RANGE OF NUMBERS READ FROM
!                      THE CURRENT FIELD
!       ISTR           SAME AS IEND EXCEPT FIRST NUMBER
!       IWORD          SAME AS IBIT BUT REFERS TO THE WORD NUMBER
!
!     FUNCTIONS
!       XRGDCF PROCESSES THE ABOVE TYPES OF CARDS WHICH ALL HAVE
!       FORMATS AS FOLLOWS:  '****XXXX   M1,M2,..'
!       WHERE M- IS IN ANY OF THE FOLLOWING FORMS ( NNN  OR NNN-NNN).
!       NNN IS AN INTEGER NUMBER AND THE '-' REFERS TO A RANGE
!       WHERE THE RANGE MUST BE IN ASCENDING ORDER.
!       XRGDCF CALLS XDCODE TO CONVERT THE CARD IMAGE TO 80A1 AND
!       CALLS XRGDEV TO VALIDATE THE SYNTAX AND TO GET A M-
!       ENTRY FROM THE CARD.  BASED ON THE VALUE(S) RETURNED IN
!       NUM, THE CORRESPONDING BITS ARE TURNED ON IN THE MODULE
!       EXECUTION DECISION TABLE.  PROCESSING CONTINUES UNTIL ALL
!       FIELDS OF THE CARD HAVE BEEN PROCESSED.
!
!
!     SUBROUTINES CALLED - XDCODE, XRGDEV
!
!     CALLING SUBROUTINES - XRGRFM
!
!     ERRORS - NONE
!
!
   Ierror = 0
   Icol = 9
   CALL xdcode
   DO
      CALL xrgdev
      IF ( Ierror/=0 .OR. Icol>80 ) EXIT
      istr = Num(1)
      iend = Num(2)
      DO k = istr , iend
         iword = (k-1)/31
         ibit = 2**(31*iword+31-k)
         Irestb(iword+1) = orf(Irestb(iword+1),ibit)
      ENDDO
      Icol = Icol + 1
   ENDDO
END SUBROUTINE xrgdcf
