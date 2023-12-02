!*==xrgdcf.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xrgdcf(Irestb)
   USE c_system
   USE c_xrgdxx
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(7) :: Irestb
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: ibit , iend , istr , iword , k
   EXTERNAL orf , xdcode , xrgdev
!
! End of declarations rewritten by SPAG
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
   ierror = 0
   icol = 9
   CALL xdcode
   SPAG_Loop_1_1: DO
      CALL xrgdev
      IF ( ierror/=0 .OR. icol>80 ) EXIT SPAG_Loop_1_1
      istr = num(1)
      iend = num(2)
      DO k = istr , iend
         iword = (k-1)/31
         ibit = 2**(31*iword+31-k)
         Irestb(iword+1) = orf(Irestb(iword+1),ibit)
      ENDDO
      icol = icol + 1
   ENDDO SPAG_Loop_1_1
END SUBROUTINE xrgdcf
