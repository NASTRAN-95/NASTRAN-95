
SUBROUTINE xrgsub(Irestb,Subset)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ichar(80) , Icol , Icount , Idmap , Ierror , Ignore , Ind , Iphase , Irestr , Iscr , Istate , Itype , Limit(2) ,         &
         & Member(2) , Name(2) , Nsubst , Num(2) , Number , Nument , Record(20)
   COMMON /xrgdxx/ Irestr , Nsubst , Iphase , Icol , Number , Itype , Istate , Ierror , Num , Ind , Nument , Record , Ichar ,       &
                 & Limit , Icount , Idmap , Iscr , Name , Member , Ignore
!
! Dummy argument declarations
!
   INTEGER Irestb(7) , Subset(12)
!
! Local variable declarations
!
   INTEGER iend , istr , k , kk
!
! End of declarations
!
!****
!     PURPOSE - XRGSUB PROCESSES THE ****SBST CONTROL CARD IN
!               RIGID FORMAT DATA BASE
!
!     AUTHOR  - RPK CORPORATION; DECEMBER, 1983
!
!     INPUT
!       ARGUMENTS
!         SUBSET        SUBSET NUMBERS GIVEN BY THE USER
!       OTHER
!         /XRGDXX/
!           NSUBST      NUMBER OF SUBSET NUMBERS GIVEN BY USER
!           NUM         2 WORD ARRAY CONTAINING A RANGE OF NUMBERS
!                       FROM THE LIST OF NUMBERS ON THE ****SBST
!                       CONTROL CARD
!         NUMENT        NUMBER OF WORDS PER ENTRY IN THE MODULE
!                       EXECUTION DECISION TABLE
!
!     OUTPUT
!       ARGUMENTS
!         IRESTB        MODULE EXECUTION DECISION TABLE ENTRY FOR
!                       CURRENT DMAP STATEMENT
!       OTHER
!         /XRGDXX/
!           ICOL      COLUMN NUMBER BEING PROCESSED ON THE CARD
!           IERROR    ERROR FLAG - NON-ZERO IF AN ERROR OCCURRED
!           IGNORE    IGNORE FLAG SET TO NON-ZERO IF THE DMAP
!                     STATEMENT IS TO BE DELETED BY THE SUBSET
!           LIMIT     LOWER/UPPER LIMITS OF VALUES WITHIN AN
!                     ENTRY ON THE CARD
!
!     LOCAL VARIABLES
!       IEND          VALUE OF NUM(2)
!       ISTR          VALUE OF NUM(1)
!
!     FUNCTIONS
!        XRGSUB CALLS XRGDEV TO EXTRAPOLATE THE NUMBER FROM THE
!        THE CARD AND THEN IT COMPARES THE NUMBER(S) WITH THOSE
!        SUPPLIED BY THE USER AS SUBSETS.  IF A MATCH IS FOUND,
!        IGNORE IS SET AND THE MODULE EXECUTION DECISION TABLE
!        ENTRY IS SET TO ZERO.  CHECKS CONTINUE UNTIL ALL VALUES
!        GIVEN ON ****SBST CARD HAD BEEN CHECK OR UNTIL A MATCH
!        IS FOUND
!
!     SUBROUTINES CALLED - XDCODE,XRGDEV
!
!     CALLING SUBROUTINES - XRGDFM
!
!     ERRORS - NONE
!
!****
!
   Icol = 9
   Ierror = 0
   CALL xdcode
   Limit(1) = 1
   Limit(2) = 12
   DO
      CALL xrgdev
      IF ( Ierror/=0 .OR. Icol>80 ) GOTO 99999
      istr = Num(1)
      iend = Num(2)
      DO k = istr , iend
         DO kk = 1 , Nsubst
            IF ( k==Subset(kk) ) GOTO 100
         ENDDO
      ENDDO
      Icol = Icol + 1
   ENDDO
 100  DO k = 1 , Nument
      Irestb(k) = 0
   ENDDO
   Ignore = 1
99999 END SUBROUTINE xrgsub
