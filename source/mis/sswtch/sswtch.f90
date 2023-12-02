!*==sswtch.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sswtch(Nbit,L)
   USE c_sem
   USE c_system
   USE c_xlink
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nbit
   INTEGER :: L
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , iret , nbit2
   INTEGER , SAVE :: renter
   INTEGER :: spag_nextblock_1
!
! End of declarations rewritten by SPAG
!
!
!     PURPOSE OF THIS ROUTINE IS TO SET L = 1 IF SENSE SWITCH BIT IS
!     ON, OTHERWISE L = 0.
!
!     SENSE SWITCH DEFINITION
!      1 = DUMP CORE WHEN SUBROUTINE DUMP OR PDUMP(NO ARGUMENTS) IS
!          CALLED
!      2 = DUMP FIAT TABLE AFTER ALLOCATION
!      3 = DUMP DATA POOL DICTIONARY AFTER ALLOCATION
!      4 = DUMP OSCAR FILE AT END OF XGPI
!      5 = CONSOLE MESSAGE DESIRED (BEGIN)
!      6 = CONSOLE MESSAGE DESIRED (END)
!      7 = EIGENVALUE EXTRACTION DIAGNOSTICS
!          (DETERMINANT AND INVERSE POWER)
!      8 = TRACES NPTP ON 1108
!      9 = TURNS ON PRINTER PLOTTER FOR ANY XYPLOT REQUESTS
!     10 = USES ALTERNATE ALGORITHUM FOR NON LINEAR LOADS SEE SPR 153
!     11 = ACTIVE ROW AND COLUMN TIME PRINTS
!     12 = CONPLEX EIGENVALUE EXTRACTION DIAGNOSTICS
!          (INVERSE POWER)
!     28 = PUNCHES OUT LINK SPECIFICATION TABLE - DECK XBSBD
!     29 = PROCESS LINK SPECIFICATION UPDATE DECK
!     30 = PUNCHES OUT ALTERS TO XSEM-S FOR SWITCHES 1-15
!     31 = PRINT LINK SPECIFICATION TABLE
!
   DATA renter/4HREEN/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         L = 0
         IF ( iret==1 ) RETURN
         IF ( Nbit>31 ) THEN
!
            nbit2 = Nbit - 31
            IF ( andf(lshift(1,nbit2-1),switch(2))/=0 ) L = 1
            RETURN
         ELSE
            IF ( andf(lshift(1,Nbit-1),switch(1))/=0 ) L = 1
            RETURN
         ENDIF
!
!
         ENTRY pressw(Nbit,L)
!     =====================
!
!     PRESSW IS CALLED ONLY BY BGNSYS AND XCSA TO SETUP DIAGNOSTIC BITS
!     FOR A PARTICULAR LINK.
!     BITS  0 THRU 47 ARE USED FOR 48 DIAGNOSTICS
!     BITS 49 THRU 63 ARE RESERVED FOR 15 LINK NOS.
!     NBIT HERE (INPUT) CONTAINS BCD WORD NSXX WHERE XX IS LINK NO.
!
         iret = 0
         IF ( Nbit==renter ) RETURN
         IF ( switch(3)+switch(2)==0 ) iret = 1
         i = 32 - maxlnk
         IF ( rshift(switch(2),i)/=0 ) THEN
            DO i = 1 , maxlnk
               IF ( Nbit==ns(i) ) GOTO 10
            ENDDO
            i = 0
            IF ( Nbit==ns(16) ) i = 5
            IF ( Nbit==ns(17) ) i = 8
            IF ( Nbit==ns(18) ) i = 13
            IF ( Nbit==ns(19) ) i = 6
            IF ( Nbit==ns(20) ) i = 2
            IF ( Nbit==ns(21) ) i = 9
            IF ( Nbit==ns(22) ) i = 11
            IF ( Nbit==ns(23) ) i = 15
            IF ( i==0 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
 10         nbit2 = i + 31 - maxlnk
            IF ( andf(lshift(1,nbit2),switch(2))==0 ) iret = 1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         IF ( iret==0 ) switch(1) = switch(3)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE sswtch
