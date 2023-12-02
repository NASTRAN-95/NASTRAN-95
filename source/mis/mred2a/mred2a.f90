!*==mred2a.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mred2a
   IMPLICIT NONE
   USE C_BITPOS
   USE C_BLANK
   USE C_PATX
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: eqst , imsg , itest , kaa , kbb , kib , kii , uprt , usetmr
   INTEGER , SAVE :: item
   INTEGER , DIMENSION(7) :: itrlr
   INTEGER , DIMENSION(2) , SAVE :: modnam
   EXTERNAL calcv , gmprtn , mtrxi , mtrxo , rdtrl , smsg , smsg1
!
! End of declarations rewritten by SPAG
!
!
!     THIS SUBROUTINE PARTITIONS THE STIFFNESS MATRIX INTO BOUNDARY AND
!     INTERIOR POINTS AND THEN SAVES THE PARTITIONING VECTOR ON THE SOF
!     AS THE UPRT ITEM FOR THE MRED2 MODULE.
!
!     INPUT DATA
!     GINO - USETMR   - USET TABLE FOR REDUCED SUBSTRUCTURE
!            KAA      - SUBSTRUCTURE STIFFNESS MATRIX
!
!     OUTPUT DATA
!     GINO - KBB      - KBB PARTITION MATRIX
!            KIB      - KIB PARTITION MATRIX
!            KII      - KII PARTITION MATRIX
!     SOF  - UPRT     - PARTITION VECTOR FOR ORIGINAL SUBSTRUCTURE
!
!     PARAMETERS
!     INPUT  - GBUF   - GINO BUFFER
!              INFILE - INPUT FILE NUMBERS
!              ISCR   - SCRATCH FILE NUMBERS
!              KORLEN - LENGTH OF OPEN CORE
!              KORBGN - BEGINNING ADDRESS OF OPEN CORE
!              OLDNAM - NAME OF SUBSTRUCTURE BEING REDUCED
!     OTHERS - USETMR - USETMR INPUT FILE NUMBER
!              KAA    - KAA INPUT FILE NUMBER
!              KBB    - KBB OUTPUT FILE NUMBER
!              KIB    - KIB OUTPUT FILE NUMBER
!              KII    - KII OUTPUT FILE NUMBER
!              UPRT   - KAA PARTITION VECTOR FILE NUMBER
!
   !>>>>EQUIVALENCE (Eqst,Infile(4)) , (Usetmr,Infile(5)) , (Kaa,Infile(6)) , (Kbb,Iscr(1)) , (Kib,Iscr(2)) , (Kii,Iscr(3)) ,            &
!>>>>    & (Uprt,Iscr(5))
   DATA modnam/4HMRED , 4H2A  /
   DATA item/4HUPRT/
!
!     LOCATE PARTITIONING VECTOR
!
   IF ( Dry/=-2 ) THEN
      IF ( Bounds ) THEN
         CALL mtrxi(uprt,Oldnam,item,0,itest)
         IF ( itest/=1 ) GOTO 50
         itrlr(1) = eqst
         CALL rdtrl(itrlr)
         Nsub(1) = itrlr(6)
         Nsub(2) = itrlr(7)
      ELSE
         Lcore = Korlen
         Fuset = usetmr
         CALL calcv(uprt,Un,Ui,Ub,Z(Korbgn))
      ENDIF
!
!     PARTITION STIFFNESS MATRIX
!
!                  **         **
!                  *     .     *
!        **   **   * KBB . KBI *
!        *     *   *     .     *
!        * KAA * = *...........*
!        *     *   *     .     *
!        **   **   * KIB . KII *
!                  *     .     *
!                  **         **
!
      CALL gmprtn(kaa,kii,0,kib,kbb,uprt,uprt,Nsub(1),Nsub(2),Z(Korbgn),Korlen)
!
!     SAVE PARTITIONING VECTOR
!
      IF ( Bounds ) RETURN
      CALL mtrxo(uprt,Oldnam,item,0,itest)
      IF ( itest==3 ) RETURN
!
!     PROCESS MODULE FATAL ERRORS
!
 50   IF ( itest==2 ) THEN
         imsg = -11
      ELSEIF ( itest==3 ) THEN
         imsg = -1
         CALL smsg(imsg,item,Oldnam)
         RETURN
      ELSEIF ( itest==4 ) THEN
         imsg = -2
         CALL smsg(imsg,item,Oldnam)
         RETURN
      ELSEIF ( itest==5 ) THEN
         imsg = -3
         CALL smsg(imsg,item,Oldnam)
         RETURN
      ELSEIF ( itest==6 ) THEN
         imsg = -10
      ELSE
         imsg = -9
      ENDIF
      Dry = -2
      CALL smsg1(imsg,item,Oldnam,modnam)
   ENDIF
!
END SUBROUTINE mred2a
