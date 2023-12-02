!*==mred2a.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mred2a
   USE c_bitpos
   USE c_blank
   USE c_patx
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
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
   IF ( dry/=-2 ) THEN
      IF ( bounds ) THEN
         CALL mtrxi(uprt,oldnam,item,0,itest)
         IF ( itest/=1 ) GOTO 50
         itrlr(1) = eqst
         CALL rdtrl(itrlr)
         nsub(1) = itrlr(6)
         nsub(2) = itrlr(7)
      ELSE
         lcore = korlen
         fuset = usetmr
         CALL calcv(uprt,un,ui,ub,z(korbgn))
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
      CALL gmprtn(kaa,kii,0,kib,kbb,uprt,uprt,nsub(1),nsub(2),z(korbgn),korlen)
!
!     SAVE PARTITIONING VECTOR
!
      IF ( bounds ) RETURN
      CALL mtrxo(uprt,oldnam,item,0,itest)
      IF ( itest==3 ) RETURN
!
!     PROCESS MODULE FATAL ERRORS
!
 50   IF ( itest==2 ) THEN
         imsg = -11
      ELSEIF ( itest==3 ) THEN
         imsg = -1
         CALL smsg(imsg,item,oldnam)
         RETURN
      ELSEIF ( itest==4 ) THEN
         imsg = -2
         CALL smsg(imsg,item,oldnam)
         RETURN
      ELSEIF ( itest==5 ) THEN
         imsg = -3
         CALL smsg(imsg,item,oldnam)
         RETURN
      ELSEIF ( itest==6 ) THEN
         imsg = -10
      ELSE
         imsg = -9
      ENDIF
      dry = -2
      CALL smsg1(imsg,item,oldnam,modnam)
   ENDIF
!
END SUBROUTINE mred2a
