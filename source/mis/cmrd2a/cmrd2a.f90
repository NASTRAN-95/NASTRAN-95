!*==cmrd2a.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmrd2a
   IMPLICIT NONE
   USE C_BITPOS
   USE C_BLANK
   USE C_PATX
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: imsg , itest , kaa , kbb , kbi , kib , kii , uprt , usetmr
   INTEGER , SAVE :: item
   INTEGER , DIMENSION(2) , SAVE :: modnam
   EXTERNAL calcv , gmprtn , mtrxo , smsg
!
! End of declarations rewritten by SPAG
!
!
!     THIS SUBROUTINE PARTITIONS THE STIFFNESS MATRIX INTO BOUNDARY AND
!     INTERIOR POINTS AND THEN SAVES THE PARTITIONING VECTOR ON THE SOF
!     AS THE UPRT ITEM FOR THE CMRED2 MODULE.
!
!     INPUT DATA
!     GINO - USETMR - USET TABLE FOR REDUCED SUBSTRUCTURE
!            KAA    - SUBSTRUCTURE STIFFNESS MATRIX
!
!     OUTPUT DATA
!     GINO - KBB  - KBB PARTITION MATRIX
!            KIB  - KIB PARTITION MATRIX
!            KII  - KII PARTITION MATRIX
!     SOF  - UPRT - PARTITION VECTOR FOR ORIGINAL SUBSTRUCTURE
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
!              KBI    - KBI OUTPUT FILE NUMBER
!              KII    - KII OUTPUT FILE NUMBER
!              UPRT   - KAA PARTITION VECTOR FILE NUMBER
!
   !>>>>EQUIVALENCE (Usetmr,Infile(6)) , (Kaa,Infile(7)) , (Kbb,Iscr(1)) , (Kib,Iscr(2)) , (Kii,Iscr(4)) , (Kbi,Iscr(3)) , (Uprt,Iscr(5))
   DATA modnam/4HCMRD , 4H2A  /
   DATA item/4HUPRT/
!
!     SET UP PARTITIONING VECTOR
!
   IF ( Dry==-2 ) RETURN
   Lcore = Korlen
   Fuset = usetmr
   CALL calcv(uprt,Un,Ui,Ub,Z(Korbgn))
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
   CALL gmprtn(kaa,kii,kbi,kib,kbb,uprt,uprt,Nsub(1),Nsub(2),Z(Korbgn),Korlen)
!
!     SAVE PARTITIONING VECTOR
!
   CALL mtrxo(uprt,Oldnam,item,0,itest)
   IF ( itest==3 ) THEN
      RETURN
!
!     PROCESS MODULE FATAL ERRORS
!
   ELSEIF ( itest==4 ) THEN
      imsg = -2
   ELSEIF ( itest==5 ) THEN
      imsg = -3
   ELSEIF ( itest==6 ) THEN
!
      WRITE (Iprntr,99001) Ufm , modnam , item , Oldnam
99001 FORMAT (A23,' 6632, MODULE ',2A4,' - NASTRAN MATRIX FILE FOR ','I/O OF SOF ITEM ',A4,', SUBSTRUCTURE ',2A4,', IS PURGED.')
      Dry = -2
      RETURN
   ELSE
      WRITE (Iprntr,99002) Ufm , modnam , item , Oldnam
!
99002 FORMAT (A23,' 3211, MODULE ',2A4,8H - ITEM ,A4,' OF SUBSTRUCTURE ',2A4,' HAS ALREADY BEEN WRITTEN.')
      Dry = -2
      RETURN
   ENDIF
   CALL smsg(imsg,item,Oldnam)
!
END SUBROUTINE cmrd2a
