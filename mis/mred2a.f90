
SUBROUTINE mred2a
   IMPLICIT NONE
!
! COMMON variable declarations
!
   LOGICAL Bounds
   INTEGER Dry , Eqst , Fuset , Gbuf1 , Idum1 , Idum2(5) , Idum3(6) , Idum4(9) , Idum5(10) , Idum6 , Idum7(6) , Idum8 , Idum9 ,     &
         & Infile(12) , Iprntr , Iscr(10) , Kaa , Kbb , Kib , Kii , Korbgn , Korlen , Lcore , Nsub(3) , Oldnam(2) , Ub , Ui , Un ,  &
         & Uprt , Usetmr , Usrmod , Z(1)
   COMMON /bitpos/ Idum4 , Un , Idum5 , Ub , Ui
   COMMON /blank / Idum1 , Dry , Idum6 , Gbuf1 , Idum2 , Infile , Idum3 , Iscr , Korlen , Korbgn , Oldnam , Idum7 , Usrmod , Idum9 ,&
                 & Bounds
   COMMON /patx  / Lcore , Nsub , Fuset
   COMMON /system/ Idum8 , Iprntr
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER imsg , item , itest , itrlr(7) , modnam(2)
!
! End of declarations
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
   EQUIVALENCE (Eqst,Infile(4)) , (Usetmr,Infile(5)) , (Kaa,Infile(6)) , (Kbb,Iscr(1)) , (Kib,Iscr(2)) , (Kii,Iscr(3)) ,            &
    & (Uprt,Iscr(5))
   DATA modnam/4HMRED , 4H2A  /
   DATA item/4HUPRT/
!
!     LOCATE PARTITIONING VECTOR
!
   IF ( Dry==-2 ) GOTO 99999
   IF ( Bounds ) THEN
      CALL mtrxi(Uprt,Oldnam,item,0,itest)
      IF ( itest/=1 ) GOTO 100
      itrlr(1) = Eqst
      CALL rdtrl(itrlr)
      Nsub(1) = itrlr(6)
      Nsub(2) = itrlr(7)
   ELSE
      Lcore = Korlen
      Fuset = Usetmr
      CALL calcv(Uprt,Un,Ui,Ub,Z(Korbgn))
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
   CALL gmprtn(Kaa,Kii,0,Kib,Kbb,Uprt,Uprt,Nsub(1),Nsub(2),Z(Korbgn),Korlen)
!
!     SAVE PARTITIONING VECTOR
!
   IF ( Bounds ) GOTO 99999
   CALL mtrxo(Uprt,Oldnam,item,0,itest)
   IF ( itest==3 ) GOTO 99999
!
!     PROCESS MODULE FATAL ERRORS
!
 100  IF ( itest==2 ) THEN
      imsg = -11
   ELSEIF ( itest==3 ) THEN
      imsg = -1
      CALL smsg(imsg,item,Oldnam)
      GOTO 99999
   ELSEIF ( itest==4 ) THEN
      imsg = -2
      CALL smsg(imsg,item,Oldnam)
      GOTO 99999
   ELSEIF ( itest==5 ) THEN
      imsg = -3
      CALL smsg(imsg,item,Oldnam)
      GOTO 99999
   ELSEIF ( itest==6 ) THEN
      imsg = -10
   ELSE
      imsg = -9
   ENDIF
   Dry = -2
   CALL smsg1(imsg,item,Oldnam,modnam)
!
99999 END SUBROUTINE mred2a
