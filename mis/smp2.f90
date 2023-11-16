
SUBROUTINE smp2
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Icom
   COMMON /blank / Icom
!
! Local variable declarations
!
   INTEGER go , kdaa , kdaab , kdao , kdff , kdoo , mcb(7) , scr1 , scr2 , ua , uf , uo , uset
   REAL o
!
! End of declarations
!
!*****
! THIS MODULE, WHICH IS CALLED ONLY FOR DIFFERENTIAL STIFFNESS, PARTI-
! TIONS KDFF AND THEN COMPUTES KDAA AS FOLLOWS ....
!                       I
!               -D      I     D
!               K       I    K
!   D            AA     I     AO
!  K   =     ----------------------
!   FF                  I
!                D  T   I     D
!              (K  )    I    K
!                AO     I     OO
!                       I
!
!   D     -D         D                 D         T             D
!  K   =  K     +   K    X  G    +   (K    X  G )    +   G  X K    X  G
!   AA     AA        AO      O         AO      O          O    OO      O
!
!*****
!
! DMAP CALL ...
!
!     SMP2     USET,GO,KDFF/KDAA/
!
!
!
!
!
! INPUT FILES
!
   DATA uset , go , kdff/101 , 102 , 103/
!
! OUTPUT FILE
!
   DATA kdaa/201/
!
! SCRATCH FILES
!
   DATA scr1 , scr2 , kdaab , kdao , kdoo/301 , 302 , 303 , 304 , 305/
!
! USET BIT POSITIONS
!
   DATA uf , ua , uo/26 , 25 , 30/
!
!  TEST FOR PRESENCE OF KDFF
!
   mcb(1) = kdff
   CALL rdtrl(mcb)
   IF ( mcb(1)<0 ) RETURN
!
! PARTITION KFF
!
   CALL upart(uset,scr1,uf,ua,uo)
   CALL mpart(kdff,kdaab,kdao,o,kdoo)
!
! COMPUTE KDAA
!
   CALL elim(kdaab,kdao,kdoo,go,kdaa,scr1,scr2,306)
END SUBROUTINE smp2