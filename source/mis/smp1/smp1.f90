!*==smp1.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE smp1
   IMPLICIT NONE
   USE C_BITPOS
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(3) :: aa , ff
   INTEGER :: aab , i
   INTEGER , SAVE :: baa , bff , go , k4aa , k4ff , kaa , kff , koob , loo , maa , mff , oab , oob , scr1 , scr2 , scr3 , scr4 ,    &
                   & scr5 , scr6 , scr7 , uset
   INTEGER , DIMENSION(3) , SAVE :: ioab , ioob
   INTEGER , DIMENSION(7) :: mcb
   EXTERNAL elim , factor , mpart , rdtrl , solver , upart
!
! End of declarations rewritten by SPAG
!
!
!     SMP1 PARTITIONS KFF INTO KAAB,KOAB AND KOOB
!     GO IS SOLVED FROM THE EQUATION KOOB*GO = -KOAB
!     KAA IS THEN COMPUTED FROM THE EQUATION KAA = KAAB + KOAB(T)*GO
!     IF ANY OF THE MATRICES MFF, BFF OR K4FF IS PRESENT, THEN
!     PARTITIONS ARE MADE AND THE EQUATION
!     XAA = XAAB + GO(T)*XOAB + XOAB(T)*GO + GO(T)*XOOB*GO
!     IS EVALUATED WHERE X = M OR B OR K4
!
   !>>>>EQUIVALENCE (ff(3),mff) , (ff(2),bff) , (ff(1),k4ff) , (aa(3),maa) , (aa(2),baa) , (aa(1),k4aa) , (ioob(3),oob) , (ioab(3),oab) ,&
!>>>>    & (aab,scr5) , (ioob(1),scr6) , (ioab(1),scr7)
   DATA uset , kff , mff , bff , k4ff/101 , 102 , 103 , 104 , 105/
   DATA go , kaa , koob , loo , maa/201 , 202 , 203 , 204 , 205/
   DATA oob , oab , baa , k4aa/206 , 207 , 208 , 209/
   DATA scr1 , scr2 , scr3 , scr4 , scr5/301 , 302 , 303 , 304 , 305/
   DATA scr6 , scr7 , ioob(2) , ioab(2)/306 , 307 , 306 , 307/
!
!     MATRIX NAME EQUIVALENCES NOT REFERENCED
!
!     EQUIVALENCED    (AAB,   MAAB,   BAAB,   K4AAB,  SCR5)
!                     (OOB,   MOOB,   BOOB,   K4OOB,  SCR6)
!                     (OAB,   MOAB,   BOAB,   K4OAB,  SCR7)
!
!     PARTITION KFF INTO KAAB,KOAB, AND KOOB
!
   CALL upart(uset,scr1,Uf,Ua,Uo)
   CALL mpart(kff,aab,ioab,0,koob)
!
!     DECOMPOSE KOOB INTO LOO
!
   CALL factor(koob,loo,scr2,scr3,scr4,scr6)
!
!     SOLVE KOOB*GO = -KOAB FOR GO
!     THEN COMPUTE  KAA = KAAB + KOAB(T)*GO
!
   CALL solver(loo,go,ioab,aab,kaa,0,0,scr4)
!
   DO i = 1 , 3
!
!                     K4FF
!     TEST TO SEE IF   BFF  IS PRESENT
!                      MFF
!
      mcb(1) = ff(i)
      CALL rdtrl(mcb)
      IF ( mcb(1)>=0 ) THEN
!
!         K4FF                              K4AAB, K4OAB, K4OOB
!     IF   BFF IS PRESENT, PARTITION INTO    BAAB,  BOAB,  BOOB
!          MFF                               MAAB,  MOAB,  MOOB
!
!     THEN COMPUTE  K4AA, BAA, MAA  RESPECTIVELY
!
         CALL mpart(ff(i),aab,ioab(i),0,ioob(i))
         CALL elim(aab,ioab(i),ioob(i),go,aa(i),scr2,scr3,scr4)
      ENDIF
   ENDDO
!
END SUBROUTINE smp1
