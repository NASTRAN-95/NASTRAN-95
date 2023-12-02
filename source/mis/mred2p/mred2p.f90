!*==mred2p.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mred2p(Nus,Nuf,N2)
   IMPLICIT NONE
   USE C_BLANK
   USE C_PACKX
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nus
   INTEGER :: Nuf
   INTEGER :: N2
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: hab , i , iform , imsg , itest , j , kolmns
   INTEGER , SAVE :: item
   INTEGER , DIMENSION(7) :: itrlr1
   INTEGER , DIMENSION(2) , SAVE :: modnam
   REAL , DIMENSION(1) :: rz
   EXTERNAL close , gopen , makmcb , mtrxo , pack , smsg , smsg1 , wrttrl
!
! End of declarations rewritten by SPAG
!
!
!     THIS SUBROUTINE OUTPUTS THE HAB MATRIX TO THE SOF AS THE HORG ITEM
!     FOR THE MRED2 MODULE.
!
   !>>>>EQUIVALENCE (Hab,Iscr(2)) , (Rz(1),Z(1))
   DATA modnam/4HMRED , 4H2P  /
   DATA item/4HHORG/
!
!     FORM HAB MATRIX
!
!        **   **   **     **
!        *     *   *   .   *
!        * HAB * = * I . 0 *
!        *     *   *   .   *
!        **   **   **     **
!
   IF ( Dry/=-2 ) THEN
      kolmns = Nus + Nuf + N2
      IF ( N2==0 ) kolmns = kolmns + (Nmodes-Nuf)
      Typin = 1
      Typout = 1
      Irow = 1
      Nrow = Nus + Nuf
      Incr = 1
      iform = 2
      CALL makmcb(itrlr1,hab,Nrow,iform,Typin)
      CALL gopen(hab,Z(Gbuf1),1)
      DO i = 1 , kolmns
         DO j = 1 , Nrow
            rz(Korbgn+j-1) = 0.0
            IF ( i<=Nus+Nuf ) THEN
               IF ( j==i ) rz(Korbgn+j-1) = 1.0
            ENDIF
         ENDDO
         CALL pack(Z(Korbgn),hab,itrlr1)
      ENDDO
      CALL close(hab,1)
      CALL wrttrl(itrlr1)
!
!     STORE HAB MATRIX AS HORG ON SOF
!
      CALL mtrxo(hab,Oldnam,item,0,itest)
      IF ( itest/=3 ) THEN
!
!     PROCESS MODULE FATAL ERRORS
!
         IF ( itest==2 ) THEN
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
   ENDIF
END SUBROUTINE mred2p
