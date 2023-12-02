!*==mred2p.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mred2p(Nus,Nuf,N2)
   USE c_blank
   USE c_packx
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
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
   IF ( dry/=-2 ) THEN
      kolmns = Nus + Nuf + N2
      IF ( N2==0 ) kolmns = kolmns + (nmodes-Nuf)
      typin = 1
      typout = 1
      irow = 1
      nrow = Nus + Nuf
      incr = 1
      iform = 2
      CALL makmcb(itrlr1,hab,nrow,iform,typin)
      CALL gopen(hab,z(gbuf1),1)
      DO i = 1 , kolmns
         DO j = 1 , nrow
            rz(korbgn+j-1) = 0.0
            IF ( i<=Nus+Nuf ) THEN
               IF ( j==i ) rz(korbgn+j-1) = 1.0
            ENDIF
         ENDDO
         CALL pack(z(korbgn),hab,itrlr1)
      ENDDO
      CALL close(hab,1)
      CALL wrttrl(itrlr1)
!
!     STORE HAB MATRIX AS HORG ON SOF
!
      CALL mtrxo(hab,oldnam,item,0,itest)
      IF ( itest/=3 ) THEN
!
!     PROCESS MODULE FATAL ERRORS
!
         IF ( itest==2 ) THEN
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
   ENDIF
END SUBROUTINE mred2p
