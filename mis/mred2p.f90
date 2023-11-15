
SUBROUTINE mred2p(Nus,Nuf,N2)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Dry , Gbuf1 , Hab , Idum1 , Idum2 , Idum3(17) , Idum4(12) , Idum5 , Incr , Iprntr , Irow , Iscr(10) , Korbgn , Korlen ,  &
         & Nmodes , Nrow , Otfile(6) , Typin , Typout , Z(1)
   REAL Oldnam(2) , Rz(1)
   COMMON /blank / Idum1 , Dry , Idum2 , Gbuf1 , Idum3 , Otfile , Iscr , Korlen , Korbgn , Oldnam , Idum4 , Nmodes
   COMMON /packx / Typin , Typout , Irow , Nrow , Incr
   COMMON /system/ Idum5 , Iprntr
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER N2 , Nuf , Nus
!
! Local variable declarations
!
   INTEGER i , iform , imsg , item , itest , itrlr1(7) , j , kolmns , modnam(2)
!
! End of declarations
!
!
!     THIS SUBROUTINE OUTPUTS THE HAB MATRIX TO THE SOF AS THE HORG ITEM
!     FOR THE MRED2 MODULE.
!
   EQUIVALENCE (Hab,Iscr(2)) , (Rz(1),Z(1))
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
      CALL makmcb(itrlr1,Hab,Nrow,iform,Typin)
      CALL gopen(Hab,Z(Gbuf1),1)
      DO i = 1 , kolmns
         DO j = 1 , Nrow
            Rz(Korbgn+j-1) = 0.0
            IF ( i<=Nus+Nuf ) THEN
               IF ( j==i ) Rz(Korbgn+j-1) = 1.0
            ENDIF
         ENDDO
         CALL pack(Z(Korbgn),Hab,itrlr1)
      ENDDO
      CALL close(Hab,1)
      CALL wrttrl(itrlr1)
!
!     STORE HAB MATRIX AS HORG ON SOF
!
      CALL mtrxo(Hab,Oldnam,item,0,itest)
      IF ( itest/=3 ) THEN
!
!     PROCESS MODULE FATAL ERRORS
!
         IF ( itest==2 ) THEN
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
      ENDIF
   ENDIF
99999 END SUBROUTINE mred2p
