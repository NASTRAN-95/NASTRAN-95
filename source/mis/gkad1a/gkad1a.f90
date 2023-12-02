!*==gkad1a.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gkad1a(Usetd,Go,God,Scr1,Ue,Ua,Ud)
   USE C_PARMEG
   USE C_PATX
   USE C_SYSTEM
   USE C_ZZZZZZ
   IMPLICIT NONE
   INTEGER Core(1) , Ia(7) , Ia11(7) , Ia12(7) , Ib11(7) , Ib12(7) , Idum(54) , Iprec , Irule , Lc , N , N4 , No , Nz , Uset1
   COMMON /parmeg/ Ia , Ia11 , Ia12 , Ib11 , Ib12 , Nz , Irule
   COMMON /patx  / Lc , N , No , N4 , Uset1
   COMMON /system/ Idum , Iprec
   COMMON /zzzzzz/ Core
   INTEGER B1dd , Baa , Go , God , k1dd , K41dd , K4aa , kaa , M1dd , Maa , Scr1 , Usetd
   REAL Ua , Ud , Ue
   INTEGER i , ient , iout , ipv1(7)
   INTEGER korsz
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     GKAD1A WILL EXPAND GO BY NULL MATRIX TO MAKE GOD, AND
!     AA-S TO D-S ADDING ZEROS FOR E-S
!
!
!
         ient = 0
         spag_nextblock_1 = 2
      CASE (2)
!
!     COMPUTE CORE FOR CALCV AND MERGE
!
         Lc = korsz(Core)
!
!     BUILD PART VECTOR
!
         Uset1 = Usetd
         CALL calcv(Scr1,Ud,Ua,Ue,Core(1))
!
!     SET UP FOR MERGE
!
         Nz = Lc
         Irule = 0
         DO i = 1 , 7
            Ia11(i) = 0
            Ia(i) = 0
            Ia12(i) = 0
            Ib11(i) = 0
            Ib12(i) = 0
         ENDDO
         ipv1(1) = Scr1
         CALL rdtrl(ipv1)
         IF ( ient/=0 ) THEN
!
!     VECTOR MADE, SET UP MCB-S
!
            Ia(2) = N + No + N4
            Ia(3) = Ia(2)
            Ia(4) = 6
            Ia(5) = Iprec
            Ia11(1) = kaa
            Ia(1) = k1dd
            iout = 1
            CALL rdtrl(Ia11)
            IF ( Ia11(1)>0 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            k1dd = 0
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     SET UP FOR 2 WAY MERGE
!
            Ia11(1) = Go
            CALL rdtrl(Ia11)
            Ia(1) = God
            Ia(2) = N + No + N4
            Ia(3) = Ia11(3)
            Ia(4) = Ia11(4)
            Ia(5) = Ia11(5)
!     BUILD NULL COLUMN IN CORE
            i = 0
            Core(1) = 0
            Core(i+2) = 1
            Core(i+3) = Ia(3)
            Core(i+4) = 2
            Core(i+5) = 1
            Core(i+6) = 0
            Core(i+7) = 0
            CALL merge(ipv1(1),Core(1),Core(1))
            CALL wrttrl(Ia)
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         RETURN
!
!
         ENTRY gkad1b(Usetd,Maa,Baa,K4aa,M1dd,B1dd,K41dd,Ua,Ue,Ud,Scr1)
!     ================================================================
!
         ient = 1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
         CALL merge(ipv1(1),ipv1(1),Core(1))
         CALL wrttrl(Ia)
         spag_nextblock_1 = 5
      CASE (5)
         DO
            IF ( iout==2 ) THEN
               iout = 3
               Ia(1) = M1dd
               Ia11(1) = Maa
               CALL rdtrl(Ia11)
               IF ( Ia11(1)>0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               M1dd = 0
            ELSEIF ( iout==3 ) THEN
               iout = 4
               Ia(1) = K41dd
               Ia11(1) = K4aa
               CALL rdtrl(Ia11)
               IF ( Ia11(1)>0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               K41dd = 0
            ELSEIF ( iout==4 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ELSE
               iout = 2
               Ia(1) = B1dd
               Ia11(1) = Baa
               CALL rdtrl(Ia11)
               IF ( Ia11(1)>0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               B1dd = 0
            ENDIF
         ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE gkad1a
