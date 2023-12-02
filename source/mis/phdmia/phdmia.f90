!*==phdmia.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE phdmia
   USE c_mahcin
   USE c_phdmix
   USE c_system
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: c1 , c1a , c2 , c3 , h1 , h1a , h2 , h3 , iz , kdmt8 , kdmtb , kdmti , kdmtr0 , kdmtr1 , kdmtr2 , kfmt8 ,      &
                   & kfmtb , kfmti , kfmtr1 , kfmtr2
   REAL , SAVE :: dmi , dmis , p , s
   REAL :: dmips , ps
   LOGICAL :: first
   INTEGER , DIMENSION(8) :: fmt , iqx
   INTEGER :: i , i1 , icard , iq , irow , k , kp , l , n , n1 , nkp , nout , ret , ret1
   INTEGER , DIMENSION(22) , SAVE :: kfmt
   INTEGER , DIMENSION(23) , SAVE :: lfmt
   REAL , DIMENSION(8) :: qx
   INTEGER :: spag_nextblock_1
!
! End of declarations rewritten by SPAG
!
!
!     PUNCH SINGLE- OR DOUBLE-FIELD DMI CARDS FOR REAL, SINGLE-
!     PRECISION MATRICES.
!
!  $MIXED_FORMAT
!
   !>>>>EQUIVALENCE (qx(1),iqx(1)) , (lfmt(2),kfmt(1))
   DATA dmi , iz , p , dmis , s/3HDMI , 0 , 1H+ , 4HDMI* , 1H*/
   DATA kfmt/3*4H$$$$ , 16*4H**** , 4HA1,A , 4H2,I5 , 4H)   /
   DATA kfmti , kfmtr1 , kfmtr2/4HI8 , , 4HF8.1 , 4H,   /
   DATA kdmti , kdmtr1 , kdmtr2/4HI16, , 4H1PE1 , 4H6.8,/
   DATA kfmtb , kfmt8 , kdmtr0/4H     , 4H4X,  , 4H  E1/
   DATA kdmtb , kdmt8/4H     , 4H8X, /
   DATA h1 , h2 , h3 , c1 , c2 , c3 , h1a , c1a/4H(A4, , 4H4X,  , 4H2A4, , 4H(A1, , 4HA2,  , 4HI5,  , 4H A4, , 4H A1,/
   DATA lfmt(1)/4H(1X,/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     IBM/AIX (MACH=8) DOES NOT LIKE THE NON-ANSI STANDARD FORMAT
!     1PE16.8 (THE STANDARD IS 1P,E16.8).
!
         IF ( mach==8 ) kdmtr1 = kdmtr0
!
!     CALLED INITIALLY FOR EACH MATRIX.
!
!     SET PUNCH UNIT TO 7 FOR IBM AND CDC AND TO 1 FOR UNIVAC
!
         erno = 0
         nout = noutpt
         kp = kpp
         nkp = 8/kp
         icard = -1
         icard1 = 0
         IF ( kp==2 ) THEN
            dmips = dmis
            ps = s
            DO i = 12 , 19
               kfmt(i) = kdmtb
            ENDDO
         ELSE
            dmips = dmi
            ps = p
         ENDIF
         WRITE (np,99001) dmi , name , iz , ifo , itin , itout , ir , ic , p , nam , icard1
99001    FORMAT (A3,5X,2A4,4I8,8X,2I8,A1,A2,I5)
         IF ( nout>0 ) THEN
            WRITE (nout,99002) dmi , name , iz , ifo , itin , itout , ir , ic , p , nam , icard1
99002       FORMAT (1H1,/1X,A3,5X,2A4,4I8,8X,2I8,A1,A2,I5)
            l = 1
         ENDIF
         RETURN
!
!
         ENTRY phdmib
!     ============
!
!     CALLED FOR FIRST NON-ZERO ELEMENT OF EACH COLUMN.
!
         iq = 0
         irow = iro
         first = .TRUE.
         iq = iq + 1
         fmt(iq) = 0
         iqx(iq) = 0
         iq = iq + 1
         fmt(iq) = 1
         iqx(iq) = icol
         iq = iq + 1
         fmt(iq) = 1
         iqx(iq) = irow
         iq = iq + 1
         fmt(iq) = 2
         qx(iq) = x
         RETURN
!
!
         ENTRY phdmic
!     ============
!
!     CALLED FOR EACH NON-ZERO ELEMENT OF COLUMN EXCEPT FIRST ONE.
!
!     LOOK FOR FULL CARD
!
         IF ( iq>=nkp ) THEN
            ASSIGN 20 TO ret
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     DETERMINE IF NEW ENTRY IS CONSECUTIVE OR NON-CONSECUTIVE.
!
 20      IF ( iro/=irow+1 ) THEN
!
            iq = iq + 1
            irow = iro
            fmt(iq) = 1
            iqx(iq) = iro
            IF ( iq>=nkp ) THEN
               ASSIGN 40 TO ret
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
            irow = iro
            iq = iq + 1
            fmt(iq) = 2
            qx(iq) = x
            RETURN
         ENDIF
 40      iq = iq + 1
         fmt(iq) = 2
         qx(iq) = x
         RETURN
!
!
         ENTRY phdmid
!     ============
!
!     ENTRY POINT FOR COLUMN TERMINATION CALL
!
         IF ( iq<=0 ) RETURN
         ASSIGN 60 TO ret
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 60      RETURN
      CASE (2)
!
!     PUNCH CARD
!
         n = iq
         ASSIGN 80 TO ret1
!
!     BUILD FORMAT FOR CARD IMAGE.
!
         icard = icard + 1
         icard1 = icard + 1
         IF ( icard1>99999 ) THEN
!
!
!     ERROR MESSAGES
!
            erno = 1
            RETURN
         ELSE
            IF ( kp==2 ) THEN
               IF ( first ) THEN
                  i1 = 2
                  kfmt(1) = h1
                  kfmt(2) = h2
                  kfmt(3) = h3
                  kfmt(4) = kdmt8
                  kfmt(5) = kdmtb
               ELSE
                  i1 = 1
                  kfmt(1) = c1
                  kfmt(2) = c2
                  kfmt(3) = c3
               ENDIF
               DO i = i1 , n
                  k = fmt(i)
                  IF ( k==2 ) THEN
                     kfmt(2*i+2) = kdmtr1
                     kfmt(2*i+3) = kdmtr2
                  ELSE
                     kfmt(2*i+2) = kdmti
                     kfmt(2*i+3) = kdmtb
                  ENDIF
               ENDDO
               IF ( n<nkp ) THEN
                  n1 = n + 1
                  DO i = n1 , nkp
                     kfmt(2*i+2) = kdmt8
                     kfmt(2*i+3) = kdmt8
                  ENDDO
               ENDIF
            ELSE
               IF ( first ) THEN
                  i1 = 2
                  kfmt(1) = h1
                  kfmt(2) = h2
                  kfmt(3) = h3
                  kfmt(4) = kfmtb
                  kfmt(5) = kfmtb
               ELSE
                  i1 = 1
                  kfmt(1) = c1
                  kfmt(2) = c2
                  kfmt(3) = c3
               ENDIF
               DO i = i1 , n
                  k = fmt(i)
                  IF ( k==2 ) THEN
                     kfmt(2*i+2) = kfmtr1
                     kfmt(2*i+3) = kfmtr2
                  ELSE
                     kfmt(2*i+2) = kfmti
                     kfmt(2*i+3) = kfmtb
                  ENDIF
               ENDDO
               IF ( n<nkp ) THEN
                  n1 = n + 1
                  DO i = n1 , nkp
                     kfmt(2*i+2) = kfmt8
                     kfmt(2*i+3) = kfmt8
                  ENDDO
               ENDIF
            ENDIF
            GOTO ret1
         ENDIF
 80      IF ( first ) THEN
            WRITE (np,kfmt,ERR=140) dmips , name , (qx(l),l=2,iq) , ps , nam , icard1
            GOTO 140
         ELSE
            WRITE (np,kfmt,ERR=100) ps , nam , icard , (qx(l),l=1,iq) , ps , nam , icard1
         ENDIF
 100     lfmt(2) = c1a
         IF ( nout<=0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( l>=nlpp ) THEN
            WRITE (nout,99003)
            l = 0
         ENDIF
         WRITE (nout,lfmt,ERR=120) ps , nam , icard , (qx(l),l=1,iq) , ps , nam , icard1
 120     l = l + 1
         spag_nextblock_1 = 3
      CASE (3)
         iq = 0
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 140     lfmt(2) = h1a
         IF ( nout<=0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( l>=nlpp ) THEN
            WRITE (nout,99003)
            l = 0
         ENDIF
         WRITE (nout,lfmt,ERR=160) dmips , name , (qx(l),l=2,iq) , ps , nam , icard1
 160     l = l + 1
         spag_nextblock_1 = 4
      CASE (4)
         first = .FALSE.
         iq = 0
         spag_nextblock_1 = 5
      CASE (5)
         GOTO ret
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99003 FORMAT (1H1)
!
!
!
END SUBROUTINE phdmia
