!*==rsort.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rsort(Nwds,Keywx,L,Nx)
   IMPLICIT NONE
   USE C_SYSTEM
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nwds
   INTEGER :: Keywx
   REAL , DIMENSION(1) :: L
   INTEGER :: Nx
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL :: bck , mag
   INTEGER :: i , ia , ii , iia , iii , j , jj , keywd , nnn
   INTEGER , DIMENSION(2) , SAVE :: nam
   REAL , DIMENSION(50) :: temp
   EXTERNAL mesage
!
! End of declarations rewritten by SPAG
!
!
!     RSORT SORTS REAL NUMBERS IN L(NWDS,NCOL)
!           WHERE NCOL = IABS(NX)/NWDS
!
!     IF KEYWX .LT. 0 SORT BY ABSOLUTE VALUE
!     IF NX    .LT. 0 SORT IN DECREASING SEQUENCE
!
!     COMMENTS FROM G.C./UNISYS
!     THIS ROUTINE IS INEFFICIENT FOR LARGE ARRAY OF L
!
   DATA nam/4HRSOR , 4HT   /
!
   IF ( Nwds>50 ) THEN
      WRITE (Nout,99001)
99001 FORMAT (' *** ARRAY TEMP OF 50 EXCEEDED')
      CALL mesage(-37,0,nam)
   ENDIF
!
   mag = .FALSE.
   bck = .FALSE.
   IF ( Keywx<0 ) mag = .TRUE.
   IF ( Nx<0 ) bck = .TRUE.
   keywd = iabs(Keywx)
   nnn = iabs(Nx)
   iii = Nwds + keywd
   ia = Nwds - keywd
   IF ( nnn-Nwds>=Nwds ) THEN
      DO i = iii , nnn , Nwds
         jj = i - Nwds
         IF ( bck ) THEN
            IF ( mag ) THEN
               IF ( abs(L(jj))>=abs(L(i)) ) CYCLE
            ELSEIF ( L(jj)>=L(i) ) THEN
               CYCLE
            ENDIF
         ELSEIF ( mag ) THEN
            IF ( abs(L(i))>=abs(L(jj)) ) CYCLE
         ELSEIF ( L(i)>=L(jj) ) THEN
            CYCLE
         ENDIF
         SPAG_Loop_2_1: DO
            jj = jj - Nwds
            IF ( jj<=0 ) THEN
               jj = Nwds
            ELSE
               IF ( bck ) THEN
                  IF ( mag ) THEN
                     IF ( abs(L(jj))<abs(L(i)) ) CYCLE
                  ELSEIF ( L(jj)<L(i) ) THEN
                     CYCLE
                  ENDIF
               ELSEIF ( mag ) THEN
                  IF ( abs(L(i))<abs(L(jj)) ) CYCLE
               ELSEIF ( L(i)<L(jj) ) THEN
                  CYCLE
               ENDIF
               jj = jj + ia + Nwds
            ENDIF
            ii = i - keywd
            DO j = 1 , Nwds
               ii = ii + 1
               temp(j) = L(ii)
            ENDDO
            EXIT SPAG_Loop_2_1
         ENDDO SPAG_Loop_2_1
         SPAG_Loop_2_2: DO
            iia = ii - Nwds
            L(ii) = L(iia)
            ii = ii - 1
            IF ( ii<=jj ) THEN
               ii = ii - Nwds
               DO j = 1 , Nwds
                  ii = ii + 1
                  L(ii) = temp(j)
               ENDDO
               EXIT SPAG_Loop_2_2
            ENDIF
         ENDDO SPAG_Loop_2_2
      ENDDO
   ENDIF
!
END SUBROUTINE rsort
