
SUBROUTINE rsort(Nwds,Keywx,L,Nx)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ibuf , Nout
   COMMON /system/ Ibuf , Nout
!
! Dummy argument declarations
!
   INTEGER Keywx , Nwds , Nx
   REAL L(1)
!
! Local variable declarations
!
   LOGICAL bck , mag
   INTEGER i , ia , ii , iia , iii , j , jj , keywd , nam(2) , nnn
   REAL temp(50)
!
! End of declarations
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
         DO
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
            EXIT
         ENDDO
         DO
            iia = ii - Nwds
            L(ii) = L(iia)
            ii = ii - 1
            IF ( ii<=jj ) THEN
               ii = ii - Nwds
               DO j = 1 , Nwds
                  ii = ii + 1
                  L(ii) = temp(j)
               ENDDO
               EXIT
            ENDIF
         ENDDO
      ENDDO
   ENDIF
!
END SUBROUTINE rsort
