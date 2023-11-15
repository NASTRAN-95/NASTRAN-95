
FUNCTION kompnt(Ig,Ic,Ideg,Iw,Icc,Jg)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Mm , Nn
   COMMON /bands / Nn , Mm
!
! Dummy argument declarations
!
   INTEGER Ic(1) , Icc(1) , Ideg(1) , Ig(1) , Iw(1) , Jg(1)
   INTEGER kompnt
!
! Local variable declarations
!
   INTEGER i , ia , ii , is , ki , ko , n , nc
!
! End of declarations
!
!
!     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!
!     THIS FUNCTION HAS AS ITS VALUE THE NUMBER OF COMPONENTS STORED
!     IN THE CONNECTION ARRAY IG.
!     ALSO, IC AND ICC ARE SET UP.
!     IC(I) =COMPONENT INDEX FOR NODE I
!     ICC(I)=THE STARTING POSITION TO BE USED FOR LABELS IN COMPONENT I
!     THUS, ICC(I+1)-ICC(I)= THE NUMBER OF NODES IN COMPONENT I
!
!     INTEGER          BUNPK
!
   DO i = 1 , Nn
      Icc(i) = 0
      Ic(i) = 0
   ENDDO
   nc = 0
   Icc(1) = 1
 100  DO i = 1 , Nn
      IF ( Ic(i)==0 ) GOTO 200
      kompnt = nc
   ENDDO
   GOTO 99999
 200  nc = nc + 1
   ki = 0
   ko = 1
   Iw(1) = i
   Ic(i) = nc
   IF ( nc>=1 ) THEN
      is = Icc(nc) + 1
      Icc(nc+1) = is
   ENDIF
   DO
      ki = ki + 1
      ii = Iw(ki)
      n = Ideg(ii)
      IF ( n==0 ) GOTO 100
      CALL bunpak(Ig,ii,n,Jg)
      DO i = 1 , n
         ia = Jg(i)
         IF ( Ic(ia)==0 ) THEN
            Ic(ia) = nc
            ko = ko + 1
            Iw(ko) = ia
            is = Icc(nc+1) + 1
            Icc(nc+1) = is
         ENDIF
      ENDDO
      IF ( ko<=ki ) GOTO 100
   ENDDO
99999 END FUNCTION kompnt
