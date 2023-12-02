!*==idist.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION idist(Ns,Ml,Maxlev,Ig,Ic,Ideg,Idis,Iw,Icc,Jg)
   IMPLICIT NONE
   USE C_BANDS
!
! Function and Dummy argument declarations rewritten by SPAG
!
   INTEGER :: idist
   INTEGER :: Ns
   INTEGER :: Ml
   INTEGER :: Maxlev
   INTEGER , DIMENSION(1) :: Ig
   INTEGER , DIMENSION(1) :: Ic
   INTEGER , DIMENSION(1) :: Ideg
   INTEGER , DIMENSION(1) :: Idis
   INTEGER , DIMENSION(1) :: Iw
   INTEGER , DIMENSION(1) :: Icc
   INTEGER , DIMENSION(1) :: Jg
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ia , icn , ii , k , ki , ko , l , ll , n , nnc
   EXTERNAL bunpak
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!
!     THIS FUNCTION HAS AS ITS VALUE THE MAXIMUM DISTANCE OF ANY NODE
!     IN COMPONENT IC(NS) FROM THE NODE NS.
!     THE DISTANCE OF EACH NODE IN THIS COMPONENT IS STORED IN THE ARRAY
!     IDIS.
!     THE MAXIMUM NUMBER OF NODES AT THE SAME DISTANCE FROM NS IS
!     STORED IN ML.
!
!     INTEGER          BUNPK
!
   icn = Ic(Ns)
   nnc = Icc(icn+1) - Icc(icn)
   DO i = 1 , Nn
      IF ( Ic(i)==Ic(Ns) ) Idis(i) = 0
   ENDDO
   ll = 1
   l = 0
   ki = 0
   ko = 1
   Ml = 0
   Iw(1) = Ns
   Idis(Ns) = -1
   SPAG_Loop_1_1: DO
      ki = ki + 1
      IF ( ki==ll ) THEN
         l = l + 1
         ll = ko + 1
         k = ko - ki + 1
         IF ( k>Ml ) THEN
            Ml = k
            IF ( Ml>Maxlev ) THEN
               idist = 1
               RETURN
            ENDIF
         ENDIF
      ENDIF
      ii = Iw(ki)
      n = Ideg(ii)
      IF ( n/=0 ) THEN
         CALL bunpak(Ig,ii,n,Jg)
         DO i = 1 , n
            ia = Jg(i)
            IF ( Idis(ia)==0 ) THEN
               Idis(ia) = l
               ko = ko + 1
               Iw(ko) = ia
            ENDIF
         ENDDO
         IF ( ko<nnc ) CYCLE
      ELSE
!
         l = 0
      ENDIF
      EXIT SPAG_Loop_1_1
   ENDDO SPAG_Loop_1_1
   idist = l
   Idis(Ns) = 0
   k = ko - ll + 1
   IF ( k>Ml ) Ml = k
   RETURN
END FUNCTION idist
