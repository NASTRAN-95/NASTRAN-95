
FUNCTION idist(Ns,Ml,Maxlev,Ig,Ic,Ideg,Idis,Iw,Icc,Jg)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Nn
   COMMON /bands / Nn
!
! Dummy argument declarations
!
   INTEGER Maxlev , Ml , Ns
   INTEGER Ic(1) , Icc(1) , Ideg(1) , Idis(1) , Ig(1) , Iw(1) , Jg(1)
   INTEGER idist
!
! Local variable declarations
!
   INTEGER i , ia , icn , ii , k , ki , ko , l , ll , n , nnc
!
! End of declarations
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
 100  ki = ki + 1
   IF ( ki==ll ) THEN
      l = l + 1
      ll = ko + 1
      k = ko - ki + 1
      IF ( k>Ml ) THEN
         Ml = k
         IF ( Ml>Maxlev ) THEN
            idist = 1
            GOTO 99999
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
      IF ( ko<nnc ) GOTO 100
   ELSE
!
      l = 0
   ENDIF
   idist = l
   Idis(Ns) = 0
   k = ko - ll + 1
   IF ( k>Ml ) Ml = k
   RETURN
99999 RETURN
END FUNCTION idist
