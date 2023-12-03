!*==relabl.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE relabl(Ns,Nodes,Ig,Ic,Ideg,Idis,Iw,New,Icc,Ild,Iaj,Jg,Idim)
   USE c_bands
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ns
   INTEGER , DIMENSION(1) :: Nodes
   INTEGER , DIMENSION(1) :: Ig
   INTEGER , DIMENSION(1) :: Ic
   INTEGER , DIMENSION(1) :: Ideg
   INTEGER , DIMENSION(1) :: Idis
   INTEGER , DIMENSION(1) :: Iw
   INTEGER , DIMENSION(1) :: New
   INTEGER , DIMENSION(1) :: Icc
   INTEGER , DIMENSION(1) :: Ild
   INTEGER , DIMENSION(1) :: Iaj
   INTEGER , DIMENSION(1) :: Jg
   INTEGER :: Idim
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ia , icn , ii , ij , iz , j , jj , jt , k , ki , ko , l , ll , m , n , n1 , n2 , ngrid , nnc , nt , x
   EXTERNAL bunpak
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!
!     GENERATE A RELABELING SCHEME STARTING WITH NS NODES FOR WHICH
!     LABELS HAVE BEEN STORED IN ARRAY NODES.
!     SET UP ILD AND NEW.
!     ILD(OLD) = NEW
!     NEW(NEW) = OLD, THE INVERSE OF ILD
!     IAJ IS DIMENSIONED TO IDIM
!
!
   i = Nodes(1)
   icn = Ic(i)
   nt = Icc(icn) - 1
   DO i = 1 , nn
      IF ( Ic(i)==icn ) Idis(i) = 0
   ENDDO
   DO j = 1 , Ns
      jj = Nodes(j)
      Idis(jj) = -1
      jt = j + nt
      New(jt) = jj
      Ild(jj) = jt
   ENDDO
   ki = nt
   ko = Ns + nt
   ll = ko
   l = 1
   j = ko
   nnc = Icc(icn+1) - 1
   SPAG_Loop_1_1: DO
      ki = ki + 1
      IF ( ki==ll ) THEN
         l = l + 1
         ll = ko + 1
      ENDIF
      ii = New(ki)
      n = Ideg(ii)
      IF ( n==0 ) EXIT SPAG_Loop_1_1
      ij = 0
      CALL bunpak(Ig,ii,n,Jg)
      DO i = 1 , n
         ia = Jg(i)
         IF ( Idis(ia)==0 ) THEN
            ij = ij + 1
            IF ( ij<=Idim ) THEN
!
               Idis(ia) = l
               ko = ko + 1
               Iaj(ij) = ia
               Iw(ij) = Ideg(ia)
            ELSE
!
!     DIMENSION EXCEEDED.  STOP JOB.
!
               ngrid = -2
               RETURN
            ENDIF
         ENDIF
      ENDDO
      IF ( ij<1 ) THEN
      ELSEIF ( ij==1 ) THEN
         j = ko
         iz = Iaj(1)
         New(ko) = iz
         Ild(iz) = ko
      ELSE
         SPAG_Loop_2_2: DO
            x = 0
            DO i = 2 , ij
               IF ( Iw(i)<Iw(i-1) ) THEN
                  x = Iw(i)
                  Iw(i) = Iw(i-1)
                  Iw(i-1) = x
                  x = Iaj(i)
                  Iaj(i) = Iaj(i-1)
                  Iaj(i-1) = x
               ENDIF
            ENDDO
            IF ( x<=0 ) THEN
               DO i = 1 , ij
                  j = j + 1
                  iz = Iaj(i)
                  New(j) = iz
                  Ild(iz) = j
               ENDDO
               EXIT SPAG_Loop_2_2
            ENDIF
         ENDDO SPAG_Loop_2_2
      ENDIF
      IF ( ko>=nnc ) EXIT SPAG_Loop_1_1
   ENDDO SPAG_Loop_1_1
!
!     REVERSE SEQUENCE FOR THIS COMPONENT (ICN).
!
!     ICC IS AN ARRAY USED FOR IDENTIFYING COMPONENTS IN THE NEW ARRAY.
!     ICC(N1) CONTAINS THE INDEX FOR THE NEW ARRAY AT WHICH COMPONENT
!         N1 STARTS.
!
   n1 = Icc(icn) - 1
   n2 = nn - Icc(icn+1) + 1
   IF ( n2>nn ) n2 = 0
!
!     REVERSE THE NODAL CM SEQUENCE, OMITTING THE FIRST N1 AND THE LAST
!     N2 POINTS.
!
!     NEW(N1) = OLD LABEL FOR NODE NOW LABELLED N1.
!     ILD(N1) = NEW LABEL FOR NODE ORIGINALLY LABELED N1.
!     N1      = NUMBER OF POINTS AT BEGINNING OF SEQUENCE TO OMIT FROM
!               REVERSAL.
!     N2      = NUMBER OF POINTS AT END OF SEQUENCE TO OMIT FROM
!               REVERSAL.
!     NN      = NUMBER OF NODES.
!     J       = NUMBER OF INTERCHANGES TO MAKE.
!
   j = (nn-n1-n2)/2
   IF ( j<=0 ) RETURN
   ll = nn - n2 + 1
!
!     MAKE INTERCHANGES IN NEW ARRAY.
!
   DO i = 1 , j
      l = ll - i
      k = New(l)
      m = n1 + i
      New(l) = New(m)
      New(m) = k
   ENDDO
!
!     CORRECT ILD, THE INVERSE OF NEW.
!
   l = 1 + n1
   m = nn - n2
   DO i = l , m
      k = New(i)
      Ild(k) = i
   ENDDO
!
END SUBROUTINE relabl