
SUBROUTINE detm
   IMPLICIT NONE
   DOUBLE PRECISION Det1(4) , Detx(4) , P(4) , Ps1(4)
   REAL Epsi , Fact1 , Rmax , Rmin , Rminr , Sml1
   INTEGER Iadd , Ic , Idet , Iev(7) , Ifail , Iffnd , Ik(7) , Im(7) , Ipdet1(4) , Ipdeta , Ipdetx(4) , Ips , Ipsav , Is , Isng ,   &
         & Iterm , K , Lama , Lcore , Mz , N2ev , Nd , Ndcmp , Ne , Nev , Nevm , Nfail , Nfound , Nit , Npole , Nsmove , Nstart ,   &
         & Prec , Scr1 , Scr2 , Scr3 , Scr4 , Scr5 , Scr6 , Scr7
   COMMON /detmx / P , Detx , Ps1 , Det1 , N2ev , Ipsav , Ips , Idet , Ipdeta , Prec , Nstart , Ndcmp , Ic , Nsmove , Iterm , Is ,  &
                 & Nd , Iadd , Sml1 , Ipdetx , Ipdet1 , Ifail , K , Fact1 , Iffnd , Nfail , Npole , Isng
   COMMON /regean/ Im , Ik , Iev , Scr1 , Scr2 , Scr3 , Scr4 , Scr5 , Lcore , Rmax , Rmin , Mz , Nev , Epsi , Rminr , Ne , Nit ,    &
                 & Nevm , Scr6 , Scr7 , Nfound , Lama
   INTEGER idone , iscr7 , itime1 , itime2 , itleft , name(2)
   DATA name/4HDETE , 4HRM  /
!
!     RMAX = APPROXIMATE MAGNITUDE OF LARGEST EIGENVALUE OF INTEREST
!
!     RMIN = LOWEST  NON-ZERO  EIGENVALUE
!
!     MZ = NUMBER OF ZERO EIGENVALUES
!
!     NEV = NUMBER OF NON-ZERO EIGENVALUES IN RANGE OF INTEREST
!
!     EPSI = CONVERGENCE CRITERION
!
!     RMINR = LOWEST EIGENVALUE OF INTEREST
!
!     NE   =  NUMBER OF PERMISSIBLE CHANGES OF EPSI
!
!     NIT = INTERATIONS TO AN EIGENVALUE
!
!     NEVM = MAXIMUM NUMBER OF EIGENVALUES DESIRED
!
!     IS  = STARTING SET COUNTER
!
!     IC  = COUNTER FOR CHANGE OF CONVERGENCE CRITERIA
!
!     NFOUND  = THE NUMBER OF EIGENVALUES FOUND TO DATA
!
!      IM = MASS MATRIX CONTROL BLOCK
!
!      IK = K MATRIX CONTROL BLOCK
!
!        A = M +P*K
!
!     IEV = EIGENVECTOR CONTROL BLOCK
!
   Nstart = 0
   Lcore = 0
   Ndcmp = 0
   Nsmove = 0
   Npole = 0
   Iterm = 1
   Iffnd = 0
   Nfail = 0
!*****
   Prec = Ik(5)
!*****
   iscr7 = Scr7
   IF ( Mz>Nevm ) GOTO 400
   IF ( Im(1)<=0 ) THEN
!
!     MASS MATRIX PURGED -- ASSUME IDENTITY
!
      Im(1) = Ik(1)
      CALL rdtrl(Im(1))
      Im(4) = 8
   ENDIF
   CALL detm1(*600)
 100  DO
      CALL klock(itime1)
      CALL detm3(*300,*400,*200)
      Nfound = Nfound + 1
      CALL fdvect(Sml1,P(3))
      idone = Nfound + 1
      IF ( Mz>0 ) idone = idone + Mz
      CALL detm4
      IF ( idone>Nevm ) GOTO 500
      CALL klock(itime2)
      CALL tmtogo(itleft)
      IF ( 2*(itime2-itime1)>itleft ) EXIT
   ENDDO
!
!     INSUFFICIENT TIME TO FIND ANOTHER E. V.
!
 200  CALL mesage(45,Nevm-idone,name)
   Iterm = 3
   GOTO 500
 300  CALL detm2
   GOTO 100
 400  Iterm = 2
 500  Scr7 = iscr7
   CALL detm5
   RETURN
!
!     SINGULAR MATRIX EVERYWHERE
!
 600  Iterm = 4
   GOTO 500
END SUBROUTINE detm
