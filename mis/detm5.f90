
SUBROUTINE detm5
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Consts(5) , Core(5) , Epsi , Fact1 , P(32) , Prec , Rmax , Rmin , Rminr , Scr6 , Sml1 , Tphi
   DOUBLE PRECISION Det(1) , Ps(1) , Psave(1)
   INTEGER Iadd , Ic , Idet , Ifail , Iffnd , Im(26) , Ipdet(8) , Ipdet1(4) , Ipdeta , Ipdetx(4) , Ipout , Ips , Ipsav , Is ,       &
         & Iterm , K , Lama , Lcore , Mz , N2ev , Nd , Ndcmp , Ne , Nev , Nevm , Nfail , Nfound , Nit , Nsmove , Nstart , Sysbuf
   COMMON /condas/ Consts
   COMMON /detmx / P , N2ev , Ipsav , Ips , Idet , Ipdeta , Prec , Nstart , Ndcmp , Ic , Nsmove , Iterm , Is , Nd , Iadd , Sml1 ,   &
                 & Ipdetx , Ipdet1 , Ifail , K , Fact1 , Iffnd , Nfail
   COMMON /regean/ Im , Lcore , Rmax , Rmin , Mz , Nev , Epsi , Rminr , Ne , Nit , Nevm , Scr6 , Ipout , Nfound , Lama
   COMMON /system/ Sysbuf
   COMMON /zzzzzz/ Psave
!
! Local variable declarations
!
   INTEGER i , n2ev2 , nnd , nni , nnp , nz
   INTEGER korsz
!
! End of declarations
!
!
!     WRITES EIGENVALUE SUMMARY FOR DETERMINANT METHOD
!
!
!
   EQUIVALENCE (Psave(1),Ps(1),Det(1),Ipdet(1),Core(1))
   EQUIVALENCE (Consts(2),Tphi)
!
! ----------------------------------------------------------------------
!
   nz = korsz(Psave) - Lcore - Sysbuf
   CALL gopen(Ipout,Ipdet(nz+1),1)
   Ipdet(1) = 1
   Ipdet(2) = Nfound
   IF ( Mz>0 ) Ipdet(2) = Ipdet(2) + Mz
   Ipdet(3) = Nstart
   Ipdet(4) = Ic
   Ipdet(5) = Nsmove
   Ipdet(6) = Ndcmp
   Ipdet(7) = Nfail
   Ipdet(8) = Iterm
   DO i = 9 , 12
      Ipdet(i) = 0
   ENDDO
   CALL write(Ipout,Ipdet(1),12,0)
   IF ( Ndcmp/=0 ) THEN
      n2ev2 = Iadd + Nd
      DO i = 1 , n2ev2
         nnd = i + Idet
         nnp = i + Ips
         nni = i + Ipdeta
!
!     PUT UUT STRRTING POINT SUMMARY
!
         Ipdet(1) = i
         Core(2) = Psave(nnp)
         Core(3) = sqrt(abs(Core(2)))
         Core(4) = Core(3)/Tphi
         Core(5) = Psave(nnd)
         Ipdet(6) = Ipdet(nni)
!
!     SCALE DETERMINANTE FOR PRETTY PRINT
!
         IF ( Core(5)/=0.0 ) THEN
            DO WHILE ( abs(Core(5))>=10.0 )
               Core(5) = Core(5)*0.1
               Ipdet(6) = Ipdet(6) + 1
            ENDDO
            DO WHILE ( abs(Core(5))<1.0 )
               Core(5) = Core(5)*10.0
               Ipdet(6) = Ipdet(6) - 1
            ENDDO
         ENDIF
         CALL write(Ipout,Core(1),6,0)
      ENDDO
   ENDIF
   CALL write(Ipout,Core(1),0,1)
   CALL close(Ipout,1)
END SUBROUTINE detm5
