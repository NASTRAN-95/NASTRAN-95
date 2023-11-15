
SUBROUTINE fvrs1a(Base,Base1,Z,W,Buf,Index,Modfrl,Basexg,Nrow,Nf,Nfx,Fkmax,Omega)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER In , Incr , Iout , Nl , Ns
   COMMON /packx / In , Iout , Ns , Nl , Incr
!
! Dummy argument declarations
!
   INTEGER Basexg , Fkmax , Nf , Nfx , Nrow
   LOGICAL Modfrl
   REAL Omega
   COMPLEX Base(3,Nfx) , Base1(3,Nfx) , Z(Nrow)
   REAL Buf(1) , W(Nf)
   INTEGER Index(1)
!
! Local variable declarations
!
   INTEGER i , k , l , mcb(7) , npts
!
! End of declarations
!
!
!
!
!
!
!
!-----------------------------------------------------------------------
!     COMPUTE NUMBER OF GRID POINTS (SCALAR POINTS ARE NOT ALLOWED).
!-----------------------------------------------------------------------
   npts = Nrow/6
!-----------------------------------------------------------------------
!     GENERATE BASE TABLE
!----------------------------------------------------------------------
   IF ( Modfrl ) THEN
      CALL fvrs1c(Base,W,Omega,Nf)
   ELSE
      CALL fvrs1b(Base,W,Nf)
   ENDIF
!---------------------------------------------------------------------
!     SORT BASE BY INDEX TO MAKE IT COMPATIBLE TO FRLX
   IF ( Modfrl ) CALL fvrs1d(Base,Base1,Index,Nfx)
!---------------------------------------------------------------------
!     PREPARE TO OUTPUT BASEXG
!----------------------------------------------------------------------
   CALL gopen(Basexg,Buf,1)
!-------------------------------
!     DEFINE MCB
   mcb(1) = Basexg
   mcb(2) = 0
   mcb(3) = Nrow
   mcb(4) = 2
   mcb(5) = 3
   mcb(6) = 0
   mcb(7) = 0
!-------------------------------
!     DEFINE PACKING CONSTANTS
   In = 3
   Iout = 3
   Ns = 1
   Nl = Nrow
   Incr = 1
!-----------------------------------------------------------------------
!     GENERATE AND PACK 1ST NF COLUMNS OF BASEXG
!     BASEXG-1
!     ZERO OUT COLUMN
   DO i = 1 , Nrow
      Z(i) = (0.0,0.0)
   ENDDO
   DO i = 1 , Nfx
      l = 1
      DO k = 1 , npts
         Z(l) = Base(1,i)
         l = l + 6
      ENDDO
      CALL pack(Z,Basexg,mcb)
   ENDDO
   IF ( Fkmax>=2 ) THEN
!----------------------------------------------------------------------
!     GENERATE AND PACK 2ND NF COLUMNS OF BASEXG
!     BASEXG-2
!     ZERO COLUMN
      DO i = 1 , Nrow
         Z(i) = (0.0,0.0)
      ENDDO
      DO i = 1 , Nfx
         l = 1
         DO k = 1 , npts
            Z(l+1) = Base(2,i)
            Z(l+2) = Base(3,i)
            l = l + 6
         ENDDO
         CALL pack(Z,Basexg,mcb)
      ENDDO
      IF ( Fkmax>=3 ) THEN
!----------------------------------------------------------------------
!     GENERATE AND PACK 3RD NF COLUMNS OF BASEXG
!     BASEXG-3
         DO i = 1 , Nfx
            l = 1
            DO k = 1 , npts
               Z(l+1) = Base(3,i)
               Z(l+2) = -Base(2,i)
               l = l + 6
            ENDDO
            CALL pack(Z,Basexg,mcb)
         ENDDO
!-----------------------------------------------------------------------
!     GENERATE 4TH THRU FKMAX NF COLUMN GROUPS-(NULL)INTO BASEXG
         IF ( Fkmax>=4 ) THEN
            Ns = 1
            Nl = 1
            Z(1) = (0.0,0.0)
            DO i = 4 , Fkmax
               DO k = 1 , Nfx
                  CALL pack(Z,Basexg,mcb)
               ENDDO
            ENDDO
         ENDIF
      ENDIF
   ENDIF
!----------------------------------------------------------------------
!     CLOSE OUTPUT DATA BLOCK
   CALL close(Basexg,1)
   CALL wrttrl(mcb)
END SUBROUTINE fvrs1a
