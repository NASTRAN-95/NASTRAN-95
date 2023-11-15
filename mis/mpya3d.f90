
SUBROUTINE mpya3d(Aa,Bb,Nrow,Band,Cc)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER Band , Nrow
   REAL A(1) , B(1) , C(1)
   DOUBLE PRECISION Aa(1) , Bb(1) , Cc(1)
!
! Local variable declarations
!
   DOUBLE PRECISION dd
   INTEGER i , ia , ia1 , iaid , ib , ibia , ibic , ic , icid , id , id1 , id11n , id2 , ii , j , jj , kk
!
! End of declarations
!
!
!     WITH ENTRY MPYA3S (A,B,NROW,BAND,C)
!
!     WAS NAMED DATBAD/DATBAS IN UAI CODE
!
!     THESE ROUTINES PERFORM TRIPLE MATRIX MULTIPLY OF THE FORM
!
!                          T
!                 C = C + A * B * A
!
!     ON TWO INCOMING ROW-LOADED MATRICES A AND B, AND ADD THEM TO
!     MATRIX C
!
!     THE INCOMING MATRICES MUST BE SQUARE (AND OBVIOUSLY OF THE SAME
!     SIZE, NROW.) AND
!     SYMMETRICAL (SINCE WE OPERATE ONLY ON LOWER TRIANGULAR MATRICES)
!
!     MATRIX A CAN BE A PSUEDO-DIAGONAL MATRIX, I.E. A MATRIX HAVING
!     SQUARE PARTITIONS OF NON-ZERO TERMS ALONG ITS DIAGONAL.
!     THESE PARTITIONS ARE OF THE SIZE  BAND X BAND.
!     NOTE THAT NROW MUST BE AN INTEGER MULTIPLE OF BAND.
!
!     THIS ALGORITHM IS SUITABLE FOR TRIPLE MULTIPLIES INVOLVING GLOBAL
!     TRANSFORMATIONS.
!
!
!
!
!     DOUBLE PRECISION VERSION
!
   ii = 0
   DO ib = 1 , Nrow
      ia1 = ((ib-1)/Band+1)*Band
!
      DO id1 = 1 , Nrow , Band
         id2 = id1 + Band - 1
         IF ( id1>ia1 ) EXIT
!
         id11n = (id1-1)*Nrow
         DO id = id1 , id2
            jj = id11n
            dd = 0.0D0
!
            DO ic = id1 , id2
               ibic = ii + ic
               icid = jj + id
               IF ( Aa(icid)/=0.0D0 ) dd = dd + Bb(ibic)*Aa(icid)
               jj = jj + Nrow
            ENDDO
!
            IF ( dd/=0.0D0 ) THEN
               kk = (id-1)*Nrow
!
               DO ia = id , ia1
                  ibia = ii + ia
                  IF ( Aa(ibia)/=0.0D0 ) THEN
                     iaid = kk + id
                     Cc(iaid) = Cc(iaid) + dd*Aa(ibia)
                  ENDIF
                  kk = kk + Nrow
               ENDDO
            ENDIF
!
         ENDDO
      ENDDO
      ii = ii + Nrow
   ENDDO
!
!     COPY THE LOWER TRIANGLE TO THE UPPER
!
   kk = Nrow - 1
   ii = 0
   DO i = 1 , kk
      ib = i + 1
      jj = i*Nrow
      DO j = ib , Nrow
         Cc(ii+j) = Cc(jj+i)
         jj = jj + Nrow
      ENDDO
      ii = ii + Nrow
   ENDDO
!
   RETURN
!
!
   ENTRY mpya3s(A,B,Nrow,Band,C)
!     ==============================
!
!     SINGLE PRECISION VERSION
!
   ii = 0
   DO ib = 1 , Nrow
      ia1 = ((ib-1)/Band+1)*Band
!
      DO id1 = 1 , Nrow , Band
         id2 = id1 + Band - 1
         IF ( id1>ia1 ) EXIT
!
         id11n = (id1-1)*Nrow
         DO id = id1 , id2
            jj = id11n
            dd = 0.0D0
!
            DO ic = id1 , id2
               ibic = ii + ic
               icid = jj + id
               IF ( A(icid)/=0.0 ) dd = dd + dble(B(ibic))*dble(A(icid))
               jj = jj + Nrow
            ENDDO
            IF ( dd/=0.0D0 ) THEN
               kk = (id-1)*Nrow
!
               DO ia = id , ia1
                  ibia = ii + ia
                  IF ( A(ibia)/=0.0 ) THEN
                     iaid = kk + id
                     C(iaid) = sngl(dble(C(iaid))+dd*dble(A(ibia)))
                  ENDIF
                  kk = kk + Nrow
               ENDDO
            ENDIF
!
         ENDDO
      ENDDO
      ii = ii + Nrow
   ENDDO
!
!     COPY THE LOWER TRIANGLE TO THE UPPER
!
   kk = Nrow - 1
   ii = 0
   DO i = 1 , kk
      ib = i + 1
      jj = i*Nrow
      DO j = ib , Nrow
         C(ii+j) = C(jj+i)
         jj = jj + Nrow
      ENDDO
      ii = ii + Nrow
   ENDDO
!
END SUBROUTINE mpya3d
