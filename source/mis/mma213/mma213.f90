!*==mma213.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
 
SUBROUTINE mma213(Zi,Zc)
   IMPLICIT NONE
   USE I_MMACOM
   USE C_MPYADX
   USE C_NAMES
   USE C_PACKX
   USE C_SYSTEM
   USE C_TYPE
   USE C_UNPAKX
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: Zi
   COMPLEX , DIMENSION(2) :: Zc
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ii , indx , indxa , indxb , indxd , irow1 , irowa1 , irowan , irowb1 , irowbn , irown , irows , k , nac , nadens ,&
            & naform , nanzwd , nar , natype , nbc , nbdens , nbform , nbnzwd , nbr , nbtype , ncc , ncdens , ncform , ncnzwd ,     &
            & ncr , nctype , ndc , nddens , ndform , ndnzwd , ndr , ndtype , nout
   REAL :: sysbuf
!
! End of declarations rewritten by SPAG
!
!
!     MMA211 PERFORMS THE MATRIX OPERATION USING METHOD 21
!       IN COMPLEX SINGLE PRECISION.
!
!       (+/-)A(T & NT) * B (+/-)C = D
!
!     MMA213 USES METHOD 21 WHICH IS AS FOLLOWS:
!       1.  THIS IS FOR "A" NON-TRANSPOSED AND TRANSPOSED
!       2.  UNPACK AS MANY COLUMNS OF "B" INTO MEMORY AS POSSIBLE
!           LEAVING SPACE FOR A COLUMN OF "D" FOR EVERY COLUMN "B" READ.
!       3.  INITIALIZE EACH COLUMN OF "D" WITH THE DATA FROM "C".
!       4.  USE UNPACK TO READ MATRICES "B" AND "C".
!
!     MEMORY FOR EACH COLUMN OF "B" IS AS FOLLOWS:
!         Z(1)   = FIRST NON-ZERO ROW NUMBER FOR COLUMN
!         Z(2)   = LAST NON-ZERO ROW NUMBER FOR COLUMN
!         Z(3-N) = VALUES OF NON-ZERO ROWS
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Nout)
   !>>>>EQUIVALENCE (Filea(2),Nac) , (Filea(3),Nar) , (Filea(4),Naform) , (Filea(5),Natype) , (Filea(6),Nanzwd) , (Filea(7),Nadens)
   !>>>>EQUIVALENCE (Fileb(2),Nbc) , (Fileb(3),Nbr) , (Fileb(4),Nbform) , (Fileb(5),Nbtype) , (Fileb(6),Nbnzwd) , (Fileb(7),Nbdens)
   !>>>>EQUIVALENCE (Filec(2),Ncc) , (Filec(3),Ncr) , (Filec(4),Ncform) , (Filec(5),Nctype) , (Filec(6),Ncnzwd) , (Filec(7),Ncdens)
   !>>>>EQUIVALENCE (Filed(2),Ndc) , (Filed(3),Ndr) , (Filed(4),Ndform) , (Filed(5),Ndtype) , (Filed(6),Ndnzwd) , (Filed(7),Nddens)
!
!
!   OPEN CORE ALLOCATION AS FOLLOWS:
!     Z( 1        ) = ARRAY FOR ONE COLUMN OF "A" MATRIX
!     Z( IDX      ) = ARRAY FOR MULTIPLE COLUMNS OF "D" MATRIX
!     Z( IBX      ) = ARRAY FOR MULTIPLE COLUMNS OF "B" MATRIX
!        THROUGH
!     Z( LASMEM   )
!     Z( IBUF4    ) = BUFFER FOR "D" FILE
!     Z( IBUF3    ) = BUFFER FOR "C" FILE
!     Z( IBUF2    ) = BUFFER FOR "B" FILE
!     Z( IBUF1    ) = BUFFER FOR "A" FILE
!     Z( NZ       ) = END OF OPEN CORE THAT IS AVAILABLE
!
!
! PROCESS ALL OF THE COLUMNS OF "A"
!
   DO ii = 1 , nac
!
! READ A COLUMN FROM THE "A" MATRIX
!
      CALL mmarc3(Zi,Zc)
!
! CHECK IF "A" COLUMN IS NULL
!
      IF ( Zi(1)/=0 ) THEN
         IF ( T/=0 ) THEN
!
!  TRANSPOSE CASE ( A(T) * B + C )
!
! COMPLEX SINGLE PRECISION
            DO i = 1 , ncolpp
               indxb = ibx + 2*i + (i-1)*nwddnbr
               irowb1 = Zi(indxb-2)
               IF ( irowb1==0 ) CYCLE
               irowbn = Zi(indxb-1)
               indx = 1
               irowa1 = Zi(indx)
               indxb = ((indxb+1)/2) - irowb1
               indxa = 1 - irowa1
               indxd = idx + (i-1)*nwddndr
               indxd = ((indxd+1)/2) - 1 + ii
               DO
                  irows = Zi(indx+1)
                  irowan = irowa1 + irows - 1
                  irow1 = max0(irowa1,irowb1)
                  irown = min0(irowan,irowbn)
                  IF ( irown>=irow1 ) THEN
                     indxa = ((indx+1)/2) + 1 - irowa1
                     DO k = irow1 , irown
                        Zc(indxd) = Zc(indxd) + Zc(indxa+k)*Zc(indxb+k)
                     ENDDO
                  ENDIF
                  indx = indx + 2 + irows*nwdd
                  IF ( indx>=lasind ) EXIT
                  irowa1 = Zi(indx)
               ENDDO
            ENDDO
         ELSE
!
! "A" NON-TRANSPOSE CASE    ( A*B + C )
!
! COMPLEX SINGLE PRECISION
            DO i = 1 , ncolpp
               indx = 1
               irowa1 = Zi(indx)
               indxb = ibx + 2*i + (i-1)*nwddnbr
               irowb1 = Zi(indxb-2)
               irowbn = Zi(indxb-1)
               IF ( ii>=irowb1 .AND. ii<=irowbn ) THEN
                  indxb = ((indxb+1)/2) + ii - irowb1
                  IF ( Zc(indxb)/=(0.0,0.0) ) THEN
                     indxd = idx + (i-1)*nwddndr
                     indxd = ((indxd+1)/2) - 1
                     DO
                        irows = Zi(indx+1)
                        irowan = irowa1 + irows - 1
                        indxa = ((indx+1)/2) + 1 - irowa1
                        DO k = irowa1 , irowan
                           Zc(indxd+k) = Zc(indxd+k) + Zc(indxa+k)*Zc(indxb)
                        ENDDO
                        indx = indx + 2 + irows*nwdd
                        IF ( indx>=lasind ) EXIT
                        irowa1 = Zi(indx)
                     ENDDO
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
      ENDIF
! END OF PROCESSING THIS COLUMN OF "A" FOR THIS PASS
   ENDDO
!  NOW SAVE COLUMNS COMPLETED
   DO k = 1 , ncolpp
      indx = idx2 + (k-1)*ndr
      CALL pack(Zc(indx+1),Filed,Filed)
   ENDDO
END SUBROUTINE mma213
