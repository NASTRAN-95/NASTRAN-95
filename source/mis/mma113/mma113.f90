!*==mma113.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
 
SUBROUTINE mma113(Zi,Zc)
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
   INTEGER :: i , ibrowi , idrow , idx2x , ii , indx , indxa , indxav , indxb , indxv , irow1 , irowa1 , irowan , irowb1 , irowbn , &
            & irown , irows , j , k , nac , nadens , naform , nanzwd , nar , natype , nbc , nbdens , nbform , nbnzwd , nbr ,        &
            & nbtype , ncc , ncdens , ncform , ncnzwd , ncr , nctype , ndc , nddens , ndform , ndnzwd , ndr , ndtype , nout ,       &
            & nwddnar
   REAL :: sysbuf
!
! End of declarations rewritten by SPAG
!
!
!     MMA113 PERFORMS THE MATRIX OPERATION IN COMPLEX SINGLE PRECISION
!       (+/-)A(T & NT) * B (+/-)C = D
!
!     MMA113 USES METHOD 11 WHICH IS AS FOLLOWS:
!       1.  THIS IS FOR "A" NON-TRANSPOSED AND TRANSPOSED
!       2.  UNPACK AS MANY COLUMNS OF "A" INTO MEMORY AS POSSIBLE
!           LEAVING SPACE FOR ONE COLUMN OF "B" AND "D".
!       3.  INITIALIZE EACH COLUMN OF "D" WITH THE DATA FROM "C".
!       4.  UNPACK COLUMNS OF "C" MATRIX BUT USE GETSTR (MMARC1,2,3,4)
!           TO READ COLUMNS OF "B".
!
!     MEMORY FOR EACH COLUMN OF "A" IS AS FOLLOWS:
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
!     Z( 1        ) = ARRAY FOR ONE COLUMN OF "B" MATRIX IN COMPACT FORM
!     Z( IDX      ) = ARRAY FOR ONE COLUMN OF "D" MATRIX
!     Z( IAX      ) = ARRAY FOR MULTIPLE COLUMNS OF "A" MATRIX
!        THROUGH
!     Z( LASMEM   )
!     Z( IBUF4    ) = BUFFER FOR "D" FILE
!     Z( IBUF3    ) = BUFFER FOR "C" FILE
!     Z( IBUF2    ) = BUFFER FOR "B" FILE
!     Z( IBUF1    ) = BUFFER FOR "A" FILE
!     Z( NZ       ) = END OF OPEN CORE THAT IS AVAILABLE
!
   DO ii = 1 , nbc
!
! READ COLUMN FROM THE "B" MATRIX
!
      CALL mmarc3(Zi,Zc)
!
! NOW READ "C", OR SCRATCH FILE WITH INTERMEDIATE RESULTS.
! IF NO "C" FILE AND THIS IS THE FIRST PASS, INITIALIZE "D" COLUMN AS ZERO.
!
      IF ( ifile/=0 ) THEN
         Iurow1 = 1
         Iurown = ndr
         Typeu = ndtype
         IF ( ipass==1 ) Typeu = ndtype*Signc
         CALL unpack(*50,ifile,Zc(idx2+1))
         GOTO 100
      ENDIF
 50   DO j = 1 , ndr
         Zc(idx2+j) = (0.0,0.0)
      ENDDO
 100  nwddnar = nwdd*nar
!
! CHECK IF COLUMN OF "B" IS NULL
!
      irowb1 = Zi(1)
      irows = Zi(2)
      irowbn = irowb1 + irows - 1
      indx = 1
!
! CHECK FOR NULL COLUMN FROM THE "B" MATRIX
!
      IF ( irowb1/=0 ) THEN
         IF ( T/=0 ) THEN
!
!  TRANSPOSE CASE ( A(T) * B + C )
!
            idrow = ibrow
! COMPLEX SINGLE PRECISION
            DO i = 1 , ncolpp
               indx = 1
               indxa = iax + 2*i + (i-1)*nwddnar
               irowa1 = Zi(indxa-2)
               IF ( irowa1/=0 ) THEN
                  irowan = Zi(indxa-1)
                  indxav = ((indxa+1)/2) - irowa1
                  DO WHILE ( indx<lasind )
                     irowb1 = Zi(indx)
                     irows = Zi(indx+1)
                     irowbn = irowb1 + irows - 1
                     indxv = (indx+3)/2
                     indx = indx + 2 + irows*nwdd
                     irow1 = max0(irowa1,irowb1)
                     irown = min0(irowan,irowbn)
                     IF ( irown>=irow1 ) THEN
                        idx2x = idx2 + idrow
                        indxb = indxv - irowb1
                        DO k = irow1 , irown
                           Zc(idx2x+i) = Zc(idx2x+i) + Zc(indxav+k)*Zc(indxb+k)
                        ENDDO
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ELSE
!
! "A" NON-TRANSPOSE CASE    ( A * B  +  C )
!
! COMPLEX SINGLE PRECISION
            DO i = 1 , ncolpp
               ibrowi = ibrow + i
               indxa = iax + 2*i + (i-1)*nwddnar
               irowa1 = Zi(indxa-2)
               IF ( irowa1/=0 ) THEN
                  irowan = Zi(indxa-1)
                  indxav = ((indxa+1)/2) - irowa1
                  DO WHILE ( ibrowi>=irowb1 )
                     IF ( ibrowi<=irowbn ) THEN
                        indxv = ibrowi - irowb1 + (indx+3)/2
                        IF ( Zc(indxv)/=(0.0,0.0) ) THEN
                           DO k = irowa1 , irowan
                              Zc(idx2+k) = Zc(idx2+k) + Zc(indxav+k)*Zc(indxv)
                           ENDDO
                        ENDIF
                        EXIT
                     ELSE
                        indx = indx + 2 + irows*nwdd
                        IF ( indx>=lasind ) GOTO 150
                        irowb1 = Zi(indx)
                        irows = Zi(indx+1)
                        irowbn = irowb1 + irows - 1
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ENDIF
      ENDIF
! END OF PROCESSING THIS COLUMN FOR THIS PASS
!  NOW SAVE COLUMN
 150  CALL pack(Zc(idx2+1),ofile,Filed)
   ENDDO
END SUBROUTINE mma113
