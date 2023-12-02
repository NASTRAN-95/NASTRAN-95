!*==mma414.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
 
SUBROUTINE mma414(Zi,Zd,Zdc)
   USE i_mmacom
   USE c_mpyadx
   USE c_names
   USE c_packx
   USE c_system
   USE c_type
   USE c_unpakx
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: Zi
   REAL*8 , DIMENSION(2) :: Zd
   COMPLEX*16 , DIMENSION(2) :: Zdc
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , icnt , icolb , ii , indx , indxa , indxav , indxb , indxbl , indxbs , indxbv , indxd , irow1 , irowa1 , irowan ,  &
            & irowb1 , irowbn , irown , irows , iwr , k , nac , nadens , naform , nanzwd , nar , natype , nbc , nbdens , nbform ,   &
            & nbnzwd , nbr , nbtype , ncc , ncdens , ncform , ncnzwd , ncr , nctype , ndc , nddens , ndform , ndnzwd , ndr ,        &
            & ndtype , ntms
   REAL :: sysbuf
!
! End of declarations rewritten by SPAG
!
!
!     MMA414 PERFORMS THE MATRIX OPERATION USING METHOD 41
!       IN COMPLEX DOUBLE PRECISION
!       (+/-)A(T & NT) * B (+/-)C = D
!
!     MMA414 USES METHOD 41 WHICH IS AS FOLLOWS:
!       1.  THIS IS FOR "A" NON-TRANSPOSED AND TRANSPOSED
!       2.  READ AS MANY COLUMNS OF "B" INTO MEMORY AS POSSIBLE
!           INTO MEMORY IN COMPACT FORM LEAVING SPACE FOR A FULL
!           COLUMN OF "D" FOR EVERY COLUMN "B" READ.  SEE SUBROUTINES
!           MMARM1,2,3,4 FOR FORMAT OF COMPACT FORM.
!       3.  INITIALIZE EACH COLUMN OF "D" WITH THE DATA FROM "C".
!       4.  CALL UNPACK TO READ MATRICES "C".
!       5.  CALL MMARC1,2,3,4 TO READ COLUMNS OF MATRIX "A".
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Iwr)
   !>>>>EQUIVALENCE (Filea(2),Nac) , (Filea(3),Nar) , (Filea(4),Naform) , (Filea(5),Natype) , (Filea(6),Nanzwd) , (Filea(7),Nadens)
   !>>>>EQUIVALENCE (Fileb(2),Nbc) , (Fileb(3),Nbr) , (Fileb(4),Nbform) , (Fileb(5),Nbtype) , (Fileb(6),Nbnzwd) , (Fileb(7),Nbdens)
   !>>>>EQUIVALENCE (Filec(2),Ncc) , (Filec(3),Ncr) , (Filec(4),Ncform) , (Filec(5),Nctype) , (Filec(6),Ncnzwd) , (Filec(7),Ncdens)
   !>>>>EQUIVALENCE (Filed(2),Ndc) , (Filed(3),Ndr) , (Filed(4),Ndform) , (Filed(5),Ndtype) , (Filed(6),Ndnzwd) , (Filed(7),Nddens)
!
!
!   OPEN CORE ALLOCATION AS FOLLOWS:
!     Z( 1        ) = ARRAY FOR ONE COLUMN OF "A" MATRIX
!     Z( IBX      ) = ARRAY FOR MULTIPLE COLUMNS OF "B" MATRIX
!                     (STORED IN COMPACT FORM)
!     Z( IDX      ) = ARRAY FOR MULTIPLE COLUMNS OF "D" MATRIX
!                     (FULL COLUMN SPACE ALLOCATION)
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
   irfile = filea(1)
   sign = 1
   DO ii = 1 , nac
!
! READ A COLUMN FROM THE "A" MATRIX
!
      CALL mmarc4(Zi,Zd)
!
! CHECK IF COLUMN FROM "A" MATRIX IS NULL
!
      IF ( Zi(1)/=0 ) THEN
         IF ( t/=0 ) THEN
!
!  TRANSPOSE CASE ( A(T) * B + C )
!
! COMPLEX DOUBLE PRECISION
            indxb = ibx
            DO i = 1 , ncolpp
               indxa = 1
               icolb = ibrow + i
               IF ( icolb/=iabs(Zi(indxb)) ) THEN
                  CALL spag_block_1
                  RETURN
               ENDIF
               indxbl = Zi(indxb+1) + ibx - 1
               indxb = indxb + 2
               indxd = (idx4+(i-1)*ndr) + ii
               SPAG_Loop_3_2: DO WHILE ( indxb<indxbl )
                  irowb1 = Zi(indxb)
                  irows = Zi(indxb+1)
                  irowbn = irowb1 + irows - 1
                  indxbs = indxb
                  indxb = indxb + 2 + irows*nwdd
                  SPAG_Loop_4_1: DO
                     irowa1 = Zi(indxa)
                     ntms = Zi(indxa+1)
                     irowan = irowa1 + ntms - 1
                     IF ( irowbn<irowa1 ) EXIT SPAG_Loop_4_1
                     IF ( irowan>=irowb1 ) THEN
                        irow1 = max0(irowa1,irowb1)
                        irown = min0(irowan,irowbn)
                        IF ( irown<irow1 ) EXIT SPAG_Loop_4_1
                        indxbv = ((indxbs+3)/2) + 2*(irow1-irowb1) - 1
                        indxav = ((indxa+3)/2) + 2*(irow1-irowa1) - 1
                        icnt = 2*(irown-irow1) + 1
                        DO k = 1 , icnt , 2
                           Zdc(indxd) = Zdc(indxd) + dcmplx(Zd(indxav+k),Zd(indxav+k+1))*dcmplx(Zd(indxbv+k),Zd(indxbv+k+1))
                        ENDDO
                        IF ( irowan>irowbn ) EXIT SPAG_Loop_4_1
                     ENDIF
                     indxa = indxa + 2 + ntms*nwdd
                     IF ( indxa>=lasind ) EXIT SPAG_Loop_3_2
                  ENDDO SPAG_Loop_4_1
               ENDDO SPAG_Loop_3_2
               indxb = indxbl
            ENDDO
         ELSE
!
! "A" NON-TRANSPOSE CASE    ( A*B + C )
!
! COMPLEX DOUBLE PRECISION
            indxb = ibx
            DO i = 1 , ncolpp
               icolb = ibrow + i
               IF ( icolb/=iabs(Zi(indxb)) ) THEN
                  CALL spag_block_1
                  RETURN
               ENDIF
               indxbl = Zi(indxb+1) + ibx - 1
               indxb = indxb + 2
               indxd = (idx4+(i-1)*ndr)
               SPAG_Loop_3_3: DO WHILE ( indxb<indxbl )
                  irowb1 = Zi(indxb)
                  irows = Zi(indxb+1)
                  irowbn = irowb1 + irows - 1
                  IF ( ii>irowbn ) THEN
                     indxb = indxb + 2 + irows*nwdd
                  ELSE
                     IF ( ii<irowb1 ) EXIT SPAG_Loop_3_3
                     indxbv = ((indxb+3)/2) + 2*(ii-irowb1)
                     IF ( Zd(indxbv)==0.0D0 .AND. Zd(indxbv+1)==0.0D0 ) EXIT SPAG_Loop_3_3
                     indxa = 1
                     DO
                        irowa1 = Zi(indxa)
                        ntms = Zi(indxa+1)
                        irowan = irowa1 + ntms - 1
                        indxav = ((indxa+3)/2)
                        DO k = irowa1 , irowan
                           Zdc(indxd+k) = Zdc(indxd+k) + dcmplx(Zd(indxav),Zd(indxav+1))*dcmplx(Zd(indxbv),Zd(indxbv+1))
                           indxav = indxav + 2
                        ENDDO
                        indxa = indxa + 2 + ntms*nwdd
                        IF ( indxa>=lasind ) EXIT SPAG_Loop_3_3
                     ENDDO
                  ENDIF
               ENDDO SPAG_Loop_3_3
               indxb = indxbl
            ENDDO
         ENDIF
      ENDIF
! END OF PROCESSING THIS COLUMN OF "A" FOR THIS PASS
   ENDDO
!  NOW SAVE COLUMNS COMPLETED
   DO k = 1 , ncolpp
      indx = idx4 + (k-1)*ndr + 1
      CALL pack(Zdc(indx),filed,filed)
   ENDDO
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
      WRITE (iwr,99001) icolb , Zi(indxb) , ibx , indxb
99001 FORMAT (' UNEXPECTED COLUMN FOUND IN PROCESSING MATRIX B',/,' COLUMN EXPECTED:',I6,/,' COLUMN FOUND   :',I6,/,' IBX =',I7,    &
             &'  INDXB =',I7)
      CALL mesage(-61,0,0)
   END SUBROUTINE spag_block_1
END SUBROUTINE mma414
