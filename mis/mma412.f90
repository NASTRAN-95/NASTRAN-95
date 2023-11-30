
 
SUBROUTINE mma412(Zi,Zd)
   IMPLICIT NONE
   INCLUDE 'MMACOM.COM'
   INTEGER Cls , Clsrew , Filea(7) , Fileb(7) , Filec(7) , Filed(7) , Incrp , Incru , Iprc(2) , Iprow1 , Iprown , Irc(4) , Iurow1 , &
         & Iurown , Iwr , Ksystm(152) , Nac , Nadens , Naform , Nanzwd , Nar , Natype , Nbc , Nbdens , Nbform , Nbnzwd , Nbr ,      &
         & Nbtype , Ncc , Ncdens , Ncform , Ncnzwd , Ncr , Nctype , Ndc , Nddens , Ndform , Ndnzwd , Ndr , Ndtype , Nwords(4) , Nz ,&
         & Rd , Rdrew , Signab , Signc , T , Typei , Typep , Typeu , Wrt , Wrtrew
   REAL Prec1 , Scrtch , Sysbuf , Time
   COMMON /mpyadx/ Filea , Fileb , Filec , Filed , Nz , T , Signab , Signc , Prec1 , Scrtch , Time
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Cls
   COMMON /packx / Typei , Typep , Iprow1 , Iprown , Incrp
   COMMON /system/ Ksystm
   COMMON /type  / Iprc , Nwords , Irc
   COMMON /unpakx/ Typeu , Iurow1 , Iurown , Incru
   DOUBLE PRECISION Zd(2)
   INTEGER Zi(2)
   INTEGER i , icolb , ii , indx , indxa , indxav , indxb , indxbl , indxbv , indxd , irow1 , irowa1 , irowan , irowb1 , irowbn ,   &
         & irown , irows , k , ntms
!
!     MMA412 PERFORMS THE MATRIX OPERATION USING METHOD 41
!       IN REAL DOUBLE PRECISION
!       (+/-)A(T & NT) * B (+/-)C = D
!
!     MMA412 USES METHOD 41 WHICH IS AS FOLLOWS:
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
   Irfile = Filea(1)
   Sign = 1
   DO ii = 1 , Nac
!
! READ A COLUMN FROM THE "A" MATRIX
!
      CALL mmarc2(Zi,Zd)
!
! CHECK FOR NULL COLUMN FROM "A" MATRIX
!
      IF ( Zi(1)/=0 ) THEN
         IF ( T/=0 ) THEN
!
!  TRANSPOSE CASE ( A(T) * B + C )
!
! READ DOUBLE PRECISION
            indxb = Ibx
            DO i = 1 , Ncolpp
               indxa = 1
               icolb = Ibrow + i
               IF ( icolb/=iabs(Zi(indxb)) ) GOTO 100
               indxbl = Zi(indxb+1) + Ibx - 1
               indxb = indxb + 2
               indxd = (Idx2+(i-1)*Ndr) + ii
               DO WHILE ( indxb<indxbl )
                  irowb1 = Zi(indxb)
                  irows = Zi(indxb+1)
                  irowbn = irowb1 + irows - 1
                  indxbv = ((indxb+3)/2) - irowb1
                  indxb = indxb + 2 + irows*Nwdd
                  DO
                     irowa1 = Zi(indxa)
                     ntms = Zi(indxa+1)
                     irowan = irowa1 + ntms - 1
                     IF ( irowbn<irowa1 ) EXIT
                     IF ( irowan>=irowb1 ) THEN
                        irow1 = max0(irowa1,irowb1)
                        irown = min0(irowan,irowbn)
                        IF ( irown<irow1 ) EXIT
                        indxav = ((indxa+3)/2) - irowa1
                        DO k = irow1 , irown
                           Zd(indxd) = Zd(indxd) + Zd(indxav+k)*Zd(indxbv+k)
                        ENDDO
                        IF ( irowan>irowbn ) EXIT
                     ENDIF
                     indxa = indxa + 2 + ntms*Nwdd
                     IF ( indxa>=Lasind ) GOTO 5
                  ENDDO
               ENDDO
 5             indxb = indxbl
            ENDDO
         ELSE
!
! "A" NON-TRANSPOSE CASE    ( A*B + C )
!
! READ DOUBLE PRECISION
            indxb = Ibx
            DO i = 1 , Ncolpp
               icolb = Ibrow + i
               IF ( icolb/=iabs(Zi(indxb)) ) GOTO 100
               indxbl = Zi(indxb+1) + Ibx - 1
               indxb = indxb + 2
               indxd = (Idx2+(i-1)*Ndr)
               DO WHILE ( indxb<indxbl )
                  irowb1 = Zi(indxb)
                  irows = Zi(indxb+1)
                  irowbn = irowb1 + irows - 1
                  IF ( ii>irowbn ) THEN
                     indxb = indxb + 2 + irows*Nwdd
                  ELSE
                     IF ( ii<irowb1 ) EXIT
                     indxbv = ((indxb+3)/2) + ii - irowb1
                     IF ( Zd(indxbv)==0.0 ) EXIT
                     indxa = 1
                     DO
                        irowa1 = Zi(indxa)
                        ntms = Zi(indxa+1)
                        irowan = irowa1 + ntms - 1
                        indxav = ((indxa+3)/2) - irowa1
                        DO k = irowa1 , irowan
                           Zd(indxd+k) = Zd(indxd+k) + Zd(indxav+k)*Zd(indxbv)
                        ENDDO
                        indxa = indxa + 2 + ntms*Nwdd
                        IF ( indxa>=Lasind ) GOTO 10
                     ENDDO
                  ENDIF
               ENDDO
 10            indxb = indxbl
            ENDDO
         ENDIF
      ENDIF
! END OF PROCESSING THIS COLUMN OF "A" FOR THIS PASS
   ENDDO
!  NOW SAVE COLUMNS COMPLETED
   DO k = 1 , Ncolpp
      indx = Idx2 + (k-1)*Ndr + 1
      CALL pack(Zd(indx),Filed,Filed)
   ENDDO
   GOTO 99999
 100  WRITE (Iwr,99001) icolb , Zi(indxb) , Ibx , indxb
99001 FORMAT (' UNEXPECTED COLUMN FOUND IN PROCESSING MATRIX B',/,' COLUMN EXPECTED:',I6,/,' COLUMN FOUND   :',I6,/,' IBX =',I7,    &
             &'  INDXB =',I7)
   CALL mesage(-61,0,0)
99999 RETURN
END SUBROUTINE mma412