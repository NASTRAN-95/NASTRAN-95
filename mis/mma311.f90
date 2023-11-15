
 
SUBROUTINE mma311(Zi,Zr)
   IMPLICIT NONE
   INCLUDE 'MMACOM.COM'
!
! COMMON variable declarations
!
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
!
! Dummy argument declarations
!
   INTEGER Zi(2)
   REAL Zr(2)
!
! Local variable declarations
!
   INTEGER i , icola , idrow , idxx , ii , indxa , indxal , indxav , indxb , indxbv , irow1 , irowa1 , irowan , irowb1 , irowbn ,   &
         & irown , irows , j , k , ntms
!
! End of declarations
!
!
!     MMA311 PERFORMS THE MATRIX OPERATION IN REAL SINGLE PRECISION
!       (+/-)A(T & NT) * B (+/-)C = D
!
!     MMA311 USES METHOD 31 WHICH IS AS FOLLOWS:
!       1.  THIS IS FOR "A" NON-TRANSPOSED AND TRANSPOSED
!       2.  CALL MMARM1 TO PACK AS MANY COLUMNS OF "A" INTO MEMORY
!           AS POSSIBLE LEAVING SPACE FOR ONE COLUMN OF "B" AND "D".
!       3.  INITIALIZE EACH COLUMN OF "D" WITH THE DATA FROM "C".
!       4.  UNPACK COLUMNS OF "C" MATRIX BUT USE GETSTR (MMARC1,2,3,4)
!           TO READ COLUMNS OF "B".
!
   EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Iwr)
   EQUIVALENCE (Filea(2),Nac) , (Filea(3),Nar) , (Filea(4),Naform) , (Filea(5),Natype) , (Filea(6),Nanzwd) , (Filea(7),Nadens)
   EQUIVALENCE (Fileb(2),Nbc) , (Fileb(3),Nbr) , (Fileb(4),Nbform) , (Fileb(5),Nbtype) , (Fileb(6),Nbnzwd) , (Fileb(7),Nbdens)
   EQUIVALENCE (Filec(2),Ncc) , (Filec(3),Ncr) , (Filec(4),Ncform) , (Filec(5),Nctype) , (Filec(6),Ncnzwd) , (Filec(7),Ncdens)
   EQUIVALENCE (Filed(2),Ndc) , (Filed(3),Ndr) , (Filed(4),Ndform) , (Filed(5),Ndtype) , (Filed(6),Ndnzwd) , (Filed(7),Nddens)
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
   Irfile = Fileb(1)
   DO ii = 1 , Nbc
!      PRINT *,' PROCESSING COLUMN=',II
!
! READ A COLUMN FROM THE "B" MATRIX
!
      CALL mmarc1(Zi,Zr)
!
! NOW READ "C", OR SCRATCH FILE WITH INTERMEDIATE RESULTS.
! IF NO "C" FILE AND THIS IS THE FIRST PASS, INITIALIZE "D" COLUMN TO ZERO.
!
      IF ( Ifile/=0 ) THEN
         Iurow1 = 1
         Iurown = Ndr
         Typeu = Ndtype
         IF ( Ipass==1 ) Typeu = Ndtype*Signc
         CALL unpack(*50,Ifile,Zr(Idx))
         GOTO 100
      ENDIF
 50   DO j = 1 , Ndr
         Zr(Idx+j-1) = 0
      ENDDO
!
! CHECK IF COLUMN OF "B" IS NULL
!
 100  irowb1 = Zi(1)
      irows = Zi(2)
      irowbn = irowb1 + irows - 1
      indxb = 1
      indxa = Iax
!
! CHECK FOR NULL COLUMN FROM "B" MATRIX
!
      IF ( irowb1/=0 ) THEN
         IF ( T/=0 ) THEN
!
!  TRANSPOSE CASE ( A(T) * B + C )
!
            idrow = Ibrow
            idxx = Idx + idrow - 1
! SINGLE PRECISION
            DO i = 1 , Ncolpp
               icola = Ibrow + i
               IF ( icola/=iabs(Zi(indxa)) ) GOTO 200
               indxal = Zi(indxa+1) + Iax - 1
               indxa = indxa + 2
               indxb = 1
               DO WHILE ( indxb<Lasind )
                  irowb1 = Zi(indxb)
                  irows = Zi(indxb+1)
                  irowbn = irowb1 + irows - 1
                  indxbv = indxb + 2 - irowb1
                  indxb = indxb + 2 + irows
                  DO WHILE ( indxa<indxal )
                     irowa1 = Zi(indxa)
                     ntms = Zi(indxa+1)
                     irowan = irowa1 + ntms - 1
                     IF ( irowbn<irowa1 ) GOTO 105
                     IF ( irowan<irowb1 ) THEN
                        indxa = indxa + 2 + ntms
                     ELSE
                        irow1 = max0(irowa1,irowb1)
                        irown = min0(irowan,irowbn)
                        indxav = indxa + 2 - irowa1
                        DO k = irow1 , irown
                           Zr(idxx+i) = Zr(idxx+i) + Zr(indxav+k)*Zr(indxbv+k)
                        ENDDO
                        IF ( irowan>irowbn ) GOTO 105
                        indxa = indxa + 2 + ntms
                     ENDIF
                  ENDDO
                  EXIT
 105           ENDDO
               indxa = indxal
            ENDDO
         ELSE
!
! "A" NON-TRANSPOSE CASE    ( A * B  +  C )
!
! SINGLE PRECISION
            DO i = 1 , Ncolpp
               indxal = Zi(indxa+1) + Iax - 1
               icola = Ibrow + i
               IF ( icola/=iabs(Zi(indxa)) ) GOTO 200
               indxa = indxa + 2
               DO WHILE ( icola>=irowb1 )
                  IF ( icola<=irowbn ) THEN
                     indxbv = icola - irowb1 + indxb + 2
                     IF ( Zr(indxbv)==0. ) EXIT
                     DO WHILE ( indxa<indxal )
                        irowa1 = Zi(indxa)
                        ntms = Zi(indxa+1)
                        irowan = irowa1 + ntms - 1
                        indxav = indxa + 2 - irowa1
                        DO k = irowa1 , irowan
                           Zr(Idx+k-1) = Zr(Idx+k-1) + Zr(indxav+k)*Zr(indxbv)
                        ENDDO
                        indxa = indxa + 2 + ntms
                     ENDDO
                     EXIT
                  ELSE
                     indxb = indxb + 2 + irows
                     IF ( indxb>Lasind ) GOTO 150
                     irowb1 = Zi(indxb)
                     irows = Zi(indxb+1)
                     irowbn = irowb1 + irows - 1
                  ENDIF
               ENDDO
               indxa = indxal
            ENDDO
         ENDIF
      ENDIF
! END OF PROCESSING THIS COLUMN FOR THIS PASS
!  NOW SAVE COLUMN
 150  CALL pack(Zr(Idx),Ofile,Filed)
   ENDDO
   GOTO 99999
 200  WRITE (Iwr,99001) icola , Zi(indxa) , Iax , indxa
99001 FORMAT (' UNEXPECTED COLUMN FOUND IN PROCESSING MATRIX A',/,' COLUMN EXPECTED:',I6,/,' COLUND FOUND   :',I6,/,' IAX =',I7,    &
             &' INDXA=',I7)
   CALL mesage(-61,0,0)
99999 END SUBROUTINE mma311
