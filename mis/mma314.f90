
SUBROUTINE mma314(Zi,Zd,Zdc)
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
   DOUBLE COMPLEX Zdc(2)
   INTEGER Zi(2)
   INTEGER i , icola , idrow , idxx , ii , indxa , indxal , indxav , indxb , indxbs , indxbv , irow1 , irowa1 , irowan , irowb1 ,   &
         & irowbn , irown , irows , j , k , kcnt , ntms
!
!     MMA314 PERFORMS THE MATRIX OPERATION IN COMPLEX DOUBLE PRECISION
!       (+/-)A(T & NT) * B (+/-)C = D
!
!     MMA314 USES METHOD 31 WHICH IS AS FOLLOWS:
!       1.  THIS IS FOR "A" NON-TRANSPOSED AND TRANSPOSED
!       2.  CALL MMARM1 TO PACK AS MANY COLUMNS OF "A" INTO MEMORY
!           AS POSSIBLE LEAVING SPACE FOR ONE COLUMN OF "B" AND "D".
!       3.  INITIALIZE EACH COLUMN OF "D" WITH THE DATA FROM "C".
!       4.  UNPACK COLUMNS OF "C" MATRIX BUT USE GETSTR (MMARC1,2,3,4)
!           TO READ COLUMNS OF "B".
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Iwr)
   !>>>>EQUIVALENCE (Filea(2),Nac) , (Filea(3),Nar) , (Filea(4),Naform) , (Filea(5),Natype) , (Filea(6),Nanzwd) , (Filea(7),Nadens)
   !>>>>EQUIVALENCE (Fileb(2),Nbc) , (Fileb(3),Nbr) , (Fileb(4),Nbform) , (Fileb(5),Nbtype) , (Fileb(6),Nbnzwd) , (Fileb(7),Nbdens)
   !>>>>EQUIVALENCE (Filec(2),Ncc) , (Filec(3),Ncr) , (Filec(4),Ncform) , (Filec(5),Nctype) , (Filec(6),Ncnzwd) , (Filec(7),Ncdens)
   !>>>>EQUIVALENCE (Filed(2),Ndc) , (Filed(3),Ndr) , (Filed(4),Ndform) , (Filed(5),Ndtype) , (Filed(6),Ndnzwd) , (Filed(7),Nddens)
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
      CALL mmarc4(Zi,Zd)
!
! NOW READ "C", OR SCRATCH FILE WITH INTERMEDIATE RESULTS.
! IF NO "C" FILE AND THIS IS THE FIRST PASS, INITIALIZE "D" COLUMN TO ZERO.
!
      IF ( Ifile/=0 ) THEN
         Iurow1 = 1
         Iurown = Ndr
         Typeu = Ndtype
         IF ( Ipass==1 ) Typeu = Ndtype*Signc
         CALL unpack(*50,Ifile,Zdc(Idx4+1))
         GOTO 100
      ENDIF
 50   DO j = 1 , Ndr
         Zdc(Idx4+j) = 0
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
            idxx = Idx4 + idrow
! DOUBLE PRECISION
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
                  indxbs = indxb
                  indxb = indxb + 2 + irows*Nwdd
                  DO WHILE ( indxa<indxal )
                     irowa1 = Zi(indxa)
                     ntms = Zi(indxa+1)
                     irowan = irowa1 + ntms - 1
                     IF ( irowbn<irowa1 ) GOTO 105
                     IF ( irowan<irowb1 ) THEN
                        indxa = indxa + 2 + ntms*Nwdd
                     ELSE
                        irow1 = max0(irowa1,irowb1)
                        irown = min0(irowan,irowbn)
                        indxav = ((indxa+3)/2) + 2*(irow1-irowa1) - 1
                        indxbv = ((indxbs+3)/2) + 2*(irow1-irowb1) - 1
                        kcnt = (irown-irow1)*2 + 1
                        DO k = 1 , kcnt , 2
                           Zdc(idxx+i) = Zdc(idxx+i) + dcmplx(Zd(indxav+k),Zd(indxav+k+1))*dcmplx(Zd(indxbv+k),Zd(indxbv+k+1))
                        ENDDO
                        IF ( irowan>irowbn ) GOTO 105
                        indxa = indxa + 2 + ntms*Nwdd
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
! DOUBLE PRECISION
            DO i = 1 , Ncolpp
               indxal = Zi(indxa+1) + Iax - 1
               icola = Ibrow + i
               IF ( icola/=iabs(Zi(indxa)) ) GOTO 200
               indxa = indxa + 2
               DO WHILE ( icola>=irowb1 )
                  IF ( icola<=irowbn ) THEN
                     indxbv = 2*(icola-irowb1) + (indxb+3)/2
                     IF ( Zd(indxbv)==0. ) EXIT
                     DO WHILE ( indxa<indxal )
                        irowa1 = Zi(indxa)
                        ntms = Zi(indxa+1)
                        irowan = irowa1 + ntms - 1
                        indxav = ((indxa+3)/2)
                        DO k = irowa1 , irowan
                           Zdc(Idx4+k) = Zdc(Idx4+k) + dcmplx(Zd(indxav),Zd(indxav+1))*dcmplx(Zd(indxbv),Zd(indxbv+1))
                           indxav = indxav + 2
                        ENDDO
                        indxa = indxa + 2 + ntms*Nwdd
                     ENDDO
                     EXIT
                  ELSE
                     indxb = indxb + 2 + irows*Nwdd
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
 150  CALL pack(Zdc(Idx4+1),Ofile,Filed)
   ENDDO
   GOTO 99999
 200  WRITE (Iwr,99001) icola , Zi(indxa) , Iax , indxa
99001 FORMAT (' UNEXPECTED COLUMN FOUND IN PROCESSING MATRIX A',/,' COLUMN EXPECTED:',I6,/,' COLUND FOUND   :',I6,/,' IAX =',I7,    &
             &' INDXA=',I7)
   CALL mesage(-61,0,0)
99999 RETURN
END SUBROUTINE mma314