
SUBROUTINE mma4(Zi,Zr,Zd,Zc,Zdc)
   IMPLICIT NONE
   INCLUDE 'MMACOM.COM'
   INTEGER Cls , Clsrew , Filea(7) , Fileb(7) , Filec(7) , Filed(7) , Incrp , Incru , Iprc(2) , Iprow1 , Iprown , Irc(4) , Iurow1 , &
         & Iurown , Ksys58 , Ksystm(152) , Nac , Nadens , Naform , Nanzwd , Nar , Natype , Nbc , Nbdens , Nbform , Nbnzwd , Nbr ,   &
         & Nbtype , Ncc , Ncdens , Ncform , Ncnzwd , Ncr , Nctype , Ndc , Nddens , Ndform , Ndnzwd , Ndr , Ndtype , Nout , Nwords(4)&
         & , Nz , Rd , Rdrew , Scrtch , Signab , Signc , Sysbuf , Typei , Typep , Typeu , Wrt , Wrtrew
   REAL Prec1 , T , Time
   COMMON /mpyadx/ Filea , Fileb , Filec , Filed , Nz , T , Signab , Signc , Prec1 , Scrtch , Time
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Cls
   COMMON /packx / Typei , Typep , Iprow1 , Iprown , Incrp
   COMMON /system/ Ksystm
   COMMON /type  / Iprc , Nwords , Irc
   COMMON /unpakx/ Typeu , Iurow1 , Iurown , Incru
   COMPLEX Zc(2)
   DOUBLE PRECISION Zd(2)
   DOUBLE COMPLEX Zdc(2)
   INTEGER Zi(2)
   REAL Zr(2)
   INTEGER i , ibuf1 , ibuf2 , ibuf3 , ibuf4 , ibx2 , indxd , itest , jbegn , jend , k , kone , kzero , lasmems , len , module(3)
!
!     MMA4 PERFORMS THE MATRIX OPERATION USING METHODS 40 AND 41
!       (+/-)A(T & NT) * B (+/-)C = D
!
!     MMA4 IS DESIGNED AS FOLLOWS:
!       1.  THIS IS FOR "A" NON-TRANSPOSED AND TRANSPOSED
!       2.  PACK (IN COMPACT FORM) AS MANY COLUMNS OF THE "B" MATRIX INTO
!           MEMORY AS POSSIBLE LEAVING SPACE FOR A FULL COLUMN OF THE
!           "D" MATRIX FOR EACH COLUMN OF THE "B" MATRIX READ.
!           SEE SUBROUTINES MMARM1,2,3,4 FOR FORMAT OF COMPACT FORM.
!       3.  INITIALIZE EACH COLUMN OF "D" WITH THE DATA FROM "C".
!       4.  CALL UNPACK TO READ MATRIX "C".
!       5.  FOR METHOD 40, CALL UNPACK TO READ COLUMNS OF MATRIX "A".
!       6.  FOR METHOD 41, CALL MMARC1,2,3,4 TO READ COLUMNS OF "A" INTO
!           MEMORY IN COMPACT FORM.
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Nout)
   !>>>>EQUIVALENCE (Ksystm(58),Ksys58)
   !>>>>EQUIVALENCE (Filea(2),Nac) , (Filea(3),Nar) , (Filea(4),Naform) , (Filea(5),Natype) , (Filea(6),Nanzwd) , (Filea(7),Nadens)
   !>>>>EQUIVALENCE (Fileb(2),Nbc) , (Fileb(3),Nbr) , (Fileb(4),Nbform) , (Fileb(5),Nbtype) , (Fileb(6),Nbnzwd) , (Fileb(7),Nbdens)
   !>>>>EQUIVALENCE (Filec(2),Ncc) , (Filec(3),Ncr) , (Filec(4),Ncform) , (Filec(5),Nctype) , (Filec(6),Ncnzwd) , (Filec(7),Ncdens)
   !>>>>EQUIVALENCE (Filed(2),Ndc) , (Filed(3),Ndr) , (Filed(4),Ndform) , (Filed(5),Ndtype) , (Filed(6),Ndnzwd) , (Filed(7),Nddens)
!
   DATA module/4HMMA4 , 4H     , 4H    /
   DATA kzero/1H0/
   DATA kone/1H1/
   DATA jbegn/4HBEGN/ , jend/3HEND/
   module(3) = jbegn
   IF ( Nastor==1 ) module(2) = kzero
   IF ( Nastor==2 ) module(2) = kone
   CALL conmsg(module,3,0)
   Incru = 1
   Typei = Ndtype
   Typep = Ndtype
   Nwdd = Nwords(Ndtype)
   Nwdb = Nwords(Nbtype)
!
!   OPEN CORE ALLOCATION AS FOLLOWS:
!     Z( 1        ) = ARRAY FOR ONE COLUMN OF "A" MATRIX
!     Z( IBX      ) = ARRAY FOR MULTIPLE COLUMNS OF "B" MATRIX
!     Z( IDX      ) = ARRAY FOR MULTIPLE COLUMNS OF "D" MATRIX
!        THROUGH
!     Z( LASMEM   )
!     Z( IBUF4    ) = BUFFER FOR "D" FILE
!     Z( IBUF3    ) = BUFFER FOR "C" FILE
!     Z( IBUF2    ) = BUFFER FOR "B" FILE
!     Z( IBUF1    ) = BUFFER FOR "A" FILE
!     Z( NZ       ) = END OF OPEN CORE THAT IS AVAILABLE
!
   Ibx = 1 + Nwdd*Nar
   IF ( Nastor==2 .OR. Ksys58==41 ) Ibx = 1 + Nwdd*Nar + Nar
   IF ( mod(Ibx,2)==0 ) Ibx = Ibx + 1
   ibx2 = ((Ibx+1)/2)
   ibuf1 = Nz - Sysbuf
   ibuf2 = ibuf1 - Sysbuf
   ibuf3 = ibuf2 - Sysbuf
   ibuf4 = ibuf3 - Sysbuf
   lasmems = ibuf4 - 1
!
! INSURE THAT LASMEM IS ON A QUAD WORD BOUNDARY TO ALLOW FOR COMPLEX
! DOUBLE DATA TO BE REFERENCED FOR "D" MATRIX COLUMNS
!
   itest = mod(lasmems,4)
   IF ( itest/=1 ) THEN
      IF ( itest==0 ) lasmems = lasmems - 3
      IF ( itest==2 ) lasmems = lasmems - 5
      IF ( itest==3 ) lasmems = lasmems - 6
   ENDIF
   Iprow1 = 1
   Iprown = Ndr
   Incrp = 1
   CALL gopen(Filea,Zr(ibuf1),Rdrew)
   CALL gopen(Fileb,Zr(ibuf2),Rdrew)
   IF ( Filec(1)/=0 .AND. Signc/=0 ) CALL gopen(Filec,Zr(ibuf3),Rdrew)
   Ipass = 0
   Ircoln = 0
   Nwddndr = Nwdd*Ndr
   CALL gopen(Filed,Zr(ibuf4),Wrtrew)
   Filed(2) = 0
   Filed(6) = 0
   Filed(7) = 0
 100  Ipass = Ipass + 1
   Ircol1 = Ircoln + 1
   Ircoln = Nbc
   Irfile = Fileb(1)
   Sign = Signab
   Lasmem = lasmems - Ibx
   IF ( Ipass/=1 ) CALL dsspos(Irfile,Irpos(1),Irpos(2),Irpos(3))
   IF ( Ndtype==1 ) CALL mmarm1(Zi(Ibx),Zr(Ibx),Nwddndr)
   IF ( Ndtype==2 ) CALL mmarm2(Zi(Ibx),Zd(ibx2),Nwddndr)
   IF ( Ndtype==3 ) CALL mmarm3(Zi(Ibx),Zc(ibx2),Nwddndr)
   IF ( Ndtype==4 ) CALL mmarm4(Zi(Ibx),Zd(ibx2),Nwddndr)
   Ncolpp = Ircoln - Ircol1 + 1
   Ibrow = Ircol1 - 1
   Idx = Lasmem + Ibx
   Idx2 = ((Idx+1)/2) - 1
   Idx4 = (Idx+1)/4
   IF ( Ipass/=1 ) THEN
      CALL rewind(Filea)
      CALL skprec(Filea,1)
   ENDIF
!
! NOW READ INTO MEMORY THE "C" FILE.
! READ AS MANY COLUMNS OF THIS AS WERE READ OF THE "B" MATRIX
!
   IF ( Filec(1)==0 .OR. Signc==0 ) THEN
!
! "C" MATRIX IS NULL OR "SIGNC" IS ZERO
!
      len = Idx + Ncolpp*Nwddndr - 1
      DO k = Idx , len
         Zr(k) = 0.0
      ENDDO
   ELSE
      indxd = Idx
      Typeu = Ndtype*Signc
      Iurow1 = 1
      Iurown = Ncr
      DO i = 1 , Ncolpp
         CALL unpack(*120,Filec,Zr(indxd))
         GOTO 140
!
! NULL COLUMN READ ON "C"
!
 120     len = indxd + Nwddndr - 1
         DO k = indxd , len
            Zr(k) = 0.0
         ENDDO
 140     indxd = indxd + Nwddndr
      ENDDO
   ENDIF
!
! PROCESS ALL OF THE COLUMNS OF "A" MATRIX
!
   Sign = 1
   IF ( Ksys58/=40 ) THEN
      IF ( Ksys58==41 ) GOTO 200
      IF ( Nastor==2 ) GOTO 200
   ENDIF
! PROCESS ALL OF THE COLUMNS OF "B", ADD "C" DATA ON FIRST PASS
   IF ( Ndtype==1 ) CALL mma401(Zi,Zr)
   IF ( Ndtype==2 ) CALL mma402(Zi,Zd)
   IF ( Ndtype==3 ) CALL mma403(Zi,Zc)
   IF ( Ndtype==4 ) CALL mma404(Zi,Zd,Zdc)
   GOTO 300
 200  IF ( Ndtype==1 ) CALL mma411(Zi,Zr)
   IF ( Ndtype==2 ) CALL mma412(Zi,Zd)
   IF ( Ndtype==3 ) CALL mma413(Zi,Zc)
   IF ( Ndtype==4 ) CALL mma414(Zi,Zd,Zdc)
 300  IF ( Ircoln<Nbc ) GOTO 100
!
! ALL COLUMNS OF A HAVE BEEN PROCESSED, MULTIPLICATION COMPLETE
!
   CALL close(Filea,Clsrew)
   CALL close(Fileb,Clsrew)
   CALL close(Filec,Clsrew)
   CALL close(Filed,Clsrew)
   module(3) = jend
   CALL conmsg(module,3,0)
END SUBROUTINE mma4