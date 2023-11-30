
 
SUBROUTINE mma2(Zi,Zr,Zd,Zc,Zdc)
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
   INTEGER i , iavail , ibuf1 , ibuf2 , ibuf3 , ibuf4 , indxb , indxd , itest , jbegn , jend , k , kone , kzero , len , m ,         &
         & module(3) , npass
!
!     MMA2 PERFORMS THE MATRIX OPERATION USING METHODS 20 AND 21
!       (+/-)A(T & NT) * B (+/-)C = D
!
!     MMA2 IS DESIGNED AS FOLLOWS:
!       1.  THIS IS FOR "A" NON-TRANSPOSED AND TRANSPOSED
!       2.  UNPACK AS MANY COLUMNS OF "B" INTO MEMORY AS POSSIBLE
!           LEAVING SPACE FOR A COLUMN OF "D" FOR EVERY COLUMN "B" READ.
!       3.  INITIALIZE EACH COLUMN OF "D" WITH THE DATA FROM "C".
!       4.  CALL UNPACK TO READ MATRICES "B" AND "C".
!       5.  FOR METHOD 20, CALL UNPACK TO READ COLUMNS OF MATRIX "A".
!       6.  FOR METHOD 21, CALL MMARC1,2,3,4 TO READ COLUMNS OF MATRIX "A"
!           INTO MEMORY IN COMPACT FORM.
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Nout) , (Ksystm(58),Ksys58)
   !>>>>EQUIVALENCE (Filea(2),Nac) , (Filea(3),Nar) , (Filea(4),Naform) , (Filea(5),Natype) , (Filea(6),Nanzwd) , (Filea(7),Nadens)
   !>>>>EQUIVALENCE (Fileb(2),Nbc) , (Fileb(3),Nbr) , (Fileb(4),Nbform) , (Fileb(5),Nbtype) , (Fileb(6),Nbnzwd) , (Fileb(7),Nbdens)
   !>>>>EQUIVALENCE (Filec(2),Ncc) , (Filec(3),Ncr) , (Filec(4),Ncform) , (Filec(5),Nctype) , (Filec(6),Ncnzwd) , (Filec(7),Ncdens)
   !>>>>EQUIVALENCE (Filed(2),Ndc) , (Filed(3),Ndr) , (Filed(4),Ndform) , (Filed(5),Ndtype) , (Filed(6),Ndnzwd) , (Filed(7),Nddens)
!
   DATA module/4HMMA2 , 4H     , 4H    /
   DATA kzero/1H0/
   DATA kone/1H1/
   DATA jbegn/4HBEGN/ , jend/3HEND/
   IF ( Nastor==1 .OR. Ksys58==20 ) module(2) = kzero
   IF ( Nastor==2 .OR. Ksys58==21 ) module(2) = kone
   module(3) = jbegn
   CALL conmsg(module,3,0)
   Incru = 1
   Typei = Ndtype
   Typep = Ndtype
   Nwdd = Nwords(Ndtype)
   Irfile = Filea(1)
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
   Idx = 1 + Nwdd*Nar
   IF ( Nastor==2 .OR. Ksys58==21 ) THEN
!
! REDEFINE IDX AND INSURE A QUAD WORD BOUNDARY FOR COMPLEX DOUBLE
!
      Idx = 1 + Nwdd*Nar + Nar
      itest = mod(Idx,4)
      IF ( itest/=1 ) THEN
         IF ( itest==0 ) Idx = Idx + 1
         IF ( itest==2 ) Idx = Idx + 3
         IF ( itest==3 ) Idx = Idx + 2
      ENDIF
   ENDIF
   Idx2 = ((Idx+1)/2) - 1
   Idx4 = (Idx+1)/4
   ibuf1 = Nz - Sysbuf
   ibuf2 = ibuf1 - Sysbuf
   IF ( Filec(1)==0 ) THEN
      ibuf4 = ibuf2 - Sysbuf
   ELSE
      ibuf3 = ibuf2 - Sysbuf
      ibuf4 = ibuf3 - Sysbuf
   ENDIF
   Lasmem = ibuf4 - 1
   Iprow1 = 1
   Iprown = Ndr
   Incrp = 1
   Sign = 1.0
   CALL gopen(Filea,Zr(ibuf1),Rdrew)
   CALL gopen(Fileb,Zr(ibuf2),Rdrew)
   IF ( Filec(1)/=0 ) CALL gopen(Filec,Zr(ibuf3),Rdrew)
   CALL gopen(Filed,Zr(ibuf4),Wrtrew)
   Filed(2) = 0
   Filed(6) = 0
   Filed(7) = 0
!
!   DETERMINE HOW MANY COLUMNS OF "B" CAN BE READ INTO MEMORY AND HOW
!   MANY COLUMNS OF "D" CAN BE HELD IN MEMORY FOR ONE PASS
!
   iavail = Lasmem - Idx + 1
!
!   NCOLPP  -  NUMBER OF COLUMNS OF "B" THAT CAN BE READ IN ONE PASS
!   NPASS   -  NUMBER OF PASSES NEEDED TO READ ENTIRE "B" MATRIX
!
   Nwddndr = Nwdd*Ndr
   Nwddnbr = Nwdd*Nbr
   Ncolpp = iavail/(2+Nwddnbr+Nwddndr)
   IF ( Ncolpp<=0 ) CALL mesage(-8,iavail+Nwddnbr+Nwddndr,module)
   IF ( Ncolpp>Nbc ) Ncolpp = Nbc
   npass = ((Nbc-1)/Ncolpp) + 1
   Ibx = Idx + Ncolpp*Nwddndr
   DO m = 1 , npass
      Ipass = m
      IF ( m==npass ) Ncolpp = Nbc - (Ncolpp*(npass-1))
      CALL rewind(Filea)
      CALL skprec(Filea,1)
      indxb = Ibx
      indxd = Idx
      Typeu = Ndtype*Signab
      DO i = 1 , Ncolpp
         Iurow1 = -1
         CALL unpack(*20,Fileb,Zr(indxb+2))
         Zi(indxb) = Iurow1
         Zi(indxb+1) = Iurown
         GOTO 40
! NULL COLUMN READ ON "B"
 20      Zi(indxb) = 0
         Zi(indxb+1) = 0
 40      indxb = indxb + Nwddnbr + 2
      ENDDO
      IF ( Filec(1)==0 .OR. Signc==0 ) THEN
!
! "C" MATRIX IS NULL OR "SIGNC" IS ZERO
!
         len = Idx + Ncolpp*Nwddndr - 1
         DO k = Idx , len
            Zr(k) = 0.
         ENDDO
      ELSE
         Typeu = Ndtype*Signc
         Iurow1 = 1
         Iurown = Ncr
         DO i = 1 , Ncolpp
            CALL unpack(*50,Filec,Zr(indxd))
            GOTO 60
!
! NULL COLUMN READ ON "C"
!
 50         len = indxd + Nwddndr - 1
            DO k = indxd , len
               Zr(k) = 0.0
            ENDDO
 60         indxd = indxd + Nwddndr
         ENDDO
      ENDIF
!
! PROCESS ALL OF THE COLUMNS OF "A"
!
      IF ( Ksys58/=21 ) THEN
         IF ( Ksys58/=20 ) THEN
            IF ( Nastor==2 ) GOTO 100
         ENDIF
         IF ( Ndtype==1 ) CALL mma201(Zi,Zr)
         IF ( Ndtype==2 ) CALL mma202(Zi,Zd)
         IF ( Ndtype==3 ) CALL mma203(Zi,Zc)
         IF ( Ndtype==4 ) CALL mma204(Zi,Zd,Zdc)
         CYCLE
      ENDIF
 100  IF ( Ndtype==1 ) CALL mma211(Zi,Zr)
      IF ( Ndtype==2 ) CALL mma212(Zi,Zd)
      IF ( Ndtype==3 ) CALL mma213(Zi,Zc)
      IF ( Ndtype==4 ) CALL mma214(Zi,Zd,Zdc)
   ENDDO
   CALL close(Filea,Clsrew)
   CALL close(Fileb,Clsrew)
   CALL close(Filec,Clsrew)
   CALL close(Filed,Clsrew)
   module(3) = jend
   CALL conmsg(module,3,0)
END SUBROUTINE mma2