
SUBROUTINE fbsf(Zs,Zd)
!
!     GIVEN A LOWER TRIANGULAR FACTOR WITH DIAGONAL SUPERIMPOSED, AND
!     WRITTEN WITH TRAILING STRING DEFINITION WORDS, FBS WILL PERFORM
!     THE FORWARD-BACKWARD SUBSTITUTION NECESSARY TO SOLVE A LINEAR
!     SYSTEM OF EQUATIONS.
!
!     OPEN CORE IS DEFINED AS FOLLOWS
!
!     ZS(   1         ) - FIRST RIGHT HAND VECTOR ON FILE DBB
!                         (SIZE = NCOL*NWDS)
!                         NCOL = NUMBER OF COLUMNS (ROWS) IN LOWER
!                                TRIANGULAR MATRIX
!                         NWDS = 1, IF MATRICES ARE REAL SINGLE
!                              = 2, IF MATRICES ARE REAL DOUBLE OR
!                                COMPLEX SINGLE
!                              = 4, IF MATRICES ARE COMPLEX DOUBLE
!     ZS( NCOL*NWDS+1 ) - NEXT RIGHT HAND VECTOR
!         .
!         .               ( "KN" RIGHT HAND VECTORS WILL BE LOADED INTO
!         .               MEMORY)
!         .
!     ZS( BUF1        ) - BUFFER FOR FILE WITH RIGHT HAND VECTORS
!                         AND FOR SOLUTION VECTORS
!     ZS( BUF2        ) - BUFFER FOR FILE WITH TRIANGULAR MATRIX
!
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Cdp , Csp , Dbb(7) , Dbl(7) , Dbu(7) , Dbx(7) , Eofnrw , Eol , I1 , I2 , Incr1 , Incr2 , Itype1 , Itype2 , Itype3 , Ix , &
         & Iy , J1 , J2 , Ksys94 , Lcore , Lout , Nl , Norew , Nout , Prc(2) , Prec , Rd , Rdp , Rdrew , Rew , Rlcmpx(4) , Rsp ,    &
         & Scrx , Sign , Skip(91) , Sysbuf , Typeb , Typex , Words(4) , Wrt , Wrtrew
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   DOUBLE PRECISION Xd(2) , Yd(2)
   REAL Xs(4) , Ys(4)
   COMMON /fbsx  / Dbl , Dbu , Dbb , Dbx , Lcore , Prec , Sign , Scrx
   COMMON /logout/ Lout
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp , Csp , Cdp
   COMMON /packx / Itype1 , Itype2 , I1 , J1 , Incr1
   COMMON /system/ Sysbuf , Nout , Skip , Ksys94
   COMMON /type  / Prc , Words , Rlcmpx
   COMMON /unpakx/ Itype3 , I2 , J2 , Incr2
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zblpkx/ Yd , Iy
   COMMON /zntpkx/ Xd , Ix , Eol
!
! Dummy argument declarations
!
   DOUBLE PRECISION Zd(1)
   REAL Zs(1)
!
! Local variable declarations
!
   INTEGER begn , block(15) , buf1 , buf2 , dbl1 , end , i , icdpsg , icspsg , j , k , k1 , kn , l , last , ll , ln , nbrlod ,      &
         & need , nnn , nnndbl , npass , nterms , nvecs , nwds , opcls , oprd , opwrt , rc , subnam(2) , switch , typel , wds
   LOGICAL ident
!
! End of declarations
!
   EQUIVALENCE (Dbl(2),Nl) , (Dbb(5),Typeb) , (Dbx(5),Typex) , (Xd(1),Xs(1)) , (Yd(1),Ys(1))
   DATA subnam/4HFBSF , 4H    /
   DATA begn/4HBEGN/
   DATA end/4HEND /
!
!     GENERAL INITIALIZATION
!
   buf2 = Lcore - Sysbuf
   buf1 = buf2 - Sysbuf
   rc = Rlcmpx(Typeb)
   typel = Dbl(5)
   wds = Words(typel)
   nwds = wds*Nl
   nbrlod = Dbb(2)
   ident = .FALSE.
   IF ( Dbb(4)==8 ) ident = .TRUE.
   IF ( ident ) nbrlod = Nl
   switch = 1
   IF ( typel==Rsp .AND. rc==2 ) switch = 2
   IF ( typel==Rdp .AND. rc==2 ) switch = 3
   dbl1 = Dbl(1)
   nnn = buf1 - 1
   nvecs = nnn/nwds
   IF ( nvecs==0 ) CALL mesage(-8,nwds-nnn,subnam)
   IF ( switch/=1 ) nvecs = nvecs/2
   npass = (nbrlod+nvecs-1)/nvecs
   subnam(2) = begn
   CALL conmsg(subnam,2,0)
   npass = (nbrlod+nvecs-1)/nvecs
   IF ( npass/=1 ) THEN
      need = nwds*nbrlod + 2*Sysbuf
      WRITE (Lout,99001) npass , need
99001 FORMAT (I4,' PASSES REQUIRED, OPEN CORE NEEDS TO BE ',I7,' TO ELIMINATE THIS')
   ENDIF
   I2 = 1
   J2 = Nl
   Incr2 = 1
   I1 = 1
   J1 = Nl
   Incr1 = 1
   Itype1 = typel
   Itype2 = Typex
   Itype3 = Sign*typel
   Dbx(2) = 0
   Dbx(6) = 0
   Dbx(7) = 0
   nnndbl = nnn/2
   nterms = Rlcmpx(typel)*Nl
   k1 = 1
   oprd = Rdrew
   opwrt = Wrtrew
   block(1) = Dbl(1)
!
!     OPEN LOWER TRIANGULAR FACTOR FILE (DBL1)
!
   CALL gopen(dbl1,Zs(buf2),Rdrew)
!
!     OPEN RIGHT HAND VECTORS FILE (DBB) AND COMPUTE EXTENT OF THIS PASS
!
 100  kn = min0(k1+nvecs-1,nbrlod)
   last = (kn-k1+1)*nwds
   opcls = Norew
   IF ( kn==nbrlod ) opcls = Rew
   IF ( ident ) THEN
!
!     SPECIAL CASE - GENERATE IDENTITY MATRIX
!
      DO k = 1 , nnndbl
         Zd(k) = 0.0D+0
      ENDDO
      l = 0
      IF ( typel==2 ) THEN
         DO k = k1 , kn
            Zd(l+k) = 1.0D+0
            l = l + nterms
         ENDDO
      ELSEIF ( typel==3 ) THEN
         DO k = k1 , kn
            Zs(l+2*k-1) = 1.0
            l = l + nterms
         ENDDO
      ELSEIF ( typel==4 ) THEN
         DO k = k1 , kn
            Zd(l+2*k-1) = 1.0D+0
            l = l + nterms
         ENDDO
      ELSE
         DO k = k1 , kn
            Zs(l+k) = 1.0
            l = l + nterms
         ENDDO
      ENDIF
   ELSE
      CALL gopen(Dbb,Zs(buf1),oprd)
      IF ( switch==2 ) THEN
!
!     SPECIAL CASE - FACTOR IS RSP AND VECTORS ARE CSP
!
         last = 2*(kn-k1+1)*nwds
         l = 0
         DO k = 1 , nnndbl
            Zd(k) = 0.0D+0
         ENDDO
         DO k = k1 , kn
            icspsg = Csp*Sign
            CALL intpk(*110,Dbb,0,icspsg,0)
            DO
               CALL zntpki
               Zs(l+Ix) = Xs(1)
               Zs(l+Ix+Nl) = Xs(2)
               IF ( Eol/=0 ) EXIT
            ENDDO
 110        l = l + 2*Nl
         ENDDO
      ELSEIF ( switch==3 ) THEN
!
!     SPECIAL CASE - FACTOR IS RDP AND VECTORS ARE CDP
!
         last = 2*(kn-k1+1)*nwds
         l = 0
         DO k = 1 , nnndbl
            Zd(k) = 0.0D+0
         ENDDO
         DO k = k1 , kn
            icdpsg = Cdp*Sign
            CALL intpk(*120,Dbb,0,icdpsg,0)
            DO
               CALL zntpki
               Zd(l+Ix) = Xd(1)
               Zd(l+Ix+Nl) = Xd(2)
               IF ( Eol/=0 ) EXIT
            ENDDO
 120        l = l + 2*Nl
         ENDDO
      ELSE
!
!     NORMAL CASE - FILL CORE WITH RIGHT HAND VECTORS
!
         DO l = 1 , last , nwds
            CALL unpack(*130,Dbb,Zs(l))
            CYCLE
 130        ln = l + nwds - 1
            DO ll = l , ln
               Zs(ll) = 0.
            ENDDO
         ENDDO
      ENDIF
!
!    CLOSE RIGHT HAND VECTORS FILE (DBB).
!    START FORWARD-BACKWARD SUBSTITUTION ON RIGHT HAND VECTORS NOW IN CORE
!
      CALL close(Dbb,opcls)
   ENDIF
   CALL rewind(dbl1)
   CALL fwdrec(*200,dbl1)
!
   j = typel
   IF ( j==2 ) THEN
      CALL fbs2(block,Zs,Zs(last),nwds)
   ELSEIF ( j==3 ) THEN
      CALL fbs3(block,Zs,Zs(last),nwds)
   ELSEIF ( j==4 ) THEN
      CALL fbs4(block,Zs,Zs(last),nwds)
   ELSE
      CALL fbs1(block,Zs,Zs(last),nwds)
   ENDIF
!
!     OPEN AND PACK SOLUTION VECTORS ONTO OUTPUT FILE (DBX)
!
   CALL gopen(Dbx,Zs(buf1),opwrt)
   IF ( switch==2 ) THEN
!
!     SPECIAL CASE - FACTOR IS RSP AND VECTORS ARE CSP, CALL BLDPK
!
      l = 0
      DO k = k1 , kn
         CALL bldpk(Csp,Typex,Dbx,0,0)
         DO i = 1 , Nl
            Ys(1) = Zs(l+i)
            Ys(2) = Zs(l+i+Nl)
            Iy = i
            CALL zblpki
         ENDDO
         CALL bldpkn(Dbx,0,Dbx)
         l = l + 2*Nl
      ENDDO
   ELSEIF ( switch==3 ) THEN
!
!     SPECIAL CASE - FACTOR IS RDP AND VECTORS ARE CDP, CALL BLDPK
!
      l = 0
      DO k = k1 , kn
         CALL bldpk(Cdp,Typex,Dbx,0,0)
         DO i = 1 , Nl
            Yd(1) = Zd(l+i)
            Yd(2) = Zd(l+i+Nl)
            Iy = i
            CALL zblpki
         ENDDO
         CALL bldpkn(Dbx,0,Dbx)
         l = l + 2*Nl
      ENDDO
   ELSE
!
!     NORMAL CASE - CALL PACK
!
      DO l = 1 , last , nwds
         CALL pack(Zs(l),Dbx,Dbx)
      ENDDO
   ENDIF
!
!     CLOSE OUTPUT FILE, AND TEST FOR MORE PASSES
!
   CALL close(Dbx,opcls)
   IF ( kn==nbrlod ) GOTO 300
   k1 = kn + 1
   oprd = Rd
   opwrt = Wrt
   GOTO 100
!
!     ERROR
!
 200  CALL mesage(-2,dbl1,subnam)
!
!     JOB DONE. CLOSE TRIANGULAR FACTOR FILE.
!
 300  CALL close(dbl1,Rew)
   subnam(2) = end
   CALL conmsg(subnam,2,0)
END SUBROUTINE fbsf
