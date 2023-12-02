!*==mintrp.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mintrp(Ni,Xi,Nd,Xd,Type,Symm1,Symk1,Dz,Infile,Outfil,Scr,Scr1,G,Ncore,Nogo,Ipres)
   USE c_mpyadx
   USE c_packx
   USE c_saddx
   USE c_system
   USE c_unpakx
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ni
   REAL , DIMENSION(1) :: Xi
   INTEGER :: Nd
   REAL , DIMENSION(2) :: Xd
   INTEGER :: Type
   INTEGER :: Symm1
   INTEGER :: Symk1
   REAL :: Dz
   INTEGER :: Infile
   INTEGER :: Outfil
   INTEGER :: Scr
   INTEGER :: Scr1
   REAL , DIMENSION(1) :: G
   INTEGER :: Ncore
   INTEGER :: Nogo
   INTEGER :: Ipres
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: ai , ar
   INTEGER :: buff , gpoint , i , isng , ity , j , jj , k , kd , kt , ncol , nii
   INTEGER , DIMENSION(2) , SAVE :: name
   LOGICAL :: nimag , spec
   EXTERNAL close , gopen , lsplin , mesage , mpyad , pack , rdtrl , sadd , skprec , ssplin , unpack , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!
!
   !>>>>EQUIVALENCE (Alpha(1),Ar) , (Alpha(2),Ai)
!
   DATA name/4HMINT , 4HRP  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!-----------------------------------------------------------------------
!
         spec = .FALSE.
         Nogo = 0
!
!     DETERMINE TYPE OF CALL FOR G
!     NEGATIVE VALUE FOR KO CALL LSPLIN, POSITIVE CALL SSPLIN
!
!
!     CHECK CORE NEED AT LEAST 1 BUFFER + G
!
         ity = iabs(Type)
         kd = 0
         IF ( ity>3 ) kd = 1
         ncol = (1+kd)*Nd
         IF ( sysbuf+ncol*Ni>Ncore ) CALL mesage(-8,0,name)
!
!     PROTECT AGAINST BAD CALL
         IF ( Symk1<0 ) Symk1 = -1
         IF ( Symm1<0 ) Symm1 = -1
         IF ( Symk1>0 ) Symk1 = 1
         IF ( Symm1>0 ) Symm1 = 1
!     TRANSPOSE FLAG ON
         kt = 1
!     SPECIAL CASE
         IF ( Nd==1 .AND. ity<4 ) THEN
!
!     TEST FOR SPECIAL CASE
!
            nii = 2*Ni
            k = 0
            DO i = 1 , nii , 2
               k = k + 1
               IF ( Xi(i)==Xd(1) .AND. Xi(i+1)==Xd(2) ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
         IF ( Type<0 ) THEN
            nii = 2*Ni
            DO i = 1 , nii , 2
               Xi(i) = 0.0
            ENDDO
            nii = 2*Nd
            DO i = 1 , nii , 2
               Xd(i) = 0.0
            ENDDO
            CALL lsplin(Ni,Xi,Nd,Xd,Symk1,kd,kt,Dz,-1.0,-1.0,1.0,G,Ncore,isng)
            IF ( isng==2 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
            CALL ssplin(Ni,Xi,Nd,Xd,Symm1,Symk1,kd,kt,Dz,G,Ncore,isng)
            IF ( isng==2 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!     PUT OUT G
         buff = Ncore - sysbuf + 1
         nimag = .TRUE.
         IF ( ity==3 .OR. ity==6 ) nimag = .FALSE.
         IF ( .NOT.(nimag) ) THEN
            iti = Scr
            Scr = Outfil
            Outfil = iti
         ENDIF
         ito = 1
         jj = ncol
         iti = 1
         nn = Ni
         b(3) = Ni
         b(5) = 1
         gpoint = 1
         spag_nextblock_1 = 2
      CASE (2)
         incr = 1
         j = 1
         ii = 1
         b(1) = Scr
         b(2) = 0
         b(4) = 2
         b(6) = 0
         b(7) = 0
         CALL gopen(Scr,G(buff),1)
         DO i = j , jj
            CALL pack(G(gpoint),Scr,b)
            gpoint = gpoint + Ni
         ENDDO
         CALL close(Scr,1)
         CALL wrttrl(b)
         IF ( .NOT.(spec) ) THEN
!
!     MULT INFILE BY G
!
            c(1) = 0
            a(1) = Infile
            CALL rdtrl(a)
            d(1) = Outfil
            d(3) = a(3)
            d(4) = 2
            d(5) = a(5)
            IF ( ity==2 .OR. ity==5 ) d(5) = 1
            IF ( d(5)==1 .AND. a(5)==4 ) d(5) = 2
            nwords = Ncore
            nt = 0
            isab = 1
            ipre = Ipres
            scrm = Scr1
            CALL mpyad(G,G,G)
            CALL wrttrl(d)
            IF ( .NOT.(nimag) ) THEN
!
!     IMAG PART ONLY WANTED
!
               nmat = 1
               lcore = Ncore
               ma(1) = Outfil
               CALL rdtrl(ma)
               ita = 3
               alpha(1) = (0.0,-1.0)
               mc(1) = Scr
               mc(2) = ma(2)
               mc(3) = ma(3)
               mc(4) = 2
               mc(5) = ma(5)
               mc(6) = 0
               mc(7) = 0
               ai = -1.0D0
               IF ( ma(5)==4 ) ita = 4
               IF ( ita==4 ) ar = 0.0D0
               CALL sadd(G,G)
               CALL wrttrl(mc)
            ENDIF
         ENDIF
         RETURN
      CASE (3)
!
!     PACK OUT COLUMN OF INFILE
!
         a(1) = Infile
         CALL rdtrl(a)
         buff = Ncore - sysbuf + 1
         CALL gopen(Infile,G(buff),0)
         incru = 1
         in = 1
         nnn = a(3)
         iout = a(5)
         IF ( k/=1 ) THEN
            k = k - 1
            CALL skprec(Infile,k)
         ENDIF
         CALL unpack(*20,Infile,G)
         CALL close(Infile,1)
         spec = .TRUE.
         Scr = Outfil
         iti = a(5)
         nn = a(3)
         jj = 1
         gpoint = 1
         IF ( ity==3 ) gpoint = 2
         ito = 1
         IF ( ity==1 ) ito = 3
         IF ( a(5)==4 ) ito = ito + 1
         b(3) = a(3)
         b(5) = ito
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 20      CALL mesage(-7,0,name)
         spag_nextblock_1 = 4
      CASE (4)
         Nogo = 1
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE mintrp
