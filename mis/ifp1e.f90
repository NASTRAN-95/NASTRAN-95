
SUBROUTINE ifp1e(Isubc,Symseq,Nwdsc,I81,Icaste)
   IMPLICIT NONE
   LOGICAL Bit64
   INTEGER Blank , Case(200,2) , Casecc , Core(1) , Corey(401) , Iben , Icc , Ieor , Is , Istr , Isub , Lencc , Ncpw4 , Nmodes ,    &
         & Nset , Nsym , Nwpc
   REAL Corex(1) , Equal , Scr1 , Zzzzbb
   COMMON /ifp1a / Scr1 , Casecc , Is , Nwpc , Ncpw4 , Nmodes , Icc , Nset , Nsym , Zzzzbb , Istr , Isub , Lencc , Iben , Equal ,   &
                 & Ieor
   COMMON /xifp1 / Blank , Bit64
   COMMON /zzzzzz/ Corex
   INTEGER I81 , Icaste , Nwdsc
   INTEGER Isubc(5) , Symseq(1)
   INTEGER i , i2 , ido , ii , ik , iloop , imov , ip , iup , iword , j , k , none , nset1 , nwor
!
!     IFP1E WRITES CASECC OUT FROM CASE
!
   !>>>>EQUIVALENCE (Corex(1),Corey(1),Case(1,1)) , (Core(1),Corey(401))
   DATA none/4HNONE/
!
!     INITIALIZE
!
!
!     SKIP FILTER INTO SUBCASES FOR SYM SUBCASES
!
   DO i = 1 , 16
      IF ( Case(i,2)==0 ) Case(i,2) = Case(i,1)
   ENDDO
   IF ( Case(38,2)==0 ) Case(38,2) = Case(38,1)
   IF ( Nsym<=1 .OR. Case(16,2)/=0 ) THEN
      DO i = 1 , 7
         ik = (i-1)*3 + 17
         IF ( Case(ik,2)==0 ) THEN
            DO j = 1 , 3
               ii = ik + j - 1
               Case(ii,2) = Case(ii,1)
            ENDDO
         ENDIF
         iword = Case(ik,2)
         IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
         IF ( iword==none ) Case(ik,2) = 0
      ENDDO
   ENDIF
   DO j = 1 , 3
      DO i = 1 , 32
         k = 32*j + i + 6
         iword = Case(k,2)
         IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
         IF ( iword/=Blank ) GOTO 100
      ENDDO
      DO i = 1 , 32
         k = 32*j + i + 6
         Case(k,2) = Case(k,1)
      ENDDO
 100  ENDDO
   j = 129
   DO i = 1 , 5
      Case(j,2) = Isubc(i)
      j = j + 1
   ENDDO
   DO i = 135 , Lencc
      IF ( Case(i,2)==0 ) Case(i,2) = Case(i,1)
   ENDDO
!     IMOV = CASE(136,2)*100000000  !! VAX/IBM INTGER OVERFLOW FOR ANOMA
   imov = Case(136,2)
   IF ( imov<0 ) imov = 0
   imov = imov*100000000
   Case(136,2) = iabs(Case(136,2))
   Case(2,2) = Case(2,2) + imov
   Case(3,2) = Case(3,2) + imov
   IF ( Case(7,2)/=0 ) Case(7,2) = Case(7,2) + imov
   IF ( Case(8,2)/=0 ) Case(8,2) = Case(8,2) + imov
   Icaste = Case(8,2)
   DO iloop = 1 , Nmodes
      IF ( Case(1,2)>99999999 ) CALL ifp1d(-625)
!
!     CHECK FOR METHOD AND LOAD IN SAME SUBCASE
!
      IF ( Case(5,2)/=0 .AND. Case(4,2)+Case(6,2)+Case(7,2)/=0 ) CALL ifp1d(-627)
      IF ( Case(4,2)==Case(6,2) .AND. Case(4,2)/=0 .OR. Case(6,2)==Case(7,2) .AND. Case(6,2)/=0 .OR. Case(4,2)==Case(7,2) .AND.     &
         & Case(4,2)/=0 ) CALL ifp1d(-628)
      CALL write(Casecc,Case(1,2),Lencc,0)
      Case(1,2) = Case(1,2) + 1
      IF ( Case(16,2)>0 ) THEN
         ido = Case(Lencc,2)
         CALL write(Casecc,Symseq(1),ido,0)
      ENDIF
      IF ( Nset/=0 ) THEN
         ip = Nwdsc + 1
         DO i = 1 , Nset
            nwor = Core(ip)
            CALL write(Casecc,Core(ip-1),2,0)
            CALL write(Casecc,Core(ip+2),nwor,0)
            ip = ip + nwor + 3
         ENDDO
      ENDIF
      CALL write(Casecc,Core(1),0,1)
   ENDDO
   Nmodes = 1
   IF ( Nset/=0 ) THEN
!
!     REMOVE ALL SETS REFERING TO SUBCASE ONLY
!
      iup = Nwdsc
      ip = Nwdsc
      nset1 = Nset
      imov = 0
      DO i = 1 , Nset
         IF ( Core(ip+2)/=1 ) THEN
            imov = 1
            nset1 = nset1 - 1
            ip = ip + Core(ip+1) + 3
         ELSE
            IF ( imov/=0 ) THEN
               ido = Core(ip+1) + 3
               DO j = 1 , ido
                  ii = iup + j - 1
                  ik = ip + j - 1
                  Core(ii) = Core(ik)
               ENDDO
            ENDIF
            iup = iup + Core(ip+1) + 3
            ip = ip + Core(ip+1) + 3
         ENDIF
      ENDDO
      Nset = nset1
      I81 = iup
   ENDIF
   DO i = 1 , Lencc
      Case(i,2) = 0
      IF ( i>38 .AND. i<135 ) Case(i,2) = Blank
   ENDDO
   CALL ifp1f(*99999,iword,i2)
   DO i = 1 , 5
      Isubc(i) = Core(i2)
      i2 = i2 + 1
   ENDDO
99999 RETURN
END SUBROUTINE ifp1e