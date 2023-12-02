!*==ifp1e.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifp1e(Isubc,Symseq,Nwdsc,I81,Icaste)
   IMPLICIT NONE
   USE C_IFP1A
   USE C_XIFP1
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(5) :: Isubc
   INTEGER , DIMENSION(1) :: Symseq
   INTEGER :: Nwdsc
   INTEGER :: I81
   INTEGER :: Icaste
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(200,2) :: case
   INTEGER , DIMENSION(1) :: core
   INTEGER :: i , i2 , ido , ii , ik , iloop , imov , ip , iup , iword , j , k , nset1 , nwor
   INTEGER , SAVE :: none
   EXTERNAL ifp1d , ifp1f , mvbits , write
!
! End of declarations rewritten by SPAG
!
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
      IF ( case(i,2)==0 ) case(i,2) = case(i,1)
   ENDDO
   IF ( case(38,2)==0 ) case(38,2) = case(38,1)
   IF ( Nsym<=1 .OR. case(16,2)/=0 ) THEN
      DO i = 1 , 7
         ik = (i-1)*3 + 17
         IF ( case(ik,2)==0 ) THEN
            DO j = 1 , 3
               ii = ik + j - 1
               case(ii,2) = case(ii,1)
            ENDDO
         ENDIF
         iword = case(ik,2)
         IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
         IF ( iword==none ) case(ik,2) = 0
      ENDDO
   ENDIF
   SPAG_Loop_1_1: DO j = 1 , 3
      DO i = 1 , 32
         k = 32*j + i + 6
         iword = case(k,2)
         IF ( Bit64 ) CALL mvbits(Blank,0,32,iword,0)
         IF ( iword/=Blank ) CYCLE SPAG_Loop_1_1
      ENDDO
      DO i = 1 , 32
         k = 32*j + i + 6
         case(k,2) = case(k,1)
      ENDDO
   ENDDO SPAG_Loop_1_1
   j = 129
   DO i = 1 , 5
      case(j,2) = Isubc(i)
      j = j + 1
   ENDDO
   DO i = 135 , Lencc
      IF ( case(i,2)==0 ) case(i,2) = case(i,1)
   ENDDO
!     IMOV = CASE(136,2)*100000000  !! VAX/IBM INTGER OVERFLOW FOR ANOMA
   imov = case(136,2)
   IF ( imov<0 ) imov = 0
   imov = imov*100000000
   case(136,2) = iabs(case(136,2))
   case(2,2) = case(2,2) + imov
   case(3,2) = case(3,2) + imov
   IF ( case(7,2)/=0 ) case(7,2) = case(7,2) + imov
   IF ( case(8,2)/=0 ) case(8,2) = case(8,2) + imov
   Icaste = case(8,2)
   DO iloop = 1 , Nmodes
      IF ( case(1,2)>99999999 ) CALL ifp1d(-625)
!
!     CHECK FOR METHOD AND LOAD IN SAME SUBCASE
!
      IF ( case(5,2)/=0 .AND. case(4,2)+case(6,2)+case(7,2)/=0 ) CALL ifp1d(-627)
      IF ( case(4,2)==case(6,2) .AND. case(4,2)/=0 .OR. case(6,2)==case(7,2) .AND. case(6,2)/=0 .OR. case(4,2)==case(7,2) .AND.     &
         & case(4,2)/=0 ) CALL ifp1d(-628)
      CALL write(Casecc,case(1,2),Lencc,0)
      case(1,2) = case(1,2) + 1
      IF ( case(16,2)>0 ) THEN
         ido = case(Lencc,2)
         CALL write(Casecc,Symseq(1),ido,0)
      ENDIF
      IF ( Nset/=0 ) THEN
         ip = Nwdsc + 1
         DO i = 1 , Nset
            nwor = core(ip)
            CALL write(Casecc,core(ip-1),2,0)
            CALL write(Casecc,core(ip+2),nwor,0)
            ip = ip + nwor + 3
         ENDDO
      ENDIF
      CALL write(Casecc,core(1),0,1)
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
         IF ( core(ip+2)/=1 ) THEN
            imov = 1
            nset1 = nset1 - 1
            ip = ip + core(ip+1) + 3
         ELSE
            IF ( imov/=0 ) THEN
               ido = core(ip+1) + 3
               DO j = 1 , ido
                  ii = iup + j - 1
                  ik = ip + j - 1
                  core(ii) = core(ik)
               ENDDO
            ENDIF
            iup = iup + core(ip+1) + 3
            ip = ip + core(ip+1) + 3
         ENDIF
      ENDDO
      Nset = nset1
      I81 = iup
   ENDIF
   DO i = 1 , Lencc
      case(i,2) = 0
      IF ( i>38 .AND. i<135 ) case(i,2) = Blank
   ENDDO
   CALL ifp1f(*99999,iword,i2)
   DO i = 1 , 5
      Isubc(i) = core(i2)
      i2 = i2 + 1
   ENDDO
99999 END SUBROUTINE ifp1e
