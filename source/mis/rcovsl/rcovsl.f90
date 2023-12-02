!*==rcovsl.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rcovsl(Name,Item,In,Amat,Scr2,Scr3,Out,Z,Iz,Lcore,First,Rfno)
   IMPLICIT NONE
   USE C_MPYADX
   USE C_NAMES
   USE C_PACKX
   USE C_SYSTEM
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: Name
   INTEGER :: Item
   INTEGER :: In
   INTEGER :: Amat
   INTEGER :: Scr2
   INTEGER :: Scr3
   INTEGER :: Out
   REAL , DIMENSION(1) :: Z
   INTEGER , DIMENSION(1) :: Iz
   INTEGER :: Lcore
   LOGICAL :: First
   INTEGER :: Rfno
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: buf1 , i , icol , ifact , ip , itm , j , n , nb , ncol , nfact , nl , nr , nrow , nrs , nst , rc , type
   INTEGER , DIMENSION(2) :: fss
   INTEGER , DIMENSION(3) :: ibuf
   INTEGER , SAVE :: soln , srd
   INTEGER , DIMENSION(2) , SAVE :: subr
   EXTERNAL close , gopen , makmcb , mesage , mpyad , mtrxi , pack , rdtrl , sfetch , sjump , smsg , suread , wrttrl
!
! End of declarations rewritten by SPAG
!
!
!     RCOVSL CALCULATES THE STATIC LOAD VECTORS FOR THE SUBSTRUCTURING
!     PHASE 2 AND PHASE 3 OPERATIONS FROM THE SUBSTRUCTURE SOLN ITEM
!
   DATA soln/4HSOLN/ , srd/1/
   DATA subr/4HRCOV , 4HSL  /
!
!     INITIALIZE
!
   buf1 = Lcore - Sysbuf + 1
   Itypp = 1
   Irowp = 1
   Incp = 1
   Mcore = Lcore
   T = 0
   Signpf = 1
   Prec = 0
!
!     READ LOAD MATRIX FROM SOF ONTO GINO FILE
!
   Pmx(1) = In
   CALL rdtrl(Pmx)
   IF ( Pmx(1)<=0 ) THEN
      itm = Item
      CALL mtrxi(Scr2,Name,Item,Z(buf1),rc)
      IF ( rc==3 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
      IF ( rc/=1 ) THEN
!
!     ERRORS
!
         CALL smsg(rc-2,itm,Name)
         CALL spag_block_1
         RETURN
      ELSE
         Pmx(1) = Scr2
         CALL rdtrl(Pmx)
      ENDIF
   ENDIF
   Nrowp = Pmx(2)
   type = Pmx(5)
   IF ( Rfno==8 .AND. type<=2 ) type = type + 2
   Otypp = type
   IF ( .NOT.(First) ) THEN
!
!     PROCESS INITIAL SOLN DATA
!
      itm = soln
      CALL sfetch(Name,soln,srd,rc)
      IF ( rc/=1 ) THEN
         CALL smsg(rc-2,itm,Name)
         CALL spag_block_1
         RETURN
      ELSE
         CALL suread(fss,2,n,rc)
         IF ( rc/=1 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         CALL suread(ibuf,3,n,rc)
         IF ( rc/=1 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( Rfno==3 ) THEN
            CALL spag_block_1
            RETURN
         ENDIF
         nb = ibuf(2)
         nst = ibuf(3)
!
!     INTILIZE SCR1 FILE
!
         CALL makmcb(Fmx,Amat,Nrowp,2,type)
         CALL gopen(Amat,Z(buf1),Wrtrew)
!
!     PACK FACTOR MATRIX FOR R. F. 1,2
!
         IF ( Rfno==8 .OR. Rfno==9 ) THEN
!
!     PACK FACTOR MATRIX FOR R. F. 8,9
!
            CALL suread(Iz(1),3*nb,n,rc)
            IF ( rc/=1 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            CALL suread(nl,1,n,rc)
            IF ( rc/=1 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            IF ( nl<=0 ) THEN
               CALL spag_block_1
               RETURN
            ENDIF
            IF ( nl>=buf1 ) CALL mesage(-8,0,subr)
            CALL suread(Iz(1),nl,n,rc)
            IF ( rc/=1 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            n = 1
            CALL sjump(n)
            IF ( n<0 ) THEN
               CALL spag_block_4
               RETURN
            ENDIF
            ip = 1
            IF ( Rfno==8 ) ip = 2
            IF ( Rfno==8 ) Itypp = 3
            ifact = nl + 1
            nfact = nl + nl*ip
            icol = nfact + 1
            ncol = nfact + ip*Nrowp
            IF ( ncol>=buf1 ) CALL mesage(-8,0,subr)
!
            DO i = 1 , nst
               DO j = icol , ncol
                  Z(j) = 0.0
               ENDDO
               n = 1
               CALL sjump(n)
               IF ( n<0 ) THEN
                  CALL spag_block_4
                  RETURN
               ENDIF
               CALL suread(Z(ifact),nl*ip,n,rc)
               IF ( rc/=1 ) THEN
                  CALL spag_block_3
                  RETURN
               ENDIF
               nrow = ifact - ip
               nrs = icol - ip
               DO j = 1 , nl
                  nrow = nrow + ip
                  nr = nrs + Iz(j)*ip
                  Z(nr) = Z(nrow)
                  IF ( ip==2 ) Z(nr+1) = Z(nrow+1)
               ENDDO
               CALL pack(Z(icol),Amat,Fmx)
            ENDDO
            CALL close(Amat,Rew)
            CALL wrttrl(Fmx)
         ELSE
            DO i = 1 , nst
               DO j = 1 , Nrowp
                  Z(j) = 0.0
               ENDDO
               n = 1
               CALL sjump(n)
               IF ( n<0 ) THEN
                  CALL spag_block_4
                  RETURN
               ENDIF
               CALL suread(nl,1,n,rc)
               IF ( rc/=1 ) THEN
                  CALL spag_block_3
                  RETURN
               ENDIF
               IF ( nl>=0 ) THEN
                  IF ( nl/=0 ) THEN
                     IF ( Nrowp+2*nl>=buf1 ) CALL mesage(-8,0,subr)
                     CALL suread(Z(Nrowp+1),2*nl,n,rc)
                     IF ( rc/=1 ) THEN
                        CALL spag_block_3
                        RETURN
                     ENDIF
                     nrow = Nrowp - 1
                     DO j = 1 , nl
                        nrow = nrow + 2
                        nr = Iz(nrow)
                        Z(nr) = Z(nrow+1)
                     ENDDO
                  ENDIF
                  CALL pack(Z(1),Amat,Fmx)
               ENDIF
            ENDDO
            CALL close(Amat,Rew)
            CALL wrttrl(Fmx)
         ENDIF
      ENDIF
   ENDIF
!
!     OUT = LOADS*FACTORS
!
   Fmx(1) = Amat
   CALL rdtrl(Fmx)
   Cmx(1) = 0
   CALL makmcb(Slmx,Out,Pmx(3),2,type)
   Scr = Scr3
   CALL mpyad(Z,Z,Z)
   CALL wrttrl(Slmx)
   CALL spag_block_2
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
!
!     NO SCALAR LOADS
!
      Out = 0
      CALL close(Amat,Rew)
      CALL spag_block_2
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
      RETURN
   END SUBROUTINE spag_block_2
   SUBROUTINE spag_block_3
      CALL smsg(rc+4,itm,Name)
      CALL spag_block_1
      RETURN
   END SUBROUTINE spag_block_3
   SUBROUTINE spag_block_4
      CALL smsg(7,itm,Name)
      CALL spag_block_1
      RETURN
   END SUBROUTINE spag_block_4
END SUBROUTINE rcovsl
