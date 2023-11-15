
SUBROUTINE rcovsl(Name,Item,In,Amat,Scr2,Scr3,Out,Z,Iz,Lcore,First,Rfno)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Cmx(7) , Fmx(7) , Incp , Irowp , Itypp , Mcore , Norew , Nout , Nrowp , Otypp , Pmx(7) , Prec , Rd , Rdrew , Rew , Scr , &
         & Signc , Signpf , Slmx(7) , Sysbuf , T , Wrt , Wrtrew
   COMMON /mpyadx/ Pmx , Fmx , Cmx , Slmx , Mcore , T , Signpf , Signc , Prec , Scr
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew
   COMMON /packx / Itypp , Otypp , Irowp , Nrowp , Incp
   COMMON /system/ Sysbuf , Nout
!
! Dummy argument declarations
!
   INTEGER Amat , In , Item , Lcore , Out , Rfno , Scr2 , Scr3
   LOGICAL First
   INTEGER Iz(1) , Name(2)
   REAL Z(1)
!
! Local variable declarations
!
   INTEGER buf1 , fss(2) , i , ibuf(3) , icol , ifact , ip , itm , j , n , nb , ncol , nfact , nl , nr , nrow , nrs , nst , rc ,    &
         & soln , srd , subr(2) , type
!
! End of declarations
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
      IF ( rc==3 ) GOTO 100
      IF ( rc/=1 ) THEN
!
!     ERRORS
!
         CALL smsg(rc-2,itm,Name)
         GOTO 100
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
         GOTO 100
      ELSE
         CALL suread(fss,2,n,rc)
         IF ( rc/=1 ) GOTO 300
         CALL suread(ibuf,3,n,rc)
         IF ( rc/=1 ) GOTO 300
         IF ( Rfno==3 ) GOTO 100
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
            IF ( rc/=1 ) GOTO 300
            CALL suread(nl,1,n,rc)
            IF ( rc/=1 ) GOTO 300
            IF ( nl<=0 ) GOTO 100
            IF ( nl>=buf1 ) CALL mesage(-8,0,subr)
            CALL suread(Iz(1),nl,n,rc)
            IF ( rc/=1 ) GOTO 300
            n = 1
            CALL sjump(n)
            IF ( n<0 ) GOTO 400
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
               IF ( n<0 ) GOTO 400
               CALL suread(Z(ifact),nl*ip,n,rc)
               IF ( rc/=1 ) GOTO 300
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
               IF ( n<0 ) GOTO 400
               CALL suread(nl,1,n,rc)
               IF ( rc/=1 ) GOTO 300
               IF ( nl>=0 ) THEN
                  IF ( nl/=0 ) THEN
                     IF ( Nrowp+2*nl>=buf1 ) CALL mesage(-8,0,subr)
                     CALL suread(Z(Nrowp+1),2*nl,n,rc)
                     IF ( rc/=1 ) GOTO 300
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
   GOTO 200
!
!     NO SCALAR LOADS
!
 100  Out = 0
   CALL close(Amat,Rew)
 200  RETURN
 300  CALL smsg(rc+4,itm,Name)
   GOTO 100
 400  CALL smsg(7,itm,Name)
   GOTO 100
END SUBROUTINE rcovsl
