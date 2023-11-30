
SUBROUTINE ascm11(Name,Iphase,Isol,Nogo)
   IMPLICIT NONE
   INTEGER Idat(126) , Ioct , Iph , Iptbs , Irdm , Ixtra , Noct , Nph , Nptbs , Nrdm , Nxtra
   COMMON /asdbd / Irdm , Nrdm , Ixtra , Nxtra , Ioct , Noct , Iptbs , Nptbs , Iph , Nph , Idat
   INTEGER Iphase , Isol , Name , Nogo
   INTEGER comnd(6,7) , i , icomnd , isave(21) , j , k , l , ptbs(7,12) , rdmap(18,2) , subnam(2) , xtra(4)
   INTEGER khrfn1
   REAL slash
!
!     EXIO COMMANDS DMAP DATA
!
   DATA comnd/4HSOFI , 2 , 4 , 0 , 12 , 0 , 4HSOFO , 2 , 4 , 0 , 12 , 0 , 4HREST , 2 , 4 , 0 , 12 , 0 , 4HDUMP , 2 , 4 , 0 , 12 ,   &
      & 0 , 4HCHEC , 2 , 4 , 0 , 12 , 0 , 4HCOMP , 2 , 4 , 0 , 12 , 0 , 4HAPPE , 2 , 4 , 0 , 12 , 0/
   DATA slash/1H//
   DATA isave/1 , 7 , 1 , 1 , 11 , 3 , 1 , 13 , 2 , 1 , 15 , 1 , 2 , 6 , 1 , 2 , 11 , 3 , 2 , 14 , 2/
   DATA rdmap/4HEXIO , 4H     , 4H  // , 4HS,N, , 4HDRY/ , 4HMACH , 4H!*DE , 4HVI*/ , 4H*UNI , 4HTNAM , 4HE*!* , 4HFORM , 4H*!*M ,  &
       &4HODE* , 4H!*PO , 4HSI*/ , 4H*ITE , 4HM*/  , 4H     , 4H     , 4H  *N , 4HAME0 , 4H001* , 4H!*NA , 4HME00 , 4H02*/ ,        &
      & 4H*NAM , 4HE000 , 4H3*!* , 4HNAME , 4H0004 , 4H*!*N , 4HAME0 , 4H005* , 4H $   , 4H    /
   DATA xtra/4HMACH , 4HPOSI , 4HITEM , 4HNAME/
   DATA ptbs/1 , 21 , 21 , 4 , 101 , 0 , 0 , 1 , 27 , 27 , 4 , 102 , 0 , 0 , 1 , 34 , 34 , 8 , 103 , 0 , 0 , 1 , 45 , 45 , 4 , 104 ,&
      & 0 , 0 , 1 , 52 , 52 , 4 , 105 , 0 , 0 , 1 , 59 , 59 , 4 , 106 , 0 , 0 , 1 , 66 , 66 , 4 , 107 , 0 , 0 , 2 , 12 , 12 , 8 ,   &
      & 108 , 0 , 0 , 2 , 23 , 23 , 8 , 109 , 0 , 0 , 2 , 34 , 34 , 8 , 110 , 0 , 0 , 2 , 45 , 45 , 8 , 111 , 0 , 0 , 2 , 56 , 56 , &
      & 8 , 112 , 0 , 0/
   DATA subnam/4HASCM , 2H11/
!
!     RESTORE TO ORIGINAL DATA BY REPLACEING ! BY / IN RDMAP ARRAY
!     (SEE ASCM01 FOR EXPLANATION))
!
   DO l = 1 , 21 , 3
      i = isave(l+1)
      j = isave(l)
      k = isave(l+2)
      rdmap(i,j) = khrfn1(rdmap(i,j),k,slash,1)
   ENDDO
!
!     VALIDATE COMMAND AND SET POINTERS
!
   DO i = 1 , 7
      IF ( Name==comnd(1,i) ) GOTO 100
   ENDDO
!
!     INPUT ERROR
!
   CALL mesage(7,0,subnam)
   Nogo = 1
   GOTO 99999
 100  icomnd = i
   Irdm = 1
   Nrdm = comnd(2,icomnd)
   Ixtra = Irdm + 18*Nrdm
   Nxtra = comnd(3,icomnd)
   Ioct = Ixtra + Nxtra
   Noct = comnd(4,icomnd)
   Iptbs = Ioct + 3*Noct
   Nptbs = comnd(5,icomnd)
   Iph = Iptbs + 7*Nptbs
   Nph = comnd(6,icomnd)
!
!     MOVE RDMAP DATA
!
   k = 0
   IF ( Nrdm/=0 ) THEN
      DO j = 1 , Nrdm
         DO i = 1 , 18
            k = k + 1
            Idat(k) = rdmap(i,j)
         ENDDO
      ENDDO
   ENDIF
!
!     MOVE XTRA DATA
!
   IF ( Nxtra/=0 ) THEN
      DO i = 1 , Nxtra
         k = k + 1
         Idat(k) = xtra(i)
      ENDDO
   ENDIF
!
!     MOVE PTBS DATA
!
   IF ( Nptbs/=0 ) THEN
      DO j = 1 , Nptbs
         DO i = 1 , 7
            k = k + 1
            Idat(k) = ptbs(i,j)
         ENDDO
      ENDDO
   ENDIF
!
   RETURN
!
99999 RETURN
END SUBROUTINE ascm11