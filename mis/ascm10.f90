
SUBROUTINE ascm10(Name,Iphase,Isol,Nogo)
   IMPLICIT NONE
   INTEGER Idat(109) , Ioct , Iph , Iptbs , Irdm , Ixtra , Noct , Nph , Nptbs , Nrdm , Nxtra
   COMMON /asdbd / Irdm , Nrdm , Ixtra , Nxtra , Ioct , Noct , Iptbs , Nptbs , Iph , Nph , Idat
   INTEGER Iphase , Isol , Name , Nogo
   INTEGER comnd(6,6) , i , icomnd , j , k , ptbs(7,10) , rdmap(18,2) , subnam(2) , xtra(1)
   INTEGER khrfn1
   REAL slash
!
!     SUBSTRUCTURE UTILITY COMMANDS DMAP DATA
!
   DATA comnd/4HDEST , 2 , 0 , 0 , 3 , 0 , 4HEDIT , 2 , 0 , 0 , 3 , 0 , 4HEQUI , 2 , 1 , 0 , 5 , 0 , 4HSOFP , 2 , 0 , 0 , 10 , 0 ,  &
       &4HDELE , 2 , 0 , 0 , 10 , 0 , 4HRENA , 2 , 0 , 0 , 5 , 0/
   DATA slash/1H//
   DATA rdmap/4HSOFU , 4HT    , 4H  // , 4HDRY/ , 4H*NAM , 4HE    , 4H *!* , 4HOPER , 4H*/OP , 4HT!*N , 4HAME0 , 4H002* , 4H!*PR ,  &
       &4HEF*/ , 4H*ITM , 4H1*!* , 4HITM2 , 4H*/   , 4H     , 4H     , 4H  *I , 4HTM3* , 4H!*IT , 4HM4*/ , 4H*ITM , 4H5* $ ,        &
      & 4H     , 4H     , 8*4H    /
   DATA xtra/4HPREF/
   DATA ptbs/1 , 16 , 18 , 8 , 4HNAME , 0 , 0 , 1 , 27 , 29 , 4 , 4HOPER , 0 , 0 , 1 , 34 , 35 , 3 , 4HOPTI , 0 , 0 , 1 , 38 , 40 , &
      & 8 , 4HNEW  , 0 , 0 , 1 , 49 , 51 , 4 , 4HPREF , 0 , 0 , 1 , 56 , 58 , 4 , 4HITM1 , 0 , 0 , 1 , 63 , 65 , 4 , 4HITM2 , 0 ,   &
      & 0 , 2 , 11 , 12 , 4 , 4HITM3 , 0 , 0 , 2 , 17 , 19 , 4 , 4HITM4 , 0 , 0 , 2 , 24 , 26 , 4 , 4HITM5 , 0 , 0/
   DATA subnam/4HASCM , 2H10/
!
!     RESTORE TO ORIGINAL DATA BY REPLACEING ! BY / IN RDMAP ARRAY
!     (SEE ASCM01 FOR EXPLANATION))
!
   rdmap(7,1) = khrfn1(rdmap(7,1),3,slash,1)
   rdmap(10,1) = khrfn1(rdmap(10,1),2,slash,1)
   rdmap(13,1) = khrfn1(rdmap(13,1),1,slash,1)
   rdmap(16,1) = khrfn1(rdmap(16,1),3,slash,1)
   rdmap(5,2) = khrfn1(rdmap(5,2),1,slash,1)
!
!     VALIDATE COMMAND AND SET POINTERS
!
   DO i = 1 , 6
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
END SUBROUTINE ascm10
