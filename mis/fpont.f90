
SUBROUTINE fpont
   IMPLICIT NONE
   REAL Bg , Core(1) , Old
   INTEGER Ifm , Lc , N(12) , Slt
   COMMON /loadx / Lc , Slt , Bg , Old , N , Ifm
   COMMON /zzzzzz/ Core
   REAL flag , gpco1(3) , gpco2(3) , gpco3(3) , gpco4(3) , gridp(7) , scale , vect(3) , vect1(3) , vect2(3) , xl
   INTEGER gpid , i , icosyt , igpco(4,5) , in , iord(5) , ip1 , ip2 , ip3 , ip4 , iparm , l , minus , n1 , np , nr , pont(5) ,     &
         & swload(2)
!
!     DOES DIRECT,TPONT,FPONT,AND SCALAR LOADS
!
   EQUIVALENCE (gpid,gridp(2)) , (gridp(4),ip1) , (gridp(5),ip2) , (gridp(6),ip3) , (gridp(7),ip4) , (igpco(2,1),gpco1(1)) ,        &
    & (igpco(2,2),gpco2(1)) , (igpco(2,3),gpco3(1)) , (igpco(2,4),gpco4(1)) , (icosyt,gridp(3))
   DATA swload/4HFPON , 4HT   /
!
   nr = 6
   np = 5
   minus = 5
 100  CALL read(*400,*500,Slt,gridp(2),nr,0,flag)
   scale = gridp(3)
   pont(1) = ip1
   pont(2) = ip2
   IF ( np/=3 ) THEN
      pont(3) = ip3
      pont(4) = ip4
   ENDIF
   pont(np) = gpid
   CALL permut(pont(1),iord(1),np,Old)
   DO i = 1 , np
      l = iord(i)
      CALL fndpnt(igpco(1,l),pont(l))
   ENDDO
   IF ( np==3 ) THEN
      DO i = 1 , 3
         vect(i) = gpco2(i) - gpco1(i)
      ENDDO
   ELSE
      DO i = 1 , 3
         vect1(i) = gpco2(i) - gpco1(i)
         vect2(i) = gpco4(i) - gpco3(i)
      ENDDO
      CALL cross(vect1(1),vect2(1),vect(1))
   ENDIF
   CALL norm(vect(1),xl)
 200  IF ( igpco(1,np)/=0 ) CALL basglb(vect(1),vect(1),igpco(2,np),igpco(1,np))
 300  CALL fndsil(gpid)
   gpid = gpid + (Ifm-minus)*3 - 1
   DO i = 1 , 3
      in = gpid + i
      Core(in) = Core(in) + vect(i)*scale
   ENDDO
   GOTO 700
 400  n1 = -2
   GOTO 600
 500  n1 = -3
 600  iparm = Slt
   CALL mesage(n1,iparm,swload)
 700  RETURN
!
!
   ENTRY tpont
!     ===========
!
!     TPONT PROCESSES FORCE1 AND MOMENT1 CARDS
!
   nr = 4
   np = 3
   minus = 3
   GOTO 100
!
!
   ENTRY direct
!     ============
!
!     DIRECT PROCESSES FORCE+ MOMENT CARDS
!
   np = 1
   minus = 1
   CALL read(*400,*500,Slt,gridp(2),6,0,flag)
   DO i = 1 , 3
      vect(i) = gridp(i+4)
   ENDDO
   CALL fndpnt(igpco(1,1),gpid)
   scale = gridp(4)
   IF ( icosyt==igpco(1,np) ) GOTO 300
   IF ( icosyt/=0 ) CALL glbbas(vect(1),vect(1),igpco(2,1),icosyt)
   GOTO 200
!
!
   ENTRY sload
!     ===========
!
!     SLOAD PROCESSES SLOAD CARDS
!
   CALL read(*400,*500,Slt,gridp(2),2,0,flag)
   CALL fndsil(gpid)
   Core(gpid) = Core(gpid) + gridp(3)
   GOTO 700
END SUBROUTINE fpont
