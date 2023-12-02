!*==qopen.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE qopen(Namfil,Buff,Iop) !HIDESTARS (*,Namfil,Buff,Iop)
   IMPLICIT NONE
   USE I_DSIOF
   USE I_XNSTRN
   USE C_SYSTEM
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Namfil
   INTEGER , DIMENSION(10) :: Buff
   INTEGER :: Iop
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: dname
   INTEGER :: i , ibashi , ibaslo , ibasts , locbuf
   INTEGER , SAVE :: init
   INTEGER , DIMENSION(7) :: itrl
!
! End of declarations rewritten by SPAG
!
   DATA init/0/
   name = Namfil
   iocode = Iop
   IF ( init==0 ) THEN
      ibasbf = locfx(ibase)
      CALL dsiodd
      nbuff = Isysbf - 4
      nbfz = 1
      IF ( lenwpb/=0 ) nbfz = nbuff/lenwpb + .1
      init = 1
   ENDIF
   IF ( iand(Idiag,2**14)/=0 ) CALL dsmsg(1)
   locbuf = locfx(Buff)
   indbas = locbuf - ibasbf + 1
   IF ( mod(indbas,2)==0 ) indbas = indbas + 1
   IF ( fcb(2,ifilex)/=0 ) CALL dsmsg(5)
   DO i = 1 , maxpri
      ibasts = fcb(2,i)
      IF ( ibasts/=0 ) THEN
         ibashi = ibasts + Isysbf - 2
         ibaslo = ibasts - Isysbf
         IF ( indbas>ibaslo .AND. indbas<=ibashi ) CALL dsmsg(3)
      ENDIF
   ENDDO
   ibase(indbas) = Namfil
   fcb(2,ifilex) = indbas
   fcb(12,ifilex) = indbas
   CALL dbmnam(name,dname,ifilex)
   IF ( iocode>1 ) THEN
      IF ( fcb(13,ifilex)/=dname(1) .OR. fcb(14,ifilex)/=dname(2) ) THEN
!        CALL DBMSRF( DNAME, IUNI )
!        IF ( IUNI .EQ. IFILEX ) GO TO 35
         itrl(1) = name
         CALL rdtrl(itrl)
         DO i = 2 , 7
            IF ( itrl(i)/=0 ) GOTO 50
         ENDDO
         IF ( iocode==3 ) iocode = 1
         IF ( iocode==2 ) iocode = 0
         GOTO 100
      ENDIF
 50   nblock = fcb(4,ifilex)
      IF ( nblock/=0 ) THEN
         CALL dbmmgr(1)
         indclr = fcb(3,ifilex) + indbas - 1
         indcbp = indclr
         GOTO 200
      ENDIF
   ENDIF
 100  nblock = 1
   fcb(13,ifilex) = dname(1)
   fcb(14,ifilex) = dname(2)
   CALL dbmmgr(1)
   indclr = indbas + 5
   indcbp = indclr
   IF ( iocode/=0 ) THEN
      ibase(indbas+3) = 1
      ibase(indbas+4) = 0
      fcb(8,ifilex) = 0
   ENDIF
 200  IF ( nblock/=ibase(indbas+3) ) CALL dsmsg(102)
   CALL dssdcb
!        PRINT *,' QOPEN,UN,CLR,BLK,IOP=',IFILEX,FCB(3,IFILEX),
!     &     FCB(4,IFILEX),IOP
END SUBROUTINE qopen
