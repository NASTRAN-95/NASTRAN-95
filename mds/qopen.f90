
SUBROUTINE qopen(*,Namfil,Buff,Iop)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   REAL Dum1(77) , Dum2(21)
   INTEGER Idiag , Isysbf
   COMMON /system/ Isysbf , Dum1 , Idiag , Dum2
   INTEGER Iop , Namfil
   INTEGER Buff(10)
   INTEGER dname(2) , i , ibashi , ibaslo , ibasts , init , itrl(7) , locbuf
   INTEGER locfx
   DATA init/0/
   Name = Namfil
   Iocode = Iop
   IF ( init==0 ) THEN
      Ibasbf = locfx(Ibase)
      CALL dsiodd
      Nbuff = Isysbf - 4
      Nbfz = 1
      IF ( Lenwpb/=0 ) Nbfz = Nbuff/Lenwpb + .1
      init = 1
   ENDIF
   IF ( iand(Idiag,2**14)/=0 ) CALL dsmsg(1)
   locbuf = locfx(Buff)
   Indbas = locbuf - Ibasbf + 1
   IF ( mod(Indbas,2)==0 ) Indbas = Indbas + 1
   IF ( Fcb(2,Ifilex)/=0 ) CALL dsmsg(5)
   DO i = 1 , MAXPRI
      ibasts = Fcb(2,i)
      IF ( ibasts/=0 ) THEN
         ibashi = ibasts + Isysbf - 2
         ibaslo = ibasts - Isysbf
         IF ( Indbas>ibaslo .AND. Indbas<=ibashi ) CALL dsmsg(3)
      ENDIF
   ENDDO
   Ibase(Indbas) = Namfil
   Fcb(2,Ifilex) = Indbas
   Fcb(12,Ifilex) = Indbas
   CALL dbmnam(Name,dname,Ifilex)
   IF ( Iocode>1 ) THEN
      IF ( Fcb(13,Ifilex)/=dname(1) .OR. Fcb(14,Ifilex)/=dname(2) ) THEN
!        CALL DBMSRF( DNAME, IUNI )
!        IF ( IUNI .EQ. IFILEX ) GO TO 35
         itrl(1) = Name
         CALL rdtrl(itrl)
         DO i = 2 , 7
            IF ( itrl(i)/=0 ) GOTO 50
         ENDDO
         IF ( Iocode==3 ) Iocode = 1
         IF ( Iocode==2 ) Iocode = 0
         GOTO 100
      ENDIF
 50   Nblock = Fcb(4,Ifilex)
      IF ( Nblock/=0 ) THEN
         CALL dbmmgr(1)
         Indclr = Fcb(3,Ifilex) + Indbas - 1
         Indcbp = Indclr
         GOTO 200
      ENDIF
   ENDIF
 100  Nblock = 1
   Fcb(13,Ifilex) = dname(1)
   Fcb(14,Ifilex) = dname(2)
   CALL dbmmgr(1)
   Indclr = Indbas + 5
   Indcbp = Indclr
   IF ( Iocode/=0 ) THEN
      Ibase(Indbas+3) = 1
      Ibase(Indbas+4) = 0
      Fcb(8,Ifilex) = 0
   ENDIF
 200  IF ( Nblock/=Ibase(Indbas+3) ) CALL dsmsg(102)
   CALL dssdcb
!        PRINT *,' QOPEN,UN,CLR,BLK,IOP=',IFILEX,FCB(3,IFILEX),
!     &     FCB(4,IFILEX),IOP
END SUBROUTINE qopen
