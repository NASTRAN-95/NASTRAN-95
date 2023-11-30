
SUBROUTINE sdretd(Elid,Ti,Grids)
   IMPLICIT NONE
   LOGICAL Bufflg , Endid , Eorflg , Record
   INTEGER Defalt , Eltype , Gptt , Idefm , Ideft , Iout , Itemp , Oldeid , Oldel
   REAL Dum , Dum2(2) , Dum20(20) , Dummy(6)
   CHARACTER*23 Ufm
   COMMON /sdr2de/ Dum2 , Defalt
   COMMON /sdr2x2/ Dummy , Gptt , Dum20
   COMMON /sdrett/ Eltype , Oldel , Eorflg , Endid , Bufflg , Itemp , Ideft , Idefm , Record , Oldeid
   COMMON /system/ Dum , Iout
   COMMON /xmssg / Ufm
   INTEGER Elid , Grids
   INTEGER Ti(33)
   REAL flag
   INTEGER i , id , maxwds , name(2) , nwords
!
!     THIS ROUTINE (CALLED BY -SDR2E-) READS ELEMENT TEMPERATURE
!     DATA FROM A PRE-POSITIONED RECORD
!
!     ELID   = ID OF ELEMENT FOR WHICH DATA IS DESIRED
!     TI     = BUFFER DATA IS TO BE RETURNED IN
!     GRIDS  = 0 IF EL-TEMP FORMAT DATA IS TO BE RETURNED
!            = NO. OF GRID POINTS IF GRID POINT DATA IS TO BE RETURNED.
!     ELTYPE = ELEMENT TYPE TO WHICH -ELID- BELONGS
!     OLDEL  = ELEMENT TYPE CURRENTLY BEING WORKED ON (INITIALLY 0)
!     OLDEID = ELEMENT ID FROM LAST CALL
!     EORFLG = .TRUE. WHEN ALL DATA HAS BEEN EXHAUSTED IN RECORD
!     ENDID  = .TRUE. WHEN ALL DATA HAS BEEN EXHAUSTED WITHIN AN ELEMENT
!              TYPE.
!     BUFFLG = NOT USED
!     ITEMP  = TEMPERATURE LOAD SET ID
!     IDEFT  = NOT USED
!     IDEFM  = NOT USED
!     RECORD = .TRUE. IF A RECORD OF DATA IS INITIALLY AVAILABLE
!     DEFALT = THE DEFALT TEMPERATURE VALUE OR -1 IF IT DOES NOT EXIST
!     AVRAGE = THE AVERAGE ELEMENT TEMPERATURE
!
   DATA name/4HSDRE , 4HTD  / , maxwds/33/
!
   IF ( Oldeid==Elid ) RETURN
   Oldeid = Elid
   IF ( Itemp==0 ) THEN
      DO i = 1 , maxwds
         Ti(i) = 0
      ENDDO
      RETURN
!
   ELSEIF ( .NOT.Record .OR. Eorflg ) THEN
!
!     NO MORE DATA FOR THIS ELEMENT TYPE
!
      Endid = .TRUE.
      GOTO 200
   ELSE
      IF ( Eltype/=Oldel ) GOTO 300
      IF ( Endid ) THEN
         Endid = .TRUE.
         GOTO 200
      ENDIF
   ENDIF
 100  DO
!
!     HERE WHEN ELTYPE IS AT HAND AND END OF THIS TYPE DATA
!     HAS NOT YET BEEN REACHED.  READ AN ELEMENT ID
!
      CALL read(*500,*600,Gptt,id,1,0,flag)
      IF ( id==0 ) THEN
         Endid = .TRUE.
         EXIT
      ELSEIF ( iabs(id)==Elid ) THEN
         IF ( id<=0 ) EXIT
!
!     MATCH ON ELEMNT ID MADE AND IT WAS WITH DATA
!
         CALL read(*500,*600,Gptt,Ti,nwords,0,flag)
!
!     IF QUAD4 (ELTYPE 64) OR TRIA3 (ELTYPE 83) ELEMENT, SET FLAG FOR
!     SQUD42 OR STRI32
!
         IF ( Eltype/=64 .OR. Eltype/=83 ) RETURN
         Ti(7) = 13
         IF ( Ti(6)/=1 ) Ti(7) = 2
         RETURN
      ELSEIF ( id>0 ) THEN
         CALL read(*500,*600,Gptt,Ti,nwords,0,flag)
      ENDIF
   ENDDO
!
!     NO DATA FOR ELEMENT ID DESIRED, THUS USE DEFALT
!
 200  IF ( Defalt==-1 ) THEN
!
!     NO TEMP DATA OR DEFALT
!
      WRITE (Iout,99001) Ufm , Elid , Itemp
99001 FORMAT (A23,' 4016, THERE IS NO TEMPERATURE DATA FOR ELEMENT',I9,' IN SET',I9)
      CALL mesage(-61,0,0)
   ELSEIF ( Grids>0 ) THEN
!
      IF ( Eltype==64 .AND. Eltype==83 ) THEN
!                 QUAD4             TRIA3
         Ti(4) = 0
         Ti(5) = 0
         Ti(6) = 0
         Ti(7) = 0
      ENDIF
      DO i = 1 , Grids
         Ti(i) = Defalt
      ENDDO
      Ti(Grids+1) = Defalt
      RETURN
   ELSE
      DO i = 2 , maxwds
         Ti(i) = 0
      ENDDO
      Ti(1) = Defalt
      IF ( Eltype==34 ) Ti(2) = Defalt
      RETURN
   ENDIF
!
!     LOOK FOR MATCH ON ELTYPE (FIRST SKIP ANY UNUSED ELEMENT DATA)
!
 300  IF ( .NOT.(Endid) ) THEN
      DO
         CALL read(*500,*600,Gptt,id,1,0,flag)
         IF ( id<0 ) THEN
         ELSEIF ( id==0 ) THEN
            EXIT
         ELSE
            CALL read(*500,*600,Gptt,Ti,nwords,0,flag)
         ENDIF
      ENDDO
   ENDIF
!
!     READ ELTYPE AND COUNT
!
   CALL read(*500,*400,Gptt,Ti,2,0,flag)
   Oldel = Ti(1)
   nwords = Ti(2)
   Endid = .FALSE.
   GOTO 100
!     END OF RECORD HIT
!
 400  Eorflg = .TRUE.
   Endid = .TRUE.
   GOTO 200
!
 500  CALL mesage(-2,Gptt,name)
 600  CALL mesage(-3,Gptt,name)
END SUBROUTINE sdretd