!*==ds1etd.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ds1etd(Elid,Ti,Grids)
   USE c_ds1ett
   USE c_system
   USE c_xmssg
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Elid
   INTEGER , DIMENSION(2) :: Ti
   INTEGER :: Grids
!
! Local variable declarations rewritten by SPAG
!
   REAL :: flag
   INTEGER , SAVE :: gptt , maxwds
   INTEGER :: i , id , nwords
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL mesage , read
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE (CALLED BY -DS1-) READS ELEMENT TEMPERATURE
!     DATA FROM A PRE-POSITIONED RECORD
!
!     ELID   = ID OF ELEMENT FOR WHICH DATA IS DESIRED
!     TI     = BUFFER DATA IS TO BE RETURNED IN
!     GRIDS  = 0 IF EL-TEMP FORMAT DATA IS TO BE RETURNED
!            = NO. OF GRID POINTS IF GRID POINT DATA IS TO BE RETURNED.
!     ELTYPE = ELEMENT TYPE TO WHICH -ELID- BELONGS
!     OLDEL  = ELEMENT TYPE CURRENTLY BEING WORKED ON (INITIALLY 0)
!     OLDEID = ELEMENT ID FROM LAST CALL
!     EORFLG =.TRUE. WHEN ALL DATA HAS BEEN EXHAUSTED IN RECORD
!     ENDID  =.TRUE. WHEN ALL DATA HAS BEEN EXHAUSTED WITHIN AN ELEMENT
!              TYPE.
!     BUFFLG = NOT USED
!     ITEMP  = TEMPERATURE LOAD SET ID
!     IDEFT  = NOT USED
!     IDEFM  = NOT USED
!     RECORD =.TRUE. IF A RECORD OF DATA IS INITIALLY AVAILABLE
!     DEFALT = THE DEFALT TEMPERATURE VALUE OR -1 IF IT DOES NOT EXIST
!     AVRAGE = THE AVERAGE ELEMENT TEMPERATURE
!
   DATA name/4HDS1E , 4HTD  / , maxwds/33/ , gptt/102/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         IF ( oldeid==Elid ) RETURN
         oldeid = Elid
!
         IF ( itemp<=0 ) THEN
            DO i = 1 , maxwds
               Ti(i) = -1
            ENDDO
            RETURN
!
         ELSEIF ( .NOT.record .OR. eorflg ) THEN
!
!     NO MORE DATA FOR THIS ELEMENT TYPE
!
            endid = .TRUE.
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         IF ( eltype/=oldel ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( endid ) THEN
            endid = .TRUE.
         ELSE
            SPAG_Loop_1_1: DO
!
!     HERE WHEN ELTYPE IS AT HAND AND END OF THIS TYPE DATA
!     HAS NOT YET BEEN REACHED.  READ AN ELEMENT ID
!
               CALL read(*40,*60,gptt,id,1,0,flag)
               IF ( id==0 ) THEN
                  endid = .TRUE.
                  EXIT SPAG_Loop_1_1
               ELSEIF ( iabs(id)==Elid ) THEN
                  IF ( id<=0 ) EXIT SPAG_Loop_1_1
!
!     MATCH ON ELEMNT ID MADE AND IT WAS WITH DATA
!
                  CALL read(*40,*60,gptt,Ti,nwords,0,flag)
                  RETURN
               ELSEIF ( id>0 ) THEN
                  CALL read(*40,*60,gptt,Ti,nwords,0,flag)
               ENDIF
            ENDDO SPAG_Loop_1_1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
!     NO DATA FOR ELEMENT ID DESIRED, THUS USE DEFALT
!
         IF ( defalt==-1 ) THEN
!
!     NO TEMP DATA OR DEFALT
!
            WRITE (iout,99001) ufm , Elid , itemp
99001       FORMAT (A23,' 4016, THERE IS NO TEMPERATURE DATA FOR ELEMENT',I9,' IN SET',I9)
            CALL mesage(-61,0,0)
         ELSEIF ( Grids>0 ) THEN
!
            DO i = 1 , Grids
               Ti(i) = defalt
            ENDDO
            Ti(Grids+1) = defalt
            RETURN
         ELSE
            DO i = 2 , maxwds
               Ti(i) = 0
            ENDDO
            Ti(1) = defalt
            IF ( eltype==34 ) Ti(2) = defalt
            RETURN
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
!
!     LOOK FOR MATCH ON ELTYPE (FIRST SKIP ANY UNUSED ELEMENT DATA)
!
         IF ( .NOT.(endid) ) THEN
            SPAG_Loop_1_2: DO
               CALL read(*40,*60,gptt,id,1,0,flag)
               IF ( id<0 ) THEN
               ELSEIF ( id==0 ) THEN
                  EXIT SPAG_Loop_1_2
               ELSE
                  CALL read(*40,*60,gptt,Ti,nwords,0,flag)
               ENDIF
            ENDDO SPAG_Loop_1_2
         ENDIF
!
!     READ ELTYPE AND COUNT
!
         CALL read(*40,*20,gptt,Ti,2,0,flag)
         oldel = Ti(1)
         nwords = Ti(2)
         endid = .FALSE.
         iback = 1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     END OF RECORD HIT
!
 20      eorflg = .TRUE.
         endid = .TRUE.
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 40      CALL mesage(-2,gptt,name)
 60      CALL mesage(-3,gptt,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ds1etd
