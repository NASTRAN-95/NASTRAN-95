!*==cnstrc.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cnstrc(Gp,Ele,Buf,Max)
   USE c_blank
   USE c_gpta1
   USE c_names
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Gp
   INTEGER , DIMENSION(1) :: Ele
   INTEGER , DIMENSION(1) :: Buf
   INTEGER :: Max
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: ae , br , nmsg1 , q4 , t3
   INTEGER :: b1 , b2 , b3 , elid , etype , exgp , gpt , i , idx , ingp , j , labgp , m , mtype , n , n1 , n2 , ne16 , nel ,        &
            & neltyp , ngppe , ngpts , ntypes , offset , setid , setnum , sign
   INTEGER , DIMENSION(2) :: eid , err
   INTEGER , DIMENSION(32) :: gpts
   INTEGER , DIMENSION(14) , SAVE :: msg1
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , DIMENSION(6) :: off
   INTEGER , DIMENSION(50) :: type
   EXTERNAL close , clstab , fread , gopen , intlst , mesage , read , write , wrtprt
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS SUBROUTINE BUILDS THE ELSETS FILE
!     THIS SUBROUTINE IS CALLED ONLY BY DPLTST, WHICH IS THE DRIVER OF
!     DMAP MODULE PLTSET
!     THE SUBROUITNE PLTSET OF THE PLOT MODULE HAS NOTHING TO DO WITH
!     THIS SUBROUTINE
!
!     REVISED 10/1990 BY G.CHAN/UNISYS TO INCLUDE OFFSET FOR BAR, TRIA3
!     AND QUAD4 ELEMENTS
!
   !>>>>EQUIVALENCE (eid(1),elid)
   DATA name/4H CNS , 4HTRC / , ae/72/
   DATA nmsg1/14/
   DATA msg1/4H(33X , 4H,44H , 4HNO P , 4HLOTA , 4HBLE  , 4HSTRU , 4HCTUR , 4HAL E , 4HLEME , 4HNTS  , 4HEXIS , 4HT IN , 4H SET ,   &
       &4H,I8)/
   DATA br , t3 , q4/2HBR , 2HT3 , 2HQ4/
!
   b1 = 1
   b2 = b1 + bufsiz
   b3 = b2 + bufsiz
   CALL gopen(mset,Buf(b3),inprew)
   CALL gopen(ect2,Buf(b2),outrew)
!
   DO setnum = 1 , nsets
      spag_nextblock_1 = 1
      SPAG_DispatchLoop_1: DO
         SELECT CASE (spag_nextblock_1)
         CASE (1)
            CALL fread(mset,setid,1,0)
            DO i = 1 , ngp
               Gp(i) = 0
            ENDDO
!
!     READ THE EXPLICIT ELEMENT NUMBERS IN THIS SET.
!
            CALL fread(mset,nel,1,0)
            IF ( nel>=Max ) CALL mesage(-8,0,name)
            Ele(nel+1) = 0
            CALL fread(mset,Ele,nel,0)
!
!     READ THE ELEMENT TYPES TO BE INCLUDED OR EXCLUDED IN THIS SET.
!
            CALL fread(mset,ntypes,1,0)
            CALL fread(mset,type,ntypes,0)
!
!     GENERATE AN ECT FOR THE ELEMENTS INCLUDED IN THIS SET.
!
            CALL gopen(ect1,Buf(b1),inprew)
            spag_nextblock_1 = 2
         CASE (2)
            CALL read(*20,*20,ect1,etype,1,0,i)
!
!     CHECK WHETHER OR NOT THIS ELEMENT TYPE IS TO BE EXCLUDED.
!
            mtype = -1
            labgp = 1
            IF ( etype==ae ) labgp = -2
            IF ( ntypes/=0 ) THEN
               DO i = 1 , ntypes , 2
                  IF ( -etype==type(i) ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
            ENDIF
            spag_nextblock_1 = 4
         CASE (3)
            mtype = type(i+1)
            spag_nextblock_1 = 4
         CASE (4)
!
!     THIS ELEMENT TYPE MAY BE INCLUDED AS A TYPE AND/OR SOME OF THEM
!     MAY BE INCLUDED SPECIFICALLY. READ -NGPPE- = NUMBER OF GRID
!     POINTS PER ELEMENT FOR THIS TYPE.
!
            CALL fread(ect1,ngppe,1,0)
            IF ( ngppe<=0 ) THEN
!
!     SKIP THIS ELEMENT TYPE (NON-EXISTENT)
!
               CALL fread(ect1,0,0,1)
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ELSE
               idx = (etype-1)*incr
               neltyp = 0
               ne16 = ne(idx+16)
               offset = 0
               IF ( ne16==br ) offset = 6
               IF ( ne16==t3 .OR. ne16==q4 ) offset = 1
!
!     CHECK WHETHER OR NOT THIS ELEMENT TYPE IS TO BE INCLUDED.
!
               IF ( ntypes/=0 .AND. mtype<0 ) THEN
                  DO i = 1 , ntypes , 2
                     IF ( etype==type(i) .OR. type(i)==ntyps+1 ) THEN
                        spag_nextblock_1 = 7
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
               ENDIF
            ENDIF
            spag_nextblock_1 = 5
         CASE (5)
            SPAG_Loop_2_1: DO
!
!     NOW CHECK WHETHER OR NOT ANY OF THE ELEMENTS OF THIS TYPE ARE
!     EXPLICITLY INCLUDED. PUT ALL SUCH ON THE NEW ECT (ECT2).
!
               CALL read(*10,*10,ect1,eid,2,0,i)
               CALL fread(ect1,gpts,ngppe,0)
               IF ( offset/=0 ) CALL fread(ect1,off,offset,0)
               IF ( nel>0 ) THEN
                  m = 0
                  n = 1
!
!     FOR TYPES DELETED ONLY SEARCH LIST AFTER TYPE WAS KNOWN TO BE
!     DELETED (2ND WORD OF TYPE)
!
                  IF ( mtype>0 ) n = mtype
                  IF ( n<=nel ) EXIT SPAG_Loop_2_1
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO SPAG_Loop_2_1
            SPAG_Loop_2_2: DO
               CALL intlst(Ele,n,sign,n1,n2)
               IF ( sign<0 ) THEN
                  IF ( elid>=n1 .AND. elid<=n2 ) m = 0
               ELSE
                  IF ( elid>=n1 .AND. elid<=n2 ) m = 1
               ENDIF
               IF ( n>nel ) EXIT SPAG_Loop_2_2
            ENDDO SPAG_Loop_2_2
            spag_nextblock_1 = 6
         CASE (6)
            IF ( m/=0 ) THEN
               IF ( neltyp==0 ) THEN
                  CALL write(ect2,ne(idx+16),1,0)
                  CALL write(ect2,ngppe,1,0)
               ENDIF
               CALL write(ect2,eid,2,0)
               CALL write(ect2,gpts,ngppe,0)
               IF ( offset/=0 ) CALL write(ect2,off,offset,0)
               neltyp = neltyp + 1
               DO i = 1 , ngppe
                  j = gpts(i)
                  Gp(j) = labgp
               ENDDO
!
!     AERO ELEMENT - CENTER ONLY LABELED
!
               IF ( etype==ae ) Gp(j) = 1
            ENDIF
            spag_nextblock_1 = 5
         CASE (7)
!
!     THIS ELEMENT TYPE IS TO BE INCLUDED, EXCEPT THE ONES EXPLICITLY
!     EXCLUDED
!
!     ONLY SEARCH LIST AFTER TYPE WAS INCLUDED
!
            mtype = type(i+1)
            spag_nextblock_1 = 8
         CASE (8)
            SPAG_Loop_2_4: DO
               CALL read(*10,*10,ect1,eid,2,0,i)
               CALL fread(ect1,gpts,ngppe,0)
               IF ( offset/=0 ) CALL fread(ect1,off,offset,0)
               IF ( nel>0 ) THEN
                  m = 1
                  n = 1
                  IF ( mtype>0 ) n = mtype
                  IF ( n<=nel ) THEN
                     SPAG_Loop_3_3: DO
                        CALL intlst(Ele,n,sign,n1,n2)
                        IF ( sign>0 ) THEN
                           IF ( elid>=n1 .AND. elid<=n2 ) m = 1
                        ELSE
                           IF ( elid>=n1 .AND. elid<=n2 ) m = 0
                        ENDIF
                        IF ( n>nel ) THEN
                           IF ( m/=0 ) EXIT SPAG_Loop_3_3
                           CYCLE SPAG_Loop_2_4
                        ENDIF
                     ENDDO SPAG_Loop_3_3
                  ENDIF
               ENDIF
               EXIT SPAG_Loop_2_4
            ENDDO SPAG_Loop_2_4
            IF ( neltyp==0 ) THEN
               CALL write(ect2,ne(idx+16),1,0)
               CALL write(ect2,ngppe,1,0)
            ENDIF
            CALL write(ect2,eid,2,0)
            CALL write(ect2,gpts,ngppe,0)
            IF ( offset/=0 ) CALL write(ect2,off,offset,0)
            DO i = 1 , ngppe
               j = gpts(i)
               Gp(j) = labgp
            ENDDO
!
!     AERO ELEMENT - CENTER ONLY LABELED
!
            IF ( etype==ae ) Gp(j) = 1
            neltyp = neltyp + 1
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
!
!     END OF NEW ECT FOR THIS ELEMENT TYPE
!
 10         IF ( neltyp>0 ) CALL write(ect2,0,1,0)
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
!
!     END OF ECT FOR THIS ELEMENT SET
!
 20         CALL close(ect1,rew)
            CALL write(ect2,0,0,1)
!
!     FLAG ALL GRID POINTS TO BE EXCLUDED FROM A DEFORMED SHAPE.
!
            CALL fread(mset,ngpts,1,0)
            IF ( ngpts>=Max ) CALL mesage(-8,0,name)
            Ele(ngpts+1) = 0
            CALL fread(mset,Ele,ngpts,1)
            IF ( ngpts>0 ) THEN
               CALL gopen(exgpid,Buf(b1),inprew)
               DO gpt = 1 , ngp
                  CALL fread(exgpid,exgp,1,0)
                  CALL fread(exgpid,ingp,1,0)
                  m = 0
                  n = 1
                  SPAG_Loop_3_5: DO
                     CALL intlst(Ele,n,sign,n1,n2)
                     IF ( sign>0 ) THEN
                        IF ( exgp>=n1 .AND. exgp<=n2 ) m = 0
                     ELSE
                        IF ( exgp>=n1 .AND. exgp<=n2 ) m = ingp
                     ENDIF
                     IF ( n>ngpts ) THEN
                        IF ( m/=0 ) THEN
                           IF ( Gp(m)/=-2 ) Gp(m) = -Gp(m)
                        ENDIF
                        EXIT SPAG_Loop_3_5
                     ENDIF
                  ENDDO SPAG_Loop_3_5
               ENDDO
               CALL close(exgpid,rew)
            ENDIF
!
!     GENERATE A GRID POINT LIST FOR THIS SET (CONVERT THE INTERNAL
!     GRID POINT NUMBERS TO POINTERS TO THE GRID POINTS PECULIAR TO
!     THIS SET)
!
            CALL gopen(grid,Buf(b1),outnor)
            ngpts = 0
            DO i = 1 , ngp
               IF ( Gp(i)/=0 ) THEN
                  ngpts = ngpts + 1
                  Gp(i) = isign(ngpts,Gp(i))
               ENDIF
            ENDDO
            IF ( ngpts==0 ) THEN
               err(1) = 1
               err(2) = setid
               CALL wrtprt(merr,err,msg1,nmsg1)
            ENDIF
!
            CALL write(grid,ngpts,1,0)
            CALL write(grid,Gp,ngp,0)
            IF ( setnum/=nsets ) CALL close(grid,norew)
            EXIT SPAG_DispatchLoop_1
         END SELECT
      ENDDO SPAG_DispatchLoop_1
   ENDDO
!
!     ALL DONE. THE SET DEFINITION FILE (MSET) + THE SHORT ECT FILE
!     (ECT1) WILL NOT BE NEEDED AGAIN.
!
   CALL clstab(grid,rew)
   CALL clstab(ect2,rew)
   CALL close(mset,rew)
END SUBROUTINE cnstrc
