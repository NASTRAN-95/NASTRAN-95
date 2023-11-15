
SUBROUTINE dsxfsz
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'GINOX.COM'
!
! COMMON variable declarations
!
   INTEGER Ifiat(643) , Ifist(100) , Lfist , Mem(2) , Nfist
   COMMON /xfiat / Ifiat
   COMMON /xfist / Lfist , Nfist , Ifist
   COMMON /zzzzzz/ Mem
!
! Local variable declarations
!
   INTEGER i , ifrblk , index , indx , ipblks , itotal , itotl1 , itotl2 , lasblk , lblock , lim , maxusd , maxusm , nexblk ,       &
         & numblk , nun
!
! End of declarations
!
   Idsn = Ifilex
   nun = 0
   itotal = 0
   DO
      lasblk = Fcb(6,Idsn)
      ifrblk = Fcb(5,Idsn)
      numblk = lasblk - ifrblk + 1
      IF ( Idsn==Ifilex ) THEN
         ipblks = numblk
         IF ( Fcb(10,Ifilex)/=0 ) THEN
            index = Fcb(10,Ifilex)
            lblock = Mem(index+3)
            ipblks = ipblks + lblock
         ENDIF
      ELSE
         nun = nun + 1
         itotal = itotal + numblk
      ENDIF
      Idsn = iand(Mdsfcb(3,Idsn),Maskh2)
      IF ( Idsn==0 ) THEN
         lim = 2*Nfist
         DO i = 1 , lim , 2
            IF ( Name==Ifist(i) ) THEN
               IF ( Ifist(i+1)>0 ) THEN
                  indx = Ifist(i+1)
                  Ifiat(indx+7) = ipblks*2**16 + nun*2**8
                  Ifiat(indx+8) = itotal*2**16
               ENDIF
               EXIT
            ENDIF
         ENDDO
         maxusm = 0
         maxusd = 0
! ACCUMULATE TOTAL I/O USAGE STATISTICS
         DO i = 1 , 80
            IF ( i/=7 ) THEN
               itotl1 = 0
               itotl2 = 0
               IF ( Fcb(4,i)/=0 ) THEN
                  nexblk = Fcb(10,i)
                  IF ( nexblk/=0 ) itotl1 = Mem(nexblk+3)
                  IF ( Fcb(5,i)/=0 ) itotl2 = Fcb(6,i) - Fcb(5,i) + 1
                  maxusm = maxusm + itotl1
                  maxusd = maxusd + itotl2
               ENDIF
            ENDIF
         ENDDO
         IF ( Maxblk<maxusm ) Maxblk = maxusm
         IF ( Maxdsk<maxusd ) Maxdsk = maxusd
         EXIT
      ENDIF
   ENDDO
END SUBROUTINE dsxfsz
