     H SrtSeq(*LangIDShr) AltSeq(*Ext)
      **************************************************************************
      **                                                                      **
      **            Electronic Billing Ansi x12 837 Format (5010)             **
      **                                                                      **
      **            © copyright 2011 - health care software, inc.             **
      **************************************************************************
      **                         abstract                                     **
      **   2 Modes -                                                          **
      **   B -program used to create electronic claim                         **
      **   P -program used to display a single claim                          **
      **************************************************************************
      **                         modification summary                         **
      **                                                                      **
      **  Change Date   Programmer/Comments                  Install Date     **
      **                                                                      **
      **  10/04/2011    Brett Miller                                          **
      **               -added file EBPSEQ for 277 processing                  **
      **                                                                      **
      **  10/12/2011    Brett Miller                                          **
      **               -added clear to totseg incase you need to create       **
      **                a seperate transaction set for each claim             **
      **                                                                      **
      **  10/20/2011    Brett Miller                                          **
      **               -total charge change for 837P                          **
      **                                                                      **
      **  10/28/2011    Brett Miller                                          **
      **               -changes for relationship code differences             **
      **               -changes for bill splitting                            **
      **                                                                      **
      **  10/31/2011    Brett Miller                                          **
      **               -new relationship table BIR5                           **
      **                                                                      **
      **  11/09/2011    Brett Miller                                          **
      **               -changes for loops 2410A                               **
      **                                                                      **
      **  11/15/2011    Brett Miller                                          **
      **               -changes for max charges                               **
      **                                                                      **
      **  12/01/2011    Brett Miller                                          **
      **               -send cat 1 and cat 2 into XFXPRV                      **
      **               -get lvl info for alternate type                       **
      **               -new pattern #DOCT                                     **
      **               -place of service override at clm level                **
      **                                                                      **
      **  12/27/2011    Brett Miller                                          **
      **               -printing changes                                      **
      **                                                                      **
      **   1/23/2012    Charles Nagy - Ticket 272334                          **
      **               -Multiple ST/SE segment handling                       **
      **               -Additional updates                                    **
      **               -Pulled from RES by Brett Miller
      **                                                                      **
      **   1/23/2012    Brett Miller - Ticket 271962 RES                      **
      **               -dependent changes and new patterns                    **
      **                                                                      **
      **   1/23/2012    Brett Miller                                          **
      **               -changes for NOS procedures                            **
      **                                                                      **
      **   1/26/2012    Brett Miller                                          **
      **               -Data dependencies with patterns                       **
      **                                                                      **
      **  05/07/2012    Michael Kundla                        05/07/2012      **
      **                - Changes for single patient master                   **
      **                                                                      **
      **   5/17/2012    Brett Miller                                          **
      **               -changes for ICN                                       **
      **               -changed lower case 'q' to be '!' because of           **
      **                of the changes to be able to type in lowercase        **
      **               -get doc# at claim level loops                         **
      **                                                                      **
      **   1/22/2014    Brett Miller                                          **
      **               -changes for ICD10                                     **
      **                                                                      **
      **  04/16/2014    Colin Grubb                               06/10/2014  **
      **               -increased work field size                             **
      **                                                                      **
      **  10/09/2014    Steve Ferguson                                        **
      **               -comment out unnecessary bbtorb override               **
      **                                                                      **
      **   2/06/2012    Justin Sarnak                                         **
      **               -Add pattern #REMK                                     **
      **               -Calculate true value of FLCST3 after getting totchg   **
      **   4/09/2013    James Powell                            04/10/2013    **
      **               -only recalculate FLCST3 if not a primary claim        **
      **                pulled from MVP by Steve Ferguson                     **
      **                                                                      **
      **   3/24/2015    Steve Ferguson                                        **
      **               -Add lockbox address                                   **
      **                                                                      **
      **   6/04/2015    Alex Hansen                                           **
      **               -Changed ICN/DCN logic to more closely mimic HBBPRDT   **
      **                                                                      **
      **   6/11/2015    Alex Hansen                                           **
      **               -Rebilled retransmitted claims should also include     **
      **                ICN/DCN number                                        **
      **                                                                      **
      **  06/08/2015    Alex Price                              06/19/2015    **
      **               -Added logic for bill aproval for rebills only flag    **
      **                                                                      **
      **   7/20/2015    Alex Hansen                                           **
      **               -Updated ICN/DCN logic to match updates to HBBPRDT     **
      **                                                                      **
      **  06/16/2015    Justin Sarnak                                         **
      **               -bug fix for ICD-10: always append 'A' to diag segment **
      **                - Installed by Michael Kundla                         **
      **                                                                      **
      **   8/21/2015    Brett Miller                                          **
      **               -ICD10 change for surgical procedures: Need to append  **
      **                'B' to the default                                    **
      **                - Installed by Michael Kundla                         **
      **                                                                      **
      **  11/30/2015    Alex Hansen  GOL-1050                                 **
      **               -Updated ICN/DCN logic to match updates to HBBPRDT     **
      **                (GOL-961)                                             **
      **                - Installed by Michael Kundla                         **
      **                                                                      **
      **  12/23/2015    Alex Hansen  GOL-1050                                 **
      **               -More updates for ICN/DCN                              **
      **                - Installed by Michael Kundla                         **
      **                                                                      **
      **  04/11/2016    Alex Hansen  GOL-1396                                 **
      **               -Added parm to HBXICN                                  **
      **  04/15/2016   -Get ICN number if original claim is also a rebill     **
      **                - Installed by Michael Kundla                         **
      **                                                                      **
      **  05/05/2016    Alex Hansen  GOL-1478                                 **
      **               -Get the ICN number as populated in electronic files   **
      **                - Installed by Michael Kundla                         **
      **                                                                      **
      **  08/03/2016    Alex Price HDEV-9762                    10/24/2016    **
      **               -Added code to return charge descriptions(UC)          **
      **                                                                      **
      **   3/07/2016    James Powell                                          **
      **               -Added ability to override field with blanks           **
      **               -pulled from SUN by Matthew Viola HDEV-12092 10/17/2016**
      **                                                                      **
      **   3/19/2014    Matt Spicher  HDEV-13056               11/28/2016     **
      **               -Add ALTNM logic                                       **
      **               -Pulled from SHS by MMCCLELLAN on 03/28/2014           **
      **               -Pulled from AHS to HCS by Alex Hansen                 **
      **                                                                      **
      **  04/23/2014    Matt McClellan - AHS                                  **
      **               -Moved web preview code back to bottom of pattern sr.  **
      **               -pulled from AHS by Matthew Viola 01/04/2016 HDEV-13755**
      **                                                                      **
      **  02/27/2018    Ryan Brandeisky                                       **
      **               -increased size of lvladr to include the second        **
      **                address line.                                         **
      **                Pulled from COM by CDOUGHERTY HDEV-20998              **
      **                                                                      **
      **  07/31/2018    Kevin Pillsbury  HDEV-24916                           **
      **               -added pattern PV6_ to look up a provider variable     **
      **                by level 6 only, with no payor/plan or cat1/cat2      **
      **                                                                      **
      **  12/01/2014    Alex Hansen                               10/19/2018  **
      **               -added pattern to concatenate remark lines             **
      **               -pulled from LTT by ZHEUSCHKEL HDEV-26506              **
      **                                                                      **
      **  03/21/2019    Alex Hansen  HDEV-30029                               **
      **               -added eClaimID value (for random electronic claim ID) **
      **               -added logic for myAbility to transmit and track       **
      **                claims                                                **
      **                                                                      **
      **  04/19/2019    bgates - HDEV-30634                                   **
      **               -Expanded FLCST3                                       **
      **                                                                      **
      **  03/14/2018    Paul Kawalkowski                                      **
      **                - Lengthened WKST02, STCNTR to 9,0 from 4,0           **
      **                - Lengthened BHT03 and ST02 to 9 from 4               **
      **                Pulled from CHC by Diana Cavaliere HDEV-22102 7/16/19 **
      **                                                                      **
      **  10/28/2019    Jim Powell - HDEV-34216                   12/02/2019  **
      **               -Added logic to handle ALTNM and ALTAD provider        **
      **                variables when previewing electronic claims           **
      **               -Change altername address 1 logic to be consistent     **
      **                with change made in HDEV-20998                        **
      **                                                                      **
      **  05/18/2017    Dean Arnold  HDEV-15363                               **
      **                - Increased UBDCNT from 3,0 to 4,0                    **
      **                - Added counter3 and scounter3 as 4,0                 **
      **  12/09/2019    - Pulled by MPALMIERI                                 **
      **                                                                      **
      **  12/13/2019    Alex Price   HDEV-31385                               **
      **                - Added Cat1 to provider look ups and 5010 file       **
      **                                                                      **
      **  02/17/2020  Catherine Dougherty HDEV-35865                          **
      **              - Add alternate name for box 2 (Pay To Addressee)       **
      **                                                                      **
      **  03/01/2020  Nick Ela            HDEV-36109                          **
      **              - Increase size of ALTNME to allow longer ALTNMs        **
      **                                                                      **
      **  07/10/2020  Alex Hansen  HDEV-37871                                 **
      **              - Increase size of PRVVAR from 70 to 80 to match        **
      **                variable size in XFXPRV                               **
      **                                                                      **
      **  09/16/2020  Alex Hansen  HDEV-39063                                 **
      **              - Added logic for bill approval for system rebills      **
      **                                                                      **
      **  09/07/2021  W Davidson   HDEV-20815                                 **
      **               -Load CDRCOD with an '11' for Medicaid claims over 90  **
      **              - days or bbcdrc if populated                           **
      **                                                                      **
      **  09/14/2022  Matt Viola IBILL-260                                    **
      **              -allow two provider variables in one pattern            **
      **              -add delay code                                         **
      **              -send multiple 2300 loops for bed tax                   **
      **              -override room rate for bed tax                         **
      **              -allowances and payments for ur/ny stored at line item  **
      **              -Bug fix for NY Medicaid when rev 169 was first on      **
      **               claim but not only rev code on claim                   **
      **              -Correct room rate override for bed tax                 **
      **              -Needed new variable to keep track of amount owed for   **
      **               non covering secondary charges                         **
      **              -Use new field FL02H for prior payment amount instead   **
      **               of FLCAC2 (which is now secondary noncovered amount)   **
      **              -Change override logic to check for plan specific       **
      **               overrides if not plan specific loop setup found        **
      **              -Do override logic last (after pattern and specialval)  **
      **              -added provider variable NYMED                          **
      **              -changed override logic to not use goto                 **
      **                                                                      **
      **  01/10/2023  Alex Hansen  IBILL-1268                                 **
      **              -change above logic for overrides to happen last        **
      **               to use an application preference                       **
      **                                                                      **
      **  10/24/2023  Alex Hansen  IBILL-1713                                 **
      **              -add chain to e5pub04t                                  **
      **                                                                      **
      **  07/08/2024  Vicky Wolfe  IBILL-2186                                 **
      **              -Modify to fix 1500 Electronic Claims Bug to insure     **
      **               both electronic and paper claims print with correct    **
      **               service dates on all charges                           **
      **                                                                      **
      **  12/09/2024  Victoria Wolfe IBILL-2306                               **
      **              -create a bill exception that will convert 23 code      **
      **               to FC Value code on electronic UB04 claims             **
      **                                                                      **
      **  12/23/2024  Matt Viola ICON-550                                     **
      **              -pulled changes needed for BrightSpring                 **
      **                                                                      **
      **  02/06/25    Caleb Hefty IBILL-2379                                  **
      **              -Fix the bug where place of service codes are not       **
      **               populated when using pattern #POSD when the 1500 has   **
      **               multiple charges.                                      **
      **                                                                      **
      **  08/14/25    Saikiran Parupalli ICON-967                            **
      **              -Added Bill Exception 861                              **
      **                                                                      **
      **************************************************************************
     fhblbilnm  uf   e           k disk    rename(hbfbill:hbfbillm)
     fhblbilhc  if   e           k disk
     fhmpmast   if   e           k disk
     fhxplvl6   if   e           k disk
     fhxplvl5   if   e           k disk
     fhxplvl4   if   e           k disk
     fhxplvl3   if   e           k disk
     fhxplvl2   if   e           k disk
     fhxplvl1   if   e           k disk
     fhmpmams   if   e           k disk
     fhmllice   if   e           k disk
     fhxpbnfit  if   e           k disk
     fhxpprocc  if   e           k disk
     fhbpcdsc   if   e           k disk
     fhbpcpt4   if   e           k disk
     fhbpbrev   if   e           k disk
     fe5pub04   if   e           k disk    rename(hbfub04:e5fub04)
     fe5p1500   if   e           k disk
     fe5pubdet  uf   e           k disk    rename(hbfubdet:e5fubdet)
     fe5pchr15  if   e           k disk
     fe5ploops  if   e           k disk
     fe5lloops  if   e           k disk    rename(e5floops:e5floops2)
     fe5pfield  if   e           k disk
     fe5lfield  if   e           k disk    rename(e5ffield:e5ffield2)
     fe5pcmpnt  if   e           k disk
     fe5lcmpnt  if   e           k disk    rename(e5fcmpnt:e5fcmpnt2)
     f                                     prefix(x:1)
     febpebtri  if   e           k disk
     fe5povrdt  if   e           k disk
     fe5pFub04  if   e           k disk
     fe5pF1500  if   e           k disk
     fe5pdepnd  if   e           k disk
     fhmpovcs   if   e           k disk
     fe5pub04t  if   e           k disk    rename(hbfub04t:e5fub04t)
     febpseq    uf a e           k disk
     fe5p5010   o    e           k disk    rename(e5f837:e5f5010)
     fe5pelhtml uf a e           k disk
     fprinter   o    f  132        printer oflind(*inof) usropn
      *------------------------------------------------------------------*

     d ary2000         s              1    dim(10000)
     d totalwa         s             11  2 dim(1000)
     d totchga         s             11  2 dim(1000)
     d totcina         s             11  2 dim(1000)
     d totpaya         s             11  2 dim(1000)
     d urNyRev         s              4    dim(1000) inz(*blanks)

     d                 ds
     d hrundt                        14  0
     d  hruntme                       6  0 overlay(hrundt)

     d ldads           ds          1024
     d ldausr                 54     63
     d                sds
     d  pgmnam           *proc
     d                 ds
     d  curtim                 1     14  0
     d  runtme                 1      6  0
     d  rundte                 7     14  0
     d hx6tid          ds
     d  taxid                  1     10
      ** Value Codes - Patient
     d valcod          ds
     d  vcc                    1     24    dim(12)
     d  movc01                 1      2
     d  movc02                 3      4
     d  movc03                 5      6
     d  movc04                 7      8
     d  movc05                 9     10
     d  movc06                11     12
     d  movc07                13     14
     d  movc08                15     16
     d  movc09                17     18
     d  movc10                19     20
     d  movc11                21     22
     d  movc12                23     24

      ** Value Amounts - Patient
     d                 ds
     d  vca                    1    108  2 dim(12)
     d  mova01                 1      9  2
     d  mova02                10     18  2
     d  mova03                19     27  2
     d  mova04                28     36  2
     d  mova05                37     45  2
     d  mova06                46     54  2
     d  mova07                55     63  2
     d  mova08                64     72  2
     d  mova09                73     81  2
     d  mova10                82     90  2
     d  mova11                91     99  2
     d  mova12               100    108  2

     d rtnaddress      ds            80
     d  rtnadr                 1     25
     d  rtnad2                26     50
     d  rtncty                51     65
     d  rtnst                 66     67
     d  rtnzp1                68     72
     d  rtnzp2                73     76

     d bgdFut          ds
     d  bgdDTc                        3    overlay(bgdFut : 1)
     d  bgdDTs                       18    overlay(bgdFut : 4)

     d                 ds
     d bgdFil                        39
     d  bgd508                        1    overlay(bgdFil : 39)
     d  b5snpi                        1    overlay(bgdFil : 40)
      **************************************************************************
     d*altnme          s             30
     d altnme          s             60
     d altnm2          s             60
     d altad1          s             40
     d altad2          s             20
     d altcty          s             15
     d altsta          s              2
     d altSeqNum       s               n
     d altzp1          s              5
     d altzp2          s              4
      *****d bht03           s              4
     d bht03           s              9
     d billtrack       s             14
     d billtrack20     s             20
     d countr          s              3  0
     d counter2        s              3  0
     d counter3        s              4  0
     d SCounter2       s              3  0
     d Scounter3       s              4  0
     d clmseq          s              5  0 inz(1)
     d cdrcod          s              2
     d date6           s              6
     d dataflag        s               n
     d datafield       s          10000
     d datafields      s          10000
     d dataline        s          10000
     d depfield        s             10    inz('FIELD')
     d depcomp         s             10    inz('COMP ')
     d depdata         s             10    inz('DATA ')
     d depdataN        s             10    inz('NOTDATA')
     d descrp          s             24
     d doc#            s              9  0
     d eClaimID        s             32
     d fielddsc        s             30
     d frdate          s              8  0
     d todate          s              8  0
     d icndcn          s             23
     d index           s              5  0
     d header          c                   'CLAIMS SENT TO '
     d hldsep          s              1
     d hold1           s              1
     d hold2           s              1
     d hold3           s              1
     d hlddft          s             30
     d hldhcs          s              6
     d i               s              5  0
     d insdsc          s             15
     d instyp          s              1
     d lbxadr          s             40
     d lcode           s              5
     d lv1nam          s             40
     d lvlnam          s             30
     d lvlphn          s             10  0
     d lvladr          s             40
     d lvlcty          s             30
     d lvlsta          s              2
     d lvlzp1          s              5
     d lvlzp2          s              4
     d lvltax          s             15
     d maxcharge       s               n
     d mdate           s              8  0
     d mode            s              1
     d myAbility       s               n
     d ohtml           s          32000
     d once            s              1    inz(' ')
     d overrideLast    s               n
     d jj              s              5  0
     d len             s              5  0
     d namein          s             26
     d newsep          s              1
     d last            s             18
     d first           s             12
     d maxseq          s              4  0
     d monday          s              4
     d middle          s              1
     d nonSec          s              9  2
     d nyMedicaid      s               n
     d jrorsr          s              3
     d weuser          s             10
     d wedate          s              8
     d wetime          s              6
     d ovrtype         s              1
     d payor           s              6  0
     d plan            s              5  0
     d prmlv6          s              6  0
     d prmmem          s             10
     d prmpln          s              5  0
     d prmprc          s              8
     d prmpyr          s              6  0
     d prmrev          s              4  0
     d prmsct          s              1
     d prvacc          s             12  0
     d prvct1          s              3
     d prvct2          s              3
     d prvpln          s              5  0
     d prvubc          s              6  0
     d prvvar          s             80
     d prvdte          s              8  0
     d proc#           s              8
     d prtfdt          s              8  0
     d prvid           s              5
     d rc4num          s              4  0
     d reqfrm          s              2
     d reqtyp          s              2
     d rev169          s              3  0 dim(99)
     d rev169Count     s              3  0
     d rpthdr          s             40
     d rqpos           s              2
     d rqport          s              1
     d rttype          s              2
     d rqsct1          s              3
     d rqsct2          s              3
     d rqsct3          s              3
     d rqspln          s              5  0
     d rqspyr          s              6  0
     d savfrm          s              2
     d savlop          s              6
     d savseq          s              5  0
     d savtth          s                   like(bgdtth)
     d savtyp          s              2
     d sav510          s              1
     d sbmlvl          s              6  0
     d sfdseq          s              5  0
     d sfdlop          s              6
     d sfdseg          s              5
     d sfdfsq          s              5  0
     d sfname          s              6
     d sfddsc          s             70
     d sfddlt          s              1
     d sfsmin          s              4  0
     d sfsmax          s              4  0
     d sformt          s              3
     d sfpatt          s             15
     d sfhcsf          s              6
     d sfdeft          s             30
     d sftsep          s              1
     d sfusge          s              1
     d stotal          s              6  0
      *****d st02            s              4
     d st02            s              9
      *****d stcntr          s              6  0 inz(0)
     d stcntr          s              9  0 inz(0)
     d svcst3          s             11  2
     d sysrbl          s               n
     d npos            s              5  0
     d ppos            s              5  0
     d spos            s              5  0
     d special         s               n
     d taxid2          s             20
     d taxlevel        s              1
     d taxlevelid      s              6  0
     d tempIndex       s              4  0
     d temp92          s              9  2
     d totchg          s             11  2
     d totchgx         s             11  2
     d totCpy          s              9  2
     d totseg          s             10  0
     d trknm2          s              2  0
     d trknum2         s              3  0
     d time4           s              4
     d tmptim          s              6  0
     d tmp508          s              4  0
     d xa              s              5  0
     d xc              s              5  0
     d xodfrm          s              2
     d xodtyp          s              2
     d xod510          s              1
     d xodubc          s              6  0
     d xodpln          s              5  0
     d xodseq          s              5  0
     d xodlop          s              6
     d xodseg          s              5
     d xodfsq          s              5  0
     d xodcsq          s              5  0
     d xodtbl          s              4
     d xodata          s             30
     d ydate           s              8  0
     d ydate1          s              8  0
     d ydate2          s              8  0
     d ydays           s              5  0
     d year            s              2
     d cyear           s              4
     d wsaccn          s             17
      *****d wkst02          s              4  0 inz(0)
     d wkst02          s              9  0 inz(0)
     d yudate          s              8
     d yutime          s              4
     d zeroseq         s              5  0 inz(0)
     d zip4            s              4    inz('9998')
     d wbgdct1         s              3    inz(' ')
     d wbgdct2         s              3    inz(' ')
     d wbgdct3         s              3    inz(' ')
      *-- Variables for NPI processing
     d snpi            s             15
     d npiName         s             26
     d rqlevel6        s              6  0
     d rqaccount       s             12  0
     d rqdoctor#       s              9  0
     d rqlictype       s              5
     d rqstate         s              2
     d rqstatechk      s              1
     d rqcat2          s              1
     d rtnlicense      s             15
     d rtntherapist    s              9  0
     d rtncat2         s              1
     d rtnexpire       s              8  0
      *-------------------------------------------------------------*
      /copy copysrc,hxxtable
      /include copysrc,hxxappprfp

     d HBXSYSRBL       pr                  extpgm('HBXSYSRBL')
     d  level6                        6  0 const
     d  account                      12  0 const
     d  billtrak                     14    const
     d  sysrebil                       n

     d XFXLICE         pr                  extpgm('XFXLICE')
     d  level6                        6  0 const
     d  account                      12  0 const
     d  doctor#                       9  0 const
     d  lictype                       5    const
     d  state                         2    const
     d  statechk                      1    const
     d  cat2                          1    const
     d  rtnlicense                   15
     d  rtntherapist                  9  0
     d  rtncat2                       1
     d  rtnexpire                     8  0
      *-------------------------------------------------------------*

     c     *entry        plist
     c                   parm                    reqfrm
     c                   parm                    reqtyp
     c                   parm                    billtrack
     c                   parm                    mode
     c                   parm                    weuser
     c                   parm                    wedate
     c                   parm                    wetime
     c                   parm                    weseqn

     c                   eval      i8bfrm = reqfrm
     c                   eval      i8btyp = reqtyp
     c                   time                    curtim
     c                   eval      stotal = 0
      **
     c                   eval      rpthdr = *blanks
     c                   movel(p)  header        rpthdr
     c                   exsr      srctab
     c                   movel     'BELB'        tcode
     c                   movel     reqtyp        ecode
     c                   exsr      srtabl
     c                   if        tind <> 'E'
     c                   eval      rpthdr = header + %trim(ldesc)
     c                   endif

       HXXAPPPRF('Billing' : 'altSeqNum' : prefDesc);
       altSeqNum = (prefDesc = 'Y');

       HXXAPPPRF('Billing' : '5010OverrideLast' : prefDesc);
       overrideLast = (prefDesc = 'Y');

     c                   eval      billtrack20 = billtrack
     c                   time                    hrundt
     c                   move      hrundt        mdate
     c                   exsr      srcmdy
     c                   move      ydate         yudate
     c                   movel     hruntme       yutime

       // Check if using myAbility interface
       EXEC SQL
         SET :myAbility =
           CASE
             WHEN is_api_vendor_active('myAbility','postClaimsBatch') = 'Y'
             THEN '1'
             ELSE '0'
           END;

     c     frmtyp        chain     ebfebtri                           79
     c     *in79         cabeq     *on           skip
     c     frmtyp        chain     e5floops                           79
     c     *in79         cabeq     *on           skip
      **
     c                   if        mode = 'P'
      **
     c     *dtaara       define    ebaicn        icnsq             9 0
     c     *dtaara       define    ebagcn        gcnseq            9 0
     c                   add       1             gcnseq
     c                   if        gcnseq = 0
     c                   add       1000          gcnseq
     c                   endif
     c                   endif

     c                   eval      totchg = 0
     c                   eval      nonSec = 0
     c                   eval      totchgx = 0
     c                   if        mode = 'P'
     c     billtrack     chain     hbfbill                            79
     c     *in79         cabeq     *on           skip
     c     acctky        chain     hmfmast                            79
     c     *in79         cabeq     *on           skip
     c     bbplv6        chain     hxflvl6
     c                   eval      totseg = 0
     c                   eval      lbxadr = hx6lba
     c                   eval      lvlnam = hx6nam
     c                   eval      lvlphn = hx6phn
     c****                   eval      lvladr = hx6a11
     c                   eval      lvladr = %trim(hx6a11) + ' ' + %trim(hx6a12)
     c                   eval      lvlcty = hx6ct1
     c                   eval      lvlsta = hx6st1
     c                   eval      lvlzp1 = hx6z11
     c                   eval      lvlzp2 = hx6z12
      **
     c                   exsr      chkalt
      **
     c                   call      'XFXTAXID'
     c                   parm      bbplv6        sbmlvl
     c                   parm      *blanks       taxid2
     c                   parm      '6'           taxlevel
     c                   parm                    taxlevelid
      **
     c                   eval      lvltax = taxid2
     c                   goto      noloop
     c                   endif
     c     *loval        setll     hbfbillm
     c                   dou       *in71 = *on
     c                   read      hbfbillm                               71
     c     *in71         cabeq     *on           skip
     c                   eval      totseg = 0
     c                   eval      payor = bbpayr
     c                   eval      plan = bbplan
     c     frmtyppp      chain     e5floops                           79
     c                   if        *in79 = *on
     c                   eval      payor = 0
     c                   eval      plan = 0
     c                   endif
     c     acctky        chain     hmfmast                            79
     c                   if        *in79 = *on
     c                   iter
     c                   else
     c     bbplv6        chain     hxflvl6
     c                   eval      totseg = 0
     c                   eval      lbxadr = hx6lba
     c                   eval      lvlnam = hx6nam
     c                   eval      lvlphn = hx6phn
     c****                   eval      lvladr = hx6a11
     c                   eval      lvladr = %trim(hx6a11) + ' ' + %trim(hx6a12)
     c                   eval      lvlcty = hx6ct1
     c                   eval      lvlsta = hx6st1
     c                   eval      lvlzp1 = hx6z11
     c                   eval      lvlzp2 = hx6z12
      **
     c                   exsr      chkalt
      **
     c                   call      'XFXTAXID'
     c                   parm      bbplv6        sbmlvl
     c                   parm      *blanks       taxid2
     c                   parm      '6'           taxlevel
     c                   parm                    taxlevelid
      **
     c                   eval      lvltax = taxid2
     c                   endif
     c                   eval      totchg = 0


     c     noloop        tag

     c                   eval      i8bpyr = bbpayr
     c                   eval      i8bpln = bbplan
     c                   eval      i8bct1 = bbcat1

     c                   eval      i8blpr = bbprov
     c                   movel(p)  ETRSID        i8blid
     c                   eval      prvid = 'SBMTR'
     c                   exsr      srprov
     c                   if        prvvar <> *blanks
     c                   movel(p)  prvvar        i8blid
     c                   movel(p)  prvvar        i8blpr
     c                   endif

     c                   eval      prvid = 'NYMED'
     c                   exsr      srprov
     c                   eval      nyMedicaid = (prvvar = 'Y')

     c     bbplv6        chain     hxflvl6
     c                   if        not %found
     c                   move      *blanks       hx6tid
     c                   eval      hx6flg = 'C'
     c                   eval      lvlnam = *blanks
     c                   eval      lbxadr = *blanks
     c                   else
     c                   z-add     hx6num        i8bll6
     c                   Z-ADD     HX6L5A        i8bll5
     c                   eval      lvlnam = hx6nam
     c                   eval      lbxadr = hx6lba
     c                   exsr      chkalt
     c                   endif
     c                   exsr      chainlevels
     c****               exsr      geticn

     c                   eval      billtrack20 = bbtrak
     c                   if        mode = 'P'
     c     billtrack     cabeq     *blanks       skip
     c                   endif
     c                   if        ETR510 = 'I'
     c     bbtrak        chain     e5fub04                            79
     c                   else
     c     bbtrak        chain     e5f1500                            79
     c                   endif
     c                   if        *in79 = *on
     c                   iter
     c                   endif
     c     bbtrak        chain     e5fub04t
     c                   if        not %found(e5pub04t)
     c                   eval      sht63a = *blanks
     c                   eval      sht63b = *blanks
     c                   eval      sht63c = *blanks
     c                   eval      lng63a = *blanks
     c                   eval      lng63b = *blanks
     c                   eval      lng63c = *blanks
     c                   endif
     c                   exsr      geticn
     c                   if        ETR510 = 'I'

     c                   if        nyMedicaid
     c                   eval      ydate1 = %dec(yudate : 8 :0)
     c                   eval      ydate2 = bbtodt
     c                   exsr      srsubd
     c                   if        ydays > 90
     c                   if        bbcdrc <> '' and bbcdrc <> '00'
     c                   eval      FL07A = bbcdrc
     c                   else
     c                   eval      FL07A = '11'
     c                   endif
     c                   endif
     c                   exsr      reseqUbDet
     c                   endif

     c                   eval      cdrcod = ' '
     c                   eval      ydate1 = %dec(yudate : 8 :0)
     c                   eval      ydate2 = bbtodt
     c                   exsr      srsubd
     c                   if        ydays > 90
     c                   eval      prvid = 'CDRCD'
     c                   exsr      srprov
     c                   if        prvvar <> *blanks
     c                   if        bbcdrc  <> ' '
     c                   eval      cdrcod = %trim(bbcdrc)
     c                   else
     c                   eval      cdrcod = prvvar
     c                   endif
     c                   else
     c                   eval      cdrcod = ' '
     c                   endif
     c                   endif

      * rel 1
     c                   exsr      srctab
     c                   eval      tcode = 'BIR5'
     c                   eval      ecode = FL59A
     c                   exsr      srtabl
     c                   if        tind <> 'E' and %subst(hmap:1:2) <> *blanks
     c                   eval      FL59A = hmap
     c                   endif
      * rel 2
     c                   exsr      srctab
     c                   eval      tcode = 'BIR5'
     c                   eval      ecode = FL59B
     c                   exsr      srtabl
     c                   if        tind <> 'E' and %subst(hmap:1:2) <> *blanks
     c                   eval      FL59B = hmap
     c                   endif

      * rel 3
     c                   exsr      srctab
     c                   eval      tcode = 'BIR5'
     c                   eval      ecode = FL59C
     c                   exsr      srtabl
     c                   if        tind <> 'E' and %subst(hmap:1:2) <> *blanks
     c                   eval      FL59C = hmap
     c                   endif
     c                   endif

     c                   eval      totalwa = *zeros
     c                   eval      totchga = *zeros
     c                   eval      totcina = *zeros
     c                   eval      totpaya = *zeros

     c                   eval      totCpy = 0

     c                   if        ETR510 = 'I'
     c                   exsr      gettotchg
     c                   eval      xc = 1
     c                   else
     c                   eval      savtth = 0
     c                   exsr      gettotchgp
     c                   eval      bgdtth = savtth
     c                   endif
     c                   eval      trknm2 = 0
     c                   eval      countr = 0
     c                   eval      counter2 = 0
     c                   eval      counter3 = 0
     c                   eval      clmseq += 1

     c                   add       1             stotal
     c                   eval      wkst02 += 1
     c                   move(p)   wkst02        st02
     c                   eval      bht03 = st02
      **
      **
     c                   z-add     bbfrdt        ydate
     c                   exsr      srcymd
     c                   z-add     mdate         frdate
      **
     c                   z-add     bbtodt        ydate
     c                   exsr      srcymd
     c                   z-add     mdate         todate
     c                   move      *blanks       insdsc
     c     bilkey        chain     xffbnfit
     c                   if        %found
     c                   movel     xfbnam        insdsc
     c                   endif
     c                   movel(p)  bbtrak        wsaccn
     c                   if        mode <> 'P'
     c                   if        once = *blanks
     c                   eval      once = 'X'
      **
     c                   move      *blanks       lv1nam
     c     bbplv1        chain     hxflvl1
     c                   if        %found
     c                   movel     hx1nam        lv1nam
     c                   call      'XFXCNTR'
     c                   parm                    lv1nam
     c                   endif
     c                   open      printer
     c                   eval      *inof = *on
      **
     c                   endif
     c                   except    detail
     c                   endif

     c                   if        mode = 'P'
     c     frmtyppp      setll     e5floops
     c                   dou       *in78 = *on
     c     reread2       tag
     c     frmtyppp      reade     e5floops                               78
     c                   if        *in78 = *off
     c                             and lsddlt <> 'Y'

     c     lsdhdr        cabne     'H'           reread2
     c*    mode          cabne     'P'           reread2

     c                   eval      dataline = *blanks
     c                   exsr      datadepend
     c     dataflag      cabeq     *off          reread2

     c*                  if        mode = 'P'
     c                   exsr      modecol1
     c*                  else
     c*                  eval      dataline = %trim(lsdseg) + %trim(etrsep)
     c*                  endif
     c     unqkey        setll     e5ffield                                     =BUILD
     c                   dou       *in70 = *on                                  =SUBFILE
     c     unqkey        reade(n)  e5ffield                               70
     c                   if        *in70 = *off
     c                             and IFDDLT <> 'Y'
     c                   exsr      datadependF
     c                   if        dataflag = *off
     c                   exsr      addsep
     c                   iter
     c                   endif

      ** DEPEND FIELD
      *
     c     depkeyF       setll     e5fdepnd
     c     checkother    tag
     c     depkeyF       reade     e5fdepnd                               79
     c                   if        *in79 = *off
     c                   if        depdep <> depfield
     c                             and depdep <> depcomp
     c                   goto      checkother
     c                   endif
      * look at fields
     c                   if        DENCSQ = 0
     c     depfld        chain     e5ffield                           79
     c                   if        *in79 = *on
     c                   eval      len = 0
     c                   eval      len = %len(%trim(dataline))
     c                   if        len <> 0
     c                   if        %subst(dataline:len:1) = etrsep
     c                   eval      %subst(dataline:len:1) = etrssp
     c                   endif
     c                   endif
     c                   if        mode = 'P'
     c                   exsr      writepreview
     c                   else
     c                   exsr      writeelcdata
     c                   endif
     c                   goto      reread2
     c                   else
     c                   eval      datafield = *blanks
     c                   eval      hldhcs = ifhcsf
     c                   eval      ovrtype = 'F'
     c                   exsr      getvalues
     c                   exsr      override
     c                   if        datafield = *blanks
     c                   eval      len = 0
     c                   eval      len = %len(%trim(dataline))
     c                   if        len <> 0
     c                   if        %subst(dataline:len:1) = etrsep
     c                   eval      %subst(dataline:len:1) = etrssp
     c                   endif
     c                   endif
     c                   if        mode = 'P'
     c                   exsr      writepreview
     c                   else
     c                   exsr      writeelcdata
     c                   endif
     c                   goto      reread2
     c                   endif
     c                   endif
     c                   endif
     c                   endif
      ** DEPEND FIELD
     c     comkey        chain     e5fcmpnt                           79
     c                   if        *in79 = *off
     c                   exsr      components
     c                   else
     c                   if        ifhcsf <>  *blanks
     c                   eval      fielddsc = *blanks
     c                   if        ETR510 = 'I'
     c     ifhcsf        chain     e5fFub04                           79
     c                   else
     c     ifhcsf        chain     e5fF1500                           79
     c                   endif
     c                   if        *in79 = *off
     c                   if        ETR510 = 'I'
     c                   eval      fielddsc = %trim(u4name) + '-' + u4desc
     c                   else
     c                   eval      fielddsc = %trim(b5name) + '-' + b5desc
     c                   endif
     c                   endif
     c                   eval      hldhcs = ifhcsf
     c                   eval      ovrtype = 'F'
     c                   exsr      getvalues
     c                   if        not overrideLast
     c                   exsr      override
     c                   endif
     c                   if        datafield = *blanks
     c                   eval      datafield = ifdeft
     c                   endif
     c                   if        ifpatt <> *blanks
     c                   exsr      pattern
     c                   endif
     c                   if        overrideLast
     c                   exsr      override
     c                   endif
     c                   if        mode <> 'P'
     c                   eval      datafield = %subst(datafield:1:ifsmax)
     c                   endif
     c                   if        mode = 'P'
     c                   exsr      modecol2
     c                   else
     c                   eval      dataline = %trim(dataline) +
     c                                         %trim(datafield)
     c                   endif
     c                   else
     c                   eval      special = *off
     c                   eval      hlddft = ifdeft
     c                   exsr      specialval
     c                   if        ifpatt <> *blanks
     c                   exsr      pattern
     c                   endif
     c                   if        overrideLast
     c                   exsr      override
     c                   endif
     c                   if        mode <> 'P'
     c                   eval      datafield = %subst(datafield:1:ifsmax)
     c                   endif
     c                   eval      dataline = %trim(dataline) +
     c                                         %trim(datafield)
     c                   endif
     c                   if        ifsmin = ifsmax and ifsmin <> 1
     c                             and mode <> 'P'
     c                   Eval      index = %size(dataline)-1
     c                   dow       index > 0
     c                   if        %subst(dataline:index:1)= etrsep or
     c                             %subst(dataline:index:1)= etrssp or
     c                             %subst(dataline:index:1)= etrces
     c                   leave
     c                   endif
     c                   eval      index = index - 1
     c                   enddo
     c                   if        index <> 0
     c                   select
     c                   when      iftsep = 'D'
     c                   eval      %subst(dataline:index+ifsmax+1:1) = etrsep
     c                   when      iftsep = 'S'
     c                   eval      %subst(dataline:index+ifsmax+1:1) = etrssp
     c                   when      iftsep = 'C'
     c                   eval      %subst(dataline:index+ifsmax+1:1) = etrces
     c                   endsl
     c                   endif
     c
     c                   else
     c                   exsr      addsep
     c                   endif
      ** END COM
     c                   endif
     c                   endif
     c                   enddo
     c                   if        mode = 'P'
     c                   exsr      writepreview
     c                   else
      ** SKIP all header on batch run
     c***                exsr      writeelcdata
     c                   endif
     c                   endif
     c                   enddo
      ** end header section
     c                   endif

     c                   eval      maxcharge = *off
     c     toomanylines  tag
     c                   if        maxcharge = *On
      ** MAKE SURE ANOTHER CHARGE
     c****                   eval      scounter2 = counter2 + 1
     c                   if        ETR510 = 'I'
     c                   eval      scounter3 = counter3 + 1
     c     Strkchg       chain(n)  e5fubdet                           60
     c                   else
     c                   eval      scounter2 = counter2 + 1
     c     Strakchg      chain     e5fchr15                           60
     c                   endif
     c                   if        *in60 = *on
     c                   goto      reread
     c                   endif
      ** MAKE SURE ANOTHER CHARGE
     c                   if        ETR510 = 'I'
     c                             and altSeqNum
     c                   eval      lsdseq = 420
     c                   else
     c                   eval      lsdseq = 230
     c                   endif
     c     begingloop    setll     e5floops
     c                   eval      maxcharge = *off
     c                   goto      reread
     c                   endif
     c     frmtyppp      setll     e5floops
     c                   dou       *in78 = *on
     c     reread        tag
     c     frmtyppp      reade     e5floops                               78
     c                   if        *in78 = *off
     c                             and lsddlt <> 'Y'
     c                   if        lsdhdr <> 'F'  and lsdhdr <> 'C'
     c                   iter
     c                   endif

     c                   if        ETR510 = 'P'
     c                   eval      totchg = totchga(trknm2 + 1)
     c                   elseif    nyMedicaid
     c                   eval      totchg = totchga(xc)
     c                   if        flcac2 = 'A8' and flcac1 <> 'A8'
     c                   eval      nonSec = -totcina(xc)
     c                   elseif    flcac1 = 'A8' and flcac2 = 'A8'
     c                   eval      nonSec = totchg
     c                   else
     c                   eval      nonSec = 0
     c                   endif
     c                   eval      flcst1 = *blanks
     c                   eval      flcst2 = *blanks
     c                   eval      flcst3 = 0
     c                   eval      flcs01 = *blanks
     c                   eval      flcs02 = *blanks
     c                   eval      flcs03 = 0
     c                   eval      fl75a = *blanks
     c                   eval      fl02h = '000000000'
     c                   if        totalwa(xc) <> 0
     c                   eval      flcst1 = 'CO'
     c                   if        totalwa(xc) < 0
     c                   eval      flcst2 = '45'
     c                   else
     c                   eval      flcst2 = '94'
     c                   endif
     c                   eval      flcst3 = -totalwa(xc)
     c                   endif
     c                   if        totcina(xc) <> 0
     c                   if        totalwa(xc) = 0
     c                   eval      flcst1 = 'PR'
     c                   eval      flcst2 = '2'
     c                   eval      flcst3 = -totcina(xc)
     c                   else
     c                   eval      flcs01 = 'PR'
     c                   eval      flcs02 = '2'
     c                   eval      flcs03 = -totcina(xc)
     c                   endif
     c                   endif
     c                   if        totpaya(xc) <> 0
     c                   eval      fl75a = 'D'
     c                   movel     totpaya(xc)   fl02h
     c                   endif
     c                   endif

     c     lsdhdr        cabeq     'C'           chargeskip

     c                   exsr      datadepend
     c                   if        dataflag = *off
     c                   goto      reread
     c                   endif
      *
     c     depkey        setll     e5fdepnd
     c     segdeptag     tag
     c     depkey        reade     e5fdepnd                               79
     c                   if        *in79 = *off
     c                             and depdep <> depfield
     c                             and depdep <> depcomp
     c                             and depdep <> depdataN
     c                             and depfsq = 0
      * look at fields
     c                   if        DENCSQ = 0
     c     depfld        chain     e5ffield                           79
     c                   if        *in79 = *on
     c                   iter
     c                   else
     c                   eval      datafield = *blanks
     c                   eval      hldhcs = ifhcsf
     c                   eval      ovrtype = 'F'
     c                   exsr      getvalues
     c                   if        not overrideLast
     c                   exsr      override
     c                   endif
     c                   if        %subst(ifpatt:1:4) = 'PRV_'
     c                             or %subst(ifpatt:1:3) = 'PR_'
     c                             or %subst(ifpatt:1:4) = 'PV6_'
     c                             or %subst(ifpatt:1:4) = '#DOC'
     c                             or %subst(ifpatt:1:4) = '#FLD'
     c                             or %subst(ifpatt:1:4) = '#PVR'
     c                   exsr      pattern
     c                   endif
     c                   if        overrideLast
     c                   exsr      override
     c                   endif
     c                   if        datafield = *blanks
     c                   iter
     c                   endif
     c                   endif
     c                   else
     c     depcom        chain     e5fcmpnt                           79
     c                   if        *in79 = *on
     c                   iter
     c                   else
     c                   eval      datafield = *blanks
     c                   eval      hldhcs = ichcsf
     c                   eval      ovrtype = 'C'
     c                   exsr      getvalues
     c                   exsr      override
     c                   if        datafield = *blanks
     c                   iter
     c                   endif
     c                   endif
      * look at compents
     c                   endif
     c                   goto      segdeptag
     c                   endif
      *
     c                   eval      dataline = *blanks
     c                   if        mode = 'P'
     c                   exsr      modecol1
     c                   else
     c                   eval      dataline = %trim(lsdseg) + %trim(etrsep)
     c                   endif

     c     chargeskip    tag
     c     unqkey        setll     e5ffield                                     =BUILD
     c                   dou       *in70 = *on                                  =SUBFILE
     c     unqkey        reade(n)  e5ffield                               70
     c                   if        *in70 = *off
     c                             and IFDDLT <> 'Y'
     c                   if        ifpatt = 'REPEAT_CHARGES'
     c                   exsr      repeatCHARGE
     c                   if        maxcharge = *on
     c                   if        nyMedicaid
     c                   eval      xc += 1
     c                   endif
     c                   goto      toomanylines
     c                   else
     c                   goto      reread
     c                   endif
     c***                leave
     c                   endif
     c                   exsr      datadependF
     c                   if        dataflag = *off
     c                   exsr      addsep
     c                   iter
     c                   endif
     c                   if        lsdhdr = 'C'
     c**                 iter
     c                   goto      reread
     c                   endif
      ** DEPEND FIELD
      *
     c     depkeyF       setll     e5fdepnd
     c     checkother2   tag
     c     depkeyF       reade     e5fdepnd                               79
     c                   if        *in79 = *off
     c                   if        depdep <> depfield
     c                   goto      checkother2
     c                   endif
      * look at fields
     c                   if        DENCSQ = 0
     c     depfld        chain     e5ffield                           79
     c                   if        *in79 = *on
     c                   eval      len = 0
     c                   eval      len = %len(%trim(dataline))
     c                   if        len <> 0
     c                   if        %subst(dataline:len:1) = etrsep
     c                   eval      %subst(dataline:len:1) = etrssp
     c                   endif
     c                   endif
      **
     c                   if        mode = 'P'
     c                   exsr      writepreview
     c                   else
     c                   exsr      writeelcdata
     c                   endif
      **
     c                   goto      reread
     c                   else
     c                   eval      datafield = *blanks
     c                   eval      hldhcs = ifhcsf
     c                   eval      ovrtype = 'F'
     c                   exsr      getvalues
     c                   exsr      override
     c                   if        datafield = *blanks
     c                   eval      len = 0
     c                   eval      len = %len(%trim(dataline))
     c                   if        len <> 0
     c                   if        %subst(dataline:len:1) = etrsep
     c                   eval      %subst(dataline:len:1) = etrssp
     c                   endif
     c                   endif
      **
     c                   if        mode = 'P'
     c                   exsr      writepreview
     c                   else
     c                   exsr      writeelcdata
     c                   endif
      **
     c                   goto      reread
     c                   endif
     c                   endif
     c                   endif
     c                   endif
      ** DEPEND FIELD

     c     comkey        chain     e5fcmpnt                           79
     c                   if        *in79 = *off
     c                   exsr      components
     c                   else
     c                   if        ifhcsf <>  *blanks
     c                   eval      fielddsc = *blanks
     c                   if        ETR510 = 'I'
     c     ifhcsf        chain     e5ffub04                           79
     c                   else
     c     ifhcsf        chain     e5ff1500                           79
     c                   endif
     c                   if        *in79 = *off
     c                   if        ETR510 = 'I'
     c                   eval      fielddsc = %trim(u4name) + '-' + u4desc
     c                   else
     c                   eval      fielddsc = %trim(b5name) + '-' + b5desc
     c                   endif
     c                   endif
     c                   eval      hldhcs = ifhcsf
     c                   eval      ovrtype = 'F'
     c                   exsr      getvalues
     c                   if        not overrideLast
     c                   exsr      override
     c                   endif
     c                   if        datafield = *blanks
     c                   eval      datafield = ifdeft
     c                   endif
     c                   if        ifpatt <> *blanks
     c                   exsr      pattern
     c                   endif
     c                   if        overrideLast
     c                   exsr      override
     c                   endif
     c                   if        mode <> 'P'
     c                   eval      datafield = %subst(datafield:1:ifsmax)
     c                   endif
     c                   if        mode = 'P'
     c                   exsr      modecol2
     c                   else
     c                   eval      dataline = %trim(dataline) +
     c                                         %trim(datafield)
     c                   endif
     c                   else
     c                   eval      special = *off
     c                   eval      hlddft = ifdeft
     c                   exsr      specialval
     c                   if        ifpatt <> *blanks
     c                   exsr      pattern
     c                   endif
     c                   if        overrideLast
     c                   exsr      override
     c                   endif
     c                   if        mode <> 'P'
     c                   eval      datafield = %subst(datafield:1:ifsmax)
     c                   endif
     c                   eval      dataline = %trim(dataline) +
     c                                         %trim(datafield)
     c                   endif
     c                   exsr      addsep
      ** END COM
     c                   endif
     c                   endif
     c                   enddo
     c                   if        mode = 'P'
     c                   exsr      writepreview
     c                   else
     c                   exsr      writeelcdata
     c                   endif
     c                   endif
     c                   enddo
      ** END LOOP -

     c                   if        mode = 'P'
     c     frmtyppp      setll     e5floops
     c                   dou       *in78 = *on
     c     reread3       tag
     c     frmtyppp      reade     e5floops                               78
     c                   if        *in78 = *off
     c                             and lsddlt <> 'Y'
     c                   if        lsdhdr <> 'E'
     c                   iter
     c                   endif
     c*                  if        mode <> 'P'
     c*                  iter
     c*                  endif
     c                   exsr      datadepend
     c                   if        dataflag = *off
     c                   goto      reread3
     c                   endif
     c                   eval      dataline = *blanks
     c*                  if        mode = 'P'
     c                   exsr      modecol1
     c*                  else
     c*                  eval      dataline = %trim(lsdseg) + %trim(etrsep)
     c*                  endif
     c     unqkey        setll     e5ffield                                     =BUILD
     c                   dou       *in70 = *on                                  =SUBFILE
     c     unqkey        reade(n)  e5ffield                               70
     c                   if        *in70 = *off
     c                             and IFDDLT <> 'Y'
     c                   exsr      datadependF
     c                   if        dataflag = *off
     c                   exsr      addsep
     c                   iter
     c                   endif
     c     comkey        chain     e5fcmpnt                           79
     c                   if        *in79 = *off
     c                   exsr      components
     c                   else
     c                   if        ifhcsf <>  *blanks
     c                   eval      fielddsc = *blanks
     c                   if        ETR510 = 'I'
     c     ifhcsf        chain     e5ffub04                           79
     c                   else
     c     ifhcsf        chain     e5ff1500                           79
     c                   endif
     c                   if        *in79 = *off
     c                   if        ETR510 = 'I'
     c                   eval      fielddsc = %trim(u4name) + '-' + u4desc
     c                   else
     c                   eval      fielddsc = %trim(b5name) + '-' + b5desc
     c                   endif
     c                   endif
     c                   eval      hldhcs = ifhcsf
     c                   eval      ovrtype = 'F'
     c                   exsr      getvalues
     c                   if        not overrideLast
     c                   exsr      override
     c                   endif
     c                   if        datafield = *blanks
     c                   eval      datafield = ifdeft
     c                   endif
     c                   if        ifpatt <> *blanks
     c                   exsr      pattern
     c                   endif
     c                   if        overrideLast
     c                   exsr      override
     c                   endif
     c*                  if        mode <> 'P'
     c*                  eval      datafield = %subst(datafield:1:ifsmax)
     c*                  endif
     c*                  if        mode = 'P'
     c                   exsr      modecol2
     c*                  else
     c*                  eval      dataline = %trim(dataline) +
     c*                                        %trim(datafield)
     c*                  endif

     c                   else
     c                   eval      special = *off
     c                   eval      hlddft = ifdeft
     c                   exsr      specialval
     c                   if        ifpatt <> *blanks
     c                   exsr      pattern
     c                   endif
     c                   if        overrideLast
     c                   exsr      override
     c                   endif
     c*                  if        mode <> 'P'
     c*                  eval      datafield = %subst(datafield:1:ifsmax)
     c*                  endif
     c                   eval      dataline = %trim(dataline) +
     c                                         %trim(datafield)
     c                   endif
     c                   exsr      addsep
      ** END COM
     c                   endif
     c                   endif
     c                   enddo
     c*                  if        mode = 'P'
     c                   exsr      writepreview
     c*                  else
     c*                  exsr      writeelcdata
     c*                  endif
     c                   endif
     c                   enddo
      ** end footer section
     c                   endif
      ** END LOOP -
     c     mode          cabeq     'P'           skip

     c                   move      'Y'           bbelct
     c****               IF        bbaafl <> *blanks

     c                   callp     HBXSYSRBL(bbplv6 : bbaccn : bbtrak : sysrbl)

     c                   if        bbaafl = 'X'
     c                             or (bbaafl ='R' and bbbsts <> 'R')
     c                             or (bbaafl ='S' and sysrbl = *off)
     c                   movel(P)  '*SYSTEM'     bbtsnm
     c                   else
     c                   movel     LDAUSR        BBTSNM
     c                   endif
     c                   eval      bbcdrc   =    cdrcod
     c                   time                    tmptim
     c                   movel     tmptim        BBTSTM
     c                   z-add     *date         mdate
     c                   exsr      srcmdy
     c                   z-add     ydate         BBTSDT
     c                   update    hbfbillm

       // Write out detail records for each claim if sending to myAbility
       if myAbility;
         EXEC SQL
           INSERT INTO tbma837clm (
             claim_837_unique_id,
             bill_tracking_number,
             claim_status,
             claim_status_ts)
           VALUES (
             :eClaimID,
             :bbtrak,
             'Awaiting Validation',
             CURRENT_TIMESTAMP);
       endif;

     c                   enddo

     c     skip          tag
     c                   if        mode <> 'P' and stotal <> 0
     c                   except    prvtot
     c                   except    lastr
     c                   close     printer
     c                   endif

     c                   eval      *inlr = *on
     c********************************************************************
     c     modecol1      begsr
     c                   if        lsduse = 'R'
     c                   eval      dataline =
     c                             '<span style=' + '''color:blue'''+
     c                             ' title='''+%trim(lsdlop)     +
     c                             ' : '+%trim(lsdseg)     +
     c                               '-' + %trim(lsddsc) +'''>' +
     c                               %trim(lsdseg) +
     c                              '</span>' + %trim(etrsep)
     c                   else
     c                   eval      dataline =
     c                             '<span style=' + '''color:purple'''+
     c                             ' title='''+%trim(lsdlop)     +
     c                             ' : '+%trim(lsdseg)     +
     c                               '-' + %trim(lsddsc) +'''>' +
     c                               %trim(lsdseg) +
     c                              '</span>' + %trim(etrsep)
     c                   endif
     c                   endsr
     c********************************************************************
     c     modecol2      begsr
     c                   eval      dataline = %trim(dataline) +
     c                             '<span style=' + '''color:blue'''+
     c                             ' title='''+%trim(ifname) +
     c                             '-' + %trim(ifddsc) + '::' +
     c                             %trim(fielddsc) + '''>' +
     c                             %trim(datafield) + '</span>'
     c                   endsr
     c*****************************************************************
     c     *inzsr        begsr

     c     *dtaara       define    *lda          ldads
     c                   in        *dtaara

     c     licky2        klist
     c                   kfld                    hmddr#
     c                   kfld                    lcode

     c     ovrkey        klist
     c                   kfld                    xODFRM
     c                   kfld                    xODTYP
     c                   kfld                    xOD510
     c                   kfld                    xODUBC
     c                   kfld                    xODPLN
     c                   kfld                    xODSEQ
     c                   kfld                    xODLOP
     c                   kfld                    xODSEG
     c                   kfld                    xODFSQ
     c                   kfld                    xODCSQ
     c                   kfld                    xODATA

     c     ovcsKey       klist
     c                   kfld                    bbplv6
     c                   kfld                    bbaccn
     c                   kfld                    bbpayr
     c                   kfld                    bbplan
     c                   kfld                    bbplcy
     c                   kfld                    bbiseq
     c                   kfld                    bbtodt

     c     ovcsKey2      klist
     c                   kfld                    bbplv6
     c                   kfld                    bbaccn
     c                   kfld                    bbpayr
     c                   kfld                    bbplan
     c                   kfld                    bbplcy
     c                   kfld                    bbiseq

     c     bilkey        klist
     c                   kfld                    bbpayr
     c                   kfld                    bbplan


     c     trkchg        klist
     c                   kfld                    billtrack20
     c****                   kfld                    counter2
     c                   kfld                    counter3

     c     acctky        klist
     c                   kfld                    bbplv6
     c                   kfld                    bbaccn

     c     trakchg       klist
     c                   kfld                    bbtrak
     c                   kfld                    counter2

     c     reseqkey      klist
     c                   kfld                    billtrack20
     c                   kfld                    tempIndex

     c     Strakchg      klist
     c                   kfld                    bbtrak
     c                   kfld                    Scounter2

     c     Strkchg       klist
     c                   kfld                    billtrack20
     c****                   kfld                    Scounter2
     c                   kfld                    Scounter3

     c     looprpt       klist
     c                   kfld                    savFRM
     c                   kfld                    savTYP
     c                   kfld                    sav510
     c                   kfld                    payor
     c                   kfld                    plan
     c                   kfld                    savSEQ
     c                   kfld                    savLOP

     c     looprpt2      klist
     c                   kfld                    savFRM
     c                   kfld                    savTYP
     c                   kfld                    sav510
     c                   kfld                    payor
     c                   kfld                    plan

     c     comkey        klist
     c                   kfld                    reqfrm
     c                   kfld                    reqtyp
     c                   kfld                    etr510
     c                   kfld                    payor
     c                   kfld                    plan
     c                   kfld                    IFDSEQ
     c                   kfld                    IFDLOP
     c                   kfld                    IFDSEG
     c                   kfld                    IFDFSQ

     c     begingloop    klist
     c                   kfld                    reqfrm
     c                   kfld                    reqtyp
     c                   kfld                    etr510
     c                   kfld                    payor
     c                   kfld                    plan
     c                   kfld                    lsdseq

     c     depkey        klist
     c                   kfld                    LSD510
     c                   kfld                    LSDFRM
     c                   kfld                    LSDTYP
     c                   kfld                    payor
     c                   kfld                    plan
     c                   kfld                    LSDSEQ
     c                   kfld                    LSDLOP
     c                   kfld                    LSDSEG

     c     depkeyF       klist
     c                   kfld                    LSD510
     c                   kfld                    LSDFRM
     c                   kfld                    LSDTYP
     c                   kfld                    payor
     c                   kfld                    plan
     c                   kfld                    LSDSEQ
     c                   kfld                    LSDLOP
     c                   kfld                    LSDSEG
     c                   kfld                    IFDFSQ

     c     depkeydata    klist
     c                   kfld                    LSD510
     c                   kfld                    LSDFRM
     c                   kfld                    LSDTYP
     c                   kfld                    payor
     c                   kfld                    plan
     c                   kfld                    LSDSEQ
     c                   kfld                    LSDLOP
     c                   kfld                    LSDSEG
     c                   kfld                    zeroseq
     c                   kfld                    depdata

     c     depkeydataN   klist
     c                   kfld                    LSD510
     c                   kfld                    LSDFRM
     c                   kfld                    LSDTYP
     c                   kfld                    payor
     c                   kfld                    plan
     c                   kfld                    LSDSEQ
     c                   kfld                    LSDLOP
     c                   kfld                    LSDSEG
     c                   kfld                    zeroseq
     c                   kfld                    depdataN

     c     depkeydataf   klist
     c                   kfld                    IFD510
     c                   kfld                    IFDFRM
     c                   kfld                    IFDTYP
     c                   kfld                    payor
     c                   kfld                    plan
     c                   kfld                    IFDSEQ
     c                   kfld                    IFDLOP
     c                   kfld                    IFDSEG
     c                   kfld                    IFDFSQ
     c                   kfld                    depdata

     c     depkeydatafN  klist
     c                   kfld                    IFD510
     c                   kfld                    IFDFRM
     c                   kfld                    IFDTYP
     c                   kfld                    payor
     c                   kfld                    plan
     c                   kfld                    IFDSEQ
     c                   kfld                    IFDLOP
     c                   kfld                    IFDSEG
     c                   kfld                    IFDFSQ
     c                   kfld                    depdataN

     c     depkeyc       klist
     c                   kfld                    IFD510
     c                   kfld                    IFDFRM
     c                   kfld                    IFDTYP
     c                   kfld                    payor
     c                   kfld                    plan
     c                   kfld                    IFDSEQ
     c                   kfld                    IFDLOP
     c                   kfld                    IFDSEG
     c                   kfld                    IFDFSQ

     c     depcom        klist
     c                   kfld                    DENFRM
     c                   kfld                    DENTYP
     c                   kfld                    DEN510
     c                   kfld                    payor
     c                   kfld                    plan
     c                   kfld                    DENSEQ
     c                   kfld                    DENLOP
     c                   kfld                    DENSEG
     c                   kfld                    DENFSQ
     c                   kfld                    DENCSQ
     c
     c     depfld        klist
     c                   kfld                    DENFRM
     c                   kfld                    DENTYP
     c                   kfld                    DEN510
     c                   kfld                    payor
     c                   kfld                    plan
     c                   kfld                    DENSEQ
     c                   kfld                    DENLOP
     c                   kfld                    DENSEG
     c                   kfld                    DENFSQ
     c
     c     wehkey        klist
     c                   kfld                    weuser
     c                   kfld                    wedate
     c                   kfld                    wetime
     c                   kfld                    weseqn

     c     wehkey2       klist
     c                   kfld                    weuser
     c                   kfld                    wedate
     c                   kfld                    wetime

     c     unqkey        klist
     c                   kfld                    lsdfrm
     c                   kfld                    lsdtyp
     c                   kfld                    lsd510
     c                   kfld                    payor
     c                   kfld                    plan
     c                   kfld                    lsdseq
     c                   kfld                    lsdlop
     c                   kfld                    lsdseg

     c     frmtyp        klist
     c                   kfld                    reqfrm
     c                   kfld                    reqtyp

     c     frmtyppp      klist
     c                   kfld                    reqfrm
     c                   kfld                    reqtyp
     c                   kfld                    etr510
     c                   kfld                    payor
     c                   kfld                    plan

     c     seqkey        klist
     c                   kfld                    bbtrak
     c                   kfld                    trknum2

     c                   endsr
     c*****************************************************************
     c     datadepend    begsr
     c** DATA DEPENDENT LOOP CHECK

     c                   eval      dataflag = *on

     c     depkeydata    chain     e5fdepnd                           79
     c                   if        *in79 = *off
     c     depfld        chain     e5ffield                           79
     c                   if        *in79 = *on
     c                   eval      dataflag = *off
     c                   else
     c                   if        DENCSQ = 0
     c     depfld        chain     e5ffield                           79
     c                   if        *in79 = *on
     c                   eval      dataflag = *off
     c                   else
     c                   eval      datafield = *blanks
     c                   eval      hldhcs = ifhcsf
     c                   eval      ovrtype = 'G'
     c                   exsr      getvalues
     c                   if        not overrideLast
     c                   exsr      override
     c                   endif
     c                   if        %subst(ifpatt:1:4) = 'PRV_'
     c                             or %subst(ifpatt:1:3) = 'PR_'
     c                             or %subst(ifpatt:1:4) = 'PV6_'
     c                             or %subst(ifpatt:1:4) = '#DOC'
     c                             or %subst(ifpatt:1:4) = '#FLD'
     c                             or %subst(ifpatt:1:4) = '#PVR'
     c                   exsr      pattern
     c                   endif
     c                   if        overrideLast
     c                   exsr      override
     c                   endif
     c                   if        datafield = *blanks
     c                   eval      dataflag = *off
     c                   endif
     c                   endif
     c                   else
     c     depcom        chain     e5fcmpnt                           79
     c                   if        *in79 = *on
     c                   eval      dataflag = *off
     c                   else
     c                   eval      datafield = *blanks
     c                   eval      hldhcs = ichcsf
     c                   eval      ovrtype = 'D'
     c                   exsr      getvalues
     c                   if        not overrideLast
     c                   exsr      override
     c                   endif
     c                   if        %subst(icpatt:1:4) = 'PRV_'
     c                             or %subst(icpatt:1:3) = 'PR_'
     c                             or %subst(icpatt:1:4) = 'PV6_'
     c                             or %subst(icpatt:1:4) = '#DOC'
     c                             or %subst(icpatt:1:4) = '#FLD'
     c                             or %subst(icpatt:1:4) = '#PVR'
     c                             or %subst(icpatt:1:5) = '#BOX4'
     c                             or %subst(icpatt:1:5) = '#ICDV'
     c                   eval      ifpatt = icpatt
     c                   exsr      pattern
     c                   endif
     c                   if        overrideLast
     c                   exsr      override
     c                   endif
     c                   if        datafield = *blanks
     c                   eval      dataflag = *off
     c                   endif
     c                   endif
      * look at compents
     c                   endif
     c                   endif

     c                   if        (DEDAT1 = '#NOTBL' or
     c                             DEDAT2 = '#NOTBL' or
     c                             DEDAT3 = '#NOTBL' or
     c                             DEDAT4 = '#NOTBL' or
     c                             DEDAT5 = '#NOTBL')
     C                             and datafield = *blanks
     c                   eval      dataflag = *off
     c                   endif


     c                   if        datafield <> DEDAT1
     c                             and datafield <> DEDAT2
     c                             and datafield <> DEDAT3
     c                             and datafield <> DEDAT4
     c                             and datafield <> DEDAT5
     c                             and DEDAT1 <> '#NOTBL'
     c                             and DEDAT2 <> '#NOTBL'
     c                             and DEDAT3 <> '#NOTBL'
     c                             and DEDAT4 <> '#NOTBL'
     c                             and DEDAT5 <> '#NOTBL'
     c                   eval      dataflag = *off
     c                   endif

     c                   endif
     c** EMD DATA CHECK
      ** Check for NOT DATA depend
     c     depkeydataN   chain     e5fdepnd                           79
     c                   if        *in79 = *off
     c     depfld        chain     e5ffield                           79
     c                   if        *in79 = *on
     c                   eval      dataflag = *off
     c                   else
     c                   if        DENCSQ = 0
     c     depfld        chain     e5ffield                           79
     c                   if        *in79 = *on
     c                   eval      dataflag = *off
     c                   else
     c                   eval      datafield = *blanks
     c                   eval      hldhcs = ifhcsf
     c                   eval      ovrtype = 'G'
     c                   exsr      getvalues
     c                   if        not overrideLast
     c                   exsr      override
     c                   endif
     c                   if        %subst(ifpatt:1:4) = 'PRV_'
     c                             or %subst(ifpatt:1:3) = 'PR_'
     c                             or %subst(ifpatt:1:4) = 'PV6_'
     c                             or %subst(ifpatt:1:4) = '#DOC'
     c                             or %subst(ifpatt:1:4) = '#FLD'
     c                             or %subst(ifpatt:1:4) = '#PVR'
     c                   exsr      pattern
     c                   endif
     c                   if        overrideLast
     c                   exsr      override
     c                   endif
     c                   endif
     c                   else
     c     depcom        chain     e5fcmpnt                           79
     c                   if        *in79 = *on
     c                   eval      dataflag = *off
     c                   else
     c                   eval      datafield = *blanks
     c                   eval      hldhcs = ichcsf
     c                   eval      ovrtype = 'D'
     c                   exsr      getvalues
     c                   if        not overrideLast
     c                   exsr      override
     c                   endif
     c                   if        %subst(icpatt:1:4) = 'PRV_'
     c                             or %subst(icpatt:1:3) = 'PR_'
     c                             or %subst(icpatt:1:4) = 'PV6_'
     c                             or %subst(icpatt:1:4) = '#DOC'
     c                             or %subst(icpatt:1:4) = '#FLD'
     c                             or %subst(icpatt:1:4) = '#PVR'
     c                             or %subst(icpatt:1:5) = '#BOX4'
     c                             or %subst(icpatt:1:5) = '#ICDV'
     c                   eval      ifpatt = icpatt
     c                   exsr      pattern
     c                   endif
     c                   if        overrideLast
     c                   exsr      override
     c                   endif
     c                   endif
      * look at compents
     c                   endif
     c                   endif

     c                   if        (DEDAT1 = '#NOTBL' or
     c                             DEDAT2 = '#NOTBL' or
     c                             DEDAT3 = '#NOTBL' or
     c                             DEDAT4 = '#NOTBL' or
     c                             DEDAT5 = '#NOTBL')
     C                             and datafield <> *blanks
     c                   eval      dataflag = *off
     c                   endif

     c                   if        (datafield = DEDAT1
     c                             or datafield = DEDAT2
     c                             or datafield = DEDAT3
     c                             or datafield = DEDAT4
     c                             or datafield = DEDAT5)
     c                             and datafield <> *blanks
     c                   eval      dataflag = *off
     c                   endif

     c                   endif
     c                   endsr
     c*****************************************************************
     c     datadependF   begsr
     c** DATA DEPENDENT LOOP CHECK

     c                   eval      sfdseq = ifdseq
     c                   eval      sfdlop = ifdlop
     c                   eval      sfdseg = ifdseg
     c                   eval      sfdfsq = ifdfsq
     c                   eval      sfname = ifname
     c                   eval      sfddsc = ifddsc
     c                   eval      sfddlt = ifddlt
     c                   eval      sfsmin = ifsmin
     c                   eval      sfsmax = ifsmax
     c                   eval      sformt = iformt
     c                   eval      sfpatt = ifpatt
     c                   eval      sfhcsf = ifhcsf
     c                   eval      sfdeft = ifdeft
     c                   eval      sftsep = iftsep
     c                   eval      sfusge = ifusge

     c                   eval      dataflag = *on

     c     depkeydataF   chain     e5fdepnd                           79
     c                   if        *in79 = *off
     c     depfld        chain     e5ffield2                          79
     c                   if        *in79 = *on
     c                   eval      dataflag = *off
     c                   else
     c                   if        DENCSQ = 0
     c     depfld        chain     e5ffield2                          79
     c                   if        *in79 = *on
     c                   eval      dataflag = *off
     c                   else
     c                   eval      datafield = *blanks
     c                   eval      hldhcs = ifhcsf
     c                   eval      ovrtype = 'G'
     c                   exsr      getvalues
     c                   exsr      override
     c                   if        datafield = *blanks
     c                   eval      dataflag = *off
     c                   endif
     c                   endif
     c                   else
     c     depcom        chain     e5fcmpnt                           79
     c                   if        *in79 = *on
     c                   eval      dataflag = *off
     c                   else
     c                   eval      datafield = *blanks
     c                   eval      hldhcs = ichcsf
     c                   eval      ovrtype = 'D'
     c                   exsr      getvalues
     c                   exsr      override
     c                   if        datafield = *blanks
     c                   eval      dataflag = *off
     c                   endif
     c                   endif
      * look at compents
     c                   endif
     c                   endif

     c                   if        (DEDAT1 = '#NOTBL' or
     c                             DEDAT2 = '#NOTBL' or
     c                             DEDAT3 = '#NOTBL' or
     c                             DEDAT4 = '#NOTBL' or
     c                             DEDAT5 = '#NOTBL')
     C                             and datafield = *blanks
     c                   eval      dataflag = *off
     c                   endif

     c                   if        datafield <> DEDAT1
     c                             and datafield <> DEDAT2
     c                             and datafield <> DEDAT3
     c                             and datafield <> DEDAT4
     c                             and datafield <> DEDAT5
     c                             and DEDAT1 <> '#NOTBL'
     c                             and DEDAT2 <> '#NOTBL'
     c                             and DEDAT3 <> '#NOTBL'
     c                             and DEDAT4 <> '#NOTBL'
     c                             and DEDAT5 <> '#NOTBL'
     c                   eval      dataflag = *off
     c                   endif

     c                   endif
     c** EMD DATA CHECK
      ** Check for NOT DATA depend
     c     depkeydataFN  chain     e5fdepnd                           79
     c                   if        *in79 = *off
     c     depfld        chain     e5ffield2                          79
     c                   if        *in79 = *on
     c                   eval      dataflag = *off
     c                   else
     c                   if        DENCSQ = 0
     c     depfld        chain     e5ffield2                          79
     c                   if        *in79 = *on
     c                   eval      dataflag = *off
     c                   else
     c                   eval      datafield = *blanks
     c                   eval      hldhcs = ifhcsf
     c                   eval      ovrtype = 'G'
     c                   exsr      getvalues
     c                   exsr      override
     c                   endif
     c                   else
     c     depcom        chain     e5fcmpnt                           79
     c                   if        *in79 = *on
     c                   eval      dataflag = *off
     c                   else
     c                   eval      datafield = *blanks
     c                   eval      hldhcs = ichcsf
     c                   eval      ovrtype = 'D'
     c                   exsr      getvalues
     c                   exsr      override
     c                   endif
      * look at compents
     c                   endif
     c                   endif

     c                   if        (DEDAT1 = '#NOTBL' or
     c                             DEDAT2 = '#NOTBL' or
     c                             DEDAT3 = '#NOTBL' or
     c                             DEDAT4 = '#NOTBL' or
     c                             DEDAT5 = '#NOTBL')
     c                             and datafield <> *blanks
     c                   eval      dataflag = *off
     c                   endif


     c                   if        datafield = DEDAT1
     c                             or datafield = DEDAT2
     c                             or datafield = DEDAT3
     c                             or datafield = DEDAT4
     c                             or datafield = DEDAT5
     c                   eval      dataflag = *off
     c                   endif

     c                   endif
    
     c                   eval      ifdseq = sfdseq
     c                   eval      ifdlop = sfdlop
     c                   eval      ifdseg = sfdseg
     c                   eval      ifdfsq = sfdfsq
     c                   eval      ifname = sfname
     c                   eval      ifddsc = sfddsc
     c                   eval      ifddlt = sfddlt
     c                   eval      ifsmin = sfsmin
     c                   eval      ifsmax = sfsmax
     c                   eval      iformt = sformt
     c                   eval      ifpatt = sfpatt
     c                   eval      ifhcsf = sfhcsf
     c                   eval      ifdeft = sfdeft
     c                   eval      iftsep = sftsep
     c                   eval      ifusge = sfusge

     c                   endsr
     c*****************************************************************
     c     pattern       begsr

     c                   eval      spos = 0
     c                   eval      spos = %scan('#MODS':ifpatt)
     c                   if        spos <> 0

     c                   eval      hold1 = %subst(ifpatt:7:1)

     c                   select
     c                   when      hold1 = '1'
     c                   eval      datafield = %subst(datafield:1:2)
     c                   when      hold1 = '2'
     c                   eval      datafield = %subst(datafield:3:2)
     c                   when      hold1 = '3'
     c                   eval      datafield = %subst(datafield:5:2)
     c                   when      hold1 = '4'
     c                   eval      datafield = %subst(datafield:7:2)
     c                   endsl
     c                   endif

     c                   eval      spos = 0
     c                   eval      spos = %scan('#NAME':ifpatt)
     c                   if        spos <> 0
     c                   eval      namein = datafield
     c                   call      'XFXBNAM'
     c                   parm                    namein
     c                   parm                    last
     c                   parm                    first
     c                   parm                    middle
     c                   parm                    jrorsr

     c                   eval      hold1 = %subst(ifpatt:7:1)

     c                   eval      datafield = first
     c                   select
     c                   when      hold1 = 'F'
     c                   eval      datafield = first
     c                   when      hold1 = 'M'
     c                   eval      datafield = middle
     c                   when      hold1 = 'L'
     c                   eval      datafield = last
     c                   when      hold1 = 'S'
     c                   eval      datafield = jrorsr
     c                   when      hold1 = 'P'
     c                   eval      datafield = *blanks
     c                   endsl
     c                   endif

     c                   eval      spos = 0
     c                   eval      spos = %scan('#BOX4':ifpatt)
     c                   if        spos <> 0
     c                   eval      hold1 = %subst(ifpatt:7:1)
     c                   eval      hold2 = %subst(ifpatt:8:1)
     c                   eval      hold3 = %subst(ifpatt:9:1)
     c                   select
     c                   when      hold1 = '1'
     c                   eval      hold1 = %subst(datafield:2:1)
     c                   when      hold1 = '2'
     c                   eval      hold1 = %subst(datafield:3:1)
     c                   when      hold1 = '3'
     c                   eval      hold1 = %subst(datafield:4:1)
     c                   endsl
     c                   select
     c                   when      hold2 = '1'
     c                   eval      hold2 = %subst(datafield:2:1)
     c                   when      hold2 = '2'
     c                   eval      hold2 = %subst(datafield:3:1)
     c                   when      hold2 = '3'
     c                   eval      hold2 = %subst(datafield:4:1)
     c                   endsl
     c                   select
     c                   when      hold3 = '1'
     c                   eval      hold3 = %subst(datafield:2:1)
     c                   when      hold3 = '2'
     c                   eval      hold3 = %subst(datafield:3:1)
     c                   when      hold3 = '3'
     c                   eval      hold3 = %subst(datafield:4:1)
     c                   endsl
     c                   eval      datafield = %trim(hold1) +
     c                                %trim(hold2) + %trim(hold3)
     c                   endif

     c                   eval      spos = 0
     c                   eval      spos = %scan('*CONCAT':ifpatt)                            UBTDAT
     c                   if        spos <> 0
     c                   eval      newsep = %subst(ifpatt:8:1)
     c                   eval      datafields = datafield
     c                   eval      hldhcs = %subst(ifpatt:9:6)
     c                   eval      ovrtype = 'F'
     c                   exsr      getvalues
     c                   exsr      override
     c                   if        newsep = '*'
     c                   eval      datafield = %trim(datafields) + ' '
     c                                         + %trim(datafield)
     c                   else
     c                   eval      datafield = %trim(datafields) + %trim(newsep)
     c                                         + %trim(datafield)
     c                   endif
     c                   endif

     c                   eval      spos = 0
     c                   eval      spos = %scan('PRV_':ifpatt)
     c                   if        spos <> 0
     c                   eval      prvid = %subst(ifpatt:5:5)
     c                   exsr      srprov
     c                   if        prvvar <> *blanks
     c                   eval      datafield = %trim(prvvar)
     c                   endif
     c                   endif
     c                   eval      spos = 0
     c                   eval      spos = %scan('PR_':ifpatt)
     c                   if        spos <> 0
     c                   eval      prvid = %subst(ifpatt:4:5)
     c                   exsr      srprov
     c                   if        prvvar <> *blanks
     c                   eval      datafield = %trim(prvvar)
     c                   else
     c                   eval      spos = %scan('PR_':ifpatt:spos+1)
     c                   if        spos <> 0
     c                   eval      prvid = %subst(ifpatt:spos+3:4)              //can only fit 4
     c                   exsr      srprov                                       //char prov var
     c                   if        prvvar <> *blanks
     c                   eval      datafield = %trim(prvvar)
     c                   endif
     c                   endif
     c                   endif
     c                   endif

     c                   eval      spos = 0
     c                   eval      spos = %scan('PV6_':ifpatt)
     c                   if        spos <> 0
     c                   eval      prvid = %subst(ifpatt:5:5)
     c                   exsr      srprov6
     c                   if        prvvar <> *blanks
     c                   eval      datafield = %trim(prvvar)
     c                   endif
     c                   endif

     c                   eval      spos = 0
     c                   eval      spos = %scan('YYYYMMDD':ifpatt)
     c                   if        spos <> 0
     c                   movel     datafield     mdate
     c                   exsr      srcmdy
     c                   eval      datafield = %char(ydate)
     c                   endif

     c                   eval      spos = 0
     c                   eval      spos = %scan('#POSD':ifpatt)
     c                   if        spos <> 0
     c                   exsr      srposd
     c                   endif

     c                   eval      spos = 0
     c                   eval      spos = %scan('STRIP':ifpatt)                              UBTDAT
     c                   if        spos <> 0
     c****8              eval      newsep = %subst(ifpatt:6:1)
     c                   eval      newsep = %subst(ifpatt:spos+5:1)
     c     nextcheck     tag
     c                   eval      spos = 0
     c                   eval      spos = %scan(newsep:datafield)                            UBTDAT
     c                   if        spos <> 0
     c                   eval      npos = spos - 1
     c                   eval      ppos = spos + 1
     c                   eval      datafield = %subst(datafield:1:npos) +
     c****                          %subst(datafield:ppos:2000 - spos)
     c                              %subst(datafield:ppos:10000 - spos)
     c                   goto      nextcheck
     c                   endif
     c                   endif

     c*****              if        datafield <> *blanks
     c*****                        and mode = 'P'
     c*****              eval      datafield = '<span style=' +
     c*****                        '''color:green'''+ ' title='''+%trim(ifname)+
     c*****                        '-'+%trim(ifddsc) + '''>' +
     c*****                        %trim(datafield) + '</span>'
     c*****              endif

     c                   eval      spos = 0
     c                   eval      spos = %scan('#DOCN':ifpatt)
     c                   if        spos <> 0
     c                   movel     datafield     doc#
     c                   eval      datafield = *blanks
     c                   eval      namein = *blanks
     c                   if        b5snpi = 'Y'                                 =bilexc 861
     c                   exsr      srlicense
     c                   if        rtnlicense <> *blanks
     c                   eval      snpi = rtnlicense
     c                   exsr      getNPIName
     c                   if        npiName <> *blanks
     c                   eval      namein = npiName
     c                   endif
     c                   endif
     c                   else
     c     doc#          chain     hmfmams
     c                   if        %found(hmpmams)
     c                   eval      namein = hmdnam
     c                   endif
     c                   endif
     c                   if        namein <> *blanks
     c                   call      'XFXBNAM'
     c                   parm                    namein
     c                   parm                    last
     c                   parm                    first
     c                   parm                    middle
     c                   parm                    jrorsr

     c                   eval      hold1 = %subst(ifpatt:7:1)

     c                   eval      datafield = first
     c                   select
     c                   when      hold1 = 'F'
     c                   eval      datafield = first
     c                   when      hold1 = 'M'
     c                   eval      datafield = middle
     c                   when      hold1 = 'L'
     c                   eval      datafield = last
     c                   when      hold1 = 'S'
     c                   eval      datafield = jrorsr
     c                   when      hold1 = 'P'
     c                   eval      datafield = *blanks
     c                   endsl
     c                   endif
     c                   endif

     c                   eval      spos = 0
     c                   eval      spos = %scan('#DOCL':ifpatt)
     c                   if        spos <> 0
     c                   movel     datafield     doc#
     c                   eval      datafield = *blanks
     c                   if        b5snpi = 'Y'                                 =bilexc 861
     c                   exsr      srlicense
     c                   if        rtnlicense <> *blanks
     c                   eval      datafield = rtnlicense
     c                   endif
     c                   else
     c     doc#          chain     hmfmams
     c                   if        %found(hmpmams)
     c                   eval      datafield = *blanks
     c                   eval      lcode = 'NPI'
     c     licky2        chain     hmllice
     c                   if        %found(hmllice)
     c                   eval      datafield = hlcnum
     c                   endif
     c                   endif
     c                   endif
     c                   endif

     c                   eval      spos = 0
     c                   eval      spos = %scan('#DOCT':ifpatt)
     c                   if        spos <> 0
     c                   movel     datafield     doc#
     c                   eval      datafield = *blanks
     c     doc#          chain     hmfmams
     c                   if        %found(hmpmams)
     c                   eval      datafield = *blanks
     c                   eval      lcode = 'TAXON'
     c     licky2        chain     hmllice
     c                   if        %found(hmllice)
     c                   eval      datafield = hlcnum
     c                   endif
     c                   endif
     c                   endif

     c     chargekey     klist
     c                   kfld                    bgdlv6
     c                   kfld                    bgdtac

     c     lvl6proc      klist
     c                   kfld                    bgdlv6
     c                   kfld                    bgdtpr
     c
     c                   eval      spos = 0
     c                   eval      spos = %scan('#SECDS':ifpatt)
     c                   if        spos <> 0
     c                   eval      datafield = *blanks
      **Begin Prof
     c                   if        ETR510 = 'P'
     c                   exsr      srctab
     c                   movel     'BNOS'        tcode
     c                   movel     bgdtc4        ecode
     c                   exsr      srtabl
     c                   if        tind <> 'E'
     c     chargekey     chain     hbpcdsc
     c                   if        %found and bgcdsc <> *blanks
     c                   eval      datafield = bgcdsc
     c                   else
     c     lvl6proc      chain     hxpprocc
     c                   if        %found(hxpprocc)
     c                   eval      datafield = xfpdsc
     c                   endif
     c                   endif
     c                   endif
     c                   endif
      **End Prof
     c                   endif

     c                   eval      spos = 0
     c                   eval      spos = %scan('#PVR_':ifpatt)
     c                   if        spos <> 0 and datafield = *blanks
     c                   eval      prvid = %subst(ifpatt:6:5)
     c                   exsr      srprov
     c                   if        prvvar <> *blanks
     c                   eval      datafield = %trim(prvvar)
     c                   endif
     c                   endif

     c                   eval      spos = 0
     c                   eval      spos = %scan('#FLD_':ifpatt)
     c                   if        spos <> 0 and datafield = *blanks
     c                   eval      hldhcs = %subst(ifpatt:6:6)
     c                   exsr      getvalues
     c                   exsr      override
     c                   endif

     c                   eval      spos = 0
     c                   eval      spos = %scan('#ICDV':ifpatt)
     c****               if        spos <> 0 and datafield = *blanks
     c                   if        spos <> 0
     c                   if        (ETR510 = 'I' and fl66 = '0') or
     c                             (ETR510 = 'P' and b21icd = '0')
     c                   if        ifdeft = 'BR' or ifdeft = 'BQ'
     c                   eval      datafield = 'B' + %trim(ifdeft)
     c                   else
     c                   eval      datafield = 'A' + %trim(ifdeft)
     c                   endif
     c                   endif
     c                   endif

     c***                eval      spos = 0
     c***                eval      spos = %scan('#REMK':ifpatt)
     c***                if        spos <> 0
     c***                eval      datafield = 'MEDICAL NECESSITY REQUIREMENTS +
     c***                          ARE NOT MET'
     c***                endif

     c                   eval      spos = 0
     c                   eval      spos = %scan('#REMK':ifpatt)
     c                   if        spos <> 0
     c                   eval      datafield = %trim(FL80A) + ' ' +
     c                                         %trim(FL80B) + ' ' +
     c                                         %trim(FL80C) + ' ' +
     c                                         %trim(FL80D)
     c                   endif


     c                   if        datafield <> *blanks
     c                             and mode = 'P'
     c                   eval      datafield = '<span style=' +
     c                             '''color:green'''+ ' title='''+%trim(ifname)+
     c                             '-'+%trim(ifddsc) + '''>' +
     c                             %trim(datafield) + '</span>'
     c                   endif

     c                   endsr
     c*****************************************************************
     c     srprov        begsr
      **   ======        =====
     c                   call      'XFXPRV'
     c                   parm                    bbplv6
     c                   parm                    bbaccn
     c                   parm                    bbpayr
     c                   parm                    bbplan
     c*****              parm      mmpct1        prvct1
     c                   parm      bbcat1        prvct1
     c                   parm      mmpct2        prvct2
     c                   parm                    prvid
     c                   parm      *blanks       prmprc
     c                   parm      0             prmrev
     c                   parm      bbtodt        prvdte
     c                   parm      *blanks       prvvar
      **
     c                   endsr
     c*****************************************************************
     c     srprov6       begsr

     c                   call      'XFXPRV'
     c                   parm                    bbplv6
     c                   parm      0             prvacc
     c                   parm      0             prvubc
     c                   parm      0             prvpln
     c*****              parm      *blanks       prvct1
     c                   parm      bbcat1        prvct1
     c                   parm      *blanks       prvct2
     c                   parm                    prvid
     c                   parm      *blanks       prmprc
     c                   parm      0             prmrev
     c                   parm      bbtodt        prvdte
     c                   parm      *blanks       prvvar

     c                   endsr
      *****************************************************************
     c     components    begsr

     c** Code below compensates for user error - if they put fields in the wrong order
      ** the components will self correct

     c                   eval      len = 0
     c                   eval      len = %len(%trim(dataline))
     c                   if        len <> 0
     c                   if        %subst(dataline:len:1) = etrssp
     c                   eval      %subst(dataline:len:1) = etrsep
     c                   endif
     c                   endif

     c     comkey        setll     e5fcmpnt                                     =BUILD
     c                   dou       *in74 = *on                                  =SUBFILE
     c     comkey        reade(n)  e5fcmpnt                               74
     c                   if        *in74 = *off

     c     depkeyc       chain     e5fdepnd                           79
     c                   if        *in79 = *off
      * look at fields
     c                   if        DENCSQ = 0
     c                   else
     c     depcom        chain     e5fcmpnt2                          79
     c                   if        *in79 = *on
     c                   iter
     c                   else
     c                   eval      datafield = *blanks
     c                   eval      hldhcs = xchcsf
     c                   eval      ovrtype = 'C'
     c                   exsr      getvalues
     c                   exsr      override
     c                   if        datafield = *blanks
     c                   eval      len = 0
     c                   eval      len = %len(%trim(dataline))
     c                   if        len <> 0
     c                   if        %subst(dataline:len:1) = etrsep
     c                   if        depdep <> depfield
     c                   eval      %subst(dataline:len:1) = etrssp
     c                   else
     c                   eval      %subst(dataline:len+1:1) = etrsep
     c                   leave
     c                   endif
     c                   endif
     c                   endif
     c                   iter
     c                   endif
     c                   endif
      * look at compents
     c                   endif
     c                   endif

     c                   if        ichcsf <>  *blanks
     c                   eval      fielddsc = *blanks
     c                   if        ETR510 = 'I'
     c     ichcsf        chain     e5ffub04                           79
     c                   else
     c     ichcsf        chain     e5ff1500                           79
     c                   endif
     c                   if        *in79 = *off
     c                   if        ETR510 = 'I'
     c                   eval      fielddsc = %trim(u4name) + '-' + u4desc
     c                   else
     c                   eval      fielddsc = %trim(b5name) + '-' + b5desc
     c                   endif
     c                   endif
     c                   eval      hldhcs = ichcsf
     c                   eval      ovrtype = 'C'
     c                   exsr      getvalues
     c                   if        not overrideLast
     c                   exsr      override
     c                   endif
     c                   if        datafield = *blanks
     c                   eval      datafield = icdeft
     c                   endif
     c                   if        icpatt <> *blanks
     c                   eval      ifpatt = icpatt
     c                   eval      ifdeft = icdeft
     c                   exsr      pattern
     c                   endif
     c                   if        overrideLast
     c                   exsr      override
     c                   endif
     c                   if        mode <> 'P'
     c                   eval      datafield = %subst(datafield:1:icsmax)
     c                   endif
     c                   if        mode = 'P'
     c                   exsr      modecol2
     c                   else
     c                   eval      dataline = %trim(dataline) +
     c                                         %trim(datafield)
     c                   endif
     c                   else
     c                   eval      special = *off
     c                   eval      hlddft = icdeft
     c                   exsr      specialval
     c                   if        icpatt <> *blanks
     c                   eval      ifpatt = icpatt
     c                   eval      ifdeft = icdeft
     c                   exsr      pattern
     c                   endif
     c                   if        overrideLast
     c                   exsr      override
     c                   endif
     c                   if        mode <> 'P'
     c                   eval      datafield = %subst(datafield:1:icsmax)
     c                   endif
     c                   eval      dataline = %trim(dataline) +
     c                                         %trim(datafield)
     c                   endif
     c                   select
     c                   when      ictsep = 'D'
     c                   eval      dataline = %trim(dataline) + etrsep
     c                   when      ictsep = 'S'
     c                   eval      dataline = %trim(dataline) + etrssp
     c                   when      ictsep = 'C'
     c                   eval      dataline = %trim(dataline) + etrces
     c                   endsl
     c                   endif
     c                   enddo

     c                   movea     dataline      ary2000
      /free
           for i = %len(%trim(dataline)) downto 1;
              if i = %len(%trim(dataline)) ;
                 hldsep = ary2000(i);
              else;
                 if ary2000(i) <> etrces;
                    leave;
                 endif;
              endif;
                ary2000(i) = *blanks;
           endfor;
                ary2000(i + 1) = hldsep ;
      /end-free

     c                   movea     ary2000       dataline

     c                   endsr
     c*****************************************************************
     c     specialval    begsr

     c                   select
     c                   when      hlddft = '*BLANKS'
     c                   eval      datafield = *blanks
     c                   when      hlddft = '*DATE'
     c                   eval      datafield = yudate
     c                   eval      special = *on
     c                   when      hlddft = '*DATE6'
     c                   move      yudate        date6
     c                   eval      datafield = date6
     c                   eval      special = *on
     c                   when      hlddft = '*TIME'
     c                   eval      datafield = yutime
     c                   eval      special = *on
     c                   when      hlddft = '*TIME4'
     c                   movel     yutime        time4
     c                   eval      datafield = time4
     c                   eval      special = *on
     c                   other
     c                   eval      datafield = hlddft
     c                   endsl

     c                   if        special = *on
     c                   if        mode = 'P'
     c                   eval      datafield =
     c                             '<span style=' + '''color:red'''+
     c                             ' title='''+%trim(hlddft)  +
     c                               '''>' +
     c                               %trim(datafield) +
     c                              '</span>'
     c                   endif
     c                   else
     c                   if        hlddft <> '*BLANKS'
     c                             and mode = 'P'
     c                   eval      datafield = '<span style=' +
     c                             '''color:red'''+ ' title='''+%trim(ifname)+
     c                             '-'+%trim(ifddsc) + '''>' +
     c                             %trim(datafield) + '</span>'
     c                   endif
     c                   endif

     c                   endsr
     c*****************************************************************
     c     repeatCHARGE  begsr

     c                   eval      savFRM = LSDFRM
     c                   eval      savTYP = LSDTYP
     c                   eval      sav510 = LSD510
     c                   eval      savSEQ = LSDSEQ
     c                   eval      savLOP = LSDLOP
     c


     c                   eval      countr = 0
     c                   eval      trknm2 += 1

     c     nxtchg        tag
     c                   eval      countr += 1
     c****                   eval      counter2 += 1
     c                   if        ETR510 = 'I'
     c                   eval      counter3 += 1
     c     trkchg        chain(n)  e5fubdet                           60
     c                   else
     c                   eval      counter2 += 1
     c     trakchg       chain     e5fchr15                           60
     c                   endif
     c     *in60         cabeq     *on           endrpt

     c                   if        nyMedicaid
     c                             and (ubdtrc= '0169' and urNyRev(xc)<>'0169'
     c                             or ubdtrc<>'0169' and urNyRev(xc)<>'*all')
     c                   eval      counter3 -= 1
     c                   eval      maxcharge = *on
     c                   goto      endrpt
     c                   endif

     c                   eval      trknum2 = trknm2
     c                   if        countr = 1
     c     seqkey        setll     ebfseq
     c                   dou       *in76 = '1'
     c     seqkey        reade     ebfseq                                 76
     c                   if        *in76 = '0'
     c                   delete    ebfseq
     c                   endif
     c                   enddo
     c                   endif

     c                   eval      sqtrak = bbtrak
     c                   eval      sqtrk2 = trknm2
     c                   eval      sqseq = countr
     c                   if        ETR510 = 'I'
     c                   eval      sqchrg = UBDTC4
     c                   if        UBDTSD <> 0
     c                   eval      mdate = UBDTSD
     c                   exsr      srcmdy
     c                   eval      sqfrdt = ydate
     c                   eval      sqtodt = ydate
     c                   else
     c                   eval      sqfrdt = UBFDAT
     c                   eval      sqtodt = UBTDAT
     c                   endif
     c                   else
     c                   eval      sqchrg = BGDTC4
     c                   eval      sqfrdt = bgdtsd
     c                   eval      sqtodt = bgdtsd
     c                   endif
     c                   write     ebfseq

     c     looprpt       setll     e5floops2
     c                   dou       *in68 = *on
     c     rereadc       tag
     c     looprpt2      reade     e5floops2                              68
     c                   if        *in68 = *off
     c                             and lsddlt <> 'Y'
     c***                if        lsdlop <> savlop
     c***                leave
     c***                endif
     c                   if        lsdhdr <> 'C'
     c***                iter
     c                   leave
     c                   endif

     c                   exsr      datadepend
     c                   if        dataflag = *off
     c                   goto      rereadc
     c                   endif
      **DEP
      *
     c     depkey        setll     e5fdepnd
     c     trydepagn     tag
     c     depkey        reade     e5fdepnd                               79
     c                   if        *in79 = *off
     c                             and depdep <> depfield
      * look at fields
     c                   if        DENCSQ = 0
     c     depfld        chain     e5ffield                           79
     c                   if        *in79 = *on
     c                   iter
     c                   else
     c                   eval      datafield = *blanks
     c                   eval      hldhcs = ifhcsf
     c                   eval      ovrtype = 'F'
     c                   exsr      getvalues
     c                   if        not overrideLast
     c                   exsr      override
     c                   endif
     c                   if        %subst(ifpatt:1:4) = 'PRV_'
     c                             or %subst(ifpatt:1:3) = 'PR_'
     c                             or %subst(ifpatt:1:4) = 'PV6_'
     c                             or %subst(ifpatt:1:4) = '#DOC'
     c                             or %subst(ifpatt:1:4) = '#FLD'
     c                             or %subst(ifpatt:1:4) = '#PVR'
     c                   exsr      pattern
     c                   endif
     c                   if        overrideLast
     c                   exsr      override
     c                   endif
     c                   if        datafield = *blanks
     c                   iter
     c                   endif
     c                   endif
     c                   else
     c     depcom        chain     e5fcmpnt                           79
     c                   if        *in79 = *on
     c                   iter
     c                   else
     c                   eval      datafield = *blanks
     c                   eval      hldhcs = ichcsf
     c                   eval      ovrtype = 'C'
     c                   exsr      getvalues
     c                   exsr      override
     c                   if        datafield = *blanks
     c                   iter
     c                   endif
     c                   endif
      * look at compents
     c                   endif
     c                   goto      trydepagn
     c                   endif
      **DEP
     c                   eval      dataline = *blanks
     c                   if        mode = 'P'
     c                   exsr      modecol1
     c                   else
     c                   eval      dataline = %trim(lsdseg) + %trim(etrsep)
     c                   endif
     c     unqkey        setll     e5ffield                                     =BUILD
     c                   dou       *in67 = *on                                  =SUBFILE
     c     unqkey        reade(n)  e5ffield                               67
     c                   if        *in67 = *off
     c                             and IFDDLT <> 'Y'
     c                   exsr      datadependF
     c                   if        dataflag = *off
     c                   exsr      addsep
     c                   iter
     c                   endif
     c     comkey        chain     e5fcmpnt                           79
     c                   if        *in79 = *off
     c                   exsr      components
     c                   else
     c                   if        ifhcsf <>  *blanks
     c                   eval      fielddsc = *blanks
     c                   if        ETR510 = 'I'
     c     ifhcsf        chain     e5ffub04                           79
     c                   else
     c     ifhcsf        chain     e5ff1500                           79
     c                   endif
     c                   if        *in79 = *off
     c                   if        ETR510 = 'I'
     c                   eval      fielddsc = %trim(u4name) + '-' + u4desc
     c                   else
     c                   eval      fielddsc = %trim(b5name) + '-' + b5desc
     c                   endif
     c                   endif
     c                   eval      hldhcs = ifhcsf
     c                   eval      ovrtype = 'F'
     c                   exsr      getvalues
     c                   if        not overrideLast
     c                   exsr      override
     c                   endif
     c                   if        datafield = *blanks
     c                   eval      datafield = ifdeft
     c                   endif
     c                   if        ifpatt <> *blanks
     c                   exsr      pattern
     c                   endif
     c                   if        overrideLast
     c                   exsr      override
     c                   endif
     c                   if        mode <> 'P'
     c                   eval      datafield = %subst(datafield:1:ifsmax)
     c                   endif
     c                   if        mode = 'P'
     c                   exsr      modecol2
     c                   else
     c                   eval      dataline = %trim(dataline) +
     c                                         %trim(datafield)
     c                   endif
     c                   else
     c                   eval      special = *off
     c                   eval      hlddft = ifdeft
     c                   exsr      specialval
     c                   if        ifpatt <> *blanks
     c                   exsr      pattern
     c                   endif
     c                   if        overrideLast
     c                   exsr      override
     c                   endif
     c                   if        mode <> 'P'
     c                   eval      datafield = %subst(datafield:1:ifsmax)
     c                   endif
     c                   eval      dataline = %trim(dataline) +
     c                                         %trim(datafield)
     c                   endif
     c                   exsr      addsep
      ** END COM
     c                   endif
     c                   endif
     c                   enddo
     c                   if        mode = 'P'
     c                   exsr      writepreview
     c                   else
     c                   exsr      writeelcdata
     c                   endif
     c                   endif
     c                   enddo

     c                   if        ETR510 = 'P' and countr = 50
     c                   eval      maxcharge = *on
     c                   goto      endrpt
     c                   endif
     c                   goto      nxtchg

     c     endrpt        endsr
      *****************************************************************
     c     geticn        begsr

     c                   eval      icndcn = *blanks
     c****               if        bbtorb = '6' or bbtorb = '7' or bbtorb = '8'
     c****                         or bbbsts = 'R' or bbbsts = 'V'
     c****               if        bbbsts = 'R' and (bbtorb = ' '
     c****                         or bbtorb = 'X' or bbtorb = '7'
     c****                         or bbtorb = '8')
     c****                         or bbbsts = 'V' and (bbtorb = ' '
     c****                         or bbtorb = 'X' or bbtorb = '7'
     c****                         or bbtorb = '8')
     c****                         or (bbbsts = 'T' and %subst(bbptob:3:1) = '7'
     c****                         and (bbtorb = ' ' or bbtorb = 'X'
     c****                         or bbtorb = '7' or bbtorb = '8'))
     c****                         or (bbbsts = ' ' and %subst(bbptob:3:1) = '7'
     c****                         and bbtorb <> '8')
     c****                         or (bbbsts = 'B' and bbtorb = '7')

 001 c****               call      'HBXICN'
 001 c****               parm                    bbplv6
 001 c****               parm                    bbaccn
 001 c****               parm                    bbfrdt
 001 c****               parm                    bbtodt
 001 c****               parm                    bbpayr
 001 c****               parm                    bbplan
 001 c****               parm                    bbplcy
 001 c****               parm                    bbiseq
 001 c****               parm                    bbtrak
 001 c****               parm                    icndcn

     c                   if        etr510 = 'I'
     c                   if        FL05A = 'ABC'
     c                   eval      icndcn = FL64A
     c                   elseif    FL05A = 'BAC'
     c                   eval      icndcn = FL64B
     c                   elseif    FL05A = 'BCA'
     c                   eval      icndcn = FL64C
     c                   endif
     c                   else
     c                   eval      icndcn = B522B
     c                   endif

B002 c                   if        icndcn = *blanks
     c                             and bbtcn <> *blanks
     c                   movel     bbtcn         icndcn
     c                   endif
     c****               endif

     c****               if        icndcn = *blanks
     c****               eval      bbtorb = '1'
     c****               endif

     c                   endsr
      *****************************************************************
     c     gettotchg     begsr

     c                   eval      xc = 0
     c                   eval      xa = 0
     c     billtrack20   setll     e5fubdet
     c                   dou       %eof(e5pubdet)
     c     billtrack20   reade(n)  e5fubdet
     c                   if        not %eof(e5pubdet)
     c                   if        nyMedicaid
     c                   if        ubdtrc = '0169'
     c                   if        xa <> 0
     c                   eval      xc = %lookup(ubdtrc : urNyRev : 1 : xa)
     c                   endif
     c                   if        xc = 0
     c                   eval      xa += 1
     c                   eval      urNyRev(xa) = ubdtrc
     c                   eval      xc = xa
     c                   endif
     c                   else
     c                   if        xa <> 0
     c                   eval      xc = %lookup('*all' : urNyRev : 1 : xa)
     c                   endif
     c                   if        xc = 0
     c                   eval      xa += 1
     c                   eval      urNyRev(xa) = '*all'
     c                   eval      xc = xa
     c                   endif
     c                   endif
     c                   eval      totchga(xc) += ubdttc
     c                   eval      totalwa(xc) += ubcs03
     c                   eval      totpaya(xc) += ubcs06
     c                   eval      totcina(xc) += ubcs09
     c                   else
     c                   eval      totchg += ubdttc
     c                   endif

     c                   eval      totchgx += ubdttc
     c                   endif
     c                   enddo

     c                   z-add     flcst3        svcst3
     c                   eval      flcst3 = totchg - flcst3

     c                   endsr
      *****************************************************************
     c     gettotchgp    begsr

     c                   eval      xc = 0
     c                   eval      xa = 1
     c     bbtrak        setll     e5fchr15
     c                   dou       %eof(e5pchr15)
     c     bbtrak        reade     e5fchr15
     c                   if        not %eof(e5pchr15)
     c*******************eval      totchg += bgdtam
     c                   if        bgdtth <> 0
     c                   eval      savtth = bgdtth
     c                   endif
     c                   if        xc = 50
     c                   eval      xa += 1
     c                   eval      xc = 0
     c                   endif
     c                   eval      xc += 1
     c                   eval      totchga(xa) += bgdtam
     c                   eval      totchgx += bgdtam
     c                   select
     c                   when      (bbISeq = bgdSq1)
     c                   eval      totCpy += bgdCp1
     c                   when      (bbISeq = bgdSq2)
     c                   eval      totCpy += bgdCp2
     c                   when      (bbISeq = bgdSq3)
     c                   eval      totCpy += bgdCp3
     c                   endsl
     c                   endif
     c                   enddo

     c                   endsr
     c*****************************************************************
     c     parse100      begsr

     c                   add       1             totseg

     c                   eval      len = %len(%trimr(ohtml))
     c
     c                   if        len <> 0
     c     1             do        len           jj
     c
     c                   eval      wehtml = %subst(ohtml:jj:100)
     c
     c                   if        wehtml <> *blanks
     c                   eval      weseqn = weseqn + 1
     c                   write     e5felhtml
     c                   endif
     c
     c                   eval      jj = jj + 99
     c                   enddo
     c                   endif
     c
     c                   endsr
      *****************************************************************
     c     writepreview  begsr
     c                   eval      ohtml = *blanks
     c                   if        ifhcsf <> *blanks
     c                   eval      ohtml = '<tr><td colspan="7">'+
     c                                ' '+ %trim(dataline) +
     c                                '</td></tr>'
     c                   exsr      parse100
     c                   else
     c                   eval      ohtml = '<tr><td colspan="7">' +
     c                                %trim(dataline) +
     c                                '</td></tr>'
     c                   exsr      parse100
     c                   endif
     c                   endsr
     c*****************************************************************
     c     addsep        begsr
     c                   select
     c                   when      iftsep = 'D'
     c                   eval      dataline = %trim(dataline) + etrsep
     c                   when      iftsep = 'S'
     c                   eval      dataline = %trim(dataline) + etrssp
     c                   when      iftsep = 'C'
     c                   eval      dataline = %trim(dataline) + etrces
     c                   endsl
     c                   endsr
     c*****************************************************************
     c     writeelcdata  begsr

     c                   movea     dataline      ary2000
      /free
           for i = %len(%trim(dataline)) downto 1;
              if i = %len(%trim(dataline)) ;
                 hldsep = ary2000(i);
              else;
                 if ary2000(i) <> etrsep;
                    leave;
                 endif;
              endif;
                ary2000(i) = *blanks;
           endfor;
                ary2000(i + 1) = hldsep ;
      /end-free

     c                   movea     ary2000       dataline

     c                   eval      i8bldt = %trim(dataline)
     c                   eval      i8sgmt = lsdseg
     c                   add       1             totseg
     c                   write     e5f5010

     c                   endsr
     c*****************************************************************
     c     override      begsr
     c                   eval      xodfrm = LSDFRM
     c                   eval      xodtyp = LSDTYP
     c                   eval      xod510 = LSD510
     c                   eval      xodubc = payor
     c                   eval      xodpln = plan
     c                   eval      xodata = %trim(datafield)

     c                   select
     c                   when      ovrtype = 'G'
     c                   eval      xodseq = DENSEQ
     c                   eval      xodlop = DENLOP
     c                   eval      xodseg = DENSEG
     c                   eval      xodfsq = DENFSQ
     c                   eval      xodcsq = 0
     c                   when      ovrtype = 'F'
     c                   eval      xodseq = LSDSEQ
     c                   eval      xodlop = LSDLOP
     c                   eval      xodseg = LSDSEG
     c                   eval      xodfsq = IFDFSQ
     c                   eval      xodcsq = 0
     c                   when      ovrtype = 'C'
     c                   eval      xodseq = ICDSEQ
     c                   eval      xodlop = ICDLOP
     c                   eval      xodseg = ICDSEG
     c                   eval      xodfsq = ICDFSQ
     c                   eval      xodcsq = ICDCSQ
     c                   when      ovrtype = 'D'
     c                   eval      xodseq = DENSEQ
     c                   eval      xodlop = DENLOP
     c                   eval      xodseg = DENSEG
     c                   eval      xodfsq = DENFSQ
     c                   eval      xodcsq = DENCSQ
     c                   endsl

     c****     planOverride  tag
     c                   dow       1 = 1
     c     ovrkey        chain     e5fovrdt
     c                   if        %found(e5povrdt)
     c                             and IOODTA <> *blanks
     c                   if        IOODTA = '*BLANKS'
     c                   eval      datafield = *blanks
     c                   else
     c                   eval      datafield = %trim(ioodta)
     c                   endif
     c                   else
     c                   eval      xODATA = '*ALL'
     c     ovrkey        chain     e5fovrdt
     c                   if        %found(e5povrdt)
     c                             and IOODTA <> *blanks
     c                   if        IOODTA = '*BLANKS'
     c                   eval      datafield = *blanks
     c                   else
     c                   eval      datafield = %trim(ioodta)
     c                   endif
     c                   endif
     c                   endif

     c                   if        payor = 0 and xodubc = 0
     c                   eval      xodubc = bbpayr
     c                   eval      xodpln = bbplan
     c                   eval      xodata = %trim(datafield)
     c****               goto      planOverride
     c                   iter
     c                   else
     c                   leave
     c                   endif

     c                   enddo

     c                   endsr
     c*****************************************************************
     c     chainlevels   begsr

     c     hx6l5a        chain     hxflvl5
     c     hx5l4a        chain     hxflvl4
     c     hx4l3a        chain     hxflvl3
     c     hx3l2a        chain     hxflvl2
     c     hx2l1a        chain     hxflvl1
     c                   endsr
     c*****************************************************************
     c     srcmdy        begsr

     c                   call      'XFXCMDY'
     c                   parm                    mdate
     c                   parm                    ydate

     c                   endsr
     c*****************************************************************
     c     srcymd        begsr

     c                   call      'XFXCYMD'
     c                   parm                    ydate
     c                   parm                    mdate

     c                   endsr
      *****************************************************************
     c     srtabl        begsr
     c                   call      'XFXTABL'
     c                   parm                    tcode
     c                   parm                    ecode
     c                   parm                    hmap
     c                   parm                    edate
     c                   parm                    sdesc
     c                   parm                    ldesc
     c                   parm                    tind
     c                   endsr
      *****************************************************************
     c     srctab        begsr
     c                   eval      tcode = *blanks
     c                   eval      ecode = *blanks
     c                   eval      hmap = *blanks
     c                   eval      edate = 0
     c                   eval      sdesc = *blanks
     c                   eval      ldesc = *blanks
     c                   eval      tind = *blanks
     c                   endsr
      **********************************************************************
     c     srposd        begsr
      **
      **   bbtrak        chain     e5fchr15                           60
      **                 if        %found(e5pchr15)
      **
       sqlCod = 0;
       exec SQL
        Select bgdct1, bgdct2, bgdct3
          Into :wbgdct1, :wbgdct2, :wbgdct3
         From E5PCHR15
        Where bbTrak = :bbTrak
        Fetch First Row Only;

     c                   if        sqlCod = 0
     c                   call      'HBXPTOS'
     c                   parm      bbpayr        rqspyr
     c                   parm      bbplan        rqspln
     c                   parm      wbgdct1       rqsct1
     c                   parm      wbgdct2       rqsct2
     c                   parm      wbgdct3       rqsct3
     c                   parm      'B'           rqport
     c                   parm      '  '          rqpos
     c                   parm      '  '          rttype
     c                   eval      datafield = %trim(rqpos)
     c                   else
     c                   eval      datafield = *blanks
     c                   endif

     c                   endsr
      **********************************************************************************************
     c     getvalues     begsr

     c                   if        ovrtype = 'F' or ovrtype = 'G'
     c                   eval      spos = %scan('INS_':ifpatt)
     c                   if        spos <> 0
     c                   eval      hold1 = %subst(ifpatt:spos+4:1)
     c                   if        hold1 <> ' '
     c                   select
     c                   when      hold1 = 'A'
     c                   eval      spos = %scan('A':FL05A)
     c                   when      hold1 = 'B'
     c                   eval      spos = %scan('B':FL05A)
     c                   when      hold1 = 'C'
     c                   eval      spos = %scan('C':FL05A)
     c                   endsl
     c                   select
     c                   when      spos = 1
     c                   eval      hold1 = 'A'
     c                   when      spos = 2
     c                   eval      hold1 = 'B'
     c                   when      spos = 3
     c                   eval      hold1 = 'C'
     c                   endsl
     c                   eval      hldhcs = %xlate('!':hold1:ifhcsf)
     c                   endif
     c                   endif
     c                   endif

     c                   if        ifname = 'SE01'
     c                   eval      totseg += 1
     c                   endif
      /copy copysrc,e5xvalues

     c                   if        nyMedicaid
     c                   select

      ** Change rate code for bed tax
     c                   when      hldhcs = 'FL39A2'
     c                             and fl39a1 = '24' and urNyRev(xc) = '0169'
     c     ovcsKey       setgt     hmfovcs
     c     ovcsKey2      readpe    hmfovcs
     c                   if        not %eof(hmpovcs)
     c                             and %lookup('84' : vcc) <> 0
     c                   eval      datafield = %char( vca( %lookup('84' : vcc)))
     c                   endif

     c                   when      hldhcs = 'FL39B2'
     c                             and fl39b1 = '24' and urNyRev(xc) = '0169'
     c     ovcsKey       setgt     hmfovcs
     c     ovcsKey2      readpe    hmfovcs
     c                   if        not %eof(hmpovcs)
     c                             and %lookup('84' : vcc) <> 0
     c                   eval      datafield = %char( vca( %lookup('84' : vcc)))
     c                   endif

     c                   when      hldhcs = 'FL39C2'
     c                             and fl39c1 = '24' and urNyRev(xc) = '0169'
     c     ovcsKey       setgt     hmfovcs
     c     ovcsKey2      readpe    hmfovcs
     c                   if        not %eof(hmpovcs)
     c                             and %lookup('84' : vcc) <> 0
     c                   eval      datafield = %char( vca( %lookup('84' : vcc)))
     c                   endif

     c                   when      hldhcs = 'FL39D2'
     c                             and fl39d1 = '24' and urNyRev(xc) = '0169'
     c     ovcsKey       setgt     hmfovcs
     c     ovcsKey2      readpe    hmfovcs
     c                   if        not %eof(hmpovcs)
     c                             and %lookup('84' : vcc) <> 0
     c                   eval      datafield = %char( vca( %lookup('84' : vcc)))
     c                   endif

     c                   when      hldhcs = 'FL40A2'
     c                             and fl40a1 = '24' and urNyRev(xc) = '0169'
     c     ovcsKey       setgt     hmfovcs
     c     ovcsKey2      readpe    hmfovcs
     c                   if        not %eof(hmpovcs)
     c                             and %lookup('84' : vcc) <> 0
     c                   eval      datafield = %char( vca( %lookup('84' : vcc)))
     c                   endif

     c                   when      hldhcs = 'FL40B2'
     c                             and fl40b1 = '24' and urNyRev(xc) = '0169'
     c     ovcsKey       setgt     hmfovcs
     c     ovcsKey2      readpe    hmfovcs
     c                   if        not %eof(hmpovcs)
     c                             and %lookup('84' : vcc) <> 0
     c                   eval      datafield = %char( vca( %lookup('84' : vcc)))
     c                   endif

     c                   when      hldhcs = 'FL40C2'
     c                             and fl40c1 = '24' and urNyRev(xc) = '0169'
     c     ovcsKey       setgt     hmfovcs
     c     ovcsKey2      readpe    hmfovcs
     c                   if        not %eof(hmpovcs)
     c                             and %lookup('84' : vcc) <> 0
     c                   eval      datafield = %char( vca( %lookup('84' : vcc)))
     c                   endif

     c                   when      hldhcs = 'FL40D2'
     c                             and fl40d1 = '24' and urNyRev(xc) = '0169'
     c     ovcsKey       setgt     hmfovcs
     c     ovcsKey2      readpe    hmfovcs
     c                   if        not %eof(hmpovcs)
     c                             and %lookup('84' : vcc) <> 0
     c                   eval      datafield = %char( vca( %lookup('84' : vcc)))
     c                   endif

     c                   when      hldhcs = 'FL41A2'
     c                             and fl41a1 = '24' and urNyRev(xc) = '0169'
     c     ovcsKey       setgt     hmfovcs
     c     ovcsKey2      readpe    hmfovcs
     c                   if        not %eof(hmpovcs)
     c                             and %lookup('84' : vcc) <> 0
     c                   eval      datafield = %char( vca( %lookup('84' : vcc)))
     c                   endif

     c                   when      hldhcs = 'FL41B2'
     c                             and fl41b1 = '24' and urNyRev(xc) = '0169'
     c     ovcsKey       setgt     hmfovcs
     c     ovcsKey2      readpe    hmfovcs
     c                   if        not %eof(hmpovcs)
     c                             and %lookup('84' : vcc) <> 0
     c                   eval      datafield = %char( vca( %lookup('84' : vcc)))
     c                   endif

     c                   when      hldhcs = 'FL41C2'
     c                             and fl41c1 = '24' and urNyRev(xc) = '0169'
     c     ovcsKey       setgt     hmfovcs
     c     ovcsKey2      readpe    hmfovcs
     c                   if        not %eof(hmpovcs)
     c                             and %lookup('84' : vcc) <> 0
     c                   eval      datafield = %char( vca( %lookup('84' : vcc)))
     c                   endif

     c                   when      hldhcs = 'FL41D2'
     c                             and fl41d1 = '24' and urNyRev(xc) = '0169'
     c     ovcsKey       setgt     hmfovcs
     c     ovcsKey2      readpe    hmfovcs
     c                   if        not %eof(hmpovcs)
     c                             and %lookup('84' : vcc) <> 0
     c                   eval      datafield = %char( vca( %lookup('84' : vcc)))
     c                   endif

     c                   endsl
     c                   endif
2306 c                   if        bbform = 'CM' and
2306 c                             ((hldhcs = 'FL39A1' and fl39a1 = '23') or
2306 c                              (hldhcs = 'FL39B1' and fl39b1 = '23') or
2306 c                              (hldhcs = 'FL39C1' and fl39c1 = '23') or
2306 c                              (hldhcs = 'FL39D1' and fl39d1 = '23') or
2306 c                              (hldhcs = 'FL40A1' and fl40a1 = '23') or
2306 c                              (hldhcs = 'FL40B1' and fl40b1 = '23') or
2306 c                              (hldhcs = 'FL40C1' and fl40c1 = '23') or
2306 c                              (hldhcs = 'FL40D1' and fl40d1 = '23') or
2306 c                              (hldhcs = 'FL41A1' and fl41a1 = '23') or
2306 c                              (hldhcs = 'FL41B1' and fl41b1 = '23') or
2306 c                              (hldhcs = 'FL41C1' and fl41c1 = '23') or
2306 c                              (hldhcs = 'FL41D1' and fl41d1 = '23'))
2306 c                   eval      datafield = 'FC'
2306 c                   endif
     c                   if        hldhcs = 'PYRTYP'
     c                             and ifpatt = 'INS_A'
     c                   movel     datafield     instyp
     c                   if        instyp = 'P'
     c                   z-add     svcst3        flcst3
     c                   endif
     c                   endif

     c                   endsr
     o**************************************************************************
     c     chkalt        begsr
      **
     c                   eval      altnme = lvlnam

     c                   eval      altad1 = lvladr
     c                   eval      altcty = lvlcty
     c                   eval      altsta = lvlsta
     c                   eval      altzp1 = lvlzp1
     c                   eval      altzp2 = lvlzp2
      **
     c                   eval      prvid = 'ALTNM'
     c                   exsr      srprov
     c                   if        prvvar <> *blanks
     c                   eval      altnme = prvvar
     c                   endif

     c                   eval      prvid = 'ALTAD'
     c                   exsr      srprov
     c                   if        prvvar <> *blanks
     c                   eval      rtnaddress = prvvar
     c                   eval      altad1 = %trim(rtnadr) + ' ' + %trim(rtnad2)
     c***                eval      altad1 = rtnadr
     c***                eval      altad2 = rtnad2
     c                   eval      altcty = rtncty
     c                   eval      altsta = rtnst
     c                   eval      altzp1 = rtnzp1
     c                   eval      altzp2 = rtnzp2
     c                   endif
      **
     c                   eval      prvid = 'ALTN2'
     c                   exsr      srprov
     c                   if        prvvar <> *blanks
     c                   eval      altnm2 = prvvar
     c                   endif
      **
     c                   endsr
      **************************************************************************
     c     srsubd        begsr
      **
     c                   call      'XFXSUBD'
     c                   parm                    ydate1
     c                   parm                    ydate2
     c                   parm      0             ydays
      **
     c                   endsr
      **************************************************************************
       //put rev 169 at end or beginning for their own 2300 loop
     c     reseqUbDet    begsr

     c                   eval      rev169 = 0
     c                   eval      rev169Count = 0
     c                   eval      maxseq = 0

     c     billtrack20   setll     e5fubdet
     c                   dou       %eof(e5pubdet)
     c     billtrack20   reade(n)  e5fubdet
     c                   if        %eof(e5pubdet)
     c                   leave
     c                   endif
     c                   eval      maxseq = ubdcnt
     c                   if        ubdtrc = '0169'
     c                   eval      rev169Count += 1
     c                   eval      rev169(rev169Count) = ubdcnt
     c                   endif
     c                   enddo

     c                   if        rev169(1) = 1 or rev169Count = 0
     c                   leavesr
     c                   elseif    rev169(rev169Count) = maxseq
     c                   leavesr
     c                   endif

     c     1             do        rev169Count   tempIndex
     c     reseqKey      chain     e5fubdet
     c                   if        %found
     c                   eval      ubdcnt = maxseq + tempIndex
     c                   update    e5fubdet
     c                   endif
     c                   enddo

     c                   eval      tempIndex = rev169(1)
     c     reseqKey      setll     e5fubdet
     c                   dou       %eof(e5pubdet)
     c     billtrack20   reade     e5fubdet
     c                   if        %eof(e5pubdet)
     c                   leave
     c                   endif
     c                   eval      ubdcnt -= rev169Count
     c                   update    e5fubdet
     c                   enddo

     c                   endsr
      **************************************************************************
     c     srlicense     begsr
      **
     c                   eval      rqdoctor# = doc#
     c                   call      'XFXLICE'
     c                   parm      bbplv6        rqlevel6
     c                   parm      bbaccn        rqaccount
     c                   parm                    rqdoctor#
     c                   parm      'SNPI'        rqlictype
     c                   parm      ' '           rqstate
     c                   parm      ' '           rqstatechk
     c                   parm      ' '           rqcat2
     c                   parm                    rtnlicense
     c                   parm                    rtntherapist
     c                   parm                    rtncat2
     c                   parm                    rtnexpire
      **
     c                   endsr
      **************************************************************************
     c     getNPIName    begsr
      **
     c                   eval      npiName = *blanks
     c                   if        snpi <> *blanks
     c/exec sql
     c+ SELECT NPNAME
     c+ INTO :npiName
     c+ FROM TABLE(NPI_HMPNPI(:snpi,DEFAULT,DEFAULT,
     c+                        DEFAULT,DEFAULT,DEFAULT))
     c+ FETCH FIRST ROW ONLY
     c/end-exec
     c                   endif
      **
     c                   endsr
      **********************************************************************************************
     oprinter   h    of                     1 03
     o                                           10 'RUN DATE: '
     o                       rundte              20 '  /  /    '
     o                       lv1nam              86
     o                                          127 'PAGE '
     o                       page          z    132
     o*------------------------------------------------------------------------*
     o          h    of                     2
     o                                           10 'RUN TIME: '
     o                       runtme              20 '0 :  :  '
     o                       rpthdr              86
     o                       pgmnam             132
     o*------------------------------------------------------------------------*
     o          h    of                     0
     o                                           24 '   _________ ____       '
     o                                           48 '                 _______'
     o                                           72 '___ __________ ______ __'
     o                                           96 '___ _____________       '
     o                                          120 ' _______________   _____'
     o                                          132 '__________  '
     o*------------------------------------------------------------------------*
     o          h    of                     2
     o                                           24 '   ACCOUNT # NAME       '
     o                                           48 '                 FROM DA'
     o                                           72 'TE  TO DATE    PAYOR# PL'
     o                                           96 'AN# POLICY NUMBER       '
     o                                          120 ' INS DESCRIPTION   TRACK'
     o                                          132 'ING NUMBER  '
     o*------------------------------------------------------------------------*
     o          ef           detail         1
     o                       bbaccn        4     12
     o                       mmname              39
     o                       frdate              51 '  /  /    '
     o                       todate              62 '  /  /    '
     o                       bbpayr        4     69
     o                       bbplan        4     75
     o                       bbplcy              96
     o                       insdsc             113
     o                       wsaccn             132
     o*------------------------------------------------------------------------*
     o          ef           prvtot      1  1
     o                                           15 'TOTAL CHARGES: '
     o                       totchgx       l     40
     o*------------------------------------------------------------------------*
     o          ef           lastr       1  1
     o                                           18 'TOTAL CLAIMS SENT:'
     o                       stotal        l     26
     o**************************************************************************
