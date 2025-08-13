      /copy copysrc,hxxcntrl

     *****************************************************************
     **                                                             **
     **                     1500 bill program                       **
     **                                                             **
     **     ***** note ***** note ***** note ***** note *****     **
     **     changes to this program may also need to be made in     **
     **              HBR1500  - printing bill from history          **
     **                                                             **
     **         Copyright 2014 - Health Care Software, Inc.         **
     **                                                             **
     **   do not remove bill form exceptions from this program!!  **
     **   they are user maintainable, if your client does not     **
     **   need them,they shouldnt set them up! if you have new    **
     **   ones add them as the next exception number. do not use  **
     **   existing exception number for a new one!!!!!            **
     **                                                             **
     *****************************************************************
     **                           abstract                          **
     **                                                             **
     **  the purpose of this program is to generate the 1500 bills. **
     **                                                             **
     **-------------------------------------------------------------**
     **                                                             **
     **  indicators used within the program:                        **
     **                                                             **
     **  01 - normal 1500 bill                                      **
     **  02 - florida 1500 bill (niu)                               **
     **  03 - texas workmans comp 1500 bill (niu)                   **
     **                                                             **
     **  16 - BILL EXCEPTION 510 - PRINT BY SERVICE DATE / ICD9 CODE**
     **                                                             **
     **  21 - patient male                                          **
     **  22 - patient female                                        **
     **  23 - patient zip code print                                **
     **  24 - secondary insurance carrier                           **
     **  25 - workmans comp                                         **
     **  26 - auto accident                                         **
     **  27 - other accident                                        **
     **                                                             **
     **  31 - patient single                                        **
     **  32 - patient married                                       **
     **  33 - patient other                                         **
     **  34 - medicare                                              **
     **  35 - medicaid                                              **
     **  36 - other                                                 **
     **                                                             **
     **  38 - print box 9D with provider variable 159D              **
     **  39 - attachment (documentation flag = 'y')                 **
     **                                                             **
     **  41 - self                                                  **
     **  42 - spouse                                                **
     **  43 - parent                                                **
     **  44 - other                                                 **
     **  45 - employed                                              **
     **  46 - enrolled in school                                    **
     **                                                             **
     **  51 - print box 10D using BOX10D field.                     **
     **  53 - letter of med with bill                               **
     **  54 - progress notes with bill                              **
     **  55 - rx required to bill                                   **
     **  56 - physician referral required to bill                   **
     **  57 - level 1 & level 6 names to print under form           **
     **                                                             **
     **  61 - demo/guarantor chain                                  **
     **                                                             **
     **  70 - charge chain                                          **
     **  76 - chain indicator                                       **
     **  77 - chain indicator                                       **
     **  78 - chain indicator                                       **
     **  79 - chain indicator; loop indicator for bill exceptions   **
     **                                                             **
     **  bx24af - box 24a printing exception flag                   **
     **  bx24cf - box 24c printing exception flag                   **
     **  bx29f  - box 29 printing exception flag                    **
     **                                                             **
     *****************************************************************
     **                    Modification Summary                     **
     **                                                             **
     ** Change Date    Programmer/Comments             Install Date **
     ** 03/03/2014-    bgates - Project 869             04/14/2014  **
     ** 04/11/2014     New version (02-12) based on HBB1500N        **
     **                                                             **
     ** 04/15/2014    Will Pedicone - Ticket 418852                 **
     **              -Created bill exception 649 to print ALTGP     **
     **               provider variable in box 33B only for specific**
     **               procedure codes. Will print blank otherwise.  **
     **                                                             **
     **  05/05/2014    Will Pedicone - Ticket 422729                **
     **               -Changed the assigned physician lookup        **
     **                to use the account discharge date if it less **
     **                than the bill 'to date' (and not zero).      **
     **                                                             **
     **  09/05/2914    Will Pedicone - TIcket 448108                **
     **               -Set the CPT code before writing electronic   **
     **                detail record.                               **
     **                                                             **
     ** 09/24/2014    Will Pedicone - Ticket 418852                 **
     **              -Created bill exception 658 to print referring **
     **               physician 'STATE' license code.               **
     **                                                             **
     ** 09/30/2014    Will Pedicone - Ticket 437690                 **
     **              -Created bill exception 660 to print PRVDR     **
     **               provider variable in box 33B.                 **
     **                                                             **
     ** 01/13/2015    Will Pedicone - Ticket 481139                 **
     **              -Created bill exception 677 to print ICN       **
     **               number in box 22B                             **
     **                                                             **
     ** 03/12/2015    Will Pedicone - Ticket 494059                 **
     **              -Created bill exception 690 to print           **
     **               rendering provider taxonomy code in box 24J   **
     **                                                             **
      * 02/09/2015   bgates - Ticket 452166                         **
      *             -Added parameters to HBXSTEPD and XFXPRTCD calls**
      * 02/17/2015  -Use print modifiers if returned and not        **
      *              on charge                                      **
      * 03/17/2015  -Allow print modifiers to override if modifiers **
      *              are coming from procedure                      **
      * 05/01/2015  -Suppress modifier edits if overriding from     **
      *              contract                                       **
      *              **SCAN 150511**                                **
      *                                                             **
      * 06/17/2015   Will Pedicone - Ticket 523724                  **
      *             -Extended field size of address printed by      **
      *              bill exception 185 to 26 characters from 20.   **
      *              added 2nd line of address.                     **
      *                                                             **
      * 06/26/2015   Will Pedicone - Ticket 497951                  **
      *             -Corrected bill exception 364 to work as it is  **
      *              described.                                     **
      *                                                             **
      * 05/21/2015   bgates - ICD10                                 **
      *             -Transposed version and body part in XFXDIAG    **
      *              call to match HCS                              **
      * 06/02/2015  -Removed unused HMPDIAG reference               **
      *                                                             **
      **  05/12/2014    bgates - Project 688                        **
      **               -Added Claim type report information         **
      **  05/07/2015    bgates - Ticket 403644                      **
      **               -Repulled changes                            **
      **                                                            **
      **  08/31/2015    Will Pedicone - Ticket 537613               **
      **               -corrected order in which ICD version is     **
      **                checked                                     **
      **                                                            **
      **  10/13/2015    Will Pedicone - Ticket 541102               **
      **               -corrected modifier placement into combined  **
      **                field                                       **
      **                                                            **
      **  12/01/2015   Will Pedicone - Ticket 549234                **
      **               -Changed bill exception 17 to make field     **
      **                left-adjusted instead of right-adjusted.    **
      **                Extended upper 24J field size from 10 to 14.**
      **                                                            **
      **  04/11/2016   Will Pedicone - Ticket 566798                **
      **               -Print up to 12 diagnosis codes on paper form**
      **                                                            **
      ** 04/25/2016   Will Pedicone - Ticket 568176                 **
      **             -Created bill exception 814 to print PRVDR     **
      **              variable in 32B without qualifier, and bill   **
      **              exception 815 to print PRVDR variable in 33B  **
      **              with qualifier.                               **
      **                                                            **
      ** 05/03/2016   bgates - Ticket 566538             05/11/2016 **
      **             -Added bill exception 817 to print a monthly   **
      **              sequenced authorization number                **
      ** 05/05/2016  -Added inclusion logic for exception 817. If   **
      **              not included, the unsequenced authorization   **
      **              number will be used                           **
      **                                                            **
      ** 12/30/2016   Charles Nagy - Ticket 589809 / HDEV-13882     **
      **             -Added bill exception 200 to print referring   **
      **              physician name in box 31                      **
      **                                                            **
      **  1/09/2017   Charles Nagy - Ticket 589809 / HDEV-13977     **
      **             -Added bill exception 835 to print aide        **
      **              name in box 31                                **
      **                                                            **
      **  1/12/2017   Charles Nagy - Ticket 588479 / HDEV-13629     **
      **             -Created bill exception 833 to print the word  **
      **              ATYPICAL in box 33A                           **
      **                                                            **
      **  2/10/2017   Charles Nagy - Ticket 588479 / HDEV-14681     **
      **             -Modified bill exception 833 to only print the **
      **              word ATYPICAL in box 33A if box 33A is blank  **
      **                                                            **
      **  2/14/2017   Charles Nagy - Ticket 587439 - HDEV-13092     **
      **             -Exclude Private Pay writing out Boxes 9a-9d   **
      **             -Exclude Private Pay writing out to 837 fields **
      **                                                            **
      **  3/30/2017   Charles Nagy - Ticket 596288 / HDEV-15656     **
      **             -Added code to populate the proc description   **
      **              field in E5PCHR15                             **
      **                                                            **
      **  4/18/2017   Charles Nagy - Ticket 594110 / HDEV-14798     **
      **             -Created bill exception 837 to print the VASID **
      **              provider variable in box 23, and to print the **
      **              payor/plan address info in boxes 5 and 7      **
      **             -Ignore bill exception 196 if exception 837    **
      **              is active                                     **
      **                                                            **
      ** 10/11/2017   Charles Nagy - Ticket 596299 / HDEV-18979     **
      **             -Created bill exception 861 to check for       **
      **              SNPI license code. If found, get supervisor   **
      **              name from NPI file for box 31, and use SNPI   **
      **              value for NPI in box 24J.                     **
      **                                                            **
      ** 12/29/2017   Charles Nagy - Ticket 619254 / HDEV-20618     **
      **             -Clear PRTHNM (box 31) for each claim. Was     **
      **              holding onto value from prior claim.          **
      **                                                            **
      **  6/20/2018   Charles Nagy - Ticket 627189 / HDEV-24369     **
      **             -Added code to populate the NPI in box 24J     **
      **              in E5PCHR15                                   **
      **                                                            **
      **  1/18/2019   Charles Nagy - Ticket 1855797 / HDEV-28107    **
      **             -Created bill exception 885 to print the       **
      **              TAXON provider variable in box 33B without    **
      **              a qualifier                                   **
      **                                                            **
      **  1/30/2019   Charles Nagy - Ticket 1996774 / HDEV-28422    **
      **             -Populate physname for bill exception 835      **
      **              so it will in turn populate b531A             **
      **                                                            **
      **  2/12/2019   Charles Nagy - Ticket 1887435 / HDEV-28686    **
      **             -Created bill exception 888 to print the       **
      **              insurance as primary with no other insurances **
      **              on the 837.                                   **
      **                                                            **
      **  2/27/2019   Charles Nagy - Ticket 1887016 / HDEV-28670    **
      **             -Created bill exception 887 to not print the   **
      **              Referring Physician information in Box 17     **
      **                                                            **
      **  3/05/2019   Charles Nagy - Ticket 632894 / HDEV-25935     **
      **             -Fixed old bug that was setting Box 11D to N   **
      **              on multi page claims when there is another    **
      **              insurance.                                    **
      **                                                            **
      **  3/15/2019   Charles Nagy - Ticket 1992256 / HDEV-29878    **
      **             -Created bill exception 891 to function the    **
      **              same as b.e. 17, minus the CAT II requirement **
      **                                                            **
      **  4/01/2019   Charles Nagy - Ticket 1920403 / HDEV-29315    **
      **             -Set B505E and B507E to blanks if telephone    **
      **              number is zero (837P)                         **
      **                                                            **
      **  4/02/2019   Charles Nagy - Ticket 1920453 / HDEV-28707    **
      **             -Added code to populate the qualifier in box   **
      **              24I, the PIN in box 24J, and the NPI in box   **
      **              24J bottom in E5PCHR15 for use in the 837     **
      **             -Modified the field used in the change for     **
      **              Ticket 627189 / HDEV-24369 above              **
      **                                                            **
      **  6/21/2019   Charles Nagy - Ticket 2087991 / HDEV-31931    **
      **             -Changed INnSS# fields to be populated with    **
      **              Insurance Subscriber SSN                      **
      **                                                            **
      **  1/14/2020   Charles Nagy - Ticket 2366668 / HDEV-35279    **
      **             -Added bill exception 552 to print Attending   **
      **              Physician NPI in box 24J                      **
      **             -Added bill exception 908 to print Referring   **
      **              Physician NPI in box 24J                      **
      **                                                            **
      **  1/17/2020   Charles Nagy - Ticket 2371913 / HDEV-35315    **
      **             -Added bill exception 909 to print the VASID   **
      **              provider variable + the authorization number  **
      **              in box 23 - with procedure exclusion          **
      **                                                            **
      **  5/20/2020   Charles Nagy - Ticket 2511929 / HDEV-37479    **
      **             -Created bill exception 916 to print the NPI   **
      **              provider variable in Box 32B w/o qualifier    **
      **             -Created bill exception 917 to print the NPI   **
      **              provider variable in Box 33B w/o qualifier    **
      **              -Also sets exc401 flag to eliminate the need  **
      **               to turn on b.e. 401 to print Box 33B         **
      **                                                            **
      **  9/03/2020   Charles Nagy - INC001543635 / HDEV-39096      **
      **             -Modified bill exception 909 to check the      **
      **              PAYID provider variable against table B909    **
      **              to determine whether VASID provider variable  **
      **              should be prepended to the IVSVC auth number  **
      **                                                            **
      ** 10/06/2020   Charles Nagy - INC001567947 / HDEV-39642      **
      **             -Created bill exception 922 to print the FALOC **
      **              provider variable in Box 32B w/o qualifier    **
      **                                                            **
      **  4/15/2021   Charles Nagy - INC001651970 / HDEV-21875      **
      **             -Deprecated bill exception 677 and modified to **
      **              always populate Box22 with ICN/DCN for type   **
      **              of bill 7 or 8                                **
      **                                                            **
      **  4/16/2021   Charles Nagy - INC001624746 / HDEV-41465      **
      **             -Created new bill exception 931, based on      **
      **              bill exception 591, to print the procedure    **
      **              description in box 24 for a list of included  **
      **              procedures                                    **
      **                                                            **
      **  7/12/2021   Charles Nagy - INC001710142 / HDEV-42027      **
      **             -Modified bill exception 909 to put a '-'      **
      **              between the VASID provider variable and the   **
      **              authorization number.                         **
      **                                                            **
      **  7/20/2021   Charles Nagy - INC001613057 / HDEV-42034      **
      **             -Created new bill exception 935, based on      **
      **              bill exception 189, to pull the PAYTO         **
      **              provider variable information                 **
      **                                                            **
      **  9/22/2021   Charles Nagy - CTASK0096604 / HDEV-42158      **
      **             -Created bill exception 940 to print the       **
      **              rounded qty in hours based on the time of     **
      **              procedure in box 24G for a list of included   **
      **              procedures                                    **
      **                                                            **
      ** 10/20/2021   Charles Nagy - CTASK0097490                   **
      **              Modified bill exception 935 to:               **
      **              -Also populate Box33 with the PAYTO provider  **
      **               variable address information                 **
      **              -Populate the BN837 provider variable address **
      **               information in B533Ax fields for Loop 2010AA **
      **              (Note: Not happy with this overall solution)  **
      **                                                            **
      **  2/10/2022   Charles Nagy - WebPT Interface                **
      **             -Created bill exception 941 to pull ICD codes  **
      **              from HBPCHGDX to populate Box 21              **
      **                                                            **
      **  8/22/2022   Charles Nagy - CTASK0107029                   **
      **             -Created bill exception 946 to pull WebPT      **
      **              Authorization Number to populate Box 23       **
      **                                                            **
      **  9/12/2022   Charles Nagy - CTASK0108313                   **
      **             -Default Insurance Subscriber gender code      **
      **             -Created bill exception 947 to print name of   **
      **              therapist on charge record in Box 31          **
      **                                                            **
      **  9/16/2022   Charles Nagy - CTASK0108435                   **
      **             -Created bill exception 948 to print the       **
      **              2ndry charge code description above the       **
      **              dates of service in box 24.                   **
      **                                                            **
      ** 10/21/2022   Charles Nagy - CTASK0109425                   **
      **              Created new bill exceptions:                  **
      **             -950 to print PRVDR provider variable in       **
      **              Box 24J PIN with G2 qualifier in Box 24I      **
      **             -951 to print blanks in Box 24J NPI            **
      **             -952 to print PRVDR provider variable with G2  **
      **              qualifier in Box 33B                          **
      **                                                            **
      **  2/13/2023   Charles Nagy - CTASK0113027                   **
      **             -Expanded authorization # on charge from       **
      **              18 to 20 characters                           **
      **                                                            **
      **  6/07/2023   Charles Nagy - CTASK0115568                   **
      **             -Created bill exception 956 to print the       **
      **              adjustment/void description in box 19         **
      **             -Removed BBBFUT data structure                 **
      **                                                            **
      ** 12/18/2023   Charles Nagy - CTASK0119939                   **
      **             -Populate physname for Box 31 default          **
      **                                                            **
      **  3/14/2024   Charles Nagy - CTASK0121475                   **
      **             -Created bill exception 963 to print the       **
      **              Individual Providing Service name and         **
      **              relation in box 19                            **
      **                                                            **
      **  5/21/2024   Charles Nagy - CTASK0124111                   **
      **             -Bug fix related to bill exception 941 to skip **
      **              Box 24E pointer if dx code not in Box 21      **
      **                                                            **
      **  5/29/2024   Charles Nagy - CTASK0124298                   **
      **             -Modified bill exception 963 to use . as the   **
      **              delimiter instead of : for NAME and REL       **
      **                                                            **
      **  2/21/2025   Charles Nagy - CTASK0132657                   **
      **             -Modified bill exception 452 to reflect the    **
      **              qty in hours as 6,2                           **
      **                                                            **
      **  3/24/2025   Charles Nagy - CTASK0133838                   **
      **             -Added bill exception 974 to print additional  **
      **              claim information in Box 19                   **
      **             -Pulled from WSC and adapted for BHS           **
      **                                                            **
     *****************************************************************
     FHBLBILFA  UP   E           K DISK
     FHBPCHGBL  IF   E           K DISK
     FHBPCHRG   IF   E           K DISK
     fhbpchr15  if   e           k disk    rename(hbfchr15 : hbfchr15s)
     f                                     prefix(z : 1)
     FHBLCHG15  IF   E           K DISK
     FHBLCH15D  IF   E           K DISK    RENAME(HBFCHR15:HBFCH15D)
     Fhblch15e  if   e           k disk    rename(hbfchr15:hbfch15e)
     F                                     prefix(zz : 2)
     fe5pchr15  uf a e           k disk    prefix(zz : 2)
     fe5p1500   uf a e           k disk
     FHBPBLDTS  IF   E           K DISK
     FHAPDEMO   IF   E           K DISK
     FHAPGUAR   IF   E           K DISK
     FHMPMAMS   IF   E           K DISK
     F****HMPDIAG   IF   E           K DISK
     FHXPINSD   IF   E           K DISK
     fhxpprocc  if   e           k disk
     FHXPLVL1   IF   E           K DISK
     FHXPLVL5   IF   E           K DISK
     FHXPLVL6   IF   E           K DISK
     FHBPMAST   IF   E           K DISK
     FHXPBNFIT  IF   E           K DISK
     FHMLPLNP3  IF   E           K DISK
     FHMPLICE   IF   E           K DISK
     FHMLLICE   IF   E           K DISK    RENAME(HMFLICE:HMFLICE2)
     FHBPBFEX   IF   E           K DISK
     FHBLBEXCT  IF   E           K DISK
     FHBPBFXO   IF   E           K DISK
     FHAPDEMOI  IF   E           K DISK
     FHALICOVH  IF   E           K DISK
     FHALIVSVD  IF   E           K DISK
     FHMLPDIAT  IF   E           K DISK
     FHXPCDFLE  IF   E           K DISK
     FHAPFAMLY  IF   E           K DISK
     fHALFAMLC  if   e           k disk    rename(HAFFAMLY:HAFFAMLC)
     fhbpcdgn   if   e           k disk
     FHXPPRRC   IF   E           K DISK    prefix(xp:2)
     FHXLPRRCD  IF   E           K DISK    prefix(xp:2)
     F                                     RENAME(HXFPRRC:HXFPRRCD)
     FHBPCPT4   IF   E           K DISK
     FHBPNCPT4  IF   E           K DISK    RENAME(HBFCPT4:HBFNCPT4)
     FHBPCPMD   IF   E           K DISK
     FHBPNCPMD  IF   E           K DISK    RENAME(HBFCPMD:HBFNCPMD)
     FHXLTITLB  IF   E           K DISK
     FHBPDIEM   IF   E           K DISK
     FHXPPROCM  IF   E           K DISK
     FHMPNPI    IF   E           K DISK
     FHBPCHGDX  IF   E           K DISK
     fhbpcdsc   if   e           k disk
     FHBPGTOB   O    E           K DISK
     FHBPBILLH  O    E             DISK    RENAME(HBFBILL:HBFBILLH)
     FHBSBILL   O    E             DISK    RENAME(HBFBILL:HBFBILLS)
     FHBR1500CN O    E             PRINTER USROPN infds(filinfN)                No Overlay
     F                                     RENAME(BILL15:BILLP1)
     FHBR1500C  O    E             PRINTER USROPN infds(filinfO)                Overlay
     F                                     RENAME(BILL15:BILLP2)
     FPRINTER   O    F  198        PRINTER OFLIND(*INOA)
     F                                     USROPN
     FPRINTR2   O    F  198        PRINTER OFLIND(*INOE)
     F                                     USROPN
     *****************************************************************
     **                       array definition                      **
     *****************************************************************
     D DIAD            S              1    DIM(7)                               =dx codes
     D DIAN            S              1    DIM(7)                               =dx codes
     D DXC             S              7    DIM(12)                              =dx codes
     D PBF             S              8  0 DIM(6)                               =svc date-from
     D PBT             S              8  0 DIM(6)                               =svc date-to
     D TOT             S              9  2 DIM(3)                               =total due    #
     D ICR             S              5  3 DIM(%elem(row))                      =increment
     D ICRN            S              5  3 DIM(%elem(row))                      =increment
     D ICRO            S              5  3 DIM(%elem(row))                      =increment
     D qty5            s              5  0 dim(6)
      *****************************************************************
     d filinfN         ds
     d  prtfilN               83     92

     d filinfO         ds
     d  prtfilO               83     92

     d                 ds
     d  ediag                  1     84    dim(12)
     d  bdia01                 1      7
     d  bdia02                 8     14
     d  bdia03                15     21
     d  bdia04                22     28
     d  bdia05                29     35
     d  bdia06                36     42
     d  bdia07                43     49
     d  bdia08                50     56
     d  bdia09                57     63
     d  bdia10                64     70
     d  bdia11                71     77
     d  bdia12                78     84
     d                 ds
     d  sdiag                  1     84    dim(12)
     d  sdia01                 1      7
     d  sdia02                 8     14
     d  sdia03                15     21
     d  sdia04                22     28
     d  sdia05                29     35
     d  sdia06                36     42
     d  sdia07                43     49
     d  sdia08                50     56
     d  sdia09                57     63
     d  sdia10                64     70
     d  sdia11                71     77
     d  sdia12                78     84
      *---------------------------------------------------------------*
     D**** BBBFUT          DS
     D****  BBTAXN                 1     10
     D****  BBADUN                11     16
     D****  BBINVN                17     26
     D****  BBINVD                27     34
     D****  BBADJR                35     36
     D****  BBVODR                37     38
     D****  BBP15A                39     39
      /copy copysrc,hbxbbbfut
     D                 DS
     D  PRBIRT                 1      8  0
     D  PRBDMM                 1      2  0
     D  PRBDDD                 3      4  0
     D  PRBDYY                 5      8  0
     D  PRBDYR                 7      8  0
     D                 DS
     D  RPTDTE                 1      8  0
     D  RPTDMM                 1      2  0
     D  RPTDDD                 3      4  0
     D  RPTDYY                 5      8  0
     D  RPTDYR                 7      8  0
     D                 DS
     D  SBBDTE                 1      8  0
     D  PRS1YY                 1      4  0
     D  PRS1YR                 3      4  0
     D  PRS1MM                 5      6  0
     D  PRS1DD                 7      8  0
     D                 DS
     D  INJDTE                 1      8  0
     D  INDTYR                 3      4  0
     D  INDTYY                 1      4  0
     D  INDTMM                 5      6  0
     D  INDTDD                 7      8  0
     D                 DS
     D  SB2BDT                 1      8  0
     D  PRS2YR                 3      4  0
     D  PRS2YY                 1      4  0
     D  PRS2MM                 5      6  0
     D  PRS2DD                 7      8  0
     D                 DS
     D  DXDESC                 1     18
     D  DXCODE                 1      7
     D  DXNAME                 9     18
     D                 DS
     D  WKDAT6                 1      6  0
     D  MMDD6                  1      4  0
     D  MM6                    1      2  0
     D  DD6                    3      4  0
     D  YY6                    5      6  0
     D                 DS
     D  WRKYMD                 1      8  0
     D  WRKYY                  1      4  0
     D  WRKMM                  5      6  0
     D  WRKDD                  7      8  0
     D                 DS
     D  WRKMDY                 1      8  0
     D  WRKM                   1      2  0
     D  WRKD                   3      4  0
     D  WRKY                   5      8  0
     D                 DS
     D  TLDS                   1     10
     D  TLP1                   1      3
     D  TLP2                   4      6
     D  TLP3                   7     10
     D                 DS
     D  B21                    1     80
     D                                     DIM(4)                               =dx descr
     D  PB211A                 1     20
     D  PB212A                21     40
     D  PB213A                41     60
     D  PB214A                61     80
     D BINCHK          DS
     D  BINARY                 1      4B 0
     D  VALUEH                 3      3
     D  VALUEL                 4      4
     D RQFCL           DS
     D  AFC                    1      6
     D                                     DIM(3)                               =active f/c's
     D RQUBC           DS
     D  AUB                    1     18  0
     D                                     DIM(3)                               =active pyrs
     D RQPLC           DS
     D  APL                    1     15  0
     D                                     DIM(3)                               =active plns
     D RQPOL           DS
     D  APO                    1     60
     D                                     DIM(3)                               =active policy#
     D LEVEL           DS           297
     D  LV6ABR               196    198
     D                 DS
     D  RUNDT                  1     14  0
     D  RUNTME                 1      6  0
     D  RUNDTE                 7     14  0
     D                 DS
     D  PRCMOD                 1      7
     D  CPT142                 1      5
     D  MOD142                 6      7
     D                 DS
     D  WRKADR                 1    112
     D  ADDR1                  1     25
     D  ADDR2                 26     50
     D  CITY                  51     65
     D  STATE                 66     67
     D  ZIP1                  68     72
     D  ZIP2                  73     76
     D  NAME                  77    102
     D  PHONE                103    112
     D                 DS
     D  DXD                    1     72
     D                                     DIM(4)                               =dx descr
     D  DXD1                   1     18
     D  DXD2                  19     36
     D  DXD3                  37     54
     D  DXD4                  55     72
     D                 DS
     D  DTF                    1     48
     D                                     DIM(6)                               =svc date-from
     D  DTF01                  1      8
     D  DTF02                  9     16
     D  DTF03                 17     24
     D  DTF04                 25     32
     D  DTF05                 33     40
     D  DTF06                 41     48
     D                 DS
     D  DTT                    1     48
     D                                     DIM(6)                               =svc date-to
     D  DTT01                  1      8
     D  DTT02                  9     16
     D  DTT03                 17     24
     D  DTT04                 25     32
     D  DTT05                 33     40
     D  DTT06                 41     48
     D                 DS
     D  DTF6                   1     36
     D                                     DIM(6)                               =6 gigit svc dt-from
     D  DTF601                 1      6
     D  DTF602                 7     12
     D  DTF603                13     18
     D  DTF604                19     24
     D  DTF605                25     30
     D  DTF606                31     36
     D                 DS
     D  DTT6                   1     36
     D                                     DIM(6)                               =6 digit svc dt-to
     D  DTT601                 1      6
     D  DTT602                 7     12
     D  DTT603                13     18
     D  DTT604                19     24
     D  DTT605                25     30
     D  DTT606                31     36
     D                 DS
     D  DTFM                   1     12
     D                                     DIM(6)                               =date break
     D  DTFM01                 1      2
     D  DTFM02                 3      4
     D  DTFM03                 5      6
     D  DTFM04                 7      8
     D  DTFM05                 9     10
     D  DTFM06                11     12
     D                 DS
     D  DTTM                   1     12
     D                                     DIM(6)                               =out array's
     D  DTTM01                 1      2
     D  DTTM02                 3      4
     D  DTTM03                 5      6
     D  DTTM04                 7      8
     D  DTTM05                 9     10
     D  DTTM06                11     12
     D                 DS
     D  DTFD                   1     12
     D                                     DIM(6)                               =for seperate
     D  DTFD01                 1      2
     D  DTFD02                 3      4
     D  DTFD03                 5      6
     D  DTFD04                 7      8
     D  DTFD05                 9     10
     D  DTFD06                11     12
     D                 DS
     D  DTTD                   1     12
     D                                     DIM(6)                               =boxes vs
     D  DTTD01                 1      2
     D  DTTD02                 3      4
     D  DTTD03                 5      6
     D  DTTD04                 7      8
     D  DTTD05                 9     10
     D  DTTD06                11     12
     D                 DS
     D  DTFY                   1     12
     D                                     DIM(6)                               =pushed
     D  DTFY01                 1      2
     D  DTFY02                 3      4
     D  DTFY03                 5      6
     D  DTFY04                 7      8
     D  DTFY05                 9     10
     D  DTFY06                11     12
     D                 DS
     D  DTTY                   1     12
     D                                     DIM(6)                               =together
     D  DTTY01                 1      2
     D  DTTY02                 3      4
     D  DTTY03                 5      6
     D  DTTY04                 7      8
     D  DTTY05                 9     10
     D  DTTY06                11     12
     D                 DS
     D  POS                    1     12
     D                                     DIM(6)                               =place of svc
     D  POS01                  1      2
     D  POS02                  3      4
     D  POS03                  5      6
     D  POS04                  7      8
     D  POS05                  9     10
     D  POS06                 11     12
     D                 DS
     D  CPT                    1     48
     D                                     DIM(6)                               =cpt4 code
     D  CPT01                  1      8
     D  CPT02                  9     16
     D  CPT03                 17     24
     D  CPT04                 25     32
     D  CPT05                 33     40
     D  CPT06                 41     48
     D                 DS
     D  MOD                          12    DIM(6)                               =cpt modifier
     D  MOD01                  1     12
     d  mod011                 1      3
     d  mod012                 4      6
     d  mod013                 7      9
     d  mod014                10     12
     D  MOD02                 13     24
     d  mod021                13     15
     d  mod022                16     18
     d  mod023                19     21
     d  mod024                22     24
     D  MOD03                 25     36
     d  mod031                25     27
     d  mod032                28     30
     d  mod033                31     33
     d  mod034                34     36
     D  MOD04                 37     48
     d  mod041                37     39
     d  mod042                40     42
     d  mod043                43     45
     d  mod044                46     48
     D  MOD05                 49     60
     d  mod051                49     51
     d  mod052                52     54
     d  mod053                55     57
     d  mod054                58     60
     D  MOD06                 61     72
     d  mod061                61     63
     d  mod062                64     66
     d  mod063                67     69
     d  mod064                70     72
     D                 DS
     D  DIX                           7    DIM(6)                               =dx index
     D  DIX01                  1      7
     D  DIX02                  8     14
     D  DIX03                 15     21
     D  DIX04                 22     28
     D  DIX05                 29     35
     D  DIX06                 36     42
     D                 DS
     D  DDX                           7    DIM(6)                               =detail diags #
     D  DDX01                  1      7
     D  DDX02                  8     14
     D  DDX03                 15     21
     D  DDX04                 22     28
     D  DDX05                 29     35
     D  DDX06                 36     42
     d                 ds
     d dxv                            8    dim(99)
     d  dxv1                   1      8
     d  dxv2                   9     16
     d  dxv3                  17     24
     d  dxv4                  25     32
     d  dxv5                  33     40
     d  dxv6                  41     48
     d  dxv7                  49     56
     d  dxv8                  57     64
     d  dxv9                  65     72
     d  dxv10                 73     80
     d  dxv11                 81     88
     d  dxv12                 89     96
     D                 DS
     D  AMT                    1     54  2
     D                                     DIM(6)                               =amount
     D  AMT01                  1      9  2
     D  AMT02                 10     18  2
     D  AMT03                 19     27  2
     D  AMT04                 28     36  2
     D  AMT05                 37     45  2
     D  AMT06                 46     54  2
     D                 DS
     D  QTY                    1     18  0
     D                                     DIM(6)                               =quantity
     D  QTY01                  1      3  0
     D  QTY02                  4      6  0
     D  QTY03                  7      9  0
     D  QTY04                 10     12  0
     D  QTY05                 13     15  0
     D  QTY06                 16     18  0
     D                 DS
     D  QTY4                   1     24  0
     D                                     DIM(6)                               =4 digit qty
     D  QTY41                  1      4  0
     D  QTY42                  5      8  0
     D  QTY43                  9     12  0
     D  QTY44                 13     16  0
     D  QTY45                 17     20  0
     D  QTY46                 21     24  0
     D                 DS
     D  QTY2                   1     36  2
     D                                     DIM(6)                               =4 digit qty,
     D  QTY21                  1      6  2                                      =really this time
     D  QTY22                  7     12  2
     D  QTY23                 13     18  2
     D  QTY24                 19     24  2
     D  QTY25                 25     30  2
     D  QTY26                 31     36  2
     D                 DS
     D  PIN                    1     84
     D                                     DIM(6)                               =pin number
     D  PIN01                  1     14
     D  PIN02                 15     28
     D  PIN03                 29     42
     D  PIN04                 43     56
     D  PIN05                 57     70
     D  PIN06                 71     84
     d                 ds
     d bx24i                          3    dim(12)
     d  bx24i1                 1      3
     d  bx24i2                 4      6
     d  bx24i3                 7      9
     d  bx24i4                10     12
     d  bx24i5                13     15
     d  bx24i6                16     18
     d                 ds
     d pdsc                    1    180
     d                                     dim(6)
     d  pdsc01                 1     30
     d  pdsc02                31     60
     d  pdsc03                61     90
     d  pdsc04                91    120
     d  pdsc05               121    150
     d  pdsc06               151    180
     d                 ds
     d npi                           10    dim(12)
     d  npi01                  1     10
     d  npi02                 11     20
     d  npi03                 21     30
     d  npi04                 31     40
     d  npi05                 41     50
     d  npi06                 51     60
     D                 DS
     D  NAMEOT                 1     35
     D  NAMF                   1     15
     D  NAML                  16     35
     D                 ds
     D MLTIDIAG                      24
     D  MDIAG1                 1      2  0
     D  MDIAG1C                1      2
     D  MDIAG2                 3      4  0
     D  MDIAG2C                3      4
     D  MDIAG3                 5      6  0
     D  MDIAG3C                5      6
     D  MDIAG4                 7      8  0
     D  MDIAG4C                7      8
     D  MDIAG5                 9     10  0
     D  MDIAG5C                9     10
     D  MDIAG6                11     12  0
     D  MDIAG6C               11     12
     D  MDIAG7                13     14  0
     D  MDIAG7C               13     14
     D  MDIAG8                15     16  0
     D  MDIAG8C               15     16
     D  MDIAG9                17     18  0
     D  MDIAG9C               17     18
     D  MDIAG10               19     20  0
     D  MDIAG10C              19     20
     D  MDIAG11               21     22  0
     D  MDIAG11C              21     22
     D  MDIAG12               23     24  0
     D  MDIAG12C              23     24

     d                 ds
     d wrktim                         4  0
     d  xfphr                         2  0 overlay(wrktim)
     d  xfpmn                         2  0 overlay(wrktim:3)
     d                 ds
     d bgdfil                        39
     d  bgdaut                       20    overlay(bgdfil:10)
     d  bgd508                        1    overlay(bgdfil:39)
     d****  bgd508                        1    overlay(bgdfil:24)
     d                 ds
     d rpupn14                       14
     d  rpupnq                        2    overlay(rpupn14)
     d  rpupnn                       12    overlay(rpupn14:3)

     d zzdfut          ds
     d  zzddtc                        3    overlay(zzdfut : 1)
     d  zzddts                       18    overlay(zzdfut : 4)
     d****  zzd24j                       10    overlay(zzdfut : 22)

     d                 ds
     d  ebillptr               1      8    dim(4)
     d   bgdptr                1      2
     d   bgdpt2                3      4
     d   bgdpt3                5      6
     d   bgdpt4                7      8

     D                SDS
     D  PGMNAM           *PROC

      /copy copysrc,hxxlda
      /copy copysrc,hxxdiag
      /copy copysrc,hxxrow
      /copy copysrc,hxxcol
      /copy copysrc,hxxppd
     *****************************************************************
     d ALPHABET        c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
     d continue        c                   const('CONTINUED ON NEXT PAGE')
     D PRM             C                   CONST('PRIMARY')
     D SEC             C                   CONST('SECONDARY')
     D TER             C                   CONST('TERTIARY')
     d slf             c                   const('SELF PAY')
     d slash1          c                   const('/')
     d slash2          c                   const('/')
     d slash3          c                   const('/')
     d slash4          c                   const('/')
     d slash5          c                   const('/')
     **
     D SIGN            C                   CONST('SIGNATURE ON FILE')
     *----------------------------------------------------------------------
     * Stand Alone Fields - TOP
     *----------------------------------------------------------------------
     D A               S              1  0
     D ACCBA2          S              9  2
     D ACCCHG          S              9  2
     D ACCDUE          S              9  2
     D ACCNT#          S             12  0
     D ACCPAT          S              9  2
     D ACCPGS          S              5  0
     D ACCPG2          S              5  0
     D ACTION          S              1
     d arcmbr          s              9
     d arctyp          s              1
     D ASGFLG          S              1
     d b060cd          s              8  0
     d b060fc          s              2
     d b060l6          s              6  0
     d b060pl          s              5  0
     d b060py          s              6  0
     d b060st          s              2
     d b060tp          s              1
     D BALDUB          S              8  2
     D BALDUE          S              8  2
     D BALTOT          S              9  2
     D BB              S              2  0
     D BGDTQTMP        S              5  0
     D BILCHG          S              9  2
     d BILEXC38        s               n
     d BILEXC152       s               n
     D BILEXC214       S               N
     D bilexc364       s               n
     D BILEXC452       S               N
     D BILEXC463       S               N
     D bilexc582       s               n
     d bilexc837       s               n
     d bilexc861       s               n
     d bilexc885       s               n
     d bilexc935       s               n
     d bilexc940       s               n
     d bilexc941       s               n
     D BILDTE          S              8  0
     D BILDUE          S              9  2
     D BILPAT          S              9  2
     D BILPGS          S              5  0
     D BILRAT          S              9  2
     D BILTRK          S             14
     D bgcdg5          s              7
     D bgcdg6          s              7
     D bgcdg7          s              7
     D bgcdg8          s              7
     D bgcdg9          s              7
     D bgcdg10         s              7
     D bgcdg11         s              7
     D bgcdg12         s              7
     d bodyp           s              3
     D PRRNPI          S             10
     d box19x          S             80
     D BX24AF          S              1
     D BX24CF          S              1
     D BX29F           S              1
     D CAPCDE          S              8
     D CAPHRS          S              4  2
     d capmd1          s              2
     d capmd2          s              2
     d capmd3          s              2
     d capmd4          s              2
     d caprev          s              4  0
     D CAT2            S              3
     D CHGSEQ          S              7  0
     D CHGTCK          S              7  0
     D CHGTOT          S              9  2
     d chkcnt          s              2  0
     d chkprv          s              1
     D CHPRSP          S              9  2
     d chrgdxs         s              7    dim(12)
     D CKLV6           S              6  0
     D CKTYPE          S              1
     D CLITOT          S              9  2
     D CMOYR           S              6  0
     D CNTINS          S              8  0
     D CODEKY          S              6
     D colO            s                   like(col) dim(%elem(col))
     D colN            s                   like(col) dim(%elem(col))
     D CONALW          S              8  2
     D CONTIN          S              1
     d include         s               n
     D COUNT           S              2  0
     d counter         s              5  0
     D CPT4            S              1
     D CPT8            S              8
     D CRTSPL          S              1
     D CT              S              3  0
     D D               S              2  0
     d dc              s              2  0
     D DD              S              1  0
     D DEFDTE          S              8  0
     D DETDTE          S              8  0
     D DFTFLG          S              1
     D diaglookup      s              7
     D DOC#            S              9  0
     D DOCNUM          S              9  0
     D DTLINE          S              1  0
     d dupmngc         s              1    inz('Y')
     D DX              S              1  0
     D E               S              2  0
     D ECODE           S             10
     D EDATE           S              8  0
     D EIGHT           S              8
     d elecwrite       s               n
     d epayor          s              6  0
     d eplan           s              5  0
     d epol            s             20
     D EXC401          S              1
     D EXFC            S              2
     D EXLVL6          S              6  0
     D EXPAYR          S              6  0
     D EXPLAN          S              5  0
     D EXSTAT          S              2
     d ex452q          s              6  2
     d ex463q          s              3  0
     d ex463w          s              5  2
     d ex940q          s              3  0
     d ex940w          s              5  2
     d factor          s              5  2
     D FDATE           S              8  0
     d fexct2          s              3
     d fexprc          s              7
     d fextth          s              9  0
     d firstName       s             12
     D FLAG            S              1  0
     D FORM            S              2
     D FORMTP          S              1
     D FOUND           S              1
     D FRMDTE          S              8  0
     D FRSTDT          S              8  0
     D GARFLG          S              1
     D GRSNET          S              1
     D HCSFC           S              2
     D HLDADR          S             20
     D HLDAD2          S             20
     D HLDCTY          S             15
     D HLDNAM          S             28
     D HLDSTA          S              2
     D HLDTEL          S             10  0
     D HLDZIP          S             10
     D HMAP            S             10
     D*icdver          s              2
     d icndcn          s             18
     D INS#            S              1  0
     D INSBIL          S              9
     d ix              S              2  0
     D JOBNAM          S             10
     D JOBNUM          S              6
     D JOBUSR          S             10
     d jrSr            s              3
     d keyctt          s              1
     D KEYLV6          S              6  0
     D LASTDT          S              8  0
     d lastName        s             18
     D LCODE           S              5
     D LCYCDT          S              8  0
     D LDESC           S             50
     D LDOC#           S              9  0
     D LEVEL6          S              6  0
     D LEVL            S              6
     D LEVL#           S              1  0
     D LEVL6#          S              6  0
     D LICECD          S              5
     d licct2          s              3
     D LVL6NM          S             26
     D LV6#            S              6  0
     D LV6BA2          S             11  2
     D LV6CHG          S             11  2
     D LV6DUE          S             11  2
     D LV6PAT          S             11  2
     D LV6PGS          S              5  0
     D LV6PG2          S              5  0
     D L1NAME          S             40
     D L5NAME          S             40
     D L6NAME          S             40
     d MaxPageReached  s               n
     D MDATE           S              8  0
     D METHOD          S              5
     D MIDNIT          S              1
     d middleInt       s              1
     D MLTDIX          S             15
     d modOvr          s               n
     D MSGDR#          S              9  0
     D NOBILL          S              1
     D NONCPT          S              1
     D OMTYPE          S              1
     D ONCE            S              1
     D ONETRK          S              1
     D OPEN            S              1
     D OPRNAM          S             30
     D ovrlay          S              1
     D PAGENO          S              4  0
     D PAYBA2          S              9  2
     D PAYCHG          S              9  2
     D PAYDUE          S              9  2
     D PAYOR           S              6  0
     D PAYPAT          S              9  2
     D PAYPGS          S              5  0
     D PAYPG2          S              5  0
     D physname        s                   like(hmdnam)
     D PLAN            S              5  0
     D PLNBA2          S              9  2
     D PLNCHG          S              9  2
     D PLNDUE          S              9  2
     D PLNPAT          S              9  2
     D PLNPGS          S              5  0
     D PLNPG2          S              5  0
     D POLICY          S             20
     D PRAUTH          S             20
     D PRECHG          S              1
     D PRGRP#          S             17
     D PRGRPX          S             17
     D PRIADR          S             26
     D PRIAD2          S             26
     D PRIATN          S             26
     D PRICTY          S             15
     D PRINAM          S             28
     D PRINM2          S             28
     D PRINN2          S             14
     D PRISD2          S             26
     D PRISTA          S              2
     D PRITEL          S             10  0
     D PRMCOD          S              2  0
     D PRMFCL          S              2
     D PRMFRU          S              7  2
     D PRMINU          S              6  0
     D PRMLV6          S              6  0
     d prmfle          s             10
     D PRMMBR          S             10
     D PRMNAM          S             40
     D PRMOPT          S              1
     D PRMPRC          S              7
     d prmrel          s                   like(in1rel)
     D PRMREV          S              4  0
     D PRMSDT          S              8  0
     d prmspl          s             10
     D PRMUBC          S              6  0
     D PRPROV          S             15
     D PRRESC          S              9
     D PRRG#           S             15
     D PRRPHY          S             26
     D PRRUPN          S             12
     D PRTCOD          S              9
     d prtmd1          s              2
     d prtmd2          s              2
     d prtmd3          s              2
     d prtmd4          s              2
     d prtrev          s              4  0
     D PRTDEC          S              1
     D PRTDSC          S              1
     D PRTDXD          S              1
     D PRTF            S             10
     D PRTHNM          S             21
     D PRTPMT          S              1
     D PRTPOL          S             20
     D PRTR            S             10
     D PRTTYP          S              2
     D PRVADR          S            112
     D PRVCOD          S              5
     D PRVVAR          S             75
     d qua32b          s              2
     D QUA33B          S              2
     d qualif          s              2
     D QUANTY          S              1  0
     D RANK            S              1  0
     D REGIST          S              1
     d reimtp          S              5
     D REQLV6          S              6  0
     D REQNM2          S             12  0
     D REQNUM          S             12  0
     D REQSRC          S              2
     D REQSYS          S              1
     D REQTYP          S              2
     d returndx        s              8
     D RLCODE          S              2  0
     D rfmdli          S             12
     D ROUND           S              1
     D rowO            s                   like(row) dim(%elem(row))
     D rowN            s                   like(row) dim(%elem(row))
     D RPTBA2          S             11  2
     D RPTBLS          S              5  0
     D RPTCHG          S             11  2
     D RPTDUE          S             11  2
     D RPTPAT          S             11  2
     D RPTPGS          S              5  0
     D rpttype         s              7
     D RQABL           S              3
     D RQACCT          S             12  0
     D RQCAP           S              1
     D RQCSEQ          S              7  0
     D RQDATE          S              8  0
     D RQEFD           S             24
     D RQETD           S             24
     D RQFRTM          S              4  0
     D RQGRP           S             51
     D RQLVL6          S              6  0
     D RQMRNO          S              9  0
     D RQPAYR          S              6  0
     D RQPLAN          S              5  0
     D RQPLCY          S             20
     D RQPRO           S              3
     D RQREIM          S              5
     D RQSCT1          S              3
     D RQSCT2          S              3
     D RQSCT3          S              3
     D RQSDTE          S              8  0
     D RQSPLN          S              5  0
     D RQSPRC          S              7
     D RQSPYR          S              6  0
     D RQSSVD          S              8  0
     D RQSVTP          S              2
     D RQTOTM          S              4  0
     D RQVIS           S              9
     d rtnAuth         s             18
     D RTNEXT          S              5  0
     D RTNFLG          S              1
     D RTNLV1          S              2  0
     D RTNLV2          S              2  0
     D RTNLV3          S              2  0
     D RTNLV4          S              4  0
     D RTNLV5          S              6  0
     D RTNPHN          S             10  0
     D RTTYPE          S              5
     D RUNIDX          S              1
     D SAVDTE          S              8  0
     D SAVPRC          S              7
     D SBSEX           S              1
     D SB2SEX          S              1
     D SDESC           S             20
     D SITNAM          S             30
     D SIXDIG          S              8
     d snpi            s             10
     d spayor          s              6  0
     D SPDDAT          S              8  0
     d splan           s              5  0
     d spol            s             20
     D STATEK          S              2
     D STATUS          S              2
     D STPDFL          S              5
     D STPDTP          S              5
     D SVDIAG          S              6
     D SVFC            S              2
     D SVLVL6          S              6  0
     D SVPAYR          S              6  0
     D SVPLAN          S              5  0
     D SVSTAT          S              2
     d taxid           s             15
     d taxlevel        s              1
     d taxlevelid      s              6  0
     D TCODE           S              4
     D TDATE           S              8  0
     d temp4           s              4  0
     D THERAP          S              9  0
     D THRCT2          S              3
     D TIND            S              1
     D TITLE           S              3
     D TMP508          S              4  0
     D TMPFC           S              2
     D TMPTIM          S              6  0
     d tmpxcd          s              8  0
     d tmpxfc          s              2
     d tmpxl6          s              6  0
     d tmpxpl          s              5  0
     d tmpxpr          s              7
     d tmpxpy          s              6  0
     d tmpxst          s              2
     d tmpxtp          s              1
     D TOBILL          S              7
     D TODATE          S              8  0
     D TOTAL           S              5  0
     D TOTCHG          S              8  2
     D TOTPAT          S              8  2
     D TOTPMT          S              8  2
     D TOTPYR          S              8  2
     D TRKACT          S             14
     D UBCODE          S              6  0
     D UBPLAN          S              5  0
     D V               S              2  0
     d vasid           s             75
     D WKCAT1          S              3
     D WKCAT2          S              3
     D WKLVL6          S              6  0
     D WKPAYR          S              6  0
     D WKPLAN          S              5  0
     D WRKATN          S             26
     D WRKCPT          S              5
     D WRKFCL          S              2
     D WRKFLD          S              9  2
     D WRKLV6          S              6  0
     D WRKPLN          S              5  0
     D WRKPRV          S             75
     D WRKPYR          S              6  0
     D WRKQTY          S              5  0
     D WRKSTA          S              2
     D WRKTQT          S              5  0
     d wxfftl          s             10
     d wxffnm          s             30
     d wxffa1          s             30
     d wxffa2          s             30
     d wxffct          s             20
     d wxffst          s              2
     d wxffzp          s             10
     d wx33tl          s             10
     d wx33nm          s             30
     d wx33a1          s             30
     d wx33a2          s             30
     d wx33ct          s             20
     d wx33st          s              2
     d wx33zp          s             10
     D X               S              2  0
     d xx              s              3  0
     d xy              s              3  0
     d xfptme          s              5  0
     D Y               S              2  0
     D YDATE           S              8  0
     D YDATE1          S              8  0
     D YDATE2          S              8  0
     D YDAYS           S              5  0
     D Z               S              1  0
     *------------------------------------------------------------------------*
     d HBXSEQGEN       pr                  extpgm('HBXSEQGEN')
     d  reqLvl6                       6  0 const
     d  reqAcct                      12  0 const
     d  reqRank                       1  0 const
     d  reqPayr                       6  0 const
     d  reqPlan                       5  0 const
     d  reqPlcy                      20    const
     d  reqProc                       7    const
     d  reqDate                       8  0 const
     d  reqSeqn                        n   const
     d  rtnAuth                      18

     d XFXDXPER        pr                  extpgm('XFXDXPER')
     d  inputdx                       7    const options(*varsize)
     d  inputver                      2    const options(*varsize)
     d  returndx                      8    options(*varsize)

     d XFXPRV          pr                  extpgm('XFXPRV')
     d  prmlv6                        6  0 const
     d  prmact                       12  0 const
     d  prmpyr                        6  0 const
     d  prmpln                        5  0 const
     d  prmcod                        5    const
     d  prmprc                        8    const
     d  prmrev                        4  0 const
     d  prmdte                        8  0 const
     d  prvvar                       75
     d  prvadr                      112

     d XFXBNAM         pr                  extpgm('XFXBNAM')
     d  nameIn                       26    const
     d  nameLast                     18
     d  nameFirst                    12
     d  nameMiddle                    1
     d  nameJR                        3
     *****************************************************************
     IHBFBILL
     I                                          BBACCN        L1
     I                                          BBNAME        L2
     I                                          BBPLAN        L3
     I                                          BBPAYR        L4
     I                                          BBPLV6        L5
     I                                          BBPLV5        L6
     *****************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    REQLV6                         =level 6
     C                   PARM                    CRTSPL                         =create spool flag
     C                   PARM                    PRMMBR
     c                   parm                    prmfle
     c                   parm                    prmspl
     C                   PARM                    RUNIDX                         =run index
     **
     C                   IF        *INL6 = *ON
     C                   EVAL      L6NAME = *BLANKS
     C                   EVAL      L5NAME = *BLANKS
     C     BBPLV6        CHAIN     HXFLVL6                            79
     C                   IF        *IN79 = *OFF
     C                   MOVEL(P)  HX6NAM        SITNAM
     C                   ELSE
     C                   EVAL      SITNAM = *BLANKS
     C                   ENDIF
     C                   MOVE(P)   BBPLV6        LEVL
     C                   EXSR      RMVBLK
     C                   EVAL      L6NAME = %TRIMR(LEVL) + ' ' + SITNAM
     C                   CALL      'XFXCNTR'
     C                   PARM                    L6NAME
     C     BBPLV5        CHAIN     HXFLVL5                            79
     C                   IF        *IN79 = *OFF
     C                   MOVEL(P)  HX5NAM        OPRNAM
     C                   ELSE
     C                   EVAL      OPRNAM = *BLANKS
     C                   ENDIF
     C                   MOVE(P)   BBPLV5        LEVL
     C                   EXSR      RMVBLK
     C                   EVAL      L5NAME = %TRIMR(LEVL) + ' ' + OPRNAM
     C                   CALL      'XFXCNTR'
     C                   PARM                    L5NAME
     C                   ENDIF
     **
     C                   IF        *INU2 = *ON
     C                   EVAL      L6NAME = *BLANKS
     C                   ENDIF
     **
     C                   MOVEL     'Y'           RTNFLG
     C                   MOVEL     ' '           NOBILL
     C                   move      *off          bilexc38
     C                   move      *off          bilexc152
     C                   MOVE      *OFF          BILEXC214
     C                   move      *off          bilexc364
     C                   MOVE      *OFF          BILEXC452
     C                   MOVE      *OFF          BILEXC463
     c                   eval      bilexc935 = *off
     C                   eval      bilexc940 = *off
     **
     C                   IF        ONCE <> 'D'                                  =first pass
     C                   MOVE      ' '           OPEN
     **
     c                   if        bbaafl = 'X'
     c                   eval      rpttype = 'A'
     c                   else
     c                   eval      rpttype = 'M'
     c                   endif
     c                   eval      rpttype = %trim(rpttype) + '/' + bbform
     c                   if        bbtype <> *blanks
     c                   eval      rpttype = %trim(rpttype) + '/' + bbtype
     c                   endif
     **
     c                   eval      ovrlay = 'A'
     c                   if        ovrlay = 'Y'
     c                             or ovrlay = 'A'
     c                   eval      prmspl = 'HBR1500C'
     c                   else
     c                   eval      prmspl = 'HBR1500CN'
     c                   endif
      **
     C                   IF        BBAAFL <> *BLANKS                            =if auto-approve
     C                             OR BBPROC <> *BLANKS                          or final run
     C                             AND *INU7 = *OFF
     C                   MOVE      'P'           ARCTYP                         =permanent
     C                   ELSE
     C                   MOVE      'T'           ARCTYP                         =temporary
     C                   ENDIF
      **
     c                   call      'HXXGETARC'
     c                   parm                    prmfle
     c                   parm                    arcmbr
     c                   eval      prmmbr = arctyp + arcmbr
      **
     C                   MOVE      'N'           CRTSPL
     **
     C                   CALL      'XFXMRASG'                                   =check for med rec
     C                   PARM                    ASGFLG                          assignment type
     **
     C                   CALL      'XFXGUASG'                                   =check for guarantor
     C                   PARM                    GARFLG                          assignment type
     **
     C                   CALL      'XFXRTVJA'                                   =retrieve job
     C                   PARM      *BLANKS       JOBNAM                          attributes
     C                   PARM      *BLANKS       JOBUSR
     C                   PARM      *BLANKS       JOBNUM
     **
     C     chr15skey     KLIST
     c                   kfld                    bbplv6
     c                   kfld                    bbaccn
     c                   kfld                    bgcbsq
      **
     C     DMOIKY        KLIST
     C                   KFLD                    WRKLV6
     C                   KFLD                    BBMRNO
     C                   KFLD                    UBCODE
     C                   KFLD                    UBPLAN
     C                   KFLD                    POLICY
     **
     C     LICEKY        KLIST                                                  =licence key
     C                   KFLD                    LDOC#
     C                   KFLD                    LCODE
     c**
     c     licek2        klist
     c                   kfld                    docnum
     c                   kfld                    statek
     c                   kfld                    lcode
     c**
     C     IVSKEY        KLIST
     C                   KFLD                    BBPLV6
     C                   KFLD                    BBACCN
     C                   KFLD                    BGDTPR
     **
     C     PRMKEY        KLIST                                                  =billed ins
     C                   KFLD                    BBPAYR                          key
     C                   KFLD                    BBPLAN
     **
     C     ICOVKY        KLIST
     C                   KFLD                    BBPLV6
     C                   KFLD                    BBACCN
     C                   KFLD                    RANK
     C                   KFLD                    UBCODE
     C                   KFLD                    UBPLAN
     C                   KFLD                    POLICY
     C                   KFLD                    BBFRDT
     **
     C     ICOVK2        KLIST
     C                   KFLD                    BBPLV6
     C                   KFLD                    BBACCN
     C                   KFLD                    RANK
     C                   KFLD                    UBCODE
     C                   KFLD                    UBPLAN
     C                   KFLD                    POLICY
     **
     C     ICOVK3        KLIST
     C                   KFLD                    BBPLV6
     C                   KFLD                    BBACCN
     C                   KFLD                    BBRANK
     C                   KFLD                    BBPAYR
     C                   KFLD                    BBPLAN
     C                   KFLD                    BBPLCY
     C                   KFLD                    BGDTSD
     **
     C     ICOVK4        KLIST
     C                   KFLD                    BBPLV6
     C                   KFLD                    BBACCN
     C                   KFLD                    BBRANK
     C                   KFLD                    BBPAYR
     C                   KFLD                    BBPLAN
     C                   KFLD                    BBPLCY
     **
     C     SCDKEY        KLIST                                                  =other ins
     C                   KFLD                    UBCODE                          key
     C                   KFLD                    UBPLAN
     **
     C     CHGKEY        KLIST                                                  =charge file
     C                   KFLD                    BBPLV6                          key
     C                   KFLD                    BBACCN                          key
     **
     C     ALWKEY        KLIST                                                  =allow file
     C                   KFLD                    BGDLV6                          key
     C                   KFLD                    BGDTAC
     C                   KFLD                    BGDTPR
     C                   KFLD                    BGDSEQ

     c     chr15key      klist
     c                   kfld                    bgdlv6
     c                   kfld                    bgdtac
     c                   kfld                    bgdseq
     **
     C     THPRKY        KLIST                                                  =ther prov
     C                   KFLD                    MSGDR#                          key
     C                   KFLD                    BBPAYR
     C                   KFLD                    BBPLAN
     C                   KFLD                    LEVEL6
     **
     C     THPRK2        KLIST                                                  =ther prov
     C                   KFLD                    THERAP                          key
     C                   KFLD                    BBPAYR
     C                   KFLD                    BBPLAN
     C                   KFLD                    LEVEL6
     **
     C     LICKEY        KLIST                                                  =ther license
     C                   KFLD                    MSGDR#                          key
     C                   KFLD                    WRKSTA
     C                   KFLD                    LICECD
     **
     C     LICKY2        KLIST                                                  =charge record
     C                   KFLD                    BGDTTH                          license key
     C                   KFLD                    HX6ST1
     C                   KFLD                    LICECD
     **
     c     licky3        klist
     c                   kfld                    bgdtth
     c                   kfld                    hx6st1
     c                   kfld                    licecd
     c                   kfld                    licct2
     **
     C     MASTKY        KLIST                                                  =master file
     C                   KFLD                    BBPLV6                          key
     C                   KFLD                    BBACCN
     **
     C     DEMOKY        KLIST                                                  =demographic
     C                   KFLD                    WRKLV6                          file key
     C                   KFLD                    BGMRNO
     **
     C     GUARKY        KLIST                                                  =guarantor
     C                   KFLD                    KEYLV6                          file key
     C                   KFLD                    BGGACC
     **
     C     EXCPKY        KLIST                                                  =bill form printing
     C                   KFLD                    EXSTAT                          exception key
     C                   KFLD                    EXLVL6
     C                   KFLD                    EXPAYR
     C                   KFLD                    EXPLAN
     C                   KFLD                    EXFC
     C                   KFLD                    EXCODE
     **
     C     EXCCOD        KLIST
     C                   KFLD                    FORM                           =bill form
     C                   KFLD                    FORMTP                         =form type
     **
     C     EXOKEY        KLIST
     C                   KFLD                    HESTAT
     C                   KFLD                    HELVL6
     C                   KFLD                    HEFC
     C                   KFLD                    HEPAYR
     C                   KFLD                    HEPLAN
     C                   KFLD                    HEEXCD
     C                   KFLD                    OMTYPE
     C                   KFLD                    BGDTPR
     **
     C     exoky2        klist
     C                   kfld                    tmpxst                         HESTAT
     C                   kfld                    tmpxl6                         helvl6
     C                   kfld                    tmpxfc                         hefc
     C                   kfld                    tmpxpy                         hepayr
     C                   kfld                    tmpxpl                         heplan
     C                   kfld                    tmpxcd                         heexcd
     C                   kfld                    tmpxtp                         omtype
     C                   kfld                    tmpxpr                         bgdtpr
      **
     C     EMPKEY        KLIST                                                  =employer key
     C                   KFLD                    BGPESI
     C                   KFLD                    BGPEMP
     **
     C     DIAGKY        KLIST
     C                   KFLD                    BBPLV6
     C                   KFLD                    BBACCN
     C                   KFLD                    TODATE
     **
     C     FMLYKY        KLIST                                                  =family
     C                   KFLD                    BDILV6                          contact key
     C                   KFLD                    BDIMRN
     C                   KFLD                    BDICDE
     C                   KFLD                    BDIRLC
     **
     c     prccky        klist
     c                   kfld                    bgdlv6
     c                   kfld                    bgdtpr
      **
     c     cdsckey       klist
     c                   kfld                    bgdlv6
     c                   kfld                    bgdtac
     c                   kfld                    bgdseq
      **
     C     PRTKEY        KLIST
     C                   KFLD                    PRTR
     C                   KFLD                    PRTF
     **
     C     REPKEY        KLIST
     C                   KFLD                    LEVL#
     C                   KFLD                    BBPLV5
     C                   KFLD                    TITLE
     **
     C     CHGBKY        KLIST
     C                   KFLD                    BBTRAK
     C                   KFLD                    BBPLV6
     C                   KFLD                    BBACCN
     **
     C     CHGKY2        KLIST
     C                   KFLD                    BBPLV6
     C                   KFLD                    BBACCN
     C                   KFLD                    BGCBSQ
     **
     c     chrgKey       klist
     c                   kfld                    bgdlv6
     c                   kfld                    bgdtac
     c                   kfld                    bgdseq
     **
     c     chrgKey2      klist
     c                   kfld                    bgcbl6
     c                   kfld                    bgcbac
     c                   kfld                    bgcbsq
     **
     c     chgdxKey      klist
     c                   kfld                    bgclv6
     c                   kfld                    bgctac
     c                   kfld                    bgctpr
     c                   kfld                    bgctsd
     c                   kfld                    bgctth
     **
     c     famlcKey      klist
     c                   kfld                    wrklv6
     c                   kfld                    BBMRNO
     c                   kfld                    keyctt
      **
     C                   MOVE      '15'          FORM
     C                   EVAL      CNTINS = 0
     **
     C                   EVAL      PAGENO = 0
     C                   EVAL      GTBSTP = 1
     C                   IF        BBTRAN <> 'D'
     C                   IF        BBTYPE = 'N5'
     C                   EVAL      LEVL6# = 0
     C                   CALL      'HBXNXTK'                                    =get next trk #
     C                   PARM                    LEVL6#                         =level 6 #
     C                   PARM      *BLANKS       TRKACT                         =tracking number
     C                   ELSE
     C                   IF        REQLV6 <> 0
     C                   CALL      'HBXNXTK'                                    =get next trk #
     C                   PARM      REQLV6        LEVL6#                         =level 6 #
     C                   PARM      *BLANKS       TRKACT                         =tracking number
     C                   ELSE
     C                   CALL      'HBXNXTK'                                    =get next trk #
     C                   PARM      999999        LEVL6#                         =level 6 #
     C                   PARM      *BLANKS       TRKACT                         =tracking number
     C                   ENDIF
     C                   ENDIF
     C                   MOVEL(P)  TRKACT        GTBGTN
     C**
     C                   ENDIF
     **
     C     *DTAARA       DEFINE    HXALEVEL      LEVEL                          =level dsc
     C     *DTAARA       DEFINE    *LDA          LDADS
     C                   IN        *DTAARA                                      =local
     **
     C                   TIME                    RUNDT
     **
     C                   CALL      'XFXUBMAP'
     C                   PARM      '6'           CKTYPE
     C                   PARM      BBPLV6        CKLV6
     C                   PARM                    RTNLV1
     C                   PARM                    RTNLV2
     C                   PARM                    RTNLV3
     C                   PARM                    RTNLV4
     C                   PARM                    RTNLV5
     **
     C                   MOVE      *ON           *IN47
     C                   EVAL      L1NAME = *BLANKS
     C     RTNLV1        CHAIN     HXFLVL1                            79
     C                   IF        *IN79 = *OFF
     C                   CALL      'XFXCNTR'
     C                   PARM      HX1NAM        L1NAME
     C                   ENDIF
     **
     c                   if        open = 'Y'
     C                   MOVE      *ON           *INOA
     C                   MOVE      *ON           *INOE
     c                   endif
     **
     c                   if        MaxPageReached = *off
     C                   EVAL      RPTCHG = 0
     C                   EVAL      RPTPAT = 0
     C                   EVAL      RPTDUE = 0
     C                   EVAL      RPTPGS = 0
     C                   EVAL      RPTBLS = 0
     C                   endif
     C                   eval      MaxPageReached = *off
     **
     C                   MOVE      'D'           ONCE
     C                   ELSE
     C                   EVAL      GTBSTP = 1 + PAGENO
     C                   ENDIF
      **
     c                   call      'XFXTAXID'
     c                   parm                    bbplv6
     c                   parm                    taxid
     c                   parm      '6'           taxlevel
     c                   parm                    taxlevelid
     **
     c                   callp     XFXPRV( bbplv6  : bbaccn  : bbpayr : bbplan
     c                                   : '15FRM' : *blanks : 0      : bbbild
     c                                   : prvvar : prvadr )
     C                   IF        PRVVAR <> '02-12'
     C                   GOTO      SKIP
     C                   ENDIF
     c                   if        (ovrlay = 'A' or ovrlay = 'Y') and
     c                             not %open(hbr1500c)
     c                   open      hbr1500c
     c                   endif
     c                   if        ovrlay <> 'Y'
     C                   if        not %open(hbr1500cn)
     C                   open      hbr1500cn
     C                   endif
     c                   endif
     c                   if        not %open(printer)
     C                   OPEN      PRINTER
     C                   MOVE      *ON           *INOA
     c                   endif
     c                   if        not %open(printr2)
     C                   OPEN      PRINTR2
     C                   MOVE      *ON           *INOE
     c                   endif
     C                   MOVEL     'Y'           OPEN
     *****************************************************************
      ** clear prior detail
      *****************************************************************
     c     bbtrak        setll     e5fchr15
     c                   dou       *in79 = *on
     c     bbtrak        reade     e5fchr15                               79
     c                   if        *in79 = *off
     c                   delete    e5fchr15
     c                   endif
     c                   enddo
     c     bbtrak        chain     e5f1500                            79
     c                   if        *in79 = *off
     c                   delete    e5f1500
     c                   endif
     c                   clear                   e5f1500
     c                   eval      elecwrite = *off
     c                   eval      ediag = *blanks
     c                   eval      sdiag = *blanks
      *****************************************************************
     C                   IF        BBTRAN = 'D'                                 =manual run
     C                   IF        BBTYPE = 'N5'
     C                   EVAL      LEVL6# = 0
     C                   CALL      'HBXNXTK'                                    =get next trk #
     C                   PARM                    LEVL6#                         =level 6 #
     C                   PARM      *BLANKS       TRKACT                         =tracking number
     C                   ELSE
     C                   IF        REQLV6 <> 0
     C                   CALL      'HBXNXTK'                                    =get next trk #
     C                   PARM      REQLV6        LEVL6#                         =level 6 #
     C                   PARM      *BLANKS       TRKACT                         =tracking number
     C                   ELSE
     C                   CALL      'HBXNXTK'                                    =get next trk #
     C                   PARM      999999        LEVL6#                         =level 6 #
     C                   PARM      *BLANKS       TRKACT                         =tracking number
     C                   ENDIF
     C                   ENDIF
     C                   MOVEL(P)  TRKACT        GTBGTN
     C**
     C                   MOVEL     TRKACT        BBTRAK
     C                   ENDIF
     **
     C                   EVAL      BILCHG = 0
     C                   EVAL      BILPAT = 0
     C                   EVAL      BILDUE = 0
     C                   EVAL      BILPGS = 0
     **
     C                   EVAL      RPTBLS = RPTBLS + 1
     **
     **
     C                   IF        BBDCFL = 'Y'
     C                   MOVE      *ON           *IN39
     C                   ELSE
     C                   MOVE      *OFF          *IN39
     C                   ENDIF
     **
     C                   CALL      'XFXDTAAR'                                   =set up
     C                   PARM                    BBPLV6                          level 6
     C                   PARM                    BILDTE                          cycle date
     C                   PARM                    FLAG                            run flag
     C                   PARM                    LCYCDT                          last cyc dt
     **
     C                   EVAL      YDATE = BILDTE
     C                   EXSR      SRCYMD
     C                   EVAL      RPTDTE = MDATE
     **
     ****  box 32 - facilitiy where services were rendered
     ****  box 33 - supplier's billing name and address
     **
     C                   EVAL      LVL2NM = *BLANKS                             =clear
     C                   EVAL      LVL2AD = *BLANKS                              all
     C                   EVAL      LVL2CT = *BLANKS                              bil ctr
     C                   EVAL      LVL2ST = *BLANKS                              and clinic
     C                   EVAL      LVL2ZP = *BLANKS                              info
     C                   EVAL      LVL2TL = 0                                    fields
     C                   EVAL      LVL6NM = *BLANKS
     C                   EVAL      BOX321 = *BLANKS
     C                   EVAL      BOX322 = *BLANKS
     C                   EVAL      BOX323 = *BLANKS
     C                   EVAL      BOX324 = *BLANKS
     C                   EVAL      BOX325 = *BLANKS
     C                   EVAL      BOX32A = *BLANKS
     C                   EVAL      BOX32B = *BLANKS
     C**                 EVAL      BOX32F = 0
     C                   EVAL      REGIST = *BLANKS
     **
     c                   eval      wx33nm = *blanks
     c                   eval      wx33a1 = *blanks
     c                   eval      wx33a2 = *blanks
     c                   eval      wx33ct = *blanks
     c                   eval      wx33st = *blanks
     c                   eval      wx33zp = *blanks
     c                   eval      wx33tl = *blanks
     c                   eval      wxffnm = *blanks
     c                   eval      wxffa1 = *blanks
     c                   eval      wxffa2 = *blanks
     c                   eval      wxffct = *blanks
     c                   eval      wxffst = *blanks
     c                   eval      wxffzp = *blanks
     c                   eval      wxfftl = *blanks

     C                   EVAL      WRKLV6 = BBPLV6
     C                   EVAL      BGMRNO = BBMRNO

     C     BBPLV6        CHAIN     HXFLVL6                            79        =get level 6
     C                   IF        *IN79 = *OFF                                  address
     C                   MOVEL     HX6NAM        LVL6NM                          information
     C                   MOVEL     HX6NAM        BOX321
     C                   IF        HX6A21 <> *BLANKS
     C                   MOVEL     HX6A21        BOX322                         =mailing
     C                   MOVEL     HX6CT2        BOX323                          address
     C                   MOVEL     HX6ST2        BOX324
     c**
     c                   if        hx6z22 <> *blanks
     c                   eval      box325 = hx6z21 + '-' + hx6z22
     c                   else
     c                   eval      box325 = hx6z21 + '-0000'
     c                   endif
     c**
     C                   ELSE
     C                   MOVEL     HX6A11        BOX322                         =normal
     C                   MOVEL     HX6CT1        BOX323                          address
     C                   MOVEL     HX6ST1        BOX324
     c**
     c                   if        hx6z12 <> *blanks
     c                   eval      box325 = hx6z11 + '-' + hx6z12
     c                   else
     c                   eval      box325 = hx6z11 + '-0000'
     c                   endif
      **
     c                   ENDIF
      **
     c                   eval      chkprv = sndprv
     c                   eval      chkcnt = 0
     c                   eval      prmprc = *blanks
     c                   movel     'NPI  '       prvcod
     c     bbtrak        setll     hbfchgbl
     c                   dou       %eof(hbpchgbl)
     c     bbtrak        reade     hbfchgbl
     c                   if        not %eof(hbpchgbl)
     c     chr15skey     chain     hbfchr15s
     c                   if        %found
     c                   eval      prmprc = zgdtpr
     c                   leave
     c                   endif
     c                   endif
     c                   enddo
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   movel     prvvar        box32a
     c                   endif
      **
     C                   EXSR      SRCTAB
     C                   MOVEL     'BDBS'        TCODE
     C                   MOVEL     HX6ST1        ECODE
     C                   EXSR      SRTABL
     C                   IF        TIND <> ' '
     C                   MOVE      *ON           *IN57                          =error
     C                   ENDIF
     C                   ELSE                                                    message
     C                   MOVE      *ON           *IN57                          =no clinic
     C                   ENDIF
     **
     *****************************************************************
     C                   MOVEA     '00000000'    *IN(01)                        =off form id
     C                   MOVEA     '00'          *IN(09)
     C                   MOVEA     '0000'        *IN(12)                        =off form id
     C                   MOVE      *OFF          *IN16
     C                   MOVEA     '00000000'    *IN(21)                        =off prt ids
     C                   MOVEA     '00'          *IN(29)
     C                   MOVEA     '00000000'    *IN(31)
     C                   MOVE      *OFF          *IN40
     C                   MOVEA     '000000'      *IN(41)
     C                   MOVE      *OFF          *IN48
     C                   MOVEA     '00000000'    *IN(51)
     C                   MOVEA     '00'          *IN(59)
     C                   MOVEA     '00000000'    *IN(62)
     C                   MOVEA     '00'          *IN(73)
     C                   MOVEA     '00000'       *IN(82)
     C                   MOVE      '0'           BX24AF                         =box 24a flag
     C                   MOVE      '0'           BX24CF                         =box 24c flag
     C                   MOVE      '0'           BX29F                          =box 29 flag
     C                   MOVEL     'Y'           PRTDSC
     C                   MOVEL     'Y'           PRTDXD
     C                   MOVEL     'N'           PRTDEC
     C                   MOVEL     'N'           PRTPMT
     **
     C                   EVAL      BGMRNO = 0                                   =init fields
     C                   EVAL      BGGACC = 0
     C                   EVAL      INS# = 0
     C                   EVAL      RLCODE = 0
     C                   EVAL      SBBDTE = 0
     C                   EVAL      SB2BDT = 0
     C                   EVAL      INJDTE = 0
     C                   eval      box14q = *blanks
     C                   EVAL      SBSEX = *BLANKS
     C                   EVAL      BOX01A = *BLANKS
     C                   EVAL      PRGRPX = *BLANKS
     C                   EVAL      PRGRP# = *BLANKS
     C                   EVAL      PRAUTH = *BLANKS
     C                   EVAL      TMPFC = *BLANKS
     C                   EVAL      BOX19 = *BLANKS
     C                   EVAL      WRKFCL = *BLANKS                             =financial class
     C                   EVAL      BOX20Y = *BLANKS
     C                   EVAL      BOX20N = *BLANKS
     C                   EVAL      EXC401 = 'N'
     C                   EVAL      PRTPRV = *BLANKS
     C                   EVAL      PRTHNM = *BLANKS
     **
     C                   IF        BBTYPE <> ' '
     C                   IF        BBTRAN = 'A'
     C                             OR BBTRAN = 'T'
     C                             OR BBTRAN = 'R'
     C                   MOVE      *ON           *IN82
     C                   ENDIF
     C                   IF        BBTRAN = 'M'
     C                   MOVE      *ON           *IN85
     C                   ENDIF
     C                   ENDIF
     **
     C                   IF        BBTYPE = ' '
     C                   IF        BBTRAN = 'A'
     C                             OR BBTRAN = 'T'
     C                             OR BBTRAN = 'R'
     C                   MOVE      *ON           *IN83
     C                   ENDIF
     C                   IF        BBTRAN = 'M'
     C                   MOVE      *ON           *IN84
     C                   ENDIF
     C                   ENDIF
     **
     C                   IF        ASGFLG = 'X'                                 =same med rec #
     C                   EVAL      WRKLV6 = 0
     C                   ELSE                                                   =loc specific
     C                   EVAL      WRKLV6 = BBPLV6
     C                   ENDIF
     **
     **** type of bill
     **
     C                   EVAL      TOBILL = *BLANKS
     C                   IF        BBBIND = 9
     C                   MOVEL     'REBILL'      TOBILL                         =rebill
     C                   ENDIF
     **
     C     MASTKY        CHAIN     HBFMAST                            79        =billing
     C     *IN79         CABEQ     *ON           ENDDTL                          master
     **
     C                   IF        TOBILL = *BLANKS
     C                   IF        BBFRDT = BGADDT
     C                   MOVEL     'INITIAL'     TOBILL
     C                   ELSE
     C                   IF        BBTODT = BGDSDT
     C                   MOVEL     'FINAL  '     TOBILL
     C                   ELSE
     C                   MOVEL     'INTERIM'     TOBILL
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     **
     C                   MOVE      BBTRAK        BOX26
     **
     C     BGPLV5        CHAIN     HXFLVL5                            79        =get level 5
     C                   IF        *IN79 = *OFF                                  address
     C                   MOVEL     HX5NAM        LVL2NM                          information
     C                   IF        HX5RA1 <> *BLANKS
     C                   MOVEL     HX5RA1        LVL2AD
     C                   MOVEL     HX5RCT        LVL2CT
     C                   MOVEL     HX5RST        LVL2ST
     c**
     c                   if        hx5rz2 <> *blanks
     c                   eval      lvl2zp = hx5rzp + '-' + hx5rz2
     c                   else
     c                   eval      lvl2zp = hx5rzp + '-0000'
     c                   endif
     c**
     C                   ELSE
     C                   MOVEL     HX5AD1        LVL2AD
     C                   MOVEL     HX5CTY        LVL2CT
     C                   MOVEL     HX5STA        LVL2ST
     c**
     c                   if        hx5zp2 <> *blanks
     c                   eval      lvl2zp = hx5zip + '-' + hx5zp2
     c                   else
     c                   eval      lvl2zp = hx5zip + '-0000'
     c                   endif
     c**
     C                   ENDIF
     C                   EVAL      LVL2TL = HX5PHN
     C                   ENDIF
     **
     C                   MOVEL     LVL2TL        TLDS                           =telephone
     **
     ****  print which insurance is being billed at the top of the bill
     C                   EVAL      INSBIL = *BLANKS
     C                   EVAL      RQDATE = BBTODT                              =get ins info
     C                   IF        BBPREB <> *BLANKS
     C                             AND BBTODT < BBIEFF
     C                   EVAL      RQDATE = BBIEFF
     C                   ENDIF
     C                   EXSR      SRINFO
     **
     C                   IF        BBPAYR = AUB(1)                              =insurance 1
     C                             AND BBPLAN = APL(1)
     C                             AND BBPLCY = APO(1)
     C                   MOVEL     PRM           INSBIL
     C                   EVAL      INS# = 1
     C                   EVAL      UBCODE = AUB(1)
     C                   EVAL      UBPLAN = APL(1)
     C                   MOVEL     APO(1)        POLICY
     C                   ELSE
     C                   IF        BBPAYR = AUB(2)                              =insurance 2
     C                             AND BBPLAN = APL(2)
     C                             AND BBPLCY = APO(2)
     C                   IF        AFC(1) = '  '                                =no primary ins
     C                   MOVEL     PRM           INSBIL
     C                   ELSE
     C                   MOVEL     SEC           INSBIL
     C                   ENDIF
     C                   EVAL      INS# = 2
     C                   EVAL      UBCODE = AUB(2)
     C                   EVAL      UBPLAN = APL(2)
     C                   MOVEL     APO(2)        POLICY
     C                   ELSE
     C                   IF        BBPAYR = AUB(3)
     C                             AND BBPLAN = APL(3)
     C                             AND BBPLCY = APO(3)
     C                   IF        AFC(1) = '  '                                =no primary ins
     C                             AND AFC(2) = '  '                            =no secondary ins
     C                   MOVEL     PRM           INSBIL
     C                   ELSE
     C                   IF        AFC(1) <> '  '                               =primary with
     C                             AND AFC(2) = '  '                             no secondary ins
     C                   MOVEL     SEC           INSBIL
     C                   ELSE
     C                   MOVEL     TER           INSBIL
     C                   ENDIF
     C                   ENDIF
     C                   EVAL      INS# = 3
     C                   EVAL      UBCODE = AUB(3)
     C                   EVAL      UBPLAN = APL(3)
     C                   MOVEL     APO(3)        POLICY
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     c
     c* //--- If no insurance matched, check if rank 4 self pay bill --//
     c                   if        ins# = 0
     c                             and bbrank = 4
     c                   eval      ins# = 4
     c                   eval      ubcode = 1
     c                   eval      ubplan = 1
     c                   movel     slf           insbil
     c                   endif
      **
     c                   exsr      get3ins
     **
     ****  box 10a - patients condition related to employment
     **
     C                   IF        BGCOMP <> *BLANKS
     C                             AND BGCOMP <> 'N'
     C                   MOVE      'X'           BOX10AY
     C                   MOVE      ' '           BOX10AN
     C                   ELSE
     C                   MOVE      ' '           BOX10AY
     C                   MOVE      'X'           BOX10AN
     C                   ENDIF
     **
     ****  inicator to control bill form printing
     **
     C                   IF        HX6ST1 = 'TX'                                =texas
     C                             AND BGCOMP = 'WC'                             work comp
     C                   MOVE      *ON           *IN03
     C                   ELSE
     C                   MOVE      *ON           *IN01                          =normal
     C                   ENDIF
     **
     **  box 1a - insured's id number
     **  box 11 - insured's policy group or feca number
     **  box 11a - insured's date of birth and sex
     **  box 23  - prior authorization number
     **
     C                   EVAL      PRRUPN = *BLANKS
     C                   EVAL      QUA17A = *BLANKS
     C                   CLEAR                   HAFDEMOI
     C                   EVAL      BOX11B = *BLANKS
     C                   EVAL      RLCODE = 0
     C                   EVAL      PRGRPX = *BLANKS
     C                   EVAL      PRRESC = *BLANKS
     C                   EVAL      BOX01A = *BLANKS
     C                   EVAL      SBSEX = *BLANKS
     C                   EVAL      SBBDTE = 0
     C                   EVAL      TMPFC = *BLANKS
     C                   EVAL      BOX04 = *BLANKS
     C                   EVAL      BOX07A = *BLANKS                             =clr address
     C                   EVAL      BOX07B = *BLANKS                             =clr city
     C                   EVAL      BOX07C = *BLANKS                             =clr state
     C                   EVAL      BOX07D = *BLANKS                             =clr zip cd
     C                   EVAL      BOX07E = 0                                   =clr phone
     C     DMOIKY        CHAIN     HAFDEMOI                           79
     C                   IF        *IN79 = *OFF
     C                   MOVE      BDIRLC        RLCODE
     C                   MOVEL     BDIGRP        PRGRPX
     C                   MOVEL     BDIRSC        PRRESC
     **
     C                   MOVEL     BDIPLY        BOX01A
     C                   MOVEL     BDISSX        SBSEX
     C                   EVAL      SBBDTE = BDISBD
     C                   MOVEL     BDIFCL        TMPFC
     **
     C                   IF        BDICDE = 0                                   =no fmly contacts
     C                   MOVEL(P)  BDISNM        BOX04
     **
     C                   EVAL      BOX07A = *BLANKS                             =clr address
     C                   EVAL      BOX07B = *BLANKS                             =clr city
     C                   EVAL      BOX07C = *BLANKS                             =clr state
     C                   EVAL      BOX07D = *BLANKS                             =clr zip cd
     C                   EVAL      BOX07E = 0                                   =clr phone
     **
     C                   IF        GARFLG = 'X'                                 =same guar number
     C                   EVAL      KEYLV6 = 0                                    for all locs
     C                   ELSE                                                   =loc specific
     C                   EVAL      KEYLV6 = BBPLV6
     C                   ENDIF
     **
     C     GUARKY        CHAIN     HAFGUAR                            61        =get guar
     C                   IF        *IN61 = *OFF                                 =if not fnd,
     C                   MOVEL     BRGADR        BOX07A                         =address
     C                   MOVEL     BRGCTY        BOX07B                         =city
     C                   MOVEL     BRGST         BOX07C                         =state
     c**
     c                   if        brgzp2 <> *blanks
     c                   eval      BOX07D = brgzp1 + '-' + brgzp2
     c                   else
     c                   eval      BOX07D = brgzp1 + '-0000'
     c                   endif
     c**
     C                   EVAL      BOX07E = BRGTNO                              =telephone
     C                   ELSE
     C                   EVAL      BRGENM = *BLANKS                             =guar emp name
     C                   EVAL      BRGEA1 = *BLANKS                             =guar emp address
     C                   EVAL      BRGECT = *BLANKS                             =guar emp city
     C                   EVAL      BRGEST = *BLANKS                             =guar emp state
     C                   EVAL      BRGEZ1 = *BLANKS                             =guar emp zip
     C                   EVAL      BRGETN = 0                                   =guar emp telephone
     C                   ENDIF
     **
     C                   ELSE                                                   =family contact
     C     FMLYKY        CHAIN     HAFFAMLY                           79
     C                   IF        *IN79 = *OFF
     C                   MOVEL     AFMNAM        BOX04
     C                   MOVEL     AFMAD1        BOX07A
     C                   MOVEL     AFMCTY        BOX07B
     C                   MOVEL     AFMSTA        BOX07C
     c**
     c                   if        afmzi2 <> *blanks
     c                   eval      box07d = afmzip + '-' + afmzi2
     c                   else
     c                   eval      box07d = afmzip + '-0000'
     c                   endif
     c**
     C                   CALL      'HXXPHNL'
     C                   PARM      0             PRMLV6
     C                   PARM      AFMMRN        REQNUM
     C                   PARM      AFMCDE        REQNM2
     C                   PARM      'FM'          REQSRC
     C                   PARM      'H '          REQTYP
     C                   PARM      0             RTNPHN
     C                   PARM      0             RTNEXT
     C                   EVAL      BOX07E = RTNPHN
     C                   ENDIF
     C                   ENDIF
     **
     C                   ELSE
     C                   EVAL      RLCODE = 0
     C                   EVAL      PRGRPX = *BLANKS
     C                   EVAL      PRRESC = *BLANKS
     C                   EVAL      BOX01A = *BLANKS
     C                   EVAL      SBSEX = *BLANKS
     C                   EVAL      SBBDTE = 0
     C                   EVAL      TMPFC = *BLANKS
     C                   EVAL      BOX04 = *BLANKS
     C                   EVAL      BOX07A = *BLANKS                             =clr address
     C                   EVAL      BOX07B = *BLANKS                             =clr city
     C                   EVAL      BOX07C = *BLANKS                             =clr state
     C                   EVAL      BOX07D = *BLANKS                             =clr zip cd
     C                   EVAL      BOX07E = 0                                   =clr phone
     C                   ENDIF
     **
     C                   EVAL      RANK = INS#                                  =get coverage
     C     ICOVKY        SETGT     HBFICOV
     C     ICOVK2        READPE    HBFICOV                                79
     C                   IF        *IN79 = *OFF
     C                             AND BCVEFF <= BBTODT
     C                   MOVEL     BCVTRA        PRAUTH                         =auth #
     C                   MOVEL     BCVTRA        BBAUTH
     C                   ENDIF
     **
     ****  box 8b - patient employment status (normal)
     **
     C                   IF        *IN01 = *ON
     **
     ****  box 11a - subscriber sex
     **
     C                   IF        SBSEX = 'M'
     C                   MOVE      'X'           box11am
     C                   MOVE      ' '           box11af
     C                   ELSE
     C                   IF        SBSEX = 'F'
     C                   MOVE      ' '           box11am
     C                   MOVE      'X'           box11af
     C                   ELSE
     C                   MOVE      ' '           box11am
     C                   MOVE      ' '           box11af
     C                   ENDIF
     C                   ENDIF
     **
     C                   ENDIF
     **
     C                   MOVE      'TH'          STATUS                         =get therapist
     C                   EXSR      SRASGN
     **
     C                   IF        INS# = 0
     C                   EVAL      CNTINS = CNTINS + 1
     C                   ENDIF
     **
     C                   IF        INS# <> 0
     C                   EVAL      BOX11C = *BLANKS                             =ins name
     C                   EVAL      PRINAM = *BLANKS                             =prov name  s
     C                   EVAL      PRIADR = *BLANKS                             =prov address
     C                   EVAL      PRIAD2 = *BLANKS                             =prov address line 2
     C                   EVAL      PRICTY = *BLANKS                             =prov city
     C                   EVAL      PRISTA = *BLANKS                             =prov state
     C                   EVAL      PRIZIP = *BLANKS                             =prov zip code
     C                   EVAL      PRPROV = *BLANKS                             =provider #
     C                   EVAL      PRITEL = 0                                   =telephone
     C                   EVAL      PRIATN = *BLANKS                             =provider #
     **
     ****  box 11c - provider name
     **
     C                   MOVE      'G'           GRSNET
     C     PRMKEY        CHAIN     XFFBNFIT                           79        =benfit file
     C                   IF        *IN79 = *OFF
     C                   eval      ppdlv6 = bbplv6
     C                   eval      ppdacc = bbaccn
     C                   eval      ppddte = bbtodt
     C                   eval      ppdpyr = bbpayr
     C                   eval      ppdpln = bbplan
     C                   EXSR      SRPPD
     C                   MOVEL     XFBNAM        BOX11C                         =prov name
     C                   MOVEL     XFBNAM        PRINAM                         =prov name
     C                   MOVEL     XFBADR        PRIADR                         =prov address
     C                   MOVEL     XFBAD2        PRIAD2                         =prov address line 2
     C                   MOVEL     XFBCTY        PRICTY                         =prov city
     C                   MOVEL     XFBSTA        PRISTA                         =prov state
     c**
     c                   if        xfbzp2 <> *blanks
     c                   eval      prizip = xfbzip + '-' + xfbzp2
     c                   else
     c                   eval      prizip = xfbzip + '-0000'
     c                   endif
     c**
     C                   EVAL      PRITEL = XFBTEL                              =prov zip exten
     C                   MOVEL     XFBNAM        HLDNAM                         =prov name
     C                   MOVEL     XFBADR        HLDADR                         =prov address
     C                   MOVEL     XFBAD2        HLDAD2                         =prov address line 2
     C                   MOVEL     XFBCTY        HLDCTY                         =prov city
     C                   MOVEL     XFBSTA        HLDSTA                         =prov state
     c**
     c                   if        xfbzp2 <> *blanks
     c                   eval      hldzip = xfbzip + '-' + xfbzp2
     c                   else
     c                   eval      hldzip = xfbzip + '-0000'
     c                   endif
     c**
     C                   EVAL      HLDTEL = XFBTEL                              =prov zip exten
     C                   MOVE      SNDBNG        GRSNET                         =bill at flg
     C                   MOVE      XFBFCL        WRKFCL                         =financial class
     C                   MOVEL(P)  XFBATN        WRKATN
     **
     ****  box below form - necessary attachments/requirements
     **
     C                   IF        SNDLMN = 'Y'                                 =letter
     C                   MOVE      *ON           *IN53                           med
     C                   ENDIF                                                   with bill
     **
     C                   IF        SNDBPN = 'Y'                                 =progress
     C                   MOVE      *ON           *IN54                           notes
     C                   ENDIF                                                   with bill
     **
     C                   IF        SNDPRB = 'Y'                                 =rx
     C                   MOVE      *ON           *IN55                           required
     C                   ENDIF                                                   to bill
     **
     C                   IF        SNDPRR = 'Y'                                 =phys refer
     C                   MOVE      *ON           *IN56                           required
     C                   ENDIF                                                   to bill
      **
      ****  BOX 33A - NPI and BOX33B - PROVIDER NUMBER
      **
     c                   eval      box33a = *blanks
     c                   eval      prprov = *blanks
     c                   eval      qua33b = *blanks
     c                   eval      chkprv = sndprv
     c                   EVAL      PRMPRC = *BLANKS
     c                   exsr      getproc
     c**
     c     findprovid    tag
      **
     c                   select
     c*****************************************************************
     c** code D - facility NPI
     c*****************************************************************
     c                   when      chkprv = 'D'
     c                   movel     'NPI  '       prvcod
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   movel     prvvar        box33a
     c                   endif
     c*****************************************************************
     c** code C - facility provider ID
     c*****************************************************************
     c                   when      chkprv = 'C'
     c                   movel     'PRVDR'       prvcod
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   movel     prvvar        prprov
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm                    prvcod
     C                   parm                    qua33b
     c                   endif
     c                   movel     'NPI  '       prvcod
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   movel     prvvar        box33a
     c                   endif
     c*****************************************************************
     c** code U - therapist NPI
     c*****************************************************************
     c                   when      chkprv = 'U'
     c                   eval      docnum = msgdr#
     c                   eval      statek = *blanks
     c                   eval      lcode = *blanks
     c                   movel     'NPI  '       lcode
     c     licek2        chain     hmflice                            79
     c                   if        *in79 = *off
     c                   movel     hlcnum        box33a
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm                    lcode
     C                   parm                    qua33b
     c                   endif
     c*****************************************************************
     c** code T - therapist provider ID
     c*****************************************************************
     c                   when      chkprv = 'T'
     c                   eval      level6 = bbplv6                              =check by
     c     thprky        chain     hmfplnpr                           79         level 6, payor/plan
     c                   if        *in79 = *off
     c                   movel     hmtpr#        prprov
     c                   else
     c                   eval      level6 = 0                                   =check by
     c     thprky        chain     hmfplnpr                           79         payor/plan alone
     c                   if        *in79 = *off
     c                   movel     hmtpr#        prprov
     c                   endif
     c                   endif
     C     *in79         ifne      *off
     c                   movel     'PRVDR'       prvcod
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm                    prvcod
     C                   parm                    qua33b
     c                   endif
     c**
     c                   eval      docnum = msgdr#
     c                   eval      statek = *blanks
     c                   eval      lcode = *blanks
     c                   movel     'NPI  '       lcode
     c     licek2        chain     hmflice                            79
     c                   if        *in79 = *off
     c                   movel     hlcnum        box33a
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm                    lcode
     C                   parm                    qua33b
     c                   endif
     c*****************************************************************
     c** code X - use tax id
     c*****************************************************************
     c                   when      chkprv = 'X'
     c                   eval      prprov = hx6tid
     c                   eval      lcode = *blanks
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm      'TAXID'       lcode
     C                   parm                    qua33b
     c                   movel     'NPI  '       prvcod
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   movel     prvvar        box33a
     c                   endif
     c                   endsl
     c*****************************************************************
     c** if no provider variable found for method, try another
     c*****************************************************************
     c                   select
     c                   when      chkprv = 'D'
     c                             and box33a = *blanks
     c                   eval      chkprv = 'U'
     c                   goto      findprovid
     c                   when      chkprv = 'C'
     c                             and prprov = *blanks
     c                   eval      chkprv = 'T'
     c                   goto      findprovid
     c                   when      chkprv = 'T'
     c                             and prprov = *blanks
     c                   eval      chkprv = 'X'
     c                   goto      findprovid
     c                   endsl
     c**
     C                   MOVEL     PRPROV        BBPROV
      **
     c                   if        prprov <> *blanks
     c     qua33b        cat       prprov:1      prprov
     c                   endif
      **
     c                   eval      prmprc = *blanks
     c                   movel     '1533B'       prvcod
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   eval      prprov = *blanks
     c                   movel     prvvar        prprov
     c                   endif
      **
     C                   ENDIF
     **
     *****  box 1 - get financial class
     **
     C     BBPAYR        CHAIN     XFFINSD                            79
     C                   IF        *IN79 = *OFF
     C                   EXSR      SRCTAB
     C                   MOVEL     'BFCL'        TCODE
     C                   MOVEL     XFIFCL        ECODE
     C                   EXSR      SRTABL
     C                   MOVE      HMAP          HCSFC
     C                   IF        HCSFC = 'MC'                                 =medicare
     C                   MOVE      *ON           *IN34
     c                   eval      box01X1 = 'X'
     C                   ELSE
     C                   IF        XFIFCL = 'MD'                                =medicaid
     C                             or bbpayr = 5223                             =yes this is a rig
     C                   MOVE      *ON           *IN35
     c                   eval      box01X2 = 'X'
     C                   ELSE
     C                   MOVE      *ON           *IN36                          =other
     c                   eval      box01X3 = 'X'
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     **
     C     DEMOKY        CHAIN     HAFDEMO                            61        =get demo
     C                   IF        *IN61 = *ON                                  =if not fnd,
     C                   EVAL      BDPBDT = 0                                   =clr birth
     C                   EVAL      BDPSEX = *BLANKS                             =clr sex
     C                   EVAL      BDPMST = *BLANKS                             =clr martl
     C                   EVAL      BDPAD1 = *BLANKS                             =clr address
     C                   EVAL      BDPCTY = *BLANKS                             =clr city
     C                   EVAL      BDPSTA = *BLANKS                             =clr state
     C                   EVAL      BDPZP1 = *BLANKS                             =clr zip cd
     C                   EVAL      BDPZP2 = *BLANKS                             =clr zip cd
     C                   EVAL      BDPHTL = 0                                   =clr phone
     C                   EVAL      BDPSS# = 0                                   =clr soc sec #
     C                   ENDIF
     c**
     c** Box 05 D
     c                   if        bdpzp2 <> *blanks
     c                   eval      box05d = bdpzp1 + '-' + bdpzp2
     c                   else
     c                   eval      box05d = bdpzp1 + '-0000'
     c                   endif
     **
     ****  box 3a - patient birth date
     **
     C                   EVAL      YDATE = BDPBDT
     C                   EXSR      SRCYMD
     C                   EVAL      PRBIRT = MDATE
     **
     ****  box 3b - patient sex
     **
     C                   IF        BDPSEX = 'M'
     C                   MOVE      *ON           *IN21                          =male
     c                   eval      box03m = 'X'
     c                   eval      box03f = ' '
     C                   ELSE
     C                   IF        BDPSEX = 'F'
     C                   MOVE      *ON           *IN22                          =female
     c                   eval      box03m = ' '
     c                   eval      box03f = 'X'
     C                   ENDIF
     C                   ENDIF
     **
     ****  box 6 - patient relationship to insured
     **
     C                   IF        RLCODE = 01
     C                   MOVE      *ON           *IN41                          =self
     c                   eval      box06a = 'X'
     C                   ELSE
     C                   IF        RLCODE = 02
     C                   MOVE      *ON           *IN42                          =spouse
     c                   eval      box06b = 'X'
     C                   ELSE
     C                   IF        RLCODE = 18
     C                   MOVE      *ON           *IN43                          =parent
     c                   eval      box06c = 'X'
     C                   ELSE
     C                   MOVE      *ON           *IN44                          =other
     c                   eval      box06d = 'X'
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     **
     ****  box 9 - other insured's name
     ****  box 9a - other insured's policy or group number
     ****  box 9b - other insured's date of birth
     ****  box 11d - is there another health benefit plan
     **
     C                   EVAL      PRTPOL = *BLANKS
     **
     C                   IF        INS# = 1                                     =ins 1
     C                   IF        AFC(2) <> '  '                               =no 2nd
     C                             AND AFC(2) <> 'SP'                           =self pay
     C                             AND AFC(2) <> 'CT'                           =contract
     C                             AND AFC(2) <> 'PP'                           =private pay
     C                   MOVE      *ON           *IN24                          =if found,
     C                   MOVE      'X'           BOX11DY
     C                   EVAL      UBCODE = AUB(2)                              =ub code
     C                   EVAL      UBPLAN = APL(2)                              =plan code
     C                   MOVEL     APO(2)        PRINN2                         =policy no.
     C                   MOVEL     APO(2)        POLICY                         =policy no.
     C                   CLEAR                   HAFDEMOI
     C     DMOIKY        CHAIN     HAFDEMOI                           79
     C                   MOVEL     BDISNM        PRISD2                         =insured nm
     C                   EVAL      SB2BDT = BDISBD
     C                   MOVEL     BDISSX        SB2SEX                         =sex
     C                   ELSE
     C                   IF        AFC(3) <> '  '                               =no 3rd
     C                             AND AFC(3) <> 'SP'                           =self pay
     C                             AND AFC(3) <> 'CT'                           =contract
     C                             AND AFC(3) <> 'PP'                           =private pay
     C                   MOVE      *ON           *IN24                          =if found,
     C                   MOVE      'X'           BOX11DY
     C                   EVAL      UBCODE = AUB(3)                              =ub code
     C                   EVAL      UBPLAN = APL(3)                              =plan code
     C                   MOVEL     APO(3)        PRINN2                         =policy no.
     C                   MOVEL     APO(3)        POLICY                         =policy no.
     C                   CLEAR                   HAFDEMOI
     C     DMOIKY        CHAIN     HAFDEMOI                           79
     C                   MOVEL     BDISNM        PRISD2                         =insured nm
     C                   EVAL      SB2BDT = BDISBD
     C                   MOVEL     BDISSX        SB2SEX                         =sex
     C                   ELSE
     C                   EVAL      *IN24 = *OFF
     C                   EVAL      BOX11DN = 'X'
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     **
     C                   IF        INS# = 2                                     =ins 2
     C                   IF        AFC(1) <> 'CT'                               =not contract
     C                             AND AFC(1) <> 'SP'                           =not private duty
     C                             AND AFC(1) <> 'PP'                           =not private pay
     C                             AND AFC(1) <> '  '                           =no 1st
     C                   MOVE      *ON           *IN24                          =if found,
     C                   MOVE      'X'           BOX11DY
     C                   EVAL      UBCODE = AUB(1)                              =ub code
     C                   EVAL      UBPLAN = APL(1)                              =plan code
     C                   MOVEL     APO(1)        PRINN2                         =policy no.
     C                   MOVEL     APO(1)        POLICY
     C                   CLEAR                   HAFDEMOI
     C     DMOIKY        CHAIN     HAFDEMOI                           79
     C                   IF        *IN79 = *OFF
     C                   MOVEL     BDISNM        PRISD2                         =insured nm
     C                   EVAL      SB2BDT = BDISBD
     C                   MOVEL     BDISSX        SB2SEX                         =sex
     C                   ENDIF
     C                   ELSE
     C                   EVAL      *IN24 = *OFF
     C                   EVAL      BOX11DN = 'X'
     C                   ENDIF
     C                   ENDIF
     **
     C                   IF        INS# = 3                                     =ins 3
     C                   IF        AFC(2) <> 'CT'                               =not contract
     C                             AND AFC(2) <> 'SP'                           =not private duty
     C                             AND AFC(2) <> 'PP'                           =not private pay
     C                             AND AFC(2) <> '  '                           =no 2nd
     C                   MOVE      *ON           *IN24                          =if found,
     C                   MOVE      'X'           BOX11DY
     C                   EVAL      UBCODE = AUB(2)                              =ub code
     C                   EVAL      UBPLAN = APL(2)                              =plan code
     C                   MOVEL     APO(2)        PRINN2                         =policy no.
     C                   MOVEL     APO(2)        POLICY
     C                   CLEAR                   HAFDEMOI
     C     DMOIKY        CHAIN     HAFDEMOI                           79
     C                   IF        *IN79 = *OFF
     C                   MOVEL     BDISNM        PRISD2                         =insured nm
     C                   EVAL      SB2BDT = BDISBD
     C                   MOVEL     BDISSX        SB2SEX                         =sex
     C                   ENDIF
     C                   ELSE
     C                   IF        AFC(1) <> 'CT'                               =not contract
     C                             AND AFC(1) <> 'SP'                           =not private duty
     C                             AND AFC(1) <> 'PP'                           =not private pay
     C                             AND AFC(1) <> '  '                           =no 1st
     C                   MOVE      *ON           *IN24                          =if found,
     C                   MOVE      'X'           BOX11DY
     C                   EVAL      UBCODE = AUB(1)                              =ub code
     C                   EVAL      UBPLAN = APL(1)                              =plan code
     C                   MOVEL     APO(1)        PRINN2                         =policy no.
     C                   MOVEL     APO(1)        POLICY
     C                   CLEAR                   HAFDEMOI
     C     DMOIKY        CHAIN     HAFDEMOI                           79
     C                   IF        *IN79 = *OFF
     C                   MOVEL     BDISNM        PRISD2                         =insured nm
     C                   EVAL      SB2BDT = BDISBD
     C                   MOVEL     BDISSX        SB2SEX                         =sex
     C                   ENDIF
     C                   ELSE
     C                   EVAL      *IN24 = *OFF
     C                   EVAL      BOX11DN = 'X'
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     **
     ****  box 9d - insurance plan name or program name
     **
     C     SCDKEY        CHAIN     XFFBNFIT                           79
     C                   IF        *IN79 = *OFF
     C                   ENDIF
     **
      **
     c                   eval      prmprc = *blanks
     c                   eval      *in38 = *off
     c                   movel     '159D '       prvcod
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   eval      prinm2 = *blanks
     c                   movel     prvvar        prinm2
     c                   eval      *in38 = *on
     c                   endif
     **
     ****  box 10b - patients condition related to auto accident
     C                   IF        BGAUTO <> *BLANKS
     C                             AND BGAUTO <> 'N'
     C                   MOVE      *ON           *IN26                          =auto accid
     C                   MOVE      'X'           BOX10BY
     C                   MOVE      ' '           BOX10BN
     C                   ELSE
     C                   MOVE      ' '           BOX10BY
     C                   MOVE      'X'           BOX10BN
     C                   ENDIF
     **
     ****  box 10c - patients condition related to other accident
     **
     C                   IF        BGACCI <> *BLANKS
     C                             AND BGACCI <> 'N'
     C                   MOVE      'X'           BOX10CY
     C                   MOVE      ' '           BOX10CN
     C                   ELSE
     C                   MOVE      ' '           BOX10CY
     C                   MOVE      'X'           BOX10CN
     C                   ENDIF
     **
     ****  box 12a - set up patient signature on file
     **
     C                   MOVEL     SIGN          box12
     **
     ****  box 12b - patient signature on file date
     **
     C                   EVAL      YDATE = BGADDT                               =convert
     C                   EXSR      SRCYMD                                        admission
     C                   EVAL      PRADDT = MDATE                                date
     c                   eval      pradt6= %int(%char(%date(praddt:*usa):*mdy0))
     **
     ****  box 13  - set up signature on file insured
     **
     C                   MOVEL     SIGN          box13
     **
     ****  box 17 - name of referring physician
     ****  box 17a - id number of referring physician
     **
     c                   eval      box17q = *blanks
     C                   EVAL      PRRUPN = *BLANKS
     C                   EVAL      QUA17A = *BLANKS
     C                   EVAL      rfmdli = *blanks
 004 c                   move      *blanks       prrnpi
     C                   EVAL      PRRPHY = *BLANKS
     C                   MOVE      'RF'          STATUS
     C                   EXSR      SRASGN
     C                   IF        MSGDR# <> 0
     C     MSGDR#        CHAIN     HMFMAMS                            79
     C                   IF        *IN79 = *OFF
     c                   eval      box17q = 'DN'
     C                   MOVEL     HMDNAM        PRRPHY                         =name
     C                   EVAL      LDOC# = MSGDR#
     c                   eval      lcode = *blanks
     C                   MOVEL     'NPI '        LCODE
     C     LICEKY        CHAIN     HMFLICE2                           79
     C                   IF        *IN79 = *OFF
     C                   MOVEL     HLCNUM        PRRNPI
     C                   ELSE
     c                   eval      lcode = *blanks
     C                   MOVEL     'UPIN'        LCODE
     C     LICEKY        CHAIN     HMFLICE2                           79
     C                   IF        *IN79 = *OFF
     C                   MOVEL     HLCNUM        PRRUPN                         =upin #
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm                    lcode
     C                   parm                    qua17a
     C                   ENDIF
     C                   ENDIF
     c*****************************************************************
     c**   Get Referring Physician Mediciad License Info
     c*****************************************************************
     C                   MOVEL     HX6ST1        WRKSTA                         =With State
     C                   MOVEL     'MCAID'       LICECD
     C     LICKEY        CHAIN     HMFLICE                            79
     C                   IF        *IN79 = *OFF
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm                    licecd
     C                   parm                    qualif
     C                   eval      rfmdli = qualif + ' ' + HLCNUM
     C                   ELSE
     C                   EVAL      WRKSTA = *BLANKS                             =W/O state
     C     LICKEY        CHAIN     HMFLICE                            79
     C                   IF        *IN79 = *OFF
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm                    licecd
     C                   parm                    qualif
     C                   eval      rfmdli = qualif + ' ' + HLCNUM
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      **
     c                   eval      prmprc = *blanks
     c                   movel     '1517 '       prvcod
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   eval      prrphy = *blanks
     c                   movel     prvvar        prrphy
     c                   endif
      **
     c                   eval      prmprc = *blanks
     c                   movel     '1517A'       prvcod
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   movel     prvvar        rpupn14
     c                   eval      qua17a = rpupnq
     c                   eval      prrupn = rpupnn
     c                   endif
     **
     ****  box 22  - set up medicaid resubmission
     **
     c                   eval      box22a = *blanks
     c                   eval      box22b = *blanks

     c                   eval      icndcn = *blanks
     c                   if        bbtorb = '7' or bbtorb = '8'
     c****                         or bbp15A <> ' '
     c                   movel     bbtorb        box22a
     c                   movel     bbtcn         icndcn
     c                   if        icndcn <> *blanks
     c                   movel     icndcn        box22b
     c                   endif
     c                   endif
     C****               IF        BBP15A <> ' '
     c****               eval      box22b = bbtcn
     c****               else
     c****               eval      box22b = *blanks
     C****               ENDIF
     **
     ****  box 25  - set up federal tax i.d. number ein flag
     **
     C                   MOVEL     'X'           BOX25E
     **
     ****  box 27  - set up accept assignment
     **
     C                   MOVEL     'X'           BOX27Y
     C                   MOVEL     ' '           BOX27N
     **
     ** box 31 - primary therapist name
     **
     c                   eval      physname = *blanks
     C                   MOVE      'TH'          STATUS
     C                   EXSR      SRASGN
     C                   IF        MSGDR# <> 0
     C     MSGDR#        CHAIN     HMFMAMS                            79
     C                   IF        *IN79 = *OFF
     C                   MOVEL     HMDNAM        PRTHNM                          and license #
     c                   eval      physname = hmdnam
     C                   MOVEL     'REGST'       LICECD
     C                   MOVEL     HX6ST1        WRKSTA
     C     LICKEY        CHAIN     HMFLICE                            79
     C                   IF        *IN79 = *OFF
     C                   MOVEL     HLCNUM        PRRG#
     C                   MOVEL     HLCTHC        PRTCOD
     C                   MOVEL     HMDTYP        PRTTYP
     C                   MOVEL     HLCCT2        THRCT2
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     **
     C                   EVAL      WRKPYR = BBPAYR
     C                   EVAL      WRKPLN = BBPLAN
     C                   EVAL      WKLVL6 = BBPLV6
     **
     c                   eval      prmprc = *blanks
     c                   eval      *in51 = *off
     c                   movel     '1510D'       prvcod
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   eval      wklvl6 = 0
     c                   eval      *in51 = *on
     c                   movel     prvvar        box10d
     c                   endif
      **
     C                   EVAL      DTLINE = 6                                   =default 6 dtl lines
     c                   eval      bgdct2 = fexct2
     c                   eval      bgdtpr = fexprc
     c                   eval      bgdtth = fextth
     c****               eval      physname = *blanks
     c                   eval      bilexc582 = *off
     c                   eval      bilexc837 = *off
     c                   eval      bilexc861 = *off
     c                   eval      bilexc885 = *off
     c                   eval      bilexc941 = *off
     **
     C                   MOVE      'F'           FORMTP
     C     EXCCOD        SETLL     HBFBEXC                                      =check for
     C                   DOU       *IN79 = *ON                                  =form
     C     EXCCOD        READE     HBFBEXC                                79    =exceptions
     C                   IF        *IN79 = *OFF
     C                             AND EXDELT <> 'X'
     **
     C                   MOVEL     HX6ST1        EXSTAT                         =state & fc printing
     C                   EVAL      EXLVL6 = BBPLV6
     C                   EVAL      EXPAYR = BBPAYR
     C                   EVAL      EXPLAN = BBPLAN
     C                   MOVEL     TMPFC         EXFC
     C                   EXSR      BILEXC
     **
     C                   ENDIF
     C                   ENDDO
     c                   eval      bgdct2 = *blanks
     c                   eval      bgdtpr = *blanks
     c                   eval      bgdtth = 0
     **
     *****************************************************************
     **                    set up 1500 bill detail                  **
     *****************************************************************
     C                   EVAL      DTF = *BLANKS                                =clear
     C                   EVAL      DTT = *BLANKS                                 all
     C                   EVAL      DTF6 = *BLANKS                                detail
     C                   EVAL      DTT6 = *BLANKS                                fields
     C                   EVAL      DTTM = *BLANKS
     C                   EVAL      DTFM = *BLANKS
     C                   EVAL      DTTD = *BLANKS
     C                   EVAL      DTFD = *BLANKS
     C                   EVAL      DTTY = *BLANKS
     C                   EVAL      DTFY = *BLANKS
     C                   EVAL      PBF = 0
     C                   EVAL      PBT = 0
     C                   EVAL      POS = *BLANKS
     C                   EVAL      CPT = *BLANKS
     C                   EVAL      DDX = *BLANKS
     C                   EVAL      MOD = *BLANKS
     C                   EVAL      AMT = 0
     C                   EVAL      QTY = 0
     C                   EVAL      QTY4 = 0
     C                   EVAL      QTY2 = 0
     C                   eval      qty5 = 0
     C                   EVAL      DIX = *BLANKS
     C                   EVAL      PIN = *BLANKS
     c                   eval      pdsc = *blanks
     c                   eval      bx24i = *blanks
     c                   eval      npi = *blanks
     C                   EVAL      TOT = 0
     C                   EVAL      SVDIAG = *BLANKS                             =clr diag
     C                   EVAL      SAVDTE = 0                                   =clr date
     C                   EVAL      SAVPRC = *BLANKS                             =clr proc
     C                   EVAL      Z = 0                                        =clr index
     C                   EVAL      TOTCHG = 0                                   =clr chrgs
     C                   EVAL      TOTPYR = 0                                   =clr payr chrgs
     C                   EVAL      TOTPMT = 0                                   =clr payment
     C                   EVAL      TOTPAT = 0                                   =clr patient prorate
     C                   EVAL      BALDUE = 0                                   =clr bal due for lis
     C                   EVAL      BALDUB = 0                                   =clr bal due for bil
     C                   EVAL      CONALW = 0                                   =cntrt alw t
     C                   MOVE      '0'           ONETRK                         =tracking acct#

     c***                exsr      srsetupbox21

      *** Get diagnosis codes
     c                   callp     xfxdiag( bbplv6 : bbaccn : bbtodt : diag
     c                                    : poaf   : bodyp   : vers  )
      **
     c                   if        bilexc941 = *off
     c                   exsr      srsetupbox21
     c                   else
     c                   exsr      ex941box21
     c                   endif
      **
     c                   eval      bbamnt = 0
     c                   eval      nobill = 'X'
     c                   eval      counter = 0
      **
     C                   EVAL      BBAMNT = 0
     C                   MOVEL     'X'           NOBILL
     C                   IF        *IN16 = *OFF
     C     CHGKEY        SETLL     HBFCHR15                                     =set fil ptr
     C                   ELSE
     C     CHGKEY        SETLL     HBFCH15D                                     =set fil ptr
     C                   ENDIF
     **
     C                   DOU       *IN70 = *ON
     C     NXTRD         TAG
     C                   IF        *IN16 = *OFF
     C     CHGKEY        READE     HBFCHR15                               70    =read detail
     C                   ELSE
     C     CHGKEY        READE     HBFCH15D                               70    =read detail
     C                   ENDIF
     C                   IF        *IN70 = *OFF                                 =equal
     **
     C                   EVAL      ACTION = *BLANKS                             =skip charge
     C                   EXSR      SRCGBL                                        if not on bill
     C     ACTION        CABEQ     'N'           NXTRD
     **
     C                   MOVEL     BGDICD        SVDIAG
     **
     C                   SELECT
     C                   WHEN      BBRANK = 1
     C     BGDBU1        CABEQ     ' '           NXTRD                          =not billed
     C                   IF        BBPAYR <> BGDUB1
     C                             OR BBPLAN <> BGDPL1
     C                             OR BBPLCY <> BGDPO1
     C                   GOTO      NXTRD
     C                   ENDIF
     C                   EVAL      WRKFLD = 0
     C                   IF        *IN69 = *ON
     C                             AND BBBIND = 9
     C                   EVAL      WRKFLD = BGDCI1 + BGDCP1
     C                   EVAL      WRKFLD = WRKFLD + BGDDM1
     C                   IF        BGDGP1 = 0
     C                             AND WRKFLD = 0
     C                   GOTO      NXTRD
     C                   ENDIF
     C                   ENDIF
     C                   EXSR      SRCTAB
     C                   MOVEL     'BFCL'        TCODE
     C                   MOVEL     BGDFC1        ECODE
     C                   EXSR      SRTABL
     C                   MOVE      HMAP          HCSFC
     C                   IF        HCSFC = 'MC'
     C                             AND BGDGP1 = 0
     C                   GOTO      NXTRD
     C                   ELSE
     C                   IF        BGDFC1 = 'MD'
     C                             AND BGDGP1 = 0
     C                   GOTO      NXTRD
     C                   ELSE
     C                   ENDIF
     C                   ENDIF
     C                   WHEN      BBRANK = 2
     C     BGDBU2        CABEQ     ' '           NXTRD                          =not billed
     C                   IF        BBPAYR <> BGDUB2
     C                             OR BBPLAN <> BGDPL2
     C                             OR BBPLCY <> BGDPO2
     C                   GOTO      NXTRD
     C                   ENDIF
     C                   EVAL      WRKFLD = 0
     C                   IF        *IN69 = *ON
     C                             AND BBBIND = 9
     C                   EVAL      WRKFLD = BGDCI2 + BGDCP2
     C                   EVAL      WRKFLD = WRKFLD + BGDDM2
     C                   IF        BGDGP2 = 0
     C                             AND WRKFLD = 0
     C                   GOTO      NXTRD
     C                   ENDIF
     C                   ENDIF
     C                   EXSR      SRCTAB
     C                   MOVEL     'BFCL'        TCODE
     C                   MOVEL     BGDFC2        ECODE
     C                   EXSR      SRTABL
     C                   MOVE      HMAP          HCSFC
     C                   IF        HCSFC = 'MC'
     C                             AND BGDGP2 <= 0
     C                             AND BGDAP2 <= 0
     C                             AND BGDCI2 <= 0
     C                             AND BGDPP2 <= 0
     C                             AND BGDCP2 <= 0
     C                             AND BGDDM2 <= 0
     C                             AND BGDAJ2 <= 0
     C                   GOTO      NXTRD
     C                   ELSE
     C                   IF        BGDFC2 = 'MD'
     C                             AND BGDGP2 <= 0
     C                             AND BGDAP2 <= 0
     C                             AND BGDCI2 <= 0
     C                             AND BGDPP2 <= 0
     C                             AND BGDCP2 <= 0
     C                             AND BGDDM2 <= 0
     C                             AND BGDAJ2 <= 0
     C                   GOTO      NXTRD
     C                   ELSE
     C                   ENDIF
     C                   ENDIF
     C                   WHEN      BBRANK = 3
     C     BGDBU3        CABEQ     ' '           NXTRD                           =not billed
     C                   IF        BBPAYR <> BGDUB3
     C                             OR BBPLAN <> BGDPL3
     C                             OR BBPLCY <> BGDPO3
     C                   GOTO      NXTRD
     C                   ENDIF
     C                   EVAL      WRKFLD = 0
     C                   IF        *IN69 = *ON
     C                             AND BBBIND = 9
     C                   EVAL      WRKFLD = BGDCI3 + BGDCP3
     C                   EVAL      WRKFLD = WRKFLD + BGDDM3
     C                   IF        BGDGP3 = 0
     C                             AND WRKFLD = 0
     C                   GOTO      NXTRD
     C                   ENDIF
     C                   ENDIF
     C                   EXSR      SRCTAB
     C                   MOVEL     'BFCL'        TCODE
     C                   MOVEL     BGDFC3        ECODE
     C                   EXSR      SRTABL
     C                   MOVE      HMAP          HCSFC
     C                   IF        HCSFC = 'MC'
     C                             AND BGDGP3 <= 0
     C                             AND BGDAP3 <= 0
     C                             AND BGDCI3 <= 0
     C                             AND BGDPP3 <= 0
     C                             AND BGDCP3 <= 0
     C                             AND BGDDM3 <= 0
     C                             AND BGDAJ3 <= 0
     C                   GOTO      NXTRD
     C                   ELSE
     C                   IF        BGDFC3 = 'MD'
     C                             AND BGDGP3 <= 0
     C                             AND BGDAP3 <= 0
     C                             AND BGDCI3 <= 0
     C                             AND BGDPP3 <= 0
     C                             AND BGDCP3 <= 0
     C                             AND BGDDM3 <= 0
     C                             AND BGDAJ3 <= 0
     C                   GOTO      NXTRD
     C                   ENDIF
     C                   ENDIF
     C                   OTHER
     C     BGDBUP        CABEQ     ' '           NXTRD                           =not billed
     C                   ENDSL
     C                   MOVEL     ' '           NOBILL
     **
     C                   SELECT
     C                   WHEN      BBRANK = 1
     C                   EVAL      TOT(1) = TOT(1) + BGDNP1
     C                   WHEN      BBRANK = 2
     C                   EVAL      TOT(2) = TOT(2) + BGDNP2
     C                   WHEN      BBRANK = 3
     C                   EVAL      TOT(3) = TOT(3) + BGDNP3
     C                   ENDSL
     C                   EVAL      TOTPAT = TOTPAT + BGDNPT                     =amt prorated to
     C                   EVAL      TOTPAT = TOTPAT - BGDPPT                      client
     C                   EVAL      CHPRSP = BGDNPT - BGDPPT                     =charge pat resp.
     **
     **
     c
     c                   if        bilexc941 = *off
     c                   exsr      fillediag
     c                   else
     c                   exsr      ex941ediag
     c                   endif
     c                   if        BGDTC4 = *blanks
     c                   eval      BGDTC4 = bgdtpr
     c                   eval      zzdtc4 = bgdtpr
     c                   eval      zzdmod = *blanks
     c                   eval      zzdmd2 = *blanks
     c                   eval      zzdmd3 = *blanks
     c                   eval      zzdmd4 = *blanks
     c                   endif
      **
     C                   IF        *IN37 = *ON                                  =exception 60
     c                   eval      tmpxst = b060st
     c                   eval      tmpxl6 = b060l6
     c                   eval      tmpxfc = b060fc
     c                   eval      tmpxpy = b060py
     c                   eval      tmpxpl = b060pl
     c                   eval      tmpxcd = b060cd
     c                   eval      tmpxtp = b060tp
     c                   eval      tmpxpr = bgdtpr
     c     exoky2        chain     hbfbfxo
     c                   if        %found(hbpbfxo) = *off or bxodlt = 'D'
     C                   IF        BGDTSD = SAVDTE
     C                             AND BGDTPR = SAVPRC
     C                   IF        BILEXC452 = *OFF AND BILEXC463 = *OFF
     C                             and bilexc940 = *off
     C                   IF        BGDTQT <> 0
     c                   ADD       BGDTQT        QTY(Z)
     C                   EVAL      QTY4(Z) = QTY4(Z) + BGDTQT
     c                   ADD       BGDTQT        QTY2(Z)
     c                   eval      qty5(z) += bgdtqt
     C                   ELSE
     c                   ADD       1             QTY(Z)
     C                   EVAL      QTY4(Z) = QTY4(Z) + 1
     c                   ADD       1             QTY2(Z)
     c                   eval      qty5(z) += 1
     C                   ENDIF
     C                   ELSE
     C                   EXSR      BE604XX
     C                   ENDIF
     C                   EVAL      AMT(Z) = AMT(Z) + BGDTAM
     C                   EVAL      BBAMNT = BBAMNT + BGDTAM
     C                   EVAL      TOTCHG = TOTCHG + BGDTAM
     C                   EVAL      TOTPYR = TOTPYR + BGDTAM
     C                   EVAL      BILCHG = BILCHG + BGDTAM
     C                   EVAL      RPTCHG = RPTCHG + BGDTAM
     C                   EVAL      LV6CHG = LV6CHG + BGDTAM                     =site
     C                   EVAL      PAYCHG = PAYCHG + BGDTAM
     C                   EVAL      PLNCHG = PLNCHG + BGDTAM
     C                   EVAL      ACCCHG = ACCCHG + BGDTAM
     **
     C                   EVAL      YDATE = BGDTSD
     C                   EXSR      SRCYMD
     C                   EVAL      DEFDTE = MDATE
     **
     C                   EVAL      DETDTE = DEFDTE
     **
     C                   EVAL      CHGTOT = CHGTOT + BGDTAM
     C                   EVAL      CLITOT = CLITOT + BGDNPT                     =amt prorated to
     C                   EVAL      CLITOT = CLITOT - BGDPPT                      client
     C                   EVAL      PAYPG2 = PAYPG2 + 1
     C                   EVAL      PLNPG2 = PLNPG2 + 1
     C                   EVAL      LV6PG2 = LV6PG2 + 1
     C                   EVAL      ACCPG2 = ACCPG2 + 1
     C                   IF        BILEXC214
     C                   EVAL      QTY(Z) = 1
     C                   EVAL      QTY4(Z) = 1
     C                   EVAL      QTY2(Z) = 1
     C                   eval      qty5(z) = 1
     C                   ENDIF
     C                   GOTO      NEXTD
     C                   ELSE
     C                   IF        SAVDTE <> 0
     C**
     C                   if        bilexc364
     C                   move      'P'           omtype
     C     exokey        chain     hbfbfxo                            75
     C                   if        *in75 = *on
     C                             or bxodlt = 'D'
     C                   eval      *in74 = *off
     C                   eval(h)   qty(z) = qty2(z)
     C                   eval(h)   qty4(z) = qty2(z)
     C                   eval(h)   qty2(z) = qty2(z)
     C                   eval(h)   qty5(z) = qty2(z)
     C                   endif
     C                   endif
     C**
     C                   IF        ROUND = 'Y'
     C                   EXSR      SROUND
     C                   ENDIF
     C                   IF        BILEXC214
     C                   EVAL      QTY(Z) = 1
     C                   EVAL      QTY4(Z) = 1
     C                   EVAL      QTY2(Z) = 1
     C                   eval      qty5(z) = 1
     C                   ENDIF
     c                   eval      zzdpds = pdsc(z)
     c****               eval      zzd24j = npi(z)
     c                   eval      zz24iq = bx24i(z)
     c                   eval      zz24jp = pin(z)
     c                   eval      zz24jn = npi(z)
     C                   eval      zzdtam = amt(z)
     C                   eval      zzdtqt = qty5(z)
     C                   eval      zzcs06 = qty2(z)
     C                   eval      counter += 1
     C                   eval      zzcont = counter
     c                   eval      zzdpos = pos(z)
     c                   eval      zzdtc4 = cpt(z)
     c                   eval      zzdmod = %subst(mod(z):1:2)
     c***                eval      zzdmd2 = %subst(mod(z):3:2)
     c                   eval      zzdmd2 = %subst(mod(z):4:2)
     c***                eval      zzdmd3 = %subst(mod(z):5:2)
     c                   eval      zzdmd3 = %subst(mod(z):7:2)
     c***                eval      zzdmd4 = %subst(mod(z):7:2)
     c                   eval      zzdmd4 = %subst(mod(z):10:2)
     c                   eval      zzdptr = bgdptr
     c                   eval      zzdpt2 = bgdpt2
     c                   eval      zzdpt3 = bgdpt3
     c                   eval      zzdpt4 = bgdpt4
     C                   movel     dtf(z)        mdate
     C                   exsr      srcmdy
     C                   eval      zzdfsd = ydate
     C                   movel     dtt(z)        mdate
     C                   exsr      srcmdy
     C                   eval      zzddt1 = ydate
     C                   if        zzddt1 <> 0
     C                   eval      zzddtc = 'RD8'
     C                   eval      zzddts = %char(zzdfsd) + '-' + %char(zzddt1)
     C                   else
     C                   eval      zzddtc = 'D8'
     C                   eval      zzddts = %char(zzdfsd)
     C                   endif
     C                   write     e5fchr15
     C                   EXCEPT    BILDET
     C                   MOVE      *OFF          *IN47
     C                   ENDIF
     C                   ENDIF
     c                   endif
     C                   ENDIF
     **
     C                   IF        Z > 0                                        =last line,
     C                   if        amt(z) < 0
     C                              or (amt(z) = 0 and sndf37 <> 'Y')
     C                              and grsnet = 'G'
     C                   EVAL      DTF(Z) = *BLANKS                              line
     C                   EVAL      DTT(Z) = *BLANKS                              empty,
     C                   EVAL      DTF6(Z) = *BLANKS                             clear
     C                   EVAL      DTT6(Z) = *BLANKS                             line
     C                   EVAL      DTTM(Z) = *BLANKS
     C                   EVAL      DTFM(Z) = *BLANKS
     C                   EVAL      DTTD(Z) = *BLANKS
     C                   EVAL      DTFD(Z) = *BLANKS
     C                   EVAL      DTTY(Z) = *BLANKS
     C                   EVAL      DTFY(Z) = *BLANKS
     C                   EVAL      PBF(Z) = 0                                    and
     C                   EVAL      PBT(Z) = 0                                    backup
     C                   EVAL      POS(Z) = *BLANKS                              index
     C                   EVAL      CPT(Z) = *BLANKS
     C                   EVAL      DDX(Z) = *BLANKS
     C                   EVAL      MOD(Z) = *BLANKS
     C                   EVAL      AMT(Z) = 0
     C                   EVAL      QTY(Z) = 0
     C                   EVAL      QTY4(Z) = 0
     C                   EVAL      QTY2(Z) = 0
     C                   eval      qty5(z) = 0
     C                   EVAL      DIX(Z) = *BLANKS
     C                   EVAL      PIN(Z) = *BLANKS
     C                   eval      pdsc(z) = *blanks
     C                   EVAL      Z = Z - 1
     C                   ENDIF
     C                   ENDIF
     **
     C                   IF        Z >= DTLINE                                  =if pg full,
     C                   EXSR      NEWPAG                                        print and advance
     C                   ENDIF
     **
     C                   EVAL      Z = Z + 1                                    =incr index
     **
     c                   if        bilexc941 = *off
     c                   exsr      fillpointer
     c                   else
     c                   exsr      ex941pointer
     c                   endif
     **
     C                   EVAL      YDATE = BGDTSD
     C                   EXSR      SRCYMD
     C                   MOVE      MDATE         DTF(Z)
     C                   EVAL      PBF(Z) = MDATE
     **
     C                   MOVEL     MDATE         MMDD6                          =6 digit
     C                   MOVE      DTF(Z)        YY6                             date conversion
     C                   MOVE      WKDAT6        DTF6(Z)
     C                   MOVE      MM6           DTFM(Z)
     C                   MOVE      DD6           DTFD(Z)
     C                   MOVE      YY6           DTFY(Z)
     **
     **
     C                   EVAL      DTT(Z) = *BLANKS                             =date-to
     C                   EVAL      PBT(Z) = 0
     C                   EVAL      DTT6(Z) = *BLANKS
     C                   EVAL      DTTM(Z) = *BLANKS
     C                   EVAL      DTTD(Z) = *BLANKS
     C                   EVAL      DTTY(Z) = *BLANKS
     **
     C                   IF        BGDPOS <> *BLANKS
     C                   MOVE      BGDPOS        POS(Z)
     C                   ELSE
     C                   EXSR      SRPOSD
     C                   ENDIF
     **
     C                   IF        BGDTBN = 999963                              =if charge is an
     C                   MOVEL     BGDCT3        CAT2                            additional census
     C                   ELSE                                                    charge, use cat3 as
     C                   MOVEL     BGDCT2        CAT2                            cat2 - rescare
     C                   ENDIF                                                   specific change
     C                   EXSR      SRPTCD                                       =alt print code
     C                   IF        CPT(Z) = *BLANKS                             =if no code,
     C                   IF        BGDTC4 <> *BLANKS
     C                   MOVEL     BGDTC4        CPT(Z)
     C                   ENDIF
     C                   MOVEL     BGDTPR        CPT(Z)                          use charge
     C                   ENDIF                                                   procedure
     **
     c                   if        modOvr
     c****               eval      mod(z) = bgdmod + bgdmd2 + bgdmd3 + bgdmd4
     c                   eval      mod(z) = bgdmod + ' ' + bgdmd2 + ' '
     c                             + bgdmd3 + ' ' + bgdmd4
     c                   else
     C                   EXSR      CHKMOD
     c                   endif
     **
     c                   eval      ddx(z) = bgdicd
     **
     C                   EVAL      AMT(Z) = BGDTAM                              =detail amt
     C                   EVAL      BBAMNT = BBAMNT + AMT(Z)                     =amount billed
     **
     C                   IF        BGDTQT = 0                                   =determine
     C                   EVAL      QTY(Z) = 1                                   =detail
     C                   EVAL      QTY4(Z) = 1                                  =detail
     C                   EVAL      QTY2(Z) = 1                                  =detail
     C                   eval      qty5(z) = 1
     C                   ELSE                                                   =quantity
     c                   z-add     bgdtqt        qty(z)
     c                   z-add     bgdtqt        qty4(z)
     c                   z-add     bgdtqt        qty2(z)
     c                   z-add     bgdtqt        qty5(z)
     C                   ENDIF
     **
      **  npi number
     c                   if        bilexc861
     c                             and snpi <> *blanks
     c                   movel     snpi          npi(z)
     c                   else
     c                   if        bgdtth <> 0
     c                   eval      ldoc# = bgdtth
     c                   eval      lcode = *blanks
     c                   movel     'NPI  '       lcode
     c     liceky        chain     hmflice2                           79
     c                   if        *in79 = *off
     c                   movel     hlcnum        npi(z)
     c                   endif
     c                   endif
     c                   endif
     c                   eval      ldoc# = 0
      **
     c                   move      *off          bilexc38
     c                   move      *off          bilexc152
     C                   MOVE      *OFF          BILEXC214
     C                   move      *off          bilexc364
     C                   MOVE      *OFF          BILEXC452
     C                   MOVE      *OFF          BILEXC463
     C                   eval      bilexc940 = *off
     C                   MOVE      'D'           FORMTP
     C                   eval      *in87 = *off
     C     EXCCOD        SETLL     HBFBEXC                                      =check for
     C                   DOU       *IN79 = *ON                                  =detail line
     C     EXCCOD        READE     HBFBEXC                                79    =exceptions
     C                   IF        *IN79 = *OFF
     C                             AND EXDELT <> 'X'
     C                   MOVEL     HX6ST1        EXSTAT
     C                   EVAL      EXLVL6 = BBPLV6
     C                   EVAL      EXPAYR = BBPAYR
     C                   EVAL      EXPLAN = BBPLAN
     C                   MOVEL     TMPFC         EXFC
     C                   EXSR      BILEXC
     C                   ENDIF
     C                   ENDDO

     c     chr15key      chain     hbfch15e                                     will always find...
     c                   eval      zztrak = bbtrak
     c                   if        ldoc# <> 0
     c                   eval      zzdtth = ldoc#
     c                   endif
     c                   eval      zzdpos = pos(z)
     c                   eval      zzdtc4 = cpt(z)
     c                   eval      zzdmod = %subst(mod(z):1:2)
     c***                eval      zzdmd2 = %subst(mod(z):3:2)
     c                   eval      zzdmd2 = %subst(mod(z):4:2)
     c***                eval      zzdmd3 = %subst(mod(z):5:2)
     c                   eval      zzdmd3 = %subst(mod(z):7:2)
     c***                eval      zzdmd4 = %subst(mod(z):7:2)
     c                   eval      zzdmd4 = %subst(mod(z):10:2)
     c                   eval      zzdptr = bgdptr
     c                   eval      zzdpt2 = bgdpt2
     c                   eval      zzdpt3 = bgdpt3
     c                   eval      zzdpt4 = bgdpt4
     C                   movel     dtf(z)        mdate
     C                   exsr      srcmdy
     C                   eval      zzdfsd = ydate
     C                   movel     dtt(z)        mdate
     C                   exsr      srcmdy
     C                   eval      zzddt1 = ydate
     C                   if        zzddt1 <> 0
     C                   eval      zzddtc = 'RD8'
     C                   eval      zzddts = %char(zzdfsd) + '-' + %char(zzddt1)
     C                   else
     C                   eval      zzddtc = 'D8'
     C                   eval      zzddts = %char(zzdfsd)
     C                   endif
     **
     C                   EVAL      TOTCHG = TOTCHG + BGDTAM                     =add to bal
     C                   EVAL      TOTPYR = TOTPYR + BGDTAM                     =add to bal
     C                   EVAL      BILCHG = BILCHG + BGDTAM
     C                   EVAL      RPTCHG = RPTCHG + BGDTAM
     C                   EVAL      LV6CHG = LV6CHG + BGDTAM                     =site
     C                   EVAL      YDATE = BGDTSD
     C                   EXSR      SRCYMD
     C                   EVAL      DEFDTE = MDATE
     **
     C                   EVAL      DETDTE = DEFDTE
     **
     C                   EVAL      CHGTOT = CHGTOT + BGDTAM
     C                   EVAL      CLITOT = CLITOT + BGDNPT                     =amt prorated to
     C                   EVAL      CLITOT = CLITOT - BGDPPT                      client
     C                   EVAL      PAYPG2 = PAYPG2 + 1
     C                   EVAL      PLNPG2 = PLNPG2 + 1
     C                   EVAL      LV6PG2 = LV6PG2 + 1
     C                   EVAL      ACCPG2 = ACCPG2 + 1
     C                   IF        *IN37 = *OFF                                 =exception 60
     C**
     C                   if        bilexc364
     C                   move      'P'           omtype
     C     exokey        chain     hbfbfxo                            75
     C                   if        *in75 = *on
     C                             or bxodlt = 'D'
     C                   eval      *in74 = *off
     C                   eval(h)   qty(z) = qty2(z)
     C                   eval(h)   qty4(z) = qty2(z)
     C                   eval(h)   qty2(z) = qty2(z)
     C                   eval(h)   qty5(z) = qty2(z)
     C                   endif
     C                   endif
     C**
     C                   IF        ROUND = 'Y'
     C                   EXSR      SROUND
     C                   ENDIF
     C                   IF        BILEXC214
     C                   EVAL      QTY(Z) = 1
     C                   EVAL      QTY4(Z) = 1
     C                   EVAL      QTY2(Z) = 1
     C                   eval      qty5(z) = 1
     C                   ENDIF
     c                   eval      zzdpds = pdsc(z)
     c****               eval      zzd24j = npi(z)
     c                   eval      zz24iq = bx24i(z)
     c                   eval      zz24jp = pin(z)
     c                   eval      zz24jn = npi(z)
     C                   eval      zzdtam = amt(z)
     C                   eval      zzdtqt = qty5(z)
     C                   eval      zzcs06 = qty2(z)
     C                   eval      counter += 1
     C                   eval      zzcont = counter
     C                   write     e5fchr15
     C                   EXCEPT    BILDET
     C                   MOVE      *OFF          *IN47
     C                   ENDIF
     C                   EVAL      SAVDTE = BGDTSD
     C                   MOVEL     BGDTPR        SAVPRC                         =save proc
     C                   EVAL      PAYCHG = PAYCHG + BGDTAM
     C                   EVAL      PLNCHG = PLNCHG + BGDTAM
     C                   EVAL      ACCCHG = ACCCHG + BGDTAM
     C                   IF        PRTPMT = 'Y'                                 =include
     C                   IF        INS# = 1                                      billed
     C                   EVAL      TOTPMT = TOTPMT - BGDPP1                      payors
     C                   ENDIF                                                   payments
     C                   IF        INS# = 2
     C                   EVAL      TOTPMT = TOTPMT - BGDPP1
     C                   EVAL      TOTPMT = TOTPMT - BGDPP2
     C                   ENDIF
     C                   IF        INS# = 3
     C                   EVAL      TOTPMT = TOTPMT - BGDPP1
     C                   EVAL      TOTPMT = TOTPMT - BGDPP2
     C                   EVAL      TOTPMT = TOTPMT - BGDPP3
     C                   ENDIF
     C                   ELSE
     C                   IF        BX29F = '0'                                  =no excpt 1o
     C                   IF        INS# = 2                                     =secondary
     C                   EVAL      TOTPMT = TOTPMT - BGDPP1                     =bill show
     C                   ENDIF                                                  =only prim
     C                   IF        INS# = 3                                     =tertiary
     C                   EVAL      TOTPMT = TOTPMT - BGDPP1                     =bill show
     C                   EVAL      TOTPMT = TOTPMT - BGDPP2                     =prim & sec
     C                   ENDIF
     ** as per donna 2/19 sub  bgcppt    totpmt
     C                   ENDIF
     C                   ENDIF
     **
     C                   ENDIF
     C     NEXTD         TAG
     C                   ENDDO
     **
     C                   EVAL      BILPAT = BILPAT + TOTPAT
     C                   EVAL      RPTPAT = RPTPAT + TOTPAT
     C                   EVAL      LV6PAT = LV6PAT + TOTPAT                     =site
     C                   EVAL      PAYPAT = PAYPAT + TOTPAT
     C                   EVAL      PLNPAT = PLNPAT + TOTPAT
     C                   EVAL      ACCPAT = ACCPAT + TOTPAT
     **
     C                   IF        Z > 0                                        =last line,
     C                   IF        AMT(Z) < 0
     C                             OR AMT(Z) = 0
     C                             AND XFBCAP = *BLANKS
     C                   EVAL      DTF(Z) = *BLANKS                              line
     C                   EVAL      DTT(Z) = *BLANKS                              empty,
     C                   EVAL      PBF(Z) = 0                                    line
     C                   EVAL      PBT(Z) = 0                                    empty,
     C                   EVAL      POS(Z) = *BLANKS                              clear
     C                   EVAL      CPT(Z) = *BLANKS                              line
     C                   EVAL      DDX(Z) = *BLANKS
     C                   EVAL      AMT(Z) = 0                                    one
     C                   EVAL      QTY(Z) = 0                                    index
     C                   EVAL      QTY4(Z) = 0
     C                   EVAL      QTY2(Z) = 0
     C                   eval      qty5(z) = 0
     C                   EVAL      DIX(Z) = *BLANKS
     c                   eval      pdsc(z) = *blanks
     C                   EVAL      Z = Z - 1
     C                   ENDIF
     **
     C     Z             CABEQ     0             ENDDTL                         =last line zero
     **
     C                   EVAL      BALDUB = TOTPYR - TOTPMT
     C                   EVAL      BALDUE = TOTCHG - TOTPAT
     C                   EVAL      BILDUE = BILDUE + BALDUE
     C                   EVAL      RPTDUE = RPTDUE + BALDUE
     C                   EVAL      LV6DUE = LV6DUE + BALDUE
     C                   EVAL      PAYDUE = PAYDUE + BALDUE
     C                   EVAL      PLNDUE = PLNDUE + BALDUE
     C                   EVAL      ACCDUE = ACCDUE + BALDUE
     **
     c                   eval      todate = bgdtsd
     C     diagky        setgt     hmfpdiag
     C     chgkey        readpe    hmfpdiag
     C                   IF        REGIST = 'Y'
     C                   EVAL      INJDTE = BGIDXD
     C                   ELSE
     C                   EVAL      INJDTE = DXDATE
     C                   ENDIF
     c                   eval      box14q = '431'                               Onset of illness
     **
     C                   IF        ONETRK = '0'
     C                   MOVE      '1'           ONETRK
     C                   ENDIF
     C                   MOVE      DTF(1)        FRMDTE
     C                   MOVE      DTT(Z)        TODATE
     C                   eval      prtf = prtfilN
     C                   MOVEL     LDAFOQ        PRTR
     C                   EXSR      SETPTR
     C                   eval      rowN = row
     C                   eval      colN = col
     C                   eval      icrN = icr
     C                   eval      prtf = prtfilO
     C                   MOVEL     LDAFOQ        PRTR
     C                   EXSR      SETPTR
     C                   eval      rowO = row
     C                   eval      colO = col
     C                   eval      icrO = icr
     C                   EVAL      PAGENO = PAGENO + 1
     c                   if        EXC401 = 'Y'
     c                   movel     prprov        prtprv
     c                   endif
     C                   IF        *IN37 = *ON
     C**
     C                   if        bilexc364
     C                   move      'P'           omtype
     C     exokey        chain     hbfbfxo                            75
     C                   if        *in75 = *on
     C                             or bxodlt = 'D'
     C                   eval      *in74 = *off
     C                   eval(h)   qty(z) = qty2(z)
     C                   eval(h)   qty4(z) = qty2(z)
     C                   eval(h)   qty2(z) = qty2(z)
     C                   eval(h)   qty5(z) = qty2(z)
     C                   endif
     C                   endif
     C**
     C                   IF        ROUND = 'Y'
     C                   EXSR      SROUND
     C                   ENDIF
     C                   IF        BILEXC214
     C                   EVAL      QTY(Z) = 1
     C                   EVAL      QTY4(Z) = 1
     C                   EVAL      QTY2(Z) = 1
     C                   eval      qty5(z) = 1
     C                   ENDIF
     c                   eval      zzdpds = pdsc(z)
     c****               eval      zzd24j = npi(z)
     c                   eval      zz24iq = bx24i(z)
     c                   eval      zz24jp = pin(z)
     c                   eval      zz24jn = npi(z)
     C                   eval      zzdtam = amt(z)
     C                   eval      zzdtqt = qty5(z)
     C                   eval      zzcs06 = qty2(z)
     C                   eval      counter += 1
     C                   eval      zzcont = counter
     C                   if        cpt(z) <> *blanks
     c                   eval      zzdtc4 = cpt(z)
     c                   eval      zzdmod = %subst(mod(z):1:2)
     c***                eval      zzdmd2 = %subst(mod(z):3:2)
     c                   eval      zzdmd2 = %subst(mod(z):4:2)
     c***                eval      zzdmd3 = %subst(mod(z):5:2)
     c                   eval      zzdmd3 = %subst(mod(z):7:2)
     c***                eval      zzdmd4 = %subst(mod(z):7:2)
     c                   eval      zzdmd4 = %subst(mod(z):10:2)
     c                   endif
     C                   if        zzddt1 <> 0
     C                   eval      zzddtc = 'RD8'
     C                   eval      zzddts = %char(zzdfsd) + '-' + %char(zzddt1)
     C                   else
     C                   eval      zzddtc = 'D8'
     C                   eval      zzddts = %char(zzdfsd)
     C                   endif
     C                   write     e5fchr15
     C                   EXCEPT    BILDET
     C                   MOVE      *OFF          *IN47
     C                   ENDIF
     c                   if        ovrlay = 'A' or ovrlay = 'Y'
     C                   eval      col = colO
     C                   eval      row = rowO
     C                   eval      icr = icrO
     C                   WRITE     BILLP2
     c                   endif
     c                   if        ovrlay <> 'Y'
     C                   eval      col = colN
     C                   eval      row = rowN
     C                   eval      icr = icrN
     C                   WRITE     BILLP1
     c                   endif
      **
     c                   if        elecwrite = *off
     c                   exsr      fillelecdata
     c                   eval      elecwrite = *on
     c                   endif
      **
     C                   EXCEPT    DETAIL                                       =print report detail
     **
     c                   if        ovrlay = 'A'
     c                   eval      crtspl = 'A'
     c                   else
     C                   MOVE      'Y'           CRTSPL
     c                   endif
     **
     C                   EVAL      BILPGS = BILPGS + 1
     C                   EVAL      RPTPGS = RPTPGS + 1
     C                   EVAL      LV6PGS = LV6PGS + 1
     C                   EVAL      PAYPGS = PAYPGS + 1
     C                   EVAL      PLNPGS = PLNPGS + 1
     C                   EVAL      ACCPGS = ACCPGS + 1
     **
     C                   ENDIF
     C                   ENDIF
     **
     C     ENDDTL        TAG
     **
     C     PRMKEY        CHAIN     XFFBNFIT                           79        =benfit file
     C                   IF        *IN79 = *OFF
     C                             AND XFBCAP <> *BLANKS
     C                   MOVEL     ' '           NOBILL
     C                   ENDIF
     **
     C                   IF        NOBILL = 'X'                                 =remove from approva
     C                             AND BBAAFL = ' '                              if no charges
     C                   MOVEL     'G'           BBTRAN
     C                   ENDIF
     **
     C                   IF        BBPROC = *BLANKS                             =not processed
     **
     C                   IF        NOBILL = 'X'
     C                   EVAL      BBTYPE = *BLANKS
     C                   ENDIF
     **
     C                   IF        BBTYPE <> *BLANKS                            =electronic
     C                   MOVE      'Y'           BBELCT                         =flag as electronic
     C                   ENDIF
     **
     C                   IF        BBTRAN = 'D'
     C                   MOVEL     'M'           BBTRAN
     C                   ENDIF
     **
     C                   IF        REQLV6 = 0                                   =corp level
     C                   MOVE      'C'           BBPROC                         =flag as corp proces
     C                   ELSE                                                   =site level
     C                   MOVE      'R'           BBPROC                         =flag as site proces
     C                   ENDIF                                                  =processed flag chec
     **
     C                   IF        RTNFLG = 'Y'
     C                   EVAL      GTBEDP = PAGENO                              =end page
     C                   MOVEL     BBTRAK        GTBITN                         =bill traking #
     c                   eval      gtbmbr = prmmbr
     c                   eval      gtbfle = prmfle
     C                   WRITE     HBFGTOB
     C                   ENDIF                                                  =processed flag chec
     **
     *----------------------------------------------------
     ** fill in bbtorb if blank or x:
     C                   IF        BBTORB = *BLANKS
     C                             OR BBTORB = 'X'
     *
     C                   IF        BBIEFF > BGADDT
     C                   EVAL      FRSTDT = BBIEFF
     C                   ELSE
     C                   EVAL      FRSTDT = BGADDT
     C                   ENDIF
     *
     C                   IF        BBIEDT < BGDSDT
     C                             OR BGDSDT = 0
     C                   EVAL      LASTDT = BBIEDT
     C                   ELSE
     C                   EVAL      LASTDT = BGDSDT
     C                   ENDIF
     *
     C                   SELECT
     C                   WHEN      BBBSTS = 'N'
     C                   MOVE      '0'           BBTORB
     C                   WHEN      BBRBMT = '2'
     C                             OR BBRBMT = '4'
     C                             OR BBRBMT = '7'
     C                   MOVE      '5'           BBTORB
     C                   WHEN      BBTORB = 'X'
     C                   MOVE      '7'           BBTORB
     C                   WHEN      FRSTDT >= BBFRDT
     C                             AND LASTDT <= BBTODT
     C                   MOVE      '1'           BBTORB
     C                   WHEN      FRSTDT >= BBFRDT
     C                             AND LASTDT > BBTODT
     C                   MOVE      '2'           BBTORB
     C                   WHEN      FRSTDT < BBFRDT
     C                             AND LASTDT > BBTODT
     C                   MOVE      '3'           BBTORB
     C                   WHEN      FRSTDT < BBFRDT
     C                             AND LASTDT <= BBTODT
     C                   MOVE      '4'           BBTORB
     C                   OTHER
     C                   MOVE      '7'           BBTORB
     C                   ENDSL
     *
     C                   ENDIF
     *----------------------------------------------------
     **
     C                   WRITE     HBFBILLH                                     =write to history
     C                   WRITE     HBFBILLS                                     =write to save file
     C                   DELETE    HBFBILL                                      =delete bill process
     **
     C                   ELSE                                                   =already processed
     **
     C                   IF        *INU1 = *ON
     C                             AND *INU7 = *ON
     C                   MOVE      'X'           BBSCRB
     C                   ENDIF
     **
     C                   IF        *INU8 = *ON                                  =electronic run
     C                   MOVE      'Y'           BBELCT                         =flag as electronic
     C                   ENDIF
     **
     C                   IF        *INU7 = *OFF                                 =not a scrub
     C                             AND BBTYPE = *BLANKS
     C                             OR *INU8 = *ON
     C                   IF        BBAAFL <> *BLANKS                            =approval not needed
     C                   MOVEL     '*SYSTEM'     BBTSNM
     C                   ELSE
     C                   MOVEL     LDAUSR        BBTSNM                         =tran setup user
     C                   ENDIF
     C                   TIME                    TMPTIM
     C                   MOVEL     TMPTIM        BBTSTM                         =tran setup time
     C                   EVAL      MDATE = *DATE
     C                   EXSR      SRCMDY
     C                   EVAL      BBTSDT = YDATE                               =tran setup date
     C                   ENDIF
     **
     C                   IF        RTNFLG = 'Y'
     C                             AND *INU8 = *OFF
     C                   CALL      'HBXDGTOB'
     C                   PARM                    BBTRAK
     C                   EVAL      GTBEDP = PAGENO                              =end page
     C                   MOVEL     BBTRAK        GTBITN                         =bill traking #
     c                   eval      gtbmbr = prmmbr
     c                   eval      gtbfle = prmfle
     C                   WRITE     HBFGTOB
     C                   ENDIF
     **
     C                   UPDATE    HBFBILL                                      =update processed fl
     **
     C                   ENDIF
     **
     C                   IF        BILPGS <> 0
     C                   EXCEPT    BILTOT
     C                   EVAL      BALTOT = CHGTOT - CLITOT
     C                   EVAL      PAYBA2 = PAYBA2 + BALTOT
     C                   EVAL      PLNBA2 = PLNBA2 + BALTOT
     C                   EVAL      ACCBA2 = ACCBA2 + BALTOT
     C                   EVAL      LV6BA2 = LV6BA2 + BALTOT
     C                   EVAL      RPTBA2 = RPTBA2 + BALTOT
     C                   EXCEPT    BILTO2
     C                   EVAL      CLITOT = 0
     C                   EVAL      BALTOT = 0
     C                   EVAL      CHGTOT = 0
     **
     C                   MOVE      *ON           *IN47
     C                   ENDIF
     *****************************************************************
     C                   if        pageno > 9900
     c                   eval      MaxPageReached = *on
     c                   endif
      **
     c                   if        bilexc941 = *off
     c     bbtrak        chain     e5f1500                            79
     c                   if        *in79 = *off
     c                   eval      ediag = sdiag
     c                   update    e5f1500
     c                   endif
     c                   endif

     c                   IF        MaxPageReached
     c                   if        ovrlay = 'A' or ovrlay = 'Y' and
     c                             %open(hbr1500c)
     c                   close     hbr1500c
     c                   endif
     c                   if        ovrlay <> 'Y'
     c                   if        %open(hbr1500cn)
     c                   close     hbr1500cn
     c                   endif
     c                   endif
     c                   if        crtspl = 'A' or crtspl = 'Y'
     c                   call      'HXCARCH'
     c                   parm                    prmmbr
     c                   parm                    prmfle
     c                   parm                    prmspl
     c                   endif
     c                   eval      crtspl = 'N'
     c                   eval      once = ' '
     c                   ENDIF

     C     SKIP          TAG
     *****************************************************************
     CL1                 IF        ACCPGS <> 0
     CL1                 EXCEPT    ACCTOT
     CL1                 EXCEPT    ACCTO2
     CL1                 EVAL      ACCPG2 = 0
     CL1                 EVAL      ACCBA2 = 0
     CL1                 EVAL      ACCCHG = 0
     CL1                 EVAL      ACCPAT = 0
     CL1                 EVAL      ACCDUE = 0
     CL1                 EVAL      ACCPGS = 0
     CL1                 ENDIF
     *****************************************************************
     CL3                 IF        PLNPGS <> 0
     CL3                 EXCEPT    PLNTOT
     CL3                 EXCEPT    PLNTO2
     CL3                 EVAL      PLNPG2 = 0
     CL3                 EVAL      PLNBA2 = 0
     CL3                 EVAL      PLNCHG = 0
     CL3                 EVAL      PLNPAT = 0
     CL3                 EVAL      PLNDUE = 0
     CL3                 EVAL      PLNPGS = 0
     CL3                 ENDIF
     *****************************************************************
     CL4                 IF        PAYPGS <> 0
     CL4                 EXCEPT    PAYTOT
     CL4                 EXCEPT    PAYTO2
     CL4                 EVAL      PAYBA2 = 0
     CL4                 EVAL      PAYPG2 = 0
     CL4                 EVAL      PAYCHG = 0
     CL4                 EVAL      PAYPAT = 0
     CL4                 EVAL      PAYDUE = 0
     CL4                 EVAL      PAYPGS = 0
     CL4                 ENDIF
     *****************************************************************
     CL5                 IF        LV6PGS <> 0
     CL5                 EXCEPT    LV6TOT
     CL5                 EXCEPT    LV6TO2
     CL5                 EVAL      LV6BA2 = 0
     CL5                 EVAL      LV6PG2 = 0
     CL5                 EVAL      LV6CHG = 0
     CL5                 EVAL      LV6PAT = 0
     CL5                 EVAL      LV6DUE = 0
     CL5                 EVAL      LV6PGS = 0
     CL5                 ENDIF
     *****************************************************************
     CLR                 IF        OPEN = 'Y'
     CLR                 if        ovrlay = 'A' or ovrlay = 'Y' and
     C                             %open(hbr1500c)
     CLR                 close     HBR1500C
     CLR                 endif
     CLR                 if        ovrlay <> 'Y'
     CLR                 if        %open(hbr1500cn)
     CLR                 CLOSE     HBR1500CN
     CLR                 endif
     CLR                 endif
     CLR                 EXCEPT    TOTALS
     CLR                 CLOSE     PRINTER                                      =close print
     CLR                 CLOSE     PRINTR2                                      =close print
     CLR                 MOVE      'N'           OPEN
     CLR                 ENDIF
      *****************************************************************
     c     fillelecdata  begsr
      **
     c                   eval      B5TRAK = bbtrak
     c                   eval      B501A  = box01a
     c                   eval      B502   = bgname
     c                   eval      mdate = prbirt
     c                   exsr      srcmdy
     c                   eval      B503A  = ydate
     c                   if        *in21 = *on
     c                   eval      B503BC = 'M'
     c                   else
     c                   eval      B503BC = 'F'
     c                   endif
     c                   eval      B504   = box04
     c                   eval      B505A  = bdpad1
     c                   eval      B505B  = bdpcty
     c                   eval      B505C  = bdpsta
     c                   eval      B505D  = bdpzp1
     c                   eval      B505E  = %char(bdphtl)
     c                   if        B505E  = '0'
     c                   eval      B505E  = *blanks
     c                   endif
     c                   eval      B506   = rlcode
     c****               if        box07a = 'SAME'
     c****               eval      B507A  = bdpad1
     c****               eval      B507B  = bdpcty
     c****               eval      B507C  = bdpsta
     c****               eval      B507D  = bdpzp1
     c****               eval      B507E  = %char(bdphtl)
     c****               else
     c                   eval      B507A  = box07a
     c                   eval      B507B  = box07b
     c                   eval      B507C  = box07c
     c                   eval      B507D  = box07d
     c                   eval      B507E  = %char(box07e)
     c                   if        B507E  = '0'
     c                   eval      B507E  = *blanks
     c                   endif
     c****               endif
     c                   eval      B508A  = bdpmst
     c                   eval      B508B  = bgpesi
     c                   eval      B509   = prisd2
     c                   eval      B509A  = prtpol
     c                   eval      B509B1 = sb2bdt
     c                   eval      B509C  = *blanks                             =RES no use box 9C
     c                   eval      B509D  = prinm2
     c                   if        box10ay = 'X'
     c                   eval      B510A  = 'Y'
     c                   else
     c                   eval      B510A  = 'N'
     c                   endif
     c                   if        box10by = 'X'
     c                   eval      B510B1 = 'Y'
     c                   else
     c                   eval      B510B1 = 'N'
     c                   endif
     c                   eval      B510B2 = bgmvas
     c                   if        box10cy = 'X'
     c                   eval      B510C  = 'Y'
     c                   else
     c                   eval      B510C  = 'N'
     c                   endif
     c                   eval      B510D  = box10d
     c                   eval      B511   = prgrpx
     c                   eval      B511A1 = sbbdte
     c                   if        box11am= 'X'
     c                   eval      B511A2 = 'M'
     c                   else
     c                   eval      B511A2 = 'F'
     c                   endif
     c                   eval      B511B  = box11b
     c                   eval      B511C  = box11c
     c                   eval      B514   = injdte
     c                   eval      B515   = 0
     c                   eval      B516A  = 0
     c                   eval      B516B  = 0
     c                   eval      B517   = prrphy
     c                   eval      B517A  = prrupn
     c                   eval      B517B  = prrnpi
     c                   eval      B518A  = 0
     c                   eval      B518B  = 0
     c                   eval      B519   = box19
     c                   eval      %subst(B5FUT1 : 11 : 80) = box19x
     c                   if        box20y = 'X'
     c                   eval      B520   = 'Y'
     c                   else
     c                   eval      B520   = 'N'
     c                   endif
     c                   eval      B520B  = 0
     c****               eval      B521A  = dxc(1)
     c****               eval      B521B  = dxc(2)
     c****               eval      B521C  = dxc(3)
     c****               eval      B521D  = dxc(4)
     c                   eval      B521A  = ediag(1)
     c                   eval      B521B  = ediag(2)
     c                   eval      B521C  = ediag(3)
     c                   eval      B521D  = ediag(4)
     c                   eval      B522A  = *blanks
     c                   eval      B522B  = *blanks
     c                   eval      B523   = prauth
     c                   eval      B5251  = hx6tid
     c                   eval      B5252  = box25e
     c                   eval      B526   = box26
     c                   if        box27y = 'X'
     c                   eval      B527   = 'Y'
     c                   else
     c                   eval      B527   = 'Y'
     c                   endif
     c                   eval      B528   = totchg
     c                   eval      B529   = totpmt
     c                   eval      B530   = baldue
     c                   if        physname <> *blanks
     c                   eval      B531A = physname
     c                   else
     c                   eval      B531A  = *blanks
     c                   endif
     c                   eval      mdate = rptdte
     c                   exsr      srcmdy
     c                   eval      B531B  = ydate
     c                   eval      B532A1 = box321
     c                   eval      B532A2 = box322
     c                   eval      B532A3 = box323
     c                   eval      B532A4 = box324
     c                   MOVEL     BOX325        B532A5
     c                   MOVE      BOX325        B532A6
     c                   eval      B532A  = box32a
     c                   eval      B532B  = box32b

     c                   if        bilexc935
     c                   eval      B533   = wx33tl
     c                   eval      B533A1 = wx33nm
     c                   eval      B533A2 = wx33a1
     c                   eval      B533A3 = wx33ct
     c                   eval      B533A4 = wx33st
     c                   eval      B533A5 = wx33zp
     c                   eval      %subst(b5fut1 : 1 : 10) = wx33zp
     c                   else
     c                   eval      B533   = %char(lvl2tl)
     c                   eval      B533A1 = lvl2nm
     c                   eval      B533A2 = lvl2ad
     c                   eval      B533A3 = lvl2ct
     c                   eval      B533A4 = lvl2st
     c                   eval      B533A5 = lvl2zp
     c                   eval      %subst(b5fut1 : 1 : 10) = lvl2zp
     c                   endif
     c                   eval      B533A  = box33a
     c                   eval      B533B  = prtprv

     c                   eval      B5FFTL = wxfftl
     c                   eval      B5FFNM = wxffnm
     c                   eval      B5FFA1 = wxffa1
     c                   eval      B5FFA2 = wxffa2
     c                   eval      B5FFCT = wxffct
     c                   eval      B5FFST = wxffst
     c                   eval      B5FFZP = wxffzp

      *** Get diagnosis codes
     c                   if        bilexc941 = *off
     c                   callp     xfxdiag( bbplv6 : bbaccn : bbtodt : diag
     c                                    : poaf   : bodyp   : vers  )

     c                   eval      bdia01 = dxs(1)
     c                   eval      bdia02 = dxs(2)
     c                   eval      bdia03 = dxs(3)
     c                   eval      bdia04 = dxs(4)
     c                   eval      bdia05 = dxs(5)
     c                   eval      bdia06 = dxs(6)
     c                   eval      bdia07 = dxs(7)
     c                   eval      bdia08 = dxs(8)
     c                   eval      bdia09 = dxs(9)
     c                   eval      bdia10 = dxs(10)
     c                   eval      bdia11 = dxs(11)
     c                   eval      bdia12 = dxs(12)
     c                   endif

     c                   eval      b21icd = icdind

     c                   if        bilexc861
     c                   eval      B5FFL1 = 'Y'
     c                   endif

     c                   write     e5f1500

     c                   endsr
     c**********************************************************************
     c     fillediag     begsr
      **   ========      =====

     c                   eval      bgdptr = *blanks
     c                   eval      bgdpt2 = *blanks
     c                   eval      bgdpt3 = *blanks
     c                   eval      bgdpt4 = *blanks

     c                   eval      diaglookup = bgdicd

B001 c     1             do        12            dc
     c     diaglookup    lookup    sdiag                                  72
B002 c                   if        *in72 = *off
B003 c                   if        sdiag(dc) = *blanks
     c                   movel     diaglookup    sdiag(dc)
E003 c                   endif
E002 c                   endif
E001 c                   enddo
      **
     c                   select
     c                   when      diaglookup = sdiag(1)
     c                   movel     '1'           bgdptr
     c                   when      diaglookup = sdiag(2)
     c                   movel     '2'           bgdptr
     c                   when      diaglookup = sdiag(3)
     c                   movel     '3'           bgdptr
     c                   when      diaglookup = sdiag(4)
     c                   movel     '4'           bgdptr
     c                   when      diaglookup = sdiag(5)
     c                   movel     '5'           bgdptr
     c                   when      diaglookup = sdiag(6)
     c                   movel     '6'           bgdptr
     c                   when      diaglookup = sdiag(7)
     c                   movel     '7'           bgdptr
     c                   when      diaglookup = sdiag(8)
     c                   movel     '8'           bgdptr
     c                   when      diaglookup = sdiag(9)
     c                   movel     '9'           bgdptr
     c                   when      diaglookup = sdiag(10)
     c                   movel     '10'          bgdptr
     c                   when      diaglookup = sdiag(11)
     c                   movel     '11'          bgdptr
     c                   when      diaglookup = sdiag(12)
     c                   movel     '12'          bgdptr
     c                   endsl

     c                   exsr      multidx
     c                   if        prvvar <> *blanks
     c                   for       dc = 1 to 12
     c                   if        dxc(dc) <> *blanks and
     c                             %lookup(dxc(dc) : sdiag) = 0
     c                   eval      sdiag(dc) = dxc(dc)
     c                   endif
     c                   endfor
     c                   eval      bgdptr = %subst(mltdix : 1 : 1)
     c                   eval      bgdpt2 = %subst(mltdix : 2 : 1)
     c                   eval      bgdpt3 = %subst(mltdix : 3 : 1)
     c                   eval      bgdpt4 = %subst(mltdix : 4 : 1)

     c                   endif

     c                   endsr
      **********************************************************************
      ** Bill Exception 941 - Fill electronic diagnosis pointers
      **********************************************************************
      /free
        begsr ex941pointer;

          ebillptr = *blanks;
          chrgdxs = *blanks;

          chain cdscKey HBFCHRG;
          if %found(HBPCHRG) and BGCTQT <> 0;

            setll chgdxKey HBFCHGDX;
            dou %eof(HBPCHGDX);
              reade chgdxKey HBFCHGDX;
              if not %eof(HBPCHGDX);

                if %lookup(BDXICD : chrgdxs) = 0;
                  ix = %lookup(*blanks : chrgdxs : 1);
                  if ix <> 0;
                    chrgdxs(ix) = BDXICD;
                  endif;
                endif;

              endif;
            enddo;

            for xx = 1 to 4;
              if chrgdxs(xx) <> *blanks;
                dc = %lookup(chrgdxs(xx) : ediag);
                if dc <> 0;
                  dix(z) = %trim(dix(z)) + %subst(alphabet : dc : 1);
                  xy = %lookup(*blanks : ebillptr);
                  ebillptr(xy) = %char(dc);
                endif;
              endif;
            endfor;
          endif;

        endsr;
      /end-free

      **********************************************************************
      ** SRSETUPBOX21 - Setup the diagnoses in box21 (DXV) from hblchg15 file
      **                also set up the 5010 diagnoses (EDIAG)
      **                DXV will have the . inserted by XFXDXPER
      **                EDIAG will be sent through to 5010
      **********************************************************************
      /free
        begsr srsetupbox21;

          dxv = *blanks;
          dxc = *blanks;
          ediag = *blanks;
          if icdVer = '09';
            icdind = '9';
          else;
            icdind = '0';
          endif;

          setll chgkey hbfchr15;
          dou %eof(hblchg15);
            reade chgkey hbfchr15;
            if not %eof(hblchg15);
              action = *blanks;
              exsr srcgbl;
              if action = 'N';
                iter;
              endif;

              chain cdsckey hbfcdgn;
              if not %found(hbpcdgn);
                bgcdg2 = *blanks;
                bgcdg3 = *blanks;
                bgcdg4 = *blanks;
              endif;
              exsr srsetupchrgdxs;

              for xx = 1 to 4;
                if chrgdxs(xx) <> *blanks and %lookup(chrgdxs(xx) : ediag) = 0;
                  dc = %lookup(*blanks : dxv);
                  if dc <> 0;
                    xfxdxper (chrgdxs(xx) : icdver : returndx);
                    dxc(dc) = chrgdxs(xx);
                    dxv(dc) = returndx;
                    ediag(dc) = chrgdxs(xx);
                  endif;
                endif;
              endfor;

            endif;
          enddo;
        endsr;
      /end-free

      **********************************************************************
      ** Bill Exception 941 - Build Box 21 from HBPCHGDX (WebPT Interface)
      **********************************************************************
      /free
        begsr ex941box21;

          chrgdxs = *blanks;
          dxv = *blanks;
          dxc = *blanks;
          ediag = *blanks;
          if icdVer = '09';
            icdind = '9';
          else;
            icdind = '0';
          endif;

          setll BBTRAK HBFCHGBL;
          dou %eof(HBPCHGBL);
            reade BBTRAK HBFCHGBL;
            if not %eof(HBPCHGBL);

              chain chrgKey2 HBFCHRG;
              if %found(HBPCHRG) and BGCTQT <> 0;

                setll chgdxKey HBFCHGDX;
                dou %eof(HBPCHGDX);
                  reade chgdxKey HBFCHGDX;
                  if not %eof(HBPCHGDX);

                    if %lookup(BDXICD : chrgdxs) = 0;
                      ix = %lookup(*blanks : chrgdxs : 1);
                      if ix <> 0;
                        chrgdxs(ix) = BDXICD;
                      endif;
                    endif;

                  endif;
                enddo;

                for xx = 1 to 12;
                if chrgdxs(xx) <> *blanks and %lookup(chrgdxs(xx) : ediag) = 0;
                  dc = %lookup(*blanks : dxv);
                  if dc <> 0;
                    xfxdxper (chrgdxs(xx) : icdver : returndx);
                    dxc(dc) = chrgdxs(xx);
                    dxv(dc) = returndx;
                    ediag(dc) = chrgdxs(xx);
                  endif;
                endif;
                endfor;

              endif;

            endif;
          enddo;

        endsr;
      /end-free

      **------------------------------------------------------------------------
      // FILLPOINTER - fill in the diagnosis pointers for paper (A-L) and 5010 (1-12)
      //  The z variable is the current line it is working on...
      **------------------------------------------------------------------------
      /free
        begsr fillpointer;

          if mltdix = *blanks;
            ebillptr = *blanks;
            chain cdsckey hbfcdgn;
            if not %found(hbpcdgn);
              bgcdg2 = *blanks;
              bgcdg3 = *blanks;
              bgcdg4 = *blanks;
              bgcdg5 = *blanks;
              bgcdg6 = *blanks;
              bgcdg7 = *blanks;
              bgcdg8 = *blanks;
              bgcdg9 = *blanks;
              bgcdg10 = *blanks;
              bgcdg11 = *blanks;
              bgcdg12 = *blanks;
            endif;
            exsr srsetupchrgdxs;
          endif;

         //Set up the diagnoses and the diagnosis pointers (1-12 for 5010, A-L for 02-12)
          for xx = 1 to 4;
            if chrgdxs(xx) <> *blanks;
              dc = %lookup(chrgdxs(xx) : sdiag);                   //Find same diagnosis to point to
              dix(z) = %trim(dix(z)) + %subst(alphabet : dc : 1);  //Fill in A-L for 1500 (02-12)
              if mltdix  = *blanks;
                xy = %lookup(*blanks : ebillptr);                  //Find blank position in 5010 ptr
                ebillptr(xy) = %char(dc);                          //Fill in 1-12 for 5010 billing
              endif;
            endif;
          endfor;
        endsr;
      /end-free

      **********************************************************************
      ** Bill Exception 941 - Fill diagnosis pointers
      **********************************************************************
      /free
        begsr ex941ediag;

          ebillptr = *blanks;
          chrgdxs = *blanks;

          chain cdscKey HBFCHRG;
          if %found(HBPCHRG) and BGCTQT <> 0;

            setll chgdxKey HBFCHGDX;
            dou %eof(HBPCHGDX);
              reade chgdxKey HBFCHGDX;
              if not %eof(HBPCHGDX);

                if %lookup(BDXICD : chrgdxs) = 0;
                  ix = %lookup(*blanks : chrgdxs : 1);
                  if ix <> 0;
                    chrgdxs(ix) = BDXICD;
                  endif;
                endif;

              endif;
            enddo;

            for xx = 1 to 4;
              if chrgdxs(xx) <> *blanks;
                dc = %lookup(chrgdxs(xx) : ediag);
                xy = %lookup(*blanks : ebillptr);
                ebillptr(xy) = %char(dc);
              endif;
            endfor;
          endif;

        endsr;
      /end-free

      **------------------------------------------------------------------------
      // SRSETUPCHRGDXS - setup the chrgdxs array
      **------------------------------------------------------------------------
      /free
        begsr srsetupchrgdxs;
          chrgdxs(1) = bgdicd;
          chrgdxs(2) = bgcdg2;
          chrgdxs(3) = bgcdg3;
          chrgdxs(4) = bgcdg4;
          chrgdxs(5) = bgcdg5;
          chrgdxs(6) = bgcdg6;
          chrgdxs(7) = bgcdg7;
          chrgdxs(8) = bgcdg8;
          chrgdxs(9) = bgcdg9;
          chrgdxs(10) = bgcdg10;
          chrgdxs(11) = bgcdg11;
          chrgdxs(12) = bgcdg12;
        endsr;
      /end-free
     *****************************************************************
     **   determine if there are any bill form printing exceptions  **
     *****************************************************************
     **
     **  codedescription
     **
     **    01   print social security number in box 1a
     **    02   print employer name in box 4
     **    03   print employer address, city, state, zip and phone #
     **         in boxes 7 a,b,c,d,e
     **    04   initialize detail"to date" from detail "from date"
     **    05   print category two with license number
     **    06   print level 6 address in box 32
     **    07   print eight digit dates - detail lines
     **    08   print diags on detail lines, box 21 blank
     **    09   print "pfth" in box 11c
     **    10   do not print payments in box 29
     **    11   print staff member provider number in box 24k
     **    12   print staff member provider number in box 33 pin#
     **    13   print location provider number box 33 group#
     **    14   print payor resource number in box 17
     **    15   print tax-id in box24k
     **    16   print staff member upin number in box 24k
     **    17   print staff member medicaid license in box 24k
     **    18   print default bc/bs license code in box 24k
     **
     **    19   print default health plus license in box 24k
     **    20   always print 'x' in box 20 outside lab
     **    21   print six digit birth date (*in62)
     **    22   print six digit subscriber birth date (*in63)
     **    23   print six digit subscriber 2 birth date (*in64)
     **niu 24   print six digit admit date (*in65)
     **    24   print eight digit admit date (*in65)
     **niu 25   print six digit diagnosis date (*in66)
     **    25   print eight digit diagnosis date (*in66)
     **niu 26   print six digit report/bill date (*in67)
     **    26   print eight digit report/bill date (*in67)
     **    27   print alternate # of detail lines (not 6)
     **         (# of lines is in box # field)
     **    28   print total charges on each page for non-primary
     **         bills
     **    31   don't print detail on charges that have been paid
     **         (*in69)
     **    34   print only the last 8 digits of the bill tracking
     **         number
     **    48   print separate bill for each diagnosis code
     **    59   summarize charges by proc for consecutive dates of service
     **    61   prints detail dates as mm dd yy
     **    62   stops heading printing (primary, secondary etc as well
     **          initial, rebill etc)
     **    63   print case manager id number in box 17a and name in 17
     **    64   print clinic provider# in box 24k
     **    65   print resource field in box 26
     **    68   indiana waiver exception (*in30): do not print boxes
     **         9, 10abc, 12, 14, 15, 16, 18, 20, 22, 23, 25, 27, 29, 32
     **    69   end of month or discharge date as service to date
     **    70   spend down date as service from date
     **    71   deduct client responsibility from billed amount
     **    72   print diag code in both box 21 & box 24e
     **    73   print account# rather than bill tarcking # in box26
     **    74   don't print box12
     **    75   use bill from date for detail from date
     **    76   don't print box27
     **    77   don't print box32
     **    78   don't print box 25 ssn/ein
     **    79   procedure provider number in pin# of box33
     **   110   don't print box 1
     **   111   don't print box 1a
     **   112   don't print box 3
     **   113   don't print box 5
     **   114   don't print box 6
     **   115   don't print box 8
     **   116   don't print box 10
     **   117   don't print box 14
     **   118   don't print box 23
     **   119   don't print box 25
     **   120   don't print box 30
     **   121   print policy number in box 9a
     **   123   don't print box 21
     **   124   don't print box 31
     **   125   use first of month for detail from date
     **   126   don't print box 10d
     **   135   print charge staff member medicaid license in pin#
     **   142   break print code into 5 char cpt and 2 char modifier
     **   147   put overriding provider number in pin#
     **   148   override remit to address with pyr/pln variable
     **   149   do not print box 5 telephone number
     **   150   do not print box 7 telephone number
     **   151   print payor/plan address in box 33
     **   152   do not print information in boxes 11 - 11d
     **   153   do not print information in boxes 09 - 09d
     **   183   print alternate account number in box 26
     **   184   print program code as part of account in box 26
     **   185   print alternate insurance information on top of bill
     **   186   print alternate facility information in box 32
     **   187   print alternate pin # in box 33
     **   188   print alternate group number in box 33
     **   189   print alternate supplier's billing information
     **   190   print alternate tax id
     **   191   force 11d to always be "no"
     **   194   don't check off sex code box in box 11a - leave blank
     **   195   print alternate taxid in boxes 25 & 26
     **   196   print auth # from ivsvc in box 23
     **   200   print referring physician name in box 31
     **   214   print quantity 1 for included procedures
     **   218   dont print address in top corner of bill
     **   219   print icd9 witout description in box 21
     **   220   force box 27 accept assignment to "no"
     **   221   don't print payor/plan in top corner of bill
     **   222   calculate from date of service as the first day of
     **         the month based on the to date of service
     **   223   from detail date based on original bill from date
     **   225   add attention line from payor/plan to top of bill
     **   227   print overrid prov # (opvdr) in box 24k
     **   228   convert days to 1/4 hour units
     **   317   use 4 digit qty field in box24g
     **   325   always check medicaid in box 1
     **   326   blank out box 4 subscriber name
     **   327   blank out box 7 subscriber address
     **   328   print client/patient name and address in box 32
     **   348   include billed payors' payments in box29
     **   349   print "signature on file" in box31
     **   350   print "whn" in box17
     **   355   print capped charge cpt4 code
     **   358   print ref phy Medicaid license in box 19
     **   363   print capped charge cpt4 code
     **   364   round qty after summarization
     **   366   print quantity with two decimal positions
     **   399   print ref phy upin box17a
     **   400   print provider number in box32b
     **   401   print box 33b
     **   417   print alternate NPI in box 33A
     **   418   print level 6 provider number in box 24j-a
     **   419   print overriding provider number in box 24J-a
     **   420   print print alternate group in box 24j-a
     **   421   print payor/plan group number in box 24j-a
     **   422   print alternate provider number in box 33B
     **   423   print alternate group number in box 24J
     **   425   clear field 32a
     **   426   clear field 33a
      **   444   print site NPI in box 17b
      **   452   print qty in hrs for hourly reimb type
     **   456   procedure provider number in box32b
      **   463   print ROUNDED qty in hrs for hourly reimb type
      **   466   print Taxon Variable in box 33B
      **   470   print TAXON Variable in box 24I,J-a
     **   480   print NPI in box 24J
     **   510   print by service date instead of ICD9 code / service date
     **   516   print ALTNP in box 24Jb(bottom)
     **   543   print PRVDR in box 32A and 33A
     **   545   print level 6 street address in box 32
     **   628   print SSN in box 23 (PRAUTH)
     **   660   print PRVDR Variable in box 33B
     **   677   print ICN number in box 22B
     **   690   print rendering provider taxonomy code
     **   814   print provider number in box32b without qualifier
     **   815   print provider number in box33b with qualifier
     **   817   print monthly sequence auth number in box 23 (PRAUTH)
     **   833   print 'ATYPICAL' in box 33A
     **   835   print aide name in box 31
      **   885   print TAXON provider variable in box 33B without qualifier
     **   916   print NPI provider variable in Box 32b w/o qualifier
     **   917   print NPI provider variable in Box 33b w/o qualifier
     **   922   print FALOC provider variable in Box 32b w/o qualifier
     **   935   pull PAYTO provider variable information
      **   940   print ROUNDED qty in hrs based on time of proc
     **
     *****************************************************************
     C     BILEXC        BEGSR
     **
     C                   EVAL      SVPAYR = EXPAYR
     C                   EVAL      SVPLAN = EXPLAN
     C                   MOVE      EXFC          SVFC
     C                   EVAL      SVLVL6 = EXLVL6
     C                   MOVEL     EXSTAT        SVSTAT
     **
     C                   MOVE      'N'           FOUND
     C                   EVAL      COUNT = 31
     **
     C     AGAIN         TAG
     **
     C     EXCPKY        CHAIN     HBFBFEX                            78
     C                   IF        *IN78 = *OFF
     C                             AND HEDELT = ' '
     **
     C                   MOVE      'Y'           FOUND
     C                   SELECT
     **
     C                   WHEN      EXCODE = 1
     C                   MOVEL(P)  BDPSS#        BOX01A
     **
     C                   WHEN      EXCODE = 2
     C     EMPKEY        CHAIN     XFFCDFLE                           77
     C                   IF        *IN77 = *OFF
     C                   MOVEL(P)  XFCDSC        BOX04
     C                   ENDIF
     **
     C                   WHEN      EXCODE = 3
     C     EMPKEY        CHAIN     XFFCDFLE                           77
     C                   IF        *IN77 = *OFF
     C                   MOVEL(P)  XFCAD1        BOX07A
     C                   MOVEL(P)  XFCCTY        BOX07B
     C                   MOVEL(P)  XFCSTA        BOX07C
     c**
     c                   if        xfczp2 <> *blanks
     c                   eval      box07d = xfczip + '-' + xfczp2
     c                   else
     c                   eval      box07d = xfczip + '-0000'
     c                   endif
     c**
     C                   EVAL      BOX07E = XFCBTL
     C                   ENDIF
     **
     C                   WHEN      EXCODE = 4
     C                   EVAL      BXODLT = *BLANKS
     C                   MOVE      'P'           OMTYPE                         =proc exclude
     C     EXOKEY        CHAIN     HBFBFXO                            76
     C                   IF        *IN76 = *ON
     C                             OR BXODLT = 'D'
     C                   MOVE      DTF(Z)        DTT(Z)
     C                   EVAL      PBT(Z) = PBF(Z)
     C                   MOVE      DTF6(Z)       DTT6(Z)
     C                   MOVE      DTF6(Z)       WKDAT6
     C                   MOVE      MM6           DTTM(Z)
     C                   MOVE      DD6           DTTD(Z)
     C                   MOVE      YY6           DTTY(Z)
     C                   ELSE
     C                   EVAL      YDATE = BBTODT
     C                   EXSR      SRCYMD
     C                   MOVE      MDATE         DTT(Z)
     C                   MOVEL     MDATE         MMDD6
     C                   MOVE      MDATE         YY6
     C                   MOVE      WKDAT6        DTT6(Z)
     C                   MOVE      MM6           DTTM(Z)
     C                   MOVE      DD6           DTTD(Z)
     C                   MOVE      YY6           DTTY(Z)
     C                   ENDIF
     **
     C                   WHEN      EXCODE = 5
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm      'STATE'       prvcod
     C                   parm                    qua33b
     C                   EVAL      PRPROV = qua33b +' '+ %TRIMR(THRCT2) + PRRG#
     **
     C                   WHEN      EXCODE = 6
     C                   MOVEL(P)  HX6A21        BOX321
     C                   MOVEL(P)  HX6A22        BOX322
     C                   MOVEL(P)  HX6CT2        BOX323
     C                   MOVEL(P)  HX6ST2        BOX324
     c
     c                   if        hx6z22 <> *blanks
     c                   eval      box325 = hx6z21 + '-' + hx6z22
     c                   else
     c                   eval      box325 = hx6z21 + '-0000'
     c                   endif
     c
     **
     C                   WHEN      EXCODE = 7
     C                   MOVE      *ON           *IN58
     **
     C                   WHEN      EXCODE = 8
     C                   MOVE      *ON           *IN59
     **
     C                   WHEN      EXCODE = 9
     C                   MOVEL(P)  'PFTH'        BOX11C
     **
     C                   WHEN      EXCODE = 10
     C                   MOVE      '1'           BX29F
     **
     C                   WHEN      EXCODE = 11
     C                   MOVE      *ON           *IN11
     C                   EVAL      LEVEL6 = BBPLV6
     C                   EVAL      THERAP = BGDTTH
     C     THPRK2        CHAIN     HMFPLNPR                           75
     C                   IF        *IN75 = *OFF
     C                   MOVEL     HMTPR#        PIN(Z)
     C                   ELSE
     C                   EVAL      LEVEL6 = 0
     C     THPRK2        CHAIN     HMFPLNPR                           75
     C                   IF        *IN75 = *OFF
     C                   MOVEL     HMTPR#        PIN(Z)
     C                   ENDIF
     C                   ENDIF
     c                   if        pin(z) <> *blanks
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm      'PRVDR'       PRVCOD
     C                   parm                    bx24i(z)
     c                   endif
     **
     C                   WHEN      EXCODE = 12
     C                   EVAL      LEVEL6 = BBPLV6
     C     THPRKY        CHAIN     HMFPLNPR                           77
     C                   IF        *IN77 = *ON
     C                   EVAL      LEVEL6 = 0
     C     THPRKY        CHAIN     HMFPLNPR                           77
     C                   IF        *IN77 = *ON
     C                   EVAL      HMTPR# = *BLANKS
     C                   ENDIF
     C                   ENDIF
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm      'PRVDR'       prvcod
     C                   parm                    qua33b
     C                   eval      prprov = qua33b + ' ' + hmtpr#
     **
     C                   WHEN      EXCODE = 14
     C                   MOVEL     PRRESC        PRRUPN
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm      'RESRC'       prvcod
     C                   parm                    qua17a
     C                   EVAL      PRRPHY = *BLANKS
     **
     C                   WHEN      EXCODE = 15
     C                   MOVEL(P)  HX6TID        PIN(Z)
     c                   if        pin(z) <> *blanks
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm      'TAXID'       licecd
     C                   parm                    bx24i(z)
     c                   endif
     **
     C                   WHEN      EXCODE = 16
     C                   EVAL      PIN(Z) = *BLANKS
     c                   eval      bx24i(z) = *blanks
     C                   EVAL      LDOC# = BGDTTH
     c                   eval      lcode = *blanks
     c                   movel     'UPIN'        lcode
     C     LICEKY        CHAIN     HMFLICE2                           76
     C                   IF        *IN76 = *OFF
     C                   MOVEL     HLCNUM        PIN(Z)
     c                   if        pin(z) <> *blanks
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm                    lcode
     C                   parm                    bx24i(z)
     c                   endif
     C                   ENDIF
     **
     C                   WHEN      EXCODE = 17
     C                   EVAL      PIN(Z) = *BLANKS
     c                   eval      bx24i(z) = *blanks
     C                   MOVEL     'MCAID'       LICECD
     C     LICKY2        SETLL     HMFLICE
     C                   DOU       *IN81 = *ON
     C     LICKY2        READE     HMFLICE                                81
     C                   IF        *IN81 = *OFF
     C                             AND HLCCT2 = BGDCT2
     C                             AND HLCNUM <> *BLANKS
     C                   MOVEL     HLCNUM        PIN(Z)
     c                   if        pin(z) <> *blanks
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm                    licecd
     C                   parm                    bx24i(z)
     c                   endif
     C                   ENDIF
     C                   ENDDO
     **
     C                   WHEN      EXCODE = 18
     C                   EVAL      PIN(Z) = *BLANKS
     c                   eval      bx24i(z) = *blanks
     C                   EXSR      SRCTAB
     C                   MOVEL     'BLIC'        TCODE
     C                   MOVEL     BGDCT1        ECODE
     C                   EXSR      SRTABL
     C                   MOVEL     LDESC         PIN(Z)
     c                   if        pin(z) <> *blanks
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm      'PRVDR'       prvcod
     C                   parm                    bx24i(z)
     c                   endif
     **
     C                   WHEN      EXCODE = 19
     C                   EVAL      PIN(Z) = *BLANKS
     c                   eval      bx24i(z) = *blanks
     C                   EXSR      SRCTAB
     C                   MOVEL     'BLI2'        TCODE
     C                   MOVEL     BGDCT1        ECODE
     C                   EXSR      SRTABL
     C                   MOVEL     LDESC         PIN(Z)
     c                   if        pin(z) <> *blanks
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm      'PRVDR'       prvcod
     C                   parm                    bx24i(z)
     c                   endif
     **
     C                   WHEN      EXCODE = 20
     C                   EVAL      BOX20Y = *BLANKS
     C                   MOVE      'X'           BOX20N
     **
     C                   WHEN      EXCODE = 21
     C                   MOVE      *ON           *IN62                          =6 digit
     **                                                    birth date
     C                   WHEN      EXCODE = 22
     C                   MOVE      *ON           *IN63                          =6 digit subscriber
     **                                                    birth date
     C                   WHEN      EXCODE = 23
     C                   MOVE      *ON           *IN64                          =6 digit subscriber
     **                                                    2 birth date
     C                   WHEN      EXCODE = 24
     C                   MOVE      *ON           *IN65                          =8 digit
     **                                                    admit date
     C                   WHEN      EXCODE = 25
     C                   MOVE      *ON           *IN66                          =8 digit
     **                                                    diagnosis date
     C                   WHEN      EXCODE = 26
     C                   MOVE      *ON           *IN67                          =8 digit
     **                                                    bill date
     C                   WHEN      EXCODE = 27
     C                   MOVE      EXBOX#        DTLINE                         =print alternate
     C                   IF        DTLINE <= 0                                   # of detail lines
     C                             OR DTLINE > 6
     C                   EVAL      DTLINE = 6
     C                   ENDIF
     **
     C                   WHEN      EXCODE = 28
     C                   MOVE      *ON           *IN68                          =print total
     **                                                    charges
     C                   WHEN      EXCODE = 31
     C                   MOVE      *ON           *IN69                          =print detail
     **
     C                   WHEN      EXCODE = 34
     C                   MOVE      BBTRAK        EIGHT
     C                   MOVEL(P)  EIGHT         BOX26
     **
     C                   WHEN      EXCODE = 35
     C                   MOVE      *OFF          *IN24
     C                   MOVE      'X'           BOX11DN
     **
     C                   WHEN      EXCODE = 38                                  =don't print
     C                   EVAL      BOX04 = *BLANKS                               box04
     C                   EVAL      BOX07A = *BLANKS                              box07      box10d
     C                   EVAL      BOX07B = *BLANKS                                         box10d
     C                   EVAL      BOX07C = *BLANKS                                         box10d
     C                   EVAL      BOX07D = *BLANKS                                         box10d
     C                   EVAL      BOX07E = 0                                               box10d
     C                   MOVE      *ON           *IN10
     C                   EVAL      WKLVL6 = 0                                    box10d     box10d
     C                   EVAL      PRGRPX = *BLANKS
     C                   EVAL      SBBDTE = 0                                    box11a     box10d
     C                   EVAL      BOX11B = *BLANKS
     C                   eval      box11bq = *blanks
     C                   EVAL      BOX11C = *BLANKS                              box11c     box10d
     C                   EVAL      BOX11DY = *BLANKS                              box11c     box10d
     C                   EVAL      BOX11DN = *BLANKS                              box11c     box10d
     C                   EVAL      box13  = *BLANKS                              box13      box10d
     C                   eval      bilexc38 = *on
     **
     C                   WHEN      EXCODE = 39                                  =don't print
     C                   MOVE      *ON           *IN40                           box14
     C                   EVAL      BOX20Y = *BLANKS                              box20
     C                   EVAL      BOX20N = *BLANKS
     C                   MOVEL     'N'           PRTDXD                          box21
     **
     C                   WHEN      EXCODE = 40                                  =don't print
     **
     C                   WHEN      EXCODE = 41                                  =don't print
     C                   MOVEA     *BLANKS       DIX                             box24e
     C                   MOVEA     *BLANKS       DDX
     C                   MOVEL     'N'           PRTDXD
     **
     C                   WHEN      EXCODE = 42                                  =don't print
     C                   EVAL      PRINAM = *BLANKS                              info at top
     C                   EVAL      PRIADR = *BLANKS
     C                   EVAL      PRIAD2 = *BLANKS
     C                   EVAL      PRICTY = *BLANKS
     C                   EVAL      PRISTA = *BLANKS
     C                   EVAL      PRIZIP = *BLANKS
     C                   EVAL      WRKPYR = 0
     C                   EVAL      WRKPLN = 0
     C                   MOVE      *ON           *IN13
     **
     C                   WHEN      EXCODE = 48                                  =print one bill
     C                   MOVE      *ON           *IN14                           per diagnosis
     **
     C                   WHEN      EXCODE = 50
     C                             OR EXCODE = 59
     C                             OR EXCODE = 473
     C                             or excode = 611
     c                   eval      include = *on
     c                   if        excode = 611
     c                   eval      omtype = 'I'                                 Proc Include
     c     exokey        chain     hbfbfxo
     c                   if        not %found or bxodlt = 'D'
     c                   eval      include = *off
     c                   endif
     c                   endif
     c                   if        include
     C                   EVAL      YDATE = BGDFSD
     C                   EXSR      SRCYMD
     C                   MOVE      MDATE         DTT(Z)
     C                   MOVEL     MDATE         MMDD6
     C                   MOVE      MDATE         YY6
     C                   MOVE      WKDAT6        DTT6(Z)
     C                   MOVE      MM6           DTTM(Z)
     C                   MOVE      DD6           DTTD(Z)
     C                   MOVE      YY6           DTTY(Z)
     c                   endif
     **
     C                   WHEN      EXCODE = 60
     C                   MOVE      *ON           *IN37
     C                   eval      b060st = hestat
     C                   eval      b060l6 = helvl6
     C                   eval      b060fc = hefc
     C                   eval      b060py = hepayr
     C                   eval      b060pl = heplan
     C                   eval      b060cd = heexcd
     C                   eval      b060tp = 'P'
     **
     C                   WHEN      EXCODE = 61
     C                   MOVE      *ON           *IN60
     **
     C                   WHEN      EXCODE = 62
     C                   EVAL      INSBIL = *BLANKS
     C                   EVAL      TOBILL = *BLANKS
     **
     C                   WHEN      EXCODE = 63
     **
     C                   EVAL      PRRUPN = *BLANKS
     C                   EVAL      PRRPHY = *BLANKS
     C                   EVAL      qua17a = *BLANKS
     C                   MOVE      'CM'          STATUS
     C                   EXSR      SRASGN
     C                   IF        MSGDR# <> 0
     C     MSGDR#        CHAIN     HMFMAMS                            76
     C                   IF        *IN76 = *OFF
     c                   eval      box17q = 'DQ'
     C                   MOVEL     HMDNAM        PRRPHY                         =name
     C                   MOVEL     'MCAID'       LICECD
     C                   MOVEL     HX6ST1        WRKSTA
     C     LICKEY        CHAIN     HMFLICE                            76
     C                   IF        *IN76 = *OFF
     C                   MOVEL     HLCNUM        PRRUPN                         =upin #
     C                   MOVEL     'PRVDR'       LICECD                         =force new qual
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm                    licecd
     C                   parm                    qua17a
     C                   ELSE
     C                   EVAL      PRRUPN = *BLANKS
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     **
     C                   WHEN      EXCODE = 64
     c                   eval      prmprc = bgdtpr
 001 c                   eval      prvcod = 'PRVDR'
     C                   EXSR      SRPRV
 001 c     prvvar        ifne      *blanks
 001 c                   movel     prvvar        pin(z)
     c                   if        pin(z) <> *blanks
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm                    prvcod
     C                   parm                    bx24i(z)
     c                   endif
     c                   else
     c                   eval      pin(z) = *blanks
     c                   eval      bx24i(z) = *blanks
     c                   endif
      **
     C                   WHEN      EXCODE = 65
     C                   MOVEL(P)  PRRESC        BOX26
     **
     C                   WHEN      EXCODE = 68
     C                   MOVE      *ON           *IN30
     C                   MOVE      *ON           *IN28
     C                   MOVEL(P)  'NONE'        PRINN2
     C                   EVAL      PRADDT = 0
     C                   MOVE      *ON           *IN15
     C                   MOVE      ' '           BOX25E
     C                   MOVEL     ' '           BOX27Y
     C                   MOVEL     ' '           BOX27N
     C                   EVAL      BOX321 = *BLANKS
     C                   EVAL      BOX322 = *BLANKS
     C                   EVAL      BOX323 = *BLANKS
     C                   EVAL      BOX324 = *BLANKS
     C                   EVAL      BOX325 = *BLANKS
     **
     C                   WHEN      EXCODE = 69
     C                   EVAL      WRKYMD = BGDTSD                              =use eom date
     C                   EVAL      CMOYR = 0                                     for service date
     C                   MOVE      WRKYY         CMOYR                           unless discharged
     C                   MOVEL     WRKMM         CMOYR                           earlier, then
     C                   EXSR      GTEOM                                         use discharge date
     C                   EXSR      SRCMDY
     C                   IF        BGDSDT <> 0
     C                             AND BGDSDT < YDATE
     C                   EVAL      YDATE = BGDSDT
     C                   EXSR      SRCYMD
     C                   ENDIF
     C                   MOVE      MDATE         DTT(Z)
     C                   MOVEL     MDATE         MMDD6
     C                   MOVE      MDATE         YY6
     C                   MOVE      WKDAT6        DTT6(Z)
     C                   MOVE      MM6           DTTM(Z)
     C                   MOVE      DD6           DTTD(Z)
     C                   MOVE      YY6           DTTY(Z)
     **
     C                   WHEN      EXCODE = 70
     C                             OR EXCODE = 323
     C                   EXSR      GETSPD
     C                   EVAL      YDATE = SPDDAT
     C                   EXSR      SRCYMD
     C                   MOVE      MDATE         DTF(Z)
     C                   MOVEL     MDATE         MMDD6
     C                   MOVE      MDATE         YY6
     C                   MOVE      WKDAT6        DTF6(Z)
     C                   MOVE      MM6           DTFM(Z)
     C                   MOVE      DD6           DTFD(Z)
     C                   MOVE      YY6           DTFY(Z)
     **
     C                   WHEN      EXCODE = 71
     C                   EVAL      AMT(Z) = AMT(Z) - CHPRSP
     C                   EVAL      BBAMNT = BBAMNT - CHPRSP
     C                   EVAL      TOTPYR = TOTPYR - CHPRSP
     **
     C                   WHEN      EXCODE = 72
     C                   MOVE      *ON           *IN49
     **
     C                   WHEN      EXCODE = 73
     C                   MOVEL(P)  BBACCN        BOX26
     **
     C                   WHEN      EXCODE = 74
     C                   EVAL      box12 = *BLANKS                              box12a     box10d
     C                   EVAL      PRADDT = 0
     C                   MOVE      *ON           *IN15
     C                   MOVE      *OFF          *IN65                          =off 6 digit dates
     **
     C                   WHEN      EXCODE = 75
     C                   MOVE      'P'           OMTYPE
     C     EXOKEY        CHAIN     HBFBFXO                            76
     C                   IF        *IN76 = *ON
     C                             OR BXODLT = 'D'
     C                   EVAL      YDATE = BBFRDT
     C                   EXSR      SRCYMD
     C                   MOVE      MDATE         DTF(Z)
     C                   MOVEL     MDATE         MMDD6
     C                   MOVE      MDATE         YY6
     C                   MOVE      WKDAT6        DTF6(Z)
     C                   MOVE      MM6           DTFM(Z)
     C                   MOVE      DD6           DTFD(Z)
     C                   MOVE      YY6           DTFY(Z)
     C                   ENDIF
     **
     C                   WHEN      EXCODE = 76
     C                   MOVEL     ' '           BOX27Y
     C                   MOVEL     ' '           BOX27N
     **
     C                   WHEN      EXCODE = 77
     C                   EVAL      BOX321 = *BLANKS
     C                   EVAL      BOX322 = *BLANKS
     C                   EVAL      BOX323 = *BLANKS
     C                   EVAL      BOX324 = *BLANKS
     C                   EVAL      BOX325 = *BLANKS
     **
     C                   WHEN      EXCODE = 78
     C                   MOVE      ' '           BOX25E
     **
     C                   WHEN      EXCODE = 79
     c                   if        bgdprv <> *blanks
     c                   MOVEL(P)  BGDPRV        PRPROV
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm      'PRVDR'       prvcod
     C                   parm                    qua33b
     c     qua33b        cat       bgdprv:1      prprov
     c                   else
     c                   eval      prprov = *blanks
     c                   eval      qua33b = *blanks
     c                   endif
     **
     C                   WHEN      EXCODE = 110
     C                   MOVEA     '000'         *IN(34)
     **
     C                   WHEN      EXCODE = 111
     C                   EVAL      BOX01A = *BLANKS
     **
     C                   WHEN      EXCODE = 112
     C                   MOVE      *ON           *IN27
     C                   MOVEA     '00'          *IN(21)
     **
     C                   WHEN      EXCODE = 113
     C                   EVAL      BDPAD1 = *BLANKS
     C                   EVAL      BDPCTY = *BLANKS
     C                   EVAL      BDPSTA = *BLANKS
     C                   EVAL      BDPZP1 = *BLANKS
     C                   EVAL      BDPZP2 = *BLANKS
     C                   EVAL      BDPHTL = 0
     C                   MOVE      *ON           *IN09
     **
     C                   WHEN      EXCODE = 114
     C                   MOVEA     '0000'        *IN(41)
     **
     C                   WHEN      EXCODE = 115
     C                   MOVEA     '000'         *IN(31)
     C                   MOVEA     '00'          *IN(45)
     **
     C                   WHEN      EXCODE = 116
     C                   EVAL      BOX10AY = *BLANKS
     C                   EVAL      BOX10AN = *BLANKS
     C                   EVAL      BOX10BY = *BLANKS
     C                   EVAL      BOX10BN = *BLANKS
     C                   EVAL      BOX10CY = *BLANKS
     C                   EVAL      BOX10CN = *BLANKS
     C                   MOVE      *OFF          *IN26
     **
     C                   WHEN      EXCODE = 117
     C                   MOVE      *ON           *IN28
     **
     C                   WHEN      EXCODE = 118
     C                   EVAL      PRAUTH = *BLANKS
     C                   EVAL      BBAUTH = *BLANKS
     **
     C                   WHEN      EXCODE = 119
     C                   EVAL      HX6TID = *BLANKS
     C                   EVAL      BOX25E = *BLANKS
     **
     C                   WHEN      EXCODE = 120
     C                   MOVE      *ON           *IN29
     **
     C                   WHEN      EXCODE = 121
     C                   MOVEL     BBPLCY        PRTPOL
     C                   MOVE      *OFF          *IN24
     C                   MOVE      'X'           BOX11DN
     **
     C                   WHEN      EXCODE = 123
     C                   MOVE      *ON           *IN08
     **
     C                   WHEN      EXCODE = 124
     C                   MOVE      *ON           *IN25
     **
     C                   WHEN      EXCODE = 125
     C                   MOVE      'I'           OMTYPE                         =proc include
     C     EXOKEY        CHAIN     HBFBFXO                            76
     C                   IF        *IN76 = *OFF
     C                             AND BXODLT <> 'D'
     C                   EVAL      WRKYMD = BBFRDT
     C                   EVAL      WRKDD = 1
     C                   EVAL      YDATE = WRKYMD
     C                   EXSR      SRCYMD
     C                   MOVE      MDATE         DTF(Z)
     C                   MOVEL     MDATE         MMDD6
     C                   MOVE      MDATE         YY6
     C                   MOVE      WKDAT6        DTF6(Z)
     C                   MOVE      MM6           DTFM(Z)
     C                   MOVE      DD6           DTFD(Z)
     C                   MOVE      YY6           DTFY(Z)
     C                   ENDIF
     **
     C                   WHEN      EXCODE = 126
     C                   EVAL      WKLVL6 = 0
     **
     C                   WHEN      EXCODE = 135
     C                   MOVEL     'MCAID'       LICECD
     C     LICKY2        SETLL     HMFLICE
     C                   DOU       *IN81 = *ON
     C     LICKY2        READE     HMFLICE                                81
     C                   IF        *IN81 = *OFF
     C                             AND HLCCT2 = BGDCT2
     C                   MOVEL(P)  HLCNUM        PRPROV
     C                   MOVEL(P)  HLCNUM        BBPROV
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm                    licecd
     C                   parm                    qua33b
     c                   if        qua33b <> *blanks
     c                   eval      prprov = qua33b + ' ' + prprov
     c                   endif
     C                   ENDIF
     C                   ENDDO
     **
     C                   WHEN      EXCODE = 142
     C                   MOVEL     BGDTC4        CPT8
     C                   IF        CPT8 <> CPT(Z)
     C                   MOVEL     CPT(Z)        PRCMOD
     C                   MOVEL(P)  CPT142        CPT(Z)
     C                   EVAL      MOD(Z) = *BLANKS                             =if using ex cd, cle
     C                   MOVEL(P)  MOD142        MOD(Z)
     C                   ENDIF
     **
     C                   WHEN      EXCODE = 148
     c                   callp     XFXPRV( bbplv6  : bbaccn  : bbpayr : bbplan
     c                                   : 'CDDO ' : *blanks : 0      : bbtodt
     c                                   : wrkprv  : wrkadr )
     C                   MOVEL(P)  NAME          LVL2NM                         =name
     C                   MOVEL(P)  ADDR1         LVL2AD                         =address
     C                   MOVEL(P)  CITY          LVL2CT                         =city
     C                   MOVEL(P)  STATE         LVL2ST                         =state
     c**
     c                   if        zip2 <> *blanks
     c                   eval      lvl2zp = zip1 + '-' + zip2
     c                   else
     c                   eval      lvl2zp = zip1 + '-0000'
     c                   endif
     c**
     C                   WHEN      EXCODE = 149
     C                   EVAL      BDPHTL = 0
     C                   MOVE      *ON           *IN09
     **
     C                   WHEN      EXCODE = 150
     C                   EVAL      BOX07E = 0
     C                   MOVE      *ON           *IN10
     **
     C                   WHEN      EXCODE = 151
     C                   MOVEL     HLDNAM        LVL2NM                         =ins name
     C                   MOVEL     HLDADR        LVL2AD                         =ins address
     C                   MOVEL     HLDCTY        LVL2CT                         =ins city
     C                   MOVEL     HLDSTA        LVL2ST                         =ins state
     C                   MOVEL     HLDZIP        LVL2ZP                         =ins zip code
     C                   EVAL      LVL2TL = HLDTEL
     **
     C                   WHEN      EXCODE = 152
     C                   EVAL      PRGRPX = *BLANKS                              box11
     C                   EVAL      SBBDTE = 0                                    box11a     box10d
     C                   MOVE      ' '           box11am                         box11a
     C                   MOVE      ' '           box11af                         box11a
     C                   EVAL      BOX11B = *BLANKS                              box11b
     C                   EVAL      BOX11C = *BLANKS                              box11c     box10d
     C                   EVAL      BOX11DY = *BLANKS                              box11c     box10d
     C                   EVAL      BOX11DN = *BLANKS                              box11c     box10d
     C                   eval      bilexc152 = *on
     **
     C                   WHEN      EXCODE = 153
     C                   EVAL      PRISD2 = *BLANKS                              box09
     C                   EVAL      PRINN2 = *BLANKS                              box09a     box10d
     C                   EVAL      SB2BDT = 0                                    box09b     box10d
     C                   EVAL      PRINM2 = *BLANKS                              box09d     box10d
     **
     C                   WHEN      EXCODE = 183
     C                   EVAL      PRMPRC = *BLANKS
     C                   MOVEL     'ALTID'       PRVCOD
     C                   EXSR      SRPRV
     C                   IF        PRVVAR <> *BLANKS
     C                   MOVEL(P)  PRVVAR        BOX26
     C                   ENDIF
     **
     C                   WHEN      EXCODE = 184
     C                   EVAL      PRMPRC = *BLANKS
     C                   MOVEL     'PGMCD'       PRVCOD
     C                   EXSR      SRPRV
     C                   IF        PRVVAR <> *BLANKS
     C                   MOVEL     PRVVAR        SIXDIG
     C                   MOVE      SIXDIG        BOX26
     C                   ENDIF
     **
     C                   WHEN      EXCODE = 185
     c                   callp     XFXPRV( bbplv6  : bbaccn  : bbpayr : bbplan
     c                                   : 'ALTIN' : *blanks : 0      : bbtodt
     c                                   : wrkprv  : wrkadr )
     C                   MOVEL     WRKPRV        PRINAM
     C                   MOVEL(P)  ADDR1         PRIADR                         =address
     C                   MOVEL(P)  ADDR2         PRIAD2                         =address
     C                   MOVEL(P)  CITY          PRICTY                         =city
     C                   MOVEL(P)  STATE         PRISTA                         =state
     c**
     c                   if        zip2 <> *blanks
     c                   eval      prizip = zip1 + '-' + zip2
     c                   else
     c                   eval      prizip = zip1 + '-0000'
     c                   endif
     c**
     **
     C                   WHEN      EXCODE = 186
     c                   callp     XFXPRV( bbplv6  : bbaccn  : bbpayr : bbplan
     c                                   : 'ALTFA' : *blanks : 0      : bbtodt
     c                                   : wrkprv  : wrkadr )
     C                   MOVEL     WRKPRV        BOX321
     C                   MOVEL(P)  ADDR1         BOX322                         =address
     C                   MOVEL(P)  CITY          BOX323                         =city
     C                   MOVEL(P)  STATE         BOX324                         =state
     c**
     c                   if        zip2 <> *blanks
     c                   eval      box325 = zip1 + '-' + zip2
     c                   else
     c                   eval      box325 = zip1 + '-0000'
     c                   endif
     c**
     C                   WHEN      EXCODE = 187
     C                   MOVEL     BGDTPR        PRMPRC
     C                   MOVEL     'ALTPN'       PRVCOD
     C                   EXSR      SRPRV
     C                   IF        PRVVAR <> *BLANKS
     C                   MOVEL(P)  PRVVAR        PRPROV
     C                   MOVEL(P)  PRVVAR        BBPROV
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm                    prvcod
     C                   parm                    qua33b
     c                   if        qua33b <> *blanks
     c                   eval      prprov = qua33b + ' ' + prprov
     c                   endif
     C                   ELSE
     c                   callp     XFXPRV( bbplv6  : bbaccn  : bbpayr : bbplan
     c                                   : 'ALTPN' : *blanks : 0      : bbtodt
     c                                   : wrkprv  : wrkadr )
     C                   IF        WRKPRV <> *BLANKS
     C                   MOVEL(P)  WRKPRV        PRPROV
     C                   MOVEL(P)  WRKPRV        BBPROV
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm                    prvcod
     C                   parm                    qua33b
     c                   if        qua33b <> *blanks
     c                   eval      prprov = qua33b + ' ' + prprov
     c                   endif
     C                   ENDIF
     C                   ENDIF
     **
     C                   WHEN      EXCODE = 189
     c                   callp     XFXPRV( bbplv6  : bbaccn  : bbpayr : bbplan
     c                                   : 'ALTBN' : *blanks : 0      : bbtodt
     c                                   : wrkprv  : wrkadr )
     C                   IF        WRKPRV <> *BLANKS
     c                   move      phone         lvl2tl
     C                   MOVEL     WRKPRV        LVL2NM
     C                   MOVEL(P)  ADDR1         LVL2AD                         =address
     C                   MOVEL(P)  CITY          LVL2CT                         =city
     C                   MOVEL(P)  STATE         LVL2ST                         =state
     c**
     c                   if        zip2 <> *blanks
     c                   eval      lvl2zp = zip1 + '-' + zip2
     c                   else
     c                   eval      lvl2zp = zip1 + '-0000'
     c                   endif
     c                   endif
     **
     C                   WHEN      EXCODE = 190
     c                   callp     XFXPRV( bbplv6  : bbaccn  : bbpayr : bbplan
     c                                   : 'ALTTX' : *blanks : 0      : bbtodt
     c                                   : wrkprv  : wrkadr )
     C                   IF        WRKPRV <> *BLANKS
     C                   MOVEL(P)  WRKPRV        HX6TID
     C                   ENDIF
     **
     C                   WHEN      EXCODE = 191
     C                   MOVE      ' '           BOX11DY
     C                   MOVE      'X'           BOX11DN
     **
     C                   WHEN      EXCODE = 192
     C     IVSKEY        SETLL     HBFIVSVC
     C                   DOU       *IN71 = *ON
     C     IVSKEY        READE     HBFIVSVC                               71
     C                   IF        *IN71 = *OFF
     C                             AND BSVDLT = *BLANKS
     C                             AND BSVEFF <= BGDTSD
     C                             AND BGDTSD <= BSVEND
     C                             and bsvpyr = bbpayr and bsvpln = bbplan
     C                             and bsvpol = bbplcy and bsvrnk = bbrank
     C                   MOVEL(P)  BSVMOD        MOD(Z)
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     **
     C                   WHEN      EXCODE = 194
     C                   MOVE      ' '           box11am                         box11a
     C                   MOVE      ' '           box11af                         box11a
     **
     C                   WHEN      EXCODE = 195
     C                   EVAL      PRMPRC = *BLANKS
     C                   MOVEL(P)  'ALTTX'       PRVCOD
     C                   EXSR      SRPRV
     C                   IF        PRVVAR <> *BLANKS
     C                   MOVEL(P)  PRVVAR        HX6TID
     C                   MOVEL(P)  PRVVAR        BOX26
     C                   ENDIF
     **
     C                   WHEN      EXCODE = 196
     c                             and bilexc837 = *off
     C     IVSKEY        SETLL     HBFIVSVC
     C                   DOU       *IN71 = *ON
     C     IVSKEY        READE     HBFIVSVC                               71
     C                   IF        *IN71 = *OFF
     C                             AND BSVDLT = *BLANKS
     C                             AND BSVEFF <= BGDTSD
     C                             AND BGDTSD <= BSVEND
     C                             and bsvpyr = bbpayr and bsvpln = bbplan
     C                             and bsvpol = bbplcy and bsvrnk = bbrank
     C                   IF        BSVAUT <> *BLANKS
     C                   MOVEL(P)  BSVAUT        PRAUTH
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     **
     c                   when      excode = 200
     c                   move      'RF'          status
     c                   exsr      srasgn
     c                   if        MSGDR# <> 0
     c     MSGDR#        chain     HMFMAMS
     c                   if        %found
     c                   movel     HMDNAM        prthnm
     c                   endif
     c                   endif
      **
     C                   WHEN      EXCODE = 207
     C                             AND BGDTSD < 20031016                        =ugly, but necessary
     C                   SELECT
     C                   WHEN      BGDTPR = 'RESHAB'
     C                             OR BGDTPR = 'BRESHAB'
     C                   MOVEL(P)  'W9533'       CPT(Z)
     C                   WHEN      BGDTPR = 'IHS'
     C                   MOVEL(P)  'W9513'       CPT(Z)
     C                   ENDSL
     **
     C                   WHEN      EXCODE = 214
     C                   MOVE      'I'           OMTYPE                         =proc include
     C     EXOKEY        CHAIN     HBFBFXO                            76
     C                   IF        *IN76 = *OFF
     C                             AND BXODLT <> 'D'
     C                   MOVE      *ON           BILEXC214
     C                   ENDIF
     **
     C                   WHEN      EXCODE = 215
     C                   EVAL      YDATE = BBBILD
     C                   EXSR      SRCYMD
     C                   EVAL      PRADDT = MDATE
     **
     C                   WHEN      EXCODE = 216
     C                   EVAL      BOX26 = *BLANKS
     **
     C                   WHEN      EXCODE = 217
     C                   EVAL      BOX322 = *BLANKS
     C                   EVAL      BOX323 = *BLANKS
     C                   EVAL      BOX324 = *BLANKS
     C                   EVAL      BOX325 = *BLANKS
     **
     C                   WHEN      EXCODE = 218                                 =don't print
     C                   EVAL      PRIADR = *BLANKS                             =in top corner of
     C                   EVAL      PRIAD2 = *BLANKS                             =bill
     C                   EVAL      PRICTY = *BLANKS
     C                   EVAL      PRISTA = *BLANKS
     C                   EVAL      PRIZIP = *BLANKS
     **
     C                   WHEN      EXCODE = 219                                 =don't print
     C                   MOVEL     'N'           PRTDSC                         =diag desc
     **
     C                   WHEN      EXCODE = 220                                 =force
     C                   MOVEL     ' '           BOX27Y                         =assignment to
     C                   MOVEL     'X'           BOX27N                         =no
     **
     C                   WHEN      EXCODE = 221                                 =don't print
     C                   MOVE      *ON           *IN13                          =payor/plan
     **
     C                   WHEN      EXCODE = 222
     C                   MOVE      'I'           OMTYPE                         =proc include
     C     EXOKEY        CHAIN     HBFBFXO                            76
     C                   IF        *IN76 = *OFF
     C                             AND BXODLT <> 'D'
     C                   MOVE      DTT(Z)        WRKMDY
     C                   EVAL      WRKD = 1
     C                   EVAL      MDATE = WRKMDY
     C                   EXSR      SRCMDY
     C                   IF        YDATE > BGADDT
     C                   MOVE      WRKMDY        MDATE
     C                   ELSE
     C                   EVAL      YDATE = BGADDT
     C                   EXSR      SRCYMD
     C                   ENDIF
     C                   MOVE      MDATE         DTF(Z)
     C                   MOVEL     MDATE         MMDD6
     C                   MOVE      MDATE         YY6
     C                   MOVE      WKDAT6        DTF6(Z)
     C                   MOVE      MM6           DTFM(Z)
     C                   MOVE      DD6           DTFD(Z)
     C                   MOVE      YY6           DTFY(Z)
     C                   ENDIF
     **
     C                   WHEN      EXCODE = 223
     C                   MOVE      'I'           OMTYPE                         =proc include
     C     EXOKEY        CHAIN     HBFBFXO                            76
     C                   IF        *IN76 = *OFF
     C                             AND BXODLT <> 'D'
     C                             AND BBTRKO <> *BLANKS
     C     BBTRKO        CHAIN     HBFBLDTS                           72
     C                   IF        *IN72 = *OFF
     C                   MOVE      DTT(Z)        WRKMDY
     C                   EVAL      WRKD = 1
     C                   EVAL      MDATE = WRKMDY
     C                   EXSR      SRCMDY
     C                   IF        BLDFDT > YDATE
     C                   EVAL      YDATE = BLDFDT
     C                   EXSR      SRCYMD
     C                   ENDIF
     C                   MOVE      MDATE         DTF(Z)
     C                   MOVEL     MDATE         MMDD6
     C                   MOVE      MDATE         YY6
     C                   MOVE      WKDAT6        DTF6(Z)
     C                   MOVE      MM6           DTFM(Z)
     C                   MOVE      DD6           DTFD(Z)
     C                   MOVE      YY6           DTFY(Z)
     C                   ENDIF
     C                   ENDIF
     **
     C                   WHEN      EXCODE = 224
     C                   MOVEL     'Y'           REGIST
     **
     C                   WHEN      EXCODE = 225
     C                   MOVEL     WRKATN        PRIATN
     **
     C                   WHEN      EXCODE = 227
     C                   MOVE      'I'           OMTYPE                         =included procs
     C     EXOKEY        CHAIN     HBFBFXO                            76
     C                   IF        *IN76 = *OFF
     C                             AND BXODLT <> 'D'
     C                   MOVE      'CM'          STATUS
     C                   EXSR      SRASGN
     C                   IF        MSGDR# <> 0
     C                   MOVEL     'MCAID'       LICECD
     C                   MOVEL     HX6ST1        WRKSTA
     C     LICKEY        CHAIN     HMFLICE                            76
     C                   IF        *IN76 = *OFF
     C                   MOVEL     HLCNUM        PIN(Z)                         =box24k
     c                   eval      bx24i(z) = *blanks
     c                   if        pin(z) <> *blanks
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm                    licecd
     C                   parm                    bx24i(z)
     C                   endif
     C                   ELSE
     C                   EVAL      PIN(Z) = *BLANKS
     c                   eval      bx24i(z) = *blanks
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     **
     C                   MOVEL     BGDTPR        PRMPRC
     C                   MOVEL     'OPVDR'       PRVCOD
     C                   EXSR      SRPRV
     C                   MOVEL     PRVVAR        PIN(Z)
     c                   eval      bx24i(z) = *blanks
     c                   if        pin(z) <> *blanks
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm      'PRVDR'       prvcod
     C                   parm                    bx24i(z)
     C                   endif
     C                   ENDIF
     **
     C                   WHEN      EXCODE = 228
     C                   MOVE      'I'           OMTYPE                         =included procs
     C     EXOKEY        CHAIN     HBFBFXO                            76
     C                   IF        *IN76 = *OFF
     C                             AND BXODLT <> 'D'
     C                   MULT      96            QTY(Z)
     C                   EVAL      QTY4(Z) = QTY4(Z) * 96
     C                   EVAL      QTY2(Z) = QTY2(Z) * 96
     C                   eval      qty5(z) = qty5(z) * 96
     C                   ENDIF
     **
     C                   WHEN      EXCODE = 229                                 =don't print
     **
     C                   WHEN      EXCODE = 243
     C                   EVAL      PRMPRC = *BLANKS
     C                   MOVEL(P)  'GBHC '       PRVCOD
     C                   EXSR      SRPRV
     C                   IF        PRVVAR <> *BLANKS
     C                   MOVEL(P)  PRVVAR        PRRUPN
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm                    prvcod
     C                   parm                    qua17a
     C                   ENDIF
     **
     C                   WHEN      EXCODE = 287
     C                   MOVE      'P'           OMTYPE                         =proc include
     C     EXOKEY        CHAIN     HBFBFXO                            76
     C                   IF        *IN76 = *ON
     C                             OR BXODLT = 'D'
     C                   MOVEL(P)  BGDTPR        PRMPRC
     C                   IF        BGDTQT > 999
     C                   EVAL      PRMINU = BGDTQT
     C                   ELSE
     C                   EVAL      PRMINU = QTY(Z)
     C                   ENDIF
     C                   EXSR      SRUCNV
     C                   Z-ADD     PRMFRU        QTY(Z)
     C                   EVAL      QTY4(Z) = PRMFRU
     C                   Z-ADD     PRMFRU        QTY2(Z)
     C                   z-add     prmfru        qty5(z)
     c                   z-add     prmfru        temp4
     c                   if        (prmfru - temp4) <> 0
     c                   z-add     prmfru        b5fam1
     c                   endif
     C                   ENDIF
     **
     C                   WHEN      EXCODE = 317
     C                   MOVE      *ON           *IN73
     **
     C                   WHEN      EXCODE = 319
     C                   EVAL      LEVL# = 5
     C                   MOVEL     'REP'         TITLE
     C     REPKEY        CHAIN     HXFTITL                            79
     C                   IF        *IN79 = *OFF
     C                   CALL      'XFXSNAM'
     C                   PARM                    HXLNAM
     C                   PARM                    NAMEOT
     C     NAMF          CAT       NAML:1        PRTCOD
     C                   ENDIF
     **
     C                   WHEN      EXCODE = 324
     C                   MOVE      'I'           OMTYPE                         =proc include
     C     EXOKEY        CHAIN     HBFBFXO                            76
     C                   IF        *IN76 = *OFF
     C                             AND BXODLT <> 'D'
     C                   EVAL      WRKYMD = BGDTSD
     C                   EVAL      WRKDD = 1
     C                   EVAL      YDATE = WRKYMD
     C                   EXSR      SRCYMD
     C                   MOVE      MDATE         DTF(Z)
     C                   MOVEL     MDATE         MMDD6
     C                   MOVE      MDATE         YY6
     C                   MOVE      WKDAT6        DTF6(Z)
     C                   MOVE      MM6           DTFM(Z)
     C                   MOVE      DD6           DTFD(Z)
     C                   MOVE      YY6           DTFY(Z)
     **
     C                   EVAL      WRKYMD = BGDTSD                              =use eom date
     C                   EVAL      CMOYR = 0                                     for service date
     C                   MOVE      WRKYY         CMOYR
     C                   MOVEL     WRKMM         CMOYR
     C                   EXSR      GTEOM
     C                   EXSR      SRCMDY
     C                   MOVE      MDATE         DTT(Z)
     C                   MOVEL     MDATE         MMDD6
     C                   MOVE      MDATE         YY6
     C                   MOVE      WKDAT6        DTT6(Z)
     C                   MOVE      MM6           DTTM(Z)
     C                   MOVE      DD6           DTTD(Z)
     C                   MOVE      YY6           DTTY(Z)
     C                   ENDIF
     **
     C                   WHEN      EXCODE = 325
     c                   eval      box01X2 = 'X'
     C                   MOVEA     '010'         *IN(34)
     **
     C                   WHEN      EXCODE = 326
     C                   EVAL      BOX04 = *BLANKS
     **
     C                   WHEN      EXCODE = 327
     C                   EVAL      BOX07A = *BLANKS
     C                   EVAL      BOX07B = *BLANKS
     C                   EVAL      BOX07C = *BLANKS
     C                   EVAL      BOX07D = *BLANKS
     C                   EVAL      BOX07E = 0
     **
     C                   WHEN      EXCODE = 328
     C                   MOVEL     BGNAME        BOX321
     C                   MOVEL     BDPAD1        BOX322
     C                   MOVEL     BDPCTY        BOX323
     C                   MOVEL     BDPSTA        BOX324
     c**
     c                   if        bdpzp2 <> *blanks
     c                   eval      box325 = bdpzp1 + '-' + bdpzp2
     c                   else
     c                   eval      box325 = bdpzp1 + '-0000'
     c                   endif
     **
     C                   WHEN      EXCODE = 342
     C                   MOVEL     'Y'           PRTDEC
     **
     C                   WHEN      EXCODE = 348
     C                   MOVEL     'Y'           PRTPMT
     **
     C                   WHEN      EXCODE = 349
     C                   MOVEL     SIGN          PRTHNM
     **
     C                   WHEN      EXCODE = 350
     C                   MOVEL     'WHN'         PRRPHY
     **
     C                   WHEN      EXCODE = 355
     C                   EXSR      GETCAP
     **
     C                   WHEN      EXCODE = 358
     C                   MOVEL(P)  rfmdli        BOX19
     **
     C                   WHEN      EXCODE = 363
     C                   EVAL      PRPROV = *BLANKS
     **
     C                   WHEN      EXCODE = 364
     c                   eval      bilexc364 = *on
     C**                 MOVEL     DTF(Z)        MDATE
     C**                 EXSR      SRCMDY
     C**                 EVAL      FDATE = YDATE
     C**                 MOVEL     DTT(Z)        MDATE
     C**                 EXSR      SRCMDY
     C**                 EVAL      TDATE = YDATE
     C**                 IF        FDATE = TDATE
     C**                 EXSR      SRIVER                                       =ins ver
     C**                 MOVE      'P'           OMTYPE
     C**   EXOKEY        CHAIN     HBFBFXO                            75
     C**                 IF        *IN75 = *ON
     C**                           OR BXODLT = 'D'
     C**                 IF        QUANTY = 9
     C**   BGDTPR        CHAIN     XFFPROCM                           76
     C**                 IF        *IN76 = *OFF
     C**                 MOVEL     'Y'           ROUND
     C**                 ENDIF
     C**                 ENDIF
     C**                 ENDIF
     C**                 ENDIF
     **
     C                   WHEN      EXCODE = 366
     C                   MOVE      'I'           OMTYPE                         =proc include
     C     EXOKEY        CHAIN     HBFBFXO                            76
     C                   IF        *IN76 = *OFF
     C                             AND BXODLT <> 'D'
     C                   MOVE      *ON           *IN74
     C                   ENDIF
     **
     C                   WHEN      EXCODE = 375                                 =don't print
     **
     c                   when      excode = 399
     c                             and prrupn = *blanks
      **
     c                   eval      prrupn = *blanks
     c                   eval      qua17a = *blanks
     C                   move      'RF'          status
     C                   exsr      srasgn
     C                   if        msgdr# <> 0
     c     msgdr#        chain     hmfmams                            79
     c                   if        *in79 = *off
     c                   eval      ldoc# = msgdr#
     c                   eval      lcode = *blanks
     c                   movel     'UPIN '       lcode
     c     liceky        chain     hmflice2                           79
     c                   if        *in79 = *off
     c                   movel     hlcnum        prrupn
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm                    lcode
     C                   parm                    qua17a
     c                   endif
     c                   endif
     C                   endif
      **
     c                   when      excode = 400
     c                   movel     bgdtpr        prmprc
     c                   movel     'PRVDR'       prvcod
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   movel     prvvar        box32b
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm                    prvcod
     C                   parm                    qua32b
     c     qua32b        cat       box32b:1      box32b
     c                   endif
      **
     c                   when      excode = 401
     c                   eval      EXC401 = 'Y'
     **
     C                   when      excode = 417
     C                   MOVEL     BGDTPR        PRMPRC
     C                   MOVEL     'ALTNP'       PRVCOD
     C                   EXSR      SRPRV
     c     prvvar        ifne      *blanks
     c                   movel     prvvar        box33a
     C                   endif
     **
     C                   WHEN      EXCODE = 418
     C                   MOVEL     BGDTPR        PRMPRC
     C                   MOVEL     'PRVDR'       PRVCOD
     C                   EXSR      SRPRV
     C                   IF        PRVVAR <> *BLANKS
     C                   MOVEL(P)  PRVVAR        pin(z)
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm      'PRVDR'       PRVCOD
     C                   parm                    bx24i(z)
     C                   ELSE
     C                   EVAL      pin(z) = *BLANKS
     C                   EVAL      bx24i(z) = *BLANKS
     C                   ENDIF
     **
     C                   WHEN      EXCODE = 419
     c                   movel     bgdtpr        prmprc
     C                   MOVEL     'OPVDR'       PRVCOD
     C                   EXSR      SRPRV
     C                   IF        PRVVAR <> *BLANKS
     C                   EVAL      PRPROV = prvvar
     C                   MOVEL(P)  PRVVAR        pin(z)
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm      'PRVDR'       PRVCOD
     C                   parm                    bx24i(z)
     C                   MOVEL(P)  PRVVAR        BBPROV
     C                   ELSE
     C                   EVAL      pin(z) = *BLANKS
     C                   EVAL      bx24i(z) = *BLANKS
     C                   ENDIF
     **
     C                   WHEN      EXCODE = 420
     C                   MOVEL     BGDTPR        PRMPRC
     C                   MOVEL     'ALTGP'       PRVCOD
     C                   EXSR      SRPRV
     C                   IF        PRVVAR <> *BLANKS
     C                   MOVEL(P)  PRVVAR        pin(z)
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm      'PRVDR'       PRVCOD
     C                   parm                    bx24i(z)
     C                   ELSE
     C                   EVAL      pin(z) = *BLANKS
     C                   EVAL      bx24i(z) = *BLANKS
     C                   ENDIF
     **
     C                   WHEN      EXCODE = 421
     C                   MOVEL(P)  BDIGRP        pin(z)
     C                   if        pin(z) <> *BLANKS
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm      'PPGRP'       PRVCOD
     C                   parm                    bx24i(z)
     C                   ELSE
     C                   EVAL      bx24i(z) = *BLANKS
     C                   ENDIF
      **
     c                   when      excode = 422
     c                   MOVEL     BGDTPR        PRMPRC
     C                   MOVEL     'ALTPN'       PRVCOD
     C                   EXSR      SRPRV
     c     prvvar        ifne      *blanks
     c                   movel     prvvar        pin(z)
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm      'PRVDR'       PRVCOD
     C                   parm                    bx24i(z)
     C                   else
     C                   eval      pin(z) = *BLANKS
     C                   eval      bx24i(z) = *BLANKS
     c                   endif
     **
     c                   when      excode = 423
     C                   MOVEL     BGDTPR        PRMPRC
     C                   MOVEL     'ALTGP'       PRVCOD
     C                   EXSR      SRPRV
     c     prvvar        ifne      *blanks
     c                   movel     prvvar        prprov
     C                   endif
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm      'PRVDR'       PRVCOD
     C                   parm                    qua33b
     **
     c                   when      excode = 424
     C                   MOVE      'RF'          STATUS
     C                   EXSR      SRASGN
     C                   IF        MSGDR# <> 0
     c                   eval      docnum = msgdr#
     c                   eval      statek = *blanks
     c                   eval      lcode = *blanks
     c                   movel     'NPI  '       lcode
     c     licek2        chain     hmflice                            79
     c                   if        *in79 = *off
     c                   movel     hlcnum        box33a
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm                    lcode
     C                   parm                    qua33b
     C                   ENDIF
     C                   ENDIF
      **
     c                   when      excode = 425
     c                   eval      box32a = *blanks
      **
     c                   when      excode = 426
     c                   eval      box33a = *blanks
      **
     c                   when      excode = 444
     c                   callp     XFXPRV( bbplv6  : bbaccn  : bbpayr : bbplan
     c                                   : 'NPI  ' : *blanks : 0      : bbtodt
     c                                   : prvvar  : prvadr )
     c     prvvar        ifne      *blanks
     c                   eval      prrnpi = *blanks
     c                   movel     prvvar        prrnpi
     c                   endif
      **
     c                   when      excode = 452
     c                             or excode = 463
     c                   eval      include = *on
     c                   if        excode = 452
     c                   eval      omtype = 'P'                                 EX452 proc exclude
     c     exokey        chain     hbfbfxo
     c                   if        %found(hbpbfxo) and bxodlt <> 'D'
     c                   eval      include = *off
     c                   endif
     c                   endif

     c                   if        include
     c                   call      'XFXRTYPE'
     c                   parm                    bgdlv6
     c                   parm                    bgdtac
     c                   parm                    bgdseq
     c                   parm                    bbrank
     c                   parm                    bbpayr
     c                   parm                    bbplan
     c                   parm                    bbplcy
     c                   parm      '     '       reimtp
     c                   parm      '     '       stpdtp
     c                   parm      '     '       stpdfl
     c                   if        reimtp = 'HOUR'
     c                              or (reimtp = 'ZERO' and sndf37 = 'Y')
     c     prccky        chain     xffprocc                           76
     c                   if        *in76 = *on
     c     bgdtpr        chain     xffprocm                           76
     c                   endif
     c                   if        *in76 = *off
     c                             and xfptim <> 0
     c                   z-add     xfptim        wrktim
     c     xfphr         mult      60            xfptme
     c                   add       xfpmn         xfptme
     c     xfptme        div(h)    60            factor
     c                   else
     c                   z-add     1             factor
     c                   endif
     c                   if        excode = 452
     c     factor        mult      bgdtqt        ex452q                         =hourly quantity
     c                   z-add     ex452q        qty(z)
     c                   z-add     ex452q        qty4(z)
     c                   z-add     ex452q        qty2(z)
     c                   z-add     ex452q        qty5(z)
     c                   move      *on           *in74
     C                   move      *on           bilexc452
     c                   else
     c     factor        mult      bgdtqt        ex463w                         =hourly quantity
     c                   eval(h)   ex463q = ex463w
     c                   z-add     ex463q        qty(z)
     c                   z-add     ex463q        qty4(z)
     c                   z-add     ex463q        qty2(z)
     c                   z-add     ex463q        qty5(z)
     C                   move      *on           bilexc463
     c                   endif
     c                   endif
     c                   endif
     **
     C                   WHEN      EXCODE = 456
     c                   if        bgdprv <> *blanks
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm      'PRVDR'       prvcod
     C                   parm                    qua32b
     c     qua32b        cat       bgdprv:1      box32b
     c                   else
     c                   eval      box32b = *blanks
     c                   eval      qua32b = *blanks
     c                   endif
     **
     C                   WHEN      EXCODE = 466
     C                             and not bilexc885
     C                   MOVEL     BGDTPR        PRMPRC
     C                   MOVEL     'TAXON'       PRVCOD
     C                   EXSR      SRPRV
     c     prvvar        ifne      *blanks
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm      'TAXON'       PRVCOD
     C                   parm                    qua33b
     c                   eval      prprov = qua33b + ' ' + prvvar
     c                   else
     c                   eval      prprov = *blanks
     c                   eval      qua33b = *blanks
     C                   endif
     **
     C                   WHEN      EXCODE = 470
     C                   MOVEL     BGDTPR        PRMPRC
     C                   MOVEL     'TAXON'       PRVCOD
     C                   EXSR      SRPRV
     c     prvvar        ifne      *blanks
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm      'TAXON'       PRVCOD
     C                   parm                    bx24i(z)
     c                   eval      pin(z) = prvvar
     c                   else
     c                   eval      pin(z) = *blanks
     c                   eval      bx24i(z) = *blanks
     C                   endif
      **
     C                   when      excode = 480
     c                   movel     bgdtpr        prmprc
     c                   movel     'NPI  '       prvcod
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   movel     prvvar        npi(z)
     c                   endif
      **
     c                   when      excode = 508
     c                   eval      *in74 = *on
     c                   eval      omtype = 'I'
     c     exokey        chain     hbfbfxo
     c                   if        %found and bxodlt <> 'D'
     c                   eval      qty2(z) = bgdtqt / 100
     c                   move      bgd508        tmp508
     c     tmp508        mult      1000          tmp508
     c                   add       tmp508        qty2(z)
     c                   endif
      **
     C                   when      excode = 510                                 =print by svc date
     C                   move      *on           *in16
      **
     C                   when      excode = 516
     c                   movel     bgdtpr        prmprc
     c                   movel     'ALTNP'       prvcod
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   movel     prvvar        npi(z)
     c                   endif
      **
     c                   when      excode = 543
     c                   movel     bgdtpr        prmprc
     c                   movel     'PRVDR'       prvcod
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   eval      box32a = *blanks
     c                   movel     prvvar        box32a
     c                   eval      box33a = *blanks
     c                   movel     prvvar        box33a
     c                   endif
      **
     c                   when      excode = 545
     c                   if        hx6a11 <> *blanks
     c                   movel(p)  hx6nam        box321
     c                   movel(p)  hx6a11        box322
     c                   movel(p)  hx6ct1        box323
     c                   movel(p)  hx6st1        box324
      **
     c                   if        hx6z12 <> *blanks
     c                   eval      box325 = hx6z11 + '-' + hx6z12
     c                   else
     c                   eval      box325 = hx6z11 + '-0000'
     c                   endif
     c                   endif
     **
     C                   when      excode = 549
     C                   movel     bgdtpr        prmprc
     C                   movel     'TAXON'       prvcod
     C                   exsr      srprv
     c     prvvar        ifne      *blanks
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm      'TAXON'       prvcod
     C                   parm                    qua32b
     c                   eval      box32b = qua32b + ' ' + prvvar
     c                   else
     c                   eval      box32b = *blanks
     c                   eval      qua32b = *blanks
     C                   endif
     **
     c                   when      excode = 552
     c                   eval      status = 'AT'
     c                   exsr      srasgn
     c                   if        msgdr# <> 0
     c     msgdr#        chain     hmfmams
     c                   if        %found
     c                   eval      physname = hmdnam
     c                   endif
     c                   eval      ldoc# = msgdr#
     c                   eval      lcode = 'NPI  '
     c     liceky        chain     hmflice2
     c                   if        %found
     c                   eval      npi(z) = hlcnum
     c                   endif
     c                   endif
     **
     c                   when      excode = 559
     c                   move      'I'           omtype                         =proc include
     c     exokey        chain     hbfbfxo                            76
     c                   if        *in76 = *off and bxodlt <> 'D'
     c                   eval      wrkymd = bgdtsd
     c                   eval      wrkdd = 1
     c                   eval      ydate = wrkymd
     c                   if        ydate < bgaddt
     c                   eval      ydate = bgaddt
     c                   endif
     c                   exsr      srcymd
     c                   move      mdate         dtf(z)
     c                   movel     mdate         mmdd6
     c                   move      mdate         yy6
     c                   move      wkdat6        dtf6(z)
     c                   move      mm6           dtfm(z)
     c                   move      dd6           dtfd(z)
     c                   move      yy6           dtfy(z)
      **
     c                   eval      wrkymd = bgdtsd                              =use eom date
     c                   eval      cmoyr = 0                                     for service date
     c                   move      wrkyy         cmoyr
     c                   movel     wrkmm         cmoyr
     c                   exsr      gteom
     c                   exsr      srcmdy
     c                   if        ydate > bgdsdt and bgdsdt <> 0
     c                   eval      ydate = bgdsdt
     c                   endif
     c                   exsr      srcymd
     c                   move      mdate         dtt(z)
     c                   movel     mdate         mmdd6
     c                   move      mdate         yy6
     c                   move      wkdat6        dtt6(z)
     c                   move      mm6           dttm(z)
     c                   move      dd6           dttd(z)
     c                   move      yy6           dtty(z)
     c                   endif

     c                   when      excode = 582                                 Alaska really...
     c                   eval      bilexc582 = *on
     c                   eval      omtype = 'I'
     c     exokey        chain     hbfbfxo
     c                   if        %found(hbpbfxo) and bxodlt <> 'D' and
     c                             bgdtth <> 0
     c                   eval      docnum = bgdtth
     c     docnum        chain     hmfmams
     c                   if        %found(hmpmams)
     c                   eval      b582n9 = 'ATYPICAL'
     c                   eval      physname = hmdnam
     c                   eval      statek = hx6st1
     c                   eval      lcode = 'MCAID'
     c     licek2        chain     hmflice
     c                   if        not %found(hmplice)
     c                   eval      statek = *blanks
     c     licek2        chain     hmflice
     c                   endif
     c                   if        %found(hmplice)
     c                   movel(p)  hlcnum        b582r2
     c                   endif
     c                   endif
     c                   endif

     c                   when      excode = 583 and bilexc582 = *off            another Alaska...
     c                   eval      omtype = 'I'
     c     exokey        chain     hbfbfxo
     c                   if        %found(hbpbfxo) and bxodlt <> 'D' and
     c                             bgdtth <> 0
     c                   eval      docnum = bgdtth
     c     docnum        chain     hmfmams
     c                   if        %found(hmpmams)
     c                   eval      physname = hmdnam
     c                   eval      statek = hx6st1
     c                   eval      lcode = 'NPI'
     c     licek2        chain     hmflice
     c                   if        not %found(hmplice)
     c                   eval      statek = *blanks
     c     licek2        chain     hmflice
     c                   endif
     c                   if        %found(hmplice)
     c                   eval      b582n8 = 'XX'
     c                   movel(p)  hlcnum        b582n9
     c                   endif
     c                   endif
     c                   endif

     c                   when      excode = 584
     c                   eval      omtype = 'P'                                 proc exclude
     c     exokey        chain     hbfbfxo
     c                   if        not %found(hbpbfxo) or bxodlt = 'D'
     c                   if        bgdsdt < bbtodt and bgdsdt <> 0
     c                   eval      ydate = bgdsdt
     c                   else
     c                   eval      ydate = bbtodt
     c                   endif
     c                   exsr      srcymd
     c                   move      mdate         dtt(z)
     c                   movel     mdate         mmdd6
     c                   move      mdate         yy6
     c                   move      wkdat6        dtt6(z)
     c                   move      mm6           dttm(z)
     c                   move      dd6           dttd(z)
     c                   move      yy6           dtty(z)
     c                   endif

     c                   when      excode = 585                                 Group Health Plan BX
     c                   movea     '000'         *in(34)
     c                   eval      *in86 = *on
     c                   eval      box01X3 = 'X'

     c                   when      excode = 586 and                             Print policy# in 11
     c                             bilexc38 = *off and bilexc152 = *off
     c                   eval      prgrpx = bbplcy
      **
     c                   when      excode = 590                                 Block insured's DoB
     c                   eval      sbbdte = 0                                   box11a

     c                   when      excode = 591                                 Print proc desc in
     c     prccky        chain     xffprocc                                     box24
     c                   if        not %found(hxpprocc)
     c     bgdtpr        chain     xffprocm
     c                   endif
     c                   if        %found
     c                   eval      *in87 = *on
     c                   eval      pdsc(z) = xfpdsc
     c                   endif

     c                   when      excode = 628
     c                   eval      prauth = %editc(bdpss# : 'X')

     c                   when      excode = 649
     c                   eval      omtype = 'I'                                 EX649 proc include
     c     exokey        chain     hbfbfxo
     c                   if        %found(hbpbfxo) and bxodlt <> 'D'
     C                   MOVEL     BGDTPR        PRMPRC
     C                   MOVEL     'ALTGP'       PRVCOD
     C                   EXSR      SRPRV
     c     prvvar        ifne      *blanks
     c                   movel     prvvar        prprov
     c                   else
     c                   move      *blanks       prprov
     C                   endif
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm      'PRVDR'       PRVCOD
     C                   parm                    qua33b
     C                   else
     C                   move      *blanks       prprov
     C                   endif
      **
     C                   when      excode = 658
     **
     C                   eval      prrupn = *blanks
     C                   eval      prrphy = *blanks
     C                   eval      qua17a = *blanks
     C                   move      'RF'          status
     C                   exsr      srasgn
     C                   if        msgdr# <> 0
     C     msgdr#        chain     hmfmams                            76
     C                   if        *in76 = *off
     C                   movel     hmdnam        prrphy                         =name
     C                   movel     'STATE'       licecd
     C                   movel     hx6st1        wrksta
     C     lickey        chain     hmflice                            76
     C                   if        *in76 = *off
     C                   movel     hlcnum        prrupn                         =upin #
     C****               movel     'STATE'       LICECD                         =force new qual
     C****               call      'XFXBQ15'
     C****               parm                    bbpayr
     C****               parm                    licecd
     C****               parm                    qua17a
     C                   else
     C                   eval      prrupn = *blanks
     C                   endif
     C                   endif
     C                   endif
     **
     c                   when      excode = 660
     c                   movel     bgdtpr        prmprc
     c                   eval      qua33b = *blanks
     c                   movel     'PRVDR'       prvcod
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   eval      prprov = prvvar
     c                   else
     c                   eval      prprov = *blanks
     c                   endif
      **
     c****               when      excode = 677
     c****               eval      icndcn = *blanks
     c****               if        bbbsts = 'R'
     c****                         and (bbtorb = '7' or bbtorb = '8')
     c****               movel     bbtcn         icndcn
     c****               if        icndcn <> *blanks
     c****               movel     icndcn        box22b
     c****               endif
     c****               endif
      **
     c                   when      excode = 690
     c                   if        bgdtth <> 0
     c                   eval      pin(z) = *blanks
     c     bgdtth        chain     hmfmams
     c                   if        %found(hmpmams)
     c                   eval      lcode = 'TAXON'
     c                   eval      ldoc# = bgdtth
     c     liceky        chain     hmllice
     c                   if        %found(hmllice)
     c                   movel     hlcnum        pin(z)
     c                   endif
     c                   endif
     c                   endif
      **
     c                   when      excode = 814
     c                   movel     bgdtpr        prmprc
     c                   movel     'PRVDR'       prvcod
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   movel     prvvar        box32b
     c                   endif
      **
     c                   when      excode = 815
     c                   movel     bgdtpr        prmprc
     c                   eval      qua33b = *blanks
     c                   movel     'PRVDR'       prvcod
     c                   exsr      srprv
     c     prvvar        ifne      *blanks
     c                   eval      prprov = prvvar
     C                   call      'XFXBQ15'
     C                   parm                    bbpayr
     C                   parm                    prvcod
     C                   parm                    qua33b
     c     qua33b        cat       prprov:1      prprov
     c                   else
     c                   eval      prprov = *blanks
     c                   endif
      **
     c                   when      excode = 817
     c     bbtrak        chain     hbfchgbl
     c                   if        %found
     c     chrgKey2      chain     hbfchrg
     c                   if        %found
     c                   eval      omtype = 'I'
     c                   eval      bgdtpr = bgctpr
     c                   eval      bgdtsd = bgctsd
     c     exokey        chain     hbfbfxo
     c                   eval      include = (%found and bxodlt <> 'D')
     c                   callp     HBXSEQGEN( bbplv6  : bbaccn : bbrank : bbpayr
     c                                      : bbplan  : bbplcy : bgdtpr : bgdtsd
     c                                      : include : rtnAuth )
     c                   eval      prauth = rtnAuth
     c                   endif
     c                   endif
      **
     c                   when      excode = 833
     c                   if        box33a = *blanks
     c                   eval      box33a = 'ATYPICAL'
     c                   endif
      **
     c                   when      excode = 835
     c                   move      'AD'          status
     c                   exsr      srasgn
     c                   if        MSGDR# <> 0
     c     MSGDR#        chain     HMFMAMS
     c                   if        %found
     c                   movel     HMDNAM        prthnm
     c                   movel     HMDNAM        physname
     c                   endif
     c                   endif
      **
     c                   when      excode = 837
     c                   eval      prmprc = *blanks
     c                   eval      prvcod = 'VASID'
     c                   exsr      srprv
     c                   if        prvvar <> *blanks
     c                   eval      prauth = prvvar
     c                   endif
     c                   eval      bdpad1 = hldadr
     c                   eval      bdpcty = hldcty
     c                   eval      bdpsta = hldsta
     c                   eval      bdpzp1 = hldzip
     c                   eval      box07a = hldadr
     c                   eval      box07b = hldcty
     c                   eval      box07c = hldsta
     c                   eval      box07d = hldzip
     c                   eval      bilexc837 = *on
      **
     c                   when      excode = 861
     c                   eval      ldoc# = bgdtth
     c                   eval      lcode = 'SNPI '
     c     liceky        chain     hmflice2
     c                   if        %found (hmllice)
     c                   eval      snpi = hlcnum
     c                   eval      bilexc861 = *on
     c     snpi          chain     hmfnpi
     c                   if        %found (hmpnpi)
     c                   eval      prthnm = npname
     c                   eval      physname = npname
     c                   endif
     c                   endif
      **
     c                   when      excode = 885
     c                   eval      prmprc = bgdtpr
     c                   eval      qua33b = *blanks
     c                   eval      prvcod = 'TAXON'
     c                   exsr      srprv
     c                   if        prvvar <> *blanks
     c                   eval      prprov = prvvar
     c                   eval      bilexc885 = *on
     c                   else
     c                   eval      prprov = *blanks
     c                   endif

     c                   when      excode = 887
     c                   eval      box17q = *blanks
     c                   eval      prrphy = *blanks
     c                   eval      qua17a = *blanks
     c                   eval      prrupn = *blanks
     c                   eval      prrnpi = *blanks

     c                   when      excode = 888
     c                   eval      in1typ = 'P'
     c                   eval      IN2TYP = *blanks
     c                   eval      IN2SS# = 0
     c                   eval      IN2NAM = *blanks
     c                   eval      IN2AD1 = *blanks
     c                   eval      IN2AD2 = *blanks
     c                   eval      IN2CTY = *blanks
     c                   eval      IN2STA = *blanks
     c                   eval      IN2ZIP = *blanks
     c                   eval      IN2FCL = *blanks
     c                   eval      IN2NPI = *blanks
     c                   eval      IN2POL = *blanks
     c                   eval      IN2REL = *blanks
     c                   eval      IN2GP# = *blanks
     c                   eval      IN2GPN = *blanks
     c                   eval      IN2INM = *blanks
     c                   eval      IN2SEX = *blanks
     c                   eval      IN2BDT = 0
     c                   eval      IN2SBR = *blanks
     c                   eval      IN3TYP = *blanks
     c                   eval      IN3SS# = 0
     c                   eval      IN3NAM = *blanks
     c                   eval      IN3AD1 = *blanks
     c                   eval      IN3AD2 = *blanks
     c                   eval      IN3CTY = *blanks
     c                   eval      IN3STA = *blanks
     c                   eval      IN3ZIP = *blanks
     c                   eval      IN3FCL = *blanks
     c                   eval      IN3NPI = *blanks
     c                   eval      IN3POL = *blanks
     c                   eval      IN3REL = *blanks
     c                   eval      IN3GP# = *blanks
     c                   eval      IN3GPN = *blanks
     c                   eval      IN3INM = *blanks
     c                   eval      IN3SEX = *blanks
     c                   eval      IN3BDT = 0
     c                   eval      IN3SBR = *blanks

     c                   when      excode = 891
     c                   eval      pin(z) = *blanks
     c                   eval      bx24i(z) = *blanks
     c                   eval      licecd = 'MCAID'
     c                   eval      licct2 = bgdct2
     c     licky3        chain     hmflice
     c                   if        not %found
     c                   eval      licct2 = *blanks
     c     licky3        chain     hmflice
     c                   if        not %found
     c     licky2        chain     hmflice
     c                   if        not %found
     c                   endif
     c                   endif
     c                   endif
     c                   if        %found and hlcnum <> *blanks
     c                   movel     hlcnum        pin(z)
     c                   if        pin(z) <> *blanks
     c                   call      'XFXBQ15'
     c                   parm                    bbpayr
     c                   parm                    licecd
     c                   parm                    bx24i(z)
     c                   endif
     c                   endif

     c                   when      excode = 908
     c                   eval      status = 'RF'
     c                   exsr      srasgn
     c                   if        msgdr# <> 0
     c     msgdr#        chain     hmfmams
     c                   if        %found
     c                   eval      physname = hmdnam
     c                   endif
     c                   eval      ldoc# = msgdr#
     c                   eval      lcode = 'NPI  '
     c     liceky        chain     hmflice2
     c                   if        %found
     c                   eval      npi(z) = hlcnum
     c                   endif
     c                   endif

     c                   when      excode = 909
     c                   eval      prmprc = *blanks
     c                   eval      prvcod = 'VASID'
     c                   exsr      srprv
     c                   if        prvvar <> *blanks
     c                   eval      vasid = prvvar
     c     bbtrak        chain     hbfchgbl
     c                   if        %found
     c     chrgKey2      chain     hbfchrg
     c                   if        %found
     c                   eval      omtype = 'P'
     c                   eval      bgdtpr = bgctpr
     c     exokey        chain     hbfbfxo
     c                   if        not %found(hbpbfxo) or bxodlt = 'D'
     c     ivskey        setll     hbfivsvc
     c                   dou       %eof(halivsvd)
     c     ivskey        reade     hbfivsvc
     c                   if        not %eof(halivsvd)
     c                             and bsvdlt = *blanks
     c                             and bsveff <= bgdtsd
     c                             and bgdtsd <= bsvend
     c                             and bsvpyr = bbpayr and bsvpln = bbplan
     c                             and bsvpol = bbplcy and bsvrnk = bbrank
     c                   if        bsvaut <> *blanks
     c                   eval      prvcod = 'PAYID'
     c                   exsr      srprv
     c                   if        prvvar <> *blanks
     c                   exsr      srctab
     c                   eval      tcode = 'B909'
     c                   eval      ecode = prvvar
     c                   exsr      srtabl
     c                   if        tind = ' '
     c****               eval      prauth = %trim(vasid) + ' ' + %trim(bsvaut)
     c                   eval      prauth = %trim(vasid) + '-' + %trim(bsvaut)
     c                   leave
     c                   else
     c                   eval      prauth = %trim(bsvaut)
     c                   leave
     c                   endif
     c                   endif
     c                   endif
     c                   endif
     c                   enddo
     c                   endif
     c                   endif
     c                   endif
     c                   endif


     c                   when      excode = 916
     c                   eval      prmprc = bgdtpr
     c                   eval      prvcod = 'NPI  '
     c                   exsr      srprv
     c                   eval      box32b = prvvar

     c                   when      excode = 917
     c                   eval      exc401 = 'Y'
     c                   eval      prmprc = bgdtpr
     c                   eval      prvcod = 'NPI  '
     c                   exsr      srprv
     c                   eval      prprov = prvvar

     c                   when      excode = 922
     c                   eval      prmprc = *blanks
     c                   eval      prvcod = 'FALOC'
     c                   exsr      srprv
     c                   eval      box32b = prvvar

     c                   when      excode = 931
     c                   eval      omtype = 'I'                                 =Procedure include
     c     exokey        chain     hbfbfxo
     c                   if        %found(hbpbfxo) and bxodlt <> 'D'
     c     prccky        chain     xffprocc
     c                   if        not %found(hxpprocc)
     c     bgdtpr        chain     xffprocm
     c                   endif
     c                   if        %found
     c                   eval      *in87 = *on
     c                   eval      pdsc(z) = xfpdsc
     c                   endif
     c                   endif

     c                   when      excode = 935
     c                   eval      bilexc935 = *on
      // Populate fields for Loop 2010AA
     c                   callp     XFXPRV( bbplv6  : bbaccn  : bbpayr : bbplan
     c                                   : 'BN837' : *blanks : 0      : bbtodt
     c                                   : wrkprv  : wrkadr )
     c                   if        wrkprv <> *blanks
     c                   eval      wx33tl = phone
     c                   eval      wx33nm = wrkprv
     c                   eval      wx33a1 = addr1
     c                   eval      wx33a2 = addr2
     c                   eval      wx33ct = city
     c                   eval      wx33st = state
     c                   if        zip2 <> *blanks
     c                   eval      wx33zp = zip1 + '-' + zip2
     c                   else
     c                   eval      wx33zp = zip1 + '-0000'
     c                   endif
     c                   endif

     c                   callp     XFXPRV( bbplv6  : bbaccn  : bbpayr : bbplan
     c                                   : 'PAYTO' : *blanks : 0      : bbtodt
     c                                   : wrkprv  : wrkadr )
     c                   if        wrkprv <> *blanks
      // Populate fields for Box33
     c                   move      phone         lvl2tl
     c                   eval      lvl2nm = wrkprv
     c                   eval      lvl2ad = addr1
     c                   eval      lvl2ct = city
     c                   eval      lvl2st = state
     c                   if        zip2 <> *blanks
     c                   eval      lvl2zp = zip1 + '-' + zip2
     c                   else
     c                   eval      lvl2zp = zip1 + '-0000'
     c                   endif
      // Populate fields for Loop 2010AB
     c                   eval      wxfftl = phone
     c                   eval      wxffnm = wrkprv
     c                   eval      wxffa1 = addr1
     c                   eval      wxffa2 = addr2
     c                   eval      wxffct = city
     c                   eval      wxffst = state
     c                   if        zip2 <> *blanks
     c                   eval      wxffzp = zip1 + '-' + zip2
     c                   else
     c                   eval      wxffzp = zip1 + '-0000'
     c                   endif
     c                   endif

     c                   when      excode = 940
     c                   eval      omtype = 'I'                                 =Proc Include
     c     exokey        chain     hbfbfxo
     c                   if        %found and bxodlt <> 'D'
     c     prccky        chain     xffprocc
     c                   if        not%found
     c     bgdtpr        chain     xffprocm
     c                   endif
     c                   if        %found and xfptim <> 0
     c                   z-add     xfptim        wrktim
     c     xfphr         mult      60            xfptme
     c                   add       xfpmn         xfptme
     c     xfptme        div(h)    60            factor
     c                   else
     c                   z-add     1             factor
     c                   endif
     c     factor        mult      bgdtqt        ex940w                         =qty in hrs
     c                   eval(h)   ex940q = ex940w                              =round qty in hrs
     c                   z-add     ex940q        qty(z)
     c                   z-add     ex940q        qty4(z)
     c                   z-add     ex940q        qty2(z)
     c                   z-add     ex940q        qty5(z)
     C                   eval      bilexc940 = *on
     c                   endif

     c                   when      excode = 941
     c                   eval      bilexc941 = *on

     c                   when      excode = 946
     c                   if        bgdaut <> *blanks
     c                   movel(p)  bgdaut        prauth
     c                   endif

     c                   when      excode = 947
     c     bbtrak        chain     hbfchgbl
     c                   if        %found
     c     chrgKey2      chain     hbfchrg
     c                   if        %found
     c     bgctth        chain     hmfmams
     c                   if        %found
     c                   eval      prthnm = hmdnam
     c                   eval      physname = hmdnam
     c                   endif
     c                   endif
     c                   endif

     c                   when      excode = 948
     c     chr15key      chain     hbfcdsc
     c                   if        %found
     c                   eval      *in87 = *on
     c                   eval      pdsc(z) = bgcdsc
     c                   else
     c     prccky        chain     xffprocc
     c                   if        not %found(hxpprocc)
     c     bgdtpr        chain     xffprocm
     c                   endif
     c                   if        %found
     c                   eval      *in87 = *on
     c                   eval      pdsc(z) = xfpdsc
     c                   endif
     c                   endif

     c                   when      excode = 950
     c                   eval      prmprc = *blanks
     c                   eval      prvcod = 'PRVDR'
     c                   exsr      srprv
     c                   if        prvvar <> *blanks
     c                   eval      bx24i(z) = 'G2'
     c                   eval      pin(z) = prvvar
     c                   else
     c                   eval      bx24i(z) = *blanks
     c                   eval      pin(z) = *blanks
     c                   endif

     c                   when      excode = 951
     c                   eval      npi(z) = *blanks

     c                   when      excode = 952
     c                   eval      prmprc = *blanks
     c                   eval      prvcod = 'PRVDR'
     c                   exsr      srprv
     c                   if        prvvar <> *blanks
     c                   eval      prprov = 'G2 ' + prvvar
     c                   endif

     c                   when      excode = 956
     c                   exsr      srctab
     c                   eval      tcode = 'BAVR'
     c                   eval      ecode = BBAVR
     c                   exsr      srtabl
     c                   if        tind <> 'E'
     c                   eval      box19 = ldesc
     c                   endif

     c                   when      excode = 963
     c                   eval      box19 = *blanks
     c                   eval      box19x = *blanks
     c                   eval      keyctt= '#'
     c     famlcKey      chain     HAFFAMLC
     c                   if        %found(HALFAMLC)
     c                   exsr      srctab
     c                   eval      tcode = 'B963'
     c                   eval      ecode = AFMRLT
     c                   exsr      srtabl
     c                   if        tind = 'E'
     c                   eval      ldesc = 'Other'
     c                   endif
     c                   callp     XFXBNAM( AFMNAM : lastName : firstName
     c                                    : middleInt : jrSr)
     c****               eval      box19  = 'NAME: ' + %trim(firstName) +
     c****                                 ' ' + %trim(lastName) +
     c****                                 ' REL: ' + %trim(ldesc)
     c****               eval      box19x = 'NAME: ' + %trim(firstName) +
     c****                                 ' ' + %trim(lastName) +
     c****                                 ' REL: ' + %trim(ldesc)
     c                   eval      box19  = 'NAME.' + %trim(firstName) +
     c                                     ' ' + %trim(lastName) +
     c                                     ' REL.' + %trim(ldesc)
     c                   eval      box19x = 'NAME.' + %trim(firstName) +
     c                                     ' ' + %trim(lastName) +
     c                                     ' REL.' + %trim(ldesc)
     c                   endif

     c                   when      excode = 974
     c                   eval      prmprc = BGDTPR
     c                   eval      prvcod = 'NTEAD'
     c                   exsr      srprv
     c                   if        prvvar <> *blanks
     c                   eval      box19 = prvvar
     c                   endif

     C                   ENDSL
     **
     C                   ENDIF
     **
     C                   IF        FOUND = 'N'
     C                             AND COUNT > 0
     **
     C                   EVAL      COUNT = COUNT - 1
     C                   EVAL      BINARY = COUNT
     **
     C                   EVAL      EXPAYR = 0
     C                   EVAL      EXPLAN = 0
     C                   MOVE      '  '          EXFC
     C                   EVAL      EXLVL6 = 0
     C                   MOVEL     '  '          EXSTAT
     **
     C                   TESTB     '7'           VALUEL               07
     C                   IF        *IN07 = *OFF
     C                   EVAL      EXPAYR = SVPAYR
     C                   EVAL      EXPLAN = SVPLAN
     C                   ENDIF
     **
     C                   TESTB     '6'           VALUEL               06
     C                   IF        *IN06 = *OFF
     C                   MOVEL     SVPAYR        EXPAYR
     C                   ENDIF
     **
     C                   TESTB     '5'           VALUEL               05
     C                   IF        *IN05 = *OFF
     C                   MOVEL     SVFC          EXFC
     C                   ENDIF
     **
     C                   TESTB     '4'           VALUEL               04
     C                   IF        *IN04 = *OFF
     C                   EVAL      EXLVL6 = SVLVL6
     C                   ENDIF
     **
     C                   TESTB     '3'           VALUEL               03
     C                   IF        *IN03 = *OFF
     C                   MOVEL     SVSTAT        EXSTAT
     C                   ENDIF
     **
     C                   GOTO      AGAIN
     **
     C                   ENDIF
     **
     C                   ENDSR
     ********************************************************************
     **      subroutine to print current page and advance to new page        **
     **************************************************************************
     C     NEWPAG        BEGSR
     **
     C                   IF        ONETRK = '0'
     C                   MOVE      '1'           ONETRK
     C                   ENDIF
     C                   IF        *IN68 = *OFF
     C                   MOVE      *ON           *IN50
     C                   ELSE
     C                   EVAL      BALDUB = TOTPYR - TOTPMT
     C                   EVAL      BALDUE = TOTCHG - TOTPAT
     C                   EVAL      BILDUE = BILDUE + BALDUE
     C                   EVAL      RPTDUE = RPTDUE + BALDUE
     C                   EVAL      LV6DUE = LV6DUE + BALDUE
     C                   EVAL      PAYDUE = PAYDUE + BALDUE
     C                   EVAL      PLNDUE = PLNDUE + BALDUE
     C                   EVAL      ACCDUE = ACCDUE + BALDUE
     C                   ENDIF
     **
     C****               IF        INS# = 1
     C****               IF        AFC(2) <> '  '                               =no 2nd
     C****                         AND AFC(2) <> 'SP'                           =self pay
     C****                         AND AFC(2) <> 'CT'                           =contract
     C****                         AND AFC(2) <> 'PP'                           =private pay
     C****                         AND TOT(2) = 0
     C****               MOVE      *OFF          *IN24
     C****               MOVE      'X'           BOX11DN
     C****               ELSE
     C****               IF        AFC(3) <> '  '                               =no 3rd
     C****                         AND AFC(3) <> 'SP'                           =self pay
     C****                         AND AFC(3) <> 'CT'                           =contract
     C****                         AND AFC(3) <> 'PP'                           =private pay
     C****                         AND TOT(3) = 0
     C****               MOVE      *OFF          *IN24
     C****               MOVE      'X'           BOX11DN
     C****               ENDIF
     C****               ENDIF
     C****               ENDIF
     **
     C****               IF        INS# = 2
     C****               IF        AFC(1) <> '  '                               =no 2nd
     C****                         AND AFC(1) <> 'SP'                           =self pay
     C****                         AND AFC(1) <> 'CT'                           =contract
     C****                         AND AFC(1) <> 'PP'                           =private pay
     C****                         AND TOT(1) = 0
     C****               MOVE      *OFF          *IN24
     C****               MOVE      'X'           BOX11DN
     C****               ENDIF
     C****               ENDIF
     **
     C****               IF        INS# = 3
     C****               IF        AFC(2) <> '  '                               =no 2nd
     C****                         AND AFC(2) <> 'SP'                           =self pay
     C****                         AND AFC(2) <> 'CT'                           =contract
     C****                         AND AFC(2) <> 'PP'                           =private pay
     C****                         AND TOT(2) = 0
     C****               MOVE      *OFF          *IN24
     C****               MOVE      'X'           BOX11DN
     C****               ELSE
     C****               IF        AFC(1) <> '  '                               =no 3rd
     C****                         AND AFC(1) <> 'SP'                           =self pay
     C****                         AND AFC(1) <> 'CT'                           =contract
     C****                         AND AFC(1) <> 'PP'                           =private pay
     C****                         AND TOT(1) = 0
     C****               MOVE      *OFF          *IN24
     C****               MOVE      'X'           BOX11DN
     C****               ENDIF
     C****               ENDIF
     C****               ENDIF
     **
     c                   eval      todate = bgdtsd
     C     diagky        setgt     hmfpdiag
     C     chgkey        readpe    hmfpdiag
     C                   IF        REGIST = 'Y'
     C                   EVAL      INJDTE = BGIDXD
     C                   ELSE
     C                   EVAL      INJDTE = DXDATE
     C                   ENDIF
     c                   eval      box14q = '431'                               Onset of illness
     **
     C                   MOVE      DTF(1)        FRMDTE
     C                   MOVE      DTT(Z)        TODATE
     C                   eval      prtf = prtfilN
     C                   MOVEL     LDAFOQ        PRTR
     C                   EXSR      SETPTR
     C                   eval      rowN = row
     C                   eval      colN = col
     C                   eval      icrN = icr
     C                   eval      prtf = prtfilO
     C                   MOVEL     LDAFOQ        PRTR
     C                   EXSR      SETPTR
     C                   eval      rowO = row
     C                   eval      colO = col
     C                   eval      icrO = icr
     C                   EVAL      PAGENO = PAGENO + 1
     c                   if        EXC401 = 'Y'
     c                   movel     prprov        prtprv
     c                   endif
     c                   if        ovrlay = 'A' or ovrlay = 'Y'
     C                   eval      col = colO
     C                   eval      row = rowO
     C                   eval      icr = icrO
     C                   WRITE     BILLP2
     c                   endif
     c                   if        ovrlay <> 'Y'
     C                   eval      col = colN
     C                   eval      row = rowN
     C                   eval      icr = icrN
     C                   WRITE     BILLP1
     c                   endif
     **
     c                   if        elecwrite = *off
     c                   exsr      fillelecdata
     c                   eval      elecwrite = *on
     c                   endif
      **
     C                   EXCEPT    DETAIL                                       =print report detail
     **
     c                   if        ovrlay = 'A'
     c                   eval      crtspl = 'A'
     c                   else
     C                   MOVE      'Y'           CRTSPL
     c                   endif
     **
     C                   EVAL      BILPGS = BILPGS + 1
     C                   EVAL      RPTPGS = RPTPGS + 1
     C                   EVAL      LV6PGS = LV6PGS + 1
     C                   EVAL      PAYPGS = PAYPGS + 1
     C                   EVAL      PLNPGS = PLNPGS + 1
     C                   EVAL      ACCPGS = ACCPGS + 1
     **
     C                   MOVE      *OFF          *IN50
     C                   EVAL      DTF = *BLANKS                                =clear
     C                   EVAL      DTT = *BLANKS                                 all
     C                   EVAL      DTF6 = *BLANKS                                detail
     C                   EVAL      DTT6 = *BLANKS                                fields
     C                   EVAL      DTFM = *BLANKS
     C                   EVAL      DTTM = *BLANKS
     C                   EVAL      DTFD = *BLANKS
     C                   EVAL      DTTD = *BLANKS
     C                   EVAL      DTFY = *BLANKS
     C                   EVAL      DTTY = *BLANKS
     C                   EVAL      PBF = 0
     C                   EVAL      PBT = 0
     C                   EVAL      POS = *BLANKS
     C                   EVAL      CPT = *BLANKS
     C                   EVAL      DDX = *BLANKS
     C                   EVAL      MOD = *BLANKS
     C                   EVAL      AMT = 0
     C                   EVAL      QTY = 0
     C                   EVAL      QTY4 = 0
     C                   EVAL      QTY2 = 0
     C                   eval      qty5 = 0
     C                   EVAL      DIX = *BLANKS
     C                   EVAL      PIN = *BLANKS
     c                   eval      bx24i = *blanks
     c                   eval      npi = *blanks
     c                   eval      pdsc = *blanks
     C                   EVAL      Z = 0                                        =clr index
     C                   IF        *IN68 = *ON
     C                   EVAL      TOTPYR = 0                                   =clr payor bal
     C                   EVAL      TOTCHG = 0                                   =clr balance
     C                   EVAL      TOTPMT = 0                                   =clr payment
     C                   EVAL      TOTPAT = 0                                   =clr client share
     C                   ENDIF
     **
     C                   ENDSR
     ********************************************************************
     C     SRINFO        BEGSR
     **         ======    =====
     C                   CALL      'XFXINSIN'
     C                   PARM      'B'           REQSYS
     C                   PARM      BGPLV6        RQLVL6
     C                   PARM      BGACCN        RQACCT
     C                   PARM      BGMRNO        RQMRNO
     C                   PARM                    RQDATE
     C                   PARM      *BLANKS       RQFCL
     C                   PARM      *BLANKS       RQUBC
     C                   PARM      *BLANKS       RQPLC
     C                   PARM      *BLANKS       RQPOL
     C                   PARM      *BLANKS       RQGRP
     C                   PARM      *BLANKS       RQABL
     C                   PARM      *BLANKS       RQVIS
     C                   PARM      *BLANKS       RQEFD
     C                   PARM      *BLANKS       RQETD
     C                   PARM      *BLANKS       RQPRO
     C                   ENDSR
      **************************************************************************
     c     get3ins       begsr

     c     benkey        klist
     c                   kfld                    epayor
     c                   kfld                    eplan

     c     demoikeyE     klist
     c                   kfld                    wrklv6
     c                   kfld                    bbmrno
     c                   kfld                    epayor
     c                   kfld                    eplan
     c                   kfld                    epol

     c                   eval      epayor = ubcode
     c                   eval      eplan  =  ubplan
     c                   eval      epol   = policy
     c     benkey        chain     xffbnfit                           79
     c                   if        *in79 = *off
     c                   movel     insbil        in1typ
     c                   eval      in1nam = xfbnam
     c                   eval      in1ad1 = xfbadr
     c                   eval      in1ad2 = xfbad2
     c                   eval      in1cty = xfbcty
     c                   eval      in1sta = xfbsta
     c                   eval      in1zip = xfbzip
     c                   exsr      srctab
     c                   eval      in1fcl = *blanks
     c                   eval      tcode = 'BCFI'
     c                   eval      ecode = XFBFCL
     c                   exsr      srtabl
     c                   if        tind <> 'E'
     c                   movel     hmap          in1fcl
     c                   endif
     c                   eval      payor = bbpayr
     c                   eval      plan  = bbplan
     c                   eval      prvcod = 'PNPI '                             =Payor National
     c                   eval      prmrev = 0
     c                   eval      prmprc = *blanks
     c                   exsr      srprv                                         Plan Identifier
     c                   if        prvvar <> *blanks
     c                   eval      in1npi = prvvar
     C                   else
     c                   eval      prvcod = 'PAYID'                             =Payor National
     c                   exsr      srprv                                         Plan Identifier
     c                   if        prvvar <> *blanks
     c                   eval      in1npi = prvvar
     c                   endif
     c                   endif

     c     demoikeyE     chain     hafdemoi                           79
     c                   if        *in79 = *off
     c                   eval      in1pol = BDIPLY
     c                   eval      in1inm = BDISNM
     c                   eval      in1sex = BDISSX
     c                   eval      in1bdt = BDISBD
     c                   if        in1sex = ' '
     c     demoky        chain     hafdemo
     c                   if        %found
     c                   eval      in1sex = bdpsex
     c                   endif
     c                   endif
     c                   if        in1bdt = 0
     c     demoky        chain     hafdemo
     c                   if        %found
     c                   eval      in1bdt = bdpbdt
     c                   endif
     c                   endif
     c                   eval      IN1GP# = BDIGRP
     c                   eval      IN1GPN = BDIGNM
     c                   eval      in1rel = *blanks
     c****               eval      in1ss# = bdpss#
     c                   eval      in1ss# = bdiss#
     c                   movel     *blanks       in1rel
     c                   exsr      srctab
     c                   eval      tcode = 'BIR5'
     c                   eval      %subst(ecode:1:2) = bdirlc
     c                   exsr      srtabl
     c                   if        tind <> 'E'
     c                   movel     hmap          in1rel
     c                   else
     c                   eval      in1rel = 'G8'
     c                   endif
     c                   endif
     c                   endif
     c*******************************
     c** secondary insurance (b)   **
     c*******************************
     c     1             do        3             e
     c                   if        (ubcode <> aub(e)
     c                             or ubplan <> apl(e)
     c                             or policy <> apo(e))
     C                   if        afc(e) <> '  '                               =no 2nd
     C                             and afc(e) <> 'SP'                           =self pay
     C                             and afc(e) <> 'CT'                           =contract
     C                             and afc(e) <> 'PP'                           =private pay
     c                   eval      epayor = aub(e)
     c                   eval      eplan = apl(e)
     c                   eval      epol = apo(e)
     c                   eval      Spayor = aub(e)
     c                   eval      Splan = apl(e)
     c                   eval      Spol = apo(e)
     c     benkey        chain     xffbnfit                           79
     c                   if        *in79 = *off
     c                   select
     c                   when      e = 1
     c                   eval      in2typ = 'P'
     c                   when      e = 2
     c                   eval      in2typ = 'S'
     c                   when      e = 3
     c                   eval      in2typ = 'T'
     c                   endsl
     c                   eval      in2nam = xfbnam
     c                   eval      in2ad1 = xfbadr
     c                   eval      in2ad2 = xfbad2
     c                   eval      in2cty = xfbcty
     c                   eval      in2sta = xfbsta
     c                   eval      in2zip = xfbzip
     c                   exsr      srctab
     c                   eval      in2fcl = *blanks
     c                   eval      tcode = 'BCFI'
     c                   eval      ecode = XFBFCL
     c                   exsr      srtabl
     c                   if        tind <> 'E'
     c                   movel     hmap          in2fcl
     c                   endif
     c                   eval      payor = epayor
     c                   eval      plan = eplan
     c                   eval      prvcod = 'PNPI '                             =Payor National
     c                   eval      prmrev = 0
     c                   eval      prmprc = *blanks
     c                   exsr      srprv                                         Plan Identifier
     c                   if        prvvar <> *blanks
     c                   eval      in2npi = prvvar
     C                   else
     c                   eval      prvcod = 'PAYID'                             =Payor National
     c                   exsr      srprv                                         Plan Identifier
     c                   if        prvvar <> *blanks
     c                   eval      in2npi = prvvar
     c                   else
     c                   eval      in2npi = apo(e)
     c                   endif
     c                   endif
     c                   endif
     c     demoikeyE     chain     hafdemoi                           79
     c                   if        *in79 = *off
     c                   eval      in2pol = BDIPLY
     c                   eval      in2inm = BDISNM
     c                   eval      in2sex = BDISSX
     c                   eval      in2bdt = BDISBD
     c                   eval      IN2GP# = BDIGRP
     c                   eval      IN2GPN = BDIGNM
     c                   eval      in2rel = *blanks
     c****               eval      in2ss# = bdpss#
     c                   eval      in2ss# = bdiss#
     c                   movel     *blanks       in2rel
     c                   exsr      srctab
     c                   eval      tcode = 'BIR5'
     c                   eval      %subst(ecode:1:2) = bdirlc
     c                   exsr      srtabl
     c                   if        tind <> 'E'
     c                   movel     hmap          in2rel
     c                   else
     c                   eval      in2rel = 'G8'
     c                   endif
     c                   endif

     c                   leave
     c                   endif
     c                   endif
     c                   enddo
     c*******************************
     c** tertiary insurance (c)    **
     c*******************************
     c     1             do        3             e
     c                   if        (ubcode <> aub(e)
     c                             or ubplan <> apl(e)
     c                             or policy <> apo(e))  and
     c                             (  Spayor <> aub(e)
     c                             or Splan  <> apl(e)
     c                             or Spol  <> apo(e))
     C                   if        afc(e) <> '  '                               =no 2nd
     C                             and afc(e) <> 'SP'                           =self pay
     C                             and afc(e) <> 'CT'                           =contract
     C                             and afc(e) <> 'PP'                           =private pay
     c                   eval      epayor = aub(e)
     c                   eval      eplan = apl(e)
     c                   eval      epol = apo(e)
     c                   select
     c                   when      e = 1
     c                   eval      in3typ = 'P'
     c                   when      e = 2
     c                   eval      in3typ = 'S'
     c                   when      e = 3
     c                   eval      in3typ = 'T'
     c                   endsl
     c     benkey        chain     xffbnfit                           79
     c                   if        *in79 = *off
     c                   eval      in3nam = xfbnam
     c                   eval      in3ad1 = xfbadr
     c                   eval      in3ad2 = xfbad2
     c                   eval      in3cty = xfbcty
     c                   eval      in3sta = xfbsta
     c                   eval      in3zip = xfbzip
     c                   exsr      srctab
     c                   eval      in3fcl = *blanks
     c                   eval      tcode = 'BCFI'
     c                   eval      ecode = XFBFCL
     c                   exsr      srtabl
     c                   if        tind <> 'E'
     c                   movel     hmap          in3fcl
     c                   endif
     c                   eval      payor = epayor
     c                   eval      plan = eplan
     c                   eval      prvcod = 'PNPI '                             =Payor National
     c                   eval      prmrev = 0
     c                   eval      prmprc = *blanks
     c                   exsr      srprv                                         Plan Identifier
     c                   if        prvvar <> *blanks
     c                   eval      in3npi = prvvar
     C                   else
     c                   eval      prvcod = 'PAYID'                             =Payor National
     c                   exsr      srprv                                         Plan Identifier
     c                   if        prvvar <> *blanks
     c                   eval      in3npi = prvvar
     c                   else
     c                   eval      in3npi = apo(e)
     c                   endif
     c                   endif
     c                   endif
     c     demoikeyE     chain     hafdemoi                           79
     c                   if        *in79 = *off
     c                   eval      in3pol = BDIPLY
     c                   eval      in3inm = BDISNM
     c                   eval      in3sex = BDISSX
     c                   eval      in3bdt = BDISBD
     c                   eval      IN3GP# = BDIGRP
     c                   eval      IN3GPN = BDIGNM
     c                   eval      in3rel = *blanks
     c****               eval      in3ss# = bdpss#
     c                   eval      in3ss# = bdiss#
     c                   movel     *blanks       in3rel
     c                   exsr      srctab
     c                   eval      tcode = 'BIR5'
     c                   eval      %subst(ecode:1:2) = bdirlc
     c                   exsr      srtabl
     c                   if        tind <> 'E'
     c                   movel     hmap          in3rel
     c                   else
     c                   eval      in3rel = 'G8'
     c                   endif
     c                   endif

     c                   endif
     c                   endif
     c                   enddo

      *** If Medicare is secondary, we need a code to put in SBR05 based on primary info
     c     aub(1)        chain     xffinsd
     c                   if        %found(hxpinsd)                              =get primary F/C
     c
     c                   select                                                 =determine primary
     c                   when      in1typ = 'P'                                 relationship code
     c                   eval      prmrel = in1rel
     c                   when      in2typ = 'P'
     c                   eval      prmrel = in2rel
     c                   when      in3typ = 'P'
     c                   eval      prmrel = in3rel
     c                   other
     c                   eval      prmrel = *blanks
     c                   endsl

     c                   if        (in1fcl = 'MA' or in1fcl = 'MB')
     c                             and in1typ <> 'P'
     c                   if        prmrel <> '18'
     c                   eval      in1sbr = '12'
     c                   else
     c                   exsr      srctab
     c                   eval      tcode = 'BCFI'
     c                   eval      ecode = xfbfcl
     c                   exsr      srtabl
     c                   if        tind <> 'E'
     c                   eval      in1sbr = sdesc
     c                   endif
     c                   endif
     c                   endif

     c                   if        (in2fcl = 'MA' or in2fcl = 'MB')
     c                             and in2typ <> 'P'
     c                   if        prmrel <> '18'
     c                   eval      in2sbr = '12'
     c                   else
     c                   exsr      srctab
     c                   eval      tcode = 'BCFI'
     c                   eval      ecode = xfbfcl
     c                   exsr      srtabl
     c                   if        tind <> 'E'
     c                   eval      in2sbr = sdesc
     c                   endif
     c                   endif
     c                   endif

     c                   if        (in3fcl = 'MA' or in3fcl = 'MB')
     c                             and in3typ <> 'P'
     c                   if        prmrel <> '18'
     c                   eval      in3sbr = '12'
     c                   else
     c                   exsr      srctab
     c                   eval      tcode = 'BCFI'
     c                   eval      ecode = xfbfcl
     c                   exsr      srtabl
     c                   if        tind <> 'E'
     c                   eval      in3sbr = sdesc
     c                   endif
     c                   endif
     c                   endif
     c                   endif

     c                   endsr
      **************************************************************************
     C     SRUCNV        BEGSR
     **
     C                   CALL      'XFXUNCNV'
     C                   PARM      BBPLV6        PRMLV6
     C                   PARM                    PRMPRC
     C                   PARM      'I'           PRMOPT
     C                   PARM                    PRMINU
     C                   PARM                    PRMFRU
     C                   PARM      BGDTSD        PRMSDT
     **
     C                   ENDSR
     **************************************************************************
     **      subroutine to retrieve payor/plan default information           **
     **************************************************************************

      /copy copysrc,hxxppdsr

     *****************************************************************
     C     SRPTCD        BEGSR                                                  =alternate
     **
     c                   eval      modOvr = *on
     **
     C                   CALL      'XFXPRTCD'                                    print codes
     C                   PARM      BBPLV6        LEVEL6                         =level 6
     C                   PARM      BBACCN        RQACCT                         =account #
     C                   PARM      BGDTSD        RQSSVD                         =service date
     C                   PARM      BBPAYR        RQSPYR                         =ins payor #
     C                   PARM      BBPLAN        RQSPLN                         =ins plan #
     C                   PARM      BGDCT1        RQSCT1                         =category 1
     C                   PARM      CAT2          RQSCT2                         =category 2
     C                   PARM      BGDCT3        RQSCT3                         =category 3
     C                   PARM      BGDTPR        RQSPRC                         =procedure code
     C                   PARM      BGDSEQ        RQCSEQ                         =chg. seq. #
     C                   PARM      BGDVTP        RQSVTP                         =visit type
     C                   PARM      'Y'           DFTFLG                         =default flag
     C                   PARM                    CPT(Z)                         =alt print code
     C                   parm                    prtmd1
     C                   parm                    prtmd2
     C                   parm                    prtmd3
     C                   parm                    prtmd4
     C                   parm      0             prtrev
     **
     c                   if        prtmd1 <> *blanks
     c                              or sndf38 = 'Y' or sndf38 = 'O'
     c     chrgKey       chain     hbfchrg
     c                   if        %found
      **
     c                   if        bgcmod = *blanks
     c                   if        prtmd1 <> *blanks
     c                              or sndf38 = 'Y' or sndf38 = 'O'
     c                   eval      bgdmod = prtmd1
     c                   eval      modOvr = *on
     c                   endif
     c                   if        prtmd2 <> *blanks
     c                              or sndf38 = 'Y' or sndf38 = 'O'
     c                   eval      bgdmd2 = prtmd2
     c                   eval      modOvr = *on
     c                   endif
     c                   if        prtmd3 <> *blanks
     c                              or sndf38 = 'Y' or sndf38 = 'O'
     c                   eval      bgdmd3 = prtmd3
     c                   eval      modOvr = *on
     c                   endif
     c                   if        prtmd4 <> *blanks
     c                              or sndf38 = 'Y' or sndf38 = 'O'
     c                   eval      bgdmd4 = prtmd4
     c                   eval      modOvr = *on
     c                   endif
     c                   endif
     c                   endif
     c                   endif
     **
     C                   IF        BBPAYR = 45                                  =yes...
     C                             AND BBPLAN = 1                               =this is a rig
     C                             AND BGDTSD < 20031001                        =job.
     C                   EXSR      SRCTAB
     C                   MOVEL     'BMCC'        TCODE
     C                   MOVEL     BGDTPR        ECODE
     C                   EXSR      SRTABL
     C                   IF        TIND <> 'E'
     C                   MOVEL     LDESC         CPT(Z)
     C                   ENDIF
     C                   ENDIF
     **
     *** temporary fix for maricopa.
     C                   IF        BBPAYR = 45
     C                             AND BBPLAN = 1
     C                             AND BGDTSD >= 20031001
     C                   SELECT
     C                   WHEN      BGDTPR = 'PERC'
     C                   MOVEL     'T1019'       CPT(Z)
     C                   WHEN      BGDTPR = 'PEST'
     C                   MOVEL     'S5165'       CPT(Z)
     C                   WHEN      BGDTPR = 'CHOR'
     C                   MOVEL     'S5130'       CPT(Z)
     C                   WHEN      BGDTPR = 'HMKR'
     C                   MOVEL     'S5130'       CPT(Z)
     C                   WHEN      BGDTPR = 'RSPT'
     C                   MOVEL     'S5150'       CPT(Z)
     C                   ENDSL
     C                   ENDIF
     **
     C                   ENDSR
     **************************************************************************
     **               file to calendar date conversion              **
     *****************************************************************
     C     SRCYMD        BEGSR
     C                   CALL      'XFXCYMD'
     C                   PARM                    YDATE
     C                   PARM                    MDATE
     C                   ENDSR
     *****************************************************************
     C     SRCMDY        BEGSR
     C                   CALL      'XFXCMDY'
     C                   PARM                    MDATE
     C                   PARM                    YDATE
     C                   ENDSR
     *****************************************************************
     C     SRADDD        BEGSR
     C                   CALL      'XFXADDD'
     C                   PARM                    YDATE1
     C                   PARM                    YDAYS
     C                   PARM                    YDATE2
     C                   ENDSR
     *****************************************************************
     C     GTEOM         BEGSR
     C                   CALL      'XFXGTEOM'
     C                   PARM                    CMOYR
     C                   PARM                    MDATE
     C                   ENDSR
     *****************************************************************
     **                     clear table fields                      **
     *****************************************************************
     C     SRCTAB        BEGSR
     **         ======    =====
     C                   EVAL      TCODE = *BLANKS
     C                   EVAL      ECODE = *BLANKS
     C                   EVAL      HMAP = *BLANKS
     C                   EVAL      EDATE = 0
     C                   EVAL      SDESC = *BLANKS
     C                   EVAL      LDESC = *BLANKS
     C                   EVAL      TIND = *BLANKS
     C                   ENDSR
     *****************************************************************
     **                        table search                         **
     *****************************************************************
     C     SRTABL        BEGSR
     **         ======    =====
     C                   CALL      'XFXTABL'
     C                   PARM                    TCODE
     C                   PARM                    ECODE
     C                   PARM                    HMAP
     C                   PARM                    EDATE
     C                   PARM                    SDESC
     C                   PARM                    LDESC
     C                   PARM                    TIND
     **
     C                   ENDSR
     *****************************************************************
     C     RMVBLK        BEGSR
     **
     C                   EVAL      A = %CHECK('0':LEVL)
     C                   IF        A <> 0
     C                   EVAL      LEVL = %SUBST(LEVL:A)
     C                   ELSE
     C                   EVAL      LEVL = *BLANKS
     C                   ENDIF
     **
     C                   ENDSR
     *****************************************************************
     C     GETSPD        BEGSR
     **
     C                   IF        EXCODE = 323
     C                   MOVEL     DTF(Z)        MDATE
     C                   EXSR      SRCMDY
     C                   ENDIF
     **
     C                   EVAL      SPDDAT = 0
     C     ICOVK3        SETGT     HBFICOV                                      =get spend down
     C                   DOU       *IN71 = *ON                                   date from most
     C     ICOVK4        READPE    HBFICOV                                71     recent verification
     C                   IF        *IN71 = *OFF                                  for charge
     C                   IF        EXCODE = 70
     C                   IF        BCVSED >= BBFRDT
     C                   EVAL      YDATE1 = BCVSED
     C                   EVAL      YDAYS = 1
     C                   EXSR      SRADDD
     C                   EVAL      SPDDAT = YDATE2
     C                   ENDIF
     C                   ENDIF
     C                   IF        EXCODE = 323
     C                   IF        BCVSED >= YDATE
     C                   EVAL      YDATE1 = BCVSED
     C                   EVAL      YDAYS = 1
     C                   EXSR      SRADDD
     C                   EVAL      SPDDAT = YDATE2
     C                   ENDIF
     C                   ENDIF
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     **
     C                   IF        SPDDAT = 0
     C                   IF        EXCODE = 70
     C                   EVAL      SPDDAT = BBFRDT                               date if no spend
     C                   ENDIF
     C                   IF        EXCODE = 323
     C                   EVAL      SPDDAT = YDATE
     C                   ENDIF
     C                   ENDIF                                                   down date found
     **
     C                   ENDSR
     *****************************************************************
     C     SRCGBL        BEGSR
     **
     C                   CALL      'XFXCKBLS'
     C                   PARM                    BGDLV6
     C                   PARM                    BGDTAC
     C                   PARM                    BGDSEQ
     C                   PARM      BBTRAK        BILTRK
     C                   PARM                    ACTION
     **
     C                   ENDSR
     *****************************************************************
     **                   provider number lookup                    **
     *****************************************************************
     C     SRPRV         BEGSR
     **
     c                   callp     XFXPRV( bbplv6  : bbaccn  : bbpayr : bbplan
     c                                   : prvcod  : prmprc  : 0      : bbtodt
     c                                   : prvvar  : prvadr )
     **
     C                   ENDSR
     *****************************************************************
     **                   place of service default                  **
     *****************************************************************
     C     SRPOSD        BEGSR
     **
     C                   CALL      'HBXPOSD'                                     place of serv.dflt
     C                   PARM      BBPAYR        RQSPYR                         =ins payor #
     C                   PARM      BBPLAN        RQSPLN                         =ins plan #
     C                   PARM      BGDCT1        RQSCT1                         =category 1
     C                   PARM      BGDCT2        RQSCT2                         =category 2
     C                   PARM      BGDCT3        RQSCT3                         =category 3
     C                   PARM      BGDTPR        RQSPRC                         =procedure code
     C                   PARM                    POS(Z)                         =dflt place of servc
     **
     C                   ENDSR
     **************************************************************************
     **  set up the rw,cl, and icr arrays up                           **
     ********************************************************************
     C     SETPTR        BEGSR
     **
     c                   eval      row = 0
     c                   eval      col = 0
     c                   eval      icr = 0
     **
     C     PRTKEY        CHAIN     HXFPRRC                            79
     C                   IF        *IN79 = *OFF
     C     PRTKEY        SETLL     HXFPRRC
     C                   DOU       *IN78 = *ON
     C     PRTKEY        READE     HXFPRRC                                78
     C                   IF        *IN78 = *OFF
     C                   EVAL      CT = XPSEQN
     C                   EVAL      ROW(CT) = XPROW
     C                   EVAL      COL(CT) = XPCOL
     C                   EVAL      ICR(CT) = XPINCR
     C                   ENDIF
     C                   ENDDO
     C                   ELSE
     C     PRTF          SETLL     HXFPRRCD
     C                   DOU       *IN78 = *ON
     C     PRTF          READE     HXFPRRCD                               78
     C                   IF        *IN78 = *OFF
     C                   EVAL      CT = XPSEQN
     C                   EVAL      ROW(CT) = XPROW
     C                   EVAL      COL(CT) = XPCOL
     C                   EVAL      ICR(CT) = XPINCR
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
     **
     C                   ENDSR
     ********************************************************************
     C     CHKMOD        BEGSR
     **         ======    =====
     C                   MOVEL     'N'           NONCPT
     C                   MOVEL     'N'           CPT4
     C                   MOVEL     CPT(Z)        WRKCPT
     **
     C     WRKCPT        CHAIN     HBFCPT4                            79
     C                   IF        *IN79 = *OFF
     C                   MOVEL     'Y'           CPT4
     C                   ENDIF
     C     WRKCPT        CHAIN     HBFNCPT4                           79
     C                   IF        *IN79 = *OFF
     C                   MOVEL     'Y'           NONCPT
     C                   ENDIF
     **
     C                   SELECT
     C                   WHEN      CPT4 = 'Y'                                   =cpt4
     C                   IF        BGDMOD <> *BLANKS                            =modifier 1
     C     BGDMOD        CHAIN     HBFCPMD                            79
     C                   IF        *IN79 = *OFF
     C                   MOVEL     BGDMOD        MOD(Z)
     C                   ENDIF
     C                   ENDIF
     C                   IF        BGDMD2 <> *BLANKS
     C     BGDMD2        CHAIN     HBFCPMD                            79
     C                   IF        *IN79 = *OFF
     C                   CAT       BGDMD2:0      MOD(Z)
     C                   ENDIF
     C                   ENDIF
     C                   IF        BGDMD3 <> *BLANKS
     C     BGDMD3        CHAIN     HBFCPMD                            79
     C                   IF        *IN79 = *OFF
     C                   CAT       BGDMD3:0      MOD(Z)
     C                   ENDIF
     C                   ENDIF
     C                   IF        BGDMD4 <> *BLANKS
     C     BGDMD4        CHAIN     HBFCPMD                            79
     C                   IF        *IN79 = *OFF
     C                   CAT       BGDMD4:0      MOD(Z)
     C                   ENDIF
     C                   ENDIF
     **
     C                   WHEN      NONCPT = 'Y'                                 =non cpt4
     C                   IF        BGDMOD <> *BLANKS                            =modifier 1
     C     BGDMOD        CHAIN     HBFNCPMD                           79
     C                   IF        *IN79 = *OFF
     C                   MOVEL     BGDMOD        MOD(Z)
     C                   ENDIF
     C                   ENDIF
     C                   IF        BGDMD2 <> *BLANKS
     C     BGDMD2        CHAIN     HBFNCPMD                           79
     C                   IF        *IN79 = *OFF
     C                   CAT       BGDMD2:0      MOD(Z)
     C                   ENDIF
     C                   ENDIF
     C                   IF        BGDMD3 <> *BLANKS
     C     BGDMD3        CHAIN     HBFNCPMD                           79
     C                   IF        *IN79 = *OFF
     C                   CAT       BGDMD3:0      MOD(Z)
     C                   ENDIF
     C                   ENDIF
     C                   IF        BGDMD4 <> *BLANKS
     C     BGDMD4        CHAIN     HBFNCPMD                           79
     C                   IF        *IN79 = *OFF
     C                   CAT       BGDMD4:0      MOD(Z)
     C                   ENDIF
     C                   ENDIF
     C                   ENDSL
     **
     C                   ENDSR
     *****************************************************************
     **         align decimal point for display
     *****************************************************************
     C     ALIGN         BEGSR
     **
     C                   IF        DIAN(5) = '.'
     C                             AND DIAD(1) = 'V'
     C                   MOVEL     DIAN(6)       DIAN(7)
     C                   MOVEL     DIAN(4)       DIAN(6)
     C                   MOVEL     DIAN(3)       DIAN(4)
     C                   MOVEL     DIAN(2)       DIAN(3)
     C                   MOVEL     DIAN(1)       DIAN(2)
     C                   EVAL      DIAN(1) = *BLANKS
     C                   MOVEL     DIAD(6)       DIAD(7)
     C                   MOVEL     DIAD(4)       DIAD(6)
     C                   MOVEL     DIAD(3)       DIAD(4)
     C                   MOVEL     DIAD(2)       DIAD(3)
     C                   MOVEL     DIAD(1)       DIAD(2)
     C                   EVAL      DIAD(1) = *BLANKS
     C                   ELSE
     C                   EXSR      RMVDEC
     C                   EVAL      DIAD = *BLANKS
     **
     C                   IF        DIAN(1) = 'E'
     C                   MOVE      DIAN(1)       DIAD(1)
     C                   EVAL      V = 2
     C                   ELSE
     C                   IF        DIAN(1) = 'V'
     C                   MOVE      ' '           DIAD(1)
     C                   MOVE      DIAN(1)       DIAD(2)
     C                   EVAL      V = 3
     C                   ELSE
     C                   MOVE      DIAN(1)       DIAD(2)
     C                   EVAL      V = 3
     C                   ENDIF
     C                   ENDIF
     **
     C                   FOR       Y = 2 TO 7
     C                   IF        V <= 7
     C                   MOVE      DIAN(Y)       DIAD(V)
     C                   EVAL      V = V + 1
     C                   IF        V = 5
     C                   MOVE      '.'           DIAD(V)
     C                   EVAL      V = V + 1
     C                   ENDIF
     C                   ENDIF
     C                   ENDFOR
     **
     C                   ENDIF
     **
     C                   ENDSR
     *****************************************************************
     **         strip diagnosis decimal point
     *****************************************************************
     C     RMVDEC        BEGSR
     **
     C                   EVAL      V = 1
     C                   EVAL      DIAN = *BLANKS
     C                   FOR       Y = 1 TO 7
     C                   IF        DIAD(Y) <> '.'
     C                             AND DIAD(Y) <> ' '
     C                   MOVE      DIAD(Y)       DIAN(V)
     C                   EVAL      V = V + 1
     C                   ENDIF
     C                   ENDFOR
     **
     C                   ENDSR
     **********************************************************************
     ***        get capped contract definitions                         ***
     **********************************************************************
     C     GETCAP        BEGSR
     **
     C                   EVAL      WKCAT1 = *BLANKS
     C                   EVAL      WKCAT2 = *BLANKS
     **
     C                   CALL      'HXCMNGC'
     C                   PARM                    BGDLV6
     C                   PARM                    BGDTAC
     C                   PARM      0             BILRAT
     C                   PARM                    BGDTSD
     C                   PARM                    BGDTPR
     C                   PARM                    BGDTQT
     C                   PARM      0             CHGSEQ
     C                   PARM                    BGDAPP
     C                   PARM      0             RQFRTM
     C                   PARM      0             RQTOTM
     C                   PARM      *blanks       RTTYPE
     C                   PARM      'N'           PRECHG                         =pre-existing charge
     C                   PARM      BGDTTH        DOCNUM
     C                   PARM                    CHGTCK
     C                   PARM      0             RQPAYR                                        harge
     C                   PARM      0             RQPLAN
     C                   PARM      *BLANKS       RQPLCY
     C                   PARM      *BLANKS       RQREIM
     C                   parm                    dupmngc
     **
     C     PRMKEY        SETLL     HBFDIEM
     C                   DOU       *IN70 = *ON
     C     PRMKEY        READE     HBFDIEM                                70
     C                   IF        *IN70 = *OFF
     C                             AND BGDTSD >= XFFDT
     C                             AND BGDTSD <= XFTDT
     C                             AND XFDLT = *BLANKS
     C                   IF        XFCT1 = 'X'
     C                   MOVEL     BGDCT1        WKCAT1
     C                   ENDIF
     C                   IF        XFCT2 = 'X'
     C                   MOVEL     BGDCT2        WKCAT2
     C                   ENDIF
     C                   LEAVE
     C                   ELSE
     C                   LEAVESR
     C                   ENDIF
     C                   ENDDO
     **
     C                   MOVE      'N'           RQCAP
     C                   EVAL      CAPHRS = 0
     C                   EVAL      CAPCDE = *BLANKS
     **
     C                   CALL      'HBXSTEPD'
     C                   PARM                    BBPAYR
     C                   PARM                    BBPLAN
     C                   PARM                    XFTDT
     C                   PARM                    WKCAT1
     C                   PARM                    WKCAT2
     C                   PARM      RQREIM        METHOD
     C                   PARM                    BGDTPR
     C                   PARM                    CAPHRS
     C                   PARM                    CAPCDE
     c                   parm      *blanks       capmd1
     c                   parm      *blanks       capmd2
     c                   parm      *blanks       capmd3
     c                   parm      *blanks       capmd4
     c                   parm      0             caprev
     C                   PARM                    RQCAP
     C                   PARM      *BLANKS       MIDNIT
     C                   PARM      *BLANKS       CONTIN
     **
     C                   IF        RQCAP = 'Y'
     C                   EVAL      WRKTQT = BGDTQT / 4
     C                   IF        WRKTQT >= CAPHRS
     C                   MOVEL     CAPCDE        CPT(Z)
     c                   if        mod(z) = *blanks and capmd1 <> *blanks
     c                   eval      mod(z) = capmd1 + ' ' + capmd2 + ' '
     c                                    + capmd3 + ' ' + capmd4
     c                   endif
     C                   EVAL      QTY(Z) = 1
     C                   EVAL      QTY4(Z) = 1
     C                   EVAL      QTY2(Z) = 1
     C                   eval      qty5(z) = 1
     C                   ENDIF
     C                   ENDIF
     **
     C     SKPCAP        ENDSR
     *****************************************************************
     C     SRASGN        BEGSR
     **         ======    ======
     **
    c                   if        bgdsdt < bbtodt
    c                             and bgdsdt <> 0
     C                   CALL      'XFXMASGN'
     C                   PARM      BBPLV6        LV6#                           =level 6
     C                   PARM      BBACCN        ACCNT#                         =acct#
     C                   PARM                    STATUS                         =status
     C                   PARM      bgdsdt        RQSDTE                         =end date
     C                   PARM                    DOC#                           =doctor #        
    c                   else
     C                   CALL      'XFXMASGN'
     C                   PARM      BBPLV6        LV6#                           =level 6
     C                   PARM      BBACCN        ACCNT#                         =acct#
     C                   PARM                    STATUS                         =status
     C                   PARM      BBTODT        RQSDTE                         =end date
     C                   PARM                    DOC#                           =doctor #
     C                   endif
     **
     C                   EVAL      MSGDR# = DOC#
     **
     C                   ENDSR
     *****************************************************************
     C     SROUND        BEGSR
     **
     C                   EVAL      WRKQTY = QTY(Z)
     C                   CALL      'XFXROUND'
     C                   PARM                    WRKQTY
     C                   PARM                    XFPTIM
     C                   PARM                    TOTAL
     C                   Z-ADD     TOTAL         QTY(Z)
     C                   EVAL      QTY4(Z) = TOTAL
     C                   Z-ADD     TOTAL         QTY2(Z)
     C                   eval      qty5(z) = total
     C                   MOVEL     'N'           ROUND
     **
     C                   ENDSR
     *****************************************************************
     c     getproc       begsr
     **
     c     chgbky        chain     hbfchgbl                           79
     c                   if        *in79 = *off
     c     chgky2        chain     hbfchrg                            79
     c                   if        *in79 = *off
     c                   eval      prmprc = bgctpr
     c                   eval      fexct2 = bgcct2
     c                   eval      fexprc = bgctpr
     c                   eval      fextth = bgctth
     c                   endif
     c                   endif
     **
     c                   ENDSR
     *****************************************************************
     C     SRIVER        BEGSR
     **         ======    =====
     C                   CALL      'XFXVERS'
     C                   PARM                    BBPLV6
     C                   PARM                    BBACCN
     C                   PARM                    BBRANK
     C                   PARM                    BBPAYR
     C                   PARM                    BBPLAN
     C                   PARM                    BBPLCY
     C                   PARM                    BGDTPR
     C                   PARM                    BGDTSD
     C                   PARM      0             QUANTY
     **
     C                   ENDSR
     *****************************************************************
     c     multidx       begsr
      **         ======    =====
     c                   eval      mltdix = *blanks
     c                   movel     savprc        prmprc
     c                   movel     'MLTDX'       prvcod
     c                   exsr      srprv
B01  c     prvvar        ifne      *blanks
     c                   movel     prvvar        mltidiag
B02  c                   if        mdiag1c = *blanks
     c                   eval      mdiag1 = 0
E02  c                   endif
B02  c                   if        mdiag2c = *blanks
     c                   eval      mdiag2 = 0
E02  c                   endif
B02  c                   if        mdiag3c = *blanks
     c                   eval      mdiag3 = 0
E02  c                   endif
B02  c                   if        mdiag4c = *blanks
     c                   eval      mdiag4 = 0
E02  c                   endif
B02  c                   if        mdiag5c = *blanks
     c                   eval      mdiag5 = 0
E02  c                   endif
B02  c                   if        mdiag6c = *blanks
     c                   eval      mdiag6 = 0
E02  c                   endif
B02  c                   if        mdiag7c = *blanks
     c                   eval      mdiag7 = 0
E02  c                   endif
B02  c                   if        mdiag8c = *blanks
     c                   eval      mdiag8 = 0
E02  c                   endif
B02  c                   if        mdiag9c = *blanks
     c                   eval      mdiag9 = 0
E02  c                   endif
B02  c                   if        mdiag10c = *blanks
     c                   eval      mdiag10 = 0
E02  c                   endif
B02  c                   if        mdiag11c = *blanks
     c                   eval      mdiag11 = 0
E02  c                   endif
B02  c                   if        mdiag12c = *blanks
     c                   eval      mdiag12 = 0
E02  c                   endif
      **
     c                   eval      x = 0
     c                   callp     xfxdiag( bbplv6 : bbaccn : bbtodt : diag
     c                                    : poaf   : bodyp   : vers  )
     c                   eval      dxv = *blanks
     c                   eval      dxc = *blanks

B03  c                   if        mdiag1 > 0
     c                   if        dxs(mdiag1) <> *blanks
     c                   eval      x += 1
     c                   eval      dxc(x) = dxs(mdiag1)
     c                   callp     XFXDXPER(dxs(mdiag1) : icdver : dxv(x))
     c                   eval      chrgdxs(x) = dxc(x)
     c                   endif
     c                   endif
     c
B03  c                   if        mdiag2 > 0
     c                   if        dxs(mdiag2) <> *blanks
     c                   eval      x += 1
     c                   eval      dxc(x) = dxs(mdiag2)
     c                   callp     XFXDXPER(dxs(mdiag2) : icdver : dxv(x))
     c                   eval      chrgdxs(x) = dxc(x)
     c                   endif
     c                   endif

B03  c                   if        mdiag3 > 0
     c                   if        dxs(mdiag3) <> *blanks
     c                   eval      x += 1
     c                   eval      dxc(x) = dxs(mdiag3)
     c                   callp     XFXDXPER(dxs(mdiag3) : icdver : dxv(x))
     c                   eval      chrgdxs(x) = dxc(x)
     c                   endif
     c                   endif

B03  c                   if        mdiag4 > 0
     c                   if        dxs(mdiag4) <> *blanks
     c                   eval      x += 1
     c                   eval      dxc(x) = dxs(mdiag4)
     c                   callp     XFXDXPER(dxs(mdiag4) : icdver : dxv(x))
     c                   eval      chrgdxs(x) = dxc(x)
     c                   endif
     c                   endif

B03  c                   if        mdiag5 > 0
     c                   if        dxs(mdiag5) <> *blanks
     c                   eval      x += 1
     c                   eval      dxc(x) = dxs(mdiag5)
     c                   callp     XFXDXPER(dxs(mdiag5) : icdver : dxv(x))
     c                   eval      chrgdxs(x) = dxc(x)
     c                   endif
     c                   endif

B03  c                   if        mdiag6 > 0
     c                   if        dxs(mdiag6) <> *blanks
     c                   eval      x += 1
     c                   eval      dxc(x) = dxs(mdiag6)
     c                   callp     XFXDXPER(dxs(mdiag6) : icdver : dxv(x))
     c                   eval      chrgdxs(x) = dxc(x)
     c                   endif
     c                   endif

B03  c                   if        mdiag7 > 0
     c                   if        dxs(mdiag7) <> *blanks
     c                   eval      x += 1
     c                   eval      dxc(x) = dxs(mdiag7)
     c                   callp     XFXDXPER(dxs(mdiag7) : icdver : dxv(x))
     c                   eval      chrgdxs(x) = dxc(x)
     c                   endif
     c                   endif

B03  c                   if        mdiag8 > 0
     c                   if        dxs(mdiag8) <> *blanks
     c                   eval      x += 1
     c                   eval      dxc(x) = dxs(mdiag8)
     c                   callp     XFXDXPER(dxs(mdiag8) : icdver : dxv(x))
     c                   eval      chrgdxs(x) = dxc(x)
     c                   endif
     c                   endif

B03  c                   if        mdiag9 > 0
     c                   if        dxs(mdiag9) <> *blanks
     c                   eval      x += 1
     c                   eval      dxc(x) = dxs(mdiag9)
     c                   callp     XFXDXPER(dxs(mdiag9) : icdver : dxv(x))
     c                   eval      chrgdxs(x) = dxc(x)
     c                   endif
     c                   endif

B03  c                   if        mdiag10 > 0
     c                   if        dxs(mdiag10) <> *blanks
     c                   eval      x += 1
     c                   eval      dxc(x) = dxs(mdiag10)
     c                   callp     XFXDXPER(dxs(mdiag10) : icdver : dxv(x))
     c                   eval      chrgdxs(x) = dxc(x)
     c                   endif
     c                   endif

B03  c                   if        mdiag11 > 0
     c                   if        dxs(mdiag11) <> *blanks
     c                   eval      x += 1
     c                   eval      dxc(x) = dxs(mdiag11)
     c                   callp     XFXDXPER(dxs(mdiag11) : icdver : dxv(x))
     c                   eval      chrgdxs(x) = dxc(x)
     c                   endif
     c                   endif

B03  c                   if        mdiag12 > 0
     c                   if        dxs(mdiag12) <> *blanks
     c                   eval      x += 1
     c                   eval      dxc(x) = dxs(mdiag12)
     c                   callp     XFXDXPER(dxs(mdiag12) : icdver : dxv(x))
     c                   eval      chrgdxs(x) = dxc(x)
     c                   endif
     c                   endif

BS3  c                   select
     c                   when      x = 1
     c                   eval      mltdix = '1              '
     c                   when      x = 2
     c                   eval      mltdix = '12             '
     c                   when      x = 3
     c                   eval      mltdix = '123            '
     c                   when      x = 4
     c                   eval      mltdix = '1234           '
     c                   when      x = 5
     c                   eval      mltdix = '12345          '
     c                   when      x = 6
     c                   eval      mltdix = '123456         '
     c                   when      x = 7
     c                   eval      mltdix = '1234567        '
     c                   when      x = 8
     c                   eval      mltdix = '12345678       '
     c                   when      x = 9
     c                   eval      mltdix = '123456789      '
     c                   when      x = 10
     c                   eval      mltdix = '12345678910    '
     c                   when      x = 11
     c                   eval      mltdix = '1234567891011  '
     c                   when      x = 12
     c                   eval      mltdix = '123456789101112'
ES3  c                   endsl
E01  c                   endif
      **
     c     endmldx       ENDSR
     *****************************************************************
     C     BE604XX       BEGSR
     **         ======    =====
     C                   IF        BGDTQT <> 0
     c                   Z-ADD     BGDTQT        BGDTQTMP
     C                   ELSE
     c                   Z-ADD     1             BGDTQTMP
     C                   ENDIF

     c                   IF        BILEXC452 = *ON
     c     FACTOR        MULT      BGDTQTMP      EX452Q
     c                   ADD       EX452Q        QTY(Z)
     c                   EVAL      QTY4(Z) = QTY4(Z) + EX452Q
     c                   ADD       EX452Q        QTY2(Z)
     c                   eval      qty5(z) += ex452q
     c                   MOVE      *ON           *IN74
     c****               ELSE

     c                   elseif    BILEXC463 = *ON
     c     FACTOR        MULT      BGDTQTMP      EX463W
     c                   EVAL(H)   EX463Q = EX463W
     c                   ADD       EX463Q        QTY(Z)
     c                   EVAL      QTY4(Z) = QTY4(Z) + EX463Q
     c                   ADD       EX463Q        QTY2(Z)
     c                   eval      qty5(z) += ex463Q

     c                   elseif    bilexc940 = *on
     c     factor        mult      bgdtqtmp      ex940w
     c                   eval(h)   ex940q = ex940w
     c                   add       ex940q        qty(z)
     c                   add       ex940q        qty4(z)
     c                   add       ex940q        qty2(z)
     c                   add       ex940q        qty5(z)
     c                   endif
     **
     C     END604XX      ENDSR
     *****************************************************************
     OPRINTER   H    OA                     1 02
     O                                           12 'REPORT DATE:'
     O                       RUNDTE              24 '  /  /    '
     O                       L1NAME             119
     O                                          193 'PAGE:'
     O                       PAGE2         4    198
     *---------------------------------------------------------------*
     O          H    OA                     1
     O                                           12 'REPORT TIME:'
     O                       RUNTME              22 '  :  :  '
     O               82                         105 'APPROVED ELECTRONIC'
     O               82                         112 ' CLAIMS'
     O               83                         109 'APPROVED MANUAL CLAIMS'
     O               84                         105 'MANUAL CLAIMS NEEDING'
     O               84                         114 ' APPROVAL'
     O               85                         106 'ELECTRONIC CLAIMS NEEDIN'
     O               85                         116 'G APPROVAL'
     O                       PGMNAM             198
     *---------------------------------------------------------------*
     O          H    OA                     1
     O                                           12 'REPORT TYPE:'
     O                       rpttype             21
     O                       L5NAME             119
     *---------------------------------------------------------------*
     O          H    OA                     2
     O                       L6NAME             119
     *---------------------------------------------------------------*
     O          H    OA                     2
     O               U7                         107 '***** SCRUB *****'
     O              NU7                         107 '***** FINAL *****'
     *---------------------------------------------------------------*
     O          H    OA                     1
     O                                           96 ' PRIOR                  '
     O                                          120 '  REFERRING             '
     O                                          144 '                TOTAL   '
     O                                          168 '    DUE FROM    BALANCE '
     *---------------------------------------------------------------*
     O          H    OA                     0
     O                                           24 '____________  __________'
     O                                           48 '________________  ______'
     O                                           72 '_________  _______  ____'
     O                                           96 '_____________  _________'
     O                                          120 '  ____________  ________'
     O                                          144 '__  __________  ________'
     O                                          168 '__  __________  ________'
     O                                          192 '__  ______________      '
     *---------------------------------------------------------------*
     O          H    OA                     1
     O                                           24 'ACCOUNT #     NAME      '
     O                                           48 '                  PROVID'
     O                                           72 'ER #       DX CODE  AUTH'
     O                                           96 'ORIZATION      RESOURCE#'
     O                                          120 '  PHYSICIAN ID  FROM DAT'
     O                                          144 'E   TO DATE     CHARGES '
     O                                          168 '    CLIENT      DUE     '
     O                                          192 '      TRACKING #        '
     *---------------------------------------------------------------*
     O          EF           DETAIL         1
     O                       BBACCN        4     12
     O                       BBNAME              40
     O                       BBPROV              57
     O                       DXC(1)              65
     O                       PRAUTH              87
     O                       PRRESC              96
     O                       QUA17a              98
     O                       PRRUPN             110
     O                       FRMDTE             122 '  /  /    '
     O                       TODATE             134 '  /  /    '
     O                       TOTCHG        L    146
     O                       TOTPAT        L    158
     O                       BALDUE        L    170
     O                       BOX26              186
     *---------------------------------------------------------------*
     O          EF           BILTOT         2
     O                                           10 'PAYOR/PLAN'
     O                       BBPAYR        4     17
     O                                           18 '/'
     O                       BBPLAN        4     23
     O                                           36 'BILL TOTAL:'
     O                       BILPGS        3     43
     O                       BILCHG        L    146
     O                       BILPAT        L    158
     O                       BILDUE        L    170
     *---------------------------------------------------------------*
     O          EF           ACCTOT         2
     O                                            5 'ACCT '
     O                       BBACCN        4     17
     O                                           24 'TOTAL:'
     O                       ACCPGS        3     30
     O                       ACCCHG        L    146
     O                       ACCPAT        L    158
     O                       ACCDUE        L    170
     *---------------------------------------------------------------*
     O          EF           PLNTOT         2
     O                                            5 'PLAN '
     O                       BBPLAN        4     10
     O                                           24 'TOTAL:'
     O                       PLNPGS        3     30
     O                       PLNCHG        L    146
     O                       PLNPAT        L    158
     O                       PLNDUE        L    170
     *---------------------------------------------------------------*
     O          EF           PAYTOT         2
     O                                            5 'PAYR '
     O                       BBPAYR        4     11
     O                                           24 'TOTAL:'
     O                       PAYPGS        3     30
     O                       PAYCHG        L    146
     O                       PAYPAT        L    158
     O                       PAYDUE        L    170
     *---------------------------------------------------------------*
     O          EF           LV6TOT         2
     O                                            5 'SITE '
     O                       BBPLV6        4     11
     O                                           24 'TOTAL:'
     O                       LV6PGS        3     30
     O                       LV6CHG        L    146
     O                       LV6PAT        L    158
     O                       LV6DUE        L    170
     *---------------------------------------------------------------*
     O          EF           TOTALS      1  1
     O                                           13 'TOTAL BILLS: '
     O                       RPTBLS        4     30
     O                       RPTCHG        L    146
     O                       RPTPAT        L    158
     O                       RPTDUE        L    170
     *****************************************************************
     OPRINTR2   H    OE                     1 02
     O                                           12 'REPORT DATE:'
     O                       RUNDTE              24 '  /  /    '
     O                       L1NAME             119
     O                                          193 'PAGE:'
     O                       PAGE3         4    198
     *---------------------------------------------------------------*
     O          H    OE                     1
     O                                           12 'REPORT TIME:'
     O                       RUNTME              22 '  :  :  '
     O               82                         102 'APPROVED ELECTRONIC'
     O               82                         116 ' CLAIMS DETAIL'
     O               83                         106 'APPROVED MANUAL CLAIMS'
     O               83                         113 ' DETAIL'
     O               84                         102 'MANUAL CLAIMS NEEDING'
     O               84                         118 ' APPROVAL DETAIL'
     O               85                         103 'ELECTRONIC CLAIMS NEEDIN'
     O               85                         120 'G APPROVAL DETAIL'
     O                       PGMNAM             198
     *---------------------------------------------------------------*
     O          H    OE                     1
     O                                           12 'REPORT TYPE:'
     O                       rpttype             21
     O                       L5NAME             119
     *---------------------------------------------------------------*
     O          H    OE                     2
     O                       L6NAME             119
     *---------------------------------------------------------------*
     O          H    OE                     2
     O               U7                         107 '***** SCRUB *****'
     O              NU7                         107 '***** FINAL *****'
     *---------------------------------------------------------------*
     O          H    OE                     1
     O                                           72 '               PRIOR    '
     O                                          100 '              REFERRING '
     *---------------------------------------------------------------*
     O          H    OE                     0
     O                                           24 '____________  __________'
     O                                           48 '________________  ______'
     O                                           72 '_________  _____________'
     O                                           96 '____  __________  ______'
     O                                          120 '______  _______________ '
     O                                          144 '  _________    _______  '
     O                                          168 '___  ___  _________  ___'
     O                                          183 '_____  ________'
     O                                          198 '_____________'
     *---------------------------------------------------------------*
     O          H    OE                     1
     O                                           24 'ACCOUNT #     NAME      '
     O                                           48 '                  PROVID'
     O                                           72 'ER #       AUTHORIZATION'
     O                                           96 '      RESOURCE #  PHYSIC'
     O                                          120 'IAN ID  TRACKING NUMBER '
     O                                          144 '  FROM DATE    TO DATE  '
     O                                          168 'POS  TOS  PROCEDURE  MOD'
     O                                          183 'IFIER  QUANTITY'
     O                                          198 'CHARGE AMOUNT'
     *---------------------------------------------------------------*
     O          EF           BILDET         1
     O               47      BBACCN        4     12
     O               47      BBNAME              40
     O               47      BBPROV              57
     O                       PRAUTH              78
     O                       PRRESC              87
     O                       QUA17A              90
     O                       PRRUPN             102
     O               47      BOX26              119
     O                       DTF(Z)             131
     O                       DTT(Z)             142
     O                       POS(Z)             147
     O                       CPT(Z)             162
     O                       MOD(Z)             173
     O              N73N74   QTY(Z)        4    180
     O               73N74   QTY4(Z)       4    180
     O               74      QTY2(Z)       M    184
     O                       AMT(Z)        L    198
     *---------------------------------------------------------------
     O          EF           BILTO2         1
     O                                          163 'TOTAL CHARGES:'
     O                       CHGTOT        L    198
     *---------------------------------------------------------------
     O          EF           BILTO2         1
     O                                          163 'DUE FROM CLIENT:'
     O                       CLITOT        L    198
     *---------------------------------------------------------------
     O          EF           BILTO2         2
     O                                          163 'BALANCE DUE:'
     O                       BALTOT        L    198
     *---------------------------------------------------------------*
     O          EF           ACCTO2         2
     O                                            5 'ACCT '
     O                       BBACCN        4     17
     O                                           24 'TOTAL:'
     O                       ACCPG2        3     30
     O                       ACCBA2        L    198
     *---------------------------------------------------------------*
     O          EF           PLNTO2         2
     O                                            5 'PLAN '
     O                       BBPLAN        4     10
     O                                           24 'TOTAL:'
     O                       PLNPG2        3     30
     O                       PLNBA2        L    198
     *---------------------------------------------------------------
     O          EF           PAYTO2         2
     O                                            5 'PAYR '
     O                       BBPAYR        4     11
     O                                           24 'TOTAL:'
     O                       PAYPG2        3     30
     O                       PAYBA2        L    198
     *---------------------------------------------------------------*
     O          EF           LV6TO2         3
     O                                            5 'SITE '
     O                       BBPLV6        4     11
     O                                           24 'TOTAL:'
     O                       LV6PG2        3     30
     O                       LV6BA2        L    198
     *----------------------------------------------------------------
     O          EF           TOTALS      1  1
     O                                           13 'TOTAL BILLS: '
     O                       RPTBLS        4     30
     O                       RPTBA2        L    198
