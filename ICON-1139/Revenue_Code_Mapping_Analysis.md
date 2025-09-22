# Revenue Code Mapping Analysis - HXHPYINQ Program

## Overview
This document provides a detailed analysis of the revenue code mapping functionality in the HXHPYINQ.SQLRPGLE program. The purpose is to understand the data structures, logic flow, and implementation patterns that can be applied to similar procedure code mapping functionality.

## Program Purpose
HXHPYINQ is a web service program for Payor/Plan File Inquiries that handles various contract-related data including:
- Charge codes
- Revenue codes and mappings
- Contract variables
- Bed hold defaults
- Timely filing information
- Contract rates
- And many other billing-related configurations

## Revenue Mapping Data Structures

### Primary Data Structure: revenueMapsDS
```rpgle
d revenueMapsDS   ds                  qualified
d  revCode                       4  0
d*cmnt|comment:Grid column for revenue code mapping|
d  revCodeDesc                  25
d*cmnt|comment:Grid column for revenue code mapping description|
d  printCode                     4
d*cmnt|comment:Grid column for revenue code mapping print code|
d  printCodeDesc                25
d*cmnt|comment:Grid column for revenue code mapping print code description|
```

### Output Array Structure: revenueMaps
```rpgle
d revenueMaps     ds                  likeds(revenueMapsDS) dim(999)
d*info|use:output|counter:revenueMapCnt|
d*cmnt|comment:Return grid containing Revenue Mapping|
```

### Counter Variable
```rpgle
d revenueMapCnt   s             10i 0
d*info|use:output|
d*cmnt|comment:Return counter for Revenue Maps grid (revenueMaps)|
```

## Key Variables and Context

### Request Parameters
The mapping logic uses several key variables derived from the request input:
- `request.payor` - Payor identifier
- `request.plan` - Plan identifier
- `request.class1` - Classification 1
- `request.class2` - Classification 2
- `request.effective` - Effective date
- `request.expiration` - Expiration date (with special handling for zero values)

### Key Variables for Database Queries
```rpgle
d keyCat1         s              3    // Category 1 key
d keyCat2         s              3    // Category 2 key  
d keyEffDt        s              8  0 // Effective date key
d keyEndDt        s              8  0 // End date key
```

## Logic Flow Analysis

### 1. Initialization Phase
Location: Lines 1195-1240 approximately

```rpgle
revenueMapCnt = 0;
clear revenueMaps;
```

All output variables and arrays are initialized to zero/blank values at the start of the program execution.

### 2. Key Variable Setup
Location: Lines 1290-1320 approximately

The program retrieves contract information from HBFDIEM (Payor Defaults/Information file) to establish the key values used in subsequent queries:

```rpgle
setll (request.payor : request.plan : vExpiration) HBFDIEM;
reade (request.payor : request.plan) HBFDIEM;
if not%eof;
  keyEffDt = xffdt;      // From HBFDIEM record
  keyEndDt = xftdt;      // From HBFDIEM record
  if xfct1 = 'X';        // Check if category 1 is used
    keyCat1 = request.class1;
  else;
    keyCat1 = *blanks;
  endif;
  if xfct2 = 'X';        // Check if category 2 is used
    keyCat2 = request.class2;
  else;
    keyCat2 = *blanks;
  endif;
```

### 3. Revenue Mapping Population
Location: BuildRevenueCodeMappings subroutine (lines 1814-1870)

#### SQL Cursor Declaration
```sql
Exec Sql  Declare ccc Cursor For
  Select Distinct
    hxmrev,                              -- Revenue code
    Case When brev.brvdsc Is Null Then ' ' Else brev.brvdsc End,  -- Revenue description
    hxmrvm,                              -- Revenue mapping print code
    hxmrvd                               -- Revenue mapping description
  From hxprvmp                           -- Revenue mapping table
  Left Outer Join hbpbrev brev On hxmrev=brev.brvcde  -- Join for revenue descriptions
  Where
    hxmubn = :request.payor
    And hxmpln = :request.plan
    And hxmct1 = :keyCat1
    And hxmct2 = :keyCat2
    And hxmefd = :keyEffDt
    And hxmetd = :keyEndDt
  Order By hxmrev
;
```

#### Data Processing Loop
```rpgle
exec sql  open ccc;
exec sql  fetch from ccc into :revenueMapsDS;

dow (sqlcod = 0);
  revenueMapCnt += 1;
  
  // Handle missing print code descriptions
  if revenueMapsDS.printCodeDesc = *blanks;
    monitor;
      keyRev = %dec(revenueMapsDS.printCode : 4 : 0);
      on-error;
        keyRev = 0;
    endmon;
    if keyRev <> 0;
      chain (keyRev) HBFBREV;              // Look up revenue description
      if %found;
        revenueMapsDS.printCodeDesc = brvdsc;
      endif;
    endif;
  endif;
  
  // Copy data to output array
  eval-corr revenueMaps(revenueMapCnt) = revenueMapsDS;
  exec sql  fetch next from ccc into :revenueMapsDS;
enddo;

exec sql  close ccc;
```

## Database Tables Involved

### Primary Table: HXPRVMP (Revenue Mapping Table)
- `hxmubn` - Payor number
- `hxmpln` - Plan number  
- `hxmct1` - Category 1
- `hxmct2` - Category 2
- `hxmefd` - Effective date
- `hxmetd` - End date
- `hxmrev` - Revenue code
- `hxmrvm` - Revenue mapping print code
- `hxmrvd` - Revenue mapping description

### Lookup Tables:
1. **HBPBREV** - Revenue code descriptions
   - `brvcde` - Revenue code
   - `brvdsc` - Revenue description

2. **HBFBREV** - Additional revenue lookup (used for print code descriptions)
   - Key: Revenue code
   - Returns: `brvdsc` (description)

3. **HBFDIEM** - Payor defaults/information
   - Contains effective dates and category usage flags
   - `xffdt` - From date
   - `xftdt` - To date
   - `xfct1` - Category 1 usage flag
   - `xfct2` - Category 2 usage flag

## Error Handling and Data Validation

### Date Handling (Special Case)
The program includes special logic for handling zero expiration dates (HDEV-40557):
```rpgle
vExpiration = request.Expiration;
If request.Expiration = 0;
  Open HMPMAST;
  Chain (request.Level6:request.Account) HMPMAST;
  If %Found(HMPMAST) and mmDsDt > 0;
    vExpiration = mmDsDt;        // Use discharge date
  ElseIf %Found(HMPMAST) and mmDsDt = 0 or Not %Found(HMPMAST);
    vExpiration = curDat;        // Use current date
  EndIf;
EndIf;
```

### Numeric Conversion Protection
```rpgle
monitor;
  keyRev = %dec(revenueMapsDS.printCode : 4 : 0);
  on-error;
    keyRev = 0;
endmon;
```

## Program Flow Context

### Subroutine Call Sequence
The BuildRevenueCodeMappings subroutine is called as part of a larger sequence:
1. BuildChargeCodes
2. BuildRevenueCodes
3. BuildRevenueCodeRangeMappings
4. BuildContractVariables
5. **BuildRevenueCodeMappings** ← Target subroutine
6. BuildBedHoldDfts
7. BuildTimelyFilings
8. BuildContractRates
9. Additional processing routines...

### Web Service Parameter Structure
The revenue mapping data is returned as part of the web service response:
```rpgle
c                   parm                    revenueMapCnt
c                   parm                    revenueMaps
```

## Key Implementation Patterns

### 1. Template-Based Data Structure Design
- Use qualified data structure as template (`revenueMapsDS`)
- Create dimensional array based on template (`revenueMaps`)
- Include proper documentation with `*cmnt` comments

### 2. Counter-Driven Array Management
- Initialize counter to zero
- Increment counter for each record processed
- Use counter in output parameter for consuming applications

### 3. SQL Cursor Pattern
- Declare cursor with embedded SQL
- Use LEFT OUTER JOIN for optional descriptions
- Process with DO WHILE loop checking SQLCOD
- Always close cursor after processing

### 4. Fallback Description Logic
- Check if description is blank after initial query
- Perform additional lookups to populate missing descriptions
- Use monitor/on-error for safe numeric conversions

### 5. Key-Based Filtering
- Establish key variables from contract defaults
- Use conditional logic for optional categories
- Apply consistent WHERE clause pattern across related queries

## Performance Considerations

1. **DISTINCT clause** - Used to eliminate duplicate records
2. **ORDER BY** - Ensures consistent output ordering
3. **Indexed access** - Uses key fields that should be indexed
4. **Single pass processing** - Efficient cursor-based iteration
5. **Conditional lookups** - Only performs additional chains when needed

## Integration Points

The revenue mapping functionality integrates with:
- Web service parameter validation
- User authorization checking  
- Contract effective date resolution
- Multi-category filtering logic
- Description lookup services

This pattern can be adapted for similar mapping requirements by modifying the table names, field names, and data structure definitions while maintaining the same overall logic flow and error handling patterns.
