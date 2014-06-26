import sys

# from R/SRC/include/Rinternals.h
NILSXP	   =  0	  
SYMSXP	   =  1	  
LISTSXP	   =  2	  
CLOSXP	   =  3	  
ENVSXP	   =  4	  
PROMSXP	   =  5	  
LANGSXP	   =  6	  
SPECIALSXP =  7	  
BUILTINSXP =  8	  
CHARSXP	   =  9	  
LGLSXP	   = 10	  
INTSXP	   = 13	  
REALSXP	   = 14	  
CPLXSXP	   = 15	  
STRSXP	   = 16	  
DOTSXP	   = 17	  
ANYSXP	   = 18	  
VECSXP	   = 19	  
EXPRSXP	   = 20	  
BCODESXP   = 21    
EXTPTRSXP  = 22    
WEAKREFSXP = 23    
RAWSXP     = 24    
S4SXP      = 25    
NEWSXP     = 30    
FREESXP    = 31    
FUNSXP     = 99    

# from R/src/main/serialize.c
REFSXP            = 255
NILVALUE_SXP      = 254
GLOBALENV_SXP     = 253
UNBOUNDVALUE_SXP  = 252
MISSINGARG_SXP    = 251
BASENAMESPACE_SXP = 250
NAMESPACESXP      = 249
PACKAGESXP        = 248
PERSISTSXP        = 247
CLASSREFSXP       = 246
GENERICREFSXP     = 245
BCREPDEF          = 244
BCREPREF          = 243
EMPTYENV_SXP	  = 242
BASEENV_SXP	      = 241

sxpNameDict = { \

"NILSXP"	   :  0	,\
"SYMSXP"	   :  1	,\
"LISTSXP"	   :  2	,\
"CLOSXP"	   :  3	,\
"ENVSXP"	   :  4	,\
"PROMSXP"	   :  5	,\
"LANGSXP"	   :  6	,\
"SPECIALSXP" :  7	,\
"BUILTINSXP" :  8	,\
"CHARSXP"	   :  9	,\
"LGLSXP"	   : 10	,\
"INTSXP"	   : 13	,\
"REALSXP"	   : 14	,\
"CPLXSXP"	   : 15	,\
"STRSXP"	   : 16	,\
"DOTSXP"	   : 17	,\
"ANYSXP"	   : 18	,\
"VECSXP"	   : 19	,\
"EXPRSXP"	   : 20	,\
"BCODESXP"   : 21   ,\
"EXTPTRSXP"  : 22   ,\
"WEAKREFSXP" : 23   ,\
"RAWSXP"     : 24   ,\
"S4SXP"      : 25   ,\
"NEWSXP"     : 30   ,\
"FREESXP"    : 31   ,\
"FUNSXP"     : 99   ,\
"REFSXP"           : 255,\
"NILVALUE_SXP"      : 254,\
"GLOBALENV_SXP"    : 253,\
"UNBOUNDVALUE_SXP"  : 252,\
"MISSINGARG_SXP"    : 251,\
"BASENAMESPACE_SXP" : 250,\
"NAMESPACESXP"      : 249,\
"PACKAGESXP"      : 248,\
"PERSISTSXP"        : 247,\
"CLASSREFSXP"      : 246,\
"GENERICREFSXP"     : 245,\
"BCREPDEF"         : 244,\
"BCREPREF"          : 243,\
"EMPTYENV_SXP"	  : 242,\
"BASEENV_SXP"	      : 241
}

sxpNumberDict = {v:k for k, v in sxpNameDict.iteritems()}

def decodeType(flags): return (flags & 255)

def decodeLevels(flags): return (flags >> 12)

def isObject(flags):
    IS_OBJECT_BIT_MASK = 1 << 8
    return (IS_OBJECT_BIT_MASK & flags) != 0

def hasAttribute(flags):
    HAS_ATTR_BIT_MASK = 1 << 9
    return (HAS_ATTR_BIT_MASK & flags) != 0

def hasTag(flags):
    HAS_TAG_BIT_MASK = 1 << 10
    return (HAS_TAG_BIT_MASK & flags) != 0

def readLength():
    l = int(f.readline())
    if l < -1:
        print "negative length of serialized vector"
        exit()
    if l == -1:
        l1 = int(f.readline())
        l2 = int(f.readline())
        finalL = (l1 << 32) + l2
        return finalL
    return l
    
if len(sys.argv) != 2:
    print ("USAGE: interpreter.py infile")
else:
    f = open(sys.argv[1]) 
    magic = f.readline()
    if magic != "RDA2\n":
        print "UNRECOGNIZED MAGIC: ", magic
        exit()
    formt = f.readline()
    version = f.readline()
    writerVersion = f.readline()
    releaseVersion = f.readline()
    flags = int(f.readline())
    t = decodeType(flags)
    levels = decodeLevels(flags)
    isObj = isObject(flags)
    hasAttr = hasAttribute(flags)
    hasT = hasTag(flags)
    print "R_MAGIC_ASCII_V2"
    print "Format: ", formt
    print "Version: ", version
    print "Writer Version: ", writerVersion
    print "Release Version: ", releaseVersion
    print "Flags: ", flags
    print "Type: ", sxpNumberDict[t]
    print "Levels: ", levels
    print "Is Object: ", isObj
    print "Has Attribute: ", hasAttr
    print "Has Tag: ", hasT
    if t == NILVALUE_SXP:
        print None
    elif t == VECSXP:
        l = readLength()
        print "Length: ", l
        
       

