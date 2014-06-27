import sys
import itertools
import collections

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
ATTRLANGSXP       = 240
ATTRLISTSXP       = 239

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
"BASEENV_SXP"	      : 241,\
"ATTRLANGSXP"       : 240,\
"ATTRLISTSXP"       : 239\
}

sxpNumberDict = {v:k for k, v in sxpNameDict.iteritems()}

symbolTable = collections.OrderedDict()
referenceTable = []

class SXP:
    attr = None
    tag = None
    car = None
    cdr = None
    def __init__(self, t):
        self.t = t
    def __str__(self):

        s = "\nType: " + sxpNumberDict[self.t] + "\n"
        s += "CAR: " + str(self.car) + "\n"
        s += "CDR: " + str(self.cdr) + "\n"
        #for c in self.cdr:
        #    s += str(c) + "\n"
        s += "attr: " + str(self.attr) + "\n"
        s += "tag: " + str(self.tag) + "\n"
        return s

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

def readLength(f):
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

def readByteCodeConstants(f, reps):
    n = int(f.readline())
    ans = [None]*n
    print "# constants: ", n
    for i in range(n):
        t = int(f.readline())
        if t == BCODESXP:
            c = readByteCode(f)
            ans[i] = c
        elif t == LANGSXP \
            or t == LISTSXP \
            or t == BCREPDEF \
            or t == BCREPREF \
            or t == ATTRLANGSXP \
            or t == ATTRLISTSXP:

            c = readBCLang(t, f, reps)
            ans[i] = c
        else:
            ans[i] = readItem(f)
    return ans

def readBCLang(t, f, reps):
     if t == BCREPREF:
        return reps[int(f.readline())]
     elif t == BCREPDEF or \
         t == LANGSXP or \
         t == LISTSXP or \
         t == ATTRLANGSXP or \
         t == ATTRLISTSXP:
             
         pos = -1
         hasAttr = False
         if (t == BCREPDEF) :
             pos = int(f.readline())
             t = int(f.readline())
         if (t == ATTRLANGSXP):
             t = LANGSXP
             hasAttr = TRUE
         elif t == ATTRLISTSXP:
             t = LISTSXP
             hasAttr = TRUE
         ans = SXP(t)
         if pos >= 0:
             print pos
             reps[pos] = ans
         if hasAttr:
             ans.attr = readItem(f)
         ans.tag = readItem(f)
         ans.car = readBCLang(int(f.readline()), f, reps)
         ans.cdr = readBCLang(int(f.readline()), f, reps)
         return ans
             
     else:
         return readItem(f)

def readByteCode(f):
    n = int(f.readline())
    print "# reps: ", n
    reps = [None] * n
    s = SXP(BCODESXP)
    s.car = readItem(f)
    print "Code: ", s.car 
    s.cdr = readByteCodeConstants(f, reps)
    s.tag = None
    return s

def readIntegerVector(f):
    # TODO
    # i don't handle the non-default formats
    n = readLength(f)
    print "Integer Vector Length: ",n 
    iv = [int(f.readline()) for i in range(n)]
    print iv
    return iv

def readChar(f):
    n = int(f.readline())
    return f.readline().rstrip("\n").decode("unicode-escape")

def inRefIndex(f, flags):
    i = flags >> 8
    if i == 0:
        return int(f.readline())
    else:
        return i

def getReadRef(ind):
    i = ind - 1
    return referenceTable[i]

def readItem(f):
    flags = int(f.readline())
    t = decodeType(flags)
    levels = decodeLevels(flags)
    isObj = isObject(flags)
    hasAttr = hasAttribute(flags)
    hasT = hasTag(flags)
    print "Flags: ", flags
    print "Type: ", sxpNumberDict[t]
    print "Levels: ", levels
    print "Is Object: ", isObj
    print "Has Attribute: ", hasAttr
    print "Has Tag: ", hasT
    if t == NILVALUE_SXP:
        return None 
    elif t == VECSXP:
        n = readLength(f)
        print "Reading vector length: ", n
        vec = [readItem(f) for _ in range(n)]
        return vec
    elif t == BCODESXP:
        bc = readByteCode(f)
        print "Bytecode SXP: ", bc
        return bc
    elif t == SPECIALSXP or t == BUILTINSXP:
        n = int(f.readline())
        cbuf = f.readline()
        print n
        print cbuf
        return cbuf
    elif t == INTSXP:
        return readIntegerVector(f)
    elif t == SYMSXP:
        s = readItem(f) 
        referenceTable.append(s)
    elif t == CHARSXP:
        c = readChar(f)
        print c
        return c
    elif t == STRSXP:
        n = readLength(f)
        #s = itertools.repeat(readItem(f), n)
        #s = [readItem(f) for _ in range(n)]
        s = readItem(f)
        print s
        return s
    elif t == REFSXP:
        ind = inRefIndex(f, flags)
        print ind
        rr = getReadRef(ind)
        print rr
        return rr
    else:
        print "Unhandled SXP type"
        print t
        print sxpNumberDict[t]
        exit()
    
def deserialize() :
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
        print "R_MAGIC_ASCII_V2"
        print "Format: ", formt
        print "Version: ", version
        print "Writer Version: ", writerVersion
        print "Release Version: ", releaseVersion
        return readItem(f)
        
       
s = deserialize()
s0 = s[0]
print "---"
#print s0
print s0.cdr[1]
