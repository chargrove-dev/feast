/*
This is free and unencumbered software released into the public domain.

Anyone is free to copy, modify, publish, use, compile, sell, or
distribute this software, either in source code form or as a compiled
binary, for any purpose, commercial or non-commercial, and by any
means.

In jurisdictions that recognize copyright laws, the author or authors
of this software dedicate any and all copyright interest in the
software to the public domain. We make this dedication for the benefit
of the public at large and to the detriment of our heirs and
successors. We intend this dedication to be an overt act of
relinquishment in perpetuity of all present and future rights to this
software under copyright law.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

For more information, please refer to <https://unlicense.org>
*/
//****************************************************************************
//**
//**    FEASTLEXMAIN.CPP
//**    Lexical Analysis
//**
//****************************************************************************
//============================================================================
//    HEADERS
//============================================================================

#include "FeastInternal.h"

namespace FEAST {

//============================================================================
//    DEFINITIONS / ENUMERATIONS / SIMPLE TYPEDEFS
//============================================================================
typedef NDword dword;
typedef NSDword sdword;
typedef NByte byte;
typedef NWord word;

#define ALLOC(type, size) LIB_Malloc(type, size)
#define FREE LIB_Free
#define FEAST_ERROR LIB_Errorf

#define CC_MEMOPERATORS \
    void* operator new(size_t size) { return(ALLOC(char, (NDword)size)); } \
    void operator delete(void* ptr) { FREE(ptr); }

// nfa node flags
enum
{
    NFANF_EPSILON       = 0x00000001, // epsilon closure node
	NFANF_ACCEPTING     = 0x00000002, // accepting node
    NFANF_EDGESET       = 0x00000004, // multiple character edge set is used

    NFANF_MAX           = 0x80000000
};

typedef bool (*tokenInterceptFunc_t)(ILexLexer*, SLexToken*, char*, int);

//============================================================================
//    CLASSES / STRUCTURES
//============================================================================
//#define BITSET_RANGECHECK

class CLexBitSet
{
public:
	byte *data;
	dword size, sizeInBytes;
    static dword defaultSize;
	
    void Init(int s)
	{
		size = s;
		sizeInBytes = (s + 7) >> 3;
		data = ALLOC(byte, sizeInBytes);
		memset(data, 0, sizeInBytes);
	}

    CLexBitSet() { if (defaultSize) Init(defaultSize); else Init(2048); } // hc
    CLexBitSet(int s) { Init(s); }
    ~CLexBitSet() { if (data) FREE(data); }

	inline dword Num()
	{
		int count=0;
		for (dword i=0;i<size;i++)
			count += ((data[i >> 3] & (1 << (i & 7))) != 0);
		return(count);
	}
	inline CLexBitSet& operator += (const dword v) // add element
	{
#ifdef BITSET_RANGECHECK
		if (v < size)
#endif
			data[v >> 3] |= (1 << (v & 7));
		return(*this);
	}
	inline CLexBitSet& operator -= (const dword v) // remove element
	{
#ifdef BITSET_RANGECHECK
		if (v < size)
#endif
			data[v >> 3] &= ~(1 << (v & 7));
		return(*this);
	}
	inline CLexBitSet& operator = (const CLexBitSet& other) // assignment
	{
		dword lowest = sizeInBytes;
		if (other.sizeInBytes < lowest)
			lowest = other.sizeInBytes;
		memcpy(data, other.data, lowest);
		return(*this);
	}
	inline CLexBitSet& operator |= (const CLexBitSet& other) // union
	{
		dword lowest = sizeInBytes;
		if (other.sizeInBytes < lowest)
			lowest = other.sizeInBytes;
		for (dword i=0; i<lowest; i++)
			data[i] |= other.data[i];
		return(*this);
	}
	inline CLexBitSet& operator &= (const CLexBitSet& other) // intersection
	{
		dword lowest = sizeInBytes;
		if (other.sizeInBytes < lowest)
			lowest = other.sizeInBytes;
		for (dword i=0; i<lowest; i++)
			data[i] &= other.data[i];
		return(*this);
	}
	inline CLexBitSet& operator -= (const CLexBitSet& other) // difference
	{
		dword lowest = sizeInBytes;
		if (other.sizeInBytes < lowest)
			lowest = other.sizeInBytes;
		for (dword i=0; i<lowest; i++)
			data[i] ^= other.data[i];
		return(*this);
	}
	inline int operator < (const CLexBitSet& other) // is subset of
	{
		dword lowest = sizeInBytes;
		if (other.sizeInBytes < lowest)
			lowest = other.sizeInBytes;
		for (dword i=0; i<lowest; i++)
			if ((data[i] & other.data[i]) != data[i])
				return(0);
		return(1);
	}
	inline int operator == (const CLexBitSet& other) // equivalent
	{
		dword lowest = sizeInBytes;
		if (other.sizeInBytes < lowest)
			lowest = other.sizeInBytes;
		for (dword i=0; i<lowest; i++)
			if (data[i] != other.data[i])
				return(0);
		return(1);
	}
	inline int operator != (const CLexBitSet& other)
	{
		return(!(*this == other));
	}
	inline int IsDisjoint(const CLexBitSet& other) // have no elements in common
	{
		dword lowest = sizeInBytes;
		if (other.sizeInBytes < lowest)
			lowest = other.sizeInBytes;
		for (dword i=0; i<lowest; i++)
			if (data[i] & other.data[i])
				return(0);
		return(1);
	}
	inline int IsIntersecting(const CLexBitSet& other) // have at least one element in common
	{
		dword lowest = sizeInBytes;
		if (other.sizeInBytes < lowest)
			lowest = other.sizeInBytes;
		for (dword i=0; i<lowest; i++)
			if (data[i] & other.data[i])
				return(1);
		return(0);
	}
	inline void Invert()
	{
		for (dword i=0; i<sizeInBytes; i++)
			data[i] ^= 0xFF;
	}
	inline void Empty()
	{
		memset(data, 0, sizeInBytes);
        //for (dword i=0; i<sizeInBytes; i++)
		//	data[i] = 0;
	}
	inline void Fill()
	{
		memset(data, 0xFF, sizeInBytes);
        //for (dword i=0; i<sizeInBytes; i++)
		//	data[i] = 0xFF;
	}
	inline int Contains(dword v)
	{
#ifdef BITSET_RANGECHECK
		if (v < size)
#endif
			return((data[v >> 3] & (1 << (v & 7))) != 0);
		return(0);
	}
	inline int IsEmpty()
	{
		for (dword i=0; i<sizeInBytes; i++)
		{
			if (data[i])
				return(0);
		}
		return(1);
	}
};

dword CLexBitSet::defaultSize = 0;

//-----------------------------------------------
// NODES
//-----------------------------------------------
class CLexNfaNode;

class CLexNfaNodeFactory
{
public:
    dword numNodes, maxNodes;
    CLexNfaNode* nodes;

    CLexNfaNodeFactory(dword nodeCount);
    ~CLexNfaNodeFactory();
    CLexNfaNode* AllocNode();
    CLexNfaNode* NodeForId(dword id);
    dword IdForNode(CLexNfaNode* node);
};

class CLexNfaNode
{
private:
    void* operator new(size_t size) { return(NULL); }
    void operator delete(void* ptr) { }
public:
	void* operator new(size_t size, void* ptr) { return(ptr); }
	void operator delete(void*, void*) { }
    void* operator new(size_t size, CLexNfaNodeFactory& factory) { return(factory.AllocNode()); }
    void operator delete(void* ptr, CLexNfaNodeFactory& factory) { FEAST_ERROR("CLexNfaNode: illegal operator delete"); }

    dword flags; // NFANF_ flags
    dword tag; // token tag
    byte edge; // single-character edge if flags indicate, zero if unused (since zero edges are illegal)
    byte priority; // priority level
    CLexNfaNode* link1; // next node (null if none)
    CLexNfaNode* link2; // another node for epsilon edges, null if unused
    tokenInterceptFunc_t tokenFunc; // intercept function, null if unused    
    CLexBitSet edgeSet; // multiple-character set if flags indicate

    CLexNfaNode()
        : edgeSet(256)
    {
        flags = tag = edge = priority = 0;
        link1 = link2 = NULL;
        tokenFunc = NULL;
    }
};

CLexNfaNodeFactory::CLexNfaNodeFactory(dword nodeCount)
{
	numNodes = 0;
	maxNodes = nodeCount;
	nodes = ALLOC(CLexNfaNode, maxNodes);
	/*for (dword i=0;i<maxNodes;i++) new(&nodes[i]) CLexNfaNode;*/
}
CLexNfaNodeFactory::~CLexNfaNodeFactory()
{
	if (nodes)
	{
		for (dword i=0;i<numNodes;++i)
			nodes[i].~CLexNfaNode();
		FREE(nodes);
	}
}
CLexNfaNode* CLexNfaNodeFactory::AllocNode()
{
    if (numNodes >= maxNodes)
        FEAST_ERROR("CLexNfaNodeFactory: AllocNode limit exceeded (%d)", maxNodes);
    numNodes++;
    return(&nodes[numNodes-1]);
}
CLexNfaNode* CLexNfaNodeFactory::NodeForId(dword id) { return(&nodes[id]); }
dword CLexNfaNodeFactory::IdForNode(CLexNfaNode* node) { return((dword)(node - nodes)); }

//-----------------------------------------------
// EXPRS
//-----------------------------------------------
class CLexNfaExpr;

class CLexNfaExprFactory
{
public:
    dword numExprs, maxExprs;
    CLexNfaExpr* exprs;

    CLexNfaExprFactory(dword exprCount);
    ~CLexNfaExprFactory();
    CLexNfaExpr* AllocExpr();
    CLexNfaExpr* ExprForId(dword id);
    dword IdForExpr(CLexNfaExpr* expr);
};

class CLexNfaExpr
{
private:
    void* operator new(size_t size) { return(NULL); }
    void operator delete(void* ptr) {}
public:
	void* operator new(size_t size, void* ptr) { return(ptr); }
	void operator delete(void*, void*) { }
    void* operator new(size_t size, CLexNfaExprFactory& factory) { return(factory.AllocExpr()); }
    void operator delete(void* ptr, CLexNfaExprFactory& factory) { FEAST_ERROR("CLexNfaExpr: illegal operator delete"); }

    CLexNfaNode* first;
    CLexNfaNode* last;

    CLexNfaExpr() { first = last = NULL; }
};

CLexNfaExprFactory::CLexNfaExprFactory(dword exprCount) { numExprs = 0; maxExprs = exprCount; exprs = ALLOC(CLexNfaExpr, maxExprs); /*for (dword i=0;i<maxExprs;i++) new(&exprs[i]) CLexNfaExpr;*/ }
CLexNfaExprFactory::~CLexNfaExprFactory() { if (exprs) FREE(exprs); }
CLexNfaExpr* CLexNfaExprFactory::AllocExpr()
{
    if (numExprs >= maxExprs)
        FEAST_ERROR("CLexNfaExprFactory: AllocExpr limit exceeded (%d)", maxExprs);
    numExprs++;
    return(&exprs[numExprs-1]);
}
CLexNfaExpr* CLexNfaExprFactory::ExprForId(dword id) { return(&exprs[id]); }
dword CLexNfaExprFactory::IdForExpr(CLexNfaExpr* expr) { return((dword)(expr - exprs)); }

//-----------------------------------------------
// DFA CLASSES
//-----------------------------------------------
class CLexDfaAcceptItem
{
public:
    dword tag;
    dword flags;
    tokenInterceptFunc_t tokenFunc;
    byte priority;

    inline CLexDfaAcceptItem& operator = (const CLexDfaAcceptItem& i)
    {
        tag = i.tag; flags = i.flags; tokenFunc = i.tokenFunc; priority = i.priority;
        return(*this);
    }
    inline int operator == (const CLexDfaAcceptItem& i)
    {
        return( (tag == i.tag) && (flags == i.flags) && (tokenFunc == i.tokenFunc) && (priority == i.priority) );
    }
    inline int operator != (const CLexDfaAcceptItem& i)
    {
        return(!(*this == i));
    }
};

class CLexDfaTable
{
public:
    dword numChars, numStates;
    dword initialState;
    word* stateNext; // [numChars*numStates]
    sdword* stateParts; // [numStates], used during minimization only
    CLexDfaAcceptItem* acceptTable; // [numStates]

    CLexDfaTable()
    {
        numChars = numStates = initialState = 0;
        stateNext = NULL;
        stateParts = NULL;
        acceptTable = NULL;
    }
    ~CLexDfaTable()
    {
        if (stateNext) FREE(stateNext);
        if (stateParts) FREE(stateParts);
        if (acceptTable) FREE(acceptTable);
    }


};

#define DFA_MAXSTATES 1024 // hc

class CLexDfaPartitionSet;

class CLexDfaPartition
{
private:
    sdword highest, count;
    sdword states[DFA_MAXSTATES];
    sdword partIndex;
    CLexDfaTable* useDFA;
public:
    CLexDfaPartition() { Init(); }
    void Init()
    {
        highest = count = partIndex = 0;
        useDFA = NULL;
        for (sdword i=0;i<DFA_MAXSTATES;i++)
            states[i] = -1;
    }
    bool Owns(sdword state, sdword* index)
    {
		for (sdword i=0;i<=highest;i++)
		{
			if (states[i] == state)
			{
				if (index)
                    *index = i;
				return(1);
			}
		}
		return(0);    
    }
    bool Add(sdword state)
    {
		if (Owns(state, NULL))
			return(0);
		if (useDFA->stateParts[state] != -1)
			return(0);
		for (sdword i=0;i<DFA_MAXSTATES;i++)
		{
			if (states[i] != -1)
                continue;
			states[i] = state;
			if (highest < i)
				highest = i;
			count++;
			useDFA->stateParts[state] = partIndex;
			return(1);
		}
        FEAST_ERROR("CLexDfaPartition::Add: Out of room for states");
		return(0);
    }
	bool Remove(sdword state)
	{
		sdword index;
		if (!Owns(state, &index))
			return(0);
		states[index] = -1;
		count--;
		useDFA->stateParts[state] = -1;
		return(1);
	}
	sdword Next(sdword start, sdword *nextstart=NULL)
	{
		sdword i;
		for (i=start; (i<=highest) && (states[i] == -1); i++)
			;
		if (i > highest)
			return(-1);
		if (nextstart)
			*nextstart = i+1;
		return(states[i]);
	}
    sdword Count() { return(count); }

    friend class CLexDfaPartitionSet;
};

class CLexDfaPartitionSet
{
private:
    CLexDfaPartition partitions[DFA_MAXSTATES];
    sdword curPartition;
    bool partAdd;
    CLexDfaTable* useDFA;
public:
    CLexDfaPartitionSet() { Init(); }
    void Init()
    {
        curPartition = 0;
        partAdd = 0;
        useDFA = NULL;
        for (sdword i=0;i<DFA_MAXSTATES;i++)
        {
            partitions[i].Init();
            partitions[i].partIndex = i;
        }
    }
    inline CLexDfaPartition& operator[] (int i)
    {
        return(partitions[i]);
    }
    inline CLexDfaPartition* operator-> ()
    {
        if (partAdd)
            curPartition++;
        partAdd = 0;
        return(&partitions[curPartition]);
    }
    void Mark()
    {
        partAdd = 1;
    }
    void SetDFA(CLexDfaTable* dfa)
    {
        useDFA = dfa;
        for (sdword i=0;i<DFA_MAXSTATES;i++)
            partitions[i].useDFA = dfa;
    }
    void MoveTo(sdword state, sdword part)
    {
        if (useDFA->stateParts[state] != -1)
            partitions[useDFA->stateParts[state]].Remove(state);
        partitions[part].Add(state);
    }
    void MoveToCurrent(sdword state)
    {
        if (partAdd)
            curPartition++;
        partAdd = 0;
        MoveTo(state, curPartition);
    }
    sdword Num()
    {
        return(curPartition+1);
    }
    sdword PendingCurrent()
    {
        if (partAdd)
            return(curPartition+1);
        return(curPartition);
    }
};

//-----------------------------------------------
// LEXER
//-----------------------------------------------
class CLexLexer
: public ILexLexer
{
public:
    CC_MEMOPERATORS

    CLexNfaNodeFactory nFact;
    CLexNfaExprFactory eFact;

    char* textPtr;
    CLexNfaNode* startNode;
    byte activePriority;
    CLexDfaTable dfa;
    dword lineCount, columnCount, tabColumns;
    bool finalized, isCaseSensitive;
    CLexBitSet edgesUsed;
	char lexErrorStr[1024];
	CLexBitSet computeMoveOutSet;
	CLexBitSet getTokenNfaNodeSet;

    CLexLexer()
        : nFact(2048), eFact(2048), edgesUsed(256) // hc
		, computeMoveOutSet(2048)
		, getTokenNfaNodeSet(2048)
    {
        textPtr = NULL;
        startNode = new(nFact) CLexNfaNode;
        activePriority = 0;
        lineCount = columnCount = 0;
        tabColumns = 8;
        finalized = 0;
		isCaseSensitive = 1;
        edgesUsed.Empty();
		lexErrorStr[0] = 0;
    }

	void LexSetLastError(const char* inError)
	{
		strncpy(lexErrorStr, inError, 1023);
		lexErrorStr[1023] = 0;
	}

    void SetIntercept(dword inTag, bool (*inIntercept)(ILexLexer*, SLexToken*, char*, int))
    {
        if (!inTag)
            return;
        if (!finalized)
		{
			for (dword i=0;i<nFact.numNodes;i++)
			{
				if (nFact.NodeForId(i)->tag == inTag)
					nFact.NodeForId(i)->tokenFunc = inIntercept;
			}
		}
		else
		{
			for (dword i=0;i<dfa.numStates;i++)
			{
				if (dfa.acceptTable[i].tag == inTag)
					dfa.acceptTable[i].tokenFunc = inIntercept;
			}
		}
    }

	void SaveDFAToFile(FILE* fp)
	{
		dword d;
		
		if (!finalized)
			FinalizeDFA();

		d = dfa.numChars; fwrite(&d, sizeof(d), 1, fp);
		d = dfa.numStates; fwrite(&d, sizeof(d), 1, fp);
		d = dfa.initialState; fwrite(&d, sizeof(d), 1, fp);
		fwrite(dfa.stateNext, sizeof(word), dfa.numChars*dfa.numStates, fp);
		for (dword i=0;i<dfa.numStates;i++)
		{
			d = dfa.acceptTable[i].tag; fwrite(&d, sizeof(d), 1, fp);
			d = dfa.acceptTable[i].flags; fwrite(&d, sizeof(d), 1, fp);
			d = dfa.acceptTable[i].priority; fwrite(&d, sizeof(d), 1, fp);
		}
	}

	void LoadDFAFromFile(FILE* fp)
	{
		dword d;

		fread(&d, sizeof(d), 1, fp); dfa.numChars = d;
		fread(&d, sizeof(d), 1, fp); dfa.numStates = d;
		fread(&d, sizeof(d), 1, fp); dfa.initialState = d;
		if (dfa.stateNext)
			FREE(dfa.stateNext);
		dfa.stateNext = ALLOC(word, dfa.numChars*dfa.numStates);
		fread(dfa.stateNext, sizeof(word), dfa.numChars*dfa.numStates, fp);
		if (dfa.acceptTable)
			FREE(dfa.acceptTable);
		dfa.acceptTable = ALLOC(CLexDfaAcceptItem, dfa.numStates);
		for (dword i=0;i<dfa.numStates;i++)
		{
			fread(&d, sizeof(d), 1, fp); dfa.acceptTable[i].tag = d;
			fread(&d, sizeof(d), 1, fp); dfa.acceptTable[i].flags = d;
			fread(&d, sizeof(d), 1, fp); dfa.acceptTable[i].priority = (byte)d;
		}
		finalized = 1;
	}

// NFA Expressions -----------------------------------------

    CLexNfaExpr* ExprCreate(byte inEdge)
    {
        CLexNfaNode* n1;
        CLexNfaNode* n2;
        CLexNfaExpr* e;

		if ((!isCaseSensitive) && (inEdge >= 'A') && (inEdge <= 'Z'))
			inEdge = (inEdge - 'A') + 'a';
        edgesUsed += inEdge;
        e = new(eFact) CLexNfaExpr;
        n1 = new(nFact) CLexNfaNode;
        n2 = new(nFact) CLexNfaNode;
        n1->edge = inEdge;
        n1->link1 = n2;
        e->first = n1;
        e->last = n2;
        return(e);
    }
    CLexNfaExpr* ExprCreate(CLexBitSet& inEdgeSet)
    {
        CLexNfaNode* n1;
        CLexNfaNode* n2;
        CLexNfaExpr* e;

        edgesUsed |= inEdgeSet;
        e = new(eFact) CLexNfaExpr;
        n1 = new(nFact) CLexNfaNode;
        n2 = new(nFact) CLexNfaNode;
        n1->edge = 0;
        n1->flags |= NFANF_EDGESET;
        n1->edgeSet = inEdgeSet;
        n1->link1 = n2;
        e->first = n1;
        e->last = n2;
        return(e);    
    }
    void ExprOr(CLexNfaExpr* dest, CLexNfaExpr* src)
    {
        CLexNfaNode* n1;
        CLexNfaNode* n2;

	    n1 = new(nFact) CLexNfaNode;
	    n2 = new(nFact) CLexNfaNode;
	    n1->edge = 0; n1->flags |= NFANF_EPSILON;
	    n1->link1 = dest->first;
	    n1->link2 = src->first;
	    dest->last->edge = 0; dest->last->flags |= NFANF_EPSILON;
	    src->last->edge = 0; src->last->flags |= NFANF_EPSILON;
	    dest->last->link1 = n2;
	    src->last->link1 = n2;
	    dest->last->link2 = src->last->link2 = NULL; // shouldn't be necessary
	    dest->first = n1;
	    dest->last = n2;
    }
    void ExprCat(CLexNfaExpr* dest, CLexNfaExpr* src)
    {
	    dest->last->edge = 0;
        dest->last->flags |= NFANF_EPSILON;
	    dest->last->link1 = src->first;
	    dest->last->link2 = NULL;
	    dest->last = src->last;
    }
    void ExprOneOrMore(CLexNfaExpr* dest)
    {
        CLexNfaNode* n1;
        CLexNfaNode* n2;

	    n1 = new(nFact) CLexNfaNode;
	    n2 = new(nFact) CLexNfaNode;
	    n1->edge = 0; n1->flags |= NFANF_EPSILON;
	    n1->link1 = dest->first;
	    dest->last->edge = 0; dest->last->flags |= NFANF_EPSILON;
	    dest->last->link1 = n2;
	    dest->last->link2 = dest->first;
	    dest->first = n1;
	    dest->last = n2;
    }
    void ExprZeroOrMore(CLexNfaExpr* dest)
    {
        CLexNfaNode* n1;
        CLexNfaNode* n2;

	    n1 = new(nFact) CLexNfaNode;
	    n2 = new(nFact) CLexNfaNode;
	    n1->edge = 0; n1->flags |= NFANF_EPSILON;
	    n1->link1 = dest->first;
        n1->link2 = n2;
	    dest->last->edge = 0; dest->last->flags |= NFANF_EPSILON;
	    dest->last->link1 = n2;
	    dest->last->link2 = dest->first;
	    dest->first = n1;
	    dest->last = n2;
    }
    void ExprZeroOrOne(CLexNfaExpr* dest)
    {
        CLexNfaNode* n1;
        CLexNfaNode* n2;

	    n1 = new(nFact) CLexNfaNode;
	    n2 = new(nFact) CLexNfaNode;
	    n1->edge = 0; n1->flags |= NFANF_EPSILON;
	    n1->link1 = dest->first;
        n1->link2 = n2;
	    dest->last->edge = 0; dest->last->flags |= NFANF_EPSILON;
	    dest->last->link1 = n2;
	    dest->last->link2 = NULL;
	    dest->first = n1;
	    dest->last = n2;
    }
    CLexNfaExpr* ExprString(char* str)
    {
        CLexNfaExpr* e;
        CLexNfaExpr* e2;

        if (!str)
            return(NULL);
        e = ExprCreate(*str);
        for (char* ptr = str+1; *ptr; ptr++)
        {
            e2 = ExprCreate(*ptr);
            ExprCat(e, e2);
        }
        return(e);
    }
    void RegisterExpr(CLexNfaExpr* e, dword tag)
    {
        CLexNfaNode* n;

        e->last->flags |= NFANF_ACCEPTING;
        e->last->tokenFunc = NULL;
        e->last->priority = activePriority;
        e->last->tag = tag;
        if (!startNode->link1)
        {
            startNode->edge = 0; startNode->flags |= NFANF_EPSILON;
            startNode->link1 = e->first;
            return;
        }
        n = new(nFact) CLexNfaNode;
        n->edge = 0; n->flags |= NFANF_EPSILON;
        n->link1 = e->first;
        n->link2 = startNode->link2;
        startNode->link2 = n;
    }


// Regex Expressions -----------------------------------------

    byte RegexLetter(char*& regex, bool* outLiteral)
    {
        byte letter;

        *outLiteral = false;
        if (*regex == '\\')
        {
            *outLiteral = true;
            regex++;
            letter = *regex++;
            if (!letter)
			{
                LexSetLastError("Regex Syntax Error: Literal '\\' without character");
				return(0);
			}
            switch(letter)
            {
            case 'n': letter = 10; break;
            case 't': letter = 9; break;
            default: break;
            }
            return(letter);
        }
        letter = *regex;
        if (!letter)
            return(0);
        regex++;
        return(letter);
    }
    bool RegexGroup(char*& regex, CLexBitSet& s)
    {
        byte a, b;
        bool literal;

        a = RegexLetter(regex, &literal);
        if (!a)
            return(0);
        if (!literal)
        {
            if (a == '.')
            {
                for (byte i=1;i<254;i++)
                    s += i;
                s -= 10; // dot doesn't recognize \n
                return(1);
            }
		    else if (strchr("[]()*?+^|", a))
            {
                regex--;
                if (a != ']')
                    LexSetLastError("Regex Syntax Error: Illegal character in group");
                return(0);
            }
        }
        if (*regex == '-')
        {
            regex++;
            b = RegexLetter(regex, &literal);
            if (!b)
			{
                LexSetLastError("Regex Syntax Error: '-' found in group without valid ending character");
				return(0);
			}
            if (!literal)
            {
    		    if (strchr(".[]()*?+^|", a))
                {
                    regex--;
                    LexSetLastError("Regex Syntax Error: Non-literal symbol is not a valid group character");
					return(0);
                }
            }
            for (byte i=a;i<=b;i++)
                s += i;
            return(1);
        }
        s += a;
        return(1);
    }
    CLexNfaExpr* RegexRange(char*& regex)
    {
        bool negate = false;
        CLexBitSet s(256);

        if (*regex == '^')
        {
            regex++;
            negate = true;
        }
        while (RegexGroup(regex, s))
            ;
        if (negate)
            s.Invert();
        return(ExprCreate(s));
    }
    CLexNfaExpr* RegexFactorPrime(char*& regex)
    {
        CLexNfaExpr* e;
        byte letter;
        bool literal;

        if (*regex == '(')
        {
            regex++;
            e = RegexExpr(regex);
            if (*regex != ')')
			{
                LexSetLastError("Regex Syntax Error: Expecting ')'");
				return(NULL);
			}
            regex++;
            return(e);
        }
        if (*regex == '[')
        {
            regex++;
            e = RegexRange(regex);
            if (*regex != ']')
			{
                LexSetLastError("Regex Syntax Error: Expecting ']'");
				return(NULL);
			}
            regex++;
            return(e);
        }
        letter = RegexLetter(regex, &literal);
        if (!letter)
            return(NULL);
        if (!literal)
        {
            if (letter == '.')
            {
                CLexBitSet s(256);
                for (byte i=1;i<=254;i++)
                    s += i;
                s -= 10; // dot doesn't recognize \n
                e = ExprCreate(s);
                return(e);
            }
		    if (strchr("[]()*?+^|", letter))
            {
			    regex--;
                return(NULL);
            }
        }
        e = ExprCreate(letter);
        return(e);
    }
    CLexNfaExpr* RegexFactor(char*& regex)
    {
        CLexNfaExpr* e = RegexFactorPrime(regex);
        if (*regex == '*')
        {
            regex++;
            if (!e)
			{
                LexSetLastError("Regex Syntax Error: Modifier '*' without expression");
				return(NULL);
			}
            ExprZeroOrMore(e);
            return(e);
        }
        if (*regex == '+')
        {
            regex++;
            if (!e)
			{
                LexSetLastError("Regex Syntax Error: Modifier '+' without expression");
				return(NULL);
			}
            ExprOneOrMore(e);
            return(e);
        }
        if (*regex == '?')
        {
            regex++;
            if (!e)
			{
                LexSetLastError("Regex Syntax Error: Modifier '?' without expression");
				return(NULL);
			}
            ExprZeroOrOne(e);
            return(e);
        }
        return(e);
    }
    CLexNfaExpr* RegexTerm(char*& regex)
    {
        CLexNfaExpr* e = RegexFactor(regex);
        CLexNfaExpr* e2;
        if (!e)
            return(NULL);
        while ((e2 = RegexFactor(regex)))
            ExprCat(e, e2);
        return(e);
    }
    CLexNfaExpr* RegexExpr(char*& regex)
    {
        CLexNfaExpr* e = RegexTerm(regex);
        CLexNfaExpr* e2;
        if (!e)
            return(NULL);
        while (*regex == '|')
        {
            regex++;
            e2 = RegexTerm(regex);
            if (!e2)
			{
                LexSetLastError("Regex Syntax Error: '|' without trailing term");
				return(NULL);
			}
            ExprOr(e, e2);
        }
        return(e);
    }
    bool RegisterToken(char* inRegex, dword inTag)
    {
        if (!inRegex)
        {
            LexSetLastError("Regex Syntax Error: NULL regex");
            return(0);
        }
        
		LexSetLastError("");
        char* regex = inRegex;
        CLexNfaExpr* e = RegexExpr(regex);
		if (LexGetLastError()[0])
			return(0);
        
		if (!e)
        {
			LexSetLastError("Regex Syntax Error: Nothing to register");
            return(0);
        }
        if (*regex)
        {
            LexSetLastError("Regex Syntax Error: Regex parse incomplete");
            return(0);
        }

        RegisterExpr(e, inTag);
        return(1);
    }


// Calculations -----------------------------------------

    inline void ComputeEpsilonClosure(CLexBitSet& inSet)
    {
        static CLexNfaNode* nodeStack[2048]; // hc
        CLexNfaNode* n;
        dword nodeIndex=0;
        dword i;

        for (i=0;i<inSet.size;i++)
        {
            if (inSet.Contains(i))
                nodeStack[nodeIndex++] = nFact.NodeForId(i);
        }
        while (nodeIndex > 0)
        {
            nodeIndex--;
            n = nodeStack[nodeIndex];
            if (!(n->flags & NFANF_EPSILON))
                continue;
            if ((n->link1) && (!inSet.Contains(nFact.IdForNode(n->link1))))
            {
                inSet += nFact.IdForNode(n->link1);
                nodeStack[nodeIndex++] = n->link1;
            }
            if ((n->link2) && (!inSet.Contains(nFact.IdForNode(n->link2))))
            {
                inSet += nFact.IdForNode(n->link2);
                nodeStack[nodeIndex++] = n->link2;
            }
        }
    }

    inline void ComputeMove(CLexBitSet& inSet, byte inEdge)
    {
        CLexNfaNode* n;
        dword i;

        if (!edgesUsed.Contains(inEdge))
        {
            inSet.Empty();
            return;
        }

        computeMoveOutSet.Empty();
        for (i=0;i<inSet.size;i++)
        {
            if (!inSet.Contains(i))
                continue;
            n = nFact.NodeForId(i);
            if (!n->link1)
                continue;
            if ((n->edge == inEdge) || ((n->flags & NFANF_EDGESET) && (n->edgeSet.Contains(inEdge))))
                computeMoveOutSet += nFact.IdForNode(n->link1);
        }
        inSet = computeMoveOutSet;
    }

    void MinimizeDFA()
    {        
	    static CLexDfaPartitionSet parts;
        sdword i, k, m, p, first;
	    sdword trans, gfirst, gnext;

	    // initialize dfa stateParts array
	    dfa.stateParts = ALLOC(sdword, dfa.numStates);
	    for (i=0;i<(sdword)dfa.numStates;i++)
		    dfa.stateParts[i] = -1;
	    parts.Init();
	    parts.SetDFA(&dfa);

	    for (i=0;i<(sdword)dfa.numStates;i++)
		    parts.MoveToCurrent(i);
	    parts.Mark();

	    // split accepting states from nonaccepting states
	    for (i=0;i<(sdword)dfa.numStates;i++)
	    {
		    if (!(dfa.acceptTable[i].flags & NFANF_ACCEPTING))
			    continue;
		    for (k=1;k<parts.Num();k++)
		    {
			    m = parts[k].Next(0);
			    if (m == -1)
				    continue;
			    if ((i!=m) && (dfa.acceptTable[i] == dfa.acceptTable[m]))
			    {
				    parts.MoveTo(i, k);
				    break;
			    }
		    }
		    if (k >= parts.Num())
		    {
			    parts.MoveToCurrent(i);
			    parts.Mark();
		    }
	    }

	    // repeated column-by-column separation
	    do
	    {
		    trans = 0;
		    for (i=0;i<parts.Num();i++)
		    {			
			    for (p=0,first=k=parts[i].Next(0); k!=-1; k=parts[i].Next(p,&p))
			    {								
				    for (m=0;m<(sdword)dfa.numChars;m++)
				    {				
					    gfirst = dfa.stateNext[first*dfa.numChars+m];
					    gnext = dfa.stateNext[k*dfa.numChars+m];
					    if ((gfirst == 0xFFFF) && (gnext == 0xFFFF))
						    continue;
					    if (((gfirst == 0xFFFF) && (gnext != 0xFFFF))
					     || ((gfirst != 0xFFFF) && (gnext == 0xFFFF))
					     || (dfa.stateParts[gfirst] != dfa.stateParts[gnext]))
					    {						
						    if (dfa.stateParts[k] != parts.PendingCurrent())
						    {
							    trans = 1;
							    parts.MoveToCurrent(k);
						    }
					    }
				    }
			    }
			    parts.Mark();
		    }
	    } while (trans);

	    // partitioning complete; assign partitions as new states

	    word *nstateNext = ALLOC(word, parts.Num()*dfa.numChars);
	    CLexDfaAcceptItem *naccept = ALLOC(CLexDfaAcceptItem, parts.Num());
	    
	    for (i=0;i<parts.Num();i++)
	    {
		    // TEST... the following is necessary to fix a crash bug that i encountered without the check,
			// but the crash is the first of this kind that i've encountered in the past two years since i
			// wrote this code, and i've been using it the whole time.  So i'm not sure if this fix is
			// appropriate since it's been a while since i wrote this stuff and it's not very well
			// documented as a result.  For now, it seems to solve the problem, so I'm keeping it.
			if (parts[i].Next(0) == -1)
				continue;
			// ...TEST
		    memcpy(&nstateNext[i*dfa.numChars], &dfa.stateNext[parts[i].Next(0)*dfa.numChars],
			    dfa.numChars*sizeof(word));
		    for (k=0;k<(sdword)dfa.numChars;k++)
			    if (nstateNext[i*dfa.numChars+k] != 0xFFFF)
				    nstateNext[i*dfa.numChars+k] = (word)dfa.stateParts[nstateNext[i*dfa.numChars+k]];
		    naccept[i] = dfa.acceptTable[parts[i].Next(0)];
	    }
	    dfa.initialState = dfa.stateParts[0];
	    dfa.numStates = parts.Num();
	    FREE(dfa.stateNext);
	    dfa.stateNext = nstateNext;
	    FREE(dfa.acceptTable);
	    dfa.acceptTable = naccept;
	    FREE(dfa.stateParts);
	    dfa.stateParts = NULL;
    }

    void ComputeDFA()
    {	    
	    bool dmark[DFA_MAXSTATES];
	    sdword i, k, m, p, curDstate;
	    CLexNfaNode *accNode, *tempNode;
	    bool done = 0;

		//LOG_Warnf("Node count: %d", nFact.numNodes);

        CLexBitSet::defaultSize = nFact.numNodes;
	    CLexBitSet tstate;
        CLexBitSet dstates[DFA_MAXSTATES];
        CLexBitSet::defaultSize = 0;

	    dfa.numChars = 256; // hc
	    dfa.numStates = 0;
	    dfa.stateNext = ALLOC(word, dfa.numChars * DFA_MAXSTATES);
	    dfa.acceptTable = ALLOC(CLexDfaAcceptItem, DFA_MAXSTATES);

	    for (i=0;i<DFA_MAXSTATES;i++)
		    dmark[i] = 0;
	    dstates[0].Empty();
	    dstates[0] += nFact.IdForNode(startNode);
	    ComputeEpsilonClosure(dstates[0]);
	    curDstate = 0;

	    accNode = NULL;
	    for (p=0; p<(int)dstates[curDstate].size; p++)
	    {
		    if (!dstates[curDstate].Contains(p))
			    continue;
		    if (!(nFact.NodeForId(p)->flags & NFANF_ACCEPTING))
                continue;
		    if ((!accNode) || (accNode->priority <= nFact.NodeForId(p)->priority))
			    accNode = nFact.NodeForId(p);
	    }

	    dfa.acceptTable[curDstate].flags = 0;
	    if (accNode)
	    {
		    dfa.acceptTable[curDstate].priority = accNode->priority;
		    dfa.acceptTable[curDstate].tag = accNode->tag;
		    dfa.acceptTable[curDstate].flags = NFANF_ACCEPTING;
		    dfa.acceptTable[curDstate].tokenFunc = accNode->tokenFunc;
	    }
	    
	    curDstate = 1;

	    while(!done)
	    {
		    //printf("1");
            done = 1;
		    for (i=0;i<curDstate;i++)
		    {
			    if (dmark[i])
				    continue;
                //printf("2");
                //printf("\nDState %d", i);
			    done = 0;
			    dmark[i] = 1;			    
			    
			    for (k=0; k<(sdword)dfa.numChars; k++)
			    {
        		    if (!edgesUsed.Contains(k))
                    {
                        dfa.stateNext[i*dfa.numChars+k] = 0xFFFF;
                        continue;
                    }
                    
                    //printf("3");
                    //printf(".");
				    tstate = dstates[i];
				    
				    ComputeMove(tstate, (byte)k);
				    
				    ComputeEpsilonClosure(tstate);
				    
				    if (tstate.IsEmpty())
				    {
					    dfa.stateNext[i*dfa.numChars+k] = 0xFFFF;
					    continue;
				    }
				    for (m=0;m<curDstate;m++)
					    if (tstate == dstates[m])
						    break;
				    if (m == curDstate)
				    {
					    dstates[curDstate] = tstate;
					    accNode = NULL;
					    for (p=0; p<(sdword)dstates[curDstate].size; p++)
					    {
                		    //printf("4");
                            tempNode = nFact.NodeForId(p);
						    if (!(tempNode->flags & NFANF_ACCEPTING))
                                continue;
						    if (!dstates[curDstate].Contains(p))
							    continue;
						    if ((!accNode) || (accNode->priority <= tempNode->priority))
							    accNode = tempNode;
					    }
					    dfa.acceptTable[curDstate].flags = 0;
					    if (accNode)
					    {
						    dfa.acceptTable[curDstate].priority = accNode->priority;
						    dfa.acceptTable[curDstate].tag = accNode->tag;
						    dfa.acceptTable[curDstate].flags = NFANF_ACCEPTING;
						    dfa.acceptTable[curDstate].tokenFunc = accNode->tokenFunc;
					    }
					    curDstate++;
					    if (curDstate >= DFA_MAXSTATES)
						    FEAST_ERROR("ComputeDFA: Too many DFA states");
				    }
				    dfa.stateNext[i*dfa.numChars+k] = (word)m;
			    }
		    }
	    }

	    dfa.numStates = curDstate;
    }

    void FinalizeDFA()
    {
	    if (finalized)
            return;
        ComputeDFA();
        MinimizeDFA();
        finalized = 1;
    }

// Token Retrieval -----------------------------------------

    dword GetTokenNFA(bool advance, SLexToken* outToken)
    {
        CLexNfaNode* acceptNode;
        CLexNfaNode* acceptNodeCandidate;
        dword i, consumed, acceptConsumed;
        SLexToken token;
        char* startTextPtr = textPtr;
        dword startLineCount = lineCount;
        dword startColumnCount = columnCount;

        if (!textPtr)
            return(0);
        
        token.mTag = 0;
        while (!token.mTag)
        {
            acceptNode = NULL;
            acceptConsumed = 0;
            consumed = 0;
            
            getTokenNfaNodeSet.Empty();
            getTokenNfaNodeSet += nFact.IdForNode(startNode);
            ComputeEpsilonClosure(getTokenNfaNodeSet);

            token.mTextLine = lineCount;
            token.mTextColumn = columnCount;

            while(1)
            {
                if (!textPtr[consumed])
                    break; // end of input stream
                if (textPtr[consumed] == 13)
                {
                    consumed++;
                    continue; // carriage returns are ignored, only \n line feeds are processed
                }
            
				if ((!isCaseSensitive) && (textPtr[consumed] >= 'A') && (textPtr[consumed] <= 'Z'))
					ComputeMove(getTokenNfaNodeSet, (byte)((textPtr[consumed] - 'A') + 'a'));
				else
					ComputeMove(getTokenNfaNodeSet, (byte)textPtr[consumed]);

                ComputeEpsilonClosure(getTokenNfaNodeSet);
                if (getTokenNfaNodeSet.IsEmpty())
                    break;

                columnCount++;
                if (textPtr[consumed] == 10)
                {
                    lineCount++; columnCount = 0;
                }
                else if (textPtr[consumed] == 9)
                    columnCount += tabColumns-1;
                consumed++;
                acceptNodeCandidate = NULL;
                for (i=0;i<getTokenNfaNodeSet.size;i++)
                {
                    if ((!(nFact.NodeForId(i)->flags & NFANF_ACCEPTING)) || (!getTokenNfaNodeSet.Contains(i)))
                        continue;
                    if ((!acceptNodeCandidate) || (acceptNodeCandidate->priority < nFact.NodeForId(i)->priority))
                        acceptNodeCandidate = nFact.NodeForId(i);
                }
                if (acceptNodeCandidate)
                {
                    acceptNode = acceptNodeCandidate;
                    acceptConsumed = consumed;
                }
            }
            if (!acceptNode)
            {
                if (outToken)
                {
                    outToken->mTag = 0;
                    outToken->mLexeme = NULL;
                    outToken->mLexemeLen = 0;
                }
                if (!advance)
                {
                    textPtr = startTextPtr;
                    lineCount = startLineCount;
                    columnCount = startColumnCount;
                }
                return(0);
            }
            
            token.mTag = acceptNode->tag;
            token.mLexeme = textPtr;
            token.mLexemeLen = acceptConsumed;

            textPtr += acceptConsumed;

            if (acceptNode->tokenFunc)
			{
				char errorBuf[256] = {0};
				if (!acceptNode->tokenFunc(this, &token, errorBuf, 256))
				{
					if (!advance)
					{
						textPtr = startTextPtr;
						lineCount = startLineCount;
						columnCount = startColumnCount;
					}
					if (outToken)
						*outToken = token;
					LexSetLastError(errorBuf);
					return(0);
				}
			}
        }
        
        if (!advance)
        {
            textPtr = startTextPtr;
            lineCount = startLineCount;
            columnCount = startColumnCount;
        }
        if (outToken)
            *outToken = token;
        return(token.mTag);
    }

    dword GetToken(bool advance, SLexToken* outToken)
    {
        dword consumed, acceptConsumed;
        word curstate;
        CLexDfaAcceptItem* acc;
        SLexToken token;
        char* startTextPtr = textPtr;
        dword startLineCount = lineCount;
        dword startColumnCount = columnCount;

        if (!finalized)
            return(GetTokenNFA(advance, outToken));

        if (!textPtr)
            return(0);
        
        token.mTag = 0;
        while (!token.mTag)
        {
            acceptConsumed = 0;
            consumed = 0;
	        curstate = (word)dfa.initialState;
            acc = NULL;

            token.mTextLine = lineCount;
            token.mTextColumn = columnCount;

	        while (1)
	        {
                //printf("Processing %c\n", textPtr[consumed]);
				//if (!strncmp(textPtr, "0.4", 3))
				//	printf("Snargus!");
                
                if (!textPtr[consumed])
                    break; // end of input stream
                if (textPtr[consumed] == 13)
                {
                    consumed++;
                    continue; // carriage returns are ignored, only \n line feeds are processed
                }
            
				if ((!isCaseSensitive) && (textPtr[consumed] >= 'A') && (textPtr[consumed] <= 'Z'))
				{
					if ((curstate = dfa.stateNext[curstate*dfa.numChars+((textPtr[consumed] - 'A') + 'a')]) == 0xFFFF)
						break;
				}
				else
				{
					if ((curstate = dfa.stateNext[curstate*dfa.numChars+textPtr[consumed]]) == 0xFFFF)
						break;
				}

                columnCount++;
                if (textPtr[consumed] == 10)
                {
                    lineCount++; columnCount = 0;
                }
                else if (textPtr[consumed] == 9)
                    columnCount += tabColumns-1;
                consumed++;
		        if (dfa.acceptTable[curstate].flags & NFANF_ACCEPTING)
		        {
			        acc = &dfa.acceptTable[curstate];
			        acceptConsumed = consumed;
		        }
	        }
            if (!acc)
            {
                if (outToken)
                {
                    outToken->mTag = 0;
                    outToken->mLexeme = NULL;
                    outToken->mLexemeLen = 0;
                }
                if (!advance)
                {
                    textPtr = startTextPtr;
                    lineCount = startLineCount;
                    columnCount = startColumnCount;
                }
                //printf("Returning no match\n");
                return(0);
            }            
            
            token.mTag = acc->tag;
            token.mLexeme = textPtr;
            token.mLexemeLen = acceptConsumed;

            textPtr += acceptConsumed;

            if (acc->tokenFunc)
			{
				char errorBuf[256] = {0};
				if (!acc->tokenFunc(this, &token, errorBuf, 256))
				{
					if (!advance)
					{
						textPtr = startTextPtr;
						lineCount = startLineCount;
						columnCount = startColumnCount;
					}
					if (outToken)
						*outToken = token;
					LexSetLastError(errorBuf);
					return(0);
				}
			}
        }

        if (!advance)
        {
            textPtr = startTextPtr;
            lineCount = startLineCount;
            columnCount = startColumnCount;
        }
        if (outToken)
            *outToken = token;
        //printf("Returning tag %d\n", token.tag);
        return(token.mTag);
    }

	// ILexLexer
	NBool LexDestroy()
	{
		delete this;
		return(1);
	}
	NBool LexCaseSensitivity(NBool inIsCaseSensitive)
	{
		isCaseSensitive = (inIsCaseSensitive!=0);
		return(1);
	}
	NBool LexRegisterToken(NDword inTag, const NChar* inRegex)
	{
	    return(RegisterToken((char*)inRegex, inTag));
	}
	NByte LexTokenPriority(NByte inPriority)
	{
		NByte oldPriority = activePriority;
		activePriority = inPriority;
		return(oldPriority);
	}
	NBool LexTokenIntercept(NDword inTag, bool (*inIntercept)(ILexLexer*, SLexToken*, char*, int))
	{
		SetIntercept(inTag, inIntercept);
		return(1);
	}
	NBool LexFinalize()
	{
		FinalizeDFA();
		return(1);
	}
	NBool LexSetText(const NChar* inText, NDword inLine=0, NDword inColumn=0, NDword inTabColumns=8)
	{
		textPtr = (char*)inText;
		lineCount = inLine;
		columnCount = inColumn;
		tabColumns = inTabColumns;
		if (!tabColumns)
			tabColumns = 8;
		return(1);
	}
	NChar* LexGetText(NDword* outLine=0, NDword* outColumn=0)
	{
		if (outLine) *outLine = lineCount;
		if (outColumn) *outColumn = columnCount;
		return(textPtr);
	}
	NChar LexGetChar(NBool inAdvance=true)
	{
		if (!textPtr)
			return(0);
		if (!inAdvance)
			return(*textPtr);
		columnCount++;
		if (*textPtr == 10)
		{
			lineCount++;
			columnCount = 0;
		}
		textPtr++;
		return(textPtr[-1]);
	}
	NDword LexGetToken(SLexToken* outToken, NBool inAdvance=true)
	{
	    return(GetToken(inAdvance, outToken));
	}
	char* LexGetLastError()
	{
		return(lexErrorStr);
	}
	const char* LexGetStockRegex(ELexStockRegex inStockRegex)
	{
		switch(inStockRegex)
		{
		case LEXREGEX_Whitespace: return("[ \\t\\n]*"); break;
		case LEXREGEX_EolComment: return("//.*"); break;
		case LEXREGEX_BlockComment: return("\\/\\*"); break;
		case LEXREGEX_Identifier: return("[a-zA-Z_]([a-zA-Z0-9_])*"); break;
		case LEXREGEX_DecInteger: return("[0-9]+"); break;
		case LEXREGEX_HexInteger: return("0[xX][0-9a-fA-F]+"); break;
		case LEXREGEX_OctInteger: return("0[qQ][0-7]+"); break;
		case LEXREGEX_BinInteger: return("0[bB][0-1]+"); break;
		case LEXREGEX_Float: return("([0-9]+[Ee][\\+\\-]?[0-9]+)|([0-9]*\\.[0-9]+([Ee][\\+\\-]?[0-9]+)?)|([0-9]+\\.[0-9]*([Ee][\\+\\-]?[0-9]+)?)"); break;
		case LEXREGEX_String: return("\\\"(\\\\.|[^\\\\\"])*\\\""); break;
		case LEXREGEX_Character: return("'(\\\\.|[^\\\\'])+'"); break;	
		default: return(NULL); break;
		}
	}
};

//============================================================================
//    PRIVATE DATA
//============================================================================
//============================================================================
//    GLOBAL DATA
//============================================================================
//============================================================================
//    PRIVATE FUNCTIONS
//============================================================================
//============================================================================
//    GLOBAL FUNCTIONS
//============================================================================
ILexLexer* ILexLexer::LexCreate()
{
	return(new CLexLexer);
}

//============================================================================
//    CLASS METHODS
//============================================================================

} // namespace FEAST
//****************************************************************************
//**
//**    END MODULE FEASTLEXMAIN.CPP
//**
//****************************************************************************

