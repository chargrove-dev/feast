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
//**    FEASTPRSMAIN.CPP
//**    Parsing
//**
//****************************************************************************
//============================================================================
//    HEADERS
//============================================================================

#include "FeastInternal.h"

#include <vector>

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

// rule flags
enum
{
	RULEF_TERMINAL	= 0x00000001,	// auto-added to terminal rules
	RULEF_CLOSURE	= 0x00000002,	// temporary flag, rule has been processed for state closure
	RULEF_GENERATED	= 0x00000004,	// auto-added to generated terminals
};

enum
{
	PRODTOKEN_INVALID=0,
	PRODTOKEN_CHARACTER,
	PRODTOKEN_STRING,
	PRODTOKEN_IDENTIFIER,
	PRODTOKEN_INTEGER,
    PRODTOKEN_COLON,
};

//============================================================================
//    CLASSES / STRUCTURES
//============================================================================
// AST node structure
class CPrsASTNode
: public IPrsNode
{
public:
    dword tag; // tag taken from production/terminal ast node tag
	CPrsASTNode* parent; // parent node, created by reduction
	dword childCount; // number of child pointers
	CPrsASTNode** children; // child node pointers
    SLexToken token; // used by terminals only, lexer token for this rule
	IPrsAttr* attr; // semantic attribute information used by client

	static NDword sNodeCount;

	void* operator new (size_t size) { sNodeCount++; return(ALLOC(NByte, (NDword)size)); }
	void operator delete (void* ptr) { sNodeCount--; FREE(ptr); }

	CPrsASTNode()
	{
		tag = 0;
		parent = NULL;
		childCount = 0;
		children = NULL;
		token.mTag = 0;
		attr = NULL;
	}
	~CPrsASTNode()
	{
		if (children)
		{
			for (NDword i=0;i<childCount;i++)
			{
				if (children[i])
					delete children[i];
			}
			FREE(children);
		}

		if (attr)
			attr->AttrDestroy();
	}

	// IPrsNode
	NBool NodeDestroy()
	{
		delete this;
		return(1);
	}
	NDword NodeGetTag() { return(tag); }
	SLexToken* NodeGetToken() { return(token.mTag ? &token : NULL); }
	IPrsNode* NodeGetParent() { return(parent); }
	NDword NodeGetChildCount() { return(childCount); }
	IPrsNode* NodeGetChild(NDword inIndex)
	{
		if (inIndex >= childCount)
			return(NULL);
		return(children[inIndex]);
	}
	void NodeSetAttr(IPrsAttr* inAttr) { attr = inAttr; }
	IPrsAttr* NodeGetAttr() { return(attr); }
};
NDword CPrsASTNode::sNodeCount = 0;

// CST node structure
class CPrsCSTNode
{
public:    
	CPrsASTNode* astNode; // abstract syntax tree node if applicable
    CPrsCSTNode* parent; // parent node, created by reduction
    CPrsCSTNode* firstChild; // first child of this parent, source of reduction
    CPrsCSTNode* nextSibling; // subsequent children in same parent
    SLexToken token; // used by terminals only, lexer token for this rule
    void* ruleInternal; // internal use only - left-side rule this node represents
    void* prodInternal; // internal use only - for nonterminals, production which led to this rule (null for terminals)

	static NDword sNodeCount;

	void* operator new (size_t size) { sNodeCount++; return(ALLOC(NByte, (NDword)size)); }
	void operator delete (void* ptr) { sNodeCount--; FREE(ptr); }

	CPrsCSTNode()
	{
		astNode = NULL;
		parent = firstChild = nextSibling = NULL;
		token.mTag = 0;
		ruleInternal = prodInternal = NULL;
	}
	~CPrsCSTNode()
	{
		while (firstChild)
		{
			CPrsCSTNode* next = firstChild->nextSibling;
			delete firstChild;
			firstChild = next;
		}
	}

	NBool NodeDestroy()
	{
		delete this;
		return(1);
	}
	const NChar* NodeGetName();
	CPrsASTNode* NodeGetASTNode() { return(astNode); }
	NBool NodeSetASTNode(CPrsASTNode* inAttr) { astNode = inAttr; return(1); }
	SLexToken* NodeGetToken() { return(token.mTag ? &token : NULL); }
	CPrsCSTNode* NodeGetParent() { return(parent); }
	CPrsCSTNode* NodeGetFirstChild() { return(firstChild); }
	CPrsCSTNode* NodeGetNext() { return(nextSibling); }
};
NDword CPrsCSTNode::sNodeCount = 0;

//-----------------------------------------------
// PRODUCTIONS / RULES
//-----------------------------------------------
class CPrsParseRule;

class CPrsParseProd
{
public:
	CPrsParseProd* next; // next production for same rule
    CPrsParseRule* parentRule; // rule this production is a member of
    dword numItems; // number of items in this production
	//FPrsRuleFunc ruleEvent; // rule event callback function to call at end of production
	dword* itemAstIndices; // [numItems] array of one-based AST indices for the given CST nodes, zero meaning not used in AST
	NDword astNodeTag; // tag of AST node to generate, or zero to use the existing node with itemAstIndex 1.

    // valid before finalize
    char* definition; // string production definition
    SLexToken* itemTokens; // [numItems] array of item tokens pointing into definition

    // valid after finalize
    CPrsParseRule** itemRules; // [numItems] array of item rule links

	~CPrsParseProd()
	{
		if (itemRules)
			FREE(itemRules);
		if (itemAstIndices)
			FREE(itemAstIndices);
	}

    int operator == (CPrsParseProd& p)
    {
        if (parentRule != p.parentRule)
            return(0);
		dword minItems = numItems;
		if (minItems > p.numItems)
			minItems = p.numItems;
        for (dword i=0;i<minItems;i++)
        {
            if (itemRules[i] != p.itemRules[i])
                return(0);
        }
        return(1);
    }
};

class CPrsParseRule
{
public:
    char* ruleName; // rule name (left-hand side result)
	char* ruleNameOrg; // original rule name (no wacky prefixes etc)
	char* ruleAlias; // rule alias, if any
    dword flags; // RULEF_ flags
    dword index; // index in rule pool
    dword lexTag; // lexer tag (terminals only)
    CPrsParseProd* firstProduction; // first in list of productions (nonterminals only)
	//FPrsTermFunc termEvent; // terminal evaluation event (terminals only)
	NDword termAstNodeTag; // tag of AST node to generate (terminals only, since terminals don't have any productions)

    void SetName(char* name)
    {
        if (ruleName)
        {
            FREE(ruleName);
            ruleName = NULL;
        }
        if (!name)
            return;
        ruleName = ALLOC(char, (NDword)strlen(name)+1);
        strcpy(ruleName, name);
    }

    void SetOrgName(char* name)
    {
        if (ruleNameOrg)
        {
            FREE(ruleNameOrg);
            ruleNameOrg = NULL;
        }
        if (!name)
            return;
        ruleNameOrg = ALLOC(char, (NDword)strlen(name)+1);
        strcpy(ruleNameOrg, name);
    }

    void SetAlias(char* name)
    {
        if (ruleAlias)
        {
            FREE(ruleAlias);
            ruleAlias = NULL;
        }
        if (!name)
            return;
        ruleAlias = ALLOC(char, (NDword)strlen(name)+1);
        strcpy(ruleAlias, name);
    }

    CPrsParseRule()
	{
		ruleName = ruleNameOrg = ruleAlias = NULL;
		firstProduction = NULL;
		flags = index = lexTag = termAstNodeTag = 0;
		/*termEvent = NULL;*/
	}
    ~CPrsParseRule()
	{
		SetName(NULL);
		SetOrgName(NULL);
		SetAlias(NULL);

		CPrsParseProd* next;
		for (CPrsParseProd* prod = firstProduction; prod; prod = next)
		{
			next = prod->next;
			prod->~CPrsParseProd();
			FREE(prod);
		}
	}
};

const NChar* CPrsCSTNode::NodeGetName()
{
	if (!ruleInternal)
		return("");
	CPrsParseRule* rule = (CPrsParseRule*)ruleInternal;
	NChar* rname = rule->ruleAlias;
	if (!rname)
		rname = rule->ruleNameOrg;
	if (!rname)
		rname = rule->ruleName;
	return(rname);
}
//-----------------------------------------------
// STATES / STATE PRODUCTIONS
//-----------------------------------------------
class CPrsParser;

class CPrsParseStateProd
{
public:
    CPrsParseStateProd* next; // next kernel item production
    CPrsParseProd* prod; // actual rule production
    dword dotPos; // closure dot position (0=start, prod->numItems=end)
    int closureFlag; // closure has been performance (used during state generation)

    void* operator new(size_t size, CPrsParser* parser, dword numRules);
    
    inline int Matches(CPrsParseStateProd& p, dword dotAdd) // same as == but with unknown comparison dotPos
    {
        return((*prod == *p.prod) && (dotPos==(p.dotPos+dotAdd)));
    }
};

class CPrsParseState
{
private:	
	static int ProdSortCompare(const void* arg1, const void* arg2)
	{
		CPrsParseStateProd* p1 = *((CPrsParseStateProd**)arg1);
		CPrsParseStateProd* p2 = *((CPrsParseStateProd**)arg2);
		if (p1->prod->parentRule->index > p2->prod->parentRule->index)
			return(-1);
		else if (p1->prod->parentRule->index < p2->prod->parentRule->index)
			return(1);
		return(0);
	}

public:
    CPrsParseStateProd* firstProd; // first kernel item production
    CPrsParseState** nextRuleStates; // [numRules]
    bool partitioningFlag; // used during partitioning stage

	void *operator new(size_t size, CPrsParser* parser, dword numRules);

	~CPrsParseState() { if (nextRuleStates) FREE(nextRuleStates); }

	int Matches(CPrsParseState& s, int groupMatch, dword dotAdd)
	{
		// see if this contains the set of kernel items (w/adjusted dotPos) in s's given group
		CPrsParseStateProd *prod, *prod2;
		for (prod2=s.firstProd;prod2;prod2=prod2->next)
		{			
			if ((s.partitioningFlag) && (prod2->closureFlag != groupMatch))
				continue;
			for (prod=firstProd;prod;prod=prod->next)
			{
				if (prod->Matches(*prod2, dotAdd))
					break;
			}
			if (!prod)
				return(0); // didn't find a match for prod2
		}
		return(1);
	}

	void SortStateProductions()
	{
		static CPrsParseStateProd* prodArray[1024]; // hc
		CPrsParseStateProd* prod;
		dword i, count;
		for (i=0,prod=firstProd; prod; i++,prod=prod->next)
			prodArray[i] = prod;
		count = i;
		qsort(prodArray, count, sizeof(CPrsParseStateProd*), ProdSortCompare);
		for (i=0; i<count-1; i++)
			prodArray[i]->next = prodArray[i+1];
		prodArray[i]->next = NULL;
		firstProd = prodArray[0];
	}
};

//-----------------------------------------------
// PARSER
//-----------------------------------------------
#define PARSER_MAXRULES 512 // hc
#define PARSER_MAXSTATES 1024 // hc
#define PARSER_MAXSTATEPRODS 16384 // hc

class CPrsParser
: public IPrsParser
{
public:
	CC_MEMOPERATORS

    CPrsParseRule rulePool[PARSER_MAXRULES];
    dword rulePoolIndex;
    CPrsParseState statePool[PARSER_MAXSTATES];
    dword statePoolIndex;
    CPrsParseStateProd stateProdPool[PARSER_MAXSTATEPRODS];
    dword stateProdPoolIndex;
	ILexLexer* prodLexer;
	NChar errorStr[1024];
	NDword errorLine, errorColumn;

    ILexLexer* lexer;
    bool finalized;
	dword lookaheadLimit;

    CPrsParseRule* MakeRule(char* name, bool* existed, bool addable)
    {
	    CPrsParseRule* rule;
	    for (dword i=0;i<rulePoolIndex;i++)
	    {
		    if (!strcmp(name, rulePool[i].ruleName))
		    {
			    if (existed)
				    *existed = 1;
			    return(&rulePool[i]);
		    }
	    }
	    if (!addable)
	    {
		    if (existed)
			    *existed = 0;
		    return(NULL);
	    }
	    if (rulePoolIndex >= PARSER_MAXRULES)
		    FEAST_ERROR("CPrsParser::MakeRule: Exceeded maximum rule count");
	    rule = &rulePool[rulePoolIndex];
	    rule->SetName(name);
        rule->flags = 0;
	    rule->index = rulePoolIndex;
	    rule->lexTag = 0;
	    rule->firstProduction = NULL;
		//rule->termEvent = NULL;
	    rulePoolIndex++;
	    if (existed)
		    *existed = 0;
	    return(rule);    
    }

    CPrsParseRule* RuleForLexTag(dword tag)
    {
	    for (dword i=0;i<rulePoolIndex;i++)
	    {
		    if (rulePool[i].lexTag == tag)
			    return(&rulePool[i]);
	    }
        return(NULL);
    }

    void RegisterTerminal(char* name, dword tag, dword astNodeTag, /*FPrsTermFunc*/void* event)
    {
        if (!name || !tag)
            return; // zero tags are auto-skipped by the lexer and shouldn't be registered
        bool existed;
        CPrsParseRule* rule = MakeRule(name, &existed, 1);
        if (existed)
            FEAST_ERROR("CPrsParser::RegisterTerminal: Rule \"%s\" already exists; terminal tags cannot share rules (share tags instead)", name);
        CPrsParseRule* tagRule;
        if ((tagRule = RuleForLexTag(tag)))
            FEAST_ERROR("CPrsParser::RegisterTerminal: Rule \"%s\" has same tag as rule \"%s\"", name, tagRule->ruleName);
        rule->flags |= RULEF_TERMINAL;
        rule->lexTag = tag;
		//rule->termEvent = event;
		rule->termAstNodeTag = astNodeTag;
    }

    void InitProductionLexer()
    {
        if (!prodLexer)
        {
            if (!(prodLexer = ILexLexer::LexCreate()))
                FEAST_ERROR("CPrsParser::RegisterNonTerminal: Cannot create production lexer");
            
            prodLexer->LexTokenPriority(0);
            prodLexer->LexRegisterToken(0, "."); // trash monster
            prodLexer->LexTokenPriority(1);
            prodLexer->LexRegisterToken(0, prodLexer->LexGetStockRegex(LEXREGEX_Whitespace)); // skip whitespace
            prodLexer->LexRegisterToken(PRODTOKEN_CHARACTER, prodLexer->LexGetStockRegex(LEXREGEX_Character));
            prodLexer->LexRegisterToken(PRODTOKEN_STRING, prodLexer->LexGetStockRegex(LEXREGEX_String));
            prodLexer->LexRegisterToken(PRODTOKEN_IDENTIFIER, "[a-zA-Z_!]([a-zA-Z_!]|[0-9])*");
			prodLexer->LexRegisterToken(PRODTOKEN_INTEGER, prodLexer->LexGetStockRegex(LEXREGEX_DecInteger));
            prodLexer->LexRegisterToken(PRODTOKEN_COLON, "\\:");
            prodLexer->LexFinalize();
        }    
    }

    void RegisterNonTerminal(char* name, char* definition, dword astNodeTag, /*FPrsRuleFunc*/void* event)
    {
        static SLexToken tempTokens[256]; // temporary token buffer, maximum production tokens
		static dword tempAstIndices[256]; // temporary ast indices buffer
        static SLexToken token;
        CPrsParseRule* rule;
        CPrsParseProd* prod;

        InitProductionLexer();

        if (!name || !definition)
            return;
        rule = MakeRule(name, NULL, 1);
        if (rule->flags & RULEF_TERMINAL)
            FEAST_ERROR("CPrsParser::RegisterNonTerminal: Rule \"%s\" is used by a terminal", name);

        prod = ALLOC(CPrsParseProd, 1);
        prod->next = NULL;
        prod->parentRule = rule;
        prod->numItems = 0;
        prod->definition = ALLOC(char, (NDword)strlen(definition)+1);
        strcpy(prod->definition, definition);
        prod->itemTokens = NULL;
        prod->itemRules = NULL;
		//prod->ruleEvent = NULL;
		prod->itemAstIndices = NULL;
		prod->astNodeTag = astNodeTag;

        prodLexer->LexSetText(prod->definition, 0, 0, 0);
		bool astIndexFound = 0;
        while (prodLexer->LexGetToken(&token))
        {
            if (token.mTag == PRODTOKEN_COLON)
			{
				if (!prod->numItems)
					FEAST_ERROR("CPrsParser::RegisterNonTerminal: Rule \"%s\" has a production with an invalid AST marker", name);
				if (prodLexer->LexGetToken(&token)!=PRODTOKEN_INTEGER)
					FEAST_ERROR("CPrsParser::RegisterNonTerminal: Rule \"%s\" has a production with an invalid AST marker", name);
				char buf[256];
				sprintf(buf, "%0.*s", token.mLexemeLen, token.mLexeme);
				tempAstIndices[prod->numItems-1] = atoi(buf);
				astIndexFound = 1;
				continue;
			}
		    // anything else is an identifier, character, or string (all copied as-is and dealt with at finalize)
		    tempTokens[prod->numItems] = token;
			tempAstIndices[prod->numItems] = 0;
		    prod->numItems++;
        }
        if (!prod->numItems)
            FEAST_ERROR("CPrsParser::RegisterNonTerminal: Rule \"%s\" has production with no valid items", name);
        prod->itemTokens = ALLOC(SLexToken, prod->numItems);
		prod->itemAstIndices = ALLOC(dword, prod->numItems);
        for (dword i=0;i<prod->numItems;i++)
        {
            prod->itemTokens[i] = tempTokens[i];
			prod->itemAstIndices[i] = tempAstIndices[i];
        }
		if (!astIndexFound)
			prod->itemAstIndices[0] = 1; // if not a single explicit AST index is specified, then use first item by default
		//prod->ruleEvent = event;

        prod->next = rule->firstProduction;
        rule->firstProduction = prod;
    }

    void ResolveProductionItemRules()
    {
	    for (dword i=0;i<rulePoolIndex;i++)
	    {
		    for (CPrsParseProd* prod = rulePool[i].firstProduction; prod; prod = prod->next)
		    {
			    prod->itemRules = ALLOC(CPrsParseRule*, prod->numItems);
			    for (dword k=0;k<prod->numItems;k++)
			    {
				    SLexToken* token = &prod->itemTokens[k];
				    static char buf[1024];
				    bool existed;

				    if ((token->mLexeme[0] != '\'') && (token->mLexeme[0] != '"'))
				    { // identifier, match up to rule
					    sprintf(buf, "%0.*s", token->mLexemeLen, token->mLexeme);
					    prod->itemRules[k] = MakeRule(buf, &existed, 0);
					    if (!existed)
						    FEAST_ERROR("CPrsParser::ResolveProductionItemRules: Unknown terminal/nonterminal \"%s\"", buf);
				    }
				    else
				    {
					    static char namebuf[256];

					    sprintf(buf, "%0.*s", token->mLexemeLen, token->mLexeme);
					    // generated "immediate" terminals, above highest priority.
					    // these might override existing terminal matches, so relays are used when
					    // necessary to say that a token could resolve to either the generated rule terminal
					    // or the original (relayed) terminal.
					    
					    // since $ is not a legal character in a rule name, it is used to identify generated terminals
					    
					    if (buf[0] == '\'')
					    {
						    // character, forced to literal
						    // Does NOT accept escape sequences
						    namebuf[0] = '$';
						    namebuf[1] = '\\';
						    namebuf[2] = buf[1];
						    namebuf[3] = 0;
					    }
					    else
					    {
						    // string, force all regex symbols to literal since these are not supposed to be regexs
						    // Does NOT accept escape sequences
						    static char* literals = "|*+?()[]^-\\.";
						    char* iptr = buf, *optr = namebuf, *litptr;

						    *optr++ = '$';
						    iptr++; // skip initial quote
						    while((*iptr) && (*iptr != '"'))
						    {
							    for (litptr=literals;*litptr;litptr++)
							    {
								    if (*iptr == *litptr)
								    {
									    *optr++ = '\\';
									    break;
								    }
							    }
							    *optr++ = *iptr++;
						    }
						    *optr = 0;
					    }
					    
					    CPrsParseRule* rule = MakeRule(namebuf, &existed, 0);
					    if (!existed)
					    {
							// register the new immediate terminal
							RegisterTerminal(namebuf, 0xFFFF5150, NULL, NULL);
                            rule = MakeRule(namebuf, NULL, 0);
                            rule->lexTag = rule->index | 0x40000000; // make it high to guarantee no conflicts
							rule->flags |= RULEF_GENERATED;
						    byte oldPriority = lexer->LexTokenPriority(255); // priority 255 is reserved for generated terminals
                            if (!lexer->LexRegisterToken(rule->lexTag, namebuf+1))
                                FEAST_ERROR("CPrsParser::ResolveProductionItemRules: RegisterToken failure on \"%s\", tag %d",
                                    namebuf+1, rule->lexTag);
                            lexer->LexTokenPriority(oldPriority);
					    }
					    prod->itemRules[k] = rule;
						sprintf(buf, "%0.*s", token->mLexemeLen, token->mLexeme);
						rule->SetOrgName(buf);
						if (prod->itemAstIndices[k])
							rule->termAstNodeTag = 0xFFFF0001;
				    } // else
			    } // for k			    
                FREE(prod->itemTokens); prod->itemTokens = NULL;
			    FREE(prod->definition); prod->definition = NULL;
		    } // for prod
	    } // for i
    }

    void GenerateStates()
    {
	    CPrsParseRule* startRule = MakeRule("!start", 0, 0);
	    if (!startRule)
		    FEAST_ERROR("CPrsParser::GenerateStates: No start rule");
	    CPrsParseState* startState = new(this, rulePoolIndex) CPrsParseState;

	    // add all startRule productions as kernel productions
	    startRule->flags |= RULEF_CLOSURE;
	    for (CPrsParseProd* prod = startRule->firstProduction; prod; prod = prod->next)
	    {
		    CPrsParseStateProd* sprod = new(this, rulePoolIndex) CPrsParseStateProd;
		    sprod->prod = prod;
		    sprod->dotPos = 0;
            sprod->closureFlag = 0;
		    sprod->next = startState->firstProd;
		    startState->firstProd = sprod;
	    }

	    int lastIndex = 0; // initialize to zero so the start state will be pending
	    while ((int)statePoolIndex != lastIndex)
	    {		
		    int old = lastIndex;
		    lastIndex = statePoolIndex;
		    for (int i=old; i<lastIndex; i++) // start at the first of the new states
		    {
			    CPrsParseState* state = &statePool[i];
			    // add all closure productions from these
			    bool gotone;
			    do
			    {
				    gotone = 0;
				    for (CPrsParseStateProd* sprod = state->firstProd; sprod; sprod = sprod->next)
				    {
					    if (sprod->closureFlag)
						    continue;
					    sprod->closureFlag = 1;
					    if (sprod->dotPos == sprod->prod->numItems)
						    continue; // dot is at the far right, no rule to process
					    CPrsParseRule *rule = sprod->prod->itemRules[sprod->dotPos];
					    if (rule->flags & RULEF_CLOSURE)
						    continue;
					    rule->flags |= RULEF_CLOSURE;
					    for (CPrsParseProd* prod = rule->firstProduction; prod; prod = prod->next)
					    {
						    // add all productions for the rule with the closure nonterminal to the right of the dot
						    // obviously only relevant to nonterminals (terminals don't have productions)
						    CPrsParseStateProd* sp2 = new(this, rulePoolIndex) CPrsParseStateProd;
						    sp2->prod = prod;
						    sp2->dotPos = sp2->closureFlag = 0;
						    sp2->next = state->firstProd;
						    state->firstProd = sp2; // fortunately, putting it at the top won't affect sprod
						    gotone = 1;
					    }
				    }
			    } while (gotone);
			    // remove the rule closure flags
			    for (int k=0;k<(int)rulePoolIndex;k++)
				    rulePool[k].flags &= ~RULEF_CLOSURE;

			    // partition the state kernel productions into groups based on closure dot symbol
			    // use closureflag to denote group number for simplicity (-1 means not assigned, 0 means far right dot)
				CPrsParseStateProd* sprod;
			    for (sprod = state->firstProd; sprod; sprod = sprod->next)
				    sprod->closureFlag = -1;
			    int curgroup = 1;
			    do
			    {
				    gotone = 0;
				    for (sprod = state->firstProd; sprod; sprod = sprod->next)
				    {
					    if (sprod->closureFlag >= 0)
						    continue; // already has a group
					    gotone = 1;
					    // check for far right dot, i.e. the no-new-state group
					    if (sprod->dotPos == sprod->prod->numItems)
					    {
						    sprod->closureFlag = 0;
						    continue;
					    }
					    // check for existing groups
					    for (CPrsParseStateProd* sp2 = state->firstProd; sp2; sp2 = sp2->next)
					    {
						    if (sp2->closureFlag <= 0)
							    continue; // doesn't have group yet, or has far right group
						    if (sprod->prod->itemRules[sprod->dotPos] == sp2->prod->itemRules[sp2->dotPos])
						    {
							    sprod->closureFlag = sp2->closureFlag; // group match
							    break;
						    }
					    }
					    if (sprod->closureFlag >= 0)
						    continue; // existing group was found
					    // create a new group
					    sprod->closureFlag = curgroup;
					    curgroup++;
				    }
			    } while (gotone);
			    // all groups from 1 and above are new states, create them and add the appropriate items
			    
			    state->partitioningFlag = 1;
			    
			    for (int k=1;k<curgroup;k++)
			    {
				    CPrsParseState* nstate = NULL;
				    CPrsParseRule *transRule = NULL;

				    // determine the transition rule
				    for (sprod = state->firstProd; sprod; sprod = sprod->next)
				    {
					    if (sprod->closureFlag == k)
					    {
						    transRule = sprod->prod->itemRules[sprod->dotPos];
						    break;
					    }
				    }
				    if (!transRule)
					    FEAST_ERROR("CPrsParser::GenerateStates: No TransRule"); // should never happen since group index must have at least one member

				    // if a state already exists with the group set of kernel items (each at dotPos+1), use it
				    for (int m=0; m<(int)statePoolIndex; m++)
				    {
					    if (statePool[m].Matches(*state, k, 1))
					    {
						    nstate = &statePool[m];
						    break;
					    }
				    }

				    // make a new state
				    if (!nstate)
				    {
					    nstate = new(this, rulePoolIndex) CPrsParseState;
					    for (sprod = state->firstProd; sprod; sprod = sprod->next)
					    {
						    if (sprod->closureFlag != k)
							    continue; // different group
						    CPrsParseStateProd* sp2 = new(this, rulePoolIndex) CPrsParseStateProd;
						    sp2->prod = sprod->prod;
						    sp2->dotPos = sprod->dotPos+1; // move it over by one in the new state
						    sp2->closureFlag = 0;
						    sp2->next = nstate->firstProd;
						    nstate->firstProd = sp2;
					    }
				    }
				    // link to the new state on the group's transition symbol
				    sprod = nstate->firstProd;
				    if (!sprod)
					    FEAST_ERROR("CPrsParser::GenerateStates: No production to link on");
				    state->nextRuleStates[transRule->index] = nstate;
			    }
			    
			    state->partitioningFlag = 0;

			    // clear out the closure flags
			    for (sprod = state->firstProd; sprod; sprod = sprod->next)
				    sprod->closureFlag = 0;
		    }
	    }

		// sort all the productions in the states by reversed rule order
		for (dword i=0;i<statePoolIndex;i++)
			statePool[i].SortStateProductions();
    }

	void FireEvents(CPrsCSTNode* node)
	{
        CPrsParseRule* rule = (CPrsParseRule*)node->ruleInternal;
        CPrsParseProd* prod = (CPrsParseProd*)node->prodInternal;
	    CPrsCSTNode *n;
	    int i;
	    
		if (prod)
		{
			// nonterminal
			CPrsASTNode* astNodes[256];
			for (i=0;i<256;i++)
				astNodes[i] = NULL;
			for (i=0,n=node->firstChild; n; i++,n=n->NodeGetNext())
			{
				if (prod->itemAstIndices[i])
					astNodes[prod->itemAstIndices[i]-1] = n->NodeGetASTNode();
			}

			if (prod->astNodeTag)
			{
				// create new ast node
				int astNodeCount = 0;
				for (i=255;i>=0;i--)
				{
					if (astNodes[i])
					{
						astNodeCount = i+1;
						break;
					}
				}
				CPrsASTNode* astNode = new CPrsASTNode;
				astNode->tag = prod->astNodeTag;
				astNode->childCount = astNodeCount;
				astNode->children = ALLOC(CPrsASTNode*, astNodeCount);
				for (i=0;i<astNodeCount;i++)
				{
					if (astNodes[i])
						astNodes[i]->parent = astNode;
					astNode->children[i] = astNodes[i];
				}
				node->NodeSetASTNode(astNode);
			}
			else
			{
				// no new ast node is being generated, so propogate default child
				node->NodeSetASTNode(astNodes[0]);
			}
		}
		else if (rule && rule->termAstNodeTag && node->NodeGetToken())
		{
			// terminal
			CPrsASTNode* astNode = new CPrsASTNode;
			astNode->tag = rule->termAstNodeTag;
			astNode->token = *node->NodeGetToken();

			node->NodeSetASTNode(astNode);

			//LOG_Logf("TerminalEvent: %d %0.*s", node->NodeGetToken()->mTag, node->NodeGetToken()->mLexemeLen, node->NodeGetToken()->mLexeme);
		}
		else
		{
			// stub node, propogate child
			node->NodeSetASTNode(node->firstChild ? node->firstChild->NodeGetASTNode() : NULL);
		}
	}

    void RecursiveFireEvents(CPrsCSTNode* inNode)
    {
		/* CDH had to linearize this because it can cause stack overflows on large files
	    CPrsCSTNode *n;
		int i;
		// recurse all child nodes if applicable
		for (i=0,n=inNode->firstChild; n; i++,n=n->NodeGetNext())
			RecursiveFireEvents(n);

		FireEvents(inNode);
		*/

		std::vector<CPrsCSTNode*> eventNodes;
		typedef std::pair<CPrsCSTNode*, bool> StackNode;
		std::vector<StackNode> nodeStack;
		std::vector<CPrsCSTNode*> childNodes;
		
		nodeStack.push_back(StackNode(inNode, false));

		while (!nodeStack.empty())
		{
			CPrsCSTNode* node = nodeStack.back().first;
			bool childrenProcessed = nodeStack.back().second;

			if (!childrenProcessed)
			{
				nodeStack.back().second = true;

				childNodes.clear();
				CPrsCSTNode *n;
				int i;
				for (i=0,n=node->firstChild; n; i++,n=n->NodeGetNext())
					childNodes.push_back(n);
				for (i=(int)childNodes.size()-1; i>=0; --i)
					nodeStack.push_back(StackNode(childNodes[i], false));
			}
			else
			{
				eventNodes.push_back(node);
				nodeStack.pop_back();
			}
		}

		int nodeCount = (int)eventNodes.size();

		for (int iNode=0; iNode<nodeCount; ++iNode)
		{
			CPrsCSTNode* node = eventNodes[iNode];

			FireEvents(node);
		}
	}

    void FinalizeParser(char* startRuleName="start")
    {
	    char buf[256];
		RegisterNonTerminal("!start", "!eoi", NULL, NULL); // allow an empty input string
	    sprintf(buf, "%s !eoi", startRuleName);
	    RegisterNonTerminal("!start", buf, NULL, NULL);
	    
        //printf("Finalize: Resolving productions...\n");
        ResolveProductionItemRules();
        //printf("Finalize: Generating states...\n");
	    GenerateStates();
        //printf("Finalize: Finalizing lexer...\n");
	    lexer->LexFinalize();

	    finalized = 1;
    }

	void DestroyParseNode(CPrsCSTNode* node)
	{
		CPrsCSTNode *p, *prev = NULL;
		if (node->parent)
		{
			for (p = node->parent->firstChild; p && (p != node); p = p->nextSibling)
				prev = p;
			if (!prev && (node != node->parent->firstChild))
				FEAST_ERROR("DestroyParseNode: Badly formed node tree\n");
		}
		if (prev)
			prev->nextSibling = node->nextSibling;
		else if ((node->parent) && (node->parent->firstChild == node))
			node->parent->firstChild = node->nextSibling;
		for (p=node->firstChild; p; p=p->nextSibling)
			p->parent = NULL; // this is a bottom-up parse node removal; the children are created before the parents
	}

    CPrsCSTNode* Parse(char* text, dword inTabSpacing, char* outError, dword* outErrorLine, dword* outErrorColumn, TDatArray<CPrsCSTNode*>& nodeLog)
    {
	    SLexToken token;
	    CPrsParseRule* startRule;
	    CPrsParseState* state, *nextState;

	    startRule = MakeRule("!start", NULL, 0);
	    if (!startRule)
		    FEAST_ERROR("CPrsParser::Parse: No start rule");	    
	    if (!finalized)
		    FinalizeParser();

        class parsePosition_t
		{
		public:
			dword stackDepth;
			int* stateStack;
			CPrsCSTNode** nodeStack;
			CPrsParseStateProd* reduction;
			char* lexPtr;
			dword lexLine, lexColumn;
			parsePosition_t* next;
			CPrsCSTNode* createdNodes;
			dword lexTokenCount;

			parsePosition_t(dword inStackDepth, int* inStateStack, CPrsCSTNode** inNodeStack, CPrsParseStateProd* inReduction, char* inLexPtr, dword inLexLine, dword inLexColumn)
			{
				stackDepth = inStackDepth;
				stateStack = ALLOC(int, stackDepth);
				if (inStateStack)
					memcpy(stateStack, inStateStack, stackDepth*sizeof(int));
				nodeStack = ALLOC(CPrsCSTNode*, stackDepth);
				if (inNodeStack)
					memcpy(nodeStack, inNodeStack, stackDepth*sizeof(CPrsCSTNode*));
				reduction = inReduction;
				lexPtr = inLexPtr;
				lexLine = 0;
				lexColumn = 0;
				next = NULL;
				createdNodes = NULL;
				lexTokenCount = 0;
			}

			void CopyFrom(const parsePosition_t& inP, bool inConstruct)
			{
				stackDepth = inP.stackDepth;
				if (inConstruct)
					stateStack = ALLOC(int, stackDepth);
				memcpy(stateStack, inP.stateStack, stackDepth*sizeof(int));
				if (inConstruct)
					nodeStack = ALLOC(CPrsCSTNode*, stackDepth);
				memcpy(nodeStack, inP.nodeStack, stackDepth*sizeof(CPrsCSTNode*));
				reduction = inP.reduction;
				lexPtr = inP.lexPtr;
				lexLine = inP.lexLine;
				lexColumn = inP.lexColumn;
				next = NULL;
				createdNodes = inP.createdNodes;
				lexTokenCount = inP.lexTokenCount;
			}

			parsePosition_t(const parsePosition_t& inP) { CopyFrom(inP, 1); }
			parsePosition_t& operator = (const parsePosition_t& inP) { CopyFrom(inP, 0); return(*this); }

			~parsePosition_t()
			{
				if (stateStack)
					FREE(stateStack);
				if (nodeStack)
					FREE(nodeStack);
			}
			
		    void* operator new(size_t size) { return(ALLOC(char, (NDword)size)); }
			void operator delete(void* ptr) { FREE(ptr); }
		};

		// conflict position stack logs positions at conflict points in the parse
		parsePosition_t* conflictPositionStack = NULL;
		dword conflictPositionStackDepth = 0;

		// initialize active parse position, not allocated to an existing stack depth but the maximum
		parsePosition_t activePos(1024, NULL, NULL, NULL, text, 0, 0); // hc
	    activePos.stackDepth = 0;
	    // push on the start state
		activePos.stateStack[activePos.stackDepth] = 0; // 0 is start state
	    activePos.nodeStack[activePos.stackDepth] = NULL;
	    activePos.stackDepth++;

	    while (1)
	    {
		    // assuming we're not set up for infinite lookahead, eliminate any conflicts in the conflict stack beyond our lookahead limit
			if (conflictPositionStack && (lookaheadLimit != 0xFFFFFFFF))
			{
				parsePosition_t* posprev = NULL;
				parsePosition_t* pos;
				for (pos = conflictPositionStack; pos; pos = pos->next)
				{
					if ((pos->lexTokenCount + lookaheadLimit) < activePos.lexTokenCount)
						break;
					posprev = pos;
				}
				if (pos)
				{
					// everything at pos and after will die, so unlink now
					if (posprev)
						posprev->next = NULL;
					else
						conflictPositionStack = NULL;
					// kill off the old stuff
					while (pos)
					{
						parsePosition_t* posnext = pos->next;
						delete pos;
						pos = posnext;
						conflictPositionStackDepth--;
					}
				}
			}
			
			// retrieve the next token
			lexer->LexSetText(activePos.lexPtr, activePos.lexLine, activePos.lexColumn, inTabSpacing);

			NDword tokenTag;
			tokenTag = lexer->LexGetToken(&token);
			if (lexer->LexGetLastError()[0])
			{
				if (outErrorLine)
					*outErrorLine = token.mTextLine+1;
				if (outErrorColumn)
					*outErrorColumn = token.mTextColumn+1;
				if (outError)
					sprintf(outError, lexer->LexGetLastError());
				return(NULL);
			}

			if (!tokenTag)
			{
				token.mTag = MakeRule("!eoi", NULL, 0)->lexTag;
				token.mLexeme = "(eoi)";
				token.mLexemeLen = 5;
			}
		    
		    CPrsParseRule* tagRule = RuleForLexTag(token.mTag);
            if (!tagRule)
                FEAST_ERROR("CPrsParser::Parse: No rule matching terminal tag %d", token.mTag);

			{
				char* rname;
				if (tagRule->ruleNameOrg)
					rname = tagRule->ruleNameOrg;
				else
					rname = tagRule->ruleName;
				//printf("Debug: Grabbing token \"%0.*s\" (Rule %s) at Line %d Column %d\n", token.mLexemeLen, token.mLexeme, rname, activePos.lexLine, activePos.lexColumn);
			}

		    state = &statePool[activePos.stateStack[activePos.stackDepth-1]];

			// if we don't have a predetermined reduction, build up possibilities for shifting and reducing
			if (!activePos.reduction)
			{
				bool canShift=0, canReduce=0;
				bool isShifting=0, isReducing=0;
				
				// if there's a next state from this one on the current terminal rule, then we can shift
				nextState = state->nextRuleStates[tagRule->index];
				if (nextState)
					canShift = 1;

				// check for possible reductions
				dword numReduce=0;
				CPrsParseStateProd* prod;
				for (prod=state->firstProd; prod; prod=prod->next)
				{
					if (prod->dotPos != prod->prod->numItems)
						continue; // not a reduction
					numReduce++;
					activePos.reduction = prod; // last production in the chain is the first one defined, top priority
				}

				// if there's at least one reduction, we can reduce
				if (activePos.reduction)
					canReduce = 1;

				// if there's more than one possibility, push the conflicts (assuming we're not LR(0))
				if (((canShift && canReduce) || (numReduce > 1)) && (lookaheadLimit))
				{			
					// reduce/reduce conflicts
					if (numReduce > 1)
					{
						//printf("Debug: Logging %d reduction conflicts\n", numReduce);

						// since these are used last, push them on in reverse order (the last listed production gets pushed first)
						// push all reductions except the first (if we have a shift/reduce conflict as well, it'll push the first
						for (prod=state->firstProd; prod && prod->next; prod=prod->next)
						{
							if (prod->dotPos != prod->prod->numItems)
								continue; // not a reduction				
							parsePosition_t* pos = new parsePosition_t(activePos);
							pos->reduction = prod;
							pos->next = conflictPositionStack;
							conflictPositionStack = pos;
							conflictPositionStackDepth++;
							activePos.createdNodes = NULL;
						}
					}
					// shift/reduce conflicts
					if (canShift && canReduce)
					{
						//printf("Debug: Logging a shift/reduce conflict\n", numReduce);

						parsePosition_t* pos = new parsePosition_t(activePos);
						pos->reduction = activePos.reduction;
						pos->next = conflictPositionStack;
						conflictPositionStack = pos;
						conflictPositionStackDepth++;
						activePos.createdNodes = NULL;
					}

					//printf("Debug: Conflict depth is now %d\n", conflictPositionStackDepth);
				}

				// on the other end of the spectrum, if there's no possibilities, then try and pop a conflict back up
				if (!(canShift || canReduce))
				{
					if (conflictPositionStackDepth)
					{
						//printf("Debug: Backing up at conflict depth %d\n", conflictPositionStackDepth);

						// destroy all tree nodes that were formed since this action was chosen
						CPrsCSTNode* nextNode;
						for (CPrsCSTNode* node = activePos.createdNodes; node; node = nextNode)
						{
							nextNode = (CPrsCSTNode*)(node->astNode);
							//printf("Destroying node %X\n", node);
							DestroyParseNode(node);
						}
						
						//printf("Debug: Tree obliterated, continue backtrack...\n");

						// pop the most recent conflict
						activePos = *conflictPositionStack;
						parsePosition_t* nextConflict = conflictPositionStack->next;
						delete conflictPositionStack;
						conflictPositionStack = nextConflict;
						conflictPositionStackDepth--;
						continue; // start over
					}
					
					if (outError)
					{
						sprintf(outError, "Found \"%0.*s\"", token.mLexemeLen, token.mLexeme); // bad transition
						dword rcount=0;
						for (dword r=0;r<rulePoolIndex;r++)
						{
							if (!state->nextRuleStates[r])
								continue;
							char* rname = rulePool[r].ruleAlias;
							if (!rname)// && (!(rulePool[r].flags & RULEF_GENERATED)))
								continue;
							if (!rname)
								rname = rulePool[r].ruleNameOrg;
							if (!rname)
								rname = rulePool[r].ruleName;
							if (!rcount)
								sprintf(outError+strlen(outError), ", expecting %s", rname);
							else
								sprintf(outError+strlen(outError), ", %s", rname);
							rcount++;
						}
						//sprintf(outError+strlen(outError), "\n");
						
						if (outErrorLine)
							*outErrorLine = token.mTextLine+1;
						if (outErrorColumn)
							*outErrorColumn = token.mTextColumn+1;
					}
					return(NULL);
				}

				if (canShift)
					activePos.reduction = NULL; // the default action in a shift/reduce conflict is to shift
			}

			// if we still don't have a reduction, then that means we should shift
			if (!activePos.reduction)
		    {			    
				//printf("Debug: Shifting\n");

	            // the token read sticks on shifts
				activePos.lexPtr = lexer->LexGetText(&activePos.lexLine, &activePos.lexColumn);
				activePos.lexTokenCount++;

			    //CPrsCSTNode* node = ALLOC(CPrsCSTNode, 1);
				CPrsCSTNode* node = new CPrsCSTNode;
				nodeLog.AddItem(node);
				//printf("Created node %X\n", node);

			    node->parent = NULL;
			    node->firstChild = NULL;
			    node->nextSibling = NULL;
			    node->astNode = NULL;
                node->token = token;
			    node->prodInternal = NULL;
			    node->ruleInternal = tagRule;

				node->astNode = (CPrsASTNode*)activePos.createdNodes; // log the action that created this node incase we need to nuke it
				activePos.createdNodes = node;

			    activePos.nodeStack[activePos.stackDepth] = node;
			    activePos.stateStack[activePos.stackDepth] = (int)(nextState - statePool);
			    activePos.stackDepth++;

		    }
			else
		    {
				// we have a reduction
				//printf("Debug: Reduction present: %s -> ", activePos.reduction->prod->parentRule->ruleName);
				//for (dword prule=0;prule<activePos.reduction->prod->numItems;prule++)
				//	printf("%s ", activePos.reduction->prod->itemRules[prule]->ruleName);
				//printf("\n");
				activePos.stackDepth -= activePos.reduction->dotPos;
                state = &statePool[activePos.stateStack[activePos.stackDepth-1]];

			    if (activePos.reduction->prod->parentRule == startRule)
				    break; // accept (we're done!)
			    
			    nextState = state->nextRuleStates[activePos.reduction->prod->parentRule->index];
			    if (!nextState)
			    {
				    if (outError)
                    {
                        
						//sprintf(outError, "Bad reduction to \"%s\": \"%0.*s\"", activePos.reduction->prod->parentRule->ruleName,
                        //    token.mLexemeLen, token.mLexeme);
						sprintf(outError, "Found \"%0.*s\"", token.mLexemeLen, token.mLexeme);
						dword rcount=0;
						for (dword r=0;r<rulePoolIndex;r++)
						{
							if (!state->nextRuleStates[r])
								continue;
							char* rname = rulePool[r].ruleAlias;
							if (!rname)// && (!(rulePool[r].flags & RULEF_GENERATED)))
								continue;
							if (!rname)
								rname = rulePool[r].ruleNameOrg;
							if (!rname)
								rname = rulePool[r].ruleName;
							if (!rcount)
								sprintf(outError+strlen(outError), ", expecting %s", rname);
							else
								sprintf(outError+strlen(outError), ", %s", rname);
							rcount++;
						}
                        if (outErrorLine)
                            *outErrorLine = token.mTextLine+1;
                        if (outErrorColumn)
                            *outErrorColumn = token.mTextColumn+1;
                    }
					// get rid of the conflict stack memory
					while (conflictPositionStack)
					{
						parsePosition_t* nextConflict = conflictPositionStack->next;
						delete conflictPositionStack;
						conflictPositionStack = nextConflict;
					}

				    return(NULL);
			    }

			    //CPrsCSTNode* node = ALLOC(CPrsCSTNode, 1);
				CPrsCSTNode* node = new CPrsCSTNode;
				nodeLog.AddItem(node);
				//printf("Created node %X\n", node);

			    node->parent = NULL;
			    node->firstChild = NULL;
			    node->nextSibling = NULL;
			    node->astNode = NULL;
                node->token.mTag = 0;
                node->token.mLexeme = "";
                node->token.mLexemeLen = 0;
                node->token.mTextLine = node->token.mTextColumn = 0;
			    node->prodInternal = activePos.reduction->prod;
			    node->ruleInternal = activePos.reduction->prod->parentRule;

				node->astNode = (CPrsASTNode*)activePos.createdNodes; // log the action that created this node incase we need to nuke it
				activePos.createdNodes = node;

			    node->firstChild = activePos.nodeStack[activePos.stackDepth];
				dword i;
			    for (i=activePos.stackDepth;i<(activePos.stackDepth+activePos.reduction->dotPos-1);i++)
				{
					activePos.nodeStack[i]->parent = node;
				    activePos.nodeStack[i]->nextSibling = activePos.nodeStack[i+1];
				}
				activePos.nodeStack[i]->parent = node;
				activePos.nodeStack[i]->nextSibling = NULL;

				//printf("Debug: Reduction pushing state %d\n", (int)(nextState - statePool));

			    activePos.nodeStack[activePos.stackDepth] = node;
			    activePos.stateStack[activePos.stackDepth] = (int)(nextState - statePool);
			    activePos.stackDepth++;

				activePos.reduction = NULL;
		    }
	    }

		// get rid of the conflict stack memory
		while (conflictPositionStack)
		{
			parsePosition_t* nextConflict = conflictPositionStack->next;
			delete conflictPositionStack;
			conflictPositionStack = nextConflict;
		}
		
        RecursiveFireEvents(activePos.nodeStack[1]);
	    return(activePos.nodeStack[1]);
    }

	CPrsParser()
	{		
		rulePoolIndex = statePoolIndex = stateProdPoolIndex = 0;
		prodLexer = NULL;
		errorStr[0] = 0; errorLine = errorColumn = 0;
		memset(statePool, 0, PARSER_MAXSTATEPRODS*sizeof(CPrsParseState));
		memset(stateProdPool, 0, PARSER_MAXSTATEPRODS*sizeof(CPrsParseStateProd));
        
        if (!(lexer = ILexLexer::LexCreate()))
            FEAST_ERROR("CPrsParser::CPrsParser(): Cannot create terminal lexer");
        finalized = 0;
		lookaheadLimit = 1;
		
		// rule 0 is always the empty (epsilon) rule
		RegisterNonTerminal("NULL", "NULL", NULL, NULL); // heh
		
		// rule 1 is always the eoi terminal, never generated by lexer but stubbed
        // in by parser on lexical failure.  Uses a tag that's extremely unlikely to be
        // used directly.
        RegisterTerminal("!eoi", 0xFFFF1234, NULL, NULL);
	}

    ~CPrsParser()
    {
        if (prodLexer)
			prodLexer->LexDestroy();
		if (lexer)
			lexer->LexDestroy();
    }

	// IPrsParser
	NBool PrsDestroy()
	{
		delete this;
		return 1;
	}
	ILexLexer* PrsGetLexer()
	{
		return(lexer);
	}
	bool PrsRegisterT(const char* inRule, unsigned long inLexTag, unsigned long inNodeTag)
	{
		RegisterTerminal((char*)inRule, inLexTag, inNodeTag, NULL);
		return(1);
	}
	bool PrsRegisterNT(const char* inRule, const char* inProduction, unsigned long inNodeTag)
	{
		RegisterNonTerminal((char*)inRule, (char*)inProduction, inNodeTag, NULL);
		return(1);
	}
	NBool PrsBuild()
	{
		if (finalized)
			return(0); // already finalized, can't do it again
		FinalizeParser();
		return(1);
	}
	NBool PrsSetRuleAlias(const NChar* inRule, const NChar* inAlias)
	{
		if (!inRule)
			return(0);
		bool existed;
		CPrsParseRule* r = MakeRule((char*)inRule, &existed, 0);
		if (!r)
			return(0);
		r->SetAlias((char*)inAlias);
		return(1);
	}
	IPrsNode* PrsExecute(const NChar* inText, NDword inTabSpacing=8)
	{
		TDatArray<CPrsCSTNode*> nodeLog;
		//LOG_Logf("Execute: Pre-parse nodes: %d", CPrsCSTNode::sNodeCount);
		CPrsCSTNode* root = Parse((char*)inText, inTabSpacing, errorStr, &errorLine, &errorColumn, nodeLog);
		//LOG_Logf("Execute: Post-parse nodes: %d", CPrsCSTNode::sNodeCount);
		CPrsASTNode* astRoot = NULL;
		if (root)
			astRoot = root->NodeGetASTNode();

		// whether the parse succeeded or failed, we don't need the CST anymore (since if it succeeded
		// we'll have an AST), so go ahead and wipe out all generated CST nodes
		for (NDword i=0;i<nodeLog.GetCount();i++)
		{
			nodeLog[i]->firstChild = NULL; // so it won't recurse to children
			nodeLog[i]->NodeDestroy();
		}

		//LOG_Logf("Execute: Post-clean nodes: %d", CPrsCSTNode::sNodeCount);

		return(astRoot);
	}

	char* PrsGetLastError(unsigned long* outLine=0, unsigned long* outColumn=0)
	{
		if (outLine) *outLine = errorLine;
		if (outColumn) *outColumn = errorColumn;
		return(errorStr);
	}
};

void *CPrsParseState::operator new(size_t size, CPrsParser* parser, dword numRules)
{
	if (parser->statePoolIndex >= PARSER_MAXSTATES)
		FEAST_ERROR("CPrsParseState: Too many states");
	CPrsParseState *res = &parser->statePool[parser->statePoolIndex];
	parser->statePoolIndex++;
	res->firstProd = NULL;
	res->nextRuleStates = ALLOC(CPrsParseState*, numRules);
	memset(res->nextRuleStates, 0, numRules*sizeof(CPrsParseState*));
	res->partitioningFlag = 0;
	return(res);
}

void *CPrsParseStateProd::operator new(size_t size, CPrsParser* parser, dword numRules)
{
	if (parser->stateProdPoolIndex >= PARSER_MAXSTATEPRODS)
		FEAST_ERROR("CPrsParseStateProd: Too many state kernel productions");
	CPrsParseStateProd *res = &parser->stateProdPool[parser->stateProdPoolIndex];
	parser->stateProdPoolIndex++;
	res->next = NULL;
	res->prod = NULL;
	res->dotPos = res->closureFlag = 0;
	return(res);
}

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
IPrsParser* IPrsParser::PrsCreate(unsigned long inLookAhead)
{
	CPrsParser* prs = new CPrsParser;
	prs->lookaheadLimit = inLookAhead;
	return(prs);
}

//============================================================================
//    CLASS METHODS
//============================================================================

} // namespace FEAST
//****************************************************************************
//**
//**    END MODULE FEASTPRSMAIN.CPP
//**
//****************************************************************************

