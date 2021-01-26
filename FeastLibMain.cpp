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
//**    FEASTLIBMAIN.CPP
//**
//****************************************************************************
#include "FeastInternal.h"

#define VERSION 1.06f

namespace FEAST {

class CLibClient
: public ILibClient
{
public:
	void* LibMalloc(unsigned long inSize)
	{
		void* ptr = malloc(inSize);
		if (!ptr)
			LibError("Out of memory");
		return(ptr);
	}
	void* LibRealloc(void* inPtr, unsigned long inNewSize)
	{
		void* ptr = realloc(inPtr, inNewSize);
		if (!ptr)
			LibError("Out of memory");
		return(ptr);
	}
	void LibFree(void* inPtr)
	{
		if (inPtr)
			free(inPtr);
	}
	void LibError(const char* inErrorStr)
	{
		printf("FEAST Error: %s", inErrorStr);
		exit(1);
	}
};

ILibClient* LIB_GetDefaultClient()
{
	static CLibClient sClient;
	return(&sClient);
}

ILibClient*& LIB_GetClientRef()
{
	static ILibClient* spClient = LIB_GetDefaultClient();
	return(spClient);
}

char* LIB_Va(const char*& inFmt, char* inBuffer=NULL, ...)
{
	static char buf[4096];
	if (!inFmt)
		return(NULL);
	if (!inBuffer)
		inBuffer = buf;
	va_list args;
	va_start(args, inBuffer);
	vsprintf(inBuffer, inFmt, args);
	va_end(args);
	return(inBuffer);
}

void* LIB_ClientMalloc(NDword inSize)
{
	// allocate a block of memory, with padding to store a pointer to the client that allocated it
	void* ptr = LIB_GetClientRef()->LibMalloc(inSize + sizeof(ILibClient*));
	*((ILibClient**)ptr) = LIB_GetClientRef();
	return((NByte*)ptr + sizeof(ILibClient*));
}
void* LIB_ClientRealloc(void* inPtr, NDword inSize)
{
	// realloc a block of memory using the client that allocated it
	NByte* basePtr = (NByte*)inPtr - sizeof(ILibClient*);
	ILibClient* client = *((ILibClient**)basePtr);
	void* ptr = client->LibRealloc(basePtr, inSize + sizeof(ILibClient*));
	*((ILibClient**)ptr) = client;
	return((NByte*)ptr + sizeof(ILibClient*));
}
void LIB_ClientFree(void* inPtr)
{
	// free a block of memory using the client that allocated it
	if (!inPtr)
		return;
	NByte* basePtr = (NByte*)inPtr - sizeof(ILibClient*);
	ILibClient* client = *((ILibClient**)basePtr);
	client->LibFree(basePtr);
}

void LIB_Errorf(const char* inFmt, ... )
{
	LIB_GetClientRef()->LibError(LIB_Va(inFmt));
}

FEAST_API ILibClient* LIB_SetClient(ILibClient* inClient)
{
	ILibClient* oldClient = LIB_GetClientRef();
	if (!inClient)
		inClient = LIB_GetDefaultClient(); // use default if null is passed in
	LIB_GetClientRef() = inClient;
	return(oldClient);
}
FEAST_API ILibClient* LIB_GetClient()
{
	return(LIB_GetClientRef());
}

FEAST_API float LIB_GetVersion()
{
	return(VERSION);
}

} // namespace FEAST

//****************************************************************************
//**
//**    END MODULE FEASTLIBMAIN.CPP
//**
//****************************************************************************

