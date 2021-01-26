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
#pragma once
#ifndef __FEASTINTERNAL_H__
#define __FEASTINTERNAL_H__
//****************************************************************************
//**
//**    FEASTINTERNAL.H
//**
//****************************************************************************
#include "Feast.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <stdarg.h>

inline void* operator new(size_t size, void* ptr) { return(ptr); } //!< Placement operator new
inline void operator delete(void*, void*) {} //!< Placement operator delete (MSVC++ only)

namespace FEAST {

typedef unsigned char NByte;
typedef unsigned short NWord;
typedef unsigned long NDword;
typedef signed long NSDword;
typedef float NFloat;
typedef char NChar;
typedef bool NBool;

void* LIB_ClientMalloc(NDword inSize);
void* LIB_ClientRealloc(void* inPtr, NDword inSize);
void LIB_ClientFree(void* inPtr);
void LIB_Errorf(const char* inFmt, ... );

#define LIB_Malloc(xType, xCount) ((xType*)LIB_ClientMalloc((xCount)*sizeof(xType)))
#define LIB_Realloc(xType, xPtr, xCount) ((xType*)LIB_ClientRealloc(xPtr, (xCount)*sizeof(xType)))
#define LIB_Free(xPtr) LIB_ClientFree(xPtr)


class CDatArray
{
protected:
	void* mData; //!< Data pointer
	NDword mCount; //!< Number of data bytes currently used
	NDword mLimit; //!< Number of data bytes available (size of mData buffer)
	NDword mElemSize; //!< Size of individual element in array

	inline void Realloc(NDword inElemSize)
	{
		mElemSize = inElemSize;
		if (mData)
		{
			if (!mElemSize || !mLimit)
			{
				LIB_Free(mData);
				mData = NULL;
			}
			else
			{
				mData = LIB_Realloc(NByte, mData, mLimit*mElemSize);
			}
		}
		else
		{
			if (!mElemSize || !mLimit)
			{
				mData = NULL;
			}
			else
			{
				mData = LIB_Malloc(NByte, mLimit*mElemSize);
			}
		}
	}
	inline CDatArray(NDword inCount, NDword inElemSize)
		: mCount(inCount), mLimit(inCount), mData(NULL), mElemSize(inElemSize)
	{
		Realloc(inElemSize);
	}
	inline ~CDatArray()
	{
		if (mData)
			LIB_Free(mData);
	}

public:
	//! Get pointer to array data
	/*! \return Pointer to data */
	inline void* GetData() const
	{
		return(mData);
	}
	//! Get size of array data in bytes
	/*! \return Size of array */
	inline NDword GetDataSize() const
	{
		return(mCount*mElemSize);
	}
	//! Change the size of array data in bytes
	/*! \param inSize Desired size of array
		\return True if size was successfully changed, false if not */
	inline NBool SetDataSize(NDword inSize)
	{
		if (inSize % mElemSize)
			return(0); // incompatible element size
		if (mData)
		{
			LIB_Free(mData);
			mData = NULL;
		}
		mCount = mLimit = inSize / mElemSize;
		Realloc(mElemSize);
		return(1);
	}
	//! Remove elements from array data
	/*! \param inIndex Starting index
		\param inCount Element count to remove
		\param inElemSize Size of individual element */
	inline void Remove(NDword inIndex, NDword inCount, NDword inElemSize)
	{
		if (!inCount)
			return;
		memmove((NByte*)mData+inIndex*inElemSize, (NByte*)mData+(inIndex+inCount)*inElemSize, (mCount-inIndex-inCount)*inElemSize);
		mCount -= inCount;
	}
};

//! TDatArray - General typed dynamic array
template <class T> class TDatArray
: public CDatArray
{
public:
	//! Construct array with an initial optional count
	/*! \param inCount Optional initial element count, defaults to zero */
	inline TDatArray(NDword inCount=0)
		: CDatArray(inCount, sizeof(T))
	{
		for (NDword i=0;i<inCount;i++)
			new(&(*this)[i]) T;
	}
	//! Copy-construct array from another array of same type
	/*! \param inArray Array to copy from */
	inline TDatArray(const TDatArray<T>& inArray)
		: CDatArray(inArray.mCount, sizeof(T))
	{
		mCount = 0;
		for (NDword i=0;i<inArray.mCount;i++)
			new(&(*this)[AddNoConstruct()]) T(inArray[i]);
	}
	//! Copy an array from another array of the same type via assignment
	/*! \param inArray Array to copy from */
	inline TDatArray& operator = (const TDatArray<T>& inArray)
	{
		if (this == &inArray)
			return(*this);
		mCount = 0;
		mLimit = inArray.mCount;
		Realloc(sizeof(T));
		for (NDword i=0;i<inArray.mCount;i++)
			new(&(*this)[AddNoConstruct()]) T(inArray[i]);
		return(*this);
	}
	//! Destructor, removes contents of array
	inline ~TDatArray()
	{
		RemoveAll();
	}
	//! Retrieve a reference to element for a given index
	/*! Note that this is not bounds checked!
		\param i Element index
		\return Reference to element */
	inline T& operator [] (int i)
	{
		return(((T*)mData)[i]);
	}
	//! Retrieve a const reference to element for a given index
	/*! Note that this is not bounds checked!
		\param i Element index
		\return Const reference to element */
	inline const T& operator [] (int i) const
	{
		return(((T*)mData)[i]);
	}
	//! Get count of elements currently in array
	/*! \return Element count */
	inline NDword GetCount() const
	{
		return(mCount);
	}
	//! Return whether a given element index is within array bound range
	/*! \param i Element index
		\return True if element is in range, false if not */
	inline NBool IsInRange(int i) const
	{
		return((i>=0) && (i<mCount));
	}
	//! Remove all elements from array, reduces current element count (but not necessarily the memory being used)
	inline void Purge()
	{
		Remove(0, mCount);
	}
	//! Shrink memory usage to make the memory limit equal to the current count
	inline void Shrink()
	{
		if (mLimit == mCount)
			return;
		mLimit = mCount;
		Realloc(sizeof(T));
	}
	//! Remove all elements completely, equivalent to Purge() followed by Shrink()
	inline void RemoveAll()
	{
		Purge();
		Shrink();
	}
	//! Add an element to the array, copied from a given item
	/*! \param inItem Item to copy from
		\return Index of added element in array */
	inline NDword AddItem(const T& inItem)
	{
		NDword i = Add();
		(*this)[i] = inItem;
		return(i);
	}
	//! Insert an element in the array, copied from a given item
	/*! \param inItem Item to copy from
		\param inIndex Optional index, defaults to zero
		\return Index of inserted element in array */
	inline NDword InsertItem(const T& inItem, NDword inIndex=0)
	{
		NDword i = Insert(1, inIndex);
		(*this)[i] = inItem;
		return(i);
	}
	//! Add a count of elements to the array in raw form, doesn't call element constructor(s)
	/*! \param inCount Optional element count, defaults to one
		\return Index of first added element in array */
	inline NDword AddNoConstruct(NDword inCount=1)
	{
		NDword i = mCount;
		if ((mCount+=inCount)>mLimit)
		{
			mLimit = mCount + (mCount>>2) + 32;
			Realloc(sizeof(T));
		}
		return(i);
	}
	//! Insert a count of elements in the array in raw form starting at a given index, doesn't call element constructor(s)
	/*! \param inCount Optional element count, defaults to one
		\param inIndex Optional starting index, defaults to zero
		\return Index of first inserted element in array */
	inline NDword InsertNoConstruct(NDword inCount=1, NDword inIndex=0)
	{
		AddNoConstruct(inCount);
		memmove(&(*this)[inIndex+inCount], &(*this)[inIndex], (mCount-(inCount+inIndex))*sizeof(T));
		return(inIndex);
	}
	//! Add a count of elements to the array, calls element constructor(s)
	/*! \param inCount Optional element count, defaults to one
		\return Index of first added element in array */
	inline NDword Add(NDword inCount=1)
	{
		NDword index = AddNoConstruct(inCount);
		for (NDword i=index; i<index+inCount; i++)
			new(&(*this)[i]) T;
		return(index);
	}
	//! Insert a count of elements in the array starting at a given index, calls element constructor(s)
	/*! \param inCount Optional element count, defaults to one
		\param inIndex Optional starting index, defaults to zero
		\return Index of first inserted element in array */
	inline NDword Insert(NDword inCount=1, NDword inIndex=0)
	{
		Add(inCount);
		memmove(&(*this)[inIndex+inCount], &(*this)[inIndex], (mCount-(inCount+inIndex))*sizeof(T));
		return(inIndex);
	}
	//! Add a count of elements to the array in raw form, zeros out the contents of the element(s)
	/*! \param inCount Optional element count, defaults to one
		\return Index of first added element in array */
	inline NDword AddZeroed(NDword inCount=1)
	{
		NDword i = AddNoConstruct(inCount);
		memset(&(*this)[i], 0, inCount*sizeof(T));
		return(i);
	}
	//! Insert a count of elements in the array in raw form starting at a given index, zeros out the contents of the element(s)
	/*! \param inCount Optional element count, defaults to one
		\param inIndex Optional starting index, defaults to zero
		\return Index of first inserted element in array */
	inline NDword InsertZeroed(NDword inCount=1, NDword inIndex=0)
	{
		AddNoConstruct(inCount);
		memmove(&(*this)[inIndex+inCount], &(*this)[inIndex], (mCount-(inCount+inIndex))*sizeof(T));
		memset(&(*this)[inIndex], 0, inCount*sizeof(T));
		return(inIndex);
	}
	//! Find an element in the array with value matching the given item
	/*! \param inItem Item with value to find
		\retval outIndex Matching value index, if result is true
		\return True if a match was found, false if not */
	inline NBool FindItem(const T& inItem, NDword* outIndex) const
	{
		for (NDword i=0;i<mCount;i++)
		{
			if (((T*)mData)[i] == inItem)
			{
				if (outIndex)
					*outIndex = i;
				return(1);
			}
		}
		return(0);
	}
	//! Add a unique item to the array; does not add if item currently exists using FindItem()
	/*! \param inItem Item to copy from, if not currently present in array
		\return Index of element in array, existing or newly added */
	inline NDword AddUnique(const T& inItem)
	{
		NDword i;
		if (FindItem(inItem, &i))
			return(i);
		return(AddItem(inItem));
	}
	//! Remove any items from array matching the given item
	/*! \param inItem Item to remove when found
		\return Count of items removed */
	inline NDword RemoveItem(const T& inItem)
	{
		NDword oldCount = mCount;
		for (NDword i=0;i<mCount;i++)
		{
			if (((T*)mData)[i] == inItem)
				Remove(i--);
		}
		return(oldCount - mCount);
	}
	//! Remove a given count of items from the array starting at a given index
	/*! \param inIndex Starting index
		\param inCount Optional element count, defaults to one */
	inline void Remove(NDword inIndex, NDword inCount=1)
	{
		for (NDword i=inIndex; i<(inIndex+inCount); i++)
			(&(*this)[i])->~T();
		CDatArray::Remove(inIndex, inCount, sizeof(T));
	}
	//! Change the raw element count of array in bytes; does not call any constructors/destructors
	inline void SetCount(NDword inNewCount)
	{
		if((mCount==inNewCount) && (mLimit==inNewCount))
			return;
		mCount = mLimit = inNewCount;
		Realloc(sizeof(T));
	}
};

} // namespace FEAST

//****************************************************************************
//**
//**    END HEADER FEASTINTERNAL.H
//**
//****************************************************************************
#endif // __FEASTINTERNAL_H__
