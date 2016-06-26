/*
Copyright (c) 1999 CERN - European Organization for Nuclear Research.
Permission to use, copy, modify, distribute and sell this software and its documentation for any purpose 
is hereby granted without fee, provided that the above copyright notice appear in all copies and 
that both that copyright notice and this permission notice appear in supporting documentation. 
CERN makes no representations about the suitability of this software for any purpose. 
It is provided "as is" without expressed or implied warranty.
*/
package cern.colt.list;

/**
Abstract base class for resizable lists holding <code>float</code> elements; abstract.
First see the <a href="package-summary.html">package summary</a> and javadoc <a href="package-tree.html">tree view</a> to get the broad picture.
*/
public abstract class AbstractFloatList extends AbstractList {
	/**
	 * The size of the list.
	 * This is a READ_ONLY variable for all methods but setSizeRaw(int newSize) !!!
	 * If you violate this principle in subclasses, you should exactly know what you are doing.
	 * @serial
	 */
	protected int size;
/**
 * Makes this class non instantiable, but still let's others inherit from it.
 */
protected AbstractFloatList() {}
/**
 * Appends the specified element to the end of this list.
 *
 * @param element element to be appended to this list.
 */
public void add(float element) {
	beforeInsert(size,element);
}
/**
 * Appends the part of the specified list between <code>from</code> (inclusive) and <code>to</code> (inclusive) to the receiver.
 *
 * @param other the list to be added to the receiver.
 * @param from the index of the first element to be appended (inclusive).
 * @param to the index of the last element to be appended (inclusive).
 * @exception IndexOutOfBoundsException index is out of range (<tt>other.size()&gt;0 && (from&lt;0 || from&gt;to || to&gt;=other.size())</tt>).
 */
public void addAllOfFromTo(AbstractFloatList other, int from, int to) {
	beforeInsertAllOfFromTo(size,other,from,to);
}
/**
 * Inserts the specified element before the specified position into the receiver. 
 * Shifts the element currently at that position (if any) and
 * any subsequent elements to the right.
 *
 * @param index index before which the specified element is to be inserted (must be in [0,size]).
 * @param element element to be inserted.
 * @exception IndexOutOfBoundsException index is out of range (<tt>index &lt; 0 || index &gt; size()</tt>).
 */
public void beforeInsert(int index, float element) {
	beforeInsertDummies(index,1);
	set(index,element);
}
/**
 * Inserts the part of the specified list between <code>otherFrom</code> (inclusive) and <code>otherTo</code> (inclusive) before the specified position into the receiver. 
 * Shifts the element currently at that position (if any) and
 * any subsequent elements to the right.
 *
 * @param index index before which to insert first element from the specified list (must be in [0,size])..
 * @param other list of which a part is to be inserted into the receiver.
 * @param from the index of the first element to be inserted (inclusive).
 * @param to the index of the last element to be inserted (inclusive).
 * @exception IndexOutOfBoundsException index is out of range (<tt>other.size()&gt;0 && (from&lt;0 || from&gt;to || to&gt;=other.size())</tt>).
 * @exception IndexOutOfBoundsException index is out of range (<tt>index &lt; 0 || index &gt; size()</tt>).
 */
public void beforeInsertAllOfFromTo(int index, AbstractFloatList other, int from, int to) {
	int length=to-from+1;
	this.beforeInsertDummies(index, length);
	this.replaceFromToWithFrom(index, index+length-1, other, from);
}
/**
 * Inserts <tt>length</tt> dummy elements before the specified position into the receiver. 
 * Shifts the element currently at that position (if any) and
 * any subsequent elements to the right.
 * <b>This method must set the new size to be <tt>size()+length</tt>.
 *
 * @param index index before which to insert dummy elements (must be in [0,size])..
 * @param length number of dummy elements to be inserted.
 * @throws IndexOutOfBoundsException if <tt>index &lt; 0 || index &gt; size()</tt>.
 */
protected void beforeInsertDummies(int index, int length) {
	if (index > size || index < 0) 
		throw new IndexOutOfBoundsException("Index: "+index+", Size: "+size);
	if (length > 0) {
		ensureCapacity(size + length);
		setSizeRaw(size + length);
		replaceFromToWithFrom(index+length,size-1,this,index);
	}
}
/**
 * Returns a deep copy of the receiver. 
 *
 * @return  a deep copy of the receiver.
 */
public Object clone() {
	return partFromTo(0,size-1);
}
/**
 * Returns true if the receiver contains the specified element.
 *
 * @param element element whose presence in the receiver is to be tested.
 */
public boolean contains(float elem) {
	return indexOfFromTo(elem,0,size-1) >=0;
}
/**
 * Deletes the first element from the receiver that is identical to the specified element.
 * Does nothing, if no such matching element is contained.
 *
 * @param element the element to be deleted.
 */
public void delete(float element) {
	int index = indexOfFromTo(element, 0, size-1);
	if (index>=0) remove(index);
}
/**
 * Returns the elements currently stored, possibly including invalid elements between size and capacity.
 *
 * <b>WARNING:</b> For efficiency reasons and to keep memory usage low, this method may decide <b>not to copy the array</b>.
 * So if subsequently you modify the returned array directly via the [] operator, be sure you know what you're doing.
 *
 * @return the elements currently stored.
 */
public float[] elements() {
	float[] myElements = new float[size];
	for (int i=size; --i >= 0; ) myElements[i]=getQuick(i);
	return myElements;
}
/**
 * Sets the receiver's elements to be the specified array.
 * The size and capacity of the list is the length of the array.
 * <b>WARNING:</b> For efficiency reasons and to keep memory usage low, this method may decide <b>not to copy the array</b>.
 * So if subsequently you modify the returned array directly via the [] operator, be sure you know what you're doing.
 *
 * @param elements the new elements to be stored.
 * @return the receiver itself.
 */
public AbstractFloatList elements(float[] elements) {
	clear();
	addAllOfFromTo(new FloatArrayList(elements),0,elements.length-1);
	return this;
}
/**
 * Ensures that the receiver can hold at least the specified number of elements without needing to allocate new internal memory.
 * If necessary, allocates new internal memory and increases the capacity of the receiver.
 *
 * @param   minCapacity   the desired minimum capacity.
 */
public abstract void ensureCapacity(int minCapacity);
/**
 * Compares the specified Object with the receiver.  
 * Returns true if and only if the specified Object is also an ArrayList of the same type, both Lists have the
 * same size, and all corresponding pairs of elements in the two Lists are identical.
 * In other words, two Lists are defined to be equal if they contain the
 * same elements in the same order.
 *
 * @param otherObj the Object to be compared for equality with the receiver.
 * @return true if the specified Object is equal to the receiver.
 */
public boolean equals(Object otherObj) { //delta
	if (! (otherObj instanceof AbstractFloatList)) {return false;}
	if (this==otherObj) return true;
	if (otherObj==null) return false;
	AbstractFloatList other = (AbstractFloatList) otherObj;
	if (size()!=other.size()) return false;

	for (int i=size(); --i >= 0; ) {
	    if (getQuick(i) != other.getQuick(i)) return false;
	}
	return true;
}
/**
 * Sets the specified range of elements in the specified array to the specified value.
 *
 * @param from the index of the first element (inclusive) to be filled with the specified value.
 * @param to the index of the last element (inclusive) to be filled with the specified value.
 * @param val the value to be stored in the specified elements of the receiver.
 */
public void fillFromToWith(int from, int to, float val) {
	checkRangeFromTo(from,to,this.size);
	for (int i=from; i<=to;) setQuick(i++,val); 
}
/**
 * Returns the element at the specified position in the receiver.
 *
 * @param index index of element to return.
 * @exception IndexOutOfBoundsException index is out of range (index
 * 		  &lt; 0 || index &gt;= size()).
 */
public float get(int index) {
	if (index >= size || index < 0)
		throw new IndexOutOfBoundsException("Index: "+index+", Size: "+size);
	return getQuick(index);
}
/**
 * Returns the element at the specified position in the receiver; <b>WARNING:</b> Does not check preconditions. 
 * Provided with invalid parameters this method may return invalid elements without throwing any exception!
 * <b>You should only use this method when you are absolutely sure that the index is within bounds.</b>
 * Precondition (unchecked): <tt>index &gt;= 0 && index &lt; size()</tt>.
 *
 * This method is normally only used internally in large loops where bounds are explicitly checked before the loop and need no be rechecked within the loop.
 * However, when desperately, you can give this method <tt>public</tt> visibility in subclasses.
 *
 * @param index index of element to return.
 */
protected abstract float getQuick(int index);
/**
 * Returns the index of the first occurrence of the specified
 * element. Returns <code>-1</code> if the receiver does not contain this element.
 *
 * @param   element   the element to be searched for.
 * @return  the index of the first occurrence of the element in the receiver; returns <code>-1</code> if the element is not found.
 */
public int indexOf(float element) { //delta
	return indexOfFromTo(element, 0, size-1);
}
/**
 * Returns the index of the first occurrence of the specified
 * element. Returns <code>-1</code> if the receiver does not contain this element.
 * Searches between <code>from</code>, inclusive and <code>to</code>, inclusive.
 * Tests for identity.
 *
 * @param element element to search for.
 * @param from the leftmost search position, inclusive.
 * @param to the rightmost search position, inclusive.
 * @return  the index of the first occurrence of the element in the receiver; returns <code>-1</code> if the element is not found.
 * @exception IndexOutOfBoundsException index is out of range (<tt>size()&gt;0 && (from&lt;0 || from&gt;to || to&gt;=size())</tt>).
 */
public int indexOfFromTo(float element, int from, int to) {
	checkRangeFromTo(from, to, size);

	for (int i = from ; i <= to; i++) {
	    if (element==getQuick(i)) return i; //found
	}
	return -1; //not found
}
/**
 * Returns the index of the last occurrence of the specified
 * element. Returns <code>-1</code> if the receiver does not contain this element.
 *
 * @param   element   the element to be searched for.
 * @return  the index of the last occurrence of the element in the receiver; returns <code>-1</code> if the element is not found.
 */
public int lastIndexOf(float element) {
	return lastIndexOfFromTo(element, 0, size-1);
}
/**
 * Returns the index of the last occurrence of the specified
 * element. Returns <code>-1</code> if the receiver does not contain this element.
 * Searches beginning at <code>to</code>, inclusive until <code>from</code>, inclusive.
 * Tests for identity.
 *
 * @param element element to search for.
 * @param from the leftmost search position, inclusive.
 * @param to the rightmost search position, inclusive.
 * @return  the index of the last occurrence of the element in the receiver; returns <code>-1</code> if the element is not found.
 * @exception IndexOutOfBoundsException index is out of range (<tt>size()&gt;0 && (from&lt;0 || from&gt;to || to&gt;=size())</tt>).
 */
public int lastIndexOfFromTo(float element, int from, int to) {
	checkRangeFromTo(from, to, size());

	for (int i = to ; i >= from; i--) {
	    if (element==getQuick(i)) return i; //found
	}
	return -1; //not found
}
/**
 * Returns a new list of the part of the receiver between <code>from</code>, inclusive, and <code>to</code>, inclusive.
 * @param from the index of the first element (inclusive).
 * @param to the index of the last element (inclusive).
 * @return a new list
 * @exception IndexOutOfBoundsException index is out of range (<tt>size()&gt;0 && (from&lt;0 || from&gt;to || to&gt;=size())</tt>).
 */
public AbstractFloatList partFromTo(int from, int to) {
	checkRangeFromTo(from, to, size);

	int length = to-from+1;
	FloatArrayList part = new FloatArrayList(length);
	part.addAllOfFromTo(this,from,to);
	return part;
}
/**
* Removes from the receiver all elements whose index is between
* <code>from</code>, inclusive and <code>to</code>, inclusive.  Shifts any succeeding
* elements to the left (reduces their index).
* This call shortens the list by <tt>(to - from + 1)</tt> elements.
*
* @param from index of first element to be removed.
* @param to index of last element to be removed.
* @exception IndexOutOfBoundsException index is out of range (<tt>size()&gt;0 && (from&lt;0 || from&gt;to || to&gt;=size())</tt>).
*/
public void removeFromTo(int from, int to) {
    checkRangeFromTo(from, to, size);
    int numMoved = size - to - 1;
    if (numMoved > 0) {
        replaceFromToWithFrom(from, from-1+numMoved, this, to+1);
        //fillFromToWith(from+numMoved, size-1, 0.0f); //delta
    }
    int width = to-from+1;
    if (width>0) setSizeRaw(size-width);
}
/**
 * Replaces a number of elements in the receiver with the same number of elements of another list.
 * Replaces elements in the receiver, between <code>from</code> (inclusive) and <code>to</code> (inclusive),
 * with elements of <code>other</code>, starting from <code>otherFrom</code> (inclusive).
 *
 * @param from the position of the first element to be replaced in the receiver
 * @param to the position of the last element to be replaced in the receiver
 * @param other list holding elements to be copied into the receiver.
 * @param otherFrom position of first element within other list to be copied.
 */
public void replaceFromToWithFrom(int from, int to, AbstractFloatList other, int otherFrom) {
	int length=to-from+1;
	if (length>0) {
		checkRangeFromTo(from, to, size());
		checkRangeFromTo(otherFrom,otherFrom+length-1,other.size());

		// unambiguous copy (it may hold other==this)
		if (from<=otherFrom) {
			for (; --length >= 0; ) setQuick(from++,other.getQuick(otherFrom++));
		}
		else {
			int otherTo = otherFrom+length-1;
			for (; --length >= 0; ) setQuick(to--,other.getQuick(otherTo--));
		}
	}
}
/**
* Replaces the part between <code>from</code> (inclusive) and <code>to</code> (inclusive) with the other list's
* part between <code>otherFrom</code> and <code>otherTo</code>. 
* Powerful (and tricky) method!
* Both parts need not be of the same size (part A can both be smaller or larger than part B).
* Parts may overlap.
* Receiver and other list may (but most not) be identical.
* If <code>from &gt; to</code>, then inserts other part before <code>from</code>.
*
* @param from the first element of the receiver (inclusive)
* @param to the last element of the receiver (inclusive)
* @param other the other list (may be identical with receiver)
* @param otherFrom the first element of the other list (inclusive)
* @param otherTo the last element of the other list (inclusive)
*
* <p><b>Examples:</b><pre>
* a=[0, 1, 2, 3, 4, 5, 6, 7]
* b=[50, 60, 70, 80, 90]
* a.R(...)=a.replaceFromToWithFromTo(...)
*
* a.R(3,5,b,0,4)-->[0, 1, 2, 50, 60, 70, 80, 90, 6, 7]
* a.R(1,6,b,0,4)-->[0, 50, 60, 70, 80, 90, 7]
* a.R(0,6,b,0,4)-->[50, 60, 70, 80, 90, 7]
* a.R(3,5,b,1,2)-->[0, 1, 2, 60, 70, 6, 7]
* a.R(1,6,b,1,2)-->[0, 60, 70, 7]
* a.R(0,6,b,1,2)-->[60, 70, 7]
* a.R(5,3,b,0,4)-->[0, 1, 2, 3, 4, 50, 60, 70, 80, 90, 5, 6, 7]
* a.R(5,0,b,0,4)-->[0, 1, 2, 3, 4, 50, 60, 70, 80, 90, 5, 6, 7]
* a.R(5,3,b,1,2)-->[0, 1, 2, 3, 4, 60, 70, 5, 6, 7]
* a.R(5,0,b,1,2)-->[0, 1, 2, 3, 4, 60, 70, 5, 6, 7]
*
* Extreme cases:
* a.R(5,3,b,0,0)-->[0, 1, 2, 3, 4, 50, 5, 6, 7]
* a.R(5,3,b,4,4)-->[0, 1, 2, 3, 4, 90, 5, 6, 7]
* a.R(3,5,a,0,1)-->[0, 1, 2, 0, 1, 6, 7]
* a.R(3,5,a,3,5)-->[0, 1, 2, 3, 4, 5, 6, 7]
* a.R(3,5,a,4,4)-->[0, 1, 2, 4, 6, 7]
* a.R(5,3,a,0,4)-->[0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 5, 6, 7]
* a.R(0,-1,b,0,4)-->[50, 60, 70, 80, 90, 0, 1, 2, 3, 4, 5, 6, 7]
* a.R(0,-1,a,0,4)-->[0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 5, 6, 7]
* a.R(8,0,a,0,4)-->[0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4]
* </pre>
*/
public void replaceFromToWithFromTo(int from, int to, AbstractFloatList other, int otherFrom, int otherTo) {
	if (otherFrom>otherTo) {
		throw new IndexOutOfBoundsException("otherFrom: "+otherFrom+", otherTo: "+otherTo);
	}

	if (this==other && to-from!=otherTo-otherFrom) { // avoid stumbling over my own feet
		replaceFromToWithFromTo(from, to, partFromTo(otherFrom, otherTo), 0, otherTo-otherFrom);
		return;
	}
	
	int length=otherTo-otherFrom+1;
	int diff=length;
	int theLast=from-1;

	if (to>=from) {
		diff -= (to-from+1);
		theLast=to;
	}
	
	if (diff>0) {
		beforeInsertDummies(theLast+1, diff);
	}
	else {
		if (diff<0) {
			removeFromTo(theLast+diff, theLast-1);
		}
	}

	if (length>0) {
		replaceFromToWithFrom(from,from+length-1,other,otherFrom);
	}
}
/**
 * Replaces the part of the receiver starting at <code>from</code> (inclusive) with all the elements of the specified collection.
 * Does not alter the size of the receiver.
 * Replaces exactly <tt>Math.max(0,Math.min(size()-from, other.size()))</tt> elements.
 *
 * @param from the index at which to copy the first element from the specified collection.
 * @param other Collection to replace part of the receiver
 * @exception IndexOutOfBoundsException index is out of range (index &lt; 0 || index &gt;= size()).
 */
public void replaceFromWith(int from, java.util.Collection other) {
	checkRange(from,size());
	java.util.Iterator e = other.iterator();
	int index=from;
	int limit = Math.min(size()-from, other.size());
	for (int i=0; i<limit; i++)
	    set(index++,((Number) e.next()).floatValue()); //delta
}
/**
 * Reverses the elements of the receiver.
 * Last becomes first, second last becomes second first, and so on.
 */
public void reverse() {
	float tmp;
	int limit=size()/2;
	int j=size()-1;

	for (int i=0; i<limit;) { //swap
		tmp=getQuick(i);
		setQuick(i++,getQuick(j));
		setQuick(j--,tmp);
	}
}
/**
 * Replaces the element at the specified position in the receiver with the specified element.
 *
 * @param index index of element to replace.
 * @param element element to be stored at the specified position.
 * @throws IndexOutOfBoundsException if <tt>index &lt; 0 || index &gt;= size()</tt>.
 */
public void set(int index, float element) {
	if (index >= size || index < 0)
		throw new IndexOutOfBoundsException("Index: "+index+", Size: "+size);
	setQuick(index,element);
}
/**
 * Replaces the element at the specified position in the receiver with the specified element; <b>WARNING:</b> Does not check preconditions.
 * Provided with invalid parameters this method may access invalid indexes without throwing any exception!
 * <b>You should only use this method when you are absolutely sure that the index is within bounds.</b>
 * Precondition (unchecked): <tt>index &gt;= 0 && index &lt; size()</tt>.
 *
 * This method is normally only used internally in large loops where bounds are explicitly checked before the loop and need no be rechecked within the loop.
 * However, when desperately, you can give this method <tt>public</tt> visibility in subclasses.
 *
 * @param index index of element to replace.
 * @param element element to be stored at the specified position.
 */
protected abstract void setQuick(int index, float element);
/**
 * Sets the size of the receiver without modifying it otherwise.
 * This method should not release or allocate new memory but simply set some instance variable like <tt>size</tt>.
 *
 * If your subclass overrides and delegates size changing methods to some other object,
 * you must make sure that those overriding methods not only update the size of the delegate but also of this class.
 * For example:
 * public DatabaseList extends AbstractFloatList {
 *    ...
 *    public void removeFromTo(int from,int to) {
 *       myDatabase.removeFromTo(from,to);
 *       this.setSizeRaw(size-(to-from+1));
 *    }
 * }
 */
protected void setSizeRaw(int newSize) {
	size = newSize;
}
/**
 * Returns the number of elements contained in the receiver.
 *
 * @returns  the number of elements contained in the receiver.
 */
public int size() {
	return size;
}
/**
 * Returns a list which is a concatenation of <code>times</code> times the receiver.
 * @param times the number of times the receiver shall be copied.
 */
public AbstractFloatList times(int times) {
	AbstractFloatList newList = new FloatArrayList(times*size());
	for (int i=times; --i >= 0; ) {
		newList.addAllOfFromTo(this,0,size()-1);
	}
	return newList;
}
/**
* Returns a string representation of the receiver, containing
* the String representation of each element.
*/
public String toString() {
	return cern.colt.Arrays.toString(partFromTo(0, size()-1).elements());
}
}
