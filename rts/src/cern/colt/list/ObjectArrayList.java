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
Resizable list holding <code>Object</code> elements; implemented with arrays.
First see the <a href="package-summary.html">package summary</a> and javadoc <a href="package-tree.html">tree view</a> to get the broad picture.
*/
public class ObjectArrayList extends AbstractList {
	/**
	 * The array buffer into which the elements of the list are stored.
	 * The capacity of the list is the length of this array buffer.
	 * @serial
	 */
	protected Object[] elements;
	
	/**
	 * The size of the list.
	 * @serial
	 */
	protected int size;
/**
 * Constructs an empty list.
 */
public ObjectArrayList() {
	this(10);
}
/**
 * Constructs a list containing the specified elements. 
 * The initial size and capacity of the list is the length of the array.
 *
 * <b>WARNING:</b> For efficiency reasons and to keep memory usage low, <b>the array is not copied</b>.
 * So if subsequently you modify the specified array directly via the [] operator, be sure you know what you're doing.
 * 
 * @param elements the array to be backed by the the constructed list
 */
public ObjectArrayList(Object[] elements) {
	elements(elements);
}
/**
 * Constructs an empty list with the specified initial capacity.
 *
 * @param   initialCapacity   the number of elements the receiver can hold without auto-expanding itself by allocating new internal memory.
 */
public ObjectArrayList(int initialCapacity) {
	this(new Object[initialCapacity]);
	size=0;
}
/**
 * Appends the specified element to the end of this list.
 *
 * @param element element to be appended to this list.
 */
public void add(Object element) {
	if (size == elements.length) ensureCapacity(size + 1);
	elements[size++] = element;
}
/**
 * Appends the part of the specified list between <code>from</code> (inclusive) and <code>to</code> (inclusive) to the receiver.
 *
 * @param other the list to be added to the receiver.
 * @param from the index of the first element to be appended (inclusive).
 * @param to the index of the last element to be appended (inclusive).
 * @exception IndexOutOfBoundsException index is out of range (<tt>other.size()&gt;0 && (from&lt;0 || from&gt;to || to&gt;=other.size())</tt>).
 */
public void addAllOfFromTo(ObjectArrayList other, int from, int to) {
	beforeInsertAllOfFromTo(size, other, from, to);
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
public void beforeInsert(int index, Object element) {
	// overridden for performance only.
	if (index > size || index < 0) 
		throw new IndexOutOfBoundsException("Index: "+index+", Size: "+size);
	ensureCapacity(size + 1);
	System.arraycopy(elements, index, elements, index+1, size-index);
	elements[index] = element;
	size++;
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
public void beforeInsertAllOfFromTo(int index, ObjectArrayList other, int from, int to) {
	int length=to-from+1;
	this.beforeInsertDummies(index, length);
	this.replaceFromToWithFrom(index, index+length-1, other, from);
}
/**
 * Inserts length dummies before the specified position into the receiver. 
 * Shifts the element currently at that position (if any) and
 * any subsequent elements to the right.
 *
 * @param index index before which to insert dummies (must be in [0,size])..
 * @param length number of dummies to be inserted.
 */
protected void beforeInsertDummies(int index, int length) {
	if (index > size || index < 0) 
		throw new IndexOutOfBoundsException("Index: "+index+", Size: "+size);
	if (length > 0) {
		ensureCapacity(size + length);
	    System.arraycopy(elements, index, elements, index + length, size-index);
		size += length;
	}
}
/**
 * Returns a copy of the receiver such that the copy and the receiver <i>share</i> the same elements, but do not share the same array to index them;
 * So modifying an object in the copy modifies the object in the receiver and vice versa;
 * However, structurally modifying the copy (for example changing its size, setting other objects at indexes, etc.) does not affect the receiver and vice versa.
 *
 * @return  a copy of the receiver.
 */
public Object clone() {
	ObjectArrayList v = (ObjectArrayList)super.clone();
	v.elements = (Object[]) elements.clone();
	return v;
}
/**
 * Returns true if the receiver contains the specified element.
 * Tests for equality or identity as specified by testForEquality.
 *
 * @param element element to search for.
 * @param testForEquality if true -> test for equality, otherwise for identity.
 */
public boolean contains(Object elem, boolean testForEquality) {
	return indexOfFromTo(elem,0,size-1, testForEquality) >=0;
}
/**
 * Returns a copy of the receiver; call <code>clone()</code> and casts the result.
 * Returns a copy such that the copy and the receiver <i>share</i> the same elements, but do not share the same array to index them;
 * So modifying an object in the copy modifies the object in the receiver and vice versa;
 * However, structurally modifying the copy (for example changing its size, setting other objects at indexes, etc.) does not affect the receiver and vice versa.
 *
 * @return  a copy of the receiver.
 */
public ObjectArrayList copy() {
	return (ObjectArrayList) clone();
}
/**
 * Deletes the first element from the receiver that matches the specified element.
 * Does nothing, if no such matching element is contained.
 *
 * Tests elements for equality or identity as specified by <tt>testForEquality</tt>.
 * When testing for equality, two elements <tt>e1</tt> and
 * <tt>e2</tt> are <i>equal</i> if <tt>(e1==null ? e2==null :
 * e1.equals(e2))</tt>.)  
 *
 * @param testForEquality if true -> tests for equality, otherwise for identity.
 * @param element the element to be deleted.
 */
public void delete(Object element, boolean testForEquality) {
	int index = indexOfFromTo(element, 0, size-1, testForEquality);
	if (index>=0) removeFromTo(index,index);
}
/**
 * Returns the elements currently stored, including invalid elements between size and capacity, if any.
 *
 * <b>WARNING:</b> For efficiency reasons and to keep memory usage low, <b>the array is not copied</b>.
 * So if subsequently you modify the returned array directly via the [] operator, be sure you know what you're doing.
 *
 * @return the elements currently stored.
 */
public Object[] elements() {
	return elements;
}
/**
 * Sets the receiver's elements to be the specified array (not a copy of it).
 *
 * The size and capacity of the list is the length of the array.
 * <b>WARNING:</b> For efficiency reasons and to keep memory usage low, <b>the array is not copied</b>.
 * So if subsequently you modify the specified array directly via the [] operator, be sure you know what you're doing.
 *
 * @param elements the new elements to be stored.
 * @return the receiver itself.
 */
public ObjectArrayList elements(Object[] elements) {
	this.elements=elements;
	this.size=elements.length;
	return this;
}
/**
 * Ensures that the receiver can hold at least the specified number of elements without needing to allocate new internal memory.
 * If necessary, allocates new internal memory and increases the capacity of the receiver.
 *
 * @param   minCapacity   the desired minimum capacity.
 */
public void ensureCapacity(int minCapacity) {
	elements = cern.colt.Arrays.ensureCapacity(elements,minCapacity);
}
/**
* Compares the specified Object with the receiver for equality.
* Returns true if and only if the specified Object is also an ObjectArrayList, both lists have the
* same size, and all corresponding pairs of elements in the two lists are equal.
* In other words, two lists are defined to be equal if they contain the
* same elements in the same order.
* Two elements <tt>e1</tt> and
* <tt>e2</tt> are <i>equal</i> if <tt>(e1==null ? e2==null :
* e1.equals(e2))</tt>.)  
*
* @param otherObj the Object to be compared for equality with the receiver.
* @return true if the specified Object is equal to the receiver.
*/
public boolean equals(Object otherObj) { //delta
	return equals(otherObj, true);
}
/**
* Compares the specified Object with the receiver for equality.
* Returns true if and only if the specified Object is also an ObjectArrayList, both lists have the
* same size, and all corresponding pairs of elements in the two lists are the same.
* In other words, two lists are defined to be equal if they contain the
* same elements in the same order.
* Tests elements for equality or identity as specified by <tt>testForEquality</tt>.
* When testing for equality, two elements <tt>e1</tt> and
* <tt>e2</tt> are <i>equal</i> if <tt>(e1==null ? e2==null :
* e1.equals(e2))</tt>.)  
*
* @param otherObj the Object to be compared for equality with the receiver.
* @param testForEquality if true -> tests for equality, otherwise for identity.
* @return true if the specified Object is equal to the receiver.
*/
public boolean equals(Object otherObj, boolean testForEquality) { //delta
	if (! (otherObj instanceof ObjectArrayList)) {return false;}
	if (this==otherObj) return true;
	if (otherObj==null) return false;
	ObjectArrayList other = (ObjectArrayList) otherObj;
	if (elements==other.elements()) return true;
	if (size!=other.size()) return false;

	Object[] otherElements = other.elements();
	Object[] theElements = elements;
	if (! testForEquality) {
		for (int i=size; --i >= 0; ) {
			if (theElements[i] != otherElements[i]) return false;
		}
	}
	else {
		for (int i=size; --i >= 0; ) {
			if (!(theElements[i]==null ? otherElements[i]==null : theElements[i].equals(otherElements[i]))) return false;
		}
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
public void fillFromToWith(int from, int to, Object val) {
	checkRangeFromTo(from,to,this.size);
	for (int i=from; i<=to;) setQuick(i++,val); 
}
/**
 * Returns the element at the specified position in the receiver.
 *
 * @param index index of element to return.
 * @exception IndexOutOfBoundsException index is out of range (index &lt; 0 || index &gt;= size()).
 */
public Object get(int index) {
	if (index >= size || index < 0)
		throw new IndexOutOfBoundsException("Index: "+index+", Size: "+size);
	return elements[index];
}
/**
 * Returns the element at the specified position in the receiver; <b>WARNING:</b> Does not check preconditions. 
 * Provided with invalid parameters this method may return invalid elements without throwing any exception!
 * <b>You should only use this method when you are absolutely sure that the index is within bounds.</b>
 * Precondition (unchecked): <tt>index &gt;= 0 && index &lt; size()</tt>.
 *
 * @param index index of element to return.
 */
public Object getQuick(int index) {
	return elements[index];
}
/**
 * Returns the index of the first occurrence of the specified
 * element. Returns <code>-1</code> if the receiver does not contain this element.
 *
 * Tests for equality or identity as specified by testForEquality.
 *
 * @param testForEquality if <code>true</code> -> test for equality, otherwise for identity.
 * @return  the index of the first occurrence of the element in the receiver; returns <code>-1</code> if the element is not found.
 */
public int indexOf(Object element, boolean testForEquality) {
	return this.indexOfFromTo(element, 0, size-1, testForEquality);
}
/**
 * Returns the index of the first occurrence of the specified
 * element. Returns <code>-1</code> if the receiver does not contain this element.
 * Searches between <code>from</code>, inclusive and <code>to</code>, inclusive.
 *
 * Tests for equality or identity as specified by <code>testForEquality</code>.
 *
 * @param element element to search for.
 * @param from the leftmost search position, inclusive.
 * @param to the rightmost search position, inclusive.
 * @param testForEquality if </code>true</code> -> test for equality, otherwise for identity.
 * @return  the index of the first occurrence of the element in the receiver; returns <code>-1</code> if the element is not found.
 * @exception IndexOutOfBoundsException index is out of range (<tt>size()&gt;0 && (from&lt;0 || from&gt;to || to&gt;=size())</tt>).
 */
public int indexOfFromTo(Object element, int from, int to, boolean testForEquality) {
	if (size==0) return -1;
	checkRangeFromTo(from, to, size);

	Object[] theElements = elements;
	if (testForEquality && element!=null) {
		for (int i = from ; i <= to; i++) {
		    if (element.equals(theElements[i])) {return i;} //found
		}

	}
	else {
		for (int i = from ; i <= to; i++) {
		    if (element==theElements[i]) {return i;} //found
		}
	}
	return -1; //not found
}
/**
 * Returns the index of the last occurrence of the specified
 * element. Returns <code>-1</code> if the receiver does not contain this element.
 * Tests for equality or identity as specified by <code>testForEquality</code>.
 *
 * @param   element   the element to be searched for.
 * @param testForEquality if <code>true</code> -> test for equality, otherwise for identity.
 * @return  the index of the last occurrence of the element in the receiver; returns <code>-1</code> if the element is not found.
 */
public int lastIndexOf(Object element, boolean testForEquality) {
	return lastIndexOfFromTo(element, 0, size-1, testForEquality);
}
/**
 * Returns the index of the last occurrence of the specified
 * element. Returns <code>-1</code> if the receiver does not contain this element.
 * Searches beginning at <code>to</code>, inclusive until <code>from</code>, inclusive.
 * Tests for equality or identity as specified by <code>testForEquality</code>.
 *
 * @param element element to search for.
 * @param from the leftmost search position, inclusive.
 * @param to the rightmost search position, inclusive.
 * @param testForEquality if <code>true</code> -> test for equality, otherwise for identity.
 * @return  the index of the last occurrence of the element in the receiver; returns <code>-1</code> if the element is not found.
 * @exception IndexOutOfBoundsException index is out of range (<tt>size()&gt;0 && (from&lt;0 || from&gt;to || to&gt;=size())</tt>).
 */
public int lastIndexOfFromTo(Object element, int from, int to, boolean testForEquality) {
	if (size==0) return -1;
	checkRangeFromTo(from, to, size);

	Object[] theElements = elements;
	if (testForEquality && element!=null) {
		for (int i = to ; i >= from; i--) {
		    if (element.equals(theElements[i])) {return i;} //found
		}

	}
	else {
		for (int i = to ; i >= from; i--) {
		    if (element==theElements[i]) {return i;} //found
		}
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
public ObjectArrayList partFromTo(int from, int to) {
	if (size==0) return new ObjectArrayList(0);

	checkRangeFromTo(from, to, size);

	Object[] part = new Object[to-from+1];
	System.arraycopy(elements, from, part, 0, to-from+1);
	return new ObjectArrayList(part);
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
	if (numMoved >= 0) {
		System.arraycopy(elements, to+1, elements, from, numMoved);
		fillFromToWith(from+numMoved, size-1, null); //delta
	}
	int width = to-from+1;
	if (width>0) size -= width;
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
public void replaceFromToWithFrom(int from, int to, ObjectArrayList other, int otherFrom) {
	int length=to-from+1;
	if (length>0) {
		checkRangeFromTo(from, to, size);
		checkRangeFromTo(otherFrom,otherFrom+length-1,other.size);
		System.arraycopy(other.elements, otherFrom, elements, from, length);
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
public void replaceFromToWithFromTo(int from, int to, ObjectArrayList other, int otherFrom, int otherTo) {
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

	//System.out.println("from="+from);
	//System.out.println("to="+to);
	//System.out.println("diff="+diff);
	
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
		System.arraycopy(other.elements, otherFrom, elements, from, length);
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
	checkRange(from,size);
	java.util.Iterator e = other.iterator();
	int index=from;
	int limit = Math.min(size-from, other.size());
	for (int i=0; i<limit; i++)
	    elements[index++] = e.next(); //delta
}
/**
 * Reverses the elements of the receiver.
 * Last becomes first, second last becomes second first, and so on.
 */
public void reverse() {
	Object tmp;
	int limit=size/2;
	int j=size-1;
	
	Object[] theElements = elements;
	for (int i=0; i<limit;) { //swap
		tmp=theElements[i];
		theElements[i++]=theElements[j];
		theElements[j--]=tmp;
	}
}
/**
 * Replaces the element at the specified position in the receiver with the specified element.
 *
 * @param index index of element to replace.
 * @param element element to be stored at the specified position.
 * @exception IndexOutOfBoundsException index is out of range (index
 * 		  &lt; 0 || index &gt;= size()).
*/
// public void set(int index, Object element) {
// 	if (index >= size || index < 0)
// 		throw new IndexOutOfBoundsException("Index: "+index+", Size: "+size);
// 	elements[index] = element;
// }

/* TODO: Update documentation to reflect new definition of set */
public void set(int index, Object element) {
	if (index < 0)
		throw new IndexOutOfBoundsException("Index: "+index+", Size: "+size);
  else if (index >= size) {
      if (index >= elements.length) ensureCapacity(index + 1);
      size = index + 1;
  }
  elements[index] = element;
}
/**
 * Replaces the element at the specified position in the receiver with the specified element; <b>WARNING:</b> Does not check preconditions.
 * Provided with invalid parameters this method may access invalid indexes without throwing any exception!
 * <b>You should only use this method when you are absolutely sure that the index is within bounds.</b>
 * Precondition (unchecked): <tt>index &gt;= 0 && index &lt; size()</tt>.
 *
 * @param index index of element to replace.
 * @param element element to be stored at the specified position.
 */
public void setQuick(int index, Object element) {
	elements[index] = element;
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
public ObjectArrayList times(int times) {
	ObjectArrayList newList = new ObjectArrayList(times*size);
	for (int i=times; --i >= 0; ) {
		newList.addAllOfFromTo(this,0,size()-1);
	}
	return newList;
}
/**
 * Returns an array containing all of the elements in the receiver in the
 * correct order.  The runtime type of the returned array is that of the
 * specified array.  If the receiver fits in the specified array, it is
 * returned therein.  Otherwise, a new array is allocated with the runtime
 * type of the specified array and the size of the receiver.
 * <p>
 * If the receiver fits in the specified array with room to spare
 * (i.e., the array has more elements than the receiver),
 * the element in the array immediately following the end of the
 * receiver is set to null.  This is useful in determining the length
 * of the receiver <em>only</em> if the caller knows that the receiver
 * does not contain any null elements.
 *
 * @param array the array into which the elements of the receiver are to
 *		be stored, if it is big enough; otherwise, a new array of the
 * 		same runtime type is allocated for this purpose.
 * @return an array containing the elements of the receiver.
 * @exception ArrayStoreException the runtime type of <tt>array</tt> is not a supertype
 * of the runtime type of every element in the receiver.
 */
public Object[] toArray(Object array[]) {
	if (array.length < size)
		array = (Object[])java.lang.reflect.Array.newInstance(array.getClass().getComponentType(), size);

	Object[] theElements = elements;
	for (int i=size; --i >=0; ) array[i]=theElements[i];

	if (array.length > size) array[size] = null;

	return array;
}
/**
* Returns a string representation of the receiver, containing
* the String representation of each element.
*/
public String toString() {
	return cern.colt.Arrays.toString(partFromTo(0, size()-1).elements());
}
/**
 * Trims the capacity of the receiver to be the receiver's current 
 * size. Releases any superfluos internal memory. An application can use this operation to minimize the 
 * storage of the receiver.
 */
public void trimToSize() {
	elements = cern.colt.Arrays.trimToCapacity(elements,size());
}
}
