package org.stringbuilder;

public interface StringBuilderInterface extends Appendable, CharSequence {
  StringBuilderInterface append(char character);

  StringBuilderInterface append(boolean bool);

  StringBuilderInterface append(int integer);

  StringBuilderInterface append(double number);

  StringBuilderInterface append(String string);

  StringBuilderInterface append(Object obj);

  StringBuilderInterface append(StringBuffer sb);

  StringBuilderInterface append(char[] str);

  StringBuilderInterface append(char[] str, int offset, int len);

  StringBuilderInterface append(long l);

  StringBuilderInterface append(float f);

  void undo();

  int capacity();

  void trimToSize();

  void setLength(int newLength);

  int codePointAt(int index);

  int codePointBefore(int index);

  int codePointCount(int beginIndex, int endIndex);

  int offsetByCodePoints(int index, int codePointOffset);

  void getChars(int srcBegin, int srcEnd, char[] dst, int dstBegin);

  void setCharAt(int index, char ch);

  StringBuilderInterface delete(int start, int end);

  StringBuilderInterface appendCodePoint(int codePoint);

  StringBuilderInterface deleteCharAt(int index);

  StringBuilderInterface replace(int start, int end, String str);

  String substring(int start);

  String substring(int start, int end);

  CharSequence subSequence(int start, int end);

  StringBuilderInterface insert(int index, char[] str, int offset, int len);

  StringBuilderInterface insert(int offset, Object obj);

  StringBuilderInterface  insert(int offset, String str);

  StringBuilderInterface insert(int offset, char[] str);
  StringBuilderInterface insert(int dstOffset, CharSequence s);

  StringBuilderInterface insert(int dstOffset, CharSequence s, int start, int end);

  StringBuilderInterface insert(int offset, boolean b);

  StringBuilderInterface insert(int offset, char c);

  StringBuilderInterface insert(int offset, int i);

  StringBuilderInterface  insert(int offset, long l);

  StringBuilderInterface insert(int offset, float f);
  StringBuilderInterface insert(int offset, double d);

  int indexOf(String str);

  int indexOf(String str, int fromIndex);

  int lastIndexOf(String str);

  int lastIndexOf(String str, int fromIndex);

  StringBuilderInterface  reverse();
}
