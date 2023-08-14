package org.stringbuilder;

public interface StringBuilderInterface<T>{
  T append(Character character);
  T append(Boolean bool);
  T append(Integer integer);
  T append(Double number);
  T append(String string);
  void undo();

}
