bool isEmpty(){  
  if(this->data == 0)
    return true;
  bool result = true;
  for (int i = 0; i < 10; i++) {
    if(readFromFile("input.txt", i)) {
      result = indexes[i].matched;
    }
    indexes[i].setMatched(true);
  }
  return result;
}
