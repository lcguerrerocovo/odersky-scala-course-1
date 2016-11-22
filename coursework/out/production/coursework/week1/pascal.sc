def pascal(r: Int, c: Int): Int = {
  if(r <= 1 || c == 1 || c == r) 1
  else {
    pascal(r - 1, c - 1) + pascal(r - 1, c)
  }
}

def pascalPrint(rows: Int) : String = {
  def pascalPrintRow(r: Int, c: Int) : String = {
    if(c != 0) {
      " " + pascal(r,c) + pascalPrintRow(r, c-1)
    }
    else ""
  }
  if(rows < 0) ""
  else pascalPrint(rows-1)  + "\n" + pascalPrintRow(rows,rows)
}
pascalPrint(10)

