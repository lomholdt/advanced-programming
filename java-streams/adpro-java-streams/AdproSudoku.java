// Simple sudoku solver and maker, incorporating the constraints of
// the game and using backtracking (via lazy Java 8 streams).  
// sestoft@itu.dk * 2015-07-31, 2015-08-04, 2015-10-01

// THIS VERSION IS MISSING CRUCIAL PARTS OF THE PUZZLE SOLVER AND
// PUZZLE GENERATOR; THESE ARE TO BE DEVELOPED IN THE MINI-PROJECT

import java.io.*;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.Random;
import java.util.stream.Stream;
import java.util.Stack;

class AdproSudoku {
  public static void main(String[] args) throws IOException, InterruptedException {
    if (args.length >= 1 && args[0].equals("solve")) {
      Reader stdIn = new InputStreamReader(System.in);
      int[][] init = new int[9][9];
      int ch = stdIn.read();
      for (int r=0; r<9; r++) {
        for (int c=0; c<9; c++) {
          init[r][c] = '1' <= ch && ch <= '9' ? ch - '0' : 0;
          ch = stdIn.read();
        }
        while (ch == '\n' || ch == '\r')
          ch = stdIn.read(); 
      }
      
      new State(init).print();
      int[] count = new int[1];
      boolean trace = args.length >= 2 && args[1].equals("trace");
      new State(init).solutions(trace).forEach(s -> { count[0]++; s.print(); });
      if (count[0] == 0)
        System.out.println(">>> No solutions! <<<");
    } else if (args.length < 2) 
      System.out.println("Usage: java AdproSudoku <blanks> <guesses> [latex] [<quantity>] [<prefix>]\n"
                       + "or\n"
                       + "       java AdproSudoku solve [trace] < sudokupuzzle.txt");
    else {
      int blanks = Integer.parseInt(args[0]), 
          guesses = Integer.parseInt(args[1]);
      Stream<State> puzzles = State.make(blanks)
        .filter(s -> s.solutions().findAny().get().getGuesses()==guesses);
      if (args.length >= 3 && args[2].equals("latex")) {
        int quantity = args.length >= 4 ? Integer.parseInt(args[3]) : 10;
        String prefix = args.length >= 5 ? args[4] + "-" : "";
        String file = (args.length >= 5 ? args[4] + "-" : "sudoku") + quantity;
        PrintStream out = new PrintStream(file + ".tex");
	      // The idiom .limit(quantity+1).skip(1) speeds up parallel computation, ahem...
	      latexDoc(out, puzzles.limit(quantity+1).skip(1).toArray(State[]::new), prefix, blanks, guesses);	
      } else if (args.length >= 3 && args[2].equals("test")){
        System.out.println("Firing Test!");

        Stream<State> testPuzzles = State.make(blanks);
        // Stream.generate(() -> State.makeSkeleton()).limit(10).forEach(s -> s.print()); // Question 3.1
        // Stream.generate(() -> State.makeSkeleton()).flatMap(s -> s.solutions()).forEach(p -> p.print()); // Question 3.2
        
        int[][] trivial = {
          {1,2,3,4,5,6,7,8,9},
          {4,5,6,7,8,9,1,2,3},
          {7,8,9,1,2,3,4,5,6},
          {2,3,4,5,6,7,8,9,1},
          {5,6,7,8,9,1,2,3,4},
          {8,9,1,2,3,4,5,6,7},
          {3,4,5,6,7,8,9,1,2},
          {6,7,8,9,1,2,3,4,5},
          {9,1,2,3,4,5,6,7,8}
        };

        // int[][] trivial2 = {
        //   {9,6,3,8,5,2,7,4,1},
        //   {1,7,4,9,6,3,8,5,1},
        //   {2,8,5,1,7,4,9,6,3},
        //   {3,9,6,2,8,5,1,7,4},
        //   {4,1,7,3,9,6,2,8,5},
        //   {5,2,8,4,1,7,3,9,6},
        //   {6,3,9,5,2,8,4,1,7},
        //   {7,4,1,6,3,9,5,2,8},
        //   {8,5,2,7,4,1,6,3,9}
        // };

        State trivialState = new State(trivial); // Question 4.3
        trivialState.makeSolutions().skip(10000).forEach(s -> s.print()); // Question 4.3 and 4.5
        


        System.out.println("Ending Test...");
      }
      else
        puzzles.forEach(s -> { s.print(); s.solutions().findAny().get().print(); });
    }
  }

  public static void latexDoc(PrintStream out, State[] puzzles, String prefix, 
                              int blanks, int guesses)
  {
    out.println("\\documentclass[12pt,a4paper]{article}\n\\begin{document}\n");
    out.println("\\renewcommand{\\familydefault}{\\sfdefault}");
    int i = 1; 
    out.println("\\Large\\sf");
    for (State puzzle : puzzles) {
      puzzle.latexTabular(out, String.format("Sudoku %s%d", prefix, i++));
      out.println("\\vfill");
    }
    out.println("\\newpage");
    out.println("\\footnotesize");
    i = 1;
    for (State puzzle : puzzles) 
      puzzle.solutions().findAny().get()
            .latexTabular(out, String.format("Solution %s%d", prefix, i++));
    out.println("\\vfill");
    out.printf("Parameters: %d blanks, %d guesses. Created %tc%n", 
                      blanks, guesses, new Date());
    out.println("\\end{document}");
  }
}

// The state of a sudoku game

class State {
  // board[r][c] is the number 1...9 in the cell, or 0 for blank
  private final int[][] board = new int[9][9];
  // Solution statistics
  private int unique = 0, guesses = 0;

  // For shuffling
  private static final Random rnd = new Random();
  
  // Initialize state from 2D array
  public State(int[][] init) {
    for (int r=0; r<9; r++)
      for (int c=0; c<9; c++)
        this.board[r][c] = init[r][c];
  }

  // Deep clone of a state
  public State(State state) { 
    this(state.board); 
    this.unique = state.unique;
    this.guesses = state.guesses;
  }

  public int get(int r, int c) {
    return board[r][c];
  }

  public void setDestructive(int r, int c, int k) {
    board[r][c] = k;
    unique++;
  }

  public int getGuesses() {
    return guesses;
  }

  public State set(int r, int c, int k, boolean trace) {
    if (trace)
      System.out.printf("Speculatively set cell (%d,%d) to %d%n", r, c, k);    
    State clone = new State(this);
    clone.board[r][c] = k;
    clone.guesses++;
    return clone;
  }

  // The block of cell (r,c)
  public int b(int r, int c) {
    return r/3*3 + c/3;
  }

  // Subsets of integer set {1, ..., 9} represented by bit patterns
  // with least significant bit representing 1, thus ...987654321.
  // Intersection is "&", union is "|", complement is "~", and
  // normalization to the domain { 1, ..., 9 } is 0x1FF & S.
  private static int singleton(int n) {
    return n == 0 ? 0 : 1 << (n - 1);
  }

  // The set of unused (available) numbers in row r
  public int R(int r) {
    int used = ~0x1FF;
    for (int c=0; c<9; c++)
      used |= singleton(get(r, c));
    return ~used;
  }
  
  // The set of unused (available) numbers in column c
  public int C(int c) {
    int used = ~0x1FF;
    for (int r=0; r<9; r++)
      used |= singleton(get(r, c));
    return ~used;
  }

  // The set of unused (available) numbers in block b
  public int B(int b) {
    int used = ~0x1FF, r0 = b/3*3, c0 = b%3*3;
    for (int r=0; r<3; r++)
      for (int c=0; c<3; c++)
        used |= singleton(get(r0+r, c0+c));
    return ~used;
  }

  // A set of numbers that must be used in row r of block b,
  // because cannot be used in that row outside block b
  public int RB(int r, int b) {
    int res = R(r);
    for (int c=0; c<9; c++)
      if (get(r, c) == 0 && b(r, c) != b)
        res &= ~C(c);
    return res;
  }  

  // A set of numbers that must be used in column c of block b,
  // because cannot be used in that column outside block b
  public int CB(int c, int b) {
    int res = C(c);
    for (int r=0; r<9; r++)
      if (get(r, c) == 0 && b(r, c) != b)
        res &= ~R(r);
    return res;
  }  

  // The set of numbers available for cell (r,c)
  public int A(int r, int c) {
    return R(r) & C(c) & B(b(r, c));
  }

  // The numbers excluded from other cells than (r,c) in its block
  public int E(int r, int c) {
    int r0 = r/3*3, r1 = r0+(r+1)%3, r2 = r0+(r+2)%3;
    int c0 = c/3*3, c1 = c0+(c+1)%3, c2 = c0+(c+2)%3;
    int b = b(r, c), block = B(b);
    int rex = (~R(r1) & ~R(r2) | RB(r, b)) & block, 
        cex = (~C(c1) & ~C(c2) | CB(c, b)) & block; 
    int Erc = rex & cex;
    Erc |= get(r, c2)!=0 ? rex & ~C(c1) : 0;
    Erc |= get(r, c1)!=0 ? rex & ~C(c2) : 0;
    Erc |= get(r2, c)!=0 ? ~R(r1) & cex : 0;
    Erc |= get(r1, c)!=0 ? ~R(r2) & cex : 0;
    Erc |= get(r, c1)!=0 && get(r, c2)!=0 ? rex : 0;
    Erc |= get(r1, c)!=0 && get(r2, c)!=0 ? cex : 0;
    return Erc;
  }

  // First fill in all uniquely determined cells; if after that there
  // are no blanks we have a solution. Otherwise some cell
  // (bp.r,bp.c) is underdetermined, and we obtain all solutions
  // created from all possible values bp.possible of that cell.
  public Stream<State> solutions(boolean trace) {
    State state = new State(this);
    BranchPoint bp = null;
    while (bp == null && state.blankCount() > 0)
      bp = state.deterministic(trace);
    if (state.blankCount() == 0)
      return Stream.of(state);
    else {
      int r = bp.r, c = bp.c;
      if (trace && bp.possible.size() > 0)
        state.print();
      return bp.possible.stream().flatMap(i -> set(r, c, i, trace).solutions(trace));
    }
  }

  public Stream<State> solutions() { 
    return solutions(false);
  }

  // If some cell is impossible, return with bp.possible empty; if
  // some cell is uniquely determined, fill it and return null; if no
  // cell is impossible or uniquely determined, return non-null bp
  // with smallest bp.possible -- which has at least two elements.  If
  // after the for-loops bp==null, then there were no blanks.
  protected BranchPoint deterministic(boolean trace) {
    BranchPoint bp = null;
    for (int r=0; r<9; r++)
      for (int c=0; c<9; c++) 
        if (get(r, c) == 0) {
          int Arc = A(r, c), Erc = E(r, c), cand = Erc == 0 ? Arc : Arc & Erc;
          if (cand == 0) {
            if (trace)
              System.out.printf("No solution for cell (%d,%d)%n", r, c);
            return new BranchPoint(r, c, new ArrayList<Integer>()); // Empty set of solutions
          }
        }
    for (int r=0; r<9; r++)
      for (int c=0; c<9; c++) 
        if (get(r, c) == 0) {
          int Arc = A(r, c), Erc = E(r, c), cand = Erc == 0 ? Arc : Arc & Erc;
          ArrayList<Integer> possible = members(cand);
          if (possible.size() == 1) {
            if (trace)
              System.out.printf("Unique solution for cell (%d,%d): %d%n", r, c, possible.get(0));
            setDestructive(r, c, possible.get(0));
            return null;
          } else if (bp == null || possible.size() < bp.possible.size()) 
            bp = new BranchPoint(r, c, permutation(cand));
        }
    if (trace && bp != null)
      System.out.printf("Split on cell (%d,%d) with values %s%n", bp.r, bp.c, bp.possible);    
    return bp;
  }

  static class Cell {
    public final int r, c;
    public Cell(int r, int c) {
      this.r = r;
      this.c = c;
    }
  }

  static class BranchPoint extends Cell {
    public final ArrayList<Integer> possible;
    public BranchPoint(int r, int c, ArrayList<Integer> possible) {
      super(r, c);
      this.possible = possible;
    }
  }
  
  // The members of an integer set represented as a bit pattern
  public static ArrayList<Integer> members(int S) {
    ArrayList<Integer> res = new ArrayList<>();
    for (int i=1; i<=9; i++)
      if ((S & singleton(i)) != 0)
        res.add(i);
    return res;
  }

  public static ArrayList<Integer> permutation(int S) {
    ArrayList<Integer> perm = members(S);
    Collections.shuffle(perm, rnd);
    return perm;
  }

  // The cardinality of an integer set represented as a bit pattern
  public static int card(int S) {
    int count = 0;
    for (int i=1; i<=9; i++)
      if ((S & singleton(i)) != 0)
        count++;
    return count;
  }

  public int blankCount() {
    return Arrays.stream(board)
      .map(row -> (int)Arrays.stream(row).filter(k -> k == 0).count())
      .reduce(0, Integer::sum);
  }

  public ArrayList<Cell> nonBlanks() {
    ArrayList<Cell> res = new ArrayList<>();
    for (int r=0; r<9; r++) 
      for (int c=0; c<9; c++) 
        if (get(r, c) != 0)
          res.add(new Cell(r, c));
    return res;
  }

  private static final Object printlock = new Object();
  
  public void print() {
    synchronized (printlock) {
      String divider = "-------------------------------\n";
      System.out.print(divider);
      for (int r=0; r<9; r++) {
	System.out.print("|");
	for (int c=0; c<9; c++) 
	  System.out.printf(" %s %s", get(r, c) == 0 ? " " : get(r, c),
			    c % 3 == 2 ? "|" : "");
	System.out.printf("%n%s", r % 3 == 2 ? divider : "");
      }
      System.out.printf("%d unique, %d guess(es)%n", unique, guesses); 
    }
  }

  public void latexTabular(PrintStream out, String caption) {
    out.println("\\begin{center}");
    out.println("\\begin{tabular}{||c|c|c||c|c|c||c|c|c||}\\hline\\hline");
    for (int r=0; r<9; r++) {
      for (int c=0; c<9; c++) 
        out.printf("~%s~%s", get(r, c) == 0 ? " " : get(r, c),
                                    c < 8 ? "&" : "\\\\\\hline");
      out.printf("%n%s", r < 8 && r % 3 == 2 ? "\\hline\n" : "");
    }
    out.println("\\hline\\end{tabular}\\\\[1.5ex]");
    out.println(caption);
    out.println("\\end{center}");
    // out.printf("%d unique, %d guess(es)%n", unique, guesses); 
  }

  // Return random partial sudoku constrained enough so it is easy to
  // construct the full solved sudoku (but may also be overconstrained).
  static State makeSkeleton() { 
    int[][] init = new int[9][9];
    ArrayList<Integer> firstBlock = permutation(0x1FF);
    for (int i=0; i<9; i++)
      init[i/3][i%3] = firstBlock.get(i);
    State state = new State(init);
    ArrayList<Integer> firstRow = permutation(state.R(0)), 
                       firstCol = permutation(state.C(0));
    for (int i = 0; i < 9; i++) { // fill row 0 and col 0
      if(init[i/9][i%9] == 0) init[i/9][i%9] = firstRow.get(i % firstRow.size()); // fill row 0
      if(init[i%9][i/9] == 0) init[i%9][i/9] = firstCol.get(i % firstCol.size()); // fill coll 0
      state = new State(init);
    }
    for (int i = 3; i < 6; i++) {
      init[i%9][3] = permutation(state.A(i%9, 3)).get(0); // TODO Crap implementation, maybe figure out something clever instead
      state = new State(init);
    }
    for (int i = 6; i < 9; i++) {
      init[i%9][6] = permutation(state.A(i%9, 6)).get(0); // TODO Crap implementation, maybe figure out something clever instead
      state = new State(init);    
    }
    for (int i = 3; i < 6; i++) {
      init[i%9][7] = permutation(state.A(i%9, 7)).get(0); // TODO Crap implementation, maybe figure out something clever instead
      state = new State(init);
    }

    return new State(init); 
  }

  // Question 3.5
  public static Stream<State> make(int blanks) {
    Stream<State> skeleta = Stream.generate(() -> makeSkeleton());
    Stream<State> sudokus = skeleta.parallel().flatMap(s -> s.solutions().limit(1)); // Taken from slides
    Stream<State> puzzles = sudokus.flatMap(s -> new State(s.board).eraseEnough(blanks).limit(1)); // Taken from slides
    return puzzles;
  }
  
  // Question 3.3
  protected Stream<State> eraseEnough(int blanks) {
    if (blankCount() >= blanks) { return Stream.of(this); }
    else{ return eraseOne().flatMap(x -> x.eraseEnough(blanks)).limit(1); }
  }

  // Question 3.4
  protected Stream<State> eraseOne() {
    ArrayList<Cell> nonBlanks = nonBlanks();
    Collections.shuffle(nonBlanks);
    Stream<Cell> soc = nonBlanks.stream().limit(4);
    return soc.map(x -> erase(x.r, x.c)).filter(y -> y.solutions().count() == 1);
  }

  protected State erase(int r, int c) {
    State clone = new State(this.board);
    clone.board[r][c] = 0;
    return clone;
  }

  // Question 4.1
  protected State rowPerm(){
    State copy = new State(this);
    for (int i = 0; i < 9; i=i+3) { // band
        Stack<Integer> stack = getRndStack(2, 1);
        int a = stack.pop();
        int b = stack.pop();
        int c = stack.pop();
        int[] row1 = copy.board[i+a];
        int[] row2 = copy.board[i+b];
        int[] row3 = copy.board[i+c];
        copy.board[i+a] = row3;
        copy.board[i+b] = row1;
        copy.board[i+c] = row2;
    }
    return copy;
  }

  // Question 4.1
  protected State colPerm(){
    State copy = new State(this);
    for (int i = 0; i < 9; i=i+3) { // column
      Stack<Integer> stack = getRndStack(2, 1);
      int a = stack.pop();
      int b = stack.pop();
      int c = stack.pop();
      for (int j = 0; j < 9; j++) { // row
        int row1 = copy.board[j][i+a];
        int row2 = copy.board[j][i+b];
        int row3 = copy.board[j][i+c];
        copy.board[j][i+a] = row3;
        copy.board[j][i+b] = row1;
        copy.board[j][i+c] = row2;
      }    
    }
    return copy;
  }

  // Question 4.1
  protected State rowBandPerm(){
    State copy = new State(this);
    int[][] tmpBoard = getCloneOf(this.board);
    Stack<Integer> stack = getRndStack(6, 3);
    for (int i = 0;i < 9;i=i+3) {
      int rowRange = stack.pop();
      for (int j = 0; j < 3; j++) {
        copy.board[i+j] = tmpBoard[rowRange+j];
      }
    }
    return copy;
  }

  // Question 4.1
  protected State colBandPerm(){
    State copy = new State(this);
    int[][] tmpBoard = getCloneOf(this.board);
    Stack<Integer> stack = getRndStack(6, 3);
    for (int i = 0; i < 9; i=i+3) {
      int rowRange = stack.pop();
      for (int k = 0; k < 9; k++) {
        for (int j = 0; j < 3; j++) {
          copy.board[k][i+j] = tmpBoard[k][rowRange+j];
        }
      } 
    }
    return copy;
  }

  // protected State nineNumberPerm(){ // Non random JL edition
  //   State copy = new State(this);
  //   for (int row = 0; row < 9; row++) {
  //     for (int col = 0; col < 9; col++) {
  //       int current = copy.board[row][col];
  //       switch (current) {
  //         case 0: continue;
  //         default: copy.board[row][col] = (current%9)+1;
  //       }
  //     }
  //   }
  //   return copy;
  // }
  
  // Question 4.1
  protected State nineNumberPerm(){
    State state = new State(this);
    ArrayList<Integer> numbers = new ArrayList<Integer>();
    for (int i = 0; i < 9; i++) numbers.add(i, i+1);
    Collections.shuffle(numbers);
    for (int j = 0; j < 9; j++) {
      for (int k = 0; k < 9; k++) {
        state.board[k][j] = numbers.get(this.board[k][j]-1);
      }
    }
    return state;
  }

  protected Stack<Integer> getRndStack(int to, int interval){
    Stack<Integer> stack = new Stack<>();
    for(int i = 0; i < to+1; i=i+interval){ stack.push(i); }
    Collections.shuffle(stack);
    return stack;
  }

  protected int[][] getCloneOf(int[][] that){
    int[][] clone = new int[that.length][that.length];
    for (int i = 0; i < that.length; i++) {
      for (int j = 0; j < that.length; j++) {
        clone[i][j] = that[i][j];
      }
    }
    return clone;
  }

  // Question 4.2
  protected State randomTransform(State s){
    State copy = new State(s);
    int val = Math.abs(rnd.nextInt())%5;
    switch (val) {
      case 0: copy = copy.rowPerm(); break;
      case 1: copy = copy.colPerm(); break;
      case 2: copy = copy.rowBandPerm(); break;
      case 3: copy = copy.colBandPerm(); break;
      case 4: copy = copy.nineNumberPerm(); break;
      default: copy = copy.rowPerm();
    }
    copy.swapWithinRowbands();
    copy.swapWithinColbands();
    return copy;
  }

  // Question 4.6
  protected State randomTransformRandom(State s){
    State copy = new State(s);
    Stack<Integer> rnd = new Stack<>();
    for (int i = 0; i < 5; i++) { rnd.add(i); }
    Collections.shuffle(rnd);
    while(!rnd.isEmpty()){
      switch (rnd.pop()) {
        case 0: copy = copy.rowPerm(); break;
        case 1: copy = copy.colPerm(); break;
        case 2: copy = copy.rowBandPerm(); break;
        case 3: copy = copy.colBandPerm(); break;
        case 4: copy = copy.nineNumberPerm(); break;
        default: copy = copy.rowPerm();
      }
    }
    copy.swapWithinRowbands();
    copy.swapWithinColbands();
    return copy;
  }

  public Stream<State> makeSolutions(){
    return Stream.iterate(this, s -> this.randomTransform(s));
  }

  // Question 4.4
  protected State swapWithinRowbands(){
    State copy = new State(this);
    int[][] tmpBoard = getCloneOf(this.board);
    copy.print();
    for (int i = 0; i < 9; i=i+3) { // for each band row
      for(int j = 0;  j < 9; j++){ // column current
        int currA = copy.board[i+0][j], 
            currB = copy.board[i+1][j], 
            currC = copy.board[i+2][j];
        int[] current = { currA, currB, currC }; Arrays.sort(current);
        for(int k = j+1; k < 9; k++){ // column lookup
          int lookA = copy.board[i+0][k], 
              lookB = copy.board[i+1][k], 
              lookC = copy.board[i+2][k];
          int[] lookup = { lookA, lookB, lookC }; Arrays.sort(lookup);
          if(Arrays.equals(current, lookup)){ // swap current, lookup
            int tmpA = lookA, tmpB = lookB, tmpC = lookC;
            copy.board[i+0][k] = currA; copy.board[i+1][k] = currB; copy.board[i+2][k] = currC;
            copy.board[i+0][j] = tmpA; copy.board[i+1][j] = tmpB; copy.board[i+2][j] = tmpC;
            break; 
          }
        }
      }
    }
    return copy;
  }

  // Question 4.4
  protected State swapWithinColbands(){
    State copy = new State(this);
    int[][] tmpBoard = getCloneOf(this.board);
    for (int i = 0; i < 9; i=i+3) { // for each band row
      for(int j = 0;  j < 9; j++){ // column current
        int currA = copy.board[j][i+0], 
            currB = copy.board[j][i+1],
            currC = copy.board[j][i+2];
        int[] current = { currA, currB, currC }; Arrays.sort(current);
        for(int k = j+1; k < 9; k++){ // column lookup
          int lookA = copy.board[k][i+0], 
              lookB = copy.board[k][i+1], 
              lookC = copy.board[k][i+2];
          int[] lookup = { lookA, lookB, lookC }; Arrays.sort(lookup);
          if(Arrays.equals(current, lookup)){ // swap current, lookup
            int tmpA = lookA, tmpB = lookB, tmpC = lookC;
            copy.board[k][i+0] = currA; copy.board[k][i+1] = currB; copy.board[k][i+2] = currC;
            copy.board[j][i+0] = tmpA; copy.board[j][i+1] = tmpB; copy.board[j][i+2] = tmpC;
            break; 
          }
        }
      }
    }
    return copy;
  }
}
