# Bucklang

I'm making a programming language that I'd like to use.

Key features:

- Inbuilt collection objects (lists, maps) are immutable by default, with an API inspired by Immutable.JS
- Syntax is a cross between Javascript and Scala
- The language is interpreted. I am writing it in Scala.
    - I am using FastParse as a parser; as always, thanks heaps to Li Haoyi for writing half the libraries I needed for this project :D

## Example code


    class ConnectFour {
      new (board = {}) {
        this.board = board || {};
      }
      
      addPiece(player, x) {
        val y = this.board.count(([x2, y]) => x == x2);
        this.board.set([x, y], player);
      }
      
      toString() {
        val rowToString = (y) => "|#{7.times((x) => this.board.get([x, y], '-')}|";
        return range(6).reverse().map(rowToString).join("\n");
      }
      
      ::DELTAS = [[0, 1], [1, 0], [1, 1], [1, -1]];
      
      checkWinner (player) {
        val checkWinnerWithDeltaAndStart = (x, y, dx, dy) => (
          range(4).map(i => [x + i * dx, y + i * dy]).map(p => this.board.get(p)).count(player) == 4;
        );
        
        return ranges(6, 7).cross(::DELTAS).any(([x, y], [dx, dy]) => checkWinnerWithDeltaAndStart(x, y, dx, dy));
      }
      
      
      checkDraw () => this.board.size == 42
      
      ::play() {
        val board = ConnectFour();
        
        print(board.toString() + "\n");
        
        42.times((turnNumber, {...break}) => {
          val playerNumber = turnNumber % 2 + 1;
          print("Player #{playerNumber}'s turn");
          val x = getValidatedInput("%d", (x) => (board.board.has([x, 6]) && "That column is full"));
          board.addPiece(playerNumber, []);
          
          if (board.checkWinner(playerNumber)) {
            print("Player #{playerNumber} has won!");
            break();
          }
        })
      }
    }
