package o1.adventure


/** The class `Adventure` represents the text adventure game 'The Overlook'. The adventure consists of a player and
  * a number of areas that make up the game world. It provides methods for playing the game one
  * turn at a time and for checking the state of the game.
  *
  * N.B. This version of the class has a lot of "hard-coded" information which pertain to a very
  * specific adventure game that involves a small trip through a twisted forest. All newly created
  * instances of class `Adventure` are identical to each other. To create other kinds of adventure
  * games, you will need to modify or replace the source code of this class. */
class Adventure {

  /** The title of the adventure game. */
  val title = "The Overlook"

  private val pantry      = new Area("Pantry", "You are in a pantry. There's blood on the floor.\nIt's chilly.")
  private val kitchen     = new Area("Kitchen", "You are in the hotel kitchen. Not a soul.\nIt's eerily quiet.")
  private val bar         = new Area("Bar", "You are in the hotel bar. There's a man standing behind the counter.\nHe looks right through you. Just as if you really weren't there.\nYou remember that his name is Loyd.\nThe exits to the east are blocked by butlers.")
  private val diner       = new Area("Diner", "You are the hotel diner. It's empty.\nThe exits to west are blocked from the inside.")
  private val lobby       = new Area("Lobby", "You are in the lobby of the hotel. 'The Overlook' is written on the desk.\nA child and a woman ")
  private val home        = new Area("Home", "Home sweet home! Now the only thing you need is a working remote control.")
  private val destination = home

       pantry.setNeighbors(Vector(/*"north" -> kitchen*/                                                          ))
      kitchen.setNeighbors(Vector("north" -> lobby,   "east" -> bar,     "south" -> pantry, "west" -> diner   ))
          bar.setNeighbors(Vector(                                                          "west" -> kitchen ))
        diner.setNeighbors(Vector(                    "east" -> kitchen                                       ))
        lobby.setNeighbors(Vector(/*north" -> home,*/                       "south" -> kitchen                   ))
         home.setNeighbors(Vector(                                       "south" -> lobby                     ))

  //place these two items in diner and bar, respectively
  this.pantry.addItem(new Item("fireaxe", "             +-+\n=============| |\n            `:_;'\n\nIt's a fireaxe. Has many uses..."){
    def use = {
      if (player.location == pantry){
        pantry.setNeighbors(Vector("north" -> kitchen))
        "You use the fireaxe to destroy the hinges. The door falls to the ground."
      }
      else "You swing your axe but hit nothing."
    }
  })
  this.kitchen.addItem(new Item("knife", {
        "___________________________________ ______________________\n"+
        raw"\                                  | (_)     (_)    (_)   \ " + "\n" +
        raw" `.                                |  __________________   }" +"\n" +
        raw"   `-..........................____|_(                  )_/" + "\n\nIt's a big kitchen knife. Recently sharpened."
  }){
    def use = {
      if (player.location == wendy.location){
        lobby.setNeighbors(Vector("north" -> home,"south" -> kitchen))
        "You use the knife to lunge towards Wendy."
      }
      else "You swing your knife and hit noone."
    }
  })
  this.diner.addItem(new Item("money", "___________________________________\n|#######====================#######|\n|#(|)*UNITED STATES OF AMERICA*(|)#|\n" + raw"|#**          /===\   ********  **#|" + "\n|*# {G}      | (') |             #*|\n" + raw"|#*  ******  | /v\ |    O N E    *#|" + "\n" + raw"|#(|)         \===/            (|)#|" + "\n|##=========TEN DOLLARS==========##|\n------------------------------------\n\nIt's 10 dollars."){
    def use = {
      if (player.location == bartender.location){
        player.drop("money")
        bartender.get("money")
        "You buy a glass of bourbon. Loyd seems happy, you should speak with him again."
      }
      else "I think I should use this in the bar instead."
    }
  })
  
  /** The character that the player controls in the game. */
  val player = new Player("Jack Torrance", pantry)
  
  /** NPCs: bartender and Wendy */
  val bartender = new Player("Loyd", bar)
  val wendy = new Player("Wendy Torrance", lobby)

  /** The number of turns that have passed since the start of the game. */
  var turnCount = 0
  /** The maximum number of turns that this adventure game allows before time runs out. */
  val timeLimit = 25


  /** Determines if the adventure is complete, that is, if the player has won. */
  def isComplete = false

  /** Determines whether the player has won, lost, or quit, thereby ending the game. */
  def isOver = this.isComplete || this.player.hasQuit || this.turnCount == this.timeLimit

  /** Returns a message that is to be displayed to the player at the beginning of the game. */
  def welcomeMessage = "\nd888888b db   db d88888b       .d88b.  db    db d88888b d8888b. db       .d88b.   .d88b.  db   dD\n`~~88~~' 88   88 88'          .8P  Y8. 88    88 88'     88  `8D 88      .8P  Y8. .8P  Y8. 88 ,8P'\n   88    88ooo88 88ooooo      88    88 Y8    8P 88ooooo 88oobY' 88      88    88 88    88 88,8P\n   88    88~~~88 88~~~~~      88    88 `8b  d8' 88~~~~~ 88`8b   88      88    88 88    88 88`8b\n   88    88   88 88.          `8b  d8'  `8bd8'  88.     88 `88. 88booo. `8b  d8' `8b  d8' 88 `88.\n   YP    YP   YP Y88888P       `Y88P'     YP    Y88888P 88   YD Y88888P  `Y88P'   `Y88P'  YP   YD\n\nYou wake up in a cold room with a throbbing headache.\nOddly, you can't remember why you are here and your head is bleeding.\nYou see a bunch of Heinz baked beans on some shelves, jummy.\n\nThere's a door in front of you with the word Redrum, you get a strange urge. MAKE HER STAY FOREVER"


  /** Returns a message that is to be displayed to the player at the end of the game. The message
    * will be different depending on whether or not the player has completed their quest. */
  def goodbyeMessage = {
    if (this.isComplete)
      "You made her stay, a happy family stays together.\nGame completed."
    else if (this.turnCount == this.timeLimit)
      "Oh no! You collapsed from the bleeding.\nGame over!"
    else  // game over due to player quitting
      "Quitter!"
  }


  /** Plays a turn by executing the given in-game command, such as "go west". Returns a textual
    * report of what happened, or an error message if the command was unknown. In the latter
    * case, no turns elapse. */
  def playTurn(command: String) = {
    val action = new Action(command)
    val outcomeReport = action.execute(this.player)
    if (turnCount == 12) {
      "You are starting to faint from the bleeding. MAKE HER STAY FASTER!!!"
    }
    if (outcomeReport.isDefined) {
      this.turnCount += 1
    }
    outcomeReport.getOrElse("Unknown command: \"" + command + "\".")
  }


}



