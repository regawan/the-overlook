package o1.adventure

import scala.collection.mutable.Map


/** A `Player` object represents a player character controlled by the real-life user of the program.
  *
  * A player object's state is mutable: the player's location and possessions can change, for instance.
  *
  * @param startingArea  the initial location of the player */
class Player(name: String,startingArea: Area) {

  private var currentLocation = startingArea        // gatherer: changes in relation to the previous location
  private var quitCommandGiven = false              // one-way flag
  private var itemsCarried = Map[String, Item]()


  /** Determines if the player has indicated a desire to quit the game. */
  def hasQuit = this.quitCommandGiven


  /** Returns the current location of the player. */
  def location = this.currentLocation


  /** Attempts to move the player in the given direction. This is successful if there
    * is an exit from the player's current location towards the direction name. Returns
    * a description of the result: "You go DIRECTION." or "You can't go DIRECTION." */
  def go(direction: String) = {
    val destination = this.location.neighbor(direction)
    this.currentLocation = destination.getOrElse(this.currentLocation)
    if (destination.isDefined) "You go " + direction + "." else "You can't go " + direction + "."
  }


  /** Causes the player to rest for a short while (this has no substantial effect in game terms).
    * Returns a description of what happened. */
  def rest() = {
    "You rest for a while. Better get a move on, though."
  }


  /** Signals that the player wants to quit the game. Returns a description of what happened within
    * the game as a result (which is the empty string, in this case). */
  def quit() = {
    this.quitCommandGiven = true
    ""
  }
  
  
  def get(itemName: String) = {
    if (this.location.contains(itemName)) {
      this.itemsCarried += itemName -> this.location.removeItem(itemName).get
      s"You pick up the $itemName."
      }
    else s"There is no $itemName here to pick up."
  }
  
  def has(itemName: String) = this.itemsCarried.contains(itemName)
  
  def drop(itemName: String) = {
    if (this.has(itemName)){
      this.location.addItem(this.itemsCarried(itemName))
      this.itemsCarried -= itemName
      s"You drop the $itemName."
    } else "You don't have that!"
  }

  def examine(itemName: String) = {
    if (this.has(itemName))
      "You look closely at the " + itemName + ".\n" + this.itemsCarried(itemName).description
    else "If you want to examine something, you need to pick it up first."
  }
  
  def inventory = {
    if (this.itemsCarried.nonEmpty)
      "You are carrying\n" + this.itemsCarried.keys.mkString("\n")
    else "You are empty-handed."
  }
  
  def help = {
    "Your objective is to make her stay forever, one way or the other.\nWalk around the hotel and beat the challenges.\nHint: You remember Wendy is in the lobby.\nCommand list:\ngo <direction>\nrest\nget <item>\ndrop <item>\ninventory\nexamine <item>\nsay <name>\n use <item>\nquit"
  }
  
  def use(itemName: String) = {
    if (this.has(itemName)){
      this.itemsCarried(itemName).use
    }
    else "If you want to use something, you need to pick it up first."
  }
  
  def say(name: String) = {
    if (name == "loyd"){
      if (this.location.name == "Bar") {
        this.location.addItem(new Item("keycard", "It's Loyd's keycard.") {
          def use = {
            ""
          }
        })
        "You: 'Hi Loyd. I always liked you. You were always the best of them.'\n     'Best goddamned bartender from Timbuctoo to Portland Maine.'\nLoyd: 'Thank you for saying so.'\nYou: 'You know Loyd, I'm in a pickle here.'\nLoyd: 'How can I help you Sir?'\n"
      }
      else "If you want to talk to Loyd you should find him first."
    }
    else if (name == "wendy"){
      if (this.location.name == "Lobby")
        "You: 'Hi Wendy. Why are you crying? You are going to stay here forever!'\n     'This is what you wanted right!?'\nWendy: 'LEAVE US ALONE!!!'\nYou: 'You know Wendy, I'm in a REAL pickle here.'\nWendy: 'PLEASE JACK'\nYou: 'I think you have to stay, HERE'S JOHNNY!'"
      else "If you want to talk to Wendy you should find her first."
    }
    else s"There is noone named $name in this room."
  }
  

  /** Returns a brief description of the player's state, for debugging purposes. */
  override def toString = "Now at: " + this.location.name


}


