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
  var itemsCarried = Map[String, Item]()


  /** Determines if the player has indicated a desire to quit the game. */
  def hasQuit = this.quitCommandGiven


  /** Returns the current location of the player. */
  def location = this.currentLocation


  /** Attempts to move the player in the given direction. This is successful if there
    * is an exit from the player's current location towards the direction name. Returns
    * a description of the result: "You go DIRECTION." or "You can't go DIRECTION." */
  def go(direction: String): String = {
    val destination = this.location.neighbor(direction)
    if (destination.isDefined && !destination.get.isPassable) "The door is locked. It looks like it can be opened with a keycard."
    if (destination.isDefined && destination.get.isPassable ) {
      this.currentLocation = destination.getOrElse(this.currentLocation)
      "You go " + direction + "."
    }
    else "You can't go " + direction + "."
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
  
  //Get specified item
  def get(itemName: String) = {
    if (this.location.contains(itemName)) {
      this.itemsCarried += itemName -> this.location.removeItem(itemName).get
      s"You pick up the $itemName.\n" + this.itemsCarried(itemName).description
      }
    else s"There is no $itemName here to pick up."
  }
  
  //Determines whether the player has a specified item
  def has(itemName: String) = this.itemsCarried.contains(itemName)
  
  //Drops an item to the current area
  def drop(itemName: String) = {
    if (this.has(itemName)){
      this.location.addItem(this.itemsCarried(itemName))
      this.itemsCarried -= itemName
      s"You drop the $itemName."
    } else "You don't have that!"
  }
  
  //Returns a description of the specified item
  def examine(itemName: String) = {
    if (this.has(itemName))
      "You look closely at the " + itemName + ".\n" + this.itemsCarried(itemName).description
    else "If you want to examine something, you need to pick it up first."
  }
  
  //Returns a string with all carried items
  def inventory = {
    if (this.itemsCarried.nonEmpty)
      "You are carrying\n" + this.itemsCarried.keys.mkString("\n")
    else "You are empty-handed."
  }
  
  //Returns a a few tips if you get lost and a list of available commands
  def help = {
    "Your objective is to make her stay forever, one way or the other.\nWalk around the hotel and complete the tasks.\nHint: You remember Wendy is in the lobby.\nCommand list:\ngo <direction>\nrest\nget <item>\ndrop <item>\ninventory\nexamine <item>\nsay <name>\nuse <item>\nquit"
  }
  
  //Uses the specified item
  def use(itemName: String) = {
    if (this.has(itemName)){
      this.itemsCarried(itemName).use
    }
    else "If you want to use something, you need to pick it up first."
  }
  
  var loydIsHappy = false
  var wendyIsHappy = false
  var failed = false
  
  
  val keycard = new Item("keycard", "It's Loyd's keycard.") {
            def use = {
              if (location.name == "Kitchen"){
                location.neighbor("north").get.isPassable = true
                "You unlock the door to the lobby."
              }
              else "You can't use the keycard here."
            }
          }
  
  //say function that interacts with NPC's
  def say(name: String) = {
    if (name == "loyd"){
      if (this.location.name == "Bar"){
        if (this.loydIsHappy){ // Loyd is happy if you buy a drink with money.
          this.location.addItem(keycard)
          this.loydIsHappy = false
          "You: 'Hi Loyd. I always liked you. You were always the best of them.'\n     'Best goddamned bartender from Timbuctoo to Portland Maine.'\nLoyd: 'Thank you for saying so.'\nYou: 'You know Loyd, I'm in a pickle here.'\nLoyd: 'How can I help you Sir?'\nYou: 'I really need to get to the lobby, but it's locked.'\nLoyd: 'Of course, feel free to use my keycard. On one condition though, you have to return it.'"
        }
        else {
          val yesOrNo = readLine("Hello Sir, would you like a glass of Bourbon?(y/n):")
          if (yesOrNo.toLowerCase() == "y"){
            if (!this.has("money"))
              "I'm sorry Sir, you must not mistake us for a charity. Please return with money."
            else {
              this.loydIsHappy = true
              this.itemsCarried -= "money"
              "Thank you for your purchase. I'll be here if you need anything."
            }
          }
          else "Ok then."
        }
      }
      else "If you want to talk to Loyd you should find him first."
    }
    else if (name == "wendy"){
      if (this.location.name == "Lobby"){
        if (!this.has("knife")){
          this.wendyIsHappy = true
          "You: 'Hi Wendy. Why are you crying? We are going to stay here forever.'\n     'This is what you wanted right!?'\nWendy: 'EITHER YOU STAY OR WE LEAVE NOW'\nYou: 'You know Wendy, I'm in a REAL pickle here.'\nWendy: 'Please honey!'\nYou: 'You should eat something, lets look for some food in the kitchen...'\nWendy: 'Just a bite, and then we leave!'\nYou: 'Sure darling, whatever you say...'\nWendy: 'Show me the way, honey.'"
        }
        else {
          this.failed=true
          "You: 'Hi Wendy. Why are you crying? We are going to stay here forever!'\nWendy: 'No Jack, I know what you are doing... You are a MONSTER!!!\n       'The police is waiting behind that door, go to hell Jack.'"
        }
      } 
      else "If you want to talk to Wendy you should find her first."
    }
    else s"There is noone named $name in this room."
  }
  

  /** Returns a brief description of the player's state, for debugging purposes. */
  override def toString = "Now at: " + this.location.name


}


