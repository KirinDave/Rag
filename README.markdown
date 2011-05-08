What Is This?
============

This is a project description I wrote to record part of the process I like to follow when I'm learning a new language. Some people have been asking me if they could get insight into my process, since I learn a not of new languages quickly.

I'm finally getting around to learning Haskell, so I'm taking the time to record my projects and document them in a project format. For the people following along, this is my project. 

Introduction
------------ 
	
When I was a young pup, the internet was a new thing. Prodigy was the in thing, AOL was new and shiny, and "warez" was just a trick to search. There was an ancient game called "SPAM: Simple Programmable Adventure For Macintosh" that we played with in the computer lab. "Simple" was a good name for it; you could move around and investigate the environment, but you couldn't pick things up. Every room had a list of exits and valid commands. Exits moved you to another room, and commands spit out some text on the screen. It wasn't a very powerful system, but its file format was so simple you could type it in by hand. 

Because of this—among kids for whom programming was still a very big deal—it got very popular. People shared text adventures in online forums, often with crazy themes and almost always with hidden rooms. There were whole libraries of adventures, and they got surprisingly complex and involved given how simple the game was.

If you search for SPAM now, it's gone. The World—as Stephen King might say—has moved on. You can't even find it on 
Google, it's that vanished. So I'm going to recreate it.

Specifics
---------

I am going to create a very simple Interactive Fiction Machine. It will read in files that desribe our environment, and then read it back to us and let us move around them. I call it RAG, Reconstituted Adventure Game.

The output should look like this (normal text is what the computer writes, bold text is what the player writes back):

	A Maze
	You are in the center of a maze. 
	Exits: east, south
	> south
	
	The Center of the Maze
	A fountain quietly burbles in the center of this maze, housed by a belltower. A gentle breeze
	rustles the hedges surrounding you. A thick velvet rope hangs down from the belltower, swaying
	softly.
	Exits: north, east, west
	> drink from fountain
	
	Sorry, I don't understand.
	> look
	
	The Center of the Maze
	A fountain quietly burbles in the center of this maze, housed by a belltower. A gentle breeze
	rustles the hedges surrounding you. A thick velvet rope hangs down from the belltower, swaying
	softly.
	Exits: north
	> pull rope
	
	A deep gonging sound can be heard. There is a sound of mechanical action beneath your feet,
	and the hedge to the south ripples and shuffles like a door has opened behind it. (You can go
	south from here).	
	> south
	
	The Real Center Of The Maze
	The wizard's tower stands here, forboding. Its dark stone buttresses crowd back the hedges of the
	maze. Clouds gather about the top of its spiraling reach. There is a door in front of you, slightly ajar.
	Exits: door, north
	> door
	
	... 

From looking at this output, one might notice a few things right away:
* It's obvious that there are rooms.
* I can "look" in these rooms to repeat the description.
* I can type in random garbage and the program should tell us it doesn't understand.
* I can type in the name of an exit and move that way to a new room.
* Some rooms have hidden exits that don't show up
* Some rooms have special commands the player can type to get more instructions.

SPAM adventures don't have things that you might expect if you played Zork. The player has no inventory, it doesn't track what you'reholding or what you can use. The world doesn't have doors or bridges (we might say it is "static", once loaded it never changes). Only the player's knowledge of the world changes. In this way, it's a lot like a book. 

So what might the file look like for this little adventure? The RAG file is simple. Here's an example of what the adventure above might look like:

```
1|A Maze|You are in the center of a maze.|south=2,east=2||
2|The Center of the Maze|A fountain quietly burbles in the center of this maze, housed by a belltower. A gentle breeze rustles the hedges surrounding you. A thick velvet rope hangs down from the belltower, swaying softly.|north=1|south=3|pull rope=A deep gonging sound can be heard. There is a sound of mechanical action beneath your feet, and the hedge to the south ripples and shuffles like a door has opened behind it. (You can go south from here).\dance=You dance quietly while no one can see you.
3|The Real Center of the Maze|The wizard's tower stands here, forboding. Its dark stone buttresses crowd back the hedges of the maze. Clouds gather about the top of its spiraling reach. There is a door in front of you, slightly ajar.|north=2,door=4||
```

You can probably figure it out, but let's document this carefully. Each room is made up of 6 fields separated by 5 |'s. Any of these fields can be blank. Some of the fields are complicated, but the first 3 are simple: id | Room Title | Room Description. Note that no newlines are allowed! One room per line. The next field is a little trickier, it's the visible exits for the room, which consists of a comma-separated list visible exists, done where = denotes the relationship. The next field after that is identical, but instead denotes hidden exits. The last field is the most complicated, it's the list of "free actions" in the room. For example, when we pull the rope in the center of the maze, it looks up the "pull rope" field and displays that text. Multiple actions are separated by backslashes.

Laying this out visually:
```
id | room name | room description | exits | hidden exists | free actions
```

id = any number. The game starts in room #1.
room name = The name of the room. WIll be displayed.
room description = The description for the room. Will be displayed.
exits = A comma-separated list of name = number where the number is an ID for another room.
hidden exits = Like exits, but the game will not display these exits.
free actions = A list of backslash separated free actions, string = action text. Actions may contain = signs, but not backslashes.

This should be enough to get someone following along. Get your program to run this maze, and then consider adding one of the following features:

* Try to word-wrap the description so it fits in an 80 character wide terminal.
* Some terminal colors would look pretty sweet.
* Make it so that you can reload the maze while you are in it! This is great for making new mazes. (Hint: What happens if an exit leads to a room that doesn't exist?)
* What if the file is up on a website. Do we really HAVE to download it to our computer first?
* Your adventure may behave unexpectedly or crash when the file is missing a | or has an error. 
* Could your program behave better? Could it help the author fix the error with a good error message?


