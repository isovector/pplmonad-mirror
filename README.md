# pplmonad

My take on a classic.

## Getting started ##

### System dependencies ###

To build and run pplmonad you need the following software on your system:

* [Haskell Platform](https://www.haskell.org/platform/)
* [Simple DirectMedia Layer 2 (SDL2)](https://libsdl.org)
* [SDL\_image\_2.0](https://libsdl.org/projects/SDL_image/)
* [SDL\_mixer\_2.0](https://libsdl.org/projects/SDL_mixer/)

Install them using your system package manager: APT for Debian users, Homebrew for Mac OS users, etc. The Homebrew commands to install these are, respectively:

    brew cask install haskell-platform
    brew install sdl2
    brew install sdl2_image
    brew install sdl2_mixer

### Stack ###

pplmonad is a program written in Haskell using tools provided in the Haskell Platform. One of them, [Stack](https://docs.haskellstack.org/en/stable/README/), is an interface to the rest of them. Stack runs the [Glasgow Haskell Compiler (GHC)](https://www.haskell.org/ghc/) on the source code and uses [Cabal](https://www.haskell.org/cabal/) to manage packages (libraries) from the [Stackage](https://www.stackage.org) server.

Use the following command to make Stack build pplmonad:

    stack build

After the build process is complete, run the resulting binary with:

    stack exec pplmonad

### Haskell dependencies ###

The libraries that pplmonad depends upon are:

* [array](https://hackage.haskell.org/package/array)
* [containers](https://hackage.haskell.org/package/containers)
* [mtl](https://hackage.haskell.org/package/mtl)
* [random](https://hackage.haskell.org/package/random)
* [sdl2](https://hackage.haskell.org/package/sdl2)
* [sdl2-image](https://hackage.haskell.org/package/sdl2-image)
* [sdl2-mixer](https://hackage.haskell.org/package/sdl2-mixer)
* [text](https://hackage.haskell.org/package/text)
* [time](https://hackage.haskell.org/package/time)
* [Yampa](https://hackage.haskell.org/package/Yampa)

You may need to run `stack install` on these package names before they are available to `stack build`, as in:

    stack install Yampa

### Offline data ###

At run time pplmonad loads files from a directory called `offline` in the current working directory. These files are \"offline data,\" and include images, sound clips, and text files. The program terminates unless the following files exist and are well-formed:

* offline/maps/last-edited.txt
* offline/textures/default.png
* offline/sounds/accept.wav
* offline/sounds/main-theme.mp3
* offline/sounds/battle-theme.wav

The contents of the default map file, \"offline/maps/last-edited.txt\", must be text in the format written by the Field Editor. For example:

    Terrain {tBackgroundColor = White, tElements = fromList []}

In the future pplmonad will handle missing offline data more gracefully.

## Controls ##

### Normal ###

* D-pad: Arrow keys
* A: Return key
* B: right Shift key
* Start: Space bar

### Field Editor ###

* Change tool: \"Q\" key
* Select/actuate: right mouse button\*

\*this would be the left mouse button but for a bug in [version 2.4.0.1 of sdl2](https://hackage.haskell.org/package/sdl2-2.4.0.1) that [should be fixed](https://github.com/haskell-game/sdl2/pull/177) in the next release

### Debug/development ###

* Stop main automaton: hold \"0\" key
* Retard main automaton: hold \"1\" key
* Accelerate main automaton: hold \"2\" key
* Open/close Field Editor: Tab key

## Field Editor ##

pplmonad includes a direct-manipulation interface called the Field Editor for creating and editing maps. While running pplmonad, during the Field phase, turn the Field Editor on and off by pressing the Tab key. When it is on, you can use the mouse to alter the layout of the tiles that comprise the current terrain and save it as offline data.

The Field Editor makes changes to the `Terrain` structure used in the current `locale`. It makes those changes through the current *tool*, which is one of the following:

* Basic
* Portal
* Habitat
* Erase
* Clear All
* Set Background Color

To use the current tool, click the mouse (see \"Controls\" above) while the mouse cursor is over the appropriate terrain tile. Some tools, such as Basic, only affect the tile under the cursor. Others, such as Clear All, do the same thing no matter where you click.

You can use a different tool by pressing the \"change tool\" key (see \"Controls\" above). When you press this button, the next tool in the list becomes the current tool.

You can see which tool is the current tool by moving the mouse cursor to the top of the screen. A panel will appear that displays the name of the current tool along with its current options. Some of these options are variable and depicted as such with a special marker. Click the markers with the mouse to change the option's value. The subsequent details of changing the value differ between different options; you can find them the tool descriptions below.

Also in the panel is the save button. Click this button to save the current map to an offline data file called \"offline/maps/last-edited.txt\". This is the default map that pplmonad loads at startup, so you will immediately see your saved work the next time you run.

* * *

### Basic (tile name) (collides) ###
Create a basic terrain element with a given tile image and colliding flag

* **tile name**: image for this element; click to cycle through the list of all tiles
* **collides**: whether characters collide with this element; click to toggle between colliding (\"True\") and not colliding (\"False\")

* * *

### Portal (destination) (position) ###
Make an existing terrain element a portal through which the protagonist can travel to another map

* **destination**: name of the destination map; click to cycle through the list of all maps
* **position**: initial position of the protagonist after traveling; click and drag horizontally and vertically to add an offset

* * *

### Habitat (encounter rate) (name) (level) (population) ###
Make an existing terrain element a person habitat where the protagonist can encounter a certain kind of person

* **encounter rate**: the probability of encountering any person; click and drag horizontally to add or subtract probability in hundredths
* **name**: the name of a person to add to the habitat; click to cycle through list of all names
* **level**: the level of a person to add to the habitat; click and drag horizontally to add or subtract levels
* **population**: the number of people to add (one individual, from the total population ever added, is encountered at a time); click and drag horizontally to add or subtract individuals

* * *

### Erase ###
Delete a single element

* * *

### Clear All ###
Delete all elements

* * *

### Set Background Color (color) ###
Change the color drawn behind all elements

* **color**: the background color; click to cycle through all background colors
