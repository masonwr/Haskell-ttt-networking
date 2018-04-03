# Haskell Tic-tac-toe server/client

Networking class assignment, basic Tic-tac-toe server client. Implemented in haskell.

assuming that [stack](https://docs.haskellstack.org/en/stable/README/) is installed...


``` shellsession
> git clone https://github.com/masonwr/Haskell-ttt-networking.git
> cd Haskell-ttt-networking
> make
```

this installs the following two binary's: `ttt-client` and `ttt-server`. 

To run the server `sudo ttt-server` (the server runs on port 707). 

To run the client `ttt-client [ip of host]`, sit back and marvel at
the game unfolding before your eyes (if no host is specified, localhost is assumed). 


The game logic ([Minimax](https://en.wikipedia.org/wiki/Minimax)) is adapted from Gram Hutton's excellent [Programming in
Haskell](http://www.cs.nott.ac.uk/~pszgmh/pih.html) book (highly recommended).



<!--  LocalWords:  tac shellsession cd ttt sudo ip localhost
 -->
