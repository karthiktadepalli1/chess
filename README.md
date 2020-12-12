# Analyzing chess data

Supporting repository for [analyzing](https://www.karthiktadepalli.com/chess) my chess games using R's `bigchess` library to parse games. This repository consists of:

* `pull_chesscom.sh` - a shell script to pull Chess.com
* `pgn_read.R` - the file that reads in my games in PGN form and converts them into a single, cleaned CSV.
* `analysis.Rmd` - the R Markdown document that produces the analysis on my blog.

Lichess data was obtained by downloading my game history from my Lichess profile.

This code can be repurposed for your own analysis by substituting my username with yours and re-running these three scripts in order.
