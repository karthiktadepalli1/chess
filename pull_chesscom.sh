rm -f pgns/chesscom_full.pgn
for i in "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12"; do
  curl https://api.chess.com/pub/player/therealslim_kt/games/2020/$i/pgn >> pgns/chesscom_full.pgn
done
