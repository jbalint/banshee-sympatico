#curl -g -o - 'http://jessandmorgan.com:7654/jsonrpc?request={"jsonrpc":"2.0","method":"Files.GetDirectory","params":{"directory":"/mnt/red_or_blue/torrents/"},"id":"99"}'  | jq .

curl -g -o - 'http://jessandmorgan.com:7654/jsonrpc?request={"jsonrpc":"2.0","method":"Files.GetFileDetails","params":{"file":"/mnt/red_or_blue/torrents/Saturday.Night.Live.S46E03.Issa.Rae.480p.x264-mSD[eztv.io].mkv","properties":["duration","size","title","runtime","lastplayed","resume","dateadded","playcount"]},"id":"99"}'  | jq .

#curl -g -o - 'http://jessandmorgan.com:7654/jsonrpc?request={"jsonrpc":"2.0","method":"Files.GetSources","params":{"media":"video"},"id":"99"}'  | jq .
