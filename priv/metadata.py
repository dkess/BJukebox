from sys import argv

import youtube_dl

args = {"prefer_insecure": True, "noplaylist": True, "simulate": True, "default_search":"ytsearch1", "quiet":True}

ydl = youtube_dl.YoutubeDL(args)

thing = ydl.extract_info(argv[1])
if "entries" in thing:
    thing = thing["entries"][0]

print(thing["title"])
if thing["thumbnail"]:
    print(thing["thumbnail"])
else:
    print("")
print(thing["webpage_url"])
