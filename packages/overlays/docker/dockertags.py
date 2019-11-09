import urllib.request
import json
import sys

try:
    if len(sys.argv) < 1:
        print("Specify an image to list tags from")
        quit()

    url = "https://registry.hub.docker.com/v1/repositories/" + sys.argv[1] + "/tags"
    data = json.loads(urllib.request.urlopen(url).read())
    tags = map(lambda tag: tag.get("name"), data)

    print("\n".join(tags))

except urllib.error.HTTPError as err:
    if err.code == 404:
        print("Tag not found")
        quit(127)

except KeyboardInterrupt as err:
    print("Interrupted by the user")
