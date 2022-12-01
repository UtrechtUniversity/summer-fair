# Retrieving the ontology from BioPortal

The latest version of our "Infection Transmission Ontology" is stored on BioPortal.
Here we illustrate how to retrieve ontology files from BioPortal, and we take our ontology as an example.

## Prerequisties
You need to create an account on BioPortal. In your profile you will find an API key. This key we need to connect to the portal.

## Python libraries and handy functions
```py
import urllib.request, urllib.error, urllib.parse
import json

def get_json(url):
    opener = urllib.request.build_opener()
    opener.addheaders = [('Authorization', 'apikey token=' + API_KEY)]
    return json.loads(opener.open(url).read())
    
def get_stream(url):
	opener = urllib.request.build_opener()
	opener.addheaders = [('Authorization', 'apikey token=' + API_KEY)]
	return opener.open(url).read()
```

## Listing all ontologies

```py
REST_URL = "http://data.bioontology.org"
API_KEY = <YOUR KEY>
topLevel = get_json(REST_URL + "/")
ontologies = get_json(topLevel['links']['ontologies'])
```
Each ontology is a dictionary with the following keys:

```py
ontologies[0].keys()
```
The link to access the content of an ontology is stored in `@id`

## Retrieving a specific ontology from the list
So now let us filter for the "Infection Transmission Ontology" and get the corresponding download link

```py
(ontName, ontId) = [(ont['name'], ont['@id']) for ont in ontologies if ont['name'] == "Infection Transmission Ontology"][0]
print(ontName, ontId)
```

With this link we can now access generic information about this specific ontology and the download link:

```py
get_json(ontId + "/")['links'].keys()
downloadUrl = get_json(ontId + "/")['links']['download']
stream = get_stream(downloadUrl)
```
In stream we find the binary code of the owl file.
We can save that stream to disk:

```py
with open("trans.owl", 'wb') as fileobj:
     ...:     fileobj.write(stream)
```


