# Get a copy of the data
#

.PHONY: clean data-files

data-files: cetml1659on.dat cetdl1172on.dat

cetml1659on.dat:
	curl -O https://www.metoffice.gov.uk/hadobs/hadcet/cetml1659on.dat

cetdl1172on.dat:
	curl -O https://www.metoffice.gov.uk/hadobs/hadcet/cetdl1772on.dat

clean:
	rm -f cetml1659on.dat cetdl1172on.dat *~
